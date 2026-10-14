"""Piece C: fallback residual calibration of a set of draws (minimum-KL
reweighting to the targets), for use while B's calibrate() does not
converge. Same problem as B's (SPEC.md, method 1), simpler solver.

Each record keeps its weight W_i; draw j gets w_ij = W_i m_ij exp(s_ij) /
sum_j' m_ij' exp(s_ij'), s_ij = sum of the multipliers of the cells the draw
falls in, m_ij = 0 for a draw in a zero cell (1 otherwise). The multipliers
minimize the convex dual
    f(lam) = sum_i W_i log mean_j m_ij exp(s_ij) - lam . t + (ridge/2) sum_c G_c lam_c^2
by L-BFGS on the GPU (float64; multipliers scaled by the square root of
each cell's group weight). The small ridge keeps the multipliers finite
when some cells cannot be hit together; at the optimum a cell's error as a
share of its group total is ridge * lam_c.

Presolve: draws in zero cells get weight zero, unless that would remove
every draw of a record (then the record keeps all its draws: counted).
Positive cells are set aside (left out of the solve and counted by family)
when no remaining draw reaches them, when the target exceeds the weight of
all records with some draw in the cell, or when it is below the weight of
the records all of whose draws fall in it. Other combinations that cannot
be met together (B's "block" and "joint" cases) are absorbed by the ridge
(default 1e-5) and show up as the largest remaining errors.

    sbatch c_gpu.sbatch c_resid.py --dir $P/c                     # SCF pilot
    sbatch c_gpu.sbatch c_resid.py --dir $P/full/c/P_core --arm P_core --dataset full
Writes <dir>/weights_resid_fallback.npz (w [N, K], lambda, cell) and
<dir>/report_resid_fallback.json.
"""
import argparse
import json
import os
import sys
import time

import numpy as np
import pandas as pd
import torch

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import c_arm                   # noqa: E402
import c_tilt as CT            # noqa: E402


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", required=True)
    ap.add_argument("--draws", default=None, help="default <dir>/draws.npz")
    ap.add_argument("--arm", default="scf")
    ap.add_argument("--dataset", default=None)
    ap.add_argument("--checkpoint", default=None)
    ap.add_argument("--ridge", type=float, default=1e-5)
    ap.add_argument("--max-iter", type=int, default=4000)
    ap.add_argument("--max-seconds", type=float, default=5000,
                    help="stop the solver after this long and write what it has")
    ap.add_argument("--tol", type=float, default=1e-7,
                    help="stop when every active cell is within tol of its group total")
    a = ap.parse_args()
    dev = torch.device("cuda")
    t_start = time.time()
    arm = c_arm.make_arm(a.arm, a.dataset, a.checkpoint)
    recs, cells = arm.recs, arm.cells
    T = cells["target"].to_numpy(np.float64)
    C = len(T)
    W = recs["weight"].to_numpy(np.float64)
    z = np.load(a.draws or os.path.join(a.dir, "draws.npz"))
    tokens = z["tokens"].astype(np.int64)
    N, K, _ = tokens.shape
    assert N == len(recs)
    idx = arm.contrib_index(tokens, recs)                         # [N*K, m], -1 none
    m = idx.shape[1]
    Wtot = W.sum()
    Wn = W / Wtot
    t = np.nan_to_num(T, nan=0.0) / Wtot
    # group total of each cell, from the records (A's block layout)
    blk = arm.blk
    bw = np.zeros(C // arm.VB + 1)
    for f in range(blk.shape[1]):
        ok = blk[:, f] >= 0
        np.add.at(bw, blk[ok, f], Wn[ok])
    G = np.repeat(bw[:-1], arm.VB)

    # presolve
    zero = T == 0
    in_zero = np.zeros(N * K, dtype=bool)
    ok_idx = idx >= 0
    in_zero = (ok_idx & zero[np.where(ok_idx, idx, 0)]).any(axis=1)
    mask = ~in_zero.reshape(N, K)
    relaxed = ~mask.any(axis=1)
    mask[relaxed] = True
    sel = idx[mask.reshape(-1)].reshape(-1)
    reach = np.bincount(sel[sel >= 0], minlength=C)
    pos = T > 0
    unreachable = pos & (reach == 0)
    # simple bounds: a cell's total can be at most the weight of records with
    # some remaining draw in it, and at least the weight of records all of
    # whose remaining draws are in it
    keepm = mask.reshape(-1)
    rec_of = np.repeat(np.arange(N), K)
    ii = np.repeat(rec_of[keepm], m)
    cc = idx[keepm].reshape(-1)
    okc = cc >= 0
    pairs = pd.DataFrame({"rec": ii[okc], "cell": cc[okc]})
    cnt = pairs.groupby(["cell", "rec"]).size().reset_index(name="n")
    ndraw = mask.sum(axis=1)
    upper = np.bincount(cnt["cell"], weights=W[cnt["rec"]], minlength=C)
    full_ = cnt["n"].to_numpy() == ndraw[cnt["rec"].to_numpy()]
    lower = np.bincount(cnt["cell"][full_], weights=W[cnt["rec"][full_]], minlength=C)
    above_reach = pos & ~unreachable & (T > upper * (1 + 1e-9))
    below_forced = pos & ~unreachable & (T < lower * (1 - 1e-9))
    active = pos & ~unreachable & ~above_reach & ~below_forced
    CT.log(f"{N:,} records x {K} draws, {C:,} cells: active {active.sum():,}, zero "
           f"{zero.sum():,}, unreachable {unreachable.sum():,}, no target "
           f"{np.isnan(T).sum():,}; above reach {above_reach.sum():,}; below forced "
           f"{below_forced.sum():,}; draws in zero cells {in_zero.sum():,}; records "
           f"relaxed {relaxed.sum():,}")

    # local unknowns
    act_ids = np.flatnonzero(active)
    lut = np.full(C + 1, len(act_ids), dtype=np.int64)
    lut[act_ids] = np.arange(len(act_ids))
    loc = lut[np.where(idx >= 0, idx, C)]                          # [N*K, m]
    loc_t = torch.as_tensor(loc, device=dev)
    mask_t = torch.as_tensor(mask, device=dev)
    W_t = torch.as_tensor(Wn, device=dev)
    t_t = torch.as_tensor(t[act_ids], device=dev)
    G_t = torch.as_tensor(G[act_ids], device=dev)
    scale = 1.0 / torch.sqrt(torch.clamp(torch.as_tensor(np.maximum(t[act_ids], 1e-3 * G[act_ids]),
                                                         device=dev), min=1e-300))
    logm = torch.where(mask_t, 0.0, float("-inf")).double()
    logn = torch.log(mask_t.double().sum(1))
    phi = torch.zeros(len(act_ids), dtype=torch.float64, device=dev, requires_grad=True)

    def lam_of(p):
        return torch.cat([p * scale, torch.zeros(1, dtype=torch.float64, device=dev)])

    def objective(p):
        lam = lam_of(p)
        s = lam[loc_t].sum(1).view(N, K) + logm
        lse = torch.logsumexp(s, dim=1) - logn
        return (W_t * lse).sum() - (lam[:-1] * t_t).sum() + 0.5 * a.ridge * (G_t * lam[:-1] ** 2).sum()

    def totals(p):
        with torch.no_grad():
            lam = lam_of(p)
            s = lam[loc_t].sum(1).view(N, K) + logm
            q = torch.softmax(s, dim=1)                             # share of record weight
            w = q * W_t[:, None]
            tot = torch.zeros(len(act_ids) + 1, dtype=torch.float64, device=dev)
            tot.index_add_(0, loc_t.reshape(-1), w.reshape(-1).repeat_interleave(m))
            return w, tot[:-1]

    opt = torch.optim.LBFGS([phi], lr=1.0, max_iter=50, history_size=30,
                            line_search_fn="strong_wolfe", tolerance_grad=1e-15,
                            tolerance_change=1e-18)

    def closure():
        opt.zero_grad()
        f = objective(phi)
        f.backward()
        return f
    it = 0
    hist = []
    while it < a.max_iter:
        f = opt.step(closure)
        it += 50
        _, tot = totals(phi)
        rel = ((tot - t_t).abs() / G_t).max().item()
        hist.append(dict(iter=it, f=float(f.detach()), max_rel_err=rel))
        CT.log(f"  L-BFGS {it}: f={float(f.detach()):.15e} max error share of group={rel:.3e}")
        if rel < a.tol:
            break
        if time.time() - t_start > a.max_seconds:
            CT.log("  time budget reached; writing the current solution")
            break
        if len(hist) > 3:
            df = abs(hist[-3]["f"] - hist[-1]["f"]) / max(abs(hist[-1]["f"]), 1e-12)
            de = abs(hist[-3]["max_rel_err"] - rel) / max(rel, 1e-15)
            if df < 1e-9 and de < 0.01:
                CT.log("  stalled: objective and largest error no longer changing")
                break
    w, tot = totals(phi)
    w = w.cpu().numpy() * Wtot
    lam_full = np.zeros(C)
    lam_full[act_ids] = (phi.detach() * scale).cpu().numpy()
    # report
    q = w / W[:, None]
    kl = float(np.sum(np.where(q > 0, w * np.log(np.where(q > 0, q * K, 1.0)), 0.0)) / Wtot)
    eff = 1.0 / (q ** 2).sum(axis=1)
    wr = np.repeat(w.reshape(-1), m)
    fl = idx.reshape(-1)
    achieved = np.bincount(fl[fl >= 0], weights=wr[fl >= 0], minlength=C)
    err = np.abs(achieved - np.nan_to_num(T)) / (G * Wtot)
    fam = cells["family"].to_numpy()
    rep = dict(
        solver="c_resid.py fallback (L-BFGS on the dual, ridge %g)" % a.ridge,
        n_records=int(N), K=int(K), n_cells=int(C),
        cells=dict(active=int(active.sum()), zero=int(zero.sum()),
                   unreachable=int(unreachable.sum()), above_reach=int(above_reach.sum()),
                   below_forced=int(below_forced.sum()), no_target=int(np.isnan(T).sum())),
        set_aside_by_family=pd.Series(fam[unreachable | above_reach | below_forced])
        .value_counts().to_dict(),
        draws_in_zero_cells=int(in_zero.sum()), records_relaxed=int(relaxed.sum()),
        iterations=it, history=hist[-5:],
        stopped=("converged" if hist and hist[-1]["max_rel_err"] < a.tol else
                 "time budget" if time.time() - t_start > a.max_seconds else
                 "iteration limit or stall"),
        active_max_err_share_of_group=float(err[active].max()),
        active_mean_err_share_of_group=float(err[active].mean()),
        active_cells_err_over_1_in_a_million=int((err[active] > 1e-6).sum()),
        all_positive_max_err_share_of_group=float(err[pos].max()),
        zero_cells_weight_share=float(achieved[zero].sum() / Wtot),
        kl_nats_per_unit_weight=kl,
        effective_draws_weighted_mean=float((W * eff).sum() / Wtot),
        share_records_below4_effective_draws_weighted=float(W[eff < 4].sum() / Wtot),
        max_abs_lambda=float(np.abs(lam_full).max()),
        seconds=time.time() - t_start)
    CT.log(json.dumps(rep, indent=1, default=float))
    np.savez(os.path.join(a.dir, "weights_resid_fallback.npz"), w=w, **{"lambda": lam_full},
             cell=np.arange(C))
    with open(os.path.join(a.dir, "report_resid_fallback.json"), "w") as f:
        json.dump(rep, f, indent=1, default=float)
    # E reads weights_resid.npz; fill it with these weights unless a
    # converged B result is already there (c_post.py overwrites it only
    # when B's solver converges).
    wr_path = os.path.join(a.dir, "weights_resid.npz")
    if not os.path.exists(wr_path):
        np.savez(wr_path, w=w, **{"lambda": lam_full}, cell=np.arange(C),
                 source=np.array("c_resid.py fallback"))


if __name__ == "__main__":
    main()
