"""Probe ST, the simplest alternative to the model: the SCF 2013 records
themselves, reweighted by entropy calibration to the 2016 tables (the same
target cells the tilt uses: each wealth variable's coarse-bin distribution
within income group, age range, filing status and children under 17).

A 2013 record's wealth tokens are ranks within 2013; its income group is
by the 2013 thresholds. Both are placed in the 2016 cells unchanged, which
treats the 2013 ranks as 2016 ranks (dollars then come from the 2016
quantile functions, the same as the model's draws for 2016).

Entropy calibration: new weights p proportional to p0 * exp(sum of the
record's cell multipliers), p0 the 2013 weights; multipliers minimise
sum p0 exp(.) - lambda . target + rho/2 |lambda|^2 (targets as shares of
each cell's own 2016 group: income group, age range and so on). The ridge term rho makes the problem solvable when some
2016 cells cannot be reached by 2013 records (it leaves a miss of rho x
lambda in each cell).

    python st_calib.py [--variant top2] [--rho 1e-4]
Writes $ST/calib/<variant>_rho<rho>.npz (weights of the 2013 training
records, records order) and .json (size of the reweighting).
"""
import argparse
import json
import sys
from pathlib import Path

import numpy as np
import torch

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import st_common as SC         # noqa: E402

ap = argparse.ArgumentParser()
ap.add_argument("--variant", default=None)
ap.add_argument("--rho", type=float, default=1e-4)
ap.add_argument("--scale", default="group", choices=["group", "pop"],
                help="ridge on within-group shares (group) or on population shares (pop)")
ap.add_argument("--iters", type=int, default=3000)
a = ap.parse_args()
A = SC.setup()
L = A.layout()
torch.set_num_threads(4)


as_2016 = SC.as_2016


def main():
    R = SC.records("train", a.variant)
    T = SC.targets("train", a.variant)["target"].to_numpy(np.float64)
    y = R["year"].to_numpy()
    r13 = as_2016(R[y == 2013].reset_index(drop=True))
    pop16 = R["weight"].to_numpy(float)[y == 2016].sum()
    idx = A.contrib_index(A.true_tokens(r13), r13).astype(np.int64)    # [n, 96]
    cells = np.unique(idx)
    w0 = r13["weight"].to_numpy(float)
    p0 = torch.tensor(w0 / w0.sum(), dtype=torch.float64)
    # every 2016 cell (blocks of wave 1), reachable or not
    blk16 = L.blocks[L.blocks.wave_i == 1].block.to_numpy()
    allc = np.concatenate([np.arange(b * L.VB, b * L.VB + L.VB) for b in blk16])
    allc = allc[allc < len(T)]
    tau_all = T / pop16
    unreach = np.setdiff1d(allc[tau_all[allc] > 0], cells)
    print(f"2013 records {len(r13)}; 2016 cells touched by them {len(cells)}; "
          f"2016 cells with a positive target no 2013 record reaches {len(unreach)} "
          f"(population share summed over cells {tau_all[unreach].sum():.4f})", flush=True)
    cmap = -np.ones(len(T), np.int64)
    cmap[cells] = np.arange(len(cells))
    I = torch.tensor(cmap[idx])
    # Constraints in shares of each cell's own group (not of the whole
    # population), so the ridge allows the same miss in a small group (top
    # 0.1 percent) as in a large one: cell c's row is divided by its group's
    # population share G_c (block total of targets / 24 variables / 2016 pop).
    blk_tot = np.bincount(np.arange(len(T)) // L.VB, weights=T, minlength=L.n_cells // L.VB + 1)
    G = blk_tot[cells // L.VB] / 24.0 / pop16
    if (G <= 0).any():
        raise ValueError("a 2016 group with no population reached by 2013 records")
    if a.scale == "pop":
        G = np.ones_like(G)     # ridge on population shares (misses reported in population shares)
    Gt = torch.tensor(G)
    tau = torch.tensor(tau_all[cells])     # population shares; the ridge is scaled by G^2 instead
    lam = torch.zeros(len(cells), dtype=torch.float64, requires_grad=True)
    opt = torch.optim.LBFGS([lam], lr=1, max_iter=a.iters, history_size=50,
                            tolerance_grad=1e-12, tolerance_change=1e-16,
                            line_search_fn="strong_wolfe")

    def f():
        opt.zero_grad()
        # lam here is the multiplier on the log weight; the ridge on lam x G
        # leaves a miss of rho x G x lam in the cell's within-group share
        s = lam[I].sum(1).clamp(max=60.0)
        obj = (p0 * torch.exp(s)).sum() - (lam * tau).sum() + 0.5 * a.rho * ((Gt * lam) ** 2).sum()
        obj.backward()
        return obj
    for rnd in range(5):
        opt.step(f)
        with torch.no_grad():
            q = p0 * torch.exp(lam[I].sum(1))
            tot = torch.zeros(len(cells), dtype=torch.float64).index_add_(0, I.reshape(-1), q.repeat_interleave(I.shape[1]))
            miss = ((tot - tau) / Gt).abs()
            print(f"round {rnd}: total weight {q.sum():.6f}; mean abs miss in a cell "
                  f"{miss.mean():.2e} (share of the cell's group), max {miss.max():.2e}; "
                  f"max |lambda| {lam.abs().max():.2f}", flush=True)
    with torch.no_grad():
        q = (p0 * torch.exp(lam[I].sum(1))).numpy()
    p = q / q.sum()
    pz = p0.numpy()
    kl = float((p * np.log(np.maximum(p, 1e-300) / pz)).sum())
    ess_share = float(1.0 / (p * p / pz).sum())
    kish = lambda w: w.sum() ** 2 / (w ** 2).sum()                      # noqa: E731
    out = dict(variant=a.variant, rho=a.rho, scale=a.scale, n_records=len(r13), kl_nats=kl,
               ess_share_vs_base=ess_share, kish_ess_base=float(kish(w0)),
               kish_ess_new=float(kish(p)), total_weight_share=float(q.sum()),
               unreachable_cells=int(len(unreach)),
               unreachable_share=float(tau_all[unreach].sum()),
               max_weight_ratio=float((p / pz).max()),
               weight_ratio_quantiles=np.quantile(p / pz, [0.01, 0.1, 0.5, 0.9, 0.99]).tolist())
    print(json.dumps(out), flush=True)
    od = SC.ST / "calib"
    od.mkdir(exist_ok=True)
    tag = f"{a.variant or 'base'}_rho{'pop' if a.scale == 'pop' else ''}{a.rho:g}"
    np.savez(od / f"{tag}.npz", weight=p * pop16, base_weight=w0 / w0.sum() * pop16)
    json.dump(out, open(od / f"{tag}.json", "w"), indent=1)


if __name__ == "__main__":
    main()
