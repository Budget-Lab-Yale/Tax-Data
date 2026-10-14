"""Test 3, piece C: fit the probability tilt on the real targets, draw from
the tilted model, and report.

Steps (design in C_REPORT.md):
  1. fitting pass: K_fit draws per record; variable by variable in the fill
     order, lambda for the variable is solved so the expected cells (from
     the step probabilities) hit the targets exactly, then the variable is
     drawn from the tilted step with fixed random numbers;
  2. fresh pass: K = 32 new draws from the tilted model with the fitted
     lambda (different random numbers): the delivered draws;
  3. checks and report: expected and realized target error on both passes,
     KL from the reference, float32 log-probabilities cross-checked against
     A's ref_logprob, residual calibration with B's calibrate(), and a
     comparison of lambda with B's.

Outputs (in --outdir, default $P/c): lambda.npz, draws.npz, report.json,
and weights_resid.npz unless --no-calibrate. c_post.py then runs (or reruns)
the residual calibration and the comparison with B and writes DONE.

    sbatch c_gpu.sbatch c_fit.py                    # full run
    sbatch c_gpu.sbatch c_fit.py --dev --max-records 3000 --k 8 --k-fit 8
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
import c_tilt as CT           # noqa: E402

P = "/nfs/roberts/scratch/pi_nrs36/jar335/posttrain"
A = None                      # A's module (a_common), set from the arm in main()


def parse():
    ap = argparse.ArgumentParser()
    ap.add_argument("--k", type=int, default=32, help="draws per record, delivered set")
    ap.add_argument("--k-fit", type=int, default=32, help="draws per record, fitting pass")
    ap.add_argument("--seed-fit", type=int, default=3101)
    ap.add_argument("--seed", type=int, default=3202, help="seed of the delivered draws")
    ap.add_argument("--max-records", type=int, default=None,
                    help="development: a random subset of records, targets recomputed on it")
    ap.add_argument("--dev", action="store_true", help="write to $P/dev/c, no DONE marker")
    ap.add_argument("--outdir", default=None)
    ap.add_argument("--batch-size", type=int, default=16384)
    ap.add_argument("--tol", type=float, default=1e-10)
    ap.add_argument("--max-newton", type=int, default=60)
    ap.add_argument("--no-calibrate", action="store_true")
    ap.add_argument("--b-weights", default=os.path.join(P, "b", "weights_ref.npz"))
    ap.add_argument("--a-module", default="a_common", help="ignored (kept so queued commands parse)")
    ap.add_argument("--dataset", default=None,
                    help="full, full_v2, or pilot_expB (a_full on the current model)")
    ap.add_argument("--checkpoint", default=None, help="passed to A")
    ap.add_argument("--arm", default="scf", help="scf (pilot), P_core or P_full")
    ap.add_argument("--a-dir", default=None, help="ignored: A's folder follows from --dataset")
    ap.add_argument("--ridge", type=float, default=0.0,
                    help="ridge on the multipliers for targets that cannot all be met "
                         "(0 = exact; see c_tilt.solve_step)")
    ap.add_argument("--auto-ridge", type=float, default=1e-3,
                    help="if a variable's exact solve does not converge, refit it with "
                         "this ridge (0 = never)")
    return ap.parse_args()


def family_errors(err_share, tg, fam_of_cell, pos):
    """Mean and max of |error| / group total over positive cells, by family."""
    out = {}
    for f in pd.unique(fam_of_cell):
        m = (fam_of_cell == f) & pos
        out[f] = dict(mean=float(err_share[m].mean()), max=float(err_share[m].max()))
    return out


def main():
    a = parse()
    t_start = time.time()
    out_dir = a.outdir or (os.path.join(P, "dev", "c") if a.dev else os.path.join(P, "c"))
    os.makedirs(out_dir, exist_ok=True)
    dev = torch.device("cuda")
    torch.backends.cuda.matmul.allow_tf32 = False
    torch.backends.cudnn.allow_tf32 = False
    CT.log(f"GPU {torch.cuda.get_device_name(0)}; out {out_dir}")

    import c_arm
    arm = c_arm.make_arm(a.arm, a.dataset, a.checkpoint)
    global A
    A = arm.A
    recs_all = arm.recs
    VN = arm.var_names                     # targeted (= drawn) variables, tokens order
    V = len(VN)
    if a.max_records:
        if not arm.has_truth:
            raise ValueError("--max-records needs an arm whose targets come from the records")
        rng = np.random.default_rng(7)
        sub = np.sort(rng.choice(len(recs_all), a.max_records, replace=False))
        recs = recs_all.iloc[sub].reset_index(drop=True)
        targets = A.targets(recs)
    else:
        sub = np.arange(len(recs_all))
        recs = recs_all
        targets = arm.cells
    C = len(targets)
    assert (targets["cell"].to_numpy() == np.arange(C)).all(), "targets not in cell order"
    N = len(recs)
    T = targets["target"].to_numpy(np.float64)
    n_nan = int(np.isnan(T).sum())
    W = recs["weight"].to_numpy(np.float64)
    CT.log(f"arm {a.arm}: {N:,} records, {C:,} cells, {int((T == 0).sum()):,} zero cells, "
           f"{n_nan:,} cells without a target (left free)")

    model, ctx = A.load_reference("cuda", recs=recs_all, **arm.ref_kwargs)
    if hasattr(ctx, "fill") and list(ctx.fill) != list(VN):
        raise ValueError(f"drawn variables {ctx.fill} differ from targeted {VN}")
    if any(k is None or int(k) < 0 for k in ctx.step_var):
        CT.log("some drawn variables have no target cells: drawn untilted")
    vocab = [len(t) for t in arm.tok2bin]
    blk = arm.blk if not a.max_records else recs[A.BLOCK_COLS].to_numpy(np.int64)
    geom = CT.Geometry(blk, arm.VB, arm.vb_start, arm.nbins, arm.tok2bin, T, W, dev)
    CT.log(f"fill order: {ctx.order_names}; records with no group, by family: "
           f"{geom.n_missing_by_family}; unobserved and not drawn: "
           f"{getattr(ctx, 'unfilled_hidden', None)}")

    # allowed-code group of each record (wave for the SCF, source-year in general)
    agroup_all = getattr(ctx, "agroup", ctx.wave_i)
    agroup = agroup_all[torch.as_tensor(sub, device=dev)]
    # SCF arm: the true token of every record is allowed and lies in a
    # non-zero cell for every family (so every step has a possible bin).
    if arm.has_truth:
        true_tok = torch.as_tensor(A.true_tokens(recs), device=dev)
        for v in range(V):
            al = ctx.allowed[agroup, v, true_tok[:, v]]
            if not bool(al.all()):
                raise RuntimeError(f"{VN[v]}: {int((~al).sum())} true tokens not allowed")
            tb = geom.tok2bin[v][true_tok[:, v]]
            fb = geom.forbidden(v).gather(1, tb[:, None]).squeeze(1)
            if bool(fb.any()):
                raise RuntimeError(f"{VN[v]}: {int(fb.sum())} true bins forbidden")

    # How far the families' targets disagree: for families that cover every
    # record, each implies a total weight for (variable, bin); the spread
    # across them, as a share of all weight, is a floor no tilt can beat.
    inc = None
    try:
        tt = targets.assign(t=T)
        fam_tot = tt.groupby(["family", "variable"])["t"].sum()
        full_fams = [f for f in tt["family"].unique()
                     if np.allclose(fam_tot.loc[f].to_numpy(), W.sum(), rtol=1e-6)]
        if len(full_fams) > 1:
            vb = tt[tt["family"].isin(full_fams)].groupby(["variable", "bin", "family"])["t"].sum()
            vb = vb.unstack("family")
            spread = (vb.max(axis=1) - vb.min(axis=1)) / W.sum()
            top = spread.sort_values(ascending=False).head(8)
            inc = dict(families_covering_all_records=full_fams,
                       max_spread_share_of_total_weight=float(spread.max()),
                       mean_spread_share_of_total_weight=float(spread.mean()),
                       largest=[dict(variable=k[0], bin=int(k[1]), spread=float(x))
                                for k, x in top.items()])
            CT.log(f"target consistency across families: {json.dumps(inc)[:800]}")
    except Exception as e:                    # report, do not hide
        inc = f"not computed: {e!r}"

    solve_opts = dict(tol=a.tol, max_newton=a.max_newton, ridge=a.ridge,
                      auto_ridge=a.auto_ridge)
    report = dict(settings=vars(a), arm=a.arm, n_records=N, n_cells=int(C),
                  n_zero_cells=int((T == 0).sum()), n_cells_without_target=n_nan,
                  records_without_group_by_family=geom.n_missing_by_family,
                  unobserved_not_drawn=getattr(ctx, "unfilled_hidden", None),
                  fill_order=ctx.order_names, target_consistency=inc,
                  gpu=torch.cuda.get_device_name(0), slurm_job=os.environ.get("SLURM_JOB_ID"))

    # ---- 1. fitting pass
    CT.log(f"fitting pass: K_fit = {a.k_fit}, seed {a.seed_fit}")
    lam = torch.zeros(C, dtype=torch.float64, device=dev)
    st = CT.RefStepper(A, model, ctx, a.k_fit, vocab, rec_subset=sub, batch_size=a.batch_size)
    t0 = time.time()
    fit = CT.run_pass(st, geom, lam, a.seed_fit, fit=True, solve_opts=solve_opts)
    report["fit_seconds"] = time.time() - t0
    del st
    steps = {}
    for v in ctx.step_var:
        inf = fit["infos"][v]
        steps[VN[v]] = {k: inf.get(k) for k in ("n_free", "newton", "rejected_steps", "stop", "n_at_cap",
                                                  "max_rel_err", "n_not_reached_1e-6",
                                                  "ridge_used", "exact_solve_stop",
                                                  "exact_solve_max_rel_err",
                                                  "records_zero_rule_relaxed",
                                                  "max_abs_lambda", "mean_kl_step", "seconds",
                                                  "seconds_network")}
    report["fit_steps"] = steps
    fam_of_cell = targets["family"].to_numpy()
    pos = T > 0

    def errs(totals):
        s, e = CT.target_error(geom, totals)
        e_np = e.cpu().numpy()
        s["by_family"] = family_errors(e_np, None, fam_of_cell, pos)
        s["cells_err_over_1pct"] = int((e_np[pos] > 0.01).sum())
        s["cells_err_over_10pct"] = int((e_np[pos] > 0.10).sum())
        return s

    report["fit_pass"] = dict(expected=errs(fit["expected"]), realized=errs(fit["realized"]))
    kls_fit = CT.kl_summary(geom, a.k_fit, fit["logp_new_var"].sum(1), fit["logp_ref_var"].sum(1),
                            fit["kl_step"].sum(1))
    report["fit_pass"]["kl"] = {k: v for k, v in kls_fit.items() if not torch.is_tensor(v)}
    CT.log(f"fit pass: {json.dumps(report['fit_pass'], default=float)[:2000]}")
    lam_np = lam.cpu().numpy()
    np.savez(os.path.join(out_dir, "lambda.npz"), **{"lambda": lam_np},
             cell=np.arange(C), zero_cell=(T == 0), no_target=np.isnan(T),
             k_fit=np.int64(a.k_fit),
             seed_fit=np.int64(a.seed_fit), records_subset=sub)
    del fit
    torch.cuda.empty_cache()

    # ---- 2. fresh pass with the fitted lambda
    CT.log(f"fresh pass: K = {a.k}, seed {a.seed}")
    st = CT.RefStepper(A, model, ctx, a.k, vocab, rec_subset=sub, batch_size=a.batch_size)
    t0 = time.time()
    fr = CT.run_pass(st, geom, lam, a.seed, fit=False)
    report["fresh_seconds"] = time.time() - t0
    del st
    K = a.k
    tokens = fr["tokens"].view(N, K, V).cpu().numpy().astype(np.int16)
    lp_new_var = fr["logp_new_var"].view(N, K, V)
    lp_ref_var = fr["logp_ref_var"].view(N, K, V)
    kls = CT.kl_summary(geom, K, fr["logp_new_var"].sum(1), fr["logp_ref_var"].sum(1),
                        fr["kl_step"].sum(1))
    # realized cells through A's contribution builder (the shared mapping)
    idx = arm.contrib_index(tokens, recs)
    wr = np.repeat(np.repeat(W, K) / K, idx.shape[1])
    fl = idx.reshape(-1)
    real_A = np.bincount(fl[fl >= 0], weights=wr[fl >= 0], minlength=C) / W.sum()
    real_mine = fr["realized"].cpu().numpy()
    report["check_realized_vs_A_builder_max_abs"] = float(np.abs(real_A - real_mine).max())
    report["fresh_pass"] = dict(expected=errs(fr["expected"]),
                                realized=errs(torch.as_tensor(real_A, device=dev)))
    report["fresh_pass"]["kl"] = {k: v for k, v in kls.items() if not torch.is_tensor(v)}
    # per-record KL distribution
    kr = kls["kl_stepwise_per_record"].cpu().numpy()
    qs = [0.1, 0.5, 0.9, 0.99]
    report["fresh_pass"]["kl_stepwise_per_record_quantiles"] = dict(
        zip([str(q) for q in qs], np.quantile(kr, qs).tolist()))
    kv = (fr["kl_step"].view(N, K, V).mean(1) * geom.W[:, None]).sum(0).cpu().numpy()
    report["fresh_pass"]["kl_stepwise_by_variable"] = dict(zip(VN, kv.tolist()))
    CT.log(f"fresh pass: {json.dumps(report['fresh_pass'], default=float)[:2500]}")

    # cross-check float32 log-probabilities against A's ref_logprob on a
    # sample (only when every drawn variable is targeted: ref_logprob takes
    # the targeted variables' tokens)
    if not fr["untilted"]:
        rng = np.random.default_rng(1)
        pick = rng.choice(N * K, min(4000, N * K), replace=False)
        ri = sub[pick // K]
        tk = tokens.reshape(N * K, V)[pick].astype(np.int64)
        lp_A = A.ref_logprob(model, ctx, ri, tk)
        pk = torch.as_tensor(pick, device=dev)
        mine = lp_ref_var.reshape(N * K, V)[pk].cpu().numpy()
        report["check_logp_ref_vs_A_max_abs"] = float(np.abs(lp_A - mine).max())
        pos_of = {int(r): i for i, r in enumerate(sub)}

        def bias_fn(k, rib, logits):
            # the tilt of variable k for records rib (indices into all
            # records), as a token-level addition, with zero cells at -inf
            pos_ = torch.as_tensor([pos_of[int(r)] for r in rib.cpu().numpy()], device=dev)
            s_ = geom.tilt(lam, k)[pos_]
            forb = geom.forbidden(k)
            if k in geom.relaxed_mask:
                forb = forb & ~geom.relaxed_mask[k][:, None]
            s_ = s_.masked_fill(forb[pos_], float("-inf"))
            add = torch.zeros_like(logits)
            add[:, :vocab[k]] = s_.gather(1, geom.tok2bin[k][None, :].expand(len(rib), -1)).float()
            return logits + add
        lp_A_new = A.ref_logprob(model, ctx, ri, tk, bias_fn=bias_fn)
        mine_new = lp_new_var.reshape(N * K, V)[pk].cpu().numpy()
        report["check_logp_new_vs_A_biasfn_max_abs"] = float(np.abs(lp_A_new - mine_new).max())
        CT.log(f"checks: realized vs A builder {report['check_realized_vs_A_builder_max_abs']:.2e}; "
               f"logp_ref vs A {report['check_logp_ref_vs_A_max_abs']:.2e}; "
               f"logp_new vs A bias_fn {report['check_logp_new_vs_A_biasfn_max_abs']:.2e}")

    lp_new = lp_new_var.sum(2).cpu().numpy()
    lp_ref = lp_ref_var.sum(2).cpu().numpy()
    np.savez(os.path.join(out_dir, "draws.npz"), tokens=tokens,
             logp_new=lp_new.astype(np.float32), logp_ref=lp_ref.astype(np.float32),
             logp_new_var=lp_new_var.cpu().numpy().astype(np.float32),
             logp_ref_var=lp_ref_var.cpu().numpy().astype(np.float32),
             kl_stepwise=fr["kl_step"].view(N, K, V).sum(2).cpu().numpy().astype(np.float32),
             seed=np.int64(a.seed), K=np.int64(K), rec=sub, wealth=np.array(VN),
             **{f"untilted_step{k}": t.view(N, K).cpu().numpy() for k, t in fr["untilted"].items()})
    CT.log("wrote draws.npz")
    del fr
    torch.cuda.empty_cache()

    # ---- 3. comparison with B's multipliers: c_post.py

    # ---- 4. residual calibration with B's calibrate()
    if not a.no_calibrate:
        import b_calibrate as B
        t0 = time.time()
        w, lamR, repR = B.calibrate(tokens.astype(np.int64), recs, targets,
                                    contrib_fn=arm.contrib_index)
        repR = {k: v for k, v in repR.items() if k != "cell_status"} | \
            {"status_counts": pd.Series(repR.get("cell_status", [])).value_counts().to_dict()}
        report["residual_calibration"] = repR
        report["residual_calibration_seconds"] = time.time() - t0
        np.savez(os.path.join(out_dir, "weights_resid.npz"), w=w, **{"lambda": lamR},
                 cell=np.arange(C))
        CT.log(f"residual calibration: {json.dumps(repR, default=str)[:2500]}")

    report["seconds_total"] = time.time() - t_start
    with open(os.path.join(out_dir, "report.json"), "w") as f:
        json.dump(report, f, indent=1, default=lambda x: x.item() if hasattr(x, "item") else str(x))
    # FIT_DONE marks this script's outputs; the DONE marker is written by
    # c_post.py after the residual calibration.
    with open(os.path.join(out_dir, "FIT_DONE"), "w") as f:
        f.write(time.strftime("%Y-%m-%d %H:%M:%S") + f" job {os.environ.get('SLURM_JOB_ID')}\n")
    CT.log("done")


if __name__ == "__main__":
    main()
