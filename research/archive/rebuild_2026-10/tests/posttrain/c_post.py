"""Test 3, piece C: after c_fit.py, the residual calibration of the tilted
draws (B's calibrate()), the comparison of the fitted multipliers with B's,
and the DONE marker.

    sbatch c_gpu.sbatch c_post.py            # reads and updates $P/c/report.json

Comparison with B. Raw multipliers are not comparable cell by cell: in both
methods the same number added to every bin of one group's cells for a
variable, or a bin's multipliers shifted up in one family and down in
another, changes nothing, so each solver's raw values along those
directions are arbitrary. What is comparable is the tilt each record gets
on each bin of each variable (the sum over its 11 groups), centred over the
record's possible bins. Reported: weighted root-mean-square of each
method's centred tilt, of their difference, and their correlation, overall
and by variable.
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
import c_arm                  # noqa: E402

P = "/nfs/roberts/scratch/pi_nrs36/jar335/posttrain"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", default=os.path.join(P, "c"))
    ap.add_argument("--b-weights", default=os.path.join(P, "b", "weights_ref.npz"))
    ap.add_argument("--b-report", default=os.path.join(P, "b", "report_ref.json"))
    ap.add_argument("--skip-calibration", action="store_true")
    ap.add_argument("--no-done", action="store_true")
    ap.add_argument("--a-module", default="a_common", help="ignored")
    ap.add_argument("--dataset", default=None)
    ap.add_argument("--checkpoint", default=None)
    ap.add_argument("--arm", default="scf")
    ap.add_argument("--a-dir", default=None, help="ignored")
    a = ap.parse_args()
    arm = c_arm.make_arm(a.arm, a.dataset, a.checkpoint)
    A = arm.A
    dev = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    with open(os.path.join(a.dir, "report.json")) as f:
        report = json.load(f)
    recs = arm.recs
    targets = arm.cells
    C = len(targets)
    T = targets["target"].to_numpy(np.float64)
    W = recs["weight"].to_numpy(np.float64)
    zl = np.load(os.path.join(a.dir, "lambda.npz"))
    lam_np = zl["lambda"]
    zd = np.load(os.path.join(a.dir, "draws.npz"))
    tokens = zd["tokens"].astype(np.int64)
    N, K, _ = tokens.shape
    assert N == len(recs), "draws do not cover all records"

    # ---- KL of the tilted model by income group and wave (step-wise estimate)
    klr = zd["kl_stepwise"].astype(np.float64).mean(axis=1)          # [N]
    by = {}
    gcol = arm.group_cols.get("inc")
    keys = ["year"] + ([gcol] if gcol else [])
    for key, ix in recs.groupby(keys).indices.items():
        key = key if isinstance(key, tuple) else (key,)
        ww = W[ix]
        lab = f"{key[0]}" + (f" income group {key[1]}" if gcol else "")
        by[lab] = dict(
            records=int(len(ix)), kl_weighted_mean=float((ww * klr[ix]).sum() / ww.sum()),
            kl_median_records=float(np.median(klr[ix])))
    report["kl_by_wave_income_group"] = by
    CT.log(f"KL by wave and income group: {json.dumps(by)}")

    # ---- residual calibration
    if not a.skip_calibration:
        import b_calibrate as B
        t0 = time.time()
        w, lamR, repR = B.calibrate(tokens, recs, targets, contrib_fn=arm.contrib_index)
        status = pd.Series(repR.get("cell_status", []))
        repR = {k: v for k, v in repR.items() if k != "cell_status"}
        repR["status_counts"] = status.value_counts().to_dict()
        report["residual_calibration"] = repR
        report["residual_calibration_seconds"] = time.time() - t0
        report["residual_calibration_b_code_mtime"] = time.ctime(os.path.getmtime(B.__file__))
        # weights_resid.npz is what E reads: B's weights only if B's solver
        # converged; otherwise they are kept under another name and C's
        # fallback weights (c_resid.py) stay in weights_resid.npz.
        conv = bool(repR.get("converged", True))
        fn = "weights_resid.npz" if conv else "weights_resid_B_unconverged.npz"
        report["residual_calibration_file"] = fn
        np.savez(os.path.join(a.dir, fn), w=w, **{"lambda": lamR}, cell=np.arange(C))
        CT.log(f"residual calibration: KL {repR.get('kl_nats_per_unit_weight')}, "
               f"effective draws {repR.get('effective_draws_weighted_mean')}, "
               f"statuses {repR['status_counts']}")

    # ---- comparison with B's multipliers
    if os.path.exists(a.b_weights):
        zb = np.load(a.b_weights)
        lamB_np = np.nan_to_num(zb["lambda"].astype(np.float64))
        tok2bin = arm.tok2bin
        geom = CT.Geometry(arm.blk, arm.VB, arm.vb_start, arm.nbins, tok2bin, T, W, dev)
        al_np, ag_np, _, _ = A.allowed_table(recs, arm.fill,
                                             arm.ref_kwargs.get("decode_year"))
        allowed = torch.as_tensor(al_np, device=dev)
        wave_i = torch.as_tensor(ag_np, device=dev)
        lamC = torch.as_tensor(lam_np, device=dev)
        lamB = torch.as_tensor(lamB_np, device=dev)
        by_var = {}
        tot = dict(c2=0.0, b2=0.0, d2=0.0, x=0.0)
        for v in range(len(arm.var_names)):
            nb = geom.nbins[v]
            pw = torch.zeros(allowed.shape[0], nb, dtype=torch.bool, device=dev)
            for wv in range(allowed.shape[0]):
                toks = allowed[wv, v, :len(tok2bin[v])].nonzero().squeeze(1)
                pw[wv, geom.tok2bin[v][toks]] = True
            ok = (pw[wave_i] & ~geom.forbidden(v)).double()
            n = ok.sum(1, keepdim=True)

            def centred(lam):
                s = geom.tilt(lam, v)
                return (s - (s * ok).sum(1, keepdim=True) / n) * ok
            cc, cb = centred(lamC), centred(lamB)
            ww = geom.W
            c2 = float((ww * (cc ** 2).sum(1) / n[:, 0]).sum())
            b2 = float((ww * (cb ** 2).sum(1) / n[:, 0]).sum())
            d2 = float((ww * ((cc - cb) ** 2).sum(1) / n[:, 0]).sum())
            x = float((ww * (cc * cb).sum(1) / n[:, 0]).sum())
            by_var[arm.var_names[v]] = dict(rms_C=c2 ** .5, rms_B=b2 ** .5, rms_gap=d2 ** .5,
                                       corr=x / (c2 * b2) ** .5 if c2 * b2 > 0 else None)
            for k_, val in (("c2", c2), ("b2", b2), ("d2", d2), ("x", x)):
                tot[k_] += val
        pos = T > 0
        both = pos & (lamB_np != 0)
        report["lambda_vs_B"] = dict(
            what="tilt each record gets on each bin of each variable (sum over its 11 groups), "
                 "centred over the record's possible bins; population-weighted root mean square "
                 "over records, averaged over bins and variables",
            overall=dict(rms_C=(tot["c2"] / len(arm.var_names)) ** .5, rms_B=(tot["b2"] / len(arm.var_names)) ** .5,
                         rms_gap=(tot["d2"] / len(arm.var_names)) ** .5,
                         corr=tot["x"] / (tot["c2"] * tot["b2"]) ** .5),
            by_variable=by_var,
            raw_multiplier_corr_positive_cells_where_B_nonzero=float(
                np.corrcoef(lam_np[both], lamB_np[both])[0, 1]) if both.sum() > 2 else None,
            b_weights_file=a.b_weights,
            b_weights_mtime=time.ctime(os.path.getmtime(a.b_weights)))
        CT.log(f"lambda vs B: {report['lambda_vs_B']['overall']}")
        if os.path.exists(a.b_report):
            with open(a.b_report) as f:
                rb = json.load(f)
            report["B_reference_reweighting"] = {k: rb.get(k) for k in (
                "kl_nats_per_unit_weight", "effective_draws_weighted_mean",
                "share_records_below4_effective_draws_weighted")}
    else:
        report["lambda_vs_B"] = f"not computed: {a.b_weights} not found at {time.ctime()}"

    with open(os.path.join(a.dir, "report.json"), "w") as f:
        json.dump(report, f, indent=1, default=lambda x: x.item() if hasattr(x, "item") else str(x))
    if not a.no_done:
        with open(os.path.join(a.dir, "DONE"), "w") as f:
            f.write(time.strftime("%Y-%m-%d %H:%M:%S") + f" job {os.environ.get('SLURM_JOB_ID')}\n")
    CT.log("done")


if __name__ == "__main__":
    main()
