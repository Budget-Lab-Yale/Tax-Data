"""Probe ST (copy of probe Q's q_fit.py, changes marked ST): for one pretrained model, (1) score its completion of SCF wealth
(nats per record) on validation, training and test records; (2) draw the
24 wealth variables of the SCF training and test records from the model as
it is ("before"); (3) fit piece C's tilt to the training records' own
tables (income group, age range, filing status, children under 17); (4)
draw again from the tilted model ("after") on training and test records,
the test records tilted with the multipliers fitted on training records.

Method of the tilt: ../posttrain/c_tilt.py unchanged (C_REPORT.md).

    python q_fit.py --name best --ckpt <checkpoint> [--hide none|noincome|nothing]
Outputs in $Q/runs/<name>/: draws.npz, lambda.npz, report.json.
"""
import argparse
import json
import os
import sys
import time
from pathlib import Path

import numpy as np
import torch

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "posttrain"))
import st_common as SC         # noqa: E402  (ST)
import q_common as QC          # noqa: E402
import c_tilt as CT            # noqa: E402

ap = argparse.ArgumentParser()
ap.add_argument("--name", required=True)
ap.add_argument("--ckpt", required=True)
ap.add_argument("--variant", default=None, help="ST: e.g. top2 (st_common.harden)")
ap.add_argument("--hide", default="none", choices=list(QC.HIDE))
ap.add_argument("--k", type=int, default=8)
ap.add_argument("--k-fit", type=int, default=16)
ap.add_argument("--max-records", type=int, default=None, help="development only")
ap.add_argument("--batch-size", type=int, default=16384)
a = ap.parse_args()

t_start = time.time()
out = QC.Q / "runs" / a.name
out.mkdir(parents=True, exist_ok=True)
dev = torch.device("cuda")
torch.backends.cuda.matmul.allow_tf32 = False
torch.backends.cudnn.allow_tf32 = False
A = QC.setup(a.ckpt)
L = A.layout()
CT.log(f"GPU {torch.cuda.get_device_name(0)}; run {a.name}; ckpt {a.ckpt}; hide {a.hide}")

R = {s: SC.records(s, a.variant) for s in ("train", "test")}   # ST: no val; variant weights
T_tr = SC.targets("train", a.variant)
if a.max_records:
    rng = np.random.default_rng(7)
    for s in R:
        n = min(a.max_records, len(R[s]))
        R[s] = R[s].iloc[np.sort(rng.choice(len(R[s]), n, replace=False))].reset_index(drop=True)
    T_tr = A.targets(R["train"])
T = T_tr["target"].to_numpy(np.float64)
assert (T_tr["cell"].to_numpy() == np.arange(len(T))).all()
report = dict(args=vars(a), n_records={s: len(r) for s, r in R.items()}, n_cells=len(T),
              n_zero_cells=int((T == 0).sum()), gpu=torch.cuda.get_device_name(0),
              slurm_job=os.environ.get("SLURM_JOB_ID"))
ck = torch.load(a.ckpt, map_location="cpu", weights_only=False)
report["ckpt_epoch"] = ck.get("epoch")
report["ckpt_val_loss_all"] = ck.get("val_loss")
del ck

VN = list(A.WEALTH)
V = len(VN)
tok2bin = [L.wbins[v][1] for v in VN]
vocab = [len(t) for t in tok2bin]
solve_opts = dict(tol=1e-10, max_newton=60, ridge=0.0, auto_ridge=1e-3)


def context(split):
    model, ctx = A.load_reference("cuda", recs=R[split], split=split)
    hidden = QC.hide_inputs(ctx, a.hide)
    return model, ctx, hidden


def geometry(split, target):
    recs = R[split]
    blk = recs[A.BLOCK_COLS].to_numpy(np.int64)
    return CT.Geometry(blk, L.VB, list(L.vb_start), list(L.nbins), tok2bin, target,
                       recs["weight"].to_numpy(np.float64), dev)


# ---- 1. completion score of the true wealth (fixed order, float32)
scores = {}
for split in ("train", "test"):
    model, ctx, hidden = context(split)
    lp = A.ref_logprob(model, ctx, np.arange(len(R[split])), A.true_tokens(R[split]))
    s = -lp.astype(np.float64).sum(axis=1)
    w = R[split]["weight"].to_numpy(float)
    scores[split] = dict(nats_per_record=float(s.mean()),
                         nats_per_record_weighted=float((w * s).sum() / w.sum()))
    yr = R[split]["year"].to_numpy()
    for y in (2013, 2016):            # ST: by wave
        m = yr == y
        scores[split][f"weighted_{y}"] = float((w[m] * s[m]).sum() / w[m].sum())
    del model, ctx
    torch.cuda.empty_cache()
report["completion_score"] = scores
report["hidden_inputs"] = hidden
CT.log(f"scores {scores}")

saved = {}


def draw(split, lam, seed, K, fit=False, target=None):
    model, ctx, _ = context(split)
    tgt = target if target is not None else np.full(len(T), np.nan)
    geom = geometry(split, tgt)
    st = CT.RefStepper(A, model, ctx, K, vocab, batch_size=a.batch_size)
    t0 = time.time()
    res = CT.run_pass(st, geom, lam, seed, fit=fit, solve_opts=solve_opts)
    sec = time.time() - t0
    N = len(R[split])
    kls = CT.kl_summary(geom, K, res["logp_new_var"].sum(1), res["logp_ref_var"].sum(1),
                        res["kl_step"].sum(1))
    info = dict(seconds=sec, kl_stepwise=kls["kl_stepwise"], kl_sample=kls["kl_sample"],
                kl_stepwise_se=kls["kl_stepwise_se"])
    kr = kls["kl_stepwise_per_record"].cpu().numpy()
    info["kl_per_record_quantiles"] = dict(zip(["0.5", "0.9", "0.99"],
                                               np.quantile(kr, [0.5, 0.9, 0.99]).tolist()))
    if target is not None:
        e, _ = CT.target_error(geom, res["expected"])
        r_, _ = CT.target_error(geom, res["realized"])
        info["expected_error"] = e
        info["realized_error"] = r_
    if fit:
        info["fit_steps"] = {VN[v]: {k: res["infos"][v].get(k) for k in
                                     ("newton", "stop", "n_at_cap", "max_rel_err",
                                      "ridge_used", "max_abs_lambda", "mean_kl_step")}
                             for v in res["infos"]}
    tokens = res["tokens"].view(N, K, V).cpu().numpy().astype(np.int16)
    klrec = kr.astype(np.float32)
    info["_lr"] = (res["logp_new_var"].sum(1) - res["logp_ref_var"].sum(1)).view(N, K).cpu().numpy().astype(np.float32)
    del st, model, ctx, res
    torch.cuda.empty_cache()
    return tokens, klrec, info


lam = torch.zeros(len(T), dtype=torch.float64, device=dev)
# ---- 2. before: draws from the model as it is
for split, seed in (("train", 11), ("test", 21)):
    tk, _, info = draw(split, lam, seed, a.k)
    info.pop("_lr")
    saved[f"{split}_before"] = tk
    report[f"{split}_before"] = info
    CT.log(f"{split} before: {info}")

# ---- 3. fit the tilt on the training records
_, _, info = draw("train", lam, 12, a.k_fit, fit=True, target=T)
info.pop("_lr")
report["fit"] = info
np.savez(out / "lambda.npz", **{"lambda": lam.cpu().numpy()})
CT.log(f"fit: {json.dumps(info, default=float)[:3000]}")

# ---- 4. after: fresh draws from the tilted model; test records tilted with
# the training multipliers (training zero cells forbidden)
for split, seed in (("train", 13), ("test", 23)):
    tk, klr, info = draw(split, lam, seed, a.k, target=T)
    saved[f"{split}_logratio"] = info.pop("_lr")      # ST
    saved[f"{split}_after"] = tk
    saved[f"{split}_kl_record"] = klr
    report[f"{split}_after"] = info
    CT.log(f"{split} after: {json.dumps(info, default=float)[:1500]}")

np.savez(out / "draws.npz", wealth=np.array(VN), K=np.int64(a.k), **saved,
         **{f"rec_{s}": R[s]["rec"].to_numpy() for s in ("train", "test")})
report["seconds_total"] = time.time() - t_start
with open(out / "report.json", "w") as f:
    json.dump(report, f, indent=1, default=lambda x: x.item() if hasattr(x, "item") else str(x))
with open(out / "DONE", "w") as f:
    f.write(time.strftime("%Y-%m-%d %H:%M:%S") + f" job {os.environ.get('SLURM_JOB_ID')}\n")
CT.log("done")
