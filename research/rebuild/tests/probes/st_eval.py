"""Probe ST: targeted and untargeted wealth statistics for the SCF 2016
records, truth against each comparator, on the training and test splits.
Statistics and error summaries are probe Q's (q_eval.stats, compare),
computed on the 2016 rows only. Aggregates only; writes $ST/eval/.

Comparators: model runs in $ST/runs/<name> (draws before and after the
tilt); the 2013 records as they are and reweighted by st_calib.py (their
2013 ranks decoded with the 2016 quantile functions).

    python st_eval.py <variant or base> <run names ...>
"""
import json
import sys
from pathlib import Path

import numpy as np

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import st_common as SC         # noqa: E402
import q_eval as QE            # noqa: E402  (sets up probe Q's module on full_v2)

A = QE.A
variant = None if sys.argv[1] == "base" else sys.argv[1]
names = sys.argv[2:]
OUT = SC.ST / "eval"
OUT.mkdir(exist_ok=True)
NI = QE.NI


L = A.layout()


def part16(x):
    """2016 part of q_eval's targeted_bin_share (each family-variable chunk is
    2013 blocks then 2016 blocks)."""
    out, p = [], 0
    for f in ("inc", "age", "mar", "dep"):
        ng = len(L.groups[f])
        for k in range(24):
            n = 2 * ng * int(L.nbins[k])
            out.append(x[p + n // 2:p + n])
            p += n
    assert p == len(x)
    return np.concatenate(out)


def cmp(s, t):
    """q_eval.compare, with the targeted-cell error over the 2016 cells only."""
    e = QE.compare(s, t)
    e["targeted_mean_abs_pp"] = float(100 * np.abs(part16(s["targeted_bin_share"])
                                                   - part16(t["targeted_bin_share"])).mean())
    return e


def halves(fn, r):
    hh = r["year"].to_numpy() * 10**9 + r["hh_id"].to_numpy()
    u = np.unique(hh)
    return np.isin(hh, np.random.default_rng(3).choice(u, len(u) // 2, replace=False))


def half_err(sa, sb):
    return {k: (np.array(v) / 2).tolist() if isinstance(v, list) else v / 2
            for k, v in cmp(sa, sb).items()}


res = {"variant": variant}
truth = {}
recs = {}
for sp in ("train", "test"):
    R = SC.records(sp, variant)
    m16 = R["year"].to_numpy() == 2016
    r = R[m16].reset_index(drop=True)
    recs[sp] = (R, m16, r)
    tt = A.true_tokens(r)[:, None, :]
    Dt = A.decode_dollars(tt, r, seed=5)
    truth[sp] = QE.stats(Dt, tt, r, 1)
    h = halves(None, r)
    res[f"yardstick_truth_halves_{sp}"] = half_err(QE.stats(Dt, tt, r, 1, h),
                                                   QE.stats(Dt, tt, r, 1, ~h))
    res[f"truth_levels_{sp}"] = QE.levels(truth[sp])
    res[f"truth_n_{sp}"] = dict(rows=int(len(r)), households=int(r["hh_id"].nunique()))
    print(sp, "truth done", flush=True)

# 2013 records, as they are and reweighted
R, m16, _ = recs["train"]
r13 = SC.as_2016(R[~m16].reset_index(drop=True))
t13 = A.true_tokens(r13)[:, None, :]
# A few 2013 tokens have no 2016 quantile function (a rank band the 2016
# wave does not have for that variable); move each to the nearest token the
# 2016 function has, and record how many.
al, agi, _, _ = A.allowed_table(r13, None, None)
ok = al[agi[:, None], np.arange(24)[None, :], t13[:, 0, :]]
moved = {}
for i, k in zip(*np.nonzero(~ok)):
    allowed = np.flatnonzero(al[agi[i], k])
    t = t13[i, 0, k]
    t13[i, 0, k] = allowed[np.argmin(np.abs(allowed - t))]
    moved[A.WEALTH[k]] = moved.get(A.WEALTH[k], 0) + 1
res["decode_2013_moved_tokens"] = moved
print("2013 tokens moved to the nearest 2016 token:", moved, flush=True)
try:
    D13 = A.decode_dollars(t13, r13, seed=7)
    res["decode_2013_note"] = "2013 tokens decoded with the 2016 quantile functions"
except ValueError as e:
    print("decode 2013 with 2016 functions failed:", e, flush=True)
    D13 = None
if D13 is not None:
    cal = sorted((SC.ST / "calib").glob(f"{variant or 'base'}_rho*.npz"))
    sets = {}
    if cal:
        z = np.load(cal[0])
        sets["scf2013_as_is"] = z["base_weight"]
    for fn in cal:
        z = np.load(fn)
        sets["scf2013_reweighted_" + fn.stem.split("_")[-1]] = z["weight"]
        res["calib_" + fn.stem] = json.load(open(fn.with_suffix(".json")))
    for nm, w in sets.items():
        rr = r13.copy()
        rr["weight"] = w
        s = QE.stats(D13, t13, rr, 1)
        res[nm] = {f"{sp}_after": cmp(s, truth[sp]) for sp in ("train", "test")}
        res[nm]["levels"] = QE.levels(s)
        print(nm, json.dumps({k: res[nm]["train_after"][k] for k in
                              ("targeted_mean_abs_pp", "hold_inc_age_pp", "nw_inc_age_pct",
                               "pair_joint_pp", "top1_share_pp", "corr_abs_diff")}), flush=True)

for nm in names:
    d = SC.ST / "runs" / nm
    if not (d / "DONE").exists():
        print("missing run", nm, flush=True)
        continue
    z = np.load(d / "draws.npz")
    rep = json.load(open(d / "report.json"))
    out = dict(completion_score=rep.get("completion_score"), ckpt_epoch=rep.get("ckpt_epoch"))
    K = int(z["K"])
    for sp in ("train", "test"):
        Rf, m, r = recs[sp]
        assert len(z[f"rec_{sp}"]) == len(Rf)
        w = r["weight"].to_numpy(float)
        pi = w / w.sum()
        klr = z[f"{sp}_kl_record"].astype(float)[m]
        out[f"kl_{sp}"] = float((pi * klr).sum())
        gi = r["g_inc"].to_numpy()
        out[f"kl_by_inc_{sp}"] = [float((w[gi == g] * klr[gi == g]).sum() / w[gi == g].sum())
                                  for g in range(NI)]
        lr = z[f"{sp}_logratio"].astype(np.float64)[m]
        # pooled over all records: near 0 when a few records carry large tilts
        out[f"ess_share_{sp}"] = float(1.0 / (pi * np.exp(np.minimum(lr, 700)).mean(1)).sum())
        # record by record (8 draws each), averaged with population weights
        out[f"ess_share_record_{sp}"] = float((pi / np.exp(np.minimum(lr, 700)).mean(1)).sum())
        out[f"ess_share_record_by_inc_{sp}"] = [
            float((w[gi == g] / np.exp(np.minimum(lr[gi == g], 700)).mean(1)).sum() / w[gi == g].sum())
            for g in range(NI)]
        for when in ("before", "after"):
            tk = z[f"{sp}_{when}"].astype(np.int64)[m]
            D = A.decode_dollars(tk, r, seed=6)
            s = QE.stats(D, tk, r, K)
            out[f"{sp}_{when}"] = cmp(s, truth[sp])
            out[f"{sp}_{when}_levels"] = QE.levels(s)
    key = nm[:-len(variant) - 1] if variant and nm.endswith("_" + variant) else nm
    res[key] = out
    print(nm, json.dumps({f"{k}_{w}": out[f"train_{w}"][k] for w in ("before", "after")
                          for k in ("targeted_mean_abs_pp", "hold_inc_age_pp", "nw_inc_age_pct")}),
          flush=True)

fn = OUT / f"eval_{variant or 'base'}.json"
json.dump(res, open(fn, "w"), indent=1, default=float)
print("wrote", fn, flush=True)
