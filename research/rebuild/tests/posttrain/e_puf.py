"""Piece E, arm P (SPEC.md, main session 9 October 20:30): PUF training
records completed with wealth at 2015, by the reference model and by each
method, against the SCF 2013 and 2016 training records and their
interpolation to 2015. There is no true wealth for PUF records, so every
comparison is with the SCF's aggregates.

  python e_puf.py --auto            # everything ready and new
  python e_puf.py --scf             # SCF 2013 and 2016 statistics only
  python e_puf.py --label p_m1_core --draws ... [--weights ...] [--dollars ...]

Writes $P/e/puf/<label>/stats.json with the same statistics as
e_evaluate.frame_stats. Income groups are weighted ranks of the
seven-income total among PUF training records on the PUF side and among
SCF training units of each wave on the SCF side (same edges: 50, 90, 99,
99.9). PUF records with age unobserved form their own age group; the
income-by-age comparisons use only cells the SCF has.

Interpolation to 2015 (in e_compare.py): one third 2013, two thirds 2016;
shares, rates, ranks and correlations linearly, dollar levels in logs (as
test 2's c_evaluate_puf.py).
"""
import argparse
import glob
import importlib
import json
import os
import sys
import time

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import e_evaluate as E  # noqa: E402
from c_common import WEALTH, INCOMES, Dataset, wrank  # noqa: E402

P = E.P
T2 = E.T2
AP = os.path.join(P, "a_puf")
OUT = os.path.join(E.OUT, "puf")
log = E.log


class PufBase:
    """PUF training records in the record order of $P/a_puf/records.parquet
    (or expB order before that file exists)."""

    def __init__(self):
        ds = Dataset("expB", "train", os.path.join(T2, "data"))
        self.ds = ds
        self.tinfo = ds.tinfo
        rec = ds.records
        src = rec["source"].astype(str).to_numpy()
        puf = src == "PUF"
        apath = os.path.join(AP, "records.parquet")
        self.from_a = os.path.exists(apath)
        if self.from_a:
            A = pd.read_parquet(apath)
            self.A = A
            rows = A["row"].to_numpy().astype(np.int64)
            if not puf[rows].all():
                raise SystemExit("arm P records include non-PUF rows")
        else:
            self.A = None
            rows = np.flatnonzero(puf)
        self.rows = rows
        self.N = len(rows)
        r = rec.iloc[rows].reset_index(drop=True)
        self.year = r["year"].to_numpy().astype(int)
        self.hh = self.year.astype(np.int64) * 10**9 + r["hh_id"].astype(np.int64).to_numpy()
        self.W = r["weight"].to_numpy().astype(float)
        if np.isnan(self.W).any() or (self.W <= 0).any():
            raise SystemExit("PUF weights missing or not positive")
        tot_all = rec[INCOMES].to_numpy().astype(float).sum(axis=1)
        if np.isnan(tot_all[puf]).any():
            raise SystemExit(f"{int(np.isnan(tot_all[puf]).sum())} PUF training rows lack an income")
        idx = np.flatnonzero(puf)
        rk = np.full(len(rec), np.nan)
        rk[idx] = wrank(tot_all[idx], rec["weight"].to_numpy()[idx].astype(float))
        self.inc_rank = rk[rows]
        self.inc5 = np.searchsorted(np.array(E.INC5_EDGES), self.inc_rank, side="right").astype(np.int8)
        if self.A is not None and "g_inc" in self.A.columns:
            agree = float((self.A["g_inc"].to_numpy() == self.inc5).mean())
            log(f"  PUF income groups agree with piece A's g_inc on {100 * agree:.2f}% of records")
            if agree < 1:
                self.inc5 = self.A["g_inc"].to_numpy().astype(np.int8)
        self.inc_total = tot_all[rows]
        self.tokv = {v: ds.col(v)[rows].astype(np.int64) for v in ["age_range", "married", "n_dep"] + INCOMES}
        self.incd = {v: r[v].to_numpy().astype(float) for v in INCOMES}
        self.age = self.tokv["age_range"]          # -1: age unobserved on the PUF record
        self.ages = sorted(np.unique(self.age).tolist())
        am = ds.vmeta["age_range"]["tokens"]
        self.age_labels = {int(t.get("token", i)): str(t.get("meaning", t.get("value", i)))
                           for i, t in enumerate(am)}
        self.age_labels[-1] = "age unobserved (PUF)"
        log(f"PUF records: {self.N:,} ({'piece A order' if self.from_a else 'expB order'}), "
            f"total weight {self.W.sum():,.0f}, age unobserved on {100 * (self.age < 0).mean():.1f}% of records")


def a_puf_module():
    for name in ("a_puf", "a_puf_common", "a_common"):
        try:
            m = importlib.import_module(name)
        except Exception:                        # noqa: BLE001
            continue
        if hasattr(m, "decode_dollars_puf") or name != "a_common":
            return m
    return None


def decode_puf(base, tok, seed=E.SEED):
    """Dollars at 2015 for PUF draws: piece A's PUF decoder if it exposes
    one, else test 2's decoder at year 2015 (as c_evaluate_puf.py)."""
    N, K, _ = tok.shape
    m = a_puf_module()
    for fn in ("decode_dollars_puf", "decode_dollars"):
        if m is not None and hasattr(m, fn) and m.__name__ != "a_common":
            try:
                recs = pd.read_parquet(os.path.join(AP, "records.parquet"))
                d = np.asarray(getattr(m, fn)(tok, recs, seed), float).reshape(N, K, len(WEALTH))
                log(f"  decoded with {m.__name__}.{fn}")
                return d
            except Exception as e:               # noqa: BLE001
                log(f"  {m.__name__}.{fn} failed ({type(e).__name__}: {e})")
    from c_common import decode_wealth
    C = pd.DataFrame({"source": "PUF", "year": np.repeat(base.year, K)})
    flat = tok.reshape(N * K, len(WEALTH))
    for c, v in enumerate(WEALTH):
        C["tok_" + v] = flat[:, c]
    log("  decoding with test 2's decoder at 2015")
    dec = decode_wealth(base.ds, C, seed=seed, year=2015)
    return np.column_stack([dec[v].to_numpy() for v in WEALTH]).reshape(N, K, len(WEALTH))


def write(label, out):
    d = os.path.join(OUT, label)
    os.makedirs(d, exist_ok=True)
    if CURRENT_INPUTS:
        out["inputs"] = list(CURRENT_INPUTS)
    out["time"] = time.strftime("%Y-%m-%dT%H:%M")
    tf = out.get("target_fit")
    if tf is not None and "_fit" in tf:
        np.save(os.path.join(d, "cell_fit.npy"), tf.pop("_fit"))
    tmp = os.path.join(d, "stats.json.tmp")
    json.dump(E.jsonable(out), open(tmp, "w"))
    os.replace(tmp, os.path.join(d, "stats.json"))
    log(f"wrote {d}/stats.json")


def run_scf():
    base = E.Base()
    for y in (2013, 2016):
        ridx = np.flatnonzero(base.year == y)
        st = E.frame_stats(base, ridx, base.ttok[ridx], base.tdol[ridx], base.W[ridx])
        write(f"scf{y}", {"label": f"scf{y}", "kind": "scf", "n_records": int(len(ridx)),
                          "note": f"SCF {y} training records with wealth observed (true wealth)", "stats": st})


def puf_target_fit(base, tok, w, which):
    """Target fit against $P/a_puf/targets_<which>.parquet, if piece A's PUF
    module gives a contribution builder."""
    tpath = os.path.join(AP, f"targets_{which}.parquet")
    m = a_puf_module()
    if m is None or not os.path.exists(tpath) or m.__name__ == "a_common":
        return None
    t = pd.read_parquet(tpath)
    recs = pd.read_parquet(os.path.join(AP, "records.parquet"))
    fn = None
    for nm in ("contrib_index_puf", "contrib_index"):
        if hasattr(m, nm):
            fn = getattr(m, nm)
            break
    if fn is None:
        return None
    N, K = w.shape
    tcol = next(c for c in ("target", "target_weight", "value") if c in t.columns)
    ncell = len(t)
    fit = np.zeros(ncell)
    step = 4000
    try:
        for s0 in range(0, N, step):
            sl = slice(s0, min(N, s0 + step))
            try:
                idx = fn(tok[sl], recs.iloc[sl], which)
            except TypeError:
                idx = fn(tok[sl], recs.iloc[sl])
            idx = np.asarray(idx)
            ww = np.repeat(w[sl].reshape(-1), idx.shape[1])
            ok = idx.reshape(-1) >= 0
            fit += np.bincount(idx.reshape(-1)[ok], weights=ww[ok], minlength=ncell)[:ncell]
    except Exception as e:                       # noqa: BLE001
        log(f"  PUF target fit not computed ({type(e).__name__}: {e})")
        return None
    d = t.copy()
    d["fit"] = fit
    d["target"] = d[tcol].astype(float)
    gk = [c for c in ("family", "group", "variable") if c in d.columns]
    gt = d.groupby(gk)["target"].transform("sum")
    d["err_share"] = np.abs(d["fit"] - d["target"]) / gt.where(gt > 0)
    rows = []
    for fam, s in d.groupby("family", sort=False):
        rows.append({"family": fam, "cells": int(len(s)), "mean_err_pct": 100 * float(s["err_share"].mean()),
                     "max_err_pct": 100 * float(s["err_share"].max())})
    allrow = {"family": "all", "cells": int(len(d)), "mean_err_pct": 100 * float(d["err_share"].mean()),
              "max_err_pct": 100 * float(d["err_share"].max()),
              "weight_in_zero_cells_pct": 100 * float(d.loc[d["target"] == 0, "fit"].sum() / d["target"].sum()),
              "misplaced_pct": 100 * float(0.5 * np.abs(d["fit"] - d["target"]).sum() / d["target"].sum())}
    return {"which": which, "by_family": rows, "all": allrow,
            "unreached_positive_cells": int(((d["target"] > 0) & (d["fit"] == 0)).sum())}


def run_label(base, label, draws, weights=None, dollars=None, note="", which=None, kind="draws"):
    log(f"=== {label}: {draws}" + (f" + {weights}" if weights else ""))
    D = E.load_draws(draws, base)
    tok = D["tokens"]
    N, K, _ = tok.shape
    w = E.load_weights(weights, base, K) if weights else np.repeat(base.W[:, None] / K, K, axis=1)
    dol = E.load_dollars(dollars, base, K) if dollars else decode_puf(base, tok)
    out = {"label": label, "kind": kind, "note": note, "draws_file": os.path.abspath(draws),
           "weights_file": os.path.abspath(weights) if weights else None, "n_records": N, "K": K,
           "target_set": which}
    out["move"] = E.move_size(base, w, D.get("logp_new"), D.get("logp_ref"))
    if weights:
        z = np.load(weights)
        if "cell_status" in z.files:
            cs = pd.Series(z["cell_status"].astype(str))
            out["calibration_cells"] = {"status_counts": cs.value_counts().to_dict(),
                                        "infeasible": int((~cs.isin(["active", "zero"])).sum())}
    out["target_fit"] = {wh: puf_target_fit(base, tok, w, wh) for wh in ("core", "full")}
    out["stats"] = E.frame_stats(base, *E.flat_rows(base, tok, dol, w))
    write(label, out)
    if label == "p_ref" or label.startswith("p_ref"):
        # draw noise: two halves of the reference draws
        h = K // 2
        for nm, sl in (("half_a", slice(0, h)), ("half_b", slice(h, K))):
            ww = np.repeat(base.W[:, None] / h, h, axis=1)
            st = E.frame_stats(base, *E.flat_rows(base, tok[:, sl], dol[:, sl], ww))
            write(f"p_noise_{nm}", {"label": f"p_noise_{nm}", "kind": "noise", "stats": st,
                                    "note": f"PUF reference draws {sl.start}..{sl.stop - 1}"})


def plan():
    jobs = []
    ref = os.path.join(AP, "ref_draws.npz")
    if os.path.exists(ref) and glob.glob(os.path.join(AP, "DONE_*ref*")):
        rd = os.path.join(AP, "ref_dollars.parquet")
        rd = rd if os.path.exists(rd) else None
        jobs.append(("p_ref", dict(draws=ref, dollars=rd, kind="reference",
                                   note="reference model completing PUF records, unweighted"), [ref]))
        for which in ("core", "full"):
            for wf in sorted(glob.glob(os.path.join(P, "b", f"puf_{which}", "weights*.npz"))):
                tag = os.path.basename(wf)[len("weights"):-4].strip("_")
                if "k" in tag and tag not in ("", "ref"):
                    continue     # K-sensitivity weights: shown through B's reports only
                jobs.append((f"p_m1_{which}", dict(draws=ref, weights=wf, dollars=rd, which=which, kind="method1",
                                                   note=f"method 1 on P_{which}: reference draws reweighted"),
                             [ref, wf]))
    for which in ("core", "full"):
        cdir = os.path.join(P, "c", f"puf_{which}")
        cd = os.path.join(cdir, "draws.npz")
        if os.path.exists(cd) and (os.path.exists(os.path.join(cdir, "DONE")) or
                                   glob.glob(os.path.join(P, "c", f"DONE_puf_{which}*"))):
            jobs.append((f"p_m2_{which}", dict(draws=cd, which=which, kind="method2",
                                               note=f"method 2 on P_{which}, before residual calibration"), [cd]))
            wf = os.path.join(cdir, "weights_resid.npz")
            if os.path.exists(wf):
                jobs.append((f"p_m2_{which}_cal", dict(draws=cd, weights=wf, which=which, kind="method2_cal",
                                                       note=f"method 2 on P_{which}, after residual calibration"),
                             [cd, wf]))
        for vd in sorted(glob.glob(os.path.join(P, "d", f"puf_{which}", "*", ""))):
            v = os.path.basename(os.path.dirname(vd))
            dd = os.path.join(vd, "draws.npz")
            if not (os.path.exists(dd) and (os.path.exists(os.path.join(vd, "DONE")) or
                                           glob.glob(os.path.join(P, "d", f"DONE_puf_{which}_{v}*")) or
                                           glob.glob(os.path.join(P, "d", f"puf_{which}", f"DONE_{v}*")))):
                continue
            jobs.append((f"p_m3_{which}_{v}", dict(draws=dd, which=which, kind="method3",
                                                   note=f"method 3 ({v}) on P_{which}, before residual calibration"),
                         [dd]))
            wf = os.path.join(vd, "weights_resid.npz")
            if os.path.exists(wf):
                jobs.append((f"p_m3_{which}_{v}_cal", dict(draws=dd, weights=wf, which=which, kind="method3_cal",
                                                           note=f"method 3 ({v}) on P_{which}, after residual "
                                                                "calibration"), [dd, wf]))
    return jobs


CURRENT_INPUTS = []


def stale(label, inputs):
    f = os.path.join(OUT, label, "stats.json")
    if os.path.exists(f):
        try:
            got = json.load(open(f)).get("inputs")
        except (json.JSONDecodeError, OSError):
            got = None
        if got is not None and sorted(got) != sorted(os.path.abspath(x) for x in inputs):
            return True
    return (not os.path.exists(f)) or os.path.getmtime(f) < max(os.path.getmtime(x) for x in inputs)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--auto", action="store_true")
    ap.add_argument("--scf", action="store_true")
    ap.add_argument("--label")
    ap.add_argument("--draws")
    ap.add_argument("--weights")
    ap.add_argument("--dollars")
    ap.add_argument("--which")
    args = ap.parse_args()
    os.makedirs(OUT, exist_ok=True)
    if args.scf or (args.auto and stale("scf2016", [os.path.join(P, "a", "records.parquet")])):
        run_scf()
    jobs = plan() if args.auto else []
    if args.draws or jobs:
        base = PufBase()
    if args.draws:
        run_label(base, args.label or "p_custom", args.draws, args.weights, args.dollars, which=args.which)
    attempted = os.path.join(OUT, ".attempted")
    done = set(open(attempted).read().split("\n")) if os.path.exists(attempted) else set()
    for label, kw, inputs in jobs:
        if not stale(label, inputs):
            continue
        newest = max(os.path.getmtime(x) for x in inputs)
        if time.time() - newest < 120:
            continue
        key = f"{label}:{newest:.0f}"
        if key in done:
            continue
        with open(attempted, "a") as fh:
            fh.write(key + "\n")
        CURRENT_INPUTS[:] = [os.path.abspath(x) for x in inputs]
        try:
            run_label(base, label, **kw)
        except SystemExit as e:
            log(f"  {label} failed: {e}")
        except Exception as e:                   # noqa: BLE001
            import traceback
            traceback.print_exc()
            log(f"  {label} failed: {type(e).__name__}: {e}")


if __name__ == "__main__":
    main()
