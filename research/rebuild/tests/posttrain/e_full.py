"""Piece E: pretraining evaluation of the full model (SPEC.md, main session
9 October 20:45, item 3). Reads piece H's files in $T2/model/full/:

  h_gen_puf_seedNN.parquet     whole PUF records generated from nothing
  h_puf_wealth_seedNN.parquet  PUF training records completed with wealth (SCF tag, 2015)
  h_scf_train_seedNN.parquet   SCF training records' wealth redrawn from their observed variables
  h_scf_test_seedNN.parquet    the same for SCF test records

and measures how close they are to ground truth:

  gen_puf     against the PUF training records: test 1's measures
              (../puf_generator/evaluate.py, run as a subprocess with the
              training records as reference, and again with the held-out
              test records), plus levels by AGI group computed here;
  scf_*       completed wealth against the same records' true wealth, with
              the measures of e_evaluate.py (income groups: the seven-income
              total of test 2, ranked within wave over all SCF units of the
              split);
  puf_wealth  against the SCF 2013 and 2016 training records interpolated
              to 2015, as e_puf.py (income groups: the same seven-income
              total, ranked among PUF training records).

  python e_full.py --auto       # everything ready and new (the watcher calls this)

Outputs: $P/e/full/<label>/stats.json (and for gen_puf, test 1's output
folders under $P/e/full/puf_eval/). e_compare.py turns them into tables.
"""
import argparse
import glob
import json
import os
import re
import subprocess
import sys
import time

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import e_evaluate as E  # noqa: E402
from c_common import WEALTH, token_info, wrank, wquantile  # noqa: E402

P, T2 = E.P, E.T2
VERSION = os.environ.get("E_FULL_VERSION", "full")          # "full" (v1) or "full_v2"
DATA = os.environ.get("E_FULL_DATA", os.path.join(T2, "data", VERSION))
MODEL = os.environ.get("E_FULL_MODEL", os.path.join(T2, "model", VERSION))
OUT = os.path.join(E.OUT, VERSION)
S1 = "/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator"
T1CODE = os.path.join(HERE, "..", "puf_generator")
T2INC = ["t2_wages", "t2_business", "t2_int_div", "t2_capital_gains", "t2_rent", "t2_ss_pens", "t2_ui_other"]
CLS_VARS = ["MARS", "DSI", "XTOT", "AGERANGE", "E00200", "E00300", "E00400", "E00600", "E00900",
            "E02300", "P23250"]
log = E.log

_META = {}


def meta():
    if "m" not in _META:
        _META["m"] = json.load(open(os.path.join(DATA, "meta.json")))
    return _META["m"]


class FullBase:
    """Records of one source in one split of the full dataset, shaped like
    e_evaluate.Base so its statistics functions apply unchanged."""

    def __init__(self, split, source, rows=None):
        m = meta()
        self.vars = [v["name"] for v in m["variables"]]
        self.vmeta = {v["name"]: v for v in m["variables"]}
        self.tinfo = {v: token_info(self.vmeta[v]) for v in WEALTH}
        rec = pd.read_parquet(os.path.join(DATA, f"records_{split}.parquet"))
        truth = np.load(os.path.join(DATA, f"truth_{split}.npy"))
        src = rec["source"].astype(str).to_numpy()
        yr = rec["year"].to_numpy().astype(int)
        pool = src == source
        if all(c in rec.columns for c in T2INC) and not np.isnan(rec[T2INC].to_numpy(float)[pool]).any():
            tot = rec[T2INC].to_numpy(dtype=float).sum(axis=1)
            self.income_measure = "test 2's seven-income total"
        else:
            items = ["E00200", "E00900", "P23250", "E00600", "E00300", "E02300"]
            tot = rec[items].to_numpy(dtype=float).sum(axis=1)
            self.income_measure = "sum of " + ", ".join(items)
            if np.isnan(tot[pool]).any():
                raise SystemExit(f"{split} {source}: {int(np.isnan(tot[pool]).sum())} rows lack an income item")
        rank = np.full(len(rec), np.nan)
        for y in np.unique(yr[pool]):
            ii = np.flatnonzero(pool & (yr == y))
            rank[ii] = wrank(tot[ii], rec["weight"].to_numpy()[ii].astype(float))
        keep = pool.copy()
        if source == "SCF":
            keep &= rec["wealth_observed"].fillna(False).to_numpy().astype(bool)
        if rows is None:
            rows = np.flatnonzero(keep)
        else:
            rows = np.asarray(rows, np.int64)
            if not keep[rows].all():
                raise SystemExit("requested rows outside the evaluation set")
        self.rows, self.N, self.split, self.source = rows, len(rows), split, source
        r = rec.iloc[rows].reset_index(drop=True)
        self.year = yr[rows]
        self.hh = self.year.astype(np.int64) * 10**9 + r["hh_id"].astype(np.int64).to_numpy()
        self.W = r["weight"].to_numpy().astype(float)
        if np.isnan(self.W).any() or (self.W <= 0).any():
            raise SystemExit("weights missing or not positive")
        self.inc_rank = rank[rows]
        self.inc5 = np.searchsorted(np.array(E.INC5_EDGES), self.inc_rank, side="right").astype(np.int8)
        self.inc_total = tot[rows]
        col = {n: j for j, n in enumerate(self.vars)}
        self.tokv = {v: truth[rows, col[v]].astype(np.int64) for v in CLS_VARS}
        self.cls_vars = CLS_VARS
        self.incd = {"business": r["E00900"].to_numpy().astype(float),
                     "rent": r["E25850"].to_numpy().astype(float) if source == "PUF" else None}
        self.age = self.tokv["AGERANGE"]
        self.ages = sorted(np.unique(self.age).tolist())
        self.age_labels = {int(t.get("token", i)): str(t.get("meaning", i)).split(" (")[0]
                           for i, t in enumerate(self.vmeta["AGERANGE"]["tokens"])}
        self.age_labels[-1] = "age unobserved (PUF)"
        if source == "SCF":
            self.ttok = np.column_stack([truth[rows, col[v]] for v in WEALTH]).astype(np.int64)
            self.tdol = r[WEALTH].to_numpy(dtype=float)
            if (self.ttok < 0).any() or np.isnan(self.tdol).any():
                raise SystemExit("true wealth missing on SCF records with wealth observed")
        log(f"full {split} {source}: {self.N:,} records, weight {self.W.sum():,.0f}")


def load_h(path, base):
    """H's completion parquet -> tokens and dollars [N, K, 24] in base order;
    records of the file outside the base (e.g. SCF split-offs) set aside."""
    cols = ["row", "draw"] + ["tok_" + v for v in WEALTH] + WEALTH
    C = pd.read_parquet(path, columns=cols)
    inb = np.isin(C["row"].to_numpy(), base.rows)
    if (~inb).any():
        log(f"  {int((~inb).sum()):,} rows of {os.path.basename(path)} are outside the evaluation set "
            "(records without true wealth); set aside")
        C = C.loc[inb]
    C = C.sort_values(["row", "draw"])
    rows = C["row"].unique()
    K = int(C["draw"].nunique())
    if len(C) != len(rows) * K:
        raise SystemExit(f"{path}: unequal draws per record")
    if len(rows) != base.N:
        log(f"  WARNING: file covers {len(rows):,} of {base.N:,} records; evaluating those")
    tok = C[["tok_" + v for v in WEALTH]].to_numpy().astype(np.int64).reshape(len(rows), K, len(WEALTH))
    dol = C[WEALTH].to_numpy(dtype=float).reshape(len(rows), K, len(WEALTH))
    if (tok < 0).any() or np.isnan(dol).any():
        raise SystemExit(f"{path}: wealth tokens or dollars missing on {int((tok < 0).any(2).sum())} draws")
    return rows, tok, dol


def write(label, out):
    d = os.path.join(OUT, label)
    os.makedirs(d, exist_ok=True)
    out["time"] = time.strftime("%Y-%m-%dT%H:%M")
    tmp = os.path.join(d, "stats.json.tmp")
    json.dump(E.jsonable(out), open(tmp, "w"))
    os.replace(tmp, os.path.join(d, "stats.json"))
    log(f"wrote {d}/stats.json")


def sets_of(K):
    """Draw subsets for the Monte Carlo yardstick within one file."""
    s = {"draw0": [0], "draw1": [1]}
    if K >= 4:
        h = K // 2
        s["half_a"], s["half_b"] = list(range(h)), list(range(h, 2 * h))
    return s


def run_scf(path, split, seed_tag):
    base0 = FullBase(split, "SCF")
    rows, tok, dol = load_h(path, base0)
    base = FullBase(split, "SCF", rows) if len(rows) != base0.N or not (rows == base0.rows).all() else base0
    N, K, _ = tok.shape
    tl = f"truth_{split}"
    if not os.path.exists(os.path.join(OUT, tl, "stats.json")) or base is not base0:
        write(tl, {"label": tl, "kind": "truth", "n_records": base.N,
                   "note": f"SCF {split} records' own wealth (full dataset)",
                   "stats": E.frame_stats(base, np.arange(base.N), base.ttok, base.tdol, base.W)})
    w = np.repeat(base.W[:, None] / K, K, axis=1)
    out = {"label": f"scf_{split}_{seed_tag}", "kind": "completed", "file": path, "K": K, "n_records": N,
           "note": f"SCF {split} records, wealth drawn by the full model ({seed_tag})",
           "stats": E.frame_stats(base, *E.flat_rows(base, tok, dol, w)),
           "classifier": E.classifier(base, tok, w, E.SEED + 300)}
    out["sets"] = {}
    for nm, js in sets_of(K).items():
        ww = np.repeat(base.W[:, None] / len(js), len(js), axis=1)
        out["sets"][nm] = {"n_draws": len(js),
                           "stats": E.frame_stats(base, *E.flat_rows(base, tok[:, js], dol[:, js], ww))}
    c, _ = E.classifier_test(E.class_frame(base, tok[:, 0]), E.class_frame(base, tok[:, 1]), E.SEED + 400)
    out["classifier_draw0_vs_draw1"] = c
    write(out["label"], out)


def ensure_scf_waves():
    if all(os.path.exists(os.path.join(OUT, f"scf{y}", "stats.json")) for y in (2013, 2016)):
        return
    sb = FullBase("train", "SCF")
    for y in (2013, 2016):
        lab = f"scf{y}"
        if not os.path.exists(os.path.join(OUT, lab, "stats.json")):
            ridx = np.flatnonzero(sb.year == y)
            write(lab, {"label": lab, "kind": "scf", "n_records": int(len(ridx)),
                        "note": f"SCF {y} training records with wealth observed (full dataset, true wealth)",
                        "stats": E.frame_stats(sb, ridx, sb.ttok[ridx], sb.tdol[ridx], sb.W[ridx])})


def run_puf_wealth(path, seed_tag):
    ensure_scf_waves()
    pb0 = FullBase("train", "PUF")
    rows, tok, dol = load_h(path, pb0)
    pb = FullBase("train", "PUF", rows) if len(rows) != pb0.N or not (rows == pb0.rows).all() else pb0
    N, K, _ = tok.shape
    w = np.repeat(pb.W[:, None] / K, K, axis=1)
    out = {"label": f"puf_wealth_{seed_tag}", "kind": "puf_completed", "file": path, "K": K, "n_records": N,
           "note": f"PUF training records completed with wealth by the full model ({seed_tag})",
           "stats": E.frame_stats(pb, *E.flat_rows(pb, tok, dol, w)), "sets": {}}
    for nm, js in sets_of(K).items():
        if not nm.startswith("half"):
            continue
        ww = np.repeat(pb.W[:, None] / len(js), len(js), axis=1)
        out["sets"][nm] = {"n_draws": len(js),
                           "stats": E.frame_stats(pb, *E.flat_rows(pb, tok[:, js], dol[:, js], ww))}
    write(out["label"], out)


# --------------------------------------------------------------------------
# Whole PUF records generated from nothing
# --------------------------------------------------------------------------

AGI_EDGES = [0.5, 0.9, 0.99, 0.999]
LEVEL_VARS = [("E00100", "AGI"), ("E00200", "wages"), ("E00900", "Schedule C"), ("E26270", "partnership/S corp"),
              ("P23250", "long-term gains"), ("E00600", "dividends"), ("E00300", "taxable interest"),
              ("E01500", "pensions"), ("E02400", "Social Security")]


def puf_levels(df):
    """Levels by AGI group (each file ranked on its own AGI): mean of the main
    items, share with wages and with gains, share of all AGI."""
    w = df["weight"].to_numpy(float)
    agi = df["E00100"].to_numpy(float)
    g = np.searchsorted(np.array(AGI_EDGES), wrank(agi, w), side="right")
    out = []
    for k, lab in enumerate(E.INC5_LABELS):
        m = g == k
        row = {"group": lab, "weight_share": float(w[m].sum() / w.sum()),
               "agi_share": float((w[m] * agi[m]).sum() / (w * agi).sum())}
        for v, _ in LEVEL_VARS:
            x = df[v].to_numpy(float)
            row["mean_" + v] = float((w[m] * x[m]).sum() / w[m].sum())
        for v in ("E00200", "P23250", "E00900", "E02400"):
            row["nonzero_" + v] = float((w[m] * (df[v].to_numpy(float)[m] != 0)).sum() / w[m].sum())
        out.append(row)
    tot = {"total_weight": float(w.sum()), "agi_total": float((w * agi).sum()),
           "agi_p50": float(wquantile(agi, w, [0.5])[0])}
    return out, tot


def to_values(path, outp):
    """H's generated parquet -> test 1's values format (weight + 39
    variables). Missing categorical values are reported and coded 0 (test
    1's code for 'not reported')."""
    sys.path.insert(0, T1CODE)
    import evaluate as T1  # noqa: E402
    G = pd.read_parquet(path)
    miss = [v for v in T1.VARIABLES if v not in G.columns]
    if miss:
        raise SystemExit(f"{path} lacks {miss}")
    V = G[T1.VARIABLES].copy()
    nas = {v: int(V[v].isna().sum()) for v in T1.VARIABLES if V[v].isna().any()}
    if nas:
        log(f"  generated file has missing values: {nas}")
        bad = [v for v in nas if v not in T1.CATEGORICAL]
        if bad:
            raise SystemExit(f"missing generated amounts {bad}; not evaluated")
        for v in nas:
            V[v] = V[v].fillna(0)
    V.to_parquet(outp)
    return V


def run_gen_puf(path, seed_tag):
    d = os.path.join(E.OUT, "puf_eval")          # shared by both versions (test 1's baseline cache)
    os.makedirs(d, exist_ok=True)
    vp = os.path.join(OUT, f"values_gen_{seed_tag}.parquet")
    V = to_values(path, vp)
    train = pd.read_parquet(os.path.join(S1, "data", "values_train.parquet"))
    lv, tv = puf_levels(V)
    if not os.path.exists(os.path.join(OUT, "puf_train_levels", "stats.json")):
        lt, tt = puf_levels(train)
        write("puf_train_levels", {"label": "puf_train_levels", "kind": "puf_truth", "levels": lt, "totals": tt,
                                   "note": "PUF training records (test 1's values_train)"})
    # classifier without the weight (generated records all carry the same
    # weight, so test 1's classifier, which uses the weight, separates them
    # on that alone)
    sys.path.insert(0, T1CODE)
    import evaluate as T1  # noqa: E402
    cols = T1.CATEGORICAL + T1.AMOUNTS

    def cf(df, off):
        F = pd.DataFrame({"hh": np.arange(len(df)) + off, "w": df["weight"].to_numpy(float)})
        for v in cols:
            F["t_" + v] = df[v].to_numpy(float)
        return F
    clf, imp = E.classifier_test(cf(train, 0), cf(V, 10**7), E.SEED + 500)
    clf["top_variables"] = imp.head(6).to_dict("records")
    write(f"gen_puf_{seed_tag}", {"label": f"gen_puf_{seed_tag}", "kind": "puf_generated", "file": path,
                                  "levels": lv, "totals": tv, "classifier_no_weight": clf,
                                  "note": f"whole PUF records generated from nothing by the full model ({seed_tag})"})
    for ref, lab in (("values_train.parquet", f"{VERSION}_gen_{seed_tag}_vstrain"),
                     ("values_test.parquet", f"{VERSION}_gen_{seed_tag}")):
        # test 1's evaluation takes 10-20 minutes; run it as its own Slurm job
        # so the watcher is not held up
        args = ["--synthetic", vp, "--label", lab, "--data-dir", os.path.join(S1, "data"), "--eval-dir", d,
                "--reference", os.path.join(S1, "data", ref)]
        cmd = ["sbatch", "--parsable", "--job-name=e_t1eval", "--mem=64G", "--time=01:30:00",
               os.path.join(HERE, "e_py.sbatch"), "../puf_generator/evaluate.py"] + args
        r = subprocess.run(cmd, capture_output=True, text=True)
        if r.returncode:
            log(f"  could not submit test 1's evaluation for {lab}: {r.stderr.strip()}; running it here")
            subprocess.run([sys.executable, "-u", os.path.join(T1CODE, "evaluate.py")] + args)
        else:
            log(f"  test 1's evaluation for {lab}: Slurm job {r.stdout.strip()}")


# --------------------------------------------------------------------------
# Post-training on the full model, arm P (PUF records completed with wealth)
# --------------------------------------------------------------------------

PF = os.path.join(P, VERSION)
OUTP = os.path.join(OUT, "post")
_PCTX = {}


def p_context():
    """Piece A's full-model PUF records and target sets."""
    if "ctx" not in _PCTX:
        import a_common as A
        A.configure_full(VERSION)
        import a_full as AF
        recs = AF.load_puf_records()
        base = FullBase("train", "PUF", recs["row"].to_numpy())
        _PCTX["ctx"] = (A, AF, recs, base)
    return _PCTX["ctx"]


def p_target_fit(tok, w):
    A, AF, recs, base = p_context()
    out = {}
    vp = AF.truth_view(recs)
    for nm in ("wcore", "wfull"):
        try:
            ts = AF.rebuild_set(nm)
            cells = pd.read_parquet(os.path.join(PF, "a", f"targets_{nm}.parquet"))
            G = np.stack([recs[f"g_{nm}_{fam.name}"].to_numpy() for fam, _ in ts.tables], axis=1)
            N, K = w.shape
            tot = np.zeros(ts.n_cells)
            step = 5000
            for s0 in range(0, N, step):
                sl = slice(s0, min(N, s0 + step))
                idx = ts.contrib_index(G[sl], vp.subset(sl) if hasattr(vp, "subset") else _view_rows(vp, sl),
                                       tok[sl], list(WEALTH))
                tot += ts.totals(idx, w[sl].reshape(-1))
        except Exception as e:                   # noqa: BLE001
            log(f"  target fit {nm} not computed ({type(e).__name__}: {e})")
            out[nm] = None
            continue
        t = cells["target"].to_numpy(float)
        grp = cells["puf_group_weight"].to_numpy(float)
        err = np.abs(tot - np.nan_to_num(t)) / np.where(grp > 0, grp, np.nan)
        fam = cells["family"].to_numpy()
        out[nm] = {"by_family": [{"family": f, "mean_err_pct": 100 * float(np.nanmean(err[fam == f])),
                                  "max_err_pct": 100 * float(np.nanmax(err[fam == f]))} for f in pd.unique(fam)],
                   "all": {"mean_err_pct": 100 * float(np.nanmean(err)), "max_err_pct": 100 * float(np.nanmax(err)),
                           "weight_in_zero_cells_pct": 100 * float(tot[t == 0].sum() / tot.sum())},
                   "unreached_positive_cells": int(((t > 0) & (tot == 0)).sum())}
    return out


def _view_rows(vp, sl):
    """A row slice of piece A's truth view (an object with a `tok` dict)."""
    import copy
    v = copy.copy(vp)
    v.tok = {k: x[sl] for k, x in vp.tok.items()}
    for k in ("dol", "val"):
        if hasattr(vp, k) and isinstance(getattr(vp, k), dict):
            setattr(v, k, {kk: x[sl] for kk, x in getattr(vp, k).items()})
    return v


def p_write(label, out):
    d = os.path.join(OUTP, label)
    os.makedirs(d, exist_ok=True)
    out["time"] = time.strftime("%Y-%m-%dT%H:%M")
    tmp = os.path.join(d, "stats.json.tmp")
    json.dump(E.jsonable(out), open(tmp, "w"))
    os.replace(tmp, os.path.join(d, "stats.json"))
    log(f"wrote {d}/stats.json")


def run_p(label, draws, weights=None, note=""):
    A, AF, recs, base = p_context()
    log(f"=== {label}: {draws}" + (f" + {weights}" if weights else ""))
    D = E.load_draws(draws, base)
    tok = D["tokens"]
    N, K, _ = tok.shape
    w = E.load_weights(weights, base, K) if weights else np.repeat(base.W[:, None] / K, K, axis=1)
    z = np.load(draws)
    dpq = os.path.join(os.path.dirname(draws), os.path.basename(draws).replace("draws", "dollars")
                       .replace(".npz", ".parquet"))
    if "dollars" in z.files and z["dollars"].shape == tok.shape:
        dol = z["dollars"].astype(float)
    elif os.path.exists(dpq):
        dol = E.load_dollars(dpq, base, K)
    else:
        dol = np.asarray(A.decode_dollars(tok, recs, E.SEED, names=list(WEALTH), year=2015), float)
    out = {"label": label, "note": note, "draws_file": os.path.abspath(draws),
           "weights_file": os.path.abspath(weights) if weights else None, "K": K, "n_records": N,
           "move": E.move_size(base, w, D.get("logp_new"), D.get("logp_ref"))}
    if weights:
        zw = np.load(weights)
        if "cell_status" in zw.files:
            cs = pd.Series(zw["cell_status"].astype(str))
            out["calibration_cells"] = {"status_counts": cs.value_counts().to_dict(),
                                        "infeasible": int((~cs.isin(["active", "zero"])).sum())}
    out["target_fit"] = p_target_fit(tok, w)
    out["stats"] = E.frame_stats(base, *E.flat_rows(base, tok, dol, w))
    p_write(label, out)
    if label == "p_ref":
        h = K // 2
        for nm, sl in (("half_a", slice(0, h)), ("half_b", slice(h, 2 * h))):
            ww = np.repeat(base.W[:, None] / h, h, axis=1)
            p_write(f"p_noise_{nm}", {"label": f"p_noise_{nm}", "kind": "noise",
                                      "stats": E.frame_stats(base, *E.flat_rows(base, tok[:, sl], dol[:, sl], ww))})


def plan_post():
    jobs = []
    ref = os.path.join(PF, "a", "ref_draws_P.npz")
    if not (os.path.exists(ref) and os.path.exists(os.path.join(PF, "a", "DONE_ref_P"))):
        return jobs
    jobs.append(("p_ref", ref, None, "reference full model completing PUF records, unweighted"))
    Kref = None
    try:
        Kref = np.load(ref, mmap_mode="r")["tokens"].shape
    except Exception:                            # noqa: BLE001
        pass
    for piece, dig in (("b", "1"), ("c", "2"), ("d", "3")):
        root = os.path.join(PF, piece)
        for f in sorted(glob.glob(os.path.join(root, "**", "*.npz"), recursive=True)):
            rel = os.path.relpath(f, root)
            stem = re.sub(r"[^A-Za-z0-9]+", "_", rel[:-4]).strip("_")
            if "/ps0" in "/" + rel.lower() or "/ps1" in "/" + rel.lower():
                continue
            if time.time() - os.path.getmtime(f) < 300:
                continue
            base_name = os.path.basename(f)
            if piece == "b" and base_name.startswith("weights"):
                jobs.append((f"p_m1_{stem}", ref, f, f"method 1 ({rel}): reference draws reweighted"))
            elif base_name == "draws.npz":
                jobs.append((f"p_m{dig}_{stem}", f, None, f"method {dig} ({rel}), before residual calibration"))
                for wn, tag, extra in (("weights_resid.npz", "_cal", ""),
                                       ("weights_resid_fallback.npz", "_fallback_cal",
                                        " (fallback solver with a ridge term, so targets are not hit exactly)")):
                    wf = os.path.join(os.path.dirname(f), wn)
                    if os.path.exists(wf) and time.time() - os.path.getmtime(wf) > 300:
                        jobs.append((f"p_m{dig}_{stem}{tag}", f, wf,
                                     f"method {dig} ({rel}), after residual calibration{extra}"))
    return jobs


def run_post(only=None):
    jobs = plan_post()
    if jobs:
        ensure_scf_waves()
    attempted = os.path.join(OUTP, ".attempted")
    os.makedirs(OUTP, exist_ok=True)
    done = set(open(attempted).read().split("\n")) if os.path.exists(attempted) else set()
    for label, draws, weights, note in jobs:
        if only and not re.search(only, label):
            continue
        inputs = [x for x in (draws, weights) if x]
        newest = max(os.path.getmtime(x) for x in inputs)
        st = os.path.join(OUTP, label, "stats.json")
        if os.path.exists(st) and os.path.getmtime(st) > newest:
            continue
        key = f"{label}:{newest:.0f}"
        if key in done:
            continue
        with open(attempted, "a") as fh:
            fh.write(key + "\n")
        try:
            run_p(label, draws, weights, note)
        except SystemExit as e:
            log(f"  {label} skipped: {e}")
        except Exception as e:                   # noqa: BLE001
            import traceback
            traceback.print_exc()
            log(f"  {label} failed: {type(e).__name__}: {e}")


# --------------------------------------------------------------------------

def ready(path):
    info = path.replace(".parquet", "_info.json")
    return os.path.exists(path) and os.path.exists(info) and time.time() - os.path.getmtime(path) > 120


def plan():
    jobs = []
    for f in sorted(glob.glob(os.path.join(MODEL, "h_*_seed*.parquet"))):
        m = re.match(r"h_(gen_puf|puf_wealth|scf_train|scf_test)_(seed\d+)\.parquet$", os.path.basename(f))
        if not m or not ready(f):
            continue
        which, seed = m.groups()
        label = {"gen_puf": f"gen_puf_{seed}", "puf_wealth": f"puf_wealth_{seed}",
                 "scf_train": f"scf_train_{seed}", "scf_test": f"scf_test_{seed}"}[which]
        jobs.append((label, which, seed, f))
    return jobs


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--auto", action="store_true")
    ap.add_argument("--only", help="run only labels matching this pattern")
    ap.add_argument("--post", action="store_true", help="post-training files under $P/full only")
    args = ap.parse_args()
    if args.post or args.auto:
        try:
            run_post(args.only)
        except Exception as e:                   # noqa: BLE001
            import traceback
            traceback.print_exc()
            log(f"post-training evaluation failed: {type(e).__name__}: {e}")
        if args.post:
            return
    os.makedirs(OUT, exist_ok=True)
    attempted = os.path.join(OUT, ".attempted")
    done = set(open(attempted).read().split("\n")) if os.path.exists(attempted) else set()
    for label, which, seed, f in plan():
        if args.only and not re.search(args.only, label):
            continue
        st = os.path.join(OUT, label, "stats.json")
        if os.path.exists(st) and os.path.getmtime(st) > os.path.getmtime(f):
            continue
        key = f"{label}:{os.path.getmtime(f):.0f}"
        if key in done:
            continue
        with open(attempted, "a") as fh:
            fh.write(key + "\n")
        log(f"=== {label}: {f}")
        try:
            if which == "gen_puf":
                run_gen_puf(f, seed)
            elif which == "puf_wealth":
                run_puf_wealth(f, seed)
            else:
                run_scf(f, which.split("_")[1], seed)
        except SystemExit as e:
            log(f"  {label} failed: {e}")
        except Exception as e:                   # noqa: BLE001
            import traceback
            traceback.print_exc()
            log(f"  {label} failed: {type(e).__name__}: {e}")


if __name__ == "__main__":
    main()
