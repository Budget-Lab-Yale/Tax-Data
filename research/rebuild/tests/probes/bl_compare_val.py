"""Probe BL: copy of as_compare.py with one change: the truth column is the
PUF VALIDATION records (20,768 records no model was trained on), their
weights scaled up to the training records' total returns, so that a
generator that copies training records gets no credit for it. Thresholds stay
the training records' AGI percentiles. Original description:

Probe AS (copy of o_compare.py, adapted): levels table, real PUF against
generated PUF files, with AGI computed as income items less adjustments plus
the remainder (as_agi.py) wherever a file carries AGI_REMAINDER instead of
AGI. For files from test 3's model (which draw AGI itself) the remainder is
implied: AGI less items plus adjustments.

Usage: python as_compare.py --out <json> LABEL=PATH ...
  PATH: a parquet from o_complete.py (generate mode); or `roundtrip` (the
  PUF training records' own full_v2s tokens decoded to dollars, AGI from the
  decoded parts); or `roundtrip_v2` (full_v2 tokens decoded, AGI decoded
  directly, test 3's encoding).

Thresholds: the real PUF training records' weighted AGI percentiles at 50,
90, 99, 99.9. Aggregates only are printed.
"""
import argparse
import importlib.util
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import as_agi  # noqa: E402

DATA = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s")
DATA_V2 = Path("/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/data")
PT = HERE.parent / "posttrain"
COMP = {"wages": ["E00200"], "cap_gains": ["P23250", "P22250"],
        "partnership_scorp": ["E26270"], "dividends": ["E00600"],
        "business_schC": ["E00900"], "interest": ["E00300"]}
EXTRA = {"schedule_e": ["E02000"], "pensions_ira": ["E01700", "E01400"],
         "unemployment": ["E02300"], "farm": ["E02100"], "adjustments": as_agi.ADJ}
GROUPS = [("bottom 50", 0, 0.5), ("50-90", 0.5, 0.9), ("90-99", 0.9, 0.99),
          ("99-99.9", 0.99, 0.999), ("top 0.1", 0.999, 1.01)]


def wq(x, w, q):
    o = np.argsort(x, kind="stable")
    cw = np.cumsum(w[o])
    return x[o][min(np.searchsorted(cw, q * cw[-1]), len(x) - 1)]


def stats(df, w, thr, cuts):
    agi = df["E00100"].to_numpy(float)
    rem = df[as_agi.REM].to_numpy(float)
    a = np.clip(agi, 0, None)
    A = (a * w).sum()
    out = dict(total_agi_tn=float((agi * w).sum() / 1e12),
               returns_m=float(w.sum() / 1e6),
               w_above_p99=float(w[agi >= thr[0.99]].sum() / w.sum()),
               w_above_p999=float(w[agi >= thr[0.999]].sum() / w.sum()),
               share_above_p99=float((a * w)[agi >= thr[0.99]].sum() / A),
               share_above_p999=float((a * w)[agi >= thr[0.999]].sum() / A),
               remainder_total_bn=float((rem * w).sum() / 1e9))
    g = {}
    edges = [-np.inf, cuts[0.5], cuts[0.9], thr[0.99], thr[0.999], np.inf]
    for k, (lab, _, _) in enumerate(GROUPS):
        m = (agi >= edges[k]) & (agi < edges[k + 1])
        ww = w[m]
        row = dict(weight_share=float(ww.sum() / w.sum()),
                   agi_bn=float((agi[m] * ww).sum() / 1e9))
        for c, cols in {**COMP, **EXTRA}.items():
            row[c + "_bn"] = float(sum((df[x].to_numpy(float)[m] * ww).sum() for x in cols) / 1e9)
        cg = (df["P23250"].to_numpy(float) + df["P22250"].to_numpy(float))[m]
        wg = df["E00200"].to_numpy(float)[m]
        row["itemizer_share"] = float(ww[df["FDED"].to_numpy(float)[m] == 1].sum() / ww.sum())
        row["joint_share"] = float(ww[df["MARS"].to_numpy(float)[m] == 2].sum() / ww.sum())
        row["wages_and_capgains_share"] = float(ww[(wg > 0) & (cg != 0)].sum() / ww.sum())
        row["six_components_bn"] = float(sum(row[c + "_bn"] for c in COMP))
        row["agi_minus_six_bn"] = row["agi_bn"] - row["six_components_bn"]
        r = rem[m]
        row["remainder_bn"] = float((r * ww).sum() / 1e9)
        row["remainder_share_neg"] = float(ww[r < 0].sum() / ww.sum())
        row["remainder_share_zero"] = float(ww[r == 0].sum() / ww.sum())
        row["remainder_share_pos"] = float(ww[r > 0].sum() / ww.sum())
        for qq in (0.05, 0.5, 0.95):
            row[f"remainder_p{int(qq * 100)}"] = float(wq(r, ww, qq)) if m.any() else float("nan")
        row["remainder_abs_over_agi"] = float((np.abs(r) * ww).sum() / max(abs(row["agi_bn"]) * 1e9, 1.0))
        g[lab] = row
    out["groups"] = g
    out["totals_bn"] = {c: float(sum((df[x].to_numpy(float) * w).sum() for x in cols) / 1e9)
                        for c, cols in {**COMP, **EXTRA}.items()}
    return out


def tokenizer():
    spec = importlib.util.spec_from_file_location("g_tok", PT / "g_tokenizer.py")
    g = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(g)
    return g


def agreement(true_agi, est_agi, w):
    d = np.abs(est_agi - true_agi)
    ok1 = d <= 1.0
    okp = d <= 0.01 * np.abs(true_agi)
    return dict(share_within_1_dollar=float(w[ok1].sum() / w.sum()),
                share_within_1_pct=float(w[okp].sum() / w.sum()),
                share_within_1_dollar_or_1_pct=float(w[ok1 | okp].sum() / w.sum()))


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--out", required=True)
    p.add_argument("items", nargs="+")
    a = p.parse_args()
    meta = json.load(open(DATA / "meta.json"))
    rec = pd.read_parquet(DATA / "records_train.parquet")
    puf = (rec["source"].astype(str) == "PUF").to_numpy()
    r = rec[puf].reset_index(drop=True)
    w0 = r["weight"].to_numpy(float)
    agi0 = r["E00100"].to_numpy(float)
    thr = {q: wq(agi0, w0, q) for q in (0.99, 0.999)}
    cuts = {q: wq(agi0, w0, q) for q in (0.5, 0.9)}
    rv = pd.read_parquet(DATA / "records_val.parquet")
    rv = rv[(rv["source"].astype(str) == "PUF").to_numpy()].reset_index(drop=True)
    wv = rv["weight"].to_numpy(float) * (w0.sum() / rv["weight"].to_numpy(float).sum())
    res = {"thresholds": {"p50": cuts[0.5], "p90": cuts[0.9], "p99": thr[0.99], "p99.9": thr[0.999]},
           "PUF validation records": stats(rv, wv, thr, cuts)}
    # Identity on the true dollars (should hold exactly, up to the rounding to cents).
    res["identity_true_dollars"] = agreement(agi0, as_agi.agi_from_parts(r), w0)
    edges = [-np.inf, cuts[0.5], cuts[0.9], thr[0.99], thr[0.999], np.inf]
    for item in a.items:
        lab, path = item.split("=", 1)
        if path in ("roundtrip", "roundtrip_v2"):
            g = tokenizer()
            if path == "roundtrip":
                m_, d_ = meta, DATA
            else:
                m_, d_ = json.load(open(DATA_V2 / "meta.json")), DATA_V2
            tok = np.load(d_ / "tokens_train.npy")[puf]
            q = pd.read_parquet(d_ / "quantiles.parquet")
            dec = g.decode(tok, m_, r[["source", "year"]], q, np.random.default_rng(7))
            if path == "roundtrip":
                dec["E00100"] = as_agi.agi_from_parts(dec)
            else:
                dec[as_agi.REM] = dec["E00100"].to_numpy(float) - as_agi.items_less_adjustments(dec)
            df, w = dec, w0
            ag = {"all": agreement(agi0, dec["E00100"].to_numpy(float), w0)}
            for k, (gl, _, _) in enumerate(GROUPS):
                m = (agi0 >= edges[k]) & (agi0 < edges[k + 1])
                ag[gl] = agreement(agi0[m], dec["E00100"].to_numpy(float)[m], w0[m])
                ag[gl]["true_agi_bn"] = float((agi0[m] * w0[m]).sum() / 1e9)
                ag[gl]["decoded_agi_bn_same_records"] = float((dec["E00100"].to_numpy(float)[m] * w0[m]).sum() / 1e9)
            res[lab + " record agreement (grouped by true AGI)"] = ag
        else:
            df = pd.read_parquet(path)
            if "draw" in df:
                df = df[df["draw"] == 0].reset_index(drop=True)
            w = df["weight"].to_numpy(float).copy()
            if as_agi.REM in df and "E00100" not in df:
                df["E00100"] = as_agi.agi_from_parts(df)
            elif "E00100" in df and as_agi.REM not in df:
                df[as_agi.REM] = df["E00100"].to_numpy(float) - as_agi.items_less_adjustments(df)
        res[lab] = stats(df, w, thr, cuts)
        print(f"done {lab}", flush=True)
    json.dump(res, open(a.out, "w"), indent=2)
    labs = [k for k in res if k in ("PUF validation records",) or (isinstance(res[k], dict) and "total_agi_tn" in res[k])]
    print("thresholds", res["thresholds"])
    print("identity on true dollars", res["identity_true_dollars"])
    for k in res:
        if "record agreement" in k:
            print(k, json.dumps(res[k], indent=1))
    print("\n| | " + " | ".join(labs) + " |")
    print("|---" * (len(labs) + 1) + "|")
    for key, fmt in [("total_agi_tn", "${:.2f}tn"), ("w_above_p99", "{:.2%}"),
                     ("w_above_p999", "{:.3%}"), ("share_above_p99", "{:.1%}"),
                     ("share_above_p999", "{:.1%}"), ("remainder_total_bn", "{:,.0f}")]:
        print(f"| {key} | " + " | ".join(fmt.format(res[l][key]) for l in labs) + " |")
    keys = ["agi_bn", "weight_share"] + [k + "_bn" for k in {**COMP, **EXTRA}] + \
        ["six_components_bn", "agi_minus_six_bn", "remainder_bn", "remainder_share_neg", "remainder_share_zero",
         "remainder_p5", "remainder_p50", "remainder_p95", "itemizer_share", "joint_share", "wages_and_capgains_share"]
    for c in keys:
        print(f"\n{c} by AGI group")
        print("| group | " + " | ".join(labs) + " |")
        for gl, _, _ in GROUPS:
            vals = [res[l]["groups"][gl][c] for l in labs]
            if c.endswith("_share") or "share_" in c:
                s = [f"{v:.3f}" for v in vals]
            else:
                s = [f"{v:,.0f}" for v in vals]
            print(f"| {gl} | " + " | ".join(s) + " |")


if __name__ == "__main__":
    main()
