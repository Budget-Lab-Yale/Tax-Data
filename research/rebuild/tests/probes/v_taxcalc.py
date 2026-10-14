"""Probe V: 2015 federal income tax by Tax-Calculator (PSL taxcalc) on the real
PUF training records, restricted to the model's variables, and on generated
files.

Usage: python v_taxcalc.py --out <json> LABEL=PATH ...   (PATH may be 'validation')

What Tax-Calculator sees, for real and generated records alike: the 30 dollar
amounts in the model, filing status, exemptions, dependent status, children
for the child tax credit and the EITC, and the record weight. Not given:
the AGI remainder (taxable refunds, alimony, Form 4797, other income, the
adjustments not in the model) because Tax-Calculator computes taxable Social
Security itself from gross benefits and the remainder contains it; age (set
to 40 for every head, since the real file lacks age for 42% of records);
earnings splits between spouses (all on the head). Two bounded pairs are
clipped so Tax-Calculator accepts the record: qualified dividends at most
ordinary dividends, taxable pensions at most total pensions (the share of
records clipped is reported). Records are grouped by their own AGI as in
v_score.py (model identity for summed-AGI files, drawn AGI for test 3), with
the real PUF thresholds; Tax-Calculator's own AGI is also reported.
"""
import argparse
import json
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import v_score as vs  # noqa: E402

MAP = {"E00200": "e00200", "E00900": "e00900", "E02000": "e02000", "E26270": "e26270", "P23250": "p23250",
       "P22250": "p22250", "E00600": "e00600", "E00650": "e00650", "E00300": "e00300", "E01500": "e01500",
       "E01700": "e01700", "E01400": "e01400", "E02400": "e02400", "E02100": "e02100", "E00400": "e00400",
       "E01100": "e01100", "E02300": "e02300", "E03150": "e03150", "E03210": "e03210", "E03270": "e03270",
       "E17500": "e17500", "E18400": "e18400", "E18500": "e18500", "E19200": "e19200", "E19800": "e19800",
       "E20100": "e20100"}


def to_tc(df, w):
    d = pd.DataFrame({"RECID": np.arange(1, len(df) + 1), "s006": w})
    for k, v in MAP.items():
        d[v] = np.nan_to_num(df[k].to_numpy(float))
    clip = {"qualified dividends above ordinary": float(w[d["e00650"] > d["e00600"]].sum() / w.sum()),
            "taxable pensions above total": float(w[d["e01700"] > d["e01500"]].sum() / w.sum())}
    d["e00650"] = np.minimum(d["e00650"], d["e00600"]).clip(lower=0)
    d["e01700"] = np.minimum(d["e01700"], d["e01500"])
    d["e01500"] = np.maximum(d["e01500"], 0)
    d["e01700"] = d["e01700"].clip(lower=0)
    for v in ("e00200", "e00900", "e02100"):
        d[v + "p"] = d[v]
        d[v + "s"] = 0.0
    d["MARS"] = np.clip(np.round(np.nan_to_num(df["MARS"].to_numpy(float), nan=1)), 1, 4).astype(int)
    d["XTOT"] = np.clip(np.round(np.nan_to_num(df["XTOT"].to_numpy(float))), 0, None).astype(int)
    d["DSI"] = (np.nan_to_num(df["DSI"].to_numpy(float)) == 1).astype(int)
    d["EIC"] = np.clip(np.round(np.nan_to_num(df["EIC"].to_numpy(float))), 0, 3).astype(int)
    d["n24"] = np.clip(np.round(np.nan_to_num(df["N24"].to_numpy(float))), 0, None).astype(int)
    d["age_head"] = 40
    d["FLPDYR"] = 2015
    return d, clip


def run(df, w):
    import taxcalc as tc
    d, clip = to_tc(df, w)
    rec = tc.Records(data=d, start_year=2015, gfactors=None, weights=None, adjust_ratios=None)
    calc = tc.Calculator(policy=tc.Policy(), records=rec, verbose=False)
    calc.calc_all()
    return {v: calc.array(v) for v in ("iitax", "c00100", "c04800", "eitc", "c07220", "refund", "s006", "c05800")}, clip


def tables(t, agi, g, w):
    out = {}
    for k, gl in enumerate(vs.GROUPS + ["all"]):
        m = (g == k) if gl != "all" else np.ones(len(agi), bool)
        ww = w[m]
        A = (agi[m] * ww).sum()
        out[gl] = dict(agi_bn=float(A / 1e9), tc_agi_bn=float((t["c00100"][m] * ww).sum() / 1e9),
                       taxable_income_bn=float((t["c04800"][m] * ww).sum() / 1e9),
                       tax_before_credits_bn=float((t["c05800"][m] * ww).sum() / 1e9),
                       eitc_bn=float((t["eitc"][m] * ww).sum() / 1e9),
                       net_tax_bn=float((t["iitax"][m] * ww).sum() / 1e9),
                       avg_rate_net=float((t["iitax"][m] * ww).sum() / A) if A else float("nan"))
    tot = out["all"]["net_tax_bn"]
    for gl in vs.GROUPS:
        out[gl]["share_of_net_tax"] = out[gl]["net_tax_bn"] / tot
    return out


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--out", required=True)
    p.add_argument("items", nargs="+")
    a = p.parse_args()
    rec = pd.read_parquet(vs.DATA / "records_train.parquet")
    real = vs.prepare(rec[rec["source"].astype(str) == "PUF"].reset_index(drop=True))
    wr = real["weight"].to_numpy(float)
    agi_r = real["E00100"].to_numpy(float)
    edges = np.array([-np.inf, vs.wq(agi_r, wr, 0.5), vs.wq(agi_r, wr, 0.9), vs.wq(agi_r, wr, 0.99),
                      vs.wq(agi_r, wr, 0.999)])
    res = {}
    out = Path(a.out)
    if out.exists():
        res = json.load(open(out))
    t0 = time.time()
    if "real" not in res:
        t, clip = run(real, wr)
        res["real"] = dict(tables=tables(t, agi_r, vs.group_index(agi_r, edges), wr), clipped=clip)
        print(f"real done {time.time() - t0:.0f}s net tax {res['real']['tables']['all']['net_tax_bn']:.0f}bn", flush=True)
        json.dump(res, open(out, "w"), indent=1)
    for item in a.items:
        lab, path = item.split("=", 1)
        if lab in res:
            continue
        try:
            if path == "validation":
                vrec = pd.read_parquet(vs.DATA / "records_val.parquet")
                df = vs.prepare(vrec[vrec["source"].astype(str) == "PUF"].reset_index(drop=True))
                w = df["weight"].to_numpy(float) * wr.sum() / df["weight"].to_numpy(float).sum()
            else:
                df = vs.prepare(pd.read_parquet(path))
                w = df["weight"].to_numpy(float)
            agi = df["E00100"].to_numpy(float)
            t, clip = run(df, w)
            res[lab] = dict(file=path, tables=tables(t, agi, vs.group_index(agi, edges), w), clipped=clip)
            print(f"{lab} done {time.time() - t0:.0f}s net tax {res[lab]['tables']['all']['net_tax_bn']:.0f}bn", flush=True)
        except Exception as e:  # keep going with the other files
            res[lab] = dict(file=path, error=repr(e))
            print(f"{lab} failed: {e!r}", flush=True)
        json.dump(res, open(out, "w"), indent=1)


if __name__ == "__main__":
    main()
