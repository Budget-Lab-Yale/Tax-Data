"""Checks of piece B's completion on the fabricated dataset (b_make_dev.py).

For the completed target records (PUF test records, and SCF test units with
their wealth hidden), compares with the true values:
  1. the planted link: share holding equities, by interest-and-dividend
     group (none / lower half of holders by rank / upper half);
  2. the weight correction: shares holding equities and mean rank position
     of cash, plain completion against weight-corrected against truth.
     The SCF over-samples high wealth, so plain completion should be too
     wealthy and the corrected one close to the truth;
  3. the year: the PUF is at 2015, between the SCF's 2013 and 2016, with
     equity holding rising over time.
All shares weighted by the records' population weights.

Usage: python b_check_dev.py --data $S/dev/b_expB --model $S/dev/b_expB_model
"""

import argparse
import glob
import json
from pathlib import Path

import numpy as np
import pandas as pd


def wmean(x, w):
    return float(np.sum(x * w) / np.sum(w))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data", required=True)
    ap.add_argument("--model", required=True)
    a = ap.parse_args()
    data, mdir = Path(a.data), Path(a.model)
    meta = json.load(open(data / "meta.json"))
    names = [v["name"] for v in meta["variables"]]
    truth = np.load(data / "truth_test.npy")
    rec = pd.read_parquet(data / "records_test.parquet")
    rec_tr = pd.read_parquet(data / "records_train.parquet")
    tr_truth = np.load(data / "truth_train.npy")
    j_eq, j_id, j_cash = names.index("equities"), names.index("int_div"), names.index("cash")
    ncash = meta["variables"][j_cash]["vocab_size"]
    rows_out = []

    def measures(tok, w):
        eq = tok[:, j_eq] > 0
        idv = tok[:, j_id]
        pos = idv > 0
        med = np.median(idv[pos]) if pos.any() else 0
        grp = np.where(~pos, 0, np.where(idv <= med, 1, 2))
        r = dict(eq_share=wmean(eq, w),
                 cash_pos=wmean(tok[:, j_cash] / (ncash - 1), w))
        for g, lab in ((0, "eq_share_no_intdiv"), (1, "eq_share_intdiv_low"),
                       (2, "eq_share_intdiv_high")):
            m = grp == g
            r[lab] = wmean(eq[m], w[m]) if m.any() else np.nan
        return r, med

    for f in sorted(glob.glob(str(mdir / "completed_*_seed*.parquet"))):
        df = pd.read_parquet(f)
        src = df["source"].iloc[0]
        rows = df["row"].to_numpy()
        tok = np.stack([df[f"tok_{n}"].to_numpy() for n in names], axis=1)
        w = df["weight"].to_numpy(float)
        r, _ = measures(tok, w)
        r.update(file=Path(f).name, source=src, kind="completed")
        rows_out.append(r)
        # Truth for the same records (once per file; cheap).
        trow = np.unique(rows)
        t = truth[trow]
        wt = rec["weight"].to_numpy(float)[trow]
        r, _ = measures(t, wt)
        r.update(file=Path(f).name, source=src, kind="truth")
        rows_out.append(r)
    # SCF training records by year, weighted and unweighted, for the year check.
    for yr in (2013, 2016):
        m = ((rec_tr["source"] == "SCF") & (rec_tr["year"] == yr)).to_numpy()
        r, _ = measures(tr_truth[m], rec_tr["weight"].to_numpy(float)[m])
        r.update(file=f"SCF train {yr}, weighted", source="SCF", kind="reference")
        rows_out.append(r)
        r, _ = measures(tr_truth[m], np.ones(m.sum()))
        r.update(file=f"SCF train {yr}, unweighted", source="SCF", kind="reference")
        rows_out.append(r)
    res = pd.DataFrame(rows_out)
    cols = ["file", "kind", "source", "eq_share", "eq_share_no_intdiv", "eq_share_intdiv_low",
            "eq_share_intdiv_high", "cash_pos"]
    pd.set_option("display.width", 200)
    pd.set_option("display.max_columns", 20)
    print(res[cols].round(3).to_string(index=False))
    print("population 2015 equity share (fabrication):", meta.get("population_2015_equity_share"))
    res[cols].to_csv(mdir / "check_dev.csv", index=False)
    for f in sorted(glob.glob(str(mdir / "*_info.json"))):
        i = json.load(open(f))
        print(Path(f).name, "effective draws of K:", i.get("mean_effective_draws_of_K"),
              "completion NLL per record:", (i.get("scores") or {}).get("nll_per_record"))


if __name__ == "__main__":
    main()
