"""Probe DD (test 4): itemized deductions as two variables, the deduction
claimed on the return and the amount actually paid.

Starting point: probe C's full_v3cz (all of test 4's fixes, with the six
itemized deductions zero for PUF non-itemizers). Rows, split, weights,
wealth and every other variable are unchanged.

Claimed (the six existing columns E17500, E18400, E18500, E19200, E19800,
E20100): what the return claims. PUF rows: the amount for itemizers, zero for
non-itemizers (observed for every PUF record). SCF rows: unknown for all six
(the SCF does not record the return's claim). In C's datasets the SCF's
mortgage interest and real estate tax paid sat in the claimed columns; they
move to the new paid columns.

Paid (two new columns, appended after the wealth columns; placed after the
itemized deductions in the generation order):
  MORTGAGE_INTEREST_PAID  (pairs with E19200)
  REAL_ESTATE_TAX_PAID    (pairs with E18500)
SCF rows: BR's bridge values (tokens and dollars as in full_v3cz's E19200 and
E18500). PUF itemizers: equal to the claimed amount (the PUF's itemized
amounts are before the floors and limits, so for an itemizer paid = claimed
exactly); PUF non-itemizers: unknown. Only these two get a paid variable,
because no source observes the paid amount of the other four deductions
except where it equals the claimed amount.

Two training datasets:
  full_v4dd   paid observed for PUF itemizers (the proposed design)
  full_v4ddp  paid unknown on every PUF row, observed on SCF rows only (the
              link paid = claimed for itemizers is applied outside the model,
              when PUF records are completed); built because with paid
              hidden only for non-itemizers the hiding itself tells the
              model "non-itemizer" in generation, as in probe C
Completion input (PUF records presented as SCF):
  full_v4dd           as above: claimed and form of deduction observed, paid
                      = claimed for itemizers, unknown for non-itemizers
  full_v4dd_hide      the same, but form of deduction and the six claimed
                      amounts hidden on PUF rows (the SCF never has them)

    python dd_prepare.py
"""
import json
import os
import shutil
import time
import numpy as np
import pandas as pd

SRC = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/data/full_v3cz"
Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/dd/data"
PARTS = ("train", "val", "test")
ITEMIZED = ["E17500", "E18400", "E18500", "E19200", "E19800", "E20100"]
PAIRS = [("E19200", "MORTGAGE_INTEREST_PAID"), ("E18500", "REAL_ESTATE_TAX_PAID")]
DESC = {"MORTGAGE_INTEREST_PAID": "Interest paid (mortgage interest), whether or not deducted. PUF itemizers: E19200; "
                                  "PUF non-itemizers: unknown; SCF: BR's bridge value (probe DD, dd_prepare.py)",
        "REAL_ESTATE_TAX_PAID": "Real estate tax paid, whether or not deducted. PUF itemizers: E18500; "
                                "PUF non-itemizers: unknown; SCF: BR's bridge value (probe DD, dd_prepare.py)"}


def log(*a):
    print(time.strftime("%H:%M:%S"), *a, flush=True)


def main():
    t0 = time.time()
    meta = json.load(open(os.path.join(SRC, "meta.json")))
    names = list(meta["variable_names"])
    V0 = len(names)
    fj = names.index("FDED")
    item_tok = [t["token"] for t in meta["variables"][fj]["tokens"] if t["value"] == 1.0][0]
    outs = {k: os.path.join(Q, k) for k in ("full_v4dd", "full_v4ddp", "full_v4dd_hide")}
    for o in outs.values():
        os.makedirs(o, exist_ok=True)
        if os.path.exists(os.path.join(o, "DONE")):
            os.remove(os.path.join(o, "DONE"))

    # meta: claimed columns PUF-only again; two paid columns appended
    m2 = json.loads(json.dumps(meta))
    for c, _ in PAIRS:
        v = m2["variables"][names.index(c)]
        v["group"] = "puf_only"
        v["observed_in"] = ["PUF"]
        v["scf_definition"] = "claimed deduction: not measured by the SCF, unobserved (probe DD)"
    for k, (c, p) in enumerate(PAIRS):
        v = json.loads(json.dumps(meta["variables"][names.index(c)]))
        v["name"] = p
        v["column"] = V0 + k
        v["group"] = "shared"
        v["observed_in"] = ["PUF", "SCF"]
        v["description"] = DESC[p]
        v["scf_definition"] = "BR's bridge value (probe BR), as in full_v3cz's " + c
        m2["variables"].append(v)
        m2["variable_names"].append(p)
    go = m2["generation_order"]
    i = go.index("E20100") + 1
    m2["generation_order"] = go[:i] + [p for _, p in PAIRS] + go[i:]
    m2["dd_note"] = __doc__

    q = pd.read_parquet(os.path.join(SRC, "quantiles.parquet"))
    add = []
    for c, p in PAIRS:
        r = q[q.variable == c].copy()
        log(f"quantile rows copied for {p}: {len(r)} ({sorted(set(zip(r.source.astype(str), r.year)))})")
        r["variable"] = p
        add.append(r)
    q2 = pd.concat([q] + add, ignore_index=True)

    for p in PARTS:
        t = np.load(os.path.join(SRC, f"tokens_{p}.npy")).astype(np.int64)
        r = pd.read_parquet(os.path.join(SRC, f"records_{p}.parquet"))
        scf = (r.source.astype(str) == "SCF").to_numpy()
        puf = ~scf
        item = puf & (r.FDED.to_numpy(float) == 1)
        nonit = puf & ~item
        assert ((t[puf, fj] == item_tok) == item[puf]).all()
        # PUF checks: non-itemizers claim zero in all six; claimed observed on every PUF row
        for c in ITEMIZED:
            j = names.index(c)
            x = r[c].to_numpy(float)
            assert not np.isnan(x[puf]).any(), (p, c)
            assert (x[nonit] == 0).all(), (p, c)
            assert (t[puf, j] >= 0).all(), (p, c)
            assert (t[nonit, j] == meta["variables"][j]["binning"]["zero_token"]).all(), (p, c)
        newt = np.full((len(r), len(PAIRS)), -1, dtype=np.int64)
        for k, (c, pn) in enumerate(PAIRS):
            j = names.index(c)
            x = r[c].to_numpy(float).copy()
            # SCF: bridge value moves from the claimed column to the paid column
            newt[scf, k] = t[scf, j]
            paid = np.full(len(r), np.nan)
            paid[scf] = x[scf]
            paid[item] = x[item]
            newt[item, k] = t[item, j]
            t[scf, j] = -1
            x[scf] = np.nan
            r[c] = x
            r[pn] = paid
            ws = r.weight.to_numpy(float)
            log(f"{p} {pn}: SCF observed {np.mean(newt[scf, k] >= 0):.3f}, PUF itemizers {item.sum()} observed, "
                f"PUF non-itemizers {nonit.sum()} unknown; PUF itemizers paid==claimed in dollars "
                f"{np.mean(paid[item] == r[c].to_numpy(float)[item]):.6f}, in tokens "
                f"{np.mean(newt[item, k] == t[item, j]):.6f}; SCF weighted nonzero share "
                f"{ws[scf][paid[scf] > 0].sum() / ws[scf][~np.isnan(paid[scf])].sum():.3f}")
            assert (newt[item, k] == t[item, j]).all()
            assert ((newt[:, k] < 0) == np.isnan(paid)).all()
            assert (t[scf, j] < 0).all()
        ta = np.concatenate([t, newt], axis=1)
        for j, v in enumerate(m2["variables"]):
            assert ta[:, j].max() < v["vocab_size"], v["name"]
        dt = np.int16
        # full_v4dd
        np.save(os.path.join(outs["full_v4dd"], f"tokens_{p}.npy"), ta.astype(dt))
        np.save(os.path.join(outs["full_v4dd"], f"truth_{p}.npy"), ta.astype(dt))
        r.to_parquet(os.path.join(outs["full_v4dd"], f"records_{p}.parquet"), index=False)
        # full_v4ddp: paid unknown on every PUF row
        tb = ta.copy()
        tb[puf, V0:] = -1
        rb = r.copy()
        for _, pn in PAIRS:
            rb.loc[puf, pn] = np.nan
        np.save(os.path.join(outs["full_v4ddp"], f"tokens_{p}.npy"), tb.astype(dt))
        np.save(os.path.join(outs["full_v4ddp"], f"truth_{p}.npy"), tb.astype(dt))
        rb.to_parquet(os.path.join(outs["full_v4ddp"], f"records_{p}.parquet"), index=False)
        # full_v4dd_hide: completion input, PUF rows' FDED and claimed hidden
        th = ta.copy()
        for c in ["FDED"] + ITEMIZED:
            th[puf, names.index(c)] = -1
        np.save(os.path.join(outs["full_v4dd_hide"], f"tokens_{p}.npy"), th.astype(dt))
        np.save(os.path.join(outs["full_v4dd_hide"], f"truth_{p}.npy"), ta.astype(dt))
        r.to_parquet(os.path.join(outs["full_v4dd_hide"], f"records_{p}.parquet"), index=False)
        log(p, "rows", len(r), "SCF", scf.sum(), "PUF itemizers", item.sum(), "non-itemizers", nonit.sum(),
            "tokens", ta.shape)

    for k, o in outs.items():
        mm = json.loads(json.dumps(m2))
        mm["dataset"] = k
        mm["description"] = "Probe DD (test 4): " + k + " built from full_v3cz by dd_prepare.py. " + meta["description"]
        json.dump(mm, open(os.path.join(o, "meta.json"), "w"), indent=1)
        q2.to_parquet(os.path.join(o, "quantiles.parquet"), index=False)
        with open(os.path.join(o, "DONE"), "w") as f:
            f.write(f"{time.strftime('%Y-%m-%d %H:%M:%S')} job {os.environ.get('SLURM_JOB_ID')} dd_prepare.py\n")
    log(f"done in {time.time() - t0:.0f}s")


if __name__ == "__main__":
    main()
