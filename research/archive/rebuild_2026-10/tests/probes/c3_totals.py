"""Probe C: in a generated PUF file (as_complete.py output), compute every
total from its parts (c3_identities.py: E02000, E00600, E01500, E00100), and
set the itemized deductions of non-itemizers (FDED != 1) to zero, since the
model draws them for every record (they are unknown for non-itemizers in
training). Rewrites the file in place; the drawn itemized amounts of
non-itemizers are not kept. If the file carries the drawn PUF weight PUFWT
(a model trained on full_v3cw, probe UW's weighting), each record's weight
becomes its drawn PUFWT rescaled so the weights add to the file's original
total (as uw_compare.py does); the equal weight is kept as weight_equal.

    python c3_totals.py FILE.parquet
"""
import sys
import numpy as np
import pandas as pd
import c3_identities as ci

ITEMIZED = ["E17500", "E18400", "E18500", "E19200", "E19800", "E20100"]

f = sys.argv[1]
df = pd.read_parquet(f)
for c in ci.REPLACE.values():
    assert c in df.columns, c
    assert not df[c].isna().any(), c
ci.add_totals(df)
nonit = df["FDED"].to_numpy(float) != 1
for c in ITEMIZED:
    df.loc[nonit, c] = 0.0
if "PUFWT" in df.columns and "weight_equal" not in df.columns:
    pw = df["PUFWT"].to_numpy(float)
    assert np.isfinite(pw).all() and (pw > 0).all()
    df["weight_equal"] = df["weight"].to_numpy(float)
    df["weight"] = pw * df["weight_equal"].sum() / pw.sum()
df.to_parquet(f, index=False)
w = df["weight"].to_numpy(float)
print(f, len(df), "records; total AGI $%.2ftn" % (np.sum(w * df.E00100.to_numpy(float)) / 1e12),
      "itemizer share %.3f" % (w[~nonit].sum() / w.sum()))
