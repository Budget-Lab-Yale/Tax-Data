"""Probe Y, step 4: test 2's experiment A dataset with half 2's incomes
replaced by their translation into half 1's concepts (draw 0 of
y_run.py's likelihood-proportional translation).

    python y_prepare.py unrep      -> $Q/data/expA_unreptr/

Uses test 2's a_prepare.build_dataset unchanged except for three
substitutions made here: output under $Q instead of test 2's scratch, and
an alteration step that applies the original distortion to half 1 and then
puts half 2's translated incomes in place (half 2's own values stay in
orig_<income>). Same seeds, so the same households, splits and tie draws.
"""
import os
import sys

import numpy as np
import pandas as pd

T2 = "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint"
sys.path.insert(0, T2)
import a_tokenizer as tk  # noqa: E402
import a_prepare as ap  # noqa: E402

Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/y"
base = sys.argv[1]
rule = sys.argv[2] if len(sys.argv) > 2 else ""   # "" (proportional) or "random"
tk.S = Q
tk.DATA = os.path.join(Q, "data")
ap.S = Q
os.makedirs(os.path.join(Q, "data"), exist_ok=True)
_orig_alter = ap.alter


def alter(df, variant, info):
    assert variant == base + "tr" + rule, variant
    df = _orig_alter(df, base, info)
    tr = pd.read_parquet(os.path.join(Q, f"y_translated_draw0_{base}" + ("_" + rule if rule else "") + ".parquet"))
    h2 = (df.half == 2).to_numpy()
    m = df.loc[h2, ["hh_id", "implicate", "unit"]].merge(tr, on=["hh_id", "implicate", "unit"], how="left")
    assert len(m) == h2.sum() and not m[ap.INC].isna().any().any(), "translated rows do not line up with half 2"
    for c in ap.INC:
        df.loc[h2, c] = m[c].to_numpy()
    info["translation"] = (f"half 2's seven incomes replaced by draw 0 of probe Y's translation ({rule or 'proportional'} switching) into half 1's "
                           f"concepts ({base} distortion); half 2's own values in orig_<income>")
    return df


ap.alter = alter
ap.build_dataset(f"expA_{base}tr{rule}")
open(os.path.join(Q, "data", f"expA_{base}tr{rule}", "DONE"), "w").write("probe Y\n")
print("done", flush=True)
