"""Piece F: how many draws fall in each (wave, family, group) block of the
S1 targets, against the number of target cells in the block, at 1x, 4x, 8x,
32x and 64x the real record count; and the same for the real records.
Writes $P/f/s1/block_sizes.csv and prints a summary."""
import os
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import f_common as F               # noqa: E402
import f_run                       # noqa: E402
from f_common import A, FAMILIES, WAVES  # noqa: E402

recs = A.load_records()
tab = pd.read_parquet(os.path.join(F.PF, "s1", "targets.parquet"))
FL = F.FLayout("s1")
pos = tab[(tab["target"] > 0) & (tab["block"] >= 0)].groupby("block").size()
real = recs.groupby("blk_inc").size()   # placeholder to keep the frame shape
rows = []
L = A.layout()
# real records per block
rb = {}
for f in FAMILIES:
    for b, n in recs.groupby("blk_" + f).size().items():
        rb[b] = n
for mult in (1, 4, 8, 32, 64):
    try:
        z = f_run.load_s1(mult)
    except Exception as e:      # noqa: BLE001
        print(f"{mult}x: {e}")
        continue
    wave_i, vt, dm, tot, g = f_run.draw_arrays("s1", z, recs)
    G = FL.groups(vt, dm, g)
    for fi, f in enumerate(FAMILIES):
        blk = FL.off[wave_i, fi] + G[:, fi]
        cnt = np.bincount(blk, minlength=L.n_blocks)
        for b in range(L.n_blocks):
            if L.blocks.family[b] != f:
                continue
            rows.append({"mult": mult, "block": b, "wave": int(L.blocks.wave[b]), "family": f,
                         "group": L.blocks.group_label[b], "draws": int(cnt[b]),
                         "real_records": int(rb.get(b, 0)), "positive_cells": int(pos.get(b, 0))})
d = pd.DataFrame(rows)
d.to_csv(os.path.join(F.PF, "s1", "block_sizes.csv"), index=False)
for mult, x in d.groupby("mult"):
    small = x[x["draws"] < x["positive_cells"]]
    print(f"{mult}x: {len(small)} of {len(x)} blocks have fewer draws than positive target cells; "
          f"they hold {small['positive_cells'].sum()} cells")
    print(small.sort_values("draws").head(12).to_string(index=False))
