"""Piece F: plumbing check of f_full.a_targets on the full model's data.
The PUF training records' own values, pushed through the same path as
draws, must reproduce A's PUF ground-truth targets exactly."""
import sys
from pathlib import Path
import numpy as np
import pandas as pd
sys.path.insert(0, str(Path(__file__).resolve().parent))
import f_full as FF
data = Path(sys.argv[1] if len(sys.argv) > 1 else FF.DEFAULTS["data"])
meta = __import__("json").load(open(data / "meta.json"))
from b_model import slot_columns, slot_variables
names = [v["name"] for v in slot_variables(meta)]
cols = slot_columns(meta)
rec = pd.read_parquet(data / "records_train.parquet")
rows = np.flatnonzero(rec["source"].astype(str).to_numpy() == "PUF")
tok = np.load(data / "tokens_train.npy")[rows][:, cols].astype(np.int64)
tr = np.load(data / "truth_train.npy")[rows][:, cols].astype(np.int64)
tok = np.where(tok >= 0, tok, tr)
dol = np.full(tok.shape, np.nan)
for j, n in enumerate(names):
    if n in rec.columns:
        dol[:, j] = rec[n].to_numpy(float)[rows]
W = rec["weight"].to_numpy(float)[rows]
for s in ("core", "full"):
    idx, tab = FF.a_targets(data, s, tok, dol, names)
    t = tab["target"].to_numpy()
    tot = np.bincount(idx[idx >= 0], weights=np.broadcast_to(W[:, None], idx.shape)[idx >= 0], minlength=len(t))
    p = (tab["set"] == "puf").to_numpy()
    print(s, "cells", len(t), "width", idx.shape[1], "unassigned entries", int((idx < 0).sum()),
          "| PUF set max abs diff", float(np.abs(tot[p] - t[p]).max()),
          "| wealth cells with no target", int(np.isnan(t[~p]).sum()))
