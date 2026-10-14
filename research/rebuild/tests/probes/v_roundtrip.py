"""Probe V: the PUF training records' own full_v2s tokens decoded back to
dollars (AGI from the decoded parts), written as a file in the generated
format. Scoring it shows what the tokenizer alone loses: the best any model
on these tokens could do. Output stays in scratch (record level)."""
import importlib.util
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
DATA = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s")
spec = importlib.util.spec_from_file_location("g_tok", HERE.parent / "posttrain" / "g_tokenizer.py")
g = importlib.util.module_from_spec(spec)
spec.loader.exec_module(g)
meta = json.load(open(DATA / "meta.json"))
rec = pd.read_parquet(DATA / "records_train.parquet")
puf = (rec["source"].astype(str) == "PUF").to_numpy()
r = rec[puf].reset_index(drop=True)
tok = np.load(DATA / "tokens_train.npy")[puf]
dec = g.decode(tok, meta, r[["source", "year"]], pd.read_parquet(DATA / "quantiles.parquet"), np.random.default_rng(7))
dec = pd.DataFrame(dec) if not isinstance(dec, pd.DataFrame) else dec
keep = [c for c in dec.columns if c in meta["variable_names"]]
out = dec[keep].copy()
# demographic codes unobserved on the real record stay unobserved (NaN)
out["weight"] = r["weight"].to_numpy(float)
out["source"] = "PUF"
out["year"] = 2015
if "E00100" in out:
    out = out.drop(columns=["E00100"])
p = Path(sys.argv[1])
p.parent.mkdir(parents=True, exist_ok=True)
out.to_parquet(p)
print("written", p, out.shape)
