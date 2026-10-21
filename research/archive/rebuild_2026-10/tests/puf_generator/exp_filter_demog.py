"""Write copies of files restricted to records with demographics (AGERANGE
present), for the hypothesis-3 experiment (EXPERIMENTS.md).
Usage: python exp_filter_demog.py out_dir label=path [label=path ...]
Writes out_dir/<label>.parquet; prints record counts only."""
import sys
from pathlib import Path
import pandas as pd
out = Path(sys.argv[1]); out.mkdir(parents=True, exist_ok=True)
for pair in sys.argv[2:]:
    lab, path = pair.split("=", 1)
    d = pd.read_parquet(path)
    k = d[d.AGERANGE != -1]
    k.to_parquet(out / f"{lab}.parquet", index=False)
    print(f"{lab}: kept {len(k)} of {len(d)} records with demographics")
