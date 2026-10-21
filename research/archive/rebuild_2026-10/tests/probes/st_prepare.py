"""Probe ST: the version 2 dataset with the SCF 2016 wave removed from the
training and validation splits (PUF 2015 and SCF 2013 only). The test split,
meta.json and the quantile functions are copied unchanged (the quantile
functions of every source-year stay available for decoding). Rows are
dropped from records, tokens and truth together, so they stay aligned."""
import shutil
from pathlib import Path
import numpy as np
import pandas as pd

SRC = Path("/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/data")
OUT = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/st/data_no2016")
OUT.mkdir(parents=True, exist_ok=True)
for f in ["meta.json", "quantiles.parquet"]:
    shutil.copy2(SRC / f, OUT / f)
for split in ["train", "val", "test"]:
    rec = pd.read_parquet(SRC / f"records_{split}.parquet")
    tok = np.load(SRC / f"tokens_{split}.npy")
    tru = np.load(SRC / f"truth_{split}.npy")
    assert len(rec) == len(tok) == len(tru)
    src = rec["source"].astype(str).to_numpy()
    yr = rec["year"].to_numpy().astype(int)
    print(split, "rows by source-year:",
          pd.Series(list(zip(src, yr))).value_counts().to_dict(), flush=True)
    if split == "test":
        keep = np.ones(len(rec), bool)
    else:
        keep = ~((src == "SCF") & (yr == 2016))
    rec.iloc[np.flatnonzero(keep)].reset_index(drop=True).to_parquet(OUT / f"records_{split}.parquet")
    np.save(OUT / f"tokens_{split}.npy", tok[keep])
    np.save(OUT / f"truth_{split}.npy", tru[keep])
    print(split, f"kept {keep.sum()} of {len(keep)} rows", flush=True)
(OUT / "DONE").write_text("probe ST: full_v2 with SCF 2016 removed from train and val\n")
print("done", flush=True)
