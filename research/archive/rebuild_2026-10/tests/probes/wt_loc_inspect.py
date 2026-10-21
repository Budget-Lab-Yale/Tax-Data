"""Probe WT: list columns and shapes of the files the locate step reads (no records printed)."""
import numpy as np, pandas as pd, glob
C3="/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3"; X="/nfs/roberts/scratch/pi_nrs36/jar335/probes/x"
for f in [f"{C3}/data/full_v3c/records_train.parquet", f"{C3}/data/full_v3c/records_test.parquet",
          f"{C3}/model/h_scf_test_seed01.parquet", f"{C3}/model/h_puf_wealth_seed01.parquet",
          f"{C3}/xscore/h_scf_test_seed01.parquet", f"{X}/model/full_v3x2/h_puf_wealth_seed01.parquet",
          f"{X}/model/full_v3x2/data/records_train.parquet", f"{X}/model/full_v3x2/data/records_test.parquet"]:
    try:
        d=pd.read_parquet(f); print(f, d.shape); print(list(d.columns))
        if 'source' in d: print(d.source.value_counts().to_dict())
        if 'draw' in d: print('draws', d.draw.nunique(), 'rows', d.row.nunique() if 'row' in d else None, 'row max', d.row.max() if 'row' in d else None)
    except Exception as e: print(f, 'ERR', e)
for f in [f"{C3}/data/full_v3c/truth_train.npy", f"{C3}/data/full_v3c/tokens_train.npy", f"{C3}/model/h_scf_test_seed01_tokens.npz"]:
    a=np.load(f); 
    if hasattr(a,'files'): print(f, {k:a[k].shape for k in a.files})
    else: print(f, a.shape, a.dtype)
q=pd.read_parquet(f"{C3}/data/full_v3c/quantiles.parquet"); print('quantiles', q.shape, list(q.columns)); print(q.head(3).to_string())
# row alignment c3 vs x
a=pd.read_parquet(f"{C3}/data/full_v3c/records_train.parquet",columns=['source','year','hh_id','implicate','unit','weight'])
b=pd.read_parquet(f"{X}/model/full_v3x2/data/records_train.parquet",columns=['source','year','hh_id','implicate','unit','weight'])
print('c3 vs x train rows identical:', a.shape==b.shape and bool((a.astype(str).values==b.astype(str).values).all()))
