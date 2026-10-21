"""Checks on piece H's development outputs (fabricated and expB)."""
import json
from pathlib import Path

import numpy as np
import pandas as pd

P = Path("/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev")
for d in (P / "h_model_fake", P / "h_expB"):
    for info_f in sorted(d.glob("h_*_info.json")):
        info = json.load(open(info_f))
        base = info_f.name[:-len("_info.json")]
        df = pd.read_parquet(d / f"{base}.parquet")
        z = np.load(d / f"{base}_tokens.npz")
        tok = z["tokens"]
        names = list(z["names"])
        fill = list(z["fill"])
        fj = [names.index(x) for x in fill]
        other = [j for j in range(len(names)) if j not in fj]
        print(f"\n{d.name}/{base}: rows {len(df)}, tokens {tok.shape}, fill {len(fill)}, "
              f"decoded {info['decoded']} ({info.get('decode_error')}), allowed "
              f"{info['allowed_codes']} ({info.get('allowed_error')}), order rule "
              f"{info['order_rule']}")
        print(f"  filled slots all >= 0: {bool((tok[:, :, fj] >= 0).all())}; "
              f"share of other slots hidden: {float((tok[:, :, other] < 0).mean()):.3f}")
        if info["decoded"]:
            na = {c: int(df[c].isna().sum()) for c in fill if c in df}
            print(f"  NaN dollars in filled variables: {sum(na.values())}; "
                  f"first fills: {fill[:5]}")
            print(df[[c for c in fill[:4] if c in df]].describe().loc[["mean", "50%", "max"]])
        if info.get("scores"):
            print(f"  completion NLL per record {info['scores']['nll_per_record']:.3f} over "
                  f"{info['scores']['n_records_scored']} records")
        print("  columns:", list(df.columns[:8]), "...", len(df.columns))
