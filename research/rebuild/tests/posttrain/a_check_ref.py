"""Test 3, piece A: summary checks of the reference draws (CPU).

For each available draw set: weighted mean and total net worth by wave
(truth as reported, truth decoded from its tokens, reference draws); the
share of draw weight in zero cells; positive cells that no draw reaches;
and the reference's target error by family before any calibration (absolute
error in cell weight as a share of the group's total weight). Writes
$P/a/ref_checks.json.
"""
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent))
import a_common as A          # noqa: E402

recs = A.load_records()
tab = A.load_targets()
W = recs["weight"].to_numpy()
tgt = tab["target"].to_numpy()
blk_tot = tab.groupby(["block", "variable"])["target"].transform("sum").to_numpy()
out = {}
for name in ["dev", "ref", "ref_b", "ref_k128"]:
    fn = {"dev": "dev_draws.npz", "ref": "ref_draws.npz", "ref_b": "ref_draws_b.npz",
          "ref_k128": "ref_draws_k128.npz"}[name]
    if not (A.PA / fn).exists():
        continue
    d = A.load_draws(name)
    K = d["tokens"].shape[1]
    idx = A.contrib_index(d["tokens"], recs)
    tot = A.cell_totals(idx, np.repeat(W / K, K))
    reach = np.bincount(idx.reshape(-1), minlength=A.n_cells())
    pos = tgt > 0
    err = np.abs(tot - tgt) / np.where(blk_tot > 0, blk_tot, np.nan)
    fam = {}
    for f in A.FAMILIES:
        m = (tab["family"] == f).to_numpy()
        fam[f] = dict(mean_abs_err_share=float(np.nanmean(err[m])),
                      max_abs_err_share=float(np.nanmax(err[m])))
    r = dict(K=int(K), seed=int(d["seed"]),
             mean_logp_ref_per_record=float(np.average(d["logp_ref"].mean(1), weights=W)),
             draw_weight_share_in_zero_cells=float(tot[~pos].sum() / tot.sum()),
             positive_cells_reached_by_no_draw=int((pos & (reach == 0)).sum()),
             positive_cells_reached_by_fewer_than_5_draws=int((pos & (reach < 5)).sum()),
             target_error_by_family_before_calibration=fam)
    if name in ("ref", "ref_b"):
        dol = pd.read_parquet(A.PA / ("ref_dollars.parquet" if name == "ref" else "ref_dollars_b.parquet"))
        nan = int(dol.isna().sum().sum())
        if nan:
            raise SystemExit(f"{name}: {nan} missing dollar values")
        nw = dol["nw"].to_numpy().reshape(len(recs), K)
        r["nan_dollars"] = nan
        r["nw_by_wave"] = {}
        for w in A.WAVES:
            m = (recs["year"] == w).to_numpy()
            r["nw_by_wave"][w] = dict(
                truth_reported_mean=float(np.average(recs["nw"][m], weights=W[m])),
                truth_decoded_mean=float(np.average(recs["nw_dec"][m], weights=W[m])),
                reference_mean=float(np.average(nw[m].mean(1), weights=W[m])),
                truth_reported_total_trillions=float((recs["nw"][m] * W[m]).sum() / 1e12),
                reference_total_trillions=float((nw[m].mean(1) * W[m]).sum() / 1e12))
    out[name] = r
    print(name, json.dumps(r, indent=1), flush=True)
with open(A.PA / "ref_checks.json", "w") as f:
    json.dump(out, f, indent=1)
