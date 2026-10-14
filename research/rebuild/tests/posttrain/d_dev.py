"""Test 3, piece D: development data in $P/dev/d/ for a short trial of
d_tune.py before pieces A and B deliver.

Takes the first --n records (A's records file if it exists, otherwise built
with a_common.build_records), draws K completions from the reference network
with a_common.draw_set, recomputes the targets on these records alone and
calibrates the draws to them with piece B's calibrate(). Writes
records.parquet, targets.parquet, draws.npz, weights.npz, calib.json.
"""

import argparse
import json
import sys
from pathlib import Path

import numpy as np

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "scf_joint"))
sys.path.insert(0, str(HERE))

import a_common as A  # noqa: E402


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n", type=int, default=3000)
    ap.add_argument("--k", type=int, default=8)
    ap.add_argument("--out", default=str(A.P / "dev/d"))
    a = ap.parse_args()
    out = Path(a.out)
    out.mkdir(parents=True, exist_ok=True)
    if (A.PA / "records.parquet").exists():
        recs = A.load_records()
        A.log("using A's records.parquet")
    else:
        recs, notes = A.build_records()
        A.log(f"built records: {notes}")
    # every 13th record, so both waves and many households are present
    sel = np.arange(0, len(recs), max(1, len(recs) // a.n))[:a.n]
    recs = recs.iloc[sel].reset_index(drop=True)
    recs["rec"] = np.arange(len(recs))
    recs.to_parquet(out / "records.parquet", index=False)
    targ = A.targets(recs)
    targ.to_parquet(out / "targets.parquet", index=False)
    model, ctx = A.load_reference("cuda", recs=recs)
    d = A.draw_set(model, ctx, a.k, seed=777)
    np.savez(out / "draws.npz", **d)
    from b_calibrate import calibrate
    w, lam, rep = calibrate(d["tokens"], recs, targ)
    np.savez(out / "weights.npz", w=w, **{"lambda": lam})
    rep.pop("cell_status", None)
    with open(out / "calib.json", "w") as f:
        json.dump(rep, f, indent=1, default=float)
    A.log(f"dev data: {len(recs)} records, K={a.k}; calibration KL "
          f"{rep['kl_nats_per_unit_weight']:.4f}, status {rep['status_counts']}")


if __name__ == "__main__":
    main()
