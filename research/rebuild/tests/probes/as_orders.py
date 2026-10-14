"""Probe AS: generation orders for the full_v2s model.

Usage: python as_orders.py NAME   prints the order as comma-separated names
  sizefirst  every amount (the income items, adjustments, other amounts and
             AGI_REMAINDER) in order of its weighted total of absolute dollars
             on the PUF training records, largest first; then the six
             categoricals; then the demographics
  random     a fresh random order per record (as_complete.py --order-mode
             random); the printed order is only the list of variables drawn
"""
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

DATA = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s")
CATS = ["MARS", "DSI", "XTOT", "N24", "EIC", "FDED"]
DEMO = ["AGERANGE", "GENDER", "EARNSPLIT"]


def orders():
    m = json.load(open(DATA / "meta.json"))
    v = {x["name"]: x for x in m["variables"]}
    base = [n for n in m["generation_order"] if v[n].get("group") != "scf_only"]
    assert len(base) == 40 and "AGI_REMAINDER" in base and "E00100" not in base
    amounts = [n for n in base if n not in CATS + DEMO]
    rec = pd.read_parquet(DATA / "records_train.parquet")
    r = rec[rec["source"].astype(str) == "PUF"]
    w = r["weight"].to_numpy(float)
    size = {n: float(np.nansum(np.abs(r[n].to_numpy(float)) * w)) for n in amounts}
    sizefirst = sorted(amounts, key=lambda n: -size[n]) + CATS + DEMO
    assert sorted(sizefirst) == sorted(base)
    return {"sizefirst": sizefirst, "random": base}, size


def spec(path):
    """Scoring specification (as_complete.py --mode score): several fixed
    orders and five draws of a random order per record."""
    o, _ = orders()
    sf = o["sizefirst"]
    am = [n for n in sf if n not in CATS + DEMO]
    sp = {"sizefirst": sf, "reverse": sf[::-1], "catsfirst": CATS + DEMO + am,
          "remainder_first": ["AGI_REMAINDER"] + [n for n in sf if n != "AGI_REMAINDER"],
          "remainder_last": [n for n in am if n != "AGI_REMAINDER"] + ["AGI_REMAINDER"] + CATS + DEMO}
    out = {k: {"mode": "fixed", "order": v} for k, v in sp.items()}
    out["random"] = {"mode": "random", "reps": 5}
    json.dump(out, open(path, "w"), indent=1)


if __name__ == "__main__":
    if sys.argv[1] == "--spec":
        spec(sys.argv[2])
        sys.exit(0)
    o, size = orders()
    if len(sys.argv) > 2 and sys.argv[2] == "--sizes":
        print({k: round(s / 1e9) for k, s in sorted(size.items(), key=lambda t: -t[1])}, file=sys.stderr)
    print(",".join(o[sys.argv[1]]))
