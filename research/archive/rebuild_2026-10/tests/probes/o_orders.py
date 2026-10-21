"""Probe O: the generation orders compared.

Usage: python o_orders.py NAME        prints the order as comma-separated names
       python o_orders.py --spec FILE writes the scoring specification (json)

Fixed orders (all 40 PUF variables; SCF-only variables are never drawn):
  test3       test 3's order: meta.json generation_order (six categoricals,
              demographics, AGI, the other amounts)
  demolast    T's baseline: test 3's order with the three demographics last
  agifirst    T's fix: AGI, then the rest of demolast
  compfirst   wages, capital gains (long, short), partnership and S
              corporation, dividends, interest, Schedule C, the other amounts
              in test 3's order, then AGI, then the six categoricals, then
              the demographics
  agibysize   AGI, then every other amount by its weighted total of absolute
              dollars on the PUF training records (largest first), then the
              six categoricals, then the demographics
  reverse     test 3's order reversed
  greedy      chosen by o_greedy.py: at each step the variable the model
              predicts best (highest mean log-probability of the true value
              on PUF training records), model seed 1
Orders chosen record by record (o_complete.py --order-mode):
  random        a fresh random order for each record
  entropy_low   at each step, the hidden variable whose predicted
                distribution has the lowest entropy (the model is most sure)
  entropy_high  the same, highest entropy (the model is least sure)
"""
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

DATA = Path("/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/data")
CATS = ["MARS", "DSI", "XTOT", "N24", "EIC", "FDED"]
DEMO = ["AGERANGE", "GENDER", "EARNSPLIT"]
COMPS = ["E00200", "P23250", "P22250", "E26270", "E00600", "E00300", "E00900"]
DYNAMIC = ["random", "entropy_low", "entropy_high"]


def fixed_orders():
    m = json.load(open(DATA / "meta.json"))
    v = {x["name"]: x for x in m["variables"]}
    t3 = [n for n in m["generation_order"] if v[n].get("group") != "scf_only"]
    assert len(t3) == 40, len(t3)
    amounts = [n for n in t3 if n not in CATS + DEMO + ["E00100"]]
    demolast = [n for n in t3 if n not in DEMO] + DEMO
    agifirst = ["E00100"] + [n for n in demolast if n != "E00100"]
    compfirst = COMPS + [n for n in amounts if n not in COMPS] + ["E00100"] + CATS + DEMO
    rec = pd.read_parquet(DATA / "records_train.parquet")
    r = rec[rec["source"].astype(str) == "PUF"]
    w = r["weight"].to_numpy(float)
    size = {n: float(np.nansum(np.abs(r[n].to_numpy(float)) * w)) for n in amounts}
    bysize = sorted(amounts, key=lambda n: -size[n])
    agibysize = ["E00100"] + bysize + CATS + DEMO
    out = dict(test3=t3, demolast=demolast, agifirst=agifirst, compfirst=compfirst,
               agibysize=agibysize, reverse=t3[::-1])
    gf = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/o/greedy_seed01.json")
    if gf.exists():                 # written by o_greedy.py (model seed 1)
        out["greedy"] = json.load(open(gf))["order"]
    for k, o in out.items():
        assert sorted(o) == sorted(t3), k
    return out, size


def main():
    if sys.argv[1] == "--spec":
        o, size = fixed_orders()
        spec = {k: {"mode": "fixed", "order": x} for k, x in o.items()}
        spec["random"] = {"mode": "random", "reps": 5}
        spec["entropy_low"] = {"mode": "entropy_low"}
        spec["entropy_high"] = {"mode": "entropy_high"}
        json.dump(spec, open(sys.argv[2], "w"), indent=1)
        print(json.dumps({k: (",".join(v["order"]) if "order" in v else v["mode"])
                          for k, v in spec.items()}, indent=1))
        print("weighted total of absolute dollars, $bn:",
              {k: round(v / 1e9) for k, v in sorted(size.items(), key=lambda t: -t[1])})
        return
    name = sys.argv[1]
    if name in DYNAMIC:
        print(",".join(fixed_orders()[0]["test3"]))
    else:
        print(",".join(fixed_orders()[0][name]))


if __name__ == "__main__":
    main()
