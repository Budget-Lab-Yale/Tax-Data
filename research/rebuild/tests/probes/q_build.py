"""Probe Q: build and cache the records and targets (no model)."""
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import numpy as np
import q_common as QC
A = QC.setup()
print("families", A.family_groups())
print("income thresholds", QC.income_thresholds())
for s in ("train", "test", "val"):
    r = QC.records(s)
    print(s, len(r), "weight", r["weight"].sum(), "by year", r.groupby("year").size().to_dict())
    for f in QC.FAMILIES:
        print("  ", f, r.groupby("g_" + f)["weight"].sum().round(0).to_dict())
for s in ("train", "test"):
    t = QC.targets(s)
    print(s, "cells", len(t), "zero", int((t["target"] == 0).sum()),
          "fam totals", t.groupby("family")["target"].sum().round(0).to_dict())
