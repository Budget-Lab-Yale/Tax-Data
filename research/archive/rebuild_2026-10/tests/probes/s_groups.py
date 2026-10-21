"""Probe S: totals ($bn) of AGI and its main components by AGI group (the PUF's
own thresholds), averaged over every seed and sampling draw of each recipe,
for one generation order (or the given-AGI completion).

Usage: python s_groups.py <compare_all.json> <order: agifirst|demolast|givenagi> <recipe,...>
Aggregates only.
"""
import json
import re
import sys

import numpy as np

r = json.load(open(sys.argv[1]))
order = sys.argv[2]
recipes = sys.argv[3].split(",")
groups = ["bottom 50", "50-90", "90-99", "99-99.9", "top 0.1"]
cols = [("agi_bn", "AGI"), ("wages_bn", "Wages"), ("cap_gains_bn", "Capital gains"),
        ("partnership_scorp_bn", "Partnership and S corporation"),
        ("dividends_bn", "Ordinary dividends"), ("interest_bn", "Taxable interest"),
        ("business_schC_bn", "Schedule C")]
labs = {}
for v in recipes:
    pat = re.compile(rf"^({v}_s\d+_{order}|noise_{order}4x_{v}_s\d+_g\d+_seed\d+)$")
    labs[v] = [k for k in r if pat.match(k)]
print("records averaged: " + ", ".join(f"{v} {len(x)}" for v, x in labs.items()))
head = ["PUF"] + recipes
print("| Component | AGI group | " + " | ".join(head) + " |")
print("|---" * (len(head) + 2) + "|")
for c, nm in cols:
    for g in groups:
        vals = [r["PUF training records"]["groups"][g][c]]
        for v in recipes:
            vals.append(np.mean([r[k]["groups"][g][c] for k in labs[v]]) if labs[v] else np.nan)
        print(f"| {nm} | {g} | " + " | ".join(f"{x:,.0f}" for x in vals) + " |")
