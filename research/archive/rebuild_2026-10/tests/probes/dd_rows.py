"""Probe DD: the two completed-wealth tables in DD_REPORT.md, from
$Q/dd_wealth.json (dd_wealth.py). Markdown to stdout.

    python dd_rows.py
"""
import json

import numpy as np

Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/dd"
res = json.load(open(f"{Q}/dd_wealth.json"))
COLS = ["C, deductions zero", "C, deductions unknown",
        "DD paid observed for itemizers; claimed shown", "DD paid observed for itemizers; claimed hidden",
        "DD paid SCF only; claimed shown", "DD paid SCF only; claimed hidden"]


def fm(st, v):
    if st in ("own_home", "has_mortgage"):
        return f"{100 * v:.0f}%"
    return f"${v / 1e6:.1f}m" if abs(v) >= 1e6 else f"${v / 1e3:,.0f}k"


def cell(vals, st):
    if not vals:
        return "pending"
    if len(vals) == 1:
        return fm(st, vals[0])
    return f"{fm(st, np.mean(vals))} [{fm(st, min(vals))}–{fm(st, max(vals))}]"


print("| | SCF 2015 | " + " | ".join(COLS) + " |")
print("|---" * (len(COLS) + 2) + "|")
print("| Training seeds | | " + " | ".join(str(len(res.get(c, {}))) for c in COLS) + " |")
G = {"All": "all returns", "Bottom 50%": "bottom half", "50-90%": "50th–90th", "90-99%": "90th–99th",
     "99-99.9%": "99th–99.9th", "Top 0.1%": "top 0.1%"}
ROWS = [("own_home", "Own a home", ["All", "Bottom 50%", "50-90%", "90-99%", "99-99.9%", "Top 0.1%"]),
        ("home_value", "Mean home value", ["All", "Bottom 50%", "50-90%", "Top 0.1%"]),
        ("has_mortgage", "Have a mortgage", ["All", "Bottom 50%", "50-90%"]),
        ("mortgage", "Mean mortgage debt", ["All", "Bottom 50%", "Top 0.1%"]),
        ("nw_mean", "Mean net worth", ["All", "Bottom 50%", "50-90%", "90-99%", "99-99.9%", "Top 0.1%"]),
        ("nw_median", "Median net worth", ["All", "50-90%", "Top 0.1%"])]
for st, name, gs in ROWS:
    for g in gs:
        row = [cell([res["SCF 2015"]["agi"][g][st]], st)]
        row += [cell([v["agi"][g][st] for v in res.get(c, {}).values()], st) for c in COLS]
        print(f"| {name}, {G[g]} | " + " | ".join(row) + " |")
print()
print("| PUF non-itemizers | " + " | ".join(COLS) + " |")
print("|---" * (len(COLS) + 1) + "|")
for st, name in (("own_home", "Own a home"), ("home_value", "Mean home value"), ("has_mortgage", "Have a mortgage"),
                 ("mortgage", "Mean mortgage debt"), ("nw_mean", "Mean net worth"), ("nw_median", "Median net worth")):
    print(f"| {name} | " + " | ".join(cell([v["band"]["Non-itemizer"][st] for v in res.get(c, {}).values()], st)
                                     for c in COLS) + " |")
print()
for c in COLS + ["Translated SCF (X)", "Bridge (BR)"]:
    lo = {st: min(v["band"][b][st] for v in res.get(c, {}).values() for b in ("$1-5k", "$5-10k", "$10-20k", "Above $20k"))
          for st in ("own_home", "has_mortgage")} if res.get(c) else None
    print(f"- {c}: bands with positive interest, lowest own-home {lo and round(100 * lo['own_home'])}%, "
          f"lowest have-mortgage {lo and round(100 * lo['has_mortgage'])}%")
s = res["SCF 2015"]["band"]
print("- SCF bands with positive paid interest: own home " +
      ", ".join(f"{100 * s[b]['own_home']:.0f}%" for b in ("$1-5k", "$5-10k", "$10-20k", "Above $20k")) +
      "; have a mortgage " + ", ".join(f"{100 * s[b]['has_mortgage']:.0f}%" for b in ("$1-5k", "$5-10k", "$10-20k", "Above $20k")))
