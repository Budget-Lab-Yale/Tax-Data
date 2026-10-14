"""Probe DD: copy of c3_table.py (probe AS) with the itemizer share and the six deductions added. Probe AS: summary tables from as_compare.py's json: mean over model seeds
for each arm (labels ARM_mN), with the range over seeds in brackets.

Usage: python as_table.py <compare json>
"""
import json
import re
import sys

import numpy as np

res = json.load(open(sys.argv[1]))
arms = {}
for k, v in res.items():
    if not isinstance(v, dict) or "total_agi_tn" not in v:
        continue
    m = re.match(r"(.+)_m(\d+)$", k)
    arms.setdefault(m.group(1) if m else k, []).append(v)
GROUPS = ["bottom 50", "50-90", "90-99", "99-99.9", "top 0.1"]


def cell(vals, fmt):
    vals = [x for x in vals if x is not None]
    if len(vals) == 1:
        return fmt.format(vals[0])
    return fmt.format(np.mean(vals)) + " [" + fmt.format(min(vals)) + "–" + fmt.format(max(vals)) + "]"


rows = [("Total AGI ($tn)", lambda v: v["total_agi_tn"], "{:.2f}"),
        ("Weight above true 99th pct (%)", lambda v: 100 * v["w_above_p99"], "{:.2f}"),
        ("Weight above true 99.9th pct (%)", lambda v: 100 * v["w_above_p999"], "{:.3f}"),
        ("Top 1% share of AGI (%)", lambda v: 100 * v["share_above_p99"], "{:.1f}"),
        ("Top 0.1% share of AGI (%)", lambda v: 100 * v["share_above_p999"], "{:.1f}"),
        ("Remainder, all returns ($bn)", lambda v: v["remainder_total_bn"], "{:,.0f}"),
        ("Itemizers, all returns (%)", lambda v: 100 * v.get("itemizer_share_all", float("nan")), "{:.1f}")]
for key, lab in [("ded_medical_bn", "Medical"), ("ded_state_local_income_sales_bn", "State and local income or sales tax"),
                 ("ded_real_estate_tax_bn", "Real estate tax"), ("ded_interest_bn", "Interest paid"),
                 ("ded_charity_cash_bn", "Charity, cash"), ("ded_charity_noncash_bn", "Charity, non-cash")]:
    rows.append((f"Claimed {lab}, all returns ($bn)", (lambda v, key=key: v["totals_bn"][key[:-3]]), "{:,.0f}"))
    for g in GROUPS:
        rows.append((f"Claimed {lab}, {g} ($bn)", (lambda v, key=key, g=g: v["groups"][g][key]), "{:,.0f}"))
for key, lab in [("agi_bn", "AGI"), ("wages_bn", "Wages"), ("cap_gains_bn", "Capital gains"),
                 ("partnership_scorp_bn", "Partnership and S corp"), ("dividends_bn", "Dividends"),
                 ("business_schC_bn", "Schedule C"), ("interest_bn", "Interest"),
                 ("schedule_e_bn", "Schedule E total"), ("pensions_ira_bn", "Pensions and IRA"),
                 ("six_components_bn", "Six components"), ("agi_minus_six_bn", "AGI less six components"),
                 ("remainder_bn", "Remainder")]:
    for g in GROUPS:
        rows.append((f"{lab}, {g} ($bn)", (lambda v, key=key, g=g: v["groups"][g][key]), "{:,.0f}"))
for key, lab, f in [("remainder_share_neg", "Remainder negative (% of group)", 100),
                    ("remainder_share_zero", "Remainder zero (% of group)", 100),
                    ("remainder_p5", "Remainder 5th pct ($)", 1), ("remainder_p50", "Remainder median ($)", 1),
                    ("remainder_p95", "Remainder 95th pct ($)", 1),
                    ("itemizer_share", "Itemizers (% of group)", 100), ("joint_share", "Joint filers (% of group)", 100),
                    ("wages_and_capgains_share", "Both wages and capital gains (% of group)", 100)]:
    for g in GROUPS:
        rows.append((f"{lab}, {g}", (lambda v, key=key, g=g, f=f: f * v["groups"][g][key]),
                     "{:.1f}" if f == 100 else "{:,.0f}"))
names = list(arms)
print("| | " + " | ".join(f"{n} ({len(arms[n])})" for n in names) + " |")
print("|---" * (len(names) + 1) + "|")
for lab, fn, fmt in rows:
    print(f"| {lab} | " + " | ".join(cell([fn(v) for v in arms[n]], fmt) for n in names) + " |")
