"""Probe T: markdown tables for T_REPORT.md from a t_compare.py json.
Usage: python t_table.py <compare json> <label,label,...> [groups]
Prints the top-of-distribution rows for the labels given, and with `groups`
the totals by AGI group of AGI and its main components ($bn)."""
import json
import sys

r = json.load(open(sys.argv[1]))
labs = ["PUF training records"] + [x for x in sys.argv[2].split(",") if x]
missing = [l for l in labs if l not in r]
if missing:
    print("missing:", missing)
labs = [l for l in labs if l in r]
rows = [("Total AGI", "total_agi_tn", "${:.2f}tn"),
        ("Weight above true 99th pct", "w_above_p99", "{:.2%}"),
        ("Weight above true 99.9th pct", "w_above_p999", "{:.3%}"),
        ("AGI share above true 99th pct", "share_above_p99", "{:.1%}"),
        ("AGI share above true 99.9th pct", "share_above_p999", "{:.1%}")]
print("| | " + " | ".join(labs) + " |")
print("|---" * (len(labs) + 1) + "|")
for name, k, f in rows:
    print(f"| {name} | " + " | ".join(f.format(r[l][k]) for l in labs) + " |")
if len(sys.argv) > 3:
    groups = ["bottom 50", "50-90", "90-99", "99-99.9", "top 0.1"]
    for c, nm in [("agi_bn", "AGI"), ("wages_bn", "Wages"), ("cap_gains_bn", "Capital gains (long + short term)"),
                  ("partnership_scorp_bn", "Partnership and S corporation income"),
                  ("dividends_bn", "Ordinary dividends"), ("interest_bn", "Taxable interest"),
                  ("business_schC_bn", "Schedule C business income")]:
        print(f"\n{nm}, $bn, by AGI group (true PUF thresholds)\n")
        print("| AGI group | " + " | ".join(labs) + " |")
        print("|---" * (len(labs) + 1) + "|")
        for g in groups:
            print(f"| {g} | " + " | ".join(f"{r[l]['groups'][g][c]:,.0f}" for l in labs) + " |")
