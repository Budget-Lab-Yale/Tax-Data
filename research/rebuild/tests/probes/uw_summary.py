"""Probe UW: summary tables from uw_compare.py output and the training run files.

Usage: python uw_summary.py <compare json>
Labels in the compare json: <kind>_<arm>_s<seed>[_g<draw>], kind wfirst,
random or given. Means over every seed and draw of an arm; per-model values
for the top shares. Aggregates only.
"""
import json
import re
import sys
from pathlib import Path

import numpy as np

Q = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw")
r = json.load(open(sys.argv[1]))
P = r["PUF training records"]
SIX = ["wages", "cap_gains", "partnership_scorp", "dividends", "interest", "business_schC"]
GROUPS = ["bottom 50", "50-90", "90-99", "99-99.9", "top 0.1"]
pat = re.compile(r"^(wfirst|wfirstsup|random|given|givensup)_(uw[ABCD])_s(\d+)(?:_g(\d+))?$")
cols = {}
for k in r:
    m = pat.match(k)
    if m:
        cols.setdefault((m.group(2), m.group(1)), []).append(k)
order = ([(a, "wfirst") for a in ("uwA", "uwB", "uwC", "uwD")] + [("uwA", "random")]
         + [(a, "wfirstsup") for a in ("uwA", "uwB", "uwC", "uwD")]
         + [(a, "given") for a in ("uwA", "uwB", "uwC", "uwD")]
         + [(a, "givensup") for a in ("uwA", "uwB", "uwC", "uwD")])
order = [c for c in order if c in cols]
print("files averaged: " + ", ".join(f"{a} {k}: {len(cols[(a, k)])}" for a, k in order))


def val(d, key):
    if key == "agi_top01":
        return d["groups"]["top 0.1"]["agi_bn"]
    if key.startswith("six_"):
        g = key[4:]
        return sum(d["groups"][g][c + "_bn"] for c in SIX)
    if "|" in key:
        c, g = key.split("|")
        return d["groups"][g][c]
    return d[key]


rows = [("Total AGI, $tn", "total_agi_tn", "{:.2f}"),
        ("Weight above true 99th pct", "w_above_p99", "{:.2%}"),
        ("Weight above true 99.9th pct", "w_above_p999", "{:.3%}"),
        ("Top 1% share of AGI", "share_above_p99", "{:.1%}"),
        ("Top 0.1% share of AGI", "share_above_p999", "{:.1%}"),
        ("AGI in top 0.1%, $bn", "agi_top01", "{:,.0f}"),
        ("Six components in top 0.1%, $bn", "six_top 0.1", "{:,.0f}"),
        ("Six components in 99th-99.9th, $bn", "six_99-99.9", "{:,.0f}"),
        ("Capital gains, bottom 50%, $bn", "cap_gains_bn|bottom 50", "{:,.0f}"),
        ("Partnership and S corp., bottom 50%, $bn", "partnership_scorp_bn|bottom 50", "{:,.0f}"),
        ("Dividends, bottom 50%, $bn", "dividends_bn|bottom 50", "{:,.0f}"),
        ("Wages, 99th-99.9th, $bn", "wages_bn|99-99.9", "{:,.0f}"),
        ("Wages, top 0.1%, $bn", "wages_bn|top 0.1", "{:,.0f}")]
print("\n| | PUF | " + " | ".join(f"{a} {k}" for a, k in order) + " |")
print("|---" * (len(order) + 2) + "|")
for lab, key, fmt in rows:
    out = [fmt.format(val(P, key))]
    for c in order:
        v = [val(r[x], key) for x in cols[c]]
        s = fmt.format(np.mean(v))
        if len(v) > 1:
            s += " (" + fmt.format(min(v)) + " to " + fmt.format(max(v)) + ")"
        out.append(s)
    print(f"| {lab} | " + " | ".join(out) + " |")

print("\nPer file: top 1% share, top 0.1% share, six components in top 0.1% ($bn)")
for c in order:
    for x in sorted(cols[c]):
        d = r[x]
        print(f"  {x}: {d['share_above_p99']:.1%}, {d['share_above_p999']:.1%}, "
              f"{val(d, 'six_top 0.1'):,.0f}, total AGI {d['total_agi_tn']:.2f}tn")

print("\nComponents by AGI group ($bn), means over seeds and draws")
comp = [("agi_bn", "AGI"), ("wages_bn", "Wages"), ("cap_gains_bn", "Capital gains"),
        ("partnership_scorp_bn", "Partnership and S corp."), ("dividends_bn", "Dividends"),
        ("interest_bn", "Interest"), ("business_schC_bn", "Schedule C")]
print("| Component | AGI group | PUF | " + " | ".join(f"{a} {k}" for a, k in order) + " |")
print("|---" * (len(order) + 3) + "|")
for c, nm in comp:
    for g in GROUPS:
        vals = [P["groups"][g][c]] + [np.mean([r[x]["groups"][g][c] for x in cols[k]]) for k in order]
        print(f"| {nm} | {g} | " + " | ".join(f"{v:,.0f}" for v in vals) + " |")

print("\nThe weight variable (generated files; PUF = the PUF training sample)")
wk = [c for c in order if not c[1].startswith("given")]
print("| | PUF sample | " + " | ".join(f"{a} {k}" for a, k in wk) + " |")
pw = P["weight_dist"]
for key, fmt in [("n_records", "{:,.0f}"), ("ess", "{:,.0f}"), ("ess_share", "{:.3f}"),
                 ("mean_weight", "{:,.1f}"), ("tv_distance_to_puf_sample", "{:.3f}")]:
    out = [fmt.format(pw[key]) if key in pw else "-"]
    for c in wk:
        out.append(fmt.format(np.mean([r[x]["weight_dist"][key] for x in cols[c]])))
    print(f"| {key} | " + " | ".join(out) + " |")
for sub, fmt in [("records_share_by_weight_band", "{:.1%}"), ("records_share_by_agi_group", "{:.1%}"),
                 ("mean_weight_by_agi_group", "{:,.1f}")]:
    for k in pw[sub]:
        out = [fmt.format(pw[sub][k])]
        for c in wk:
            out.append(fmt.format(np.mean([r[x]["weight_dist"][sub][k] for x in cols[c]])))
        print(f"| {sub} {k} | " + " | ".join(out) + " |")

print("\nTraining runs: kept epoch, epochs run, SCF wealth completion (nats per record, "
      "6,112 SCF validation records), PUF part of validation loss with population weights")
for arm in ("uwA", "uwB", "uwC", "uwD"):
    for f in sorted((Q / arm).glob(f"run_{arm}_seed*.json")):
        j = json.load(open(f))
        print(f"  {f.stem}: kept epoch {j.get('best_all_epoch')}, epochs run {j.get('epochs_run')}, "
              f"SCF completion {j['val_completion'].get('nll_per_record', float('nan')):.3f} "
              f"(records {j['val_completion'].get('n_records')}), "
              f"wealth amounts only {j['val_completion'].get('wealth_nll_per_record', float('nan')):.3f}, "
              f"train minutes {j['train_seconds'] / 60:.0f}, job {j.get('slurm_job_id')}")

print("\nBy band of the sampling weight (drawn weight for generated files): share of records "
      "with AGI above the true 99th / 99.9th percentile, share with AGI below -$100,000, "
      "population-weighted negative AGI ($bn), population-weighted AGI above the 99.9th ($bn)")
keys = [("records_share", "{:.1%}"), ("share_agi_above_p99", "{:.2%}"),
        ("share_agi_above_p999", "{:.3%}"), ("share_agi_below_minus100k", "{:.3%}"),
        ("negative_agi_bn", "{:,.0f}"), ("agi_above_p999_bn", "{:,.0f}")]
print("| Weight band | Measure | PUF sample | " + " | ".join(f"{a} {k}" for a, k in wk) + " |")
for band in P["leak"]:
    for key, fmt in keys:
        out = [fmt.format(P["leak"][band][key])]
        for c in wk:
            v = [r[x]["leak"][band][key] for x in cols[c] if band in r[x].get("leak", {})]
            out.append(fmt.format(np.mean(v)) if v else "-")
        print(f"| {band} | {key} | " + " | ".join(out) + " |")
