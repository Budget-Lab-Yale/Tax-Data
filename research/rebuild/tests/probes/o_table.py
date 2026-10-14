"""Probe O: markdown tables for O_REPORT.md.

Usage: python o_table.py <compare json from o_compare.py> <score json dir>
Compare labels are ORDER|mSEED|sSAMPLING. Prints:
  1. levels by order, mean of the model seeds (sampling seed 1), against the PUF
  2. the same rows, min-max across model seeds, and the sampling-noise pair
  3. components and joint checks by AGI group, mean of model seeds
  4. held-out log-probability by order, per model seed
Aggregates only.
"""
import json
import sys
from pathlib import Path

import numpy as np

ORDERS = ["test3", "demolast", "agifirst", "compfirst", "agibysize", "reverse", "random",
          "entropy_low", "entropy_high", "greedy"]
GROUPS = ["bottom 50", "50-90", "90-99", "99-99.9", "top 0.1"]
r = json.load(open(sys.argv[1]))
P = r["PUF training records"]
labs = [k for k in r if "|" in k]
seeds = sorted({int(k.split("|")[1][1:]) for k in labs})
orders = [o for o in ORDERS if any(k.startswith(o + "|") for k in labs)]


def get(d, key):
    if key.startswith("g:"):
        _, g, c = key.split(":")
        return d["groups"][g][c]
    return d[key]


def vals(o, key, ss=1):
    return [get(r[f"{o}|m{m}|s{ss}"], key) for m in seeds if f"{o}|m{m}|s{ss}" in r]


ROWS = [("Total AGI ($tn)", "total_agi_tn", "{:.2f}"),
        ("Weight above true 99th pct (%)", "w_above_p99", "pct2"),
        ("Weight above true 99.9th pct (%)", "w_above_p999", "pct3"),
        ("Top 1% share of AGI (%)", "share_above_p99", "pct1"),
        ("Top 0.1% share of AGI (%)", "share_above_p999", "pct1"),
        ("Capital gains, bottom 50 ($bn)", "g:bottom 50:cap_gains_bn", "{:,.0f}"),
        ("Partnership and S corp, bottom 50 ($bn)", "g:bottom 50:partnership_scorp_bn", "{:,.0f}"),
        ("Dividends, bottom 50 ($bn)", "g:bottom 50:dividends_bn", "{:,.0f}"),
        ("Schedule C, bottom 50 ($bn)", "g:bottom 50:business_schC_bn", "{:,.0f}"),
        ("Wages, 99-99.9 ($bn)", "g:99-99.9:wages_bn", "{:,.0f}"),
        ("AGI, top 0.1 ($bn)", "g:top 0.1:agi_bn", "{:,.0f}"),
        ("Six components, top 0.1 ($bn)", "g:top 0.1:six_components_bn", "{:,.0f}"),
        ("AGI less six components, top 0.1 ($bn)", "g:top 0.1:agi_minus_six_bn", "{:,.0f}"),
        ("Itemizers, top 0.1 (%)", "g:top 0.1:itemizer_share", "pct1"),
        ("Joint filers, top 0.1 (%)", "g:top 0.1:joint_share", "pct1"),
        ("Wages and capital gains both, top 0.1 (%)", "g:top 0.1:wages_and_capgains_share", "pct1"),
        ("Wages and capital gains both, bottom 50 (%)", "g:bottom 50:wages_and_capgains_share", "pct1")]


def fmt(x, f):
    if f.startswith("pct"):
        return f"{100 * x:.{int(f[3])}f}"
    return f.format(x)


print(f"model seeds {seeds}; orders {orders}\n")
print("## 1. Mean over model seeds, sampling seed 1\n")
print("| | PUF | " + " | ".join(orders) + " |")
print("|---" * (len(orders) + 2) + "|")
for name, k, f in ROWS:
    print(f"| {name} | {fmt(get(P, k), f)} | " +
          " | ".join(fmt(np.mean(vals(o, k)), f) for o in orders) + " |")

print("\n## 2. Range over model seeds (min to max); sampling noise (model seed 1, "
      "sampling seed 1 vs 2)\n")
print("| | PUF | " + " | ".join(orders) + " |")
print("|---" * (len(orders) + 2) + "|")
for name, k, f in ROWS:
    cells = []
    for o in orders:
        v = vals(o, k)
        c = f"{fmt(min(v), f)} to {fmt(max(v), f)}"
        if f"{o}|m1|s2" in r:
            c += f" [{fmt(get(r[o + '|m1|s1'], k), f)} / {fmt(get(r[o + '|m1|s2'], k), f)}]"
        cells.append(c)
    print(f"| {name} | {fmt(get(P, k), f)} | " + " | ".join(cells) + " |")

print("\n## 3. By AGI group, mean over model seeds\n")
for c, nm, f in [("agi_bn", "AGI ($bn)", "{:,.0f}"), ("wages_bn", "Wages ($bn)", "{:,.0f}"),
                 ("cap_gains_bn", "Capital gains ($bn)", "{:,.0f}"),
                 ("partnership_scorp_bn", "Partnership and S corporation ($bn)", "{:,.0f}"),
                 ("dividends_bn", "Dividends ($bn)", "{:,.0f}"),
                 ("interest_bn", "Interest ($bn)", "{:,.0f}"),
                 ("business_schC_bn", "Schedule C ($bn)", "{:,.0f}"),
                 ("agi_minus_six_bn", "AGI less the six components ($bn)", "{:,.0f}"),
                 ("weight_share", "Share of returns (%)", "pct2"),
                 ("itemizer_share", "Itemizers (% of group)", "pct1"),
                 ("joint_share", "Joint filers (% of group)", "pct1"),
                 ("wages_and_capgains_share", "Both wages and capital gains (% of group)", "pct1")]:
    print(f"\n{nm}\n")
    print("| AGI group | PUF | " + " | ".join(orders) + " |")
    print("|---" * (len(orders) + 2) + "|")
    for g in GROUPS:
        k = f"g:{g}:{c}"
        print(f"| {g} | {fmt(get(P, k), f)} | " +
              " | ".join(fmt(np.mean(vals(o, k)), f) for o in orders) + " |")

if len(sys.argv) > 2:
    print("\n## 4. Held-out log-probability (nats per record, lower is better)\n")
    sc = {}
    for m in seeds:
        fn = Path(sys.argv[2]) / f"h_score_val_seed{m:02d}_score.json"
        if fn.exists():
            sc[m] = json.load(open(fn))
    if sc:
        m0 = next(iter(sc))
        sp = sc[m0]["_spread"]
        print(f"records {sp['n_records']}, top 1% {sp['n_top1']}, top 0.1% {sp['n_top01']}\n")
        names = [o for o in ORDERS if o in sc[m0] or f"{o}_0" in sc[m0]]
        print("| Order | " + " | ".join(f"all, seed {m}" for m in sc) + " | " +
              " | ".join(f"top 1%, seed {m}" for m in sc) + " | " +
              " | ".join(f"top 0.1%, seed {m}" for m in sc) + " |")
        print("|---" * (1 + 3 * len(sc)) + "|")
        for o in names:
            def one(m, key):
                d = sc[m]
                if o in d:
                    return f"{d[o][key]:.3f}"
                xs = [d[k][key] for k in d if k.startswith(o + "_")]
                return f"{np.mean(xs):.3f} ({min(xs):.3f}-{max(xs):.3f})"
            print(f"| {o} | " + " | ".join(one(m, "mean_nll_weighted") for m in sc) + " | " +
                  " | ".join(one(m, "mean_nll_top1_unweighted") for m in sc) + " | " +
                  " | ".join(one(m, "mean_nll_top01_unweighted") for m in sc) + " |")
        for m in sc:
            s = sc[m]["_spread"]
            print(f"\nseed {m}: weighted mean over records of the standard deviation across "
                  f"orders {s['weighted_mean_sd_across_orders']:.3f}, of the range "
                  f"{s['weighted_mean_range_across_orders']:.3f}; top 1% mean sd "
                  f"{s['top1_mean_sd_across_orders']:.3f}; mixture over orders "
                  f"{s['log_mean_exp_over_orders_weighted']:.3f}")
        print("\nAGI's own term (weighted, nats), seed", m0)
        print("| Order | AGI term all | AGI term top 1% |")
        print("|---|---|---|")
        for k, d in sc[m0].items():
            if k.startswith("_"):
                continue
            print(f"| {k} | {d['by_variable_weighted']['E00100']:.3f} | "
                  f"{d['by_variable_top1_unweighted']['E00100']:.3f} |")
        for k in ["entropy_low", "entropy_high", "random_0"]:
            if k in sc[m0] and "mean_step_weighted" in sc[m0][k]:
                st = sc[m0][k]["mean_step_weighted"]
                print(f"\n{k}: mean step (0 = first) when scoring, weighted, seed {m0}: " +
                      ", ".join(f"{n} {v:.1f}" for n, v in sorted(st.items(), key=lambda t: t[1])))
