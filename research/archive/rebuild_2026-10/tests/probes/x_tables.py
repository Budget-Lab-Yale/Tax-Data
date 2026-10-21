"""Probe X: print the report's tables (markdown) from the outputs of
x_translate.py, x_targets.py, x_compare.py and the tilt report, if present.

    python x_tables.py > $Q/x_tables.md
"""
import json
import os

import numpy as np
import pandas as pd

Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/x"
NAMES = {"E00200": "Wages", "E00900": "Schedule C business", "E02000": "Schedule E total",
         "P23250": "Long-term gains (SCF: all gains)", "E00600": "Ordinary dividends",
         "E00300": "Taxable interest", "E00400": "Tax-exempt interest", "E01700": "Taxable pensions",
         "E02400": "Social Security", "E02300": "Unemployment (SCF 2013: plus workers' comp.)",
         "E02100": "Farm"}


def pct(x, d=1):
    return "–" if x is None or (isinstance(x, float) and np.isnan(x)) else f"{100 * x:.{d}f}"


def money(x):
    if x is None or np.isnan(x):
        return "–"
    a = abs(x)
    s = f"${a / 1e6:.2f}m" if a >= 1e6 else f"${a / 1e3:.0f}k"
    return ("-" if x < 0 else "") + s


def main():
    TRD = f"{Q}/translate_full_v3x2" if os.environ.get("XVER", "full_v3x2") == "full_v3x2" else f"{Q}/translate"
    print("Movement tables from", TRD)
    s = json.load(open(f"{TRD}/summary.json"))
    mv = pd.read_csv(f"{TRD}/moves.csv")
    print("## Demographics: shares before and after (percent of weight)\n")
    print("| Variable | Value | PUF 2015 (not dependent filers) | SCF 2013 before | SCF 2013 after | SCF 2016 before | SCF 2016 after |")
    print("|---|---|---:|---:|---:|---:|---:|")
    y13, y16 = s["years"]["2013"], s["years"]["2016"]
    for k, lab in zip(("1", "2", "3", "4"), ("single", "joint", "separate", "head of household")):
        print(f"| Filing status | {lab} | {pct(y13['filing_status_shares']['puf'][k])} | "
              f"{pct(y13['filing_status_shares']['scf_before'][k])} | {pct(y13['filing_status_shares']['scf_after'][k])} | "
              f"{pct(y16['filing_status_shares']['scf_before'][k])} | {pct(y16['filing_status_shares']['scf_after'][k])} |")
    for i, lab in enumerate(["under 26", "26-34", "35-44", "45-54", "55-64", "65+"]):
        a = "age_range_shares"
        print(f"| Age range (PUF: known ages) | {lab} | {pct(y13[a]['puf_known'][i])} | {pct(y13[a]['scf_before'][i])} | "
              f"{pct(y13[a]['scf_after'][i])} | {pct(y16[a]['scf_before'][i])} | {pct(y16[a]['scf_after'][i])} |")
    for i, lab in enumerate(["0", "1", "2", "3 or more"]):
        a = "dependents_shares"
        print(f"| Dependents | {lab} | {pct(y13[a]['puf'][i])} | {pct(y13[a]['scf_before'][i])} | "
              f"{pct(y13[a]['scf_after'][i])} | {pct(y16[a]['scf_before'][i])} | {pct(y16[a]['scf_after'][i])} |")
    a = "male_share"
    print(f"| Sex of head / primary filer | male | {pct(y13[a]['puf_known'])} | {pct(y13[a]['scf_before'])} | "
          f"{pct(y13[a]['scf_after'])} | {pct(y16[a]['scf_before'])} | {pct(y16[a]['scf_after'])} |")
    print("\n## How far households moved: demographics (percent of weight changed)\n")
    print("| Change | SCF 2013 | SCF 2016 |\n|---|---:|---:|")
    for k, lab in (("filing_status_changed_weight_share", "Filing status"),
                   ("age_range_changed_weight_share", "Age range (by one range)"),
                   ("age_range_moved_2plus_weight_share", "Age range by two or more ranges"),
                   ("dependents_changed_weight_share", "Number of dependents"),
                   ("n24_changed_weight_share", "Children under 17"),
                   ("gender_changed_weight_share", "Sex of head")):
        print(f"| {lab} | {pct(y13[k])} | {pct(y16[k])} |")
    print("\nFiling status transitions (percent of all weight):",
          {y: {k: round(100 * v, 2) for k, v in s['years'][y]['filing_status_transitions'].items()} for y in ("2013", "2016")})
    d = mv[mv.variable == "_demographics"]
    print("\n| Seven-income group | Filing status changed 2013 | 2016 | Age range changed 2013 | 2016 | Dependents changed 2013 | 2016 |")
    print("|---|---:|---:|---:|---:|---:|---:|")
    for g in ["<50", "50-90", "90-99", "top 1"]:
        a = d[(d.income_group == g) & (d.year == 2013)].iloc[0]
        b = d[(d.income_group == g) & (d.year == 2016)].iloc[0]
        print(f"| {g} | {pct(a.filing_status_changed)} | {pct(b.filing_status_changed)} | {pct(a.age_range_changed)} | "
              f"{pct(b.age_range_changed)} | {pct(a.dependents_changed)} | {pct(b.dependents_changed)} |")
    print("\n## Incomes: who has an amount, before and after (percent of weight)\n")
    print("| Item | PUF positive | SCF 2013 positive before → after | SCF 2016 before → after | PUF negative | SCF 2013 negative before → after | SCF 2016 before → after |")
    print("|---|---:|---:|---:|---:|---:|---:|")
    inc = mv[mv.variable != "_demographics"]
    for v in NAMES:
        a = inc[(inc.variable == v) & (inc.year == 2013)].iloc[0]
        b = inc[(inc.variable == v) & (inc.year == 2016)].iloc[0]
        neg = a.share_negative_puf > 0
        print(f"| {NAMES[v]} | {pct(a.share_positive_puf)} | {pct(a.share_positive_before)} → {pct(a.share_positive_after)} | "
              f"{pct(b.share_positive_before)} → {pct(b.share_positive_after)} | "
              + (f"{pct(a.share_negative_puf)} | {pct(a.share_negative_before)} → {pct(a.share_negative_after)} | "
                 f"{pct(b.share_negative_before)} → {pct(b.share_negative_after)} |" if neg else "– | – | – |"))
    print("\n## Incomes: flips and totals\n")
    print("| Item | Year | Zero to positive | Positive to zero | To a loss | Loss to zero | Dollars of the flipped-out holders, % of positive total | Total before ($bn, own dollars) | Total after | PUF total | Holding model: area under ROC curve |")
    print("|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|")
    for v in NAMES:
        for y in (2013, 2016):
            a = inc[(inc.variable == v) & (inc.year == y)].iloc[0]
            print(f"| {NAMES[v]} | {y} | {pct(a.flip_zero_to_pos)} | {pct(a.flip_pos_to_zero)} | {pct(a.flip_to_neg)} | "
                  f"{pct(a.flip_neg_to_zero)} | {pct(a.dollars_of_flipped_out_share_of_positive_total)} | "
                  f"{a.total_bn_before_own_dollars:,.0f} | {a.total_bn_after:,.0f} | {a.total_bn_puf:,.0f} | {a.auc_holding_model:.2f} |")
    print("\n## Incomes: rank moves among households positive before and after\n")
    print("Mean absolute move in percentile points of the rank among holders (SCF wave's own ranking before, the PUF's after), and share of these households moved more than 25 points, by seven-income group.\n")
    print("| Item | Year | All: mean move | All: over 25 pts | Over 40 pts | Bottom 50%: mean | over 25 | 50-90%: mean | over 25 | 90-99%: mean | over 25 | Top 1%: mean | over 25 | Mean signed move |")
    print("|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|")
    for v in NAMES:
        for y in (2013, 2016):
            a = inc[(inc.variable == v) & (inc.year == y)].iloc[0]
            cells = [pct(a.rank_shift_mean), pct(a.rank_shift_over_25pts_share), pct(a.rank_shift_over_40pts_share)]
            for g in ["<50", "50-90", "90-99", "top 1"]:
                cells += [pct(a[f"rank_shift_mean_{g}"]), pct(a[f"rank_shift_over_25pts_{g}"])]
            cells.append(pct(a.rank_signed_shift_mean))
            print(f"| {NAMES[v]} | {y} | " + " | ".join(cells) + " |")

    # disagreement
    f = f"{Q}/x_targets_disagreement.json"
    if os.path.exists(f):
        r = json.load(open(f))
        print("\n## Target disagreement (C's measure)\n")
        print("| Version | Set | Largest spread, % of PUF weight | Mean spread | Largest three (variable, bin) |")
        print("|---|---|---:|---:|---|")
        for lab, x in r.items():
            if lab == "populations":
                continue
            for st in ("wcore", "wfull"):
                dd = x[st]["disagreement"]
                top = "; ".join(f"{t['variable']} {t['bin_label']} {100 * t['spread']:.2f}" for t in dd["largest"][:3])
                print(f"| {lab} | {st} | {100 * dd['max_spread_share_of_total_weight']:.2f} | "
                      f"{100 * dd['mean_spread_share_of_total_weight']:.3f} | {top} |")
        for lab, x in r.items():
            if lab == "populations":
                continue
            for st in ("wcore", "wfull"):
                dd = x[st]["disagreement"]
                print(f"\n{lab} {st}: largest deviation from the families' mean, by family (% of weight):",
                      {k: round(100 * v, 2) for k, v in dd["largest_deviation_from_family_mean_by_family"].items()})
                if "max_spread_by_variable" in dd:
                    print("  max spread by variable:", {k: round(100 * v, 2) for k, v in list(dd["max_spread_by_variable"].items())[:8]})
                if "group_mix" in x[st]:
                    print("  group mix TVD (2013, 2016):", {k: (round(100 * v["tvd_2013"], 1), round(100 * v["tvd_2016"], 1)) for k, v in x[st]["group_mix"].items()})
        print("\nPopulations:")
        for k, v in r["populations"].items():
            print(f"- {k}: {v}")

    f = f"{Q}/x_compare.json"
    if os.path.exists(f):
        c = json.load(open(f))
        red = c.pop("scf_records_redrawn", {})
        so = c.pop("scf_with_split_offs_at_zero", {})
        nd = c.pop("puf_not_dependent_filers", {})
        print("\nSCF all filer units with split-offs at zero:", so)
        print("\nPUF completions, returns that are not dependent filers:")
        for k, v in nd.items():
            print(f"- {k}: median {money(v['median'])}, mean {money(v['mean'])}, home {pct(v['hold_primary_home'])}")
        cols = list(c.keys())
        if red:
            print("\n## SCF training records: wealth redrawn by the model against their true wealth\n")
            print("| | Median | Mean | Share owning a home (%) | Share holding stocks (%) |\n|---|---:|---:|---:|---:|")
            for y in (2013, 2016):
                t = c[f"SCF {y}"]
                print(f"| SCF {y}, true | {money(t['median'])} | {money(t['mean'])} | {pct(t['hold_primary_home'])} | {pct(t['hold_equities'])} |")
                for k, t in red.items():
                    if k.endswith(str(y)):
                        print(f"| {k} | {money(t['median'])} | {money(t['mean'])} | {pct(t['hold_primary_home'])} | {pct(t['hold_equities'])} |")
        print("\n## Completed PUF wealth\n")
        print("| Statistic | " + " | ".join(cols) + " |")
        print("|---|" + "---:|" * len(cols))
        rows = [("mean", "Mean net worth", money), ("median", "Median net worth", money), ("p90", "90th percentile", money),
                ("p99", "99th percentile", money), ("top10_share", "Top 10% share (%)", pct),
                ("top1_share", "Top 1% share (%)", pct), ("top01_share", "Top 0.1% share (%)", pct),
                ("neg_share", "Share with negative net worth (%)", pct)]
        rows += [(f"hold_{h}", f"Share holding {h} (%)", pct) for h in
                 ["cash", "equities", "bonds", "dc", "pass_throughs", "primary_home", "other_home", "primary_mortgage", "credit_cards"]]
        for k, lab, fm in rows:
            print(f"| {lab} | " + " | ".join(fm(c[x].get(k, np.nan)) for x in cols) + " |")
        for grp, glab in (("agi", "PUF AGI group (SCF: seven-income group)"), ("inc7", "Seven-income group (as EVAL_REPORT.md)")):
            for st, slab, fm in (("mean", "Mean net worth", money), ("median", "Median net worth", money),
                                 ("share", "Share of all net worth (%)", pct), ("hold_equities", "Share holding stocks (%)", pct),
                                 ("hold_bonds", "Share holding bonds (%)", pct),
                                 ("hold_pass_throughs", "Share holding a business (%)", pct),
                                 ("hold_primary_home", "Share owning a home (%)", pct)):
                print(f"\n**{slab}, by {glab}**\n")
                print("| Group | " + " | ".join(cols) + " |")
                print("|---|" + "---:|" * len(cols))
                for g in ["Bottom 50%", "50-90%", "90-99%", "99-99.9%", "Top 0.1%"]:
                    vals = []
                    for x in cols:
                        key = f"{grp}|{g}|{st}"
                        if key not in c[x]:
                            key = f"inc7|{g}|{st}"         # SCF has only the seven-income groups
                        vals.append(fm(c[x].get(key, np.nan)))
                    print(f"| {g} | " + " | ".join(vals) + " |")

    for arm in ("P_core", "P_full"):
        f = f"{Q}/c/{arm}/report.json"
        if os.path.exists(f):
            r = json.load(open(f))
            fp = r.get("fresh_pass", {})
            print(f"\n## Tilt ({arm})\n")
            print("KL:", json.dumps(fp.get("kl", {}), default=str)[:1500])
            print("per-record quantiles:", fp.get("kl_stepwise_per_record_quantiles"))
            print("target consistency:", json.dumps(r.get("target_consistency"), default=str)[:600])
            for k in fp:
                if "err" in k.lower():
                    print(k, json.dumps(fp[k], default=str)[:800])


if __name__ == "__main__":
    main()
