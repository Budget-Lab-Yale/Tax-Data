"""Probe V: collect the scorecard json files (v_score.py) into markdown tables.

Usage: python v_collect.py <out_dir> <markdown file>
Each json's label is "<model> seed <k>"; files with the same model are
averaged over seeds (range in brackets where useful). Weighted aggregates only.
"""
import glob
import json
import re
import sys
from collections import OrderedDict

import numpy as np

GROUPS = ["bottom 50", "50-90", "90-99", "99-99.9", "top 0.1"]
ORDER = ["PUF validation", "Training records decoded from tokens", "AS random", "Test 3 AGI first", "Test 3 random", "BL"]
DOLLAR_VARS = None
NAMES = {"E00200": "wages", "E00900": "Schedule C", "E02000": "Schedule E total", "E26270": "partnership and S corp",
         "P23250": "long-term gains", "P22250": "short-term gains", "E00600": "ordinary dividends",
         "E00650": "qualified dividends", "E00300": "taxable interest", "E01500": "total pensions",
         "E01700": "taxable pensions", "E01400": "taxable IRA", "E02400": "gross Social Security",
         "E25850": "rent and royalty income", "E25860": "rent and royalty loss", "E02100": "Schedule F",
         "E00400": "tax-exempt interest", "E26390": "estate and trust income", "E26400": "estate and trust loss",
         "E01100": "capital gain distributions", "E02300": "unemployment", "E03150": "IRA deduction",
         "E03210": "student loan interest", "E03270": "self-employed health insurance", "E17500": "medical",
         "E18400": "state and local income/sales tax", "E18500": "real estate tax", "E19200": "interest paid",
         "E19800": "cash charity", "E20100": "noncash charity", "AGI_REMAINDER": "AGI remainder", "E00100": "AGI"}


def nm(v):
    return f"{NAMES.get(v, v)} ({v})"


def load(d):
    by = OrderedDict()
    real = None
    for f in sorted(glob.glob(f"{d}/*.json")):
        try:
            r = json.load(open(f))
        except Exception:
            continue
        if "label" not in r or "variables" not in r:
            continue
        model = re.sub(r"\s+seed\s*\d+$", "", r["label"]).strip()
        by.setdefault(model, []).append(r)
        if real is None or ("copying" in r and "copying" not in real):
            real = r
    models = [m for m in ORDER if m in by] + [m for m in by if m not in ORDER]
    return real, by, models


def mean(xs):
    xs = [x for x in xs if x is not None and not (isinstance(x, float) and np.isnan(x))]
    return float(np.mean(xs)) if xs else float("nan")


def rng(xs):
    xs = [x for x in xs if x is not None and not (isinstance(x, float) and np.isnan(x))]
    return (min(xs), max(xs)) if xs else (float("nan"), float("nan"))


def get(r, path):
    x = r
    for p in path:
        if x is None:
            return None
        if isinstance(x, dict):
            x = x.get(p)
        else:
            return None
    return x


def fmt(x, f):
    if x is None or (isinstance(x, float) and np.isnan(x)):
        return "pending"
    return f.format(x)


def cell(runs, fn, f, show_range=False):
    vals = []
    for r in runs:
        try:
            v = fn(r)
        except Exception:
            v = None
        vals.append(v)
    if all(v is None for v in vals):
        return "—"
    m = mean(vals)
    s = fmt(m, f)
    if show_range and len([v for v in vals if v is not None]) > 1:
        lo, hi = rng(vals)
        s += f" [{fmt(lo, f)}–{fmt(hi, f)}]"
    return s


# summary measures, each a function of one run returning a number
def var_dollar_miss(r):
    tot = 0.0
    for v, row in r["variables"]["gen"].items():
        if v.startswith("_") or v in ("E00100", "AGI_REMAINDER"):
            continue
        for g in GROUPS:
            tot += abs(row[g]["total_bn"] - r["variables"]["real"][v][g]["total_bn"])
    return tot


def var_nonzero_gap(r):
    gaps = []
    for v, row in r["variables"]["gen"].items():
        if v.startswith("_") or v in ("E00100",):
            continue
        for g in GROUPS:
            gaps.append(abs(row[g]["nonzero"] - r["variables"]["real"][v][g]["nonzero"]))
    return 100 * float(np.mean(gaps))


AGE_GROUPS = ["bottom 50", "50-90", "90-99"]  # the real file has age for almost no returns above the 99th percentile


def dist_gap(r, key):
    gaps = []
    for g in (AGE_GROUPS if key == "AGERANGE" else GROUPS):
        a, b = r["joint"]["gen"][g][key], r["joint"]["real"][g][key]
        for k in set(a) | set(b):
            gaps.append(abs(a.get(k, 0) - b.get(k, 0)))
    return 100 * float(np.mean(gaps))


def max_dist_gap(r, key):
    gaps = []
    for g in GROUPS:
        a, b = r["joint"]["gen"][g][key], r["joint"]["real"][g][key]
        for k in set(a) | set(b):
            gaps.append(abs(a.get(k, 0) - b.get(k, 0)))
    return 100 * float(np.max(gaps))


def cohold_gap(r, stat=np.mean):
    gaps = []
    for g in GROUPS:
        a, b = r["joint"]["gen"][g]["coholding"], r["joint"]["real"][g]["coholding"]
        gaps += [abs(a[k] - b[k]) for k in a]
    return 100 * float(stat(gaps))


def rule_share(r, text, key="share"):
    for ru in r["rules"]["gen"]["rules"]:
        if ru["text"] == text:
            return ru.get(key, float("nan"))
    return float("nan")


TC = {}


def tc_get(label, path):
    x = TC.get(label.replace(" ", "_"))
    if x is None or "tables" not in x:
        return None
    return get(x["tables"], path)


def rate_gap(r):
    return 100 * max(abs(r["tax"]["gen"][g]["avg_rate_net"] - r["tax"]["real"][g]["avg_rate_net"]) for g in GROUPS)


def main():
    d, outp = sys.argv[1], sys.argv[2]
    real, by, models = load(d)
    try:
        TC.update(json.load(open(f"{d}/taxcalc.json")))
    except Exception:
        pass
    L = []
    P = L.append
    if real is None:
        open(outp, "w").write("No scorecard files yet.\n")
        return
    P("# Probe V scorecard tables (written by v_collect.py)\n")
    P("Runs per model: " + "; ".join(f"{m}: {len(by[m])} ({', '.join(r['label'] for r in by[m])})" for m in models) + "\n")
    P(f"AGI thresholds from the real PUF training records: {json.dumps(real['thresholds'])}\n")
    P("Means over model seeds; range over seeds in brackets. 'Real' is the PUF training records "
      "(166,156 records). 'PUF validation' is the 20,768 held-out real records scored as if generated: "
      "the size of a miss that sampling alone produces with a file that small.\n")
    hdr = "| Check | Real | " + " | ".join(models) + " |"
    sep = "|---" * (len(models) + 2) + "|"
    rows = [
        ("Total AGI ($bn)", lambda r: r["variables"]["real"]["E00100"]["all"]["total_bn"],
         lambda r: r["variables"]["gen"]["E00100"]["all"]["total_bn"], "{:,.0f}"),
        ("AGI, top 0.1% group ($bn)", lambda r: r["variables"]["real"]["E00100"]["top 0.1"]["total_bn"],
         lambda r: r["variables"]["gen"]["E00100"]["top 0.1"]["total_bn"], "{:,.0f}"),
        ("1. Sum over 30 amounts × 5 groups of |generated − real| ($bn)", lambda r: 0.0, var_dollar_miss, "{:,.0f}"),
        ("1. Mean gap in share nonzero, 31 amounts × 5 groups (points)", lambda r: 0.0, var_nonzero_gap, "{:.2f}"),
        ("2. Filing status × group, mean gap (points)", lambda r: 0.0, lambda r: dist_gap(r, "MARS"), "{:.2f}"),
        ("2. Filing status × group, largest gap (points)", lambda r: 0.0, lambda r: max_dist_gap(r, "MARS"), "{:.1f}"),
        ("2. Itemizer share, top 0.1% (%)", lambda r: 100 * r["joint"]["real"]["top 0.1"]["itemizer_share"],
         lambda r: 100 * r["joint"]["gen"]["top 0.1"]["itemizer_share"], "{:.1f}"),
        ("2. Itemizer share, 50–90 (%)", lambda r: 100 * r["joint"]["real"]["50-90"]["itemizer_share"],
         lambda r: 100 * r["joint"]["gen"]["50-90"]["itemizer_share"], "{:.1f}"),
        ("2. Co-holding of 28 pairs × 5 groups, mean gap (points)", lambda r: 0.0, cohold_gap, "{:.2f}"),
        ("2. Co-holding, largest gap (points)", lambda r: 0.0, lambda r: cohold_gap(r, np.max), "{:.1f}"),
        ("2. Any EITC children, bottom 50 (%)", lambda r: 100 * r["joint"]["real"]["bottom 50"]["eitc_children_any"],
         lambda r: 100 * r["joint"]["gen"]["bottom 50"]["eitc_children_any"], "{:.1f}"),
        ("2. Child-credit children per return, 50–90", lambda r: r["joint"]["real"]["50-90"]["ctc_children_mean"],
         lambda r: r["joint"]["gen"]["50-90"]["ctc_children_mean"], "{:.3f}"),
        ("2. Age range × group, bottom three groups, mean gap (points)", lambda r: 0.0, lambda r: dist_gap(r, "AGERANGE"), "{:.2f}"),
        ("3. Returns breaking any rule real records never break (%)",
         lambda r: 0.0, lambda r: 100 * r["rules"]["gen"]["any_share"], "{:.2f}"),
        ("3. Taxable pensions above total pensions (%)", lambda r: 0.0,
         lambda r: 100 * rule_share(r, "E01700 <= E01500"), "{:.2f}"),
        ("3. Same, by more than 5% (%)", lambda r: 0.0,
         lambda r: 100 * rule_share(r, "E01700 <= E01500", "share_by_more_than_5pct"), "{:.2f}"),
        ("3. Qualified dividends above ordinary dividends (%)", lambda r: 0.0,
         lambda r: 100 * rule_share(r, "E00650 <= E00600"), "{:.2f}"),
        ("3. Same, by more than 5% (%)", lambda r: 0.0,
         lambda r: 100 * rule_share(r, "E00650 <= E00600", "share_by_more_than_5pct"), "{:.2f}"),
        ("3. Any rule broken, top 0.1% group (%)", lambda r: 0.0,
         lambda r: 100 * r["rules"]["gen"]["any_by_group"]["top 0.1"], "{:.2f}"),
        ("4. Net income tax, all returns ($bn)", lambda r: r["tax"]["real"]["all"]["net_tax_bn"],
         lambda r: r["tax"]["gen"]["all"]["net_tax_bn"], "{:,.0f}"),
        ("4. Net income tax, top 0.1% group ($bn)", lambda r: r["tax"]["real"]["top 0.1"]["net_tax_bn"],
         lambda r: r["tax"]["gen"]["top 0.1"]["net_tax_bn"], "{:,.0f}"),
        ("4. Top 1% share of net income tax (%)",
         lambda r: 100 * (r["tax"]["real"]["99-99.9"]["share_of_net_tax"] + r["tax"]["real"]["top 0.1"]["share_of_net_tax"]),
         lambda r: 100 * (r["tax"]["gen"]["99-99.9"]["share_of_net_tax"] + r["tax"]["gen"]["top 0.1"]["share_of_net_tax"]), "{:.1f}"),
        ("4. Largest gap in average tax rate by group (points)", lambda r: 0.0, rate_gap, "{:.2f}"),
        ("4. Tax-Calculator: net income tax, all returns ($bn)", lambda r: tc_get("real", ["all", "net_tax_bn"]),
         lambda r: tc_get(r["label"], ["all", "net_tax_bn"]), "{:,.0f}"),
        ("4. Tax-Calculator: net income tax, top 0.1% group ($bn)", lambda r: tc_get("real", ["top 0.1", "net_tax_bn"]),
         lambda r: tc_get(r["label"], ["top 0.1", "net_tax_bn"]), "{:,.0f}"),
        ("4. Tax-Calculator: largest gap in average tax rate by group (points)", lambda r: 0.0,
         lambda r: 100 * max(abs(tc_get(r["label"], [g, "avg_rate_net"]) - tc_get("real", [g, "avg_rate_net"])) for g in GROUPS), "{:.2f}"),
        ("5. Classifier area under ROC curve, all returns (0.5 = indistinguishable)", lambda r: 0.5,
         lambda r: r["classifier"]["auc_overall"], "{:.3f}"),
        ("5. Same, within the top 0.1% group", lambda r: 0.5,
         lambda r: r["classifier"]["auc_within_group_models"]["top 0.1"]["auc"], "{:.3f}"),
        ("5. Same classifier on rank-band tokens instead of dollars, all returns", lambda r: 0.5,
         lambda r: r["classifier_tokens"]["auc_overall"], "{:.3f}"),
        ("5. Tokens, within the top 0.1% group", lambda r: 0.5,
         lambda r: r["classifier_tokens"]["auc_within_group_models"]["top 0.1"]["auc"], "{:.3f}"),
        ("6. Share at distance zero from a training record, all 37 variables (%)", lambda r: None,
         lambda r: 100 * r["copying"]["share_distance_zero"], "{:.2f}"),
        ("6. Same, top 0.1% group (%)", lambda r: None,
         lambda r: 100 * r["copying"]["top01_generated_share_distance_zero"], "{:.2f}"),
        ("6. Median distance to the nearest training record, top 0.1% group", lambda r: None,
         lambda r: r["copying"]["top01_generated_nn_median"], "{:.3f}"),
        ("6. Share with a token pattern identical to a training record (%)", lambda r: None,
         lambda r: 100 * r["copying"]["token_copy_share_generated"], "{:.1f}"),
        ("6. Same, top 0.1% group (%)", lambda r: None,
         lambda r: 100 * r["copying"]["token_copy_share_generated_top01"], "{:.1f}"),
    ]
    P("## Summary\n")
    P(hdr)
    P(sep)
    for lab, fr, fg, f in rows:
        rv = fr(real)
        P(f"| {lab} | {fmt(rv, f) if rv is not None else '—'} | " +
          " | ".join(cell(by[m], fg, f, show_range=(m != 'PUF validation')) for m in models) + " |")
    P("")

    gm = [m for m in models]

    # 1. every variable
    P("## 1. Every variable: total dollars by AGI group ($bn), real / " + " / ".join(gm) + "\n")
    P("| Variable | " + " | ".join(GROUPS) + " | all |")
    P("|---" * 7 + "|")
    for v in real["variables"]["real"]:
        if v.startswith("_"):
            continue
        cells = []
        for g in GROUPS + ["all"]:
            parts = [fmt(real["variables"]["real"][v][g]["total_bn"], "{:,.0f}")]
            parts += [cell(by[m], lambda r, v=v, g=g: r["variables"]["gen"][v][g]["total_bn"], "{:,.0f}") for m in gm]
            cells.append(" / ".join(parts))
        P(f"| {nm(v)} | " + " | ".join(cells) + " |")
    P("")
    P("## 1. Every variable: share nonzero by AGI group (%), real / " + " / ".join(gm) + "\n")
    P("| Variable | " + " | ".join(GROUPS) + " | all |")
    P("|---" * 7 + "|")
    for v in real["variables"]["real"]:
        if v.startswith("_") or v == "E00100":
            continue
        cells = []
        for g in GROUPS + ["all"]:
            parts = [fmt(100 * real["variables"]["real"][v][g]["nonzero"], "{:.1f}")]
            parts += [cell(by[m], lambda r, v=v, g=g: 100 * r["variables"]["gen"][v][g]["nonzero"], "{:.1f}") for m in gm]
            cells.append(" / ".join(parts))
        P(f"| {nm(v)} | " + " | ".join(cells) + " |")
    P("")
    for m in gm:
        P(f"### Largest dollar misses, {m} (mean over seeds; generated minus real, $bn)\n")
        miss = []
        for v in real["variables"]["real"]:
            if v.startswith("_") or v in ("E00100",):
                continue
            for g in GROUPS:
                gv = mean([r["variables"]["gen"][v][g]["total_bn"] for r in by[m]])
                rv = real["variables"]["real"][v][g]["total_bn"]
                miss.append((abs(gv - rv), v, g, rv, gv))
        miss.sort(reverse=True)
        P("| Variable | Group | Real | Generated | Difference |")
        P("|---|---|---|---|---|")
        for _, v, g, rv, gv in miss[:15]:
            P(f"| {nm(v)} | {g} | {rv:,.0f} | {gv:,.0f} | {gv - rv:+,.0f} |")
        P("")

    # 2. joint
    P("## 2. Joint relationships\n")
    for key, title, cap in [("MARS", "Filing status (1 single, 2 joint, 3 separate, 4 head of household), % of group", None),
                            ("FDED", "Form of deduction (1 itemized, 2 standard, 3 none), % of group", None),
                            ("EIC", "EITC children (0–3), % of group", None),
                            ("N24", "Child-tax-credit children (0, 1, 2, 3+), % of group", None),
                            ("AGERANGE", "Age range of primary filer (codes 1–6, youngest to oldest; real records: "
                             "observed ones only, which is almost none above the 99th percentile), % of group", None)]:
        P(f"### {title}; real / " + " / ".join(gm) + "\n")
        keys = sorted({k for g in GROUPS for k in real["joint"]["real"][g][key]}, key=lambda s: float(s))
        P("| Group | " + " | ".join(keys) + " |")
        P("|---" * (len(keys) + 1) + "|")
        for g in GROUPS:
            cells = []
            for k in keys:
                parts = [fmt(100 * real["joint"]["real"][g][key].get(k, 0), "{:.1f}")]
                parts += [cell(by[m], lambda r, g=g, k=k: 100 * r["joint"]["gen"][g][key].get(k, 0), "{:.1f}") for m in gm]
                cells.append(" / ".join(parts))
            P(f"| {g} | " + " | ".join(cells) + " |")
        P("")
    P("### Share holding each main item (% of group); real / " + " / ".join(gm) + "\n")
    items = list(real["joint"]["real"]["all"]["held"])
    P("| Item | " + " | ".join(GROUPS) + " |")
    P("|---" * 6 + "|")
    for it in items:
        cells = []
        for g in GROUPS:
            parts = [fmt(100 * real["joint"]["real"][g]["held"][it], "{:.1f}")]
            parts += [cell(by[m], lambda r, g=g, it=it: 100 * r["joint"]["gen"][g]["held"][it], "{:.1f}") for m in gm]
            cells.append(" / ".join(parts))
        P(f"| {it} | " + " | ".join(cells) + " |")
    P("")
    for m in gm:
        P(f"### Co-holding: the 12 largest gaps, {m} (% of group holding both)\n")
        gaps = []
        for g in GROUPS:
            for k, rv in real["joint"]["real"][g]["coholding"].items():
                gv = mean([r["joint"]["gen"][g]["coholding"][k] for r in by[m]])
                gaps.append((abs(gv - rv), k, g, rv, gv))
        gaps.sort(reverse=True)
        P("| Pair | Group | Real | Generated |")
        P("|---|---|---|---|")
        for _, k, g, rv, gv in gaps[:12]:
            P(f"| {k} | {g} | {100 * rv:.1f} | {100 * gv:.1f} |")
        P("")

    # 3. rules
    P("## 3. Rules that hold on every real training record: weighted share of returns breaking each (%)\n")
    P(f"{real['rules']['n_rules']} rules were found on the real training records. 'Validation' is the real "
      "held-out records, which shows how strict each rule is. Listed: the 30 rules most often broken by any model.\n")
    allr = {}
    for m in gm:
        for r in by[m]:
            for ru in r["rules"]["gen"]["rules"]:
                allr.setdefault(ru["text"], {}).setdefault(m, []).append(ru["share"])
    valr = {ru["text"]: ru["share"] for ru in real["rules"]["real_validation"]["rules"]}
    top = sorted(allr, key=lambda t: -max(mean(v) for v in allr[t].values()))[:30]
    P("| Rule | Validation | " + " | ".join(gm) + " |")
    P("|---" * (len(gm) + 2) + "|")
    for t in top:
        P(f"| {t} | {fmt(100 * valr.get(t, float('nan')), '{:.3f}')} | " +
          " | ".join(fmt(100 * mean(allr[t].get(m, [])), "{:.3f}") for m in gm) + " |")
    P("")
    P("### Orderings broken by more than 5% (the larger amount exceeds the smaller by over 5% plus $1), % of returns\n")
    P("| Rule | " + " | ".join(gm) + " |")
    P("|---" * (len(gm) + 1) + "|")
    for t in ["E01700 <= E01500", "E00650 <= E00600"]:
        P(f"| {t} | " + " | ".join(cell(by[m], lambda r, t=t: 100 * rule_share(r, t, "share_by_more_than_5pct"), "{:.2f}", True) for m in gm) + " |")
    P("")
    P("### Any rule broken, by AGI group (%)\n")
    P("| Group | Validation | " + " | ".join(gm) + " |")
    P("|---" * (len(gm) + 2) + "|")
    for g in GROUPS:
        P(f"| {g} | {fmt(100 * real['rules']['real_validation']['any_by_group'][g], '{:.2f}')} | " +
          " | ".join(cell(by[m], lambda r, g=g: 100 * r["rules"]["gen"]["any_by_group"][g], "{:.2f}", True) for m in gm) + " |")
    P("")

    # 4. tax
    P("## 4. Simplified 2015 federal income tax by AGI group\n")
    for key, title, f, sc in [("net_tax_bn", "Net income tax after credits ($bn)", "{:,.0f}", 1),
                              ("avg_rate_net", "Average tax rate: net income tax / AGI (%)", "{:.2f}", 100),
                              ("tax_before_credits_bn", "Income tax before credits ($bn)", "{:,.0f}", 1),
                              ("eitc_bn", "Earned income credit ($bn)", "{:,.1f}", 1),
                              ("ctc_bn", "Child tax credit, including the refundable part ($bn)", "{:,.1f}", 1),
                              ("share_of_net_tax", "Share of total net income tax (%)", "{:.1f}", 100),
                              ("share_positive_tax", "Share of returns with tax before credits (%)", "{:.1f}", 100),
                              ("agi_bn", "AGI ($bn)", "{:,.0f}", 1)]:
        P(f"### {title}\n")
        P("| Group | Real | " + " | ".join(gm) + " |")
        P("|---" * (len(gm) + 2) + "|")
        for g in GROUPS + ["all"]:
            if key == "share_of_net_tax" and g == "all":
                continue
            P(f"| {g} | {fmt(sc * real['tax']['real'][g][key], f)} | " +
              " | ".join(cell(by[m], lambda r, g=g: sc * r["tax"]["gen"][g][key], f, True) for m in gm) + " |")
        P("")

    if TC:
        P("## 4b. Tax-Calculator 6.8.4, 2015 law, by AGI group\n")
        P("Inputs for real and generated records alike: the 30 dollar amounts in the model, filing status, exemptions, "
          "dependent status, children for the child credit and the EITC; age 40 for every head; no AGI remainder "
          "(Tax-Calculator computes taxable Social Security itself). Groups by each file's own AGI as above. "
          "'Clipped' rows: share of returns where qualified dividends exceeded ordinary dividends, or taxable "
          "pensions exceeded total pensions, and were set equal so that Tax-Calculator accepts the record.\n")
        for key, title, f, sc in [("net_tax_bn", "Net individual income tax, iitax ($bn)", "{:,.0f}", 1),
                                  ("avg_rate_net", "Average tax rate: iitax / AGI (%)", "{:.2f}", 100),
                                  ("tax_before_credits_bn", "Tax before credits ($bn)", "{:,.0f}", 1),
                                  ("eitc_bn", "Earned income credit ($bn)", "{:,.1f}", 1),
                                  ("share_of_net_tax", "Share of total iitax (%)", "{:.1f}", 100),
                                  ("tc_agi_bn", "Tax-Calculator's AGI, without the remainder ($bn)", "{:,.0f}", 1)]:
            P(f"### {title}\n")
            P("| Group | Real | " + " | ".join(gm) + " |")
            P("|---" * (len(gm) + 2) + "|")
            for g in GROUPS + ["all"]:
                if key == "share_of_net_tax" and g == "all":
                    continue
                rv = tc_get("real", [g, key])
                P(f"| {g} | {fmt(sc * rv if rv is not None else None, f)} | " +
                  " | ".join(cell(by[m], lambda r, g=g: sc * tc_get(r["label"], [g, key]), f, True) for m in gm) + " |")
            P("")
        P("Clipped before Tax-Calculator (% of returns): " + "; ".join(
            f"{k}: {json.dumps(v.get('clipped'))}" for k, v in TC.items() if isinstance(v, dict)) + "\n")

    # 5. classifier
    P("## 5. Classifier: generated against real\n")
    P("Area under the ROC curve: the chance that the classifier scores a randomly chosen generated record "
      "as more likely generated than a randomly chosen real one (0.5 = cannot tell them apart, 1 = always can). "
      "Weighted, classes balanced, five-fold cross-validated.\n")
    P("| | " + " | ".join(gm) + " |")
    P("|---" * (len(gm) + 1) + "|")
    P("| All returns (one model) | " + " | ".join(cell(by[m], lambda r: r["classifier"]["auc_overall"], "{:.3f}", True) for m in gm) + " |")
    for g in GROUPS:
        P(f"| {g}, model trained within the group | " + " | ".join(
            cell(by[m], lambda r, g=g: r["classifier"]["auc_within_group_models"][g]["auc"], "{:.3f}", True) for m in gm) + " |")
    P("")
    P("### The same classifier on the rank-band tokens (the model's own codes) instead of decoded dollars\n")
    P("| | " + " | ".join(gm) + " |")
    P("|---" * (len(gm) + 1) + "|")
    P("| All returns (one model) | " + " | ".join(cell(by[m], lambda r: r["classifier_tokens"]["auc_overall"], "{:.3f}", True) for m in gm) + " |")
    for g in GROUPS:
        P(f"| {g}, model trained within the group | " + " | ".join(
            cell(by[m], lambda r, g=g: r["classifier_tokens"]["auc_within_group_models"][g]["auc"], "{:.3f}", True) for m in gm) + " |")
    P("")
    for m in gm:
        r0 = by[m][0]
        if "classifier_tokens" in r0:
            imp = list(r0["classifier_tokens"]["importance_auc_drop"].items())[:8]
            P(f"- {m}, tokens, all returns, leans on: " + "; ".join(f"{nm(k)} {v:.3f}" for k, v in imp))
    P("")
    for m in gm:
        r0 = by[m][0]
        if "classifier" not in r0:
            continue
        imp = list(r0["classifier"]["importance_auc_drop"].items())[:8]
        P(f"**{m} ({r0['label']}): variables the all-returns classifier leans on most** (drop in area under the "
          "curve when the variable is shuffled): " + "; ".join(f"{nm(k)} {v:.3f}" for k, v in imp) + "\n")
        for g in GROUPS:
            tf = r0["classifier"]["auc_within_group_models"][g].get("top_features", [])
            P(f"- within {g}: " + "; ".join(f"{nm(k)} {v:.3f}" for k, v in tf))
        P("")

    # 6. copying
    P("## 6. Copying\n")
    P("Distance: Euclidean distance over 37 variables, each replaced by its weighted rank among the real training "
      "records (0 to 1); the demographic codes are left out because the real file lacks them for 42% of records.\n")
    P("| | Real validation | " + " | ".join(gm) + " |")
    P("|---" * (len(gm) + 2) + "|")
    c = lambda fn, f: " | ".join(cell(by[m], fn, f, True) for m in gm)  # noqa: E731
    vr = real.get("copying") or {}
    for lab, vfn, gfn, f in [
        ("Median distance to nearest training record", lambda: vr["validation_nn_distance_pct"]["50"],
         lambda r: r["copying"]["generated_nn_distance_pct"]["50"], "{:.4f}"),
        ("5th percentile of that distance", lambda: vr["validation_nn_distance_pct"]["5"],
         lambda r: r["copying"]["generated_nn_distance_pct"]["5"], "{:.4f}"),
        ("Share nearer than the real records' 1st percentile (%)", lambda: 1.0,
         lambda r: 100 * r["copying"]["share_below_validation_p1"], "{:.2f}"),
        ("Share nearer than the real records' 5th percentile (%)", lambda: 5.0,
         lambda r: 100 * r["copying"]["share_below_validation_p5"], "{:.2f}"),
        ("Share at distance zero (%)", lambda: 100 * vr["validation_share_distance_zero"],
         lambda r: 100 * r["copying"]["share_distance_zero"], "{:.2f}"),
        ("Top 0.1%: median distance", lambda: vr["validation_top01_nn_median"],
         lambda r: r["copying"]["top01_generated_nn_median"], "{:.4f}"),
        ("Top 0.1%: share at distance zero (%)", lambda: float("nan"),
         lambda r: 100 * r["copying"]["top01_generated_share_distance_zero"], "{:.2f}"),
        ("Identical token pattern to a training record (%)", lambda: 100 * vr["token_copy_share_validation"],
         lambda r: 100 * r["copying"]["token_copy_share_generated"], "{:.2f}"),
        ("Same, top 0.1% (%)", lambda: float("nan"),
         lambda r: 100 * r["copying"]["token_copy_share_generated_top01"], "{:.2f}")]:
        try:
            v = vfn()
        except Exception:
            v = None
        P(f"| {lab} | {fmt(v, f)} | {c(gfn, f)} |")
    P("")
    P("### Copying by AGI group: share at distance zero (%) and median distance; real validation / " + " / ".join(gm) + "\n")
    P("| Group | Share at distance zero | Median distance |")
    P("|---|---|---|")
    for g in GROUPS:
        z = [fmt(100 * get(vr, ["by_group", g, "val_share_zero"]) if get(vr, ["by_group", g, "val_share_zero"]) is not None else None, "{:.2f}")]
        z += [cell(by[m], lambda r, g=g: 100 * r["copying"]["by_group"][g]["gen_share_zero"], "{:.2f}") for m in gm]
        md = [fmt(get(vr, ["by_group", g, "val_median"]), "{:.3f}")]
        md += [cell(by[m], lambda r, g=g: r["copying"]["by_group"][g]["gen_median"], "{:.3f}") for m in gm]
        P(f"| {g} | {' / '.join(z)} | {' / '.join(md)} |")
    P("")
    for m in gm:
        r0 = by[m][0]
        tg = get(r0, ["copying", "token_copy_share_generated_by_group"])
        if tg:
            P(f"- {m} identical token pattern by group (%): " + "; ".join(f"{g} {100 * v:.1f}" for g, v in tg.items()))
    P("")
    open(outp, "w").write("\n".join(L) + "\n")
    print("\n".join(L))


if __name__ == "__main__":
    main()
