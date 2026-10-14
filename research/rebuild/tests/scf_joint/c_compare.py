"""Part C: gather every evaluation of a dataset into side-by-side tables.

  python c_compare.py [--datasets expA_clean expB ...] [--update-report]

Reads $S/eval/<dataset>/*/summary.json and detail_*.csv. Writes
$S/eval/<dataset>/compare.md and, with --update-report, replaces the
generated block for that dataset in EVAL_REPORT.md (aggregate tables only).

Columns: the two yardsticks (sampling noise: half of the true records
against the other half; no information: wealth vectors shuffled across
records), then each method. A method with several training seeds shows the
mean over seeds and, in brackets, the lowest and highest seed.
"""
import argparse
import glob
import json
import os
import re
import sys
from datetime import datetime

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from c_common import S, WEALTH, INC_LABELS, label_group  # noqa: E402

REPORT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "EVAL_REPORT.md")

# (key, description, scale, decimals, kind)
#   kind "gap": smaller is better; yardstick column shows its typical size
#   kind "level": the truth's value is shown alongside
ROWS = [
    ("hold_all_mae_pp", "Share holding each component, all records: average gap (percentage points)", 1, 2, "gap"),
    ("hold_inc_mae_pp", "Share holding, by income group (24 components x 4 groups): average gap (points)", 1, 2, "gap"),
    ("hold_age_mae_pp", "Share holding, by age range: average gap (points)", 1, 2, "gap"),
    ("amt_inc_rank_mae_pctile", "Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks", 1, 2, "gap"),
    ("amt_inc_dollar_mae_pct", "Same, in dollars: average gap (%)", 1, 1, "gap"),
    ("amt_age_rank_mae_pctile", "Amounts among holders by age range: average gap in percentile ranks", 1, 2, "gap"),
    ("pattern_tvd", "Patterns of holding across the 24 components: share of weight in the wrong pattern (%)", 100, 1, "gap"),
    ("pair2x2_tvd_mean", "Pairs of components (276 two-by-two holding tables): average share misplaced (%)", 100, 2, "gap"),
    ("cond_mae_pp", "Selected conditional holding rates (12, e.g. home among stockholders): average gap (points)", 1, 2, "gap"),
    ("income_corr_mae", "Rank correlation of each amount with income, among holders: average gap", 1, 3, "gap"),
    ("pair_corr_mae", "Rank correlation between amounts, among joint holders: average gap", 1, 3, "gap"),
    ("topband1_ratio_mean", "Holders in the top 1% rank band of their component: completed / true, averaged over components", 1, 2, "ratio"),
    ("topband01_ratio_mean", "Same for the top 0.1% rank band", 1, 2, "ratio"),
    ("nw_p50_ratio", "Median net worth, completed / true", 1, 3, "ratio"),
    ("nw_p90_ratio", "90th percentile of net worth, completed / true", 1, 3, "ratio"),
    ("nw_p99_ratio", "99th percentile of net worth, completed / true", 1, 3, "ratio"),
    ("nw_top10", "Top 10% share of net worth (%)", 1, 1, "level"),
    ("nw_top1", "Top 1% share of net worth (%)", 1, 1, "level"),
    ("nw_top0.1", "Top 0.1% share of net worth (%)", 1, 1, "level"),
    ("nw_inc_mean_mape", "Mean net worth by income group: average gap (%)", 1, 1, "gap"),
    ("clone", "Top 1% by net worth: share whose wealth tokens repeat another household's (%)", 1, 1, "level"),
    ("classifier_auc", "Classifier telling completed from true records: AUC (0.5 = cannot tell)", 1, 3, "auc"),
]

NAMES = [(r"drf", "production forest"), (r"seq|tree|hgb", "sequential trees"),
         (r"bench|scfonly|scf_only|h2only|half2", "one-source model"), (r"joint", "joint model")]


def method_name(group):
    for pat, nm in NAMES:
        if re.search(pat, group):
            return f"{nm} ({group})"
    return group


def load(ds):
    out = {}
    for f in sorted(glob.glob(os.path.join(ROOT, ds, "*", "summary.json"))):
        s = json.load(open(f))
        s["_dir"] = os.path.dirname(f)
        if s.get("file") and not os.path.exists(s["file"]):
            print(f"  {ds}/{s['label']}: completed file no longer exists; left out of the tables")
            continue
        out[s["label"]] = s
    return out


def value(head, key, kind, ys=False):
    if kind == "level":
        if key == "clone":
            return head.get("clone_gap_pp") if ys else head.get("clone_cand")
        return head.get(f"{key}_gap_pp") if ys else head.get(f"{key}_cand")
    if kind == "ratio" and ys:
        return head.get(key + "_absdev")
    return head.get(key)


def truth_value(head, key):
    if key == "clone":
        return head.get("clone_ref")
    return head.get(f"{key}_ref")


def fmt(x, d):
    if x is None or (isinstance(x, float) and not np.isfinite(x)):
        return ""
    return f"{x:.{d}f}"


def order_key(g):
    # main columns (John, 9 October): the model trained with full loss weights,
    # plain completion, own source tag and presented as SCF; the one-source model
    # with full weights; the production forest, its survey-weighted variant, and
    # the sequential trees. Everything else goes to the "other variants" table.
    for i, pat in enumerate(["joint_w1_plain", "joint_w1_asSCF_plain", "joint_w1_scf_plain",
                             "joint_w1_puf_plain", "joint_w1_puf_asSCF_plain",
                             "half2_w1_plain", "scfonly_w1_scf_plain",
                             "_drf", "_drfsw", "_seqtrees", "_drf-puf", "_drfsw-puf", "_seqtrees-puf"]):
        if g.endswith(pat):
            return (i, g)
    return (99, g)


def headline_table(ev, small=False):
    ys = ev.get("ref_sampling")
    sh = ev.get("ref_shuffled")
    groups = {}
    for lab, s in ev.items():
        if s.get("kind") == "completed" and ((order_key(label_group(lab))[0] == 99) == small):
            groups.setdefault(label_group(lab), []).append(s)
    gnames = sorted(groups, key=order_key)
    hdr = ["Measure", "Truth", "Sampling noise", "No information (shuffled)"] + [method_name(g) for g in gnames]
    lines = ["| " + " | ".join(hdr) + " |", "|" + "---|" * len(hdr)]
    for key, desc, sc, d, kind in ROWS:
        tv = ""
        src = sh if sh else (groups[gnames[0]][0] if groups else None)
        if kind == "level" and src:
            tv = fmt(truth_value(src["headline"], key), d)
        cells = [desc, tv]
        if ys:
            v = value(ys["headline"], key, kind, ys=True)
            if kind == "level" and v is not None:
                cells.append("±" + fmt(v, d))
            elif kind == "ratio" and v is not None:
                cells.append("±" + fmt(v, d))
            else:
                cells.append(fmt(None if v is None else v * sc, d))
        else:
            cells.append("")
        cells.append(fmt(None if not sh or value(sh["headline"], key, kind) is None
                         else value(sh["headline"], key, kind) * sc, d))
        for g in gnames:
            vals = [value(s["headline"], key, kind) for s in groups[g]]
            vals = [v * sc for v in vals if v is not None]
            if not vals:
                cells.append("")
            elif len(vals) == 1:
                cells.append(fmt(vals[0], d))
            else:
                cells.append(f"{fmt(np.mean(vals), d)} [{fmt(min(vals), d)}-{fmt(max(vals), d)}]")
        lines.append("| " + " | ".join(cells) + " |")
    seeds = ", ".join(f"{method_name(g)}: {len(groups[g])} seed(s)"
                      + (" (WARNING: completed before the current data; dollars may be on an old scale)"
                         if any(s.get("stale_dollars") for s in groups[g]) else "") for g in gnames)
    return "\n".join(lines), groups, gnames, seeds


def noise_table(groups, gnames):
    keys = ["hold_inc_mae_pp", "amt_inc_rank_mae_pctile", "pattern_tvd", "nw_inc_mean_mape", "classifier_auc"]
    rows = [r for r in ROWS if r[0] in keys]
    hdr = ["Measure"] + [method_name(g) for g in gnames]
    lines = ["| " + " | ".join(hdr) + " |", "|" + "---|" * len(hdr)]
    for key, desc, sc, d, kind in rows:
        cells = [desc]
        for g in gnames:
            vals = [s["completion_noise"].get(key) for s in groups[g] if s.get("completion_noise")]
            vals = [v * sc for v in vals if v is not None]
            cells.append(fmt(np.mean(vals), d) if vals else "")
        lines.append("| " + " | ".join(cells) + " |")
    return "\n".join(lines)


def detail_mean(groups, g, name):
    dfs = []
    for s in groups[g]:
        f = os.path.join(s["_dir"], f"detail_{name}.csv")
        if os.path.exists(f):
            dfs.append(pd.read_csv(f))
    if not dfs:
        return None
    return dfs


def holding_table(ev, groups, gnames):
    """Share holding each component, all records: truth, each method, and the
    sampling noise for that cell."""
    base = None
    for g in gnames:
        dfs = detail_mean(groups, g, "hold")
        if not dfs:
            continue
        x = pd.concat(dfs)
        x = x[x["by"] == "all"].groupby("variable", sort=False)[["ref", "cand"]].mean()
        if base is None:
            base = pd.DataFrame({"Truth": x["ref"]})
        base[method_name(g)] = x["cand"]
    if base is None:
        sh = ev.get("ref_shuffled")
        if not sh:
            return ""
        x = pd.read_csv(os.path.join(sh["_dir"], "detail_hold.csv"))
        x = x[x["by"] == "all"].set_index("variable")
        base = pd.DataFrame({"Truth": x["ref"]})
    ys = ev.get("ref_sampling")
    if ys:
        n = pd.read_csv(os.path.join(ys["_dir"], "detail_hold.csv"))
        n = n[n["by"] == "all"].set_index("variable")["noise_abs_gap"]
        base["Sampling noise (±)"] = n
    base = base.reindex(WEALTH) * 100
    hdr = ["Component"] + list(base.columns)
    lines = ["| " + " | ".join(hdr) + " |", "|" + "---|" * len(hdr)]
    for v, r in base.iterrows():
        lines.append("| " + " | ".join([v] + [fmt(x, 1) for x in r.to_numpy()]) + " |")
    return "\n".join(lines)


def by_income_table(ev, groups, gnames, comps=("primary_home", "equities", "pass_throughs", "dc",
                                               "primary_mortgage", "bonds", "trusts", "other_home")):
    rows = None
    for g in gnames:
        dfs = detail_mean(groups, g, "hold")
        if not dfs:
            continue
        x = pd.concat(dfs)
        x = x[(x["by"] == "inc") & x["variable"].isin(comps)].groupby(["variable", "group"])[["ref", "cand"]].mean()
        if rows is None:
            rows = pd.DataFrame({"Truth": x["ref"]})
        rows[method_name(g)] = x["cand"]
    if rows is None:
        return ""
    ys = ev.get("ref_sampling")
    if ys:
        n = pd.read_csv(os.path.join(ys["_dir"], "detail_hold.csv"))
        n = n[n["by"] == "inc"].set_index(["variable", "group"])["noise_abs_gap"]
        rows["Sampling noise (±)"] = n.reindex(rows.index)
    rows = rows * 100
    hdr = ["Component", "Income group"] + list(rows.columns)
    lines = ["| " + " | ".join(hdr) + " |", "|" + "---|" * len(hdr)]
    for c in comps:
        for k in range(4):
            if (c, k) not in rows.index:
                continue
            r = rows.loc[(c, k)]
            lines.append("| " + " | ".join([c, INC_LABELS[k]] + [fmt(v, 1) for v in r.to_numpy()]) + " |")
    return "\n".join(lines)


def nw_tables(ev, groups, gnames):
    t1 = t2 = None
    for g in gnames:
        dfs = detail_mean(groups, g, "nw")
        if dfs:
            x = pd.concat(dfs).groupby("stat", sort=False)[["ref", "cand"]].mean()
            if t1 is None:
                t1 = pd.DataFrame({"Truth": x["ref"]})
            t1[method_name(g)] = x["cand"]
        dfs = detail_mean(groups, g, "nw_inc")
        if dfs:
            x = pd.concat(dfs).groupby("group", sort=False)[["ref_mean", "cand_mean", "ref_median", "cand_median"]].mean()
            if t2 is None:
                t2 = pd.DataFrame({"Truth, mean": x["ref_mean"], "Truth, median": x["ref_median"]})
            t2[method_name(g) + ", mean"] = x["cand_mean"]
            t2[method_name(g) + ", median"] = x["cand_median"]
    out = []
    ys = ev.get("ref_sampling")
    if t1 is not None:
        if ys:
            n = pd.read_csv(os.path.join(ys["_dir"], "detail_nw.csv")).set_index("stat")
            t1["Sampling noise (± %)"] = 100 * n["rel"].reindex(t1.index)
        hdr = ["Net worth statistic"] + list(t1.columns)
        lines = ["| " + " | ".join(hdr) + " |", "|" + "---|" * len(hdr)]
        for st, r in t1.iterrows():
            vals = []
            for c, v in zip(t1.columns, r.to_numpy()):
                if c.startswith("Sampling"):
                    vals.append(fmt(v, 1))
                elif "share" in st:
                    vals.append(fmt(100 * v, 1) + "%")
                else:
                    vals.append(f"{v / 1000:,.0f}k")
            lines.append("| " + " | ".join([st] + vals) + " |")
        out.append("\n".join(lines))
    if t2 is not None:
        if ys:
            n = pd.read_csv(os.path.join(ys["_dir"], "detail_nw_inc.csv")).set_index("group")
            t2["Sampling noise, mean (± %)"] = 100 * n["rel_mean"].reindex(t2.index)
        hdr = ["Income group"] + list(t2.columns)
        lines = ["| " + " | ".join(hdr) + " |", "|" + "---|" * len(hdr)]
        for gname, r in t2.iterrows():
            vals = [fmt(v, 1) if c.startswith("Sampling") else f"{v / 1000:,.0f}k"
                    for c, v in zip(t2.columns, r.to_numpy())]
            lines.append("| " + " | ".join([gname] + vals) + " |")
        out.append("\n".join(lines))
    return "\n\n".join(out)


def simple_detail(ev, groups, gnames, name, keys, scale, d, title_cols):
    t = None
    for g in gnames:
        dfs = detail_mean(groups, g, name)
        if not dfs:
            continue
        x = pd.concat(dfs).groupby(keys, sort=False)[["ref", "cand"]].mean()
        if t is None:
            t = pd.DataFrame({"Truth": x["ref"]})
        t[method_name(g)] = x["cand"]
    if t is None:
        return ""
    ys = ev.get("ref_sampling")
    if ys:
        n = pd.read_csv(os.path.join(ys["_dir"], f"detail_{name}.csv")).set_index(keys)["noise_abs_gap"]
        t["Sampling noise (±)"] = n.reindex(t.index)
    t = t * scale
    hdr = title_cols + list(t.columns)
    lines = ["| " + " | ".join(hdr) + " |", "|" + "---|" * len(hdr)]
    for idx, r in t.iterrows():
        idx = idx if isinstance(idx, tuple) else (idx,)
        lines.append("| " + " | ".join(list(map(str, idx)) + [fmt(v, d) for v in r.to_numpy()]) + " |")
    return "\n".join(lines)


def top_band_table(groups, gnames):
    t = None
    for g in gnames:
        dfs = detail_mean(groups, g, "top_band")
        if not dfs:
            continue
        x = pd.concat(dfs).groupby("variable", sort=False).mean(numeric_only=True)
        if t is None:
            t = pd.DataFrame({"Truth, top 1%": x["ref_top1"], "Truth, top 0.1%": x["ref_top01"]})
            if "ref_top1_unweighted" in x:
                t["Truth unweighted (sample), top 1%"] = x["ref_top1_unweighted"]
                t["Truth unweighted (sample), top 0.1%"] = x["ref_top01_unweighted"]
        t[method_name(g) + ", top 1%"] = x["cand_top1"]
        t[method_name(g) + ", top 0.1%"] = x["cand_top01"]
    if t is None:
        return ""
    t = t.reindex(WEALTH) * 100
    hdr = ["Component"] + list(t.columns)
    lines = ["| " + " | ".join(hdr) + " |", "|" + "---|" * len(hdr)]
    for v, r in t.iterrows():
        lines.append("| " + " | ".join([v] + [fmt(x, 2) for x in r.to_numpy()]) + " |")
    return "\n".join(lines)


def importance_lines(groups, gnames):
    out = []
    for g in gnames:
        dfs = detail_mean(groups, g, "importance")
        if not dfs:
            continue
        x = pd.concat(dfs).groupby("variable")["auc_drop"].mean().sort_values(ascending=False).head(6)
        out.append(f"- {method_name(g)}: " + ", ".join(f"{v} ({a:.3f})" for v, a in x.items()))
    return "\n".join(out)


def build(ds):
    ev = load(ds)
    if not ev:
        return None
    head, groups, gnames, seeds = headline_table(ev)
    ys = ev.get("ref_sampling")
    n = (ys or next(iter(ev.values())))
    parts = [f"### Dataset `{ds}`",
             f"Evaluation set: {n.get('n_records', 0):,} records"
             + (f" in {ys['n_households']:,} households" if ys else "")
             + f". Generated {datetime.now():%Y-%m-%d %H:%M}." + (f" Runs: {seeds}." if seeds else ""),
             "",
             "**Headline.** \"Sampling noise\" is the typical gap between two random halves of the "
             "true records (households kept whole), averaged over five splits; for the "
             "level rows it is the typical gap, in points, and for the ratio rows the typical "
             "distance from 1. \"No information\" gives every record the wealth of a random other "
             "record. Method columns: mean over training seeds [lowest-highest seed].",
             "", head, ""]
    small_head, sgroups, sgnames, sseeds = headline_table(ev, small=True)
    if sgroups:
        parts += ["**Other variants** (same measures): runs without `_w1` are the earlier "
                  "configuration (survey weight as a model variable; `plain` as drawn, `wc` "
                  "weight-corrected); `_small` smaller model; `_w1`, `_wsqrt` "
                  "model trained with each record's loss weighted by its population weight or its "
                  "square root, and `_nowt` the unweighted control (in these three the survey "
                  "weight is not a model variable and the plain and weight-corrected files are "
                  "identical); `drfsw` the production forest fitted with survey weights instead of "
                  "a weighted bootstrap (a diagnostic, not production). The detail tables below "
                  "show only the main runs.", "", small_head, ""]
    if groups:
        parts += ["**Completion noise** (one completion draw against another, same file): the "
                  "part of each gap that comes from drawing at random, not from the model.", "",
                  noise_table(groups, gnames), ""]
    t = holding_table(ev, groups, gnames)
    if t:
        parts += ["**Share holding each component, all records (%).**", "", t, ""]
    t = by_income_table(ev, groups, gnames)
    if t:
        parts += ["**Share holding by income group, selected components (%).** Income group: "
                  "weighted rank of the total of the seven incomes within source and year.", "", t, ""]
    t = nw_tables(ev, groups, gnames)
    if t:
        parts += ["**Net worth** (assets minus debts, dollars of the survey year; the four "
                  "unrealized-gain variables are not part of net worth).", "", t, ""]
    t = top_band_table(groups, gnames)
    if t:
        parts += ["**The top of each component, in rank.** Share of holders (with a positive amount) "
                  "in the top 1% and top 0.1% rank bands of that component (%). By construction "
                  "the weighted truth is near 1% and 0.1%; a method above that puts too many holders "
                  "at the very top, which inflates net worth at the top once decoded to dollars. The "
                  "unweighted columns count sample records instead of population weight: they show "
                  "how far the SCF sample over-represents the top, which is what a model that "
                  "learned the sample, not the population, would reproduce.", "", t, ""]
    t = simple_detail(ev, groups, gnames, "cond", ["holds", "given"], 100, 1, ["Holds", "Among those holding"])
    if t:
        parts += ["**Conditional holding rates (%).** Rows where the second column is an income "
                  "(business, int_div, capital_gains, rent, ss_pens) mean \"among units with that "
                  "income nonzero\".", "", t, ""]
    t = simple_detail(ev, groups, gnames, "income_corr", ["variable"], 1, 2, ["Component"])
    if t:
        parts += ["**Rank correlation of each amount with total income, among holders** "
                  "(blank where fewer than 200 true holder records).", "", t, ""]
    t = importance_lines(groups, gnames)
    if t:
        parts += ["**What the classifier relies on** (drop in AUC when the variable is scrambled):", "", t, ""]
    pt = puf_table(ev)
    if pt:
        parts += ["", pt]
    return "\n".join(parts)


def puf_table(ev):
    """Experiment B: PUF records completed at 2015 against the SCF."""
    groups = {}
    for lab, s in ev.items():
        if s.get("kind") == "puf":
            groups.setdefault(label_group(lab[:-4] if lab.endswith("_puf") else lab), []).append(s)
    if not groups:
        return ""
    gn = [g for g in sorted(groups, key=order_key) if order_key(g)[0] < 99]
    if not gn:
        return ""
    first = groups[gn[0]][0]["headline"]
    rows = [("hold_all_mae_pp", "hold_all_wave_change_pp", "Share holding, all records: average gap (points)", 2),
            ("hold_inc_mae_pp", "hold_inc_wave_change_pp", "Share holding by income group: average gap (points)", 2),
            ("hold_age_mae_pp", "hold_age_wave_change_pp", "Share holding by age range: average gap (points)", 2),
            ("amt_inc_dollar_mae_pct", "amt_inc_wave_change_pct", "Amounts among holders by income group: average gap (%)", 1)]
    hdr = ["Measure", "SCF 2013 against SCF 2016 (scale)"] + [method_name(g) for g in gn]
    L = ["#### PUF 2015 records completed with wealth, against the SCF interpolated to 2015", "",
         "Gaps are between the completed PUF and the SCF interpolated to 2015 (one third 2013, two "
         "thirds 2016). The scale column is the SCF's own change between its 2013 and 2016 waves "
         "(including its sampling noise). Income groups are ranks within each source. The SCF side "
         "leaves out split-off units (no SCF wealth), so it covers fewer low-wealth units than the PUF. "
         "Full tables per run: `$S/eval/expB/<label>_puf/report.md`.", "",
         "| " + " | ".join(hdr) + " |", "|" + "---|" * len(hdr)]
    for k, ks, desc, d in rows:
        cells = [desc, fmt(first.get(ks), d)]
        for g in gn:
            v = [s["headline"].get(k) for s in groups[g] if s["headline"].get(k) is not None]
            cells.append(f"{fmt(np.mean(v), d)} [{fmt(min(v), d)}-{fmt(max(v), d)}]" if len(v) > 1 else fmt(v[0] if v else None, d))
        L.append("| " + " | ".join(cells) + " |")
    hdr = ["Net worth", "SCF 2013", "SCF 2016", "SCF interpolated 2015"] + [method_name(g) for g in gn]
    L += ["", "| " + " | ".join(hdr) + " |", "|" + "---|" * len(hdr)]
    for st in ["p50", "p90", "p99", "mean", "top10%_share", "top1%_share", "top0.1%_share"]:
        key = f"nw_{st}"
        if key not in first:
            continue
        f = (lambda x: f"{100 * x:.1f}%") if "share" in st else (lambda x: f"{x / 1000:,.0f}k")
        cells = [st, f(first[key]["scf2013"]), f(first[key]["scf2016"]), f(first[key]["interp2015"])]
        for g in gn:
            v = [s["headline"][key]["puf2015"] for s in groups[g]]
            cells.append(f(np.mean(v)))
        L.append("| " + " | ".join(cells) + " |")
    return "\n".join(L)


def update_report(ds, md):
    begin, end = f"<!-- BEGIN GENERATED {ds} -->", f"<!-- END GENERATED {ds} -->"
    txt = open(REPORT).read() if os.path.exists(REPORT) else ""
    block = f"{begin}\n{md}\n{end}"
    if begin in txt:
        a, b = txt.index(begin), txt.index(end) + len(end)
        txt = txt[:a] + block + txt[b:]
    else:
        txt = txt.rstrip() + "\n\n" + block + "\n"
    open(REPORT, "w").write(txt)


ROOT = None


def main():
    global ROOT
    ap = argparse.ArgumentParser()
    ap.add_argument("--datasets", nargs="*")
    ap.add_argument("--eval-root", default=os.path.join(S, "eval"))
    ap.add_argument("--update-report", action="store_true")
    args = ap.parse_args()
    ROOT = args.eval_root
    dss = args.datasets or sorted(d for d in os.listdir(ROOT) if os.path.isdir(os.path.join(ROOT, d)))
    for ds in dss:
        md = build(ds)
        if md is None:
            continue
        open(os.path.join(ROOT, ds, "compare.md"), "w").write(md + "\n")
        print(f"wrote {os.path.join(ROOT, ds, 'compare.md')}")
        if args.update_report:
            update_report(ds, md)


if __name__ == "__main__":
    main()
