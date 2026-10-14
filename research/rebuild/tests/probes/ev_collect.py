"""Probe EV: collect ev_score.py and v_score.py outputs into one markdown file.

    python ev_collect.py <ev scratch dir> <markdown out>

Methods are the part of each label before " | " (runs after it). The summary
compares "transformer" with the main trees method: for the SCF and PUF parts
the first method whose name starts with "trees" and contains the preferred
arm (see MAIN_TREES), for generated records the first "Trees" scorecard.
Weighted aggregates only.
"""
import glob
import json
import os
import re
import sys
from collections import OrderedDict

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import v_collect as VC  # noqa: E402

INC = ["Bottom 50%", "50-90%", "90-99%", "99-99.9%", "Top 0.1%"]
AGE = ["under 35", "35-44", "45-54", "55-64", "65-74", "75 and over"]
MAR = ["single", "married filing jointly", "married filing separately", "head of household"]
HOLD = ["primary_home", "primary_mortgage", "equities", "bonds", "dc", "pass_throughs", "other_home", "cash",
        "credit_cards"]
HOLD_NAMES = {"primary_home": "own a home", "primary_mortgage": "have a mortgage", "equities": "hold stocks",
              "bonds": "hold bonds", "dc": "hold retirement accounts", "pass_throughs": "hold a business",
              "other_home": "own other real estate", "cash": "hold cash or deposits", "credit_cards": "carry card debt"}
MAIN = ["primary_home", "equities", "pass_throughs", "dc"]
MAIN_NAMES = {"primary_home": "home value", "equities": "stocks", "pass_throughs": "business", "dc": "retirement accounts"}
INC_NAMES = ["wages", "Schedule C", "Schedule E", "long-term gains", "dividends", "taxable interest",
             "taxable pensions", "Social Security", "farm", "tax-exempt interest", "unemployment"]
NAMED = ["mortgage among homeowners", "stocks among business owners", "business gains among business owners",
         "mortgage among owners of other homes"]


def load_dir(d):
    by = OrderedDict()
    truth = None
    for f in sorted(glob.glob(f"{d}/*.json")):
        r = json.load(open(f))
        if os.path.basename(f) == "truth.json":
            truth = r
            continue
        m = r["label"].split(" | ")[0].strip()
        by.setdefault(m, []).append(r)
    order = sorted(by, key=lambda m: (0 if m == "transformer" else 1 if m.startswith("transformer") else
                                      2 if m.startswith("trees") and "hidden" not in m else 3, m))
    return truth, OrderedDict((m, by[m]) for m in order)


def main_trees(by, prefer=("zero", "split", "")):
    """The trees method shown in the summary: probe TR's main files (minimum
    leaf 50) by exact name when present, so later TR variants do not move it."""
    cands = [m for m in by if m.startswith("trees") and "hidden" not in m]
    for p in prefer:
        for exact in (f"trees {p}_leaf50", f"trees {p}"):
            if exact in cands:
                return exact
    for p in prefer:
        for m in cands:
            if p in m:
                return m
    return None


def mean_rng(vals, f):
    vals = [v for v in vals if v is not None and np.isfinite(v)]
    if not vals:
        return "pending"
    s = f.format(np.mean(vals))
    if len(vals) > 1:
        s += f" [{f.format(min(vals))}–{f.format(max(vals))}]"
    return s


def money(x):
    if x is None or not np.isfinite(x):
        return "pending"
    a = abs(x)
    if a >= 1e6:
        return f"${x / 1e6:,.1f}m"
    return f"${x / 1e3:,.0f}k"


def better(a, b, se, lower=True, names=("transformer", "trees")):
    if a is None or b is None or se is None or not np.isfinite(a) or not np.isfinite(b):
        return "pending"
    d = a - b
    if abs(d) <= 2 * se:
        return "neither (within 2 sampling errors)"
    return names[0] if (d < 0) == lower else names[1]


# ------------------------------------------------------------------------------------------------ SCF
SCF_ROWS = [
    ("energy", "Per-record score: energy score on ranks of 20 assets and debts (lower is better)", "{:.4f}"),
    ("nw_score", "Per-record score on the rank of net worth alone (lower is better)", "{:.4f}"),
    ("inc_nw_mean_pct_groups", "Mean net worth by income group: average miss (%)", "{:.1f}"),
    ("inc_nw_median_pct_groups", "Median net worth by income group: average miss (%)", "{:.1f}"),
    ("inc_hold_pp", "Holding rates of 9 items by income group: average gap (points)", "{:.2f}"),
    ("age_nw_mean_pct_groups", "Mean net worth by age group: average miss (%)", "{:.1f}"),
    ("age_hold_pp", "Holding rates by age group: average gap (points)", "{:.2f}"),
    ("mar_nw_mean_pct_groups", "Mean net worth by filing status: average miss (%)", "{:.1f}"),
    ("mar_hold_pp", "Holding rates by filing status: average gap (points)", "{:.2f}"),
    ("hold_inc_age_pp", "Untargeted: holding rates of 20 items by income × age, average gap (points)", "{:.2f}"),
    ("nw_inc_age_pct", "Untargeted: mean net worth by income × age, average miss (% of group mean)", "{:.1f}"),
    ("pair_joint_inc_pp", "Untargeted: joint holding of 190 pairs of items within income groups, average gap (points)", "{:.2f}"),
    ("pair_joint_pp", "Untargeted: joint holding of 190 pairs of items, all households, average gap (points)", "{:.2f}"),
    ("corr_abs", "Untargeted: rank correlation of net worth with 11 incomes, average gap", "{:.3f}"),
    ("top1_share_pp", "Untargeted: top 1% share of net worth within income groups, average gap (points)", "{:.2f}"),
    ("mortgage_homeowners_inc_pp", "Untargeted: share of homeowners with a mortgage within income groups, average gap (points)", "{:.2f}"),
]


def scf_metric(r, k):
    return r[k] if k in ("energy", "nw_score") else r["compare"][k]


def scf_boot(r, k):
    return np.array(r["boot"][k] if k in ("energy", "nw_score") else r["boot"]["compare"][k])


def scf_summary_rows(truth, by, tm, trm):
    rows = []
    for k, lab, f in SCF_ROWS:
        tv = [scf_metric(r, k) for r in by.get(tm, [])]
        rv = [scf_metric(r, k) for r in by.get(trm, [])] if trm else []
        ta = np.mean(tv) if tv else None
        ra = np.mean(rv) if rv else None
        if tv and rv:
            diff = np.mean([scf_boot(r, k) for r in by[tm]], 0) - np.mean([scf_boot(r, k) for r in by[trm]], 0)
            se = float(np.std(diff))
            ses = f"{f.format(se)} (of the difference)"
        elif tv:
            se = float(np.std(np.mean([scf_boot(r, k) for r in by[tm]], 0)))
            ses = f"{f.format(se)} (of the transformer's value)"
        else:
            se, ses = None, "pending"
        base = ""
        if truth and "baseline_marginal" in truth:
            bm = truth["baseline_marginal"]
            bv = bm.get(k, bm["compare"].get(k))
            if bv is not None:
                base = f"; random other household: {f.format(bv)}"
        rows.append((f"{lab}{base}", "0" if k not in ("energy", "nw_score") else "—",
                     mean_rng(tv, f), mean_rng(rv, f) if trm else "pending", ses,
                     better(ta, ra, se) if (tv and rv) else "pending"))
    return rows


def scf_level_table(P, truth, by, fam, labels):
    t, sd = truth["truth"], truth.get("truth_boot_sd", {})
    ms = list(by)
    P(f"| Group | Statistic | Truth | Truth's sampling error | " + " | ".join(ms) + " |")
    P("|---" * (4 + len(ms)) + "|")
    for gi, g in enumerate(labels):
        if t[f"{fam}_w"][gi] <= 0:
            continue
        stats = [("mean net worth", f"{fam}_nw_mean", None, money), ("median net worth", f"{fam}_nw_median", None, money)]
        stats += [(f"{HOLD_NAMES[h]} (%)", f"{fam}_hold", HOLD.index(h), lambda x: f"{100 * x:.1f}")
                  for h in ("primary_home", "primary_mortgage", "equities", "pass_throughs", "dc")]
        stats += [(f"mean {MAIN_NAMES[h]}", f"{fam}_main_mean", MAIN.index(h), money) for h in MAIN]
        for name, key, j, fm in stats:
            tv = t[key][gi] if j is None else t[key][gi][j]
            sv = sd.get(key)
            sv = (sv[gi] if j is None else sv[gi][j]) if sv is not None else None
            cells = []
            for m in ms:
                vals = [(r["levels"][key][gi] if j is None else r["levels"][key][gi][j]) for r in by[m]]
                cells.append(fm(np.mean(vals)))
            P(f"| {g} | {name} | {fm(tv)} | {fm(sv) if sv is not None else '—'} | " + " | ".join(cells) + " |")
    P("")


def scf_untargeted_levels(P, truth, by):
    t, sd = truth["truth"], truth.get("truth_boot_sd", {})
    ms = list(by)
    hdr = "| | Truth | Truth's sampling error | " + " | ".join(ms) + " |"
    sep = "|---" * (3 + len(ms)) + "|"
    P("Rank correlation of net worth with each income:\n")
    P(hdr)
    P(sep)
    for j, n in enumerate(INC_NAMES):
        P(f"| {n} | {t['corr'][j]:.3f} | {sd['corr'][j]:.3f} | " +
          " | ".join(f"{np.mean([r['levels']['corr'][j] for r in by[m]]):.3f}" for m in ms) + " |")
    P("\nTop 1% share of net worth within each income group (%):\n")
    P(hdr)
    P(sep)
    for j, n in enumerate(INC):
        P(f"| {n} | {100 * t['top1_share_inc'][j]:.1f} | {100 * sd['top1_share_inc'][j]:.1f} | " +
          " | ".join(f"{100 * np.mean([r['levels']['top1_share_inc'][j] for r in by[m]]):.1f}" for m in ms) + " |")
    P("\nHolding one item given another (%):\n")
    P(hdr)
    P(sep)
    for j, n in enumerate(NAMED):
        P(f"| {n} | {100 * t['named'][j]:.1f} | {100 * sd['named'][j]:.1f} | " +
          " | ".join(f"{100 * np.mean([r['levels']['named'][j] for r in by[m]]):.1f}" for m in ms) + " |")
    for j, n in enumerate(INC):
        P(f"| mortgage among homeowners, {n} | {100 * t['named_by_inc'][j]:.1f} | {100 * sd['named_by_inc'][j]:.1f} | " +
          " | ".join(f"{100 * np.mean([r['levels']['named_by_inc'][j] for r in by[m]]):.1f}" for m in ms) + " |")
    P("\nPer-record score by income group (energy score, lower is better):\n")
    P("| | " + " | ".join(ms) + " |")
    P("|---" * (1 + len(ms)) + "|")
    for j, n in enumerate(INC):
        P(f"| {n} | " + " | ".join(f"{np.mean([r['energy_by_inc'][j] for r in by[m]]):.4f}" for m in ms) + " |")
    P("")


# ------------------------------------------------------------------------------------------------ PUF
PUF_LEVELS = [("All returns, mean", "mean", money), ("All returns, median", "median", money)]
for _g in INC + ["Top 1%"]:
    PUF_LEVELS.append((f"Mean, {_g}", f"{_g}|mean", money))
for _g in INC + ["Top 1%"]:
    PUF_LEVELS.append((f"Median, {_g}", f"{_g}|median", money))
for _h, _hn in (("primary_home", "Own a home"), ("equities", "Hold stocks"), ("pass_throughs", "Hold a business"),
                ("primary_mortgage", "Have a mortgage")):
    PUF_LEVELS.append((f"{_hn}, all returns (%)", f"hold_{_h}", lambda x: f"{100 * x:.1f}"))
    for _g in INC:
        PUF_LEVELS.append((f"{_hn}, {_g} (%)", f"{_g}|hold_{_h}", lambda x: f"{100 * x:.1f}"))
PUF_LEVELS += [("Top 1% share of net worth (%)", "top1_share", lambda x: f"{100 * x:.1f}"),
               ("Top 0.1% share of net worth (%)", "top01_share", lambda x: f"{100 * x:.1f}")]


def puf_table(P, truth, by):
    ms = list(by)
    s, sd = truth["scf"], truth["scf_boot_sd"]
    P("| | SCF 2015 | SCF sampling error | " + " | ".join(ms) + " |")
    P("|---" * (3 + len(ms)) + "|")
    for lab, k, fm in PUF_LEVELS:
        P(f"| {lab} | {fm(s[k])} | {fm(sd[k])} | " + " | ".join(mean_rng_fmt([r['stats'][k] for r in by[m]], fm) for m in ms) + " |")
    P(f"| All returns with split-off units at zero, mean | {money(s['splitoff_zero|mean'])} | {money(sd['splitoff_zero|mean'])} | "
      + " | ".join("" for m in ms) + " |")
    P(f"| All returns with split-off units at zero, median | {money(s['splitoff_zero|median'])} | {money(sd['splitoff_zero|median'])} | "
      + " | ".join("" for m in ms) + " |")
    P("| PUF returns that are not dependent filers, median | | | " +
      " | ".join(money(np.mean([r['not_dependent_filers']['median'] for r in by[m]])) for m in ms) + " |")
    P("| PUF returns that are not dependent filers, own a home (%) | | | " +
      " | ".join(f"{100 * np.mean([r['not_dependent_filers']['hold_primary_home'] for r in by[m]]):.1f}" for m in ms) + " |")
    P("")


def mean_rng_fmt(vals, fm):
    if not vals:
        return "pending"
    s = fm(np.mean(vals))
    if len(vals) > 1:
        s += f" [{fm(min(vals))}–{fm(max(vals))}]"
    return s


PUF_SUM = [("Mean net worth, top 1% of AGI", "Top 1%|mean", money, "level"),
           ("Mean net worth, top 0.1% of AGI", "Top 0.1%|mean", money, "level"),
           ("Median net worth, top 0.1% of AGI", "Top 0.1%|median", money, "level"),
           ("Mean net worth, 99th–99.9th percentile of AGI", "99-99.9%|mean", money, "level"),
           ("Median net worth, all returns", "median", money, "level"),
           ("Own a home, all returns (%)", "hold_primary_home", lambda x: f"{100 * x:.1f}", "level"),
           ("Hold a business, top 0.1% of AGI (%)", "Top 0.1%|hold_pass_throughs", lambda x: f"{100 * x:.1f}", "level")]


def puf_summary_rows(truth, by, tm, trm):
    rows = []
    s, sd = truth["scf"], truth["scf_boot_sd"]
    for lab, k, fm, _ in PUF_SUM:
        tv = [r["stats"][k] for r in by.get(tm, [])]
        rv = [r["stats"][k] for r in by.get(trm, [])] if trm else []
        if tv and rv:
            a, b = abs(np.mean(tv) - s[k]), abs(np.mean(rv) - s[k])
            if a <= 2 * sd[k] and b <= 2 * sd[k]:
                bt = "neither (both within 2 sampling errors of the SCF)"
            elif abs(a - b) <= sd[k]:
                bt = "about equal (closeness differs by less than 1 sampling error)"
            else:
                bt = "transformer" if a < b else "trees"
        else:
            bt = "pending"
        rows.append((f"PUF records: {lab}", fm(s[k]), mean_rng_fmt(tv, fm), mean_rng_fmt(rv, fm) if trm else "pending",
                     f"{fm(sd[k])} (the SCF's)", bt))
    # average miss over the six AGI groups' means
    def miss(r):
        return float(np.mean([100 * abs(r["stats"][f"{g}|mean"] / s[f"{g}|mean"] - 1) for g in INC]))
    tv = [miss(r) for r in by.get(tm, [])]
    rv = [miss(r) for r in by.get(trm, [])] if trm else []
    rows.append(("PUF records: mean net worth by AGI group (5 groups), average miss from the SCF (%)", "0",
                 mean_rng(tv, "{:.1f}"), mean_rng(rv, "{:.1f}") if trm else "pending", "—",
                 ("transformer" if np.mean(tv) < np.mean(rv) else "trees") if (tv and rv) else "pending"))
    return rows


# ------------------------------------------------------------------------------------------------ V
def v_rows(vdir):
    real, by, models = VC.load(vdir)
    if real is None:
        return [], None, {}
    val = by.get("PUF validation", [None])[0]
    tm = next((m for m in models if m.lower().startswith("transformer")), None)
    trm = ("Trees joint_leaf50" if "Trees joint_leaf50" in models else
           next((m for m in models if m.lower().startswith("trees")), None))

    def top1(r, side):
        v = r["variables"][side]["E00100"]
        return 100 * (v["99-99.9"]["total_bn"] + v["top 0.1"]["total_bn"]) / v["all"]["total_bn"]

    defs = [
        ("Generated records: total AGI ($bn)", lambda r: r["variables"]["real"]["E00100"]["all"]["total_bn"],
         lambda r: r["variables"]["gen"]["E00100"]["all"]["total_bn"], "{:,.0f}", "level"),
        ("Generated records: share of AGI above the real 99th percentile (%)", lambda r: top1(r, "real"), lambda r: top1(r, "gen"), "{:.1f}", "level"),
        ("Generated records: net income tax, all returns ($bn; probe V's simplified 2015 tax)",
         lambda r: r["tax"]["real"]["all"]["net_tax_bn"], lambda r: r["tax"]["gen"]["all"]["net_tax_bn"], "{:,.0f}", "level"),
        ("Generated records: net income tax, top 0.1% group ($bn)", lambda r: r["tax"]["real"]["top 0.1"]["net_tax_bn"],
         lambda r: r["tax"]["gen"]["top 0.1"]["net_tax_bn"], "{:,.0f}", "level"),
        ("Generated records: largest gap in average tax rate by AGI group (points)", lambda r: 0.0, VC.rate_gap, "{:.2f}", "gap"),
        ("Generated records: sum over 30 amounts × 5 AGI groups of the dollar miss ($bn)", lambda r: 0.0,
         VC.var_dollar_miss, "{:,.0f}", "gap"),
        ("Generated records: share with a nonzero amount, average gap (points)", lambda r: 0.0, VC.var_nonzero_gap, "{:.2f}", "gap"),
        ("Generated records: co-holding of 28 pairs of items, average gap (points)", lambda r: 0.0, VC.cohold_gap, "{:.2f}", "gap"),
        ("Generated records: returns breaking a rule real records never break (%)", lambda r: 0.0,
         lambda r: 100 * r["rules"]["gen"]["any_share"], "{:.2f}", "gap"),
        ("Generated records: classifier telling generated from real, area under the curve (0.5 = cannot tell)",
         lambda r: 0.5, lambda r: r["classifier"]["auc_overall"], "{:.3f}", "gap"),
        ("Generated records: the same within the top 0.1% group", lambda r: 0.5,
         lambda r: r["classifier"]["auc_within_group_models"]["top 0.1"]["auc"], "{:.3f}", "gap"),
        ("Generated records: identical to a training record on all 37 variables (%)", lambda r: None,
         lambda r: 100 * r["copying"]["share_distance_zero"], "{:.2f}", "copy"),
    ]
    rows = []
    for lab, fr, fg, f, kind in defs:
        rv = fr(real)

        def vals(m):
            out = []
            for r in by.get(m, []):
                try:
                    out.append(fg(r))
                except Exception:
                    pass
            return out
        tv, trv = vals(tm) if tm else [], vals(trm) if trm else []
        try:
            vv = fg(val) if val else None
        except Exception:
            vv = None
        if kind == "level" and vv is not None:
            se = f"{f.format(abs(vv - rv))} (validation file's miss)"
        elif vv is not None:
            se = f"{f.format(vv)} (validation file)"
        else:
            se = "—"
        if tv and trv:
            if kind == "copy":
                # unseen real records are themselves at distance zero this often (simple bottom-half returns)
                a, b = abs(np.mean(tv) - vv), abs(np.mean(trv) - vv)
                bt = ("neither (both within 3 points of unseen real records)" if max(a, b) <= 3 else
                      "transformer" if a < b else "trees")
            else:
                a, b = abs(np.mean(tv) - rv), abs(np.mean(trv) - rv)
                if kind == "level":
                    # noise: the validation file's miss, or half the spread over runs, whichever is larger
                    noise = max(abs(vv - rv) if vv is not None else 0, (max(tv) - min(tv)) / 2, (max(trv) - min(trv)) / 2)
                else:
                    noise = abs(vv - rv) if vv is not None else 0
                if a <= noise and b <= noise:
                    bt = "neither (both within noise)"
                elif abs(a - b) <= noise:
                    bt = "about equal (difference within noise)"
                else:
                    bt = "transformer" if a < b else "trees"
        else:
            bt = "pending"
        rows.append((lab, f.format(rv) if rv is not None else "—", mean_rng(tv, f), mean_rng(trv, f) if trv else "pending",
                     se, bt))
    return rows, (tm, trm), by


def main():
    d, outp = sys.argv[1], sys.argv[2]
    L = []
    P = L.append
    P("# Probe EV tables (written by ev_collect.py)\n")
    st, sby = load_dir(f"{d}/scf")
    pt, pby = load_dir(f"{d}/puf")
    trs = main_trees(sby, prefer=("split", ""))
    trp = main_trees(pby, prefer=("zero", "split", ""))
    vrows, vm, vby = v_rows(f"{d}/v")
    P("Files scored:\n")
    for nm, by in (("SCF test records", sby), ("PUF records", pby)):
        for m, rs in by.items():
            P(f"- {nm}, {m}: " + "; ".join(f"{r['label']} ({os.path.basename(r['file'])}, {r['draws']} draws)" for r in rs))
    for m, rs in vby.items():
        P(f"- Generated records, {m}: " + "; ".join(os.path.basename(str(r.get('file'))) for r in rs))
    P("")
    P("## Summary\n")
    P(f"Trees column: SCF test records `{trs}`, PUF records `{trp}`, generated records `{vm[1] if vm else None}`. "
      "Transformer: probe C's zero-coded model (`model_cz`), three training seeds. Means over runs, range in brackets.\n")
    P("| Check | Truth | Transformer | Trees | Sampling error | Better |")
    P("|---|---|---|---|---|---|")
    if st:
        for row in scf_summary_rows(st, sby, "transformer", trs):
            P("| SCF test records: " + " | ".join(row) + " |")
        hid_t = "transformer, bridge hidden"
        hid_r = ("trees split_leaf50, bridge hidden" if "trees split_leaf50, bridge hidden" in sby else
                 next((m for m in sby if m.startswith("trees") and "hidden" in m), None))
        if hid_t in sby:
            for row in scf_summary_rows(st, sby, hid_t, hid_r)[:2]:
                P("| SCF test records, mortgage interest and real estate tax hidden: " + " | ".join(row) + " |")
        # levels at the top, SCF test records
        tl = st["truth"]
        sd = st.get("truth_boot_sd", {})
        for gi, lab in ((3, "99th-99.9th"), (4, "top 0.1%")):
            for key, nm in (("inc_nw_mean", "mean"), ("inc_nw_median", "median")):
                tv = [r["levels"][key][gi] for r in sby.get("transformer", [])]
                rv = [r["levels"][key][gi] for r in sby.get(trs, [])] if trs else []
                se = sd[key][gi] if key in sd else None
                if tv and rv:
                    a, b = abs(np.mean(tv) - tl[key][gi]), abs(np.mean(rv) - tl[key][gi])
                    bt = ("neither (both within 2 sampling errors of the truth)" if max(a, b) <= 2 * se else
                          "about equal (closeness differs by less than 1 sampling error)" if abs(a - b) <= se else
                          "transformer" if a < b else "trees")
                else:
                    bt = "pending"
                P(f"| SCF test records: {nm} net worth, {lab} of income | {money(tl[key][gi])} | "
                  f"{mean_rng_fmt(tv, money)} | {mean_rng_fmt(rv, money) if trs else 'pending'} | "
                  f"{money(se)} (the truth's) | {bt} |")
    if pt:
        for row in puf_summary_rows(pt, pby, "transformer", trp):
            P("| " + " | ".join(row) + " |")
        tu = "transformer, deductions unknown (probe C)"
        trsplit = main_trees(pby, prefer=("split",))
        if tu in pby or trsplit:
            P("| *PUF records, with non-itemizers' zero mortgage interest and real estate tax not read as 'none paid':* "
              f"transformer `{tu}` against trees `{trsplit}` | | | | | |")
            for row in puf_summary_rows(pt, pby, tu, trsplit):
                P("| " + " | ".join(row) + " |")
    for row in vrows:
        P("| " + " | ".join(row) + " |")
    P("")
    if st:
        P("## SCF test records: levels by group\n")
        P(f"{st['n_records']:,} SCF test records with wealth observed ({st['n_households']:,} household-wave pairs; "
          "the five implicates of a household count as one household in the bootstrap). Truth's sampling error: "
          "standard deviation over 100 bootstrap resamples of households.\n")
        for fam, labels, nm in (("inc", INC, "Income group (seven-income total within the wave)"),
                                ("age", AGE, "Age of the head"), ("mar", MAR, "Filing status")):
            P(f"### {nm}\n")
            scf_level_table(P, st, sby, fam, labels)
        P("## SCF test records: relationships no target covers\n")
        scf_untargeted_levels(P, st, sby)
        P("Error summaries for every method (mean over runs):\n")
        ms = list(sby)
        P("| Check | " + " | ".join(ms) + " | Random other household |")
        P("|---" * (2 + len(ms)) + "|")
        for k, lab, f in SCF_ROWS:
            bm = st.get("baseline_marginal", {})
            bv = bm.get(k, bm.get("compare", {}).get(k))
            P(f"| {lab} | " + " | ".join(mean_rng([scf_metric(r, k) for r in sby[m]], f) for m in ms) +
              f" | {f.format(bv) if bv is not None else '—'} |")
        P("")
    if pt:
        P("## PUF records: completed wealth by AGI group against the SCF interpolated to 2015\n")
        puf_table(P, pt, pby)
    if os.path.exists(f"{d}/v_tables.md"):
        P("## Generated records: probe V's full scorecard\n")
        P(open(f"{d}/v_tables.md").read().replace("# Probe V scorecard tables", "### Probe V scorecard tables"))
    open(outp, "w").write("\n".join(L) + "\n")
    print("wrote", outp)


if __name__ == "__main__":
    main()
