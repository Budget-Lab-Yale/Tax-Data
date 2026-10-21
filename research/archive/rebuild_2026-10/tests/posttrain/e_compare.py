"""Piece E of test 3: set every evaluation side by side.

  python e_compare.py [--update-report]

Reads $P/e/<label>/stats.json (written by e_evaluate.py) and writes
$P/e/compare.md. With --update-report it also replaces the generated block
of EVAL_REPORT.md in this folder (aggregate tables only).

Columns, left to right: the truth (the training records' own wealth), the
reference model's draws unweighted, method 1 (the same draws reweighted),
method 2 before and after residual calibration, each method 3 variant before
and after. Gaps are taken against the truth. The Monte Carlo yardstick
compares reference draws with each other: differences smaller than it are
draw noise.
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

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(1, os.path.join(HERE, "..", "scf_joint"))
from c_common import WEALTH  # noqa: E402

P = os.environ.get("POSTTRAIN_P", "/nfs/roberts/scratch/pi_nrs36/jar335/posttrain")
OUT = os.environ.get("E_OUT", os.path.join(P, "e"))
REPORT = os.path.join(HERE, "EVAL_REPORT.md")
MA, MB, MANY_WORDS = "half_a", "half_b", "two halves of one set of reference draws"
BEGIN, END = "<!-- BEGIN GENERATED (e_compare.py) -->", "<!-- END GENERATED -->"

MIN_CELL = 30      # fewest true records in an income-by-age cell (holders, for ranks)
MIN_CORR = 200     # fewest true joint holders for a rank correlation


def colname(label):
    m = re.match(r"scf_(train|test)_seed0?(\d+)$", label)
    if m:
        return f"Full model, seed {m.group(2)}"
    fixed = {"truth_train": "Truth", "truth_test": "Truth","truth": "Truth", "truth_decoded": "Truth, tokens decoded", "ref": "Reference",
             "dev_ref": "Reference (dev, K=4)", "m1": "M1 reweight", "dev_m1": "M1 (dev)",
             "m2": "M2 tilt, before", "m2_cal": "M2 tilt, after"}
    if label in fixed:
        return fixed[label]
    m = re.match(r"m3_(.+?)(_cal)?$", label)
    if m:
        return f"M3 {m.group(1)}, {'after' if m.group(2) else 'before'}"
    return label


def order_key(label):
    pri = ["truth", "truth_decoded", "dev_ref", "ref", "dev_m1", "m1", "m2", "m2_cal"]
    if label in pri:
        return (pri.index(label), "")
    if label.startswith("m3_"):
        return (len(pri), label.replace("_cal", "~"))
    return (len(pri) + 1, label)


def load():
    S = {}
    for f in sorted(glob.glob(os.path.join(OUT, "*", "stats.json"))):
        s = json.load(open(f))
        s["_dir"] = os.path.dirname(f)
        S[s["label"]] = s
    return S


# --------------------------------------------------------------------------
# Gaps between two sets of statistics (B against A)
# --------------------------------------------------------------------------

def gaps(A, B):
    """A, B: the 'stats' dicts of e_evaluate.frame_stats. Returns a dict of
    summary gaps (B minus A, in absolute value, averaged)."""
    g = {}
    # income by age: holding share and median amount rank
    ha, hb = A["inc_age"]["hold"], B["inc_age"]["hold"]
    rows = A["inc_age"]["rows"]
    d = [abs(x - y) for k in ha if rows[k] >= MIN_CELL and k in hb for x, y in zip(ha[k], hb[k])]
    g["hold_ia_mae_pp"] = 100 * float(np.mean(d))
    g["hold_ia_max_pp"] = 100 * float(np.max(d))
    ra, rb, na = A["inc_age"]["median_rank"], B["inc_age"]["median_rank"], A["inc_age"]["median_rank_rows"]
    d = [abs(x - y) for k in ra if k in rb for x, y, n in zip(ra[k], rb[k], na[k])
         if n >= MIN_CELL and x is not None and y is not None]
    g["rank_ia_mae_pctile"] = 100 * float(np.mean(d))
    # joint holding
    pa, pb = np.array(A["p1"]), np.array(B["p1"])
    Ma, Mb = np.array(A["M11"]), np.array(B["M11"])
    tv = []
    for i in range(len(WEALTH)):
        for j in range(i + 1, len(WEALTH)):
            ca = np.array([Ma[i, j], pa[i] - Ma[i, j], pa[j] - Ma[i, j], 1 - pa[i] - pa[j] + Ma[i, j]])
            cb = np.array([Mb[i, j], pb[i] - Mb[i, j], pb[j] - Mb[i, j], 1 - pb[i] - pb[j] + Mb[i, j]])
            tv.append(0.5 * np.abs(ca - cb).sum())
    g["pair2x2_tvd_mean_pct"] = 100 * float(np.mean(tv))
    g["pair2x2_tvd_max_pct"] = 100 * float(np.max(tv))
    if "patterns" in A and "patterns" in B:
        u = set(A["patterns"]) | set(B["patterns"])
        g["pattern_tvd_pct"] = 100 * 0.5 * sum(abs(A["patterns"].get(k, 0) - B["patterns"].get(k, 0)) for k in u)
    ca = {(c["holds"], c["given"]): c["rate"] for c in A["cond"]}
    cb = {(c["holds"], c["given"]): c["rate"] for c in B["cond"]}
    g["cond_mae_pp"] = 100 * float(np.mean([abs(cb[k] - v) for k, v in ca.items()
                                            if v is not None and cb.get(k) is not None]))
    # correlations
    ia, ib, n = A["income_corr"], B["income_corr"], A["income_corr_rows"]
    g["income_corr_mae"] = float(np.mean([abs(ib[v] - ia[v]) for v in ia
                                          if n[v] >= MIN_CORR and ia[v] is not None and ib.get(v) is not None]))
    pa_, pb_, pn = A["pair_corr"], B["pair_corr"], A["pair_corr_rows"]
    d = [abs(pb_[k] - pa_[k]) for k in pa_ if pn[k] >= MIN_CORR and pa_[k] is not None and pb_.get(k) is not None]
    g["pair_corr_mae"] = float(np.mean(d))
    g["pair_corr_n"] = len(d)
    # net worth
    ga, gb = A["nw_inc"], B["nw_inc"]
    g["nw_inc_mean_mape"] = 100 * float(np.mean([abs(y["mean"] / x["mean"] - 1) for x, y in zip(ga, gb)]))
    g["nw_inc_median_mape"] = 100 * float(np.mean([abs(y["median"] / x["median"] - 1) for x, y in zip(ga, gb)
                                                    if x["median"] != 0]))
    g["nw_inc_share_sum_abs_pp"] = 100 * float(sum(abs(y["nw_share"] - x["nw_share"]) for x, y in zip(ga, gb)))
    for t in ("top10%", "top1%", "top0.1%"):
        g[f"nw_{t}_gap_pp"] = 100 * (B["nw_top"][t] - A["nw_top"][t])
    g["nw_mean_ratio"] = B["nw_mean"] / A["nw_mean"]
    g["nw_median_ratio"] = B["nw_q"]["p50"] / A["nw_q"]["p50"]
    return g


GAP_ROWS = [
    ("hold_ia_mae_pp", "Share holding each component, by income group x age (30 cells x 24): average gap, points", 2),
    ("rank_ia_mae_pctile", "Median amount rank among holders, by income group x age: average gap, percentiles", 2),
    ("pair2x2_tvd_mean_pct", "Pairs of components (276 two-by-two holding tables): average share misplaced, %", 2),
    ("pair2x2_tvd_max_pct", "Same, worst pair, %", 2),
    ("pattern_tvd_pct", "Pattern of holding across all 24 components: share of weight in the wrong pattern, %", 1),
    ("cond_mae_pp", "Twelve conditional holding rates (listed below): average gap, points", 2),
    ("income_corr_mae", "Rank correlation of each amount with total income, among holders: average gap", 3),
    ("pair_corr_mae", "Rank correlation between two amounts, among joint holders: average gap", 3),
    ("nw_inc_mean_mape", "Mean net worth by income group (5 groups): average gap, %", 1),
    ("nw_inc_median_mape", "Median net worth by income group: average gap, %", 1),
    ("nw_inc_share_sum_abs_pp", "Share of all net worth held by each income group: sum of absolute gaps, points", 1),
    ("nw_top1%_gap_pp", "Top 1% share of net worth (by net worth): gap, points", 2),
    ("nw_top0.1%_gap_pp", "Top 0.1% share of net worth: gap, points", 2),
]


# --------------------------------------------------------------------------
# Formatting
# --------------------------------------------------------------------------

def fmt(x, dec=2):
    if x is None or (isinstance(x, float) and not np.isfinite(x)):
        return "–"
    return f"{x:,.{dec}f}"


def p1(x):
    return "–" if x is None else f"{100 * x:.1f}"


def pdiff(a, b):
    return "–" if a is None or b is None else f"{100 * abs(a - b):.1f}"


def money(x):
    if x is None:
        return "–"
    a = abs(x)
    if a >= 1e6:
        return f"${x / 1e6:,.2f}m"
    return f"${x / 1e3:,.0f}k"


def table(header, rows):
    out = ["| " + " | ".join(header) + " |", "|" + "|".join(["---"] + ["---:"] * (len(header) - 1)) + "|"]
    for r in rows:
        out.append("| " + " | ".join(str(x) for x in r) + " |")
    return "\n".join(out)


# --------------------------------------------------------------------------
# Sections
# --------------------------------------------------------------------------

def yard_sets(S):
    y = S.get("yardstick")
    if not y:
        return None, None
    s = y["sets"]
    global MA, MB, MANY_WORDS
    if "all_b" in s:
        MA, MB = "all_a", "all_b"
        MANY_WORDS = f"two independent sets of {s['all_a']['n_draws']} reference draws"
    else:
        MA, MB = "half_a", "half_b"
        MANY_WORDS = "two halves of one set of reference draws"
    one = gaps(s["draw0"]["stats"], s["draw1"]["stats"]) if "draw1" in s else None
    half = gaps(s[MA]["stats"], s[MB]["stats"]) if MB in s else None
    return one, half


def cell_group_totals():
    t = pd.read_parquet(os.path.join(P, "a", "targets.parquet"))
    wcol = "wave" if "wave" in t.columns else "year"
    tcol = next(c for c in ("target", "target_weight", "value") if c in t.columns)
    gt = t.groupby([wcol, "family", "group", "variable"])[tcol].transform("sum").to_numpy()
    return t, gt, tcol


def sec_move(S, methods, title="### 1. How far each method moved from the reference model",
             bglob=None, name=None):
    name = name or colname
    bglob = bglob or [os.path.join(P, "b", "report_ref*.json")]
    lines = [title,
             "",
             "KL is the Kullback–Leibler divergence of the final distribution of wealth from the "
             "reference model's, per unit of population weight, in nats; zero means no move. "
             "For intuition, a KL of log(2) = 0.69 is what one gets by putting each record's weight "
             "evenly on half of its draws and none on the rest; the column \"as if spread over\" "
             "turns each KL into that share (e^−KL). Effective draws per record is 1 / Σ q², where "
             "q are the shares of a record's weight on its draws (K when all draws count equally). "
             "For methods 2 and 3 the KL has two parts: the changed model (estimated as the average "
             "over draws of the log-probability under the new model minus that under the reference) "
             "and the residual calibration's weights on the new model's draws.",
             ""]
    hdr = ["Method", "K", "KL, changed model", "KL, weights", "KL, total", "As if spread over",
           "Effective draws", "Records with < 4 effective draws", "Largest share on one draw"]
    rows = []
    for lab in methods:
        m = S[lab].get("move")
        if not m:
            continue
        rows.append([name(lab), m["K"], fmt(m.get("kl_model"), 3) if "kl_model" in m else "–",
                     fmt(m["kl_weights"], 3), fmt(m["kl_total"], 3),
                     f"{100 * np.exp(-m['kl_total']):.0f}%",
                     fmt(m["eff_draws_mean"], 1), f"{100 * m['eff_draws_lt4_share']:.1f}%",
                     f"{100 * m['max_draw_weight_over_record']:.0f}%"])
    lines.append(table(hdr, rows))
    # piece B's sensitivity to K, from its own reports
    sens = []
    bad_st = ["unreachable", "above_reach", "below_forced", "joint", "zero_relaxed", "block", "missed"]
    for f in sorted(sum((glob.glob(g, recursive=True) for g in bglob), []),
                    key=lambda f: int((re.search(r"_k(\d+)", f) or [0, 32])[1])):
        try:
            r = json.load(open(f))
        except (json.JSONDecodeError, OSError):
            continue
        sc = r.get("status_counts", {})
        tw = r.get("target_weight_by_status", {})
        inf = sum(sc.get(k, 0) for k in bad_st)
        infw = sum(tw.get(k, 0) for k in bad_st) / max(sum(tw.values()), 1e-300) if tw else None
        sens.append([os.path.relpath(f, os.path.join(P, "b")), r.get("K"), fmt(r.get("kl_nats_per_unit_weight"), 3),
                     f"{100 * np.exp(-r['kl_nats_per_unit_weight']):.0f}%" if r.get("kl_nats_per_unit_weight") is not None else "–",
                     fmt(r.get("effective_draws_weighted_mean"), 1),
                     f"{100 * r.get('share_records_below4_effective_draws_weighted', np.nan):.1f}%",
                     inf, f"{100 * infw:.3f}%" if infw is not None else "–",
                     f"{r['feasible_max_rel_error']:.1e}" if r.get("feasible_max_rel_error") is not None else "–"])
    if sens:
        lines += ["", "Method 1 with different numbers of reference draws per record (piece B's reports; "
                  "\"infeasible\" cells are those B's solver set aside because no weighting of the "
                  "available draws can hit them, with their share of all target weight):", "",
                  table(["Run", "K", "KL", "As if spread over", "Effective draws", "Records with < 4 effective",
                         "Infeasible cells", "Their target weight", "Largest relative error, feasible cells"], sens)]
    inf_rows = [(name(lab), S[lab]["calibration_cells"]) for lab in methods if S[lab].get("calibration_cells")]
    if inf_rows:
        lines += ["", "Infeasible cells in the calibrations evaluated here: " + "; ".join(
            f"{nm}: {c['infeasible']} cells ({100 * c['infeasible_target_weight_share']:.3f}% of target weight"
            + (", " + ", ".join(f"{k} {v}" for k, v in c["infeasible_by_family"].items()) if c["infeasible_by_family"] else "")
            + ")" for nm, c in inf_rows) + "."]
    # the pieces' own reports, for cross-checking
    lines += ["", "_Cross-check: KL as computed here from the weights and log-probabilities in each file; "
              "the pieces' own reports should agree._"]
    return "\n".join(lines)


def sec_targets(S, methods, yard):
    lines = ["### 2. Target fit", "",
             "Error in each target cell's weight as a share of its group's total weight (the group "
             "total is the weight of the records in that wave and group), averaged over the cells "
             "of each family, with the worst cell in brackets. A target cell is a (wave, family, "
             "group, wealth component, coarse bin) total; families are defined in `SPEC.md`. "
             "\"Unreached\" counts cells with a positive true total that no draw with positive "
             "weight falls in (infeasible for that set of draws).",
             ""]
    cols = [lab for lab in methods if S[lab].get("target_fit")]
    if not cols:
        return "\n".join(lines + ["(No target fit yet: piece A's targets or contribution matrix not available.)"])
    fams = [r["family"] for r in S[cols[0]]["target_fit"]["by_family"]]
    hdr = ["Family"] + [colname(c) for c in cols]
    noise = None
    if yard is not None and os.path.exists(os.path.join(yard["_dir"], f"cell_fit_{MA}.npy")):
        try:
            t, gt, _ = cell_group_totals()
            fa = np.load(os.path.join(yard["_dir"], f"cell_fit_{MA}.npy"))
            fb = np.load(os.path.join(yard["_dir"], f"cell_fit_{MB}.npy"))
            d = pd.DataFrame({"family": t["family"].to_numpy(), "e": np.abs(fa - fb) / gt})
            noise = d.groupby("family")["e"].agg(["mean", "max"])
            na = {"mean": d["e"].mean(), "max": d["e"].max()}
            hdr.append("Draw noise")
        except Exception as e:                   # noqa: BLE001
            print(f"no target-fit yardstick: {e}")
    rows = []
    for f in fams + ["all"]:
        r = [f]
        for c in cols:
            tf = S[c]["target_fit"]
            x = tf["all"] if f == "all" else next(y for y in tf["by_family"] if y["family"] == f)
            r.append(f"{x['mean_err_pct']:.3f}% ({x['max_err_pct']:.1f}%)")
        if noise is not None:
            x = na if f == "all" else noise.loc[f]
            r.append(f"{100 * x['mean']:.3f}% ({100 * x['max']:.1f}%)")
        rows.append(r)
    rows.append(["Weight in cells whose true total is zero (% of all)"] +
                 [f"{S[c]['target_fit']['all']['weight_in_zero_cells_pct']:.3f}%" for c in cols] +
                 (["–"] if noise is not None else []))
    rows.append(["Unreached cells (positive truth, no weighted draw)"] +
                 [str(S[c]["target_fit"]["unreached_positive_cells"]) for c in cols] +
                 (["–"] if noise is not None else []))
    rows.append(["Share of all weight misplaced across cells, %"] +
                 [f"{S[c]['target_fit']['all']['misplaced_pct']:.2f}" for c in cols] +
                 (["–"] if noise is not None else []))
    lines.append(table(hdr, rows))
    unr = {colname(c): S[c]["target_fit"]["unreached_by_family"] for c in cols
           if S[c]["target_fit"]["unreached_positive_cells"]}
    if unr:
        lines += ["", "Unreached cells by family: " +
                  "; ".join(f"{k}: " + ", ".join(f"{f} {n}" for f, n in v.items()) for k, v in unr.items()) + "."]
    return "\n".join(lines)


def sec_levels(S, cols, one, half):
    T = S["truth"]["stats"]
    lines = ["### 3. Untargeted relationships", "",
             "#### Net worth by income group, in levels", "",
             "Income groups rank the seven-income total within each wave over all SCF training units. "
             "Net worth is in dollars of the wave (2013 and 2016 pooled at their population weights). "
             "\"Truth, tokens decoded\" puts the true records' own wealth tokens through the same "
             "decoder as the draws: the difference from the truth is decoding noise, not model error. "
             f"The draw-noise column is the absolute difference between {MANY_WORDS} (section 4), "
             "the size of draw-to-draw noise for a column built from many draws per record.",
             ""]

    def noise(stat, k, key):
        if half is None:
            return []
        y = S["yardstick"]["sets"]
        return [abs(y[MA]["stats"][stat][k][key] - y[MB]["stats"][stat][k][key])]

    hdr = ["Income group"] + [colname(c) for c in cols] + (["Draw noise"] if half else [])
    for key, title, f in [("mean", "Mean net worth", money), ("median", "Median net worth", money),
                          ("nw_share", "Share of all net worth (%)", lambda x: f"{100 * x:.1f}")]:
        rows = []
        for k, g in enumerate(T["nw_inc"]):
            rows.append([g["group"]] + [f(S[c]["stats"]["nw_inc"][k][key]) for c in cols] +
                        [f(v) for v in noise("nw_inc", k, key)])
        lines += [f"**{title}**", "", table(hdr, rows), ""]
    # overall
    rows = []
    for nm, get, f in [("Mean net worth", lambda s: s["nw_mean"], money),
                       ("Median net worth", lambda s: s["nw_q"]["p50"], money),
                       ("90th percentile", lambda s: s["nw_q"]["p90"], money),
                       ("99th percentile", lambda s: s["nw_q"]["p99"], money),
                       ("Top 10% share (%)", lambda s: 100 * s["nw_top"]["top10%"], lambda x: f"{x:.1f}"),
                       ("Top 1% share (%)", lambda s: 100 * s["nw_top"]["top1%"], lambda x: f"{x:.1f}"),
                       ("Top 0.1% share (%)", lambda s: 100 * s["nw_top"]["top0.1%"], lambda x: f"{x:.1f}"),
                       ("Share with negative net worth (%)", lambda s: 100 * s["share_negative_nw"],
                        lambda x: f"{x:.1f}")]:
        r = [nm] + [f(get(S[c]["stats"])) for c in cols]
        if half:
            y = S["yardstick"]["sets"]
            r.append(f(abs(get(y[MA]["stats"]) - get(y[MB]["stats"]))))
        rows.append(r)
    lines += ["**All records** (top shares rank records by net worth)", "",
              table(["Statistic"] + hdr[1:], rows), ""]
    # by age
    rows = []
    for k, g in enumerate(T["nw_age"]):
        rows.append([g["group"]] + [money(S[c]["stats"]["nw_age"][k]["mean"]) for c in cols] +
                    ([money(abs(S["yardstick"]["sets"][MA]["stats"]["nw_age"][k]["mean"] -
                                S["yardstick"]["sets"][MB]["stats"]["nw_age"][k]["mean"]))] if half else []))
    lines += ["**Mean net worth by age of head**", "", table(["Age"] + hdr[1:], rows), ""]

    # conditional holding rates
    lines += ["#### Holding one component given another", "",
              "Share holding a nonzero amount of the first, among those holding the second "
              "(or with that income nonzero).", ""]
    rows = []
    for i, c0 in enumerate(T["cond"]):
        r = [c0["words"]] + [p1(S[c]['stats']['cond'][i]['rate']) for c in cols]
        if half:
            y = S["yardstick"]["sets"]
            r.append(pdiff(y[MA]['stats']['cond'][i]['rate'], y[MB]['stats']['cond'][i]['rate']))
        rows.append(r)
    lines += [table(["Rate (%)"] + hdr[1:], rows), ""]

    # correlations with income, selected
    lines += ["#### Rank correlation of amounts with total income, among positive holders", "",
              "Weighted correlation of the amount's rank band with the record's income rank.", ""]
    rows = []
    for v in ["cash", "equities", "dc", "pass_throughs", "primary_home", "primary_mortgage", "other_home",
              "kg_other", "bonds", "credit_cards"]:
        r = [v] + [fmt(S[c]["stats"]["income_corr"].get(v), 3) for c in cols]
        if half:
            y = S["yardstick"]["sets"]
            a_, b_ = y[MA]["stats"]["income_corr"].get(v), y[MB]["stats"]["income_corr"].get(v)
            r.append(fmt(abs(a_ - b_), 3) if a_ is not None and b_ is not None else "–")
        rows.append(r)
    lines += [table(["Component"] + hdr[1:], rows), ""]
    return "\n".join(lines)


def sec_gaps(S, cols, one, half):
    T = S["truth"]["stats"]
    mcols = [c for c in cols if c != "truth"]
    G = {c: gaps(T, S[c]["stats"]) for c in mcols}
    hdr = ["Gap against the truth"] + [colname(c) for c in mcols]
    if one:
        hdr.append("Noise: one ref draw vs another")
    if half:
        hdr.append("Noise: many draws vs many")
    rows = []
    for key, words, dec in GAP_ROWS:
        r = [words] + [fmt(abs(G[c].get(key, np.nan)) if "_gap_pp" in key else G[c].get(key), dec) for c in mcols]
        for y in (one, half):
            if y:
                r.append(fmt(abs(y.get(key, np.nan)), dec))
        rows.append(r)
    r = ["Classifier telling completed from true records: AUC (0.5 = cannot tell)"]
    for c in mcols:
        r.append(fmt(S[c].get("classifier", {}).get("auc"), 3) if c != "truth_decoded" else "–")
    y = S.get("yardstick", {}).get("sets", {})
    if one:
        r.append(fmt(S.get("yardstick", {}).get("classifier_draw0_vs_draw1", {}).get("auc"), 3))
    if half:
        r.append("–")
    rows.append(r)
    lines = ["#### Summary of untargeted gaps", "",
             "Each row is an average absolute gap against the truth over many cells (definitions as "
             "in test 2, with five income groups). The two noise columns compare reference draws "
             "with each other: one draw against another (the noise of a file with one draw per "
             f"record) and {MANY_WORDS} against each other (the noise of a many-draw column). "
             "A reweighted column with few effective draws per record sits "
             "between the two. Gaps no larger than the noise are not evidence of a difference.",
             "", table(hdr, rows)]
    return "\n".join(lines), G


def sec_yardstick(S, one, half):
    y = S.get("yardstick")
    lines = ["### 4. Monte Carlo yardstick", ""]
    if not y:
        return "\n".join(lines + ["(Not yet run.)"])
    files = f"`{os.path.relpath(y['draws_file'], P)}`"
    if y.get("draws_file_b"):
        files += f" and `{os.path.relpath(y['draws_file_b'], P)}`"
    lines += [f"Reference draws: {files} under the scratch folder. The yardstick puts the reference "
              "model's own draws against each other, so it measures nothing but the randomness of drawing. "
              "Its values appear as the noise columns in sections 2 and 3. Two readings: a statistic of one "
              "completed file (one draw per record) moves by about the one-against-one figure from draw to "
              f"draw; a statistic averaged over many draws per record, compared between {MANY_WORDS}, moves "
              "by about the many-against-many figure. A reweighted column with few effective draws per "
              "record lies between the two. The truth here is fixed (the training records themselves), so "
              "there is no survey-sampling noise in these comparisons; draw noise is the only noise."]
    return "\n".join(lines)


# --------------------------------------------------------------------------
# Arm P: PUF records, against the SCF interpolated to 2015
# --------------------------------------------------------------------------

A13, A16 = 1 / 3, 2 / 3
DOLLAR_KEYS = {"mean", "median", "nw_mean"}


def interp(a, b, key=None, dollars=False):
    """SCF 2015 from 2013 (a) and 2016 (b): linear, dollar levels in logs."""
    if isinstance(a, dict):
        out = {}
        for k in set(a) | set(b):
            if k.endswith("rows") or k == "rows":
                out[k] = {kk: min(a[k].get(kk, 0), b[k].get(kk, 0)) for kk in a[k]} if isinstance(a[k], dict) else a[k]
                continue
            if k == "patterns":
                out[k] = {kk: A13 * a[k].get(kk, 0) + A16 * b[k].get(kk, 0) for kk in set(a[k]) | set(b[k])}
                continue
            if k not in a or k not in b:
                continue
            out[k] = interp(a[k], b[k], k, dollars or k in DOLLAR_KEYS or k == "nw_q")
        return out
    if isinstance(a, list):
        return [interp(x, y, key, dollars) for x, y in zip(a, b)]
    if isinstance(a, str) or a is None or b is None:
        return a
    if isinstance(a, bool):
        return a
    if dollars and a > 0 and b > 0:
        return float(np.exp(A13 * np.log(a) + A16 * np.log(b)))
    return A13 * a + A16 * b


def load_puf():
    S = {}
    for f in sorted(glob.glob(os.path.join(OUT, "puf", "*", "stats.json"))):
        s = json.load(open(f))
        s["_dir"] = os.path.dirname(f)
        S[s["label"]] = s
    return S


def pcolname(label):
    fixed = {"scf2013": "SCF 2013", "scf2016": "SCF 2016", "scf2015": "SCF interpolated 2015",
             "p_ref": "PUF reference"}
    if label in fixed:
        return fixed[label]
    m = re.match(r"puf_wealth_seed0?(\d+)$", label)
    if m:
        return f"Full model, seed {m.group(1)}"
    m = re.match(r"p_m(\d)_(core|full)(?:_(.+?))?(_cal)?$", label)
    if m:
        meth, which, var, cal = m.groups()
        nm = {"1": "M1 reweight", "2": "M2 tilt", "3": "M3 " + (var or "")}[meth]
        tail = "" if meth == "1" else (", after" if cal else ", before")
        return f"{nm} P_{which}{tail}"
    m = re.match(r"p_m(\d)_(.+?)((?:_fallback)?_cal)?$", label)
    if m:
        rest = re.sub(r"_(weights|draws)(_P)?$|_(weights|draws)_", " ", m.group(2)).strip().replace("_", " ")
        nm = {"1": "M1 reweight", "2": "M2 tilt", "3": "M3 tune"}.get(m.group(1), "M" + m.group(1))
        after = ", after (fallback)" if m.group(3) == "_fallback_cal" else ", after"
        return f"{nm} {rest}" + (after if m.group(3) else (", before" if m.group(1) != "1" else ""))
    return label


def porder(label):
    pri = ["scf2013", "scf2016", "scf2015", "p_ref", "p_m1_core", "p_m1_full", "p_m2_core", "p_m2_core_cal",
           "p_m2_full", "p_m2_full_cal"]
    return (pri.index(label), "") if label in pri else (len(pri), label.replace("_cal", "~"))


def sec_puf(SP, title="## Arm P: completing PUF records (no true wealth; compared with the SCF)", sub="P",
            noise_override=None, bglob=None):
    if not ("scf2013" in SP and "scf2016" in SP):
        return f"({title.lstrip('# ')}: SCF statistics not computed yet.)"
    SP = dict(SP)
    SP["scf2015"] = {"label": "scf2015", "stats": interp(SP["scf2013"]["stats"], SP["scf2016"]["stats"]),
                     "note": "one third SCF 2013 plus two thirds SCF 2016 (dollar levels in logs)"}
    X = SP["scf2015"]["stats"]
    labs = sorted([k for k in SP if not k.startswith("p_noise")], key=porder)
    meths = [k for k in labs if not k.startswith("scf")]
    noise = noise_override
    if noise is None and "p_noise_half_a" in SP and "p_noise_half_b" in SP:
        noise = (SP["p_noise_half_a"]["stats"], SP["p_noise_half_b"]["stats"])
    lines = [title, "",
             "PUF training records (2015) are completed with the 24 wealth components, presented to the model "
             "as SCF records of 2015. The PUF has no wealth, so the comparison is with the SCF 2013 and 2016 "
             "training records and their interpolation to 2015 (one third 2013, two thirds 2016; dollar "
             "levels interpolated in logs). Income groups rank the seven-income total among PUF records on the "
             "PUF side and among SCF units of each wave on the SCF side. The SCF's own change from 2013 to 2016 "
             "(which includes its sampling noise) is a scale for how large a gap is. The PUF and SCF differ in "
             "unit, sampling and income measurement, so matching the SCF's aggregates does not validate the "
             "PUF's joint distribution.", ""]
    lines.append("Columns: " + "; ".join(f"**{pcolname(k)}**: {SP[k].get('note', '')}" for k in labs) + ".")
    lines.append("")
    if any(SP[k].get("move") for k in meths):
        lines.append(sec_move(SP, [k for k in meths if SP[k].get("move")],
                              title=f"### {sub}1. How far each method moved from the reference model",
                              bglob=bglob or [os.path.join(P, "b", "puf_*", "report*.json")], name=pcolname))
        lines.append("")
    # target fit
    tf_rows = []
    tsets = []
    for k in meths:
        for wh, x in (SP[k].get("target_fit") or {}).items():
            if wh not in tsets:
                tsets.append(wh)
    for k in meths:
        tf = SP[k].get("target_fit") or {}
        r = [pcolname(k)]
        for wh in tsets:
            x = tf.get(wh)
            r.append(f"{x['all']['mean_err_pct']:.3f}% ({x['all']['max_err_pct']:.1f}%)" if x else "–")
            r.append(str(x["unreached_positive_cells"]) if x else "–")
        tf_rows.append(r)
    if any(SP[k].get("target_fit") for k in meths):
        lines += [f"### {sub}2. Target fit", "",
              "Error in each target cell's weight as a share of its group's PUF weight, averaged over all cells "
              "of the target set, worst cell in brackets; \"unreached\": cells with a positive target that no "
              "weighted draw falls in. Core sets (core, wcore): wealth by income group, age, married, "
              "dependents. Full sets (full, wfull): also wealth by each income item's band.", "",
              table(["Column"] + sum([[f"{wh}: mean (max)", f"{wh} unreached"] for wh in tsets], []),
                    tf_rows), ""]
    # levels
    cols = labs
    hdr = ["Income group"] + [pcolname(c) for c in cols] + (["PUF draw noise"] if noise else [])
    lines += [f"### {sub}3. Untargeted relationships", "", "#### Net worth by income group, in levels", "",
              "Dollars of each year (PUF and interpolated SCF at 2015). \"PUF draw noise\" is the absolute "
              "difference between two halves of the PUF completion's draws.", ""]
    for key, title, f in [("mean", "Mean net worth", money), ("median", "Median net worth", money),
                          ("nw_share", "Share of all net worth (%)", lambda x: f"{100 * x:.1f}")]:
        rows = []
        for i, g in enumerate(X["nw_inc"]):
            r = [g["group"]] + [f(SP[c]["stats"]["nw_inc"][i][key]) for c in cols]
            if noise:
                r.append(f(abs(noise[0]["nw_inc"][i][key] - noise[1]["nw_inc"][i][key])))
            rows.append(r)
        lines += [f"**{title}**", "", table(hdr, rows), ""]
    rows = []
    for nm, get, f in [("Mean net worth", lambda s: s["nw_mean"], money),
                       ("Median net worth", lambda s: s["nw_q"]["p50"], money),
                       ("90th percentile", lambda s: s["nw_q"]["p90"], money),
                       ("99th percentile", lambda s: s["nw_q"]["p99"], money),
                       ("Top 10% share (%)", lambda s: 100 * s["nw_top"]["top10%"], lambda x: f"{x:.1f}"),
                       ("Top 1% share (%)", lambda s: 100 * s["nw_top"]["top1%"], lambda x: f"{x:.1f}"),
                       ("Top 0.1% share (%)", lambda s: 100 * s["nw_top"]["top0.1%"], lambda x: f"{x:.1f}"),
                       ("Share with negative net worth (%)", lambda s: 100 * s["share_negative_nw"],
                        lambda x: f"{x:.1f}")]:
        r = [nm] + [f(get(SP[c]["stats"])) for c in cols]
        if noise:
            r.append(f(abs(get(noise[0]) - get(noise[1]))))
        rows.append(r)
    lines += ["**All records** (top shares rank records by net worth)", "", table(["Statistic"] + hdr[1:], rows), ""]
    agelabs = [g["group"] for g in SP["scf2013"]["stats"]["nw_age"]]
    extra = [g["group"] for c in cols for g in SP[c]["stats"]["nw_age"] if g["group"] not in agelabs]
    rows = []
    for a in agelabs + sorted(set(extra)):
        r = [a]
        for c in cols:
            m = {g["group"]: g for g in SP[c]["stats"]["nw_age"]}
            r.append(money(m[a]["mean"]) if a in m else "–")
        if noise:
            m0 = {g["group"]: g for g in noise[0]["nw_age"]}
            m1 = {g["group"]: g for g in noise[1]["nw_age"]}
            r.append(money(abs(m0[a]["mean"] - m1[a]["mean"])) if a in m0 else "–")
        rows.append(r)
    lines += ["**Mean net worth by age of head** (about 42% of PUF records have no age; they are their own row)",
              "", table(["Age"] + hdr[1:], rows), ""]
    rows = []
    for i, c0 in enumerate(X["cond"]):
        r = [c0["words"]] + [p1(SP[c]['stats']['cond'][i]['rate']) for c in cols]
        if noise:
            r.append(pdiff(noise[0]['cond'][i]['rate'], noise[1]['cond'][i]['rate']))
        rows.append(r)
    lines += ["#### Holding one component given another (%)", "", table(["Rate (%)"] + hdr[1:], rows), ""]
    # gaps against interpolated SCF
    pm = [c for c in cols if not c.startswith("scf")]
    G = {c: gaps(X, SP[c]["stats"]) for c in pm}
    wave = gaps(SP["scf2013"]["stats"], SP["scf2016"]["stats"])
    nz = gaps(noise[0], noise[1]) if noise else None
    hdr2 = ["Gap against SCF interpolated 2015"] + [pcolname(c) for c in pm] + ["Scale: SCF 2013 vs 2016"] + \
        (["PUF draw noise"] if nz else [])
    rows = []
    for key, words, dec in GAP_ROWS:
        if key == "pattern_tvd_pct" and key not in wave:
            continue
        f = (lambda v: fmt(abs(v), dec)) if "_gap_pp" in key else (lambda v: fmt(v, dec))
        r = [words] + [f(G[c].get(key, np.nan)) for c in pm] + [f(wave.get(key, np.nan))]
        if nz:
            r.append(f(nz.get(key, np.nan)))
        rows.append(r)
    lines += ["#### Summary of untargeted gaps", "",
              "Same measures as for the SCF arm, against the interpolated SCF. The SCF 2013-against-2016 column "
              "is the scale from test 2: a gap is small if it is small next to how much the SCF itself moves in "
              "three years.", "", table(hdr2, rows), ""]
    return "\n".join(lines)


def build_pilot(S):
    if "truth" not in S:
        return "(No evaluation yet.)"
    if "ref" in S:   # the development columns are superseded by the full reference draws
        for k in [k for k in S if k.startswith("dev_")]:
            S.pop(k)
    one, half = yard_sets(S)
    labs = sorted([k for k in S if k not in ("yardstick",)], key=order_key)
    methods = [k for k in labs if S[k].get("move")]
    cols = labs
    parts = [f"_Generated by `e_compare.py` at {datetime.now().strftime('%Y-%m-%d %H:%M')} from "
             f"{len(S)} evaluations: {', '.join(colname(k) for k in labs)}._", ""]
    names = []
    for k in labs:
        s = S[k]
        what = s.get("note") or s.get("kind")
        extra = []
        if s.get("weights_note"):
            extra.append(s["weights_note"])
        if s.get("draws_file"):
            extra.append(f"`{os.path.relpath(s['draws_file'], P)}`")
        if s.get("weights_file"):
            extra.append(f"weights `{os.path.relpath(s['weights_file'], P)}`")
        names.append(f"- **{colname(k)}**: {what}" + (f" ({'; '.join(extra)})" if extra else "") + ".")
    SP = load_puf()
    if SP:
        parts += [sec_puf(SP), ""]
    parts += ["## Arm S: completing SCF records (true wealth known)", "",
              "The SCF training records' own wealth is known, so here every statistic is compared with the "
              "truth for the same records.", ""]
    parts += ["Columns:", ""] + names + [""]
    parts.append(sec_move(S, methods))
    parts.append("")
    parts.append(sec_targets(S, methods, S.get("yardstick")))
    parts.append("")
    parts.append(sec_levels(S, cols, one, half))
    g, G = sec_gaps(S, cols, one, half)
    parts.append(g)
    parts.append("")
    parts.append(sec_yardstick(S, one, half))
    # classifier details
    cl = [(colname(k), S[k]["classifier"]) for k in labs if S[k].get("classifier")]
    if cl:
        parts += ["", "Variables the classifier leans on most (drop in AUC when shuffled):", ""]
        for nm, c in cl:
            tv = ", ".join(f"{t['variable']} {t['auc_drop']:.3f}" for t in c.get("top_variables", [])[:4])
            parts.append(f"- {nm}: AUC {c['auc']:.3f}; {tv}")
    json.dump({k: v for k, v in G.items()}, open(os.path.join(OUT, "gaps.json"), "w"), indent=1, default=float)
    return "\n".join(parts)


def load_dir(d):
    S = {}
    for f in sorted(glob.glob(os.path.join(d, "*", "stats.json"))):
        x = json.load(open(f))
        x["_dir"] = os.path.dirname(f)
        S[x["label"]] = x
    return S


def demote(text):
    return "\n".join(("#" + ln) if ln.startswith("#") else ln for ln in text.split("\n"))


def sec_gen_puf(F, version="full"):
    gens = sorted(k for k in F if k.startswith("gen_puf_"))
    if not gens or "puf_train_levels" not in F:
        return ""
    T = F["puf_train_levels"]
    cols = [("PUF training records", T)] + [(f"Generated, seed {int(k[-2:])}", F[k]) for k in gens]
    lines = ["### Whole PUF records generated from nothing", "",
             "The full model generates as many PUF records as there are PUF training records, from nothing but "
             "the PUF tag and the year 2015, each at weight (total PUF weight) / (number generated). Each file is "
             "ranked on its own AGI into the five groups. Levels first, then test 1's distance measures.", ""]
    for key, title, f in [("mean_E00100", "Mean AGI", money), ("mean_E00200", "Mean wages", money),
                          ("mean_E00900", "Mean Schedule C", money), ("mean_E26270", "Mean partnership / S corp", money),
                          ("mean_P23250", "Mean long-term gains", money), ("mean_E00600", "Mean dividends", money),
                          ("mean_E02400", "Mean Social Security", money),
                          ("agi_share", "Share of all AGI (%)", lambda x: f"{100 * x:.1f}"),
                          ("nonzero_E00200", "Share with wages (%)", lambda x: f"{100 * x:.1f}"),
                          ("nonzero_P23250", "Share with long-term gains or losses (%)", lambda x: f"{100 * x:.1f}"),
                          ("nonzero_E00900", "Share with Schedule C (%)", lambda x: f"{100 * x:.1f}")]:
        rows = [[g["group"]] + [f(c["levels"][i][key]) for _, c in cols] for i, g in enumerate(T["levels"])]
        lines += [f"**{title}, by AGI group**", "", table(["AGI group"] + [n for n, _ in cols], rows), ""]
    rows = [["Total weight (millions)"] + [f"{c['totals']['total_weight'] / 1e6:,.1f}" for _, c in cols],
            ["Total AGI ($ billions)"] + [f"{c['totals']['agi_total'] / 1e9:,.0f}" for _, c in cols],
            ["Median AGI"] + [money(c["totals"]["agi_p50"]) for _, c in cols]]
    lines += [table(["Total", *[n for n, _ in cols]], rows), ""]
    # test 1's measures
    keys = [("marginal_tvd_median", "Single variables: median TVD across 40", 3),
            ("pair_tvd_median", "Two-way tables: median TVD across 780 pairs", 3),
            ("pair_tvd_p90", "Two-way tables: 90th percentile TVD", 3),
            ("triple_tvd_mean", "Three-way tables: mean TVD", 3),
            ("pattern_tvd", "Nonzero patterns of income items: TVD", 3),
            ("top1_agi_share", "Top 1% share of AGI", 3), ("top01_agi_share", "Top 0.1% share of AGI", 3),
            ("classifier_auc", "Classifier AUC (0.5 = indistinguishable)", 3)]
    rows = []
    for key, words, dec in keys:
        r = [words]
        for k in gens:
            for suf in ("_vstrain", ""):
                lab = f"{version}_gen_{k[len('gen_puf_'):]}{suf}"
                f = os.path.join(OUT, "puf_eval", lab, "summary.json")
                if os.path.exists(f):
                    sm = json.load(open(f))
                    r.append(fmt(sm.get(lab, {}).get(key), dec))
                    if suf == "":
                        r.append(fmt(sm.get("floor", {}).get(key), dec))
                        r.append(fmt(sm.get("test", {}).get(key), dec) if key.startswith("top") else "")
                else:
                    r += ["–"] + (["–", ""] if suf == "" else [])
        rows.append(r)
    r = ["Classifier AUC with the weight left out (E; training records against generated)"]
    for k in gens:
        c = F[k].get("classifier_no_weight")
        r += [fmt(c["auc"], 3) if c else "–", "", "", ""]
    rows.append(r)
    hdr = ["Test 1 measure"]
    for k in gens:
        sd = int(k[-2:])
        hdr += [f"Seed {sd} vs training records", f"Seed {sd} vs held-out test records",
                "Floor: training vs test records", "Test records' own value"]
    lines += ["**Test 1's measures** (TVD: total variation distance between weighted binned distributions; "
              "the floor is the training records themselves against the held-out test records, i.e. "
              "sampling noise only). Test 1's classifier uses the record weight as a variable; generated "
              "records all carry the same weight, so its AUC of 1 says only that; the last row leaves the "
              "weight out.", "", table(hdr, rows), ""]
    for k in gens[:1]:
        f = os.path.join(OUT, "puf_eval", f"{version}_gen_{k[len('gen_puf_'):]}", "report_tables.md")
        if os.path.exists(f):
            lines += [f"Test 1's full tables for seed {int(k[-2:])} against the held-out test records: "
                      f"`{os.path.relpath(f, P)}` under the scratch folder.", ""]
    return "\n".join(lines)


def sec_full_scf(F, split):
    labs = sorted(k for k in F if k.startswith(f"scf_{split}_seed"))
    tl = f"truth_{split}"
    if not labs or tl not in F:
        return ""
    S = {"truth": F[tl]}
    for k in labs:
        S[k] = F[k]
    first = F[labs[0]]
    sets = first.get("sets", {})
    S["yardstick"] = {"sets": sets, "classifier_draw0_vs_draw1": first.get("classifier_draw0_vs_draw1"),
                      "draws_file": first.get("file")}
    one, half = yard_sets(S)
    cols = ["truth"] + labs
    g, _ = sec_gaps(S, cols, one, half)
    txt = sec_levels(S, cols, one, half).replace("### 3. Untargeted relationships", "").replace(
        " (section 4)", "").replace(
        "\"Truth, tokens decoded\" puts the true records' own wealth tokens through the same "
        "decoder as the draws: the difference from the truth is decoding noise, not model error. ", "")
    K = first.get("K")
    words = {"train": "training", "test": "held-out test"}[split]
    head = [f"### SCF {words} records: wealth drawn by the full model against the truth", "",
            f"Each SCF {words} record with wealth observed has its 24 wealth components set aside and redrawn "
            f"{K} times from its observed variables (the crosswalked PUF items, demographics, year). Income "
            "groups: test 2's seven-income total, ranked within wave over all SCF units of the split. Draw "
            f"noise: draws 0 and 1 against each other (one draw per record), and the first {K // 2} draws "
            f"against the next {K // 2}.", ""]
    return "\n".join(head + [txt, g, ""])


VERSION_WORDS = {"full_v2": "version 2 (main: Schedule E total added, more SCF-observed PUF items)",
                 "full": "version 1 (fallback)"}


def build_full(version="full"):
    F = load_dir(os.path.join(OUT, version))
    parts = [f"## Pretraining: the full model, {VERSION_WORDS.get(version, version)}, against ground truth", "",
             "The full model (piece H) is one transformer over test 1's PUF variables and the 24 SCF wealth "
             "components, trained on PUF 2015 and SCF 2013 and 2016 records together. These sections measure "
             "how close its records are to the data before any post-training."]
    if not F:
        return "\n".join(parts + ["", "(No full-model files evaluated yet.)", ""])
    parts.append("")
    for t in (sec_gen_puf(F, version), sec_full_scf(F, "train"), sec_full_scf(F, "test")):
        if t:
            parts += [t, ""]
    pw = sorted(k for k in F if k.startswith("puf_wealth_seed"))
    if pw and "scf2013" in F:
        SP = {"scf2013": F["scf2013"], "scf2016": F["scf2016"]}
        for k in pw:
            SP[k] = F[k]
        sets = F[pw[0]].get("sets", {})
        noise = (sets["half_a"]["stats"], sets["half_b"]["stats"]) if "half_b" in sets else None
        parts += [sec_puf(SP, title="### PUF training records completed with wealth, against the SCF", sub="F",
                          noise_override=noise).replace("### F3. Untargeted relationships", "#### Levels")
                  .replace("#### Net worth by income group", "##### Net worth by income group"), ""]
    PP = load_dir(os.path.join(OUT, version, "post"))
    if PP and "scf2013" in F:
        SP = {"scf2013": F["scf2013"], "scf2016": F["scf2016"]}
        SP.update({k: v for k, v in PP.items()})
        parts += [sec_puf(SP, title=f"## Post-training on the full model ({version}): arm P (PUF records completed "
                          "with wealth)", sub="Q", bglob=[os.path.join(P, version, "b", "**", "report*.json")]), ""]
    else:
        parts += ["## Post-training on the full model", "", "(No post-training files on the full model evaluated yet.)",
                  ""]
    return "\n".join(parts)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--update-report", action="store_true")
    args = ap.parse_args()
    S = load()
    pilot = build_pilot(S)
    has_v2 = bool(glob.glob(os.path.join(OUT, "full_v2", "*", "stats.json")))
    fulls = [build_full("full_v2"), "", build_full("full")] if has_v2 else [build_full("full"), ""]
    body = "\n".join(fulls + ["",
                      "## Appendix: pilot on the current model", "",
                      "Everything below uses test 2's model (10 PUF-side variables), which the full model replaces. "
                      "It checked the evaluation code and the post-training methods; it is not the main result.", "",
                      demote(pilot)])
    open(os.path.join(OUT, "compare.md"), "w").write(body + "\n")
    print(f"wrote {OUT}/compare.md ({len(body)} characters)")
    if args.update_report:
        txt = open(REPORT).read() if os.path.exists(REPORT) else ""
        if BEGIN in txt and END in txt:
            pre, rest = txt.split(BEGIN, 1)
            post = rest.split(END, 1)[1]
            txt = pre + BEGIN + "\n" + body + "\n" + END + post
        else:
            txt = txt + "\n" + BEGIN + "\n" + body + "\n" + END + "\n"
        tmp = REPORT + ".tmp"
        open(tmp, "w").write(txt)
        os.replace(tmp, REPORT)
        print(f"updated {REPORT}")


if __name__ == "__main__":
    main()
