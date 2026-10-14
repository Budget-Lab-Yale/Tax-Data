"""Probe ST collector: markdown tables from $ST/eval/eval_<variant>.json into
$ST/ST_TABLES.md. Model runs with several seeds are averaged over seeds
(the range over seeds is given for the main error measures)."""
import json
import sys
from pathlib import Path

import numpy as np

ST = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/st")
INC = ["Bottom 50%", "50–90%", "90–99%", "99–99.9%", "Top 0.1%"]
INCOMES = ["wages", "Schedule C", "Schedule E", "long-term gains", "dividends",
           "taxable interest", "taxable pensions", "Social Security", "farm",
           "tax-exempt interest", "unemployment"]
WEALTH = ["cash", "other_nonfin", "primary_home", "kg_primary_home", "dc", "installment_debt",
          "primary_mortgage", "credit_cards", "equities", "life_ins", "kg_other", "kg_other_re",
          "other_home", "bonds", "pass_throughs", "kg_pass_throughs", "other_fin", "re_fund", "db",
          "other_debt", "other_mortgage", "annuities", "credit_lines", "trusts"]
NAMED = ["Mortgage among homeowners", "Equities among pass-through owners",
         "Unrealized gain among pass-through owners", "Other-property mortgage among its owners"]
GROUPS = [("full", "Model trained with 2016 (test 3's version 2)"),
          ("held", "Model trained without 2016")]
MEAS = [("targeted_mean_abs_pp", "Targeted cells (pp)", 2),
        ("hold_inc_age_pp", "Holding rates, income × age (pp)", 1),
        ("nw_inc_age_pct", "Mean net worth, income × age (%)", 0),
        ("pair_joint_pp", "Joint holding of pairs (pp)", 2),
        ("top1_share_pp", "Top 1% share within income group (pp)", 1),
        ("nw_ht_pct", "Mean net worth by household type (%)", 0),
        ("corr_abs_diff", "Correlation of net worth with incomes", 3)]


def f(x, d):
    return "" if x is None else f"{x:,.{d}f}"


def seeds(res, prefix):
    return sorted(k for k in res if k.startswith(prefix + "_s"))


def avg(dicts, key):
    v = [np.array(d[key], dtype=float) for d in dicts]
    return np.mean(v, 0), np.min(v, 0), np.max(v, 0)


def rows_for(res, sp):
    """(label, error dict, levels dict, tilt nats, ESS share, completion score 2016)"""
    out = []
    for nm in ("scf2013_as_is",):
        if nm in res:
            out.append(("SCF 2013 records as they are", res[nm][f"{sp}_after"], res[nm]["levels"], None, None, None))
    for k in sorted(x for x in res if x.startswith("scf2013_reweighted_")):
        rho = k.split("_")[-1]
        c = res.get(f"calib_{res['variant'] or 'base'}_{rho}", {})
        lab = (f"ridge {rho[6:]} on population shares" if rho.startswith("rhopop")
               else f"ridge {rho[3:]} on within-group shares")
        out.append((f"SCF 2013 records reweighted to 2016 tables ({lab})",
                    res[k][f"{sp}_after"], res[k]["levels"], c.get("kl_nats"),
                    c.get("ess_share_vs_base"), None))
    for g, lab in GROUPS:
        ss = seeds(res, g)
        if not ss:
            continue
        R = [res[s] for s in ss]
        for when, wl in (("before", "no post-training"), ("after", "tilted to 2016 tables")):
            err = {m: float(np.mean([r[f"{sp}_{when}"][m] for r in R])) for m, _, _ in MEAS}
            err["_range"] = {m: (min(r[f"{sp}_{when}"][m] for r in R), max(r[f"{sp}_{when}"][m] for r in R))
                             for m, _, _ in MEAS}
            for m in ("named_conditional_pp", "nw_mean_inc_pct", "nw_median_inc_pct", "top1_share_pp_by_inc"):
                err[m] = avg([r[f"{sp}_{when}"] for r in R], m)[0].tolist()
            lev = {m: avg([r[f"{sp}_{when}_levels"] for r in R], m)[0].tolist()
                   for m in R[0][f"{sp}_{when}_levels"]}
            kl = float(np.mean([r[f"kl_{sp}"] for r in R])) if when == "after" else None
            ess = (float(np.mean([r.get(f"ess_share_record_{sp}", np.nan) for r in R]))
                   if when == "after" else None)
            cs = float(np.mean([r["completion_score"][sp]["weighted_2016"] for r in R]))
            out.append((f"{lab}, {wl} ({len(R)} seeds)", err, lev, kl, ess, cs))
    return out


def section(res, sp, title):
    global TRUTH
    TRUTH = "Truth 2016" if res["variant"] is None else f"Truth (2016, top 1% weighted ×{res['variant'][3:]})"
    L = []
    rows = rows_for(res, sp)
    y = res[f"yardstick_truth_halves_{sp}"]
    tl = res[f"truth_levels_{sp}"]
    n = res[f"truth_n_{sp}"]
    L.append(f"### {title}: SCF 2016 {sp}ing records ({n['rows']:,} rows, {n['households']:,} households)\n"
             if sp == "train" else f"### {title}: SCF 2016 test records ({n['rows']:,} rows, {n['households']:,} households)\n")
    L.append("Errors against the 2016 truth (mean absolute error; see probe Q for the definitions).\n")
    L.append("| Comparator | Tilt or reweighting size (nats) | Effective sample (share; tilt: record by record) | Completion score, 2016 (nats) | "
             + " | ".join(m[1] for m in MEAS) + " |")
    L.append("|---" * (4 + len(MEAS)) + "|")
    for lab, e, lev, kl, ess, cs in rows:
        L.append(f"| {lab} | {f(kl, 2)} | {f(ess, 2)} | {f(cs, 2)} | "
                 + " | ".join(f(e[m], d) for m, _, d in MEAS) + " |")
    L.append("| Truth's own sampling error (two halves) | | | | "
             + " | ".join(f(y[m], d) for m, _, d in MEAS) + " |\n")
    ranged = [(lab, e) for lab, e, *_ in rows if "_range" in e]
    if ranged:
        L.append("Range over seeds of the same measures:\n")
        L.append("| Comparator | " + " | ".join(m[1] for m in MEAS) + " |")
        L.append("|---" * (1 + len(MEAS)) + "|")
        for lab, e in ranged:
            L.append(f"| {lab} | " + " | ".join(f"{f(e['_range'][m][0], d)}–{f(e['_range'][m][1], d)}"
                                                for m, _, d in MEAS) + " |")
        L.append("")
    for key, name, scale, d in (("nw_mean_inc", "Mean net worth, $ thousands", 1e-3, 0),
                                ("nw_median_inc", "Median net worth, $ thousands", 1e-3, 0),
                                ("top1_share_inc", "Top 1 percent's share of net worth within the income group, %", 100, 1)):
        L.append(f"{name}, by income group (untargeted):\n")
        L.append("| | " + " | ".join(INC) + " |")
        L.append("|---" * 6 + "|")
        L.append(f"| {TRUTH} | " + " | ".join(f(v * scale, d) for v in tl[key]) + " |")
        for lab, e, lev, *_ in rows:
            L.append(f"| {lab} | " + " | ".join(f(v * scale, d) for v in lev[key]) + " |")
        L.append("")
    L.append("Holding rates by income group, %, for four wealth items (targeted: the zero bin of each item within income groups):\n")
    items = ["primary_home", "primary_mortgage", "equities", "pass_throughs"]
    L.append("| | " + " | ".join(f"{it}, {g}" for it in items for g in ("bottom 50%", "top 0.1%")) + " |")
    L.append("|---" * 9 + "|")
    def hrow(h):
        h = np.array(h)
        return " | ".join(f(100 * h[g, WEALTH.index(it)], 1) for it in items for g in (0, 4))
    L.append(f"| {TRUTH} | " + hrow(tl["hold_by_inc"]) + " |")
    for lab, e, lev, *_ in rows:
        L.append(f"| {lab} | " + hrow(lev["hold_by_inc"]) + " |")
    L.append("")
    L.append("Relationships between wealth items, % (untargeted):\n")
    L.append("| | " + " | ".join(NAMED) + " |")
    L.append("|---" * 5 + "|")
    L.append(f"| {TRUTH} | " + " | ".join(f(100 * v, 1) for v in tl["named_conditional"]) + " |")
    for lab, e, lev, *_ in rows:
        L.append(f"| {lab} | " + " | ".join(f(100 * v, 1) for v in lev["named_conditional"]) + " |")
    L.append("")
    L.append("Rank correlation of net worth with each income (untargeted):\n")
    pick = [0, 2, 3, 4, 5, 9]
    L.append("| | " + " | ".join(INCOMES[i] for i in pick) + " |")
    L.append("|---" * 7 + "|")
    L.append(f"| {TRUTH} | " + " | ".join(f(tl["corr_nw_income"][i], 2) for i in pick) + " |")
    for lab, e, lev, *_ in rows:
        L.append(f"| {lab} | " + " | ".join(f(lev["corr_nw_income"][i], 2) for i in pick) + " |")
    L.append("")
    return L


def main():
    L = ["# Probe ST tables (written by st_table.py from the evaluation files)\n"]
    for v, title in (("base", "Stretch from 2013 to 2016"),
                     ("top2", "Harder stretch: 2016 with the top 1% of each income group's net worth weighted ×2"),
                     ("top4", "Harder stretch: 2016 with the top 1% of each income group's net worth weighted ×4")):
        fn = ST / "eval" / f"eval_{v}.json"
        if not fn.exists():
            L.append(f"## {title}\n\nPending: {fn} not written.\n")
            continue
        res = json.load(open(fn))
        L.append(f"## {title}\n")
        for k in sorted(x for x in res if x.startswith("calib_")):
            c = res[k]
            L.append(f"- Reweighting {k[6:]}: {c['n_records']:,} 2013 rows; KL {c['kl_nats']:.3f} nats; "
                     f"effective sample {100*c['ess_share_vs_base']:.1f}% of the 2013 records' own; "
                     f"largest weight ratio {c['max_weight_ratio']:.1f}; 2016 cells no 2013 record reaches: "
                     f"{c['unreachable_cells']} (population share summed over cells {c['unreachable_share']:.4f}).")
        L.append("")
        for sp in ("train", "test"):
            L += section(res, sp, title)
    (ST / "ST_TABLES.md").write_text("\n".join(L) + "\n")
    print("wrote", ST / "ST_TABLES.md")


if __name__ == "__main__":
    main()
