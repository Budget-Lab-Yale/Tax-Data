"""Probe C: summary tables for C_REPORT.md, written as markdown to stdout.

1. SCF wealth completion scores (nats per record, lower is better):
   (a) validation records, from each training run's own json (val_completion,
       the score test 3 quotes as 35.50-35.56 for version 2);
   (b) SCF test records, the 24 wealth values summed (h_complete scf_test,
       as probe BR's table), with the bridge variables observed and hidden.
2. Completed PUF wealth by AGI group (from c3_wealth.json): mean over the
   training seeds of each model, range in brackets, against the SCF
   interpolated to 2015.
The generation tables come from c3_table.py.

    python c3_summary.py
"""
import glob
import json
import os
import re

import numpy as np

T2 = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2"
P = "/nfs/roberts/scratch/pi_nrs36/jar335/probes"
WEALTH = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin',
          'pass_throughs', 'primary_home', 'other_home', 're_fund', 'other_nonfin', 'primary_mortgage',
          'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt',
          'kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
MODELS = [("Version 2 (test 3)", f"{T2}/run_full_v2_w1_seed*.json", f"{T2}/h_scf_test_seed*_info.json", None),
          ("AGI remainder (AS)", f"{P}/as/model/run_full_v2s_seed0[1-3].json", None, None),
          ("Translated SCF (X)", f"{P}/x/model/full_v3x2/run_full_v3x2_w1_seed*.json",
           f"{P}/c3/xscore/h_scf_test_seed*_info.json", None),
          ("Bridge variables (BR)", f"{P}/br/model/full_v3br/run_full_v3br_w1_seed*.json",
           f"{P}/br/model/full_v3br/h_scf_test_seed*_info.json", f"{P}/br/model/full_v3br/h_scf_test_nobr_seed*_info.json"),
          ("Combined, deductions zero (full_v3cz)", f"{P}/c3/model_cz/run_full_v3cz_seed*.json",
           f"{P}/c3/model_cz/h_scf_test_seed*_info.json", f"{P}/c3/model_cz/h_scf_test_nobr_seed*_info.json"),
          ("Combined, deductions unknown (full_v3c)", f"{P}/c3/model/run_full_v3c_seed*.json",
           f"{P}/c3/model/h_scf_test_seed*_info.json", f"{P}/c3/model/h_scf_test_nobr_seed*_info.json"),
          ("Combined, deductions zero, weight as variable (full_v3czw)", f"{P}/c3/model_czw/run_*seed*.json",
           f"{P}/c3/model_czw/h_scf_test_seed*_info.json", f"{P}/c3/model_czw/h_scf_test_nobr_seed*_info.json"),
          ("Combined, deductions zero, SCF Schedule E unobserved (full_v3czs)", f"{P}/c3/model_czs/run_full_v3czs_seed*.json",
           f"{P}/c3/model_czs/h_scf_test_seed*_info.json", f"{P}/c3/model_czs/h_scf_test_nobr_seed*_info.json")]


def fmt(vals, f="{:.2f}"):
    vals = [v for v in vals if v is not None and np.isfinite(v)]
    if not vals:
        return "pending"
    s = ", ".join(f.format(v) for v in vals)
    return s


def val_score(path):
    d = json.load(open(path))
    vc = d.get("val_completion")
    if isinstance(vc, str):
        return None
    return vc.get("nll_per_record") if vc else None


def test_score(path):
    sc = json.load(open(path))["scores"]
    if isinstance(sc, str):
        return None
    bv = sc["by_variable"]
    return sum(bv[v]["nll"] for v in WEALTH if v in bv)


def main():
    print("## SCF wealth completion scores (nats per record; lower is better)\n")
    print("Each value is one training seed. Validation: the score each training run "
          "reports for SCF validation records (all other variables observed). Test: the 24 wealth "
          "values of SCF test records, summed, as in probe BR's table; 'bridge hidden' hides mortgage "
          "interest and real estate tax too.\n")
    print("| Model | Validation records | Test records | Test records, bridge hidden |")
    print("|---|---|---|---|")
    for lab, rv, rt, rn in MODELS:
        v = [val_score(f) for f in sorted(glob.glob(rv))]
        t = [test_score(f) for f in sorted(glob.glob(rt))] if rt else []
        n = [test_score(f) for f in sorted(glob.glob(rn))] if rn else []
        print(f"| {lab} | {fmt(v)} | {fmt(t) if rt else 'not run'} | {fmt(n) if rn else 'not run'} |")
    # completed wealth
    f = f"{P}/c3/c3_wealth.json"
    if not os.path.exists(f):
        print("\nCompleted PUF wealth: pending (c3_wealth.json not written)")
        return
    res = json.load(open(f))
    groups = {}
    for k, v in res.items():
        m = re.match(r"(.+), seed (\d+)$", k)
        if m and isinstance(v, dict) and "mean" in v:
            groups.setdefault(m.group(1), []).append(v)
    scf = res["SCF interpolated 2015"]
    LAB = ["Bottom 50%", "50-90%", "90-99%", "99-99.9%", "Top 0.1%"]
    names = list(groups)

    def cell(vals, f):
        if len(vals) == 1:
            return f(vals[0])
        return f(np.mean(vals)) + " [" + f(min(vals)) + "–" + f(max(vals)) + "]"
    k_ = lambda x: f"${x / 1e3:,.0f}k"
    pc = lambda x: f"{100 * x:.0f}%"
    print("\n## Completed PUF wealth (PUF training records, five draws each; mean over training seeds, "
          "range in brackets)\n")
    print("PUF groups by true AGI; SCF groups by the seven-income total within each wave, "
          "interpolated to 2015 (one third 2013, two thirds 2016), wealth-observed units only.\n")
    rows = [("All returns, mean net worth", lambda v: v["mean"], k_),
            ("All returns, median net worth", lambda v: v["median"], k_)]
    for st, lab, f_ in (("mean", "mean net worth", k_), ("median", "median net worth", k_),
                        ("hold_primary_home", "own a home", pc), ("hold_equities", "hold stocks", pc),
                        ("hold_bonds", "hold bonds", pc), ("hold_pass_throughs", "hold a business", pc)):
        for g in LAB:
            key = f"agi|{g}|{st}"
            skey = f"inc7|{g}|{st}"
            rows.append((f"{g}, {lab}", (lambda v, key=key: v[key]), f_, skey))
    for h in ("primary_home", "equities", "dc", "pass_throughs"):
        rows.append((f"All returns, hold {h}", (lambda v, h=h: v[f"hold_{h}"]), pc))
    print("| | SCF 2015 (interpolated) | " + " | ".join(f"{n} ({len(groups[n])})" for n in names) + " |")
    print("|---" * (len(names) + 2) + "|")
    for r in rows:
        lab, fn, f_ = r[0], r[1], r[2]
        if len(r) == 4:
            sv = scf.get(r[3])
        else:
            sv = fn(scf) if True else None
        print(f"| {lab} | {f_(sv) if sv is not None else ''} | " +
              " | ".join(cell([fn(v) for v in groups[n]], f_) for n in names) + " |")
    so = res.get("scf_with_split_offs_at_zero", {})
    if so:
        print("\nSCF filer units with split-off units counted at zero net worth (X's bound): " +
              "; ".join(f"{y}: mean ${v['mean']/1e3:,.0f}k, median ${v['median']/1e3:,.0f}k, "
                        f"split-off weight share {100*v['split_off_weight_share']:.1f}%" for y, v in so.items()))
    nd = res.get("puf_not_dependent_filers", {})
    if nd:
        agg = {}
        for k, v in nd.items():
            m = re.match(r"(.+), seed (\d+)$", k)
            agg.setdefault(m.group(1) if m else k, []).append(v)
        print("\nPUF returns that are not dependent filers (the SCF's population), median net worth and home "
              "ownership: " + "; ".join(f"{n}: {cell([v['median'] for v in vs], k_)}, "
                                        f"{cell([v['hold_primary_home'] for v in vs], pc)}" for n, vs in agg.items()))


if __name__ == "__main__":
    main()
