"""Probe DD: SCF wealth completion scores (nats per record, lower is better),
as c3_summary.py: validation records (each training run's own json) and SCF
test records (24 wealth values summed; 'paid hidden' also hides mortgage
interest and real estate tax paid). Markdown to stdout.

    python dd_scores.py
"""
import glob
import json

import numpy as np

P = "/nfs/roberts/scratch/pi_nrs36/jar335/probes"
WEALTH = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin',
          'pass_throughs', 'primary_home', 'other_home', 're_fund', 'other_nonfin', 'primary_mortgage',
          'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt',
          'kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
MODELS = [("Bridge variables (BR)", f"{P}/br/model/full_v3br/run_full_v3br_w1_seed*.json",
           f"{P}/br/model/full_v3br/h_scf_test_seed*_info.json", f"{P}/br/model/full_v3br/h_scf_test_nobr_seed*_info.json"),
          ("C, deductions zero (full_v3cz)", f"{P}/c3/model_cz/run_full_v3cz_seed*.json",
           f"{P}/c3/model_cz/h_scf_test_seed*_info.json", f"{P}/c3/model_cz/h_scf_test_nobr_seed*_info.json"),
          ("C, deductions unknown (full_v3c)", f"{P}/c3/model/run_full_v3c_seed*.json",
           f"{P}/c3/model/h_scf_test_seed*_info.json", f"{P}/c3/model/h_scf_test_nobr_seed*_info.json"),
          ("DD, paid observed for PUF itemizers (full_v4dd)", f"{P}/dd/model_dd/run_full_v4dd_seed*.json",
           f"{P}/dd/model_dd/h_scf_test_seed*_info.json", f"{P}/dd/model_dd/h_scf_test_nobr_seed*_info.json"),
          ("DD, paid on SCF rows only (full_v4ddp)", f"{P}/dd/model_ddp/run_full_v4ddp_seed*.json",
           f"{P}/dd/model_ddp/h_scf_test_seed*_info.json", f"{P}/dd/model_ddp/h_scf_test_nobr_seed*_info.json")]


def fmt(vals):
    vals = [v for v in vals if v is not None and np.isfinite(v)]
    return ", ".join(f"{v:.2f}" for v in vals) if vals else "pending"


def val_score(path):
    vc = json.load(open(path)).get("val_completion")
    if isinstance(vc, str) or not vc:
        return None
    if "MORTGAGE_INTEREST_PAID" in vc.get("by_variable", {}):
        # full_v4ddp: training treats the paid variables as SCF-only, so its
        # validation score hides and scores them too; not comparable
        return float("nan")
    return vc.get("nll_per_record")


def test_score(path):
    sc = json.load(open(path))["scores"]
    if isinstance(sc, str):
        return None
    bv = sc["by_variable"]
    return sum(bv[v]["nll"] for v in WEALTH if v in bv)


print("Validation scores of the full_v4ddp models are not shown: h_train.py treats paid (observed on SCF rows only) "
      "as an SCF-only variable there, so its validation score also hides and scores paid. Test-record scores are "
      "comparable across all rows (24 wealth values).\n")
print("| Model | Validation records | Test records | Test records, paid (bridge) hidden |")
print("|---|---|---|---|")
for lab, rv, rt, rn in MODELS:
    v = [val_score(f) for f in sorted(glob.glob(rv))]
    t = [test_score(f) for f in sorted(glob.glob(rt))]
    n = [test_score(f) for f in sorted(glob.glob(rn))]
    print(f"| {lab} | {fmt(v)} | {fmt(t)} | {fmt(n)} |")
