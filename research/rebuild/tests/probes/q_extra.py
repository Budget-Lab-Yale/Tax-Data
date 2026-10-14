"""Probe Q: a few extra aggregate numbers for the report."""
import json
from pathlib import Path
Q = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/q")
INC = ["wages", "Schedule C", "Schedule E", "long-term gains", "dividends", "taxable interest",
       "taxable pensions", "Social Security", "farm", "tax-exempt interest", "unemployment"]
names = ["ep01", "ep02", "ep03", "ep04", "ep05", "ep06", "ep08", "ep12", "best", "ep20", "ep30",
         "best_noincome", "best_nothing"]
d = {}
for n in names:
    f = Q / "eval" / f"eval_{n}.json"
    if f.exists():
        d.update({k: v for k, v in json.load(open(f)).items() if k == n or k not in d})
for sp in ("train", "test"):
    print(f"\nrank correlation of net worth with each income, {sp}")
    print("truth", [round(x, 3) for x in d[f"truth_levels_{sp}"]["corr_nw_income"]])
    for n in ("best", "ep04", "best_noincome", "best_nothing", "ep02"):
        if n in d:
            for w in ("before", "after"):
                print(n, w, [round(x, 3) for x in d[n][f"{sp}_{w}_levels"]["corr_nw_income"]])
print("\nmean net worth by household type (MARS 1-4 x children 0/1+), train truth ($000):",
      [round(x / 1e3) for x in d["truth_levels_train"]["nw_mean_ht"]])
for n in ("best", "best_nothing", "ep02"):
    if n in d:
        print(n, [round(x / 1e3) for x in d[n]["train_before_levels"]["nw_mean_ht"]], "->",
              [round(x / 1e3) for x in d[n]["train_after_levels"]["nw_mean_ht"]])
for n in names:
    r = Q / "runs" / n / "report.json"
    if r.exists():
        r = json.load(open(r))
        fs = r["fit"]["fit_steps"]
        print(n, "epoch", r["ckpt_epoch"], "val score", round(r["completion_score"]["val"]["nats_per_record_weighted"], 2),
              "train score", round(r["completion_score"]["train"]["nats_per_record_weighted"], 2),
              "test score", round(r["completion_score"]["test"]["nats_per_record_weighted"], 2),
              "KL fit/train/test", round(r["fit"]["kl_stepwise"], 3), round(r["train_after"]["kl_stepwise"], 3),
              round(r["test_after"]["kl_stepwise"], 3),
              "ridge vars", sum(1 for v in fs.values() if v.get("ridge_used")),
              "at cap", sum((v.get("n_at_cap") or 0) for v in fs.values()),
              "max|lam|", round(max((v.get("max_abs_lambda") or 0) for v in fs.values()), 1),
              "exp err mean/max %", round(100 * r["train_after"]["expected_error"]["mean_share_err_pos"], 4),
              round(100 * r["train_after"]["expected_error"]["max_share_err_pos"], 2),
              "minutes", round(r["seconds_total"] / 60, 1), r["gpu"])
