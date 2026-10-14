"""Test 3, piece D: collect one variant's results into report.json and draw
its training curves (curves.png).

Reads, in --out-dir: train.json and curve.csv (d_tune.py train),
draws_info.json (d_tune.py draws), report_resid.json (b_calibrate.py on the
fresh draws). Also reads piece B's report on the reference draws, for
comparison, if present.
"""

import argparse
import json
from pathlib import Path

import pandas as pd

P = Path("/nfs/roberts/scratch/pi_nrs36/jar335/posttrain")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--b-report", default=str(P / "b/report_ref.json"))
    a = ap.parse_args()
    out = Path(a.out_dir)
    tr = json.load(open(out / "train.json"))
    di = json.load(open(out / "draws_info.json"))
    rr = json.load(open(out / "report_resid.json")) if (out / "report_resid.json").exists() else None
    br = json.load(open(a.b_report)) if Path(a.b_report).exists() else None
    keep = ["n_records", "K", "status_counts", "target_weight_by_status",
            "feasible_max_abs_error", "feasible_max_rel_error", "kl_nats_per_unit_weight",
            "effective_draws_weighted_mean", "effective_draws_unweighted_mean",
            "share_records_below4_effective_draws_weighted",
            "share_records_below4_effective_draws_unweighted",
            "effective_draws_quantiles_unweighted", "max_draw_share_of_record_weight",
            "feasible_max_error_share_of_group", "all_cells_max_error_share_of_group",
            "infeasible_cells_mean_error_share_of_group", "status_by"]
    rep = dict(
        variant=tr["variant"],
        settings={k: tr["args"].get(k) for k in ("lr", "batch", "mix", "ema", "minutes",
                                                 "eval_k", "beta", "rl_batch", "refresh_steps",
                                                 "draws", "weights")},
        training=dict(steps=tr["steps"], minutes=tr["minutes"], best_step=tr["best_step"],
                      stop_reason=tr["stop_reason"], gpu=tr.get("gpu"),
                      slurm_job_id=tr.get("slurm_job_id")),
        reference_eval=tr["reference_eval"],
        best_eval=[h for h in tr["history"] if h["step"] == tr["best_step"]][-1],
        fresh_draws=dict(k=di["k"], seed=di["seed"],
                         kl_from_reference_chain_rule=di["kl_chain"],
                         kl_from_reference_sampled=di["kl_sampled"],
                         target_fit_before_calibration_expected=di["fit_expected"],
                         target_fit_before_calibration_realized=di["fit_realized"],
                         ref_logprob_check=di["ref_logprob_check"]),
        residual_calibration=({k: rr[k] for k in keep if k in rr} if rr else None),
        method1_on_reference_draws=({k: br[k] for k in keep if k in br} if br else None),
    )
    if rr:
        rep["kl_total_approx"] = di["kl_chain"] + rr["kl_nats_per_unit_weight"]
        rep["kl_total_note"] = ("KL of the tuned model from the reference plus the residual "
                                "calibration's KL from the tuned model's draws. KL divergences "
                                "do not add exactly; this sum is the KL of the final weighted "
                                "draws from the reference only if the residual weights are "
                                "unrelated to the log-ratio of the two models.")
    with open(out / "report.json", "w") as f:
        json.dump(rep, f, indent=2, default=float)

    c = pd.read_csv(out / "curve.csv")
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    fig, ax = plt.subplots(1, 3, figsize=(13, 3.6))
    ax[0].plot(c["minutes"], c["misplaced_expected"], "o-", label="expected cells")
    ax[0].plot(c["minutes"], c["misplaced_realized"], "s--", label="drawn cells")
    ax[0].set_title("Share of weight in the wrong coarse bin")
    ax[0].set_xlabel("minutes of training")
    ax[0].legend()
    ax[1].plot(c["minutes"], c["kl_chain"], "o-", label="chain rule")
    ax[1].plot(c["minutes"], c["kl_sampled"], "s--", label="sampled log-ratio")
    if br:
        ax[1].axhline(br["kl_nats_per_unit_weight"], color="grey", ls=":",
                      label="method 1 (reweighting)")
    ax[1].set_title("KL from the reference (nats per unit weight)")
    ax[1].set_xlabel("minutes of training")
    ax[1].legend()
    ax[2].plot(c["minutes"], c["train_loss"], "o-", label="training loss")
    if c["orig_loss"].notna().any():
        ax[2].plot(c["minutes"], c["orig_loss"], "s--", label="original task loss")
    ax[2].set_title("Training losses (mean since last evaluation)")
    ax[2].set_xlabel("minutes of training")
    ax[2].legend()
    fig.suptitle(f"Method 3, variant {tr['variant']}")
    fig.tight_layout()
    fig.savefig(out / "curves.png", dpi=120)
    print(json.dumps({k: rep[k] for k in ("training", "fresh_draws")}, indent=1, default=float))
    if rr:
        print("residual calibration KL", rr["kl_nats_per_unit_weight"], "effective draws",
              rr["effective_draws_weighted_mean"], "status", rr["status_counts"])


if __name__ == "__main__":
    main()
