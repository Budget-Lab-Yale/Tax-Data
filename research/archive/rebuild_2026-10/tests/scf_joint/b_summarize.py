"""Summarize piece B's runs: one table of training runs and one of
completions, per dataset, plus training-curve plots.

Reads $S/model/<dataset>/run_*.json, train_log_*.csv and
completed_*_info.json. Writes $S/model/b_summary_runs.csv,
$S/model/b_summary_completions.csv, $S/model/<dataset>/training_curves.png,
and prints Markdown tables (aggregates only; no records).

Usage: python b_summarize.py [--root $S/model]
"""

import argparse
import glob
import json
import re
from pathlib import Path

import numpy as np
import pandas as pd


def md(df, index=True):
    """Markdown table if tabulate is installed, else plain text."""
    try:
        return df.to_markdown(index=index)
    except ImportError:
        return df.to_string(index=index)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--root", default="/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model")
    a = ap.parse_args()
    root = Path(a.root)
    runs, comps = [], []
    for ds_dir in sorted(p for p in root.iterdir() if p.is_dir()):
        ds = ds_dir.name
        for f in sorted(ds_dir.glob("run_*.json")):
            r = json.load(open(f))
            m = re.match(r"(.*)_seed(\d+)$", r["tag"])
            runs.append(dict(dataset=ds, config=m.group(1), seed=int(m.group(2)),
                             params=r["n_params"], gpu=r["gpu"],
                             n_train=sum(r["counts"]["train"].values()),
                             best_epoch=r["best_epoch"], epochs=r["epochs_run"],
                             val_loss=r["best_val_loss"],
                             val_completion_nll=(r.get("val_completion") or {}).get("nll_per_record"),
                             minutes=r["train_seconds"] / 60, job=r.get("slurm_job_id")))
        for f in sorted(ds_dir.glob("completed_*_info.json")):
            i = json.load(open(f))
            sc = i.get("scores") or {}
            comps.append(dict(dataset=ds, tag=i["tag"], seed=i["train_seed"],
                              n_records=i["n_records"], as_source=i.get("as_source"),
                              nll_per_record=sc.get("nll_per_record"),
                              n_scored=sc.get("n_records_scored"),
                              effective_draws_of_K=i.get("mean_effective_draws_of_K"),
                              decoded=i.get("decoded"), minutes=i["completion_seconds"] / 60))
        logs = sorted(ds_dir.glob("train_log_*.csv"))
        if logs:
            import matplotlib
            matplotlib.use("Agg")
            import matplotlib.pyplot as plt
            fig, ax = plt.subplots(1, 2, figsize=(11, 4))
            for f in logs:
                d = pd.read_csv(f)
                lab = f.stem.replace("train_log_", "")
                ax[0].plot(d["epoch"], d["train_loss"], label=lab, lw=1)
                ax[1].plot(d["epoch"], d["val_loss"], label=lab, lw=1)
            for k, t in enumerate(("training loss (nats per hidden value)",
                                   "validation loss (moving-average weights)")):
                ax[k].set_title(t, fontsize=10)
                ax[k].set_xlabel("epoch")
            lo = min(pd.read_csv(f)["val_loss"].min() for f in logs)
            ax[1].set_ylim(lo - 0.02, lo + 0.4)
            ax[1].legend(fontsize=6, ncol=2)
            fig.suptitle(ds)
            fig.tight_layout()
            fig.savefig(ds_dir / "training_curves.png", dpi=120)
            plt.close(fig)
    R = pd.DataFrame(runs)
    C = pd.DataFrame(comps)
    R.to_csv(root / "b_summary_runs.csv", index=False)
    C.to_csv(root / "b_summary_completions.csv", index=False)
    pd.set_option("display.width", 220)
    if len(R):
        print("## Training runs\n")
        print(md(R.round(4), index=False))
        g = R.groupby(["dataset", "config"]).agg(
            runs=("seed", "size"), val_loss_mean=("val_loss", "mean"),
            val_loss_range=("val_loss", lambda x: x.max() - x.min()),
            val_completion_nll_mean=("val_completion_nll", "mean"),
            val_completion_nll_range=("val_completion_nll", lambda x: x.max() - x.min()),
            best_epoch_mean=("best_epoch", "mean"), minutes_mean=("minutes", "mean"))
        print("\n## By configuration\n")
        print(md(g.round(4)))
    if len(C):
        print("\n## Completions\n")
        print(md(C.round(4), index=False))
        g = C.groupby(["dataset", "tag"]).agg(
            runs=("seed", "size"), nll_mean=("nll_per_record", "mean"),
            nll_range=("nll_per_record", lambda x: x.max() - x.min()),
            effective_draws=("effective_draws_of_K", "mean"))
        print("\n## Completions by tag\n")
        print(md(g.round(4)))


if __name__ == "__main__":
    main()
