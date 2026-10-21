"""Summarize piece H's training runs and completions (aggregates only).

Reads, in --model-dir (default $T2/model/full): train_log_<tag>_seedNN.csv,
run_<tag>_seedNN.json, h_*_seedNN_info.json. Writes training_curves.png and
h_summary.md there, and prints the summary. Every number is an aggregate,
so h_summary.md can be copied into the repository.
"""

import argparse
import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
import pandas as pd  # noqa: E402


def table(df):
    try:
        return df.to_markdown(index=False)
    except ImportError:                     # tabulate not installed
        cols = list(df.columns)
        out = ["| " + " | ".join(cols) + " |", "|" + "---|" * len(cols)]
        out += ["| " + " | ".join(str(x) for x in r) + " |" for r in df.itertuples(index=False)]
        return "\n".join(out)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--model-dir", default="/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full")
    ap.add_argument("--tag", default="full_w1")
    a = ap.parse_args()
    d = Path(a.model_dir)
    lines = [f"# Piece H summary ({d})", ""]
    runs = sorted(d.glob(f"run_{a.tag}_seed*.json"))
    logs = {}
    rows = []
    for r in runs:
        j = json.load(open(r))
        seed = j["args"]["seed"]
        lf = d / f"train_log_{j['tag']}.csv"
        if lf.exists():
            logs[seed] = pd.read_csv(lf)
        vc = j.get("val_completion") or {}
        rows.append(dict(seed=seed, parameters=j["n_params"], variables=j["n_variables"],
                         tokens=j["total_tokens"], gpu=j["gpu"], epochs=j["epochs_run"],
                         best_epoch=j["best_epoch"], best_val_loss=round(j["best_val_loss"], 4),
                         minutes=round(j["train_seconds"] / 60, 1),
                         steps_per_epoch=j["steps_per_epoch"],
                         val_wealth_nll=round(vc.get("wealth_nll_per_record", float("nan")), 3),
                         val_wealth_nll_popw=round(vc.get("wealth_nll_per_record_popweighted",
                                                          float("nan")), 3),
                         val_wealth_records=vc.get("n_records")))
    if rows:
        lines += ["## Training runs", "", table(pd.DataFrame(rows)), ""]
        j0 = json.load(open(runs[0]))
        lines += [f"Records by source: {j0['counts']}", f"Years: {j0['years']}",
                  f"Dropped variables: {j0['dropped_variables']}", ""]
    for seed, lg in logs.items():
        b = lg[lg["best"] == 1].iloc[-1]
        src_cols = [c for c in lg.columns if c.startswith("val_loss_")]
        lines.append(f"Seed {seed}, best epoch {int(b['epoch'])}: train {b['train_loss']:.4f}, "
                     f"validation {b['val_loss']:.4f} ("
                     + ", ".join(f"{c[9:]} {b[c]:.4f}" for c in src_cols) + ")")
    lines.append("")
    if logs:
        # Epochs 1-4 are left off: their losses are far higher and would hide
        # the late changes. A dot marks each seed's kept (best) epoch.
        first = 5
        src_cols = [c for c in next(iter(logs.values())).columns if c.startswith("val_loss_")]
        fig, axes = plt.subplots(1, 1 + len(src_cols), figsize=(5 * (1 + len(src_cols)), 4))
        axes = list(axes) if len(src_cols) else [axes]
        for k, (seed, lg) in enumerate(logs.items()):
            col = f"C{k}"
            g = lg[lg["epoch"] >= first]
            b = lg[lg["best"] == 1].iloc[-1]
            axes[0].plot(g["epoch"], g["train_loss"], ls="--", color=col,
                         label=f"training, seed {seed}")
            axes[0].plot(g["epoch"], g["val_loss"], color=col, label=f"validation, seed {seed}")
            axes[0].plot([b["epoch"]], [b["val_loss"]], "o", color=col)
            for ax, c in zip(axes[1:], src_cols):
                ax.plot(g["epoch"], g[c], color=col, label=f"seed {seed}")
                ax.plot([b["epoch"]], [b[c]], "o", color=col)
        axes[0].set_title("All records")
        for ax, c in zip(axes[1:], src_cols):
            ax.set_title(f"Validation, {c[9:]} records")
        for ax in axes:
            ax.set_xlabel(f"epoch (from {first}; dot = kept epoch)")
            ax.set_ylabel("cross-entropy, nats per hidden value")
            ax.legend(fontsize=7)
        fig.tight_layout()
        fig.savefig(d / "training_curves.png", dpi=120)
        lines.append(f"Training curves: {d / 'training_curves.png'}")
        lines.append("")
    infos = sorted(d.glob("h_*_seed*_info.json"))
    crow = []
    for f in infos:
        j = json.load(open(f))
        sc = j.get("scores") or {}
        crow.append(dict(file=f.name[:-10], mode=j["mode"], records=j["n_records"],
                         draws=j["draws"], filled=len(j["fill"]), decoded=j["decoded"],
                         allowed_codes=j["allowed_codes"],
                         minutes=round(j["completion_seconds"] / 60, 1),
                         nll_per_record=round(sc.get("nll_per_record", float("nan")), 3),
                         nll_popweighted=round(sc.get("nll_per_record_popweighted") or float("nan"), 3),
                         gpu=j["gpu"]))
    if crow:
        lines += ["## Completions", "", table(pd.DataFrame(crow)), ""]
    txt = "\n".join(lines)
    (d / "h_summary.md").write_text(txt)
    print(txt)


if __name__ == "__main__":
    main()
