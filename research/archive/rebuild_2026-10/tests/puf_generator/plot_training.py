"""Plot training and validation loss per epoch for each model size.

  python plot_training.py --model-dir $S/model --tags small base large
Writes <model-dir>/training_curves.png.
"""

import argparse
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd

p = argparse.ArgumentParser()
p.add_argument("--model-dir", required=True)
p.add_argument("--tags", nargs="+", required=True)
a = p.parse_args()
d = Path(a.model_dir)

fig, ax = plt.subplots(figsize=(8, 5))
colors = ["#1f77b4", "#d62728", "#2ca02c", "#9467bd", "#ff7f0e"]
for tag, c in zip(a.tags, colors):
    f = d / f"train_log_{tag}.csv"
    if not f.exists():
        continue
    df = pd.read_csv(f)
    ax.plot(df.epoch, df.train_loss, color=c, ls="--", lw=1, label=f"{tag} train")
    ax.plot(df.epoch, df.val_loss, color=c, lw=1.5, label=f"{tag} validation")
ax.set_xlabel("epoch (one pass over the training records)")
ax.set_ylabel("cross-entropy per hidden variable (nats)")
ax.set_ylim(top=1.6)
ax.legend(frameon=False, ncol=2)
ax.set_title("Masked training loss by model size")
fig.tight_layout()
fig.savefig(d / "training_curves.png", dpi=130)
print("wrote", d / "training_curves.png")
