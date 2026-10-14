#!/bin/bash
# Part B finishing step: pick the training run with the lowest validation
# loss among run_{small,base,large}.json, plot the training curves, generate
# synthetic.parquet from the best checkpoint, and create $S/model/DONE.
# Submit with a dependency on the training jobs:
#   sbatch --dependency=afterany:<job1>:<job2>:<job3> finish.sbatch
#SBATCH --job-name=pufgen_finish
#SBATCH --partition=gpu
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=02:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/finish_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/finish_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
S=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/puf_generator
SEED=${SEED:-20261009}
cd "$CODE"
nvidia-smi --query-gpu=name --format=csv
BEST=$(python - <<'PY'
import json, glob, os
runs = []
for f in glob.glob("/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/model/run_*.json"):
    r = json.load(open(f))
    print(f"{r['tag']}: params {r['n_params']}, best epoch {r['best_epoch']} of {r['epochs_run']}, "
          f"val loss {r['best_val_loss']:.5f}, sequential NLL/record {r['val_seq_nll_per_record']:.3f}, "
          f"train {r['train_seconds']/60:.1f} min", file=__import__('sys').stderr)
    runs.append((r["best_val_loss"], r["tag"]))
if not runs:
    raise SystemExit("no finished runs")
print(min(runs)[1])
PY
)
echo "best run: $BEST"
cp "$S/model/train_log_$BEST.csv" "$S/model/train_log.csv"
echo "$BEST" > "$S/model/best_run.txt"
python -u plot_training.py --model-dir "$S/model" --tags small base large
python -u generate.py --data-dir "$S/data" --out-dir "$S/model" \
    --checkpoint "$S/model/checkpoint_$BEST.pt" --seed "$SEED"
test -s "$S/model/synthetic.parquet"
touch "$S/model/DONE"
echo "DONE marker written $(date)"
