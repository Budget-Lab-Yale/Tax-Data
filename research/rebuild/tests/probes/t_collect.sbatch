#!/bin/bash
# Probe T: levels table for every finished variant (t_compare.py) and the
# validation losses and wealth completion scores from the run files.
# Usage: sbatch t_collect.sbatch [name [LABEL=PATH ...]]   (writes $Q/compare_<name>.json)
#SBATCH --job-name=t_collect
#SBATCH --partition=day
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --time=01:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/t_collect_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/t_collect_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
cd "$HERE"
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/t
NAME=${1:-all}
ITEMS=("demoglast_base_s1=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/main/h_gen_puf_demoglast_seed01.parquet")
for f in $Q/*/h_gen_*_seed0?_seed0?.parquet $Q/*/h_gen_*_seed0?_pufbest_seed0?.parquet; do
  [ -f "$f" ] || continue
  v=$(basename $(dirname $f)); b=$(basename $f .parquet); lab=${b#h_gen_}; lab=${lab%_seed0?}
  opt=""
  case $v in tilt*) opt=":tilt=0.9:3,0.99:10,0.999:30";; esac
  ITEMS+=("$lab=$f$opt")
  [ -n "$opt" ] && ITEMS+=("${lab}_unweighted=$f")
done
ITEMS+=("${@:2}")
python -u t_compare.py --out $Q/compare_$NAME.json "${ITEMS[@]}"
python - <<EOF
import json, glob
for f in sorted(glob.glob("$Q/*/run_*.json")):
    r = json.load(open(f))
    print(r["tag"], "epochs run", r["epochs_run"], "| kept (all) epoch", r.get("best_all_epoch"),
          "val all", round(r.get("best_all_val_loss", float("nan")), 4),
          "| PUF-best epoch", r.get("best_puf_epoch"), "PUF val (pop)", round(r.get("best_puf_val_pop", float("nan")), 4),
          "| wealth completion: kept", round(r["val_completion"].get("nll_per_record", float("nan")), 3),
          "epoch", r["val_completion"].get("epoch"),
          "PUF-best", round(r.get("val_completion_pufbest", {}).get("nll_per_record", float("nan")), 3),
          "| minutes", round(r["train_seconds"] / 60, 1), r["gpu"])
EOF
