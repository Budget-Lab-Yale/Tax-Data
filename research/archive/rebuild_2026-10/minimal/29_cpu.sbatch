#!/bin/bash -l
#SBATCH --job-name=s29cpu
#SBATCH --partition=day,week
#SBATCH --time=01:30:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step29/cpu_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step29/cpu_%x_%j.err
# Step 29, CPU steps: "fake" (29_fake.py tables: the fake survey table F29),
# "report" (29_report.py: the report tables, step29/report.md).
#   sbatch -J fake29 research/rebuild/minimal/29_cpu.sbatch fake
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
D=research/rebuild/minimal
case "${1:?fake or report}" in
  fake)   [ -f /nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/stacked_F29.parquet ] || python $D/29_fake.py tables ;;
  report) export MINIMAL_KG_PIECES=1 MINIMAL_MAP_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/map/V27
          python $D/29_report.py ;;
esac
echo "=== done $(date)"
