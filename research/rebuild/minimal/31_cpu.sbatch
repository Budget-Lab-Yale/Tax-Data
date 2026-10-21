#!/bin/bash -l
#SBATCH --job-name=s31cpu
#SBATCH --partition=day,week
#SBATCH --time=01:30:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step31/cpu_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step31/cpu_%x_%j.err
# Step 31: the report tables (31_report.py), written to step31/report.md.
#   sbatch -J report31 research/rebuild/minimal/31_cpu.sbatch          (report)
#   sbatch -J combos31 research/rebuild/minimal/31_cpu.sbatch combos   (31_combos.py)
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export MINIMAL_KG_PIECES=1 MINIMAL_MAP_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/map/V27
case "${1:-report}" in
  report) python research/rebuild/minimal/31_report.py ;;
  combos) python research/rebuild/minimal/31_combos.py ;;
esac
echo "=== done $(date)"
