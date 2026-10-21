#!/bin/bash -l
#SBATCH --job-name=s33cpu
#SBATCH --partition=day,week
#SBATCH --time=01:30:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step33/cpu_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step33/cpu_%x_%j.err
# Step 33 (33_STATUS.md), CPU steps:
#   sbatch -J tables33 research/rebuild/minimal/33_cpu.sbatch tables   (33_tables.py: table E33, maps map/V33)
#   sbatch -J report33 research/rebuild/minimal/33_cpu.sbatch report   (33_report.py: step33/report.md)
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
source research/rebuild/minimal/33_env.sh
case "${1:?tables or report}" in
  tables) [ -f $R/step33/TABLES_DONE ] || python research/rebuild/minimal/33_tables.py ;;
  report) python research/rebuild/minimal/33_report.py ;;
esac
echo "=== done $(date)"
