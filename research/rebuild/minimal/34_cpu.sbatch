#!/bin/bash -l
#SBATCH --job-name=s34cpu
#SBATCH --partition=day,week
#SBATCH --time=01:30:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step34/cpu_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step34/cpu_%x_%j.err
# Step 34 (34_STATUS.md), CPU steps:
#   sbatch -J tables34 research/rebuild/minimal/34_cpu.sbatch tables   (34_tables.py: table E34, maps map/V34)
#   sbatch -J report34 research/rebuild/minimal/34_cpu.sbatch report   (34_report.py: step34/report.md)
#   sbatch -J short34 research/rebuild/minimal/34_cpu.sbatch shortfall (34_shortfall.py: step34/shortfall.md)
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
source research/rebuild/minimal/34_env.sh
case "${1:?tables, report or shortfall}" in
  tables) [ -f $R/step34/TABLES_DONE ] || python research/rebuild/minimal/34_tables.py ;;
  report) python research/rebuild/minimal/34_report.py ;;
  shortfall) python research/rebuild/minimal/34_shortfall.py ;;
esac
echo "=== done $(date)"
