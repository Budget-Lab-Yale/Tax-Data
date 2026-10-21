#!/bin/bash -l
#SBATCH --job-name=s27cpu
#SBATCH --partition=day,week
#SBATCH --time=01:30:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step27/cpu_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step27/cpu_%x_%j.err
# Step 27, CPU steps: "tables" (27_tables.py: table E27, maps map/V27, checks),
# "fake" (27_fake.py tables: the fake survey table F27), "report"
# (27_report.py: the report tables, step27/report.md).
#   sbatch -J tables research/rebuild/minimal/27_cpu.sbatch tables
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
D=research/rebuild/minimal
case "${1:?tables, fake or report}" in
  tables) python $D/27_tables.py ;;
  fake)   python $D/27_fake.py tables ;;
  fakecheck) export MINIMAL_TABLE=F27 MINIMAL_RUN=step27/${2:-F27s1}; python $D/27_fake.py check ;;
  summary) python $D/27_summary.py ;;
  report) export MINIMAL_KG_PIECES=1 MINIMAL_MAP_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/map/V27
          python $D/27_report.py && python $D/27_summary.py ;;
esac
echo "=== done $(date)"
