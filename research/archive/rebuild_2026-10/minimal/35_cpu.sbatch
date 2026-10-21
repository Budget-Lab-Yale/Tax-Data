#!/bin/bash -l
#SBATCH --job-name=s35cpu
#SBATCH --partition=day,week
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=48G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step35/%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step35/%x_%j.err
# Step 35 (35_BUSINESS_INCOME.md): business income, PUF against SCF. Describes; changes nothing.
#   sbatch -J scfraw35 research/rebuild/minimal/35_cpu.sbatch scfraw   (35_scf_raw.R)
#   sbatch -J tab35    research/rebuild/minimal/35_cpu.sbatch tables   (35_tables.py)
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
case "${1:?mode}" in
  scfraw) module load R/4.4.1-foss-2022b
          Rscript research/rebuild/minimal/35_scf_raw.R ;;
  tables) module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
          python research/rebuild/minimal/35_tables.py ;;
  py)     module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
          python "${2:?script}" ;;
esac
echo "=== done $(date)"
