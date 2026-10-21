#!/bin/bash -l
#SBATCH --job-name=min30
#SBATCH --partition=day
#SBATCH --time=00:30:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/30_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/30_%x_%j.err
# Step 30: the mapped SCF variables against the PUF.
#   sbatch research/rebuild/minimal/30_run.sbatch compare   (30_compare.py: checks/step30/compare.md)
#   sbatch research/rebuild/minimal/30_run.sbatch process   (30_process.py: checks/step30/process.md,
#                                                            fills_2016.parquet; model step29/C4s1, CPU)
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
D=research/rebuild/minimal
case "${1:?compare}" in
  compare) python $D/30_compare.py ;;
  process) MINIMAL_TABLE=E27 python $D/30_process.py ;;
esac
echo "=== done $(date)"
