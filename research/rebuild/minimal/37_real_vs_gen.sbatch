#!/bin/bash -l
#SBATCH --job-name=s37real
#SBATCH --partition=day,week
#SBATCH --time=03:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step37/real_vs_gen_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step37/real_vs_gen_%j.err
# Step 37 (37_real_vs_gen.py): the real PUF and the generated files tilted to the same 2019
# targets (136 IRS + SOI business in six AGI groups, filers). Needs 36_business_targets.py first.
#   sbatch research/rebuild/minimal/37_real_vs_gen.sbatch            (runs D0,D0s2,D0s3)
#   sbatch --export=ALL,GEN_RUNS=Ns1 research/rebuild/minimal/37_real_vs_gen.sbatch
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
source research/rebuild/minimal/34_env.sh
export WEALTH_UNITS=all WEALTH_COUNT_BASE=scf_units MINIMAL_NONFILERS=1
export OMP_NUM_THREADS=4 OPENBLAS_NUM_THREADS=4 MKL_NUM_THREADS=4
python research/rebuild/minimal/37_real_vs_gen.py
echo "=== done $(date)"
