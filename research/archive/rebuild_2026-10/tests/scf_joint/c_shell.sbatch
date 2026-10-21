#!/bin/bash
# Part C: run one shell script from this folder on a CPU node, with the
# Python modules loaded.  Usage: sbatch c_shell.sbatch <script.sh> [args ...]
#SBATCH --job-name=c_sh
#SBATCH --partition=day
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=03:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/c_sh_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/c_sh_%j.err
set -uo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-4}
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
cd /nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
script=$1; shift
bash "$CODE/$script" "$@"
