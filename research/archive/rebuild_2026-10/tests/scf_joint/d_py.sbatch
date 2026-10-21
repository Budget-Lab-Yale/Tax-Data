#!/bin/bash
# Generic CPU Python runner for piece D: sbatch d_py.sbatch <script.py> [args...]
#SBATCH -J d_py -p day -t 04:00:00 --cpus-per-task=16 --mem=64G
#SBATCH -o /nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/d_py_%j.out
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
echo "start $(date) host $(hostname) args: $@"
python -u "$@"
echo "exit $? at $(date)"
