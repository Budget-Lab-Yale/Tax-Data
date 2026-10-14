#!/bin/bash
# Piece D, production-method refit: fit + draw (R), then encode + write (Python).
# sbatch d_drf.sbatch <dataset> <seed> [draws=5] [boot_scale=1] [num_trees=500]
# Needs $S/comp/<dataset>/drf_input/ from d_drf_prep.py (see d_submit.sh).
#SBATCH -J d_drf -p day -t 06:00:00 --cpus-per-task=16 --mem=32G
#SBATCH -o /nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/d_drf_%j.out
set -e
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
echo "start $(date) host $(hostname) args: $@"
module load R/4.4.1-foss-2022b
Rscript d_drf.R "$@"
module purge
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
if [ "${D_MODE:-}" = "sw" ]; then m=drfsw; else m=drf; fi
python -u d_drf_finish.py --dataset "$1" --seed "$2" --method $m
echo "exit $? at $(date)"
