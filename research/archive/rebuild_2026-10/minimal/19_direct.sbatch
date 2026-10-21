#!/bin/bash
#SBATCH --job-name=d19
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --time=01:30:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/19_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/19_%x_%j.err
# Step 19 (19_DIRECT.md): post-training by adjusting the network's parameters.
#   sbatch -J d19test  research/rebuild/minimal/19_direct.sbatch test
#   sbatch -J d19pre   research/rebuild/minimal/19_direct.sbatch pretrain
#   sbatch -J d19all   --export=ALL,D19_PARAMS=all  research/rebuild/minimal/19_direct.sbatch train all
#   sbatch -J d19last  --export=ALL,D19_PARAMS=last research/rebuild/minimal/19_direct.sbatch train last
# Settings D19_* come in through the environment (19_direct.py).
# Touches markers/STEP19_<what>[_<tag>]_DONE, or writes ..._FAILED.
set -uo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
WHAT=${1:?test, pretrain or train}
TAG=${2:-}
NAME=STEP19_${WHAT}${TAG:+_$TAG}
mkdir -p $R/markers
rm -f $R/markers/${NAME}_DONE $R/markers/${NAME}_FAILED
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
export MINIMAL_TABLE=E4 MINIMAL_RUN=V4 MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V4
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
echo "step 19 $WHAT $TAG $(date)"; env | grep '^D19_' | sort
nvidia-smi --query-gpu=name --format=csv,noheader
python research/rebuild/minimal/19_direct.py $WHAT $TAG
rc=$?
if [ $rc -ne 0 ]; then
  echo "19 $WHAT $TAG failed (exit $rc); log $R/logs/19_${SLURM_JOB_NAME}_${SLURM_JOB_ID}.out" > $R/markers/${NAME}_FAILED
  exit $rc
fi
touch $R/markers/${NAME}_DONE
echo "done $(date)"
