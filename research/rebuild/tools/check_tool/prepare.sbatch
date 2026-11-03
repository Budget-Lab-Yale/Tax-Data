#!/bin/bash -l
#SBATCH --job-name=check_tool_prepare
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=48G
#SBATCH --time=01:00:00
# Builds the check tool's tables of records for the year in settings.json (prepare.py). Submit from anywhere:
#   sbatch research/rebuild/tools/check_tool/prepare.sbatch
# It runs with the pipeline's own modules, because it runs the pipeline's own code. The server does not need them.
set -euo pipefail
HERE=${CHECK_TOOL_DIR:-$(dirname "$(scontrol show job "$SLURM_JOB_ID" | awk -F= '/Command=/{print $2}')")}
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
python "$HERE/prepare.py"
