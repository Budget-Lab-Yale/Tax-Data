#!/bin/bash -l
#SBATCH --job-name=check_tool
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --time=08:00:00
#SBATCH --output=check_tool_%j.out
# Starts the check tool's server (README.md). From the folder you want the job's output in (your home folder, say):
#   sbatch /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data-tabtool/research/rebuild/tools/check_tool/start.sbatch
# The output file check_tool_<job id>.out then shows the node, the port, the token and the addresses to open.
# Stop the server with scancel <job id>; it stops by itself after the time above.
set -euo pipefail
# the output holds the token: readable by you alone
OUTFILE=$(scontrol show job "$SLURM_JOB_ID" | awk -F= '/StdOut=/{print $2}')
[ -n "$OUTFILE" ] && chmod 600 "$OUTFILE" || true
HERE=${CHECK_TOOL_DIR:-$(dirname "$(scontrol show job "$SLURM_JOB_ID" | awk -F= '/Command=/{print $2}')")}
module load Python/3.12.3-GCCcore-13.3.0 Arrow/17.0.0-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
exec python -u "$HERE/server.py" "$@"
