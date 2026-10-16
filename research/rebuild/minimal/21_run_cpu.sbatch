#!/bin/bash
#SBATCH --job-name=min21c
#SBATCH --partition=day,scavenge,week
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/21_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/21_%x_%j.err
# Step 21 (21_PRIORS.md): the CPU steps.
#   sbatch -J maps  research/rebuild/minimal/21_run_cpu.sbatch maps
#   sbatch -J <name> research/rebuild/minimal/21_run_cpu.sbatch prior <name> <run> <table> <t>:<pi>,...
#       builds the prior's starting file (21_priors.py), then tilts it to the
#       v4c targets (07_baseline.py: 136 IRS targets, eight wealth targets on
#       the SCF's own units), net worth dollars from each record's own wave.
#   sbatch -J rep21 research/rebuild/minimal/21_run_cpu.sbatch report   (21_report.py)
#   sbatch -J v4ct research/rebuild/minimal/21_run_cpu.sbatch v4ctarget
#       v4c's stack, net worth dollars from the 2019 survey for every record.
#   Third round: pass --export=ALL,MINIMAL_NW_TARGET_MAP_YEAR=2018 to a prior
#   step to turn every net worth rank into 2019-survey dollars.
#   sbatch -J v4cnw research/rebuild/minimal/21_run_cpu.sbatch v4ccheck
#       the v4c stack tilted again through the per-wave net worth path; must
#       reproduce the move of tilt/v4c_scfbase/ to 1e-5 nats (one SCF row sits
#       on a cut point, so the weights differ in the fourth digit at most).
# Touches markers/STEP21_<what>_DONE.
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
WHAT=${1:?maps, prior or v4ccheck}
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
D=research/rebuild/minimal
export MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V5 MINIMAL_NW_MAP_YEARS=2015,2018,2021
export WEALTH_UNITS=all WEALTH_COUNT_BASE=scf_units
case $WHAT in
  maps) python $D/21_maps.py; TAG=maps ;;
  report) python $D/21_report.py; TAG=report_${REPORT_SET:-record} ;;
  prior)
    NAME=${2:?name}; RUN=${3:?run}; TAB=${4:?table}; SPEC=${5:?prior}; MODE=${6:-}
    export MINIMAL_RUN=$RUN MINIMAL_TABLE=$TAB STACK_VERSION=$NAME TILT_TAG=$NAME
    python $D/21_priors.py $NAME $RUN $SPEC $MODE
    python $D/07_baseline.py; TAG=$NAME ;;
  v4ctarget)
    # v4c's stack with every net worth rank turned into dollars by the 2019
    # survey's map (MINIMAL_NW_TARGET_MAP_YEAR=2018, third round)
    export MINIMAL_RUN=V4 MINIMAL_TABLE=E4 STACK_VERSION=v4c TILT_TAG=v4c_target2019 MINIMAL_NW_TARGET_MAP_YEAR=2018
    python $D/07_baseline.py; TAG=v4ctarget ;;
  v4ccheck)
    export MINIMAL_RUN=V4 MINIMAL_TABLE=E4 STACK_VERSION=v4c TILT_TAG=v4c_nwcheck
    python $D/07_baseline.py
    python - <<'PY'
import pandas as pd
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/tilt'
a = pd.read_csv(f'{R}/v4c_scfbase/summary_v4c.csv')
b = pd.read_csv(f'{R}/v4c_nwcheck/summary_v4c.csv')
d = float((a['kl_nats'] - b['kl_nats']).abs().max())
print('largest difference in the move against tilt/v4c_scfbase (nats):', d)
assert d < 1e-5   # 2026-10-16 run: 9e-8 (one SCF row sits on a cut point)
PY
    TAG=v4ccheck ;;
esac
touch $R/markers/STEP21_${TAG}_DONE
echo "done $(date)"
