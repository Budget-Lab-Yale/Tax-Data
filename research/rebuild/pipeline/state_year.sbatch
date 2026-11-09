#!/bin/bash -l
# One step of one year of the file with state weights. run_state_weights.sh submits it with each step's resources and dependencies; a step that failed can be
# resubmitted by hand with the same sbatch line (the command prints them to <output folder>/jobs.txt).
#   sbatch --array=<years> --export=ALL,STATE_TRAIN=...,STATE_OUT=...,... research/rebuild/pipeline/state_year.sbatch <step>
# Steps, each run through run.sbatch's stages (a worktree runs its own code):
#   prep        a past year's folder <output>/run/<year>: links to the training data and networks, the year's settings, targets and dollar maps
#   draw        a past year's draw and the top step's redraw (a GPU)
#   national    a past year's national-only solve (tilt.py), whose amounts set the state targets; the solve's inputs are saved for the next steps
#   start       each record's chances of each state from the networks (a GPU)
#   integrated  the integrated solve of national and state weights (state_joint.py); fails only if the year stops beyond 10 times the stopping size (state_joint.NEAR)
#   separate    the fallback: the state shares tilted on the national-only file's weights (state_weights.py --final)
#   future      a future year: the national solve, starting from the 2023 file (2024) or the 2024 file (later years), then each record's 2023 shares
#   check       every year's file against its weights (state_weights.py --check)
# Folders under <output>: final/<year>/tilt/y<year>/file_<year>.parquet is each year's final file, the one the next years and Tax-Simulator's file read, a past year's other files
# being links to the national-only folder run/<year>/tilt/y<year>; files/state_weights_<year>.parquet is its split across states; checks/, shares/, integrated/y<year>/ (fits).
set -euo pipefail
umask 027
STEP=${1:?a step}
Y=${SLURM_ARRAY_TASK_ID:-}
: "${STATE_TRAIN:?}" "${STATE_OUT:?}"
cd "${REBUILD_ROOT:-$SLURM_SUBMIT_DIR}"
P=research/rebuild/pipeline
R=$STATE_OUT/run/$Y
F=$STATE_OUT/final/$Y
echo "state_year $STEP $Y on $(hostname); code $(git rev-parse --short HEAD); $(git status --short research/rebuild/pipeline | wc -l) changed files in the pipeline"

year_settings() {  # the training settings with the target year
  python3 - "$STATE_TRAIN/settings.json" "$1" "$Y" <<'PY'
import json, sys
s = json.load(open(sys.argv[1]))
s['years']['target'] = int(sys.argv[3])
assert s['generation'].get('filers', 'synthetic') == 'synthetic', 'this command builds the synthetic file'
if int(sys.argv[3]) > 2023:
    assert s['tilt']['projection']['filing_season']['enabled'] is True
json.dump(s, open(sys.argv[2], 'w'), indent=1)
PY
}
year_env() {  # the stages' environment for the folder $1
  export REBUILD_OUT=$1 REBUILD_SETTINGS=$1/settings.json REBUILD_RUN=y$Y PYTHONDONTWRITEBYTECODE=1 SOLVER_DEBUG=1
}
past_final() {  # a past year's final folder: links to the national-only folder
  mkdir -p "$F/tilt/y$Y" "$STATE_OUT/files" "$STATE_OUT/checks"
  for d in data targets model lib gen settings.json; do ln -sfn "$R/$d" "$F/$d"; done
  for f in "$R/tilt/y$Y"/*; do ln -sfn "$f" "$F/tilt/y$Y/${f##*/}"; done
}

case $STEP in
  prep)
    test ! -e "$R/gen/y$Y"
    mkdir -p "$R/data/maps" "$R/targets" "$R/gen" "$R/tilt"
    for f in "$STATE_TRAIN"/data/*; do b=${f##*/}; [ "$b" = maps ] && continue; ln -sfn "$f" "$R/data/$b"; done
    for f in "$STATE_SIDE"/data/*_all.parquet; do ln -sfn "$f" "$R/data/${f##*/}"; done
    ln -sfn "$STATE_TRAIN/model" "$R/model"
    ln -sfn "${STATE_SOI_CV//\{year\}/$Y}" "$R/targets/soi_cv"
    ln -sfn "$STATE_LIB" "$R/lib"
    year_settings "$R/settings.json"
    year_env "$R"
    bash $P/run.sbatch targets maps ;;
  draw)
    test ! -e "$R/top_cache.pkl"
    year_env "$R"
    export REBUILD_TOP_CACHE=$R/top_cache.pkl
    bash $P/run.sbatch generate top ;;
  national)
    test -s "$R/top_cache.pkl"
    test ! -e "$R/tilt/y$Y/file_$Y.parquet"
    year_env "$R"
    export REBUILD_TOP_CACHE=$R/top_cache.pkl REBUILD_SAVE_SOLVE=$R/solve_inputs.pkl
    bash $P/run.sbatch tilt ;;
  start)
    test -s "$R/tilt/y$Y/file_$Y.parquet"
    export REBUILD_OUT=$STATE_TRAIN REBUILD_SETTINGS=$STATE_TRAIN/settings.json REBUILD_RUN=y$Y PYTHONDONTWRITEBYTECODE=1
    mkdir -p "$STATE_OUT/starts"
    STATE_RUN=$STATE_OUT/run STATE_YEARS=$Y bash $P/run.sbatch state_start ;;
  integrated)
    year_env "$R"
    rc=0
    STATE_RUN=$STATE_OUT/run bash $P/run.sbatch state_joint || rc=$?
    I=$STATE_OUT/integrated/y$Y
    if [ -s "$I/check.json" ]; then
      past_final
      ln -sfn "$I/tilt/y$Y/file_$Y.parquet" "$F/tilt/y$Y/file_$Y.parquet"
      ln -sfn "$I/fit.csv" "$F/tilt/y$Y/fit.csv"
      ln -sfn "$I/state_weights_$Y.parquet" "$STATE_OUT/files/state_weights_$Y.parquet"
      ln -sfn "$I/check.json" "$STATE_OUT/checks/check_$Y.json"
    fi
    exit $rc ;;
  separate)
    past_final
    year_env "$R"
    STATE_RUN=$STATE_OUT/final bash $P/run.sbatch state_tilt ;;
  future)
    B=$STATE_OUT/final
    test -s "$B/2023/tilt/y2023/file_2023.parquet"
    test ! -e "$F/tilt/y$Y"
    mkdir -p "$F/tilt"
    for d in data targets model lib; do ln -sfn "$B/2023/$d" "$F/$d"; done
    ln -sfn "$B/2023/tilt/y2023" "$F/tilt/y2023"
    if [ "$Y" -gt 2024 ]; then test -s "$B/2024/tilt/y2024/file_2024.parquet"; ln -sfn "$B/2024/tilt/y2024" "$F/tilt/y2024"; fi
    year_settings "$F/settings.json"
    year_env "$F"
    STATE_RUN=$B bash $P/run.sbatch tilt state_carry ;;
  check)
    export REBUILD_OUT=$STATE_TRAIN REBUILD_SETTINGS=$STATE_TRAIN/settings.json PYTHONDONTWRITEBYTECODE=1
    STATE_RUN=$STATE_OUT/final STATE_CHECK_YEARS=$(cat "$STATE_OUT/years.txt") bash $P/run.sbatch state_check ;;
  *) echo "unknown step $STEP"; exit 1 ;;
esac
