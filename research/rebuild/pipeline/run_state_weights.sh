#!/bin/bash
# The file with state weights, every year, one command, from the repository root (FEATURES.md, "State weights"):
#   research/rebuild/pipeline/run_state_weights.sh <training folder> <output folder> [--separate] [--past <years>] [--last-year <year>] [--side <folder>]
# <training folder> holds settings.json, data/ and model/ of the networks with the state slot, read only. <output folder> is created and must not hold a run.
# It submits, with Slurm dependencies, one array per step (state_year.sbatch):
#   for each past year 2015-2023: prep, draw (GPU), national-only solve, the networks' chances of each state (GPU), the integrated solve;
#   then 2024 on the 2023 file, once 2023's integrated solve has succeeded; 2025-2075 on the 2024 file: the national solve, each record keeping its 2023 shares;
#   then the check of every year, which runs whatever happened and stops on what failed.
# Each year's steps wait only for that year's earlier steps, so a failed year stops itself and, if it is 2023, the future years.
# --separate: the fallback, not the default. National weights stay at the national-only solve's and the state shares are tilted on them (state_weights.py --final).
# --past 2019,2023 or --last-year 2025: fewer years, for a test (2023 must be among the past years, unless --last-year is 2023 or earlier).
# --side: the folder of every survey year's side files, <side>/data/*_all.parquet (default <training folder>/../sidebuild).
set -euo pipefail
TRAIN=${1:?the training folder}; OUT=${2:?the output folder}; shift 2
STEP_SOLVE=integrated; PAST=2015-2023; LAST=2075; SIDE=$(dirname "$TRAIN")/sidebuild
while [ $# -gt 0 ]; do
  case $1 in
    --separate) STEP_SOLVE=separate; shift ;;
    --past) PAST=$2; shift 2 ;;
    --last-year) LAST=$2; shift 2 ;;
    --side) SIDE=$2; shift 2 ;;
    *) echo "unknown option $1"; exit 1 ;;
  esac
done
P=research/rebuild/pipeline
test -d $P || { echo "not a repository root: $(pwd)"; exit 1; }
test -s "$TRAIN/settings.json" && test -d "$TRAIN/data/filing" && test -d "$TRAIN/model" || { echo "no settings, data or networks in $TRAIN"; exit 1; }
ls "$SIDE"/data/*_all.parquet >/dev/null || { echo "no side files in $SIDE/data"; exit 1; }
if [ -e "$OUT/run" ] || [ -e "$OUT/final" ]; then echo "$OUT already holds a run"; exit 1; fi
read -r SOI_CV LIB < <(python3 -c "import json; s = json.load(open('$P/settings.json'))['state_weights']; print(s['soi_cv'], s['lib'])")
PAST_YEARS=(); for p in ${PAST//,/ }; do case $p in *-*) PAST_YEARS+=($(seq "${p%-*}" "${p#*-}")) ;; *) PAST_YEARS+=("$p") ;; esac; done
FUTURE=(); if [ "$LAST" -ge 2024 ]; then
  [[ " ${PAST_YEARS[*]} " == *" 2023 "* ]] || { echo "future years need 2023 among the past years"; exit 1; }
  FUTURE=($(seq 2024 "$LAST"))
fi
mkdir -p "$OUT/logs"
echo "${PAST_YEARS[*]} ${FUTURE[*]:-}" | xargs | tr ' ' ',' > "$OUT/years.txt"    # the years the check expects
ENVS="ALL,STATE_TRAIN=$TRAIN,STATE_OUT=$OUT,STATE_SIDE=$SIDE,STATE_SOI_CV=$SOI_CV,STATE_LIB=$LIB,REBUILD_ROOT=$(pwd)"
GPU="--partition=gpu_h100,gpu_h200 --gres=gpu:1"
CPU="--partition=day --gres=gpu:0"
sub() {  # job name, step, then sbatch's other arguments
  local name=$1 step=$2; shift 2
  echo "sbatch --job-name=$name --export=$ENVS --output=$OUT/logs/%x_%A_%a.log --kill-on-invalid-dep=yes $* $P/state_year.sbatch $step" >> "$OUT/jobs.txt"
  sbatch --parsable --job-name="$name" --export="$ENVS" --output="$OUT/logs/%x_%A_%a.log" --kill-on-invalid-dep=yes "$@" $P/state_year.sbatch "$step"
}
a=$(sub sw_prep prep $CPU --array="$PAST" --cpus-per-task=4 --mem=48G --time=01:30:00)
b=$(sub sw_draw draw $GPU --array="$PAST" --dependency=aftercorr:"$a" --cpus-per-task=4 --mem=120G --time=04:00:00)
c=$(sub sw_national national $CPU --array="$PAST" --dependency=aftercorr:"$b" --cpus-per-task=4 --mem=72G --time=04:00:00)
d=$(sub sw_start start $GPU --array="$PAST" --dependency=aftercorr:"$c" --cpus-per-task=4 --mem=64G --time=01:00:00)
e=$(sub sw_$STEP_SOLVE $STEP_SOLVE $CPU --array="$PAST" --dependency=aftercorr:"$d" --cpus-per-task=16 --mem=96G --time=06:00:00)
ALL=$e; f=none; g=none
if [ "$LAST" -ge 2024 ]; then
  f=$(sub sw_2024 future $CPU --array=2024 --dependency=afterok:"${e}_2023" --cpus-per-task=4 --mem=64G --time=02:00:00)
  ALL=$ALL:$f
fi
if [ "$LAST" -ge 2025 ]; then
  g=$(sub sw_future future $CPU --array=2025-"$LAST" --dependency=afterok:"$f" --cpus-per-task=4 --mem=64G --time=02:00:00)
  ALL=$ALL:$g
fi
h=$(sub sw_check check $CPU --dependency=afterany:"$ALL" --cpus-per-task=1 --mem=32G --time=01:00:00)
echo "prep $a, draw $b, national $c, chances $d, $STEP_SOLVE $e, 2024 $f, later years $g, check $h; logs in $OUT/logs, the sbatch lines in $OUT/jobs.txt"
