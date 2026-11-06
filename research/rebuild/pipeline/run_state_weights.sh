#!/bin/bash
# The supplemental state weights of a finished run, one command, from the repository root (FEATURES.md, "State weights"):
#   research/rebuild/pipeline/run_state_weights.sh <run folder> <output folder> [the training folder of the networks with the state slot]
# <run folder>/<year>/tilt/y<year>/file_<year>.parquet are the run's files, read only. The output folder is created and must not hold
# a file already. The networks: the third argument, else settings state_weights.networks. Four sets of jobs, each after the last:
# 2015-2023 the networks' chances (a GPU), 2015-2023 the tilt and the file (CPU, an array), 2024-2075 carried from 2023 (CPU, an array),
# and the check of every year. On a run with the same record ids only the last three need to run again if the items change.
set -euo pipefail
RUN=${1:?the run folder}; OUT=${2:?the output folder}
P=research/rebuild/pipeline
test -d $P || { echo "not a repository root: $(pwd)"; exit 1; }
TRAIN=${3:-$(python3 -c "import json; print(json.load(open('$P/settings.json'))['state_weights']['networks'])")}
test -s $TRAIN/settings.json || { echo "no networks' settings in $TRAIN"; exit 1; }
if [ -e $OUT/files ]; then echo "$OUT already holds files"; exit 1; fi
mkdir -p $OUT/logs
E="ALL,STATE_RUN=$RUN,STATE_OUT=$OUT,REBUILD_OUT=$TRAIN,REBUILD_SETTINGS=$TRAIN/settings.json"
LOG="--output=$OUT/logs/%x_%A_%a.log"
a=$(sbatch --parsable --job-name=sw_start --export=$E $LOG --cpus-per-task=4 --mem=48G --time=02:00:00 $P/run.sbatch state_start)
b=$(sbatch --parsable --job-name=sw_tilt --export=$E $LOG --dependency=afterok:$a --partition=day --gres=gpu:0 --cpus-per-task=16 --mem=96G --time=03:00:00 --array=2015-2023 $P/run.sbatch state_tilt)
c=$(sbatch --parsable --job-name=sw_carry --export=$E $LOG --dependency=afterok:$b --partition=day --gres=gpu:0 --cpus-per-task=2 --mem=48G --time=01:00:00 --array=2024-2075 $P/run.sbatch state_carry)
d=$(sbatch --parsable --job-name=sw_check --export=$E $LOG --dependency=afterok:$c --partition=day --gres=gpu:0 --cpus-per-task=1 --mem=8G --time=00:20:00 $P/run.sbatch state_check)
echo "chances $a, tilts $b, carried years $c, check $d; logs in $OUT/logs"
