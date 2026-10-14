#!/bin/bash
# Probe S: compare every finished generated or completed file with the PUF
# (t_compare.py, one json per file, skipped if already done), then print the
# summary tables (s_summary.py).
#SBATCH --job-name=s_collect
#SBATCH --partition=day
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=01:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/s_collect_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/s_collect_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
cd "$HERE"
T=/nfs/roberts/scratch/pi_nrs36/jar335/probes/t
S=/nfs/roberts/scratch/pi_nrs36/jar335/probes/s
C=$S/cmp; mkdir -p $C
ITEMS=()
for s in 1 2 3; do
  NN=0$s
  [ $s = 1 ] && A=$T/base/h_agifirst4x_base_seed01.parquet || A=$T/base/h_agifirst4x_base_s${s}_seed$NN.parquet
  [ $s = 1 ] && D=$T/base/h_demolast4x_base_seed01.parquet || D=$S/tprev/h_demolast4x_base_s${s}_seed$NN.parquet
  [ $s = 1 ] && G=$T/base/h_givenagi_base_seed01.parquet || G=$T/base/h_givenagi_base_s${s}_seed$NN.parquet
  ITEMS+=(base_s${s}_agifirst=$A base_s${s}_demolast=$D base_s${s}_givenagi=$G)
  ITEMS+=(agi50_s${s}_agifirst=$T/mixagi60/h_agifirst4x_mixagi60_s${s}_seed$NN.parquet
          agi50_s${s}_demolast=$S/tprev/h_demolast4x_agi50_s${s}_seed$NN.parquet
          agi50_s${s}_givenagi=$T/mixagi60/h_givenagi_mixagi60_s${s}_seed$NN.parquet)
done
ITEMS+=(sched_s1_agifirst=$T/sched/h_agifirst4x_sched_kept_seed01.parquet
        sched_s1_givenagi=$T/sched/h_givenagi_sched_kept_seed01.parquet)
for v in agi10 agi25 agi50 agi100 rnd50 rnd100 two50; do
  for s in 1 2 3 4 5; do
    for o in agifirst demolast; do ITEMS+=(${v}_s${s}_$o=$S/$v/h_${o}4x_${v}_s${s}_seed0$s.parquet); done
    ITEMS+=(${v}_s${s}_givenagi=$S/$v/h_givenagi_${v}_s${s}_seed0$s.parquet)
  done
done
for p in $S/noise/h_*.parquet; do [ -f "$p" ] || continue; b=$(basename $p .parquet); ITEMS+=(noise_${b#h_}=$p); done
for it in "${ITEMS[@]}"; do
  lab=${it%%=*}; p=${it#*=}
  [ -f "$p" ] || continue
  [ -f "$C/$lab.json" ] && [ "$C/$lab.json" -nt "$p" ] && continue
  python -u t_compare.py --out $C/$lab.json "$it" > /dev/null && echo "compared $lab"
done
python - <<PY
import json, glob
r = {}
for p in sorted(glob.glob("$C/*.json")):
    r.update(json.load(open(p)))
json.dump(r, open("$S/compare_all.json", "w"), indent=1)
PY
python s_summary.py $S/compare_all.json
