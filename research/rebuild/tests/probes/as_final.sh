#!/bin/bash
# Probe AS: final comparison over every generated file present. Run via as_cpu.sbatch-like job:
#   sbatch --wrap is not used; call: sbatch as_cpu_sh.sbatch as_final.sh
set -euo pipefail
G=/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/gen
O=/nfs/roberts/scratch/pi_nrs36/jar335/probes/o/gen
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/compare_final.json
ITEMS=()
for m in 1 2 3; do ITEMS+=("test3_random_m$m=$O/h_random_s1_seed0$m.parquet"); done
for m in 1 2 3; do ITEMS+=("test3_agifirst_m$m=$O/h_agifirst_s1_seed0$m.parquet"); done
for o in random sizefirst; do
  for m in 1 2 3 4; do f=$G/h_${o}_s1_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("sum_${o}_m$m=$f"); done
  for m in 1 2 3 4; do for ss in 1 2 3; do f=$G/h_${o}_s${ss}_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("sum_${o}_alldraws_m$m$ss=$f"); done; done
done
for m in 1 2; do f=$G/h_mix25_sizefirst_s1_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("sum_mix25_sizefirst_m$m=$f"); for ss in 1 2 3; do f=$G/h_mix25_sizefirst_s${ss}_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("sum_mix25_sizefirst_alldraws_m$m$ss=$f"); done; done
python -u as_compare.py --out "$OUT" "${ITEMS[@]}" > /dev/null
python -u as_table.py "$OUT"
