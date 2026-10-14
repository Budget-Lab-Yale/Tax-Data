#!/bin/bash
# Probe C: collector. Builds every table for C_REPORT.md from whatever files
# exist, into $Q/C_TABLES.md. Run by c3_collect.sbatch (CPU).
set -uo pipefail
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3
G=$Q/gen
AS=/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/gen
O=/nfs/roberts/scratch/pi_nrs36/jar335/probes/o/gen
ITEMS=()
for m in 1 2 3; do f=$O/h_random_s1_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("test3_random_m$m=$f"); done
for m in 1 2 3 4; do for ss in 1 2; do f=$AS/h_random_s${ss}_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("AS_random_m$m$ss=$f"); done; done
for m in 1 2 3; do for ss in 1 2; do f=$G/h_cz_random_s${ss}_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("Czero_random_m$m$ss=$f"); done; done
for m in 1 2; do for ss in 1 2; do f=$G/h_czw_random_s${ss}_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("CzeroW_random_m$m$ss=$f"); done; done
for m in 1 2 3; do f=$G/h_c_random_s1_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("Cunknown_random_m$m=$f"); done
for m in 1 2 3; do f=$G/h_c_random_fded_s1_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("Cunknown_fdedfirst_m$m=$f"); done
OUT=$Q/C_TABLES.md
{
  echo "# Probe C tables (written $(date) by c3_collect.sh)"
  echo
  echo "Generated files used: ${#ITEMS[@]}"
  printf '%s\n' "${ITEMS[@]}" | sed 's/=.*\/\(h_.*\)/: \1/' | sed 's/^/- /'
  echo
  echo "## Whole PUF records generated from scratch (c3_compare.py, c3_table.py)"
  echo
  echo "Columns: arm (number of files: model seeds x sampling seeds); mean, range in brackets."
  echo "Czero = combined model, itemized deductions zero for non-itemizers (full_v3cz); CzeroW = the same with"
  echo "the PUF weight as a model variable (full_v3czw); Cunknown = deductions unknown for non-itemizers"
  echo "(full_v3c), plain random order, and _fdedfirst with the deductions drawn after the form of deduction;"
  echo "AS = probe AS's model (random order); test3 = version 2 (random order, probe O's files)."
  echo
} > $OUT
python -u c3_compare.py --out $Q/compare_final.json "${ITEMS[@]}" > $Q/compare_final.log 2>&1 && \
  python -u c3_table.py $Q/compare_final.json >> $OUT || echo "generation tables FAILED (see $Q/compare_final.log)" >> $OUT
echo >> $OUT
python -u c3_wealth.py > $Q/wealth.log 2>&1 || echo "c3_wealth.py FAILED (see $Q/wealth.log)" >> $OUT
python -u c3_summary.py >> $OUT 2>> $Q/wealth.log || echo "c3_summary.py FAILED" >> $OUT
echo "main tables done $(date)" >> $OUT
# probe V's scorecard (v_score.py, unchanged) on sampling seed 1 of the combined models
mkdir -p $Q/v
for m in 1 2 3; do f=$G/h_cz_random_s1_seed0$m.parquet; j=$Q/v/Czero_seed0$m.json
  [ -f "$f" ] && [ ! -f "$j" ] && python -u v_score.py --gen "$f" --label "C zero seed $m" --out "$j" > $Q/v/log_Czero_$m.txt 2>&1; done
for m in 1 2; do f=$G/h_czw_random_s1_seed0$m.parquet; j=$Q/v/CzeroW_seed0$m.json
  [ -f "$f" ] && [ ! -f "$j" ] && python -u v_score.py --gen "$f" --label "C zero weight variable seed $m" --out "$j" > $Q/v/log_CzeroW_$m.txt 2>&1; done
if ls $Q/v/*.json >/dev/null 2>&1; then
  echo >> $OUT; echo "## Probe V scorecard on probe C's generated files (v_collect.py)" >> $OUT
  python -u v_collect.py $Q/v $Q/v_tables.md > $Q/v_collect.log 2>&1 && cat $Q/v_tables.md >> $OUT \
    || echo "v_collect.py FAILED (see $Q/v_collect.log)" >> $OUT
fi
echo "collector done $(date)" >> $OUT
