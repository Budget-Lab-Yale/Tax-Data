#!/bin/bash
# Probe DD: collector. Builds every table for DD_REPORT.md from whatever files
# exist, into $Q/DD_TABLES.md. Run by dd_collect.sbatch (CPU).
set -uo pipefail
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/dd
G=$Q/gen
C3=/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/gen
AS=/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/gen
ITEMS=()
for m in 1 2 3 4; do f=$AS/h_random_s1_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("AS_random_m$m=$f"); done
for m in 1 2 3; do f=$C3/h_cz_random_s1_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("Czero_random_m$m=$f"); done
for m in 1 2 3; do f=$C3/h_c_random_s1_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("Cunknown_random_m$m=$f"); done
for m in 1 2 3; do f=$C3/h_c_random_fded_s1_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("Cunknown_fdedfirst_m$m=$f"); done
for m in 1 2 3; do f=$G/h_dd_random_paid_s1_seed0$m.parquet; [ -f "$f" ] && [ -f $G/totals_dd_random_paid_s1_seed0$m.txt ] && ITEMS+=("DD_paidset_m$m=$f"); done
for m in 1; do f=$G/h_dd_random_s1_seed0$m.parquet; [ -f "$f" ] && [ -f $G/totals_dd_random_s1_seed0$m.txt ] && ITEMS+=("DD_paidhidden_m$m=$f"); done
for m in 1 2; do f=$G/h_ddp_random_s1_seed0$m.parquet; [ -f "$f" ] && [ -f $G/totals_ddp_random_s1_seed0$m.txt ] && ITEMS+=("DDP_m$m=$f"); done
OUT=$Q/DD_TABLES.md
{
  echo "# Probe DD tables (written $(date) by dd_collect.sh)"
  echo
  echo "Generated files used: ${#ITEMS[@]}"
  printf '%s\n' "${ITEMS[@]}" | sed 's/=.*\/\(h_.*\)/: \1/' | sed 's/^/- /'
  echo
  echo "## Whole PUF records generated from scratch (dd_compare.py, dd_table.py)"
  echo
  echo "Arms (number of files): AS = probe AS's model; Czero, Cunknown(_fdedfirst) = probe C's two codings;"
  echo "DD_paidset = full_v4dd model, paid set equal to claimed as soon as a record draws itemized;"
  echo "DD_paidhidden = the same model with paid left hidden throughout; DDP = full_v4ddp model (paid never on PUF rows)."
  echo "Every file: sampling seed 1, 664,624 records, fresh random order per record. First column: real PUF training records."
  echo
} > $OUT
python -u dd_compare.py --out $Q/compare.json "${ITEMS[@]}" > $Q/compare.log 2>&1 && \
  python -u dd_table.py $Q/compare.json >> $OUT || echo "generation tables FAILED (see $Q/compare.log)" >> $OUT
echo >> $OUT
echo "## SCF wealth completion scores (dd_scores.py)" >> $OUT
echo >> $OUT
python -u dd_scores.py >> $OUT 2>> $Q/scores.log || echo "dd_scores.py FAILED (see $Q/scores.log)" >> $OUT
echo >> $OUT
echo "## Completed PUF wealth (dd_wealth.py)" >> $OUT
python -u dd_wealth.py >> $OUT 2> $Q/wealth.log || echo "dd_wealth.py FAILED (see $Q/wealth.log)" >> $OUT
echo >> $OUT
echo "## Report tables (dd_rows.py)" >> $OUT
echo >> $OUT
python -u dd_rows.py >> $OUT 2>> $Q/wealth.log || echo "dd_rows.py FAILED (see $Q/wealth.log)" >> $OUT
echo "collector done $(date)" >> $OUT
