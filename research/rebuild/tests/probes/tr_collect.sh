# Probe TR: collector. Builds TR's tables from whatever files exist, into
# $Q/tables/TR_TABLES.md. Run by tr_cpu.sbatch after the generation and
# completion jobs (afterany).
set -uo pipefail
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/tr
T=$Q/tables; mkdir -p $T $Q/v
OUT=$T/TR_TABLES.md
ITEMS=()
for s in 1 2; do f=$Q/gen/tr_joint_leaf50_s$s.parquet; [ -f "$f" ] && ITEMS+=("TR_trees_s$s=$f"); done
for s in 1 2; do f=$Q/gen/tr_joint_leaf50_uf_s$s.parquet; [ -f "$f" ] && ITEMS+=("TR_trees_SCFunweightedfit_s$s=$f"); done
f=$Q/gen/tr_joint_leaf50_wv_s1.parquet; [ -f "$f" ] && ITEMS+=("TR_trees_SCFweightvar_s1=$f")
f=$Q/gen/tr_joint_leaf20_s1.parquet; [ -f "$f" ] && ITEMS+=("TR_trees_leaf20_s1=$f")
f=/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/gen/h_cz_random_s1_seed01.parquet; [ -f "$f" ] && ITEMS+=("C_transformer_zero_m1s1=$f")
f=/nfs/roberts/scratch/pi_nrs36/jar335/probes/bl/gen/bl_unweighted_leaf20_s1.parquet; [ -f "$f" ] && ITEMS+=("BL_trees_PUFonly_s1=$f")
{
  echo "# Probe TR tables (written $(date) by tr_collect.sh, job ${SLURM_JOB_ID:-})"
  echo
  printf -- '- %s\n' "${ITEMS[@]}"
  echo
  echo "## Whole records: PUF side (c3_compare.py, c3_table.py)"
  echo
} > $OUT
python -u c3_compare.py --out $T/compare.json "${ITEMS[@]}" > $T/compare.log 2>&1 && \
  python -u c3_table.py $T/compare.json >> $OUT || echo "generation tables FAILED (see $T/compare.log)" >> $OUT
echo >> $OUT
echo "## Copying (from the info files)" >> $OUT
for f in $Q/gen/*_info.json $Q/comp/*_info.json; do [ -f "$f" ] && python -c "
import json,sys; d=json.load(open('$f')); print('-', '$(basename $f)', {k:v for k,v in d.items() if 'share' in k or k=='copying'})" >> $OUT; done
echo >> $OUT
echo "## Completed PUF wealth and generated joint records' wealth (tr_wealth.py)" >> $OUT
echo '```' >> $OUT
python -u tr_wealth.py >> $OUT 2> $T/wealth.err || echo "tr_wealth.py FAILED (see $T/wealth.err)" >> $OUT
echo '```' >> $OUT
echo "## SCF test records (tr_scftest.py)" >> $OUT
python -u tr_scftest.py >> $OUT 2> $T/scftest.err || echo "tr_scftest.py FAILED (see $T/scftest.err)" >> $OUT
echo "main tables done $(date)" >> $OUT
f=$Q/gen/tr_joint_leaf50_s1.parquet; j=$Q/v/TR_s1.json
[ -f "$f" ] && [ ! -f "$j" ] && python -u v_score.py --gen "$f" --label "TR trees joint s1" --out "$j" > $Q/v/log_TR_s1.txt 2>&1
for c in /nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/v/Czero_seed01.json; do [ -f "$c" ] && cp "$c" $Q/v/; done
if ls $Q/v/*.json >/dev/null 2>&1; then
  echo >> $OUT; echo "## Probe V scorecard (v_collect.py): trees against C's zero-coded model seed 1" >> $OUT
  python -u v_collect.py $Q/v $T/v_tables.md > $T/v_collect.log 2>&1 && cat $T/v_tables.md >> $OUT \
    || echo "v_collect.py FAILED (see $T/v_collect.log)" >> $OUT
fi
echo "collector done $(date)" >> $OUT
