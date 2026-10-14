#!/bin/bash
# Probe BL collector 2: levels tables with the PUF validation records as the
# truth (bl_compare_val.py), for the tree synthesizer, the bootstrap of the
# training records and probe AS's transformer. Writes $Q/bl_tables_val.md.
# Run with: sbatch bl_cpush.sbatch bl_collect_val.sh
set -uo pipefail
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/bl
G=$Q/gen
A=/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/gen
ITEMS=()
for arm in bootstrap sizefirst catfirst unweighted sizefirst_leaf20 unweighted_leaf20; do
  for s in 1 2; do f=$G/bl_${arm}_s$s.parquet; [ -f "$f" ] && ITEMS+=("tree_${arm}_m$s=$f"); done
done
for o in random sizefirst; do
  for m in 1 2 3 4; do f=$A/h_${o}_s1_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("transformer_${o}_m$m=$f"); done
done
{
  echo "# Probe BL tables, truth = PUF validation records ($(date))"
  echo
  echo "The truth column is the 20,768 PUF validation records (weights scaled to the training total)."
  echo "Thresholds are the training records' AGI percentiles."
  echo
  python -u bl_compare_val.py --out $Q/bl_compare_val.json "${ITEMS[@]}" > $Q/bl_compare_val_detail.md
  python -u as_table.py $Q/bl_compare_val.json
} > $Q/bl_tables_val.md 2> $Q/bl_collect_val.err
echo "wrote $Q/bl_tables_val.md"
