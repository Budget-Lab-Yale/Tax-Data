#!/bin/bash
# Probe BL collector: levels tables (probe AS's as_compare.py and as_table.py,
# unchanged) and copying (bl_copies.py) for the tree synthesizer's files and
# probe AS's transformer files. Writes $Q/bl_tables.md.
# Run with: sbatch bl_cpush.sbatch bl_collect.sh
set -uo pipefail
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/bl
G=$Q/gen
A=/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/gen
MD=$Q/bl_tables.md
ITEMS=()
for arm in bootstrap sizefirst catfirst unweighted sizefirst_leaf20 unweighted_leaf20; do
  for s in 1 2; do f=$G/bl_${arm}_s$s.parquet; [ -f "$f" ] && ITEMS+=("tree_${arm}_m$s=$f"); done
done
for o in random sizefirst; do
  for m in 1 2 3 4; do f=$A/h_${o}_s1_seed0$m.parquet; [ -f "$f" ] && ITEMS+=("transformer_${o}_m$m=$f"); done
done
{
  echo "# Probe BL tables ($(date))"
  echo
  echo "Columns: label (number of files). Tree arms: mean over the two sampling seeds [range]."
  echo "Transformer arms (probe AS, summed AGI): mean over model seeds 1-4, sampling seed 1 [range]."
  echo "PUF training records are the truth."
  echo
  python -u as_compare.py --out $Q/bl_compare.json "${ITEMS[@]}" > $Q/bl_compare_detail.md
  python -u as_table.py $Q/bl_compare.json
  echo
  echo "## Copying of training records"
  echo
  CI=("PUF validation records (real, unseen)=validation")
  for arm in bootstrap sizefirst catfirst unweighted sizefirst_leaf20 unweighted_leaf20; do
    for s in 1 2; do f=$G/bl_${arm}_s$s.parquet; [ -f "$f" ] && CI+=("tree_${arm}_s$s=$f"); done
  done
  for m in 1 2 3 4; do f=$A/h_random_s1_seed0$m.parquet; [ -f "$f" ] && CI+=("transformer_random_m$m=$f"); done
  for m in 1 2; do f=$A/h_sizefirst_s1_seed0$m.parquet; [ -f "$f" ] && CI+=("transformer_sizefirst_m$m=$f"); done
  python -u bl_copies.py --out $Q/bl_copies.json "${CI[@]}" | grep -v '^done'
  echo
  echo "## Partial copying at the top"
  echo
  NI=("PUF validation records (real, unseen)=validation")
  for arm in bootstrap sizefirst catfirst unweighted sizefirst_leaf20 unweighted_leaf20; do
    f=$G/bl_${arm}_s1.parquet; [ -f "$f" ] && NI+=("tree_${arm}_s1=$f")
  done
  for m in 1 2; do f=$A/h_random_s1_seed0$m.parquet; [ -f "$f" ] && NI+=("transformer_random_m$m=$f"); done
  f=$A/h_sizefirst_s1_seed01.parquet; [ -f "$f" ] && NI+=("transformer_sizefirst_m1=$f")
  python -u bl_nearest.py --out $Q/bl_nearest.json "${NI[@]}" | grep -v '^done'
  echo
  echo "## Tree settings (from the info files)"
  for f in $G/bl_*_s1_info.json; do
    python -c "import json,sys; d=json.load(open(sys.argv[1])); t=d['trees']; print('-', d['tag'], 'min leaf', d['min_leaf'], 'unweighted', d['unweighted'], 'fit', round(d['fit_seconds']), 's; median over trees of leaves', sorted(x['leaves'] for x in t)[len(t)//2], '; mean share of training records in leaves with one distinct value', round(sum(x['share_records_in_leaves_with_one_distinct_value'] for x in t)/len(t),3), '; order', ' '.join(d['order']))" $f
  done
} > $MD 2> $Q/bl_collect.err
echo "wrote $MD"
