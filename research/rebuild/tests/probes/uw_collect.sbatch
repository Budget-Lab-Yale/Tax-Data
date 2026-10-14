#!/bin/bash
# Probe UW: levels tables for every generated and completed file present.
# Usage: sbatch uw_collect.sbatch <name>
#SBATCH --job-name=uw_collect
#SBATCH --partition=day
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=01:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/uw_collect_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/uw_collect_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw
ITEMS=()
for A in uwA uwB uwC uwD; do
  for f in $Q/$A/h_wfirst_${A}_s*_g*_seed*.parquet $Q/$A/h_wfirstsup_${A}_s*_g*_seed*.parquet $Q/$A/h_random_${A}_s*_g*_seed*.parquet; do
    SUF=gen; case $A in uwC|uwD) SUF=gensqrt;; esac
    [ -f "$f" ] || continue
    b=$(basename "$f" .parquet); b=${b#h_}; b=${b%_seed*}
    ITEMS+=("$b=$f:$SUF")
  done
  for f in $Q/$A/h_givenagi_${A}_s*_seed*.parquet; do
    [ -f "$f" ] || continue
    b=$(basename "$f" .parquet); b=${b#h_givenagi_}; b=${b%_seed*}
    ITEMS+=("given_${b}=$f")
  done
  for f in $Q/$A/h_givenagi_sup_${A}_s*_seed*.parquet; do
    [ -f "$f" ] || continue
    b=$(basename "$f" .parquet); b=${b#h_givenagi_sup_}; b=${b%_seed*}
    ITEMS+=("givensup_${b}=$f")
  done
done
echo "items: ${ITEMS[*]}"
python -u uw_compare.py --out $Q/compare_$1.json "${ITEMS[@]}"
python -u uw_summary.py $Q/compare_$1.json | tee $Q/tables_$1.md
echo "$(date) done"
