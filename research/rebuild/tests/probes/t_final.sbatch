#!/bin/bash
# Probe T: final levels comparison for T_REPORT.md (files in t_final_items.sh).
#SBATCH --job-name=t_final
#SBATCH --partition=day
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=01:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/t_final_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/t_final_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
cd "$HERE"; source t_final_items.sh
OK=(); for it in "${ITEMS_FINAL[@]}"; do p=${it#*=}; p=${p%%:tilt=*}; [ -f "$p" ] && OK+=("$it") || echo "missing $it"; done
python -u t_compare.py --out $Q/compare_final.json "${OK[@]}" > /dev/null
python t_table.py $Q/compare_final.json base_demolast4x,base_s1_agifirst4x,base_s2_agifirst4x,base_s3_agifirst4x,sched_pufbest_agifirst4x,tilt_kept_agifirst4x,big_kept_agifirst4x,big_pufbest_agifirst4x,pufstop_pufbest_agifirst4x
python t_table.py $Q/compare_final.json mixord_demolast4x,mixord_agifirst4x,mixord60_s1_demolast,mixord60_s2_demolast,mixord60_s1_agifirst4x,mixord60_s2_agifirst4x,mixagi60_s1_agifirst4x,mixagi60_s2_agifirst4x
python t_table.py $Q/compare_final.json base_s1_agifirst4x,base_s2_agifirst4x,mixord60_s1_agifirst4x,mixord60_s2_agifirst4x,mixagi60_s1_agifirst4x,mixagi60_s2_agifirst4x,mixagi60_s3_agifirst4x groups
python t_table.py $Q/compare_final.json base_s1_givenagi,base_s2_givenagi,base_s3_givenagi,sched_pufbest_givenagi,big_kept_givenagi,mixord_givenagi,mixord60_s1_givenagi,mixord60_s2_givenagi,mixagi60_s1_givenagi,mixagi60_s2_givenagi,mixagi60_s3_givenagi groups
