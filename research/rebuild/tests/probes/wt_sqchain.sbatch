#!/bin/bash
# Probe WT: completions from a square-root-arm model (wt_train.py
# --loss-weight wtsqrt, checkpoint checkpoint_wtsq_seedNN.pt), SCF weight drawn
# first, 10 draws, without and with the weight-group restriction
# (wt_complete.py --weight-groups 0 is h_complete.py unchanged):
#   sq_scf_test, sq_scf_test_r, sq_puf_wealth, sq_puf_wealth_r
# Usage: sbatch wt_sqchain.sbatch <model seed>
#SBATCH --job-name=wt_sqchain
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=128G
#SBATCH --time=01:30:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/wt_sqchain_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/wt_sqchain_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/wt
DATA=$Q/data/full_v3cwt
MD=$Q/model
MS=$1; NN=$(printf %02d "$MS")
CK=$MD/checkpoint_wtsq_seed$NN.pt
test -f "$CK"
cd "$HERE"
WORD=$(python -c "import json;m=json.load(open('$DATA/meta.json'));g=m['generation_order'];w=[v['name'] for v in m['variables'] if v['group']=='scf_only'];print(','.join([x for x in g if x in w]))")
run() { python -u wt_complete.py --data-dir "$DATA" --checkpoint "$CK" --out-dir "$MD" --train-seed "$MS" "$@"; }
for R in 0 1000; do
  SUF=$([ $R = 0 ] && echo "" || echo "_r")
  echo "$(date) sq_scf_test$SUF"
  run --weight-groups $R --tag sq_scf_test$SUF --mode complete --fill "SCFWT,$WORD" --order "SCFWT,$WORD" --split test \
      --target-sources SCF --draws 10 --seed $((20261023 + 100 * MS)) > $MD/log_sq_scf_test${SUF}_seed$NN.txt
  echo "$(date) sq_puf_wealth$SUF"
  run --weight-groups $R --tag sq_puf_wealth$SUF --mode complete --fill "SCFWT,$WORD" --order "SCFWT,$WORD" --split train \
      --target-sources PUF --as-source SCF --decode-year 2015 --draws 10 \
      --seed $((20261021 + 100 * MS)) --no-score > $MD/log_sq_puf_wealth${SUF}_seed$NN.txt
done
echo "$(date) job ${SLURM_JOB_ID:-}" > "$MD/DONE_sqchain_seed$NN"
echo "$(date) done"
