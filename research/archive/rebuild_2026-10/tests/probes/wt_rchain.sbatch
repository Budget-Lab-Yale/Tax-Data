#!/bin/bash
# Probe WT: as wt_chain.sbatch, but with the weight-group restriction
# (wt_complete.py --weight-groups 1000), SCF weight drawn first, 10 draws:
#   scf_test_r, puf_wealth_r. Earlier lines (kept for reference) describe wt_chain:
#   puf_wealth  PUF training records, SCF tag, decoded at 2015: the SCF weight
#               (SCFWT) drawn first, then the 24 wealth variables; 10 draws
#   scf_test    SCF test records, the same fill (their own weight hidden); 10 draws
#   scf_test_wobs  SCF test records, their own weight observed, wealth drawn; 5 draws
# Usage: sbatch wt_chain.sbatch <model seed>
#SBATCH --job-name=wt_rchain
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=128G
#SBATCH --time=01:30:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/wt_rchain_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/wt_rchain_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
PT=$HERE/../posttrain
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/wt
DATA=$Q/data/full_v3cwt
MD=$Q/model
MS=$1; NN=$(printf %02d "$MS")
CK=$MD/checkpoint_wt_seed$NN.pt
test -f "$CK"
cd "$HERE"
WORD=$(python -c "import json;m=json.load(open('$DATA/meta.json'));g=m['generation_order'];w=[v['name'] for v in m['variables'] if v['group']=='scf_only'];print(','.join([x for x in g if x in w]))")
run() { python -u wt_complete.py --data-dir "$DATA" --checkpoint "$CK" --out-dir "$MD" --train-seed "$MS" --weight-groups 1000 "$@"; }
echo "$(date) scf_test_r"
run --tag scf_test_r --mode complete --fill "SCFWT,$WORD" --order "SCFWT,$WORD" --split test \
    --target-sources SCF --draws 10 --seed $((20261023 + 100 * MS)) > $MD/log_scf_test_r_seed$NN.txt
echo "$(date) puf_wealth_r"
run --tag puf_wealth_r --mode complete --fill "SCFWT,$WORD" --order "SCFWT,$WORD" --split train \
    --target-sources PUF --as-source SCF --decode-year 2015 --draws 10 \
    --seed $((20261021 + 100 * MS)) --no-score > $MD/log_puf_wealth_r_seed$NN.txt
echo "$(date) job ${SLURM_JOB_ID:-}" > "$MD/DONE_rchain_seed$NN"
echo "$(date) done"
