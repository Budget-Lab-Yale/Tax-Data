#!/bin/bash
# Probe BR: completions from a full_v3br model, one seed (runs
# ../posttrain/h_complete.py unchanged). Usage: sbatch br_complete.sbatch <seed>
#   scf_test        SCF test records, wealth redrawn, bridge variables observed; scored
#   scf_test_nobr   the same with the bridge variables hidden too (drawn after the wealth,
#                   so the wealth score is the score without them)
#   puf_wealth      PUF training records, wealth drawn with the SCF tag at 2015; 5 draws
#SBATCH --job-name=br_comp
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=02:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/br_comp_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/br_comp_%j.err
set -euo pipefail
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/br
SEED=$1; NN=$(printf %02d "$SEED")
DATA=${DATA:-$Q/data/full_v3br}
OUT=${OUT:-$Q/model/full_v3br}
TAGBASE=${TAGBASE:-full_v3br_w1}
BRV=${BRV:-E19200,E18500}
CK=$OUT/checkpoint_${TAGBASE}_seed$NN.pt
test -f "$CK"
WORD=$(python -c "import json;m=json.load(open('$DATA/meta.json'));g=m['generation_order'];w=[v['name'] for v in m['variables'] if v['group']=='scf_only'];print(','.join([x for x in g if x in w]))")
run() { python -u h_complete.py --data-dir "$DATA" --checkpoint "$CK" --out-dir "$OUT" --train-seed "$SEED" "$@"; }
echo "$(date) scf_test"
run --tag scf_test --mode complete --fill wealth --split test --target-sources SCF --draws 5 --seed $((20261023 + 100 * SEED))
echo "$(date) scf_test_nobr"
run --tag scf_test_nobr --mode complete --fill "$WORD,$BRV" --order "$WORD,$BRV" --split test --target-sources SCF \
    --draws 5 --seed $((20261023 + 100 * SEED))
echo "$(date) puf_wealth"
run --tag puf_wealth --mode complete --fill wealth --split train --target-sources PUF --as-source SCF \
    --decode-year 2015 --draws 5 --seed $((20261021 + 100 * SEED)) --no-score
echo "$(date) job ${SLURM_JOB_ID:-}" > "$OUT/DONE_comp_seed$NN"
