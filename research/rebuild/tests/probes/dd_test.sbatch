#!/bin/bash
# Probe DD: small test of the chain's steps on early checkpoints (copies), so a
# bug shows before the real chains run. Writes to $Q/test/.
#SBATCH --job-name=dd_test
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=00:40:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/dd_test_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/dd_test_%j.err
set -euxo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
PT=$HERE/../posttrain
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/dd
T=$Q/test; mkdir -p $T
cp $Q/model_dd/checkpoint_full_v4dd_seed01.pt $T/ck_dd.pt
cp $Q/model_ddp/checkpoint_full_v4ddp_seed01.pt $T/ck_ddp.pt
DATA=$Q/data/full_v4dd
PAID=MORTGAGE_INTEREST_PAID,REAL_ESTATE_TAX_PAID
cd $HERE
ORDER=$(python -c "import json;m=json.load(open('$DATA/meta.json'));v={x['name']:x for x in m['variables']};p='$PAID'.split(',');print(','.join(n for n in m['generation_order'] if v[n].get('group')!='scf_only' and n not in p))")
for MODE in random_paid random; do
  python -u dd_complete.py --data-dir $DATA --checkpoint $T/ck_dd.pt --out-dir $T --tag "t_$MODE" \
    --train-seed 1 --mode generate --fill "$ORDER" --gen-source PUF --gen-year 2015 --draws 1 \
    --n 4000 --seed 5 --order "$ORDER" --order-mode $MODE --tokenizer $PT/g_tokenizer.py --batch-size 4000 > $T/log_gen_$MODE.txt
  python -u c3_totals.py $T/h_t_${MODE}_seed01.parquet
done
python -u dd_complete.py --data-dir $Q/data/full_v4ddp --checkpoint $T/ck_ddp.pt --out-dir $T --tag "t_ddp" \
    --train-seed 1 --mode generate --fill "$ORDER" --gen-source PUF --gen-year 2015 --draws 1 \
    --n 4000 --seed 5 --order "$ORDER" --order-mode random --tokenizer $PT/g_tokenizer.py --batch-size 4000 > $T/log_gen_ddp.txt
python -u c3_totals.py $T/h_t_ddp_seed01.parquet
cd $PT
WORD=$(python -c "import json;m=json.load(open('$DATA/meta.json'));g=m['generation_order'];w=[v['name'] for v in m['variables'] if v['group']=='scf_only'];print(','.join([x for x in g if x in w]))")
for D in full_v4dd full_v4dd_hide; do
  python -u h_complete.py --checkpoint $T/ck_ddp.pt --out-dir $T --train-seed 1 --data-dir $Q/data/$D --tag pw_$D \
    --mode complete --fill "$WORD" --split train --target-sources PUF --as-source SCF --decode-year 2015 --draws 1 \
    --seed 3 --no-score --max-records 3000 > $T/log_pw_$D.txt
done
python -u h_complete.py --checkpoint $T/ck_dd.pt --out-dir $T --train-seed 1 --data-dir $DATA --tag st --mode complete \
  --fill "$WORD" --split test --target-sources SCF --draws 1 --seed 3 --max-records 2000 > $T/log_st.txt
python -u h_complete.py --checkpoint $T/ck_dd.pt --out-dir $T --train-seed 1 --data-dir $DATA --tag stn --mode complete \
  --fill "$WORD,$PAID" --order "$WORD,$PAID" --split test --target-sources SCF --draws 1 --seed 3 --max-records 2000 > $T/log_stn.txt
echo "$(date) test done"
