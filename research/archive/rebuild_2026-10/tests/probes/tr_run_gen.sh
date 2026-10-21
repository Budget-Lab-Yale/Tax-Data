# Probe TR: whole joint records (two sampling seeds), totals from parts.
set -e
G=/nfs/roberts/scratch/pi_nrs36/jar335/probes/tr/gen
python -u tr_synth.py --mode generate --wealth-leaf 50 --seeds 1 2
for s in 1 2; do python -u c3_totals.py $G/tr_joint_leaf50_s$s.parquet; done
echo "$(date) job ${SLURM_JOB_ID:-}" > $G/DONE_gen
