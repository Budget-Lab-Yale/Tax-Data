# Probe TR: wealth completions. Arg 1: wealth minimum leaf.
L=${1:-50}
C=/nfs/roberts/scratch/pi_nrs36/jar335/probes/tr/comp
python -u tr_synth.py --mode puf_wealth --arm split --wealth-leaf $L
python -u tr_synth.py --mode scf_test --wealth-leaf $L
python -u tr_synth.py --mode scf_test_nobr --wealth-leaf $L
python -u tr_synth.py --mode puf_wealth --arm zero --wealth-leaf $L
echo "$(date) job ${SLURM_JOB_ID:-}" > $C/DONE_comp_leaf$L
