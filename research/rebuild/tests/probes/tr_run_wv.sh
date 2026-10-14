# Probe TR: wealth completions with the SCF weight as a variable (SCF trained unweighted).
C=/nfs/roberts/scratch/pi_nrs36/jar335/probes/tr/comp
python -u tr_synth.py --mode puf_wealth --arm split --wealth-leaf 50 --scf-weight-variable
python -u tr_synth.py --mode scf_test --wealth-leaf 50 --scf-weight-variable
echo "$(date) job ${SLURM_JOB_ID:-}" > $C/DONE_comp_wv
