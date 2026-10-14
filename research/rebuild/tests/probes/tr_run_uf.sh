# Probe TR: SCF trees fitted on the sample as drawn (unweighted fit, donors
# drawn in proportion to weight), with and without the weight as a predictor.
G=/nfs/roberts/scratch/pi_nrs36/jar335/probes/tr/gen
for opt in --scf-unweighted-fit --scf-weight-variable; do
  python -u tr_synth.py --mode puf_wealth --arm split --wealth-leaf 50 $opt
  python -u tr_synth.py --mode scf_test --wealth-leaf 50 $opt
  python -u tr_synth.py --mode scf_test_nobr --wealth-leaf 50 $opt
done
python -u tr_synth.py --mode generate --wealth-leaf 50 --scf-unweighted-fit --seeds 1 2
for s in 1 2; do python -u c3_totals.py $G/tr_joint_leaf50_uf_s$s.parquet; done
python -u tr_synth.py --mode generate --wealth-leaf 50 --scf-weight-variable --seeds 1
python -u c3_totals.py $G/tr_joint_leaf50_wv_s1.parquet
