# Probe TR: sensitivity, wealth trees with minimum leaf 20 rows (about four households).
G=/nfs/roberts/scratch/pi_nrs36/jar335/probes/tr/gen
python -u tr_synth.py --mode puf_wealth --arm split --wealth-leaf 20
python -u tr_synth.py --mode scf_test --wealth-leaf 20
python -u tr_synth.py --mode generate --wealth-leaf 20 --seeds 1 && python -u c3_totals.py $G/tr_joint_leaf20_s1.parquet
