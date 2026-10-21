# Probe TR: second collector pass (after the leaf-20 sensitivity job): the
# full tables again, then the copying check by AGI group.
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/tr
bash tr_collect.sh
python -u tr_copies.py > $Q/tables/tr_copies.md 2> $Q/tables/tr_copies.err || echo "tr_copies.py FAILED" >> $Q/tables/tr_copies.md
cat $Q/tables/tr_copies.md >> $Q/tables/TR_TABLES.md
echo "second pass done $(date)" >> $Q/tables/TR_TABLES.md
