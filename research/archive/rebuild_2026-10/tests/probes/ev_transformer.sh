#!/bin/bash
# Probe EV: score the transformer's existing outputs (probe C, zero coding,
# model_cz, three training seeds), copy the reference scorecards, collect.
set -euo pipefail
E=/nfs/roberts/scratch/pi_nrs36/jar335/probes/ev
C=/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/model_cz
for s in 1 2 3; do
  python -u ev_score.py scf --label "transformer | seed $s" --file $C/h_scf_test_seed0$s.parquet --out $E/scf/transformer_seed0$s.json
  python -u ev_score.py scf --label "transformer, bridge hidden | seed $s" --file $C/h_scf_test_nobr_seed0$s.parquet --out $E/scf/transformer_nobr_seed0$s.json
  python -u ev_score.py puf --label "transformer | seed $s" --file $C/h_puf_wealth_seed0$s.parquet --out $E/puf/transformer_seed0$s.json
done
python -u ev_prep_v.py
python -u v_collect.py $E/v $E/v_tables.md
python -u ev_collect.py $E EV_TABLES.md
cp EV_TABLES.md $E/EV_TABLES.md
