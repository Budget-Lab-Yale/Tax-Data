#!/bin/bash
# Probe EV: rescore the transformer's files after adding within-income-group
# checks, and add probe C's unknown-coded model (model/, deductions unknown
# for non-itemizers) as a second transformer column. Then collect.
set -euo pipefail
E=/nfs/roberts/scratch/pi_nrs36/jar335/probes/ev
C=/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3
for s in 1 2 3; do
  python -u ev_score.py scf --label "transformer | seed $s" --file $C/model_cz/h_scf_test_seed0$s.parquet --out $E/scf/transformer_seed0$s.json
  python -u ev_score.py scf --label "transformer, bridge hidden | seed $s" --file $C/model_cz/h_scf_test_nobr_seed0$s.parquet --out $E/scf/transformer_nobr_seed0$s.json
  python -u ev_score.py scf --label "transformer, deductions unknown (probe C) | seed $s" --file $C/model/h_scf_test_seed0$s.parquet --out $E/scf/transformer_unknown_seed0$s.json
  python -u ev_score.py puf --label "transformer, deductions unknown (probe C) | seed $s" --file $C/model/h_puf_wealth_seed0$s.parquet --out $E/puf/transformer_unknown_seed0$s.json
done
python -u ev_score.py scf --truth-only --out $E/scf/truth.json
python -u v_collect.py $E/v $E/v_tables.md > /dev/null
python -u ev_collect.py $E EV_TABLES.md
cp EV_TABLES.md $E/EV_TABLES.md
