#!/bin/bash
# Piece E development: fabricate draws, then evaluate them (run inside e_py-like job).
set -euo pipefail
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
D=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/e
export E_OUT=$D/out
python -u $CODE/e_dev_fake.py
python -u $CODE/e_evaluate.py --truth
python -u $CODE/e_evaluate.py --label fake_ref --draws $D/fake_draws.npz --kind reference
python -u $CODE/e_evaluate.py --label fake_m1 --draws $D/fake_draws.npz --weights $D/fake_weights.npz --kind method1
python -u $CODE/e_evaluate.py --label fake_m2 --draws $D/fake_draws_new.npz --kind method2
python -u $CODE/e_evaluate.py --yardstick --draws $D/fake_draws.npz
