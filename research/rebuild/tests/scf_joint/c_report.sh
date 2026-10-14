#!/bin/bash
# Part C: refresh every dataset's generated block in EVAL_REPORT.md (run in c_shell.sbatch).
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
S=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
flock $S/eval/.report.lock python -u $CODE/c_compare.py --update-report
