#!/bin/bash
# Piece E: syntax check of E's scripts (Slurm, per the compute rule).
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
cd $CODE && python -m py_compile e_evaluate.py e_puf.py e_full.py e_compare.py && echo SYNTAX_OK
