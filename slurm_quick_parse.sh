#!/bin/bash
#SBATCH --job-name=parse
#SBATCH --partition=day
#SBATCH --time=00:02:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_quick_parse.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_quick_parse.err
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
Rscript -e 'parse("src/imputations/wealth.R"); parse("src/imputations/tilt_solver.R"); parse("src/eda/wealth_harness.R"); parse("src/eda/report_v3.R"); parse("src/main_phase12_snapshot.R"); parse("src/eda/diagnose_age_income.R"); parse("src/main_placeholder.R"); cat("syntax OK\n")'
