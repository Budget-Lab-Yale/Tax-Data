#!/bin/bash
#SBATCH --job-name=wealth_Xabl_pp
#SBATCH --partition=day
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_wealth_Xabl_pp.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_wealth_Xabl_pp.err

# Post-processing: reads the 3 per-spec caches, produces diagnostics + plots.
# Submit with --dependency=afterok:<small_id>:<medium_id>:<large_id>.

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
Rscript src/eda/test_wealth_X_ablation_postproc.R
