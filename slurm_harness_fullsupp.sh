#!/bin/bash
#SBATCH --job-name=hrn_fsup
#SBATCH --partition=day
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=16
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_harness_fullsupp.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_harness_fullsupp.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

OUTPUT_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline

# retrain arg forces full DRF retrain — required because:
#  (1) compute_percentile now uses full-support breaks (pctile values
#      shift for every feature even where has_* didn't change);
#  (2) feature list shrunk from 20 to 13 (dropped has_* binaries that
#      were patching the collapsed zero-bucket).
# Old forest cache is incompatible and was moved to old_backup_2026042417/.
Rscript src/eda/wealth_harness.R "$OUTPUT_DIR" retrain
