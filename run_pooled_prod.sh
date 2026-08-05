#!/bin/bash
#SBATCH --job-name=pooled_prod
#SBATCH --partition=day
#SBATCH --time=18:00:00
#SBATCH --cpus-per-task=16
#SBATCH --mem=384G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/pooled_prod_%j.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/pooled_prod_%j.err

# Cached full pipeline run with pooled (2019+2022) wealth donors.
# estimate_models=0 / do_lp=0  => load every cached imputation fit + the
# cached LP solve (nothing retrains; the pooled wealth forests are already
# cached as pool1922). Produces a complete new vintage: all years + Forbes
# splice + mortality. mem=384G covers the pooled tilt's wide bottom-cell
# donor-weight matrix (chunk_size=1000 in the wealth call).
export TAXDATA_ESTIMATE_MODELS=0
export TAXDATA_DO_LP=0

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
Rscript src/main.R
