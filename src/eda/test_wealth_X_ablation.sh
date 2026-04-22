#!/bin/bash
#SBATCH --job-name=wealth_Xabl
#SBATCH --partition=day
#SBATCH --time=16:00:00
#SBATCH --cpus-per-task=16
#SBATCH --mem=128G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_wealth_Xabl.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_wealth_Xabl.err

# Per-spec worker. Expects SPEC env var set to small|medium|large via
#   sbatch --export=ALL,SPEC=<spec> \
#          --job-name=wealth_Xabl_<spec> \
#          --output=.../slurm_wealth_Xabl_<spec>.out \
#          --error=.../slurm_wealth_Xabl_<spec>.err \
#          src/eda/test_wealth_X_ablation.sh

if [ -z "$SPEC" ]; then
  echo "ERROR: SPEC env var not set. Submit with --export=ALL,SPEC=<small|medium|large>." >&2
  exit 1
fi

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
Rscript src/eda/test_wealth_X_ablation.R "$SPEC"
