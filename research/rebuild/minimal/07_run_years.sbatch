#!/bin/bash
#SBATCH --job-name=min07yrs
#SBATCH --partition=day
#SBATCH --time=01:30:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/07_years_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/07_years_%j.err
# Several-years table: build the 2016 and 2022 SCF waves with 01_build_table.R
# in wave mode (the 2019 wave comes from 06_build_scf2019.R), then band and
# split the PUF plus the three waves with 01_band.py --years.
# Usage: sbatch research/rebuild/minimal/07_run_years.sbatch [band]
# ("band" reruns only the Python part.)
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
if [ "${1:-all}" != "band" ]; then
  module load R/4.4.1-foss-2022b
  Rscript research/rebuild/minimal/01_build_table.R --scf-wave 2016
  Rscript research/rebuild/minimal/01_build_table.R --scf-wave 2022
  module purge
fi
test -f /nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/scf2019_units.parquet
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python research/rebuild/minimal/01_band.py --years
