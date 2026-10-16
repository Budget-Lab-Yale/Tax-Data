#!/bin/bash
#SBATCH --job-name=min06scf
#SBATCH --partition=day
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/06_scf2019_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/06_scf2019_%j.err
# Minimal example, step 6, part 1: build the 2019 SCF tax units (R), then
# write them as parquet (Python).
# Usage: sbatch research/rebuild/minimal/06_run_scf2019.sbatch
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
module load R/4.4.1-foss-2022b
Rscript research/rebuild/minimal/06_build_scf2019.R
module purge
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python -c "
import pandas as pd
d = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data'
u = pd.read_csv(f'{d}/scf2019_units.csv.gz')
assert not u.isna().any().any()
u.to_parquet(f'{d}/scf2019_units.parquet', index=False)
print('wrote', len(u), 'rows to scf2019_units.parquet')
"
