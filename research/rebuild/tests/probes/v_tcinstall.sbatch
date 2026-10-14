#!/bin/bash
# Probe V: try to install Tax-Calculator (taxcalc) into a virtual environment in scratch.
#SBATCH --job-name=v_tcinst
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=8G
#SBATCH --time=00:40:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/v_tcinst_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/v_tcinst_%j.err
set -uo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a
V=/nfs/roberts/scratch/pi_nrs36/jar335/probes/v/tc/venv
python -m venv --system-site-packages $V
source $V/bin/activate
timeout 1500 pip install --no-cache-dir taxcalc 2>&1 | tail -20
python -c "import taxcalc; print('TAXCALC_OK', taxcalc.__version__)"
echo "$(date) done"
