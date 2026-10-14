#!/bin/bash
# Probe V: Tax-Calculator runs. Usage: sbatch v_tc.sbatch <v_taxcalc.py args...>
#SBATCH --job-name=v_tc
#SBATCH --partition=day
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --time=02:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/v_tc_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/v_tc_%j.err
set -uo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a
V=/nfs/roberts/scratch/pi_nrs36/jar335/probes/v/tc/venv
source $V/bin/activate
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
if ! python -c "import numpy, pandas, pyarrow, taxcalc; pandas.read_parquet('/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s/records_val.parquet'); print('imports ok', numpy.__version__, pandas.__version__)"; then
  echo "imports failed; pinning numpy and pandas to the versions the system modules were built for"
  pip install --no-cache-dir "numpy<2.3" "pandas<2.3" 2>&1 | tail -3
  python -c "import numpy, pandas, pyarrow, taxcalc; print('imports ok after pin', numpy.__version__, pandas.__version__, taxcalc.__version__)"
fi
python -u v_taxcalc.py "$@"
echo "$(date) done"
