#!/bin/bash -l
#SBATCH --job-name=s32cpu
#SBATCH --partition=day,week
#SBATCH --time=01:30:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step32/cpu_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step32/cpu_%x_%j.err
# Step 32 (32_STATUS.md): the CPU steps other than the tilt.
#   sbatch -J maps32   research/rebuild/minimal/32_cpu.sbatch maps        (32_maps.py -> map/V32/)
#   sbatch -J mix32k1  research/rebuild/minimal/32_cpu.sbatch mix 1       (32_mix.py: the three starting files of seed 1)
#   sbatch -J check32  research/rebuild/minimal/32_cpu.sbatch check       (32_report.py check -> step32/check_m16.md)
#   sbatch -J report32 research/rebuild/minimal/32_cpu.sbatch report      (32_report.py -> step32/report.md)
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export MINIMAL_KG_PIECES=1 MINIMAL_TABLE=E27 MINIMAL_ALL_RECORDS=1
D=research/rebuild/minimal
case "${1:?maps, mix, check or report}" in
  maps)   python $D/32_maps.py ;;
  mix)    python $D/32_mix.py ${2:?seed number} ;;
  check)  python $D/32_report.py check ;;
  report) python $D/32_report.py ;;
  *) echo "unknown step $1"; exit 1 ;;
esac
echo "=== done $(date)"
