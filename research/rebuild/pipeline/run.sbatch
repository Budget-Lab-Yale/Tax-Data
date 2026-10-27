#!/bin/bash -l
#SBATCH --job-name=rebuild
#SBATCH --partition=gpu_h100,gpu_h200
#SBATCH --gres=gpu:1
#SBATCH --time=08:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/logs/%x_%j.out
# The whole pipeline, or some stages, from the repository root:
#   sbatch research/rebuild/pipeline/run.sbatch                  every stage
#   sbatch research/rebuild/pipeline/run.sbatch train generate   only those
# Stages, in order: scf, cex, filing, cps (the surveys' tax units: the shared rules in Python, then R; filing is the
# CPS's units and their calibrated filing chances, which cps reads), sipp (the SIPP's tax units, in Python), targets (SOI), errors (SOI's CVs), table, train,
# generate, tilt (the year's targets, the reweighting and the run report), report (the CEX, CPS and translation reports).
# The R stages, targets and the table need no GPU; run them alone on a CPU partition if you like:
#   sbatch --partition=day --gres=gpu:0 research/rebuild/pipeline/run.sbatch scf cex filing cps sipp targets table
# H100 and H200 only: one stack from a B200 drew fewer rare business items (PRECISION.md); until that is
# tested, runs to be compared stay on one kind of GPU. REBUILD_PIPELINE runs a copy of the code elsewhere;
# REBUILD_SETTINGS a settings file other than the pipeline's own (every stage reads it).
# The stages run from the repository the job is submitted from (sbatch's submit directory, which must be a repository
# root; REBUILD_ROOT names another), so a worktree runs its own code. The log goes to the folder in the #SBATCH line
# above unless sbatch's --output says otherwise.
set -euo pipefail
cd "${REBUILD_ROOT:-$SLURM_SUBMIT_DIR}"
test -d research/rebuild/pipeline || { echo "not a repository root: $(pwd)"; exit 1; }
echo "repository $(pwd), output ${REBUILD_OUT:-paths.out of the settings}"
P=${REBUILD_PIPELINE:-research/rebuild/pipeline}
export REBUILD_SETTINGS=${REBUILD_SETTINGS:-$P/settings.json}
STAGES=${*:-scf cex filing cps sipp targets errors table train generate tilt report}
PY_MODULES='PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a'
export PYTHONDONTWRITEBYTECODE=1
for s in $STAGES; do
  echo "=== $s $(date)"
  case $s in scf|cex|filing|cps) ;; *) module load $PY_MODULES ;; esac
  case $s in
    scf)      module purge; module load $PY_MODULES; python $P/roster_scf.py   # the units, by the shared rules
              module purge; module load R/4.4.1-foss-2022b; Rscript $P/build_scf.R; module purge ;;
    cex)      module purge; module load $PY_MODULES; python $P/roster_cex.py   # the units, by the shared rules
              module purge; module load R/4.4.1-foss-2022b; Rscript $P/build_cex.R; module purge ;;
    filing)   module purge; module load $PY_MODULES; python $P/filing.py units       # the CPS's units, by the shared rules
              module purge; module load R/4.4.1-foss-2022b                          # the statutory filing requirement
              R_LIBS=$HOME/R/4.4.1-foss-2022b Rscript $P/filing_requirement.R
              module purge; module load $PY_MODULES; python $P/filing.py calibrate   # the filing rule, separate returns
              module purge ;;
    cps)      module purge; module load R/4.4.1-foss-2022b   # ipumsr is not in the module; John's own library has it
              R_LIBS=$HOME/R/4.4.1-foss-2022b Rscript $P/build_cps.R; module purge ;;
    sipp)     python $P/roster_sipp.py ;;   # the SIPP's units by the shared rules, with each adult's wages and tips
    table)    python $P/build_table.py ;;
    targets)  python $P/targets.py ;;
    errors)   python $P/errors.py soi_cv ;;   # SOI's published CVs, parsed from the workbooks in <out>/targets/soi_cv/raw
    train)    python $P/train.py ;;
    generate) python $P/generate.py ;;
    tilt)     python $P/tilt.py; python $P/report_tilt.py ;;
    report)   python $P/report_cex.py; python $P/report_cps.py; python $P/report_sipp.py; python $P/report_translation.py ;;
    *) echo "unknown stage $s"; exit 1 ;;
  esac
done
echo "=== done $(date)"
