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
# non_micro_tables (not in the default list) rebuilds research/rebuild/resources/non_micro/ from its sources (build_non_micro_tables.py).
# filing_season (not in the default list) reads SOI's November filing-season tables into research/rebuild/resources/filing_season/ (build_filing_season.py).
# calculator (an experiment, not in the default list: each record's taxes from the GPU tax calculator, calculator.py).
# taxsim_file (not in the default list): the year file as Tax-Simulator's tax_units_<year>.csv (taxsim_file.py).
# Settings generation.filers real_puf (4 November 2026; FEATURES.md, PACKAGE.md): generate draws the survey items on
# every 2015 PUF record and then, in a past year, runs the top step's redraw at $1 billion on the same GPU into the top
# cache (REBUILD_TOP_CACHE, default <out>/top_cache.pkl), so the job's GPU is released when both are written; the
# tilt reads the cache on a CPU. taxsim_file is then followed by package (package.py): the files that let a holder of
# the 2015 PUF rebuild the year's Tax-Simulator file, and their round-trip test (CPU; R is loaded for the R script).
# top (not in the default list): the top step's network part alone into the top cache (a GPU; REBUILD_TOP_ONLY).
# package (not in the default list): the package alone, on a finished year (filers real_puf only).
# state_start, state_joint, state_tilt, state_carry, state_check (not in the default list; FEATURES.md, "State weights"): the file with state
# weights, every year, one command that runs these stages and the year's others through state_year.sbatch:
# research/rebuild/pipeline/run_state_weights.sh <training folder> <output folder>.
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
# settings generation.filers ('synthetic' or 'real_puf') and whether the year's start is generated (a past year)
filers() { python -c "import json, os; print(json.load(open(os.environ['REBUILD_SETTINGS']))['generation'].get('filers', 'synthetic'))"; }
# (5 November) the last line only: importing target_list prints notes in some years (2018: SOI lines filled from the
# neighbouring years), and with them the comparison with "generate" failed and the real-PUF top step was skipped
start_mode() { PYTHONPATH=$P${PYTHONPATH:+:$PYTHONPATH} python -c "import target_list as T, common; print(T.start_spec(common.TARGET_YEAR)['mode'])" | tail -n 1; }
# the package (package.py): the files and the Python merge script's round trip, then the R merge script run on the
# same PUF (R with the arrow package, John's library) and its round trip; any failure stops the job
run_package() {
  python $P/package.py build
  read -r PKG PUF CHK < <(python $P/package.py paths)
  module purge; module load R/4.4.1-foss-2022b
  R_LIBS=$HOME/R/4.4.1-foss-2022b Rscript "$PKG/merge_puf.R" "$PUF" "$PKG" "$CHK/merged_by_r.parquet"
  module purge; module load $PY_MODULES
  python $P/package.py check_r
}
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
    maps)     python $P/build_table.py maps ;;   # the target year's dollar maps alone, from the table already built (not in the default list)
    targets)  python $P/targets.py ;;
    non_micro_tables) python $P/build_non_micro_tables.py ;;   # the non-microdata imputations' published tables (committed; run when a source changes)
    filing_season) python $P/build_filing_season.py ;;   # SOI's November filing-season tables into research/rebuild/resources/filing_season/ (committed; run when SOI posts a new one)
    errors)   python $P/errors.py soi_cv ;;   # SOI's published CVs, parsed from the workbooks in <out>/targets/soi_cv/raw
    train)    python $P/train.py ;;
    generate) python $P/generate.py
              if [ "$(filers)" = real_puf ] && [ "$(start_mode)" = generate ]; then
                echo "=== top (filers real_puf: the redraw at \$1 billion on this GPU, into the top cache) $(date)"
                REBUILD_TOP_ONLY=1 python $P/tilt.py
              fi ;;
    top)      REBUILD_TOP_ONLY=1 python $P/tilt.py ;;   # the top step's network part into the top cache (a GPU)
    tilt)     python $P/tilt.py; python $P/report_tilt.py ;;
    report)   python $P/report_cex.py; python $P/report_cps.py; python $P/report_sipp.py; python $P/report_translation.py ;;
    calculator) python $P/calculator.py; python $P/report_calculator.py ;;   # every record's taxes from the GPU tax calculator (settings calculator), against SOI
    taxsim_file) python $P/taxsim_file.py   # the year file as the CSV Tax-Simulator reads (production's 188 columns)
              if [ "$(filers)" = real_puf ]; then echo "=== package (filers real_puf) $(date)"; run_package; fi ;;
    package)  run_package ;;   # filers real_puf only (package.py stops otherwise)
    state_start)  for y in ${STATE_YEARS:-2015 2016 2017 2018 2019 2020 2021 2022 2023}; do   # the networks' chances of each state (a GPU)
                    python $P/state_start.py start --file $STATE_RUN/$y/tilt/y$y/file_$y.parquet --year $y --out $STATE_OUT/starts/start_$y.npz
                  done ;;
    state_joint)  export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK OPENBLAS_NUM_THREADS=$SLURM_CPUS_PER_TASK MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK
                  python $P/state_joint.py --run $STATE_RUN --out $STATE_OUT --year ${STATE_YEAR:-$SLURM_ARRAY_TASK_ID} --start-dir $STATE_OUT/starts --tag integrated ;;   # a past year: the integrated solve (CPU)
    state_tilt)   export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK OPENBLAS_NUM_THREADS=$SLURM_CPUS_PER_TASK
                  python $P/state_weights.py --run $STATE_RUN --out $STATE_OUT --year ${STATE_YEAR:-$SLURM_ARRAY_TASK_ID} --final ;;   # a past year, the fallback: the separate tilt and its file (CPU)
    state_carry)  python $P/state_weights.py --run $STATE_RUN --out $STATE_OUT --year ${STATE_YEAR:-$SLURM_ARRAY_TASK_ID} --carry ;;   # a projection year: the 2023 shares (CPU)
    state_check)  python $P/state_weights.py --run $STATE_RUN --out $STATE_OUT --year 2015 --check ${STATE_CHECK_YEARS:+--years $STATE_CHECK_YEARS} ;;   # every year's file there and in order, or it stops
    *) echo "unknown stage $s"; exit 1 ;;
  esac
done
echo "=== done $(date)"
