# The Tax-Data rebuild

The rebuild makes a synthetic file of tax units, filers and non-filers, for every year 2015-2075. One generative model
of tax units is pretrained on the 2015 PUF and four household surveys (the SCF, the CEX, the CPS and the SIPP);
records drawn from it are then reweighted to each year's published totals, with taxes computed by tax-simulator-gpu
inside the reweighting. Each year's file comes with state weights and as Tax-Simulator's `tax_units_<year>.csv`.
Nothing here feeds `src/main.R` yet.

## Stages

The runner, `pipeline/run.py`, runs the stages in this order. Each stage is a separate process that reads and writes
files in the run folder.

1. **Training data** (once): survey tax units for the SCF, CEX, CPS (with its filing stage) and SIPP; SOI's tables and
   their coefficients of variation; the training table and the side files for every survey year (`prep/`).
2. **Pretraining** (once, GPU): three networks with different seeds (`pretrain/`).
3. **Each past year, 2015-2023**: the year's folder, SOI numbers and dollar maps (`scale/`); the draw and the top of the
   file on a GPU (`generate/`); the national reweighting (`reweight/`); each record's chance of each state (GPU); the
   integrated solve of national and state weights.
4. **Each projection year**: 2024 from the 2023 file, then 2025-2075 from the 2024 file, each a reweighting and the
   2023 state split carried forward.
5. **Every year**: the Tax-Simulator file, and with `generation.filers` `real_puf` the package for PUF holders
   (`output/`). Then a check of every year's state weights.

`FEATURES.md` describes each stage.

## Running it

From the repository root. On Bouchet each step is a Slurm job, submitted with dependencies so that a year's steps
wait only for that year's earlier steps:

    python3 research/rebuild/pipeline/run.py training <training folder> --slurm
    python3 research/rebuild/pipeline/run.py years <training folder> <output folder> --slurm

On a plain machine the same commands without `--slurm` run the steps one after another, with `python` and `Rscript`
from the path; GPU steps fall back to the CPU, slowly.

- `training` prepares the training data (stages `scf`, `cex`, `filing`, `cps`, `sipp`, `soi`, `soi_cv`, `table`,
  `side_files`) and then trains the networks listed in settings `generation.models`. The training folder must already
  hold two downloads made by hand: the SCF replicate weights in `data/scf_rw/<wave>/` and SOI's coefficient-of-variation
  workbooks in `targets/soi_cv/raw/`. If the folder has no `settings.json`, the pipeline's is copied in.
- `years` makes every year's file: each past year (`--past`, default 2015-2023), then 2024 from 2023 and the later
  years (`--last-year`, default 2075) from 2024, the check of every year's state weights, and each year's
  Tax-Simulator file (`--no-taxsim` leaves it out). `--past 2019,2023 --last-year 2030` makes a short test. `--side`
  names the folder of side files (default `<training folder>/sidebuild`). The output folder must not already hold a
  run.
- `stage <folder> <stage> ... [--run y2019]` runs named stages on one folder, for example `report`, `calculator`,
  `package`, `non_micro_tables` or `filing_season` (the full list is `STAGES` in `run.py`). The `years` command makes
  the synthetic file only; for a real-PUF file (settings `generation.filers` `real_puf`) the stage `taxsim_file` is
  followed by the package and its checks of both merge scripts.
- `--gpu-partition` sets the Slurm partitions for the GPU steps (default `gpu_h100,gpu_h200`). With `--slurm` the
  submitted lines are written to `<folder>/jobs.txt` and the logs to `<folder>/logs/`.

The runner reads three optional environment variables: `REBUILD_PY_ENV` and `REBUILD_R_ENV`, shell lines run before
a Python or an R stage, and `REBUILD_R_LIBS`, the R library for the filing and CPS R scripts. On Bouchet,
`slurm/bouchet_env.sh` sets all three, and `slurm/stage.sbatch` runs one step as a job.

Every setting is in `pipeline/settings.json`; each run folder gets its own copy, which the stages read through
`REBUILD_SETTINGS`. The PUF year (2015) and the target year are its `years`. The runner sets the other environment
variables a stage reads: `REBUILD_OUT` (the output folder), `REBUILD_RUN` (the run's subfolder: `y<year>` for a year,
the network's name for training), `REBUILD_SEED` (the training seed) and `REBUILD_SAVE_SOLVE` (where the national
solve saves its inputs for the state solve). `REBUILD_PASSES` sets fewer training passes for a quick test, and
`REBUILD_THREADS` the number of threads for the integrated solve.

Outside the repository the pipeline needs: the raw data named in settings `paths.raw` (PUF, SCF, CEX, CPS ASEC and its
March basic file, SIPP, SOI tables and W-2 statistics, Macro-Projections); tax-simulator-gpu at the commit in settings
`taxes.commit` with its own Python (`taxes.python`); R with ipumsr, arrow and data.table; production files
`src/nonfilers/*.R` and `src/imputations/wealth_schema.R`; and
`research/state_weights/nonfiler_residual/resources/` (the filing stage).

Time and memory per step are in `FEATURES.md`, section 7. Every year takes about 3 GPU hours of training plus about 4
hours of wall time on Bouchet.

## Outputs

The training folder holds `data/` (the survey files, the training table, the dollar maps), `targets/` (SOI numbers
and errors), `model/<network>/` and `sidebuild/` (the side files). The output folder holds, for each year,
`final/<year>/` with `gen/y<year>/` (the drawn records), `tilt/y<year>/` (the year file `file_<year>.parquet`, every
target before and after in `fit.csv`, `targets.csv`, `summary.md`, the run report `report.md`) and `taxsim/y<year>/`
(the Tax-Simulator file); `files/state_weights_<year>.parquet`; and `checks/`. A past year's draw and national solve
are in `run/<year>/`, the integrated solve in `integrated/y<year>/`. Every dollar is in nominal dollars of the year.

The installed run is under `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/` (`data/`, `targets/`, `model/`, `gen/`,
`tilt/`, `taxsim/`; `INSTALLED.txt` says where every year's files and the state weights are). It changes only by an
install John approves. No real-PUF files or packages exist for the installed networks.

## Other documents

| File | What it holds |
|---|---|
| `FEATURES.md` | the method of record: every stage, its rules and settings, time and memory, known limitations |
| `TARGETS.md` | the reweighting's targets: principles, how a year's targets are written, standard errors, the solve |
| `TAX_UNITS.md` | how each survey is split into tax units |
| `AGES.md` | how every age and sex in a unit is set |
| `CALCULATOR_CONTRACT.md`, `CALCULATOR_CONTRACT.csv` | the columns tax-simulator-gpu reads and where each comes from |
| `TAXSIM_FILE_COLUMNS.csv` | where each column of the Tax-Simulator file comes from |
| `PACKAGE.md` | what a holder of the 2015 PUF receives and how they rebuild the Tax-Simulator file |
| `ROADMAP.md` | the goal, and the open work between the pipeline and it |
| `HISTORY.md` | the record of how the pipeline was built, day by day |

The record of every experiment before 20 October 2026 is in `research/archive/rebuild_2026-10/`; work since then is in
scratch, `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/<date>/`.

## Working rules

- Plain English for John: tables and real numbers, no jargon.
- Nothing launched, and no design rule changed, without his go.
- Heavy compute through Slurm, never on a login node.
- A new feature is a change to the pipeline and its settings, with `FEATURES.md` updated and a line in `HISTORY.md`.
  Experiments that do not become features are not kept here.
