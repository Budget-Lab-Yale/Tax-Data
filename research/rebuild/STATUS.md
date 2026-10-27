# Status: tips (SIPP) and auto loan interest (SCF), 26-27 October

A working note so a later session can resume. The results page is `explore_tips_autoloan_RESULTS.md`.

- Branch `rebuild-tips-autoloan` in the worktree `Tax-Data-tips`; outputs in
  `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild_tips` (REBUILD_OUT). The latest run's folders are only read.
- Jobs are submitted from the worktree root, so `run.sbatch` runs the worktree's code
  (it now `cd`s to the submit directory), with `--output` pointing at `rebuild_tips/logs/`:

      cd Tax-Data-tips
      REBUILD_OUT=/nfs/roberts/scratch/pi_nrs36/jar335/rebuild_tips sbatch --output=$REBUILD_OUT/logs/%x_%j.out \
        research/rebuild/pipeline/run.sbatch <stages>

- Copied into `rebuild_tips` from the latest run before starting (inputs, not outputs): `data/scf_rw/` (the SCF
  replicate weights) and `targets/soi_cv/raw/` with its manifest (SOI's CV workbooks).
- Exploration scripts and their logs: `rebuild_tips/explore/`, `rebuild_tips/logs/`; the SIPP data dictionaries
  downloaded from Census are in `rebuild_tips/docs/`.

## Done
1. SCF vehicle loan items checked in all four waves (same labels and codes; explore/scf_auto_explore.py).
2. SIPP releases checked (codes, tips inside job earnings, industry codings; explore/sipp_explore.py).
3. Code written and committed (ebc23e2).
4. CPU stages run (26 October evening; scf and table rerun at 23:27 after the SCF column was renamed auto_int_exp
   and SIPP wages set to rank by count from the top, dollar_line 0). Tables in data/; translation report has SIPP rows.
5. SIPP unit checks pass integrity in every year (data/checks/checks_sipp_<year>.md); SIPP vs CPS unit totals in
   explore/units_vs_cps.md.

## Running (27 October, from 23:35)
- 30930966 tips_quick: REBUILD_SETTINGS=explore/settings_quick.json REBUILD_RUN=quick REBUILD_PASSES=3, stages
  train generate tilt report.
- 30930967, 30930968: train label_seed20261016 and label_seed20261017 (REBUILD_RUN, REBUILD_SEED).

## Next
- When both models are trained: generate tilt report (REBUILD_RUN unset = seed20261016), then
  explore/results_tables.py (through explore/py.sbatch) and the results page.
