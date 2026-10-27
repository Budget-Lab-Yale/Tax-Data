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

## Done since (27 October, 00:00-01:20)
- Both models trained (H100 67 min, H200 35 min); generate, tilt and report ran (tilt/seed20261016/). The report
  stage's last step, report_translation.py, failed because it looks for model/seed20261016; rerun with
  REBUILD_RUN=label_seed20261016 (job 30937848, CPU, writes model/label_seed20261016/translation_report.md).
- explore/results_tables.md, explore/compare_latest.md, explore/tips_top.py written and run.
- Results page written: explore_tips_autoloan_RESULTS.md.

## Open
- Decision 7 of the results page (tips at high wages) waits for John.

## Stage 2, 27 October (targets; results page `explore_stage2_RESULTS.md`)
- Added to the default targets: G2's Tables 1.3 and 4.3 (238), W-2 box 7 tips (4), SCF auto loan interest (8); 746
  in all. Reweighting only, CPU: runs `tilt/S2` (all), `tilt/G2`, `tilt/G2_tips`, `tilt/G2_auto` (variants through
  copies of targets_config in `explore/stage2/`, written with `REBUILD_TILT_RUN`). The runs used this code before it
  was committed, so their reports say "with uncommitted changes".
- Open: decision 7 of stage 1; whether to meet the W-2 count of tipped returns by weights (as now) or at generation.
