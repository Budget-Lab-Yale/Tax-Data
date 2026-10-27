# Status: stage 3, the PUF items the model did not draw (27 October)

Worktree `Tax-Data-puf`, branch `rebuild-puf-vars` (from `rebuild-tips-autoloan` at c58daa7); outputs in
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild_puf` (REBUILD_OUT). `rebuild_tips` and `rebuild` are only read. Results
page (when done): `explore_stage3_RESULTS.md`. Scripts and their notes: `rebuild_puf/explore/stage3/`; logs:
`rebuild_puf/logs/`. Jobs are submitted from the worktree root, as below for stage 1.

- Inputs copied from `rebuild_tips` (not outputs): `data/scf_rw/`, `targets/soi_cv/raw/` and `targets/soi_cv/manifest.csv`.
- Code: `build_table.py` (STAGE3: DIV_PARTS, ADJ_PARTS, NEW_ITEMS; dividends and adjustments computed from parts),
  `targets.py` (SOI lines that grow the new items), `dollars.py` and `tilt.py` (the two new computed totals),
  `settings.json` (the CPS's translation rows: dividends to div_pref, div_ord zero, the 11 adjustment parts zero).
- The inventory file named in the brief (`variable_inventory.md` in a scratchpad under /tmp) was gone; the list came
  from the brief and production's `config/variable_guide/baseline.csv` and `src/process_puf.R`.

## Progress
1. 03:05 CPU stages scf ... errors submitted (job 30953541).
2. 03:25 PUF fields checked on the raw PUF (`explore/stage3/puf_fields.py`, `other_item.py`); new PUF rows tested
   (`test_puf_rows.py`: totals equal parts exactly; every growth factor found).
3. 03:32 table built (job 30957103) and checked (`check_table.py`, `check_table.md`): every new item on the PUF rows
   equals the raw PUF; PUF, SCF, CEX and SIPP rows and the AGI label identical to stage 1's table; the CPS's
   dividends within 2.4% of stage 1's in 2019. Commit e8473a7.
4. 03:40 quick test (settings `explore/stage3/settings_quick.json`, REBUILD_RUN=quick, 4 passes; job 30957496) and
   the two full models (REBUILD_RUN=label_seed20261016 / label_seed20261017, jobs 30957529 / 30957530) submitted.
   Next: generate, tilt, report for the pooled run (no REBUILD_RUN: folder seed20261016), then
   `report_translation.py` with REBUILD_RUN=label_seed20261016, then `before_tilt.py` (stage3) and
   `compare_stage3.py seed20261016 label_seed20261016,label_seed20261017`.
5. 04:50 both models trained (H100, 73 minutes each); 05:20 generate (15 min) and tilt done: 746 of 746 targets met,
   87.3% kept. Tables: `explore/stage3/tables_seed20261016.md`, `extras.md`. Results page written:
   `explore_stage3_RESULTS.md`. The translation report (job 30958770, REBUILD_RUN=label_seed20261016) was still
   running at the hand-off; it writes `model/label_seed20261016/translation_report.md`.

## Open
- See the results page, section 6 (2020+ growth for repealed deductions; other_item_exp negatives).

# Earlier: tips (SIPP) and auto loan interest (SCF), 26-27 October

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
