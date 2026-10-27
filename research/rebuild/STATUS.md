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
3. Code written: sipp_read.py, roster_sipp.py, build_scf.R (auto_int_exp), build_table.py, translate.py, model.py,
   train.py, generate.py, dollars.py, tilt.py, tax_units.py (SIPP fallbacks), settings.json, run.sbatch.

## Next
- Run the CPU stages (scf cex filing cps sipp targets errors table), check the units and the translation report.
- Quick training test (REBUILD_PASSES small), then the two pooled models, generate, tilt, report.
