---
title: "Step 31: the cell method, first full run — job status and how to resume"
role: status
workstream: rebuild
status: current
updated: 2026-10-18
sot: research/rebuild/design/cell_method.md
supersedes: []
superseded_by: null
---

# Step 31: job status

The question: run the cell method of `design/cell_method.md` as section 8
of that note sets it out, and compare it with today's rule (step 27 R0),
step 27's learned curve (R2J) and step 29's per-cell counts by slice (C4).

Scratch root: `/nfs/roberts/scratch/pi_nrs36/jar335/minimal` (written `$R`).
Logs: `$R/logs/step31/`. Every job ID as submitted: `$R/logs/step31/jobs.txt`.
Commands run from the repository root. Every step skips itself when its
output exists, so resubmitting the same command resumes:

    bash research/rebuild/minimal/31_submit.sh

The report is written by the last job to `$R/step31/report.md`; to rewrite
it by hand: `sbatch -J report31 research/rebuild/minimal/31_cpu.sbatch`.

## What each run is

| Name | What | Code |
|---|---|---|
| Ks1-3 | the cell method, table E27, 80 passes, label on; seeds 20261016-18 | `31_train.py` with `31_settings_scf.json` |
| tilts of Ks1-3 | 136 IRS targets + 8 SCF wealth targets for 2019, wealth counts on the SCF's units (`tilt/step31/s31_ks<k>`) and on the file's units (`tilt/step31/s31_ks<k>_file`) | `31_tilt.sbatch` → `07_baseline.py` |
| file-units tilts of C4, R2J, R0 | the comparison runs' existing stacks, tilted with the counts on the file's units (`tilt/step31/s29_c4s<k>_file`, `s27_r2js<k>_file`, `s27_r0s<k>_file`); their SCF-units tilts are step 29's and step 27's | `31_tilt.sbatch` |
| smokeK | 4-pass smoke test of `31_train.py`; not a result | |

## How the code follows the note, and where it departs

- Settings: every setting of the note's section 2 is in
  `31_settings_scf.json`, read by `31_train.py` and copied into each run's
  `train_meta.json`. The code asserts what it does not yet handle (only the
  "unchanged" dollar rule; only the −$3,000 loss cap).
- Cell table (steps 1-4): as the note, from every 2016-wave SCF training row
  against the PUF side (2015 PUF plus crude non-filers). One departure: a
  slice counts wholly in the range holding its 2015 PUF median, so the four
  or five slices per item that straddle a range edge are not split (step 30
  split them). For married, wages $75k-200k, interest, this gives 19.7, 7.0
  and 1.7 points filled in under $100, $100-$500 and $500-$2,000 against the
  note's 19.6, 7.3 and 1.3 (total 28.3 against 28.2).
- Other waves (step 5): the table's shares of zeros times each wave's own
  zeros, by cell; the slices sit on each wave's grown ruler, so the ranges
  stretch with it.
- Training (steps 6-10): as step 29's C4, with the multipliers one per wave,
  cell and range (not per slice). Refit every 10 batches while filling in and
  after every pass, as step 29.
- Generation, dollars and tilt (steps 11-13): step 29's stack and tilt
  settings, unchanged.

## Jobs (submitted 2026-10-18 12:14)

| Job | What |
|---|---|
| 29983973 | smokeK, 4 passes: done; the scaling hit every target to rounding |
| 29984140 / 29984141 / 29984142 | Ks1: train + stack / tilt, SCF units / tilt, file units |
| 29984143 / 29984144 / 29984145 | Ks2 |
| 29984146 / 29984147 / 29984148 | Ks3 |
| 29984149-51 | file-units tilts of step 29 C4s1-3 |
| 29984152-54 | file-units tilts of step 27 R2Js1-3 |
| 29984155-57 | file-units tilts of step 27 R0s1-3 |
| 29984158 | the report (after every run and tilt, whether or not each succeeded) |
| 29984159 | report sections 0-3 on smokeK (test of the report code; output `$R/step31/smoke/report.md`): done, sections 0-3 written without error |

## All jobs finished (2026-10-18, 13:00)

Every job completed. Two file-units tilts (R0s1, R2Js2) missed the solver
tolerance on the first try and passed on the script's one-thread retry. The
report is `$R/step31/report.md`.

## Possible things to look into (not run)

- **When filling in starts.** Fill-ins start at pass 3 (`fill_from_pass` in
  the settings sheet), carried over from steps 22-29 and never tested. A
  check: one seed each starting at pass 2, 3, 6 and 10, comparing who the
  hidden holders are, net worth for units with none of the three items, and
  the tilt's move. Noted by John, 2026-10-18; not to run for now.

## Checks asked for by John, 2026-10-18 13:15

| Job | What | Output |
|---|---|---|
| 29988888, 29988889 (done) | the cell method on step 29's fake survey (table F29), seeds 20261016-17: train, then `29_fake.py check` (hidden holders and dollars against the planted truth; comparable with step 29's F29 runs) | `$R/step29/fake/F29Ks1.md`, `F29Ks2.md`; models `$R/model/step31/F29Ks<k>/` |
| 29988892 (done) | `31_combos.py`: share of units holding each combination of the three items, by cell, 2016 wave: PUF side, SCF reported, SCF filled in (cell method and step 29 C4) | `$R/step31/combos.md` |

Resubmit the fake runs with
`sbatch -J F29Ks1 --export=ALL,MINIMAL_TABLE=F29 research/rebuild/minimal/31_run.sbatch F29Ks1 20261016`
(and `F29Ks2 20261017`). The step 29 report (`29_report.py`) summarises
every F29 run found in `$R/step29/fake/`; the F29K runs need their own row there.

Of the "outside data" checks proposed: holders by AGI bracket against IRS
tables is **not** a test of the fill-in, because the generated file's
income items are drawn under the PUF label and the tilt targets them;
dropped. Net worth by wage group before the tilt, against the SCF, is in
section D of the report.
- **A 2019 starting file mixed across waves (for later; post-training, not
  this step).** Draw each record's net worth under the 2019 wave's tag for
  80% of records and the 2016 wave's for 20% (step 21's machinery:
  `MINIMAL_STACK_NW_YEAR`, `MINIMAL_NW_MAP_YEARS`, `21_priors.py`), then
  reweight. No retraining needed. Step 21's mixing was built for table Y1
  and will likely need adapting to E27. Noted by John, 2026-10-18.

## Done, 18 October afternoon

Every job above finished. Results written up in `31_RESULTS.md`; the
session's discussion in `31_DISCUSSION_2026-10-18.md`; `LATEST.md` now
points at this step.
