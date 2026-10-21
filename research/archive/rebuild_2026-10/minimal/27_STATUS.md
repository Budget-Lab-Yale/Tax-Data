---
title: "Step 27: job status and how to resume"
role: status
workstream: rebuild
status: current
updated: 2026-10-17
sot: research/rebuild/minimal/27_MAPPING_SPEC.md
supersedes: []
superseded_by: null
---

# Step 27: job status

Scratch root: `/nfs/roberts/scratch/pi_nrs36/jar335/minimal` (written `$R`
below). Logs: `$R/logs/step27/`. The job list as submitted, one line per
job: `$R/logs/step27/jobs.txt`. Every job skips a step whose output already
exists, so resubmitting the same command resumes. Commands are run from the
repository root. The write-up is `27_RESULTS.md`; the report tables are
written by the last job to `$R/step27/report.md`.

## What each run is

| Name | Training | SCF waves | Reporting rule |
|---|---|---|---|
| R0s1-3 | `02_model.py` (today's rule: SCF dollars as reported), label on | 2013, 2016, 2019, 2022 | none |
| R2s1-3 | `27_train.py`, label on | all four | family A only (S-curve and dollar factor), fit on the 2016 wave |
| R1s1-3 | `27_train.py`, label on | 2016 only | family A only |
| R2ABs1-3 | `27_train.py` with `MINIMAL_FAMILY_B=1` | all four | families A and B, probability updated every pass (the spec's method) |
| R1ABs1-3 | the same | 2016 only | families A and B |
| R2Ws1 | `27_train.py`, factor grid 0.2 to 5 (`MINIMAL_FACTOR_GRID`) | all four | family A only; sensitivity of the dollar factor |
| R2Ps1-3 | `27_train.py`, fill-ins from the PUF-labelled belief (`MINIMAL_FILL_FROM=puf`) | all four | family A only; diagnostic |
| F27s1 | `27_train.py` on the fake-survey table F27 | fake survey | family A; the test of the split |
| R0F27s1 | `02_model.py` on F27 (today's rule) | fake survey | none; comparison for the fake test |
| **J runs** (second version, the ones the results use) | `27_train.py` with `MINIMAL_SETTINGS_FIT=joint`: the reporting settings fit on the chance of exactly what each unit reported | | |
| R2Js1-3 | joint fit | all four | family A only |
| R2JABs1-3 | joint fit, both families (`MINIMAL_FAMILY_B=1`) | all four | families A and B: the spec's method |
| R1JABs1-3 | joint fit, both families | 2016 only | families A and B: the spec's R1 |
| R2PJs1 | joint fit, fill-ins from the PUF-labelled belief | all four | family A; diagnostic |
| F27Js1 | joint fit on the fake survey | fake survey | family A |

The first version (runs without J) fit the S-curve on whether each unit
reports anything and the dollar factor separately; on the fake survey that
ran the dollar factor to the edge of its grid (0.5) and made the curve far
too steep, so those runs are kept for the record but not used for the
results (27_RESULTS.md).

Seeds: s1 = 20261016, s2 = 20261017, s3 = 20261018.

## Jobs

| Job | What | Output |
|---|---|---|
| 29896442 | 2013 SCF wave (`27_scf2013.sbatch`) | `$R/data/scf2013_units.csv.gz` (done) |
| 29896967 | Tables (`27_cpu.sbatch tables`, `27_tables.py`) | `$R/data/stacked_E27.parquet`, `band_edges_E27.json`, `$R/map/V27/`, `$R/step27/tables_checks.md`, `irs_counts.csv`, `kg_mix.json` (done) |
| 29897250, 29897251 | Smoke test, 4 passes, train + score + stack + tilt (`smokeS2`) | `$R/model/step27/smokeS2/`, `$R/tilt/step27/s27_smokes2/` (done; not a result) |
| 29897545 | Smoke test of family B, 5 passes (`smokeAB`) | `$R/model/step27/smokeAB/` (not a result) |
| 29896988, 29896990, 29896992 | R0s1, R0s2, R0s3 (`27_run.sbatch`: train, stack) | `$R/model/step27/R0s<k>/`, `$R/gen/gen_puf_stack_s27_r0s<k>.parquet` |
| 29896994, 29896996, 29896998 | R2s1-3 (train, score, stack) | `$R/model/step27/R2s<k>/` (model.pt, train_log.csv, train_meta.json, fills.parquet, score.json) |
| 29897000, 29897002, 29897004 | R1s1-3 | `$R/model/step27/R1s<k>/` |
| 29897564, 29897566, 29897568 | R2ABs1-3 | `$R/model/step27/R2ABs<k>/` |
| 29897570, 29897572, 29897574 | R1ABs1-3 | `$R/model/step27/R1ABs<k>/` |
| 29897576 | R2Ws1 | `$R/model/step27/R2Ws1/` |
| one per run (`t<run>` in jobs.txt) | The tilt of each run (`27_tilt.sbatch`), after its run job | `$R/tilt/step27/s27_<run, lower case>/` |
| 29897289 | Fake-survey table F27 (`27_cpu.sbatch fake`) | `$R/data/stacked_F27.parquet`, `$R/step27/fake_note.json` (done) |
| 29897290 | F27s1: training on the fake survey (`27_run.sbatch`, `MINIMAL_TABLE=F27`) | `$R/model/step27/F27s1/` |
| 29897291 | The fake-survey check, cancelled and resubmitted as 29898362 | |
| 29898082 to 29898097, and the `dist-target tilt` lines of jobs.txt | Each run's tilt with the capital gains targets restated (`27_tilt.sbatch <run> dist`, 166 IRS targets) | `$R/tilt/step27/s27_<run>_dist/` |
| 29898087 | The restated tilt of R2s3 (first version): FAILED, solver tolerance missed by 1.5e-7% after both thread settings; not rerun | |
| 29899271, 29899275, 29899278 | R2Ps1-3 | `$R/model/step27/R2Ps<k>/` |
| 29898361 | R0F27s1 | `$R/model/step27/R0F27s1/` |
| 29898362 | Fake check of F27s1 | `$R/step27/fake_check.md` |
| 29899325, 29899326 | F27Js1 and its fake check | `$R/step27/fake_check_F27Js1.md` |
| 29899327 to 29899356 | The J runs, their tilts and restated tilts (see jobs.txt) | `$R/model/step27/<run>/`, `$R/tilt/step27/s27_<run>/`, `..._dist/` |
| 29897584, 29898102, 29899281 | Report jobs cancelled and resubmitted as runs were added | |
| 29899374 | The final report tables (`27_cpu.sbatch report`), after every tilt and both fake checks (afterany, so it runs even if one fails) | `$R/step27/report.md` |

## How to rerun a step

- Tables: delete `$R/step27/TABLES_DONE` and the E27 files, then
  `sbatch -J tables research/rebuild/minimal/27_cpu.sbatch tables`.
- One run (example R2ABs1, seed 20261016):
  `sbatch -J R2ABs1 research/rebuild/minimal/27_run.sbatch R2ABs1 20261016`, then
  `sbatch -J tR2ABs1 --dependency=afterok:<id> research/rebuild/minimal/27_tilt.sbatch R2ABs1`.
  A run's steps are skipped when their outputs exist (`model.pt`,
  `score.json`, `gen_meta_s27_<run>.json`, `tables_s27_<run>.md`); delete
  them to redo a step.
- Scores only: inside a GPU job, `MINIMAL_TABLE=E27 MINIMAL_RUN=step27/<run> python research/rebuild/minimal/27_score.py`.
- Fake survey: `sbatch -J fake research/rebuild/minimal/27_cpu.sbatch fake`;
  `sbatch -J F27Js1 --export=ALL,MINIMAL_TABLE=F27 research/rebuild/minimal/27_run.sbatch F27Js1 20261016`;
  `sbatch -J fakecheck research/rebuild/minimal/27_cpu.sbatch fakecheck F27Js1`
  (writes `$R/step27/fake_check_F27Js1.md`).
- A restated-target tilt: `sbatch -J dR2JABs1 research/rebuild/minimal/27_tilt.sbatch R2JABs1 dist`.
- Report: `sbatch -J report research/rebuild/minimal/27_cpu.sbatch report`.

## State at the end of the session (2026-10-17, 18:00)

Every job has ended. All completed except 29898087 (the restated tilt of
the first-version run R2s3, solver tolerance missed by 1.5e-7%). The report
tables are in `$R/step27/report.md` and `$R/step27/summary.md`; the
write-up is `27_RESULTS.md`.
