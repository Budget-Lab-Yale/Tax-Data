---
title: "Step 29: job status and how to resume"
role: status
workstream: rebuild
status: current
updated: 2026-10-17
sot: research/rebuild/minimal/27_MAPPING_SPEC.md
supersedes: []
superseded_by: null
---

# Step 29: job status

The question: how many SCF zero-reporters are hidden holders of interest,
dividends and capital gains, and at what sizes, under three rules. All three
keep the source label on, fix the dollar factor at 1 (reporters' dollars are
taken as right), use family A only, and let the model decide which zeros are
the hidden holders (each pass, each zero's slice is drawn from the
SCF-labelled model's belief given the whole record, times the rule's factor).

Scratch root: `/nfs/roberts/scratch/pi_nrs36/jar335/minimal` (written `$R`).
Logs: `$R/logs/step29/`. Every job ID as submitted: `$R/logs/step29/jobs.txt`.
Commands run from the repository root. Every step skips itself when its
output exists, so resubmitting the same command resumes. The write-up is
`29_RESULTS.md`; the report tables are written by the last job to
`$R/step29/report.md`.

## What each run is

| Name | Rule | Table | Code |
|---|---|---|---|
| C1s1-3 | rule 1: one S-curve per item, the same for every household; dollar factor 1 | E27 (2015 PUF with capital gains pieces, crude non-filers, SCF 2013, 2016, 2019, 2022) | `29_train.py` `MINIMAL_RULE=curve` |
| C2s1-3 | rule 2: the S-curve's middle varies by cell (wage group x married), steepness shared; factor 1 | E27 | `MINIMAL_RULE=cellcurve` |
| C3s1-3 | rule 3: per-cell counts and slices (Larrimore, Mortenson and Splinter's method), no curve; the factor h fit on the 2016 wave is used in every wave | E27 | `MINIMAL_RULE=cellcount` |
| C4s1-3 | rule 3 applied wave by wave: the 2016 wave's cell shares of hidden holders (by slice) pinned in every wave, h fit per wave and cell. Added after C3's fill-ins rose pass by pass in the other waves | E27 | `MINIMAL_RULE=cellcount_wave` |
| C5s1-3 | as C4, but hidden holders spread over slices from the smallest amounts up (`MINIMAL_RULE3_ALLOC=smallest_first`). Added after the fake survey showed C3's spread putting hidden dollars at 3 to 10 times the truth | E27 | `MINIMAL_RULE=cellcount_wave` |
| F29C3s1-2, F29C6s1-2 | rule 3 on the fake survey: cumulative spread (C3) and smallest-first spread (C6) | F29 | `MINIMAL_RULE=cellcount` |
| C1s4-5, C2s4-5 | extra seeds (20261019, 20261020) for rules 1 and 2, added after capital gains fill-ins ran away in C2s1 and in C1s1's 2021 wave | E27 | as C1, C2 |
| F29C1s1-2, F29C2s1-2, F29C3s1-2 | the three rules on the fake survey | F29 (below) | `29_train.py` |
| F29R2Js1 | step 27's R2J method (one S-curve, learned dollar factor) on the fake survey | F29 | `27_train.py` with `MINIMAL_SETTINGS_FIT=joint` |
| F29R0s1 | today's rule (survey zeros taken as zeros) on the fake survey | F29 | `02_model.py` |
| smoke* | 4-pass smoke tests of the code; not results | | |

Seeds: s1 = 20261016, s2 = 20261017, s3 = 20261018. The comparisons R2J and
R0 on the real SCF are step 27's runs (`$R/model/step27/R2Js<k>`,
`R0s<k>`), read only.

Table F29 (`29_fake.py tables`): half of the 2015 PUF filers (seed 29) as a
fake survey reporting married, dependents and wages exactly; interest,
dividends and capital gains (line 13) through an S-curve whose middle is
about $303 for households with no wages or wages under $25,000, $175 for
$25,000 to $75,000, and $101 for $75,000 and over (steepness 1.3); dollars
exact; true other income in the survey's own slot. The other half stays the
PUF. Note: `$R/step29/fake/fake_note.json`.

## Files

New: `29_train.py`, `29_fake.py`, `29_report.py`, `29_run.sbatch`,
`29_tilt.sbatch`, `29_cpu.sbatch`, `29_submit.sh`, this file,
`29_RESULTS.md`. No existing file was changed.

Outputs: models `$R/model/step29/<run>/` (model.pt, train_log.csv,
train_meta.json with cells and rule-3 targets, fills.parquet); stacks
`$R/gen/gen_puf_stack_s29_<run>.parquet`; tilts
`$R/tilt/step29/s29_<run>/`; fake checks `$R/step29/fake/<run>.json` and
`.md`; report `$R/step29/report.md`.

## Jobs (submitted 2026-10-17 from 22:08; full list with every change in `$R/logs/step29/jobs.txt`)

| Job | What |
|---|---|
| 29918146 | F29 table (`29_cpu.sbatch fake`): done |
| 29918170, 29918171, 29918172, 29918173, 29918294 | smoke tests (4 passes) of rules 1, 2, 3 on E27, rule 3 on F29, and the tilt of the rule-3 smoke run: not results |
| 29918508 | smokeC3b: 6-pass smoke test of the corrected rule 3: not a result |
| 29918296 / 29918297 | C1s1 run (train, stack) / its tilt |
| 29918298 / 29918299 | C1s2 |
| 29918300 / 29918301 | C1s3 |
| 29918509 / 29918510 | C3s1 |
| 29918511 / 29918512 | C3s2 |
| 29918514 / 29918515 | C3s3 |
| 29918516 / 29918517 | C2s1 |
| 29918519 / 29918520 | C2s2 |
| 29918521 / 29918522 | C2s3 |
| 29918319, 29918320 | F29C1s1, F29C1s2 (train, then fake check) |
| 29918523, 29918524 | F29C3s1, F29C3s2 |
| 29918323, 29918324 | F29C2s1, F29C2s2 |
| 29918325 | F29R2Js1 |
| 29918326 | F29R0s1 |
| 29919332 / 29919333 | C1s4 |
| 29919334 / 29919335 | C1s5 |
| 29919336 / 29919337 | C2s4 |
| 29919338 / 29919339 | C2s5 |
| 29919438 / 29919439 | C4s1 |
| 29919440 / 29919441 | C4s2 |
| 29919442 / 29919443 | C4s3 |
| 29921531 / 29921532 | C5s1 |
| 29921533 / 29921534 | C5s2 |
| 29921536 / 29921537 | C5s3 |
| 29921538, 29921539 | F29C6s1, F29C6s2 |
| 29919716 / 29919717, 29919718 / 29919719, 29919732 / 29919733, 29919734 | C2s4, C2s5, C4s1 and the tilt of C2s3, resubmitted after a "Disk quota exceeded" error on scratch at about 22:31 killed the first attempts (29919336, 29919338, 29919438, 29918522) |
| 29918966, 29919202, 29921195 | test runs of the report on partial results (overwritten) |
| 29923454 / 29923456 | C5s2, rerun after a second "Disk quota exceeded" killed 29921533 at pass 38 (its tilt 29921534 was cancelled) |
| 29923457 | second run of the report, afterany the rerun C5s2's tilt; rewrites `$R/step29/report.md` (the first report's file is kept as `$R/step29/report_before_C5s2.md`) |
| 29918525 | the report (`29_cpu.sbatch report`), afterany every tilt and fake run (dependency list extended with `scontrol update` as runs were added) |

Cancelled and replaced (not results): the first submissions of C3s1-3,
their tilts, F29C3s1-2 (29918302, 29918308, 29918310, 29918303, 29918309,
29918311, 29918321, 29918322), of C2s1-3 and their tilts (29918312, 29918315,
29918317, 29918313, 29918316, 29918318), and the first report job 29918327.
Reasons: rule 3 refit its factor h only once per pass, and the smoke test
showed the hidden-holder share swinging (63% of interest zeros filled in pass
3, 4% in pass 4); it now refits every 10 batches. The cell merge rule merged
unmarried $75,000-200,000 into $25,000-75,000 first and then over $200,000
into that; it now merges the smallest cell first. Rule 3's slice allocation
was then changed (jobs held, not resubmitted) from slice-by-slice flooring to
the cumulative version described in 29_RESULTS.md.

## How to rerun a step

- One real run (example C3s2): `sbatch -J C3s2 research/rebuild/minimal/29_run.sbatch C3s2 20261017`,
  then `sbatch -J tC3s2 --dependency=afterok:<id> research/rebuild/minimal/29_tilt.sbatch C3s2`.
  Delete `$R/model/step29/C3s2/model.pt` to retrain, `$R/gen/gen_meta_s29_c3s2.json` to restack,
  `$R/tilt/step29/s29_c3s2/tables_s29_c3s2.md` to retilt.
- One fake run (example F29C2s1): `sbatch -J F29C2s1 --export=ALL,MINIMAL_TABLE=F29 research/rebuild/minimal/29_run.sbatch F29C2s1 20261016`.
  Delete `$R/step29/fake/F29C2s1.json` to redo only the check.
- The report: `sbatch -J report29 research/rebuild/minimal/29_cpu.sbatch report`.
- Everything first submitted: `bash research/rebuild/minimal/29_submit.sh` (skips what exists). It covers C1, C2, C3 seeds 1-3 and the fake runs F29C1-3, F29R2J, F29R0; C4, C5, F29C6 and seeds 4-5 of C1 and C2 were submitted by hand with the one-run commands above (names select the rule in `29_run.sbatch`; seed k is 20261015 + k).

## State (2026-10-17, 23:45)

Every job has ended. All results are in: the rerun of C5s2 (29923454), its
tilt (29923456) and the second report (29923457) completed, and
`$R/step29/report.md` is the final report (the first report's file is kept as
`$R/step29/report_before_C5s2.md`). `29_RESULTS.md` is written from the final
report. Jobs that failed and were replaced are listed above; none is
outstanding.
