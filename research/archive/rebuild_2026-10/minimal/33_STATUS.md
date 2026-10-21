---
title: "Step 33: the whole chain with more variables — job status and how to resume"
role: status
workstream: rebuild
status: current
updated: 2026-10-18
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 33: job status

The question (John, 18 October): how does the whole method behave when
about twelve more PUF income items are added, with the SCF items that
measure the same things put on the SCF side? Results, when in:
`33_RESULTS.md` (written by hand from `$R/step33/report.md`).

Scratch root: `/nfs/roberts/scratch/pi_nrs36/jar335/minimal` (written `$R`).
Logs: `$R/logs/step33/`. Every job ID as submitted: `$R/logs/step33/jobs.txt`.
Commands run from the repository root.

## What the chain is

| Piece | What | Code | Output |
|---|---|---|---|
| SCF items | four more SCF household items (X5704, X5714, X5722, X5706) split into tax units by the production builder, each wave | `33_build_scf.R`, `33_scf.sbatch <wave>` | `$R/data/scf<wave>_units33.csv.gz`, `checks_scf<wave>_33.csv` |
| Table E33 | E27 plus eleven PUF items; other income becomes the remainder; SCF side gets three shared items | `33_tables.py`, `33_cpu.sbatch tables` | `$R/data/stacked_E33.parquet`, `band_edges_E33.json`, maps `$R/map/V33/`, checks `$R/step33/tables_checks.md` |
| Settings | the SCF settings sheet: step 31's cell method unchanged, plus `step33_items` | `33_settings_scf.json` | |
| Shared settings | the environment every step-33 job uses | `33_env.sh` | |
| Training + stack | `31_train.py` unchanged (reads the sheet through `MINIMAL_SETTINGS`), 80 passes, then `03_stack.py` as step 31 except the third file is stratified on business income | `33_run.sbatch <run> <seed>` | `$R/model/step33/<run>/`, `$R/gen/gen_puf_stack_s33_<run>.parquet` |
| Tilt | 136 IRS targets + 8 SCF wealth targets for 2019, both count bases | `33_tilt.sbatch <run> scf_units|file_units [tei]` | `$R/tilt/step33/s33_<run>[_file][_tei]/` |
| Report | PUF fit, relationships, net worth before and after, moves | `33_report.py`, `33_cpu.sbatch report` | `$R/step33/report.md` |

Runs: Ns1, Ns2, Ns3 = the base network (4 layers, width 128, as step 31),
seeds 20261016-18 (step 31's seeds). Bs1 = a bigger network (6 layers,
width 256, 8 heads, feed-forward width 512), seed 20261016. Ns1 also gets
a tilt with SOI tax-exempt interest targets added (`_tei`). "smoke" = a
4-pass test of the whole chain; not a result.

To resubmit the whole chain (each step skips itself when its output exists):

    bash research/rebuild/minimal/33_submit.sh none

To rewrite the report by hand: `sbatch -J report33 research/rebuild/minimal/33_cpu.sbatch report`.

## Changes to shared code (each leaves every earlier run unchanged when unset)

- `02_model.py`: (a) slots for the items listed in the band-edge file under
  `meta.step33_items` (only table E33 has that key); (b) `MINIMAL_WIDTH`,
  `MINIMAL_LAYERS`, `MINIMAL_HEADS`, `MINIMAL_FFN` set the network size of
  a new training run (defaults 128, 4, 4, 256 as before).
- `04_dollars.py`: `MINIMAL_EXTRA_AMOUNTS` (more amounts get dollars,
  appended after the others so earlier random streams are unchanged) and
  `MINIMAL_AGI_FOLD` (after drawing, the nine new AGI parts are added back
  into other_agi, so AGI and the 136 targets are computed as before; the
  drawn remainder is kept as `other_agi_remainder`).
- `03_stack.py`: `MINIMAL_STACK_OTHER_STRATIFIER` (the third file is
  stratified on that slot instead of other_agi).
- `soi_cells.py`: `MINIMAL_TEI_TARGETS=1` adds tax-exempt interest (count
  and amount per AGI bracket) to the IRS targets.

## Jobs

| Time | Job | What | State |
|---|---|---|---|
| 13:43 | 29991506-09 | SCF items, waves 2013, 2016, 2019, 2022 | done |
| 13:50 | 29992099 | table E33, first try | failed: 2019 and 2022 wave ids are offset by 1e8 x (year - 2015) in table Y1; fixed |
| 13:52 | 29992149 | table E33 | done: AGI identity exact on all 207,692 PUF records; checks in `$R/step33/tables_checks.md` |
| 13:52 | 29992229 / 29992230 | smoke: 4 passes + stack / tilt | done: the whole chain runs (train, stack, dollars, tilt) |
| 14:00 | 29992792, then a rerun | report code on the smoke run (`$R/step33/smoke/report.md`) | done |

The GPU jobs' memory request was lowered from 96G to 64G while pending
(`scontrol update`) so they would start; `33_run.sbatch` now asks for 64G.
Speed: about 50 seconds a pass for the base network once filling in starts
(about 70 minutes for 80 passes), about 93 seconds for the bigger network
(about 2 hours).
| 13:52 | 29992251 / 52 / 53 / 55 | Ns1: train + stack / tilt SCF units / tilt file units / tilt SCF units with tax-exempt interest targets | submitted |
| 13:52 | 29992257 / 58 / 59 | Ns2 | submitted |
| 13:52 | 29992260 / 61 / 62 | Ns3 | submitted |
| 13:52 | 29992265 / 66 / 67 | Bs1 (bigger network) | submitted |
| 13:52 | 29992268 | report (after every run and tilt, whether or not each succeeded) | submitted |
| 14:24 | 29994413 / 14 / 15 | Bs2: bigger network, seed 20261017 (added for a second seed of the bigger network; not in `33_submit.sh`) | submitted |
| 14:24 | 29994416 | second report, after Bs2 as well; overwrites `$R/step33/report.md` | submitted |

To resubmit Bs2 by hand: `sbatch -J Bs2 --export=ALL,MINIMAL_LAYERS=6,MINIMAL_WIDTH=256,MINIMAL_HEADS=8,MINIMAL_FFN=512 research/rebuild/minimal/33_run.sbatch Bs2 20261017`, then its two tilts as for the others.

Interim report on Ns2 alone (14:20): `$R/step33/interim/report.md`.

## Progress, 15:30

- Ns1, Ns2, Ns3 (base network): trained, stacked and tilted on both bases;
  Ns1's tilt with tax-exempt interest targets done. Report on these three:
  `$R/step33/base/report.md`. Written up in `33_RESULTS.md` sections 4-6.
- Bs1 (bigger network): training, pass 60 of 80 at 15:30; expected done
  with stack and tilts about 16:15. The chain's report (29992268) runs
  after its tilts and writes `$R/step33/report.md`.
- Bs2 (bigger network, second seed): about 2.7 minutes a pass on its GPU,
  so expected about 18:00 with tilts; the second report (29994416) runs
  after it and overwrites `$R/step33/report.md` with Bs2 included.
- Note: `--export=ALL,STEP33_RUNS=a,b` does not work (sbatch splits the
  list at the commas); leave `STEP33_RUNS` unset, the report finds every
  finished run by itself.

## Progress, 16:15 (last update by the agent; its session ends about 17:20)

- Bs1 finished (train, stack, both tilts); the chain's report (29992268)
  finished and wrote `$R/step33/report.md` with Ns1-3 and Bs1.
  `33_RESULTS.md` is written from it.
- **Still running:** Bs2 (29994413, bigger network, seed 20261017; at about
  2.7 minutes a pass, expected to finish training about 18:00), then its
  tilts (29994414, 29994415) and the second report (29994416), which
  rewrites `$R/step33/report.md` with Bs2 included. `33_RESULTS.md` section
  7 reports Bs1 only; when Bs2 is in, compare its rows in the report with
  Bs1's.
- If Bs2 fails: resubmit with the command above; the training skips itself
  if `model.pt` exists, so a failed stack resumes from it.
- Nothing else is unfinished.

## John's instructions for the next round (2026-10-18, evening)

- Step 32 (year mix): good to know it is not decisive; he still wants
  priors set by some rolling most-recent-waves rule later.
- Capitalization (`design/capitalization_hedge.md`): not now; keep the
  note and its findings for later.
- Business income: disaggregate fully on the PUF side: Schedule C, the
  partnership fields and the S-corporation fields (the five of each:
  passive and nonpassive income and loss, and the rest the variable guide
  lists), the other Schedule E net components (rents and royalties,
  estates and trusts, and so on), and farm. The SCF side keeps one total;
  the total the SCF is compared with is computed from the PUF parts (as
  for capital gains), never drawn beside them.
- Open: how to treat the SCF's business losses (1.2% of SCF units against
  6.3% of PUF-side units). John's reading: a definition difference, taxable
  income net of avoidance and losses on returns against something closer
  to book income in the survey.
