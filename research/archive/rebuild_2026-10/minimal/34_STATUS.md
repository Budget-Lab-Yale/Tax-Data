---
title: "Step 34: business income in its parts on the PUF side — job status and how to resume"
role: status
workstream: rebuild
status: current
updated: 2026-10-18
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 34: job status

The question (John, 18 October): break step 33's one PUF business income
item (Schedules C, E and F together) into every part the 2015 PUF carries,
make each part a PUF-only model variable, compute the business income total
from the drawn parts (as the capital gains total is computed from its
pieces), and rerun training, stack, 2019 dollars and tilt as step 33 did.
The SCF side and its settings do not change.

Scratch root: `/nfs/roberts/scratch/pi_nrs36/jar335/minimal` (written `$R`).
Logs: `$R/logs/step34/`. Job IDs as submitted: `$R/logs/step34/jobs.txt`.
Commands run from the repository root.

## Jobs

| Time | Job | What | State |
|---|---|---|---|
| 17:49 | 30005363 | `34_explore.py`: the PUF's Schedule C, E and F fields, signs, totals | done (`$R/step34/explore.txt`) |
| 17:53 | 30005510 | `34_explore2.py`: SOI Table 1.4 item names for the parts; signs in the non-filer pool | done (`$R/step34/explore2.txt`) |
| 18:05 | 30006101 | table E34 (`34_tables.py`, `34_cpu.sbatch tables`) | done: all identities exact; checks in `$R/step34/tables_checks.md` |
| 18:05 | 30006105 / 30006270 | smoke: 4 passes + stack, then tilt (SCF-units base); a test of the chain, not a result | train + stack done 18:28 (the computed total works); tilt running |
| 18:06 | 30006111 / 12 / 13 | Ns1 (seed 20261016): train + stack / tilt SCF units / tilt file units | after the table |
| 18:06 | 30006118 / 19 / 20 | Ns2 (seed 20261017) | after the table |
| 18:06 | 30006210 / 11 / 12 | Ns3 (seed 20261018) | after the table |
| 18:06 | 30006217 | report (`34_report.py`), after every run and tilt whether or not each succeeded | after the tilts |
| 18:10 | 30006312 | `34_explore3.py`: what the Schedule E remainder is made of | done (`$R/step34/explore3.txt`) |

## What the chain is

| Piece | What | Code | Output |
|---|---|---|---|
| Table E34 | E33 plus eighteen PUF-owned business income parts; business_income unchanged on both sources | `34_tables.py`, `34_cpu.sbatch tables` | `$R/data/stacked_E34.parquet`, `band_edges_E34.json`, maps `$R/map/V34/`, checks `$R/step34/tables_checks.md` |
| Settings | step 33's SCF sheet unchanged, plus a block `step34_parts` that records the parts (checked by the table job) | `34_settings_scf.json` | |
| Shared settings | the environment every step-34 job uses | `34_env.sh` | |
| Training + stack | `31_train.py` unchanged, 80 passes; `03_stack.py` as step 33 except: business_income on a PUF record is computed from the drawn parts before net worth is drawn (`MINIMAL_COMPUTED_EXTRA=1`), and the third file is stratified on S corporation nonpassive income (the largest part) | `34_run.sbatch <run> <seed>` | `$R/model/step34/<run>/`, `$R/gen/gen_puf_stack_s34_<run>.parquet` |
| Tilt | 136 IRS targets + 8 SCF wealth targets for 2019, both count bases (no SOI targets for the parts: the target files from `01_targets.py` have none) | `34_tilt.sbatch <run> scf_units|file_units` | `$R/tilt/step34/s34_<run>[_file]/` |
| Report | part fit, combinations, totals by wage group, loss sources, tilt and net worth against step 33 | `34_report.py`, `34_cpu.sbatch report` | `$R/step34/report.md` |

Runs: Ns1, Ns2, Ns3 = the base network (4 layers, width 128), seeds
20261016-18, as steps 31 and 33. No bigger network (John: only if the parts
fit badly).

To resubmit the whole chain (each step skips itself when its output exists):

    bash research/rebuild/minimal/34_submit.sh none

To rewrite the report by hand: `sbatch -J report34 research/rebuild/minimal/34_cpu.sbatch report`.

## Changes to shared code (each leaves every earlier run unchanged when unset)

- `03_generate.py`: `MINIMAL_COMPUTED_EXTRA=1` computes every total listed in
  the band-edge file's `meta.computed_totals` (table E34 only:
  business_income from its eighteen parts with their signs) the way
  `MINIMAL_COMPUTED_TOTAL` computes the line-13 capital gains total: the
  total stays present and masked while the PUF variables are drawn; then
  each part gets a dollar point drawn within its band, the signed sum is
  banded on the PUF's 2015 business income cut points, and only then is net
  worth drawn. Unset, no code path or random draw changes.
- `04_dollars.py`: `MINIMAL_SUM_TOTAL=<total>` with
  `MINIMAL_SUM_PARTS=a:1,b:-1,...`: after the dollar draws, the total's own
  draw is kept as `<total>_drawn` and the total becomes the exact signed sum
  of the parts' dollars, before `MINIMAL_AGI_FOLD`. Unset, nothing changes.

## Progress, 18:35

- Table E34 done; every identity exact (see `34_RESULTS.md` section 1).
- Ns1, Ns2, Ns3 started training 18:13-18:17. Speed about 83 seconds a pass
  on an RTX A5000 once filling in starts (step 33: about 50 on an A40),
  so 80 passes take about 110 minutes: training ends about 20:05-20:10, the
  stack takes about 9 minutes, then the two tilts per run and the report.
  Expected report about 20:45 (`$R/step34/report.md`).
- Smoke report (30007804, `$R/step34/smoke/report.md`) after the smoke tilt:
  tests the report code. Not a result.

## 19:35: the agent stopped with the session

The jobs above continue on their own. `34_RESULTS.md` has section 1 only;
fill in the rest from `$R/step34/report.md` when report34 (30006217) has
run. See `HANDOFF_2026-10-18.md`.
