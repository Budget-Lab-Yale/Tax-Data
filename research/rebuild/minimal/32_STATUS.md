---
title: "Step 32: a 2019 starting file that mixes SCF waves — job status and how to resume"
role: status
workstream: rebuild
status: current
updated: 2026-10-18
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 32: job status

The question (John, 18 October): step 31 drew net worth for every record of
the 2019 file under the 2016 SCF wave's year token (tax year 2015). What
happens if instead about 80% of records draw net worth under the 2019 wave's
token (tax year 2018) and 20% under the 2016 wave's? No retraining: step
31's three trained models (`model/step31/Ks1`, `Ks2`, `Ks3`) are used as
they are. Step 21 built this for an earlier table; this step adapts it to
table E27 and step 31's settings.

Scratch root: `/nfs/roberts/scratch/pi_nrs36/jar335/minimal` (written `$R`).
Logs: `$R/logs/step32/`. Every job ID as submitted: `$R/logs/step32/jobs.txt`.
Commands run from the repository root.

## What runs

For each of the three seeds of step 31:

| Name | What |
|---|---|
| token-2018 stack | step 31's stack command again (same model, seeds and settings), except that net worth is drawn under the SCF label with the 2019 wave's year token. The PUF variables are drawn first under "PUF, 2015", exactly as in step 31. Run on the GPU type step 31's stack ran on (seeds 1 and 2: RTX A5000; seed 3: B200), because the same seed gives the same draws only on the same GPU type; if so, the PUF part of the records is identical to step 31's and only net worth differs. |
| token-2015 stack (check) | the same through the new code path with the 2016 wave's token; should equal step 31's stack row for row. |
| m16 | 100% 2016 wave: step 31's stack itself, with a net worth year column added. |
| m80 | 80% 2019 wave, 20% 2016 wave: step 31's stack with its weights times 0.2, plus the token-2018 stack with its weights times 0.8 (800,000 records; a mixture of the two samples, as step 21's `21_priors.py`). |
| m19 | 100% 2019 wave: the token-2018 stack. |
| tilts | each file tilted to 2019 with step 31's settings (136 IRS targets exact, eight SCF 2019 wealth targets, the non-filer count), wealth counts on the SCF's units and, separately, on the file's units. |

**Net worth dollars.** A net worth band is a position within the wave it
was drawn under. Two rules are run:

- **Own wave** (the main runs): a record drawn under the 2016 wave's token
  gets step 31's dollars (the 2016 wave's map, unchanged); a record drawn
  under the 2019 wave's token gets the 2019 wave's map, in the 2015 dollars
  the wealth targets use. Under this rule m16 is step 31 exactly.
- **2019 map** (`t19`): every rank is turned into dollars with the 2019
  wave's map, whatever wave it was drawn under. This is what plan
  decisions 3 and 10 say for a 2019 file; step 31 did not do it (it used the
  2016 wave's map). Run for m16 and m80 (for m19 the two rules coincide).

## Code (all new files; no existing file changed)

| File | What |
|---|---|
| `32_maps.py` | map folder `$R/map/V32/`: copies of `map/V27/` (step 31's maps), plus per-wave net worth maps `map_scf_nw_2015.parquet` (step 31's net worth map, unchanged) and `map_scf_nw_2018.parquet` (built from table E27's 2019-wave rows as `21_maps.py` built them for table Y1). Checks in the docstring. |
| `32_stack.sbatch` | one per-token stack (`03_stack.py` with `MINIMAL_STACK_NW_YEAR`), output `$R/gen/step32/gen_puf_stack_s32_ks<k>_nw<year>.parquet` |
| `32_mix.py` | the three starting files `$R/gen/gen_puf_stack_s32_ks<k>_{m16,m80,m19}.parquet`; compares the new stacks with step 31's (`$R/step32/stack_check_ks<k>.csv`) |
| `32_tilt.sbatch` | one tilt (`07_baseline.py`), output `$R/tilt/step32/s32_ks<k>_<file>[_t19][_file]/` |
| `32_report.py` | the reproduction check (`$R/step32/check_m16.md`) and the report tables (`$R/step32/report.md`) |
| `32_cpu.sbatch` | runs `32_maps.py`, `32_mix.py`, the check or the report |
| `32_submit.sh` | the whole chain |

Shared code is used through settings that already exist
(`MINIMAL_STACK_NW_YEAR` in `03_stack.py`; `MINIMAL_NW_MAP_YEARS` and
`MINIMAL_NW_TARGET_MAP_YEAR` in `04_dollars.py`). No shared file was edited.

## How to resubmit

    bash research/rebuild/minimal/32_submit.sh

Every step skips itself when its output exists. To rewrite the report by
hand: `sbatch -J report32 research/rebuild/minimal/32_cpu.sbatch report`.

## Jobs (submitted 2026-10-18, 13:45; the same chain as `32_submit.sh`, submitted by hand in two parts)

| Job | What |
|---|---|
| 29991783 | maps (`map/V32/`): done 13:45, every check passed (see below) |
| 29991784 / 29991785 | seed 1: stack under token 2018 / token 2015 (check); RTX A5000 |
| 29991786 / 29991787 | seed 2: the same; RTX A5000 |
| 29991788 / 29991789 | seed 3: the same; B200 |
| 29991814, 29991829, 29991916 | the three starting files of seeds 1, 2, 3 (after the token-2018 stack succeeds) |
| 29991815-28 | seed 1: ten tilts (m16, m80, m19 own wave; m16, m80 2019 map; each on both bases) |
| 29991895-29991915 | seed 2: ten tilts |
| 29991917-29991929 | seed 3: ten tilts |
| 29991930 | the reproduction check, `$R/step32/check_m16.md` (after the m16 tilts, whether or not they succeed) |
| 29991931 | the report, `$R/step32/report.md` (after every tilt, whether or not each succeeds) |

Every job ID with its role: `$R/logs/step32/jobs.txt`.

## What the maps job found

- The 2016 wave's map rebuilt from table E27 equals step 31's map exactly.
- The 2019 wave's map, in 2015 dollars, totals $89.142 trillion against
  the wealth target's $89.14 trillion (+0.003%), and equals step 21's map
  of the same wave.
- No SCF row of either wave lacks net worth.
- **Step 31's net worth map is in 2016 dollars, not 2015 dollars.** Its
  weighted total is $86.865 trillion against $85.757 trillion for the same
  wave in 2015 dollars, a ratio of 1.0129, exactly the ratio of the two
  price levels. The wealth targets and the conversion to 2019 dollars in
  the report both treat the map's values as 2015 dollars, so step 31's net
  worth before the tilt is about 1.3% too high. Table Y1 (and so E27) holds
  each wave in its own survey year's dollars; step 21's maps converted
  them (`21_maps.py`), step 27's did not. Step 32 keeps step 31's map for
  the 2016-wave records so that the comparison with step 31 is exact; the
  2019 wave's map is on the targets' basis. A question for John (below).

## Done, 18 October, 14:05

All 42 jobs completed (no solver retries). The reproduction check passed
in every seed and on both bases: every weight identical to step 31's. On
the GPU type step 31 used, the new path drew step 31's stack row for row,
and the 2019-token stacks have exactly step 31's PUF records. Report
tables: `$R/step32/report.md`; write-up: `32_RESULTS.md`, which ends with
two questions for John (step 31's net worth map is in 2016 dollars; which
dollar rule for net worth). `LATEST.md` is not changed: step 32 is a
post-training variant of step 31, not a new current run.
