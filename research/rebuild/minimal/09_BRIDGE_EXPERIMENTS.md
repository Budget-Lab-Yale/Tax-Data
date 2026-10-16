---
title: "Minimal example: the bridge-variable experiment grid"
role: plan
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# The bridge-variable experiment grid

The problem is stated in `08_STATE_2026-10-14.md`: the variables shared by
the PUF and the SCF (the bridge) are measured differently in the two
sources, so wealth learned on the SCF comes out wrong on PUF-style
records (about 2x). Fixes live in one of three places: the data, the
model, or the output. This grid tries each, on the seven-variable table,
with one scoreboard. John asked for it on 15 October at about 12:45 a.m.;
the runs start only when he says so. Every run is a retrain of the same
network (four layers, width 128) on a seven-variable table; a few minutes
each on one GPU.

## The scoreboard (identical for every run)

1. Wealth on PUF-style records against the actual SCF: total, mean,
   median, top 1% and top 0.1% shares, 99th percentile; mean and median by
   wage group. (`06_wealth_tables.py` functions.)
2. PUF-style holder shares for interest, dividends, capital gains against
   the PUF and the IRS; these must not break.
3. The label swap on shared variables (from `02_evaluate.py`): how much
   the prediction for a shared variable changes under the other label.
4. The meter: the move in nats, and the effective sample size ratio, that
   the tilt needs to hit the 2019 SCF wealth targets (six soft targets,
   `07_baseline.py`), with and without the 136 IRS income targets.
5. Held-out loss per variable, noting which are not comparable across
   runs because the bands changed.

One row per run in a summary table at the top of `09_RESULTS.md`.

## The runs

Control.

- **E0. Current model (v2).** The 2x. Everything is measured against it.

Fix the data.

- **E1. Production's threshold.** Zero PUF interest, dividends, capital
  gains under the detection limit; within-source ranks. Not viable as
  output (the small amounts are gone), but shows how much the holder-share
  alignment alone removes.
- **E2. Coarsest-common split.** Shared part = amount at or above the
  limit, else zero (both sources, within-source ranks); PUF-owned small
  part = amount below the limit (absent on SCF). The generalized production
  recipe. (`01_variants.py` table A.)
- **E3. E2 with the limit raised** until the within-bracket means in the
  detection table agree (where the reporting rate reaches about one), to
  see what assumption 3 costs.
- **E4. Common dollar scale.** Shared variables banded on PUF cut points
  for both sources; no split. (`01_variants.py` table B.)
- **E5. E2 plus E4.** The split with the shared part on the common scale;
  the exact version of E2.
- **E6. D23 translation as designed.** Flip the most likely SCF
  non-holders to holders until the shares match the PUF's, rank within
  cells. The archive's answer (`joint_model_history.md`, probe Y rules).

Fix the model.

- **E7. Shared variables predicted without the source token.** Married,
  n_dep, wages, interest, dividends, capital gains get one rule for both
  sources; other_agi and net_worth keep the label. Nothing else changed.
- **E8. E7 plus coarsened zeros** (the hard line). The failed run of 14
  October repeated with the thing that made it fail removed.
- **E9. Reporting curve in the loss.** An SCF zero scored on p(0) plus
  the sum over amounts of p(amount) times the chance it went unreported;
  a reported amount scored on p(amount) times the chance it was reported.
  The curve comes from the detection table. Needs E7 and the common
  scale (E4).
- **E10. SCF zeros for the three variables treated as unknown** (masked
  in training, never scored). The simplest model fix; measures what
  "known zero" on the SCF side is worth.

Fix the output.

- **E11. Tilt only.** No data or model change; tilt the E0 file to the
  SCF wealth targets by income group as well as in total. How much the
  output step alone can carry, and what it does to wealth given income.
- **E12. The best of E1 to E10, plus the tilt.** The real candidate.

Cross-cutting.

- **E13. Each run with net worth drawn under the SCF label and under the
  PUF label** (the owner-label option halved the top gap on its own).
- **E14. The winner on the three-wave table** (`stacked_years.parquet`),
  to see whether the fix holds across years.

## Order and effort

E0, E1, E2, E4, E7, E10 first (one table each or no table change; cheap).
Then E3, E5, E8, E9, E6 (more preparation). E11 and E12 on the results.
E13 is a generation option, run for every model. E14 last. About an hour
of GPU in all; the preparation is most of the wall time.

## What we expect, written down before running

E2 and E7 each remove a large piece of the 2x; E5 or E9 removes most of
the rest; E12 closes it. The grid exists so that this guess is tested
rather than believed.

## Revision history

- 2026-10-15: written, before any run.
