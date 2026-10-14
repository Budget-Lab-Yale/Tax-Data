---
title: "Test 4, probe Q — how good must pretraining be? Untargeted wealth statistics before and after the tilt"
role: report
workstream: rebuild
status: current
updated: 2026-10-10
sot: research/rebuild/joint_model_plan.md
supersedes: []
superseded_by: null
---

# Probe Q: does the tilt fix the statistics it did not target?

## The question

John's question for post-training: he has no feel for how good the
pretrained model has to be. The tilt (test 3's method 2: one number added
to the model's log-probabilities per target cell, the network unchanged)
makes the targeted tables come out right. Do the tables that were not
targeted also come out right? If a poor model, once tilted, gets them
nearly as right as a good one, pretraining quality matters little beyond
the targets. If not, is the size of the tilt a usable warning?

## What was run

Everything here is on the SCF records of the version 2 data (`full_v2`),
where the true wealth is known, and every number comes from the jobs
listed under "Files".

**Models.** The version 2 model was retrained with test 3's settings
(`q_train.py`, a copy of `h_train.py` that also saves the model after
chosen epochs; an epoch is one pass over the training data). Training
stopped at epoch 30 and kept epoch 15 (test 3's run kept epoch 16 of 31). That gives a
ladder of models of the same design at different stages: epochs 1, 2, 4, 8,
12, 15 (the kept model, "best"), 20 and 30. Epochs 3, 5 and 6 come from a
second, shorter run with the same settings (its validation losses agree
with the first run's to 0.02 nats). Two further models are deliberately
weak in a different way: the best model with the eleven SCF-measured
incomes hidden from it when it completes wealth ("incomes hidden"), and the
best model with every input hidden except the source and year ("all inputs
hidden"). The second one predicts wealth from nothing about the household.

**Quality of each model before post-training** is measured by the
completion score of test 3's piece H: for SCF validation records, minus the
log of the probability the model gives the true 24 wealth values, filled
one at a time in the fixed order, in nats per record (population-weighted;
lower is better). The best model scores 32.0.

**Records and targets.** The SCF training records with wealth observed
(42,057 rows: five implicates of each tax unit, 2013 and 2016). Targets are
their own tables, as John asked: for each wave, each of the 24 wealth
variables' distribution over 13 or 14 coarse bins (zero, negative, and
rank bands among holders, as test 3) within each group of four
families: income group (bottom 50 percent, 50–90, 90–99, 99–99.9, top 0.1,
by the sum of the eleven incomes the SCF measures, thresholds from the
training records), age range (6), filing status (4) and children under 17
(4). That is 12,122 target cells, 1,348 of them zero. This is test 3's
"core" target set, with filing status and children in place of married and
dependents, which the version 2 data does not carry.

**Tilt.** Piece C's code unchanged (`c_tilt.py`), driven by `q_fit.py`:
fitted on 16 draws per record; then 8 fresh draws per record from the
tilted model. Every fit converged; one variable of one model (epoch 3)
needed the small ridge. On fresh draws the expected target cells miss by
0.003 to 0.03 percent of a group's population on average. The **size of the
tilt** is the KL divergence of the tilted model from the pretrained one, in
nats per record (population-weighted): on average, the log of how much
more likely the tilted model makes its own draws than the pretrained model
did. 0.5 nats means draws about 1.6 times as likely (in the sense of a
geometric mean); 4.6 nats about 100 times.

**Test records.** The same multipliers, fitted on the training records'
tables, were applied to the 12,021 SCF test records (households the model
never saw), and their draws compared with the test records' true wealth.

**Untargeted statistics** (`q_eval.py`), each on the same records as the
truth, before and after the tilt:

- holding rates of each wealth variable by income group × age range (30 cells; the targets cover each family alone, not the cross);
- mean net worth by income group × age range;
- joint holding of each pair of the 20 asset and debt variables (190 pairs), and four named pairs: mortgage among homeowners, equities among owners of pass-through businesses, an unrealized gain among owners of pass-through businesses, mortgage on other real estate among its owners;
- the top 1 percent's share of net worth within each income group;
- mean net worth by household type (filing status × any children);
- the rank correlation between net worth and each of the eleven incomes.

Net worth itself is not targeted (only its components are). Truth is each
record's true wealth tokens turned into dollars the same way as the draws,
so the comparison is not affected by the decoding.

**Two yardsticks.** (1) The truth's own sampling error: the same
statistic on two random halves of the households, half the difference
between them, roughly one standard error. A perfect model would miss the
truth by about this much, because each record's actual wealth is one draw
from its distribution. (2) Draw noise: the best model's tilted draws split
into two halves of 4; it is small.

## Results

### Training records, one row per model, ordered by the size of the tilt

Error before / after the tilt. "Targeted cells": mean absolute error in a
cell's share of its group, percentage points. Holding rates and pairs:
mean absolute error, percentage points. Mean net worth by income × age:
mean absolute error in dollars, as a percent of the income group's true
mean. Household type: mean absolute percent error. Correlation: mean
absolute difference in rank correlation over the eleven incomes.

| Model | Completion score (nats per record) | Tilt size (nats) | Targeted cells | Holding rates, income × age | Mean net worth, income × age (%) | Joint holding of pairs | Top 1% share within income group | Mean net worth by household type (%) | Correlation of net worth with incomes |
|---|---|---|---|---|---|---|---|---|---|
| Epoch 30 | 32.9 | 0.45 | 0.61 / 0.09 | 1.6 / 0.7 | 16 / 5 | 0.65 / 0.11 | 5.2 / 1.6 | 8 / 3 | 0.007 / 0.005 |
| Epoch 20 | 32.2 | 0.48 | 0.62 / 0.09 | 1.5 / 0.7 | 18 / 6 | 0.51 / 0.13 | 4.5 / 1.6 | 10 / 6 | 0.007 / 0.006 |
| Best (epoch 15) | 32.0 | 0.51 | 0.63 / 0.09 | 1.6 / 0.8 | 24 / 6 | 0.52 / 0.14 | 5.3 / 1.5 | 9 / 4 | 0.008 / 0.006 |
| Epoch 12 | 32.0 | 0.54 | 0.67 / 0.09 | 1.8 / 0.8 | 31 / 7 | 0.64 / 0.16 | 5.8 / 1.5 | 13 / 4 | 0.010 / 0.007 |
| Epoch 8 | 32.1 | 0.61 | 0.75 / 0.09 | 1.9 / 0.8 | 55 / 6 | 0.60 / 0.19 | 4.6 / 1.3 | 28 / 4 | 0.016 / 0.014 |
| Epoch 6 | 32.4 | 0.69 | 0.88 / 0.09 | 2.6 / 0.9 | 71 / 5 | 0.83 / 0.23 | 6.0 / 1.3 | 48 / 5 | 0.018 / 0.020 |
| Epoch 5 | 32.9 | 0.78 | 0.98 / 0.09 | 3.4 / 0.9 | 71 / 5 | 1.10 / 0.27 | 5.1 / 1.0 | 51 / 5 | 0.028 / 0.026 |
| Best, incomes hidden | 32.9 | 0.80 | 1.28 / 0.09 | 4.5 / 1.0 | 124 / 8 | 0.66 / 0.15 | 18.7 / 1.7 | 18 / 4 | 0.122 / 0.061 |
| Epoch 4 | 33.8 | 1.01 | 1.15 / 0.09 | 4.3 / 0.9 | 82 / 5 | 1.64 / 0.34 | 6.3 / 1.2 | 52 / 5 | 0.041 / 0.033 |
| Best, all inputs hidden | 33.3 | 1.31 | 1.82 / 0.09 | 8.3 / 1.1 | 269 / 9 | 0.44 / 0.16 | 23.7 / 2.2 | 138 / 4 | 0.226 / 0.067 |
| Epoch 3 | 35.5 | 1.76 | 1.86 / 0.09 | 9.6 / 1.0 | 154 / 6 | 4.80 / 0.41 | 6.8 / 2.5 | 70 / 6 | 0.069 / 0.043 |
| Epoch 2 | 39.3 | 4.59 | 3.87 / 0.09 | 22.8 / 1.2 | 618 / 7 | 14.36 / 0.48 | 10.8 / 4.5 | 328 / 6 | 0.125 / 0.054 |
| Epoch 1 | 55.3 | 19.71 | 8.50 / 0.09 | 53.1 / 1.3 | 5,131 / 8 | 54.05 / 0.53 | 27.2 / 7.4 | 3,068 / 16 | 0.185 / 0.059 |
| Truth's own sampling error | | | 0.57 | 1.3 | 6 | 0.20 | 1.1 | 5.5 | 0.009 |
| Draw noise (best model, 8 draws) | | | 0.09 | 0.2 | 1 | 0.03 | 0.4 | 1.0 | 0.000 |

### Test records (tilt fitted on the training records)

| Model | Tilt size (nats) | Targeted cells | Holding rates, income × age | Mean net worth, income × age (%) | Joint holding of pairs | Top 1% share within income group | Mean net worth by household type (%) | Correlation of net worth with incomes |
|---|---|---|---|---|---|---|---|---|
| Epoch 30 | 0.47 | 1.19 / 1.22 | 2.9 / 2.6 | 25 / 12 | 0.87 / 0.54 | 5.8 / 2.5 | 17 / 10 | 0.017 / 0.015 |
| Best (epoch 15) | 0.53 | 1.18 / 1.22 | 2.9 / 2.7 | 28 / 11 | 0.75 / 0.52 | 5.1 / 2.9 | 17 / 11 | 0.016 / 0.013 |
| Epoch 8 | 0.63 | 1.23 / 1.22 | 3.2 / 2.7 | 49 / 12 | 0.89 / 0.51 | 4.2 / 2.8 | 31 / 11 | 0.024 / 0.020 |
| Epoch 5 | 0.80 | 1.40 / 1.23 | 4.2 / 2.7 | 64 / 12 | 1.20 / 0.55 | 5.2 / 2.3 | 58 / 12 | 0.027 / 0.032 |
| Best, incomes hidden | 0.84 | 1.71 / 1.24 | 5.2 / 2.7 | 100 / 17 | 0.80 / 0.50 | 16.9 / 2.6 | 27 / 16 | 0.132 / 0.067 |
| Epoch 4 | 1.02 | 1.54 / 1.23 | 4.9 / 2.7 | 68 / 13 | 1.61 / 0.58 | 3.8 / 3.3 | 49 / 11 | 0.039 / 0.038 |
| Best, all inputs hidden | 1.33 | 2.12 / 1.24 | 8.6 / 2.7 | 248 / 18 | 0.73 / 0.51 | 24.6 / 2.9 | 148 / 17 | 0.237 / 0.069 |
| Epoch 3 | 1.77 | 2.18 / 1.24 | 9.8 / 2.7 | 129 / 18 | 4.72 / 0.63 | 4.5 / 5.1 | 79 / 11 | 0.070 / 0.049 |
| Epoch 2 | 4.59 | 4.09 / 1.24 | 22.8 / 2.7 | 537 / 21 | 14.22 / 0.68 | 7.8 / 6.2 | 335 / 14 | 0.129 / 0.058 |
| Epoch 1 | 19.69 | 8.65 / 1.25 | 52.9 / 2.8 | 4,538 / 24 | 53.87 / 0.72 | 24.6 / 9.3 | 3,177 / 31 | 0.189 / 0.064 |
| Truth's own sampling error | | 1.00 | 2.4 | 13 | 0.41 | 2.7 | 9.6 | 0.017 |

On test records the targeted cells cannot be hit better than about 1.2
points: the targets are the training records' tables, and the test
records' own tables differ from them by sampling error.

### Relationships between particular wealth items (training records)

Model minus truth, percentage points, before / after the tilt. Truth:
68.7, 90.1 and 39.6 percent.

| Model | Tilt size | Mortgage among homeowners | Unrealized gain among pass-through owners | Other-property mortgage among its owners |
|---|---|---|---|---|
| Best (epoch 15) | 0.51 | +0.3 / −0.2 | +2.7 / +0.9 | −3.0 / −0.7 |
| Epoch 8 | 0.61 | +0.5 / −0.4 | +0.1 / +0.1 | −5.1 / −1.6 |
| Epoch 6 | 0.69 | −0.2 / −0.6 | −5.2 / −3.9 | −8.6 / −6.2 |
| Epoch 5 | 0.78 | −0.9 / −1.2 | −18.4 / −13.0 | −15.8 / −13.9 |
| Epoch 4 | 1.01 | −0.4 / −3.0 | −34.7 / −33.4 | −24.7 / −23.3 |
| Best, all inputs hidden | 1.31 | +0.4 / −0.1 | +1.8 / +0.5 | −4.6 / −0.4 |
| Epoch 3 | 1.76 | +0.8 / −11.2 | −40.4 / −63.7 | −20.1 / −28.1 |
| Epoch 2 | 4.59 | 0.0 / −15.9 | −35.3 / −68.7 | −8.4 / −29.2 |

### Net worth against particular incomes (training records)

Rank correlation of net worth with an income, truth against the model
after the tilt:

| | Wages | Schedule E | Long-term gains | Dividends | Taxable interest | Tax-exempt interest |
|---|---|---|---|---|---|---|
| Truth | 0.18 | 0.26 | 0.19 | 0.44 | 0.41 | 0.23 |
| Best, after tilt | 0.18 | 0.25 | 0.19 | 0.43 | 0.41 | 0.23 |
| Best, incomes hidden: before / after | 0.01 / 0.18 | 0.08 / 0.16 | 0.06 / 0.13 | 0.16 / 0.27 | 0.15 / 0.25 | 0.09 / 0.16 |
| Epoch 4, after tilt | 0.15 | 0.21 | 0.18 | 0.37 | 0.35 | 0.20 |

### Net worth levels by income group (training records, $ thousands)

Before / after the tilt. The test-record version and every model are in
`q_tables_full.md`.

| | Bottom 50% | 50–90% | 90–99% | 99–99.9% | Top 0.1% |
|---|---|---|---|---|---|
| Truth, mean | 110 | 365 | 2,104 | 11,765 | 51,897 |
| Best, mean | 148 / 113 | 426 / 375 | 2,293 / 2,175 | 12,066 / 12,168 | 42,094 / 46,566 |
| Epoch 4, mean | 212 / 113 | 648 / 376 | 3,411 / 2,180 | 13,654 / 12,087 | 24,748 / 49,998 |
| All inputs hidden, mean | 662 / 118 | 629 / 387 | 656 / 2,180 | 661 / 11,761 | 722 / 46,436 |
| Epoch 2, mean | 1,166 / 112 | 1,911 / 375 | 3,669 / 2,146 | 7,617 / 11,856 | 11,723 / 65,372 |
| Truth, median | 16 | 147 | 1,034 | 7,093 | 27,501 |
| Best, median | 17 / 18 | 144 / 144 | 947 / 956 | 6,561 / 6,971 | 20,998 / 26,474 |
| Epoch 4, median | 27 / 19 | 176 / 159 | 1,377 / 1,036 | 8,391 / 7,065 | 15,447 / 24,723 |
| Epoch 2, median | 312 / 32 | 619 / 205 | 1,531 / 1,169 | 3,959 / 6,579 | 6,389 / 24,753 |

The tilt is largest, and errors left after it are largest, at the top.
For the best model the tilt is 0.38 nats per record in the bottom half and
4.8 in the top 0.1 percent; after the tilt, holding rates by income × age
miss by 0.6 points in the bottom half and 5.6 in the top 0.1 percent
(training records; the truth's own sampling error is also largest there).

## What it means for John's question

1. **Statistics built from the targeted groupings come out right after the
   tilt, almost regardless of pretraining.** Holding rates and mean net
   worth by income × age, the top 1 percent's share within income groups,
   net worth by filing status and children: every model with a tilt up to
   about 1.3 nats lands within one to two times the truth's own sampling
   error after the tilt. On the test records, epochs 4 to 30 differ from
   each other after the tilt by less than the truth's sampling error on
   these measures. The model with every input hidden,
   which knows nothing about the household, goes from missing mean net
   worth by income × age by 269 percent to 9 percent (sampling error 6).
   The four target families pin these statistics down.

2. **Relationships the targets do not touch are not fixed by the tilt;
   they come from pretraining.** Two kinds showed this:
   - *Between wealth items.* The share of pass-through owners with an
     unrealized gain is 90 percent; epoch 4 has it 34 points too low before
     the tilt and 33 points too low after. Joint holding of pairs over all
     190 pairs ends at 0.34 points for epoch 4 against 0.14 for the best
     model.
   - *Between wealth and particular incomes.* The incomes-hidden model,
     tilted, gets income groups right, but the correlation of net worth
     with dividends is 0.27 against 0.44, and with interest 0.25 against
     0.41. The income-group targets carry wages (the bulk of income) but
     not the other items.

3. **Large tilts make some untargeted relationships worse.** From epoch 3
   down (tilt 1.8 nats and more), the tilt pushes holding of each item
   toward its target separately and breaks links between items: mortgage
   among homeowners goes from right (+0.8, 0.0 points) to 11 and 16 points
   too low; unrealized gain among pass-through owners from 40 to 64 points
   too low.

4. **The tilt size is a partial warning.** Along the training ladder it
   tracks the untargeted errors: at 0.6 nats or less (epoch 8 on) every
   untargeted measure is at or near the truth's sampling error; at 0.7 to
   1.0 nats (epochs 4 to 6) the aggregate measures are fine but particular
   pairs of items are off by 4 to 33 points; at 1.8 nats or more the tilt
   damages relationships. But the tilt size only sees errors along the
   targeted groupings. The incomes-hidden model has a smaller tilt (0.80
   nats) than epoch 4 (1.01) and the worst correlations with particular
   incomes of any model with a tilt below 1.3. A model can be wrong in a
   direction the targets do not measure, and the tilt will not show it.

5. **The completion score is a weaker guide still.** The models with
   inputs hidden score only 0.8 and 1.2 nats per record worse than the best
   model (most of the score is how well each wealth item is predicted from
   the others), yet before the tilt they are among the worst on every
   untargeted table except joint holding of pairs.

For the plan: on this evidence, pretraining matters beyond the targets for
exactly the relationships no target covers, and in the PUF arm those are
the ones of interest (wealth against particular PUF items, and the mix of
wealth items within a household). A tilt of about 0.6 nats per record or
less, with the model trained to near its best epoch, left them close to the
truth here; anything a model has not learned along a direction the targets
do not measure stays wrong after the tilt, whatever the tilt's size. Each
such relationship needs either its own target or a held-out check.

## Caveats

- The truth is the training records, which the models were trained on.
  Later epochs fit them more closely (training-record completion score 27.6
  at epoch 30 against 29.6 at epoch 15), which favours them on the training
  rows; the test-record table does not have that advantage.
- One training run, plus a second short run for epochs 3, 5 and 6. Seed-
  to-seed differences in test 3 were small (completion scores within 0.06
  nats) but were not measured for the ladder.
- Four target families only (test 3's core set). Test 3's full set also
  groups by each income item; with it, the incomes-hidden model's
  correlation errors would probably be targeted away. Not run.
- Eight draws per record for the evaluation; the draw noise yardstick shows
  this is small next to the differences reported.
- The "incomes hidden" and "all inputs hidden" models are the best network
  run with inputs withheld, not separately trained models.

## Files

Code in this folder: `q_train.py` and `q_train.sbatch` (training with
saved epochs), `q_common.py` (records, income groups, the four target
families on the version 2 data; reuses piece A's `a_common.py` without
editing it), `q_build.py`, `q_fit.py` (completion score, draws before,
tilt fit, draws after, on training and test records), `q_eval.py`
(statistics and yardsticks), `q_table.py`, `q_extra.py`, `q_gpu.sbatch`,
`q_cpu.sbatch`. Full tables: `q_tables_full.md`.

Scratch (`/nfs/roberts/scratch/pi_nrs36/jar335/probes/q/`): checkpoints in
`model/` (job 28922153) and `model_b/` (job 28923766); records and targets
in `a/`; one folder per model in `runs/` (draws, multipliers, report;
jobs 28922633–28922637, 28922883–28922885, 28923169–28923170,
28924122–28924124); evaluations in `eval/`; logs in `../logs/`.

## Unfinished

Nothing is running. Not done: a second training seed for the ladder; the
full target set (income-item families); the same exercise on PUF records
(no truth there); a version where the weak model is trained without the
incomes rather than having them withheld at completion.
