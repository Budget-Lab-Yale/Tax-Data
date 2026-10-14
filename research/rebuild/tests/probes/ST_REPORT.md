---
title: "Test 4, probe ST — stretching the model to a year it was not trained on (SCF 2016)"
role: report
workstream: rebuild
status: current
updated: 2026-10-11
sot: research/rebuild/joint_model_plan.md
supersedes: []
superseded_by: null
---

# Probe ST: does post-training stretch the model to another year?

## The question

The plan builds each year by prompting one model with that year, turning
ranks into dollars with the year's own distributions, and post-training to
the year's targets ("Years" in `joint_model_plan.md`). For future years
nothing in the training data comes from the year itself. Does that work?
The SCF has two waves, 2013 and 2016, so it can be tested where the truth
is known: train without 2016, build 2016, compare with the real 2016.

## What was run

**Model trained without 2016.** Test 3's version 2 data with the SCF 2016
rows removed from the training and validation splits: 166,156 PUF 2015
rows and 22,963 SCF 2013 rows (implicate rows) remain for training, 24,481
SCF 2016 training rows are dropped (`st_prepare.py`, job 29097829). Trained
with test 3's recipe unchanged (`../posttrain/h_train.py` as in
`h_train.sbatch`), three seeds (`st_train.sbatch`, jobs 29097830–32). The
year enters the model as a number, so 2016 lies outside the years it saw
(2013 for the SCF, 2015 for the PUF).

**Building 2016.** The model completes the 24 wealth variables of the real
SCF 2016 records, given their real incomes and demographics, with the year
set to 2016. The model works in ranks (each variable's position within its
source and year), so it predicts 2016 wealth ranks; those ranks become
dollars through the **SCF 2016 quantile functions** (the 2016 dollar value
at each rank). This follows the plan: each year's levels come from that
year's own distributions, which are part of the year's information like
the targets. So the test is of how wealth is arranged across households in
2016 given its levels, not of the levels themselves. The truth is each
record's true wealth tokens decoded the same way, so decoding does not
enter the comparison.

**Post-training.** Probe Q's tilt (test 3's method 2, `c_tilt.py`
unchanged, driven by `st_fit.py`, a copy of `q_fit.py`): one number added
to the model's log-probabilities per target cell. Targets are 2016 tables:
each wealth variable's distribution over 13 or 14 coarse bins (zero,
negative, rank bands among holders) within income group (bottom 50
percent, 50–90, 90–99, 99–99.9, top 0.1, by the sum of the eleven incomes
the SCF measures), age range (6), filing status (4) and children under 17
(4), built from the SCF 2016 training records' true wealth. Fitted on 16
draws per record; evaluated on 8 fresh draws per record. Records are the
SCF training-split rows of both waves, as in probe Q; only the 2016 rows
are evaluated (the 2013 rows are tilted to their own 2013 tables, which
does not affect the 2016 cells). The test-split 2016 records (households
neither model saw) are tilted with the same numbers and compared with
their own truth.

**Size of the tilt** is the KL divergence of the tilted model from the
pretrained one, nats per 2016 record, population-weighted (as probe Q).
**Effective sample** is the share of the pretrained model's draws that
would carry the tilted distribution if one reweighted them instead: one
over the average ratio of tilted to pretrained probability, over the
tilted draws (1 means no change; 0.5 means half the draws carry the
weight).

**Comparators.**

1. The model trained with 2016 included (test 3's version 2 checkpoints,
   three seeds), without and with the same tilt. It saw the 2016 training
   records, which favours it on the training-record tables; the test-record
   tables do not have that advantage.
2. The model trained without 2016, without post-training.
3. The same, tilted to the 2016 tables.
4. The simplest alternative: the SCF 2013 training records themselves
   (20,595 rows with wealth observed), reweighted by entropy calibration
   (new weights as close as possible to the old in the KL sense) to the same
   2016 tables (`st_calib.py`). Their 2013 ranks are treated as 2016 ranks
   and decoded with the 2016 quantile functions, the same as the model.
   Some 2016 cells cannot be reached by any 2013 record, so the calibration
   carries a small ridge penalty (it misses each cell by the ridge times the
   cell's multiplier). The version reported in the tables puts the ridge
   (10⁻⁴) on cells as shares of the whole population; a second version with
   the ridge on shares within each group is in the collector's file and
   does worse (2.0 nats, effective sample 4 percent). A ridge of 10⁻⁶ did
   not converge and was dropped. Also shown: the 2013 records as they are,
   which measures how much 2016 differs from 2013 in rank terms.

**Untargeted statistics** (probe Q's `q_eval.py`, on the 2016 rows):
holding rates by income group × age range; mean net worth by income group
× age range; joint holding of each pair of the 20 asset and debt items, and
four named relationships (mortgage among homeowners and three others);
the top 1 percent's share of net worth within each income group; mean net
worth by household type (filing status × any children); rank correlation
of net worth with each of the eleven incomes. Yardstick: the truth's own
sampling error (two random halves of the households, half the
difference).

**Harder stretch.** A "future" with a known truth: the 2016 records with
the weight of every row whose net worth is in the top 1 percent of its
income group multiplied by 2 (and, for one seed, by 4). This makes wealth
more concentrated within income groups than in 2016, a larger change than
2013→2016. The tables of these reweighted records are the targets and
their statistics the truth; the model trained without 2016, the model
trained with 2016 (seed 1) and the reweighted 2013 records are stretched
to them in the same way.

## Results

All numbers are from the collector's file
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/st/ST_TABLES.md` (job 29098330,
from `eval/eval_base.json`, `eval_top2.json`, `eval_top4.json`). Model rows
average three seeds unless marked; the seed-to-seed range is in that file.

### How far 2016 is from 2013 in rank terms

The 2013 records as they are, decoded with the 2016 quantile functions,
already miss the 2016 targeted cells by only 0.87 points, against a
sampling error of 0.56 (training records). Because each year's levels come
from its own quantile functions, what has to be stretched is the
arrangement of wealth across households, and that changed little between
2013 and 2016. This is a mild test; the harder stretch below is larger at
the top.

### Main test: building 2016, SCF 2016 training records

Error against the 2016 truth, before / after post-training where both
apply. Measures as probe Q: targeted cells and holding rates in percentage
points; mean net worth by income × age as a percent of the income group's
true mean; household type as mean absolute percent error; correlation as
mean absolute difference over the eleven incomes. Effective sample of the
tilt is record by record (see "What was run"); pooled over all records it
is near zero for every model, because of the top income groups.

| Comparator | Tilt or reweighting size (nats per record) | Effective sample | Completion score, 2016 records (nats) | Targeted cells | Holding rates, income × age | Mean net worth, income × age (%) | Joint holding of pairs | Top 1% share within income group | Mean net worth by household type (%) | Correlation of net worth with incomes |
|---|---|---|---|---|---|---|---|---|---|---|
| SCF 2013 records as they are | | | | 0.87 | 3.0 | 15 | 0.61 | 1.0 | 10 | 0.028 |
| SCF 2013 records reweighted to the 2016 tables | 1.31 | 12% | | 0.40 | 2.3 | 13 | 0.31 | 5.2 | 30 | 0.033 |
| Model trained without 2016 | 0.76 | 37% | 31.81 | 0.82 / 0.09 | 2.8 / 1.2 | 58 / 8 | 0.86 / 0.22 | 6.8 / 1.4 | 39 / 6 | 0.018 / 0.014 |
| Model trained with 2016 (test 3) | 0.49 | 53% | 29.66 | 0.60 / 0.09 | 1.9 / 1.1 | 27 / 6 | 0.40 / 0.15 | 5.7 / 1.2 | 17 / 4 | 0.013 / 0.009 |
| Truth's own sampling error | | | | 0.56 | 1.7 | 7 | 0.25 | 1.3 | 5 | 0.013 |

The model trained with 2016 saw these records in training (its completion
score on them is 2.2 nats better). On test records neither model saw, the
gap in completion score is 0.48 nats (32.64 against 32.16).

### Main test: SCF 2016 test records (households neither model saw)

Same measures. The test records are tilted with the numbers fitted on the
training records, so targeted cells cannot be hit better than the
difference between the two samples (about 1.2 points). With 1,125
households the sampling error is large and most differences below are
inside it.

| Comparator | Targeted cells | Holding rates, income × age | Mean net worth, income × age (%) | Joint holding of pairs | Top 1% share within income group | Mean net worth by household type (%) | Correlation of net worth with incomes |
|---|---|---|---|---|---|---|---|
| SCF 2013 records as they are | 1.32 | 4.4 | 18 | 0.90 | 2.7 | 27 | 0.039 |
| SCF 2013 records reweighted to the 2016 tables | 1.24 | 4.3 | 20 | 0.74 | 6.9 | 56 | 0.044 |
| Model trained without 2016, before / after | 1.30 / 1.22 | 4.2 / 3.6 | 52 / 18 | 1.08 / 0.60 | 9.1 / 3.6 | 52 / 20 | 0.022 / 0.023 |
| Model trained with 2016, before / after | 1.20 / 1.23 | 3.9 / 3.6 | 30 / 18 | 0.77 / 0.58 | 7.9 / 4.5 | 34 / 24 | 0.026 / 0.021 |
| Truth's own sampling error | 1.04 | 3.3 | 21 | 0.62 | 1.3 | 20 | 0.024 |

### Levels by income group, 2016 training records (untargeted)

Mean net worth, $ thousands:

| | Bottom 50% | 50–90% | 90–99% | 99–99.9% | Top 0.1% |
|---|---|---|---|---|---|
| Truth 2016 | 113 | 388 | 2,350 | 13,689 | 63,036 |
| SCF 2013 records as they are | 129 | 412 | 2,285 | 13,225 | 61,353 |
| SCF 2013 records reweighted | 123 | 412 | 2,386 | 15,058 | 120,140 |
| Model trained without 2016, before | 191 | 587 | 2,992 | 15,601 | 53,716 |
| Model trained without 2016, after | 120 | 407 | 2,460 | 14,158 | 61,812 |
| Model trained with 2016, after | 117 | 403 | 2,427 | 13,600 | 62,287 |

Median net worth, $ thousands:

| | Bottom 50% | 50–90% | 90–99% | 99–99.9% | Top 0.1% |
|---|---|---|---|---|---|
| Truth 2016 | 17 | 154 | 1,234 | 9,066 | 39,177 |
| SCF 2013 records as they are | 20 | 165 | 1,084 | 5,812 | 28,854 |
| SCF 2013 records reweighted | 19 | 165 | 1,076 | 7,683 | 36,537 |
| Model trained without 2016, before | 26 | 170 | 1,232 | 8,207 | 28,847 |
| Model trained without 2016, after | 22 | 159 | 1,167 | 8,292 | 37,773 |
| Model trained with 2016, after | 18 | 152 | 1,098 | 7,846 | 36,426 |

Top 1 percent's share of net worth within the income group, %:

| | Bottom 50% | 50–90% | 90–99% | 99–99.9% | Top 0.1% |
|---|---|---|---|---|---|
| Truth 2016 | 22.3 | 16.2 | 14.0 | 11.9 | 14.1 |
| SCF 2013 records as they are | 22.5 | 19.4 | 13.4 | 12.5 | 14.7 |
| SCF 2013 records reweighted | 23.1 | 18.9 | 15.1 | 4.7 | 0.0 |
| Model trained without 2016, before | 36.5 | 28.8 | 17.8 | 14.5 | 13.3 |
| Model trained without 2016, after | 23.0 | 18.0 | 14.7 | 12.9 | 11.9 |
| Model trained with 2016, after | 22.8 | 18.0 | 15.1 | 12.0 | 12.5 |

The reweighted 2013 records put a few households at the top on very large
weights (the largest weight is 3,625 times its original): mean net worth in
the top 0.1 percent doubles, and in the top two groups single records carry
so much of the group's weight that the measured top 1 percent share falls
to 5 and 0 percent.

Relationships between wealth items, % of holders (untargeted):

| | Mortgage among homeowners | Equities among pass-through owners | Unrealized gain among pass-through owners | Other-property mortgage among its owners |
|---|---|---|---|---|
| Truth 2016 | 68.0 | 31.3 | 89.6 | 40.8 |
| SCF 2013 records as they are | 69.3 | 34.0 | 90.6 | 38.3 |
| SCF 2013 records reweighted | 68.0 | 36.0 | 89.8 | 40.9 |
| Model trained without 2016, before / after | 69.7 / 67.9 | 36.7 / 34.9 | 93.2 / 88.9 | 36.9 / 39.3 |
| Model trained with 2016, before / after | 68.2 / 67.9 | 34.9 / 33.6 | 92.4 / 89.6 | 39.4 / 40.3 |

Rank correlation of net worth with particular incomes (untargeted):

| | Wages | Schedule E | Long-term gains | Dividends | Taxable interest | Tax-exempt interest |
|---|---|---|---|---|---|---|
| Truth 2016 | 0.16 | 0.26 | 0.22 | 0.46 | 0.40 | 0.23 |
| SCF 2013 records as they are | 0.21 | 0.27 | 0.16 | 0.42 | 0.43 | 0.23 |
| SCF 2013 records reweighted | 0.17 | 0.30 | 0.14 | 0.43 | 0.45 | 0.25 |
| Model trained without 2016, after | 0.15 | 0.25 | 0.22 | 0.42 | 0.38 | 0.22 |
| Model trained with 2016, after | 0.15 | 0.26 | 0.22 | 0.43 | 0.40 | 0.22 |

### Where the tilt is spent

Tilt size by income group, nats per record (training records; three-seed
average from the evaluation files), and the record-by-record effective
sample:

| | Bottom 50% | 50–90% | 90–99% | 99–99.9% | Top 0.1% |
|---|---|---|---|---|---|
| Model trained without 2016: tilt size | 0.56 | 0.75 | 1.35 | 2.88 | 5.21 |
| Model trained with 2016: tilt size | 0.36 | 0.48 | 0.92 | 2.24 | 4.22 |
| Model trained without 2016: effective sample | 44% | 37% | 18% | 4% | 0% |
| Model trained with 2016: effective sample | 59% | 52% | 32% | 8% | 1% |

Most of the tilt, for both models, is the top income groups, where even
the model that saw 2016 needs a large correction (as in probe Q). Not
having seen 2016 adds about 0.2 nats in the bottom half and about 1 nat in
the top 0.1 percent.

### Harder stretch: more concentrated wealth within income groups

The truth is the 2016 training records with the top 1 percent of net
worth in each income group weighted ×2 (or ×4). Its top 1 percent share
rises by 1–4 points (×2: 26.6, 19.2, 16.8, 13.5, 14.7 percent by income
group, against 2016's 22.3, 16.2, 14.0, 11.9, 14.1).

| Comparator (training records) | Tilt or reweighting size | Targeted cells | Holding rates, income × age | Mean net worth, income × age (%) | Joint holding of pairs | Top 1% share within income group | Mean net worth by household type (%) | Correlation of net worth with incomes |
|---|---|---|---|---|---|---|---|---|
| ×2: SCF 2013 records reweighted | 1.30 | 0.40 | 2.4 | 12 | 0.31 | 5.8 | 27 | 0.031 |
| ×2: model trained without 2016, after (3 seeds) | 0.76 | 0.09 | 1.3 | 8 | 0.22 | 1.6 | 3 | 0.015 |
| ×2: model trained with 2016, after (1 seed) | 0.49 | 0.09 | 1.2 | 6 | 0.15 | 1.6 | 2 | 0.010 |
| ×2: truth's own sampling error | | 0.56 | 1.7 | 9 | 0.25 | 1.5 | 6 | 0.015 |
| ×4: SCF 2013 records reweighted | 1.33 | 0.41 | 2.5 | 13 | 0.33 | 4.4 | 28 | 0.031 |
| ×4: model trained without 2016, after (1 seed) | 0.79 | 0.09 | 1.3 | 11 | 0.23 | 3.1 | 4 | 0.015 |
| ×4: model trained with 2016, after (1 seed) | 0.52 | 0.09 | 1.2 | 7 | 0.17 | 3.3 | 4 | 0.010 |
| ×4: truth's own sampling error | | 0.57 | 1.8 | 10 | 0.27 | 1.6 | 7 | 0.017 |

Top 1 percent's share within the income group under ×4, %:

| | Bottom 50% | 50–90% | 90–99% | 99–99.9% | Top 0.1% |
|---|---|---|---|---|---|
| Truth (×4) | 28.3 | 20.4 | 17.5 | 12.5 | 13.6 |
| Model trained without 2016, after | 26.6 | 20.9 | 19.0 | 20.7 | 10.2 |
| Model trained with 2016, after | 30.3 | 23.0 | 21.1 | 18.1 | 11.0 |
| SCF 2013 records reweighted | 25.4 | 21.3 | 18.1 | 6.5 | 1.8 |

At ×2 both models, tilted, follow the more concentrated truth as well as
they follow 2016 itself. At ×4 the top 1 percent share is where it breaks,
in both models alike (3.1 and 3.3 points against a sampling error of
1.6), mostly in the 99–99.9 group (21 and 18 percent against 12.5). The
tilt barely grows (0.79 against 0.76 nats): the targets are each item's
distribution over coarse rank bands, and the change is inside the top
band and in how items combine within a household, which the tables do not
describe. Dollar levels within the top band come from the fixed 2016
quantile functions.

## What it means for stretching the model to future years

1. **On this test, stretching works as well as having the year in
   training.** The model that never saw 2016, tilted to 2016's tables,
   matches the model that did see 2016, tilted the same way, on every
   untargeted statistic to within about the truth's sampling error, on the
   training records and on unseen households. Both land near the sampling
   error after the tilt.

2. **Not having the year costs tilt, mostly at the top.** 0.76 nats per
   record against 0.49, and an effective sample of 37 percent against 53
   percent. Probe Q found untargeted statistics held up to about 0.6 nats
   along the epoch ladder; here 0.76 nats spent on a year shift did no
   visible harm. Without the tilt, the model trained without 2016 is
   clearly worse (mean net worth by income × age off by 58 percent).

3. **The model beats reweighting the old survey.** Reweighting the 2013
   records to the same tables needs 1.3 nats per record, keeps an
   effective 12 percent of them, and breaks the top: a few households
   carry thousands of times their weight, mean net worth in the top 0.1
   percent doubles, net worth by household type misses by 30 percent. The
   model tilts each household's wealth instead of the household weights,
   so no household becomes dominant.

4. **The test is mild, and the hard part is what the targets do not
   describe.** In rank terms 2016 is close to 2013 (the 2013 records as
   they are miss the targets by 0.87 points against a sampling error of
   0.56), and the quantile functions of the target year are taken as
   known. A change concentrated within the top rank band (the ×4 case) is
   not fixed by any of the methods, because none of the targets sees it.
   For future years this means: the projected quantile functions carry the
   levels, the tables carry the shape, and anything outside both — the
   concentration within the top band, the mix of items within a household
   — stays as the pretrained model has it. Those need their own targets if
   a projection is expected to change them.

## Caveats

- One pair of waves, three years apart. A projection ten years out is a
  larger stretch, and the year input has never been asked to extrapolate
  that far.
- The 2016 quantile functions were taken from the 2016 data. In a real
  projection they are themselves projected, and their errors pass straight
  into dollar levels; this probe does not test that.
- Targets were the SCF's own tables with the four target families of probe
  Q; no PUF arm, no administrative totals.
- The harder stretch is constructed by reweighting real records, so its
  truth is consistent by construction; a real change in concentration
  could move incomes and wealth together in ways this does not imitate.
- Harder stretch: ×2 used three seeds of the model trained without 2016
  and one of test 3's model; ×4 one seed of each.
- The model trained with 2016 is evaluated on records it trained on (the
  training-record table) and on unseen records (the test-record table);
  the conclusion holds on both.

## To finish

Nothing is pending: every job of the chain finished before this report was
written (01:25). The jobs, for the record (logs in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/`, list in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/st/chain_jobs.txt`):

| Jobs | What | Output | Fills |
|---|---|---|---|
| 29097829 | data without SCF 2016 | `st/data_no2016/` | — |
| 29097830–32 | training, seeds 1–3 | `st/model/` | all model rows |
| 29098112–14 | tilt, test 3's model, seeds 1–3 | `st/runs/full_s*/` | main tables |
| 29098314, 29098316, 29100116 | tilt, model without 2016, seeds 1–3 (29100116 replaces 29098318, which ran out of GPU memory on a node another process was using) | `st/runs/held_s*/` | main tables |
| 29098315, 29098317, 29098319, 29098321 | ×2 harder stretch | `st/runs/*_top2/` | harder stretch |
| 29098320, 29098686 | ×4 harder stretch (29098686 replaces 29098322, same failure) | `st/runs/*_top4/` | harder stretch |
| 29101048, 29101050, 29101052; 29101520–22 | reweighting the 2013 records (within-group and population ridge) | `st/calib/` | reweighted rows |
| 29098327, 29101523, 29098329 | evaluation, base / ×2 / ×4 | `st/eval/eval_*.json` | all tables |
| 29098330 | collector | `st/ST_TABLES.md` | all tables |

Not done and worth doing: the same test on the PUF side (no second PUF
year in the data yet); a tilt with targets inside the top rank band (finer
top bins, or the top 1 percent share itself as a target), to see whether
the ×4 case is then fixed; ×4 with more seeds.

## Files

Code in this folder: `st_prepare.py` (data without 2016), `st_train.sbatch`
(training), `st_common.py` (probe Q's set-up pointed at this probe's
scratch folder; the harder-stretch reweighting), `st_fit.py` (a copy of
`q_fit.py`: completion score by wave, draws before, tilt fit, draws after,
and the log ratio of tilted to pretrained probability for the effective
sample), `st_calib.py` (entropy calibration of the 2013 records),
`st_eval.py` (probe Q's statistics on the 2016 rows; targeted-cell error
over 2016 cells only), `st_table.py` (collector), `st_submit.sh` (the
chain), `st_gpu.sbatch`, `st_cpu.sbatch`. Scratch:
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/st/`.

