---
title: "Test 2 — evaluation of completed wealth: method and results"
role: results
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/scf_joint/SPEC.md
supersedes: []
superseded_by: null
---

# Test 2: evaluation of completed wealth

Part C of `SPEC.md`. The question is whether wealth filled in for records
that do not report it (the "completed" records) looks like the true wealth of
those records. In experiment A the records are SCF 2016 filers whose wealth
was hidden (half 1, the pretend PUF), so the truth is known. In experiment B
the check uses held-out SCF units (truth known) and, separately, PUF records
completed at 2015 (no truth; compared with the SCF on aggregates).

## 9 October: follow-up training and calibration checks

### Capacity and setup audit prompted by the memorization question

An isolated overfit probe selected 64 SCF training rows with distinct,
complete shared contexts, hid all 24 wealth fields, and trained the existing
six-layer, width-256 transformer without dropout or regularization. It
reached 100% accuracy on all 1,536 wealth tokens (and on all 64 complete
wealth records) after 250 steps; initial wealth-token cross entropy was
5.12 and final cross entropy was below 0.001. Job `28891998` completed with
exit code zero. This confirms that the architecture can memorize the
tokenized records in a small in-sample check. It does not show generalization
or fidelity of decoded dollars. Reproduce with `b_overfit_probe.sbatch`;
result in `logs/b_overfit_result.json`.

The earlier completion-task training experiment also helps locate the
problem: when every SCF row trains with all wealth hidden, seed 1's training
loss fell from 0.91 at epoch 13 to 0.68 at epoch 20 while validation loss
rose from 1.65 to 2.07. Its best checkpoint was epoch 5 and had worse
held-out wealth completion NLL than the random-mask baseline (38.75 versus
35.51 per record). More direct task training alone therefore did not solve
generalization in this configuration.

The shared inputs are not interchangeable measurements: weighted nonzero
rates for interest/dividends are 34.8% of PUF returns versus 18.0% of SCF
filer units, and for capital gains 15.8% versus 8.3% (see
`a_crosswalk.md`). Source-year rank bins align positive holders' ranks but
do not repair the differing probability of an exact zero. This is a
concrete bridge problem for transferring an SCF wealth conditional to a
PUF record. A source embedding can learn that the income fields differ,
but no PUF record supplies a wealth label for estimating how the SCF
wealth conditional should change with that source embedding.

All scheduled follow-up jobs finished successfully: 15 training/evaluation
tasks, four-model oracle calibration, and six completion-order probes. Results
below are means over three training seeds. The follow-up models have the same
base size and use full population loss weights; `scf_weight` is hidden in
training. `b_followup_summary.py` prints the full ranges and other metrics.

| Model | Held-out SCF mean wealth error by income group | SCF wealth ownership gap (points) | PUF ownership gap against interpolated SCF (points) |
|---|---:|---:|---:|
| Source-labeled random masks | 20.8% | 2.30 | 4.98 as PUF; 2.58 if presented as SCF |
| No source label, random masks | 21.8% | 2.41 | 4.67 |
| No source label, corrected orderless masks | 24.1% | 2.49 | 5.21 |
| No source label, fixed-order objective | 16.4% | 2.33 | 4.18 |
| Raw DRF Stage 2 refit, before calibration | 16.3% | 1.90 | 1.46 |
| Survey-weighted DRF diagnostic | 7.2% | 1.84 | 1.14 |

The raw DRF row is the refitted production forest's donor draw before its
Stage 3 tilt and rescale, not the actual final production file. The
survey-weighted DRF row is an experimental change to the forest's training
and donor weights, not the production method. The 7.2% number must not be
presented as raw DRF performance. Both rows use the same underlying seven
income categories and demographic fields as the transformer; DRF engineers
more features from them (including total-income rank and eight hard income
cells), keeps percentile information in a different form, zeroes PUF
interest/dividends at $100 or below, and copies an observed SCF wealth
vector. On held-out SCF, where PUF–SCF measurement mismatch is absent from
the prediction task, raw DRF improves ownership and mean-wealth errors.
The fixed-order transformer essentially ties raw DRF on the latter single
measure (16.4% versus 16.3%) but has worse wealth-amount rank error (5.50
versus 4.54). Raw DRF has a worse top-1% wealth share (45.2% versus truth
37.3%, transformer 37.5%) and repeats another household's full balance
sheet for 56% of the generated top-1% records, versus 0% for the transformer.
The baseline transformer is competitive with raw DRF as an uncalibrated
generator; raw DRF does not dominate it. In the held-out SCF, the top 1%
by **income** holds 30.6% of net worth in truth, 25.1% in the transformer,
and 38.2% in raw DRF. Across the four income groups, the sum of absolute
share errors is 11.0 percentage points for the transformer and 15.2 for
raw DRF. Raw DRF remains closer on mean net worth by income group (16.3%
average relative error versus 20.8%) and on ownership rates by income.
The experimental survey-weighted DRF is stronger on several measures, but
is a different method. The companion `JOINT_DISTRIBUTION_VIEW.md` and HTML
show the levels and cross-variable relationships side by side.

The fixed-order objective improves the held-out mean-wealth error against
the other transformer variants, but its SCF amount-rank error worsens to
5.50 (source-labeled random masks: 5.00), its income–wealth rank-correlation
gap is 0.073 (baseline: 0.064), and its PUF amount gap against interpolated
SCF is 47.2% (presented-as-SCF baseline: 32.9%). It is not a general win.
The PUF columns compare aggregates, not individual truth: PUF wealth is
unobserved. The real PUF and SCF differ in unit, sampling and measurement,
so even matching their aggregates does not validate the unobserved joint.

On the clean split-SCF test, removing the source label helped relative to
using each artificial label, as detailed below. Corrected orderless masks
were similar or worse. Fixed-order masks lowered the ownership gap from
3.96 to 3.57 points against no-source random masks, but worsened the
income–wealth correlation gap from 0.053 to 0.126. These objectives with
source-specific missing fields are based on observed subsequences; the
fixed-order training loss is not an exact observed-data likelihood for a
single complete-data joint distribution.

Two diagnostics narrow the interpretation. First, decoding the *true*
held-out wealth tokens yields mean-wealth error by income group of 0.36%
(clean split) and 0.72% (real SCF test), so wealth-token decoding by itself
does not explain the model errors. This does not test information lost when
income was tokenized. Second, reversing only the order used to generate
wealth in the no-source random-mask model changes held-out SCF mean-wealth
error from 21.8% to 24.9% and its income-correlation gap from 0.066 to
0.079. The orderless model changes from 24.1% to 25.4% and from 0.069 to
0.084. The arbitrary conditional predictions are therefore not fully
consistent across sampling orders in this implementation.

The calibration diagnostic supplies the *true* SCF mean net worth for four
income groups as targets and reweights five generated completions per
record while preserving each record's total population weight. All twelve
model/seed cases were feasible. By construction the four targeted mean
errors become zero. For the source-labeled model, ownership error stays
2.30 to 2.32 points, amount-rank error moves 5.00 to 4.72, and the top
1% wealth share moves 37.5% to 37.1% (truth 37.3%). Effective draws remain
about 4.97 of five per record. This shows these four aggregate targets are
cheap to match *when their true values are supplied*; it does not test
whether targets in a future year are known or whether unmeasured tax–wealth
relationships are right.

The code and detailed outputs are under
`$S/followup_20261009/{model,eval}`; launchers are `b_followup.sbatch`,
`c_followup_tilt.sh`, and `b_order_probe.sbatch`. Slurm jobs `28887140`,
`28887348`, and `28887376` all completed with exit code zero. No
production default was changed.

## 9 October: removing the explicit source label

User-requested ablation on `expA_clean`: one shared transformer without a
source embedding, matched to `joint_w1` across seeds 1–3. Same data, full
population loss weights, model size, initial values of all shared
parameters, masking, stopping rule, and five completion draws. The source
metadata remains in the data; only its embedding is disabled. The unused
embedding remains allocated to preserve seeded initialization. This is
not a change to an observed-data-likelihood objective.

Three training seeds; mean [minimum, maximum]. Same 15,380 graded records; five draws each.
True top 1% wealth share: 39.159102967309934

| Metric | Source label, own source | Source label, presented as SCF | No source label | SCF half 2 only |
|---|---|---|---|---|
| Holding by income group: gap (pp) | 5.06 [4.33, 5.45] | 3.80 [3.45, 4.28] | 3.96 [3.77, 4.18] | 4.35 [3.84, 5.06] |
| Amounts by income group: rank gap | 6.04 [5.91, 6.12] | 5.81 [5.38, 6.34] | 5.68 [5.39, 6.06] | 5.67 [5.34, 6.22] |
| Income-wealth rank correlation: gap | 0.092 [0.079, 0.116] | 0.052 [0.047, 0.059] | 0.053 [0.050, 0.056] | 0.048 [0.045, 0.050] |
| Mean net worth by income group: error (%) | 53.7 [45.8, 62.8] | 45.8 [39.3, 53.5] | 44.3 [34.1, 53.4] | 36.4 [27.6, 44.1] |
| Top 1% share of net worth (%) | 34.3 [31.9, 36.2] | 39.7 [37.0, 44.5] | 39.1 [38.3, 40.0] | 40.3 [37.9, 41.8] |
| Classifier AUC (ideal 0.5) | 0.770 [0.754, 0.783] | 0.709 [0.700, 0.721] | 0.708 [0.692, 0.722] | 0.760 [0.754, 0.766] |

Removing the source label improves all three seeds' holding-rate errors,
amount-rank errors, income–wealth correlation errors, and classifier AUC
relative to completing under their original artificial source label.
Mean net worth by income group improves on average, but not in every seed.
The mean top 1% wealth share is almost exact and the three-seed range is
narrower. These seed ranges describe training variation, not confidence
intervals for the population.

The result largely matches the existing workaround of presenting records
as SCF: it is not a broad accuracy improvement over that workaround.
Nor does pooling beat the half-2-only transformer on every measure; the
mean-net-worth errors by income group remain substantial. The experiment
supports sharing across artificial source labels in a common population.
It does not establish transfer across real surveys with different
measurement or coverage, or identify relationships never jointly observed.
Missingness patterns can still reveal a record's source.

Reproduce with `sbatch b_no_source.sbatch`; summarize with
`python3 b_no_source_summary.py`. Array job `28886439`, seeds 1–3.
Artifacts: `$S/no_source/model/expA_clean` and
`$S/no_source/eval/expA_clean`; logs:
`$S/logs/b_no_source_28886439_<seed>.{out,err}`. Baseline artifacts are
unchanged. Checks passed for old checkpoint loading, source-invariant
logits, checkpoint round trip, identical initialization, and matching
training arguments and record counts. Input file timestamps predate the
baseline checkpoints.

All three jobs completed with exit code zero. For each seed, all 87,215
output rows (17,443 records × five draws, including records excluded from
grading because true wealth is unavailable) are exactly identical when
completed under their own source label versus relabeled SCF, including
decoded amounts.

## Summary

See the dated notes in `STATUS.md` for what has been run. The tables below
the line "Results by dataset" are regenerated by `c_compare.py` each time a
new evaluation finishes, so they may be newer than this summary.

### Summary as of 14:50, 9 October (piece C)

All completed files that B and D had written by 14:45 are evaluated,
including D's rerun production forest (`drf`) and its survey-weighted
variant (`drfsw`), and B's model trained with each record's loss weighted
by its population weight (`w1`). Numbers are means over three training
seeds. "Sampling noise" is the gap between two random halves of the true
records; a perfect imputation would show about half of it (see "How to read
the tables"). The earlier model configuration (survey weight as a model
variable, unweighted loss) and other variants are in each dataset's "Other
variants" table.

**Experiment A, clean case** (SCF 2016 filers, half 1's wealth hidden and
completed; 15,380 records graded; 2,063 split-off unit records without true
wealth left out):

| | Sampling noise | Joint model, own source | Joint model, half 1 presented as SCF | Half-2-only model | Production forest | Survey-weighted forest | Sequential trees |
|---|---|---|---|---|---|---|---|
| Share holding by income group: average gap (points) | 3.5 | 5.1 | 3.8 | 4.4 | 2.7 | 2.3 | 6.9 |
| Amounts among holders by income group: average gap (percentile ranks) | 6.6 | 6.0 | 5.8 | 5.7 | 5.6 | 5.0 | 13.2 |
| Rank correlation of amount with income: average gap | 0.076 | 0.092 | 0.052 | 0.048 | 0.042 | 0.038 | 0.306 |
| Holders in their component's top 1% rank band, completed / true | (1.0) | 1.3 | 2.0 | 1.9 | 1.7 | 1.4 | 0.9 |
| Top 1% share of net worth (truth 39.2%) | ±2.6 | 34.3% | 39.7% | 40.3% | 44.7% | 40.4% | 26.6% |
| Mean net worth by income group: average gap (%) | 12 | 54 | 46 | 36 | 28 | 11 | 87 |
| Top 1% records whose balance sheet repeats another household's | 0% | 0% | 0% | 0% | 80% | 73% | 0% |
| Classifier AUC (0.5 = cannot tell) | 0.53 | 0.77 | 0.71 | 0.76 | 0.80 | 0.79 | 0.78 |

**Experiment B, held-out SCF units** (2013 and 2016; 12,021 records graded):

| | Sampling noise | Joint model (PUF + SCF) | SCF-only model | Production forest | Survey-weighted forest | Sequential trees |
|---|---|---|---|---|---|---|
| Share holding by income group: average gap (points) | 3.5 | 2.3 | 2.6 | 1.9 | 1.8 | 4.3 |
| Amounts among holders by income group: average gap (percentile ranks) | 6.7 | 5.0 | 5.1 | 4.5 | 4.1 | 9.0 |
| Top 1% share of net worth (truth 37.3%) | ±1.8 | 37.5% | 43.0% | 45.2% | 39.8% | 27.1% |
| Mean net worth by income group: average gap (%) | 12 | 21 | 25 | 16 | 7 | 38 |
| Top 1% records whose balance sheet repeats another household's | 0% | 0% | 0% | 56% | 58% | 0% |
| Classifier AUC | 0.51 | 0.60 | 0.61 | 0.62 | 0.63 | 0.63 |

**Experiment B, PUF records completed at 2015** (no truth; gaps against the
SCF interpolated to 2015; the SCF's own 2013-to-2016 change as the scale):

| | SCF 2013 vs 2016 | Joint model, as PUF | Joint model, PUF presented as SCF | Production forest | Survey-weighted forest | Sequential trees |
|---|---|---|---|---|---|---|
| Share holding by income group: average gap (points) | 2.1 | 5.0 | 2.6 | 1.5 | 1.1 | 3.9 |
| Amounts among holders by income group: average gap (%) | 31 | 56 | 33 | 20 | 15 | 85 |
| Top 1% share of net worth (SCF interpolated 37.1%) | | 28.3% | 37.3% | 44.1% | 39.6% | 27.9% |

What this shows:

1. **Training the model with the population weights fixed the top.** In the
   earlier configuration (survey weight as a model variable, records
   unweighted) the model put about three times too many holders in each
   component's top 1% rank band, because the SCF sample over-represents the
   wealthy (counted by sample records, 2–18% of each component's holders sit
   in its top 1% band), and the top 1% held 60–73% of net worth against
   39%. With the loss weighted by the population weight, the top 1% share
   is 34–40% in experiment A and 37.5% in experiment B (truth 39% and 37%).
2. **Holding and the middle of the amounts are near sampling noise for the
   models and the forests**; the forests are somewhat closer on holding by
   income group. The sequential trees lose most of the link between
   amounts and income.
3. **Combining sources helps a little in experiment B and not in
   experiment A.** On held-out SCF units the joint model is slightly better
   than the SCF-only model, most clearly at the top (37.5% against 43.0%).
   In experiment A the joint model completing half 1 as its own source is
   worse than the half-2-only model; presenting half 1 as SCF brings it
   level. For PUF records the same choice matters: presented as PUF, the
   completions miss holding and amounts by income group by more than twice
   the SCF's own three-year change; presented as SCF they are close to it
   and the top 1% share matches the SCF (37.3% against 37.1%).
4. **The forests copy balance sheets at the top**: 56–80% of their top 1%
   by net worth repeat a donor household's exact combination of wealth
   bands. The survey-weighted forest is the closest method on most
   aggregate rows; its cost is this copying.
5. **No method is indistinguishable from the truth** to the classifier
   (AUC 0.60–0.63 in experiment B, 0.71–0.80 in experiment A, against 0.51–0.53
   between two halves of the truth).
6. **The income-measurement variants** (`expA_noise`, `expA_recat`,
   `expA_unrep`) are in the generated blocks below; they were not yet
   re-summarized for the weighted runs.

## How to read the tables

Each table has the truth, two yardsticks, and one column per method.

- **Sampling noise.** The true records' households are split at random into
  two halves, and each statistic is computed on each half; the column shows
  the typical gap between the halves, averaged over five different splits.
  It answers: how much does this statistic move from sampling alone, at this
  sample size? **This yardstick is generous.** Each half has half the
  records, and the two halves differ in their incomes as well as their
  wealth, while a completion is compared with the truth on the very same
  records. On fabricated data where the true model is known (below), a
  perfect imputation showed gaps about **half** the size of this column. So
  a method is close to the best achievable when its gaps are around half of
  the sampling-noise column, not when they equal it.
- **No information (shuffled).** Every record keeps its incomes, age and
  weight, and takes the whole wealth vector (all 24 components together) of
  a random other record, drawn by weight. Each component's overall
  distribution and the patterns across components stay right; the link
  between wealth and income or age is gone. This is the bad end of the scale
  for anything that depends on income or age. For measures of how the wealth
  components relate to each other it is not a bad end (it keeps those
  relationships exactly), and for cloning it is the worst case (every
  completed balance sheet is a copy).
- **Method columns.** One per method: the joint model, the one-source model
  (trained without the other source), the production forest and the
  sequential trees. A method run with several training seeds shows the mean
  over seeds and, in brackets, the lowest and highest seed; that range is
  the training-noise yardstick.
- **Completion noise.** A separate table compares one completion draw with
  another from the same file (same model, same records): the part of each
  gap that comes from drawing at random.

All statistics are weighted by the records' population weights (each SCF
implicate at one fifth of its household's weight; each completion draw at
one fifth of its record's weight when there are five draws).

## The measures

- **Income group.** The weighted rank of the total of the seven incomes
  (wages, business, interest and dividends, capital gains, rent, Social
  Security and pensions, unemployment and other) within source and year,
  grouped as bottom 50%, 50–90%, 90–99%, top 1%. **Age range** is the PUF
  age range of the primary filer (SCF: the unit's head).
- **Share holding.** For each of the 24 components (14 assets, 6 debts,
  4 unrealized gains), the weighted share of records with a nonzero amount,
  overall, by income group and by age range. Reported as the gap in
  percentage points, averaged over components (and groups).
- **Amounts among holders.** The 10th, 25th, 50th, 75th and 90th weighted
  percentiles of each component's positive amounts, by income group and by
  age range, in two units: in rank (the percentile rank of that amount among
  all positive amounts of the component, from the token's rank band; the gap
  is in percentile points) and in dollars (the gap as a percentage). Cells
  with fewer than 30 holder records on either side are left out.
- **The top of each component, in rank.** The share of a component's
  holders whose completed amount falls in the top 1% (and top 0.1%) rank
  band of that component's positive amounts. Weighted, the truth is about
  1% (0.1%) by construction; the headline shows completed divided by true,
  averaged over components. The same shares counted over sample records
  (unweighted) show how much the SCF sample over-represents the top. This
  measure does not depend on turning ranks back into dollars, so it
  separates a model problem at the top from a decoding problem.
- **Patterns of holding.** Each record has a pattern: which of the 24
  components it holds. The measure is the share of weight that would have
  to move to make the completed patterns' frequencies match the true ones
  (total variation distance: half the sum of absolute differences in
  shares). With about 900 distinct patterns in 17,000 records this has a
  large sampling floor; read it against the yardstick.
- **Pairs of components.** For each of the 276 pairs, the two-by-two table
  of holding (both, first only, second only, neither); the share misplaced,
  averaged over pairs.
- **Conditional holding rates.** Twelve rates readable on their own, such
  as homeownership among stockholders, a mortgage among homeowners, business
  assets among units with business income, and unrealized gains on other
  assets among units with realized capital gains.
- **Rank correlations.** Among holders of a component, the rank correlation
  of its amount with total income (does wealth rise with income as it
  should); and among records holding both of a pair, the rank correlation
  of the two amounts. Computed where the truth has at least 200 such
  records.
- **Net worth.** The 14 assets minus the 6 debts, in dollars (the four
  unrealized-gain variables are not part of net worth, as in production):
  percentiles, mean, and the shares held by the top 10%, 1% and 0.1%; mean
  and median by income group.
- **Cloning.** Among the top 1% by net worth, the share whose combination
  of the 24 wealth tokens is shared with a record of a different household
  in the same top 1% (implicates and draws of the same household do not
  count). A donor-based method copies whole balance sheets; a model that
  draws each component should rarely repeat one.
- **Classifier test.** A gradient-boosted classifier (an ensemble of
  decision trees fitted one after another, each correcting the last) tries
  to tell completed records from true records, given the shared variables
  and the 24 wealth tokens; fitted and scored with the population weights,
  with households kept whole on one side of the fit/score split. The AUC is
  the probability that it ranks a random completed record above a random
  true one: 0.5 means it cannot tell them apart. The report also lists the
  variables it relies on most.

## Checks on fabricated data

Before real data existed, the measures were run on a fabricated dataset in
the same layout (18,085 records, 3,000 households, five implicates, wealth
generated from a known model of income and age; `c_make_dev.py`) with four
fabricated completions. The measures behave as they should:

| Measure | Sampling noise | Perfect copy of truth | Oracle (true model) | Income link broken | Shuffled |
|---|---|---|---|---|---|
| Share holding by income group: average gap (points) | 6.27 | 0.00 | 3.03 | 20.98 | 20.97 |
| Amounts by income group: average gap (percentile ranks) | 5.34 | 0.00 | 2.94 | 19.06 | 19.24 |
| Rank correlation of amount with income: average gap | 0.049 | 0.000 | 0.021 | 0.503 | 0.511 |
| Mean net worth by income group: average gap (%) | 30.7 | 0.0 | 17.3 | 172.0 | 159.7 |
| Classifier AUC | 0.510 | 0.500 | 0.517 | 0.688 | 0.680 |

"Oracle" redraws wealth from the true fabricated model given each record's
incomes and age: what a perfect imputation model would produce. Its gaps are
about half the sampling-noise column, which is the basis for the reading
rule above. "Income link broken" redraws wealth from the true model using a
random other record's income. (On these fabricated data, holding of the 24
components is nearly independent across components, so the pattern measure
is near its maximum for every candidate; real SCF patterns are far more
concentrated.)

## Re-running

All runs go through Slurm (`c_py.sbatch` runs one script, `c_shell.sbatch`
one shell script). From `research/rebuild/tests/scf_joint/`:

```
# one pass over everything ready (yardsticks, completed files, report tables)
sbatch c_shell.sbatch c_watch.sh 1
# keep watching for new files every 10 minutes for up to ~12 hours
# (optionally one dataset per job: ... c_watch.sh 70 expB)
sbatch --time=12:00:00 c_shell.sbatch c_watch.sh 70
# refresh only the report tables
sbatch c_shell.sbatch c_report.sh
# a single file by hand
sbatch c_py.sbatch c_evaluate.py --dataset expA_clean --completed <file> --label <name>
sbatch c_py.sbatch c_evaluate_puf.py --dataset expB --completed <file> --label <name>
sbatch c_py.sbatch c_compare.py --update-report
```

Outputs: `/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/eval/<dataset>/<label>/`
(`summary.json`, `detail_*.csv`), and `eval/<dataset>/compare.md`.

## Results by dataset

Generated blocks follow, one per dataset (written by `c_compare.py
--update-report`). Experiment B's PUF section is written by
`c_evaluate_puf.py` into `$S/eval/expB/<label>_puf/report.md`.

<!-- BEGIN GENERATED expA_clean -->
### Dataset `expA_clean`
Evaluation set: 15,380 records in 2,818 households. Generated 2026-10-09 15:10. Runs: joint model (model_joint_w1_plain): 3 seed(s), joint model (model_joint_w1_asSCF_plain): 3 seed(s), one-source model (model_half2_w1_plain): 3 seed(s), production forest (comp_drf): 3 seed(s), production forest (comp_drfsw): 3 seed(s), sequential trees (comp_seqtrees): 3 seed(s).

**Headline.** "Sampling noise" is the typical gap between two random halves of the true records (households kept whole), averaged over five splits; for the level rows it is the typical gap, in points, and for the ratio rows the typical distance from 1. "No information" gives every record the wealth of a random other record. Method columns: mean over training seeds [lowest-highest seed].

| Measure | Truth | Sampling noise | No information (shuffled) | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | one-source model (model_half2_w1_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) |
|---|---|---|---|---|---|---|---|---|---|
| Share holding each component, all records: average gap (percentage points) |  | 1.02 | 0.12 | 3.59 [3.35-3.89] | 2.02 [1.94-2.15] | 2.51 [1.92-3.27] | 0.89 [0.86-0.93] | 1.02 [0.98-1.09] | 1.35 [1.32-1.38] |
| Share holding, by income group (24 components x 4 groups): average gap (points) |  | 3.52 | 13.02 | 5.06 [4.33-5.45] | 3.80 [3.45-4.28] | 4.35 [3.84-5.06] | 2.69 [2.64-2.74] | 2.32 [2.28-2.35] | 6.86 [6.77-6.93] |
| Share holding, by age range: average gap (points) |  | 2.74 | 6.39 | 4.03 [3.84-4.15] | 2.57 [2.40-2.77] | 3.05 [2.56-3.59] | 1.71 [1.62-1.77] | 2.21 [2.20-2.22] | 3.16 [3.14-3.18] |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks |  | 6.63 | 17.84 | 6.04 [5.91-6.12] | 5.81 [5.38-6.34] | 5.67 [5.34-6.22] | 5.62 [5.53-5.71] | 5.01 [4.95-5.12] | 13.24 [13.16-13.28] |
| Same, in dollars: average gap (%) |  | 69.8 | 244.9 | 53.7 [51.7-55.4] | 52.4 [49.0-58.0] | 52.0 [49.5-55.7] | 52.1 [51.4-52.6] | 45.2 [43.9-46.5] | 159.0 [158.5-159.5] |
| Amounts among holders by age range: average gap in percentile ranks |  | 6.99 | 9.84 | 5.95 [5.77-6.27] | 6.41 [6.04-7.04] | 6.47 [6.13-7.07] | 5.98 [5.92-6.04] | 5.84 [5.79-5.89] | 7.80 [7.73-7.87] |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) |  | 51.1 | 4.7 | 52.3 [50.0-54.8] | 44.7 [44.2-45.6] | 48.2 [46.6-50.8] | 46.2 [46.1-46.3] | 44.3 [44.2-44.5] | 49.1 [48.8-49.4] |
| Pairs of components (276 two-by-two holding tables): average share misplaced (%) |  | 1.84 | 0.21 | 5.75 [5.53-6.03] | 3.30 [3.16-3.48] | 4.14 [3.15-5.34] | 1.64 [1.58-1.70] | 1.77 [1.72-1.84] | 2.81 [2.79-2.84] |
| Selected conditional holding rates (12, e.g. home among stockholders): average gap (points) |  | 2.81 | 13.87 | 4.45 [4.29-4.62] | 3.63 [3.07-4.08] | 4.18 [3.35-4.89] | 5.82 [5.50-6.12] | 7.44 [7.34-7.52] | 10.35 [10.19-10.44] |
| Rank correlation of each amount with income, among holders: average gap |  | 0.076 | 0.429 | 0.092 [0.079-0.116] | 0.052 [0.047-0.059] | 0.048 [0.045-0.050] | 0.042 [0.040-0.045] | 0.038 [0.036-0.042] | 0.306 [0.305-0.309] |
| Rank correlation between amounts, among joint holders: average gap |  | 0.115 | 0.016 | 0.102 [0.097-0.111] | 0.096 [0.091-0.099] | 0.097 [0.092-0.101] | 0.102 [0.101-0.103] | 0.097 [0.096-0.097] | 0.248 [0.245-0.251] |
| Holders in the top 1% rank band of their component: completed / true, averaged over components |  | ±0.35 | 1.00 | 1.30 [0.95-1.65] | 2.00 [1.63-2.32] | 1.94 [1.79-2.16] | 1.66 [1.62-1.72] | 1.41 [1.35-1.51] | 0.94 [0.93-0.94] |
| Same for the top 0.1% rank band |  | ±1.36 | 1.16 | 1.87 [1.21-2.41] | 2.81 [2.20-3.55] | 2.49 [2.19-2.84] | 2.59 [2.25-2.84] | 2.12 [1.93-2.34] | 1.00 [0.87-1.10] |
| Median net worth, completed / true |  | ±0.122 | 0.998 | 1.744 [1.655-1.789] | 1.301 [1.259-1.334] | 1.354 [1.273-1.440] | 1.192 [1.179-1.209] | 1.154 [1.143-1.175] | 1.783 [1.778-1.786] |
| 90th percentile of net worth, completed / true |  | ±0.081 | 1.000 | 1.317 [1.186-1.402] | 1.319 [1.147-1.527] | 1.204 [1.092-1.301] | 1.083 [1.077-1.093] | 1.057 [1.056-1.061] | 0.938 [0.929-0.951] |
| 99th percentile of net worth, completed / true |  | ±0.049 | 0.976 | 1.207 [0.988-1.383] | 1.459 [1.277-1.635] | 1.393 [1.320-1.469] | 1.249 [1.246-1.251] | 0.986 [0.974-0.996] | 0.530 [0.524-0.535] |
| Top 10% share of net worth (%) | 77.6 | ±1.3 | 77.8 | 73.5 [70.6-75.3] | 79.3 [77.0-80.6] | 79.4 [79.0-80.0] | 80.6 [80.4-80.8] | 77.4 [77.3-77.6] | 62.8 [62.2-63.2] |
| Top 1% share of net worth (%) | 39.2 | ±2.6 | 40.3 | 34.3 [31.9-36.2] | 39.7 [37.0-44.5] | 40.3 [37.9-41.8] | 44.7 [44.6-44.7] | 40.4 [39.8-40.8] | 26.6 [25.2-27.3] |
| Top 0.1% share of net worth (%) | 15.3 | ±1.7 | 16.4 | 11.1 [9.9-11.8] | 12.9 [12.0-14.5] | 13.3 [12.8-13.6] | 18.8 [18.7-19.1] | 17.4 [17.0-17.6] | 10.5 [9.2-11.4] |
| Mean net worth by income group: average gap (%) |  | 12.0 | 178.7 | 53.7 [45.8-62.8] | 45.8 [39.3-53.5] | 36.4 [27.6-44.1] | 28.1 [27.0-29.8] | 11.2 [10.4-11.7] | 87.4 [85.2-89.6] |
| Top 1% by net worth: share whose wealth tokens repeat another household's (%) | 0.0 | ±0.0 | 71.7 | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 80.1 [79.0-81.2] | 72.8 [72.4-73.2] | 0.0 [0.0-0.0] |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) |  | 0.530 | 0.806 | 0.770 [0.754-0.783] | 0.709 [0.700-0.721] | 0.760 [0.754-0.766] | 0.804 [0.794-0.812] | 0.788 [0.785-0.792] | 0.775 [0.765-0.784] |

**Other variants** (same measures): runs without `_w1` are the earlier configuration (survey weight as a model variable; `plain` as drawn, `wc` weight-corrected); `_small` smaller model; `_w1`, `_wsqrt` model trained with each record's loss weighted by its population weight or its square root, and `_nowt` the unweighted control (in these three the survey weight is not a model variable and the plain and weight-corrected files are identical); `drfsw` the production forest fitted with survey weights instead of a weighted bootstrap (a diagnostic, not production). The detail tables below show only the main runs.

| Measure | Truth | Sampling noise | No information (shuffled) | one-source model (model_half2_nowt_plain) | one-source model (model_half2_nowt_wc) | one-source model (model_half2_plain) | one-source model (model_half2_small_plain) | one-source model (model_half2_small_wc) | one-source model (model_half2_w1_large_plain) | one-source model (model_half2_w1_large_wc) | one-source model (model_half2_w1_wc) | one-source model (model_half2_wc) | one-source model (model_half2_wsqrt_plain) | one-source model (model_half2_wsqrt_wc) | joint model (model_joint_asSCF_plain) | joint model (model_joint_asSCF_wc) | joint model (model_joint_nowt_asSCF_plain) | joint model (model_joint_nowt_asSCF_wc) | joint model (model_joint_nowt_plain) | joint model (model_joint_nowt_wc) | joint model (model_joint_plain) | joint model (model_joint_small_asSCF_plain) | joint model (model_joint_small_asSCF_wc) | joint model (model_joint_small_plain) | joint model (model_joint_small_wc) | joint model (model_joint_w1_asSCF_wc) | joint model (model_joint_w1_large_asSCF_plain) | joint model (model_joint_w1_large_asSCF_wc) | joint model (model_joint_w1_large_plain) | joint model (model_joint_w1_large_wc) | joint model (model_joint_w1_wc) | joint model (model_joint_wc) | joint model (model_joint_wsqrt_asSCF_plain) | joint model (model_joint_wsqrt_asSCF_wc) | joint model (model_joint_wsqrt_plain) | joint model (model_joint_wsqrt_wc) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Share holding each component, all records: average gap (percentage points) |  | 1.02 | 0.12 | 4.32 | 4.32 | 3.51 [2.69-3.97] | 3.24 [2.08-3.90] | 3.01 [1.94-3.77] | 3.15 [2.91-3.53] | 3.15 [2.91-3.53] | 2.51 [1.92-3.27] | 3.33 [2.25-3.91] | 2.72 [2.34-3.26] | 2.72 [2.34-3.26] | 2.36 [2.17-2.69] | 2.11 [1.81-2.49] | 2.22 | 2.22 | 4.45 | 4.45 | 4.56 [3.30-6.38] | 1.99 [1.25-2.48] | 1.65 [1.07-2.22] | 3.57 [2.10-4.67] | 3.40 [1.92-4.42] | 2.02 [1.94-2.15] | 2.26 [2.17-2.43] | 2.26 [2.17-2.43] | 2.52 [2.23-3.01] | 2.52 [2.23-3.01] | 3.59 [3.35-3.89] | 4.45 [3.09-6.29] | 2.19 [1.94-2.51] | 2.19 [1.94-2.51] | 3.81 [3.10-4.55] | 3.81 [3.10-4.55] |
| Share holding, by income group (24 components x 4 groups): average gap (points) |  | 3.52 | 13.02 | 6.02 | 6.02 | 5.50 [4.98-5.99] | 5.16 [4.52-5.64] | 4.75 [4.22-5.16] | 5.28 [4.77-5.57] | 5.28 [4.77-5.57] | 4.35 [3.84-5.06] | 5.16 [4.40-5.68] | 4.63 [4.24-5.05] | 4.63 [4.24-5.05] | 4.14 [4.04-4.22] | 3.75 [3.74-3.77] | 3.98 | 3.98 | 6.11 | 6.11 | 6.02 [5.17-7.65] | 4.29 [3.61-4.81] | 3.74 [3.28-4.05] | 5.10 [4.31-5.55] | 4.67 [3.97-5.02] | 3.80 [3.45-4.28] | 4.16 [3.94-4.41] | 4.16 [3.94-4.41] | 4.45 [3.63-4.91] | 4.45 [3.63-4.91] | 5.06 [4.33-5.45] | 5.82 [4.95-7.36] | 4.05 [3.86-4.33] | 4.05 [3.86-4.33] | 5.28 [4.11-6.07] | 5.28 [4.11-6.07] |
| Share holding, by age range: average gap (points) |  | 2.74 | 6.39 | 4.40 | 4.40 | 3.75 [2.98-4.16] | 3.58 [2.55-4.20] | 3.43 [2.48-4.17] | 3.48 [3.25-3.87] | 3.48 [3.25-3.87] | 3.05 [2.56-3.59] | 3.62 [2.62-4.15] | 3.12 [2.66-3.48] | 3.12 [2.66-3.48] | 2.78 [2.58-3.12] | 2.64 [2.40-3.04] | 2.66 | 2.66 | 4.50 | 4.50 | 4.92 [3.75-6.46] | 2.62 [2.06-3.01] | 2.40 [1.94-2.80] | 3.96 [2.68-4.83] | 3.87 [2.52-4.72] | 2.57 [2.40-2.77] | 2.73 [2.60-2.86] | 2.73 [2.60-2.86] | 2.92 [2.71-3.31] | 2.92 [2.71-3.31] | 4.03 [3.84-4.15] | 4.90 [3.63-6.47] | 2.69 [2.42-3.12] | 2.69 [2.42-3.12] | 4.18 [3.83-4.68] | 4.18 [3.83-4.68] |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks |  | 6.63 | 17.84 | 7.00 | 7.00 | 6.90 [6.78-7.11] | 6.04 [5.65-6.40] | 5.39 [5.17-5.59] | 5.97 [5.62-6.16] | 5.97 [5.62-6.16] | 5.67 [5.34-6.22] | 6.24 [5.95-6.74] | 6.05 [5.90-6.33] | 6.05 [5.90-6.33] | 6.52 [5.94-7.03] | 5.78 [5.30-6.60] | 6.48 | 6.48 | 6.34 | 6.34 | 6.21 [5.58-6.72] | 6.09 [5.76-6.62] | 5.22 [5.13-5.33] | 6.21 [5.68-6.54] | 5.65 [5.35-5.95] | 5.81 [5.38-6.34] | 5.50 [5.18-5.91] | 5.50 [5.18-5.91] | 6.47 [5.75-6.93] | 6.47 [5.75-6.93] | 6.04 [5.91-6.12] | 5.87 [5.28-6.29] | 6.06 [5.63-6.79] | 6.06 [5.63-6.79] | 5.74 [5.39-5.97] | 5.74 [5.39-5.97] |
| Same, in dollars: average gap (%) |  | 69.8 | 244.9 | 74.0 | 73.7 | 69.6 [67.7-72.7] | 59.9 [54.2-62.8] | 50.2 [46.3-52.1] | 55.5 [52.5-59.1] | 55.7 [52.1-59.3] | 51.8 [49.3-55.5] | 59.7 [56.6-61.7] | 55.9 [54.0-57.8] | 56.0 [54.1-57.8] | 65.9 [59.8-70.1] | 54.9 [50.4-61.8] | 67.0 | 66.3 | 70.1 | 69.7 | 60.6 [52.1-68.1] | 62.4 [57.9-71.0] | 49.4 [47.7-52.7] | 65.0 [57.4-69.9] | 54.7 [49.9-58.0] | 52.2 [48.7-57.7] | 49.1 [45.5-53.6] | 49.1 [45.6-54.0] | 61.3 [51.9-67.8] | 61.3 [51.8-67.8] | 53.7 [51.7-55.3] | 54.8 [47.5-62.6] | 57.3 [51.4-64.7] | 57.2 [51.3-64.6] | 51.5 [47.8-56.7] | 51.5 [47.8-56.4] |
| Amounts among holders by age range: average gap in percentile ranks |  | 6.99 | 9.84 | 7.90 | 7.90 | 7.62 [7.17-8.06] | 6.78 [6.28-7.11] | 6.29 [5.99-6.53] | 6.51 [6.29-6.69] | 6.51 [6.29-6.69] | 6.47 [6.13-7.07] | 6.97 [6.65-7.27] | 6.97 [6.63-7.60] | 6.97 [6.63-7.60] | 7.31 [6.54-8.12] | 6.66 [6.07-7.39] | 7.21 | 7.21 | 6.47 | 6.47 | 6.56 [5.89-7.02] | 6.81 [6.60-7.22] | 6.27 [6.05-6.40] | 6.46 [6.02-6.71] | 6.11 [5.83-6.30] | 6.41 [6.04-7.04] | 5.80 [5.59-6.03] | 5.80 [5.59-6.03] | 6.12 [5.96-6.38] | 6.12 [5.96-6.38] | 5.95 [5.77-6.27] | 6.20 [5.58-6.58] | 6.74 [6.30-7.48] | 6.74 [6.30-7.48] | 6.08 [5.90-6.40] | 6.08 [5.90-6.40] |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) |  | 51.1 | 4.7 | 52.0 | 52.0 | 50.6 [49.1-52.3] | 52.4 [47.7-55.5] | 51.8 [47.2-54.3] | 46.6 [45.7-47.4] | 46.6 [45.7-47.4] | 48.2 [46.6-50.8] | 50.1 [48.3-52.0] | 48.7 [47.5-49.7] | 48.7 [47.5-49.7] | 46.1 [46.1-46.2] | 45.3 [45.1-45.5] | 45.8 | 45.8 | 58.1 | 58.1 | 55.4 [51.1-60.0] | 47.3 [44.8-48.9] | 46.6 [44.3-47.9] | 55.3 [52.3-58.8] | 55.1 [52.2-58.7] | 44.7 [44.2-45.6] | 43.2 [42.9-43.4] | 43.2 [42.9-43.4] | 47.3 [46.5-48.2] | 47.3 [46.5-48.2] | 52.3 [50.0-54.8] | 55.2 [50.4-60.2] | 44.9 [43.8-45.9] | 44.9 [43.8-45.9] | 52.7 [49.5-55.8] | 52.7 [49.5-55.8] |
| Pairs of components (276 two-by-two holding tables): average share misplaced (%) |  | 1.84 | 0.21 | 6.92 | 6.92 | 5.64 [4.38-6.40] | 5.29 [3.40-6.33] | 4.90 [3.11-6.16] | 5.30 [4.83-6.13] | 5.30 [4.83-6.13] | 4.14 [3.15-5.34] | 5.39 [3.73-6.29] | 4.46 [3.83-5.19] | 4.46 [3.83-5.19] | 3.75 [3.47-4.13] | 3.43 [3.08-3.92] | 3.71 | 3.71 | 7.31 | 7.31 | 7.39 [5.39-10.27] | 3.40 [2.29-4.10] | 2.84 [2.07-3.60] | 5.79 [3.44-7.56] | 5.49 [3.15-7.11] | 3.30 [3.16-3.48] | 3.87 [3.72-4.13] | 3.87 [3.72-4.13] | 4.20 [3.83-4.90] | 4.20 [3.83-4.90] | 5.75 [5.53-6.03] | 7.22 [5.09-10.13] | 3.49 [3.08-3.97] | 3.49 [3.08-3.97] | 6.18 [5.11-7.35] | 6.18 [5.11-7.35] |
| Selected conditional holding rates (12, e.g. home among stockholders): average gap (points) |  | 2.81 | 13.87 | 4.35 | 4.35 | 4.78 [3.71-5.55] | 5.18 [4.82-5.39] | 4.76 [4.52-4.98] | 8.31 [7.88-8.76] | 8.31 [7.88-8.76] | 4.18 [3.35-4.89] | 4.94 [3.71-5.99] | 4.51 [3.13-5.76] | 4.51 [3.13-5.76] | 4.17 [3.41-4.66] | 4.11 [3.44-4.47] | 3.94 | 3.94 | 3.76 | 3.76 | 5.37 [3.20-6.66] | 3.65 [3.20-4.26] | 3.50 [2.81-3.96] | 5.14 [4.53-6.06] | 5.32 [4.14-6.65] | 3.63 [3.07-4.08] | 6.30 [5.39-6.78] | 6.30 [5.39-6.78] | 5.87 [4.94-7.70] | 5.87 [4.94-7.70] | 4.45 [4.29-4.62] | 5.17 [3.13-6.62] | 3.53 [3.07-4.24] | 3.53 [3.07-4.24] | 4.76 [4.22-5.06] | 4.76 [4.22-5.06] |
| Rank correlation of each amount with income, among holders: average gap |  | 0.076 | 0.429 | 0.051 | 0.052 | 0.050 [0.044-0.055] | 0.048 [0.042-0.051] | 0.046 [0.044-0.047] | 0.064 [0.061-0.067] | 0.064 [0.061-0.067] | 0.048 [0.045-0.050] | 0.050 [0.043-0.056] | 0.047 [0.042-0.052] | 0.047 [0.042-0.052] | 0.046 [0.042-0.052] | 0.047 [0.042-0.052] | 0.051 | 0.051 | 0.074 | 0.074 | 0.059 [0.051-0.066] | 0.042 [0.041-0.044] | 0.047 [0.041-0.055] | 0.065 [0.051-0.077] | 0.068 [0.058-0.076] | 0.052 [0.047-0.059] | 0.069 [0.061-0.078] | 0.069 [0.061-0.077] | 0.093 [0.069-0.117] | 0.093 [0.069-0.116] | 0.092 [0.079-0.116] | 0.062 [0.058-0.068] | 0.047 [0.045-0.050] | 0.047 [0.044-0.050] | 0.073 [0.070-0.076] | 0.073 [0.070-0.076] |
| Rank correlation between amounts, among joint holders: average gap |  | 0.115 | 0.016 | 0.103 | 0.103 | 0.112 [0.101-0.123] | 0.120 [0.114-0.125] | 0.116 [0.114-0.119] | 0.103 [0.100-0.107] | 0.103 [0.100-0.107] | 0.097 [0.092-0.101] | 0.109 [0.097-0.121] | 0.098 [0.096-0.101] | 0.099 [0.096-0.101] | 0.100 [0.093-0.106] | 0.097 [0.092-0.102] | 0.095 | 0.095 | 0.093 | 0.093 | 0.091 [0.086-0.097] | 0.122 [0.109-0.134] | 0.114 [0.101-0.124] | 0.102 [0.097-0.107] | 0.098 [0.094-0.101] | 0.096 [0.091-0.099] | 0.093 [0.090-0.098] | 0.093 [0.090-0.098] | 0.107 [0.096-0.116] | 0.107 [0.096-0.115] | 0.102 [0.097-0.111] | 0.090 [0.087-0.092] | 0.093 [0.090-0.097] | 0.093 [0.090-0.096] | 0.095 [0.094-0.096] | 0.095 [0.094-0.096] |
| Holders in the top 1% rank band of their component: completed / true, averaged over components |  | ±0.35 | 1.00 | 3.81 | 3.81 | 4.03 [3.27-4.96] | 3.59 [3.18-3.99] | 2.85 [2.54-3.29] | 1.51 [0.85-1.85] | 1.51 [0.85-1.85] | 1.94 [1.79-2.16] | 3.39 [2.60-4.56] | 2.46 [2.29-2.74] | 2.46 [2.29-2.74] | 3.86 [3.68-4.19] | 3.32 [3.06-3.56] | 4.09 | 4.09 | 5.15 | 5.15 | 3.67 [3.23-3.91] | 3.94 [3.41-4.84] | 3.15 [2.77-3.71] | 4.48 [3.71-4.96] | 3.62 [2.81-4.05] | 2.00 [1.63-2.32] | 1.40 [1.13-1.57] | 1.40 [1.13-1.57] | 0.98 [0.77-1.33] | 0.98 [0.77-1.33] | 1.30 [0.95-1.65] | 3.44 [2.88-3.87] | 2.78 [2.24-3.07] | 2.78 [2.24-3.07] | 2.23 [1.88-2.78] | 2.23 [1.88-2.78] |
| Same for the top 0.1% rank band |  | ±1.36 | 1.16 | 7.41 | 7.41 | 8.51 [5.69-11.31] | 6.82 [4.79-8.05] | 5.98 [4.10-7.17] | 3.67 [1.32-5.14] | 3.67 [1.32-5.14] | 2.49 [2.19-2.84] | 7.21 [4.74-11.28] | 3.78 [3.28-4.22] | 3.78 [3.28-4.22] | 7.63 [7.05-8.13] | 6.33 [6.02-6.58] | 8.68 | 8.68 | 14.34 | 14.34 | 9.43 [8.74-9.98] | 7.64 [6.36-9.64] | 5.72 [4.97-6.30] | 12.81 [10.29-16.35] | 10.51 [7.45-13.25] | 2.81 [2.20-3.55] | 2.70 [1.82-3.15] | 2.70 [1.82-3.15] | 1.58 [1.26-1.94] | 1.58 [1.26-1.94] | 1.87 [1.21-2.41] | 9.32 [8.04-10.00] | 4.98 [3.99-5.83] | 4.98 [3.99-5.83] | 4.30 [3.53-5.63] | 4.30 [3.53-5.63] |
| Median net worth, completed / true |  | ±0.122 | 0.998 | 1.508 | 1.507 | 1.425 [1.224-1.541] | 1.443 [1.078-1.645] | 1.386 [1.051-1.619] | 1.374 [1.186-1.580] | 1.376 [1.188-1.584] | 1.353 [1.269-1.439] | 1.395 [1.222-1.516] | 1.385 [1.201-1.512] | 1.385 [1.203-1.509] | 1.446 [1.391-1.483] | 1.394 [1.342-1.430] | 1.285 | 1.284 | 1.884 | 1.885 | 1.981 [1.833-2.257] | 1.220 [1.073-1.320] | 1.172 [1.037-1.249] | 1.871 [1.680-2.007] | 1.782 [1.596-1.895] | 1.301 [1.260-1.334] | 1.329 [1.257-1.447] | 1.329 [1.258-1.445] | 1.675 [1.406-1.825] | 1.676 [1.410-1.826] | 1.741 [1.652-1.787] | 1.915 [1.761-2.170] | 1.390 [1.330-1.508] | 1.385 [1.326-1.501] | 1.855 [1.778-1.910] | 1.852 [1.777-1.908] |
| 90th percentile of net worth, completed / true |  | ±0.081 | 1.000 | 1.915 | 1.912 | 1.617 [1.492-1.732] | 1.627 [1.588-1.666] | 1.434 [1.416-1.447] | 1.016 [0.958-1.109] | 1.013 [0.954-1.106] | 1.207 [1.099-1.299] | 1.439 [1.351-1.559] | 1.321 [1.227-1.374] | 1.319 [1.223-1.370] | 1.586 [1.490-1.751] | 1.363 [1.295-1.497] | 1.566 | 1.557 | 2.025 | 2.032 | 1.769 [1.527-2.076] | 1.616 [1.430-1.748] | 1.346 [1.292-1.395] | 1.753 [1.702-1.827] | 1.436 [1.367-1.497] | 1.321 [1.144-1.529] | 1.003 [0.900-1.088] | 0.999 [0.898-1.084] | 0.995 [0.882-1.142] | 0.994 [0.881-1.138] | 1.316 [1.184-1.400] | 1.605 [1.449-1.873] | 1.462 [1.307-1.715] | 1.461 [1.308-1.716] | 1.475 [1.251-1.701] | 1.475 [1.251-1.697] |
| 99th percentile of net worth, completed / true |  | ±0.049 | 0.976 | 2.605 | 2.607 | 2.799 [2.311-3.711] | 3.159 [2.726-3.655] | 2.325 [2.024-2.705] | 0.934 [0.769-1.075] | 0.934 [0.777-1.071] | 1.382 [1.310-1.459] | 2.156 [1.850-2.706] | 1.791 [1.590-2.095] | 1.790 [1.606-2.087] | 3.102 [2.997-3.187] | 2.315 [2.305-2.326] | 2.855 | 2.896 | 6.099 | 6.046 | 4.240 [4.038-4.595] | 3.457 [2.540-4.175] | 2.278 [1.793-2.678] | 5.485 [4.530-6.124] | 3.775 [3.697-3.859] | 1.455 [1.277-1.620] | 0.934 [0.900-0.989] | 0.925 [0.892-0.981] | 0.783 [0.689-0.946] | 0.780 [0.674-0.952] | 1.196 [0.991-1.369] | 3.760 [3.214-4.492] | 2.028 [1.705-2.443] | 2.033 [1.739-2.456] | 2.012 [1.718-2.458] | 2.002 [1.722-2.441] |
| Top 10% share of net worth (%) | 77.6 | ±1.3 | 77.8 | 87.7 | 87.8 | 89.5 [86.6-91.6] | 88.5 [86.7-90.5] | 88.4 [86.8-90.2] | 75.0 [68.2-79.2] | 75.0 [68.2-79.3] | 79.4 [78.8-79.9] | 88.6 [85.1-91.8] | 84.0 [82.0-86.5] | 83.9 [82.1-86.3] | 88.7 [88.0-89.2] | 87.6 [87.2-88.2] | 89.2 | 89.0 | 94.2 | 94.2 | 91.7 [90.9-92.2] | 89.4 [88.5-90.9] | 87.8 [87.2-88.2] | 92.4 [91.5-93.7] | 91.8 [90.7-92.7] | 79.3 [77.1-80.4] | 75.1 [73.6-76.5] | 75.1 [73.7-76.5] | 69.7 [67.5-71.0] | 69.6 [67.2-71.0] | 73.4 [70.6-75.1] | 92.1 [91.3-92.9] | 84.8 [82.6-86.3] | 84.7 [82.6-86.3] | 82.8 [81.3-84.1] | 82.8 [81.3-83.9] |
| Top 1% share of net worth (%) | 39.2 | ±2.6 | 40.3 | 57.1 | 57.4 | 63.1 [54.5-70.2] | 60.2 [57.4-65.8] | 63.0 [60.0-67.1] | 39.1 [28.5-47.6] | 39.2 [28.5-47.8] | 40.1 [38.0-41.7] | 62.7 [52.3-73.1] | 50.3 [47.4-53.5] | 50.1 [47.4-52.9] | 60.2 [59.9-60.7] | 59.9 [58.0-61.3] | 60.9 | 60.2 | 74.2 | 73.8 | 70.7 [68.6-73.8] | 58.6 [56.8-61.0] | 58.0 [55.3-59.6] | 67.3 [63.9-72.2] | 70.5 [67.4-74.4] | 39.8 [37.5-44.1] | 38.8 [33.8-43.1] | 38.8 [33.9-42.9] | 33.4 [33.3-33.7] | 33.3 [32.7-33.8] | 34.2 [31.9-35.6] | 73.4 [71.6-75.4] | 52.2 [50.6-54.4] | 52.0 [50.3-53.9] | 52.3 [51.7-53.2] | 52.2 [51.6-53.2] |
| Top 0.1% share of net worth (%) | 15.3 | ±1.7 | 16.4 | 28.3 | 28.7 | 31.9 [27.8-35.8] | 26.5 [21.7-30.6] | 32.4 [26.3-36.2] | 14.8 [9.4-20.1] | 14.9 [9.4-20.5] | 13.2 [12.8-13.8] | 34.4 [27.8-37.7] | 21.9 [18.9-24.1] | 21.8 [18.7-23.4] | 26.5 [24.0-29.2] | 28.2 [25.9-30.8] | 26.3 | 25.6 | 24.8 | 25.5 | 30.2 [28.7-32.9] | 23.7 [20.7-28.1] | 26.6 [22.7-29.1] | 25.7 [25.5-25.9] | 31.6 [30.3-33.8] | 13.0 [12.6-13.8] | 14.5 [12.3-17.1] | 14.6 [12.5-17.1] | 12.9 [11.9-14.5] | 12.8 [11.9-13.9] | 11.1 [10.0-12.1] | 32.3 [29.5-34.2] | 21.0 [19.8-21.9] | 20.7 [19.2-21.8] | 21.9 [20.7-23.8] | 21.8 [20.7-23.8] |
| Mean net worth by income group: average gap (%) |  | 12.0 | 178.7 | 226.0 | 231.2 | 311.9 [198.5-434.9] | 259.4 [225.5-323.9] | 242.8 [190.0-289.0] | 21.7 [17.2-28.8] | 21.2 [16.0-28.9] | 35.9 [27.0-42.6] | 261.1 [153.6-397.5] | 87.8 [65.3-107.6] | 86.9 [65.1-105.7] | 232.8 [183.2-259.6] | 170.1 [137.3-208.0] | 219.9 | 217.2 | 897.5 | 882.9 | 548.8 [396.5-737.8] | 248.8 [208.7-308.7] | 159.5 [141.0-191.9] | 592.3 [499.5-696.3] | 467.0 [381.1-532.6] | 46.2 [37.8-55.9] | 25.3 [15.7-35.0] | 25.2 [16.5-34.6] | 38.9 [36.4-42.1] | 37.9 [34.7-40.8] | 53.9 [46.1-61.9] | 595.0 [430.4-874.8] | 110.5 [87.3-146.6] | 111.1 [85.5-150.1] | 138.3 [93.7-186.8] | 143.1 [95.3-195.3] |
| Top 1% by net worth: share whose wealth tokens repeat another household's (%) | 0.0 | ±0.0 | 71.7 | 0.0 | 0.0 | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.1 [0.0-0.3] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) |  | 0.530 | 0.806 | 0.777 | 0.777 | 0.779 [0.776-0.784] | 0.773 [0.744-0.790] | 0.760 [0.725-0.780] | 0.762 [0.755-0.768] | 0.762 [0.755-0.768] | 0.760 [0.754-0.766] | 0.778 [0.772-0.785] | 0.760 [0.753-0.771] | 0.760 [0.753-0.771] | 0.714 [0.711-0.719] | 0.705 [0.697-0.709] | 0.715 | 0.715 | 0.800 | 0.800 | 0.783 [0.757-0.813] | 0.722 [0.697-0.737] | 0.720 [0.711-0.730] | 0.796 [0.786-0.803] | 0.783 [0.775-0.788] | 0.709 [0.700-0.721] | 0.698 [0.693-0.704] | 0.698 [0.693-0.704] | 0.738 [0.724-0.763] | 0.738 [0.724-0.763] | 0.770 [0.754-0.783] | 0.779 [0.754-0.805] | 0.710 [0.704-0.713] | 0.710 [0.704-0.713] | 0.769 [0.749-0.791] | 0.769 [0.749-0.791] |

**Completion noise** (one completion draw against another, same file): the part of each gap that comes from drawing at random, not from the model.

| Measure | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | one-source model (model_half2_w1_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) |
|---|---|---|---|---|---|---|
| Share holding, by income group (24 components x 4 groups): average gap (points) | 1.02 | 0.94 | 1.06 | 0.91 | 0.96 | 1.01 |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks | 2.67 | 2.75 | 2.48 | 2.18 | 2.09 | 2.70 |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) | 38.9 | 30.6 | 32.6 | 12.4 | 13.6 | 32.6 |
| Mean net worth by income group: average gap (%) | 7.2 | 6.4 | 7.9 | 4.5 | 3.9 | 7.3 |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) | 0.506 | 0.507 | 0.499 | 0.496 | 0.495 | 0.497 |

**Share holding each component, all records (%).**

| Component | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | one-source model (model_half2_w1_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|---|
| cash | 95.7 | 96.7 | 97.1 | 97.4 | 95.8 | 95.9 | 95.8 | 0.4 |
| equities | 20.1 | 27.2 | 22.7 | 21.2 | 21.7 | 21.4 | 19.9 | 1.5 |
| bonds | 11.7 | 12.4 | 11.8 | 10.6 | 13.0 | 13.6 | 11.3 | 1.2 |
| dc | 52.2 | 55.9 | 55.7 | 55.5 | 56.4 | 56.6 | 55.8 | 0.4 |
| db | 6.2 | 6.9 | 5.7 | 6.2 | 6.8 | 6.6 | 5.9 | 0.8 |
| life_ins | 20.0 | 17.5 | 16.4 | 15.1 | 18.4 | 18.4 | 18.3 | 2.3 |
| annuities | 4.4 | 4.0 | 3.0 | 2.6 | 4.8 | 4.2 | 3.1 | 0.5 |
| trusts | 2.1 | 1.5 | 1.1 | 1.2 | 2.0 | 1.6 | 1.1 | 0.5 |
| other_fin | 9.9 | 7.4 | 7.2 | 6.7 | 9.1 | 9.2 | 7.8 | 1.4 |
| pass_throughs | 12.9 | 22.3 | 14.3 | 15.7 | 12.2 | 12.2 | 11.0 | 0.5 |
| primary_home | 65.3 | 72.8 | 68.1 | 70.7 | 64.8 | 66.5 | 66.0 | 1.8 |
| other_home | 15.0 | 23.2 | 18.2 | 17.8 | 16.1 | 15.9 | 12.2 | 0.3 |
| re_fund | 6.9 | 9.2 | 8.2 | 9.5 | 7.0 | 6.4 | 5.3 | 1.0 |
| other_nonfin | 86.8 | 87.0 | 87.3 | 87.5 | 87.0 | 87.3 | 87.3 | 1.3 |
| primary_mortgage | 43.4 | 48.0 | 45.6 | 46.2 | 44.8 | 46.1 | 45.1 | 1.2 |
| other_mortgage | 6.0 | 7.3 | 6.4 | 5.0 | 5.9 | 6.4 | 3.9 | 0.8 |
| credit_lines | 1.8 | 1.7 | 1.5 | 1.2 | 1.9 | 2.0 | 1.5 | 0.4 |
| credit_cards | 46.3 | 43.9 | 42.8 | 45.4 | 43.9 | 44.6 | 45.1 | 1.4 |
| installment_debt | 52.8 | 49.3 | 50.7 | 51.1 | 51.3 | 52.0 | 51.9 | 1.7 |
| other_debt | 5.5 | 5.1 | 4.2 | 4.4 | 5.5 | 5.8 | 5.2 | 0.6 |
| kg_primary_home | 62.0 | 67.9 | 64.5 | 67.0 | 61.3 | 63.2 | 61.3 | 2.1 |
| kg_other_re | 16.8 | 27.3 | 21.8 | 26.0 | 16.9 | 16.6 | 14.6 | 0.8 |
| kg_pass_throughs | 12.1 | 16.7 | 12.3 | 14.2 | 10.8 | 10.8 | 8.6 | 0.6 |
| kg_other | 18.3 | 22.3 | 20.0 | 19.1 | 18.9 | 19.2 | 16.9 | 1.0 |

**Share holding by income group, selected components (%).** Income group: weighted rank of the total of the seven incomes within source and year.

| Component | Income group | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | one-source model (model_half2_w1_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|---|---|
| primary_home | Bottom 50% | 47.0 | 56.5 | 48.5 | 50.7 | 46.0 | 48.4 | 55.6 | 2.5 |
| primary_home | 50-90% | 76.4 | 82.8 | 80.1 | 83.4 | 75.9 | 77.5 | 72.0 | 1.5 |
| primary_home | 90-99% | 91.1 | 95.4 | 95.7 | 96.9 | 92.0 | 92.3 | 81.9 | 4.0 |
| primary_home | Top 1% | 91.1 | 95.6 | 94.9 | 96.6 | 97.1 | 95.0 | 85.2 | 5.5 |
| equities | Bottom 50% | 8.9 | 14.1 | 10.3 | 9.5 | 9.7 | 9.0 | 12.6 | 0.8 |
| equities | 50-90% | 21.1 | 30.8 | 25.4 | 23.1 | 24.2 | 24.5 | 21.6 | 2.8 |
| equities | 90-99% | 55.1 | 60.3 | 56.4 | 54.9 | 55.5 | 53.3 | 39.4 | 4.7 |
| equities | Top 1% | 89.9 | 79.8 | 77.8 | 78.7 | 76.2 | 81.9 | 53.2 | 4.2 |
| pass_throughs | Bottom 50% | 5.8 | 12.5 | 6.2 | 7.1 | 6.2 | 6.1 | 7.0 | 1.4 |
| pass_throughs | 50-90% | 14.4 | 24.0 | 14.6 | 16.1 | 12.0 | 12.6 | 11.3 | 1.5 |
| pass_throughs | 90-99% | 30.8 | 50.2 | 40.5 | 43.4 | 30.6 | 29.1 | 22.9 | 5.1 |
| pass_throughs | Top 1% | 62.1 | 73.5 | 69.7 | 73.6 | 81.7 | 69.4 | 41.1 | 10.3 |
| dc | Bottom 50% | 27.9 | 37.0 | 34.7 | 35.7 | 34.5 | 34.9 | 40.9 | 2.2 |
| dc | 50-90% | 65.8 | 66.8 | 68.0 | 66.7 | 68.9 | 69.3 | 64.5 | 2.4 |
| dc | 90-99% | 91.1 | 84.5 | 87.3 | 86.4 | 91.0 | 89.0 | 77.9 | 1.3 |
| dc | Top 1% | 84.9 | 84.8 | 85.6 | 86.3 | 86.3 | 90.0 | 77.1 | 11.3 |
| primary_mortgage | Bottom 50% | 24.8 | 31.6 | 27.2 | 26.9 | 26.6 | 27.3 | 34.6 | 1.8 |
| primary_mortgage | 50-90% | 55.1 | 58.0 | 57.2 | 58.7 | 56.4 | 57.7 | 51.0 | 2.0 |
| primary_mortgage | 90-99% | 70.0 | 71.5 | 71.5 | 71.6 | 69.6 | 72.1 | 61.9 | 3.3 |
| primary_mortgage | Top 1% | 57.0 | 62.4 | 58.7 | 59.3 | 56.9 | 65.4 | 59.1 | 11.1 |
| bonds | Bottom 50% | 5.6 | 7.2 | 5.9 | 4.9 | 6.5 | 6.6 | 8.5 | 1.4 |
| bonds | 50-90% | 12.9 | 14.0 | 13.2 | 11.7 | 14.3 | 14.9 | 11.8 | 1.8 |
| bonds | 90-99% | 29.4 | 24.5 | 27.5 | 26.4 | 31.2 | 34.0 | 19.7 | 3.6 |
| bonds | Top 1% | 39.8 | 34.1 | 41.4 | 43.4 | 47.1 | 44.6 | 26.0 | 5.9 |
| trusts | Bottom 50% | 1.3 | 0.8 | 0.5 | 0.5 | 0.7 | 0.5 | 0.7 | 0.5 |
| trusts | 50-90% | 2.1 | 1.5 | 1.1 | 1.1 | 2.1 | 1.6 | 1.0 | 0.7 |
| trusts | 90-99% | 3.3 | 3.2 | 3.2 | 3.7 | 6.1 | 5.1 | 2.6 | 1.1 |
| trusts | Top 1% | 18.4 | 6.1 | 6.8 | 8.5 | 13.2 | 13.5 | 6.3 | 4.1 |
| other_home | Bottom 50% | 6.0 | 13.5 | 8.6 | 8.1 | 8.3 | 8.5 | 8.5 | 0.9 |
| other_home | 50-90% | 17.1 | 25.6 | 19.9 | 19.4 | 16.7 | 16.1 | 12.7 | 1.4 |
| other_home | 90-99% | 38.3 | 48.4 | 45.7 | 45.8 | 40.0 | 41.0 | 23.2 | 3.5 |
| other_home | Top 1% | 61.5 | 66.9 | 66.6 | 67.2 | 69.2 | 60.5 | 36.5 | 17.4 |

**Net worth** (assets minus debts, dollars of the survey year; the four unrealized-gain variables are not part of net worth).

| Net worth statistic | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | one-source model (model_half2_w1_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (± %) |
|---|---|---|---|---|---|---|---|---|
| p10 | -1k | 0k | -0k | -0k | -1k | -1k | 1k |  |
| p25 | 11k | 22k | 13k | 15k | 11k | 12k | 39k | 15.8 |
| p50 | 93k | 163k | 122k | 127k | 111k | 108k | 167k | 12.2 |
| p75 | 385k | 547k | 468k | 442k | 429k | 404k | 463k | 3.8 |
| p90 | 1,188k | 1,565k | 1,567k | 1,430k | 1,287k | 1,256k | 1,115k | 8.1 |
| p99 | 10,681k | 12,889k | 15,587k | 14,879k | 13,341k | 10,533k | 5,660k | 4.9 |
| mean | 725k | 888k | 983k | 937k | 924k | 762k | 533k | 3.6 |
| top10%_share | 77.6% | 73.5% | 79.3% | 79.4% | 80.6% | 77.4% | 62.8% | 1.6 |
| top1%_share | 39.2% | 34.3% | 39.7% | 40.3% | 44.7% | 40.4% | 26.6% | 6.4 |
| top0.1%_share | 15.3% | 11.1% | 12.9% | 13.3% | 18.8% | 17.4% | 10.5% | 9.9 |

| Income group | Truth, mean | Truth, median | joint model (model_joint_w1_plain), mean | joint model (model_joint_w1_plain), median | joint model (model_joint_w1_asSCF_plain), mean | joint model (model_joint_w1_asSCF_plain), median | one-source model (model_half2_w1_plain), mean | one-source model (model_half2_w1_plain), median | production forest (comp_drf), mean | production forest (comp_drf), median | production forest (comp_drfsw), mean | production forest (comp_drfsw), median | sequential trees (comp_seqtrees), mean | sequential trees (comp_seqtrees), median | Sampling noise, mean (± %) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Bottom 50% | 120k | 17k | 257k | 44k | 203k | 22k | 187k | 26k | 161k | 19k | 140k | 22k | 349k | 95k | 14.9 |
| 50-90% | 438k | 159k | 635k | 231k | 612k | 189k | 562k | 191k | 437k | 167k | 400k | 160k | 522k | 191k | 11.3 |
| 90-99% | 2,274k | 1,077k | 2,976k | 1,213k | 3,687k | 1,428k | 3,515k | 1,311k | 2,956k | 1,264k | 2,580k | 1,271k | 1,125k | 519k | 10.5 |
| Top 1% | 20,687k | 10,423k | 15,690k | 8,022k | 20,431k | 10,390k | 20,432k | 10,867k | 30,187k | 14,272k | 21,866k | 10,408k | 2,493k | 1,368k | 11.3 |

**The top of each component, in rank.** Share of holders (with a positive amount) in the top 1% and top 0.1% rank bands of that component (%). By construction the weighted truth is near 1% and 0.1%; a method above that puts too many holders at the very top, which inflates net worth at the top once decoded to dollars. The unweighted columns count sample records instead of population weight: they show how far the SCF sample over-represents the top, which is what a model that learned the sample, not the population, would reproduce.

| Component | Truth, top 1% | Truth, top 0.1% | Truth unweighted (sample), top 1% | Truth unweighted (sample), top 0.1% | joint model (model_joint_w1_plain), top 1% | joint model (model_joint_w1_plain), top 0.1% | joint model (model_joint_w1_asSCF_plain), top 1% | joint model (model_joint_w1_asSCF_plain), top 0.1% | one-source model (model_half2_w1_plain), top 1% | one-source model (model_half2_w1_plain), top 0.1% | production forest (comp_drf), top 1% | production forest (comp_drf), top 0.1% | production forest (comp_drfsw), top 1% | production forest (comp_drfsw), top 0.1% | sequential trees (comp_seqtrees), top 1% | sequential trees (comp_seqtrees), top 0.1% |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| cash | 0.83 | 0.13 | 6.57 | 2.83 | 1.13 | 0.12 | 1.15 | 0.12 | 1.04 | 0.10 | 1.20 | 0.16 | 1.03 | 0.12 | 0.76 | 0.08 |
| equities | 0.81 | 0.11 | 12.89 | 4.92 | 1.15 | 0.11 | 1.67 | 0.14 | 1.64 | 0.14 | 1.05 | 0.18 | 1.11 | 0.14 | 0.48 | 0.06 |
| bonds | 0.77 | 0.07 | 12.61 | 4.82 | 0.87 | 0.09 | 1.63 | 0.22 | 1.56 | 0.18 | 1.47 | 0.12 | 1.02 | 0.12 | 0.65 | 0.06 |
| dc | 0.55 | 0.07 | 4.20 | 1.37 | 1.23 | 0.12 | 1.50 | 0.12 | 1.38 | 0.10 | 0.88 | 0.10 | 0.98 | 0.11 | 0.58 | 0.08 |
| db | 4.12 | 0.07 | 8.06 | 1.08 | 0.79 | 0.19 | 1.23 | 0.30 | 1.06 | 0.22 | 0.81 | 0.02 | 1.16 | 0.09 | 0.84 | 0.14 |
| life_ins | 0.70 | 0.03 | 7.59 | 2.70 | 1.32 | 0.13 | 1.79 | 0.17 | 1.85 | 0.16 | 1.24 | 0.16 | 1.10 | 0.11 | 0.68 | 0.06 |
| annuities | 1.12 | 0.17 | 5.71 | 2.07 | 0.71 | 0.11 | 1.19 | 0.15 | 1.34 | 0.13 | 2.12 | 0.09 | 0.95 | 0.12 | 0.85 | 0.11 |
| trusts | 0.24 | 0.08 | 10.18 | 4.38 | 0.84 | 0.20 | 1.95 | 0.29 | 1.73 | 0.43 | 0.93 | 0.10 | 1.18 | 0.12 | 1.18 | 0.15 |
| other_fin | 1.04 | 0.11 | 14.42 | 4.68 | 1.22 | 0.08 | 2.04 | 0.16 | 2.12 | 0.20 | 1.19 | 0.17 | 0.96 | 0.15 | 0.94 | 0.14 |
| pass_throughs | 1.35 | 0.15 | 18.41 | 6.97 | 0.93 | 0.07 | 1.92 | 0.17 | 1.85 | 0.12 | 2.41 | 0.34 | 1.17 | 0.18 | 0.65 | 0.06 |
| primary_home | 1.06 | 0.16 | 10.51 | 3.45 | 1.21 | 0.11 | 1.33 | 0.14 | 1.15 | 0.10 | 1.49 | 0.26 | 1.05 | 0.12 | 0.66 | 0.06 |
| other_home | 0.62 | 0.01 | 11.28 | 1.34 | 0.94 | 0.10 | 1.59 | 0.19 | 1.45 | 0.15 | 1.07 | 0.13 | 1.07 | 0.13 | 0.39 | 0.04 |
| re_fund | 1.17 | 0.12 | 7.57 | 1.65 | 0.98 | 0.09 | 1.52 | 0.10 | 1.33 | 0.09 | 1.19 | 0.19 | 1.16 | 0.19 | 0.80 | 0.05 |
| other_nonfin | 1.00 | 0.11 | 6.80 | 2.41 | 1.10 | 0.12 | 1.22 | 0.13 | 1.10 | 0.11 | 1.32 | 0.16 | 1.00 | 0.13 | 0.72 | 0.07 |
| primary_mortgage | 1.26 | 0.14 | 5.66 | 1.35 | 1.20 | 0.13 | 1.51 | 0.16 | 1.27 | 0.13 | 1.22 | 0.22 | 1.07 | 0.12 | 0.44 | 0.03 |
| other_mortgage | 0.24 | 0.03 | 5.80 | 2.97 | 0.86 | 0.08 | 1.36 | 0.10 | 1.72 | 0.14 | 1.04 | 0.20 | 1.13 | 0.13 | 0.52 | 0.05 |
| credit_lines | 1.07 | 0.21 | 15.24 | 6.09 | 0.83 | 0.03 | 1.53 | 0.16 | 1.67 | 0.04 | 2.98 | 0.66 | 1.10 | 0.15 | 1.21 | 0.09 |
| credit_cards | 1.09 | 0.11 | 1.56 | 0.23 | 0.87 | 0.10 | 0.89 | 0.09 | 0.91 | 0.09 | 0.96 | 0.11 | 1.01 | 0.11 | 0.72 | 0.08 |
| installment_debt | 0.98 | 0.04 | 2.03 | 0.62 | 0.98 | 0.14 | 0.92 | 0.13 | 0.94 | 0.09 | 0.73 | 0.09 | 0.91 | 0.11 | 0.75 | 0.06 |
| other_debt | 0.65 | 0.13 | 11.51 | 5.70 | 0.78 | 0.10 | 1.24 | 0.16 | 1.22 | 0.14 | 1.59 | 0.34 | 1.16 | 0.10 | 0.91 | 0.06 |
| kg_primary_home | 1.49 | 0.18 | 7.96 | 2.20 | 1.28 | 0.15 | 1.51 | 0.17 | 1.21 | 0.12 | 1.42 | 0.18 | 1.01 | 0.11 | 0.54 | 0.05 |
| kg_other_re | 0.76 | 0.05 | 6.42 | 0.89 | 1.24 | 0.13 | 1.87 | 0.18 | 1.40 | 0.13 | 1.28 | 0.16 | 1.12 | 0.13 | 0.60 | 0.04 |
| kg_pass_throughs | 1.32 | 0.16 | 18.01 | 6.56 | 0.98 | 0.08 | 1.87 | 0.21 | 1.84 | 0.14 | 2.35 | 0.31 | 1.17 | 0.17 | 0.35 | 0.02 |
| kg_other | 1.48 | 0.12 | 15.43 | 6.29 | 1.18 | 0.13 | 1.63 | 0.15 | 1.49 | 0.13 | 1.20 | 0.17 | 1.16 | 0.14 | 0.46 | 0.05 |

**Conditional holding rates (%).** Rows where the second column is an income (business, int_div, capital_gains, rent, ss_pens) mean "among units with that income nonzero".

| Holds | Among those holding | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | one-source model (model_half2_w1_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|---|---|
| primary_home | equities | 87.4 | 90.6 | 89.5 | 90.8 | 81.6 | 82.6 | 73.6 | 1.9 |
| primary_mortgage | primary_home | 66.5 | 65.2 | 66.6 | 65.0 | 69.1 | 69.2 | 67.3 | 1.9 |
| kg_primary_home | primary_home | 94.9 | 92.6 | 94.4 | 94.1 | 94.6 | 94.9 | 91.3 | 1.1 |
| other_home | primary_home | 19.4 | 29.6 | 24.9 | 23.7 | 21.7 | 20.8 | 14.0 | 0.4 |
| equities | dc | 30.2 | 36.6 | 32.1 | 29.5 | 32.3 | 31.7 | 25.1 | 3.1 |
| bonds | equities | 31.3 | 23.4 | 27.3 | 23.8 | 28.8 | 30.5 | 19.2 | 4.0 |
| credit_cards | primary_home | 47.7 | 46.4 | 45.7 | 48.8 | 45.7 | 46.5 | 46.3 | 2.4 |
| pass_throughs | business | 52.6 | 47.8 | 41.6 | 39.8 | 43.8 | 31.2 | 39.5 | 5.9 |
| equities | int_div | 58.9 | 63.9 | 55.7 | 51.2 | 49.6 | 45.7 | 43.3 | 3.8 |
| kg_other | capital_gains | 70.1 | 67.1 | 64.3 | 64.8 | 54.3 | 48.5 | 36.2 | 3.7 |
| other_home | rent | 47.5 | 52.0 | 50.0 | 48.9 | 29.3 | 27.2 | 29.5 | 4.6 |
| db | ss_pens | 8.1 | 8.6 | 7.4 | 7.7 | 8.1 | 8.3 | 6.7 | 1.2 |

**Rank correlation of each amount with total income, among holders** (blank where fewer than 200 true holder records).

| Component | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | one-source model (model_half2_w1_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|---|
| cash | 0.60 | 0.47 | 0.53 | 0.51 | 0.55 | 0.56 | 0.31 | 0.03 |
| equities | 0.43 | 0.38 | 0.43 | 0.46 | 0.38 | 0.40 | 0.13 | 0.05 |
| bonds | 0.38 | 0.28 | 0.36 | 0.39 | 0.38 | 0.33 | 0.11 | 0.11 |
| dc | 0.57 | 0.41 | 0.49 | 0.47 | 0.49 | 0.50 | 0.18 | 0.03 |
| db | 0.29 | 0.17 | 0.22 | 0.24 | 0.32 | 0.32 | 0.07 | 0.14 |
| life_ins | 0.42 | 0.31 | 0.37 | 0.39 | 0.37 | 0.37 | 0.11 | 0.07 |
| annuities | 0.27 | 0.17 | 0.21 | 0.27 | 0.31 | 0.23 | 0.01 | 0.08 |
| trusts | 0.40 | 0.29 | 0.36 | 0.41 | 0.45 | 0.50 | 0.09 | 0.18 |
| other_fin | 0.48 | 0.41 | 0.49 | 0.51 | 0.50 | 0.51 | 0.15 | 0.11 |
| pass_throughs | 0.46 | 0.45 | 0.53 | 0.54 | 0.52 | 0.47 | 0.14 | 0.09 |
| primary_home | 0.59 | 0.48 | 0.54 | 0.55 | 0.54 | 0.56 | 0.22 | 0.03 |
| other_home | 0.44 | 0.37 | 0.45 | 0.47 | 0.44 | 0.43 | 0.12 | 0.10 |
| re_fund | 0.46 | 0.37 | 0.45 | 0.45 | 0.48 | 0.43 | 0.10 | 0.06 |
| other_nonfin | 0.57 | 0.47 | 0.52 | 0.52 | 0.57 | 0.55 | 0.29 | 0.02 |
| primary_mortgage | 0.49 | 0.42 | 0.49 | 0.46 | 0.51 | 0.51 | 0.18 | 0.06 |
| other_mortgage | 0.48 | 0.32 | 0.37 | 0.43 | 0.46 | 0.49 | 0.10 | 0.15 |
| credit_lines | 0.50 | 0.36 | 0.45 | 0.45 | 0.55 | 0.52 | 0.02 |  |
| credit_cards | 0.39 | 0.23 | 0.27 | 0.28 | 0.27 | 0.27 | 0.11 | 0.05 |
| installment_debt | 0.29 | 0.26 | 0.32 | 0.32 | 0.32 | 0.31 | 0.13 | 0.06 |
| other_debt | 0.46 | 0.34 | 0.43 | 0.40 | 0.42 | 0.45 | 0.09 | 0.10 |
| kg_primary_home | 0.22 | 0.20 | 0.26 | 0.27 | 0.16 | 0.17 | 0.03 | 0.04 |
| kg_other_re | 0.25 | 0.26 | 0.34 | 0.35 | 0.33 | 0.30 | 0.05 | 0.03 |
| kg_pass_throughs | 0.40 | 0.40 | 0.48 | 0.48 | 0.42 | 0.40 | 0.10 | 0.09 |
| kg_other | 0.38 | 0.31 | 0.37 | 0.40 | 0.32 | 0.33 | 0.06 | 0.08 |

**What the classifier relies on** (drop in AUC when the variable is scrambled):

- joint model (model_joint_w1_plain): primary_home (0.099), kg_primary_home (0.082), primary_mortgage (0.079), kg_pass_throughs (0.069), pass_throughs (0.067), kg_other_re (0.066)
- joint model (model_joint_w1_asSCF_plain): primary_home (0.072), kg_primary_home (0.063), primary_mortgage (0.059), cash (0.046), kg_other_re (0.040), wages (0.034)
- one-source model (model_half2_w1_plain): primary_home (0.078), kg_primary_home (0.076), kg_other_re (0.074), primary_mortgage (0.061), cash (0.051), other_home (0.046)
- production forest (comp_drf): cash (0.081), other_nonfin (0.076), dc (0.058), installment_debt (0.056), primary_home (0.054), credit_cards (0.053)
- production forest (comp_drfsw): cash (0.070), other_nonfin (0.069), dc (0.062), installment_debt (0.049), int_div (0.049), kg_primary_home (0.047)
- sequential trees (comp_seqtrees): primary_home (0.091), cash (0.075), dc (0.070), other_nonfin (0.062), kg_primary_home (0.057), primary_mortgage (0.057)

<!-- END GENERATED expA_clean -->

<!-- BEGIN GENERATED expA_noise -->
### Dataset `expA_noise`
Evaluation set: 15,380 records in 2,818 households. Generated 2026-10-09 14:47. Runs: joint model (model_joint_w1_plain): 3 seed(s), joint model (model_joint_w1_asSCF_plain): 3 seed(s), production forest (comp_drf): 3 seed(s), production forest (comp_drfsw): 3 seed(s), sequential trees (comp_seqtrees): 3 seed(s).

**Headline.** "Sampling noise" is the typical gap between two random halves of the true records (households kept whole), averaged over five splits; for the level rows it is the typical gap, in points, and for the ratio rows the typical distance from 1. "No information" gives every record the wealth of a random other record. Method columns: mean over training seeds [lowest-highest seed].

| Measure | Truth | Sampling noise | No information (shuffled) | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) |
|---|---|---|---|---|---|---|---|---|
| Share holding each component, all records: average gap (percentage points) |  | 1.02 | 0.12 | 3.77 [3.39-3.97] | 2.16 [2.05-2.23] | 0.89 [0.87-0.92] | 0.99 [0.95-1.03] | 1.34 [1.30-1.38] |
| Share holding, by income group (24 components x 4 groups): average gap (points) |  | 3.15 | 12.91 | 5.04 [4.81-5.21] | 3.83 [3.44-4.29] | 2.39 [2.32-2.45] | 2.12 [2.06-2.19] | 6.69 [6.62-6.77] |
| Share holding, by age range: average gap (points) |  | 2.74 | 6.39 | 4.13 [3.71-4.59] | 2.67 [2.44-2.83] | 1.64 [1.62-1.67] | 2.20 [2.17-2.24] | 3.16 [3.14-3.19] |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks |  | 6.31 | 17.39 | 5.71 [5.54-5.85] | 5.77 [5.55-6.09] | 5.51 [5.36-5.63] | 5.20 [5.06-5.34] | 12.66 [12.57-12.85] |
| Same, in dollars: average gap (%) |  | 63.6 | 241.6 | 53.0 [50.1-55.1] | 53.6 [50.4-56.8] | 53.3 [52.3-54.2] | 50.6 [49.6-51.6] | 154.5 [153.4-156.7] |
| Amounts among holders by age range: average gap in percentile ranks |  | 6.99 | 9.84 | 5.98 [5.83-6.24] | 6.46 [6.28-6.80] | 5.75 [5.67-5.80] | 5.78 [5.73-5.81] | 7.81 [7.76-7.88] |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) |  | 51.1 | 4.7 | 53.6 [50.0-57.9] | 45.1 [44.6-45.8] | 45.8 [45.6-46.0] | 44.4 [44.3-44.5] | 49.1 [48.9-49.3] |
| Pairs of components (276 two-by-two holding tables): average share misplaced (%) |  | 1.84 | 0.21 | 6.12 [5.50-6.59] | 3.51 [3.46-3.58] | 1.65 [1.63-1.68] | 1.71 [1.66-1.77] | 2.79 [2.75-2.83] |
| Selected conditional holding rates (12, e.g. home among stockholders): average gap (points) |  | 2.81 | 13.87 | 4.59 [4.21-5.19] | 4.24 [4.10-4.49] | 5.74 [5.44-6.12] | 7.59 [7.51-7.66] | 10.36 [10.18-10.46] |
| Rank correlation of each amount with income, among holders: average gap |  | 0.078 | 0.420 | 0.072 [0.053-0.093] | 0.047 [0.044-0.051] | 0.046 [0.045-0.047] | 0.042 [0.039-0.044] | 0.297 [0.295-0.299] |
| Rank correlation between amounts, among joint holders: average gap |  | 0.115 | 0.016 | 0.099 [0.095-0.101] | 0.094 [0.089-0.100] | 0.101 [0.099-0.103] | 0.098 [0.095-0.100] | 0.248 [0.245-0.250] |
| Holders in the top 1% rank band of their component: completed / true, averaged over components |  | ±0.35 | 1.00 | 1.27 [1.05-1.65] | 1.95 [1.64-2.39] | 1.74 [1.71-1.79] | 1.43 [1.41-1.45] | 0.94 [0.90-0.97] |
| Same for the top 0.1% rank band |  | ±1.36 | 1.16 | 1.77 [1.08-2.32] | 2.68 [2.03-3.58] | 3.17 [2.87-3.56] | 2.02 [1.79-2.20] | 1.03 [0.88-1.11] |
| Median net worth, completed / true |  | ±0.122 | 0.998 | 1.744 [1.634-1.822] | 1.314 [1.244-1.371] | 1.153 [1.138-1.181] | 1.141 [1.137-1.143] | 1.782 [1.775-1.787] |
| 90th percentile of net worth, completed / true |  | ±0.081 | 1.000 | 1.286 [1.223-1.399] | 1.334 [1.180-1.487] | 1.069 [1.060-1.086] | 1.046 [1.042-1.056] | 0.940 [0.934-0.950] |
| 99th percentile of net worth, completed / true |  | ±0.049 | 0.976 | 1.209 [1.097-1.398] | 1.497 [1.315-1.625] | 1.253 [1.248-1.261] | 0.997 [0.990-1.006] | 0.530 [0.528-0.534] |
| Top 10% share of net worth (%) | 77.6 | ±1.3 | 77.8 | 73.8 [71.6-76.0] | 79.4 [77.2-82.2] | 81.2 [81.1-81.3] | 77.8 [77.7-78.0] | 62.8 [62.1-63.1] |
| Top 1% share of net worth (%) | 39.2 | ±2.6 | 40.3 | 35.4 [33.3-37.3] | 39.4 [35.7-45.3] | 45.4 [45.2-45.5] | 40.6 [39.9-41.3] | 26.6 [25.2-27.3] |
| Top 0.1% share of net worth (%) | 15.3 | ±1.7 | 16.4 | 11.4 [10.1-12.2] | 12.5 [11.3-14.4] | 19.1 [18.7-19.4] | 17.3 [16.6-17.9] | 10.5 [9.2-11.5] |
| Mean net worth by income group: average gap (%) |  | 10.8 | 176.9 | 47.1 [41.2-51.9] | 44.5 [41.9-47.5] | 29.9 [28.1-32.3] | 9.0 [7.8-10.2] | 84.2 [82.0-86.2] |
| Top 1% by net worth: share whose wealth tokens repeat another household's (%) | 0.0 | ±0.0 | 71.7 | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 78.8 [77.9-79.6] | 71.9 [70.6-73.4] | 0.0 [0.0-0.0] |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) |  | 0.539 | 0.797 | 0.773 [0.742-0.794] | 0.709 [0.705-0.712] | 0.805 [0.804-0.806] | 0.790 [0.783-0.795] | 0.768 [0.761-0.773] |

**Other variants** (same measures): runs without `_w1` are the earlier configuration (survey weight as a model variable; `plain` as drawn, `wc` weight-corrected); `_small` smaller model; `_w1`, `_wsqrt` model trained with each record's loss weighted by its population weight or its square root, and `_nowt` the unweighted control (in these three the survey weight is not a model variable and the plain and weight-corrected files are identical); `drfsw` the production forest fitted with survey weights instead of a weighted bootstrap (a diagnostic, not production). The detail tables below show only the main runs.

| Measure | Truth | Sampling noise | No information (shuffled) | joint model (model_joint_asSCF_plain) | joint model (model_joint_asSCF_wc) | joint model (model_joint_plain) | joint model (model_joint_w1_asSCF_wc) | joint model (model_joint_w1_wc) | joint model (model_joint_wc) |
|---|---|---|---|---|---|---|---|---|---|
| Share holding each component, all records: average gap (percentage points) |  | 1.02 | 0.12 | 2.25 [2.08-2.42] | 2.03 [1.71-2.29] | 4.06 [2.81-5.90] | 2.16 [2.05-2.23] | 3.77 [3.39-3.97] | 4.02 [2.78-5.84] |
| Share holding, by income group (24 components x 4 groups): average gap (points) |  | 3.15 | 12.91 | 3.72 [3.43-4.04] | 3.47 [3.29-3.74] | 5.23 [4.12-7.22] | 3.83 [3.44-4.29] | 5.04 [4.81-5.21] | 5.13 [4.16-7.04] |
| Share holding, by age range: average gap (points) |  | 2.74 | 6.39 | 2.71 [2.58-2.81] | 2.59 [2.33-2.72] | 4.43 [3.33-6.05] | 2.67 [2.44-2.83] | 4.13 [3.71-4.59] | 4.40 [3.24-6.04] |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks |  | 6.31 | 17.39 | 6.74 [6.09-7.25] | 5.95 [5.54-6.34] | 6.16 [5.37-6.86] | 5.77 [5.55-6.09] | 5.71 [5.54-5.85] | 5.88 [5.44-6.49] |
| Same, in dollars: average gap (%) |  | 63.6 | 241.6 | 68.7 [62.2-72.0] | 56.5 [53.7-58.7] | 63.1 [50.9-77.3] | 53.8 [50.4-57.5] | 52.9 [50.0-54.9] | 58.6 [51.2-71.6] |
| Amounts among holders by age range: average gap in percentile ranks |  | 6.99 | 9.84 | 7.20 [6.51-7.88] | 6.49 [6.08-7.03] | 6.44 [5.81-7.04] | 6.46 [6.28-6.80] | 5.98 [5.83-6.24] | 6.15 [5.81-6.58] |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) |  | 51.1 | 4.7 | 45.3 [44.9-45.9] | 44.8 [44.6-45.1] | 54.1 [50.4-59.5] | 45.1 [44.6-45.8] | 53.6 [50.0-57.9] | 53.9 [50.1-59.5] |
| Pairs of components (276 two-by-two holding tables): average share misplaced (%) |  | 1.84 | 0.21 | 3.59 [3.48-3.78] | 3.36 [2.97-3.62] | 6.61 [4.69-9.69] | 3.51 [3.46-3.58] | 6.12 [5.50-6.59] | 6.52 [4.58-9.58] |
| Selected conditional holding rates (12, e.g. home among stockholders): average gap (points) |  | 2.81 | 13.87 | 3.91 [3.40-4.42] | 4.08 [3.27-4.63] | 4.59 [3.56-5.25] | 4.24 [4.10-4.49] | 4.59 [4.21-5.19] | 4.60 [3.44-5.31] |
| Rank correlation of each amount with income, among holders: average gap |  | 0.078 | 0.420 | 0.042 [0.041-0.045] | 0.043 [0.040-0.049] | 0.065 [0.062-0.067] | 0.047 [0.044-0.051] | 0.072 [0.053-0.093] | 0.064 [0.062-0.067] |
| Rank correlation between amounts, among joint holders: average gap |  | 0.115 | 0.016 | 0.100 [0.095-0.107] | 0.096 [0.091-0.106] | 0.093 [0.085-0.099] | 0.094 [0.089-0.100] | 0.099 [0.095-0.101] | 0.094 [0.088-0.100] |
| Holders in the top 1% rank band of their component: completed / true, averaged over components |  | ±0.35 | 1.00 | 3.74 [3.39-4.20] | 3.15 [2.86-3.70] | 4.01 [2.90-5.02] | 1.95 [1.64-2.39] | 1.27 [1.05-1.65] | 3.63 [2.75-4.87] |
| Same for the top 0.1% rank band |  | ±1.36 | 1.16 | 7.22 [6.46-8.54] | 6.65 [5.04-8.48] | 10.73 [8.34-13.71] | 2.68 [2.03-3.58] | 1.77 [1.08-2.32] | 10.55 [8.73-14.13] |
| Median net worth, completed / true |  | ±0.122 | 0.998 | 1.455 [1.382-1.528] | 1.395 [1.328-1.456] | 1.931 [1.755-2.181] | 1.315 [1.246-1.369] | 1.745 [1.634-1.820] | 1.876 [1.688-2.096] |
| 90th percentile of net worth, completed / true |  | ±0.081 | 1.000 | 1.567 [1.406-1.672] | 1.334 [1.217-1.396] | 1.710 [1.430-2.197] | 1.333 [1.174-1.483] | 1.286 [1.224-1.397] | 1.528 [1.324-1.905] |
| 99th percentile of net worth, completed / true |  | ±0.049 | 0.976 | 2.764 [2.582-2.990] | 2.059 [1.842-2.368] | 4.425 [3.520-6.235] | 1.504 [1.333-1.625] | 1.205 [1.103-1.377] | 3.738 [2.366-5.722] |
| Top 10% share of net worth (%) | 77.6 | ±1.3 | 77.8 | 88.0 [87.3-88.9] | 87.6 [86.3-89.1] | 92.5 [90.7-93.9] | 79.4 [77.2-82.1] | 73.8 [71.7-76.0] | 92.6 [91.1-94.8] |
| Top 1% share of net worth (%) | 39.2 | ±2.6 | 40.3 | 59.2 [58.7-59.9] | 61.1 [57.8-63.0] | 73.2 [70.8-76.8] | 39.6 [35.9-45.4] | 35.5 [33.4-37.4] | 75.8 [73.6-76.9] |
| Top 0.1% share of net worth (%) | 15.3 | ±1.7 | 16.4 | 25.6 [23.7-26.7] | 30.9 [29.8-32.2] | 30.0 [24.8-33.9] | 12.9 [11.4-15.0] | 11.6 [10.4-12.4] | 33.8 [25.7-41.1] |
| Mean net worth by income group: average gap (%) |  | 10.8 | 176.9 | 223.2 [159.7-275.7] | 189.5 [144.1-264.4] | 699.9 [381.7-1083.1] | 45.4 [43.9-48.1] | 47.5 [41.8-52.0] | 691.6 [420.4-1209.7] |
| Top 1% by net worth: share whose wealth tokens repeat another household's (%) | 0.0 | ±0.0 | 71.7 | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) |  | 0.539 | 0.797 | 0.710 [0.703-0.715] | 0.699 [0.685-0.713] | 0.769 [0.749-0.802] | 0.709 [0.705-0.712] | 0.773 [0.742-0.794] | 0.768 [0.750-0.801] |

**Completion noise** (one completion draw against another, same file): the part of each gap that comes from drawing at random, not from the model.

| Measure | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) |
|---|---|---|---|---|---|
| Share holding, by income group (24 components x 4 groups): average gap (points) | 0.89 | 0.97 | 0.80 | 0.88 | 0.93 |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks | 2.52 | 2.35 | 2.00 | 2.21 | 2.85 |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) | 39.1 | 31.0 | 12.8 | 13.5 | 33.1 |
| Mean net worth by income group: average gap (%) | 8.3 | 8.5 | 6.6 | 4.5 | 7.3 |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) | 0.503 | 0.502 | 0.502 | 0.496 | 0.497 |

**Share holding each component, all records (%).**

| Component | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|
| cash | 95.7 | 96.9 | 97.1 | 95.8 | 95.9 | 95.7 | 0.4 |
| equities | 20.1 | 25.7 | 22.5 | 21.6 | 21.3 | 19.9 | 1.5 |
| bonds | 11.7 | 12.8 | 12.0 | 12.9 | 13.5 | 11.4 | 1.2 |
| dc | 52.2 | 56.6 | 56.2 | 55.9 | 56.4 | 55.7 | 0.4 |
| db | 6.2 | 7.0 | 5.7 | 6.8 | 6.5 | 5.9 | 0.8 |
| life_ins | 20.0 | 17.3 | 16.7 | 18.1 | 18.3 | 18.4 | 2.3 |
| annuities | 4.4 | 4.2 | 3.1 | 4.8 | 4.2 | 3.1 | 0.5 |
| trusts | 2.1 | 1.5 | 1.1 | 2.1 | 1.6 | 1.1 | 0.5 |
| other_fin | 9.9 | 7.4 | 7.3 | 10.1 | 9.3 | 7.9 | 1.4 |
| pass_throughs | 12.9 | 24.0 | 14.7 | 12.1 | 12.1 | 11.0 | 0.5 |
| primary_home | 65.3 | 72.4 | 67.7 | 65.0 | 66.4 | 66.0 | 1.8 |
| other_home | 15.0 | 24.8 | 19.3 | 16.2 | 15.9 | 12.2 | 0.3 |
| re_fund | 6.9 | 9.4 | 8.8 | 7.1 | 6.4 | 5.3 | 1.0 |
| other_nonfin | 86.8 | 87.3 | 87.3 | 86.9 | 87.2 | 87.3 | 1.3 |
| primary_mortgage | 43.4 | 48.6 | 45.0 | 44.7 | 45.7 | 45.1 | 1.2 |
| other_mortgage | 6.0 | 7.7 | 6.8 | 6.1 | 6.4 | 3.9 | 0.8 |
| credit_lines | 1.8 | 1.7 | 1.4 | 2.1 | 2.0 | 1.5 | 0.4 |
| credit_cards | 46.3 | 45.4 | 42.7 | 44.3 | 44.7 | 45.2 | 1.4 |
| installment_debt | 52.8 | 50.4 | 50.7 | 50.1 | 51.8 | 51.8 | 1.7 |
| other_debt | 5.5 | 5.2 | 4.2 | 5.5 | 5.8 | 5.2 | 0.6 |
| kg_primary_home | 62.0 | 67.2 | 64.4 | 61.7 | 63.1 | 61.3 | 2.1 |
| kg_other_re | 16.8 | 29.4 | 23.2 | 17.2 | 16.6 | 14.6 | 0.8 |
| kg_pass_throughs | 12.1 | 16.7 | 12.4 | 10.6 | 10.8 | 8.6 | 0.6 |
| kg_other | 18.3 | 20.8 | 19.6 | 18.8 | 19.1 | 16.9 | 1.0 |

**Share holding by income group, selected components (%).** Income group: weighted rank of the total of the seven incomes within source and year.

| Component | Income group | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|---|
| primary_home | Bottom 50% | 47.4 | 56.6 | 48.4 | 46.2 | 48.4 | 56.0 | 2.2 |
| primary_home | 50-90% | 75.9 | 81.8 | 79.2 | 76.1 | 77.2 | 71.6 | 1.7 |
| primary_home | 90-99% | 91.1 | 94.9 | 95.6 | 92.1 | 92.2 | 81.9 | 3.6 |
| primary_home | Top 1% | 95.1 | 97.2 | 97.1 | 97.1 | 95.5 | 85.9 | 5.1 |
| equities | Bottom 50% | 8.9 | 12.9 | 9.8 | 9.5 | 8.9 | 12.4 | 0.5 |
| equities | 50-90% | 21.1 | 28.9 | 25.1 | 24.2 | 24.3 | 21.7 | 3.8 |
| equities | 90-99% | 55.1 | 58.7 | 57.6 | 54.6 | 53.6 | 39.4 | 5.1 |
| equities | Top 1% | 88.9 | 82.8 | 81.5 | 78.3 | 82.5 | 55.5 | 4.5 |
| pass_throughs | Bottom 50% | 6.8 | 14.4 | 6.1 | 6.0 | 6.2 | 7.1 | 1.7 |
| pass_throughs | 50-90% | 13.7 | 26.0 | 15.4 | 12.0 | 12.5 | 11.4 | 1.5 |
| pass_throughs | 90-99% | 30.0 | 49.4 | 41.6 | 30.2 | 28.9 | 22.2 | 2.9 |
| pass_throughs | Top 1% | 58.2 | 74.5 | 71.1 | 79.4 | 69.5 | 39.6 | 8.9 |
| dc | Bottom 50% | 28.9 | 37.4 | 34.9 | 33.3 | 34.4 | 40.5 | 2.9 |
| dc | 50-90% | 65.7 | 67.9 | 68.8 | 69.0 | 69.5 | 64.9 | 2.7 |
| dc | 90-99% | 88.0 | 84.9 | 87.6 | 90.6 | 88.7 | 77.2 | 1.3 |
| dc | Top 1% | 85.9 | 89.0 | 89.7 | 88.3 | 90.9 | 77.9 | 9.9 |
| primary_mortgage | Bottom 50% | 25.1 | 32.3 | 26.6 | 25.9 | 26.9 | 34.8 | 0.8 |
| primary_mortgage | 50-90% | 55.2 | 58.8 | 56.8 | 56.9 | 57.6 | 51.1 | 2.5 |
| primary_mortgage | 90-99% | 68.1 | 71.1 | 70.3 | 69.5 | 71.0 | 61.0 | 3.6 |
| primary_mortgage | Top 1% | 57.4 | 66.4 | 61.8 | 58.6 | 65.9 | 60.6 | 11.9 |
| bonds | Bottom 50% | 6.4 | 7.3 | 5.9 | 6.2 | 6.6 | 8.5 | 1.4 |
| bonds | 50-90% | 12.1 | 14.4 | 13.3 | 14.3 | 14.6 | 11.8 | 1.9 |
| bonds | 90-99% | 28.7 | 26.1 | 28.5 | 31.5 | 34.0 | 19.7 | 2.7 |
| bonds | Top 1% | 45.4 | 38.6 | 44.6 | 47.6 | 44.0 | 27.0 | 5.2 |
| trusts | Bottom 50% | 1.3 | 0.9 | 0.5 | 0.8 | 0.6 | 0.7 | 0.5 |
| trusts | 50-90% | 1.8 | 1.6 | 1.1 | 2.1 | 1.5 | 1.0 | 0.9 |
| trusts | 90-99% | 4.5 | 3.5 | 3.3 | 5.7 | 5.0 | 2.5 | 2.0 |
| trusts | Top 1% | 18.0 | 7.2 | 7.2 | 13.8 | 12.9 | 6.7 | 2.2 |
| other_home | Bottom 50% | 6.7 | 15.2 | 9.3 | 8.0 | 8.5 | 8.5 | 0.8 |
| other_home | 50-90% | 17.0 | 27.3 | 21.0 | 17.7 | 16.0 | 12.6 | 1.0 |
| other_home | 90-99% | 35.6 | 49.7 | 47.8 | 38.5 | 41.3 | 23.8 | 3.5 |
| other_home | Top 1% | 64.8 | 70.2 | 69.4 | 69.3 | 59.6 | 37.0 | 12.1 |

**Net worth** (assets minus debts, dollars of the survey year; the four unrealized-gain variables are not part of net worth).

| Net worth statistic | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (± %) |
|---|---|---|---|---|---|---|---|
| p10 | -1k | 0k | 0k | -1k | -0k | 1k |  |
| p25 | 11k | 23k | 13k | 11k | 12k | 39k | 15.8 |
| p50 | 93k | 163k | 123k | 108k | 107k | 167k | 12.2 |
| p75 | 385k | 540k | 477k | 432k | 402k | 463k | 3.8 |
| p90 | 1,188k | 1,527k | 1,584k | 1,270k | 1,243k | 1,116k | 8.1 |
| p99 | 10,681k | 12,914k | 15,992k | 13,384k | 10,651k | 5,662k | 4.9 |
| mean | 725k | 883k | 998k | 952k | 765k | 532k | 3.6 |
| top10%_share | 77.6% | 73.8% | 79.4% | 81.2% | 77.8% | 62.8% | 1.6 |
| top1%_share | 39.2% | 35.4% | 39.4% | 45.4% | 40.6% | 26.6% | 6.4 |
| top0.1%_share | 15.3% | 11.4% | 12.5% | 19.1% | 17.3% | 10.5% | 9.9 |

| Income group | Truth, mean | Truth, median | joint model (model_joint_w1_plain), mean | joint model (model_joint_w1_plain), median | joint model (model_joint_w1_asSCF_plain), mean | joint model (model_joint_w1_asSCF_plain), median | production forest (comp_drf), mean | production forest (comp_drf), median | production forest (comp_drfsw), mean | production forest (comp_drfsw), median | sequential trees (comp_seqtrees), mean | sequential trees (comp_seqtrees), median | Sampling noise, mean (± %) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Bottom 50% | 127k | 18k | 250k | 46k | 196k | 22k | 165k | 20k | 141k | 23k | 345k | 96k | 12.3 |
| 50-90% | 418k | 154k | 622k | 230k | 630k | 189k | 442k | 161k | 393k | 156k | 527k | 191k | 10.0 |
| 90-99% | 2,389k | 1,100k | 2,965k | 1,208k | 3,752k | 1,447k | 3,063k | 1,265k | 2,646k | 1,286k | 1,120k | 527k | 8.8 |
| Top 1% | 20,116k | 10,307k | 16,116k | 8,311k | 20,762k | 11,181k | 31,356k | 14,287k | 21,759k | 10,409k | 2,540k | 1,417k | 12.1 |

**The top of each component, in rank.** Share of holders (with a positive amount) in the top 1% and top 0.1% rank bands of that component (%). By construction the weighted truth is near 1% and 0.1%; a method above that puts too many holders at the very top, which inflates net worth at the top once decoded to dollars. The unweighted columns count sample records instead of population weight: they show how far the SCF sample over-represents the top, which is what a model that learned the sample, not the population, would reproduce.

| Component | Truth, top 1% | Truth, top 0.1% | Truth unweighted (sample), top 1% | Truth unweighted (sample), top 0.1% | joint model (model_joint_w1_plain), top 1% | joint model (model_joint_w1_plain), top 0.1% | joint model (model_joint_w1_asSCF_plain), top 1% | joint model (model_joint_w1_asSCF_plain), top 0.1% | production forest (comp_drf), top 1% | production forest (comp_drf), top 0.1% | production forest (comp_drfsw), top 1% | production forest (comp_drfsw), top 0.1% | sequential trees (comp_seqtrees), top 1% | sequential trees (comp_seqtrees), top 0.1% |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| cash | 0.83 | 0.13 | 6.57 | 2.83 | 1.11 | 0.12 | 1.16 | 0.12 | 1.21 | 0.13 | 1.03 | 0.13 | 0.75 | 0.08 |
| equities | 0.81 | 0.11 | 12.89 | 4.92 | 1.23 | 0.12 | 1.76 | 0.17 | 1.30 | 0.20 | 1.14 | 0.14 | 0.49 | 0.06 |
| bonds | 0.77 | 0.07 | 12.61 | 4.82 | 0.90 | 0.12 | 1.61 | 0.21 | 1.29 | 0.11 | 1.04 | 0.12 | 0.66 | 0.06 |
| dc | 0.55 | 0.07 | 4.20 | 1.37 | 1.23 | 0.12 | 1.47 | 0.12 | 0.99 | 0.13 | 0.98 | 0.11 | 0.59 | 0.08 |
| db | 4.12 | 0.07 | 8.06 | 1.08 | 0.86 | 0.17 | 1.33 | 0.31 | 0.84 | 0.03 | 1.16 | 0.11 | 0.83 | 0.14 |
| life_ins | 0.70 | 0.03 | 7.59 | 2.70 | 1.28 | 0.11 | 1.84 | 0.16 | 1.21 | 0.18 | 1.06 | 0.12 | 0.68 | 0.06 |
| annuities | 1.12 | 0.17 | 5.71 | 2.07 | 0.61 | 0.12 | 0.97 | 0.15 | 2.47 | 0.03 | 0.97 | 0.09 | 0.82 | 0.11 |
| trusts | 0.24 | 0.08 | 10.18 | 4.38 | 0.80 | 0.14 | 1.76 | 0.33 | 0.73 | 0.12 | 1.35 | 0.15 | 1.19 | 0.16 |
| other_fin | 1.04 | 0.11 | 14.42 | 4.68 | 1.19 | 0.08 | 2.03 | 0.15 | 1.22 | 0.23 | 1.06 | 0.15 | 0.95 | 0.14 |
| pass_throughs | 1.35 | 0.15 | 18.41 | 6.97 | 0.99 | 0.08 | 2.03 | 0.19 | 2.52 | 0.38 | 1.21 | 0.20 | 0.64 | 0.06 |
| primary_home | 1.06 | 0.16 | 10.51 | 3.45 | 1.16 | 0.10 | 1.34 | 0.13 | 1.44 | 0.28 | 1.05 | 0.13 | 0.65 | 0.06 |
| other_home | 0.62 | 0.01 | 11.28 | 1.34 | 0.88 | 0.10 | 1.46 | 0.16 | 1.21 | 0.19 | 1.05 | 0.12 | 0.39 | 0.04 |
| re_fund | 1.17 | 0.12 | 7.57 | 1.65 | 1.00 | 0.09 | 1.50 | 0.13 | 1.46 | 0.26 | 1.12 | 0.15 | 0.81 | 0.05 |
| other_nonfin | 1.00 | 0.11 | 6.80 | 2.41 | 1.10 | 0.13 | 1.24 | 0.14 | 1.41 | 0.18 | 1.02 | 0.13 | 0.72 | 0.07 |
| primary_mortgage | 1.26 | 0.14 | 5.66 | 1.35 | 1.14 | 0.13 | 1.50 | 0.16 | 1.28 | 0.22 | 1.13 | 0.13 | 0.44 | 0.03 |
| other_mortgage | 0.24 | 0.03 | 5.80 | 2.97 | 0.74 | 0.07 | 1.19 | 0.08 | 1.17 | 0.22 | 1.03 | 0.09 | 0.53 | 0.05 |
| credit_lines | 1.07 | 0.21 | 15.24 | 6.09 | 1.13 | 0.06 | 1.81 | 0.22 | 2.59 | 0.70 | 1.15 | 0.12 | 1.16 | 0.10 |
| credit_cards | 1.09 | 0.11 | 1.56 | 0.23 | 0.88 | 0.09 | 0.94 | 0.10 | 1.04 | 0.08 | 1.05 | 0.10 | 0.71 | 0.08 |
| installment_debt | 0.98 | 0.04 | 2.03 | 0.62 | 0.98 | 0.13 | 0.97 | 0.13 | 0.89 | 0.12 | 0.96 | 0.11 | 0.76 | 0.06 |
| other_debt | 0.65 | 0.13 | 11.51 | 5.70 | 0.87 | 0.12 | 1.55 | 0.19 | 1.97 | 0.43 | 1.14 | 0.11 | 0.90 | 0.08 |
| kg_primary_home | 1.49 | 0.18 | 7.96 | 2.20 | 1.22 | 0.12 | 1.46 | 0.15 | 1.41 | 0.20 | 1.01 | 0.12 | 0.53 | 0.05 |
| kg_other_re | 0.76 | 0.05 | 6.42 | 0.89 | 1.07 | 0.11 | 1.64 | 0.14 | 1.48 | 0.18 | 1.06 | 0.13 | 0.63 | 0.04 |
| kg_pass_throughs | 1.32 | 0.16 | 18.01 | 6.56 | 1.04 | 0.09 | 1.87 | 0.22 | 2.49 | 0.35 | 1.22 | 0.18 | 0.36 | 0.02 |
| kg_other | 1.48 | 0.12 | 15.43 | 6.29 | 1.18 | 0.12 | 1.59 | 0.16 | 1.53 | 0.20 | 1.15 | 0.13 | 0.46 | 0.05 |

**Conditional holding rates (%).** Rows where the second column is an income (business, int_div, capital_gains, rent, ss_pens) mean "among units with that income nonzero".

| Holds | Among those holding | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|---|
| primary_home | equities | 87.4 | 89.9 | 89.2 | 82.3 | 82.4 | 73.7 | 1.9 |
| primary_mortgage | primary_home | 66.5 | 66.4 | 66.3 | 68.8 | 68.9 | 67.3 | 1.9 |
| kg_primary_home | primary_home | 94.9 | 92.3 | 94.8 | 95.0 | 95.0 | 91.2 | 1.1 |
| other_home | primary_home | 19.4 | 31.8 | 26.5 | 21.9 | 20.8 | 14.0 | 0.4 |
| equities | dc | 30.2 | 34.7 | 32.0 | 32.3 | 31.6 | 25.2 | 3.1 |
| bonds | equities | 31.3 | 25.4 | 27.9 | 30.0 | 30.2 | 19.2 | 4.0 |
| credit_cards | primary_home | 47.7 | 47.9 | 45.7 | 46.3 | 46.6 | 46.4 | 2.4 |
| pass_throughs | business | 52.6 | 46.9 | 39.0 | 42.9 | 30.6 | 39.4 | 5.9 |
| equities | int_div | 58.9 | 61.9 | 56.1 | 49.4 | 45.0 | 43.2 | 3.8 |
| kg_other | capital_gains | 70.1 | 65.7 | 63.8 | 53.4 | 48.2 | 36.2 | 3.7 |
| other_home | rent | 47.5 | 53.9 | 52.6 | 29.8 | 26.9 | 29.4 | 4.6 |
| db | ss_pens | 8.1 | 8.6 | 7.5 | 8.5 | 8.4 | 6.7 | 1.2 |

**Rank correlation of each amount with total income, among holders** (blank where fewer than 200 true holder records).

| Component | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|
| cash | 0.58 | 0.47 | 0.52 | 0.56 | 0.56 | 0.31 | 0.03 |
| equities | 0.44 | 0.39 | 0.44 | 0.38 | 0.41 | 0.13 | 0.06 |
| bonds | 0.38 | 0.31 | 0.38 | 0.33 | 0.33 | 0.10 | 0.09 |
| dc | 0.54 | 0.43 | 0.49 | 0.48 | 0.50 | 0.18 | 0.03 |
| db | 0.26 | 0.20 | 0.22 | 0.39 | 0.32 | 0.08 | 0.16 |
| life_ins | 0.42 | 0.32 | 0.37 | 0.37 | 0.36 | 0.11 | 0.08 |
| annuities | 0.30 | 0.18 | 0.22 | 0.35 | 0.23 | 0.02 | 0.10 |
| trusts | 0.41 | 0.32 | 0.39 | 0.44 | 0.51 | 0.09 | 0.19 |
| other_fin | 0.46 | 0.43 | 0.49 | 0.47 | 0.50 | 0.15 | 0.11 |
| pass_throughs | 0.46 | 0.44 | 0.51 | 0.51 | 0.47 | 0.14 | 0.09 |
| primary_home | 0.56 | 0.48 | 0.54 | 0.54 | 0.55 | 0.22 | 0.03 |
| other_home | 0.43 | 0.37 | 0.45 | 0.41 | 0.43 | 0.12 | 0.09 |
| re_fund | 0.44 | 0.39 | 0.45 | 0.50 | 0.43 | 0.09 | 0.05 |
| other_nonfin | 0.54 | 0.46 | 0.52 | 0.57 | 0.55 | 0.28 | 0.03 |
| primary_mortgage | 0.45 | 0.41 | 0.49 | 0.50 | 0.51 | 0.18 | 0.05 |
| other_mortgage | 0.43 | 0.32 | 0.36 | 0.44 | 0.48 | 0.10 | 0.14 |
| credit_lines | 0.55 | 0.40 | 0.47 | 0.57 | 0.53 | 0.00 |  |
| credit_cards | 0.39 | 0.23 | 0.27 | 0.29 | 0.28 | 0.11 | 0.06 |
| installment_debt | 0.27 | 0.26 | 0.32 | 0.30 | 0.31 | 0.13 | 0.05 |
| other_debt | 0.40 | 0.36 | 0.42 | 0.45 | 0.45 | 0.08 | 0.12 |
| kg_primary_home | 0.21 | 0.21 | 0.28 | 0.16 | 0.17 | 0.03 | 0.03 |
| kg_other_re | 0.24 | 0.27 | 0.35 | 0.33 | 0.30 | 0.05 | 0.03 |
| kg_pass_throughs | 0.41 | 0.40 | 0.48 | 0.40 | 0.40 | 0.10 | 0.09 |
| kg_other | 0.37 | 0.33 | 0.38 | 0.32 | 0.33 | 0.06 | 0.07 |

**What the classifier relies on** (drop in AUC when the variable is scrambled):

- joint model (model_joint_w1_plain): primary_home (0.086), pass_throughs (0.085), kg_primary_home (0.079), kg_other_re (0.077), kg_pass_throughs (0.071), primary_mortgage (0.060)
- joint model (model_joint_w1_asSCF_plain): primary_home (0.068), kg_primary_home (0.063), primary_mortgage (0.054), kg_other_re (0.048), cash (0.046), kg_pass_throughs (0.038)
- production forest (comp_drf): cash (0.082), other_nonfin (0.075), primary_mortgage (0.058), primary_home (0.056), dc (0.055), kg_primary_home (0.055)
- production forest (comp_drfsw): cash (0.074), other_nonfin (0.066), dc (0.060), primary_home (0.057), kg_primary_home (0.053), installment_debt (0.048)
- sequential trees (comp_seqtrees): primary_home (0.092), cash (0.072), dc (0.069), other_nonfin (0.066), primary_mortgage (0.056), equities (0.053)

<!-- END GENERATED expA_noise -->

<!-- BEGIN GENERATED expA_recat -->
### Dataset `expA_recat`
Evaluation set: 15,380 records in 2,818 households. Generated 2026-10-09 14:47. Runs: joint model (model_joint_w1_plain): 3 seed(s), joint model (model_joint_w1_asSCF_plain): 3 seed(s), production forest (comp_drf): 3 seed(s), production forest (comp_drfsw): 3 seed(s), sequential trees (comp_seqtrees): 3 seed(s).

**Headline.** "Sampling noise" is the typical gap between two random halves of the true records (households kept whole), averaged over five splits; for the level rows it is the typical gap, in points, and for the ratio rows the typical distance from 1. "No information" gives every record the wealth of a random other record. Method columns: mean over training seeds [lowest-highest seed].

| Measure | Truth | Sampling noise | No information (shuffled) | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) |
|---|---|---|---|---|---|---|---|---|
| Share holding each component, all records: average gap (percentage points) |  | 1.02 | 0.12 | 4.41 [3.76-5.44] | 2.10 [1.95-2.27] | 0.88 [0.84-0.91] | 1.03 [0.99-1.12] | 1.35 [1.33-1.37] |
| Share holding, by income group (24 components x 4 groups): average gap (points) |  | 3.52 | 13.02 | 5.65 [4.82-6.72] | 4.02 [3.73-4.33] | 2.68 [2.62-2.73] | 2.35 [2.29-2.38] | 6.78 [6.68-6.85] |
| Share holding, by age range: average gap (points) |  | 2.74 | 6.39 | 4.95 [4.51-5.76] | 2.67 [2.51-2.91] | 1.65 [1.57-1.71] | 2.23 [2.22-2.24] | 3.20 [3.18-3.23] |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks |  | 6.63 | 17.84 | 5.80 [5.62-6.00] | 5.70 [5.37-6.27] | 5.68 [5.65-5.71] | 4.98 [4.89-5.07] | 13.20 [13.17-13.26] |
| Same, in dollars: average gap (%) |  | 69.8 | 244.9 | 51.0 [49.5-52.9] | 51.2 [47.6-56.1] | 52.6 [52.2-53.1] | 45.0 [44.1-46.3] | 158.6 [157.6-159.8] |
| Amounts among holders by age range: average gap in percentile ranks |  | 6.99 | 9.84 | 6.15 [5.69-6.58] | 6.36 [5.90-6.88] | 5.99 [5.91-6.03] | 5.88 [5.79-5.94] | 7.78 [7.71-7.82] |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) |  | 51.1 | 4.7 | 54.0 [50.5-57.0] | 44.9 [44.2-45.3] | 45.9 [45.7-46.1] | 44.4 [44.2-44.6] | 49.3 [49.0-49.6] |
| Pairs of components (276 two-by-two holding tables): average share misplaced (%) |  | 1.84 | 0.21 | 7.03 [6.11-8.61] | 3.40 [3.05-3.60] | 1.61 [1.55-1.66] | 1.78 [1.73-1.88] | 2.87 [2.84-2.88] |
| Selected conditional holding rates (12, e.g. home among stockholders): average gap (points) |  | 2.81 | 13.87 | 5.14 [4.15-6.76] | 4.25 [3.98-4.71] | 5.89 [5.67-6.13] | 7.48 [7.38-7.54] | 10.28 [10.15-10.37] |
| Rank correlation of each amount with income, among holders: average gap |  | 0.076 | 0.429 | 0.084 [0.066-0.100] | 0.050 [0.044-0.056] | 0.043 [0.040-0.045] | 0.038 [0.036-0.041] | 0.305 [0.304-0.307] |
| Rank correlation between amounts, among joint holders: average gap |  | 0.115 | 0.016 | 0.102 [0.096-0.105] | 0.095 [0.091-0.098] | 0.103 [0.102-0.106] | 0.097 [0.096-0.098] | 0.247 [0.246-0.250] |
| Holders in the top 1% rank band of their component: completed / true, averaged over components |  | ±0.35 | 1.00 | 1.40 [1.25-1.59] | 1.93 [1.59-2.31] | 1.64 [1.59-1.70] | 1.39 [1.33-1.48] | 0.94 [0.94-0.95] |
| Same for the top 0.1% rank band |  | ±1.36 | 1.16 | 1.96 [1.92-2.01] | 2.75 [2.26-3.46] | 2.60 [2.31-2.76] | 2.00 [1.88-2.11] | 1.02 [0.87-1.15] |
| Median net worth, completed / true |  | ±0.122 | 0.998 | 1.881 [1.762-2.013] | 1.299 [1.229-1.363] | 1.200 [1.172-1.218] | 1.142 [1.138-1.151] | 1.799 [1.797-1.802] |
| 90th percentile of net worth, completed / true |  | ±0.081 | 1.000 | 1.397 [1.226-1.519] | 1.311 [1.147-1.507] | 1.062 [1.057-1.069] | 1.045 [1.034-1.058] | 0.942 [0.932-0.955] |
| 99th percentile of net worth, completed / true |  | ±0.049 | 0.976 | 1.336 [1.233-1.482] | 1.442 [1.243-1.547] | 1.249 [1.246-1.255] | 0.985 [0.974-0.993] | 0.533 [0.526-0.541] |
| Top 10% share of net worth (%) | 77.6 | ±1.3 | 77.8 | 74.8 [74.3-75.6] | 79.6 [77.5-81.6] | 80.4 [80.2-80.6] | 77.5 [77.4-77.7] | 62.7 [62.1-63.1] |
| Top 1% share of net worth (%) | 39.2 | ±2.6 | 40.3 | 35.8 [33.4-38.5] | 40.1 [37.5-45.1] | 44.4 [44.3-44.5] | 40.4 [40.1-40.6] | 26.5 [25.1-27.2] |
| Top 0.1% share of net worth (%) | 15.3 | ±1.7 | 16.4 | 11.9 [9.1-14.1] | 13.6 [13.0-14.5] | 18.5 [18.3-18.7] | 17.2 [17.1-17.6] | 10.4 [9.2-11.4] |
| Mean net worth by income group: average gap (%) |  | 12.0 | 178.7 | 64.0 [61.0-66.2] | 45.3 [39.8-54.7] | 26.3 [25.4-27.1] | 10.4 [10.0-10.8] | 87.4 [85.1-89.5] |
| Top 1% by net worth: share whose wealth tokens repeat another household's (%) | 0.0 | ±0.0 | 71.7 | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 79.9 [79.6-80.3] | 72.7 [71.5-74.0] | 0.0 [0.0-0.0] |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) |  | 0.538 | 0.805 | 0.788 [0.770-0.810] | 0.715 [0.703-0.724] | 0.812 [0.809-0.817] | 0.791 [0.787-0.795] | 0.774 [0.767-0.778] |

**Other variants** (same measures): runs without `_w1` are the earlier configuration (survey weight as a model variable; `plain` as drawn, `wc` weight-corrected); `_small` smaller model; `_w1`, `_wsqrt` model trained with each record's loss weighted by its population weight or its square root, and `_nowt` the unweighted control (in these three the survey weight is not a model variable and the plain and weight-corrected files are identical); `drfsw` the production forest fitted with survey weights instead of a weighted bootstrap (a diagnostic, not production). The detail tables below show only the main runs.

| Measure | Truth | Sampling noise | No information (shuffled) | joint model (model_joint_asSCF_plain) | joint model (model_joint_asSCF_wc) | joint model (model_joint_plain) | joint model (model_joint_w1_asSCF_wc) | joint model (model_joint_w1_wc) | joint model (model_joint_wc) |
|---|---|---|---|---|---|---|---|---|---|
| Share holding each component, all records: average gap (percentage points) |  | 1.02 | 0.12 | 2.23 [2.12-2.39] | 2.07 [1.87-2.30] | 5.18 [3.67-7.50] | 2.10 [1.95-2.27] | 4.41 [3.76-5.44] | 5.10 [3.49-7.54] |
| Share holding, by income group (24 components x 4 groups): average gap (points) |  | 3.52 | 13.02 | 3.97 [3.73-4.13] | 3.72 [3.66-3.82] | 6.63 [5.42-8.66] | 4.02 [3.73-4.33] | 5.65 [4.82-6.72] | 6.58 [5.45-8.52] |
| Share holding, by age range: average gap (points) |  | 2.74 | 6.39 | 2.77 [2.60-2.99] | 2.68 [2.49-3.02] | 5.45 [4.17-7.51] | 2.67 [2.51-2.91] | 4.95 [4.51-5.76] | 5.46 [4.12-7.58] |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks |  | 6.63 | 17.84 | 6.37 [5.91-6.95] | 5.66 [5.28-6.18] | 6.06 [5.57-6.45] | 5.70 [5.37-6.27] | 5.80 [5.62-6.00] | 5.76 [5.48-5.99] |
| Same, in dollars: average gap (%) |  | 69.8 | 244.9 | 63.7 [59.7-68.5] | 53.4 [49.9-56.9] | 58.8 [53.4-65.9] | 50.9 [47.4-55.8] | 50.8 [49.4-52.5] | 53.6 [51.3-57.6] |
| Amounts among holders by age range: average gap in percentile ranks |  | 6.99 | 9.84 | 7.20 [6.63-8.05] | 6.65 [6.26-7.34] | 6.54 [6.20-6.80] | 6.36 [5.90-6.88] | 6.15 [5.69-6.58] | 6.24 [6.19-6.32] |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) |  | 51.1 | 4.7 | 45.6 [45.5-45.8] | 45.0 [44.7-45.3] | 54.4 [50.6-59.7] | 44.9 [44.2-45.3] | 54.0 [50.5-57.0] | 53.8 [49.5-59.6] |
| Pairs of components (276 two-by-two holding tables): average share misplaced (%) |  | 1.84 | 0.21 | 3.63 [3.47-3.79] | 3.48 [3.23-3.84] | 8.07 [5.92-11.64] | 3.40 [3.05-3.60] | 7.03 [6.11-8.61] | 7.94 [5.62-11.67] |
| Selected conditional holding rates (12, e.g. home among stockholders): average gap (points) |  | 2.81 | 13.87 | 3.82 [3.35-4.68] | 4.04 [3.28-4.82] | 6.08 [4.46-8.67] | 4.25 [3.98-4.71] | 5.14 [4.15-6.76] | 5.84 [4.23-8.33] |
| Rank correlation of each amount with income, among holders: average gap |  | 0.076 | 0.429 | 0.050 [0.045-0.054] | 0.051 [0.043-0.056] | 0.061 [0.059-0.064] | 0.050 [0.044-0.056] | 0.084 [0.066-0.100] | 0.060 [0.058-0.061] |
| Rank correlation between amounts, among joint holders: average gap |  | 0.115 | 0.016 | 0.103 [0.101-0.104] | 0.100 [0.098-0.103] | 0.089 [0.088-0.091] | 0.095 [0.091-0.098] | 0.102 [0.096-0.105] | 0.091 [0.091-0.091] |
| Holders in the top 1% rank band of their component: completed / true, averaged over components |  | ±0.35 | 1.00 | 3.76 [3.58-3.89] | 3.20 [3.10-3.34] | 3.61 [3.46-3.70] | 1.93 [1.59-2.31] | 1.40 [1.25-1.59] | 3.22 [3.08-3.36] |
| Same for the top 0.1% rank band |  | ±1.36 | 1.16 | 7.03 [6.64-7.23] | 6.06 [5.21-6.65] | 8.78 [8.17-9.13] | 2.75 [2.26-3.46] | 1.96 [1.92-2.01] | 8.57 [7.73-9.33] |
| Median net worth, completed / true |  | ±0.122 | 0.998 | 1.395 [1.302-1.472] | 1.349 [1.270-1.426] | 1.980 [1.807-2.301] | 1.298 [1.229-1.362] | 1.883 [1.760-2.020] | 1.924 [1.752-2.222] |
| 90th percentile of net worth, completed / true |  | ±0.081 | 1.000 | 1.527 [1.438-1.582] | 1.312 [1.234-1.359] | 1.818 [1.553-2.304] | 1.310 [1.149-1.499] | 1.396 [1.225-1.525] | 1.618 [1.419-1.970] |
| 99th percentile of net worth, completed / true |  | ±0.049 | 0.976 | 2.809 [2.536-2.996] | 2.071 [2.010-2.126] | 4.291 [3.655-4.871] | 1.428 [1.252-1.521] | 1.328 [1.232-1.468] | 3.734 [2.707-4.378] |
| Top 10% share of net worth (%) | 77.6 | ±1.3 | 77.8 | 88.5 [87.9-89.0] | 87.9 [87.7-88.1] | 91.1 [89.8-92.1] | 79.6 [77.5-81.5] | 74.8 [74.2-75.6] | 91.6 [90.8-92.4] |
| Top 1% share of net worth (%) | 39.2 | ±2.6 | 40.3 | 59.7 [58.6-60.2] | 61.1 [59.7-63.7] | 67.7 [61.6-73.9] | 40.1 [37.5-45.2] | 35.8 [33.5-38.4] | 71.3 [66.4-77.3] |
| Top 0.1% share of net worth (%) | 15.3 | ±1.7 | 16.4 | 26.9 [23.3-30.2] | 31.7 [31.5-32.2] | 29.7 [25.9-33.0] | 13.7 [12.8-15.0] | 11.9 [9.4-13.6] | 31.7 [27.6-38.0] |
| Mean net worth by income group: average gap (%) |  | 12.0 | 178.7 | 204.4 [181.2-221.7] | 187.6 [166.1-212.5] | 500.5 [431.5-536.1] | 46.1 [41.0-56.2] | 64.1 [59.6-67.6] | 543.6 [457.0-649.6] |
| Top 1% by net worth: share whose wealth tokens repeat another household's (%) | 0.0 | ±0.0 | 71.7 | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) |  | 0.538 | 0.805 | 0.705 [0.701-0.711] | 0.703 [0.698-0.706] | 0.776 [0.757-0.806] | 0.715 [0.703-0.724] | 0.788 [0.770-0.810] | 0.779 [0.759-0.815] |

**Completion noise** (one completion draw against another, same file): the part of each gap that comes from drawing at random, not from the model.

| Measure | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) |
|---|---|---|---|---|---|
| Share holding, by income group (24 components x 4 groups): average gap (points) | 0.97 | 1.01 | 0.88 | 0.94 | 1.01 |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks | 2.65 | 2.67 | 2.05 | 2.19 | 2.78 |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) | 40.1 | 30.6 | 12.4 | 13.7 | 32.8 |
| Mean net worth by income group: average gap (%) | 7.5 | 6.1 | 4.2 | 3.8 | 7.2 |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) | 0.496 | 0.503 | 0.498 | 0.498 | 0.495 |

**Share holding each component, all records (%).**

| Component | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|
| cash | 95.7 | 97.5 | 97.6 | 95.9 | 96.0 | 96.0 | 0.4 |
| equities | 20.1 | 28.1 | 22.4 | 21.5 | 21.4 | 20.1 | 1.5 |
| bonds | 11.7 | 13.6 | 11.7 | 13.1 | 13.6 | 11.4 | 1.2 |
| dc | 52.2 | 58.4 | 55.8 | 56.5 | 56.9 | 56.8 | 0.4 |
| db | 6.2 | 7.2 | 5.4 | 6.9 | 6.6 | 5.9 | 0.8 |
| life_ins | 20.0 | 18.5 | 16.2 | 18.0 | 18.3 | 18.4 | 2.3 |
| annuities | 4.4 | 4.4 | 3.1 | 4.6 | 4.2 | 3.1 | 0.5 |
| trusts | 2.1 | 1.5 | 1.1 | 2.0 | 1.6 | 1.1 | 0.5 |
| other_fin | 9.9 | 7.9 | 7.2 | 8.9 | 9.2 | 7.9 | 1.4 |
| pass_throughs | 12.9 | 26.3 | 14.8 | 12.2 | 12.0 | 11.1 | 0.5 |
| primary_home | 65.3 | 74.5 | 68.0 | 64.7 | 66.4 | 66.3 | 1.8 |
| other_home | 15.0 | 26.3 | 19.0 | 15.9 | 15.8 | 12.3 | 0.3 |
| re_fund | 6.9 | 9.0 | 8.6 | 7.0 | 6.3 | 5.3 | 1.0 |
| other_nonfin | 86.8 | 88.9 | 88.1 | 87.3 | 87.4 | 87.5 | 1.3 |
| primary_mortgage | 43.4 | 50.9 | 44.9 | 44.7 | 46.1 | 45.7 | 1.2 |
| other_mortgage | 6.0 | 8.4 | 6.6 | 5.9 | 6.5 | 4.0 | 0.8 |
| credit_lines | 1.8 | 1.6 | 1.3 | 1.9 | 1.9 | 1.6 | 0.4 |
| credit_cards | 46.3 | 48.4 | 43.0 | 44.5 | 44.9 | 45.6 | 1.4 |
| installment_debt | 52.8 | 54.2 | 51.6 | 51.9 | 52.4 | 52.6 | 1.7 |
| other_debt | 5.5 | 5.6 | 4.2 | 5.5 | 5.8 | 5.3 | 0.6 |
| kg_primary_home | 62.0 | 69.0 | 64.5 | 61.4 | 63.1 | 61.6 | 2.1 |
| kg_other_re | 16.8 | 29.3 | 22.8 | 16.6 | 16.5 | 14.7 | 0.8 |
| kg_pass_throughs | 12.1 | 17.9 | 12.7 | 10.5 | 10.6 | 8.6 | 0.6 |
| kg_other | 18.3 | 22.1 | 19.5 | 18.7 | 19.2 | 17.1 | 1.0 |

**Share holding by income group, selected components (%).** Income group: weighted rank of the total of the seven incomes within source and year.

| Component | Income group | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|---|
| primary_home | Bottom 50% | 47.0 | 58.4 | 48.1 | 45.5 | 47.9 | 55.8 | 2.5 |
| primary_home | 50-90% | 76.4 | 84.5 | 80.1 | 76.1 | 77.6 | 72.3 | 1.5 |
| primary_home | 90-99% | 91.1 | 95.9 | 95.5 | 92.0 | 92.4 | 82.0 | 4.0 |
| primary_home | Top 1% | 91.1 | 96.9 | 96.1 | 96.8 | 95.0 | 85.8 | 5.5 |
| equities | Bottom 50% | 8.9 | 14.2 | 9.8 | 9.1 | 8.9 | 12.7 | 0.8 |
| equities | 50-90% | 21.1 | 32.1 | 25.2 | 24.2 | 24.5 | 21.8 | 2.8 |
| equities | 90-99% | 55.1 | 62.7 | 56.2 | 55.5 | 53.3 | 39.8 | 4.7 |
| equities | Top 1% | 89.9 | 83.4 | 78.9 | 76.8 | 82.0 | 54.1 | 4.2 |
| pass_throughs | Bottom 50% | 5.8 | 15.3 | 6.1 | 6.4 | 6.2 | 7.1 | 1.4 |
| pass_throughs | 50-90% | 14.4 | 29.2 | 15.4 | 11.9 | 12.3 | 11.4 | 1.5 |
| pass_throughs | 90-99% | 30.8 | 54.5 | 42.5 | 29.7 | 28.9 | 22.9 | 5.1 |
| pass_throughs | Top 1% | 62.1 | 77.3 | 72.9 | 81.3 | 69.5 | 41.4 | 10.3 |
| dc | Bottom 50% | 27.9 | 39.7 | 34.9 | 34.2 | 35.1 | 41.8 | 2.2 |
| dc | 50-90% | 65.8 | 69.2 | 67.8 | 69.5 | 69.8 | 65.6 | 2.4 |
| dc | 90-99% | 91.1 | 87.0 | 87.7 | 90.9 | 89.2 | 78.6 | 1.3 |
| dc | Top 1% | 84.9 | 88.8 | 88.3 | 86.6 | 90.3 | 79.1 | 11.3 |
| primary_mortgage | Bottom 50% | 24.8 | 34.3 | 26.3 | 26.3 | 27.2 | 35.1 | 1.8 |
| primary_mortgage | 50-90% | 55.1 | 61.3 | 56.7 | 56.3 | 57.9 | 51.7 | 2.0 |
| primary_mortgage | 90-99% | 70.0 | 74.0 | 70.6 | 69.9 | 72.3 | 62.5 | 3.3 |
| primary_mortgage | Top 1% | 57.0 | 64.9 | 58.3 | 56.6 | 65.4 | 60.3 | 11.1 |
| bonds | Bottom 50% | 5.6 | 7.9 | 5.9 | 6.4 | 6.5 | 8.5 | 1.4 |
| bonds | 50-90% | 12.9 | 15.3 | 13.2 | 14.5 | 14.9 | 11.8 | 1.8 |
| bonds | 90-99% | 29.4 | 27.1 | 26.8 | 30.8 | 34.0 | 19.9 | 3.6 |
| bonds | Top 1% | 39.8 | 38.5 | 40.9 | 47.6 | 45.0 | 26.3 | 5.9 |
| trusts | Bottom 50% | 1.3 | 0.8 | 0.4 | 0.7 | 0.5 | 0.7 | 0.5 |
| trusts | 50-90% | 2.1 | 1.5 | 1.1 | 2.1 | 1.6 | 1.0 | 0.7 |
| trusts | 90-99% | 3.3 | 3.4 | 3.0 | 6.1 | 5.2 | 2.6 | 1.1 |
| trusts | Top 1% | 18.4 | 6.6 | 6.8 | 13.1 | 13.3 | 6.3 | 4.1 |
| other_home | Bottom 50% | 6.0 | 15.8 | 8.9 | 8.2 | 8.5 | 8.5 | 0.9 |
| other_home | 50-90% | 17.1 | 29.4 | 20.9 | 16.5 | 16.0 | 12.8 | 1.4 |
| other_home | 90-99% | 38.3 | 52.4 | 47.2 | 39.8 | 40.8 | 23.4 | 3.5 |
| other_home | Top 1% | 61.5 | 69.2 | 67.3 | 69.1 | 60.6 | 37.0 | 17.4 |

**Net worth** (assets minus debts, dollars of the survey year; the four unrealized-gain variables are not part of net worth).

| Net worth statistic | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (± %) |
|---|---|---|---|---|---|---|---|
| p10 | -1k | 0k | -0k | -1k | -1k | 1k |  |
| p25 | 11k | 26k | 13k | 10k | 12k | 39k | 15.8 |
| p50 | 93k | 176k | 121k | 112k | 107k | 168k | 12.2 |
| p75 | 385k | 576k | 464k | 429k | 401k | 467k | 3.8 |
| p90 | 1,188k | 1,659k | 1,557k | 1,261k | 1,241k | 1,119k | 8.1 |
| p99 | 10,681k | 14,267k | 15,402k | 13,343k | 10,524k | 5,692k | 4.9 |
| mean | 725k | 978k | 985k | 909k | 755k | 535k | 3.6 |
| top10%_share | 77.6% | 74.8% | 79.6% | 80.4% | 77.5% | 62.7% | 1.6 |
| top1%_share | 39.2% | 35.8% | 40.1% | 44.4% | 40.4% | 26.5% | 6.4 |
| top0.1%_share | 15.3% | 11.9% | 13.6% | 18.5% | 17.2% | 10.4% | 9.9 |

| Income group | Truth, mean | Truth, median | joint model (model_joint_w1_plain), mean | joint model (model_joint_w1_plain), median | joint model (model_joint_w1_asSCF_plain), mean | joint model (model_joint_w1_asSCF_plain), median | production forest (comp_drf), mean | production forest (comp_drf), median | production forest (comp_drfsw), mean | production forest (comp_drfsw), median | sequential trees (comp_seqtrees), mean | sequential trees (comp_seqtrees), median | Sampling noise, mean (± %) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Bottom 50% | 120k | 17k | 282k | 51k | 203k | 22k | 159k | 19k | 137k | 21k | 349k | 95k | 14.9 |
| 50-90% | 438k | 159k | 698k | 252k | 599k | 190k | 434k | 171k | 397k | 160k | 525k | 194k | 11.3 |
| 90-99% | 2,274k | 1,077k | 3,284k | 1,348k | 3,696k | 1,464k | 2,878k | 1,256k | 2,554k | 1,270k | 1,135k | 526k | 10.5 |
| Top 1% | 20,687k | 10,423k | 17,316k | 9,404k | 20,939k | 11,155k | 29,842k | 14,180k | 21,755k | 10,423k | 2,522k | 1,390k | 11.3 |

**The top of each component, in rank.** Share of holders (with a positive amount) in the top 1% and top 0.1% rank bands of that component (%). By construction the weighted truth is near 1% and 0.1%; a method above that puts too many holders at the very top, which inflates net worth at the top once decoded to dollars. The unweighted columns count sample records instead of population weight: they show how far the SCF sample over-represents the top, which is what a model that learned the sample, not the population, would reproduce.

| Component | Truth, top 1% | Truth, top 0.1% | Truth unweighted (sample), top 1% | Truth unweighted (sample), top 0.1% | joint model (model_joint_w1_plain), top 1% | joint model (model_joint_w1_plain), top 0.1% | joint model (model_joint_w1_asSCF_plain), top 1% | joint model (model_joint_w1_asSCF_plain), top 0.1% | production forest (comp_drf), top 1% | production forest (comp_drf), top 0.1% | production forest (comp_drfsw), top 1% | production forest (comp_drfsw), top 0.1% | sequential trees (comp_seqtrees), top 1% | sequential trees (comp_seqtrees), top 0.1% |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| cash | 0.83 | 0.13 | 6.57 | 2.83 | 1.15 | 0.13 | 1.12 | 0.11 | 1.15 | 0.15 | 1.02 | 0.12 | 0.76 | 0.08 |
| equities | 0.81 | 0.11 | 12.89 | 4.92 | 1.36 | 0.13 | 1.79 | 0.16 | 1.08 | 0.18 | 1.10 | 0.14 | 0.48 | 0.06 |
| bonds | 0.77 | 0.07 | 12.61 | 4.82 | 0.98 | 0.13 | 1.57 | 0.19 | 1.46 | 0.11 | 1.01 | 0.12 | 0.64 | 0.06 |
| dc | 0.55 | 0.07 | 4.20 | 1.37 | 1.30 | 0.13 | 1.46 | 0.12 | 0.87 | 0.11 | 0.98 | 0.11 | 0.58 | 0.08 |
| db | 4.12 | 0.07 | 8.06 | 1.08 | 0.98 | 0.23 | 1.15 | 0.32 | 0.83 | 0.02 | 1.17 | 0.09 | 0.86 | 0.14 |
| life_ins | 0.70 | 0.03 | 7.59 | 2.70 | 1.46 | 0.14 | 1.75 | 0.18 | 1.32 | 0.17 | 1.11 | 0.11 | 0.68 | 0.06 |
| annuities | 1.12 | 0.17 | 5.71 | 2.07 | 0.78 | 0.13 | 1.18 | 0.18 | 2.13 | 0.09 | 0.93 | 0.10 | 0.85 | 0.11 |
| trusts | 0.24 | 0.08 | 10.18 | 4.38 | 0.84 | 0.06 | 1.83 | 0.40 | 0.84 | 0.10 | 1.18 | 0.14 | 1.19 | 0.15 |
| other_fin | 1.04 | 0.11 | 14.42 | 4.68 | 1.39 | 0.08 | 1.87 | 0.17 | 1.21 | 0.16 | 0.94 | 0.15 | 0.94 | 0.14 |
| pass_throughs | 1.35 | 0.15 | 18.41 | 6.97 | 1.01 | 0.09 | 1.90 | 0.19 | 2.36 | 0.31 | 1.19 | 0.18 | 0.63 | 0.06 |
| primary_home | 1.06 | 0.16 | 10.51 | 3.45 | 1.23 | 0.11 | 1.30 | 0.13 | 1.46 | 0.25 | 1.03 | 0.12 | 0.66 | 0.06 |
| other_home | 0.62 | 0.01 | 11.28 | 1.34 | 1.05 | 0.11 | 1.46 | 0.16 | 1.09 | 0.13 | 1.06 | 0.12 | 0.40 | 0.04 |
| re_fund | 1.17 | 0.12 | 7.57 | 1.65 | 1.08 | 0.11 | 1.38 | 0.11 | 1.24 | 0.25 | 1.15 | 0.19 | 0.81 | 0.05 |
| other_nonfin | 1.00 | 0.11 | 6.80 | 2.41 | 1.18 | 0.13 | 1.22 | 0.13 | 1.30 | 0.17 | 1.00 | 0.13 | 0.72 | 0.07 |
| primary_mortgage | 1.26 | 0.14 | 5.66 | 1.35 | 1.28 | 0.14 | 1.52 | 0.16 | 1.19 | 0.21 | 1.06 | 0.11 | 0.44 | 0.03 |
| other_mortgage | 0.24 | 0.03 | 5.80 | 2.97 | 0.81 | 0.07 | 1.18 | 0.11 | 1.01 | 0.19 | 1.10 | 0.10 | 0.53 | 0.05 |
| credit_lines | 1.07 | 0.21 | 15.24 | 6.09 | 1.28 | 0.07 | 1.74 | 0.19 | 3.05 | 0.74 | 1.05 | 0.17 | 1.20 | 0.11 |
| credit_cards | 1.09 | 0.11 | 1.56 | 0.23 | 1.05 | 0.11 | 0.94 | 0.10 | 0.97 | 0.13 | 1.02 | 0.11 | 0.72 | 0.08 |
| installment_debt | 0.98 | 0.04 | 2.03 | 0.62 | 1.25 | 0.15 | 1.04 | 0.12 | 0.68 | 0.08 | 0.89 | 0.11 | 0.75 | 0.06 |
| other_debt | 0.65 | 0.13 | 11.51 | 5.70 | 0.91 | 0.13 | 1.35 | 0.14 | 1.55 | 0.33 | 1.04 | 0.10 | 0.91 | 0.07 |
| kg_primary_home | 1.49 | 0.18 | 7.96 | 2.20 | 1.33 | 0.13 | 1.45 | 0.15 | 1.37 | 0.17 | 0.99 | 0.11 | 0.53 | 0.05 |
| kg_other_re | 0.76 | 0.05 | 6.42 | 0.89 | 1.18 | 0.10 | 1.63 | 0.14 | 1.29 | 0.15 | 1.13 | 0.12 | 0.60 | 0.04 |
| kg_pass_throughs | 1.32 | 0.16 | 18.01 | 6.56 | 1.06 | 0.09 | 1.81 | 0.20 | 2.37 | 0.31 | 1.20 | 0.17 | 0.40 | 0.02 |
| kg_other | 1.48 | 0.12 | 15.43 | 6.29 | 1.36 | 0.14 | 1.62 | 0.18 | 1.24 | 0.19 | 1.14 | 0.14 | 0.45 | 0.05 |

**Conditional holding rates (%).** Rows where the second column is an income (business, int_div, capital_gains, rent, ss_pens) mean "among units with that income nonzero".

| Holds | Among those holding | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|---|
| primary_home | equities | 87.4 | 91.1 | 89.2 | 82.4 | 82.7 | 73.8 | 1.9 |
| primary_mortgage | primary_home | 66.5 | 67.7 | 65.8 | 69.1 | 69.4 | 67.9 | 1.9 |
| kg_primary_home | primary_home | 94.9 | 92.2 | 94.6 | 94.9 | 95.0 | 91.3 | 1.1 |
| other_home | primary_home | 19.4 | 32.9 | 25.8 | 21.4 | 20.8 | 14.0 | 0.4 |
| equities | dc | 30.2 | 37.3 | 31.7 | 32.1 | 31.6 | 25.1 | 3.1 |
| bonds | equities | 31.3 | 25.4 | 26.4 | 28.7 | 30.3 | 19.2 | 4.0 |
| credit_cards | primary_home | 47.7 | 51.2 | 46.0 | 46.4 | 46.9 | 46.8 | 2.4 |
| pass_throughs | business | 52.6 | 50.5 | 38.2 | 41.6 | 30.4 | 39.9 | 5.9 |
| equities | int_div | 58.9 | 65.3 | 55.3 | 49.4 | 45.7 | 43.4 | 3.8 |
| kg_other | capital_gains | 70.1 | 66.2 | 63.3 | 54.1 | 48.5 | 36.3 | 3.7 |
| other_home | rent | 47.5 | 54.8 | 51.1 | 29.1 | 27.2 | 29.6 | 4.6 |
| db | ss_pens | 8.1 | 9.0 | 7.1 | 8.0 | 8.3 | 6.6 | 1.2 |

**Rank correlation of each amount with total income, among holders** (blank where fewer than 200 true holder records).

| Component | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|
| cash | 0.60 | 0.48 | 0.54 | 0.55 | 0.56 | 0.31 | 0.03 |
| equities | 0.43 | 0.38 | 0.44 | 0.37 | 0.40 | 0.13 | 0.05 |
| bonds | 0.38 | 0.30 | 0.38 | 0.39 | 0.34 | 0.11 | 0.11 |
| dc | 0.57 | 0.44 | 0.50 | 0.50 | 0.51 | 0.18 | 0.03 |
| db | 0.29 | 0.20 | 0.22 | 0.31 | 0.32 | 0.07 | 0.14 |
| life_ins | 0.42 | 0.32 | 0.38 | 0.38 | 0.37 | 0.11 | 0.07 |
| annuities | 0.27 | 0.17 | 0.22 | 0.30 | 0.24 | 0.01 | 0.08 |
| trusts | 0.40 | 0.29 | 0.37 | 0.45 | 0.51 | 0.09 | 0.18 |
| other_fin | 0.48 | 0.43 | 0.51 | 0.50 | 0.51 | 0.15 | 0.11 |
| pass_throughs | 0.46 | 0.46 | 0.55 | 0.51 | 0.47 | 0.14 | 0.09 |
| primary_home | 0.59 | 0.49 | 0.55 | 0.55 | 0.56 | 0.23 | 0.03 |
| other_home | 0.44 | 0.38 | 0.45 | 0.45 | 0.43 | 0.12 | 0.10 |
| re_fund | 0.46 | 0.38 | 0.46 | 0.49 | 0.44 | 0.09 | 0.06 |
| other_nonfin | 0.57 | 0.47 | 0.53 | 0.56 | 0.55 | 0.29 | 0.02 |
| primary_mortgage | 0.49 | 0.42 | 0.50 | 0.51 | 0.51 | 0.18 | 0.06 |
| other_mortgage | 0.48 | 0.32 | 0.36 | 0.46 | 0.49 | 0.10 | 0.15 |
| credit_lines | 0.50 | 0.38 | 0.50 | 0.60 | 0.54 | 0.01 |  |
| credit_cards | 0.39 | 0.23 | 0.27 | 0.28 | 0.28 | 0.11 | 0.05 |
| installment_debt | 0.29 | 0.26 | 0.32 | 0.30 | 0.30 | 0.13 | 0.06 |
| other_debt | 0.46 | 0.34 | 0.41 | 0.39 | 0.45 | 0.09 | 0.10 |
| kg_primary_home | 0.22 | 0.22 | 0.27 | 0.15 | 0.17 | 0.03 | 0.04 |
| kg_other_re | 0.25 | 0.28 | 0.35 | 0.33 | 0.31 | 0.05 | 0.03 |
| kg_pass_throughs | 0.40 | 0.41 | 0.50 | 0.40 | 0.39 | 0.10 | 0.09 |
| kg_other | 0.38 | 0.33 | 0.38 | 0.32 | 0.34 | 0.06 | 0.08 |

**What the classifier relies on** (drop in AUC when the variable is scrambled):

- joint model (model_joint_w1_plain): primary_home (0.086), pass_throughs (0.084), kg_primary_home (0.077), kg_other_re (0.073), kg_pass_throughs (0.072), primary_mortgage (0.070)
- joint model (model_joint_w1_asSCF_plain): primary_home (0.068), kg_primary_home (0.064), primary_mortgage (0.056), cash (0.050), kg_other_re (0.049), wages (0.037)
- production forest (comp_drf): cash (0.082), other_nonfin (0.070), dc (0.062), primary_home (0.061), primary_mortgage (0.056), kg_primary_home (0.056)
- production forest (comp_drfsw): cash (0.068), other_nonfin (0.062), dc (0.057), primary_home (0.052), primary_mortgage (0.049), credit_cards (0.049)
- sequential trees (comp_seqtrees): primary_home (0.083), cash (0.077), dc (0.070), other_nonfin (0.064), primary_mortgage (0.055), equities (0.055)

<!-- END GENERATED expA_recat -->

<!-- BEGIN GENERATED expA_unrep -->
### Dataset `expA_unrep`
Evaluation set: 15,380 records in 2,818 households. Generated 2026-10-09 14:48. Runs: joint model (model_joint_w1_plain): 3 seed(s), joint model (model_joint_w1_asSCF_plain): 3 seed(s), production forest (comp_drf): 3 seed(s), production forest (comp_drfsw): 3 seed(s), sequential trees (comp_seqtrees): 3 seed(s).

**Headline.** "Sampling noise" is the typical gap between two random halves of the true records (households kept whole), averaged over five splits; for the level rows it is the typical gap, in points, and for the ratio rows the typical distance from 1. "No information" gives every record the wealth of a random other record. Method columns: mean over training seeds [lowest-highest seed].

| Measure | Truth | Sampling noise | No information (shuffled) | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) |
|---|---|---|---|---|---|---|---|---|
| Share holding each component, all records: average gap (percentage points) |  | 1.02 | 0.12 | 2.95 [2.42-3.90] | 1.94 [1.60-2.14] | 0.80 [0.75-0.84] | 0.92 [0.89-0.98] | 1.47 [1.45-1.49] |
| Share holding, by income group (24 components x 4 groups): average gap (points) |  | 3.52 | 12.95 | 4.55 [4.09-5.05] | 3.79 [3.60-4.10] | 2.76 [2.66-2.82] | 2.39 [2.36-2.44] | 7.24 [7.14-7.30] |
| Share holding, by age range: average gap (points) |  | 2.74 | 6.39 | 3.60 [3.18-4.27] | 2.54 [2.38-2.77] | 1.80 [1.75-1.83] | 2.27 [2.26-2.29] | 3.29 [3.27-3.31] |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks |  | 6.58 | 17.79 | 6.34 [5.99-6.60] | 5.67 [5.20-6.32] | 5.82 [5.77-5.90] | 5.08 [4.99-5.20] | 13.54 [13.50-13.59] |
| Same, in dollars: average gap (%) |  | 70.0 | 238.6 | 57.4 [52.9-60.2] | 49.8 [46.6-56.2] | 54.4 [53.7-55.2] | 47.1 [46.2-48.4] | 163.8 [163.5-164.0] |
| Amounts among holders by age range: average gap in percentile ranks |  | 6.99 | 9.84 | 6.04 [5.85-6.22] | 6.22 [5.78-6.78] | 6.07 [6.03-6.11] | 5.87 [5.81-5.92] | 7.95 [7.88-7.98] |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) |  | 51.1 | 4.7 | 51.0 [47.7-54.5] | 44.3 [43.2-45.1] | 46.4 [46.2-46.5] | 44.3 [44.1-44.4] | 48.8 [48.5-49.1] |
| Pairs of components (276 two-by-two holding tables): average share misplaced (%) |  | 1.84 | 0.21 | 4.89 [4.19-6.30] | 3.20 [2.66-3.49] | 1.50 [1.43-1.57] | 1.65 [1.61-1.71] | 2.92 [2.89-2.93] |
| Selected conditional holding rates (12, e.g. home among stockholders): average gap (points) |  | 3.05 | 13.80 | 4.78 [3.98-5.59] | 4.15 [3.49-4.81] | 6.39 [6.05-6.70] | 7.81 [7.73-7.92] | 11.14 [10.99-11.26] |
| Rank correlation of each amount with income, among holders: average gap |  | 0.077 | 0.422 | 0.102 [0.091-0.118] | 0.053 [0.047-0.056] | 0.043 [0.039-0.046] | 0.039 [0.038-0.040] | 0.307 [0.304-0.310] |
| Rank correlation between amounts, among joint holders: average gap |  | 0.115 | 0.016 | 0.108 [0.103-0.112] | 0.094 [0.092-0.097] | 0.107 [0.106-0.109] | 0.098 [0.098-0.099] | 0.252 [0.250-0.254] |
| Holders in the top 1% rank band of their component: completed / true, averaged over components |  | ±0.35 | 1.00 | 1.37 [1.00-1.84] | 1.85 [1.42-2.14] | 1.39 [1.36-1.43] | 1.25 [1.21-1.29] | 0.91 [0.89-0.94] |
| Same for the top 0.1% rank band |  | ±1.36 | 1.16 | 2.06 [1.41-2.82] | 2.95 [2.17-3.43] | 2.13 [2.02-2.27] | 1.68 [1.61-1.76] | 0.99 [0.88-1.11] |
| Median net worth, completed / true |  | ±0.122 | 0.998 | 1.777 [1.677-1.902] | 1.276 [1.166-1.371] | 1.147 [1.138-1.155] | 1.137 [1.129-1.145] | 1.727 [1.724-1.728] |
| 90th percentile of net worth, completed / true |  | ±0.081 | 1.000 | 1.181 [1.080-1.279] | 1.176 [1.002-1.371] | 0.978 [0.972-0.980] | 0.973 [0.966-0.980] | 0.874 [0.866-0.886] |
| 99th percentile of net worth, completed / true |  | ±0.049 | 0.976 | 1.118 [0.881-1.441] | 1.303 [1.141-1.516] | 1.194 [1.165-1.219] | 0.953 [0.949-0.960] | 0.495 [0.486-0.500] |
| Top 10% share of net worth (%) | 77.6 | ±1.3 | 77.8 | 73.2 [70.0-76.7] | 79.3 [76.5-81.3] | 79.9 [79.3-80.2] | 76.7 [76.5-77.0] | 62.5 [61.9-62.9] |
| Top 1% share of net worth (%) | 39.2 | ±2.6 | 40.3 | 36.6 [32.8-40.9] | 42.0 [38.2-47.2] | 44.4 [43.5-45.1] | 40.5 [39.8-41.1] | 27.0 [25.7-27.7] |
| Top 0.1% share of net worth (%) | 15.3 | ±1.7 | 16.4 | 12.7 [11.2-13.8] | 14.7 [13.2-16.7] | 18.2 [17.4-18.9] | 17.2 [16.6-18.0] | 10.9 [9.7-12.0] |
| Mean net worth by income group: average gap (%) |  | 12.5 | 169.8 | 54.3 [42.5-75.6] | 35.9 [25.4-47.1] | 21.6 [18.8-23.9] | 7.3 [6.8-7.7] | 82.6 [80.6-84.5] |
| Top 1% by net worth: share whose wealth tokens repeat another household's (%) | 0.0 | ±0.0 | 71.7 | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 79.3 [76.9-81.0] | 73.8 [72.8-74.3] | 0.0 [0.0-0.0] |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) |  | 0.537 | 0.798 | 0.764 [0.744-0.790] | 0.701 [0.696-0.704] | 0.809 [0.807-0.811] | 0.792 [0.788-0.801] | 0.771 [0.766-0.777] |

**Other variants** (same measures): runs without `_w1` are the earlier configuration (survey weight as a model variable; `plain` as drawn, `wc` weight-corrected); `_small` smaller model; `_w1`, `_wsqrt` model trained with each record's loss weighted by its population weight or its square root, and `_nowt` the unweighted control (in these three the survey weight is not a model variable and the plain and weight-corrected files are identical); `drfsw` the production forest fitted with survey weights instead of a weighted bootstrap (a diagnostic, not production). The detail tables below show only the main runs.

| Measure | Truth | Sampling noise | No information (shuffled) | joint model (model_joint_asSCF_plain) | joint model (model_joint_asSCF_wc) | joint model (model_joint_plain) | joint model (model_joint_w1_asSCF_wc) | joint model (model_joint_w1_wc) | joint model (model_joint_wc) |
|---|---|---|---|---|---|---|---|---|---|
| Share holding each component, all records: average gap (percentage points) |  | 1.02 | 0.12 | 2.05 [1.83-2.27] | 1.94 [1.57-2.17] | 3.70 [2.74-5.29] | 1.94 [1.60-2.14] | 2.95 [2.42-3.90] | 3.60 [2.73-5.14] |
| Share holding, by income group (24 components x 4 groups): average gap (points) |  | 3.52 | 12.95 | 3.81 [3.69-3.96] | 3.56 [3.46-3.69] | 5.41 [4.39-7.08] | 3.79 [3.60-4.10] | 4.55 [4.09-5.05] | 5.24 [4.31-6.89] |
| Share holding, by age range: average gap (points) |  | 2.74 | 6.39 | 2.56 [2.40-2.65] | 2.52 [2.19-2.76] | 4.10 [3.31-5.60] | 2.54 [2.38-2.77] | 3.60 [3.18-4.27] | 4.04 [3.29-5.52] |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks |  | 6.58 | 17.79 | 6.29 [5.76-6.86] | 5.67 [5.39-6.13] | 6.79 [5.92-7.62] | 5.67 [5.20-6.32] | 6.34 [5.99-6.60] | 6.46 [6.04-7.05] |
| Same, in dollars: average gap (%) |  | 70.0 | 238.6 | 62.3 [57.3-65.6] | 51.7 [50.1-54.6] | 71.1 [57.3-89.3] | 50.0 [46.7-56.3] | 57.4 [53.0-60.2] | 64.6 [55.5-82.5] |
| Amounts among holders by age range: average gap in percentile ranks |  | 6.99 | 9.84 | 7.11 [6.46-7.94] | 6.41 [5.94-7.18] | 6.99 [6.02-7.75] | 6.22 [5.78-6.78] | 6.04 [5.85-6.22] | 6.67 [5.93-7.35] |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) |  | 51.1 | 4.7 | 44.9 [44.7-45.0] | 44.6 [44.2-44.9] | 54.2 [51.0-59.0] | 44.3 [43.2-45.1] | 51.0 [47.7-54.5] | 54.0 [50.8-58.5] |
| Pairs of components (276 two-by-two holding tables): average share misplaced (%) |  | 1.84 | 0.21 | 3.33 [3.09-3.56] | 3.22 [2.75-3.47] | 6.13 [4.61-8.77] | 3.20 [2.66-3.49] | 4.89 [4.19-6.30] | 5.98 [4.56-8.53] |
| Selected conditional holding rates (12, e.g. home among stockholders): average gap (points) |  | 3.05 | 13.80 | 4.05 [2.88-5.18] | 4.35 [2.95-5.56] | 4.61 [3.86-5.54] | 4.15 [3.49-4.81] | 4.78 [3.98-5.59] | 4.68 [3.73-5.74] |
| Rank correlation of each amount with income, among holders: average gap |  | 0.077 | 0.422 | 0.049 [0.047-0.050] | 0.046 [0.043-0.049] | 0.077 [0.074-0.080] | 0.053 [0.047-0.056] | 0.102 [0.091-0.118] | 0.082 [0.077-0.090] |
| Rank correlation between amounts, among joint holders: average gap |  | 0.115 | 0.016 | 0.100 [0.094-0.108] | 0.096 [0.090-0.104] | 0.095 [0.087-0.098] | 0.094 [0.092-0.097] | 0.108 [0.103-0.112] | 0.095 [0.088-0.102] |
| Holders in the top 1% rank band of their component: completed / true, averaged over components |  | ±0.35 | 1.00 | 3.79 [3.60-4.08] | 3.16 [3.11-3.25] | 4.15 [3.06-5.41] | 1.85 [1.42-2.14] | 1.37 [1.00-1.84] | 3.93 [3.00-5.49] |
| Same for the top 0.1% rank band |  | ±1.36 | 1.16 | 8.24 [7.52-8.63] | 6.81 [5.97-7.24] | 12.53 [9.12-17.06] | 2.95 [2.17-3.43] | 2.06 [1.41-2.82] | 13.13 [9.43-20.50] |
| Median net worth, completed / true |  | ±0.122 | 0.998 | 1.405 [1.357-1.451] | 1.361 [1.322-1.386] | 2.032 [1.842-2.370] | 1.276 [1.166-1.370] | 1.776 [1.676-1.901] | 1.961 [1.795-2.261] |
| 90th percentile of net worth, completed / true |  | ±0.081 | 1.000 | 1.439 [1.296-1.557] | 1.248 [1.161-1.344] | 1.942 [1.576-2.558] | 1.176 [1.006-1.367] | 1.180 [1.080-1.280] | 1.727 [1.477-2.218] |
| 99th percentile of net worth, completed / true |  | ±0.049 | 0.976 | 2.698 [2.619-2.819] | 1.988 [1.898-2.070] | 6.214 [4.435-9.641] | 1.304 [1.146-1.531] | 1.115 [0.879-1.427] | 7.051 [2.960-13.898] |
| Top 10% share of net worth (%) | 77.6 | ±1.3 | 77.8 | 89.1 [88.5-89.5] | 88.0 [87.4-88.3] | 93.1 [91.4-94.7] | 79.3 [76.6-81.3] | 73.3 [70.2-76.5] | 93.6 [92.3-95.8] |
| Top 1% share of net worth (%) | 39.2 | ±2.6 | 40.3 | 63.0 [62.1-63.6] | 62.6 [60.7-65.7] | 72.3 [69.5-75.4] | 41.9 [38.4-46.3] | 36.8 [33.2-40.6] | 75.0 [73.9-76.5] |
| Top 0.1% share of net worth (%) | 15.3 | ±1.7 | 16.4 | 28.1 [24.5-30.1] | 31.6 [30.9-32.1] | 24.6 [20.6-27.0] | 14.5 [13.5-15.6] | 13.0 [11.7-13.9] | 25.7 [17.9-31.2] |
| Mean net worth by income group: average gap (%) |  | 12.5 | 169.8 | 213.8 [155.6-244.7] | 177.6 [165.8-196.7] | 818.2 [420.1-1360.1] | 35.1 [22.9-47.4] | 55.3 [43.5-78.0] | 959.5 [524.2-1810.7] |
| Top 1% by net worth: share whose wealth tokens repeat another household's (%) | 0.0 | ±0.0 | 71.7 | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) |  | 0.537 | 0.798 | 0.700 [0.688-0.711] | 0.699 [0.691-0.710] | 0.780 [0.757-0.811] | 0.701 [0.696-0.704] | 0.764 [0.744-0.790] | 0.774 [0.757-0.804] |

**Completion noise** (one completion draw against another, same file): the part of each gap that comes from drawing at random, not from the model.

| Measure | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) |
|---|---|---|---|---|---|
| Share holding, by income group (24 components x 4 groups): average gap (points) | 1.03 | 0.97 | 0.99 | 0.99 | 0.97 |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks | 2.86 | 2.72 | 1.93 | 2.08 | 2.81 |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) | 37.3 | 29.7 | 12.2 | 13.6 | 32.2 |
| Mean net worth by income group: average gap (%) | 7.7 | 10.0 | 6.3 | 3.2 | 7.3 |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) | 0.502 | 0.503 | 0.495 | 0.502 | 0.499 |

**Share holding each component, all records (%).**

| Component | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|
| cash | 95.7 | 97.0 | 97.2 | 95.7 | 95.9 | 95.7 | 0.4 |
| equities | 20.1 | 24.2 | 20.6 | 20.5 | 20.5 | 18.6 | 1.5 |
| bonds | 11.7 | 10.9 | 10.8 | 12.5 | 13.2 | 10.9 | 1.2 |
| dc | 52.2 | 55.4 | 55.1 | 55.6 | 56.1 | 55.1 | 0.4 |
| db | 6.2 | 6.3 | 5.5 | 6.7 | 6.5 | 5.8 | 0.8 |
| life_ins | 20.0 | 16.8 | 16.4 | 18.1 | 18.4 | 18.2 | 2.3 |
| annuities | 4.4 | 3.7 | 2.9 | 4.3 | 4.0 | 2.9 | 0.5 |
| trusts | 2.1 | 1.3 | 1.1 | 1.9 | 1.5 | 1.0 | 0.5 |
| other_fin | 9.9 | 6.9 | 6.9 | 8.8 | 8.9 | 7.7 | 1.4 |
| pass_throughs | 12.9 | 19.1 | 14.1 | 12.0 | 12.1 | 10.9 | 0.5 |
| primary_home | 65.3 | 73.5 | 68.1 | 64.6 | 66.5 | 65.9 | 1.8 |
| other_home | 15.0 | 21.1 | 17.6 | 16.1 | 15.8 | 12.0 | 0.3 |
| re_fund | 6.9 | 7.7 | 7.9 | 7.1 | 6.3 | 5.3 | 1.0 |
| other_nonfin | 86.8 | 87.6 | 87.7 | 87.0 | 87.3 | 87.2 | 1.3 |
| primary_mortgage | 43.4 | 48.2 | 46.9 | 45.3 | 46.4 | 45.4 | 1.2 |
| other_mortgage | 6.0 | 6.2 | 6.4 | 6.1 | 6.6 | 3.9 | 0.8 |
| credit_lines | 1.8 | 1.4 | 1.4 | 1.9 | 2.0 | 1.5 | 0.4 |
| credit_cards | 46.3 | 44.9 | 44.3 | 44.5 | 45.2 | 45.5 | 1.4 |
| installment_debt | 52.8 | 49.8 | 51.7 | 52.2 | 52.5 | 52.1 | 1.7 |
| other_debt | 5.5 | 4.8 | 4.4 | 5.5 | 5.9 | 5.2 | 0.6 |
| kg_primary_home | 62.0 | 68.1 | 64.5 | 61.1 | 63.1 | 61.2 | 2.1 |
| kg_other_re | 16.8 | 25.9 | 21.2 | 17.0 | 16.5 | 14.4 | 0.8 |
| kg_pass_throughs | 12.1 | 14.1 | 12.1 | 10.7 | 10.7 | 8.5 | 0.6 |
| kg_other | 18.3 | 19.8 | 18.0 | 17.7 | 18.3 | 15.9 | 1.0 |

**Share holding by income group, selected components (%).** Income group: weighted rank of the total of the seven incomes within source and year.

| Component | Income group | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|---|
| primary_home | Bottom 50% | 47.1 | 57.7 | 48.4 | 46.0 | 48.4 | 55.5 | 2.5 |
| primary_home | 50-90% | 76.3 | 83.4 | 80.2 | 75.7 | 77.4 | 71.8 | 1.6 |
| primary_home | 90-99% | 91.2 | 94.5 | 95.5 | 91.4 | 92.1 | 81.5 | 3.6 |
| primary_home | Top 1% | 91.0 | 95.1 | 95.0 | 97.7 | 95.7 | 84.2 | 5.7 |
| equities | Bottom 50% | 9.1 | 12.8 | 9.3 | 9.1 | 8.6 | 12.1 | 0.5 |
| equities | 50-90% | 21.0 | 27.4 | 22.9 | 22.7 | 23.6 | 20.2 | 3.2 |
| equities | 90-99% | 54.8 | 52.4 | 52.4 | 52.0 | 50.5 | 35.6 | 4.7 |
| equities | Top 1% | 88.0 | 71.7 | 73.8 | 76.0 | 81.2 | 47.4 | 5.8 |
| pass_throughs | Bottom 50% | 5.8 | 11.2 | 6.4 | 6.2 | 6.1 | 7.0 | 1.4 |
| pass_throughs | 50-90% | 14.4 | 20.3 | 14.3 | 11.6 | 12.6 | 11.3 | 1.4 |
| pass_throughs | 90-99% | 30.4 | 41.8 | 39.9 | 30.6 | 28.7 | 22.3 | 5.2 |
| pass_throughs | Top 1% | 64.1 | 65.7 | 69.7 | 83.1 | 71.4 | 40.2 | 8.6 |
| dc | Bottom 50% | 28.1 | 37.3 | 34.5 | 33.6 | 34.5 | 40.4 | 2.1 |
| dc | 50-90% | 65.7 | 66.0 | 67.1 | 68.3 | 68.8 | 63.8 | 2.4 |
| dc | 90-99% | 91.4 | 83.0 | 87.1 | 89.4 | 88.3 | 77.0 | 1.3 |
| dc | Top 1% | 83.9 | 81.9 | 85.0 | 87.7 | 89.1 | 74.6 | 10.9 |
| primary_mortgage | Bottom 50% | 24.8 | 32.5 | 28.2 | 27.0 | 27.3 | 34.7 | 1.8 |
| primary_mortgage | 50-90% | 55.0 | 58.0 | 58.5 | 56.8 | 58.1 | 51.3 | 2.2 |
| primary_mortgage | 90-99% | 70.3 | 70.8 | 74.1 | 71.3 | 73.6 | 62.8 | 3.6 |
| primary_mortgage | Top 1% | 58.7 | 60.7 | 59.9 | 58.0 | 67.5 | 60.0 | 10.9 |
| bonds | Bottom 50% | 5.8 | 6.5 | 5.4 | 6.2 | 6.5 | 8.4 | 1.4 |
| bonds | 50-90% | 12.8 | 12.4 | 12.0 | 14.0 | 14.6 | 11.5 | 1.9 |
| bonds | 90-99% | 29.2 | 21.1 | 25.2 | 29.1 | 31.8 | 18.2 | 3.4 |
| bonds | Top 1% | 37.6 | 28.4 | 37.1 | 40.5 | 42.6 | 23.0 | 7.5 |
| trusts | Bottom 50% | 1.3 | 0.8 | 0.4 | 0.7 | 0.5 | 0.7 | 0.5 |
| trusts | 50-90% | 2.1 | 1.3 | 1.0 | 2.0 | 1.6 | 0.9 | 0.7 |
| trusts | 90-99% | 3.5 | 2.8 | 3.2 | 5.1 | 4.5 | 2.1 | 1.2 |
| trusts | Top 1% | 16.9 | 4.5 | 6.4 | 13.5 | 12.9 | 5.4 | 3.2 |
| other_home | Bottom 50% | 6.0 | 12.6 | 8.4 | 8.2 | 8.4 | 8.4 | 0.9 |
| other_home | 50-90% | 17.3 | 23.3 | 19.0 | 16.8 | 16.1 | 12.6 | 1.3 |
| other_home | 90-99% | 38.0 | 42.4 | 44.6 | 40.4 | 41.0 | 22.4 | 3.7 |
| other_home | Top 1% | 60.5 | 59.5 | 65.5 | 66.9 | 57.5 | 33.9 | 16.8 |

**Net worth** (assets minus debts, dollars of the survey year; the four unrealized-gain variables are not part of net worth).

| Net worth statistic | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (± %) |
|---|---|---|---|---|---|---|---|
| p10 | -1k | 0k | -0k | -1k | -1k | 1k |  |
| p25 | 11k | 24k | 13k | 11k | 12k | 37k | 15.8 |
| p50 | 93k | 166k | 119k | 107k | 106k | 161k | 12.2 |
| p75 | 385k | 523k | 435k | 412k | 394k | 442k | 3.8 |
| p90 | 1,188k | 1,402k | 1,396k | 1,161k | 1,156k | 1,039k | 8.1 |
| p99 | 10,681k | 11,945k | 13,919k | 12,751k | 10,180k | 5,288k | 4.9 |
| mean | 725k | 842k | 913k | 843k | 707k | 502k | 3.6 |
| top10%_share | 77.6% | 73.2% | 79.3% | 79.9% | 76.7% | 62.5% | 1.6 |
| top1%_share | 39.2% | 36.6% | 42.0% | 44.4% | 40.5% | 27.0% | 6.4 |
| top0.1%_share | 15.3% | 12.7% | 14.7% | 18.2% | 17.2% | 10.9% | 9.9 |

| Income group | Truth, mean | Truth, median | joint model (model_joint_w1_plain), mean | joint model (model_joint_w1_plain), median | joint model (model_joint_w1_asSCF_plain), mean | joint model (model_joint_w1_asSCF_plain), median | production forest (comp_drf), mean | production forest (comp_drf), median | production forest (comp_drfsw), mean | production forest (comp_drfsw), median | sequential trees (comp_seqtrees), mean | sequential trees (comp_seqtrees), median | Sampling noise, mean (± %) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Bottom 50% | 125k | 17k | 281k | 49k | 196k | 22k | 154k | 19k | 137k | 22k | 342k | 93k | 18.9 |
| 50-90% | 451k | 158k | 631k | 235k | 575k | 182k | 407k | 165k | 383k | 157k | 503k | 186k | 12.9 |
| 90-99% | 2,275k | 1,055k | 2,684k | 1,028k | 3,363k | 1,249k | 2,680k | 1,018k | 2,303k | 1,106k | 994k | 464k | 8.9 |
| Top 1% | 19,992k | 9,814k | 13,626k | 6,137k | 19,044k | 8,975k | 27,236k | 14,314k | 20,417k | 10,302k | 2,102k | 1,029k | 9.4 |

**The top of each component, in rank.** Share of holders (with a positive amount) in the top 1% and top 0.1% rank bands of that component (%). By construction the weighted truth is near 1% and 0.1%; a method above that puts too many holders at the very top, which inflates net worth at the top once decoded to dollars. The unweighted columns count sample records instead of population weight: they show how far the SCF sample over-represents the top, which is what a model that learned the sample, not the population, would reproduce.

| Component | Truth, top 1% | Truth, top 0.1% | Truth unweighted (sample), top 1% | Truth unweighted (sample), top 0.1% | joint model (model_joint_w1_plain), top 1% | joint model (model_joint_w1_plain), top 0.1% | joint model (model_joint_w1_asSCF_plain), top 1% | joint model (model_joint_w1_asSCF_plain), top 0.1% | production forest (comp_drf), top 1% | production forest (comp_drf), top 0.1% | production forest (comp_drfsw), top 1% | production forest (comp_drfsw), top 0.1% | sequential trees (comp_seqtrees), top 1% | sequential trees (comp_seqtrees), top 0.1% |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| cash | 0.83 | 0.13 | 6.57 | 2.83 | 1.04 | 0.12 | 0.99 | 0.10 | 1.02 | 0.14 | 0.94 | 0.11 | 0.68 | 0.07 |
| equities | 0.81 | 0.11 | 12.89 | 4.92 | 1.29 | 0.14 | 1.60 | 0.17 | 0.93 | 0.15 | 0.97 | 0.12 | 0.50 | 0.06 |
| bonds | 0.77 | 0.07 | 12.61 | 4.82 | 0.95 | 0.13 | 1.50 | 0.23 | 1.23 | 0.09 | 0.88 | 0.10 | 0.66 | 0.06 |
| dc | 0.55 | 0.07 | 4.20 | 1.37 | 1.24 | 0.13 | 1.34 | 0.12 | 0.82 | 0.09 | 0.95 | 0.11 | 0.56 | 0.08 |
| db | 4.12 | 0.07 | 8.06 | 1.08 | 0.87 | 0.13 | 1.08 | 0.22 | 0.93 | 0.02 | 1.11 | 0.08 | 0.85 | 0.14 |
| life_ins | 0.70 | 0.03 | 7.59 | 2.70 | 1.21 | 0.12 | 1.66 | 0.19 | 1.02 | 0.10 | 0.93 | 0.07 | 0.66 | 0.06 |
| annuities | 1.12 | 0.17 | 5.71 | 2.07 | 0.72 | 0.11 | 0.97 | 0.16 | 1.99 | 0.10 | 0.81 | 0.09 | 0.89 | 0.12 |
| trusts | 0.24 | 0.08 | 10.18 | 4.38 | 0.97 | 0.34 | 1.76 | 0.45 | 0.65 | 0.06 | 0.96 | 0.11 | 1.03 | 0.17 |
| other_fin | 1.04 | 0.11 | 14.42 | 4.68 | 1.18 | 0.16 | 1.92 | 0.22 | 1.18 | 0.24 | 0.91 | 0.15 | 0.95 | 0.14 |
| pass_throughs | 1.35 | 0.15 | 18.41 | 6.97 | 1.15 | 0.11 | 1.83 | 0.20 | 2.32 | 0.29 | 1.17 | 0.18 | 0.64 | 0.06 |
| primary_home | 1.06 | 0.16 | 10.51 | 3.45 | 1.24 | 0.11 | 1.22 | 0.12 | 1.37 | 0.26 | 0.98 | 0.12 | 0.61 | 0.06 |
| other_home | 0.62 | 0.01 | 11.28 | 1.34 | 1.02 | 0.11 | 1.46 | 0.19 | 0.83 | 0.09 | 0.89 | 0.08 | 0.37 | 0.03 |
| re_fund | 1.17 | 0.12 | 7.57 | 1.65 | 1.19 | 0.12 | 1.44 | 0.12 | 1.12 | 0.35 | 1.15 | 0.20 | 0.79 | 0.06 |
| other_nonfin | 1.00 | 0.11 | 6.80 | 2.41 | 1.03 | 0.10 | 1.06 | 0.10 | 1.20 | 0.16 | 0.97 | 0.12 | 0.69 | 0.06 |
| primary_mortgage | 1.26 | 0.14 | 5.66 | 1.35 | 1.16 | 0.13 | 1.33 | 0.14 | 1.23 | 0.20 | 1.09 | 0.13 | 0.41 | 0.03 |
| other_mortgage | 0.24 | 0.03 | 5.80 | 2.97 | 0.94 | 0.08 | 1.35 | 0.11 | 0.67 | 0.12 | 0.92 | 0.10 | 0.56 | 0.05 |
| credit_lines | 1.07 | 0.21 | 15.24 | 6.09 | 0.76 | 0.08 | 1.43 | 0.09 | 2.12 | 0.47 | 1.01 | 0.08 | 1.23 | 0.10 |
| credit_cards | 1.09 | 0.11 | 1.56 | 0.23 | 0.90 | 0.11 | 0.84 | 0.09 | 0.89 | 0.11 | 1.00 | 0.11 | 0.71 | 0.08 |
| installment_debt | 0.98 | 0.04 | 2.03 | 0.62 | 1.09 | 0.14 | 0.90 | 0.12 | 0.77 | 0.09 | 0.96 | 0.11 | 0.75 | 0.06 |
| other_debt | 0.65 | 0.13 | 11.51 | 5.70 | 0.92 | 0.14 | 1.25 | 0.18 | 1.37 | 0.21 | 0.84 | 0.05 | 0.90 | 0.06 |
| kg_primary_home | 1.49 | 0.18 | 7.96 | 2.20 | 1.21 | 0.14 | 1.33 | 0.16 | 1.29 | 0.18 | 0.91 | 0.09 | 0.51 | 0.05 |
| kg_other_re | 0.76 | 0.05 | 6.42 | 0.89 | 1.22 | 0.11 | 1.65 | 0.16 | 1.11 | 0.19 | 1.04 | 0.12 | 0.59 | 0.04 |
| kg_pass_throughs | 1.32 | 0.16 | 18.01 | 6.56 | 1.22 | 0.10 | 1.78 | 0.20 | 2.23 | 0.24 | 1.22 | 0.17 | 0.36 | 0.03 |
| kg_other | 1.48 | 0.12 | 15.43 | 6.29 | 1.22 | 0.13 | 1.66 | 0.17 | 0.91 | 0.15 | 1.01 | 0.12 | 0.47 | 0.05 |

**Conditional holding rates (%).** Rows where the second column is an income (business, int_div, capital_gains, rent, ss_pens) mean "among units with that income nonzero".

| Holds | Among those holding | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|---|
| primary_home | equities | 87.4 | 89.8 | 89.1 | 80.5 | 82.1 | 72.9 | 1.9 |
| primary_mortgage | primary_home | 66.5 | 65.0 | 68.6 | 70.1 | 69.8 | 67.9 | 1.9 |
| kg_primary_home | primary_home | 94.9 | 92.0 | 94.5 | 94.5 | 95.0 | 91.4 | 1.1 |
| other_home | primary_home | 19.4 | 26.4 | 23.9 | 21.5 | 20.7 | 13.7 | 0.4 |
| equities | dc | 30.2 | 32.4 | 29.3 | 30.5 | 30.5 | 23.5 | 3.1 |
| bonds | equities | 31.3 | 20.0 | 25.2 | 27.3 | 29.1 | 18.2 | 4.0 |
| credit_cards | primary_home | 47.7 | 47.2 | 47.4 | 46.6 | 47.3 | 46.8 | 2.4 |
| pass_throughs | business | 52.6 | 44.1 | 42.0 | 43.8 | 31.3 | 39.4 | 5.9 |
| equities | int_div | 59.9 | 58.4 | 54.1 | 49.1 | 45.4 | 43.0 | 3.7 |
| kg_other | capital_gains | 68.9 | 57.1 | 58.2 | 49.0 | 44.7 | 31.2 | 6.6 |
| other_home | rent | 47.5 | 46.9 | 49.1 | 29.1 | 26.9 | 28.9 | 4.6 |
| db | ss_pens | 8.1 | 7.8 | 7.1 | 8.2 | 8.3 | 6.6 | 1.2 |

**Rank correlation of each amount with total income, among holders** (blank where fewer than 200 true holder records).

| Component | Truth | joint model (model_joint_w1_plain) | joint model (model_joint_w1_asSCF_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|
| cash | 0.59 | 0.45 | 0.51 | 0.55 | 0.56 | 0.30 | 0.03 |
| equities | 0.42 | 0.34 | 0.41 | 0.35 | 0.38 | 0.10 | 0.05 |
| bonds | 0.35 | 0.26 | 0.34 | 0.36 | 0.31 | 0.08 | 0.12 |
| dc | 0.56 | 0.40 | 0.48 | 0.49 | 0.50 | 0.17 | 0.03 |
| db | 0.28 | 0.17 | 0.19 | 0.31 | 0.32 | 0.07 | 0.14 |
| life_ins | 0.42 | 0.29 | 0.35 | 0.35 | 0.36 | 0.10 | 0.07 |
| annuities | 0.25 | 0.14 | 0.19 | 0.33 | 0.23 | 0.01 | 0.07 |
| trusts | 0.37 | 0.24 | 0.34 | 0.42 | 0.49 | 0.07 | 0.18 |
| other_fin | 0.47 | 0.37 | 0.47 | 0.47 | 0.50 | 0.14 | 0.11 |
| pass_throughs | 0.46 | 0.42 | 0.51 | 0.52 | 0.46 | 0.13 | 0.09 |
| primary_home | 0.58 | 0.46 | 0.53 | 0.54 | 0.55 | 0.21 | 0.03 |
| other_home | 0.43 | 0.34 | 0.43 | 0.43 | 0.42 | 0.11 | 0.10 |
| re_fund | 0.46 | 0.33 | 0.43 | 0.47 | 0.41 | 0.09 | 0.06 |
| other_nonfin | 0.56 | 0.45 | 0.52 | 0.56 | 0.55 | 0.28 | 0.02 |
| primary_mortgage | 0.50 | 0.40 | 0.48 | 0.51 | 0.51 | 0.18 | 0.06 |
| other_mortgage | 0.47 | 0.30 | 0.36 | 0.45 | 0.47 | 0.09 | 0.15 |
| credit_lines | 0.50 | 0.35 | 0.45 | 0.50 | 0.50 | 0.02 |  |
| credit_cards | 0.39 | 0.22 | 0.27 | 0.27 | 0.28 | 0.11 | 0.05 |
| installment_debt | 0.29 | 0.26 | 0.31 | 0.31 | 0.31 | 0.13 | 0.06 |
| other_debt | 0.46 | 0.32 | 0.40 | 0.44 | 0.46 | 0.09 | 0.10 |
| kg_primary_home | 0.22 | 0.19 | 0.25 | 0.16 | 0.16 | 0.02 | 0.03 |
| kg_other_re | 0.24 | 0.24 | 0.32 | 0.33 | 0.29 | 0.05 | 0.04 |
| kg_pass_throughs | 0.40 | 0.38 | 0.46 | 0.40 | 0.38 | 0.09 | 0.09 |
| kg_other | 0.37 | 0.28 | 0.34 | 0.30 | 0.31 | 0.04 | 0.09 |

**What the classifier relies on** (drop in AUC when the variable is scrambled):

- joint model (model_joint_w1_plain): primary_home (0.098), kg_primary_home (0.083), primary_mortgage (0.072), kg_other_re (0.066), pass_throughs (0.064), kg_pass_throughs (0.063)
- joint model (model_joint_w1_asSCF_plain): primary_home (0.063), primary_mortgage (0.053), kg_primary_home (0.052), cash (0.042), kg_pass_throughs (0.039), other_nonfin (0.032)
- production forest (comp_drf): other_nonfin (0.079), cash (0.073), dc (0.059), primary_home (0.059), installment_debt (0.058), kg_primary_home (0.057)
- production forest (comp_drfsw): cash (0.071), other_nonfin (0.066), primary_home (0.060), dc (0.060), kg_primary_home (0.053), credit_cards (0.049)
- sequential trees (comp_seqtrees): primary_home (0.085), cash (0.074), dc (0.065), equities (0.060), other_nonfin (0.059), primary_mortgage (0.052)

<!-- END GENERATED expA_unrep -->

<!-- BEGIN GENERATED expB -->
### Dataset `expB`
Evaluation set: 12,021 records in 2,207 households. Generated 2026-10-09 16:27. Runs: joint model (model_joint_w1_scf_plain): 3 seed(s), one-source model (model_scfonly_w1_scf_plain): 3 seed(s), production forest (comp_drf): 3 seed(s), production forest (comp_drfsw): 3 seed(s), sequential trees (comp_seqtrees): 3 seed(s).

**Headline.** "Sampling noise" is the typical gap between two random halves of the true records (households kept whole), averaged over five splits; for the level rows it is the typical gap, in points, and for the ratio rows the typical distance from 1. "No information" gives every record the wealth of a random other record. Method columns: mean over training seeds [lowest-highest seed].

| Measure | Truth | Sampling noise | No information (shuffled) | joint model (model_joint_w1_scf_plain) | one-source model (model_scfonly_w1_scf_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) |
|---|---|---|---|---|---|---|---|---|
| Share holding each component, all records: average gap (percentage points) |  | 1.33 | 0.14 | 1.07 [0.85-1.19] | 1.45 [1.26-1.59] | 1.00 [0.98-1.03] | 0.72 [0.66-0.79] | 1.10 [1.04-1.16] |
| Share holding, by income group (24 components x 4 groups): average gap (points) |  | 3.45 | 13.74 | 2.30 [2.28-2.32] | 2.62 [2.58-2.65] | 1.90 [1.85-1.95] | 1.84 [1.79-1.88] | 4.28 [4.22-4.37] |
| Share holding, by age range: average gap (points) |  | 3.19 | 6.15 | 2.24 [2.14-2.36] | 2.25 [2.20-2.32] | 2.03 [1.96-2.11] | 1.91 [1.87-1.94] | 2.20 [2.14-2.28] |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks |  | 6.66 | 17.42 | 5.00 [4.83-5.09] | 5.10 [4.89-5.47] | 4.54 [4.41-4.63] | 4.13 [4.09-4.18] | 8.97 [8.94-9.02] |
| Same, in dollars: average gap (%) |  | 67.5 | 240.5 | 46.4 [44.6-48.9] | 46.4 [44.7-48.6] | 43.9 [41.7-46.4] | 39.1 [38.0-39.8] | 95.3 [95.2-95.4] |
| Amounts among holders by age range: average gap in percentile ranks |  | 7.86 | 10.20 | 6.21 [6.06-6.35] | 6.43 [6.29-6.60] | 5.83 [5.80-5.88] | 5.81 [5.76-5.89] | 7.20 [7.08-7.36] |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) |  | 55.3 | 5.2 | 42.2 [42.0-42.4] | 42.0 [41.5-42.4] | 41.9 [41.7-42.2] | 41.1 [40.9-41.4] | 45.1 [45.0-45.2] |
| Pairs of components (276 two-by-two holding tables): average share misplaced (%) |  | 2.30 | 0.23 | 1.91 [1.61-2.14] | 2.40 [2.06-2.59] | 1.70 [1.66-1.77] | 1.28 [1.21-1.35] | 1.94 [1.88-2.03] |
| Selected conditional holding rates (12, e.g. home among stockholders): average gap (points) |  | 2.82 | 14.08 | 3.36 [2.91-3.86] | 4.24 [4.18-4.29] | 5.53 [5.49-5.58] | 6.76 [6.69-6.87] | 5.55 [5.42-5.64] |
| Rank correlation of each amount with income, among holders: average gap |  | 0.056 | 0.415 | 0.064 [0.062-0.068] | 0.066 [0.063-0.071] | 0.059 [0.057-0.060] | 0.058 [0.057-0.058] | 0.179 [0.178-0.181] |
| Rank correlation between amounts, among joint holders: average gap |  | 0.111 | 0.017 | 0.089 [0.084-0.093] | 0.087 [0.083-0.093] | 0.088 [0.087-0.089] | 0.086 [0.085-0.088] | 0.194 [0.191-0.196] |
| Holders in the top 1% rank band of their component: completed / true, averaged over components |  | ±0.90 | 1.05 | 1.01 [0.95-1.14] | 1.26 [1.23-1.30] | 1.16 [1.15-1.18] | 0.95 [0.91-0.98] | 0.55 [0.52-0.57] |
| Same for the top 0.1% rank band |  | ±0.94 | 1.03 | 4.52 [4.35-4.65] | 6.18 [4.78-7.46] | 13.37 [9.84-16.70] | 6.94 [5.58-8.22] | 4.99 [4.01-5.83] |
| Median net worth, completed / true |  | ±0.120 | 1.007 | 1.111 [1.049-1.176] | 1.131 [1.118-1.141] | 1.001 [0.977-1.017] | 1.019 [1.016-1.022] | 1.446 [1.441-1.449] |
| 90th percentile of net worth, completed / true |  | ±0.093 | 1.015 | 1.134 [1.094-1.209] | 1.147 [1.137-1.156] | 1.022 [1.014-1.027] | 0.982 [0.958-0.998] | 0.963 [0.949-0.978] |
| 99th percentile of net worth, completed / true |  | ±0.100 | 1.012 | 1.157 [1.076-1.230] | 1.248 [1.224-1.276] | 1.061 [1.031-1.093] | 0.994 [0.965-1.012] | 0.641 [0.633-0.650] |
| Top 10% share of net worth (%) | 77.8 | ±1.5 | 77.6 | 77.8 [76.9-78.3] | 80.0 [79.5-81.1] | 79.9 [79.8-80.0] | 77.4 [77.2-77.6] | 67.2 [66.9-67.6] |
| Top 1% share of net worth (%) | 37.3 | ±1.8 | 36.3 | 37.5 [36.8-38.0] | 43.0 [41.7-45.2] | 45.2 [44.9-45.5] | 39.8 [38.9-40.4] | 27.1 [26.9-27.4] |
| Top 0.1% share of net worth (%) | 13.3 | ±1.2 | 11.3 | 12.9 [12.1-13.9] | 16.3 [14.6-18.5] | 19.8 [18.8-20.7] | 15.2 [14.0-16.0] | 8.6 [8.3-8.9] |
| Mean net worth by income group: average gap (%) |  | 12.0 | 154.5 | 20.8 [16.5-28.6] | 25.0 [22.5-28.9] | 16.3 [15.8-16.7] | 7.2 [5.1-9.2] | 37.6 [36.4-38.7] |
| Top 1% by net worth: share whose wealth tokens repeat another household's (%) | 0.0 | ±0.0 | 71.0 | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 56.0 [52.8-59.9] | 57.5 [56.4-58.4] | 0.0 [0.0-0.0] |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) |  | 0.506 | 0.805 | 0.604 [0.596-0.617] | 0.610 [0.604-0.613] | 0.619 [0.613-0.627] | 0.625 [0.620-0.630] | 0.634 [0.629-0.639] |

**Other variants** (same measures): runs without `_w1` are the earlier configuration (survey weight as a model variable; `plain` as drawn, `wc` weight-corrected); `_small` smaller model; `_w1`, `_wsqrt` model trained with each record's loss weighted by its population weight or its square root, and `_nowt` the unweighted control (in these three the survey weight is not a model variable and the plain and weight-corrected files are identical); `drfsw` the production forest fitted with survey weights instead of a weighted bootstrap (a diagnostic, not production). The detail tables below show only the main runs.

| Measure | Truth | Sampling noise | No information (shuffled) | joint model (model_joint_scf_plain) | joint model (model_joint_scf_wc) | joint model (model_joint_w1_large_scf_plain) | joint model (model_joint_w1_large_scf_wc) | joint model (model_joint_w1_scf_wc) | joint model (model_joint_w1_task50_scf_plain) | joint model (model_joint_w1_task50_scf_wc) | joint model (model_joint_w1_task_scf_plain) | joint model (model_joint_w1_task_scf_wc) | one-source model (model_scfonly_scf_plain) | one-source model (model_scfonly_scf_wc) | one-source model (model_scfonly_small_scf_plain) | one-source model (model_scfonly_small_scf_wc) | one-source model (model_scfonly_w1_large_scf_plain) | one-source model (model_scfonly_w1_large_scf_wc) | one-source model (model_scfonly_w1_scf_wc) | one-source model (model_scfonly_w1_task_scf_plain) | one-source model (model_scfonly_w1_task_scf_wc) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Share holding each component, all records: average gap (percentage points) |  | 1.33 | 0.14 | 1.61 [1.55-1.72] | 1.20 [1.15-1.23] | 1.08 [1.01-1.14] | 1.08 [1.01-1.14] | 1.07 [0.85-1.19] | 1.23 [1.13-1.32] | 1.23 [1.13-1.32] | 6.60 [6.13-7.23] | 6.60 [6.13-7.23] | 1.67 [1.14-1.97] | 1.47 [1.25-1.64] | 1.40 [1.14-1.82] | 1.05 [0.97-1.17] | 1.99 [1.86-2.07] | 1.99 [1.86-2.07] | 1.45 [1.26-1.59] | 10.46 [9.21-11.46] | 10.46 [9.21-11.46] |
| Share holding, by income group (24 components x 4 groups): average gap (points) |  | 3.45 | 13.74 | 2.71 [2.64-2.84] | 2.37 [2.32-2.45] | 2.29 [2.24-2.35] | 2.29 [2.24-2.35] | 2.30 [2.28-2.32] | 2.31 [2.15-2.43] | 2.31 [2.15-2.43] | 6.66 [6.28-6.89] | 6.66 [6.28-6.89] | 2.86 [2.38-3.32] | 2.61 [2.26-2.87] | 2.83 [2.61-3.20] | 2.24 [2.10-2.50] | 3.69 [3.65-3.73] | 3.69 [3.65-3.73] | 2.62 [2.58-2.65] | 10.58 [10.21-10.85] | 10.58 [10.21-10.85] |
| Share holding, by age range: average gap (points) |  | 3.19 | 6.15 | 2.48 [2.33-2.57] | 2.19 [2.09-2.31] | 2.13 [2.06-2.20] | 2.13 [2.06-2.20] | 2.24 [2.14-2.36] | 2.06 [2.01-2.10] | 2.06 [2.01-2.10] | 6.61 [6.24-7.26] | 6.61 [6.24-7.26] | 2.41 [2.06-2.62] | 2.35 [2.17-2.54] | 2.28 [2.09-2.59] | 2.12 [2.06-2.17] | 2.63 [2.57-2.68] | 2.63 [2.57-2.68] | 2.25 [2.20-2.32] | 10.55 [9.64-11.41] | 10.55 [9.64-11.41] |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks |  | 6.66 | 17.42 | 5.82 [5.60-5.98] | 4.61 [4.46-4.77] | 5.21 [5.19-5.22] | 5.21 [5.19-5.22] | 5.00 [4.83-5.09] | 5.18 [4.83-5.42] | 5.18 [4.83-5.42] | 5.97 [5.58-6.40] | 5.97 [5.58-6.40] | 6.17 [5.75-6.48] | 5.05 [4.93-5.12] | 6.07 [5.59-6.41] | 5.08 [4.93-5.24] | 5.15 [5.00-5.28] | 5.15 [5.00-5.28] | 5.10 [4.89-5.47] | 6.76 [6.29-7.44] | 6.76 [6.29-7.44] |
| Same, in dollars: average gap (%) |  | 67.5 | 240.5 | 57.3 [54.6-59.0] | 42.2 [41.1-43.2] | 47.7 [47.0-48.9] | 47.7 [47.1-48.8] | 46.4 [44.3-48.9] | 47.3 [44.6-48.9] | 47.3 [44.6-48.7] | 55.5 [51.9-59.0] | 55.6 [51.8-59.3] | 62.7 [57.2-66.6] | 47.4 [45.7-48.7] | 62.4 [57.5-65.8] | 48.1 [46.4-50.2] | 47.0 [44.6-48.8] | 47.0 [44.8-48.4] | 46.3 [44.4-48.6] | 72.1 [66.6-81.5] | 72.1 [66.6-81.5] |
| Amounts among holders by age range: average gap in percentile ranks |  | 7.86 | 10.20 | 6.69 [6.62-6.81] | 5.77 [5.65-5.92] | 6.19 [6.04-6.32] | 6.19 [6.04-6.32] | 6.21 [6.06-6.35] | 5.95 [5.51-6.18] | 5.95 [5.51-6.18] | 6.51 [6.30-6.84] | 6.51 [6.30-6.84] | 6.97 [6.42-7.40] | 6.10 [5.85-6.30] | 6.94 [6.49-7.40] | 6.06 [5.91-6.16] | 6.34 [6.13-6.48] | 6.34 [6.13-6.48] | 6.43 [6.29-6.60] | 7.52 [7.38-7.78] | 7.52 [7.38-7.78] |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) |  | 55.3 | 5.2 | 43.4 [42.8-44.2] | 42.1 [41.4-42.7] | 42.8 [42.6-43.2] | 42.8 [42.6-43.2] | 42.2 [42.0-42.4] | 46.6 [46.1-47.1] | 46.6 [46.1-47.1] | 84.8 [83.2-87.2] | 84.8 [83.2-87.2] | 42.9 [41.7-44.3] | 42.2 [41.4-43.0] | 43.4 [43.0-43.9] | 42.4 [41.6-43.3] | 41.6 [41.5-41.6] | 41.6 [41.5-41.6] | 42.0 [41.5-42.4] | 90.8 [88.4-92.9] | 90.8 [88.4-92.9] |
| Pairs of components (276 two-by-two holding tables): average share misplaced (%) |  | 2.30 | 0.23 | 2.71 [2.60-2.90] | 2.06 [1.98-2.10] | 1.91 [1.82-1.98] | 1.91 [1.82-1.98] | 1.91 [1.61-2.14] | 2.11 [2.03-2.24] | 2.11 [2.03-2.24] | 10.87 [10.13-11.96] | 10.87 [10.13-11.96] | 2.75 [1.82-3.22] | 2.50 [2.14-2.84] | 2.36 [2.04-2.94] | 1.85 [1.75-2.02] | 3.36 [3.13-3.58] | 3.36 [3.13-3.58] | 2.40 [2.06-2.59] | 17.67 [15.92-19.15] | 17.67 [15.92-19.15] |
| Selected conditional holding rates (12, e.g. home among stockholders): average gap (points) |  | 2.82 | 14.08 | 3.13 [2.83-3.34] | 3.17 [3.06-3.34] | 2.60 [2.34-2.86] | 2.60 [2.34-2.86] | 3.36 [2.91-3.86] | 2.66 [2.28-2.86] | 2.66 [2.28-2.86] | 7.00 [6.40-7.64] | 7.00 [6.40-7.64] | 3.98 [3.59-4.30] | 4.11 [3.79-4.61] | 3.06 [2.73-3.48] | 3.08 [2.74-3.52] | 5.15 [4.98-5.47] | 5.15 [4.98-5.47] | 4.24 [4.18-4.29] | 9.12 [8.12-10.01] | 9.12 [8.12-10.01] |
| Rank correlation of each amount with income, among holders: average gap |  | 0.056 | 0.415 | 0.066 [0.063-0.072] | 0.065 [0.062-0.069] | 0.070 [0.067-0.073] | 0.070 [0.067-0.073] | 0.064 [0.062-0.068] | 0.061 [0.059-0.063] | 0.061 [0.059-0.063] | 0.072 [0.067-0.078] | 0.072 [0.067-0.078] | 0.061 [0.057-0.065] | 0.063 [0.061-0.067] | 0.060 [0.059-0.061] | 0.064 [0.060-0.066] | 0.076 [0.072-0.083] | 0.076 [0.072-0.083] | 0.066 [0.063-0.071] | 0.078 [0.072-0.082] | 0.078 [0.073-0.082] |
| Rank correlation between amounts, among joint holders: average gap |  | 0.111 | 0.017 | 0.085 [0.082-0.088] | 0.082 [0.081-0.084] | 0.092 [0.090-0.094] | 0.092 [0.090-0.094] | 0.089 [0.084-0.093] | 0.090 [0.088-0.093] | 0.090 [0.088-0.093] | 0.116 [0.113-0.117] | 0.116 [0.113-0.117] | 0.088 [0.087-0.088] | 0.085 [0.083-0.089] | 0.091 [0.089-0.094] | 0.087 [0.084-0.088] | 0.089 [0.088-0.091] | 0.089 [0.088-0.091] | 0.087 [0.083-0.093] | 0.125 [0.122-0.129] | 0.125 [0.122-0.129] |
| Holders in the top 1% rank band of their component: completed / true, averaged over components |  | ±0.90 | 1.05 | 2.24 [2.12-2.36] | 1.57 [1.53-1.62] | 1.01 [0.96-1.04] | 1.01 [0.96-1.04] | 1.01 [0.95-1.14] | 0.97 [0.83-1.08] | 0.97 [0.83-1.08] | 0.88 [0.85-0.92] | 0.88 [0.85-0.92] | 2.27 [2.07-2.44] | 1.68 [1.62-1.77] | 2.29 [2.17-2.38] | 1.60 [1.58-1.62] | 0.97 [0.82-1.05] | 0.97 [0.82-1.05] | 1.26 [1.23-1.30] | 0.49 [0.39-0.66] | 0.49 [0.39-0.66] |
| Same for the top 0.1% rank band |  | ±0.94 | 1.03 | 9.55 [8.14-10.53] | 7.16 [6.57-8.17] | 6.06 [5.12-7.23] | 6.06 [5.12-7.23] | 4.52 [4.35-4.65] | 6.62 [6.41-7.01] | 6.62 [6.41-7.01] | 11.29 [9.02-12.66] | 11.29 [9.02-12.66] | 9.79 [7.40-11.99] | 8.45 [6.50-11.63] | 10.44 [9.22-11.54] | 7.80 [6.97-8.83] | 6.11 [5.60-6.75] | 6.11 [5.60-6.75] | 6.18 [4.78-7.46] | 4.93 [3.92-6.45] | 4.93 [3.92-6.45] |
| Median net worth, completed / true |  | ±0.120 | 1.007 | 1.183 [1.147-1.219] | 1.132 [1.090-1.161] | 1.127 [1.099-1.143] | 1.125 [1.096-1.145] | 1.111 [1.048-1.173] | 1.340 [1.285-1.414] | 1.338 [1.284-1.412] | 2.164 [1.868-2.361] | 2.164 [1.866-2.362] | 1.237 [1.179-1.275] | 1.174 [1.131-1.196] | 1.199 [1.105-1.300] | 1.139 [1.061-1.182] | 1.244 [1.220-1.275] | 1.245 [1.221-1.276] | 1.132 [1.124-1.141] | 1.924 [1.553-2.445] | 1.920 [1.550-2.437] |
| 90th percentile of net worth, completed / true |  | ±0.093 | 1.015 | 1.484 [1.434-1.566] | 1.141 [1.092-1.190] | 1.126 [1.094-1.158] | 1.126 [1.091-1.153] | 1.134 [1.092-1.218] | 1.144 [1.095-1.181] | 1.145 [1.098-1.181] | 1.763 [1.675-1.878] | 1.760 [1.673-1.868] | 1.484 [1.341-1.640] | 1.187 [1.101-1.286] | 1.495 [1.329-1.581] | 1.165 [1.040-1.252] | 1.035 [1.002-1.075] | 1.035 [1.003-1.072] | 1.147 [1.136-1.153] | 1.673 [1.226-2.142] | 1.668 [1.228-2.134] |
| 99th percentile of net worth, completed / true |  | ±0.100 | 1.012 | 2.551 [2.301-2.805] | 1.450 [1.364-1.507] | 1.104 [1.094-1.116] | 1.109 [1.097-1.118] | 1.151 [1.066-1.226] | 1.149 [1.067-1.211] | 1.147 [1.061-1.209] | 1.282 [1.170-1.451] | 1.280 [1.169-1.453] | 2.416 [2.166-2.812] | 1.558 [1.447-1.750] | 2.568 [2.344-2.716] | 1.516 [1.500-1.544] | 0.989 [0.962-1.005] | 0.991 [0.962-1.012] | 1.243 [1.211-1.285] | 0.906 [0.692-1.179] | 0.911 [0.693-1.179] |
| Top 10% share of net worth (%) | 77.8 | ±1.5 | 77.6 | 87.3 [86.6-87.8] | 84.5 [83.4-86.2] | 78.3 [77.7-79.1] | 78.2 [77.8-78.7] | 77.7 [76.9-78.1] | 76.9 [75.9-77.8] | 77.0 [75.6-78.4] | 72.6 [70.0-75.1] | 72.6 [70.1-74.9] | 88.1 [87.4-88.6] | 85.7 [85.0-86.1] | 88.7 [87.4-89.8] | 85.4 [84.7-86.2] | 75.3 [74.6-76.2] | 75.3 [74.6-76.0] | 80.0 [79.5-80.9] | 76.8 [66.8-91.6] | 76.4 [67.1-91.0] |
| Top 1% share of net worth (%) | 37.3 | ±1.8 | 36.3 | 55.1 [54.1-55.6] | 52.3 [50.9-54.5] | 39.1 [38.0-40.6] | 38.8 [37.9-39.2] | 37.2 [36.8-37.5] | 36.5 [35.3-38.0] | 36.4 [34.9-38.7] | 31.2 [29.4-32.5] | 31.2 [29.3-32.3] | 58.8 [57.1-61.0] | 55.2 [54.6-56.0] | 59.3 [55.3-62.5] | 55.9 [54.3-57.6] | 37.0 [35.6-39.8] | 36.8 [35.5-39.3] | 42.8 [41.4-44.5] | 31.5 [27.4-34.5] | 31.1 [28.1-34.1] |
| Top 0.1% share of net worth (%) | 13.3 | ±1.2 | 11.3 | 23.4 [22.6-24.1] | 23.7 [22.9-24.7] | 15.4 [14.2-17.0] | 14.9 [14.1-15.6] | 12.5 [12.0-13.4] | 12.6 [11.2-13.6] | 12.4 [10.6-14.0] | 11.0 [9.9-11.7] | 11.0 [9.5-12.5] | 27.0 [25.2-29.1] | 26.5 [25.7-27.0] | 26.2 [23.0-29.5] | 28.0 [27.2-28.6] | 13.2 [11.5-15.6] | 13.0 [11.4-15.0] | 16.1 [14.4-17.6] | 13.5 [12.2-15.3] | 13.0 [12.5-13.6] |
| Mean net worth by income group: average gap (%) |  | 12.0 | 154.5 | 146.0 [128.6-160.8] | 64.6 [62.3-67.4] | 24.7 [22.0-29.0] | 24.7 [20.9-29.8] | 20.6 [16.4-27.9] | 18.6 [15.3-21.8] | 18.2 [16.3-21.1] | 66.2 [58.5-81.5] | 64.8 [58.7-76.5] | 170.2 [123.4-200.1] | 77.3 [63.8-100.1] | 169.3 [141.9-193.7] | 80.0 [66.3-88.2] | 13.9 [12.7-16.1] | 14.1 [12.6-15.9] | 24.9 [23.4-27.6] | 66.4 [36.9-98.0] | 66.4 [31.9-99.6] |
| Top 1% by net worth: share whose wealth tokens repeat another household's (%) | 0.0 | ±0.0 | 71.0 | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) |  | 0.506 | 0.805 | 0.604 [0.583-0.623] | 0.584 [0.577-0.589] | 0.617 [0.614-0.621] | 0.617 [0.614-0.621] | 0.604 [0.596-0.617] | 0.686 [0.680-0.692] | 0.686 [0.680-0.692] | 0.941 [0.937-0.949] | 0.941 [0.937-0.949] | 0.613 [0.605-0.623] | 0.605 [0.598-0.614] | 0.633 [0.623-0.638] | 0.619 [0.614-0.627] | 0.596 [0.587-0.602] | 0.596 [0.587-0.602] | 0.610 [0.604-0.613] | 0.974 [0.972-0.976] | 0.974 [0.972-0.976] |

**Completion noise** (one completion draw against another, same file): the part of each gap that comes from drawing at random, not from the model.

| Measure | joint model (model_joint_w1_scf_plain) | one-source model (model_scfonly_w1_scf_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) |
|---|---|---|---|---|---|
| Share holding, by income group (24 components x 4 groups): average gap (points) | 1.07 | 1.26 | 1.10 | 1.12 | 1.07 |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks | 2.77 | 2.80 | 2.44 | 2.58 | 2.83 |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) | 29.9 | 29.9 | 19.3 | 20.8 | 33.7 |
| Mean net worth by income group: average gap (%) | 4.8 | 12.7 | 5.2 | 11.0 | 5.9 |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) | 0.500 | 0.500 | 0.507 | 0.504 | 0.501 |

**Share holding each component, all records (%).**

| Component | Truth | joint model (model_joint_w1_scf_plain) | one-source model (model_scfonly_w1_scf_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|
| cash | 93.9 | 95.7 | 95.0 | 94.0 | 93.9 | 94.3 | 1.3 |
| equities | 20.0 | 21.7 | 21.8 | 19.4 | 19.7 | 19.2 | 1.6 |
| bonds | 12.9 | 13.9 | 13.0 | 13.2 | 13.2 | 12.5 | 0.7 |
| dc | 51.7 | 54.1 | 53.3 | 52.4 | 52.7 | 53.2 | 1.6 |
| db | 6.9 | 6.1 | 5.5 | 6.0 | 6.1 | 5.9 | 0.7 |
| life_ins | 19.2 | 19.5 | 16.9 | 18.5 | 18.9 | 18.7 | 2.4 |
| annuities | 4.7 | 3.8 | 3.4 | 3.9 | 4.0 | 3.3 | 0.9 |
| trusts | 2.0 | 1.5 | 1.4 | 1.6 | 1.6 | 1.3 | 0.7 |
| other_fin | 9.1 | 8.7 | 7.1 | 8.7 | 8.6 | 8.1 | 0.9 |
| pass_throughs | 11.9 | 11.2 | 11.0 | 11.6 | 11.9 | 10.5 | 1.6 |
| primary_home | 66.7 | 65.1 | 66.6 | 64.8 | 66.0 | 66.6 | 1.7 |
| other_home | 15.5 | 14.6 | 13.9 | 14.6 | 14.1 | 12.7 | 1.4 |
| re_fund | 5.6 | 6.6 | 7.7 | 7.7 | 7.4 | 7.0 | 1.3 |
| other_nonfin | 86.9 | 89.0 | 88.5 | 87.3 | 87.3 | 87.5 | 2.0 |
| primary_mortgage | 47.3 | 45.0 | 45.4 | 43.6 | 45.3 | 45.5 | 1.5 |
| other_mortgage | 6.5 | 5.7 | 4.9 | 5.8 | 5.7 | 5.0 | 0.9 |
| credit_lines | 1.6 | 1.9 | 1.6 | 2.1 | 2.1 | 1.7 | 0.8 |
| credit_cards | 43.7 | 43.4 | 40.5 | 41.4 | 42.2 | 42.9 | 1.7 |
| installment_debt | 52.4 | 52.3 | 49.4 | 50.4 | 51.0 | 51.0 | 2.3 |
| other_debt | 6.5 | 5.8 | 4.7 | 5.7 | 6.2 | 5.6 | 0.7 |
| kg_primary_home | 63.6 | 62.1 | 63.7 | 61.6 | 62.7 | 62.2 | 1.4 |
| kg_other_re | 16.4 | 15.8 | 17.0 | 16.6 | 15.9 | 15.3 | 1.3 |
| kg_pass_throughs | 11.1 | 10.5 | 10.3 | 10.7 | 11.1 | 9.4 | 0.9 |
| kg_other | 18.6 | 20.0 | 19.9 | 17.7 | 18.0 | 17.0 | 1.6 |

**Share holding by income group, selected components (%).** Income group: weighted rank of the total of the seven incomes within source and year.

| Component | Income group | Truth | joint model (model_joint_w1_scf_plain) | one-source model (model_scfonly_w1_scf_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|---|
| primary_home | Bottom 50% | 48.4 | 47.9 | 48.2 | 47.1 | 48.8 | 52.3 | 3.0 |
| primary_home | 50-90% | 78.4 | 75.2 | 78.0 | 75.5 | 76.6 | 75.2 | 2.1 |
| primary_home | 90-99% | 93.0 | 93.0 | 94.4 | 92.7 | 92.5 | 89.0 | 3.1 |
| primary_home | Top 1% | 99.7 | 97.6 | 97.6 | 97.3 | 94.8 | 94.8 | 0.5 |
| equities | Bottom 50% | 8.5 | 10.4 | 9.9 | 8.8 | 9.1 | 9.7 | 0.7 |
| equities | 50-90% | 23.1 | 24.1 | 24.4 | 21.3 | 21.4 | 21.4 | 2.4 |
| equities | 90-99% | 50.9 | 54.4 | 56.5 | 51.6 | 51.8 | 45.5 | 5.6 |
| equities | Top 1% | 79.1 | 84.6 | 84.3 | 78.9 | 83.9 | 75.1 | 10.5 |
| pass_throughs | Bottom 50% | 5.1 | 4.9 | 4.8 | 5.5 | 6.0 | 5.5 | 1.7 |
| pass_throughs | 50-90% | 12.9 | 11.8 | 11.4 | 12.4 | 12.6 | 11.4 | 1.6 |
| pass_throughs | 90-99% | 31.9 | 29.9 | 30.3 | 28.4 | 28.6 | 24.6 | 3.3 |
| pass_throughs | Top 1% | 63.5 | 67.4 | 68.3 | 66.6 | 66.1 | 50.9 | 4.6 |
| dc | Bottom 50% | 26.9 | 32.2 | 31.8 | 28.0 | 28.8 | 33.1 | 1.8 |
| dc | 50-90% | 67.3 | 67.2 | 66.3 | 67.5 | 67.8 | 65.5 | 2.5 |
| dc | 90-99% | 89.0 | 89.5 | 88.1 | 90.0 | 88.3 | 83.8 | 3.5 |
| dc | Top 1% | 90.6 | 91.1 | 88.6 | 90.1 | 91.6 | 89.5 | 8.1 |
| primary_mortgage | Bottom 50% | 28.4 | 27.5 | 27.5 | 24.6 | 26.5 | 31.0 | 2.1 |
| primary_mortgage | 50-90% | 58.8 | 55.7 | 57.3 | 56.5 | 57.9 | 54.7 | 2.8 |
| primary_mortgage | 90-99% | 78.7 | 73.2 | 71.5 | 70.8 | 72.1 | 68.0 | 4.3 |
| primary_mortgage | Top 1% | 65.5 | 62.7 | 56.8 | 53.9 | 56.9 | 61.6 | 7.8 |
| bonds | Bottom 50% | 6.6 | 7.0 | 6.2 | 7.0 | 6.4 | 8.0 | 1.7 |
| bonds | 50-90% | 14.6 | 15.6 | 14.9 | 15.3 | 15.5 | 13.9 | 2.0 |
| bonds | 90-99% | 30.1 | 32.0 | 30.3 | 28.7 | 29.5 | 23.6 | 6.4 |
| bonds | Top 1% | 43.6 | 53.8 | 49.1 | 43.7 | 43.4 | 36.9 | 15.0 |
| trusts | Bottom 50% | 1.2 | 0.7 | 0.7 | 0.8 | 0.7 | 0.7 | 0.9 |
| trusts | 50-90% | 1.9 | 1.4 | 1.4 | 1.4 | 1.4 | 1.2 | 0.9 |
| trusts | 90-99% | 4.8 | 4.4 | 4.3 | 4.8 | 5.5 | 3.3 | 1.7 |
| trusts | Top 1% | 9.9 | 12.6 | 9.8 | 13.9 | 13.2 | 8.2 | 1.9 |
| other_home | Bottom 50% | 7.9 | 8.0 | 6.8 | 6.6 | 6.4 | 8.3 | 1.4 |
| other_home | 50-90% | 16.5 | 15.3 | 14.7 | 15.4 | 15.3 | 13.4 | 1.9 |
| other_home | 90-99% | 38.6 | 34.8 | 36.0 | 40.2 | 38.2 | 25.5 | 5.1 |
| other_home | Top 1% | 70.3 | 64.2 | 62.2 | 67.6 | 58.9 | 46.1 | 7.2 |

**Net worth** (assets minus debts, dollars of the survey year; the four unrealized-gain variables are not part of net worth).

| Net worth statistic | Truth | joint model (model_joint_w1_scf_plain) | one-source model (model_scfonly_w1_scf_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (± %) |
|---|---|---|---|---|---|---|---|
| p10 | -0k | -2k | -1k | -3k | -2k | -1k |  |
| p25 | 11k | 11k | 10k | 9k | 9k | 20k | 22.2 |
| p50 | 85k | 94k | 96k | 85k | 86k | 123k | 12.0 |
| p75 | 341k | 398k | 400k | 364k | 349k | 402k | 6.1 |
| p90 | 1,090k | 1,236k | 1,250k | 1,114k | 1,070k | 1,049k | 9.3 |
| p99 | 9,948k | 11,511k | 12,416k | 10,555k | 9,884k | 6,372k | 10.0 |
| mean | 661k | 744k | 836k | 746k | 643k | 505k | 7.1 |
| top10%_share | 77.8% | 77.8% | 80.0% | 79.9% | 77.4% | 67.2% | 1.9 |
| top1%_share | 37.3% | 37.5% | 43.0% | 45.2% | 39.8% | 27.1% | 4.8 |
| top0.1%_share | 13.3% | 12.9% | 16.3% | 19.8% | 15.2% | 8.6% | 9.5 |

| Income group | Truth, mean | Truth, median | joint model (model_joint_w1_scf_plain), mean | joint model (model_joint_w1_scf_plain), median | one-source model (model_scfonly_w1_scf_plain), mean | one-source model (model_scfonly_w1_scf_plain), median | production forest (comp_drf), mean | production forest (comp_drf), median | production forest (comp_drfsw), mean | production forest (comp_drfsw), median | sequential trees (comp_seqtrees), mean | sequential trees (comp_seqtrees), median | Sampling noise, mean (± %) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Bottom 50% | 132k | 19k | 181k | 20k | 157k | 18k | 118k | 15k | 116k | 16k | 201k | 44k | 16.3 |
| 50-90% | 407k | 149k | 489k | 148k | 530k | 159k | 385k | 145k | 371k | 143k | 425k | 169k | 8.0 |
| 90-99% | 2,173k | 915k | 2,569k | 1,023k | 2,965k | 1,112k | 2,351k | 1,073k | 2,127k | 1,015k | 1,518k | 714k | 9.4 |
| Top 1% | 17,691k | 9,598k | 16,380k | 8,741k | 20,225k | 9,916k | 24,989k | 10,769k | 18,548k | 9,326k | 6,448k | 3,542k | 14.1 |

**The top of each component, in rank.** Share of holders (with a positive amount) in the top 1% and top 0.1% rank bands of that component (%). By construction the weighted truth is near 1% and 0.1%; a method above that puts too many holders at the very top, which inflates net worth at the top once decoded to dollars. The unweighted columns count sample records instead of population weight: they show how far the SCF sample over-represents the top, which is what a model that learned the sample, not the population, would reproduce.

| Component | Truth, top 1% | Truth, top 0.1% | Truth unweighted (sample), top 1% | Truth unweighted (sample), top 0.1% | joint model (model_joint_w1_scf_plain), top 1% | joint model (model_joint_w1_scf_plain), top 0.1% | one-source model (model_scfonly_w1_scf_plain), top 1% | one-source model (model_scfonly_w1_scf_plain), top 0.1% | production forest (comp_drf), top 1% | production forest (comp_drf), top 0.1% | production forest (comp_drfsw), top 1% | production forest (comp_drfsw), top 0.1% | sequential trees (comp_seqtrees), top 1% | sequential trees (comp_seqtrees), top 0.1% |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| cash | 1.51 | 0.29 | 8.26 | 2.98 | 1.17 | 0.14 | 1.26 | 0.15 | 1.05 | 0.15 | 1.00 | 0.13 | 0.91 | 0.10 |
| equities | 1.50 | 0.10 | 17.17 | 4.78 | 1.42 | 0.14 | 1.75 | 0.19 | 1.66 | 0.19 | 1.25 | 0.13 | 0.54 | 0.05 |
| bonds | 1.36 | 0.11 | 15.59 | 4.25 | 1.39 | 0.19 | 1.71 | 0.21 | 1.17 | 0.13 | 1.12 | 0.12 | 0.62 | 0.04 |
| dc | 1.42 | 0.24 | 5.48 | 1.52 | 1.22 | 0.13 | 1.31 | 0.15 | 1.09 | 0.18 | 0.99 | 0.17 | 0.66 | 0.05 |
| db | 1.03 | 0.14 | 6.20 | 1.44 | 0.77 | 0.17 | 1.02 | 0.18 | 1.33 | 0.28 | 0.90 | 0.21 | 0.67 | 0.14 |
| life_ins | 1.08 | 0.03 | 9.12 | 2.57 | 1.18 | 0.11 | 1.38 | 0.16 | 1.13 | 0.23 | 1.13 | 0.19 | 0.65 | 0.05 |
| annuities | 0.59 | 0.00 | 2.12 | 0.00 | 0.32 | 0.11 | 0.84 | 0.20 | 1.21 | 0.13 | 0.87 | 0.13 | 0.90 | 0.13 |
| trusts | 2.03 | 0.63 | 21.62 | 6.13 | 1.55 | 0.18 | 1.82 | 0.28 | 2.18 | 0.23 | 1.50 | 0.12 | 1.12 | 0.18 |
| other_fin | 0.95 | 0.13 | 15.59 | 6.65 | 0.94 | 0.16 | 1.55 | 0.19 | 1.11 | 0.20 | 1.11 | 0.16 | 0.63 | 0.08 |
| pass_throughs | 0.77 | 0.07 | 18.13 | 5.41 | 1.58 | 0.13 | 2.34 | 0.28 | 1.63 | 0.22 | 1.13 | 0.12 | 0.58 | 0.05 |
| primary_home | 0.98 | 0.19 | 10.71 | 3.71 | 1.33 | 0.13 | 1.34 | 0.16 | 1.40 | 0.23 | 1.08 | 0.13 | 0.74 | 0.06 |
| other_home | 1.46 | 0.45 | 12.48 | 3.76 | 1.54 | 0.22 | 1.87 | 0.26 | 1.71 | 0.22 | 1.20 | 0.14 | 0.42 | 0.03 |
| re_fund | 0.76 | 0.34 | 11.83 | 3.76 | 1.41 | 0.16 | 1.57 | 0.21 | 1.10 | 0.16 | 1.14 | 0.10 | 0.41 | 0.05 |
| other_nonfin | 1.68 | 0.13 | 7.36 | 2.69 | 1.14 | 0.14 | 1.19 | 0.13 | 1.15 | 0.14 | 1.07 | 0.11 | 0.87 | 0.08 |
| primary_mortgage | 1.13 | 0.06 | 6.64 | 1.61 | 1.34 | 0.12 | 1.40 | 0.14 | 1.28 | 0.20 | 1.01 | 0.11 | 0.62 | 0.04 |
| other_mortgage | 0.72 | 0.01 | 6.24 | 0.85 | 1.22 | 0.11 | 1.37 | 0.11 | 1.67 | 0.16 | 1.30 | 0.09 | 0.45 | 0.05 |
| credit_lines | 2.61 | 1.46 | 22.47 | 13.48 | 0.76 | 0.13 | 1.22 | 0.09 | 1.88 | 0.30 | 1.05 | 0.24 | 1.02 | 0.03 |
| credit_cards | 1.10 | 0.00 | 1.38 | 0.09 | 0.61 | 0.05 | 0.86 | 0.07 | 1.06 | 0.17 | 1.06 | 0.09 | 0.74 | 0.07 |
| installment_debt | 1.55 | 0.10 | 2.90 | 0.91 | 0.65 | 0.08 | 0.80 | 0.10 | 1.09 | 0.09 | 1.02 | 0.11 | 0.75 | 0.06 |
| other_debt | 3.29 | 0.09 | 14.63 | 4.71 | 0.64 | 0.07 | 0.87 | 0.11 | 1.57 | 0.24 | 1.26 | 0.11 | 0.59 | 0.08 |
| kg_primary_home | 1.08 | 0.15 | 7.42 | 2.41 | 1.58 | 0.16 | 1.53 | 0.16 | 1.33 | 0.20 | 1.07 | 0.14 | 0.69 | 0.07 |
| kg_other_re | 1.51 | 0.22 | 11.33 | 3.80 | 1.47 | 0.14 | 1.88 | 0.25 | 1.31 | 0.18 | 1.24 | 0.15 | 0.53 | 0.07 |
| kg_pass_throughs | 0.88 | 0.08 | 17.27 | 5.59 | 1.75 | 0.21 | 2.29 | 0.29 | 1.52 | 0.24 | 1.15 | 0.12 | 0.30 | 0.03 |
| kg_other | 1.81 | 0.17 | 16.04 | 5.53 | 1.57 | 0.21 | 1.81 | 0.18 | 1.57 | 0.18 | 1.22 | 0.14 | 0.38 | 0.04 |

**Conditional holding rates (%).** Rows where the second column is an income (business, int_div, capital_gains, rent, ss_pens) mean "among units with that income nonzero".

| Holds | Among those holding | Truth | joint model (model_joint_w1_scf_plain) | one-source model (model_scfonly_w1_scf_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|---|
| primary_home | equities | 83.5 | 87.2 | 89.5 | 85.2 | 85.8 | 83.6 | 2.9 |
| primary_mortgage | primary_home | 70.9 | 69.0 | 68.1 | 67.3 | 68.6 | 68.2 | 2.3 |
| kg_primary_home | primary_home | 95.2 | 95.3 | 95.6 | 95.1 | 95.0 | 92.3 | 1.0 |
| other_home | primary_home | 20.3 | 19.9 | 18.9 | 19.6 | 18.5 | 15.2 | 0.7 |
| equities | dc | 30.0 | 31.4 | 32.0 | 29.8 | 30.1 | 27.4 | 2.3 |
| bonds | equities | 31.8 | 33.1 | 31.5 | 32.4 | 32.1 | 25.9 | 3.3 |
| credit_cards | primary_home | 47.1 | 44.9 | 42.2 | 42.5 | 43.8 | 43.9 | 1.8 |
| pass_throughs | business | 58.2 | 46.9 | 44.9 | 38.0 | 34.6 | 45.3 | 1.7 |
| equities | int_div | 52.1 | 58.7 | 57.3 | 48.7 | 46.1 | 50.6 | 3.5 |
| kg_other | capital_gains | 69.4 | 73.4 | 71.7 | 60.2 | 54.2 | 59.4 | 6.7 |
| other_home | rent | 52.7 | 47.4 | 43.4 | 32.4 | 28.4 | 35.5 | 6.3 |
| db | ss_pens | 8.8 | 7.2 | 6.1 | 7.1 | 7.3 | 6.5 | 1.4 |

**Rank correlation of each amount with total income, among holders** (blank where fewer than 200 true holder records).

| Component | Truth | joint model (model_joint_w1_scf_plain) | one-source model (model_scfonly_w1_scf_plain) | production forest (comp_drf) | production forest (comp_drfsw) | sequential trees (comp_seqtrees) | Sampling noise (±) |
|---|---|---|---|---|---|---|---|
| cash | 0.58 | 0.48 | 0.52 | 0.56 | 0.55 | 0.47 | 0.03 |
| equities | 0.49 | 0.35 | 0.39 | 0.38 | 0.40 | 0.28 | 0.06 |
| bonds | 0.40 | 0.31 | 0.36 | 0.38 | 0.33 | 0.15 | 0.07 |
| dc | 0.56 | 0.50 | 0.53 | 0.56 | 0.55 | 0.35 | 0.01 |
| db | 0.29 | 0.29 | 0.29 | 0.31 | 0.29 | 0.13 | 0.07 |
| life_ins | 0.37 | 0.37 | 0.38 | 0.40 | 0.40 | 0.17 | 0.06 |
| annuities | 0.16 | 0.21 | 0.25 | 0.28 | 0.27 | 0.10 | 0.10 |
| trusts | 0.34 | 0.36 | 0.40 | 0.60 | 0.55 | 0.16 | 0.10 |
| other_fin | 0.54 | 0.45 | 0.49 | 0.48 | 0.46 | 0.22 | 0.08 |
| pass_throughs | 0.51 | 0.48 | 0.53 | 0.53 | 0.50 | 0.33 | 0.08 |
| primary_home | 0.61 | 0.52 | 0.55 | 0.60 | 0.58 | 0.48 | 0.03 |
| other_home | 0.35 | 0.39 | 0.47 | 0.44 | 0.45 | 0.26 | 0.06 |
| re_fund | 0.40 | 0.37 | 0.42 | 0.40 | 0.42 | 0.17 | 0.10 |
| other_nonfin | 0.60 | 0.51 | 0.53 | 0.57 | 0.56 | 0.41 | 0.01 |
| primary_mortgage | 0.54 | 0.48 | 0.48 | 0.50 | 0.50 | 0.36 | 0.02 |
| other_mortgage | 0.38 | 0.37 | 0.43 | 0.46 | 0.48 | 0.19 | 0.04 |
| credit_lines | 0.67 | 0.43 | 0.46 | 0.61 | 0.59 | 0.18 |  |
| credit_cards | 0.31 | 0.29 | 0.29 | 0.33 | 0.32 | 0.12 | 0.04 |
| installment_debt | 0.33 | 0.27 | 0.28 | 0.27 | 0.28 | 0.18 | 0.04 |
| other_debt | 0.31 | 0.39 | 0.42 | 0.47 | 0.44 | 0.16 | 0.07 |
| kg_primary_home | 0.14 | 0.20 | 0.24 | 0.22 | 0.20 | 0.15 | 0.02 |
| kg_other_re | 0.21 | 0.27 | 0.32 | 0.25 | 0.26 | 0.12 | 0.08 |
| kg_pass_throughs | 0.39 | 0.42 | 0.46 | 0.41 | 0.41 | 0.24 | 0.04 |
| kg_other | 0.40 | 0.32 | 0.36 | 0.35 | 0.37 | 0.25 | 0.08 |

**What the classifier relies on** (drop in AUC when the variable is scrambled):

- joint model (model_joint_w1_scf_plain): cash (0.029), primary_home (0.028), primary_mortgage (0.025), wages (0.022), other_nonfin (0.019), kg_primary_home (0.019)
- one-source model (model_scfonly_w1_scf_plain): primary_home (0.034), kg_primary_home (0.032), cash (0.028), primary_mortgage (0.026), other_nonfin (0.020), wages (0.018)
- production forest (comp_drf): cash (0.037), rent (0.030), kg_primary_home (0.025), other_nonfin (0.025), installment_debt (0.021), primary_home (0.020)
- production forest (comp_drfsw): cash (0.038), rent (0.038), kg_primary_home (0.027), primary_home (0.022), other_nonfin (0.020), int_div (0.019)
- sequential trees (comp_seqtrees): kg_primary_home (0.045), primary_home (0.040), other_nonfin (0.033), dc (0.032), primary_mortgage (0.031), wages (0.029)


#### PUF 2015 records completed with wealth, against the SCF interpolated to 2015

Gaps are between the completed PUF and the SCF interpolated to 2015 (one third 2013, two thirds 2016). The scale column is the SCF's own change between its 2013 and 2016 waves (including its sampling noise). Income groups are ranks within each source. The SCF side leaves out split-off units (no SCF wealth), so it covers fewer low-wealth units than the PUF. Full tables per run: `$S/eval/expB/<label>_puf/report.md`.

| Measure | SCF 2013 against SCF 2016 (scale) | joint model (model_joint_w1_puf_plain) | joint model (model_joint_w1_puf_asSCF_plain) | production forest (comp_drf-puf) | production forest (comp_drfsw-puf) | sequential trees (comp_seqtrees-puf) |
|---|---|---|---|---|---|---|
| Share holding, all records: average gap (points) | 1.28 | 4.19 [3.04-4.99] | 1.83 [1.72-1.95] | 1.67 [1.63-1.73] | 1.38 [1.36-1.40] | 1.28 [1.26-1.30] |
| Share holding by income group: average gap (points) | 2.08 | 4.98 [4.12-5.41] | 2.58 [2.33-2.77] | 1.46 [1.41-1.49] | 1.14 [1.12-1.15] | 3.88 [3.85-3.92] |
| Share holding by age range: average gap (points) | 1.93 | 5.02 [4.09-5.64] | 2.14 [2.02-2.35] | 1.67 [1.59-1.72] | 1.57 [1.51-1.65] | 1.87 [1.81-1.93] |
| Amounts among holders by income group: average gap (%) | 30.7 | 56.2 [54.3-58.5] | 32.9 [30.1-34.5] | 19.6 [19.1-20.5] | 14.7 [14.4-15.1] | 84.5 [84.1-84.8] |

| Net worth | SCF 2013 | SCF 2016 | SCF interpolated 2015 | joint model (model_joint_w1_puf_plain) | joint model (model_joint_w1_puf_asSCF_plain) | production forest (comp_drf-puf) | production forest (comp_drfsw-puf) | sequential trees (comp_seqtrees-puf) |
|---|---|---|---|---|---|---|---|---|
| p50 | 81k | 98k | 92k | 151k | 91k | 62k | 67k | 125k |
| p90 | 979k | 1,215k | 1,131k | 1,499k | 1,484k | 1,058k | 1,048k | 1,192k |
| p99 | 7,859k | 10,350k | 9,443k | 8,426k | 12,003k | 9,096k | 9,056k | 6,516k |
| mean | 543k | 705k | 646k | 705k | 835k | 657k | 596k | 555k |
| top10%_share | 74.8% | 76.7% | 76.1% | 67.5% | 77.6% | 79.8% | 77.9% | 66.9% |
| top1%_share | 35.1% | 38.1% | 37.1% | 28.3% | 37.3% | 44.1% | 39.6% | 27.9% |
| top0.1%_share | 13.1% | 14.7% | 14.1% | 9.8% | 13.7% | 19.3% | 15.7% | 10.8% |
<!-- END GENERATED expB -->

<!-- BEGIN GENERATED expB_multi -->
### Dataset `expB_multi`
Evaluation set: 22,335 records in 4,077 households. Generated 2026-10-10 02:07.

**Headline.** "Sampling noise" is the typical gap between two random halves of the true records (households kept whole), averaged over five splits; for the level rows it is the typical gap, in points, and for the ratio rows the typical distance from 1. "No information" gives every record the wealth of a random other record. Method columns: mean over training seeds [lowest-highest seed].

| Measure | Truth | Sampling noise | No information (shuffled) |
|---|---|---|---|
| Share holding each component, all records: average gap (percentage points) |  | 1.00 | 0.07 |
| Share holding, by income group (24 components x 4 groups): average gap (points) |  | 2.79 | 13.32 |
| Share holding, by age range: average gap (points) |  | 2.55 | 5.48 |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks |  | 5.66 | 17.16 |
| Same, in dollars: average gap (%) |  | 55.1 | 237.2 |
| Amounts among holders by age range: average gap in percentile ranks |  | 6.63 | 10.13 |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) |  | 48.9 | 4.9 |
| Pairs of components (276 two-by-two holding tables): average share misplaced (%) |  | 1.81 | 0.15 |
| Selected conditional holding rates (12, e.g. home among stockholders): average gap (points) |  | 2.35 | 13.68 |
| Rank correlation of each amount with income, among holders: average gap |  | 0.061 | 0.424 |
| Rank correlation between amounts, among joint holders: average gap |  | 0.104 | 0.015 |
| Holders in the top 1% rank band of their component: completed / true, averaged over components |  | ±0.85 | 1.03 |
| Same for the top 0.1% rank band |  | ±0.80 | 0.95 |
| Median net worth, completed / true |  | ±0.126 | 0.993 |
| 90th percentile of net worth, completed / true |  | ±0.073 | 1.027 |
| 99th percentile of net worth, completed / true |  | ±0.129 | 1.051 |
| Top 10% share of net worth (%) | 76.6 | ±0.5 | 76.4 |
| Top 1% share of net worth (%) | 37.3 | ±2.0 | 35.8 |
| Top 0.1% share of net worth (%) | 14.6 | ±1.3 | 13.5 |
| Mean net worth by income group: average gap (%) |  | 14.2 | 159.9 |
| Top 1% by net worth: share whose wealth tokens repeat another household's (%) | 0.0 | ±0.0 | 75.2 |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) |  | 0.518 | 0.830 |

**Other variants** (same measures): runs without `_w1` are the earlier configuration (survey weight as a model variable; `plain` as drawn, `wc` weight-corrected); `_small` smaller model; `_w1`, `_wsqrt` model trained with each record's loss weighted by its population weight or its square root, and `_nowt` the unweighted control (in these three the survey weight is not a model variable and the plain and weight-corrected files are identical); `drfsw` the production forest fitted with survey weights instead of a weighted bootstrap (a diagnostic, not production). The detail tables below show only the main runs.

| Measure | Truth | Sampling noise | No information (shuffled) | joint model (model_joint_w1_large_scf1922_plain) | joint model (model_joint_w1_large_scf1922_wc) | joint model (model_joint_w1_large_scf_plain) | joint model (model_joint_w1_large_scf_wc) | one-source model (model_scfonly_w1_large_scf1922_plain) | one-source model (model_scfonly_w1_large_scf1922_wc) | one-source model (model_scfonly_w1_large_scf_plain) | one-source model (model_scfonly_w1_large_scf_wc) |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Share holding each component, all records: average gap (percentage points) |  | 1.00 | 0.07 | 0.98 [0.80-1.08] | 0.98 [0.80-1.08] | 0.94 [0.84-1.03] | 0.94 [0.84-1.03] | 1.01 [0.86-1.25] | 1.01 [0.86-1.25] | 1.21 [1.07-1.46] | 1.21 [1.07-1.46] |
| Share holding, by income group (24 components x 4 groups): average gap (points) |  | 2.79 | 13.32 | 2.34 [2.31-2.36] | 2.34 [2.31-2.36] | 2.22 [2.19-2.26] | 2.22 [2.19-2.26] | 2.66 [2.48-2.90] | 2.66 [2.48-2.90] | 2.55 [2.35-2.91] | 2.55 [2.35-2.91] |
| Share holding, by age range: average gap (points) |  | 2.55 | 5.48 | 2.34 [2.28-2.41] | 2.34 [2.28-2.41] | 1.98 [1.91-2.11] | 1.98 [1.91-2.11] | 2.26 [2.19-2.32] | 2.26 [2.19-2.32] | 2.02 [1.87-2.16] | 2.02 [1.87-2.16] |
| Amounts among holders by income group, 10th-90th percentiles: average gap in percentile ranks |  | 5.66 | 17.16 | 5.19 [5.17-5.24] | 5.19 [5.17-5.24] | 4.84 [4.77-4.94] | 4.84 [4.77-4.94] | 5.30 [5.25-5.34] | 5.30 [5.25-5.34] | 4.79 [4.71-4.84] | 4.79 [4.71-4.84] |
| Same, in dollars: average gap (%) |  | 55.1 | 237.2 | 48.4 [47.4-49.8] | 48.5 [47.5-49.7] | 44.6 [43.9-45.2] | 44.9 [44.0-45.5] | 48.2 [47.9-48.5] | 48.2 [47.7-48.8] | 43.6 [43.0-43.9] | 43.7 [43.1-44.1] |
| Amounts among holders by age range: average gap in percentile ranks |  | 6.63 | 10.13 | 6.10 [6.09-6.11] | 6.10 [6.09-6.11] | 6.06 [5.97-6.16] | 6.06 [5.97-6.16] | 6.09 [5.97-6.20] | 6.09 [5.97-6.20] | 5.85 [5.65-5.96] | 5.85 [5.65-5.96] |
| Patterns of holding across the 24 components: share of weight in the wrong pattern (%) |  | 48.9 | 4.9 | 44.9 [44.3-45.2] | 44.9 [44.3-45.2] | 41.8 [41.3-42.2] | 41.8 [41.3-42.2] | 43.7 [43.5-43.9] | 43.7 [43.5-43.9] | 41.1 [40.8-41.3] | 41.1 [40.8-41.3] |
| Pairs of components (276 two-by-two holding tables): average share misplaced (%) |  | 1.81 | 0.15 | 1.79 [1.49-1.98] | 1.79 [1.49-1.98] | 1.66 [1.54-1.76] | 1.66 [1.54-1.76] | 1.79 [1.51-2.17] | 1.79 [1.51-2.17] | 2.05 [1.85-2.46] | 2.05 [1.85-2.46] |
| Selected conditional holding rates (12, e.g. home among stockholders): average gap (points) |  | 2.35 | 13.68 | 2.09 [1.99-2.17] | 2.09 [1.99-2.17] | 2.71 [2.22-3.08] | 2.71 [2.22-3.08] | 2.50 [2.26-2.80] | 2.50 [2.26-2.80] | 3.30 [2.78-3.70] | 3.30 [2.78-3.70] |
| Rank correlation of each amount with income, among holders: average gap |  | 0.061 | 0.424 | 0.073 [0.070-0.076] | 0.073 [0.070-0.076] | 0.071 [0.065-0.077] | 0.071 [0.065-0.077] | 0.076 [0.071-0.083] | 0.076 [0.071-0.084] | 0.076 [0.071-0.078] | 0.076 [0.071-0.079] |
| Rank correlation between amounts, among joint holders: average gap |  | 0.104 | 0.015 | 0.094 [0.094-0.095] | 0.095 [0.094-0.095] | 0.090 [0.087-0.094] | 0.090 [0.087-0.094] | 0.097 [0.094-0.100] | 0.097 [0.094-0.100] | 0.087 [0.085-0.089] | 0.087 [0.085-0.089] |
| Holders in the top 1% rank band of their component: completed / true, averaged over components |  | ±0.85 | 1.03 | 1.36 [1.31-1.43] | 1.36 [1.31-1.43] | 1.06 [0.99-1.12] | 1.06 [0.99-1.12] | 1.40 [1.32-1.49] | 1.40 [1.32-1.49] | 1.07 [0.96-1.16] | 1.07 [0.96-1.16] |
| Same for the top 0.1% rank band |  | ±0.80 | 0.95 | 2.67 [2.43-3.00] | 2.67 [2.43-3.00] | 8.57 [7.90-9.11] | 8.57 [7.90-9.11] | 2.22 [2.00-2.51] | 2.22 [2.00-2.51] | 6.77 [5.07-9.77] | 6.77 [5.07-9.77] |
| Median net worth, completed / true |  | ±0.126 | 0.993 | 0.958 [0.867-1.065] | 0.958 [0.867-1.064] | 1.169 [1.090-1.293] | 1.166 [1.086-1.289] | 1.022 [1.003-1.038] | 1.021 [1.000-1.041] | 1.232 [1.211-1.267] | 1.232 [1.214-1.263] |
| 90th percentile of net worth, completed / true |  | ±0.073 | 1.027 | 1.142 [1.108-1.188] | 1.141 [1.110-1.193] | 1.154 [1.128-1.206] | 1.154 [1.126-1.204] | 1.114 [1.064-1.145] | 1.112 [1.060-1.143] | 1.118 [1.034-1.185] | 1.120 [1.037-1.181] |
| 99th percentile of net worth, completed / true |  | ±0.129 | 1.051 | 1.155 [1.136-1.185] | 1.153 [1.135-1.171] | 1.104 [1.075-1.135] | 1.107 [1.066-1.146] | 1.086 [1.053-1.126] | 1.079 [1.049-1.116] | 1.076 [1.050-1.105] | 1.078 [1.042-1.109] |
| Top 10% share of net worth (%) | 76.6 | ±0.5 | 76.4 | 77.7 [76.8-78.6] | 77.7 [76.7-78.7] | 77.8 [77.5-78.3] | 77.8 [77.4-78.3] | 75.5 [75.0-75.8] | 75.6 [74.9-76.2] | 76.7 [76.2-77.4] | 76.6 [76.1-77.3] |
| Top 1% share of net worth (%) | 37.3 | ±2.0 | 35.8 | 39.7 [37.6-42.6] | 39.7 [37.4-43.0] | 39.1 [37.9-40.1] | 39.0 [37.8-40.2] | 36.9 [36.4-37.3] | 37.2 [36.4-38.5] | 38.1 [37.3-39.4] | 37.9 [37.0-39.2] |
| Top 0.1% share of net worth (%) | 14.6 | ±1.3 | 13.5 | 16.3 [14.9-18.9] | 16.3 [14.6-19.4] | 14.8 [13.3-15.7] | 14.6 [13.1-15.8] | 13.3 [12.7-13.7] | 13.9 [12.7-15.4] | 13.8 [13.0-15.0] | 13.6 [12.9-14.9] |
| Mean net worth by income group: average gap (%) |  | 14.2 | 159.9 | 24.8 [13.5-35.3] | 24.8 [12.2-35.4] | 21.0 [17.6-24.5] | 21.9 [17.5-26.1] | 17.3 [12.6-20.9] | 18.7 [12.1-25.3] | 17.8 [13.1-20.6] | 17.7 [12.9-21.0] |
| Top 1% by net worth: share whose wealth tokens repeat another household's (%) | 0.0 | ±0.0 | 75.2 | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] | 0.0 [0.0-0.0] |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) |  | 0.518 | 0.830 | 0.573 [0.556-0.591] | 0.573 [0.556-0.591] | 0.591 [0.589-0.593] | 0.591 [0.589-0.593] | 0.567 [0.561-0.573] | 0.567 [0.561-0.573] | 0.578 [0.570-0.588] | 0.578 [0.570-0.588] |

**Share holding each component, all records (%).**

| Component | Truth | Sampling noise (±) |
|---|---|---|
| cash | 95.0 | 0.7 |
| equities | 22.8 | 0.7 |
| bonds | 11.6 | 1.3 |
| dc | 52.7 | 1.2 |
| db | 7.5 | 0.6 |
| life_ins | 17.7 | 1.1 |
| annuities | 4.0 | 0.5 |
| trusts | 2.2 | 0.3 |
| other_fin | 8.7 | 1.2 |
| pass_throughs | 13.0 | 0.9 |
| primary_home | 67.1 | 0.4 |
| other_home | 14.0 | 1.1 |
| re_fund | 6.4 | 0.9 |
| other_nonfin | 87.2 | 1.3 |
| primary_mortgage | 46.8 | 1.4 |
| other_mortgage | 5.3 | 0.5 |
| credit_lines | 1.7 | 0.4 |
| credit_cards | 45.1 | 2.5 |
| installment_debt | 53.6 | 1.9 |
| other_debt | 5.9 | 0.6 |
| kg_primary_home | 64.4 | 0.4 |
| kg_other_re | 15.6 | 1.7 |
| kg_pass_throughs | 12.0 | 1.0 |
| kg_other | 21.2 | 1.3 |

<!-- END GENERATED expB_multi -->
