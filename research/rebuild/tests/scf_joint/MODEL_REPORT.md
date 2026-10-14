---
title: "Test 2 — piece B: the model"
role: results
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/scf_joint/SPEC.md
supersedes: []
superseded_by: null
---

# Piece B: the model

This report describes the model used in test 2 (`SPEC.md`, section B), how
it was trained, how records are completed, and the training results. It
reports losses, likelihood scores and a few quick checks of my own; the
full comparison of completed wealth
with the true wealth is piece C's (`EVAL_REPORT.md`).

`$S` below is `/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint`.

## Terms

- **Token.** Piece A turns each value into an integer code: a category, a
  band of weighted rank within the source and year, or exact zero. The model
  works on these codes.
- **Embedding.** A learned vector of numbers that stands for a code, a
  variable, a source or a year inside the model.
- **Hidden variable.** A variable whose value the model is not shown; it
  sees a single learned "hidden" vector in its place and predicts a
  probability for every possible code.
- **Cross-entropy (loss).** The average of minus the log of the probability
  the model gave to the true code, in nats (natural-log units). Lower is
  better.
- **Epoch.** One pass over all training records.
- **Overfitting.** Fitting quirks of the training records that do not hold
  in other records; it shows as validation loss rising while training loss
  keeps falling.
- **Training seed.** The number that fixes the random starting values and
  the random order and hiding during training. Test 1 found that changing
  only this moves results about as much as most changes to the model, so
  every configuration here is trained three times, with seeds 1, 2 and 3.

## What changed from test 1

The model is test 1's masked transformer (`puf_generator/model.py`), copied
to `b_model.py`, with four changes.

1. **Source.** A learned embedding for the record's source (`PUF`, `SCF`,
   or `SCF_H1` for the pretend-PUF half in experiment A) is added to every
   variable's input vector, so every part of the model knows which survey
   the record comes from.
2. **Year.** The year enters as a number, through a small learned function
   of (year − 2015): a two-layer network applied to (year − 2015) / 5, whose
   output is added to every variable's input vector. Its last layer starts
   at zero, so the year has no effect until training gives it one. Because
   it is a smooth function of a number rather than a label per year, the
   model can place 2015 between the 2013 and 2016 SCF waves; with only those
   two SCF years, how it does so between them is not pinned down by the
   data and should be read from piece C's 2015 comparisons.
3. **Unobserved variables.** Values a source does not observe (wealth and
   the SCF weight for the PUF and for half 1; age range for PUF records
   outside the demographic file) are coded -1. They are always hidden and
   never scored. Hidden-for-training and unobserved look the same to the
   model (the same "hidden" vector), which is what lets it complete them.
4. **Context is not a variable.** Source and year are not predicted; the
   `source` column of the token files is not used (the source is read from
   the records file).

Everything else is as in test 1: one input slot per variable, no position
encoding, a learned function of each amount code's position in its value
order, pre-normalisation transformer layers, output scores per code of the
variable's own vocabulary, a moving average of the weights (decay 0.999)
used for validation and completion.

## Training

- **Any-order hiding on observed variables.** For each record at each step a
  hiding rate r is drawn uniformly from (0, 1) and each *observed* variable
  is hidden with probability r; if none was hidden, one observed variable is
  chosen at random and hidden. The loss is the cross-entropy of the hidden
  observed variables given the visible ones. For a PUF record, the model
  thus learns the relationships among incomes and demographics; for an SCF
  record, also those involving wealth and the weight.
- **Records are unweighted** in the first set of runs. The SCF weight is a
  variable (64 bins of its rank within the SCF year), learned along with
  the rest.
- **Weighted variants** (added after the results below, on John's
  direction that the weight should not be a learned variable): the weight
  is always hidden and never scored, and each record's loss is multiplied
  by its population weight (`_w1`) or its square root (`_wsqrt`),
  normalised to mean 1 over the training records; the validation loss used
  for stopping is weighted the same way. Loss weighting was chosen over
  drawing batches in proportion to the weight because it uses every record
  in every epoch. These models are completed without the weight
  correction.
- **Validation and stopping.** Validation loss uses fixed hiding patterns
  (four per validation record, drawn once with seed 777) drawn the same way
  as in training, as test 1 found the stopping rule must match the training
  hiding. Training stops when the validation loss has not improved for 15
  epochs; the model kept is the one with the lowest validation loss.
- **Second measure: completion log-likelihood.** For the best model, on
  validation SCF records: all 25 SCF-only variables are hidden and the
  score is the sum, over the completion order, of minus the log probability
  of each true value given the shared variables and the true values of the
  SCF-only variables before it in the order. This scores the model exactly
  as it is used for completion, in nats per record (lower is better), and
  is comparable between the joint model and a model trained on one source
  only, because both are scored on the same records.
- **Settings.** Base size: 6 layers, width 256, 8 attention heads (about 6
  million parameters, fewer than test 1's 8.6 million because the
  vocabularies are smaller). Small size (`_small`): 4 layers, width 128, 4
  heads. Batch 512 records; AdamW optimiser, learning rate 5e-4 with a
  1,000-step warm-up and cosine decay to one tenth over at most 300 epochs;
  weight decay 0.01; dropout 0.1; gradient norm clipped at 1; bf16 mixed
  precision. Seeds 1, 2, 3.

## Completion

`b_complete.py`. For each target record every observed value is kept, and
the unobserved variables are drawn one at a time at temperature 1, each
given everything observed and drawn so far: first any unobserved shared
variable (PUF age range outside the demographic file), then `meta.json`'s
`generation_order` (the SCF weight, then the 24 wealth variables from most
to least commonly held). Five completions ("draws") per record.

**The weight correction.** The SCF over-samples wealthy households, so the
model, trained on unweighted records, learns the sample's relationship
between incomes and wealth, which is tilted toward wealth. For each draw,
the weight-corrected completion draws K = 20 completions of the record and
keeps one with probability proportional to its drawn SCF weight (each
weight code decoded to its average weight through piece A's quantile
function for that source and year, interpolated for the PUF at 2015). If
the model has learned the sample well, the kept draw follows the
population relationship. The plain completion is the first of the same 20
draws, so the two files differ only by the selection. The info file
reports the average effective number of draws out of 20 that the selection
uses (the square of the sum of the weights over the sum of their squares);
a low number means a few draws dominate and the correction is noisy.

**Targets.**

- Experiment A: half 1 (the `test` split). The joint model completes it
  twice: as its own source (`SCF_H1`, tag `joint`) and presented as source
  `SCF` (tag `joint_asSCF`). The first is what SPEC.md asks; the second is
  added because the joint model never sees wealth for an `SCF_H1` record,
  so how its source embedding interacts with wealth is not trained, and
  presenting the record as `SCF` asks for the relationship it did learn.
  The benchmark trained on half 2 alone (`half2`) knows only source `SCF`
  and completes half 1 presented as `SCF`.
- Experiment B: the SCF test units with all SCF-only variables hidden,
  including their weight (tags `joint_scf`, `scfonly_scf`), so they are
  completed exactly as a PUF record would be; and the PUF test records at
  2015, decoded to dollars at 2015, both as their own source (`joint_puf`)
  and presented as `SCF` (`joint_puf_asSCF`), for the same reason as in
  experiment A: the model never sees wealth on a record tagged `PUF`, so
  what the `PUF` tag does to completed wealth is an extrapolation.
- The option is `b_complete.py --complete-as-source <name>`, added at the
  request of the main session (John's discussion, 9 October).
- SCF units split off a household (about 13% of SCF filer units) have their
  wealth marked unobserved in the final data (John, 9 October), because
  their wealth is zero by construction. The model trains on their incomes
  and weight only and their wealth is never scored.

Output columns: SPEC.md, "Contract changes", B 13:05. The info file next to
each completion also gives the completion log-likelihood of the true values
where they are known (half 1's wealth in experiment A; the SCF test units'
wealth and weight in experiment B).

## Check on fabricated data

Before piece A's files existed, the code was run on fabricated records
(`b_make_dev.py`; nothing real): a "PUF" at 2015 (60,000 records, a simple
random sample, wealth unobserved) and an "SCF" at 2013 and 2016 (about
20,000 records each) that over-samples records with high latent wealth,
with six wealth variables. Two relationships were planted: holding equities
depends strongly on a latent wealth level that also drives interest and
dividends (a shared variable), and equity holding rises over time. A small
model (3 layers, width 128) was trained jointly and on the SCF alone, and
the PUF test records and the SCF test units were completed
(`b_check_dev.py`, output `$S/dev/b_expB_model/check_dev.csv`). Shares are
weighted by the records' population weights.

| Records | Completion | Share holding equities | ... with no interest or dividends | ... lower half of interest and dividends | ... upper half |
|---|---|---|---|---|---|
| PUF 2015 | truth | 0.435 | 0.279 | 0.439 | 0.824 |
| PUF 2015 | joint model, plain | 0.537 | 0.444 | 0.453 | 0.858 |
| PUF 2015 | joint model, weight-corrected | 0.445 | 0.320 | 0.392 | 0.815 |
| SCF test | truth | 0.426 | 0.270 | 0.534 | 0.904 |
| SCF test | joint model, plain | 0.473 | 0.333 | 0.560 | 0.940 |
| SCF test | joint model, weight-corrected | 0.410 | 0.249 | 0.515 | 0.922 |
| SCF test | SCF-only model, plain | 0.497 | 0.354 | 0.595 | 0.941 |
| SCF test | SCF-only model, weight-corrected | 0.432 | 0.273 | 0.543 | 0.922 |

Reference: the SCF training records hold equities at 0.63 (2013) and 0.71
(2016) unweighted, 0.37 and 0.46 weighted. What this shows:

- The planted link is recovered: completed PUF records hold equities far
  more often when their interest and dividends are high, in about the true
  proportions.
- Plain completion reproduces the over-sampled SCF and is too wealthy
  (0.54 against 0.44 for the PUF); the weight correction brings it to 0.45.
  The mean rank of cash moves the same way (0.443 plain, 0.402 corrected,
  0.395 true). On average 16 of the 20 draws carry weight in the selection.
- At 2015 the corrected PUF share (0.445) is between the SCF's 2013 and
  2016 weighted shares, close to the true 0.439 of the fabricated 2015
  population.

## Runs on piece A's data

All on A's final datasets (rebuilt 13:21–13:24, split-off units' wealth
unobserved). Configurations, each with training seeds 1, 2, 3
(`b_pipeline.sbatch`):

| Dataset | Configurations |
|---|---|
| `expA_clean` | `joint`, `half2` (benchmark), `joint_small`, `half2_small`; weighted variants below |
| `expB` | `joint`, `scfonly`, `scfonly_small`; weighted variants below |
| `expA_noise`, `expA_recat`, `expA_unrep` | `joint` |

Per-run tables: `$S/model/b_summary_runs.csv` and
`b_summary_completions.csv` (from `b_summarize.py`); training curves:
`$S/model/<dataset>/training_curves.png`. GPUs were whatever the queue
gave (RTX A5000, L40S, H200, RTX PRO 6000); a base-size run trained in 2–5
minutes, and the completions (5 draws × 20 per record) took 5–12 minutes
each on an A5000.

### Training (means over three seeds)

"Validation loss" is the cross-entropy of randomly hidden observed values
on the validation records, in nats per hidden value; it is not comparable
across datasets or between joint and single-source models, whose
validation records differ. "Completion log-likelihood" is comparable
between models on the same dataset: the SCF validation households with all
SCF-only variables hidden, nats per record (25 variables: the weight and
24 wealth amounts), lower is better.

| Dataset | Model | Parameters | Best epoch | Validation loss | Completion log-likelihood (range over seeds) |
|---|---|---|---|---|---|
| expA_clean | joint | 6.06m | 40 | 1.508 | 39.89 (0.13) |
| expA_clean | joint, small | 1.44m | 46 | 1.511 | 40.04 (0.12) |
| expA_clean | half 2 only | 6.06m | 64 | 1.584 | 40.62 (0.12) |
| expA_clean | half 2 only, small | 1.44m | 76 | 1.579 | 40.45 (0.26) |
| expA_noise | joint | 6.06m | 41 | 1.507 | 39.86 (0.15) |
| expA_recat | joint | 6.06m | 41 | 1.510 | 39.90 (0.17) |
| expA_unrep | joint | 6.06m | 40 | 1.499 | 39.86 (0.12) |
| expB | joint | 6.06m | 17 | 1.658 | 38.66 (0.03) |
| expB | SCF only | 6.06m | 33 | 1.546 | 38.62 (0.07) |
| expB | SCF only, small | 1.44m | 39 | 1.548 | 38.75 (0.09) |

The runs stop early (15 epochs without improvement), well before the
300-epoch limit; training loss keeps falling after the best epoch, so the
stopping rule is what limits overfitting. Seeds agree closely on these
losses (ranges 0.03–0.26 nats per record). In experiment A the joint model
scores about 0.7 nats per record better than the half-2 benchmark on half
2's own validation households; in experiment B the joint and SCF-only
models score the same.

### Completion log-likelihood of the true values (means over three seeds)

Experiment A: half 1's 17,443 units, scored on their weight and, for the
15,380 with wealth observed, the 24 wealth amounts. Experiment B: the
13,415 SCF test units, same scoring. Nats per record, lower is better. Not
comparable with the weighted runs below, which have no weight variable.

| Dataset | Completion | Mean | Range over seeds |
|---|---|---|---|
| expA_clean | joint, half 1 as its own source | 35.15 | 0.35 |
| expA_clean | joint, half 1 as SCF | 34.77 | 0.13 |
| expA_clean | joint small, own source | 35.30 | 0.09 |
| expA_clean | joint small, as SCF | 34.94 | 0.12 |
| expA_clean | half 2 only | 35.40 | 0.16 |
| expA_clean | half 2 only, small | 35.27 | 0.15 |
| expA_noise | joint, own source / as SCF | 35.17 / 34.79 | 0.36 / 0.15 |
| expA_recat | joint, own source / as SCF | 35.20 / 34.76 | 0.23 / 0.11 |
| expA_unrep | joint, own source / as SCF | 35.24 / 34.77 | 0.54 / 0.10 |
| expB | joint, SCF test units | 34.32 | 0.03 |
| expB | SCF only, SCF test units | 34.32 | 0.04 |

Two regularities: presenting half 1 as `SCF` is better than its own tag by
about 0.4 nats per record in every experiment A dataset, and the joint
model presented as SCF is better than the half-2 benchmark by about 0.6.
The three altered-income variants score the same as the clean case; this
likelihood does not show the effect of the alterations.

### The top of the wealth distribution, and weighted training

Piece C's comparison (`EVAL_REPORT.md`) found the completions far too
wealthy at the top. My own diagnosis on `expA_clean` (scripts
`b_quick_check.py`, `b_diag_weight.py`, `b_calib_top.py`; details in
`STATUS.md`, 14:15):

- The weight drawn given incomes matches the sample (15% of draws in the
  lowest-weight eighth against 13% truly), and low drawn weights go with
  top wealth as in the truth.
- But records with high weights (ordinary households, nearly all the
  population weight) get top wealth 6–13 times too often: 1.6–3.3% of them
  are above the true 99th percentile of net worth against 0.25%. The model
  gives them a small probability of top wealth, learned from the many
  over-sampled wealthy records with similar incomes, and the population
  weighting turns that small probability into a large excess at the top.
- The weight correction cannot remove this: for a record whose incomes
  look wealthy, the high-weight draws it prefers are also wealthy, so the
  top moves from the low-weight group to the high-weight group instead of
  leaving.

Following John's direction, the weight was then taken out of the model's
variables (`--drop-vars scf_weight`: always hidden, never scored) and used
in training instead (`--loss-weight survey` or `sqrt`: each record's loss
multiplied by its population weight or its square root, normalised to mean
1; the validation loss used for stopping is weighted the same way).
Completion is then plain, without the correction. Seed 1, half 1 with
wealth observed, population-weighted (truth: mean net worth $725k, median
$93k, 1.0% of weight above the true 99th percentile, 0.10% above the
99.9th, which hold 15% of all net worth):

| Model | Mean | Median | Above true 99th | Above true 99.9th | Share of net worth above true 99.9th |
|---|---|---|---|---|---|
| joint, as SCF, unweighted, weight-corrected | $1.63m | $134k | 2.2% | 0.5% | 48% |
| joint, as SCF, weight dropped, unweighted | $2.04m | $120k | 2.5% | 0.7% | 55% |
| joint, as SCF, square-root weights | $1.38m | $124k | 1.9% | 0.5% | 43% |
| joint, as SCF, full weights | $0.97m | $123k | 1.5% | 0.3% | 25% |
| half 2 only, unweighted, weight-corrected | $1.55m | $142k | 2.3% | 0.3% | 40% |
| half 2 only, full weights | $0.99m | $135k | 1.5% | 0.2% | 22% |

Full weighting roughly halves the excess at the top; it does not remove it,
and the median stays 30–45% high. Square-root weights do less. Over three
seeds (ranges):

| expA_clean, half 1 | Mean | Median | Above true 99th | Above true 99.9th | Share above 99.9th | Holding-rate gap |
|---|---|---|---|---|---|---|
| joint, as SCF, full weights | $0.90–1.08m | $118–125k | 1.4–1.9% | 0.2–0.3% | 16–25% | 0.019–0.022 |
| joint, own source, full weights | $0.76–0.99m | $155–167k | 1.0–1.5% | 0.1–0.2% | 9–16% | 0.033–0.039 |
| half 2 only, full weights | $0.88–0.99m | $119–135k | 1.4–1.6% | 0.2% | 16–22% | 0.019–0.033 |
| joint, as SCF, square-root weights | $1.28–1.68m | $124–141k | 1.7–2.5% | 0.4–0.5% | 37–43% | 0.019–0.025 |
| half 2 only, square-root weights | $1.13–1.47m | $112–141k | 1.7–2.2% | 0.3–0.4% | 31–41% | 0.023–0.033 |

("Holding-rate gap": mean absolute difference between completed and true
shares holding each of the 24 components.) In experiment A, with full
weights, the joint model and the half-2 benchmark are about equal.

Experiment B, the 13,415 SCF test units (2013 and 2016 together, each in
its own year's dollars; truth: mean $661k, median $85k, 13% of net worth
above the true 99.9th percentile):

| expB, SCF test units | Mean | Median | Above true 99th | Above true 99.9th | Share above 99.9th | Holding-rate gap |
|---|---|---|---|---|---|---|
| joint, unweighted, weight-corrected | $0.98–1.04m | $92–98k | 1.4–1.5% | 0.3–0.4% | 36–41% | 0.011–0.012 |
| SCF only, unweighted, weight-corrected | $1.03–1.27m | $96–101k | 1.5–1.8% | 0.4% | 40–44% | 0.013–0.016 |
| joint (PUF + SCF), full weights | $0.69–0.80m | $89–100k | 1.1–1.3% | 0.1–0.2% | 15–18% | 0.008–0.012 |
| SCF only, full weights | $0.82–0.87m | $95–97k | 1.3% | 0.2–0.3% | 23–28% | 0.013–0.016 |

Here the joint model with full weights is close to the truth at the top
and better than the SCF-only model in all three seeds (the ranges do not
overlap): the first result in which adding the PUF clearly helps
completion of SCF records. With full weights the joint training set counts
as 80,710 effective records of 208,387 (the PUF also over-samples high
incomes); the SCF-only set as 36,911 of 47,444. These are my quick checks
(`$S/model/<dataset>/b_quick_check.csv`); piece C's evaluation is the
reference.

The three altered-income variants of experiment A, joint model with full
weights, half 1 as SCF, three seeds: mean net worth $0.83–1.06m, median
$109–128k, 1.2–1.8% above the true 99th percentile, 15–27% of net worth
above the true 99.9th — the same as the clean case within seed noise.
These aggregate measures do not show the alterations; piece C's measures
by income group are where they would appear.

## Questions for John

1. **Weighted training as the default.** Full loss weighting improves the
   top a great deal in both experiments and, in experiment B, makes the
   joint model better than the SCF-only one. Square-root weights do less.
   Should the remaining work use full weights throughout, and drop the
   weight-corrected completion?
2. **Source tag at completion.** Presenting half 1 as SCF gives better
   likelihoods and holding rates than its own tag; its own tag gives a top
   tail closer to the truth but a median 60–80% too high. For the PUF in
   experiment B both are written (`joint_w1_puf`, `joint_w1_puf_asSCF`);
   piece C's 2015 comparison should decide which to use.
3. **What remains in experiment A.** Even with full weights the top is
   still 1.5 times too full above the 99th percentile and the median 30%
   high, with half the 2016 SCF as training data. Experiment B, with more
   SCF records and the PUF, does not show this. Whether to pursue it in
   experiment A (more seeds, a smaller model, more training data) or treat
   it as a small-sample effect is open.

## Problems and notes

- The first runs on real data were started while piece A was rewriting the
  datasets (13:10) and were cancelled; their files are set aside under
  `$S/dev/` and should not be used.
- In experiment B the model sometimes drew a negative-value code (for
  example a negative home value) for a year whose SCF has no negative
  values, which A's decoder cannot turn into dollars. Completion now draws
  only codes the decoder can handle for the record's year (SPEC.md,
  contract change B 13:45); the experiment B completions were rewritten.
- On a provisional version of `expA_clean`, the base model began to overfit
  after about 40 epochs (validation loss rising while training loss fell),
  so the small model is also trained there and on the SCF-only side of
  experiment B.
