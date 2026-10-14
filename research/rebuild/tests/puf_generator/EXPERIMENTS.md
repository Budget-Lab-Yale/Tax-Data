---
title: "Test 1 — diagnostic experiments (night of 2026-10-08)"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-08
sot: research/rebuild/tests/puf_generator/SPEC.md
supersedes: []
superseded_by: null
---

# Diagnostic experiments, night of 8 October 2026

## Summary (main session, 8 October, 23:50)

1. **Noise is larger than it looked.** Retraining the same base model
   with a different training seed moves the results as much as most of
   the changes tried: the share of weight misplaced in the age range × AGI
   table ranges over 2.9–3.8%, and the top 0.1% share of long-term gains
   over 29–41%, across three training runs (the data: 45.6%). One training
   run per variant cannot rank variants; three or more are needed.
2. **The model has learned the relationships; generation from nothing is
   where the error enters.** Given the true values of the other
   variables, the model's predictions are close to calibrated, within
   about sampling noise for most variables, and within noise for age
   given everything else. The error sits in a few low-information early
   steps of generation (the weight, AGI given age, wages given AGI) and
   compounds as the model's own draws feed later steps. The 2–3% excess
   in the population total comes entirely from the learned distribution
   of the weight.
3. **What did not fix it:**
   - a larger model;
   - a different generation order (demographics last fixes the "too
     retired" oldest group but moves the error to the youngest; age after
     AGI overshoots the other way);
   - redrawing each variable given all others;
   - training on the generation order (fixes the oldest group and the
     population total, but overfits and makes records hang together
     worse);
   - training without the high-income records that lack demographics
     (so John's missing-values hypothesis is not the cause);
   - weighting the loss toward the population.
4. **What this means for D22.** The planned first use is imputation:
   completing real records given their observed variables. That uses the
   high-information predictions, which are the ones found close to
   calibrated. Generating whole records from nothing, the hardest use, is
   what shows the problems. So these results are discouraging for
   generating whole files and encouraging for imputation, which test 2
   checks directly.
5. **Size.** The small model is clearly worse. Base and large are close:
   base slightly better on two-way tables, large better on the population
   total and the top wage share.

## The question

The transformer learns most of the PUF's joint structure but not all of it.
The persistent misses, the same at every model size (2.7, 8.6 and 33 million
parameters):

- **Age.** Age is sorted toward its typical income more sharply than in the
  data: too many of the youngest and oldest at low AGI, too few middle ages;
  the oldest group is too retired (27% with wages against 37%). Age range ×
  AGI band: about 4% of the population in the wrong cell against the
  training records, at every size.
- **The top.** The top 0.1% holds too little long-term capital gain (39–42%
  against 46%) and too much wage income.
- **The population total** comes out 2–3% high (the weight is a generated
  variable).
- AGI not adding up from its components is a known binning effect, not a
  question here.

Because size does not help, the cause is probably not capacity. This file
records experiments on what it is. Earlier findings are in `EVAL_REPORT.md`
and `STATUS.md`; the age diagnostics are `diag_age.py` and
`diag_age_agi.py`.

## Hypotheses

1. **Generation order and the training–generation mismatch.** Training
   spreads its effort over every pattern of hidden variables; generation
   uses only 40 specific ones, starting with almost everything hidden. Age
   is drawn eighth, before any income. Errors in the low-information
   conditionals, and errors compounding as earlier draws feed later ones,
   would hit early variables hardest. Tests: different orders; redrawing
   each variable given all others; training directly on the generation
   order (each step sees exactly the variables before it in the order).
2. **Miscalibrated conditionals.** If the model's predicted probabilities,
   given the true earlier variables, already show the distortion, the
   problem is in what was learned. If they do not, it arises during
   generation from the model's own earlier draws. Test: on validation
   records, average the model's predicted distribution of each variable
   given the true earlier variables and compare with the observed
   distribution.
3. **The oversampled top crowding out ordinary filers.** 42% of training
   records are high-income records without demographics (2% of the
   population). Test: train on records with demographics only.
4. **Unweighted training.** Each record counts equally in training,
   whatever its weight. Test: a loss weighted by a damped version of the
   record weight.

## Results

(Filled in by the agents below, one section each.)

## Results — diagnostics (hypothesis 2 and the noise, order and training-set batch)

*Written by the diagnostics agent, 8 October 22:20–00:30. Scripts:
`diag2_batch_table.py`, `diag2_ageagi.py`, `diag2_calib.py`,
`diag2_calib2.py`, `diag2_calib3.py`, `diag2_partial.py`, `diag2_bf16.py`
(with `diag2_*.sbatch`); aggregate outputs in `$S/diag2/`. TVD (total
variation distance) throughout is half the sum of absolute differences in
weighted shares: the share of weight that would have to move to make two
tables equal.*

**Summary.** The model's one-step conditionals — its predicted
distribution for each variable given the true values of the variables
before it in the generation order — are close to calibrated for most
variables: averaged over the validation records they reproduce each
variable's distribution, and its relationship with AGI, to within about
sampling noise, and the model's stated confidence matches how often it is
right to about one percentage point (wages: 3–5 points overconfident). The
exceptions sit early in the order, where little is visible: the weight's
own distribution (which by itself accounts for the 2–3% excess
population), AGI given age (twice its sampling noise), wages given AGI,
filing status for the large model, and wages, Social Security and pensions
for the oldest groups. Generated files are clearly further from the data
than the one-step conditionals, and fixing the early variables at their
true values removes much of the gap, so the error has two sources: about
two-thirds of the age × AGI misfit is the AGI-given-age step itself, and
the rest arises from generating age and the variables before it. 16-bit
arithmetic in generation is not a cause. On the batch: the small model is
worse than base and large on every joint measure by more than seed noise;
base and large are close (base slightly better on two- and three-way
tables, large better on total population and the wage share at the top).
Generating demographics last fixes the "too retired" oldest group (34.8%
with wages against 35.3% in training) and improves nonzero patterns, but
moves the age error to the youngest group and lowers the top 0.1% share of
long-term gains further; generating age after AGI and wages reverses the
oldest-group error (40.5% with wages) rather than removing it; two redraw
passes make most measures worse. Top 0.1% shares vary a lot between seeds
of the same model (long-term gains 0.36–0.41 across the four base files),
so single-file comparisons at the top are unreliable; every generated file
is below the data's 0.456.

### The noise, order and training-set batch (14 files)

Ranges are over the four seeds (09–12) of each model. "Seed-to-seed" is
the seed 10 file measured against the seed 09 file of the same model. The
floor is real against real (training records against test records). Pair
= two-way tables (median and 90th percentile of TVD over variable pairs);
three-way = mean TVD over nine three-way tables; patterns = TVD of the
nonzero-income patterns; AUC = how well a classifier separates generated
from real records (0.5 = cannot tell them apart).

Against the **test** records:

| | small | base | large | base, demographics last | base, two redraw passes | floor | seed-to-seed (s/b/l) |
|---|---|---|---|---|---|---|---|
| pair median | 0.0211–0.0220 | 0.0159–0.0169 | 0.0170–0.0175 | 0.0149 | 0.0174 | 0.0122 | 0.005 / 0.006 / 0.005 |
| pair 90th pct | 0.0337–0.0356 | 0.0289–0.0307 | 0.0318–0.0325 | 0.0293 | 0.0387 | 0.0224 | 0.010 / 0.011 / 0.010 |
| three-way mean | 0.0585–0.0605 | 0.0515–0.0530 | 0.0535–0.0550 | 0.0513 | 0.0550 | 0.0378 | 0.019 / 0.019 / 0.018 |
| patterns | 0.124–0.128 | 0.111–0.116 | 0.115–0.119 | **0.098** | 0.115 | 0.080 | 0.041 / 0.043 / 0.041 |
| classifier AUC | 0.554–0.570 | 0.545–0.556 | 0.545–0.552 | 0.562 | 0.560 | 0.505 | 0.50 / 0.50 / 0.50 |

Against the **training** records (same files; no real-versus-real floor
exists for this comparison):

| | small | base | large | base, demographics last | base, two redraw passes |
|---|---|---|---|---|---|
| pair median | 0.0213–0.0223 | 0.0141–0.0157 | 0.0160–0.0170 | 0.0139 | 0.0166 |
| pair 90th pct | 0.0304–0.0319 | 0.0232–0.0248 | 0.0250–0.0258 | 0.0246 | 0.0300 |
| three-way mean | 0.0449–0.0465 | 0.0385–0.0396 | 0.0380–0.0385 | **0.0360** | 0.0442 |
| patterns | 0.095–0.097 | 0.083–0.087 | 0.085–0.089 | **0.070** | 0.084 |
| classifier AUC | 0.631–0.637 | 0.618–0.621 | 0.616–0.624 | 0.613 | 0.613 |

(Distances to the training records are smaller than to the test records
mainly because the training set is eight times larger, so its own sampling
noise is smaller; the classifier AUC is higher for the same reason — more
records to learn from. These are not by themselves evidence of copying.)

Population and the top 0.1% (ranked by AGI; share of each income held):

| | small | base | large | demographics last | two redraws | training records | test records |
|---|---|---|---|---|---|---|---|
| total weight / training | 1.027–1.032 | 1.031–1.036 | **1.018–1.021** | 1.033 | 1.028 | 1 | — |
| AGI | 0.090–0.093 | 0.089–0.095 | 0.093–0.112 | 0.093 | 0.095 | 0.089 | 0.089 |
| wages | 0.038–0.041 | 0.041–0.044 | **0.038–0.039** | 0.044 | 0.048 | 0.037 | 0.037 |
| long-term gains | 0.416–0.429 | 0.362–0.413 | 0.396–0.438 | 0.348 | 0.443 | 0.456 | 0.457 |
| dividends | 0.172–0.197 | 0.190–0.204 | 0.193–0.217 | 0.215 | 0.224 | 0.230 | 0.226 |

**(i) Model size.** Small is worse than base and large on every joint
measure, by several times the seed range. Base and large are close: base
is slightly better on two-way tables (pair 90th percentile 0.029–0.031
against 0.032–0.033, ranges not overlapping) and against test on
three-way tables; against training the three-way tables tie. Large is
better on the total population (1.8–2.1% high against 3.1–3.6%) and on the
wage share of the top 0.1% (0.038–0.039 against 0.037 in the data; base
0.041–0.044), both beyond seed noise. On the top shares of AGI,
long-term gains and dividends the seed ranges of the three sizes overlap.

**(ii) Order variants.** Demographics last: better nonzero patterns
(0.098 against 0.111–0.116, beyond seed noise) and slightly better two-way
median; classifier AUC slightly worse (0.562 against 0.545–0.556); the top
0.1% long-term-gain share falls further (0.348, below every base seed).
Two redraw passes: two-way 90th percentile and three-way worse, wage share
at the top worse (0.048), long-term gains and dividends at the top better
(0.443, 0.224, the closest of any file). Age results are in the next table.

### Age range × AGI band, every generated file against the training records

Non-dependent filers only, as in `diag_age_agi.py`. "Wrong-cell share" is
the total variation distance (TVD): half the sum, over the 8 × 10 cells of
age range by AGI band, of the absolute difference in weight shares; it is
the share of weight that would have to move to make the two tables equal.
Every generated file has 166,156 records, the same as the training records.

| File | Wrong-cell share vs training | Age mix alone vs training | Oldest age range with wages |
|---|---|---|---|
| training records | 0 | 0 | 35.3% |
| test records (20,768) | 0.024 | 0.008 | 36.9% |
| small, seeds 09–12 | 0.036–0.038 | 0.014–0.017 | 32.0–32.7% |
| base, seeds 09–12 | 0.032–0.038 | 0.018–0.021 | 27.1–27.7% |
| large, seeds 09–12 | 0.037–0.040 | 0.012–0.016 | 32.0–33.2% |
| base, demographics generated last | 0.032 | 0.024 | 34.8% |
| base, two redraw passes | 0.044 | 0.037 | 26.2% |
| base, age after AGI and wages (Part C) | 0.034 | 0.024 | 40.5% |
| base, default order, 32-bit arithmetic | 0.035 | 0.020 | 28.0% |

Seed-to-seed yardstick: two files from the same model with different seeds
differ by 0.010–0.016 in the wrong-cell share (18 pairs, all three sizes),
and by 0.002–0.008 in the age mix alone.

Reading: the three model sizes are indistinguishable on this table (their
seed ranges overlap). Every model sits at roughly 0.036 against a seed noise
of about 0.013, so the age misfit is real and not a sampling accident.
Generating demographics last puts the file at the bottom of the base
model's seed range (0.032, not clearly better), but it nearly removes the
"too retired" problem for the oldest group (34.8% with wages against 35.3%
in training, versus 27% for base in the default order); the error moves
instead into the youngest group, which is now too common at AGI under
$25,000 (+0.7 and +0.6 percentage points of all weight in the two lowest
positive bands). Two redraw passes make things worse on every age measure.

### Hypothesis 2: one-step conditionals (teacher-forced) versus generated files

Method. On the 20,768 validation records, for each variable in the default
generation order, the model is shown the true values of the variables
before it and asked for the distribution of the variable (later variables
hidden). Averaging these predicted distributions over records, weighted by
record weight, gives what the model's one-step conditional implies for the
variable's distribution in validation; it is compared with the observed
distribution ("teacher-forced", meaning each step is fed the true earlier
values rather than the model's own draws). Amounts are compared on about 12
groups (negative, zero, ten positive groups of equal training count), so
that differences between neighbouring bins do not count. Yardstick for the
teacher-forced comparison: draw one value per record from the model's own
predicted distribution and compare that with the average — the gap a
perfectly calibrated model would show from sampling alone. Yardstick for
the generated files: the training records against validation (real versus
real, 166,156 against 20,768 records, the same sizes as a generated file
against validation).

Mean TVD over the 39 variables after the weight (coarse groups):

| | base | large | small |
|---|---|---|---|
| teacher-forced, against validation | 0.0059 | 0.0058 | — |
| sampling yardstick for that | 0.0033 | 0.0035 | — |
| generated files (mean of 4 seeds), against validation | 0.0073 | 0.0095 | 0.0114 (2 seeds) |
| training records against validation (yardstick) | 0.0048 | 0.0048 | 0.0048 |

Selected variables (coarse TVD against validation):

| Variable | Teacher-forced base / large | Sampling yardstick base / large | Generated base / large | Training records |
|---|---|---|---|---|
| MARS | 0.006 / **0.019** | 0.004 / 0.005 | 0.002 / **0.019** | 0.008 |
| AGERANGE | 0.014 / 0.010 | 0.011 / 0.009 | **0.019** / 0.011 | 0.004 |
| AGI | 0.010 / 0.016 | 0.008 / 0.005 | 0.014 / **0.024** | 0.010 |
| wages | 0.008 / 0.013 | 0.004 / 0.002 | **0.020 / 0.019** | 0.009 |
| pensions (E01500) | **0.016** / 0.010 | 0.005 / 0.009 | **0.015 / 0.022** | 0.009 |
| Social Security (E02400) | **0.013** / 0.010 | 0.003 / 0.008 | **0.013** / 0.016 | 0.009 |

Calibration of the top choice (given the true earlier variables): the
model's average probability for its most likely value against how often
that value is the true one. MARS 0.638 vs 0.631 (base), 0.641 vs 0.631
(large); XTOT 0.734 vs 0.723, 0.731 vs 0.723; AGERANGE 0.426 vs 0.415,
0.422 vs 0.414; AGI bin 0.043 vs 0.038, 0.043 vs 0.040; wages bin 0.465 vs
0.439 (base), 0.491 vs 0.439 (large). So the model is close to calibrated
on these variables, slightly overconfident everywhere (by about one
percentage point), and more clearly overconfident on wages (3 points for
base, 5 for large). Within bins of the predicted probability the picture is
the same; the largest gap is in the small group of records where the model
gives age 60–70% probability (base: right 58% of the time; large: 48%;
about 3% of weight).

**Each variable jointly with AGI band.** For the 29 variables after AGI in
the default order, the table (variable in ~12 groups) × (10 AGI bands),
against validation; teacher-forced tables place each record's predicted
distribution in its true AGI band. Mean TVD over the 29 variables:

| | base | large |
|---|---|---|
| teacher-forced | 0.0101 | 0.0101 |
| sampling yardstick for that | 0.0085 | 0.0084 |
| generated files (mean of 4 seeds) | 0.0204 | 0.0277 |
| training records against validation (yardstick) | 0.0162 | 0.0162 |
| base, demographics last / two redraw passes | 0.0216 / 0.0242 | |

Given the true earlier values, the conditionals reproduce each variable's
relationship with AGI almost to sampling noise. The exception is wages,
which come straight after AGI: 0.025 (base) and 0.029 (large) against a
yardstick of 0.012. Generated files are further from validation than real
records are, modestly for base and clearly for large.

**Age range × AGI band** (all records, against validation; TVD):

| | base | large |
|---|---|---|
| AGI given the true age range and the variables before it (teacher-forced) | 0.028 | 0.027 |
| sampling yardstick for that | 0.014 | 0.016 |
| age range given *all* other variables, including AGI (teacher-forced, non-dependent filers) | 0.020 | 0.020 |
| sampling yardstick for that | 0.017 | 0.019 |
| generated files, seeds 09–12 | 0.042–0.045 | 0.042–0.045 |
| base, demographics last / two redraw passes | 0.036 / 0.053 | |
| training records against validation (yardstick) | 0.024 | 0.024 |

The age range's own distribution, given the variables before it, is close
to calibrated (coarse TVD 0.014 against a yardstick of 0.011 for base;
0.010 against 0.009 for large). The step after it — AGI given age — is not:
about twice its sampling yardstick. The generated files are about twice the
real-versus-real gap. When the model sees everything else, its age
predictions within AGI bands are close to calibrated, so the relationship
has been learned; it is the low-information conditionals used in
generation that carry the error.

**Income by age range** (non-dependent filers, validation, % of the age
range's weight). Teacher-forced = model's probability given the true age
and the true variables before each income variable.

| Age range | | with wages | with Social Security | with pensions | AGI under $25k |
|---|---|---|---|---|---|
| oldest (6) | observed | 35.3 | 91.5 | 71.3 | 34.1 |
| | teacher-forced base / large | 31.6 / 32.6 | 89.4 / 90.8 | 71.1 / 72.8 | 36.7 / 36.7 |
| | generated base / large (4 seeds) | 27.4 / 32.4 | 91.6 / 91.0 | 75.4 / 72.0 | 39.7 / 41.5 |
| | generated base, demographics last | 34.8 | 91.0 | 69.7 | 36.7 |
| second oldest (5) | observed | 83.0 | 19.3 | 29.6 | 23.8 |
| | teacher-forced base / large | 83.7 / 84.0 | 15.5 / 17.2 | 27.8 / 29.8 | 22.1 / 23.5 |
| | generated base / large | 85.6 / 85.6 | 14.9 / 15.3 | 24.6 / 23.9 | 22.1 / 23.7 |
| second youngest (2) | observed | 94.6 | 1.1 | 6.2 | 37.0 |
| | teacher-forced base / large | 95.0 / 94.4 | 0.4 / 0.4 | 4.5 / 4.1 | 36.6 / 38.0 |
| | generated base / large | 95.0 / 94.1 | 0.4 / 0.4 | 4.0 / 3.5 | 41.6 / 41.2 |

The "too retired" oldest group is partly in the one-step conditionals
(wages 3–4 points too rare and low AGI 2.6 points too common, given the
true age) and partly added in generation (base drops a further 4 points on
wages). Social Security and pensions in the two oldest groups are too rare
already at one step (Social Security in the second-oldest group: 15.5%
against 19.3% for base). Sampling noise on these shares is roughly one percentage point.

**The population total.** The weight is the first variable, drawn with
nothing visible, so the model's distribution of it is a single learned
distribution. Its implied total population (that distribution, times the
average weight at each weight value, times the record count) is 2.95% above
the training total for base and 1.83% for large; the generated files come
out 3.1–3.6% (base) and 1.8–2.1% (large) high. So the population excess is
already in the first step: the model puts slightly too much probability on
the high-weight (ordinary filer) values and too little on the low-weight
(high-income, oversampled) values.

**Where along the order the error builds up** (`diag2_partial.py`). Start
from the validation records with the first k variables of the default
order set to their true values and let the model draw the rest exactly as
in generation (each record continued four times). k = 0 is ordinary
generation. Against validation:

| True values given for | Age × AGI TVD base / large | Oldest with wages base / large | Oldest with AGI < $25k base / large | Each variable × AGI band, mean TVD base / large |
|---|---|---|---|---|
| nothing (ordinary generation) | 0.045 / 0.041 | 27.9 / 32.6 | 40.5 / 41.4 | 0.023 / 0.028 |
| weight | 0.041 / 0.041 | 27.2 / 32.2 | 39.2 / 40.7 | 0.020 / 0.026 |
| weight and filing variables (7) | 0.042 / 0.035 | 27.4 / 32.7 | 38.6 / 38.5 | 0.019 / 0.020 |
| … and age range (8) | 0.031 / 0.029 | 28.8 / 33.6 | 36.9 / 36.4 | 0.019 / 0.019 |
| … and gender, earnings split (10) | 0.031 / 0.029 | 31.9 / 33.2 | 36.9 / 37.0 | 0.020 / 0.020 |
| … and AGI (11) | 0 (both true) | 31.5 / 32.0 | 34.1 (true) | 0.012 / 0.013 |
| … and wages (12) | 0 | 35.3 (true) | 34.1 | 0.011 / 0.012 |
| observed | 0 | 35.3 | 34.1 | 0 |

Sampling yardstick for the age × AGI column, roughly 0.015 (see above).
Reading: about two-thirds of the age × AGI error (0.031 of 0.045 for base)
is already there when age and everything before it are true — it is the
AGI-given-age step. The rest comes from drawing age itself. For the oldest
group's wages (base), the one-step wage conditional given true AGI and
everything before loses 4 points (31.5% against 35.3%), a generated
earnings split loses about 3 more, and generated age and earlier variables
about 1 more. The large model loses less at each stage. The joint of each
later variable with AGI roughly doubles (0.011 → 0.023 for base, 0.012 →
0.028 for large) as more of the record is generated; for large most of
that increase appears when the weight and filing variables are generated
rather than given.

**Arithmetic precision is not the cause.** `generate.py` runs the model in
bfloat16, a 16-bit number format with about three significant digits; the
teacher-forced checks above ran in full 32-bit precision. One-step
predictions differ between the two by at most 0.4% of probability (AGI;
under 0.1% for most variables). A full base-model file generated in 32-bit
precision with the original seed (`diag2_bf16.py`) lands inside the
bfloat16 seed range on every measure checked: total weight 3.5% high,
age × AGI TVD against validation 0.041, oldest age range with wages 28.0%,
mean coarse TVD 0.0070.

### Part C: age placed after AGI and wages

Same model, order changed to weight, filing variables, AGI, wages, then
age range, gender, earnings split, then the rest. Teacher-forced on
validation:

| | base | large |
|---|---|---|
| age range's own distribution (TVD), default order | 0.014 (yardstick 0.011) | 0.010 (0.009) |
| same, age after AGI and wages | **0.024** (0.005) | **0.016** (0.007) |
| age range × AGI band (non-dependent filers), age given earlier variables incl. AGI and wages | 0.033 | 0.029 |
| (compare: age given all other variables; sampling yardstick 0.017–0.019) | 0.020 | 0.020 |

Moving age after AGI and wages does not give a better-calibrated age
conditional: given weight, filing variables, AGI and wages, the model puts
too much weight on the oldest group (17.3% against 15.7% for base) and its
age-by-AGI table is further off than when it sees all variables. This is
another low-information conditional (9 of 40 variables visible) that is
less accurate than the nearly full conditionals.

A base-model file generated in this order (seed 20261009,
`$S/diag2/synthetic_base_ageafter.parquet`), against training,
non-dependent filers: age × AGI wrong-cell share 0.034 (base seeds
0.032–0.038, so no better); age mix alone 0.024 (base seeds 0.018–0.021,
worse); oldest age range with wages 40.5% against 35.3% in training — the
"too retired" error is reversed into "too few retired", 5 points the other
way. Because wages are now drawn before age, age is drawn given wages, and
the conditional over-assigns wage earners to the oldest group. Headline
measures against test (labels `base_ageafter`, `base_ageafter_vstrain` in
`$S/eval/`): pair median 0.0149 and patterns 0.102 (both better than the
base seeds, 0.0159–0.0169 and 0.111–0.116), pair 90th percentile 0.0319
and three-way 0.0551 (both worse than 0.029–0.031 and 0.052–0.053), AUC
0.555, total weight 3.6% high, top 0.1% shares of AGI / wages / long-term
gains / dividends 0.091 / 0.042 / 0.384 / 0.192 (all inside the base seed
ranges). Like demographics-last, a reordering moves the error around
rather than removing it.

### Not finished

- Teacher-forced checks for the small model, and for the order variants
  as whole procedures (only the age-after-AGI order was checked one step
  at a time).
- A sampling yardstick for the partial-generation table computed exactly
  (the 0.015 quoted is borrowed from the one-step tables).
- Whether the oldest group's one-step wage error comes from too little
  training data in that cell or from the unweighted loss; the training
  experiments in the next section bear on this.
- The top 0.1% shares were not examined with teacher forcing; with seed
  ranges of 4–8 points on long-term gains, a reliable comparison of the top
  needs several files per variant or a larger generated file.

## Results — training experiments (hypotheses 1, 3, 4)

*Written 8 October, 22:35 to 23:45. All runs listed finished and are
reported.*

**Summary.** None of the three training changes fixes the misses, and the
most important finding is about noise: three training runs of the
unchanged base model differ by about a point on age × AGI (2.9–3.8%), by
three points on the oldest group's share with wages (27–30%) and by ten
points on the top 0.1% share of long-term gains (29–41%), as much as most
variants differ from base. Measured against that, training directly on
the generation order (hypothesis 1) does one thing reliably: the oldest
group's share with wages rises to 34–37% (training records 35%) in three
runs, and the population total comes out 100–101% instead of 102–104%.
Age × AGI does not improve beyond the noise. The price is high: the model
overfits within about 20 epochs, is worse even at the conditionals it was
trained on, and its records hang together much less well (classifier AUC
0.75–0.77 against 0.62; AGI within 2% of its components for 11–13% of
records against 35–37%). Half prefix, half random masks is roughly
neutral. Training only on records with demographics (hypothesis 3) gets
the population total of those records right (100% against 103%) but its
age relationships are no better than a retrained base model's. Weighting
the loss by the square root of the record weight (hypothesis 4) makes the
model learn a tilted population; once that is undone the total is right
but the top 0.1% share of long-term gains falls to 25% and age is no
better. Comparisons of training changes need several training seeds per
variant.

### What was run

All runs use the base size (6 layers, width 256, 8.6 million parameters)
unless noted, training seed 20261008 unless noted, the same training and
validation records, the
same settings as the existing base model otherwise, and training capped at
25 minutes of wall time (`--max-hours 0.42`). Each run generates 166,156
records with seed 20261009 in the default order (unless noted) and is
compared with the training records. Outputs: `$S/exp/<name>/`
(checkpoint, per-epoch log, `run_<name>.json`, `synthetic.parquet`);
evaluations in `$S/exp/eval/<label>/`; age diagnostics in `$S/exp/diag/`.

| Name | Hypothesis | What changes |
|---|---|---|
| `prefixv` | 1 | Every training mask follows the generation order: a cut point k is drawn uniformly from 0..39, the first k variables in the order are visible and the rest hidden. Validation masks are drawn the same way, so early stopping tracks what is trained. |
| `mix`, `mixv` | 1 | Half the masks as in `prefixv`, half the original random masks. `mix` stops on the original (random-mask) validation loss, `mixv` on half/half validation masks. |
| `prefix` | 1 | As `prefixv`, but early stopping on the original random-mask validation loss. Kept as a record of why that is the wrong stopping rule (below). |
| `prefixv_ageafter` | 1 (extra) | As `prefixv`, with the order changed so age range comes right after AGI and wages (training and generation). |
| `prefixv_large` | 1 (extra) | As `prefixv`, large size (8 layers, width 512). |
| `demog` | 3 | Trained only on records with the age range present (95,741 of 166,156 training records; 11,967 of 20,768 validation records). Generates 95,741 records. |
| `wdamp` | 4 | Each record's loss counts in proportion to the square root of its weight, rescaled to average 1. Loss weights run from 0.05 to 1.9 (median 0.83); the effective number of records is 66% of the actual number. |
| `prefixv_drop3` | 1 (extra) | As `prefixv`, dropout 0.3 instead of 0.1, to test whether the early overfitting is the problem. |
| `base_tseed2`, `base_tseed3` | yardstick | The original base model's settings exactly, training seeds 20261013 and 20261014: how much a retrained model moves on its own. |
| `mixv_tseed2`, `prefixv_tseed2`, `demog_tseed2` | yardstick | `mixv`, `prefixv`, `demog` with training seed 20261013. |

A first `prefix_ageafter` and `prefix_large` run, stopped on random-mask
validation loss, were cancelled after 20 and 10 epochs once it was clear
that rule stopped them too early: a model trained only on prefix masks
gets worse at random masks while still getting better at the ones it uses.

### Code changes

`train.py` gained four options; with their defaults it behaves exactly as
before (the existing `--order-frac` option, default 0, does the prefix
masking):

- `--train-order a,b,...` — the order used by prefix masks and by the
  sequential log-likelihood (default: `meta.json`'s order).
- `--drop-token VAR=TOKEN` — drop training and validation records with
  that code (default: none). `AGERANGE=0` is "not in demographic file".
- `--loss-weight-power p` — weight each record's loss by weight^p,
  rescaled to mean 1 (default 0: unweighted). Validation loss stays
  unweighted.
- `--val-mask-mode random|train` — `train` draws validation masks like the
  training masks (default `random`, the original).

`model.py` and `generate.py` are unchanged. New scripts: `exp_run.sbatch`
(train then generate), `exp_eval.sbatch` (`evaluate.py` into
`$S/exp/eval/`), `exp_diag.py` / `exp_diag.sbatch` (age × AGI and the
other key figures, adapted from `diag_age_agi.py`), `exp_filter_demog.py`
/ `.sbatch` (copies restricted to records with demographics),
`exp_seqnll.py` / `.sbatch` (sequential log-likelihood of a checkpoint
under a chosen order or subset), `exp_reweight.py` / `.sbatch` (undo the
damped-weight tilt in a generated file).

### The yardstick: the existing base model, four generation seeds, against training records

Definitions (all weighted, against the training records): "age × AGI
misplaced" is the share of weight that would have to move to make the age
range × AGI band table among non-dependent filers match (10 AGI bands;
rows include "not in demographic file"). "Oldest with wages" is the share
of non-dependent filers in age range 6 with wages; 35.3% in the training
records (an earlier note gave 37%; the definition there may differ). Top 0.1% shares
rank records by AGI and take the positive part of the variable, as in
`evaluate.py`.

| File | Age × AGI misplaced | Oldest with wages | Top 0.1% share of long-term gains | Top 0.1% share of wages | Total weight vs training |
|---|---|---|---|---|---|
| Training records | — | 35.3% | 45.6% | 3.68% | 100% |
| base, seeds 09 / 10 / 11 / 12 | 3.8 / 3.6 / 3.2 / 3.7% | 27.4 / 27.6 / 27.7 / 27.1% | 38.9 / 36.2 / 39.2 / 41.3% | 4.33 / 4.10 / 4.25 / 4.40% | 103.3 / 103.6 / 103.4 / 103.1% |
| large, seed 09 | 4.0% | 32.1% | 39.6% | 3.87% | 101.8% |

Seed-to-seed spread for base: about 0.6 points on age × AGI, under 1 point
on oldest-with-wages, 5 points on the top long-term-gains share, 0.3 points
on the top wage share, 0.5 points on total weight.

### Results

**Sequential log-likelihood** is minus the log probability of a validation
record under the generation procedure (the 40 conditionals in order),
averaged over validation records, in nats; lower is better. It is the only
loss comparable across masking schemes. The masked validation loss is not:
it averages over whichever masks a run validates on (random masks for the
original, prefix masks for `prefixv`), so it is not shown. The original
base model gives 50.60 under the default order and 50.61 under the
age-after-AGI order (it was trained on all orders, so the order barely
matters to it); the large model 50.57.

**"AGI adds up"** is the share of records whose AGI is within 2% of the sum
of its components, from `evaluate.py`: 80.5% for the training records
themselves, 61.3% for training records passed through the binning (the
best a model working on bins can do).

**The noise yardstick has two parts.** Regenerating from the same model
with another seed moves age × AGI by about 0.6 points and the top 0.1%
long-term-gains share by 5 points. Retraining the same base model with
other training seeds (`base_tseed2`, `base_tseed3`, everything else
identical) moves things more: age × AGI 2.9% in both against 3.2–3.8%,
oldest with wages 29.6% and 28.6% against 27.1–27.7%, top 0.1% share of
long-term gains 31.1% and 29.0% against 36–41%, total weight 103.1% and
102.4%. Sequential log-likelihood is stable (50.60, 50.67, 50.62). So
across three base training runs age × AGI spans 2.9–3.8%, oldest with
wages 27–30%, the top long-term-gains share 29–41%, and the median
two-way TVD 0.014–0.017 (classifier AUC is stable at 0.62); a variant has
to fall outside those ranges before it means anything.

All rows: one training run each, generation seed 20261009, against the
training records.

| Run | Best epoch (stopped) | Sequential log-lik. | Age × AGI misplaced | Oldest with wages | Top 0.1%: long-term gains | Top 0.1%: wages | Total weight | AGI adds up | Two-way median TVD | Three-way mean TVD | Classifier AUC |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Training records | | | — | 35.3% | 45.6% | 3.68% | 100% | 80.5% | — | — | — |
| base, generation seeds 09–12 | 70 (85) | 50.60 | 3.2–3.8% | 27.1–27.7% | 36.2–41.3% | 4.10–4.40% | 103.1–103.6% | 34.8–35.2% | 0.0141–0.0150 | 0.0385–0.0393 | 0.618–0.620 |
| `base_tseed2` (base retrained, training seed 20261013) | 69 (84) | 50.67 | 2.9% | 29.6% | 31.1% | 3.50% | 103.1% | 37.1% | 0.0135 | 0.0376 | 0.620 |
| `base_tseed3` (base retrained, training seed 20261014) | 68 (83) | 50.62 | 2.9% | 28.6% | 29.0% | 3.61% | 102.4% | 37.5% | 0.0172 | 0.0373 | 0.621 |
| large, seed 09 | 55 (70) | 50.57 | 4.0% | 32.1% | 39.6% | 3.87% | 101.8% | 37.5% | 0.0162 | 0.0385 | 0.620 |
| `prefixv` | 19 (34) | 52.13 | 2.8% | 36.2% | 34.0% | 3.16% | 101.4% | 12.2% | 0.0143 | 0.0366 | 0.764 |
| `prefixv_tseed2` (training seed 20261013) | 18 (33) | 52.11 | 3.6% | 33.5% | 33.1% | 3.28% | 100.3% | 10.6% | 0.0157 | 0.0386 | 0.770 |
| `prefix` (stopped on random-mask loss) | 30 (45) | 51.92 | 2.9% | 33.4% | 35.1% | 3.38% | 100.5% | 22.2% | 0.0175 | 0.0347 | 0.714 |
| `prefixv_large` | 19 (34) | 51.89 | 3.1% | 36.6% | 35.8% | 3.42% | 101.3% | 13.0% | 0.0129 | 0.0378 | 0.748 |
| `prefixv_drop3` (dropout 0.3) | 25 (40) | 52.56 | 4.0% | 31.8% | 28.8% | 3.67% | 102.5% | 8.6% | 0.0241 | 0.0472 | 0.812 |
| `prefixv_ageafter` | 19 (34) | 52.11 (own order) | 3.7% | 49.0% | 42.1% | 3.61% | 101.5% | 12.8% | 0.0164 | 0.0435 | 0.774 |
| `mix` | 51 (66) | 50.81 | 2.9% | 31.9% | 28.9% | 3.60% | 101.6% | 39.7% | 0.0172 | 0.0345 | 0.615 |
| `mixv` | 42 (57) | 50.80 | 2.9% | 33.0% | 39.1% | 3.49% | 103.4% | 37.5% | 0.0167 | 0.0323 | 0.631 |
| `mixv_tseed2` (training seed 20261013) | 43 (58) | 50.77 | 3.6% | 30.7% | 38.3% | 3.78% | 102.9% | 37.6% | 0.0209 | 0.0377 | 0.631 |
| `wdamp`, as generated | 80 (95) | 51.43 (not comparable, see below) | 5.4% | 29.4% | 5.8% | 1.32% | 167.4% | 40.5% | 0.0291 | 0.0525 | 0.744 |
| `wdamp`, weights corrected | same model | | 3.5% | 30.3% | 24.7% | 3.81% | 100.7% | 39.6% | 0.0160 | 0.0359 | 1.0 (artefact, see below) |

Records with demographics only (hypothesis 3). Both the model's records
and the training records are restricted to those with the age range
present; the base model's files are restricted the same way. This model
never sees the top strata, so top shares, total weight against the full
file, and anything about records without demographics are left out.
Total weight is against the training records with demographics. The
"age × something" columns are the share of weight misplaced in that
two-way table among non-dependent filers. The base model's sequential
log-likelihood here is computed on the same 11,967 validation records with
demographics.

| Run | Sequential log-lik. | Age × AGI | Age marginal | Age × has wages | Age × has Social Security | Age × has pension | Age × filing status | Oldest with wages | Total weight | Two-way median TVD | Classifier AUC |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Training records | — | — | — | — | — | — | — | 35.3% | 100% | — | — |
| base, generation seeds 09–12 | 34.66 | 3.2–3.8% | 1.7–2.1% | 2.9–3.4% | 2.0–2.5% | 2.9–3.3% | 3.2–3.5% | 27.1–27.7% | 103.2–103.8% | 0.0138–0.0148 (2 seeds) | 0.628–0.631 (2 seeds) |
| `demog` | 34.36 | 3.3% | 1.1% | 2.2% | 1.3% | 2.5% | 3.2% | 29.4% | 100.3% | 0.0133 | 0.634 |
| `demog_tseed2` (training seed 20261013) | 34.39 | 2.9% | 1.1% | 2.3% | 1.7% | 2.7% | 3.2% | 28.4% | 100.3% | 0.0124 | 0.639 |
| `base_tseed2` (base retrained, restricted to records with demographics) | — | 2.9% | 0.9% | 2.0% | 1.4% | 2.7% | 2.8% | 29.6% | 103.2% | — | — |

### What the results say

- **Training on the generation order (hypothesis 1) moves one age
  relationship in the right direction but makes the model worse overall,
  because it overfits.** With prefix masks the oldest group's share with
  wages is 36.2%, 33.5% (second training seed) and 36.6% (large size),
  against 35.3% in the training records and 27–30% for three base
  training runs; and the population total comes out 100.3–101.4% against
  102.4–103.6%. Age × AGI misplacement (2.8%, 3.6%, 3.1%) is inside the
  base range (2.9–3.8%), so that does not improve. The cost is large: the
  model stops improving on validation after about 19 epochs while its
  training loss keeps falling (with only 40 possible masks per record it
  fits the training records quickly; random masks act as a regulariser),
  and more dropout does not help (0.3 is worse on every measure). It ends
  up worse at the very conditionals it was trained for (sequential
  log-likelihood 52.1 in both training seeds against 50.6–50.7 for base).
  Records hang together much less well: only 11–13% have AGI within 2% of
  their components (base 35–37%), and the classifier tells them from real
  ones far more easily (AUC 0.75–0.77 against 0.62). The top 0.1% share of
  long-term gains (33–36%) is at the low end of the base range, and the
  top wage share (3.2–3.4%) is now slightly too low.
- **Half prefix, half random masks (`mix`, `mixv`)** keep the regularising
  effect: sequential log-likelihood 50.8 (0.2 worse than base), AGI adds
  up as often as base (38–40%), classifier AUC 0.615–0.631, three-way
  tables about as good as base (0.032–0.038 against 0.037–0.039), two-way
  tables a little worse (0.017–0.021 against 0.014–0.017 across the three
  base training runs). Oldest with wages
  32–33% in the first run, but a second training seed of `mixv` gives
  age × AGI 3.6% and oldest with wages 30.7%, back inside the range of
  the base model's three training runs. Nothing about the top improves.
  Half-and-half masking is roughly neutral, not a fix.
- **Age drawn after AGI and wages, with prefix training
  (`prefixv_ageafter`)**, overshoots the other way: 49% of the oldest group
  have wages, against 35% in the training records. Drawn after wages, age
  is set largely by the wage draw, and the prefix-trained conditional for
  age given wages appears too sharp; not investigated further.
- **Records with demographics only (hypothesis 3)** gets the population
  total of those records right (100.3% in both training seeds, against
  103.2–103.8% for base restricted the same way), and its sequential
  log-likelihood on those records is 0.3 nats better (34.36 and 34.39
  against 34.66). Its age relationships looked better than the original
  base model's in the first run, but a retrained base model restricted to
  the same records does as well (age × AGI 2.9%, age × wages 2.0%, age ×
  Social Security 1.4%, against 2.9–3.3%, 2.2–2.3% and 1.3–1.7% for the
  two `demog` runs), and the oldest group stays too retired (28–29% with
  wages against 35%). The oversampled top crowding out ordinary filers is
  not what distorts age; it may be part of why the total comes out 3%
  high.
- **Damped weighting (hypothesis 4) does not help.** A model trained with
  each record counting in proportion to the square root of its weight
  learns a tilted population: it generates too many high-weight (ordinary)
  records and too few top records, so as generated its total weight is
  167% and the top shares collapse. That is expected, not a bug. Undoing
  the tilt (each generated record's weight divided by its loss weight:
  new weight = weight × mean(w^0.5) / w^0.5, mean over training records)
  gives the right total (100.7%) and a better top wage share (3.81%), but
  the top 0.1% share of long-term gains falls to 25%, probably because the
  tilted model generates fewer top records, each carrying a larger
  corrected weight (not checked); age × AGI 3.5% and oldest with wages
  30% are no better than base. Its sequential log-likelihood is not
  comparable (it is fitted to the tilted population and scored on the
  untilted one). The corrected weights are no longer on the PUF's grid of
  73 weight values, so the classifier separates the files on the weight
  alone (AUC 1.0); snapping them to the nearest grid value moved the total
  to 123%, so no fair classifier test was possible tonight.
- **The 2–4% excess in the population total** appears in all three base
  training runs (102.4–103.6%) and in the large model (101.8%).
  Prefix-trained models come out at 100.3–102.5%, the half-and-half runs
  at 101.6–103.4%, and the demographics-only runs at 100.3% of their own
  population. Prefix training and dropping the high-income records both
  seem to remove most of it; why is not established.

### Not finished or not done

- Only one or two training seeds per variant, and three for base. The
  ranges quoted are ranges over those few runs, not confidence intervals.
- No fair classifier test for the weight-corrected damped-weight model
  (see above). A version of `evaluate.py`'s classifier without the weight
  variable would settle it.
- Prefix training was tried with the original learning-rate schedule and
  dropout 0.1 or 0.3 only. Stronger regularisation (weight decay, smaller
  model, more data per step) was not tried.
- All runs were capped at 25 minutes of training; none hit the cap. Every
  run stopped on its own, 15 epochs after its best validation loss (the
  longest took 18 minutes).
- Runs went to whichever GPU was free (B200, L40S, RTX A5000, RTX 5000
  Ada); this changes speed, not results, apart from floating-point
  differences.

