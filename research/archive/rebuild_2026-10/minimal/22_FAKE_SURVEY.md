---
title: "Step 22: the fake-survey test of decision 18 (translation learned in pretraining)"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-16
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 22: the fake-survey test

Test 1 of `design/translation_by_latent_amounts.md`, section 8, asked for
by John on 16 October ("run the whole thing").

## What it is

A survey made out of the PUF, so the right answer is known.

1. The 2015 PUF records are split at random into two piles of different
   returns. Pile A plays the PUF. Pile B (30,000 records from the other
   half) plays the survey.
2. The survey keeps other income, which the PUF pile does not have. Other
   income plays the part net worth plays in the real setup: a variable
   only the survey has, which the model must hand to PUF returns.
3. The survey's interest and dividends are broken on purpose (below). Its
   wages, capital gains, marriage, dependents and other income stay true.
4. Each method trains the model, then gives other income to the PUF pile.
   The PUF pile's true other income was never shown to the model, so each
   method can be scored against it.

The setup is otherwise the real one: one stacked table, PUF and survey
rows, the same 84 slices, the same transformer and training code
(`02_model.py`). There is one year (2015) and no net worth column.

## The distortions

A holder of x dollars reports a nonzero amount with chance
1 / (1 + exp(-1.05 (ln x - ln 150))): 13% at $50, 50% at $150, 95% at
$2,500. On the built tables this leaves 12.5% of survey units reporting
interest against 28.3% who hold it (the real SCF: about 12% against the
PUF's 28%), and 13.1% reporting dividends against 18.3%.

| | What is broken |
|---|---|
| D1 | Small holders report zero, by the curve above; reporters' dollars are right |
| D2 | D1, and reporters give 0.8 of their true dollars |
| D3 | D1, but households with other income below the median report less and those above more (log-odds shifted by 0.75 each way): the hidden holders are poorer than the reporters, which the data cannot show |
| D4 | D1, but one draw decides both items: a household that omits interest tends to omit dividends too |

## The methods

| | What it does |
|---|---|
| Undistorted survey | Today's model on the survey before it was broken: the best this sample size allows |
| Today's rule | Decision 15: reported dollars placed on the PUF's dollar scale, source label on every slot |
| Ranks | Each file on its own percentiles (the old rule) |
| Fill in once before training | A separate simple model (PUF shares by wage group and marriage, times the planted chance of reporting zero) fills in each survey zero once; reporters' dollars divided by the planted factor; then today's model |
| No label, no fill-in | The model with no source label, survey zeros taken as zeros |
| Latent, settings fixed | Decision 18 with the reporting settings fixed at the planted ones: no source label; every pass, each survey zero gets a true amount drawn from the model's belief given the rest of the record times the chance of reporting zero; everything trains on the drawn amounts |
| Latent, settings learned | The same with the S-curve's two settings learned by gradient steps and the dollar factor re-estimated after each pass |

## Code and where things are

- `22_fake_survey.py build` makes the tables (scratch
  `data/stacked_F22_<D>_<dollar|rank>.parquet`) and `fake22/aux_<D>.json`.
- `22_run.sbatch <D> <seed> <arms...>` trains and scores; each arm writes
  `fake22/<arm>_<D>_s<seed>/score.json`. Finished arms are skipped, so a
  resubmission resumes.
- `22_fake_survey.py report` (`22_report.py`) writes `fake22/report.md`.

## Results

Two seeds per method (one seed = one training run). The undistorted
survey is one pair of runs shared by all four distortions. Full tables:
scratch `fake22/report.md`.

**How to read the numbers.** The measure is the mean other income of PUF
returns in a group, with every amount held between the 1st and 99th
percentiles of the survey's other income (-$32,440 to $180,090; a
handful of very large amounts otherwise swing the means). Shown
as the percentage gap from the truth. Even the undistorted survey is off:
+6% over all returns and +19% for returns with no interest. That part is
the model's own error at this sample size, not translation, so **the
comparison is each method against the undistorted survey.** Runs with
different seeds differ by about 5 to 10 points on the no-interest group.

### Returns with no interest (54 million of 76 million): mean other income, gap from the truth ($5,551)

| | D1 | D2 | D3 | D4 |
|---|---:|---:|---:|---:|
| Undistorted survey | +19% | +19% | +19% | +19% |
| Today's rule | +37% | +38% | +31% | +36% |
| Ranks | +40% | +39% | +34% | +41% |
| Fill in once before training | +30% | +29% | +25% | +29% |
| No label, no fill-in | +34% | +35% | +28% | +35% |
| Latent, settings fixed | +14% | +16% | +12% | +16% |
| Latent, settings learned | +17% | +18% | +15% | +19% |

Seed by seed (D1): undistorted +21 and +17; today's rule +39 and +35;
latent with settings fixed +19 and +10.

### What the runs show

1. **Today's rule overstates other income for returns with no interest by
   about 18 points beyond the model's own error,** under every distortion.
   That is the hidden-holder error the design note predicted: the survey's
   zero group mixes true non-holders with holders who did not report, and
   the PUF's zeros inherit the mixture.
2. **The latent method with the true reporting settings removes it.** Its
   gap is within seed noise of the undistorted survey's under D1, D2 and
   D4. It finds the right number of hidden holders (interest: 17.8 to
   19.9% of survey zeros, truth 18.1%; dividends 5.7 to 6.5%, truth 6.1%)
   and ranks which households they are well (0.83 to 0.85 on a scale where
   0.5 is a coin flip and 1 is perfect).
3. **Filling in once before training gets about half way.** Its fill-in
   model knows only wages and marriage, not other income, so the hidden
   holders it creates carry no link to other income (the congeniality
   point of the design note, section 6). It also gives too few returns
   with small interest any other income (87.7% against 97.5% under D1).
4. **Removing the source label alone does nothing** (+34%, like today's
   rule): the label is not the problem; the zeros are.
5. **D3, the case the data cannot check** (hidden holders poorer than
   reporters with the same interest): the latent method lands at +12%,
   about 7 points below the undistorted survey, the over-correction the
   design note predicted. Today's rule is at +31%. So with this planted
   gap (a shift of 0.75 on the log-odds of reporting by other income) the
   unverifiable assumption costs about 7 points, against 12 to 18 points
   for not modelling the zeros at all.
6. **Learning the settings costs little.** The learned method lands 2 to
   3 points above the fixed one under every distortion, within seed noise
   of the undistorted survey. Learned settings, two seeds each:

   | | Planted | D1 | D2 | D3 | D4 |
   |---|---:|---:|---:|---:|---:|
   | Interest dollar factor | 1.00 (D2: 0.80) | 0.97, 1.00 | 0.80, 0.80 | 0.93, 1.00 | 0.97, 0.97 |
   | Dividends dollar factor | 1.00 (D2: 0.80) | 0.95, 0.95 | 0.80, 0.76 | 0.98, 1.04 | 0.97, 0.98 |
   | Interest: reporting chance at $50 | 13% | 30%, 29% | 29%, 28% | 35%, 33% | 29%, 29% |
   | Interest: at $500 | 78% | 75%, 77% | 76%, 80% | 87%, 91% | 73%, 75% |
   | Interest: hidden holders among zeros (truth) | | 18.0, 17.0 (18.1) | 18.6, 17.3 (18.1) | 16.5, 15.7 (16.5) | 18.1, 17.2 (18.2) |

   The dollar factor is recovered within about 5%. The curve is too
   generous at small amounts (about 30% at $50 against 13%) but right
   from $500 up, and the count of hidden holders is close, because most
   hidden holders are small and the PUF pins how many there are.
7. **The overall log score barely moves** (2.87 to 2.90 across methods).
   The error is a shift in the mean for one large group, which a score
   averaged over the whole distribution hardly sees. The fill-in-first
   method scores worst (2.90).

### Learning the settings: what it took

The learned method needed five changes before it recovered the planted
settings. Each failure was a loop between the settings and the
fill-ins, and each points at a rule for the real design:

1. Fitting the curve to the filled-in amounts and the factor to mean log
   dollars: the two locked into a wrong pair (interest factor 2.4, a flat
   curve).
2. Fitting the curve to the chance of reporting anything, and the factor
   by a likelihood search over a grid: the factor went the other way (to
   the grid's floor, 0.5) with a curve too steep. Spreading each slice's
   dollars evenly also misplaced the PUF's rounding lumps ($10, $20); the
   PUF's own dollars inside each slice fixed that part.
3. Fitting the settings against the model's belief without the survey's
   own variable: the same drift.
4. Training the belief about true amounts from PUF rows only (survey rows
   train those slots only when their own variable is visible): the same
   drift.
5. Fitting the settings against the belief given **only the variables the
   survey reports truly** (marriage, dependents, wages, capital gains; the
   other broken item hidden too): it works. Under D2 after 20 passes: both
   dollar factors 0.80 (planted 0.80); interest reporting chance 29% /
   87% / 99% at $50 / $500 / $5,000 (planted 13% / 78% / 98%); hidden
   holders among interest zeros 17.5% (truth 18.1%).

The lesson for the design (confirmed by John and added to plan decision
18, 2026-10-16): **the
reporting settings of a source must be fit against a belief that rests
only on what that source reports truly.** If any input to that belief is
itself a fill-in, a wrong setting can feed itself. Changes 4 and 5 are
both kept in the code.

### Open

- The undistorted survey itself is off by +19% for returns with no
  interest and up to +28% for earners above $200,000. That is the model's
  error with a 30,000-record survey and the same with every method. Not
  diagnosed (candidates: too little survey data for the top; smoothing
  across the 84 slices).
- The learned curve is too generous at small amounts (about 30% at $50
  against 13%) in every run. Not diagnosed; it costs a few points at most
  here.

## Revision history

- 2026-10-16: written; all seven methods, two seeds each.
