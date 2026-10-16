---
title: "Tax-Data rebuild — the joint model plan"
role: plan
workstream: rebuild
status: superseded
updated: 2026-10-09
sot: self
supersedes: []
superseded_by: research/rebuild/design/plan.md
---

# The joint model plan

This is the plan for building Tax-Data's microdata from one model trained on
many surveys. It states in one place what decisions D22, D23 and D24 in
`design_decisions.md` decided on 8 and 9 October 2026; that file keeps the
full record and reasoning. Where the two differ, this file describes the
plan and the log describes how it was reached.

## The goal

For any year, a file of household (tax-unit) records in which every record
carries the full set of variables Tax-Simulator needs: the PUF's tax
variables, wealth, and in time consumption and other survey variables.
Each record must be individually realistic, and the file as a whole must
reproduce what each source says about the variables it measures.

## The approach in three stages

1. **Pretraining.** Train one transformer on many surveys stacked together.
   It learns how variables go together, and can fill in any variables of a
   record given any others, or generate whole records.
2. **Pretraining check.** Measure how close the model's records come to the
   ground truth: each survey's own tables and joint distributions, through
   that survey's way of measuring.
3. **Post-training.** Shift the model, or reweight its records, by the
   smallest amount (measured by entropy distance) that hits the year's
   targets. How far it has to move is the measure of whether the
   pretrained model is good enough.

## Each variable has one authority

"Tax concept" is shorthand for the file's concept, which differs by
variable. Each shared variable has one authority: the source that measures
it directly. Translation runs toward the authority's concept.

| Variables | Authority | Concept |
|---|---|---|
| Incomes, filing status, other tax items | IRS (PUF, SOI) | what is on the return |
| Age, sex, household makeup | Census (ACS, CPS) | the person's actual age, etc.; file definition: age of the primary filer and spouse |
| Wealth | SCF | the SCF's balance sheet |

Where the PUF lacks a variable or has it for only some records (age is
withheld for every return in the top 1 percent of AGI and missing for
non-filers), the PUF is not the authority. Its records are completed for
that variable by the model, like wealth, learning from surveys that observe
it (the SCF has ages for top-income households), and the authority's
tables are post-training targets (Census population by age; SOI returns by
age and AGI). Because the PUF's missing ages depend on income, generation
must draw demographics after the incomes; drawing age first taught the
model that a return with a visible age is never a top-income return
(found 9 October).

## Preprocessing: every survey translated into tax concepts first (D23, D24)

A survey's variables often share a name with a PUF variable but mean
something different: they are defined differently, measured differently,
or describe a differently built unit. Marital status is not filing status;
SCF dividends miss the small holders a tax return records (19 percent of
PUF returns report dividends against 12 percent of SCF units, with similar
totals). If the model learns wealth given SCF-measured incomes and is then
asked about a PUF record, the relationship does not transfer.

So before training, every survey row is put into tax concepts, by rules
written down per survey:

1. **Definitions.** Arithmetic that builds tax-concept variables from the
   survey's items (SCF business income = Schedule C plus Schedule F; a new
   tax-side variable where no single PUF line matches, such as total
   Schedule E).
2. **Coverage.** A survey's weights are trusted. They change only for who is
   in scope: the Forbes 400, which the SCF leaves out; filers against
   non-filers.
3. **Translation of the shared variables.** Any remaining disagreement
   between a survey's weighted distribution of the shared variables
   (filing or marital status, age, dependents, incomes) and the file's is
   read as a difference in meaning. It is removed by moving each survey
   household to its values in tax terms, keeping its weight and its other
   variables (such as wealth) unchanged. One variable at a time, in a fixed
   order, each within cells of those already translated:
   - marital status to filing status, by a probabilistic crosswalk;
   - age (near identity, adjusted for whose age: the SCF's head against the
     PUF's primary filer);
   - dependents;
   - wages by rank;
   - each other income item by rank with zeros included, the records
     flipped between zero and positive chosen as the most likely ones given
     the whole row (never at random).
   Several draws per household, because the crosswalk and the flips are
   probabilistic.
4. **Encoding.** Amounts as ranks within source and year, so that changes
   in dollar levels over time come from each year's own distributions and
   targets.
5. **Unshared variables** stay in their own survey's concept. The SCF is
   the authority for wealth.

Why translate rather than reweight: reweighting would say the survey has
the wrong number of each kind of household, which changes its overall
distributions (the SCF's wealth distribution among them) and contradicts
trusting its weights. Translation says the survey labels the same
households differently. It keeps every survey's overall distributions
exactly, and it is an explicit function that can be inspected and checked.

**Assumptions** (to be tested): within a cell, households keep their order
(a household ranking higher on reported dividends ranks higher on tax-return
dividends); how a respondent answers does not depend on their wealth once
their true tax amounts are known; the crosswalk probabilities are right;
cells are coarse enough for the survey's sample.

**Safeguard.** Report how far households had to move. Small moves are
translation. Large ones (an age shifted by 15 years, an income rank moved
from the 50th to the 90th percentile) mean a real disagreement, or an error
in our own construction (the file's imputed ages, a rule in a unit
builder), and are investigated, not absorbed.

## The pretraining data and the model

- **The data.** All surveys stacked: one row per tax unit, one column per
  variable any survey measures, plus source and year. A variable a survey
  does not measure is unknown on that row, never zero. Each row carries its
  survey weight, and training counts rows by weight, so the model learns
  the population rather than each survey's sampling design.
- **Derived variables are never predicted (D25, a requirement).** A total
  or any fixed function of other variables (AGI, taxable income, net worth,
  total Schedule E) is not a model variable. The model draws the parts plus
  one remainder variable where the parts do not cover the total, and the
  total is computed. With AGI drawn as its own variable, the income items
  of the top 0.1% of generated returns summed to about half the PUF's; with
  AGI computed as the sum, to about nine-tenths (probe AS, 10–11 October).
- **Training.** Repeatedly, hide some of a row's known values and train the
  model to predict them from the rest. Unknown values are never scored.
  The model then gives, for any variable, its probability distribution
  given whatever else is fixed.
- **Generation.** Fix what is known and draw the rest one variable at a
  time. Completing a PUF record fixes its tax variables and draws wealth;
  generating from scratch fixes only the year.
- **The end state** is records generated from scratch or close to it (D7,
  "no generated records", is under review). Completing real PUF records is
  the first step.
- **Sources.** Now the 2015 PUF (test 1's 39 variables plus total
  Schedule E) and the 2013 and 2016 SCF; ages from the CPS or ACS. Later
  the CPS, ACS, SIPP and CEX, over many years. A large model pays off only
  with that much data; with the PUF and SCF alone, size has not helped.

## Years

There is no single base year. How variables relate to each other changes
slowly, so it is learned in rank terms from every survey of every year,
with year as a model input so that relationships can drift where the data
show drift. The level and spread of each variable in a year come from that
year's own information. To build year t:

1. prompt the model with year t (or the nearest year with data);
2. turn ranks into dollars with year t's distributions (SOI for tax items,
   interpolated or projected; the nearest SCF waves for wealth);
3. post-train to year t's targets.

Future years use projected distributions and projected targets (CBO, SSA).
Each year is built on its own; records need not carry across years (D13).

## Post-training and targets

Three methods, compared in test 3:

1. **Reweight draws.** Draw several completions per record and choose
   weights that hit the targets, closest to equal weights. The model is
   unchanged.
2. **Tilt the probabilities.** Add one number per target cell to the
   model's log-probabilities, network unchanged; draws are unweighted.
3. **Tune the network.** Retrain it to hit the targets while penalized for
   moving away from the original (on reweighted draws, or directly).

Targets, each with a stated precision:

- each survey's tables, compared after passing the file's records through
  that survey's way of measuring (as tax tables are compared after running
  tax law, D19);
- administrative totals (SOI) after running the year's tax law;
- projections for future years.

Each survey's overall distributions enter at their full sampling precision;
after translation they no longer conflict with the file's tax variables.
Reweighting never makes an implausible record, only makes some records
more common, so realism depends on the pretrained model and hitting totals
on post-training.

## What it cannot do

The model can learn relationships between variables only where some survey
observes them together. Variables never observed together (partnership
income and how wealth splits into stocks and bonds) are linked only through
the shared variables, by assumption. A bigger model does not change this.
More overlap does: more shared variables, chains of surveys, and **bridge
variables** such as mortgage interest and real estate tax, which the SCF
measures for everyone and the PUF records for itemizers (unknown, not zero,
for non-itemizers).

## Evidence so far

The full history of the experiments of 8–14 October, with what each
changed and where the work should resume, is `joint_model_history.md`.

- **Test 1** (8 October; PUF only): the model's predictions are close to
  calibrated when it sees most of a record; it is weakest generating whole
  records from nothing. Model size did not matter.
- **Test 2** (9 October; PUF + SCF, ten shared variables, source labels
  rather than translation): on held-out SCF records the model matched the
  top 1 percent's share of wealth (37.5 against 37.3 percent) and never
  copied a household's balance sheet, but wealth rose too little with
  income (the top 1 percent by income held 25 percent of net worth against
  31 percent). A survey-weighted version of the production forest did
  better on wealth by income group.
- **Test 3** (9–10 October; the 40-variable PUF + SCF model, post-training):
  running at the time of writing. First findings: whole PUF records
  generated from nothing are too thin at the top (top 1 percent's share of
  AGI 9–10 percent against 19.5; total AGI $6.5–6.7 trillion against
  $8.1 trillion), and the SCF wealth targets carried onto PUF groups
  contradict each other because the two sources' mixes of income and age
  differ, which is the problem D24's translation addresses. Results:
  `tests/posttrain/STATUS.md` and `EVAL_REPORT.md`.

## Next steps

This list dates from 9 October. The current view of what to do next,
after the experiments of 9–14 October, is the end of
`joint_model_history.md` ("Open issues" and "Where I think the work should
resume").

0. Write the authority table above in full: for each shared variable, the
   owner of the concept and its exact definition.
1. Build D24's translation for the SCF (filing status, age, dependents and
   the shared incomes), check it on test 2's distorted SCF datasets where
   the truth is known, and retrain on translated data.
2. Ages and other demographics for the PUF from the CPS or ACS.
3. Bridge variables: mortgage interest and real estate tax first.
4. The top of the generated distribution: generate demographics after
   incomes (checked 9 October, job 28917534: weight above the true 99th
   percentile rises from 0.30% to 0.65% against 1.00%, total AGI from
   $6.62tn to $7.37tn against $8.09tn; a large part of the cause, not all
   of it); separate early
   stopping for PUF and SCF, since the SCF part overfits while the PUF part
   is still learning.
5. More surveys.

## Effect on earlier decisions (to confirm with John)

- **D2, D4, D6 and D16** (age the file forward year by year from 2015;
  aging carries the distribution's shape; demographic aging only by
  weights): replaced by building each year from the year-conditioned model
  plus that year's distributions and targets, if this plan holds.
- **D5** (values carry amounts, weights carry participation): no longer a
  rule of the method; the model draws both.
- **D7** (no generated records): under review; the end state is generated
  records.
- **D21** (pooled survey donors with several copies per record): replaced
  by the joint model, as D22 said it would be if adopted.
- **D3, D8, D14, D15, D17, D19** (entropy calibration, tracking weight
  concentration, weight caps, a hand-written target list with precisions,
  evaluation through the target list, comparing tax tables after running
  tax law): kept; they become the post-training step.
