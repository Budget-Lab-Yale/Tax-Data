---
title: "Tax-Data rebuild — plan for the one-model design"
role: plan
workstream: rebuild
status: current
updated: 2026-10-16
sot: self
supersedes: [research/archive/joint_model_plan_2026-10-14_superseded.md, research/archive/design_decisions_2026-10-14_superseded.md]
superseded_by: null
---

# Plan for the one-model design

The short version of `idea_space.md`, which records the discussion of
14 October 2026 in full. This file says what is decided, what is open, what
the fallbacks are, and what to test next. The problem statement is still
`research/rebuild/BRIEF.md`; the experiment record is
`research/rebuild/joint_model_history.md`.

## What is being built

One probability model of tax units, p(record | year), trained on every
survey stacked together, each translated into tax concepts first. A year's
file is a sample from the model after post-training to that year's targets.
The PUF is training data, not a backbone. The purpose of the remaining days
is to learn whether this is possible, not to finish it.

## Decided on 14 and 15 October

1. **The object is the model; the file is a sample.** The hybrid (generate,
   then calibrate whole-record weights) and the file-with-helper (real PUF
   records completed by a model) are fallbacks, and both are known to work.
2. **Year stays in the model** as a coordinate. Amounts are positions on
   the PUF's dollar scale for that year (decision 15), so the year
   coordinate carries only composition and association drift; dollar
   levels come from the rank-dollar map. (Until 15 October this read
   "ranks within source and year"; that rule doubled wealth on PUF-style
   records, see `minimal/09_RESULTS.md`.)
3. **The reference for a year is a rolling prior:** the year token drawn
   from a kernel around t at generation, bandwidth chosen by held-out
   waves, one-sided at the forward edge.
4. **Records enter pretraining; tables enter post-training as targets with
   a precision; tax law enters as a function** between a record and any
   target on a reported amount.
5. **Guard targets** are a legitimate kind of target: they hold a region in
   place while other targets move things. The SCF's own tables are both
   training data and targets, and that is fine.
6. **The 2015 PUF is the only tax prior.** No pseudo-records for later tax
   years.
7. **The base includes the 2015 non-filer pool.** Non-filers are rows in
   pretraining with a filing flag; the residual partition is a target per
   year.
8. **Post-training is by tilting.** Tilt-then-distill, direct gradient and
   policy gradient are written down as fallbacks in that order. The word is
   "tilting," not "raking."
9. **Generation order: the record's own source's variables first, borrowed
   variables last** (changed by John 2026-10-16; was "block order, income
   group, tax variables, wealth, demographics last"). On a PUF record every
   PUF variable, demographics included, is drawn under the PUF label, in a
   random order inside the block; then each borrowed variable, net worth,
   is drawn as decision 16 says. Drawing married and dependents after net
   worth under the PUF label asked about a combination no PUF record has,
   and left non-joint returns with two or more dependents at 8.7% against
   the PUF's 11.6%; with the PUF variables first they come out at 11.4%
   (`minimal/20_SEPARATE_MODELS.md`, arm C).
10. **The rank-dollar map** is a per-variable, per-year sorted list of
    dollar values. Within-band draws happen before the tilt. All nominal
    growth and within-variable inequality change live in the maps. Identities
    (AGI from parts, wages from the split) are enforced on the drawn amounts.
11. **Chain across years:** post-train year t+1 starting from year t's
    post-trained model, with the target list's structure fixed across years.
12. **Output weights:** generated records start equal; the tilt factor is
    the weight. Income-group counts come from IRS tables as tilt targets on
    AGI computed from the drawn parts, never as a group label inside the
    model (clarified 2026-10-14 after the minimal example; a derived
    variable is never a model input, see D25). Tilt-then-redraw gives an
    equal-weight file. The top is oversampled by fixing a real variable's
    band at generation and weighting by the model's band probability over
    the sampling rate, which is the IRS's own design applied to the
    generator.
13. **Acceptance test:** the pretrained model reproduces the surveys'
    relationships on held-out records, and post-training hits a year's
    targets with a small move (under about 1.3 nats). The reform run and the
    backtest are later checks, not the acceptance test.
14. **Source label:** set, never predicted. Label-responsive for the
    variables a survey owns; label-indifferent for the variables surveys
    share, which translation is responsible for.
15. **The translation rule** (confirmed by John 2026-10-15). Get each
    survey's dollars onto the PUF's terms first, then place every record on
    the PUF's dollar scale for that year, with the source label kept. In
    order: population (filing rule), definition (fix in the table builder,
    or make the variable survey-owned), documented dollar errors (scale
    factor from the range where both files report fully, not the ratio of
    totals), then the mapping. Top-coded regions and unasked questions are
    absent on the survey side, never zero. Omission of small amounts and
    rounding need nothing. **The population check** (John, 2026-10-16):
    once each survey's tax units are built the way the IRS builds them and
    nonfilers are handled, its weighted count of units should equal the
    IRS return count for the same year. A gap that a difference in years
    does not explain is evidence that the tax-unit construction or the
    filing rule is not right yet (now: 143 million SCF units against 158
    million returns for 2019). Case by case in
    `research/rebuild/minimal/14_TRANSLATION_RULE.md`; the evidence is
    `minimal/09_RESULTS.md`, `10_RESULTS_A.md`, `10_RESULTS_YEARS.md` and
    `13_RESULTS_B.md`.

16. **A borrowed variable is drawn as its owner would see the record**
    (John, 2026-10-15). Net worth on a PUF record is drawn under the SCF
    label, with the slots the SCF never has (other income) left out, so
    the model is asked only questions of a kind it saw in training. Drawing
    under the PUF label, or with other income visible, asks the model about
    combinations no training record had, and its answer then depends on
    how the network extrapolates, not on data
    (`minimal/16_MODEL_SIDE_FIXES.md`). Code: `03_generate.py`,
    `borrowed={'label': 'owner', 'foreign': 'absent'}`.

17. **Tilt targets are in dollars** (John, 2026-10-15). A target stated
    by rank ("the top 1% of units holds 38% of net worth") is converted to
    fixed dollar lines when the target sheet is built, from the source that
    defines it: the number of returns above the source's 99th percentile in
    dollars and the net worth they hold. The tilt only ever sees targets
    whose value on each record does not depend on the weights, so it solves
    once. The conversion states two things on the target sheet: which
    population a count is of, the same for every count target (on
    2026-10-16 the count above $1 million was found on the SCF's own base
    of 143 million units while the percentile counts were on 158 million
    returns, which made the two contradict each other,
    `minimal/19_DIRECT.md`; John then chose the SCF's own weighted counts
    for every count target, which treats the 15 million extra returns as
    sitting below every line, `minimal/15_BASELINE_V4.md`, section v4c) and where
    the dollar line comes from when the published source gives only shares
    (for example, a Distributional Financial Accounts share converted with
    the SCF wave for that year). Replaces the rank iteration of
    `minimal/06_wealth_tables.py`, which did not settle in any v4 run.

## Open

- **A model-side answer to the translation problem.** Decision 15 is
  good enough to reach the stopping point, but it is a rule applied to the
  data before training. John (2026-10-15) still wants something more
  foundational: a mechanism inside the transformer that represents how
  each source reports a shared variable (for example, a true amount plus
  a source-specific chance of reporting it). That mechanism is what would
  handle mismatches the data cannot fix, such as a survey zero that hides
  a real amount. The three in-model attempts so far (E7 to E9 in
  `minimal/09_RESULTS.md`) failed, for reasons not yet understood; the
  investigation is `minimal/16_MODEL_SIDE_FIXES.md`.

- **Training weights.** D26 (survey weights within income groups, record
  counts across groups, group sizes from IRS counts at generation) is the
  proposal. Untested. Known weak point: the SCF's within-group skew toward
  the wealthy. Alternative: a single power on the weights between 0 and 1.
- **How much each survey counts in total** in the training signal. Default
  under consideration: the authority for a variable dominates it.
- **Year versus observable state** as the coordinate, and so what "distance
  between years" means for the rolling prior.
- **Model class:** transformer against sequential trees, on the multi-survey
  job with the general weighting rule applied to both.
- **The target list:** which tables, which years, which held out, each
  precision. Guard targets included.
- **Non-itemizers' deduction amounts** (D20) and **top-0.1% completed
  wealth** remain unsolved from the experiments.
- The thirteen authority-table questions
  (`research/rebuild/tests/probes/AU_AUTHORITY_TABLE.md`).
- **Diffusion-style generation** with no draw order: noted as a later
  possibility, not to be pursued now.

- **Confidence intervals.** Train about 20 models on bootstrap resamples
  of the training data (the SCF's replicate weights), tilt each to targets
  drawn from their sampling error, and read the spread of any statistic.
  Assumptions (translation rule, year mix, dollar growth) are sensitivity
  runs, not part of the interval. Not started (2026-10-15).

## Next tests, in order

Each should be scorable with the existing scorer
(`research/rebuild/tests/probes/ev_score.py`) so that one run answers one
question.

1. **The weighting rule (D26)** on both the transformer and the trees, on the
   same combined data. Measures: the top 0.1%'s income mix and wealth, the
   middle groups' wealth, tax by AGI group.
2. **Block order** against fully random order, same model. Measures: the
   order-sensitivity numbers (span across orders in nats per record; the top
   1% share and total AGI across orders).
3. **Within-band draws from the rank-dollar map**, then the classifier test.
   The banding alone scored 0.72; the target is to see the generated-versus-real
   score fall toward 0.5 once amounts are continuous.
4. **Label indifference after translation.** For shared variables, how much
   does the model's answer change when the same record is shown under the
   PUF label and the SCF label? Not yet measured.
5. **Tax law in the tilt.** One censored target family: 2019 itemizer counts
   and amounts, with tax-simulator-gpu's 2019 law run on each generated
   record before tilting. Measures: fit, move in nats, and the untargeted
   deduction distributions.
6. **Rolling prior bandwidth** by held-out SCF waves: predict each wave's
   records from a mix of the others; choose the weights or the window by
   held-out log-probability (cross-validation), or fit the weights directly
   (stacking), and check on held-out targets. The smallest tilt alone is
   not the criterion: it can favour a prior that is good only on targeted
   moments. (Methods added 2026-10-15; `minimal/17_DISCUSSION_2026-10-15.md`,
   section 2.)
7. **Tilt-then-distill**, and direct adjustment of the parameters. First
   tried on the minimal example on 15 October at John's request
   (`minimal/18_DISTILL.md`, `minimal/19_DIRECT.md`); needed in earnest
   only if a tilt needs more than about 1.3 nats on a real year's targets.
8. **Add the CPS and the 2015 non-filer pool** to pretraining, translated,
   with the filing flag as a variable. First three-survey run.

## Stopping point reached (2026-10-15, evening)

John judged the v4b baseline (`minimal/15_BASELINE_V4.md`) good enough as
the stopping point. The first step after it is nonfilers: the PUF is
selected on filing, the SCF is not, and the filing rule tried for the
wealth targets (SCF units above the filing threshold) left 91 million
units against 158 million returns, so it needs real work, not a
threshold. Then the order below.

## Order of work after the minimal example (decided 2026-10-14, late)

The minimal example (`research/rebuild/minimal/`, baseline then `07_BASELINE.md`, now `15_BASELINE_V4.md`)
showed the chain works on seven variables and one year. John's order for
what follows, each a delta off that baseline:

1. More years: add the 2019 and 2022 SCF waves; the year token and the
   rolling prior get their first real test.
2. More variables: from seven to about twenty. Itemized deductions are
   left out of this step on purpose: they are a function of the tax
   calculation, and the 2015 to 2019 law change makes them a tax-law
   question, not a data question. They come in with step 4.
3. Network size: a sweep of sizes at each variable count, alongside 1 and 2.
4. Tax law in the loop, later. Itemized deductions enter here.
5. The top (aggregate records, thin SCF top, Forbes), after evidence from
   1 and 2.

## Future refinements (nice to have, not needed for the stopping point)

Things known to be crude in the current version, worth doing later. Add
to this list rather than letting them live only in reports.

- **Growth of the PUF's dollar scale by bracket, not by one factor per
  item.** For a year without a PUF, the 2015 cut points are grown by the
  IRS growth in the item's total, the same factor at every amount. SOI's
  tables by AGI bracket would let small and large amounts grow at
  different rates. This growth is part of the post-training prior: what it
  gets wrong, the tilt has to undo, and a better prior means a smaller
  move. (Noted 2026-10-15.)

## Fallback ladder

1. Model-as-population with tilting (this plan).
2. The same with tilt-then-distill, then direct gradient, then policy
   gradient.
3. Hybrid: generate from the model, calibrate whole-record weights. The
   tree baseline fits here.
4. File-with-helper: real PUF records completed by a model or trees, then
   calibrated. Every data lesson carries over.
5. The current pipeline with the linear program replaced by entropy
   calibration.

## Effect on the earlier decisions (D1–D26, archived)

| Earlier decision | Status under this plan |
|---|---|
| D1 one best estimate; D12 JAX acceptable; D13 records need not persist | Kept. |
| D2, D4, D6, D16 (age-then-calibrate loop from 2015; aging carries shape; demographic aging by weights) | Replaced. Each year is generated from the year-conditioned model; D4's content moves into the rank-dollar maps. |
| D3 entropy calibration; D14 weight caps; D15 hand-written target list; D17 evaluation through the list; D18 consistent target structure; D19 tax calculation before targeting | Kept; they are the tilt and its target list. D18 is the reason chaining is safe. |
| D5 values carry amounts, weights carry participation | No longer a rule; the model draws both. |
| D7 no generated records | Reversed. Records are generated. |
| D8, D9 track weight concentration; errors persist | Kept as the effective-sample-size check on the tilt factors and as the reason for guard targets. |
| D10, D21 pooled donors with copies | Replaced by the model and the rolling prior; the pooled-SCF test's findings are the evidence for the rolling prior. |
| D11 current-pipeline bugs not a priority | Kept. |
| D20 impute underlying expenses for non-itemizers | Kept, still open. |
| D22 one joint model; D23 translate before training; D24 trust survey weights, translate meaning; D25 derived variables never predicted | Kept; they are the core of this plan. |
| D26 survey weights within income groups | Kept as the proposal to test first. |

## Revision history

- 2026-10-16: decision 17 uses the SCF's own counts; decision 15 gets the
  population check (weighted unit counts should match the IRS return count).
- 2026-10-16: decision 9 changed (own-source variables first, borrowed last);
  decision 17 states one base for every count target.
- 2026-10-15: stopping point recorded; confidence intervals added to Open;
  next tests 6 and 7 updated.
- 2026-10-15: decision 17 (tilt targets in dollars) added.
- 2026-10-15: decision 16 (how a borrowed variable is drawn) added.
- 2026-10-15: open item on a model-side answer to translation added.
- 2026-10-15: decision 15 (the translation rule) added and decision 2
  reworded to match; "Future refinements" section started.
- 2026-10-14 (late): order of work after the minimal example added; itemized deductions deferred to the tax-law step.
- 2026-10-14 (evening): item 12 reworded; see above.
- 2026-10-14: written. Supersedes `joint_model_plan.md` and
  `design_decisions.md` (archived).
