---
title: "Tax-Data rebuild — the one-model design, as discussed on 14 October 2026"
role: method
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/design/plan.md
supersedes: [research/archive/joint_model_plan_2026-10-14_superseded.md, research/archive/design_decisions_2026-10-14_superseded.md, research/archive/synthesis_2026-10-14_superseded.md, research/archive/rlhf_analogy_2026-10-14_superseded.md, research/archive/d21_pooling_questions_2026-10-14_superseded.md]
superseded_by: null
---

# The one-model design

This is a detailed record of a conversation between John Ricco and Claude on
14 October 2026, in which the rebuild's design was gone through from the top.
John's purpose for the remaining days of the two-week window is not a finished
pipeline. It is to understand the space of ideas well enough to know whether a
neural network can hold a faithful representation of the tax-unit population,
and to leave a clear record of that space for later sessions. This file is
that record. `plan.md` beside it is the short version: what is decided, what
is open, what to test next.

Every number here comes from the experiment reports under
`research/rebuild/tests/` or from the archived synthesis of the background
research. Where a statement is Claude's opinion rather than a result, it says
so. Terms are defined where they first appear and collected in the glossary
at the end.

## 1. Where things stood at the start

Eight days of experiments (8–14 October) had produced a plan organized around
one transformer trained on the PUF and the SCF stacked together. The
conversation began with a map of that work.

**Settled, with evidence behind it.** These carry forward unchanged.

- Minimum-change calibration replaces the linear program. The old program
  put 62% of records at a factor of 0.34 and 38% at 1.66, systematically
  against high-income records.
- Every survey is translated into tax concepts before training, and
  reweighted only for coverage. Translation recovered known truth in a test
  where one half of the SCF had been deliberately distorted, and cut the
  contradictions among wealth targets on the real SCF.
- Totals are computed from their parts, never drawn. Making AGI a sum fixed
  the top of generated returns more than anything else: the top 0.1%'s
  income items went from $341bn to $642bn against $702bn in the PUF.
- The oversampled top must count by its records, not by its population
  weight. Population-weighted training lost the top on the PUF and the SCF
  alike, for the transformer and for the tree baseline.
- Post-train by tilting the model's probabilities, not by reweighting a
  finite set of draws. Reweighting 32 draws per record could not reach the
  records it needed (worst target error 38%); the tilt hit every target
  (worst error 2.0%).
- Bridge variables (mortgage interest, real estate tax, which the SCF has for
  every owner and the PUF for itemizers) improve wealth completion. Each
  itemized deduction needs two variables, the amount claimed and the amount
  paid.

**Open at the start.** The choice of model (a sequential tree baseline tied or
beat the transformer at generating tax records); how survey weights enter
training (D26, proposed and untested); whether the end state is completed real
PUF records or records generated from scratch; whether the year-by-year aging
loop is replaced; thirteen questions in the authority table.

**Untouched by any experiment.** Projection years; precision-weighted soft
targets (every run used exact targets taken from the training data's own
tables); non-filers; a target list and an evaluation harness; consumption and
the other surveys.

The observation that framed the rest: most of what the experiments taught is
about how the data are built, not about the transformer. A sequential tree
given the same data construction was competitive. So the question "is the
network the point, or is the data construction the point, with any decent
generator on top" had to be asked directly.

## 2. John's goal

John's answer: he wants to know whether a deep-learning model can represent
microsimulation-relevant microdata, as a question in its own right. If a
simpler pipeline (the PUF, trees, aging and reweighting) is as good and
simpler, he will use it. But the search itself is the deliverable for these
six days: a thorough look at the space of ideas and the tools available, and
a written record of it.

The conversation then went through four questions in order.

1. What object are we building?
2. What information flows in?
3. How is it combined?
4. How do we know it is right?

## 3. Question 1: what object are we building

### 3.1 Three candidates

- **A file, with the model as a helper.** Real PUF records are the backbone.
  The network fills in what the PUF lacks (wealth, ages at the top,
  non-itemizers' expenses) and calibration sets the weights. This is what
  every experiment through 14 October actually did. It is safe, and it does
  not answer John's question, because the population is still the 2015
  sample.
- **A model, with the file as one sample from it.** The network is the
  representation of the population for any year. A year's file is a draw,
  disposable and regenerable. The PUF is training data, not a backbone.
  Post-training shifts the model.
- **A hybrid.** Generate many records from the model, then calibrate
  whole-record weights to the year's targets. The model supplies the support
  (the set of records that can exist); calibration supplies the counts.

**Decision: the model, with the file as a sample.** The hybrid is the
fallback, and the tree baseline fits it too. The file-with-helper version is
already shown to work and is the floor.

### 3.2 What "possible" means for the chosen object

For the file-with-helper, the experiments have already said yes. For the
model-as-population, the test is harder: a sample from the model, with no
real record in it, has to reproduce what the PUF-based file produces when
passed through Tax-Simulator, under current law and under reforms, including
at the thresholds and phase-outs where reforms bite. One experiment did the
current-law half of this for 2015 and the tax test passed (income tax $1,190bn
against $1,158bn; every AGI group's average tax rate within about 2.3 points).
No reform has been run. (John later set a different acceptance criterion;
see section 6.)

### 3.3 What a network can do that a tree cannot

The honest reading of the tree result: on a fixed set of variables from one
source, a sequential tree is a good density estimator and it won. The
network's case rests on things a tree cannot do:

- condition on any subset of variables, in any order;
- learn one representation across surveys that observe different variables;
- condition smoothly on year;
- be adjusted by gradients.

None of these had been the subject of a test. Under the file-with-helper
object they are nice to have. Under the model-as-population object they are
the whole point.

### 3.4 Tax law inside the adjustment

A capability not mentioned in any earlier document. Tax-simulator-gpu is
written in JAX, so the tax calculation runs fast on a GPU and is
differentiable. A neural generator can therefore be post-trained against
targets on reported amounts (itemizer counts in 2019, tax by AGI group) with
the tax law inside the loss, rather than around it as D19 did with values
frozen. Later in the conversation (section 5.2) it turned out that
differentiability is not even required for the tilting method chosen: with
records fixed, tax owed under a year's law is just a number per record, and
the tilt handles any per-record number. What is required is that the
calculator runs quickly on a batch of records. It does: the full file runs in
100–220 seconds, and a batch of a few thousand records is far less.

John's reaction: "the can-be-smoothed-by-gradients thing is huge."

### 3.5 The vision as John stated it

One big probability object: a pretrained transformer that represents the
space of realistic joint distributions among tax-unit variables, with year
as one of its dimensions. Survey data from many years go in, so that it
learns how relationships change over time and can generate plausible tax
units across time. Some tax units are more plausible in some years than
others: a 25-year-old who is married with two children was more plausible
in 2000 than in 2025. The pretrained object is a prior over plausible
records; post-training bends it to hit targets for other years.

Claude's restatement, which John accepted: the thing being built is
p(record | year). Its support is the union of every household any survey has
ever observed, in any year. Year conditioning says which parts of that
support are likely now. Post-training for a given year bends the
distribution by the smallest amount that makes it agree with that year's
tables. A year's file is a sample from the bent distribution.

### 3.6 Year carries two kinds of change, kept apart

John's mortgage example (a $30,000 earner with a house and mortgage was
plausible in 2000, not in 2020) is nominal growth. His married-at-25 example
is drift in how variables go together. These are handled in different
places.

Storing amounts as ranks within source and year strips nominal growth out
of the model entirely. The model's year coordinate carries only composition
and association drift. Dollar levels come back from each year's own
marginals, through the rank-dollar map (section 5.4). So the object is two
pieces: the model, which holds shape and association by year, and a
rank-dollar map per variable per year, which holds levels. They fail
differently and are checked differently.

### 3.7 Does year need to be in the model at all

John's challenge: post-training targets fix the exact share of units that
are married, married and aged 55–65, holding interest while itemizing, and
so on. What the prior contributes is only the untargeted joints: say, what
fraction of married 55–65-year-olds receive unemployment benefits. So year
matters only as the reference the divergence penalty bends off, and
"probably just not that important, most things we can target with
marginals."

Claude's answer, which John accepted:

**What is right.** Minimum-divergence post-training preserves the prior's
untargeted conditionals inside every targeted cell, so the prior's only job
is the untargeted joints. And the penalty is never year-dependent.
Divergence is measured from whatever reference is supplied. "Penalized less
for diverging from 2009" really means "choose a reference closer to 2022."
Year conditioning is that choice: it moves the starting point, not the
penalty.

**Where "most things we can target" fails.** The target list is a few
hundred cells. The untargeted joints are combinatorial, and the evidence on
file says they drift:

- In the pooled-SCF test, homeownership among stockholders in 2022 was 7.5
  points too high with 2016 donors, 3.7 with 2019 donors, 1.2 with the right
  wave. No income-group table touches it.
- In the same test, calibration did not find the right wave inside an
  equal-weight pool: the 2022 wave's share stayed at 25–26% after fitting 84
  targets. Starting with 60% on the right wave did most of the work.
- In the stretching test (model trained without SCF 2016, tilted to 2016
  tables), a change inside the top wealth band, which no target sees, was
  missed.

The lean toward the nearest wave that the pooled test needed is year
conditioning done by hand.

**The capital gains example, concretely.** Post-train a 2009-like prior to a
2022 count of top-wealth units with capital gains. It upweights the 2009
units that had gains. They were a minority, so each gets a large factor and
the effective sample falls. Whatever else was peculiar about people who
realized gains in a low-gains year (more business sellers, fewer stock
sellers) comes along, because it is untargeted. The count is hit; the
composition behind it is 2009's. The move in nats says it was a stretch.

**The cleanest statement.** Every survey of year t is a huge bundle of
implicit targets: all its joint distributions, at sampling precision.
Putting year in the model is how those enter, in pretraining, at the cost of
one token. Leaving year out means the only year-t information the file gets
is the hand-written target list. Same information, two mechanisms, and
pretraining absorbs vastly more of it than the list ever will. This is the
same argument as for the model existing at all.

**Where John's instinct holds.** For years with no survey, year conditioning
gives nothing beyond the nearest observed year, and post-training carries it.

### 3.8 Year versus observable state

An open branch, added because John asked for breadth. Year may be the wrong
coordinate. What makes the 25-year-old with two children less plausible is a
set of economic and demographic states (fertility, interest rates,
unemployment, the standard deduction), not the calendar. Conditioning on
observable state instead of or alongside year would let a projection year
condition on CBO's projected state rather than on a number the model has
never seen. It also makes the tax-law dependence explicit: who itemizes is a
function of the law, and the law could be an input. Whether the surveys are
rich enough to identify this is unknown. Held as the open fork "what is the
distance between years."

### 3.9 The rolling prior

John: the reference for year t could lean mostly on t, some on t−3, a little
on t−6, and so on. The production wealth imputation already pools SCF 2019
and 2022 and tilts to 2022, so there is precedent.

Agreed, and it drops into the model version with almost no machinery:

- **What it is.** A kernel over years. The reference for year t is a mixture
  of the model's year-t′ slices, weighted by distance from t. In the model
  this is one line at generation: draw the year token from the kernel around
  t for each record, instead of fixing it. Training is untouched.
- **The model already does some of this.** A model with year as a shared
  coordinate is a rolling prior with a learned bandwidth, because nearby
  years share parameters. The explicit kernel adds a bandwidth that is
  controlled and readable. If the model's own smoothing is good, the explicit
  kernel should buy little; that is checkable.
- **The bandwidth can be estimated.** Wide kernel: more records at the thin
  top, more drift error. Narrow: the reverse. The pooled-SCF test was this
  experiment at two bandwidths, and the lean won at the edges. The general
  version: for each observed wave, predict its tables from a kernel over the
  other waves, and choose the bandwidth that minimizes error plus the
  post-training move. The bandwidth can differ by region of the
  distribution, since the top is both the thinnest and the fastest-moving.
- **One-sided at the forward edge.** For 2040 there is no t+3. Mass sits on
  the last observed year and decays backward.
- **The same fork again.** A rolling prior in time is a kernel in a
  one-dimensional state space, the calendar. A kernel over observable states
  is the same idea in more dimensions: 2022 would pull from other
  high-realization years rather than from 2019 and 2016 by proximity.

## 4. Question 2: what flows in

### 4.1 The rule

Records enter pretraining. Tables enter post-training as targets, each with
a precision. Tax law enters as a function, sitting between a record and any
target that measures a reported amount.

### 4.2 Three things that do not fit the rule cleanly, and John's answers

**The SCF's own tables.** The SCF's records are in pretraining, so the model
already knows the SCF's wealth by income group. D24 also makes those tables
post-training targets. Double-counting?

John: no problem; how would it differ from the IRS targets? Targets are
arbitrary. They clean up small deviations, and they also guard against drift
caused by squeezing the distribution elsewhere to hit other targets. His
analogy: in RLHF, one targets the behaviour "Absolutely, I can help with
that" to be more likely; as a side effect the model may start saying
"Absolutely, I love you"; so one also targets a reduction in that, where the
second target exists to stop the probability space being squeezed in an
unwanted way.

Recorded as the idea of **guard targets**: a target whose job is not to add
information but to hold a region in place while other targets move things.
It is the same thing as the "reward hacking" row in the old RLHF-analogy
note. Consequence: the target list has a second kind of entry, and guard
targets can be fitted or held out. One technical point: minimum-divergence
post-training preserves untargeted conditionals only inside each targeted
cell; across cells relationships can move (the correlation of net worth with
dividends shifted in one test). Guard targets are how that is noticed and
stopped.

**Pseudo-records for tax years with tables only.** The IRS gives microdata
for 2015 and tables for 2016–2023. Under the rule, the model's tax-side
knowledge of 2019 arrives only through post-training. The alternative is
self-training: post-train to 2019, feed the resulting records back as
training data labelled 2019.

John: no. The 2015 PUF is the only tax prior, "because that's what we
have."

Consequences recorded:

- The year coordinate is informed on the tax side only through the surveys'
  shared variables after translation. Wages, dividends, marital status and
  age in every CPS and SCF wave carry real year-to-year drift into the
  model. Variables only the PUF has (the split of Schedule E into
  partnership and S-corporation income, for instance) have joints frozen at
  2015 and move only through post-training.
- Translating a 2019 survey into "the file's 2019 concepts" has no 2019
  microdata to translate against, only SOI tables. So the translation
  reference for a non-2015 year is a set of marginals by cell, not records.
  Fine for a rank-within-cell translation, but it should be said.

**Non-filers.** Do they enter pretraining as a kind of unit with a filing
flag, or stay a separate population attached afterward?

John: the base should be "the PUF" in the sense of PUF plus the 2015
non-filer pool, as a preprocessing step. Technically imputation, but along
rows rather than columns, and easier to think about that way.

Recorded, with one point in its favour that had not been noticed: unlike the
PUF, the non-filer pool exists for every year 2017–2023 (Block E), so the
non-filer side of the model has a multi-year record set, and the residual
partition for each year (filing adults plus pool adults plus netting equal
the Social Security area adult population) is a natural target. The filing
decision becomes a model variable.

### 4.3 Sources and the years they cover

From the archived synthesis, written here so the design has it in one
place. Verified there against the Budget Lab's data store as of 7 October
2026.

| Source | Kind | Years | Enters as |
|---|---|---|---|
| IRS SOI 2015 PUF | records, 166,156 | 2015 | pretraining; the only tax microdata |
| SOI Publication 1304 tables | tables | every year through 2023 (2024 expected 2027) | targets and rank-dollar maps; Tables 1.5 (by age), 4.3 (percentiles to top 0.001%), 1.4CV (coefficients of variation, a measured precision) are unused today |
| IRS filing-season statistics | tables, by AGI class only | 2024 | targets, coarse |
| SCF | records, about 4,600–6,300 households a wave | 2013, 2016, 2019, 2022 on disk; public waves back to 1989; 2025 promised for late 2026 | pretraining (wealth authority); its tables as guard targets |
| CPS ASEC | records | through the 2026 survey | pretraining (demographics; the non-filer pool); survey change usable for composition and the middle of the wage distribution only |
| ACS | records | through 2024; 2025 delayed | pretraining (demographics, property tax, group quarters) |
| SIPP | records | through 2024 | pretraining (tips, overtime) |
| CEX | records | through 2024 | pretraining (consumption) |
| Non-filer pool (ASEC-built) | records | 2017–2023 | pretraining with a filing flag; the residual partition as a target per year |
| Distributional Financial Accounts | tables, quarterly | continuous | wealth targets between SCF waves |
| SSA W-2 wage statistics | tables by bracket | through 2023 | wage rank-dollar maps by percentile |
| CBO February 2026 revenue workbook | projected Form 1040 lines, counts, top shares, realizations | 2022–2036 | projection-year targets and map growth |
| Macro-Projections (Budget Lab) | projected aggregates | to 2099, constant shares of GDP after 2056 | long-run map growth |

What a projection year knows: CBO's projected aggregates and counts; SSA's
population and wage projections; nothing distributional beyond about 2056.
So a projection year's targets are few and imprecise, the rank-dollar maps
are grown from the last observed year, and the rolling prior sits on the
last observed year.

## 5. Question 3: how it is combined

### 5.1 Settled by the experiments

Listed in section 1: translation before training; totals computed from
parts; the oversampled top counted by records; bridge variables; the
claimed/paid split for deductions; tilting over reweighting draws. These
stand.

### 5.2 Post-training: the four methods, concretely

Take one target to run through all of them: "in 2022, 1.2 million returns
in the 99th to 99.9th percentile of AGI had capital gains." The pretrained
model, prompted with 2022, produces 0.9 million. Each method gets from 0.9 to
1.2 while changing the model as little as possible.

1. **Tilting.** Each record's probability is multiplied by a bonus if it is
   in the target cell: new probability = old probability × exp(λ) for records
   in the cell, with λ solved so the count is 1.2 million. Three hundred
   targets, three hundred λ's. The network is untouched; the λ's sit on top.
   What it cannot do: it only makes kinds of records the model already
   produces more or less common. A combination the model gives nearly zero
   probability cannot be rescued (the bonus needed would wreck the effective
   sample size), and it cannot raise the dollar amount of gains inside a
   record. Inside a target cell the mix is untouched: if the model's top-1%
   records are 74% joint filers and the PUF says 83%, no tilt on an AGI
   target fixes it; that cell needs its own target.
2. **Distillation.** Do the tilt, then retrain the network by ordinary
   maximum likelihood on records drawn from the tilted model. The result is a
   network that produces 1.2 million natively, no λ's needed. Nothing new is
   learned, but the adjustment is baked in, so next year's post-training can
   start from this year's model. In LLM terms, fine-tuning on one's own
   filtered outputs.
3. **Direct gradient.** Write a loss: (predicted count − 1.2 million)² plus β
   times the divergence from the original network, and update the weights.
   The catch: the predicted count is an average over random draws, and a
   draw has no gradient. Test 3 computed the exact expected count at each
   generation step instead, which cost 72 forward passes per training step.
   In 80 minutes it got through about 500 steps, cut the share of records in
   the wrong wealth bin from 4.10% to 2.25%, moved the model by 0.167 nats,
   and was still improving when time ran out. It works when a target depends
   on one variable; it does not work when a target depends on the whole
   record, such as tax owed.
4. **Policy gradient, the RLHF family.** The identity behind it: the
   gradient of "expected score" equals the average over draws of (score ×
   gradient of the log-probability of that draw). The log-probability of a
   whole record is the sum of the chain's log-probabilities, which the
   network gives. So: draw 4,000 records, score each (which cell it is in, or
   tax under 2019 law, anything computable), and push up each record's
   log-probability in proportion to its score, with a divergence penalty
   holding the model near the original. That is REINFORCE; PPO and GRPO are
   the same with better variance control. The scorer need only be fast, not
   differentiable.

What 3 and 4 can do that 1 and 2 cannot: change what is inside records. If
the model's "capital gains given top AGI" is too low, retraining shifts that
conditional, and because the network is smooth, nearby conditionals move
with it. That is where "summoning" records the prior barely produces comes
from: not from the weights being free, but from the network generalizing
from what it is pushed toward. A nuance: distillation and policy gradient
also only ever see sampled records, so they too get new records only through
that generalization; the direct gradient sees every possible value at each
step and can raise an unsampled value's probability directly.

The minimum-divergence answer to soft targets is exactly the tilt formula. So
methods 2–4 are not aiming at a different answer; they try to make the
network absorb the same answer, and in doing so can reach records the tilt
could not.

**Compute.** Pretraining the current model (about 6.9 million parameters)
takes 8–18 minutes on one GPU. The tilt takes minutes. Retraining was hours
in test 3. Not an obstacle.

**Decision.** Tilting is the method, first. All four stay written down as
fallbacks, in the order 1, 2, 3, 4. John's reasoning: tilting is practical;
retraining has the elegance of the LLM analogy and fits the one-big-object
view, so it is the natural next step if tilting runs out. Claude's view: RL
is overengineering for now, because tilt-then-distill (method 2) gives a
retrained network, with the tax law included, with no RL machinery. RL
becomes worth it only if distillation cannot move the network far enough.

**Relation to raking.** John asked whether tilting is raking. Yes in form:
raking's solution is weight × exp(sum of λ's for the record's cells), the
same formula, and D3 already called raking the special case of entropy
calibration with margin counts. What changes when a model is raked instead
of a sample: no finite sample to run out of; targets can be any per-record
function (dollar totals, joint cells, tax owed under the year's law), which
the survey literature calls generalized raking; soft targets with
precisions; and the tilt can be applied per generation step on the bins of
the variable being drawn, which shifts that variable's conditional and so is
slightly more than raking. Deming and Stephan wrote the formula down in
1940. John: it is more than raking, and he does not want it called raking;
raking was his frame of reference. The word used here is **tilting**.

### 5.3 Generation order

Training and generation are separate. In training, for every record a
random subset of fields is hidden and the model predicts them from the rest.
That is the same whatever the generation order. The order question is only
about generation.

**Why order matters at all.** An LLM generates one token at a time in one
fixed order and is trained in that same order, so its answers are always
consistent. This model is trained to answer "AGI given nothing," "AGI given
wages," "AGI given wages and age" as separate questions, and nothing forces
the answers to agree. Which order is used at generation decides which
answers are relied on. Every model trained this way has the problem to some
degree.

**Why not train in one fixed order like an LLM.** A fixed-order model needs
the first fields present in every training row. The CPS has no wealth, the
SCF has no itemized deductions, the PUF has no age at the top. There is no
order in which every survey has the early fields.

**Was it the AGI problem?** Mostly. Before AGI was derived, ten orders gave
top-1% shares of AGI from 4.7% to 24.5%. After, the two main orders gave
19.1% (random order) and 20.6% (largest amounts first), within sampling
noise. The gap between the best and worst order for a single record fell
from 2.3 nats to 1.8. A real inconsistency remains.

**Random versus fixed.** Tested. A fresh random order per record was the
best compromise: each fixed order relies on one chain of conditionals with
its own systematic error (AGI-first put false capital gains in the bottom
half; items-first overshot the top), and random order mixes the chains so
the errors partly cancel. Training part of the time on the exact generation
order helped before AGI was derived and overshot after.

**Block order, new and untested.** A fixed order across blocks, random
inside each block. The data already dictate the blocks: income group first
(its count comes from IRS tables, section 5.6), then tax variables, then
wealth, then demographics last because the PUF hides age for every return in
the top 1%, and drawing age earlier taught the model that a return with a
visible age is never a top return. John: "love the block order idea."

**Diffusion, noted for later.** A model that generates the whole record at
once, by starting from noise in every field and cleaning up all fields
together over many passes, has no draw order. For categorical data like
rank bands the standard version ("unmask fields a few at a time, in random
order, allowing revisions") is mathematically close to the current model
plus the ability to revise, so it may reduce rather than remove the order
problem. These models do well on tabular benchmarks. Held as a possibility
for later, not discussed further.

### 5.4 The rank-dollar map

John's name for it. Also acceptable: crosswalk. Not "ladder."

**Where it sits.** At generation, after the model and before the tilt.
Neither pretraining nor post-training touches it. The sequence for a year:

1. Draw records from the model in rank space, with year t's token (or the
   kernel around it).
2. Convert every rank to a dollar amount using year t's rank-dollar map for
   that variable.
3. Compute each record's totals (AGI from its parts), run year t's tax law,
   and compute its contribution to each target.
4. Tilt. Each record gets a factor from the targets it hits. That factor is
   its weight.

**What the map is.** For each variable and year, the full sorted list of
dollar values, not a few percentile points. For 2015 that is the PUF's own
values, about 166,000 per variable. For 2019 there are no records, only SOI
brackets with a count and a total each, so the 2019 list is the 2015 list
stretched so each bracket's count and mean match 2019. The shape within a
bracket comes from 2015, the level from 2019. At the top, SOI's percentile
tables go down to the top 0.001%, so the stretching can be fine where it
matters.

**Drawing within a band.** The model says "wages in band 41." Instead of
giving every such record one value, draw a value at random from the 2015
values inside band 41. This gives every record its own amount and is the
fix for the banding problem: a classifier told generated records from real
ones with a score of 0.81, and real records that had merely been banded
scored 0.72, so most of what made generated records detectable was the
bands, not the model.

Do the within-band draw before the tilt. The tilt needs each record's dollar
contribution to any dollar target and needs tax law run on actual amounts.
Tilting on band means and drawing afterward would shift the totals and leave
the tax calculation done on the wrong numbers. Drawing first makes the tilt
exact on the file that ships, and the draw is cheap.

Paired variables must keep their identities: wages of the primary and spouse
add to total wages. Either draw the total and the split, or draw the parts
and compute the total. Same rule as AGI.

**Where nominal growth and inequality live.** The model never sees a
dollar, so it has no notion of inflation. For a projection year the map is
the last observed map grown forward: by wage percentile where SSA or CBO
gives it, by AGI bin where SOI average amounts give it, by one aggregate
factor otherwise. This is what `project_puf.R` does today with the
per-variable growth factors, moved out of the records and into the map. D4
("aging carries the best guess of distribution shape") survives as the
construction of the year's maps. The improvement over today is not the map
step; it is that the model and the tilt change the composition underneath it
rather than freezing 2015's shape.

**A risk to watch.** Suppose the target is "total wages in 2019 = $8
trillion." The tilt can only hit that by making high-wage records more
common. If the 2019 map is too low, the tilt distorts the composition of the
population to make up the difference. This is the "composition stands in
for growth" problem the synthesis flagged in the current pipeline. The fix
is to get the maps right, which for observed years is possible because SOI
publishes the actual distributions.

**Consistency across maps.** AGI is computed from its parts, each with its
own map, so the implied AGI distribution is a consequence of the parts' maps.
If SOI's AGI distribution for a year is a target, it constrains the maps
jointly. That is a constraint on how the maps are built, not on the model.

**Band granularity.** John is not worried about it. Finer bands: the model
must learn more, with fewer records per band at the top. Coarser: the
within-band draw does more and the model carries less. Once there is a
within-band draw, band count matters less for realism and more for what the
model can learn. The current 66 bands with extra resolution at the top
passed the tax test. If it ever matters, what should decide it is tax law:
a band that straddles a threshold hands the which-side decision to the
within-band draw, which knows nothing about the record. The check is to list
the thresholds, see which bands they fall in, and split any band where it
matters.

**The rejected alternative.** Dollars in the model in real terms, with the
year coordinate carrying levels. Rejected because the model would have to
extrapolate dollar levels into 2040, which it cannot, and because nominal
drift would be confounded with real change. Ranks keep the model's job to
"how variables relate" and leave "how big the numbers are" to a step that
can be inspected.

### 5.5 Chaining across years

John's question: when post-training for year t+N, start from the pretrained
model, or from the post-trained model for t+N−1?

This is D2 against D13 in model form, and the path-dependence test of
7 October (D18) answers it. With tilting, the λ's add across years. If every
year targets the same cells with that year's values, starting from the
previous year's post-trained model and starting from the pretrained model
give the same answer, because the final λ for each cell is pinned by the
latest target (the test found a maximum difference of 8 × 10⁻¹⁵). If a cell
was targeted in t+N−1 and not in t+N, chaining keeps that adjustment and
starting fresh forgets it (the test: an unemployment share of 14.8% chained
against 6.8% direct, truth 8.0%).

So the choice is: should last year's targets persist where this year is
silent? For projection years, clearly yes; starting 2040 from the pretrained
model would throw away everything the 2023 tables corrected. For observed
years it does not matter if the target structure is consistent, which D18
already requires.

**Decision: chain.** Start from t+N−1's post-trained model; keep the target
list's structure fixed across years; the rolling prior sits underneath as the
reference the chain started from. Two channels for two kinds of information:
surveys enter through the year coordinate and the kernel; targets enter
through the chain of tilts. The order of operations (kernel on the
pretrained year coordinate, then the accumulated tilt) should be written as
one formula so nobody has to rediscover it.

### 5.6 Weights

Two different things go by the name.

**Training weights: how much each record counts when the model learns.**

The problem. The IRS samples nearly every rich return and a thin slice of
ordinary ones, so a top return represents about 1 unit and an ordinary one
about 1,000. About 20,000 of the PUF's 166,156 records are in the top 0.1%
of AGI. Counting records by population weight, those 20,000 together are
0.1% of the training signal and the model never learns what rich returns
look like. That was the mistake of the first week. The SCF has the same
issue by design: its top 0.1% by income is 4% of its records and 0.1% of its
weight. Ignoring weights entirely, the model learns rich returns well but
believes they are 12% of the population.

The proposal (D26, 14 October, untested). Sort records into income groups
(bottom half, 50–90, 90–99, 99–99.9, top 0.1%). Inside a group, use the
survey weights, scaled so the group's records add up to the group's record
count. Across groups, count records. Never put the weight into the model as
a variable. At generation, set how many records go in each group from IRS
counts.

Why it is consistent with the tilt. The PUF is a stratified sample: the IRS
split returns into income strata, sampled each at its own rate, and
attached weights to recombine. The textbook use of a stratified sample is to
estimate within each stratum and combine with the known stratum sizes.
Nobody pools the strata with population weights first. That is all the
proposal is: train within income strata, supply the stratum sizes at
generation from IRS counts. The survey weights stay in use within a stratum
because the IRS also stratified on things inside the groups (presence of
certain schedules), and the within-group weights carry that. The model is
never asked how common top returns are: generation sets the group first and
draws the rest given the group, which the block order enforces.

How sure. Fairly sure of the direction, not of the recipe. Evidence for the
direction: population-weighted training lost the top in every version, on
the PUF and the SCF, for the transformer and the trees; training on the
sample as drawn fixed it in both. Evidence for the specific recipe: none.
Risks named: anything that spans groups, such as how wages relate to age
across the whole distribution, is learned with distorted group proportions,
and if the model's representation of a variable leans on cross-group
structure that could leak; the SCF picks wealthy households on a wealth
estimate not in our data, so within an income group its sample still leans
rich, which the within-group weights handle in part and the SCF's own
wealth-by-income tables, as guard targets, handle in the rest.

A simpler alternative if the group machinery is unwanted: raise every weight
to a power between 0 and 1, sliding between population weighting and none.
Less principled, one number to tune, and it would reveal the same thing.

**Output weights: what Tax-Simulator reads.**

Every generated record starts with the same weight. The number of records in
each income group is set from IRS counts. The tilt gives each record a
multiplier, and that multiplier is its final weight. If a few records end up
with huge multipliers, the model was far from the targets; the spread of
the multipliers is the effective-sample-size check. No decision needed.

Two additions from John, both adopted:

- **Tilt, then redraw.** Once the tilt defines the adjusted population, draw
  a fresh sample from it and give every record the same weight.
- **Oversample the top.** The tilt defines a population; one can draw from
  it any way one likes as long as the weights say how. Draw ten times as
  many top-0.1% records as their population share and give each a tenth of
  the weight. That is a stratified sample, and it is what the IRS did when
  designing the PUF. The payoff is real: a generated file at PUF scale had
  only about 660 records in the top 0.1%, and the top-1% share bounced by a
  point or two between draws from that alone (17.5–19.1% across four draws
  from one model). The file size is now a free choice, and records can be
  allocated wherever Tax-Simulator's answers are most sensitive: the top,
  and near thresholds. Caveat: more draws reduce noise, they do not add
  information; if the model learned the top badly, oversampling gives a
  precise estimate of the wrong thing.

### 5.7 Several surveys

John's remaining unease about training weights was really about the
multi-survey case. How it fits together:

- **Each survey's weights are used only inside that survey.** The rule is
  applied survey by survey. Inside an income group, PUF records are weighted
  by PUF weights rescaled to the number of PUF records there; SCF records by
  SCF weights rescaled to the number of SCF records there. The two surveys'
  weights are never compared, which is right, since they represent different
  populations, in different years, with different units.
- **Groups are defined on something every survey has after translation:**
  income in tax terms, by year. Translation already puts each survey's income
  on the file's scale, so "top 0.1% of income in 2016" means the same for an
  SCF 2016 row as for the file. Groups are year-specific, like the ranks.
- **The balance between surveys only matters for shared variables.** Wealth
  is learned from SCF rows only; itemized deductions from PUF rows only. The
  PUF's 166,000 records against the SCF's 47,000 rows does not affect what
  the model learns about wealth. It affects the shared variables (incomes,
  age, filing status, dependents), and for those translation has already
  forced agreement within cells. What remains is residual disagreement, and
  whoever has more records wins it.
- **The one knob is how much each survey counts in total.** By record count
  now. The CPS, once in with many years, would dominate every shared
  variable. The sensible default is that the authority for a variable
  dominates it: the PUF for tax items, Census surveys for demographics. A
  per-survey, per-variable scaling; one of D26's unsettled items.

**The source label.** John: the network should have label-responsive
predictions; it learns one set of relationships for the PUF, another for the
SCF, another for the CPS, and at inference it is shown a label to pick which
to use: incomes drawn under the PUF label, then the record shown with the
SCF label to draw wealth. This is already how every run works (a check of
every script on 14 October confirmed it). Two kinds of label-responsiveness
have to be told apart:

- *"Wealth given income" responding to the label.* Wanted. Wealth exists only
  on SCF rows, so the SCF label is the only way to ask for it.
- *"Income itself" responding to the label.* Wanted small. Drawing wealth
  under the SCF label hands it incomes generated under the PUF label, which
  is a fair question only if SCF-labelled and PUF-labelled incomes mean the
  same thing. The dividend problem from test 2 is the concrete case: in the
  raw SCF 12% of units report dividends against 19% of PUF returns, because
  the SCF misses small holders, so the model learned that an SCF unit with
  any dividends is fairly rich; a PUF return with $300 of dividends shown
  under the SCF label was read as a dividend holder, therefore rich,
  therefore given too much stock. Translation fixes the SCF's incomes to
  look like tax incomes before training, so the hand-off no longer misleads.

In one line: label-responsive for the variables a survey owns,
label-indifferent for the variables they share. John: in principle the
preprocessing step (translation; he also called it concordance) handles this.

**The check.** After translation, does the model's view of a shared variable
depend on the label? The plan called for measuring this and nobody has. If
the label still moves the answer, translation is incomplete and the survey
balance is doing work it should not.

**Where the evidence is thin.** No run has had more than two surveys. The
SCF has been translated once. The CPS and the non-filer pool have never
been in the training data. This is the least tested piece of the design.

## 6. Question 4: how we know it is right

Claude laid out four layers: whether the pretrained model reproduces
held-out data; whether the post-trained model reproduces a year, checked on
tables not used as targets; whether it projects, by a backtest that builds
through year T with CBO's projections of the time and scores against the
actual later tables; and whether Tax-Simulator cares, by running
tax-simulator-gpu on a generated file and the PUF-based file under current
law and the reform scenarios already in the repository (top rate, SALT, CTC,
capital gains, estate tax, mortgage interest repeal, TCJA extension).

John's criterion is the first two, and he is less worried about this
question than the others: "memorizing the data is the test." Can the model
reproduce all the relationships observed in the surveys in one pretraining,
on held-out records, and can it then be squeezed to hit targets within
reason? "Within reason" already has a rough number: moves under about 1.3
nats fixed the targeted statistics without damage, and moves above about
1.8 started making untargeted things worse (test of 9–10 October).

The backtest and the reform run stay on the list as later checks, not as the
acceptance test. The held-out table list (which tables are never targets)
still has to be decided before fitting and kept fixed across years.

## 7. Carried over, still open

- Itemized deductions: the claimed/paid split works when the amount paid is
  known only on SCF rows; drawing the amount paid for non-itemizers (D20) is
  not done.
- Completed PUF wealth in the top 0.1%: the cause is the starved SCF top;
  the fixes so far get either the top or the lower groups right, not both.
- The thirteen authority-table questions
  (`research/rebuild/tests/probes/AU_AUTHORITY_TABLE.md`): dependents
  (tax-return dependents against household children); cohabiting couples;
  whose age and when; dependent filers, which the SCF lacks; two wealth
  variables that may be mislabelled (`db`, `re_fund`); which pieces to
  translate together (Schedule C, farm, Schedule E); capital gains concept;
  wages and elective deferrals; the population for age targets; earnings
  split bands.
- The target list itself: which tables, which years, which held out, and
  each precision.

## 8. The fallback ladder

If the model-as-population fails at any step, the next rung down is already
shown to work:

1. Model-as-population with tilting (this design).
2. Model-as-population with tilt-then-distill, then direct gradient, then
   policy gradient (section 5.2).
3. Hybrid: generate from the model, calibrate whole-record weights; the tree
   baseline fits here.
4. File-with-helper: real PUF records completed by the model or by trees,
   calibrated. Every data lesson from the experiments carries over.
5. The current pipeline with the linear program replaced by entropy
   calibration (the synthesis's week-one plan).

## 9. Glossary

- **Authority.** The source that measures a variable directly and whose
  concept the file uses: the IRS for tax items, Census surveys for
  demographics, the SCF for wealth.
- **Block order.** Generation in a fixed order across blocks of variables
  (income group, tax variables, wealth, demographics) and a random order
  inside each block. Untested.
- **Guard target.** A target whose job is to hold a region of the
  distribution in place while other targets move things, rather than to add
  information.
- **Label (source label).** A token on every training row saying which
  survey it came from. Set, never predicted, at generation.
- **Nat.** The unit of the divergence between the model and its post-trained
  version. Smaller means less change. Moves under about 1.3 nats per unit of
  weight were harmless in the tests; above about 1.8 they damaged untargeted
  relationships.
- **Rank-dollar map (crosswalk).** For one variable and one year, the sorted
  list of dollar values that turns a rank into an amount. Built from the
  PUF for 2015, from SOI tables for later observed years, and grown forward
  for projection years. Holds all nominal growth and all change in
  inequality within a variable.
- **Rolling prior.** The reference distribution for year t, formed as a
  kernel-weighted mixture of the model's slices for nearby years, one-sided
  at the forward edge.
- **Tilting.** Post-training by multiplying each record's probability by
  exp(sum of λ's for the target cells it falls in), with the λ's solved so
  the targets are hit within their precisions. The minimum-divergence
  adjustment. Same formula as raking; more general in what a target can be.
- **Translation.** The preprocessing step that moves each survey household's
  shared variables to their values in tax terms, one variable at a time
  within cells, keeping its weight and its unshared variables. John also
  calls it concordance.

## Revision history

- 2026-10-14: written from the conversation of that day. Supersedes the
  joint model plan, the design decisions log, the synthesis, the RLHF note
  and the D21 questions, all moved to `research/archive/`.
