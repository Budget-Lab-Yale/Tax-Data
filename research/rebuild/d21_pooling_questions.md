---
title: "Tax-Data rebuild — open questions under D21 (pooled donors)"
role: method
workstream: rebuild
status: open
updated: 2026-10-08
sot: research/rebuild/design_decisions.md
supersedes: []
superseded_by: null
---

# Open questions under D21 (pooled donors)

D21 in `design_decisions.md` decides that imputed variables are drawn
from a pool of survey waves, with several copies per record, before
calibration. This file lists the details still to settle, one at a time,
in the order John and Claude are working through them. Each entry gives
the issue, the choices, Claude's lean, and the status. Answers are
recorded here as they are given, and the settled ones move into
`design_decisions.md`.

Evidence so far:

- **Toy test** (simulated data): recorded in D21.
- **Real-SCF test:** `background/12_pooled_scf_test.md`, summarised in D21.
  - An equal-weight pool helps only for years between waves. At the edges
    (2022 forward, 2013 back) it does worse than the nearest single wave.
  - Calibration does not pick out the right wave on its own.
  - Starting weight on the year's own or nearest wave does most of the
    work.
  - Copies keep weights from swinging but barely change accuracy.
  - Holder counts by income group are the targets that matter.

The questions fall into four groups:

- **A. What goes into the pool:** 1–5.
- **B. How the draws are made:** 6–10.
- **C. Copies and the output file:** 11–13.
- **D. How the years use the pool:** 14–16.

## A. What goes into the pool

### Q1. Which waves — asked 2026-10-07, awaiting answer

The SCF waves on disk are 2013, 2016, 2019 and 2022. 2025 is expected
around the end of 2026, and public waves go back every three years to
1989. Each wave has roughly 4,600–6,300 households.

More waves mean more donors and more kinds of household. That matters
most at the top, where any one wave is thin and the imputation ends up
cloning a few rich households. Older waves describe a more distant
economy. Under the real-SCF result, starting weight is concentrated on
the nearest wave, so distant waves get little weight and mainly add
households that would otherwise be missing. Each added wave costs
harmonizing work (Q2) and tax-unit construction (Q4).

Choices:

- (a) 2013, 2016, 2019 and 2022, plus 2025 when released;
- (b) 2016 onward;
- (c) further back, for depth at the top.

**Lean:** (a). Extend back only if the top is too thin.

### Q2. Making waves comparable — open (work, not a choice)

Pooling assumes a variable means the same thing in every wave.

- The SCF summary extracts (the SCFP files) are built by the Fed to be
  comparable across waves, so the main balance-sheet components are
  probably fine.
- Anything built from the raw question-level data needs checking wave by
  wave: the tax-unit split, income composition, and account allocation
  flags.
- The same issue is larger for the CPS, whose income questions were
  redesigned in 2014.

### Q3. Common dollars — open

Donors from different waves are in different dollars and economies, and
all are imputed onto the 2015 base file.

Choices:

- **(a) Ratios to income.** Impute home value ÷ income and multiply by
  the recipient's income. This keeps each wave's relationship between
  wealth and income. It needs a separate rule at zero or negative income.
- **(b) Deflate levels by a general index** (prices or wages).
- **(c) Deflate each component by the index used to age it.** For
  example, deflate a 2022 donor's stocks back to 2015 by the DFA growth
  rate for that component and income bucket, the same rate the loop later
  uses to age them forward. A copy from a 2022 donor then arrives in 2022
  at exactly its original values, provided the record is in the donor's
  bucket.

Real-SCF test:

- The summary extracts are already in 2022 dollars, adjusted by the
  Fed's price index.
- Using no price adjustment is clearly worse.
- Ratios to income start closer to the truth than price-adjusted levels
  at the edge years, and end about the same in between.
- Neither reproduces the rise in wealth relative to income across waves.
- (c) was not tested.

**Lean:** (c), with ratios to income where a bucket's index is poorly
measured.

### Q4. Tax units for every wave — open (work, not a choice)

The SCF counts households; the PUF counts tax units.
`src/imputations/stage1_scf_tax_units.R` splits households into tax
units, following the Fed's SCF-to-TAXSIM rules. It is written for 2022,
and the pooling prototype ran it on 2019. 2013 and 2016 need it too, with
a check that every question code it reads exists and means the same thing
in those waves.

The alternative is pooling households and skipping the split. The
real-SCF test used households.

### Q5. How much each wave counts in the pool — open

Each wave's survey weights add up to that year's population.

Choices:

- **(a) Equal shares.**
- **(b) A fixed lean** toward the base year when the base is built.
- **(c) Copies drawn wave by wave, with weight moving among them over
  time.** Each record gets copies from every wave. Each year, before
  calibrating, some of a record's weight moves from its older-wave copies
  toward its newer-wave copies. For history this can be a straight
  interpolation between the neighbouring waves. Each record's total weight
  is unchanged, so no income, count or tax target is touched.

Real-SCF test: equal shares do worse than the nearest single wave at the
edges, and calibration does not correct the mix (the target wave's share
stayed at 25–26%). Starting with 60% on the target wave closed most of
the gap.

**Lean:** (c), leaning strongly toward the year's own or nearest wave.

## B. How the draws are made

### Q6. Conditioning variables — open

The imputation draws wealth given variables both the PUF and the survey
have:

- income by source (wages, business, interest, dividends, capital gains,
  Social Security, pensions);
- age;
- marital status;
- dependents.

Interest and dividends are the strongest predictors of wealth. The
definitions differ: the SCF asks about the previous calendar year's
household income in an interview, while the PUF has tax-unit income from a
return. Ranks within each source are safer than dollar levels.

A second choice is whether the survey wave is itself a conditioning
variable. If it is, each copy comes from a named wave, as in Q5(c). If not,
a record's copies are a random mix of waves.

**Lean:** condition on ranks within each source, and draw copies wave by
wave.

### Q7. One set of copies for all imputed variables — open

A copy has to be one coherent household, because Tax-Simulator reads each
record as a unit. Separate copies for each variable would multiply the
file size, or put wealth and consumption on different records.

So each copy carries all its imputed variables, drawn in a chain:

1. wealth from the SCF given the PUF variables;
2. consumption from the CEX given the PUF variables and, where the CEX can
   support it, that copy's wealth;
3. mortgage interest and property tax given that copy's home value and
   mortgage debt.

Variables observed on the PUF, such as Schedule A amounts for 2015
itemizers, are the same on every copy.

**Choices:** whether to adopt one copy set and a chain, and the order of
the chain.

**Lean:** one copy set, in the order wealth, then consumption, then
itemizable expenses.

### Q8. The imputation model and survey weights — open (can be decided per variable)

The candidates are DRF, quantile forests, or a hot deck within cells.
Choose by holding out donors and comparing each method against a simple
baseline.

Survey weights must count. Today `drf` ignores them, and multi-threaded
quantregForest drops them. There are two ways to include them:

- resample donors in proportion to weight before fitting;
- or find neighbours, then draw among them in proportion to weight.

The real-SCF test used a weighted hot deck.

### Q9. Circular dependencies at entry — open

Some imputations use weighted quantities:

- consumption is matched by weighted income rank;
- wealth-aging buckets are weighted ranks;
- the non-filer pool is sized as population minus weighted filers.

At the 2015 base this is mild. The pre-calibration weights are the PUF's
own sampling weights, which represent 2015 filers. Calibrating filers and
non-filers together removes the need to size the pool as a residual.

**Lean:**

- accept ranks from the PUF weights and check the difference once;
- move non-filers to joint calibration (open question 8 in
  `design_decisions.md`).

### Q10. The top of the distribution — open (with open question 9)

The SCF excludes the Forbes 400 by design. The Forbes splice builds
billionaire records from PUF top-tail pieces, and the PUF's aggregate top
records are dropped today. Pooling gives roughly double the donors at the
top, so fewer rich households are cloned. Forbes records have known net
worth, so they may need one copy, or copies that vary only in
composition.

## C. Copies and the output file

### Q11. Number of copies — open

In the toy test, five copies cut the largest record-level weight change
from 3.3× to 2.0×, and twenty cut it to 1.6×.

In the real-SCF test, copies cut the share of records moved outside
0.5–2× (from 2–7% with one copy to 0–2% with twenty), but barely changed
accuracy. The largest single changes, 3–14× even with twenty copies, came
from thin table cells and appear in the benchmark too.

The number can vary by record. A low-income unit with little wealth gains
almost nothing from extra copies. A top-1% unit gains a lot, because its
wealth is variable and drives estate, capital gains and SALT provisions.
Monitoring should track how weight spreads across each record's copies.

**Lean:** vary by income, with something like 1–2 copies at the bottom and
10 or more at the top.

### Q12. What leaves Tax-Data — open

About 200,000 filer records plus non-filers, times the number of copies,
is what Tax-Simulator reads each year. Its run time grows roughly in
proportion.

Choices:

- **(a) Ship all copies.** The most accurate, and smoother wherever a
  provision depends on imputed variables.
- **(b) One copy per record at output.** Chosen with probability
  proportional to its weight and given the record's full weight. This is
  right on average, but adds noise and no longer hits the imputed-variable
  targets exactly. Using the same random number per record every year
  keeps a record's balance sheet from jumping between years.
- **(c) Both files:** all copies for distributional and wealth-sensitive
  work, one copy for quick runs.

The answer depends on what Tax-Simulator's run time can absorb.

### Q13. Copies for added records — open (mostly work)

Non-filers from the CPS and any top-tail records need wealth, consumption
and so on. They follow the same scheme, with conditioning variables from
the CPS.

## D. How the years use the pool

### Q14. Targets rich enough to recover each wave — open

Calibration reshapes only what targets constrain. Elsewhere the starting
mix stands.

- **Only counts and totals can be targets.** Medians are not sums over
  records, so they cannot enter linear calibration directly. A median can
  be turned into a count, such as "households in this income group with
  net worth above $X".
- **Precision can come from the SCF itself.** Its 999 sets of replicate
  weights give each target's sampling error.

Real-SCF test:

- Holder counts by income group matter most. Without them, holding rates
  by decile were 45–95% further off.
- Totals alone do little.
- No table by income group fixes patterns across assets, such as
  homeownership among stockholders; those come from the donors unless a
  joint table targets them.
- With precisions set to the SCF's sampling error, calibration moved
  little.

**Choices:** which cells to use, including whether to add joint cells
across assets.

### Q15. Years between waves — open

SCF targets exist every third year. Under D18, a dimension calibrated
only in some years keeps its adjustment afterwards, while aging and other
targets let wealth drift between waves.

Choices:

- **(a) DFA targets every year,** with SCF tables as extra, more precise
  evidence in wave years. The DFA scales SCF totals to the Financial
  Accounts, so DFA and SCF disagree on levels in wave years, and one of
  them has to be the level concept.
- **(b) Interpolate SCF tables between waves.** For history, both
  neighbouring waves are known.
- **(c) Carry the last wave's tables forward,** aged, with looser
  precision.

**Lean:** (a), with DFA levels as the level concept, since they are
consistent with the national accounts used elsewhere.

### Q16. Aging between waves, and projection — open

Between waves, imputed values grow by DFA growth rates by component and
income bucket. The bucket comes from PUF income, which every copy shares,
so all copies of a record grow by the same factors.

After the last wave there is no new survey evidence. Only households
already in the pool exist, growth comes from the macro projections, and
the starting mix stays where the last wave left it.

**Choice:** confirm that projection years add nothing beyond aging and
any CBO-based targets.

## Answers

None recorded yet.
