---
title: "Tax-Data rebuild — design decisions"
role: method
workstream: rebuild
status: open
updated: 2026-10-14
sot: self
supersedes: []
superseded_by: null
---

# Design decisions

This file records the decisions John has made about the shape of the
rebuild, and the questions still open. The background research lives in
`research/rebuild/background/`. Its summary is
`research/rebuild/synthesis.md`. The problem statement is
`research/rebuild/BRIEF.md`.

**Read `joint_model_plan.md` first.** As of 9 October 2026 the rebuild is
organized around one joint model trained on all surveys (D22), with every
survey translated into tax concepts before training (D23), survey weights
trusted and differences in meaning translated household by household (D24).
The plan states these in one place and lists which earlier decisions they
replace.

Each decision has a status:

- **Decided:** John has agreed it.
- **Proposed:** suggested but not yet discussed.
- **Open:** not yet decided.

## The overall structure

### D1. One best estimate per year — decided (2026-10-06)

The goal is a single best microdata file for each year. A distribution over
possible files, as in a full Bayesian treatment, is not needed.

### D2. Age, then calibrate, then repeat — decided (2026-10-07)

The pipeline runs as a loop over years:

1. Start from the calibrated file for year t.
2. Age every record's dollar values from t to t+1.
3. Reweight the aged file so it hits year t+1's targets.
4. The result is the calibrated file for t+1. Repeat.

Information therefore carries forward. A correction made to fit 2016's
tables persists into later years until a later target overrides it.
Projection years use the same loop with fewer targets, so the aged file
carries more of the answer.

### D3. Entropy calibration — decided (2026-10-07)

Each year's reweighting chooses new weights that hit the targets while
staying as close as possible to the aged file's weights. Closeness is
measured by relative entropy:

```
Σᵢ  wᵢ log(wᵢ / bᵢ) − wᵢ + bᵢ
```

Here bᵢ is a record's weight after aging and wᵢ its new weight. Raking is the
special case where every target is a count on one margin.

Targets can be exact, met within a tolerance band, or soft. A soft target
can miss by an amount set by its stated precision. Which form each target
takes is decided target by target.

### D4. Aging carries the best guess of distribution shape — decided (2026-10-07)

The aging step does not grow every record by one factor per variable. It
uses the best available evidence on how growth differs across the
distribution:

- wages: SSA growth by wage percentile;
- other income: SOI average amounts by AGI bin;
- projection years: CBO growth rates, by percentile where CBO provides it.

This leaves the reweighting to correct only departures from the expected
growth. If aging is too crude, reweighting meets income growth by shifting
the file's composition toward high earners, which distorts every variable
correlated with income.

### D5. Values carry amounts; weights carry participation — decided (2026-10-07)

Growing values changes how much each recipient has. It cannot turn a zero
into a positive amount. Changes in how many people have a type of income,
file jointly or itemize therefore come through the weights. Paired variables
grow by the same factor so that identities such as
`wages1 + wages2 = wages` hold exactly.

### D6. Demographic aging through weights only — decided (2026-10-07)

Record ages stay fixed. Changes in the population's age structure come
from reweighting to population-by-age targets. The rebuild does not
simulate life events such as aging a year, marriage, divorce, children
growing up or death, and has no transition models.

### D7. No generated records — decided (2026-10-07)

**Under review (2026-10-09).** John: the expected end state is records drawn
from the joint model from scratch, or close to it (D22), with the final
calibration done on whole-record weights. Test 3's from-scratch arms
(`tests/posttrain/`) measure what that takes. D7 stands until those
results are in.

The rebuild does not generate synthetic records from a model. Every record
traces back to a source record (the PUF, or the non-filer pool).

**Exception held in reserve:** splitting a record whose weight has grown
very large into several copies with slightly varied values. This is a
remedy for weight concentration (see D8), to be used only if the
monitoring shows it is needed.

### D8. Track weight concentration every year — decided (2026-10-07)

Weights spread out over a long chain of years, for two reasons.

- **Noise accumulates.** Independent year-to-year adjustments compound like
  a random walk.
- **Trends repeat.** The gaps between the aged file and reality are partly
  persistent trends, so the same records are pushed the same way every
  year. Population aging, carried by weights under D6, is the clearest
  case.

Every year's run reports the effective sample size, overall and by cell,
and each record's cumulative weight ratio against its original weight.
Caps on the cumulative ratio, and the splitting remedy in D7, are available
if needed.

### D9. Errors persist too — decided (2026-10-07)

A bad target in one year carries forward like a good one; a pandemic year
is the main risk. The protections are soft targets with honest precisions
and a check against held-out tables in every year.

### D10. Imputed variables enter in their donor year — superseded by D21 (2026-10-07)

An imputed variable is attached in the year its donor survey describes.
For example, wealth from the 2022 SCF is imputed onto the 2022 calibrated
file. From then on it is aged and reweighted with everything else.

D21 replaces this. Imputed variables are drawn from a pool of all survey
waves when the base file is built, and each wave enters as targets in its
own year.

## Practical scope

### D11. The current pipeline's bugs are not a priority — decided (2026-10-07)

The bugs found in the current code (state and local tax arguments, the
consumption bootstrap, the auto-loan cap) are not being fixed in the
current pipeline. The imputation weighting problems are fixed in the new
version instead. These are: multi-threaded quantile forests dropping survey
weights, `drf` ignoring weights, and equal-probability leaf draws.

### D12. JAX is acceptable — decided (2026-10-06)

A port to JAX is acceptable. GPUs are available. The weights-only
calibration is small enough to run on a CPU in seconds, so the GPU matters
mainly for any later step that moves values inside the optimization.

### D13. Records do not have to persist across years — decided (2026-10-06), narrowed by D2

On 2026-10-06 John allowed separate files per year with no common records.
Under the loop in D2, records do carry forward from year to year with
changing weights. D13 still permits a break, such as a new base file or
added non-filer pool records, where one is needed.

**Confirmed 2026-10-08:** Tax-Simulator needs no record-level link across
years (no basis, carryforward or history carried between years' files).
Each year can therefore be built on its own, from the base file plus the
expected change to that year, rather than through the chain in D2. Under
D18's conditions the two give the same answer. Which to use is open.

### D14. Weight caps — decided provisionally (2026-10-07)

- **Per year.** Each year's adjustment factor is capped between 0.5 and 2.
  These numbers are placeholders, to revisit once real runs exist.
  Year-to-year adjustments should normally be a few percent, so a record
  reaching a cap signals a problem with a target or with the aging step.
- **Cumulative.** The change from each record's 2015 weight is reported
  every year but not capped. Population aging alone should raise some
  records' weights several-fold by mid-century.
- **Exact targets.** With caps in place, some combinations of exact
  targets may become unreachable. Every exact target therefore needs a
  fallback form, a band or a soft version.

### D15. A hand-written target list — decided (2026-10-07)

All targets are written by hand in one list. Each entry gives:

- the year or years it applies to;
- what is measured: a single variable, or a cell in a joint distribution
  (for example, returns with capital gains by AGI bracket and filing
  status);
- the value and its source;
- its form: exact, a band, or soft with a stated precision;
- whether it is fitted or held out as a check.

Precisions are set by hand.

Vocabulary: in this design the prior is the aged file, and the targets are
the evidence that corrects it.

### D16. The loop starts in 2015 — decided (2026-10-07)

The base is the 2015 PUF. Proposed first step: calibrate the 2015 PUF, with
non-filers added, to the 2015 SOI tables before aging to 2016, since the PUF
as processed does not reproduce the published tables exactly (for example,
the aggregate top records are dropped).

### D17. Evaluation runs mostly through the target list — decided (2026-10-07)

- **Fit.** The misses on fitted targets, against their tolerances.
- **Out-of-sample checks.** Targets flagged as held out, computed in every
  run.
- **Backtests.** Run the loop with every target after year T switched to
  held out, then score the later years.
- **Outside the list.** The downstream Tax-Simulator comparison, and the
  weight-concentration tracking from D8.

### D18. Keep the target list's structure consistent across years — decided (2026-10-07)

**The concern.** Because the loop in D2 starts each year from the previous
year's file, unusual years such as 2020 and 2021 could leave traces in later
years, including projections.

**Weights.** Under entropy calibration, a record's weight after a chain of
years is its base weight times exp(Σ over years of λₜ·aᵢ). If every year
targets the same quantities (same variables and cells, with different
values), the exponents add up. The chained result is then identical to a
single calibration from the base file straight to the final year's
targets, so intermediate years leave no trace.

If a year has a target that later years drop, that year's adjustment is
never undone. Later years do not target that dimension, and
minimum-change calibration does not move what is not targeted.

**Values.** Values are path-independent as long as each year's growth
factor is a ratio of observed levels. The factors then multiply out to the
total change between the end years.

**Test (2026-10-07).** The test used 5,000 records, with age and income mix
targeted every year. Unemployment receipt was 30% in 2020, 15% in 2021 and
8% in 2022. An untargeted variable was correlated with unemployment.

| Scenario | Chained vs direct weights | Unemployed share in 2022 file (truth 8.0%) | Untargeted variable mean |
|---|---|---|---|
| Unemployment targeted every year | identical (max difference 8 × 10⁻¹⁵) | 8.0% | 0.115, same as direct |
| Unemployment targeted only in 2020–21 | differ by up to 125% | 14.8% chained, 6.8% direct | 0.210 chained, 0.099 direct |

The script and output are in
`/nfs/roberts/scratch/pi_nrs36/jar335/path_dep/`. That is scratch space and
may be purged.

**Rule.** A target added for an unusual year must also appear, with normal
values, in the years before and after it. A dimension that can be measured
only in an odd year is either left out or carried forward.

**Approximations.** Real runs are only approximately path-independent,
because of:

- residual misses on soft targets;
- binding caps (D14);
- aging moving records between dollar cells.

These effects are expected to be small but have not been tested.

**Projection start.** Projection years start from the last calibrated
year, which is 2023 and looks normal. If the last observed year is ever
unusual, the loop can start projections from a chosen year's file instead.

### D19. Targets on reported amounts go through a tax calculation — decided (2026-10-07)

**What Tax-Data holds.** Tax-Data holds the joint distribution of
underlying ("universe") variables only: mortgage interest paid, charitable
giving, state and local taxes paid, and so on, whether or not they appear on
a return. It does not store reported amounts. What appears on a return is an
outcome that Tax-Simulator models downstream.

**What a target measures.** A published target often measures a reported
amount rather than an underlying one. What a return reports is a function of
the underlying values and that year's tax law. For example, after 2017,
Schedule A mortgage interest is reported only by those who itemize under the
higher standard deduction. In general a target is

```
Tₖ ≈ Σᵢ wᵢ · hₖ(xᵢ ; law_t)
```

where xᵢ is a record's underlying values and hₖ is what the record would
report for target k under year t's law. For most targets hₖ is the value
itself, or 1 if the record is in the target's cell. For targets on reported
tax amounts, hₖ is a tax calculation: who itemizes, the SALT cap, the
capital-loss limit, the taxable share of Social Security, the 2020
unemployment exclusion, the 2025 law's capped deductions, and so on.

**How it fits the loop.** Each year of the loop in D2 becomes:

1. Age values from t to t+1.
2. Apply the tax calculation under year t+1's law to every record. This
   produces the reported quantities that year's targets measure.
3. Calibrate weights to year t+1's targets. Each target uses either the
   underlying value or the computed reported quantity, as its definition
   requires.
4. Discard the computed reported quantities. The output file carries only
   underlying values.

**Why it stays simple.** Values are fixed during calibration (D4), so each
hₖ(xᵢ; law_t) is a fixed number per record. Every target is still linear in
the weights. The solver is unchanged, and the tax calculation does not need
to compute derivatives.

**Which calculator.** The tax rules should match Tax-Simulator's (or
tax-simulator-gpu's), so that the file is calibrated to reproduce SOI under
the rules the downstream model applies. This couples the two repositories,
so Tax-Data must pin a calculator version.

**The default.** Most targets assume that what is reported equals the
underlying amount, because there is no practical alternative. An explicit
tax calculation is used where the law changes what is visible in a way that
matters for a target.

### D20. Impute underlying itemizable expenses for 2015 non-itemizers — decided (2026-10-07)

The 2015 PUF records Schedule A detail only for returns that itemized in
2015, roughly 30% of returns. The current variable guide defines these
variables as amounts "for itemizers": `med_exp`, `salt_inc_sales`,
`salt_prop`, `salt_pers`, `int_exp` and the mortgage variables, `char_cash`,
`char_noncash`, `inv_int_exp` and the miscellaneous lines. Everyone else
carries zeros, so the 2015 base holds only the observed part of the
universe.

These expenses are imputed for 2015 non-itemizers when the 2015 base is
built, before the loop starts.

Why they are needed:

- **Current law.** The 2025 law's charitable deduction for non-itemizers
  applies from 2026.
- **Reforms.** Any reform that moves people across the itemizing threshold
  depends on the expenses of people near it.
- **Calibration.** D19's tax calculation needs underlying expenses for
  every record near the threshold.

The 2015 return still carries information about non-itemizers: they chose
not to itemize. Roughly, their itemizable expenses after floors and limits
were below their standard deduction, though this is not exact, since some
who would have gained did not itemize. So:

- **2015 itemizers** keep their PUF amounts, which are their universe
  values.
- **2015 non-itemizers** get draws from survey distributions conditional on
  that upper bound.

Candidate sources:

| Expense | Candidate source |
|---|---|
| Mortgage interest and property tax | SCF, ACS (property tax and mortgage payments for all homeowners), CEX |
| Charitable giving | CEX, PSID philanthropy module |
| State and local income tax | Needs state of residence, which the 2015 PUF lacks. The existing DINA-based tax-rate imputation is a starting point. |
| Medical expenses | MEPS, CEX |

Which PUF fields are recorded before or after floors and limits (for
example, whether medical expenses are before the AGI floor) has not been
checked.

### D21. Impute before calibrating, from pooled survey waves, with several draws per record — decided (2026-10-07)

**Three operations, in this order within a year:**

1. add records (non-filers, the top of the distribution);
2. impute variables the PUF lacks or censors;
3. calibrate.

Imputed variables therefore exist when the weights are set. Targets on
them (wealth by income group, PCE totals) can move the weights. They can
also feed tax-calculated targets (D19), such as itemizer counts.

**The concern this answers.** If the donor is a single survey wave, for
example the 2016 SCF, then kinds of household that first appear in later
waves are never on the file. Calibration can change how much each record
counts, but it cannot create a balance sheet nobody has.

**Pooled donors.** Pooling is firm (John, 2026-10-07). The details listed
under "To be settled" are open. The donor pool for each imputation combines
every available wave of the survey. For wealth, the waves on disk are SCF
2013, 2016, 2019 and 2022; 2025 will be added when it is released.
Every kind of household observed in any wave can then appear on the file.
Each year's calibration decides how much of each kind there is, using that
year's targets: the SCF wave in wave years, and the Distributional
Financial Accounts quarterly.

**Several draws per record.** Each record gets several copies. The copies
share every PUF variable but carry different draws from the pool, and the
record's weight is split among them. Moving weight between copies of the
same record changes, say, the wealth distribution at a given income. It
leaves every income, count and tax target untouched. Without copies, the
only way to get more of a balance-sheet type is to upweight whole PUF
records, which drags their incomes along.

This is fractional imputation in the survey-statistics literature (Kim and
Fuller's fractional hot deck is the usual reference; to be checked before
relying on its details). It gives continuity across years, because records
keep their copies. It also behaves like re-imputing every year, because
each year's calibration can shift weight among the copies.

A variant gives each record one or more copies per wave, with starting
weights leaning toward the wave nearest the year being built. With no
targets on the variable in a given year, the file then defaults to the
nearest wave rather than to the pooled average.

**Imputation happens once, at entry.** Imputation happens when the base
file is built, or when new records join, such as a new non-filer pool year.
After that, imputed variables are aged and calibrated like everything else.
New survey waves enter as targets and as growth rates, not as new draws.

**Relation to other decisions.**

- **D7.** Copies trace to source records, so D7 holds. Splitting moves from
  a reserve remedy to standard practice for imputed variables.
- **D18.** Wave targets appear only in wave years, so each wave's
  adjustment persists until the next one. That is probably right for the
  newest evidence, but it is a known instance of D18's residue.
- **D5.** Calibration fixes participation and mix. Level gaps (imputed
  consumption 15% low everywhere) are fixed in values.

**To be settled.** The open details are in `d21_pooling_questions.md`,
numbered Q1–Q16, with the choices, a lean and the status of each:

- **What goes into the pool:**
  - which waves (Q1);
  - making waves comparable (Q2);
  - common dollars (Q3);
  - tax units for every wave (Q4);
  - how much each wave counts (Q5).
- **How the draws are made:**
  - conditioning variables (Q6);
  - one set of copies for all imputed variables (Q7);
  - the imputation model and survey weights (Q8);
  - circular dependencies at entry (Q9);
  - the top (Q10).
- **Copies and the output file:**
  - number of copies (Q11);
  - what leaves Tax-Data (Q12);
  - copies for added records (Q13).
- **How the years use the pool:**
  - targets rich enough to recover each wave (Q14);
  - years between waves (Q15);
  - aging and projection (Q16).

**Real-data test (2026-10-07).** Report:
`background/12_pooled_scf_test.md`; scripts in `tests/pooled_scf/`.

The setup:

- SCF households, not tax units, in the waves 2013, 2016, 2019 and 2022.
- Half of one wave serves as the recipients, with their balance sheets
  held out.
- The other donor sets are a single other wave, an equal-weight pool of the
  other waves, and an equal-weight pool that includes the target wave's
  other half. The benchmark is the target wave's other half alone.
- 84 targets: holder counts and totals by income group and component.
- 10 random splits.

The real data are less kind than the toy:

- **Pooling helps only between waves.** For 2019, the pool of other waves
  beat the nearest single wave. For 2022 (looking forward) and 2013
  (looking back), the equal-weight pool did worse than the nearest single
  wave. Every projection year is at the forward edge.
- **Calibration does not find the right wave inside the pool.** With the
  target wave's other half in an equal-weight pool, its donors held 25% of
  the weight before calibration and 25–26% after. Starting with 60% on the
  target wave closed most of the gap to the benchmark.
- **Patterns across assets that no target describes stay where the donors
  put them.** In 2022, homeownership among stockholders was 80.4%. After
  calibration, it was 7.5 points too high with 2016 donors, 3.7 points too
  high with 2019 donors, and 1.2 points too high with the benchmark.
- **Copies control weights, not accuracy.** They cut the share of records
  moved outside 0.5–2×, but barely changed accuracy.
- **Holder counts by income group are the targets that matter.** Totals
  alone do little.
- **Precisions.** With precisions set to the SCF's sampling error,
  calibration moves little, so the donors and starting weights decide the
  result.

What this changes:

- The pool's role is mainly to widen the range of households available.
- The starting weights across waves (wave-by-wave copies, leaning strongly
  toward the year's own or nearest wave) do most of the work.
- Calibration alone does not do it.

**Test (2026-10-07).**

The setup:

- 5,000 recipient records carry income only.
- Two donor "waves" of 3,000 each carry home ownership, home value, stock
  holding and stock amount.
- Wave 2 has higher stock participation. It also has a kind of household
  absent from wave 1: middle-income renters with stocks worth 3 to 8 times
  income (8% of units).
- The truth is wave-2 balance sheets at the recipients' incomes.
- Targets, by income quartile: number of owners, total home value, number
  of stock holders and total stocks (0.5% precision).
- Imputation is a nearest-income hot deck. Calibration is soft entropy
  calibration.
- Averages over 20 replications. All the statistics below are untargeted.

| Method | Max target miss | New-type share (truth 8.0%) | Stocks by income decile, mean error | Income decile shares, max distortion | Largest record weight change | Records outside 0.5–2× | Effective sample size |
|---|---|---|---|---|---|---|---|
| Wave 1 only, 1 draw | 11.5% | 0.0% | 39% | 36% | 85× | 17% | 16% |
| Wave 1 only, 5 draws | 9.9% | 0.0% | 38% | 36% | 27× | 11% | 45% |
| Pooled, 1 draw | 0.5% | 7.1% | 14% | 4.4% | 3.3× | 1.1% | 95% |
| Pooled, 5 draws | 0.5% | 7.2% | 12% | 4.1% | 2.0× | 0.01% | 99% |
| Pooled, 20 draws | 0.5% | 7.2% | 11% | 4.0% | 1.6× | <0.01% | 99% |
| Per wave 3+3, 80% prior on wave 2 | 0.3% | 7.8% | 11% | 2.5% | 1.5× | 0% | 100% |
| Wave 2 only, 5 draws (benchmark) | 0.3% | 8.0% | 12% | 2.2% | 1.4× | <0.01% | 100% |

The effective-sample-size column is computed on record-level weights,
summing over each record's copies, relative to the number of records.

What the test shows:

- A single-wave donor cannot reach the later distribution. Calibration
  misses targets by about 10% even when they are set near-exact. It
  distorts the income distribution by up to 36% and produces weights up to
  85 times their starting value. Extra draws from the same wave do not
  help.
- Pooling does most of the work. It hits the targets, recovers most of the
  new kind of household (7.1% against 8.0%, starting from about half that
  by construction), and keeps weights near their starting values.
- Copies mainly protect the PUF side of the file. Going from one draw to
  five cuts the largest record-level weight change from 3.3× to 2.0×. It
  brings records outside D14's 0.5–2× range from 1% to almost none.
- Leaning the starting weights toward the nearest wave comes close to the
  benchmark.

Limits of the test:

- Income is the only shared variable.
- Recipients and donors have the same income distribution.
- The targets measure the dimension that changed.
- The stocks-by-decile error has a noise floor of about 11–12%, which the
  benchmark also shows.

The script and output are in
`/nfs/roberts/scratch/pi_nrs36/jar335/pooled_donor/`. That is scratch space
and may be purged.

### D22. One joint model trained on all sources as the reference — decided (2026-10-09; proposed 2026-10-08)

A single transformer model learns the joint distribution of tax-unit
variables from every source at once: the PUF, SCF waves, CPS, CEX and
others. Each source observes only some variables, and the unobserved ones
are left out of training (masking). Source and year are inputs.

Its role is the reference distribution, the equivalent of a pretrained
language model. Calibration of weights to year t's targets is the
equivalent of post-training (see `rlhf_analogy.md`).

First use: imputation. Real PUF and non-filer records are completed by the
model, conditional on their observed variables, so D7 holds. Generating
whole records is a later step, if any. If adopted, this replaces D21's
pooled donors and copies; several completions per record play the role of
copies.

**Revised direction (John, 2026-10-09).** Imputation onto real records is the
first test, but the expected end state is generating whole records from
scratch or close to it, then calibrating whole-record weights to targets on
every variable, the PUF's included. Test 3 (`tests/posttrain/SPEC.md`)
measures how far post-training must move the model to reproduce its own
training data exactly, for completion and from-scratch generation.

Tests, in order:

1. **PUF only.** Hold out part of the PUF. Measure how far the model's
   generated records are from the held-out records, on multi-variable
   tables, the zero patterns and the top. Two training-sample draws give
   the noise floor, the closest any method could get. The question is
   whether the model is close enough to that floor, not whether it beats
   it. Also check that it does not reproduce training records.
2. **Joint imputation.** Train on the PUF plus SCF with masking. Impute
   wealth onto held-out SCF tax units and compare with the current forests
   and with a sequential decision-tree imputation of the same variables.

### D23. Translate every survey into PUF concepts before training — decided (2026-10-09)

**This is the central principle for how the joint model (D22) is built.**

Every survey row that enters the model is first translated into PUF
concepts: the same definitions, the same way of measuring, and the same
population as the tax data. The translation is done in preprocessing, as
explicit rules and models written down per survey, not left to the network
to work out through a source label. Wealth, consumption and other variables
the PUF lacks keep their own survey's concept (the SCF is the authority for
wealth).

Why: the model learns, for example, wealth given incomes from SCF rows. If
those incomes are SCF-measured, the relationship does not transfer to a PUF
record, whose incomes are tax-measured. Test 2's mismatch is the example: 19
percent of PUF returns report dividends against 12 percent of SCF units, so
a PUF return with $300 of dividends is read as an SCF dividend holder, who
is richer, and gets too much stock wealth. Rank encoding (test 2) translates
levels and scale only; it does not translate who has an amount, money
reported under the wrong heading, random reporting error, or population.

**The steps, for each survey:**

1. **Definitions.** User-written arithmetic rules that build PUF-concept
   variables from the survey's items (e.g. SCF business = Schedule C plus
   Schedule F; a new PUF-side variable where no PUF line matches, as total
   Schedule E). Fully visible; assumes the rules are right.
2. **Population.** Revised the same day by D24: survey weights are trusted
   and change only for coverage (who is in scope); differences in what the
   shared variables mean, demographics included, are translated, not
   reweighted. See D24.
3. **Measurement, per shared variable.**
   - Line up the shares at zero, positive and negative with the PUF's for
     the survey's year, by flipping records. The records flipped are the
     **most likely** ones given everything in the row, wealth included (an
     SCF unit reporting no dividends but holding $200,000 in stocks is a
     likely hidden holder), never random ones: random flips put dividend
     holders on households with no stocks and weaken the very relationship
     being learned. Deletions likewise remove the least likely holders, after
     checking the gap is not definitional (step 1).
   - Map amounts among holders by rank.
   - Later: random reporting error, by drawing several PUF-concept values
     per row from a noise model (without it, reported noise passes into the
     model and weakens measured relationships).
   The general form: for each row, draw plausible PUF-concept values from
   their likely values given the whole row. The flip-and-rank version is its
   simplest special case.
4. **Encoding.** Year-specific ranks, as now, so that year-to-year changes
   in dollar levels come from each year's own distributions and targets.
5. **Unshared variables** stay as the survey records them.

**Assumptions it rests on** (to be tested, not taken as given):

- Given the true tax-return amounts, how a respondent answers does not
  depend on their wealth (non-differential measurement error).
- After step 2, remaining differences in who reports an amount are
  misreporting.
- Misreporting among holders preserves order (until a noise model is added).
- The likelihood model used to choose flips is right.

**What it does not solve:** relationships no survey observes together
(partnership income and the split of wealth into stocks and bonds stay
linked only through shared variables; bridge variables such as mortgage
interest observed for PUF itemizers add links); and checking itself, since
after translation the shared variables match the PUF by construction.
Checks must come from outside: test 2's distorted SCF datasets (truth
known), published studies linking surveys to tax records, and whether
completed PUF wealth, passed back through the SCF's way of measuring,
reproduces the SCF's wealth tables.

**Step 2 and the post-training targets.** Reweighting the SCF changes its
unconditional wealth distribution, which post-training also targets. These
do not conflict once each is assigned its job:

- The model learns wealth *given* the variables it conditions on.
  Reweighting on those variables changes the mix of households, not the
  relationship, so step 2's weights matter for training mainly through
  step 3 (they define the gap to translate).
- An unconditional SCF table describes the SCF's population. Where that
  differs from the file's population (filers only, dependent filers, the
  very top), the raw SCF table is the wrong target. The right target is the
  SCF's information carried onto the file's population: SCF shares within
  groups times the file's group totals (as test 3's wealth targets are
  built), or the SCF under step 2's weights.
- Raw unconditional totals are targeted only where the two populations
  coincide: wealth of all U.S. tax units, filers and non-filers together,
  with the Forbes adjustment, against the SCF's household total.
- If post-training still needs a large move to hit those targets, that is a
  finding: the population or measurement step is off, or the sources truly
  disagree. Today's production tilt (Stage 3 of `wealth.R`) bends wealth
  given incomes to absorb exactly this kind of gap; under D23 the gap is
  first explained, and only the remainder is calibrated away.

### D24. Trust survey weights; translate households into tax concepts — decided (2026-10-09)

Refines D23. The shared variables X are the whole bundle a survey and the
file have in common: filing or marital status, dependents, age, and every
shared income item. Each has a survey meaning and a tax meaning, differing
by definition, measurement and frame (marital status is not filing status;
the SCF's head is not the PUF's primary filer). The file's own X is tax
items from the IRS plus demographics imputed from the CPS or ACS.

**The rule.** A survey's weights are trusted: they change only for
coverage (who is in scope, e.g. the Forbes 400, filers against
non-filers). A disagreement between a survey's weighted distribution of X
and the file's is read as a difference in *meaning*, and is removed by
moving each survey household to its X in tax terms, keeping its weight and
its unshared variables (wealth) unchanged.

**Why not reweight.** Reweighting and translation both make the X
distributions agree, but they explain the gap differently and preserve
different things:

| | Reweighting | Translation |
|---|---|---|
| Explanation of the gap | the survey has the wrong number of each kind of household | the survey labels the same households differently |
| Keeps | wealth given survey-measured X | the survey's weights and every unconditional distribution (e.g. SCF wealth) |
| Changes | the survey's unconditional distributions, wealth included | wealth given X, now given tax-measured X |
| Side effects | every variable on a reweighted record moves with it | limited to where households sit in X |

Reweighting to fix a difference in meaning says the survey's weighting is
wrong, which for the SCF is not credible. Translation is an explicit
function from survey concepts to tax concepts, which can be inspected and
checked against definitional rules and linked survey–tax studies.
Reweighting does the same job implicitly. Both are ways of matching two
distributions: reweighting changes how much mass sits at fixed points,
translation moves mass between points. The cause decides which: meaning →
translate; coverage → reweight.

This dissolves the conflict behind the production tilt. "PUF X, SCF
wealth overall, SCF wealth given X" cannot all hold only if X means the
same thing in both. With translation, the SCF's overall wealth and the
file's X both hold exactly, and wealth given tax-measured X is whatever the
translation implies. The tilt reaches a similar place by reweighting wealth
draws within X under an exponential form; translation does it by relabeling
households under an order assumption.

**The algorithm (per survey, per year).** Translate one variable at a time
in a fixed order, each within cells of the variables already translated,
matching the file's conditional distribution at each step:

1. Marital status to filing status: a probabilistic crosswalk (e.g. a married
   SCF couple to joint or separate filing; an unmarried parent to head of
   household or single), probabilities depending on age and children, set
   so the translated mix matches the file's.
2. Age: within filing status, near identity plus the head versus primary
   filer adjustment.
3. Dependents: within filing status and age.
4. Wages: by rank, within filing status by age group.
5. Each further income item: by rank with zeros included, within cells of
   everything already translated, zeros flipped by likelihood (D23).

Several draws per household, since the categorical steps and the zero flips
are probabilistic.

**Assumptions:** within a cell, households keep their order (a household
ranking higher on reported dividends ranks higher on tax-return dividends);
the crosswalk probabilities and the chain's order and cells are right;
cells are coarse enough for the survey's sample (about 6,000 SCF households a
wave).

**Safeguard:** report how far households moved. Small moves are
translation. Large ones (an age shifted by 15 years, an income rank moved
from the 50th to the 90th percentile) mean the sources disagree about
something real, or that our own imputation (e.g. the file's ages) or a
unit-construction rule is wrong; they are flagged and investigated, not
absorbed.

**Post-training:** each survey's unconditional distributions enter as
targets at their full sampling precision; translation leaves them
untouched, so they no longer conflict with the file's X.

### D25. Derived variables are never predicted — decided (2026-10-11), a requirement

John, 11 October: a must-have. Any variable that is a sum or other fixed
function of variables in the model is never a model variable. The model
draws the parts; the derived variable is computed from them by its
definition, after generation. Where the parts in the model do not add up to
the total, the model gets one remainder variable (the total less the parts,
which can be negative), so the identity holds exactly on every record.

**Why.** When AGI was drawn as its own variable, the model could draw a
top-0.1% AGI and then income items that added up to far less: in the top
0.1% of generated records the six main items summed to $341bn against
$702bn in the PUF, and AGI exceeded them by $313bn against $28bn. With AGI
replaced by a remainder (AGI less 10 income items plus 3 adjustments) and
computed as the sum, the same model, data and training gave $642bn
(range $541–730bn over four training runs), a top 1% share of AGI of 19.4%
against 19.4%, and no false capital gains in the bottom half; SCF wealth
completion was unchanged (probe AS, `tests/probes/AS_REPORT.md`, 10–11
October).

**Scope.** Tax totals (AGI, total income, taxable income, line 13 capital
gain from short- and long-term gains, total Schedule E from its pieces),
wealth totals (net worth from assets and debts), and anything Tax-Simulator
computes from the parts. A part that is a share of another (qualified
within ordinary dividends, taxable within total pensions) is a bounded
relationship, not a sum; it is drawn as the part and its complement, or
checked as a constraint. The same holds for split-sum identities
(wages1 + wages2 = wages): exact by construction.

**Still to apply in the version 2 variable list:** total Schedule E
(`E02000`) is in the model beside its own pieces (partnership and S
corporation, rent and royalty, estate and trust); ordinary dividends
(`E00600`) beside qualified (`E00650`); total pensions (`E01500`) beside
taxable (`E01700`). Each should become the pieces plus a remainder.

### D26. How survey weights enter training — proposed (2026-10-14), not yet tested

**The problem.** Surveys interview some kinds of people far more than
their share of the population: the IRS samples nearly every high-income
return, and the SCF goes out of its way to find wealthy households. Each
record's weight says how many real households it stands for, so rich
records carry small weights and ordinary ones large weights. Until now the
model learned with each record counting in proportion to its weight. Then
the oversampled rich, though they are many records (about 20,000 of the
PUF's 166,156 are in the top 0.1% of AGI; 4% of SCF records are in the
SCF's top 0.1% by income), counted for almost nothing, and the model
learned the top badly on both the tax side and the wealth side (probes T,
UW, WT, `tests/probes/`).

Probes UW and WT fixed this by training unweighted with the weight as a
model variable. That does not generalize: a weight's size means different
things in different surveys, because it reflects each survey's design.

**The idea.** A survey chooses whom to interview mostly by characteristics
we observe, chiefly income. Among households with the same income, the
survey's sample is close to fair, so how other variables go together
within an income group can be learned without leaning on the weights.
The weights are mainly needed to say how many households are in each
income group. (The standard result for regressions; Solon, Haider and
Wooldridge, "What Are We Weighting For?", 2015.) Where a survey also picks
on something we do not observe, as the SCF does with its wealth estimate,
the weights still matter within the group, and they are kept there.

**The rule, the same for every survey.**

1. **Groups.** Define income groups by a variable that means the same in
   every survey after translation (D24), for example AGI class: bottom 50,
   50–90, 90–99, 99–99.9, top 0.1 percent.
2. **Inside a group, keep the survey's weights.** Each record's training
   weight is its survey weight, rescaled so that the training weights of a
   survey's records in one group add up to the number of those records.
3. **Across groups, attention follows the number of records**, not the
   number of households they stand for, so the oversampled top is learned
   as well as its sample allows.
4. **The survey weight itself is never a model variable.** It is used only
   to compute the training weights before training.

Toy example (made-up numbers):

| Source | Income group | Survey weight | Training weight |
|---|---|---|---|
| PUF | Ordinary | 1,500 | 1.0 |
| PUF | Ordinary | 1,500 | 1.0 |
| PUF | Top 1% | 3 | 1.5 |
| PUF | Top 1% | 1 | 0.5 |
| SCF | Ordinary | 30,000 | 1.0 |
| SCF | Top 1% | 100 | 1.6 |
| SCF | Top 1% | 25 | 0.4 |

The PUF top-1% records (survey weights 3 and 1) become 1.5 and 0.5: still
three to one within the group, but the group now counts by its two
records, not by the four households they stand for.

**Making records.**

1. Set how many records fall in each income group from the authority: IRS
   counts of returns by AGI class for the year.
2. For each record, draw its tax variables given its group, with the
   source label set to PUF (the PUF is the authority for tax variables).
   Totals are computed from parts (D25).
3. Draw its wealth given its tax variables, with the source label set to
   SCF (the SCF is the authority for wealth; the model has only seen
   wealth on SCF rows). Translation (D24) makes an SCF household's incomes
   mean the same as a PUF record's, so this is a fair question.

Completing a real PUF record skips steps 1 and 2: its income, and so its
group, is known.

**Why this is consistent.** Steps 2 and 3 of the rule change how much
attention each group gets, not what is learned about how variables go
together within a group. Generation always fixes the group first, so it
only ever uses within-group relationships, and the group sizes come from
the authority. What the model learns without fixing the group (for
example, how common top incomes are) is wrong under this rule and must not
be used; this is why generation starts from the group counts.

**To settle when testing.**

- How fine the groups are: finer groups correct more of a survey's design
  but leave fewer records per group.
- The balance between surveys: with attention following record counts,
  the PUF's 166,156 records outweigh the SCF's roughly 47,000; whether to
  rescale each survey's total.
- Whether groups need more than income for the SCF (its sampling also
  depends on a wealth estimate built from tax records).
- The check: probe EV's scorer (`tests/probes/ev_score.py`) for both the
  transformer and the trees, against probes UW and WT's weight-as-variable
  versions and population-weighted training, with the top 0.1 percent's
  income mix and wealth, the middle groups' wealth, and tax by AGI group
  as the main measures.

## Open questions

1. **The bounded variant.** Should logit calibration, which adds hard
   floors and ceilings to the entropy distance, implement the caps in D14,
   or should caps be enforced separately?
2. **Precision values.** D15 sets precisions by hand. Starting points
   include SOI's published coefficients of variation (Table 1.4CV) and
   judgement by source group. How strongly the aged file resists change
   overall is a related setting.
3. **Publishing 2015 and 2016.** The loop starts in 2015, but should those
   years be published? Under D21, wealth exists from 2015 (drawn from the
   pooled SCF waves and calibrated to the 2016 wave), so this is now a
   question of quality rather than of missing variables.
4. **The target list (deferred).** Which Pub 1304 tables, for which years? Which CBO
   lines in projection years? Which sources are held out as checks? The
   candidates are tax by AGI, the top-percentile table and projected
   incomes.
5. **How far to trust survey change (deferred).** The evidence supports using surveys
   for demographic composition and for the middle of the wage
   distribution, as weak evidence. It does not support using them at the
   bottom, the very top, or for capital income.
6. **Matching SSA percentiles (deferred).** SSA wage statistics count every W-2 earner,
   including non-filers. Wage growth by percentile has to be mapped onto
   the right population.
7. **Itemized deductions after 2017.** The structure is settled in D19 and
   D20. Still open:
   - the imputation models for non-itemizers' expenses;
   - running tax law on blurred PUF amounts is slightly biased near
     thresholds.
8. **Non-filers in the loop.** The ASEC non-filer pool already has its own
   year structure. How does it join the recursion?
9. **The top of the distribution.**
   - The PUF's aggregate top records are currently dropped.
   - The demographic file misses the top 1–2% of returns.
   - The Forbes splice already exists.

   How the top is handled in the loop is undecided.
10. **Evaluation details.** D17 sets the structure. Still to choose: the
    first test (proposed: filers only, 2015–2023, against each year's Pub
    1304 tables), the backtest cutoff year, the benchmark (uniform
    growth), the noise floor and the set of Tax-Simulator comparisons.

11. **Variables that may not fit D21.** D21 is the default. The other
    orderings stay available for any variable where it works badly.

    Within a year:
    - calibrate, then impute (the imputation sees final weights, but
      imputed variables cannot move weights);
    - calibrate, impute, calibrate again;
    - an imputation that matches its own targets by tilting its draws.

    Across years:
    - impute once and age;
    - re-impute every year from the nearest wave;
    - age between waves and re-anchor at each wave.

    The questions that decide it, variable by variable:
    1. Does the variable feed a tax-calculated target used in calibration?
    2. Should a miss on this variable move weights, or only its own
       distribution?
    3. Does the information about change come from repeated surveys or from
       aggregates?
    4. Does the imputation depend on weighted quantities?
    5. Does Tax-Simulator need year-to-year continuity of a record's values?
    6. How much draw noise is acceptable in year-over-year changes?

Questions 7–9 (itemized deductions, non-filers, the top of the
distribution) are next. John has thoughts on them.

## Revision history

- 2026-10-07: created. Records D1–D13 and the open questions from the
  discussion on 6–7 October.
- 2026-10-07: added D14 (caps), D15 (target list), D16 (start in 2015) and
  D17 (evaluation through the target list). Revised the open questions.
- 2026-10-07: added D18 (consistent target structure across years, with the
  path-dependence test).
- 2026-10-07: added D19 (tax calculation applied to underlying variables
  before targeting) and D20 (impute underlying itemizable expenses for 2015
  non-itemizers).
- 2026-10-07: added D21 (impute before calibrating, from pooled survey
  waves, with several draws per record, with a toy test). D10 superseded.
  Added open question 11 (the other orderings, kept for variables where
  D21 fits badly).
- 2026-10-07: D21 — pooling marked firm; expanded the list of details to
  settle; noted the real-SCF test in progress.
- 2026-10-07: D21 — added the real-SCF test results.
- 2026-10-08: moved D21's open details to `d21_pooling_questions.md`
  (Q1–Q16).
- 2026-10-08: D13 — no links across years are needed; each year may be
  built on its own. Added `rlhf_analogy.md`.
- 2026-10-08: added D22 (proposed): one joint transformer model across
  sources as the reference, used first for imputation, with two tests.
- 2026-10-09: D7 under review and D22 revised: the expected end state is
  whole records generated from the joint model, calibrated on whole-record
  weights. Test 3 (post-training, `tests/posttrain/`) started.
- 2026-10-09: added D23 (decided): translate every survey into PUF concepts
  (definitions, population, measurement) before training; marked D22 and
  D23 as the organizing decisions.
- 2026-10-09: added D24 (decided): trust survey weights; translate households
  into tax concepts; reweight only for coverage. D23's population step
  revised to point to it.
- 2026-10-09: D22 marked decided (John reoriented the work to it). Added
  `joint_model_plan.md`, the standalone plan built on D22–D24.
- 2026-10-11: added D25 (decided, a requirement): derived variables are
  never predicted; the model draws the parts plus a remainder and totals are
  computed. From probe AS (`tests/probes/AS_REPORT.md`).
- 2026-10-14: added D26 (proposed, not tested): survey weights used only
  within income groups; attention across groups follows record counts;
  group sizes from the authority at generation; the weight never a model
  variable.
