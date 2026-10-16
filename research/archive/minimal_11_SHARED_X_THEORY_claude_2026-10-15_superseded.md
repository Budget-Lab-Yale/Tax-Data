---
title: "Why the PUF and a survey disagree on a shared variable, and what to do about it (Claude's version)"
role: evidence
workstream: rebuild
status: superseded
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: research/rebuild/minimal/11_SHARED_X_THEORY.md
---

# Why the PUF and a survey disagree on a shared variable

Written 15 October, independently of the Codex version
(`11_SHARED_X_THEORY_codex.md`); the comparison is in
`11_SHARED_X_THEORY.md`. John asked for the exhaustive list of mechanisms,
the range of fixes, and what that implies about a rule.

## Setting

P (the PUF) has X and Z. S (a survey) has X and Y. The model learns Y
given X from S and applies it to P-style records. So the question is
always: does "X = this" describe the same kind of household in S as in P?

A value of X in a file is produced in five stages, and each stage can
differ between the files:

1. **Population.** Which households are in the file, and how they are cut
   into units.
2. **Definition.** What the variable is supposed to measure.
3. **Time.** Which year the value is for.
4. **Measurement.** How the recorded value differs from the true value
   under that definition.
5. **Weight.** How many households each record stands for.

The PUF is not the truth either. It is exact on definition and
measurement for filers, but it has its own population limits (filers
only; the top aggregated into four records) and its own editing (interest
rounded to $10, blurring at the top).

## A. The mechanisms

For each: what it does to S's distribution of X relative to P's, and
whether the two marginal distributions plus outside totals (SOI) can
detect it. "Zero mass" is the share at exactly zero; "shape" is the
distribution of the positive values; "top" is above the 99th percentile.

### Population

| # | Mechanism | Effect on S relative to P | Detectable from marginals + SOI? | Example |
|---|---|---|---|---|
| A1 | Coverage: S has households that do not file | Extra zero mass on every variable at once; extra low positives; totals barely change | Yes jointly: zeros up on all variables together; a filing rule applied to S says how much | SCF 30% zero wages vs PUF 17% |
| A2 | Unit construction: households cut into tax units by rule, dependents, young adults folded into parents, split-offs | Unit counts differ; X per unit shifts (incomes combined or split) | Partly: unit counts vs SOI return counts by group | Production: 170M SCF units vs 188M PUF; the SCF's 18 to 24 year olds |
| A3 | Sampling frame: S oversamples the wealthy (SCF list sample); P subsamples and aggregates its top | S has more units at the top than P; P's top is truncated | Yes: top share vs SOI, counts above $1M | SCF 1.4 to 1.8x the PUF's holders above $10,000 |
| A4 | Weights: S calibrated to population counts, P to return counts | Uniform or group-specific scaling of mass | Yes: counts by group vs SOI | 140M SCF units vs 151M PUF returns in 2015 |

### Definition

| # | Mechanism | Effect | Detectable? | Example |
|---|---|---|---|---|
| B1 | Concept: S's variable includes components P's does not, or excludes some | S = P + W. Zero mass down (W's holders become holders); shape and totals change wherever W's holders sit | Totals off; the pattern by bracket is not a uniform shift; but telling it from a scale error needs the questionnaire | CPS interest 2.3x SOI (includes IRA interest); SCF business income 2x SOI |
| B2 | Netting: S nets gains and losses, or asks household totals, where P has per-item per-return amounts | Fewer loss units; different sign pattern | Yes: loss share vs SOI | SCF 1.8% capital-loss units vs PUF 5.6% |
| B3 | Reference period: income year vs survey year | A one-year shift of the whole distribution | Only via totals | SCF 2016 asks about 2015; wealth is at interview |

### Time

| # | Mechanism | Effect | Detectable? | Example |
|---|---|---|---|---|
| C1 | Nominal growth, uniform | All dollars scale by g; ranks unchanged | Yes: SOI totals | Wages 2021 / 2015 = 1.27 |
| C2 | Distributional change: growth differs by percentile; the zero mass changes | Ranks of the same household move; the top moves most; holder shares change | Partly: SOI counts and amounts by AGI bracket per year | Interest holders fell as rates fell; capital gains 2021 / 2015 = 2.9 |
| C3 | Policy change alters what is reported under the same name | Definition drift between years | Only from knowledge of the law | 2018 law; realization timing |

### Measurement

| # | Mechanism | Effect | Detectable? | Example |
|---|---|---|---|---|
| D1 | Small-amount omission: probability of reporting rises with the amount | Zero mass up; low positives thinned; dollars among reporters right; totals right | Yes: by-bracket ratio rises from below 1 to 1; within-bracket means equal; totals equal | SCF interest: a quarter of PUF holders under $100, all of them above $2,000 |
| D2 | Rounding and heaping | Amounts under half a unit become zero; spikes at round values; totals right | Yes: spikes in S's value histogram | SCF amounts in round thousands |
| D3 | Scale misreporting: amounts systematically understated or overstated by a factor | Cumulative shares shift one way at every level; totals off by the factor; within-bracket means of reported values still agree | Yes with totals; without totals it looks like a shape difference | John's "survey reports half"; CPS dividends 0.41x |
| D4 | Top-coding or capping | Spike at the cap, nothing above; totals short at the top | Yes: spike at the maximum | CPS |
| D5 | Item nonresponse and imputation | Imputed values smooth the shape; several implicates | Weakly | SCF five implicates |
| D6 | Classical noise (recall, proxy reporting) | Spreads the distribution: more mass in both tails, less in the middle; ranks shuffled; totals right | Weakly from marginals; needs a validation study | Wages in the CPS |
| D7 | Agency editing of P itself | Rounding, blurring, aggregation at the top | Known from documentation | PUF interest to $10; four aggregated records |

### Combinations

Real surveys have several at once. The SCF's interest: D1 plus A1 plus
A3. CPS interest: B1 plus D4 plus A1. A different year of the SCF: C1
plus C2 plus whatever drifted in D1.

## B. What the signatures look like

The detection table gives four things per variable: the zero share, the
cumulative share above each bracket edge (S over P), the within-bracket
mean (S over P), and the total (S over P or SOI). Each mechanism leaves a
different pattern:

| Mechanism | Zero share | Cumulative ratio by amount | Within-bracket means | Total |
|---|---|---|---|---|
| D1 omission | S higher | below 1 at low amounts, rising to 1 | equal | equal |
| D3 scale down by s | equal | below 1 at every amount | equal | s |
| B1 definition add-on | S lower | above 1 at every amount, not uniform | roughly equal | above 1 |
| A1 nonfilers | S higher on all variables | about equal above small amounts | equal | about equal |
| A3 top oversample | equal | 1 until the top, then above 1 | equal below the top | slightly above 1 |
| D4 top-coding | equal | 1 until the cap, then 0 | equal below the cap | below 1 |
| D2 rounding | S higher | below 1 under the rounding unit, then 1, with spikes | equal | equal |
| C1 uniform growth | equal | shifted by g | shifted by g | g |
| D6 classical noise | equal | above 1 at both ends, below 1 in the middle | equal | equal |

Two points on identification:

- **The within-bracket means cannot distinguish scale from omission.**
  Under a uniform scaling the people in S's $100 to $500 bracket are true
  $200 to $1,000 households, and their reported mean still looks like
  P's. The totals and the direction of the cumulative ratios do the
  work. Last night's claim that the means proved the SCF's dollars were
  fine was wrong on its own; the totals ($90B vs $89B) and the
  cumulative pattern (short below $2,000, long above $10,000) are the
  evidence.
- **Scale up and definition add-on have the same signature** (totals
  above 1, cumulative ratios above 1). The questionnaire breaks the tie,
  not the data. The CPS interest case is definition; the simulated
  doubled capital gains were scale, and the simple rule could not tell
  (it called them "definition or mixed").

## C. The options

Each option is an operation on S's X before banding, on the banding
rule, in the model, or in the output. For each: what it corrects, what
it leaves, what it makes worse, what it assumes, how it behaves across
years. Results from last night in brackets.

| # | Option | Corrects | Leaves | Makes worse | Assumes | Across years |
|---|---|---|---|---|---|---|
| O1 | Rank within positives, zero as its own bin (v2) | C1, D3 (ranks ignore scale) | A1, A3 | D1, D2: the zero gap is pushed into the positive ranks [the 2x] | same holder share and shape | free |
| O2 | Rank within source including zeros (ties placed at random, or by a prior) | C1, D3, most of D1 and D2 (the zero gap is absorbed) | shape differences from A3 and D1's tail: SCF $500 goes to PUF $1,000, SCF $10,000 to PUF $7,000 | nothing relative to O1 | same overall distribution of X | free |
| O3 | Band S on P's dollar cut points (E4) | D1, D2, A3 shape (a dollar is a dollar) [0.98x] | A1, B1, D3 | D3 (wrong dollars taken as right) [1.68x when halved] | S's dollars equal P's | needs P's cuts per year |
| O4 | Translate S's dollars (factor, or quantile map above a line), then O3 | D3, C1 [1.02x when halved] | B1 if misread as scale [0.80x] | | the factor is identified (totals) | needs P's cuts per year |
| O5 | Zero P's small amounts (production's threshold) | holder-share alignment for D1 [1.13x] | the small amounts themselves | P's output | | free |
| O6 | Split: shared part above a line, P-owned small part (E2, E5) | some of D1 | most of it [1.17 to 1.55x] | | the line | free |
| O7 | Flip S's non-holders to holders (D23 as designed, E6) | the level [1.04x] | the shape [top 1% at 50%] | | which non-holders | free |
| O8 | Reporting curve in the loss (E9) | D1 in principle | | failed without the label [1.41x]; untested with it | common dollars | needs O3 |
| O9 | Source token the model may use | lets the model hold a fatter zero bin for S (needed by O3) | | with a coarsened loss, lets S separate entirely (v3) | | |
| O10 | Restrict S to P's population (filing rule), or add a filer token | A1 | | | the rule reproduces P's population | free |
| O11 | P's cut points for years without a PUF: grown by SOI totals (Y1), unchanged (Y2), by S's own percentile growth (Y3), or from SOI by-bracket tables | makes O3 and O4 work across years [all within noise of each other; mean 0.86 to 1.12] | C2 if one factor per item | | growth pattern | |
| O12 | Output tilt to targets | any marginal | Y given X; blind to B1 [0.010 nats on the definition case] | | | per year |
| O13 | Fix the definition in the table builder | B1, B2 | | | questionnaire knowledge | free |
| O14 | Fix unit construction | A2 | | | | free |
| O15 | Undo P's own editing where possible (aggregated top, rounding) | D7 | | | documentation | |

## D. Mechanism to fix

| Mechanism | Fix | Fallback |
|---|---|---|
| A1 nonfilers | O10 | nothing else works; the tilt cannot reach Y given X |
| A2 units | O14 | |
| A3 top sampling | O3 or O4 (dollar anchoring); the PUF's own top needs O15 | O2 is wrong at the top |
| A4 weights | O12 counts by group | |
| B1, B2 definition | O13 | the data cannot fix it; O4 misreads it as scale |
| B3 timing | O13 or accept | |
| C1 uniform growth | O1 or O2 (free), or O3 with O11 | |
| C2 distributional change | O2 (uses S's own change), or O11 from SOI by-bracket tables | one factor per item is approximate |
| C3 policy | O13 per year | |
| D1 omission | O3 with O9, or O2 as the year-free approximation; O8 later | O5 for features only |
| D2 rounding | same as D1 | |
| D3 scale | O4 | O2 absorbs it only if uniform |
| D4 top-coding | treat above the cap as P-owned (absent on S) | |
| D5, D6 noise | nothing clean; accept, or O8-style noise model | |
| D7 PUF editing | O15 | |

## E. The rule

1. **Read the questionnaire and the unit builder first.** Fix
   definitions (B) and units (A2) in the table builder. No data step
   can do this, and the data will misread a definition gap as scale.
2. **Decide the population.** Apply a filing rule to S and either drop
   the units that would not file or tag them. Check the zero shares on all
   variables jointly before and after.
3. **Run the detection table per shared variable**, in the same year's
   dollars, with the SOI total for that year. Classify by the signature
   table: omission, scale, top-coding, rounding, top oversample, or
   "not one of these, go back to step 1".
4. **Translate dollars only for scale** (divide by s from the totals).
   For top-coding, mark the capped region as absent on S. For omission
   and rounding, do nothing to the dollars.
5. **Band on P's cut points for that year** (O3), with the source token
   kept (O9). Where P has no year, build P's cut points for the year
   (O11).
6. **Write the sheet**: one line per variable per survey wave, case,
   operation, evidence.
7. **Check Y given X after training**, not only the tilt move: the tilt
   cannot see a definition error (D3 run). Wealth by wage group against
   S is the check we have.

What this cannot decide from the data alone: definition vs scale when
totals are above 1 (needs the questionnaire); shape differences vs scale
when there is no outside total for the variable (wealth, consumption);
noise (D5, D6) at all.

## F. The year question

John's point: ranking over the whole distribution including zeros (O2)
pools years with no outside input, because a rank is a rank in any year.
It also absorbs most of D1 and all of D3 and C1. What it assumes is that
S's distribution of X has the same shape as P's in that year, including
the top. The detection table says that is off by about 1.5x in dollars
between $100 and $10,000 for interest and more at the top (A3), in
opposite directions. How much wealth error that costs is an empirical
question we have not run; it is the one-run test proposed this morning.

O3 assumes the dollars are right and nothing about the shape, but needs
P's quantile function in every year. P's quantile function is what SOI
publishes, to the resolution of AGI brackets, for every year: counts and
amounts per item by bracket. So P's cut points exist in every year, at
coarse resolution, without a PUF. One growth factor per item (Y1) is the
crude version; the by-bracket tables are the better one and are not yet
built. Also worth asking: other PUF years exist (the IRS publishes a PUF
most years); if more than 2015 is available, the question changes.

My view: O3 with per-year P cut points from SOI is the right rule, O2 is
the right approximation when the outside information is missing, and the
two should be run side by side on the three-wave table so the cost of
the shape assumption is a number rather than an argument.
