---
title: "Borrowing change over time from annual surveys and administrative tables"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# Borrowing change over time from annual surveys and administrative tables

Background paper 11 for the Tax-Data rebuild (see `research/rebuild/BRIEF.md`).
The question: reweighting the 2015 PUF can only reshuffle weight among the
combinations of characteristics the 2015 file contains, and cannot change the
shape of a distribution among similar records. Can we instead take how the
population changed after 2015 from sources observed every year, and use that
change to evolve the PUF or to build synthetic records for later years?

The paper works through three families of approach and some others, then
reports a small empirical test: whether the CPS ASEC's changes since 2015 track
the administrative changes, even where its levels do not.

"Verified" means I opened the document, page, file or code and saw the fact.
"Metadata only" means I confirmed author, title, journal and year through the
Crossref or JMLR record but did not read the paper. "Believed" means I did not
check. The web-search allowance was exhausted before this work started; I
fetched known addresses directly. ssa.gov refuses automated access, so the SSA
wage statistics came from Internet Archive copies of the SSA pages.

## Summary

1. **Every approach rests on one of two testable assumptions.** (a) *Stable
   conditionals*: given the variables a survey shares with the PUF, the
   distribution of the PUF-only variables is the same in year t as in 2015.
   (b) *Stable measurement error*: a survey's error at a given rank and group
   does not change over time, so its changes are right even where its levels
   are wrong. Reweighting by survey change (1a) needs both; moving values by
   survey change (1b) needs (b); updating drivers (2) needs (a).
2. **For wages, CPS change tracks administrative change in the middle, not at
   the tails.** 2015 to 2023, CPS ASEC against SSA W-2 wage statistics: mean
   wage per earner +38.1% and +38.6%; median +42.9% and +44.5%; 90th
   percentile +34.0% and +35.8%. The 10th and 99th percentiles grew 13 and 15
   points faster in the CPS, and its top 10% wage share rose 2.0 points while
   SSA's fell 1.0. The cumulative gap in the mean reached 4.7 points in 2019
   before closing.
3. **For returns, CPS tax units track SOI's totals and broad shifts, not the
   top or capital income.** 2015 to 2023 against SOI Table 1.4: returns +7.8%
   and +6.7%, wages +46.8% and +43.5%, AGI +53.0% and +49.7%, Social Security
   +62.7% and +60.9%; the shift of wages and AGI out of brackets under $50,000
   is about the same size. Returns above $200,000 grow 34 to 117 points
   faster in the CPS. Capital income by bracket is unusable: the largest
   interest and dividend amounts Census releases rose several-fold with its
   2019 processing change.
4. **SSA's annual wage statistics are a better source of wage-distribution
   change than any survey.** They cover every W-2 earner every year, to over
   $50 million. Surveys are needed for what SSA and SOI do not publish:
   demographic composition, and income rank by age, marital status and
   children, below roughly the 90th percentile.
5. **Approach 1a (change through weights) is "change targets" inside the
   planned calibration:** a 2015 PUF statistic times the survey's
   year-t/2015 ratio, with precision from survey replicate weights plus a
   drift term measured in a backtest. A classifier density ratio is a useful
   diagnostic, not the estimator.
6. **Approach 1b (change through values) should carry the proportional change
   at each rank, not changes-in-changes in dollar levels.** Quantile delta
   mapping and Realtime Inequality's rank adjustment do this. It is the
   direct answer to the brief's question, because it changes shapes within
   groups of similar records.
7. **Approach 2 (2015 relationships, current drivers) is imputation-as-prior
   with the 2015 PUF as donor.** It creates new records. Capital gains,
   dividends, interest, pass-through income, unemployment compensation, IRA
   distributions and law-affected deductions do not have stable 2015
   conditionals over 2016–2023 and need year-specific tilts fitted to SOI.
   The top 5% should stay PUF records.
8. **Approach 3 (panel transitions) is not needed for annual cross-sections
   over 2016–2023.** It matters only for variables that depend on the prior
   year (EITC lookback, carryforwards, safe harbours) and for long-horizon
   cohort effects; a one-step model attached to each cross-section covers the
   first.
9. **Series breaks inside 2015–2023:** the 2019 CPS ASEC processing change and
   retirement-question redesign; pandemic nonresponse in the 2020 survey
   (income year 2019); TCJA line-item changes in SOI. In the check, CPS growth
   from 2015 to 2019 ran 7 to 11 points ahead of SOI for wages, AGI and
   Social Security.
10. **Backtest:** fit through 2019, project 2020–2023, score against Pub 1304
    by bracket, marital status and age and against SOI's top-percentile
    table; benchmark against the current method and against rescaling totals.

**Ranked recommendation:** (1) age each earner's wages by the SSA change at
their percentile, then calibrate; (2) add survey change targets for
composition below the 90th percentile, with measured precision; (3) add CPS
rank shifts by demographic cell; (4) prototype approach 2 for the bottom 95%
and stable variables only; (5) panel transitions only for history-dependent
variables.

---

## 1. Terms

- **Change target.** A target for year t built as a 2015 PUF statistic times a
  survey's ratio of that statistic in year t to 2015. Example: the PUF's 2015
  count of joint returns with a head aged 65 or older, times the CPS growth in
  such tax units.
- **Density ratio.** For two distributions of the same variables, the ratio of
  their probability densities at each point. Multiplying 2015 weights by the
  ratio of the year-t density to the 2015 density turns a 2015 sample into a
  year-t sample, to the extent the variables cover what changed.
- **Entropy balancing.** Choosing new weights as close as possible to the old
  ones (closeness measured by relative entropy) subject to matching chosen
  averages exactly (Hainmueller 2012). It is the same computation as raking
  or entropy calibration.
- **Quantile mapping.** Moving each record's value so that its rank in the old
  distribution is kept and its value becomes the new distribution's value at
  that rank. **Quantile delta mapping** keeps the record's own level and
  multiplies it by the source's proportional change at that rank (Cannon,
  Sobie and Murdock 2015).
- **Changes-in-changes.** Athey and Imbens's (2006) generalization of
  difference-in-differences to whole distributions; described in section 5.
- **Drivers.** Variables observed in year t from surveys or administrative data
  on which the other variables are modelled to depend.
- **Forecast-then-correct.** Data-assimilation term for: move the state one
  step with a model, then adjust it to that step's observations.

## 2. What borrowing change can add, and the two assumptions

Write a year-t tax-unit record as (x, z): x the variables a survey also
measures (ages, filing status, children, wages, broad income types), z the
variables only the PUF has (capital gains detail, pass-through types,
deductions, and so on). The year-t population is the year-t distribution of x
times the year-t conditional distribution of z given x.

- Reweighting the PUF to year-t tables changes the weight on each (x, z)
  combination the PUF already has. It cannot add a combination or move a value.
- Borrowing change from a survey supplies information about how the
  distribution of x moved, beyond what SOI tables reveal. It supplies nothing
  about z given x; for that, every approach assumes the 2015 conditional holds
  (assumption a) or fits year-t adjustments to SOI tables.
- The survey measures x with error. Its *level* of, say, interest income is not
  usable (CPS ASEC interest runs well above SOI; the repository records a
  ratio of 2.6, `src/nonfilers/asec_tax_units.R`, verified). Its *change* is
  usable only if the error is stable (assumption b).

Assumption (b) can be checked wherever an administrative series measures the
same thing every year. That is the purpose of section 11.

## 3. Sources observed every year

Years are income years. "On disk" means under
`/nfs/roberts/project/pi_nrs36/shared/raw_data/` (verified by listing).

| Source | Years available | Unit | Variables shared with the PUF | Weaknesses for borrowing change |
|---|---|---|---|---|
| CPS ASEC (`CPS-ASEC/cps_asec_common`, IPUMS extract with replicate weights) | 2014–2025 on disk | person, household; tax units constructible (`src/nonfilers/asec_tax_units.R`) | filing status (constructed), ages, children, wages by person, self-employment, interest, dividends, rent, retirement income, Social Security, unemployment; capital gains only from 2018 | capped and swapped top values; nonresponse and imputation highest in the tails; 2019 processing break; pandemic nonresponse |
| ACS 1-year (`ACS`) | through 2024 on disk | person, household, group quarters | ages, marital status, children, wages, self-employment, combined interest-dividends-rent, retirement, Social Security, state | coarse income items, no tax concepts; about 25 times the CPS sample (per `03_data_microdata.md`) |
| SIPP (`SIPP`) | releases through 2025 (calendar 2024) on disk | person-month, panels up to four years | monthly wages, self-employment, transfers, tips, assets | small; attrition |
| SCF (`SCF/v1`) | 2009 (the 2007–09 panel file `SCFP2009panel.csv`), 2013, 2016, 2019, 2022 on disk | family | all major income types including capital gains and business income, wealth | every three years; small; excludes the Forbes 400 |
| PSID (`PSID`) | test extracts only on disk | family, every two years | wages, business, transfers, wealth | small at the top; I found no full extract |
| SSA wage statistics | every year; 2015–2023 fetched for this paper | W-2 earner | wages (net compensation) by $5,000 interval to $200,000, coarser above | wages only; includes deferred-compensation contributions, which PUF wages exclude; not on disk |
| SOI W-2 statistics (`IRS-Ind/national/w2`) | 2014, 2016–2020 | W-2 earner matched to returns | wages by age, sex, wage size and AGI | no 2015; stalled after 2020 (per `IRS-Ind/NOTES.md`) |
| SOI Pub 1304 tables (`IRS-Ind/aligned`) | 2011–2023 | return, by AGI bracket | all return lines | aggregates only |
| Saez–Zucman age tables (`Saez-Zucman-PUF`) | 2012–2016 | cells of internal SOI data | means of 47 variables in 12 demographic groups by 16 AGI groups (per `03_data_microdata.md`) | end in 2016 |
| Linked consecutive CPS ASECs (`CPS-ASEC-Panel/v4`) | pairs from 1980–81 (first row checked; last year not checked) | person, two years | as the CPS | only people who stay at the same address |
| SSA Earnings Public-Use File (`SSA-EPUF`) | earnings histories to 2006 | person | wages and self-employment by year | old |

## 4. Approach 1a: borrowing change through weights

**Method.** Estimate how the survey's distribution of shared variables x moved
from 2015 to year t, and multiply PUF weights by that movement.

- *Classifier route.* Pool the survey's 2015 and year-t records, fit a model of
  the probability that a record is from year t given x, and convert the odds to
  a density ratio r(x) (Qin 1998; Bickel, Brückner and Scheffer 2009;
  Sugiyama, Suzuki and Kanamori 2012 give the theory; Kanamori, Hido and
  Sugiyama 2009 a direct least-squares estimator). Then w_i,t = w_i,2015 ×
  r(x_i). This is the DiNardo–Fortin–Lemieux (1996) reweighting used in wage
  decompositions, run forward in time.
- *Entropy-balancing route.* Pick the moments that matter (counts by age band ×
  filing status × number of children; counts by income-rank band × age band;
  wage-earner counts by those cells), compute their survey change ratios, and
  ask the PUF weights to reproduce the 2015 PUF moments times those ratios.
  This is the same computation as the planned entropy calibration, with change
  targets added.

**Assumptions.**
- (a) The 2015 conditional of PUF-only variables given x holds in year t.
- (b) The survey's ratio of year-t to 2015 density on x equals the true ratio.
  This needs a stable survey measurement process: the same questions, the same
  editing and imputation, the same nonresponse pattern, the same released
  maxima. It fails across the breaks in section 10.
- Overlap: the year-t distribution of x must sit inside the support of the
  2015 PUF. Weights cannot create a combination the PUF lacks, so 1a does not
  answer the motivating question for value shapes. It handles composition.
- Income variables must be compared on a common scale. Using the survey's dollar
  levels to evaluate r at PUF dollar values imports the level gap (survey
  interest at rank q differs from PUF interest at rank q). Use within-year rank,
  not dollars, for income variables in x, and dollars only for demographics.
  Tax-Data already matches CEX consumption to the PUF by within-dataset income
  rank for the same reason.

**Data.** CPS ASEC annually with replicate weights; ACS for finer demographics
and state. Shared variables: head and spouse age, filing status (constructed),
number and ages of children, number of earners, wage rank of each earner,
presence of self-employment, Social Security and retirement income, and broad
income rank. Not usable from surveys as change information: capital income
amounts, capital gains, anything above about the 90th percentile.

**Top of the distribution.** Restrict the survey's change targets to cells below
a rank cut-off (Realtime Inequality uses CPS wages only below the 90th
percentile, verified in the paper's data-sources table). Above it, SOI Table
1.4 brackets, SOI's top-percentile table and the SSA wage distribution govern.
The calibration has to know which targets cover which records; a record above
the cut-off contributes zero to survey change targets.

**Precision.** Each change target y_j,t = m_j,PUF,2015 × (m_j,svy,t /
m_j,svy,2015) gets a variance from two parts: the survey sampling variance of
the ratio, computed from replicate weights; and a drift variance for failures
of assumption (b), estimated in the backtest from how far survey ratios missed
the SOI or SSA ratio where both exist. Section 11 gives a first look at the
size of the second term.

**Classifier versus entropy balancing.** A classifier estimates every
interaction at once, which is attractive, but its ratio is noisy where data are
thin, needs regularizing, and has no natural precision for the calibration to
use. Entropy balancing on chosen moments gives one target per moment, each with
a standard error. Recommendation: use entropy balancing inside the main
calibration, and use a classifier only as a diagnostic: its ability to tell
year t from 2015 says how much composition moved, and its most important
features suggest which moments to target.

**Two-week prototype.** Days 1–3: CPS tax units 2015–2023 (the builder exists);
define 40–80 cells (age band × filing status × children × earner count; wage
rank band × age band). Days 4–6: change ratios with replicate-weight standard
errors. Days 7–10: add them as change targets to the entropy calibration
prototype, alongside SOI targets. Days 11–14: backtest as in section 12.

**Backtest.** Score whether adding change targets improves the fit to held-out
Pub 1304 counts by marital status and age (the returns-by-marital-status-and-age
family is on disk in `IRS-Ind/aligned`), which are the closest administrative
analogue of the composition changes the survey supplies.

## 5. Approach 1b: borrowing change through values

**Three variants.**

- *Changes-in-changes in levels.* Athey and Imbens (2006, verified metadata;
  the formula is standard) build the counterfactual distribution of a treated
  group from a control group's change over time. Treat the survey as the
  control and the PUF as the group of interest: a PUF value y moves to
  F_svy,t⁻¹(F_svy,2015(y)), the year-t survey value at the rank y would have had
  in the 2015 survey. The identifying assumption is that one monotone function
  of an unobserved rank produces the outcome in both groups, and only the
  distribution of that rank differs between them. A survey whose measured
  dollars differ from tax dollars violates this: the map is evaluated at PUF
  dollar levels on the survey's dollar scale. Not recommended for tax income.
- *Quantile delta mapping at a rank.* Keep each record's PUF rank q within its
  cell and multiply its value by F_svy,t⁻¹(q) / F_svy,2015⁻¹(q). Climate
  scientists use this to keep a model's projected change at each quantile while
  correcting its level (Cannon, Sobie and Murdock 2015, metadata only). The
  assumption is that the ratio of survey to true value at each rank and cell is
  constant over time. It allows the survey's level to be wrong.
- *Realtime Inequality's two-part method* (verified, Blanchet, Mouillet, Saez
  and Zucman 2026, section 4.3 and appendix C.2). An administrative source gives
  the overall wage level at each percentile. In their case this is QCEW payroll
  cells corrected to tax data by a regression that, in their words, "does not
  vary with time". The CPS gives each demographic cell's movement in average
  wage rank. Each record's rank x moves to x + x(1 − x)Δy, with Δy the cell's
  relative change in average rank, ranks are re-sorted to be uniform, and the
  record takes the administrative wage at its new rank. Their table of data
  sources states they use the SSA wage distribution "because it is better at
  capturing low wages", and use the CPS only for the bottom 90%.

**Assumptions.** Rank stability within cells. The survey's percentile
ratios, or its cell rank shifts, are right even though its levels are wrong.
Section 11 shows this holds for CPS wages between about the 25th and 95th
percentiles over 2015–2023, within a few points, and fails at the 10th and
99th percentiles and in single years.

**What it changes that reweighting cannot.** It changes the shape of each
income distribution within groups of similar records: a 2015 PUF earner at the
20th percentile of wages gets the 20th-percentile growth, not the average
growth. This is the gap the brief names.

**Data.** For wages: SSA wage statistics (earner level) for the overall
distribution in every year; CPS for demographic cells' rank movements. For
Social Security benefits: SSA's own benefit distributions (believed published in
the Annual Statistical Supplement; I did not check which tables) and the CPS,
whose Social Security totals tracked SOI within two points by 2022 and 2023, though not in 2019 (section 11). For
capital income: no survey change is usable; use SOI Table 1.4 by bracket and
the top-percentile table, as the current shape-preserving rescale does by
bracket.

**Practical detail for the PUF.** Apply the wage mapping to each earner after
the primary/spouse split (`src/imputations/earnings_split.R`), because SSA
counts earners, not returns. SSA's net compensation includes elective
deferrals; PUF wages (box 1) do not. Use proportional changes, not levels,
so the definitional gap cancels to the extent the deferral share at each
percentile is stable.

**Top.** SSA's intervals run to $50 million and over (verified on the 2015 and
2023 pages), so wages at the top have an administrative change every year.
Other income at the top: SOI tables.

**Precision.** In a values approach the mapping sets record values; precision
enters through the calibration that follows. Two options. Treat the mapped
values as the prior and let the calibration's weights absorb disagreements with
SOI. Or give the mapping a few free parameters per year, such as a tilt of the
growth curve across percentiles, with a prior centred on the survey or SSA
change and a variance from the backtest, fitted jointly with the weights. This
is formulation A of `04_methods_first_principles.md` (rank-dependent value
transformations), with the survey or SSA change as the centre of the prior
rather than zero.

**Two-week prototype.** Days 1–2: parse SSA wage statistics 2015–2023 (done for
this paper). Days 3–5: map PUF earner wages to each year by SSA percentile
growth; check that SOI wages by AGI bracket move toward Table 1.4 before any
reweighting. Days 6–9: add CPS cell rank shifts for age × sex × marital status.
Days 10–14: backtest.

**Backtest.** Compare wage totals and returns with wages by AGI bracket with
Pub 1304 Table 1.4 for 2020–2023, against uniform wage growth. Realtime
Inequality's own backtest, one year ahead, 1976–2024, found the bias for the
bottom 50% of factor income fell from +0.8 to −0.3 points a year and for the
top 1% from −1.6 to +0.2 points, against uniform growth (Table 2, verified).

## 6. Approach 2: keep the 2015 relationships, update the drivers

**Method.** Build the year-t population of drivers from current sources, draw
each record's remaining variables from a model of p(z | drivers) fitted to the
2015 PUF, then calibrate to year-t SOI tables.

**Candidate drivers.** Variables with good annual measurement and a plausible
role as causes of the rest:
- head and spouse age, filing status, number and ages of children, from CPS or
  ACS tax units with Census population controls;
- each earner's wages, at their SSA-mapped level (section 5);
- Social Security benefits, from SSA administrative distributions or the CPS;
- self-employment presence (CPS), with amounts from the conditional model;
- state of residence (ACS), if state work continues.

**Which 2015 conditionals are plausibly stable over 2016–2023.** Judgement,
not tested here:

| Likely stable over a few years | Not stable |
|---|---|
| taxable pensions given age and Social Security | capital gains given age and wages (market cycle; record realizations in 2021) |
| dependents' ages given head age and filing status | dividends and taxable interest given income (interest rates rose sharply in 2022–23; SOI interest grew 227% from 2015 to 2023, most of it in 2023) |
| charitable giving given income, before the itemization filter | pass-through and S-corporation income versus wages (TCJA's pass-through deduction and corporate rate changed organizational choices) |
| Schedule C presence given age and wages, roughly | unemployment compensation (2020–21) |
| | IRA distributions given age (required-distribution age changed in 2020 and 2023; distributions waived in 2020) |
| | student loan interest (payments paused 2020–23), mortgage interest given income (rates) |

For the unstable variables, the 2015 conditional is a prior, and each year
needs its own adjustment fitted to SOI tables: for example an exponential tilt
of p(z | drivers) whose parameters are chosen so that the file matches Table
1.4 by AGI bracket for that variable. The wealth module already uses this
device (`src/imputations/tilt_solver.R`).

**Relation to imputation-as-prior.** It is the same structure. The 2015 PUF
plays the donor survey and the year-t driver file plays the recipient, as SCF
does for wealth today. Three differences:
- The records themselves are new. The record set is the driver file, so the
  output is synthetic year-t records, not reweighted PUF records.
- The model is fitted to tax data, so z has tax-return concepts by
  construction. Only the drivers carry survey measurement error.
- It relies on conditional independence: z is independent of anything not in
  the drivers, given the drivers. Joint relationships between z and
  survey-only variables are not identified. Realtime Inequality warns of this
  for its matched file (per `08_practice_other_fields.md`); the statistical
  matching literature makes the same point (D'Orazio, Di Zio and Scanu 2006,
  metadata only).

**Top.** Surveys are thin and capped at the top, so the driver file is weak
there. Keep PUF records (and the Forbes splice) as the population above about
the 95th percentile of income. Age them by approach 1b for wages and
by SOI Table 1.4 bracket totals and the top-percentile table for other income.
Build only the rest from drivers. The seam needs a continuity check like the
one used for consumption's top-tail adjustment.

**Series breaks.** Only through the drivers. Demographic drivers are robust to
the CPS income breaks; wage drivers taken from SSA avoid them.

**Precision.** The conditional model is the prior; the year-specific tilts are
the parameters; SOI tables are the targets with their stated precision. This is
formulation C of `04_methods_first_principles.md`, restricted to the stable
variables and the bottom 95%.

**Two-week prototype.** Days 1–4: driver file for 2019 and 2022 from CPS tax
units, wages SSA-mapped. Days 5–8: fit p(z | drivers) on the 2015 PUF for a
subset (pensions, Schedule C, charitable giving, interest, dividends) with the
existing quantile-forest helpers. Days 9–11: draw, calibrate to Table 1.4.
Days 12–14: compare with the reweighted PUF on held-out tables. Two weeks is
tight for the full variable list; a subset tells whether the approach is
worth it.

**Backtest.** Same as section 12, plus one check specific to this approach:
untargeted cross-tabulations (for example, returns with both Schedule C and
pensions, by age) against the 2015 PUF aged by the current method. If they
disagree, the reason should be a known change in drivers, not drift.

## 7. Approach 3: follow records forward with panel transitions

**Method.** Each year: move every record forward one year with estimated
transition probabilities and amounts (forecast), then calibrate to that year's
targets (correct). Repeat. Data assimilation does this (Evensen 1994; Gordon,
Salmond and Smith 1993 for the particle version; metadata only).

**What can be estimated, and how well.**

| Transition | Source on disk | Quality |
|---|---|---|
| death | SSA and NCHS life tables (`NCHS-LifeTables`, `SSA-Demographic`) | good; deterministic given tables |
| marriage, divorce, widowhood | PSID (extracts only), SIPP, linked CPS ASEC pairs | fair; linked CPS loses movers, who are disproportionately the people changing marital status |
| births; children aging out | ACS fertility; deterministic ageing | good |
| employment entry and exit | linked CPS ASEC, SIPP | good for rates; fair for amounts |
| earnings growth by age and rank | linked CPS, SIPP, PSID; SSA earnings file to 2006 | fair below the top; top mobility only from published administrative studies (Kopczuk, Saez and Song 2010, metadata only) |
| retirement and benefit claiming | SIPP, PSID | fair |
| capital income, capital gains, pass-through income | PSID wealth module; SCF 2007–09 panel (one crisis-period panel, verified abstract) | poor |

Linked CPS ASEC pairs come from the CPS rotation: households in their first
four months in sample in one March are in months five to eight a year later,
so at most about half the sample can be linked, and only people who stay at
the same address (believed; the standard reference on linking is Madrian and
Lefgren 1999/2000, whose NBER page I verified but whose text I did not read).

**Drift.** Errors in transition models accumulate. Calibrating each year stops
targeted aggregates from drifting but not untargeted joint features. Repeated
reweighting concentrates weight on fewer records, as in particle filters,
whose weight collapse in many dimensions is well documented (Snyder,
Bengtsson, Bickel and Anderson 2008, metadata only). Remedies: keep the
horizon short, resample and clone records when the effective sample size
falls, and compare untargeted statistics with independent cross-sections each
year.

**Does it matter when records need not persist?** Mostly not, for 2016–2023.
Tax-Simulator reads one cross-section per year. Transitions add value only
where a year-t variable depends on the record's own past:
- prior-year earned income (the 2020 and 2021 EITC and child tax credit
  lookback elections; `ei_prior_yr` is a placeholder set to zero today, per
  `03_data_microdata.md`);
- capital loss, net operating loss and credit carryforwards (`nols`,
  `prior_yr_cred`, `amt_nols`);
- prior-year tax for estimated-tax safe harbours;
- cost basis and holding periods (already carried by the wealth module);
- long-horizon cohort effects (for example, women's earnings histories, and
  defined-contribution balances maturing into retirement income), which matter
  for 2030 and beyond rather than for 2016–2023.

For these, a one-step transition model attached to each cross-section is
enough: for each year-t record, draw a plausible year t−1 value from a
model fitted on linked CPS pairs or SIPP. No multi-year record chain is
needed.

**Top, breaks, precision.** The panels are weakest at the top; SOI tables
govern there. Linked CPS pairs straddling the 2019 processing change measure the
break, not behaviour. In the precision framing, each year's forecast is the
prior for that year; the transition model's error is the prior's variance.

**Two-week prototype.** Only the one-step model for prior-year earned income
and carryforwards. A full forecast-then-correct loop is not a two-week project.

**Backtest.** For the one-step model: the share of returns electing the 2020
and 2021 EITC lookback, if SOI publishes it (I did not check).

## 8. Other routes

- **IRS's own change information.** Where SOI or its contractors published
  change, it has no survey measurement error. On disk: the Saez–Zucman age
  tables for 2012–2016 (internal SOI cells, so 2015→2016 change by
  demographic group and AGI group is available from IRS data); SOI W-2
  statistics for 2014 and 2016–2020 by age, sex and wage size; state and ZIP
  code tables for every year to 2023. PSZ's `pufonline2016.dta`, also on disk,
  is a 2012 PUF updated to internal 2016 tabulations and can serve as a
  comparison file for 2016.
- **SOI's synthetic 2016 PUF**, if released (per `03_data_microdata.md`, not
  released as of that note). It would be a second, model-based prior for 2016.
- **Pseudo-panels.** Follow birth-year × sex cohorts across CPS or ACS
  cross-sections. Cohort-cell changes become change targets without linking
  records. This gives approach 3's cohort information with approach 1a's
  machinery.
- **Change targets for joint distributions SOI does not publish.** Examples:
  the share of joint returns with two earners by AGI band; the age mix within
  AGI bands. These are where surveys add most, because SOI says nothing.

## 9. The top of the distribution

Common treatment for all approaches:
- Surveys contribute change information only below a rank cut-off: about the
  90th percentile for CPS wages (Realtime Inequality's choice), lower for
  capital income.
- Above it, SOI governs. Table 1.4 brackets reach $10 million and over; SOI's
  top-percentile table gives shares and thresholds for the top 1%, 0.1%, 0.01%
  and 0.001% (I believe this is Table 4.3 in SOI's tax rates and tax shares
  series; I did not open it for this paper, and it is not in `IRS-Ind`). SSA
  wage statistics reach $50 million and over for wages.
- Between, a taper: Realtime Inequality's CPS weight goes to zero at the 90th
  percentile. In the precision framing a taper is simply a survey change
  target whose variance rises with rank.
- Records at the top stay as PUF records (and Forbes records) in every
  approach.

## 10. Series breaks

| Break | Years affected | Effect | Source |
|---|---|---|---|
| CPS ASEC income-question redesign | 2014 survey (income 2013) split sample; full from 2015 survey | levels shift; outside 2015–2023 | Census historical footnotes, verified |
| CPS ASEC updated processing system | 2019 survey (income 2018); Census publishes 2017 estimates on the updated system and says to use them for comparisons with 2018 onward | largest released values rose: interest from $99,999 (income years 2015–2017) to $200,000–280,000, dividends from $100,000 to $400,000–999,999; the wage maximum varies between $1.3 and $2.1 million with no clear break (my reading of the files) | Census footnotes, verified; maxima from the files, verified |
| CPS ASEC retirement income questions | from 2019 survey | `INCRETIR` falls 61% in dollars at the break; the repository bridges it with five components (decision S16) | `src/nonfilers/asec_tax_units.R`, verified |
| Pandemic nonresponse | 2020 survey (income 2019), and 2021 by the same authors' account (not verified) | nonresponse rose and became more related to income; after adjustment, 2019 median household income was 2.8% below the survey estimate | Rothbaum and Bee, Census working paper SEHSD WP2020-10, abstract verified |
| Population controls | 2020 Census-based and Vintage 2025 controls in later surveys | weights shift | Census footnotes, verified; survey years not checked |
| TCJA and form redesign | SOI 2018 onward | pensions and IRA published combined in 2018; capped SALT, QBI deduction added; exemptions removed | `IRS-Ind/NOTES.md`, verified |
| SOI W-2 statistics format change | 2019 | age tables dropped | `IRS-Ind/NOTES.md`, verified |

The practical rule: a survey change target that spans a break gets a larger
variance, or a bridge (as S16 does for retirement income), or is dropped.

## 11. Empirical check: do survey changes track administrative changes?

### What was compared

- **Earner level.** All W-2 wage earners in SSA's wage statistics ("net
  compensation": W-2 compensation plus deferred-compensation contributions),
  2015–2023, against CPS ASEC persons with positive wage and salary income,
  weighted by person weight. Percentiles from SSA's intervals by linear
  interpolation; from the CPS by weighted quantile.
- **Return level.** SOI Pub 1304 Table 1.4, all returns, 2015–2023
  (`IRS-Ind/aligned/panel_income_sources.csv`), against CPS ASEC tax units
  built with the repository's rules (`build_asec_tax_units`,
  `add_filing_requirement`). A CPS unit counts as a filer if it is required
  to file or is a non-dependent unit with wages. The AGI-like measure is the
  repository's gross income (wages, self-employment, interest, dividends, rent,
  unemployment, survivor and disability pensions, other income, harmonised
  retirement income). It leaves out capital gains (not asked before income year
  2018) and taxable Social Security. Brackets are SOI's nominal brackets.

Scripts and outputs: `/nfs/roberts/scratch/pi_nrs36/jar335/borrow_change_tmp/`
(scripts in `scripts/`, tables and logs in `out/`, SSA pages in `dl/`; main
script `check_growth.R`, tables `tables.R` and `yearly.R`). This is cluster
scratch space, not the repository, and may be purged. All computation ran in
Slurm jobs.

### Missing values and data handling

- CPS raw columns: no NA values in any year.
- CPS not-in-universe codes (persons under 15) set to zero income, as the
  repository's reader does: 28,429 persons (2023) to 41,861 (2015) per year.
  `INCOTHER` showed no not-in-universe code from income year 2018; the largest
  values after cleaning were checked in every year for leftover codes and none
  were found.
- No zero or missing CPS weights.
- SOI: 1,254 bracket cells flagged "combined" (amount reported in an adjacent
  bracket) were set to zero, so totals are unaffected. The $200,000–500,000
  bracket is published as one row; the split rows are empty in every year
  checked and were dropped after a check.
- `ipumsr` is not installed under the cluster's R modules, so the CPS files
  were read from their fixed-width layout and not-in-universe codes were
  blanked by rule (all nines). This should be noted if the repository's reader
  is ever compared.

### Results: wages per earner

Growth from 2015, percent (top shares: change in percentage points).

| Statistic | SSA 2015→19 | CPS 2015→19 | SSA 2015→22 | CPS 2015→22 | SSA 2015→23 | CPS 2015→23 |
|---|---|---|---|---|---|---|
| Earners | +5.3 | +5.4 | +7.0 | +5.0 | +8.0 | +6.4 |
| Mean wage | +12.6 | +17.3 | +32.7 | +34.6 | +38.6 | +38.1 |
| 10th percentile | +14.8 | +18.9 | +32.8 | +57.1 | +44.1 | +57.1 |
| 25th percentile | +20.0 | +22.2 | +47.0 | +44.4 | +57.9 | +50.0 |
| Median | +14.5 | +17.1 | +36.6 | +38.8 | +44.5 | +42.9 |
| 75th percentile | +11.7 | +16.7 | +29.8 | +33.3 | +36.1 | +36.7 |
| 90th percentile | +11.4 | +14.0 | +30.0 | +28.0 | +35.8 | +34.0 |
| 95th percentile | +12.0 | +15.4 | +31.8 | +34.6 | +37.1 | +38.5 |
| 99th percentile | +12.3 | +25.8 | +32.8 | +50.0 | +36.7 | +52.0 |
| Top 10% share (pp) | −0.4 | +2.9 | −0.4 | +2.8 | −1.0 | +2.0 |
| Top 1% share (pp) | −0.4 | +0.5 | −0.4 | +0.4 | −0.8 | −0.3 |

Levels differ: in 2015 the SSA median was $29,933 and the CPS median $35,000;
SSA counted 160.8 million earners and the CPS 154.1 million.

Cumulative mean-wage growth, year by year, CPS minus SSA in percentage points:
+3.1 (2016), +2.8 (2017), +2.8 (2018), +4.7 (2019), +1.9 (2020), +0.4 (2021),
+1.9 (2022), −0.5 (2023). The CPS ran ahead through 2020 and fell back. The
2019 gap is the largest; income year 2019 is the 2020 survey, where Census
found pandemic nonresponse biased income upward.

CPS percentiles are heaped at round numbers (the 90th percentile is exactly
$100,000 in 2015 and 2016), so single-percentile growth carries a few points of
rounding noise.

### Results: tax returns, totals

Growth from 2015, percent.

| Item | SOI 2015→19 | CPS 2015→19 | SOI 2015→22 | CPS 2015→22 | SOI 2015→23 | CPS 2015→23 |
|---|---|---|---|---|---|---|
| Returns | +4.9 | +4.7 | +7.2 | +6.1 | +6.7 | +7.8 |
| AGI (CPS: AGI-like) | +17.2 | +28.1 | +45.3 | +41.9 | +49.7 | +53.0 |
| Wages | +16.3 | +23.5 | +36.9 | +41.2 | +43.5 | +46.8 |
| Social Security (total) | +18.8 | +29.7 | +44.0 | +44.1 | +60.9 | +62.7 |
| Taxable interest | +59.8 | +176.5 | +39.3 | +65.0 | +227.3 | +238.2 |
| Ordinary dividends | +27.5 | +76.8 | +58.4 | +76.1 | +93.7 | +123.3 |
| Business (Schedule C and farm net) | +4.1 | +6.4 | +21.1 | +16.3 | +9.8 | +36.4 |
| Pensions and IRA (SOI taxable; CPS total) | +17.6 | +47.2 | +43.1 | +52.0 | +45.3 | +59.0 |
| Unemployment compensation | −21.4 | −14.7 | +11.1 | −22.5 | +13.6 | −5.4 |

Levels: the CPS rule gives 134.0 million filing units in 2015 against SOI's
150.5 million returns.

### Results: shifts across AGI brackets

Share of each item in four AGI groups, 2015 and 2023, percent. Nominal brackets,
so bracket creep moves everyone up in both sources.

| Item and AGI group | SOI 2015 | SOI 2023 | CPS 2015 | CPS 2023 |
|---|---|---|---|---|
| Returns, under $50k | 61.4 | 49.5 | 54.6 | 41.4 |
| Returns, $50–200k | 34.1 | 42.0 | 40.7 | 48.1 |
| Returns, $200–500k | 3.6 | 6.8 | 4.1 | 9.1 |
| Returns, $500k+ | 0.9 | 1.6 | 0.5 | 1.4 |
| Wages, under $50k | 22.6 | 15.1 | 19.5 | 12.0 |
| Wages, $50–200k | 51.5 | 48.8 | 57.5 | 50.8 |
| Wages, $200–500k | 14.8 | 21.6 | 16.4 | 26.3 |
| Wages, $500k+ | 11.0 | 14.4 | 6.6 | 11.0 |
| AGI, under $50k | 17.3 | 11.3 | 19.7 | 11.9 |
| AGI, $500k+ | 20.1 | 24.5 | 6.5 | 11.8 |
| Dividends, $500k+ | 46.2 | 52.4 | 4.1 | 23.7 |
| Interest, $500k+ | 45.5 | 53.2 | 3.6 | 11.1 |

AGI percentiles among returns, growth 2015→2023 (SOI from Table 1.4 brackets
by Pareto interpolation; CPS from its AGI-like measure): 10th +47.6 (SOI)
against +59.1 (CPS); 25th +46.0 / +45.2; median +41.5 / +36.7; 75th +34.2 /
+36.3; 90th +36.9 / +41.7; 95th +41.3 / +45.6; 99th +42.2 / +56.4. For
2015→2019 the CPS grows faster at every percentile, by 5 to 27 points.

Returns by bracket, growth 2015→2023 (SOI / CPS): $1–10k −28 / −42;
$50–75k +21 / +20; $100–200k +49 / +40; $200–500k +102 / +136;
$500k–1M +101 / +218; $1M+ +82 / +120. CPS unweighted filing units above
$1 million number 150 to 235 per year, and $500,000–1 million 251 to 640.

### What the check shows

- In the middle of the wage distribution, the CPS's change over eight years
  is within about 2 points of the administrative change. The mean is within 0.5
  points by 2023. Assumption (b) is roughly right there.
- It is not right at the bottom decile, at the 99th percentile, or for top
  shares, and it is not right year to year: year-on-year growth in the mean
  differs by up to 3 points (2016: 4.2% in the CPS against 1.1% in SSA; 2020:
  0.3% against 2.8%).
- For return-level aggregates of wages, AGI, Social Security and counts, the
  CPS's change from 2015 to 2022 or 2023 is within about 1–4 points of SOI's;
  from 2015 to 2019 it is 7 to 11 points too fast for wages, AGI and Social
  Security, while the count of returns matches. Both the 2019 processing
  change and the 2020 survey's nonresponse fall in that window.
- Above $200,000 of AGI the CPS overstates growth in counts by tens of points.
  Part of this is probably the higher released maxima from the 2019
  processing change (I did not separate it from other causes). The same
  change makes CPS capital income by bracket unusable as change information.
- Coincidence of totals by 2023 does not establish stable measurement error:
  the CPS's cumulative mean-wage growth ran up to 4.7 points ahead of SSA's
  in between.
- For a drift variance in the precision framing, these numbers suggest a
  standard deviation of 1 to 3 points per year for survey change targets on
  wages in the middle of the distribution, 10 points or more at the tails, and
  no use for capital income amounts. The backtest should measure this properly,
  by cell.

Limits of this check: one survey, one administrative wage source, nominal
brackets, an AGI-like measure without capital gains, and a filing rule that
reaches 89% of SOI's return count. SOI's W-2 statistics by age and sex
(2014, 2016–2020) would allow the same check within demographic cells, which is
what approaches 1a and 1b actually use; I did not run it.

## 12. Backtest design

Following `10_evaluation_design.md`:
- **Cutoff 2019.** Fit using SOI tables through 2019 and survey and SSA data
  through year t for each projected year t (surveys arrive before SOI tables,
  which is the point of borrowing change). Project 2020–2023.
- **Score** against Pub 1304: returns and amounts by AGI bracket (Table 1.4
  items), returns by marital status and age, and SOI's top-percentile table.
  Hold out income tax after credits (Table 3.3) for Tax-Simulator.
- **Benchmarks:** the current method (one growth factor per variable, cell
  weight factors, the 2017 linear program), and rescaling each variable's
  total only.
- **Two runs:** with realized totals (isolates distribution) and with the
  forecasts available at the time.
- **A second cutoff at 2017** (score 2018–2019) separates pandemic effects from
  the TCJA, and a third using only income years 2015–2018 avoids the 2019 CPS
  processing break.
- **Report** errors by bracket and year, and the drift variance of each survey
  change target, which then becomes that target's stated precision.

## 13. Ranked recommendation

1. **Age wages by the SSA change at each earner's percentile (approach 1b,
   SSA version), then calibrate.** Administrative, annual, full distribution,
   fills the specific gap the brief names. Low risk; two weeks is enough. Use
   proportional change at each rank, not changes-in-changes in levels.
2. **Add survey change targets for composition (approach 1a, entropy-balancing
   form) inside the planned precision-weighted calibration.** Demographic
   cells, and income-rank bands by demographic cell, below the 90th
   percentile; never capital income amounts. Precision from replicate weights
   plus a backtest-measured drift term. Larger variance or a bridge across
   2019–2020.
3. **Add CPS rank shifts by demographic cell to the wage mapping** (the
   Realtime Inequality method), after item 1 works. The check supports the CPS
   in the middle of the distribution only.
4. **Prototype approach 2 for the bottom 95% and the stable variables only.**
   It produces new records and so can change shapes that neither reweighting
   nor rank mapping reaches. It is also the most work and depends on
   conditionals that are unstable for several large items.
5. **Panel transitions only for history-dependent variables**, as a one-step
   model attached to each cross-section. A full forecast-then-correct loop is
   not justified for 2016–2023.

## Sources

**Verified (opened and read the relevant part)**
- Blanchet, Mouillet, Saez and Zucman, "Real-time inequality," *Journal of
  Public Economics* 260 (2026) 105701. https://eml.berkeley.edu/~saez/BMSZjpube26realtimeinequality.pdf
  (sections 4.3, Table 2, the data-sources table, appendix C.2).
- SSA, "Wage Statistics for 2015" … "2023" (net compensation intervals).
  https://www.ssa.gov/cgi-bin/netcomp.cgi?year=2015 (blocked to this cluster);
  read via https://web.archive.org/web/2025id_/https://www.ssa.gov/cgi-bin/netcomp.cgi?year=2015
  and the same for 2016–2023 (2020 from a December 2024 capture).
- Census Bureau, historical income table footnotes.
  https://www.census.gov/topics/income-poverty/income/guidance/cps-historic-footnotes.html
- Rothbaum and Bee, "Coronavirus Infects Surveys, Too: Survey Nonresponse
  Bias and the Coronavirus Pandemic," Census SEHSD WP2020-10 (abstract page).
  https://www.census.gov/library/working-papers/2020/demo/SEHSD-WP2020-10.html
- Madrian and Lefgren, "A Note on Longitudinally Matching Current Population
  Survey (CPS) Respondents," NBER Technical Working Paper 247 (1999); published
  as *Journal of Economic and Social Measurement* 26(1), 2000 (NBER page).
  https://www.nber.org/papers/t0247
- Bickel, Brückner and Scheffer, "Discriminative Learning Under Covariate
  Shift," *JMLR* 10 (2009) 2137–2155 (abstract page).
  https://www.jmlr.org/papers/v10/bickel09a.html
- Kanamori, Hido and Sugiyama, "A Least-squares Approach to Direct Importance
  Estimation," *JMLR* 10 (2009) 1391–1445 (abstract page).
  https://www.jmlr.org/papers/v10/kanamori09a.html
- Bricker, Bucks, Kennickell, Mach and Moore, "Surveying the Aftermath of the
  Storm: Changes in Family Finances from 2007 to 2009," FEDS 2011-17 (abstract).
  https://www.federalreserve.gov/pubs/feds/2011/201117/201117abs.html
- Repository and data store: `src/nonfilers/asec_tax_units.R`,
  `config/runscripts/baseline.yaml`, `IRS-Ind/NOTES.md`, the CPS ASEC
  `variables.csv` layouts, and the background papers 03, 04, 06, 08 and 10.

**Metadata only (Crossref record checked; content from memory)**
- Athey and Imbens (2006), *Econometrica* 74, 431–497. https://doi.org/10.1111/j.1468-0262.2006.00668.x
- Hainmueller (2012), *Political Analysis* 20, 25–46. https://doi.org/10.1093/pan/mpr025
- DiNardo, Fortin and Lemieux (1996), *Econometrica* 64, 1001–1044. https://doi.org/10.2307/2171954
- Chernozhukov, Fernández-Val and Melly (2013), "Inference on Counterfactual
  Distributions," *Econometrica* 81, 2205–2268 (Crossref lists no authors; the
  authors are from memory). https://doi.org/10.3982/ECTA10582
- Sugiyama, Suzuki and Kanamori (2012), *Density Ratio Estimation in Machine
  Learning*, Cambridge University Press. https://doi.org/10.1017/CBO9781139035613
- Qin (1998), *Biometrika* 85, 619–630. https://doi.org/10.1093/biomet/85.3.619
- Cannon, Sobie and Murdock (2015), *Journal of Climate* 28, 6938–6959. https://doi.org/10.1175/JCLI-D-14-00754.1
- Burkhauser, Feng, Jenkins and Larrimore (2012), "Recent Trends in Top Income
  Shares in the United States: Reconciling Estimates from March CPS and IRS Tax
  Return Data," *Review of Economics and Statistics* 94, 371–388.
  https://doi.org/10.1162/REST_a_00200 (directly relevant: a CPS–IRS
  comparison of trends; I did not read it).
- Bollinger, Hirsch, Hokayem and Ziliak (2019), "Trouble in the Tails?",
  *Journal of Political Economy* 127, 2143–2185. https://doi.org/10.1086/701807
- Hokayem, Bollinger and Ziliak (2015), *JASA* 110, 935–945. https://doi.org/10.1080/01621459.2015.1029576
- Abowd and Stinson (2013), *Review of Economics and Statistics* 95, 1451–1467. https://doi.org/10.1162/REST_a_00352
- Juhn, Murphy and Pierce (1993), *Journal of Political Economy* 101, 410–442. https://doi.org/10.1086/261881
- Machado and Mata (2005), *Journal of Applied Econometrics* 20, 445–465. https://doi.org/10.1002/jae.788
- Kopczuk, Saez and Song (2010), *Quarterly Journal of Economics* 125, 91–128. https://doi.org/10.1162/qjec.2010.125.1.91
- Larrimore, Mortenson and Splinter, "Household Incomes in Tax Data,"
  *Journal of Human Resources* 56, 600–631. https://doi.org/10.3368/jhr.56.2.0718-9647R1
- Evensen (1994), *Journal of Geophysical Research: Oceans* 99, 10143–10162. https://doi.org/10.1029/94JC00572
- Gordon, Salmond and Smith (1993), *IEE Proceedings F* 140, 107–113. https://doi.org/10.1049/ip-f-2.1993.0015
- Snyder, Bengtsson, Bickel and Anderson (2008), *Monthly Weather Review* 136, 4629–4640. https://doi.org/10.1175/2008MWR2529.1
- D'Orazio, Di Zio and Scanu (2006), *Statistical Matching: Theory and Practice*, Wiley. https://doi.org/10.1002/0470023554
- Deville and Särndal (1992), *JASA* 87, 376–382. https://doi.org/10.1080/01621459.1992.10475217
- Blanchet, Fournier and Piketty, "Generalized Pareto Curves," *Review of
  Income and Wealth* 68 (2022), 263–288. https://doi.org/10.1111/roiw.12510

Several of these (Juhn–Murphy–Pierce, Machado–Mata, Chernozhukov et al.,
Abowd–Stinson, Hokayem et al., Larrimore et al., Deville–Särndal, Blanchet et
al.) are listed as standard references for the techniques named; the text
above does not rest on any specific claim from them.

## What I looked for and could not find or verify

- SSA's wage statistics pages directly (ssa.gov returns 403 to this cluster);
  used Internet Archive copies instead. The 2020 capture is from December 2024;
  the others from late 2025 or early 2026.
- SOI's top-percentile table (believed to be Table 4.3 of the "tax rates and
  tax shares" series): not on disk in `IRS-Ind`, not opened.
- Whether the 2021 CPS ASEC had the same nonresponse bias as 2020: I read only
  the abstract of Rothbaum and Bee, which covers the 2020 survey.
- Which survey years first used 2020 Census-based population controls.
- Whether SOI publishes counts of 2020–2021 EITC lookback elections.
- SSA's Social Security benefit distributions by amount, for use as a driver.
- The last year in the linked CPS ASEC file (`CPS-ASEC-Panel/v4`).
- Full content of Burkhauser, Feng, Jenkins and Larrimore (2012), the most
  directly relevant earlier comparison of CPS and IRS trends.
