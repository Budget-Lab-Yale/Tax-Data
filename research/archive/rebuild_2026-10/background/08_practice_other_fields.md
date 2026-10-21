---
title: "How other fields build and project population microdata"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# How other fields build and project population microdata

Background research for the Tax-Data rebuild. Scope: everyone outside US
federal tax scoring who combines incomplete microdata with external totals
and carries the result forward in time. US tax scorers (PolicyEngine, TPC,
JCT, CBO's tax models, Tax-Calculator) are covered in a separate note.

Sourcing convention used below. **Verified** means I opened the document and
read the passage. **Not verified** means I saw only a search-result summary or
am relying on prior knowledge, usually because the site blocked automated
access (cbo.gov, ssa.gov, ifs.org.uk and treasury.govt.nz all returned
"forbidden" errors). Part-way through, the shared web-search allowance for
this session ran out, so some later items rest on documents whose addresses I
already knew.

## Summary

1. **Every model reviewed uses the same basic method.** It takes a base
   microdata file, grows each income source by its own index ("uprating"),
   adjusts weights to hit demographic and labour-market counts ("reweighting"
   or "static ageing"), and, for programs, selects which eligible units take
   up the program so that totals match administrative caseloads
   ("alignment"). None of them treats every target as a measurement with a
   stated precision, as the Tax-Data framing proposes. The nearest thing is a
   transport-planning tool, PopulationSim, whose weighting objective is
   "stay close to the starting weights, and pay a user-set penalty for
   missing each control." That is the proposed framing restricted to weights.

2. **The closest analogue to Tax-Data's post-2015 gap is the Piketty–Saez–
   Zucman (PSZ) and Realtime Inequality work.** They start from an old public
   use file (2012 in one documented update; 2014 per the 2026 paper), age it
   to later years using IRS tabulations, then hold each capital-income
   component's distribution fixed while rescaling its total. They change the
   *shape* of the distribution only for wages, using industry-by-county
   payroll tabulations (QCEW) and the monthly CPS.

3. **Their backtest supports that split.** Realtime Inequality ages each
   year's file one and two years forward, using only data that would have
   been available, and compares the result with the actual later file, for
   1975–2024. Holding capital-income distributions fixed works well because
   capital-income concentration moves slowly. Growing everyone at the
   aggregate rate is biased upward for the bottom 50% and downward for the
   top 1%. In recession years, using wage-distribution data cuts the bias
   for the bottom half from 3.6 to 0.2 percentage points.

4. **Tax-Data's single growth factor per variable is the standard method
   elsewhere for every source except earnings.** HMRC's projections of the UK
   tax-record sample grow earnings at different rates for seven bands of the
   pay distribution, using real-time payroll data, then rescale so mean
   growth matches the official forecast. DYNASIM calibrates to Social
   Security Trustees' assumptions on how much of total earnings falls under
   the taxable maximum, which is a long-run assumption about earnings
   dispersion.

5. **Published forecast-error records exist and are informative.** HMRC
   publishes how far its projections of the tax-record sample were off,
   compared with the sample data that later arrived (2001 onward). For
   taxpayer counts, total income and total liabilities, the average error is
   2–4% one year ahead and 4–6% three years ahead. The Federal Reserve's
   Distributional Financial Accounts drop one survey wave and check how well
   the method predicts it. BEA publishes ranges, not point estimates, for top
   shares in its provisional year.

6. **The two static-model manuals reviewed (Australia's CAPITA and
   Canada's SPSD/M) warn against long horizons.** CAPITA advises against
   use beyond about five years. Its reasons: wages are grown by one average
   index, and the population structure is held fixed. SPSD/M's manual notes
   that a growth factor changes the average amount per recipient, not the
   number of recipients. Pension and long-term models (DYNASIM, and by
   reputation CBO's long-term model and SSA's MINT) reach 75 years by
   simulating births, deaths, marriage, work and earnings for each person,
   and aligning to Trustees or CBO totals.

7. **Survey and tax data are merged in three ways.**
   - Reweight the survey's top tail to tax tabulations and replace incomes
     above a data-chosen "merging point" (the World Inequality Lab's method).
   - Match each tax record to one survey record so that both files' joint
     distributions survive (optimal-transport matching, in Realtime
     Inequality).
   - Hand out national-accounts totals in the proportions SOI reports by AGI
     group (BEA).

   Realtime Inequality's authors stress that a matched file says nothing
   reliable about pairs of variables that no source observes together.

8. **A matched or survey-built non-filer population tends to have too little
   wage income.** PSZ found that IRS information returns for 1999–2014 show
   about twice as many wage earners among non-filers as a CPS-based non-filer
   sample, each earning about twice as much. They scale the CPS non-filers
   up to match. Auten and Splinter build non-filers directly from information
   returns in 56 cells. This bears directly on Tax-Data's ASEC non-filer pool.

9. **Totals from the national accounts and totals from tax returns are not
   the same quantity.** BEA multiplies SOI amounts by audit-based
   underreporting factors before using them. Auten and Splinter allocate the
   national accounts' underreported income using IRS audit tabulations.
   Targets for a tax-return file should be stated in tax-return terms, with
   any national-accounts total bridged first.

10. **Rebasing is routine elsewhere.** The Dutch planning bureau (CPB)
    moved its base year from 2013 to 2016 to 2018 to 2021 as register data
    arrived. HMRC rebases to a new sample each year. The Urban Institute's
    health model (HIPSM) deliberately keeps a pre-ACA base year and simulates
    the law forward rather than use transitional-year data. That is the
    same logic as simulating post-TCJA itemization from pre-TCJA records.

## Terms used below

- **Uprating**: multiplying a dollar variable by a growth index.
- **Reweighting** (also **calibration**, **static ageing**): changing record
  weights so weighted totals match known counts, while keeping weights as
  close as possible to where they started. Methods differ in how "close" is
  measured:
  - **Raking** (iterative proportional fitting) adjusts weights to each
    control in turn, repeating until all match.
  - **Linear calibration** minimises a squared-difference measure.
  - **Entropy calibration** minimises a log-ratio measure, which keeps
    weights positive.
- **Alignment**: forcing a simulated yes/no outcome (employed, on a program,
  died) to hit an external count by choosing which units get the outcome.
- **Dynamic ageing**: simulating each person's life events year by year from
  estimated probabilities.
- **Temporal disaggregation**: estimating values between infrequent
  observations (for example, quarterly values between triennial surveys)
  using related series that are observed every quarter.
- **Optimal-transport matching**: pairing records of two files so the total
  distance between paired records on shared variables is smallest. Each
  record's weight must be fully used, so both files' distributions are kept.

## 1. Distributional national accounts

### 1.1 Piketty–Saez–Zucman (PSZ) synthetic microfiles

**Problem.** Distribute 100% of US national income and household wealth
across adults each year, starting from tax data that cover filers only, omit
non-taxable income, and come out late.

**Base data and combination** (verified, PSZ data appendix and SOI
working paper). The external version starts from public use files, 1962
onward. Where the public file leaves things out, they add internal-data
tabulations that Saez produced for SOI. These cover AGI above $10 million,
filers by age and gender, the split of earnings between spouses, and
non-filers. A synthetic record is added in 1996–2008 so that file totals
match IRS totals for each income component. Non-filers come from the CPS
using Tax Policy Center filing rules. The CPS non-filers are then reweighted
to adult population counts for ages 20–65 and 65+. Then the wage earners
among them get double weight, and their wages are scaled up, to match the
IRS information-return statistics on non-filers. In 1999–2014 those
statistics show "about twice as many wage earners in the IRS non-filer
sample than in our CPS built sample of non-filers, and conditional on being
a wage earner, they earn about twice as much." Saez's SOI paper reports that
filers plus information-return non-filers come within about 1% of Census
adult counts in most years from 1999 to 2013.

**Ageing past the last public use file** (verified, PSZ update log, February
2022 version). In the update the log describes:

- For 2013–2016 they started from the 2012 public use file and "update[d]
  the 2012 public use file to match the tabulated statistics from internal
  data." Those tabulations are by income group and demographic group for
  each income component.
- For 2017 they started from the 2012 file again, scaled weights uniformly
  to the adult population, and scaled each income component to SOI's
  published 2017 total.
- For 2018 and later, adult population grows 0.9% a year and each component
  grows with nominal GDP. They say plainly that this "amounts to assuming
  that the distribution of fiscal income in 2017 and after remains stable."
- Taxes after TCJA were recomputed with NBER TAXSIM.

The log does not describe the reweighting algorithm used to match the
tabulations.

**Validation.** Saez's SOI paper compares income shares from the public
files plus tabulations against the same statistics computed on internal IRS
files. They track closely for top shares, bottom-50% shares, age groups and
earnings splits. URL: https://irs.gov/pub/irs-soi/16rpsaeztabulations.pdf

**Public.** Microfiles, Stata code and archived vintages:
https://gabriel-zucman.eu/usdina/ (verified). Update log:
https://gabriel-zucman.eu/files/PSZUpdates.pdf (verified). Data appendix
(read from a university mirror, since I did not find it on the authors' site):
https://statmath.wu.ac.at/~hauser/LVs/Oetrie3/ALT/Folien/Projekte/DNA_DistributionalNationalAccounts/PSZ2018DataAppendix.pdf

### 1.2 Realtime Inequality (Blanchet, Mouillet, Saez, Zucman)

This is the most directly relevant project to Tax-Data's post-2015 problem.
Source: *Journal of Public Economics* 260 (2026), article 105701, verified in
full: https://eml.berkeley.edu/~saez/BMSZjpube26realtimeinequality.pdf .
Code (Stata, Python, R; BSD licence), verified:
https://github.com/leonmouillet/real-time-inequality .

**Base data.** The annual PSZ microfiles. The paper's source table states
that "the last public-use microfile dates from 2014. For later years, we
update the microdata using IRS tabulations of income," and that the latest
annual microfile is 2024. Each record is a synthetic adult.

**Combining with surveys.** They match the PSZ file one-to-one to the March
CPS (with ACS group-quarters records added so prisons, nursing homes and
dormitories are covered) and to the SCF. They use optimal transport on eight
income components (L1 distance, meaning the sum of absolute differences).
This brings in race, education and age. The match is done separately by
marital status, age above or below 65, and employment status. SCF variables
are used for the top 5% and CPS variables for the rest. The Python script
uses the POT optimal-transport library and splits the problem into wage bins
when the distance matrix would exceed 1 GB.

They test the match by checking that demographic composition by income
bracket is the same in the survey and in the matched file. They also warn
that the matched file "cannot provide reliable information about the joint
distribution of variables that are not jointly included in at least one of
the original databases". Their examples: income by state, and race at the
top of the wealth distribution.

**Ageing forward.** Three steps.

1. Rescale each component of income to its current national-accounts total,
   component by component. Quarterly-only series are split into months
   using Denton's method, a standard way to interpolate a low-frequency
   series along a high-frequency indicator.
2. Change labour income at the record level. Employment and unemployment
   insurance receipt are adjusted by cell (race × education × gender ×
   5-year age × marital status). Each cell's target comes from a logistic
   regression on the monthly CPS, and all cells together are forced to the
   BLS employment total. Within a cell, units to switch are chosen at
   random, not by predicted probability. Wage ranks are then shifted by cell
   using CPS rank movements. Wages are assigned from a target distribution
   by percentile. That distribution averages two sources: a QCEW-based
   estimate, and a CPS-based estimate for the bottom 80%, tapering to zero
   weight at the 90th percentile. The QCEW-based estimate treats each
   industry × county × ownership cell as a pseudo-record, and is corrected
   by a fixed regression to match tax-data percentiles. Values within
   percentiles are filled with generalized Pareto interpolation.
3. Hold the distribution within each capital-income component fixed and
   change only its total. New programs (stimulus payments, expanded credits,
   PPP loans) are allocated by program rules.

**Validation (backtest).** For every year 1975–2023 they age the year-t
file to t+1 and t+2 using only the data that would have been available.
They then compare predicted with actual growth by income group against two
benchmarks: uniform growth, and rescaling totals only (Table 2 and Appendix
Tables A1–A3). For one-year-ahead factor income:

| Group | Root mean squared error, their method | Same, uniform growth | Bias, their method | Bias, uniform growth |
|---|---|---|---|---|
| Bottom 50% | 2.0 pp | 2.8 pp | −0.3 pp | +0.8 pp |
| Middle 40% | 0.8 pp | 1.2 pp | +0.2 pp | +0.4 pp |
| Next 9% | 0.9 pp | 0.8 pp | −0.3 pp | −0.2 pp |
| Top 1% | 3.1 pp | 4.4 pp | +0.2 pp | −1.6 pp |

Rescaling totals alone does well in normal years but overstates growth for
the bottom half in recessions. That matches what BEA found in its own
quarterly prototype. The largest top-1% misses fall in tax-reform years
(1987–88), when income shifting is not visible in payroll tabulations.

### 1.3 Auten and Splinter

Source: *Journal of Political Economy* 132(7), 2024, verified:
https://www.davidsplinter.com/AutenSplinter-Tax_Data_and_Inequality.pdf
(DOI https://doi.org/10.1086/728741).

This is a historical series, not a projection model, but two of its methods
apply to Tax-Data.

- **Non-filers from information returns.** They use W-2, SSA-1099, 1099-R,
  1099-DIV, 1099-INT, 1099-G, 1099-MISC and K-1 amounts. These are tabulated
  for 56 non-filer groups (2 marital × 4 age × 7 income). "Married"
  non-filers are formed by pairing non-filing men and women at the same
  address. They use 2000, 2010 and 2018 and interpolate between. They also
  remove dependent filers and non-resident filers from the filer base,
  about 9% of returns in 2019 including married-filing-separately.
- **Underreported income.** The national-accounts adjustment for
  underreporting is allocated using IRS National Research Program audit
  tabulations: 10 misreporting-ratio groups × 11 reported-income groups,
  assigned at random within groups. 5% goes to non-filers.

They use internal SOI samples, so the method is documented but the data are
not public.

### 1.4 BEA distribution of personal income

Sources, verified: the technical document
https://apps.bea.gov/data/special-topics/distribution-of-personal-income/national/technical-document.pdf
and the landing page
https://www.bea.gov/data/special-topics/distribution-of-personal-income
(official years 2000–2023, provisional 2024, experimental 2025).

**Base data.** CPS ASEC households. BEA hands out personal-income totals
from the national accounts to them.

**Top incomes.** First, SOI amounts for wages, self-employment, dividends
and interest are multiplied by audit-based misreporting factors. Then, for
each of five components, SOI's shares in three AGI groups (under $200,000;
$200,000–$500,000; $500,000 and over) set what share of the national total
CPS tax units in the same AGI groups receive.

**Benefit underreporting.** BEA follows CBO's approach and uses a random
forest, with predictions made out of sample, to choose which CPS people
under-report SSI, Medicaid and SNAP. It then scales to administrative totals.

**Provisional year.** Estimated with a one-year lag using IRS 1040 data held
at Census. Top 1% and top 5% shares are published as ranges "reflect[ing]
analysis conducted with historical data." When final SOI data later arrived,
top shares fell inside the published ranges unless the method had changed.

A separate paper (Cornwall and Gindelsky, BEA WP2024-6, verified:
https://www.bea.gov/sites/default/files/papers/BEA-WP2024-6.pdf) nowcasts
the Gini and quintile shares from national-accounts aggregates using an
elastic net, a penalised regression. It produces summary statistics, not
microdata.

### 1.5 Federal Reserve Distributional Financial Accounts

Source: Batty et al., chapter in the NBER volume *Measuring Distribution and
Mobility of Income and Wealth*, verified:
https://www.nber.org/system/files/chapters/c14456/c14456.pdf . FEDS paper
page (not opened):
https://www.federalreserve.gov/econres/feds/introducing-the-distributional-financial-accounts-of-the-united-states.htm

**Method.** Three steps.

1. Reconcile each SCF balance-sheet item to its Financial Accounts
   household-sector (B.101.h) concept, and add the Forbes 400.
2. Interpolate between triennial SCF waves and forecast past the latest one.
   This is done for four wealth groups (top 1%, next 9%, next 40%, bottom
   50%) using the Fernández method. That method regresses the infrequent
   series on quarterly indicators and spreads the remaining gap across
   quarters as if it followed a random walk. The indicators are the B.101.h
   series itself, the S&P 500, the federal funds rate, the FHFA house price
   index and the debt-to-income ratio.
3. Multiply each group's share by the quarterly B.101.h total.

The output is group-level tables, not microdata.

**Validation.** They drop an SCF wave and predict it. Sums of squared errors
across 19 categories and 4 groups were about $4–5 trillion for the 2013
forecast, $23–24 trillion for 2016, and $5–7 trillion for the 2010 and 2013
interpolations. Total net worth was $75 trillion in 2013 and $90 trillion in
2016. The three error models they compared performed about the same.

### 1.6 World Inequality Database methods

**Generalized Pareto interpolation.** Blanchet, Fournier and Piketty
(*Review of Income and Wealth*, 2022) recover a smooth full distribution
from tax tabulations of thresholds and bracket averages. They do this by
interpolating the curve of b(p): the ratio of mean income above rank p to
the income at rank p. I did not open the paper; both the paper URL
https://halshs.archives-ouvertes.fr/halshs-02658851v1/file/2017-03_.pdf and
https://wid.world/gpinter/ blocked or returned no content. I verified only
that Realtime Inequality uses it to fill values within percentiles, and that
the WID working paper below points to an R package and web interface at
wid.world/gpinter.

**Combining a survey with tax tabulations.** Blanchet, Flores and Morgan,
"The Weight of the Rich" (WID working paper 2018/12, October 2019 version),
verified:
https://wid.world/www-site/uploads/2018/10/WID_WORKING_PAPER_2018_12_TheWeightOfTheRich_Oct19.pdf

1. Choose a "merging point" from the data: the income above which the ratio
   of tax density to survey density stops being flat.
2. Reweight the whole survey by truncated linear calibration (weights at
   least 1) to tax brackets above the merging point. The brackets are
   discretised from generalized Pareto interpolation. Age and sex totals are
   held at the same time. Brackets are grouped so that each has at least 5
   survey observations and no weight changes by more than a factor of 5
   either way.
3. Replace incomes above the survey's highest observation with draws from
   the tax distribution.

Below the merging point, every weight gets the same adjustment, so the
bottom of the distribution is not distorted. The calibration is presented
as a model of non-response that depends on income.

## 2. Other countries' tax-benefit models

### 2.1 United Kingdom

**HMRC Survey of Personal Incomes (SPI) and Personal Tax Model** (verified,
Income Tax Liabilities Statistics supporting documentation, June 2020:
https://assets.publishing.service.gov.uk/media/5ef45bc2e90e075c4fa0ab9a/Income_Tax_Liabilities_Statistics_June_2020_-_Supporting_Documentation.pdf ).

**Base data.** The SPI is a sample of tax records from payroll withholding,
self-assessment and repayment claims. It arrives about 21 months after the
tax year. Savings income missing at record level is imputed so that totals
match control totals. Dividends for people outside self-assessment are
imputed from the Family Resources Survey. HMRC states that the SPI does not
represent people below the personal allowance and makes "no attempt" to
estimate them, so the published statistics cover taxpayers only.

**Projection to the current year.**

- Weights are rescaled by main income source. Employment and
  self-employment cases are first scaled to ONS population projections by
  single year of age, separately for men and women, then scaled uniformly
  to the OBR's employment and self-employment forecasts. Other cases absorb
  the rest of the population change.
- Each income source is uprated by an OBR series.
- Pay is the exception. Each SPI case falls in one of seven bands cut at
  the 10th, 25th, 75th, 90th, 95th and 99th percentiles of the real-time
  payroll (RTI) distribution. Each band's growth is adjusted by the gap
  between that percentile's historical RTI growth and mean growth. Then all
  cases are rescaled so mean earnings growth matches the OBR.
- Behavioural adjustments are made for dividend forestalling around rate
  changes.

The 2024 release (base year 2019-20, projected to 2023-24) describes the same
approach: every income source grown at one rate for everyone except pay
(verified summary:
https://www.gov.uk/government/statistics/income-tax-liabilities-statistics-tax-year-2020-to-2021-to-tax-year-2023-to-2024/bulletin-commentary ).
That release skipped the newest year, 2020-21, as a base because COVID
distorted it.

**Published projection errors** (Table 6, projections released after spring
Budgets since 2001). Mean absolute error:

| Horizon | Taxpayers | Higher/additional-rate taxpayers | Total income | Total liabilities |
|---|---|---|---|---|
| One year ahead | 2% | 4% | 2% | 3% |
| Three years ahead | 4% | 5% | 5% | 6% |

HMRC suggests one standard deviation of past errors as a rough 68% band, and
warns that projected regional breakdowns and income shares are "indicative."

**ONS household income nowcast** (2016 methodology, experimental; verified
summary:
https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/incomeandwealth/methodologies/nowcastinghouseholdincomeintheukmethodology2016 ).

- Base: three pooled years of the Living Costs and Food Survey.
- Uprating: earnings by the Annual Survey of Hours and Earnings and the
  national accounts; benefits by CPI.
- Reweighting: to Annual Population Survey totals, including 12
  economic-status groups.
- Policy: HM Treasury's tax-benefit model (IGOTM).

ONS says the direction of change is more reliable than the level. I did not
establish whether the series is still published.

**IFS TAXBEN and UKMOD.** Not verified; ifs.org.uk blocked access. UKMOD's
public resources page lists modelling-convention documents
(https://www.microsimulation.ac.uk/ukmod/resources/), which I did not read.
From prior knowledge: both run on the Family Resources Survey. UKMOD follows
the EUROMOD convention of uprating each income source by its average change.
IFS reweights to OBR demographic and labour-market forecasts for its
projections of poverty and inequality.

### 2.2 EUROMOD and Eurostat flash estimates

**EUROMOD updating** (verified, EUROMOD country report for Germany, release
Y16:
https://euromod-web.jrc.ec.europa.eu/sites/default/files/2026-02/Y16_CR_DE.pdf ).
"Updating factors are generally based on changes in the average value of an
income component between the year of the data and the policy year." The
base data is EU-SILC; for Germany, EU-SILC is the only source. The standard
model does not reweight. An optional "benefit calibration adjustment"
randomly drops eligible units from a benefit until simulated spending
matches actual spending.

**Eurostat flash estimates for 2025** (verified, methodological note, May
2026:
https://ec.europa.eu/eurostat/documents/d/experimental-statistics/methodological-note-flash-estimates-2025 ).

- Base: EU-SILC 2024, which records 2023 incomes.
- The labour market is updated by simulating individual transitions, not
  by reweighting. Transitions are chosen over reweighting since the COVID
  shock.
- Probabilities of moving into or out of employment come from logistic
  regressions on the longitudinal Labour Force Survey.
- Within strata (sex, age, employee or self-employed, contract type,
  sector), individuals are sorted by probability and the highest are
  selected until the Labour Force Survey's net employment change is met. A
  second round fills any remaining gap by sex and age only.
- New workers get earnings by predictive mean matching. That method picks
  a real donor whose predicted earnings are closest to the recipient's.
- Wages are uprated by sector-specific labour cost indices.
- Policies are simulated in EUROMOD.

**A caution on that selection rule** (verified summary of Li and
O'Donoghue, "Evaluating binary alignment methods in microsimulation
models," *Journal of Artificial Societies and Social Simulation* 17(1), 2014:
https://www.jasss.org/17/1/15.html ). Picking the units with the highest
predicted probability "severely distorts distributions," because
low-probability units are never chosen. Methods that add a random term
before sorting, or adjust probabilities smoothly, keep the model's
distribution better. Eurostat's fine strata limit the problem. Realtime
Inequality avoids it by choosing at random within cells.

A survey of nowcasting in fifteen models (O'Donoghue and Loughrey, same
journal, 17(4), 2014, verified summary: https://www.jasss.org/17/4/12.html )
found about half used reweighting and most projected only 1–10 years.

### 2.3 Canada: SPSD/M (Statistics Canada)

This is the model closest in structure to Tax-Data: a survey host enriched
with tax records, then grown forward. Verified sources: database creation
guide and growth and validation guide for version 20.0, base year 2008:
https://datalib.usask.ca/dli/social/spsdm/spsdm-v20.0/doc_en/database%20creation%20guide.pdf
and
https://datalib.usask.ca/dli/social/spsdm/spsdm-v20.0/doc_en/growth%20and%20validation%20guide.pdf .
The current release is version 30.1.1 with database year 2018
(https://www150.statcan.gc.ca/n1/en/catalogue/89F0002X2024002, verified). I
found no public creation guide for versions after 20, so I do not know
whether the host survey or methods have changed.

**Building the base year** (version 20).

- Host: the Survey of Labour and Income Dynamics. About 80% of respondents
  allowed linkage to their tax return.
- High incomes: records from a large sample of tax returns (the
  "Greenbook") are grouped into cells of at least 5 similar filers and
  averaged. Host records are duplicated and sorted on an imputed total
  income, then matched to these averaged records. Grouping is the
  disclosure protection.
- Unemployment insurance: claim histories are attached by matching within
  cells of administrative records.
- Group quarters: institutionalised elderly are created by cloning host
  records.
- Missing tax items (about 40 deductions and credits) are imputed
  stochastically.

**Growing to other years.**

- Household weights are raked to provincial age-sex population estimates
  and projections, and simultaneously to Labour Force Survey employment
  and unemployment rates.
- Dollar items: "for historical years, the year to year change in each
  growth factor parameter is forced to reproduce the growth of a stable T1
  tax series — all on a provincial basis." Future years use private-sector
  projections supplied by the Parliamentary Budget Officer.
- The manual describes this as static growth that "force[s] agreement with
  aggregate control totals by minimal change (if any) to the underlying
  micro data." It argues that each change to microdata "will either weaken
  or distort correlations that should be present or introduce spurious
  correlations."

The manual also warns: "this technique increases mean amounts per individual
or per family. If the changes responsible for the discrepancy ... result
from distributional differences, this approach may lead to false
inferences." Its example: raising social assistance totals by 20% through a
growth factor raises the average benefit, when in fact the caseload grew.
For count changes it offers new weight files or "record conversion"
(switching selected records into recipients).

**Validation.** Base-year tables compare model totals with the national
accounts, the T1 and T4 files and program records. For example, wages are
within 0.4% of the T4 total; self-employment income is 19% above the T1
figure. The manual notes that future years "can only be validated with
other projections."

### 2.4 Australia

**Treasury CAPITA** (verified, Treasury working paper 2017-05:
https://treasury.gov.au/sites/default/files/2019-03/170915-PT-Treasury-Research-Institute-Paper-CAPITA-MSN-edits.pdf ).

- Base: the ABS Survey of Income and Housing, plus Census records for
  people in non-private dwellings. Tax deductions are imputed from tax data.
- Uprating: income items by CPI or average weekly earnings.
- Weights: adjusted "in a way which minimises the change in the weights."
  Family members share a weight. The benchmarks are age-sex counts, births,
  households, labour-force status and recipients of each payment.
- Treasury warns that "including too many benchmarks can cause
  interactions between benchmarks and distort the weights."
- Treasury advises against use past the four-year forward estimates
  because wages are uprated by one average, the income distribution does
  not change, and population structure is held fixed.

**Grattan Institute `grattan` R package** (verified documentation:
https://search.r-project.org/CRAN/refmans/grattan/html/project.html ). It
projects the ATO 2% sample of tax returns. Wages can be uprated
differentially by income position (the default for sample files from
2016-17 and earlier). Work-related deductions grow with the labour force and
capital gains by their own index. Weights can be moderated by age-specific
population forecasts.

**Parliamentary Budget Office SMART** (not verified; search summary only:
https://www.pbo.gov.au/publications-and-data/data-and-tools/SMART ). It
reportedly uses a 16% sample of ATO records benchmarked to projected filer
counts, assumes equal wage growth across the distribution, and adjusts
revenue totals to official projections.

**NATSEM STINMOD+.** I found no current public technical documentation.

### 2.5 New Zealand: Treasury TAWA

Not verified; treasury.govt.nz blocked automated access. Search summaries
say TAWA pools three years of the Household Economic Survey and adds
Inland Revenue and Ministry of Social Development records linked in Stats
NZ's Integrated Data Infrastructure (IDI). A methodology report exists, as
do older Treasury working papers on minimum-distance reweighting of the
survey (WP 03/17 and WP 03/33:
https://www.treasury.govt.nz/publications/wp/survey-reweighting-tax-microsimulation-modelling-wp-03-17 ).
I could not read how TAWA projects to forecast years.

### 2.6 Netherlands: CPB MIMOSI

Verified (CPB background notes, in Dutch:
https://www.cpb.nl/sites/default/files/omnidownload/CPB-Achtergronddocument-dec2019-Gevolgen-koopkrachtramingen-van-overstap-op-nieuw-bronbestand.pdf ,
https://www.cpb.nl/sites/default/files/omnidownload/CPB-publicatie-verantwoording-IPO-verwerking-2021.pdf ,
https://www.cpb.nl/sites/default/files/omnidownload/CPB-Notitie-1okt2017-Raming-aantal-personen-huishoudens-onder-de-lageinkomensgrens.pdf ).

- **Base.** A sample of about 100,000 core persons and their household
  members (about 280,000 people) drawn from Statistics Netherlands'
  register-based income and wealth statistics.
- **Rebasing.** The base year moved 2013 → 2016 (December 2019) → 2018
  (June 2021) → 2021. CPB kept 2021 despite COVID and removed the
  COVID-only programs from it, so that the gap between base and forecast
  years would not grow too large.
- **Forecast years.** Individual job changes, marriages and divorces are not
  simulated. The file is reweighted to expected counts of one- and
  two-person households, children, self-employed, students, and recipients
  of unemployment and social assistance.
- **Algorithm.** A search summary of CPB documentation says the
  Huang–Fuller bounded iterative procedure is used to keep weights
  positive. I did not open that document.

### 2.7 Germany

FiFoSiM (verified summary of Peichl and Schaefer, *International Journal of
Microsimulation*: https://microsimulation.pub/articles/00008 ):

- Base: a 10% sample of the 1998 federal income tax statistics (FAST) and
  the household panel survey (SOEP), each used to fill the other's gaps by
  statistical matching. Regression-based in one direction, propensity score
  in the other.
- Ageing: weights adjusted over 52 cells (13 age groups × joint or separate
  assessment × East or West); income types uprated by different factors.
- Validation: against official revenue estimates for 2005–2007.

German tax statistics come out every three years with a lag of five to six
years (search summary), so German modellers face a long gap like
Tax-Data's. ZEW-EviSTA documentation (ZEW discussion paper 22-026:
https://ftp.zew.de/pub/zew-docs/dp/dp22026.pdf) was not opened.

## 3. US health and transfer models

**Urban HIPSM** (verified, model page:
https://www.urban.org/research/data-methods/data-analysis/quantitative-data-analysis/microsimulation/health-insurance-policy-simulation-model-hipsm ).

- Core data: the 2012 and 2013 ACS, combined and reweighted to 2013.
- Urban chose pre-ACA data deliberately "because later ACS data reflect the
  ACA's transitional period." The model is calibrated each year to the
  latest Medicaid and marketplace enrollment by state.
- Future-year weights come from Urban's population projections, which
  match Census projections nationally.
- Employer offers, immigration status and program eligibility are imputed,
  and workers are grouped into synthetic firms.

**Urban TRIM3** (verified page:
https://www.urban.org/research/data-methods/data-analysis/quantitative-data-analysis/microsimulation/transfer-income-model-trim ).
It runs on each year's CPS ASEC and "corrects for underreporting of benefits
in survey data." From prior knowledge, not verified: it simulates
eligibility under program rules, then selects participants among eligible
units so caseloads match administrative totals by characteristic.

**RAND COMPARE** (verified, RAND TR-971, 2011, appendix describing the model:
https://www.rand.org/content/dam/rand/pubs/technical_reports/2011/RAND_TR971.pdf ).

- Base: 2001–02 SIPP, 2002–03 MEPS and 2006 Kaiser/HRET data, combined into
  a synthetic population of people, families and firms.
- Ageing: the data were aged with Census population projections and
  National Health Expenditure Accounts projections.
- Top tail: the top 1% of spending was recalibrated to a Society of
  Actuaries large-claims database, because MEPS misses the extreme tail.

**CBO's health insurance simulation model (HISIM2).** Not verified; cbo.gov
blocked access. From prior knowledge: it builds on survey data (SIPP-based,
with other surveys matched in) and is projected over the ten-year window to
match CBO's demographic and economic projections and enrollment data.

## 4. Dynamic and pension models

**Urban DYNASIM** (verified, Favreault, Smith and Johnson, "The Dynamic
Simulation of Income Model (DYNASIM): An Overview," September 2015:
https://www.urban.org/sites/default/files/publication/67366/2000391-The-Dynamic-Simulation-of-Income-Model-DYNASIM-%20An-Overview.pdf ).
This describes DYNASIM3; DYNASIM4, on the 2004 and 2008 SIPP, was announced
for 2016.

- **Base.** A self-weighting sample of about 113,000 people from the
  1990–93 SIPP, adjusted toward Social Security area population totals
  (raising historically undercounted groups). Earnings histories come from
  matching to the PSID and to a 1972 CPS–SSA exact match. Assets are
  adjusted to the 2007 SCF distribution because SIPP asset data are weak.
- **Ageing.** Each person's events are simulated year by year from
  estimated equations: death, birth, marriage, divorce, immigration,
  education, disability, work, hours, wages, job change, pensions and
  claiming.
- **Alignment.** Employment by age and sex is aligned to Trustees' rates.
  Annual earnings are aligned to Trustees' earnings totals. The model also
  calibrates to the Trustees' assumption on "wage dispersion (namely, the
  share of aggregate earnings that are taxable)". The top 0.1% of earners
  have their own procedure. Mortality, fertility, immigration and
  disability incidence are aligned to the Trustees' intermediate
  assumptions.
- **Horizon.** 75 years (to 2087 at the time). Federal income tax is
  computed with a calculator run on SIPP records matched to an SOI file.

**CBO long-term model (CBOLT) and SSA's MINT.** Not verified; both agencies'
sites blocked access. From prior knowledge:

- CBOLT is a dynamic microsimulation of a sample built from SIPP matched to
  SSA earnings records. It projects demographics, earnings and benefits
  over 75 years, aligned to CBO's projections.
- MINT starts from SIPP panels matched to SSA administrative earnings and
  benefit records. It projects retirement income for cohorts, aligned to
  Trustees assumptions.

CBO also has a working paper titled "Projection and Alignment Methods for
Static Microsimulation Models" (https://www.cbo.gov/publication/52147),
which I could not open.

**What these models imply for 2097.** All three get to 75 years by simulating
events for each person and aligning to official demographic and economic
assumptions, not by reweighting a fixed base file. The Trustees' long-range
assumptions include the taxable share of earnings, which pins down a part of
earnings dispersion through the 2090s. Tax-Data could use that as a target.

## 5. Nowcasting the income distribution

Already covered above:

- Realtime Inequality (section 1.2): monthly microdata with a published
  backtest.
- BEA's provisional year and machine-learning nowcast (section 1.4).
- Federal Reserve DFA (section 1.5): quarterly, by temporal disaggregation.
- ONS (section 2.1): reweighting plus uprating, experimental.
- Eurostat flash estimates (section 2.2): simulated labour-market
  transitions plus EUROMOD.

The European Central Bank publishes experimental Distributional Wealth
Accounts, built in a similar way to the DFA from its household finance
survey. The two addresses I tried returned "not found," so I have nothing
verified to report on their method.

The common finding across these projects: the totals carry most of the
short-run signal. Changes in the earnings distribution, driven by employment
swings, are the main thing a pure rescaling misses, and they matter most in
recessions.

## 6. Synthetic populations in transport and epidemiology

These fields build full synthetic populations (every household, often
located to small areas) from a sample (usually census microdata) and
marginal counts for many areas.

**PopulationSim** (open-source, ActivitySim consortium; verified:
https://activitysim.github.io/populationsim/application_configuration.html ).

- Reweighting is by relative entropy: minimise
  Σₙ xₙ·ln(xₙ/wₙ) over new weights xₙ, given starting weights wₙ.
  Household and person controls are handled at once, across several
  geographic levels.
- Each control i is relaxed by a factor zᵢ. The objective adds a penalty
  uᵢ·zᵢ·ln(zᵢ), where uᵢ is a user-set "importance."
- The documentation: "High importance weights (e.g., 1E10) on all controls
  result in a hard constrained optimization ... Low importance weights
  (e.g., <50) results in an almost unconstrained problem. The user may also
  specify different importance weights for each marginal control."
- A cap on expansion (default 30 times the starting weight) bounds each
  weight.
- Linear programming then rounds weights to whole numbers, and records are
  copied that many times.

This is the "prior plus soft targets with stated precision" idea applied to
weights, with hard and soft constraints separated by the size of uᵢ. In
Tax-Data terms, uᵢ would be set from each target's reliability.

**Other methods** (not verified; prior knowledge). Older tools use iterative
proportional fitting, or "iterative proportional updating" (Ye and others,
2009), which matches household and person totals together. Combinatorial
optimisation selects a whole-number set of sample households to fit area
totals. Epidemiological agent-based models (for example the RTI US synthetic
population) use the same fitting on census microdata, then add place and
contact structure. Their lesson for Tax-Data is narrow. These are mature,
open tools for fitting weights to many partly conflicting totals with
explicit trade-offs, and for converting weights into duplicated records when
a downstream model needs whole units.

## 7. Practices most worth borrowing for Tax-Data

### 7.1 A pseudo-real-time backtest with a naive benchmark

Realtime Inequality, HMRC and the DFA all judge their ageing method the same
way. They pretend the latest data do not exist, project, and compare with
what later arrived. The comparison is by group and horizon, against a simple
benchmark (uniform growth, or rescaling totals only). HMRC publishes a
running error table.

Tax-Data can do this two ways. Within SOI's published tables, hold out some
2016–2022 tables while fitting and score the file against them. If earlier
public use files are available to the Budget Lab, age them to 2015 and
compare with the 2015 file. The output is a table of errors by variable,
income group and horizon. That table would turn "how much weight should the
prior get" from a judgement into a measured quantity: the observed error at
each horizon is an estimate of a target's precision.

### 7.2 Entropy-type reweighting with per-target importance and bounded ratios

The method John proposes for weights already exists in working, documented
form. Three versions:

- PopulationSim's relative-entropy objective with an importance per
  control.
- The World Inequality Lab's truncated calibration with brackets grouped
  so that no weight moves by more than a factor of 5.
- CPB's bounded iterative procedure.

Each keeps weights close to the starting file and lets each target miss in
proportion to how little it is trusted. That avoids the corner solutions of
the current linear program, where 62% of records sit at the 0.34 floor and
38% at the 1.66 cap. CAPITA's warning applies: many interacting targets
distort weights. A bound on weight ratios and a report of how far weights
moved (for example, the effective sample size) should come with the method.

### 7.3 Change distribution shape only where there is direct evidence; keep counts and means separate

The backtested pattern is:

- Rescale each income component's total.
- Leave the distribution within each capital-income component alone in the
  short run.
- Change the earnings distribution using direct evidence.

HMRC uses payroll percentiles. Realtime Inequality uses industry-by-county
payroll tabulations and the CPS. For the US, candidate sources are SSA wage
statistics by earnings bracket, SOI wages by AGI bracket, and QCEW.

SPSD/M's warning applies to Tax-Data's `grow_with` column. A single factor
raises the average per recipient. Any rise in the number of people with a
given income source must come from weights or from changing records. Each
variable's growth should be split into growth in recipients and growth per
recipient, and each part targeted separately where data allow.

### 7.4 For long horizons, align to official demographic and earnings-dispersion assumptions, and say what is held fixed

The static models (CAPITA, SPSD/M) say they should not be used far ahead.
The long-horizon models (DYNASIM, and by reputation CBOLT and MINT) get
there by aligning to Trustees or CBO assumptions on population by age,
employment, mortality, immigration and the taxable share of earnings.

Tax-Data does not need dynamic simulation to borrow the useful part. Treat
those assumptions as the main targets for later years, including the
taxable-earnings share as a target on the earnings distribution. Then state
in the documentation which joint relationships are frozen at the base year.

### Two further points

**Non-filers.** PSZ's and Auten and Splinter's evidence suggests a
survey-built non-filer pool understates wage earners and wages among
non-filers. Published SOI information-return tabulations for non-filers, if
any exist for recent years, would be the natural targets. I did not check
which tabulations are public; Saez's 2016 paper said the non-filer
tabulations would be "publicly disclosed at a later date."

**Concept bridging.** BEA and Auten and Splinter adjust between
national-accounts and tax-return concepts before using either as a target.
Tax-Data targets that come from national-accounts series (consumption,
wealth, interest) should go through an explicit bridge, with its own stated
uncertainty.

## What I looked for and could not find or verify

- CBO: the HISIM2 description, the CBOLT overview, and "Projection and
  Alignment Methods for Static Microsimulation Models." cbo.gov returned
  403 for both pages and PDFs.
- SSA: MINT documentation and the Trustees' assumption on the taxable share
  of earnings. ssa.gov returned 403.
- IFS TAXBEN projection method. ifs.org.uk returned 403.
- New Zealand TAWA methodology report and Treasury working papers. Blocked
  by a bot challenge.
- CPB MIMOSI main model documentation. Two PDF links returned web pages,
  not files. The Huang–Fuller detail rests on a search summary.
- SPSD/M documentation newer than version 20. The public library mirror
  stops there.
- NATSEM STINMOD+ technical documentation. None found.
- Generalized Pareto interpolation paper, the gpinter tool page and the
  EUROMOD labour-market add-on document. The archive site blocked access,
  and the others returned no usable content.
- European Central Bank Distributional Wealth Accounts methodology. The
  addresses I tried returned "not found."
- Eurostat flash-estimate backtest error figures. The 2016 quality paper is
  listed at http://www.ine.es/q2016/docs/q2016Final00290.pdf but I did not
  open it.
- Which reweighting algorithm PSZ used to make the 2012 public use file
  match internal tabulations for 2013–2016. Their update log does not say.
