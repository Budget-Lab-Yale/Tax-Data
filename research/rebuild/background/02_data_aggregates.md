---
title: "Non-IRS aggregate and tabular data for targets, nowcasts and projections"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# Non-IRS aggregate and tabular data for targets, nowcasts and projections

This note covers published totals, distributions and projections from
sources other than IRS Statistics of Income (SOI). It looks at three uses:

- targets for the joint distribution of tax-unit variables after 2015;
- nowcasts for 2024–2026, the years IRS data do not yet cover;
- projections to 2097.

Survey microdata and SOI are covered in other notes.

## Summary

1. **CBO's revenue workbook is the most useful non-IRS target.** Sheet 3
   projects Form 1040 lines for 2022–2036: each income source, AGI,
   deductions (including the 2025 law's new ones), taxable income by
   bracket, return and itemizer counts, and top 1–50 percent AGI shares.
   `resources/cbo_1040.csv` comes from it, but the repo uses only eight
   columns, as growth rates. Only 2022 is "actual".
2. **Little published data reaches 2097, and none of it is about tax
   returns.** It covers population by age, sex and marital status (CBO
   to 2099, already used), the share of earnings below the Social
   Security taxable maximum (CBO, 17 September 2026, to 2100), the
   Trustees' tables to 2100, and AGI shares by statutory rate in 2056.
   After CBO's 10-year window, Macro-Projections holds income components
   at fixed shares of GDP.
3. **Timely nowcast sources for 2024–2026:** BEA personal income (to
   August 2026), QCEW wages (to 2026Q1), Treasury receipts, Census state
   tax collections, Z.1 and DFA (to 2026Q2), Nonemployer Statistics (to
   2024). SSA's W-2 wage statistics (2024; 2025 due mid-October 2026)
   are the only timely *distribution*: workers and wages by bracket up
   to $50 million and over.
4. **BEA revised its IRS-based components on 30 September 2026.** 2025
   personal interest rose $421 billion; dividends and proprietors' income
   fell. Earlier vintages are stale for 2023 onward.
5. **BEA no longer reconciles personal income with AGI** (last year
   2005). National-accounts totals are mostly growth signals. Only a few
   lines are near a 1040 concept: monetary interest, owner-occupied
   mortgage interest, and wages net of deferrals.
6. **Bracketed tables on a tax-unit basis are few, and mostly built from
   SOI.** JCT's JCX-45-25 has 2025 counts and amounts of SALT,
   charitable, mortgage and medical deductions by income class. That is a
   direct target for simulating who itemizes. Others: CBO household
   income by group (to 2022), Treasury's 2024 family counts,
   Auten–Splinter (to 2022), BEA's household distribution. None should be
   weighted as independent of the prior.
7. **The sources of the current resource files are undocumented.** I
   traced them: `cbo_1040.csv` to CBO sheet 3; the mortgage series to
   BEA owner-occupied mortgage interest; the SALT series to NIPA Table 3.3
   (property taxes include business property); charity to Giving USA.
   The `ui` column's source is not identified.
8. **Several inputs are a vintage or more behind:** SSA life tables
   (2024 report), the local DFA copy (to 2025Q4), Giving USA (now through
   2025), and the BEA series behind mortgage interest and SALT.
9. **No data exist yet on the 2025 law's new deductions.** Tax-year 2026
   W-2s will carry tip and overtime codes; they reach the IRS in 2027.
   Until then: CBO's "additional deductions" line, JCT cost estimates,
   BLS counts of tipped jobs.
10. **Gaps no non-IRS source fills:** IRA distributions, capital-gains
    realizations (only CBO and Treasury), pass-through income by
    recipient, and US-assembled car loans. Giving USA starts from SOI, so
    it is a circular check on the charitable deduction. The 2025 ACS
    one-year release is delayed with no date.

## How this was checked

- I read the repository code and resource files and the shared raw-data
  holdings myself. I also opened the CBO, BEA and Macro-Projections
  workbooks stored on the cluster.
- Four research helpers did the web search in parallel, one per group:
  - CBO, JCT and Treasury;
  - BEA, the Federal Reserve and distributional national accounts;
  - SSA, BLS, Census and the Centers for Medicare & Medicaid Services
    (CMS);
  - private and other trackers.
- cbo.gov, jct.gov landing pages and ssa.gov refuse automated requests,
  so those pages were read through Internet Archive copies, most taken
  in September 2026. JCT PDFs, Treasury, BEA, BLS, Census, the Federal
  Reserve and the Treasury Fiscal Data API were read directly.
- The helpers ran out of web-search allowance partway through. A few
  items are therefore marked as seen only in search results.

**Verified** means someone in this pass opened the page or file and saw
the fact stated. **Not verified** means it rests on a search-result
snippet, a link title or memory. Release dates and coverage are as of
6 October 2026.

## What the repository uses today

| Resource or interface | What it is (source) | How it is used | Checked |
|---|---|---|---|
| `resources/cbo_1040.csv` | CBO, "Revenue Projections, by Category", February 2026, sheet "3. Individual Income Tax Details": wages, taxable interest plus ordinary dividends, qualified dividends, capital gain or loss, net business income, taxable pensions, taxable Social Security, total income. The current sheet starts in 2022, so rows 2019–2021 presumably come from earlier CBO vintages (not checked). | Year-on-year growth of each aggregate, 2020–2036 (`income_near` in `src/project_puf.R`). Eight of the file's nine columns are used; `txbl_ss` is read but dropped: `ss` grows with CBO Social Security outlays from 2020. | Verified against the local copy of the workbook at `shared/raw_data/CBO-Budget-Proj/v3/20260211/baseline/Revenue.xlsx` |
| `ui` column in the same file | Unknown. It does not match NIPA Table 3.12 unemployment insurance (2020: 471.5 here vs 528.0 in NIPA), nor the SOI Table 1.4 totals in Compiled-SOI-Tables. It may be CBO fiscal-year unemployment compensation outlays. | Growth of `ui` | Source not identified |
| `resources/itemized_deduction_projections.csv`, `mortgage` | BEA "Monetary interest paid: households: owner-occupied housing", NIPA interest table (FRED W498RC1A027NBEA). 2017–2020 match exactly. Later years differ slightly from current BEA values: 2024 is 474.7 here vs 477.2 now. | Growth of mortgage interest and balances, 2018–2024 | Verified, https://fred.stlouisfed.org/series/W498RC1A027NBEA |
| same file, `salt_inc_sales`, `salt_prop` | NIPA Table 3.3, state and local: income taxes plus sales taxes, and property taxes, as annual averages of the quarterly values. 2017 matches to the decimal. | Growth of SALT components | Verified against `shared/raw_data/NIPA/20240125/3-3.csv` |
| same file, `charity` | Giving USA, giving by individuals ($286.65 billion in 2017, $392.45 billion in 2024) | Growth of charitable deduction | Verified for 2024, https://givingusa.org/giving-usa-2025-u-s-charitable-giving-grew-to-592-50-billion-in-2024-lifted-by-stock-market-gains/ |
| `resources/pce_historical.csv`, `pce_targets_2017.csv`, `nipa_2_4_5u.csv` | BEA NIPA Table 2.4.5U, personal consumption expenditures by product. The local file was last revised 9 April 2026. | Benchmark and growth of the eight consumption categories | Verified (file header) |
| `resources/dfa/` | Federal Reserve Distributional Financial Accounts. The local copy ends 2025Q4. | Wealth growth by income group, 2023 to the last DFA year | Verified (file contents) |
| `resources/sp500_total_return_index.csv` | S&P 500 total return index, 1926–2025. Source not stated. | Capital-gains basis model | Source not identified |
| `resources/PerLifeTables_*_TR2024.csv`, `ssa_life_table_2022.csv` | SSA Office of the Chief Actuary period life tables, 2024 Trustees Report | Mortality ledger | File names |
| `resources/return_counts_2023.csv`, `nonfiler_weight_targets.csv` | SOI Publication 1304 Table 1.6 counts, and the non-filer residual series built in the state-weights work | Weight growth through 2023 | Not in scope here |
| `Macro-Projections` interface (vintage 2026022522) | Built from: CBO economic projections (February 2026); historical economic and budget data; budget and revenue projections; long-term outlook (February 2026); demographic projections (January 2026); SSA average wage index; CPI and chained CPI from FRED. Runs 1970–2099. | Growth of every dollar variable after 2036 (GDP, wages, proprietors, rent, interest, corporate profits, consumption, Social Security outlays). Married and unmarried population by single year of age for weights after 2023. | Verified (`dependencies.csv`; code in `../Macro-Projections/src/macro-projections.R`) |

How the projection code combines these:

- 2018–2019 dollar growth comes from SOI averages per return.
- 2020–2036 comes from the CBO 1040 series and the BEA, NIPA and Giving
  USA series above. Thirty-two variables, mostly deductions, credits and
  small income items, grow with CBO total income.
- From 2037 the growth comes from Macro-Projections.
- In the checked-out Macro-Projections code, income components after
  CBO's 10-year window are held at their average share of GDP over the
  last three years of that window. GDP then grows at the long-term
  outlook's rates.
- The long-term outlook ends in 2056. The checked-out code (which still
  names 2023 vintages) stops 30 years after its first projection year. I
  did not find how the 2026 vintage was carried from 2057 to 2099.

### Shared raw-data holdings relevant to this topic but unused by Tax-Data

These sit under `/nfs/roberts/project/pi_nrs36/shared/raw_data/` and
were downloaded for other Budget Lab work. They save a download step.

- `CBO-Demographic-Projections/` (January 2026): Social Security area
  population by age, four marital states and sex, 2022–2099; mortality
  rates; migration flows. `NOTES.md` there says cbo.gov did not block
  the cluster on 2026-08-30. It returned 403 to a plain request on
  2026-10-06.
- `CBO-LTBO/v3/20260225/`: long-term outlook economic and budget
  workbooks.
- `BEA-NIPA-T312/`: government social benefits, 2014–2025.
- `BLS-QCEW/`: state wage totals by year.
- `Census-PEP/`: Vintage 2024 population estimates.
- `SSA-EEDATA-SC/` (2017–2023): earnings by state and county.
- `SSA-OASDI-SC/` (2017–2025): beneficiaries by state and county.
- `SSA-Supplement/`: Annual Statistical Supplement.
- `Treasury-Fiscal/`: Daily and Monthly Treasury Statement extracts
  through July 2026.
- `Census-NEWS/`: Census National Experimental Wellbeing Statistics,
  2016–2021. This is survey income corrected with linked administrative
  records.
- `IRS-SOI-W2/`: SOI W-2 tables for 2019–2020. These are IRS, but noted
  because they bear on the SSA wage statistics comparison.

## CBO

### Revenue Projections, by Category (Budget and Economic Outlook: 2026 to 2036, 11 February 2026)

- **Where:**
  - Report: https://www.cbo.gov/publication/61882 (publication number
    verified in the local economic workbook).
  - Workbook:
    https://www.cbo.gov/system/files/2026-02/51138-2026-02-Revenue.xlsx
  - All vintages: https://www.cbo.gov/recurring-data/51138
  - CSV versions of CBO's data products: https://github.com/US-CBO/cbo-data
- **Sheets** (verified locally): revenue projections, baseline changes,
  individual income tax details, payroll taxes, excise taxes, capital
  gains realizations, legislation, corporate profits.
- **Sheet 3, "Detailed Individual Income Tax Projections"**, calendar
  years 2022–2036:
  - **Income:** salaries and wages; taxable interest plus ordinary
    dividends (excluding qualified dividends); qualified dividends;
    capital gain or loss; net business income from Schedules C, E and F
    (before the passive-loss and excess-business-loss limits and the
    qualified business income deduction); taxable pensions, annuities and
    IRA distributions; taxable Social Security; all other income; total
    income.
  - **AGI and deductions:** statutory adjustments; AGI; personal
    exemption; standard deduction (non-itemizers only); itemized
    deductions after limits; qualified business income deduction;
    "additional deductions", which the footnote says are the 2025 law's
    deductions for tips, overtime, car-loan interest and taxpayers 65 and
    over; taxable income.
  - **Rate structure:** taxable income and tax in each of seven ordinary
    brackets; income taxed at capital-gains rates; AMT; credits; net
    investment income tax; total liability.
  - **Counts:** number of returns (168.0 million in 2025); number
    itemizing (20.9 million in 2025); number affected by the AMT.
  - **Credits:** earned income credit and child tax credit, including
    the refundable parts.
  - **Distribution:** shares of AGI held by the top 1, 5, 10, 25 and 50
    percent (top 1 percent: 23.3 percent in 2025). Groups are defined on
    the distribution of tax units by AGI.
- **Footnotes** (verified locally):
  - The model "begins with a sample of tax returns, projects them into
    future years on the basis of expected economic and demographic
    changes, and then applies tax rules."
  - The sample is "over 300,000 tax returns filed in 2022".
  - The return count includes dependents and "estimates of future
    additional filers". It does not account for changes in filing
    thresholds.
- **Sheet 6, capital gains realizations and tax receipts, 1996–2036.**
  - Realizations through 2023 are Treasury estimates; 2024 onward are
    CBO's ($1,286 billion in 2024, $1,635 billion in 2025).
  - Realizations here are net gains on returns with a net gain. That
    differs from the "capital gain or loss" line on sheet 3, which nets
    losses.
- **Unit and timing:** tax returns, national totals. Updated with each
  baseline: February each year, and in some years a summer update.
- **Fit to tax-return variables:** the closest of any non-IRS source.
  The concepts are SOI Publication 1304 concepts.
- **Limits:**
  - Only 2022 is labelled actual. 2023 onward is model output, even
    though SOI has since published 2023.
  - There is nothing by age, filing status or state.
  - It is a projection resting on CBO's own aging of a 2022 file, so it
    shares assumptions with the prior.
- **Projection or observation:** projection (2022 actual).
- **No update since February 2026.** CBO's data page as archived on 21
  September 2026 lists no later baseline (verified by archive). The next
  baseline is expected in early 2027 (not verified).

### Economic Projections and historical economic data

- **Workbook** (verified locally):
  https://www.cbo.gov/system/files/2026-02/51135-2026-02-Economic-Projections.xlsx.
  Quarterly, calendar-year and fiscal-year sheets, 2023–2036.
  - Actual values reflect data released as of 3 December 2025, so they
    predate the September 2026 BEA revision.
- **Income block (national-accounts concepts):** personal income;
  compensation; wages and salaries; nonwage income; farm and nonfarm
  proprietors' income; rental income; personal interest income; personal
  dividend income; corporate profits.
- **Other series:** CPI-U, chained CPI-U, PCE prices, employment, labor
  force, the civilian noninstitutional population, households (occupied
  housing units), interest rates, and the FHFA house price index.
- **Historical data in CSV:**
  https://www.cbo.gov/system/files/2026-02/55022-2026-02-Historical-Economic-Data.zip
  (verified link).
- **Fit:** these are national-accounts concepts. See the BEA section for
  how they differ from AGI. The repo uses them through Macro-Projections.
- **Short-term update:** "CBO's Current View of the Economy From 2026 to
  2028" (January 2026, publication 61831, data file 61831-Data.xlsx)
  (verified by archive).

### Long-Term Budget Outlook (data only, 2026 to 2056)

- **Where:** https://www.cbo.gov/publication/62044 (verified). Released
  11 February 2026 and corrected 25 February. This year CBO published
  data without a narrative report; the last full report was March 2025.
- **Main workbook:** https://www.cbo.gov/system/files/2026-02/62044-2026-LTBO.xlsx
  (verified), 23 sheets.
  - The sheet most useful here gives the shares of AGI taxed at
    statutory rates of 0, 10–15, 20–35 and 37 percent, for 2026 and 2056:
    25/36/31/8 percent in 2026 and 18/32/39/11 percent in 2056.
  - That is a bracket-creep target 30 years out.
- **Economic workbook:**
  https://www.cbo.gov/system/files/2026-02/57054-2026-02-LTBO-econ.xlsx
  (verified, also local). Annual 1996–2056:
  - GDP and potential GDP, price indexes, labor force (two population
    bases), unemployment;
  - earnings as a share of compensation, growth of real earnings per
    worker, and the share of earnings below the Social Security taxable
    maximum;
  - interest rates and population.
- **Projection.** It ends in 2056.

### CBO's 2026 Long-Term Projections for Social Security (17 September 2026)

- **Where:** https://www.cbo.gov/publication/62556 (verified by
  archive). Files:
  - https://www.cbo.gov/system/files/2026-09/62556-2026-Long-Term-Social-Security-Projections.xlsx
  - https://www.cbo.gov/system/files/2026-09/62556-2026-Additional-Info.xlsx
- **Contents, annually 2026–2100:**
  - covered workers by sex;
  - covered earnings and taxable payroll in dollars, and the share of
    covered earnings below the taxable maximum. Values: 2026 $13.58
    trillion covered, $11.28 trillion taxable, 83.1 percent; 2097 $166.8
    trillion covered, $134.6 trillion taxable, 80.7 percent;
  - Old-Age and Survivors Insurance and Disability Insurance
    beneficiaries;
  - interest rates;
  - GDP and taxable payroll from 1986.
- **Fit:** covered earnings are close to wages plus self-employment
  earnings. The share below the taxable maximum is a summary of how much
  earnings sit at the top, all the way to 2100. It is the only published
  CBO series that constrains the shape of the wage distribution after
  2036.
- **Projection.** The repo does not use it.

### Demographic Outlook: 2026 to 2056 (7 January 2026)

- **Where:**
  - Report: https://www.cbo.gov/publication/61879
  - Data:
    https://www.cbo.gov/system/files/2026-01/57059-2026-01-Demographic-Projections.zip
    (verified; local copy in `shared/raw_data/CBO-Demographic-Projections/`).
- **Contents (CSV):**
  - Social Security area population by single year of age (0 to 100+),
    marital status (single, married, divorced, widowed) and sex,
    2022–2099;
  - civilian noninstitutionalized population on two bases;
  - fertility, mortality and gross migration by age and sex, to 2099;
  - adjustment factors that reweight the monthly CPS to CBO's population.
- **Unit:** persons. Despite the title, the series run to 2099.
- **Use today:** Macro-Projections takes married and unmarried counts by
  age from it. The four-way marital split, which matters for
  head-of-household and widowed filers, is not used.

### The Distribution of Household Income, 2022 (22 January 2026)

- **Where:** https://www.cbo.gov/publication/61911 (verified by archive).
  Files under https://www.cbo.gov/system/files/2026-01/:
  - `61911-supplemental-data.xlsx` (12 tables);
  - `61911-additional-data-for-researchers.zip` (36 CSV files);
  - a table builder;
  - the data underlying the figures.
- **Years:** 1979–2022, released about three years after the data year.
  The previous edition (2021 data) was September 2024.
- **Unit:** households (people sharing a housing unit), ranked by
  size-adjusted income, with equal numbers of people per quintile.
- **Groups:** quintiles, the 81st–90th, 91st–95th and 96th–99th
  percentiles, and the top 1 percent. These are shown for all
  households, households with children, elderly households and
  nonelderly childless households, under three ranking measures.
- **Components** (average dollars per household):
  - wages, employee deferrals, employer health insurance and payroll
    taxes;
  - business income, capital gains, taxable and tax-exempt interest,
    rental income, dividends;
  - corporate tax borne by labor and by capital;
  - Social Security, Medicare, unemployment insurance and other
    transfers;
  - the four federal taxes;
  - household counts.
- **Fit:**
  - Households, not tax units. Income is broader than AGI.
  - Averages times counts give aggregates by group.
  - Useful as shape targets for 2015–2022: for example the top-1-percent
    share of capital gains, business income or dividends.
  - Built from SOI data combined with the CPS (from a description of the
    method; not verified in the report itself). It is therefore not
    independent of IRS data.
- **Projection versions:** the last "Projected Changes in the
  Distribution of Household Income" covered 2016–2021 (December 2019;
  seen in search only).
- **Observation.** The repo does not use it.

### Tax Parameters and Effective Marginal Tax Rates (February 2026)

- **Where:**
  https://www.cbo.gov/system/files/2026-02/53724-2026-02-Tax-Parameters.xlsx
  (verified by archive).
- **Contents:** projected rates, bracket thresholds, standard deductions,
  the SALT cap, AMT and credit parameters, and effective marginal tax
  rates.
- Not a target. It is the parameter path consistent with sheet 3, which
  Tax-Simulator needs, not Tax-Data.

### CBO documentation and other products

- **Documents seen in search only:** "An Overview of CBO's
  Microsimulation Tax Model" (June 2018,
  https://www.cbo.gov/publication/54096) and "Projection and Alignment
  Methods for Static Microsimulation Models"
  (https://www.cbo.gov/publication/52147). Neither was opened. Neither
  publishes the targets CBO ages its file to.
- **"Revenue Collections Through the Individual Income Tax Filing
  Season"** (2026, https://www.cbo.gov/publication/62442): CBO's reading
  of tax year 2025 payments. Useful for a 2025 nowcast. Title only.
- **Monthly Budget Review:** monthly, a summary of receipts.

## JCT

### Estimates of Federal Tax Expenditures for Fiscal Years 2025–2029 (JCX-45-25, 3 December 2025)

- **Where:**
  - PDF:
    https://www.jct.gov/getattachment/8c830c45-1680-4f7e-a649-2a0106f6b6e3/x-45-25.pdf
    (verified)
  - Page: https://www.jct.gov/publications/2025/jcx-45-25/
- **Basis:** law as of 31 August 2025, so after the 2025 reconciliation
  act. Built on CBO's January 2025 baseline.
- **Table 2:** for calendar year 2025, by 12 income classes from below
  $15,000 to $1,000,000 and over:
  - all returns, including people who do not file;
  - itemizing returns;
  - returns with positive taxable income;
  - returns with positive liability;
  - liability.
- **Totals as read** (from a garbled text extract; check against the
  PDF): about 190.9 million returns, 21.1 million itemizers and $2.204
  trillion liability.
- **Table 3:** counts and amounts by the same income classes for untaxed
  Social Security, the medical deduction, the SALT deduction, the
  charitable deduction, the mortgage interest deduction, student loan
  interest, the dependent care credit, the earned income credit, the
  child tax credit and education credits.
- **Unit:** tax returns, including nonfilers as potential returns.
  Dependents and negative-income units are excluded.
- **Income classes** use JCT "expanded income": AGI plus tax-exempt
  interest, employer health and life insurance, the employer share of
  payroll tax, workers' compensation, untaxed Social Security, the
  insurance value of Medicare, AMT preferences, excluded foreign earned
  income, and the individual's share of business taxes.
- **Fit:**
  - Table 3 is one of the few published bracketed tables of itemized
    deductions after the 2017 and 2025 laws. That makes it a natural
    "simulate the reporting process" target: compute who itemizes under
    2025 law and compare counts and amounts by class.
  - To match the classes, Tax-Data would need to compute expanded
    income.
- **Projection** (the 2025 value is projected from older SOI data).
  Annual, usually December, PDF only. The 2026 edition is not out.

### Distribution tables for the 2025 reconciliation act

- **JCX-37-25** (29 July 2025): verified,
  https://www.jct.gov/getattachment/d8615f75-b9b7-4bf7-b25d-48b645fddf94/x-37-25.pdf.
  - Calendar years 2027, 2029, 2031 and 2033, by the same 12
    expanded-income classes.
  - Federal taxes under present law and under the act.
- **JCX-36-25** (current-policy baseline): title only.
- **Use:** the present-law columns give tax by income class: a soft
  distribution check for future years.

### JCT model documentation

- **JCX-48-23** (30 October 2023),
  https://www.jct.gov/getattachment/3f7eb8cb-f024-4b37-814d-3050e47c5e43/x-48-23.pdf
  (verified).
  - **Base and nonfilers:** about 360,000 tax year 2019 returns from SOI,
    with linked information returns. Nonfilers come from the Continuous
    Work History Sample, sized as resident population minus people on
    returns.
  - **Projection:** aged to CBO's forecast with more than 30 growth
    factors, wage growth by wage class, and nonlinear reweighting to more
    than 100 targets. These include income-source shares by income
    class, estimated from past SOI files. Target values are not
    published.
- **January 2025 slides,** "The Joint Committee on Taxation Revenue
  Estimating Process"
  (https://www.jct.gov/getattachment/29cf59ce-31ca-450b-adea-3848d0d33350/Revenue-Estimating-Process-January-2025.pdf,
  verified). The 2025 model uses more than 360,000 returns filed in 2020,
  reweighted so that "many economic targets are achieved simultaneously
  for each year through 2035".

Both JCT and Treasury (below) document a design close to the brief's:
an aged prior, reweighted to many simultaneous targets.

### Overview of the Federal Tax System as in Effect for 2025 (JCX-38-25)

- Verified:
  https://www.jct.gov/getattachment/5fd0e21a-ea5d-441b-943b-0784cb87989d/x-38-25.pdf.
- Receipts by source, 1975–2024; business returns by type, 1978–2022.
- No individual-return counts by income. Of little use here.

## Treasury Office of Tax Analysis

- **Distribution tables, 2025 current law** (released November 2024;
  verified). No 2026 table is posted.
  - Table 001 (families, cash income, federal taxes):
    https://home.treasury.gov/system/files/131/Distribution-of-Tax-Burden-Current-Law-2025.xlsx
  - Table 002 (income by source):
    https://home.treasury.gov/system/files/131/Distribution-of-Income-by-Source-2025.xlsx
  - **Unit:** tax families including nonfilers: 189.3 million families
    and 344.7 million people.
  - **Groups:** deciles of size-adjusted "cash income", plus the
    90–95th, 95–99th and 99–99.9th percentiles and the top 0.1 percent.
  - Cash income is broader than AGI.
  - Projection for the first budget year.
- **"2024 Family Counts by Filing Status, Presence of Children, and Race
  and Hispanic Ethnicity"** (January 2025):
  https://home.treasury.gov/system/files/131/2024-Family-Counts-by-Filing-Status-Children-Race-Hispanic-Ethnicity-01142025.xlsx
  (verified).
  - 184.55 million tax units in 2024, nonfilers included under the
    status they would use, by income decile.
  - A direct check on the total tax-unit count and the filing-status
    and child mix of filers plus non-filers.
- **"Income Tax Withholding Among 2022 Non-filers"** (January 2025):
  https://home.treasury.gov/system/files/131/Nonfiler-Withholding-01162025.pdf
  (verified). 50.3 million people on tax year 2022 information returns
  who neither filed nor appeared as a dependent. Relevant to the
  non-filer pool.
- **Technical Paper 12, "U.S. Treasury Individual Income Tax Model"**
  (May 2023): https://home.treasury.gov/system/files/131/TP-12.pdf
  (verified).
  - Lists the targets Treasury ages its 2016-based file to: 35 count
    targets (filing status, 11 age classes, AGI over $1 million in four
    classes, and others) and 49 dollar targets (including positive
    capital gains in nine income classes and modified AGI in 15 classes).
  - Values are not published. It is the best published template for
    which targets a government model uses.
- **Tax expenditures, FY2027** (fiscal years 2025–2035; verified):
  https://home.treasury.gov/system/files/131/Tax-Expenditures-FY2027.xlsx.
  Aggregates by provision only, with no income distribution.
- **Monthly Treasury Statement, Table 4** (Fiscal Data API `mts_table_4`,
  https://fiscaldata.treasury.gov/datasets/monthly-treasury-statement/;
  verified).
  - Individual income tax split into withheld and other (non-withheld),
    and payroll taxes by trust fund.
  - Monthly from March 2015; latest August 2026, published about eight
    business days after month end.
  - Non-withheld receipts in April were $472.0 billion in 2026 vs $453.2
    billion in 2025. These mix final payments for the prior year with
    first-quarter estimated payments for the current year. They are an
    early, noisy signal for capital gains and other non-wage income.
- **Daily Treasury Statement** (API
  `/v1/accounting/dts/deposits_withdrawals_operating_cash`; verified).
  - The line "Taxes – Withheld Individual/FICA" combines income tax and
    payroll tax withholding. Daily, one business day lag.
  - The older table that split federal tax deposits by type ended on 13
    February 2023.
  - Withholding growth reflects both wage growth and changes in the
    withholding tables. The 2025 law breaks year-on-year comparisons for
    2025–2026.

## BEA

### Personal income by type, national

- **Tables:**
  - Table 2.1 (personal income and its disposition).
  - Detail tables:
    - 2.2B: wages by industry;
    - 3.12: about 43 lines of government social benefits;
    - 7.8: supplements to wages;
    - 7.9: rental income;
    - 7.10: dividends;
    - 7.11: interest;
    - 7.14: proprietors' income vs IRS;
    - 7.18: NIPA wages vs BLS wages;
    - 7.20–7.25: pension plans.
  - All verified in BEA's table register,
    https://apps.bea.gov/national/Release/TXT/TablesRegister.txt.
  - Data files: `NipaDataA.txt` (with quarterly and monthly versions)
    under https://apps.bea.gov/national/Release/TXT/.
- **Timing:** monthly, about four weeks after the month; annual from
  1929. The latest release, 30 September 2026, covered August 2026. The
  next is 29 October 2026.
- **2026 annual update** (30 September 2026; verified,
  https://apps.bea.gov/scb/issues/2026/09-september/0926-nea-annual-update.htm).
  - Revisions from 2021, using SOI tax year 2023 business tabulations.
  - Revisions in billions of dollars for 2023 / 2024 / 2025:
    - personal interest income: +250 / +315 / +421;
    - personal dividend income: −116 / −84 / −97;
    - proprietors' income: −99 / −107 / −92;
    - wages: +3 / +23 / +18.
- **Fit to tax-return variables:**
  - **Wages** are close to W-2 wages, but include the misreporting
    adjustment and employee elective deferrals. Employer health and
    pension contributions are in supplements, not wages.
  - **Personal interest** is roughly half imputed (bank services,
    life-insurance and pension-fund earnings). Only the monetary-interest
    line in Table 7.11 ($1,266 billion received by persons in 2025) is
    comparable to taxable plus tax-exempt interest, and it still includes
    interest earned inside IRAs and other tax-deferred accounts.
  - **Personal dividends** include imputed dividends: monetary dividends
    were $1,931 billion of $2,151 billion in 2025.
  - **Rental income** is dominated by owner-occupied imputed rent ($898.5
    billion of the total in 2025).
  - **Proprietors' income** is adjusted for depreciation and inventory
    valuation and includes the misreporting adjustment.
  - **Transfers** include in-kind Medicare and Medicaid.
  - **Pension and IRA distributions are not personal income. Capital
    gains are excluded.**
- **Pension benefit payments** (Tables 7.20–7.25) are published
  separately. All employer plans paid $1,925.8 billion in 2025; defined
  contribution plans alone $1,037.5 billion. These cover private,
  federal, and state and local plans, not IRAs or Social Security. A
  rough benchmark for taxable pensions; whether rollovers are included
  was not confirmed.
- **Monetary interest paid on owner-occupied housing**: FRED
  W498RC1A027NBEA, $518.5 billion in 2025 (verified). This is what the
  repo's mortgage series already uses. It is the aggregate of all
  owner-occupant mortgage interest before the $750,000 limit and before
  the choice to itemize.
- **Observation,** with recent quarters partly extrapolated.

### Reconciliation of personal income with AGI

- **NIPA Table 7.19 is discontinued** and not in the current register
  (verified).
- **The last BEA article** is Ledbetter, *Survey of Current Business*,
  November 2007, covering 2004–2005:
  https://apps.bea.gov/scb/pdf/2007/11%20November/1107_pi_agi.pdf
  (verified).
  - Personal income includes, but AGI excludes:
    - nontaxable transfers;
    - employer contributions to pension and insurance funds;
    - imputed income;
    - investment income of life insurers and pension plans;
    - nonprofit and fiduciary income;
    - depreciation and inventory adjustments.
  - AGI includes, but personal income excludes:
    - employee payroll tax contributions;
    - capital gains;
    - taxable pensions and IRA distributions;
    - other items.
  - In 2005 the gap between BEA-derived AGI and IRS AGI was $1,286
    billion, 14.8 percent. The misreporting adjustment explained $508
    billion of it.
- **Treasury Working Paper 109** (Foertsch, 2016):
  https://home.treasury.gov/system/files/131/wp-109.pdf (verified by one
  helper; another got a 404 from a different link).
  - Rebuilds the reconciliation for 1993–2012.
  - Finds IRS AGI equal to 71.1 percent of adjusted personal income in
    2012.
- **Misreporting adjustments** still published:
  - Table 7.14: nonfarm proprietors and partnerships, $611.8 billion in
    2023, through 2023.
  - Table 7.18: wages, $172.6 billion in 2025.
  - Both are amounts BEA adds for income that tax returns do not show.
    They measure part of how far NIPA levels sit above what returns
    report.

### Distribution of Personal Income

- **Where:** https://www.bea.gov/data/special-topics/distribution-of-personal-income
  (verified).
  - National summary file:
    https://apps.bea.gov/data/special-topics/distribution-of-personal-income/national/summary-file.csv
  - Technical document:
    https://apps.bea.gov/data/special-topics/distribution-of-personal-income/national/technical-document.pdf
- **Status:** now an official NIPA table (Table 2.10). The latest
  release was 16 June 2026:
  - 2000–2023 revised;
  - 2024 provisional, with top shares given as ranges;
  - an experimental nowcast of 2025 quintile shares.
- **Content:** household personal income and disposable income by
  decile, with components (compensation, proprietors, rent, interest,
  dividends, social benefits, taxes), and summary measures (Gini, top 1
  and 5 percent shares, median).
- **Unit:** households ranked by equivalized income (income divided by
  the square root of household size).
- **Construction:**
  - The CPS ASEC is scaled to NIPA totals, with the top adjusted to SOI
    shares in three AGI bands.
  - Taxes come from PolicyEngine from 2021 onward.
  - BEA warns 2023–2024 are volatile because SOI data were missing.
- **State prototype:** 2012–2024, distributional metrics only.
  https://apps.bea.gov/data/special-topics/distribution-of-personal-income/state/Distributional_metrics.xlsx
- **Joint distribution of disposable income and PCE, 2000–2023:**
  https://www.bea.gov/sites/default/files/2026-05/joint_dist_summary.xlsx
  (verified). Decile shares of income, consumption and saving, and the
  income-by-consumption matrix. A possible check on the consumption
  imputation.
- **Fit:**
  - Household, not tax unit. Capital gains are excluded.
  - The top is calibrated to SOI, so it is partly circular for the
    top. It is more informative on the bottom and middle and on
    transfers.
- **Estimate and nowcast.**

### State and county personal income, state PCE

- **State personal income** (SAINC tables): quarterly. The 2026Q2 data
  came out 30 September 2026, and 2025 annual data were added; the next
  release is 23 December 2026.
  https://www.bea.gov/data/income-saving/personal-income-by-state
- **County personal income** (CAINC): 2024 is the latest. 2025 is due 2
  December 2026.
- **State PCE:** 2025 released 30 September 2026.
- Same concepts as the national accounts. Measured by place of
  residence. Relevant to the state-weights work.

## Federal Reserve

### Financial Accounts of the United States (Z.1)

- **Where:** https://www.federalreserve.gov/releases/z1/ (verified).
  - Latest release 11 September 2026, for 2026Q2; next 10 December 2026.
  - CSV zip: https://www.federalreserve.gov/releases/z1/data/FRB_Z1_csv.zip
- **Table codes changed on 11 June 2026.**
  - B.101 (households and nonprofits) is now S1M.b; B.101.h (households
    only) is now S14.b; R.101 (change in net worth) is now S1M.r.
  - Mapping file:
    https://www.federalreserve.gov/releases/z1/current/z1_table_mapping.csv
  - Any code that pulls series by old table name will need updating.
- **Content:** stocks of deposits, bonds, equities and mutual funds,
  pension entitlements, real estate, mortgages and consumer credit.
  Revaluation tables give accrued holding gains.
- **Fit:** stocks, not income flows. They serve as wealth targets and as
  the bases for interest, dividends and gains. Accrued gains are not
  realized gains. IRA assets are not a separate household line.

### Distributional Financial Accounts (DFA)

- **Where:** https://www.federalreserve.gov/releases/z1/dataviz/dfa/ ;
  zip at https://www.federalreserve.gov/releases/z1/dataviz/download/zips/dfa.zip
  (verified).
- **Coverage:** quarterly, 1989Q3–2026Q2, updated about a week after
  Z.1. The local copy ends 2025Q4.
- **Content:** about 28 asset and liability lines by:
  - wealth group (top 0.1 percent, next 0.9, 90–99, 50–90, bottom 50);
  - income group (0–20, 20–40, 40–60, 60–80, 80–99, 99–100);
  - age, generation, education and race.

  Detail files include household counts and group cutoffs.
- **Unit:** households. The SCF supplies the shares between surveys; Z.1
  supplies the totals.
- **Unresolved question.** The public code builds percentile rankings on
  both realized and "normal" income. The published income cutoffs appear
  to use realized income. Project memory says the DFA uses "usual
  income". Check this before relying on either.
- **Estimate.** The shares after the 2022 SCF are model-based.

### New York Fed Quarterly Report on Household Debt and Credit

- **Where:** https://www.newyorkfed.org/microeconomics/hhdc (verified).
  Workbook for 2026Q2:
  https://www.newyorkfed.org/medialibrary/interactives/householdcredit/data/xls/HHD_C_Report_2026Q2.xlsx
- **Content:** balances from a credit-bureau sample. For 2026Q2
  (released 11 August 2026): mortgages $13.1 trillion, home equity lines
  $459 billion, auto loans $1.71 trillion. Originations by credit score
  and age, and debt by age.
- **Timing:** quarterly, about six weeks after the quarter.
- **Fit:** balances, not interest paid. Combine with rates for mortgage
  and car-loan interest. No income detail, and no split between new and
  used vehicles.

## Distributional national accounts research

- **Piketty–Saez–Zucman US distributional national accounts.**
  - Latest vintage February 2022, tabulations 1913–2019:
    https://gabriel-zucman.eu/usdina/ (verified).
  - Unit: equal-split adults, meaning each married couple's income is
    split equally between the two spouses. Scaled to national income.
  - Built from the PUF. Stale and not independent of the prior.
- **Realtime Inequality** (Blanchet, Saez, Zucman):
  https://realtimeinequality.org (verified).
  - Monthly 1976-01 to 2026-06, generated 26 September 2026.
  - Factor, pre-tax, disposable and post-tax income and wealth: totals,
    averages, shares and thresholds from the bottom 50 percent to the top
    0.00001 percent.
  - Units: equal-split adults, working-age adults, and tax units.
  - The downloadable database has no income components.
  - Recent months are nowcasts scaled to national accounts.
  - Database:
    https://pub-718b8afebf4a450498e2111e3bbf4901.r2.dev/temp_data/full-online-database.xlsx
- **Auten–Splinter.** Workbook dated February 2026, covering 1960–2022:
  https://davidsplinter.com/AutenSplinter-IncomeIneq.xlsx (verified).
  - Top shares of fiscal, pre-tax and after-tax income, income sources
    by fractile, and adjustments for underreporting.
  - Table C0: tax units, returns and filing shares.
  - Table C30: non-filer counts and incomes for 2000, 2010 and 2018.
  - Unit: tax units. The closest of these to Tax-Data's concepts, and
    C0 and C30 bear on the non-filer work.
- **World Inequality Database, US.** Updated 18 June 2026, through 2024;
  US source data appear to end in 2022 (not verified).
  https://wid.world/country/usa/

## SSA

### Wage Statistics (net compensation from W-2s)

- **Where:**
  - Wage statistics: https://www.ssa.gov/cgi-bin/netcomp.cgi (add
    `?year=YYYY`; data from 1991)
  - Average wage index: https://www.ssa.gov/oact/cola/AWI.html
  - Read through archive copies; verified for 2023.
- **Content:**
  - Workers and aggregate and average amounts in $5,000 brackets to
    $200,000, then wider brackets up to $50 million and over.
  - A reconciliation from W-2 taxable compensation to net compensation.
    For 2023: compensation subject to income tax $10.661 trillion, plus
    elective deferrals $446.0 billion, giving net compensation $11.103
    trillion, for 173.67 million wage earners.
- **Unit:** individual workers, all of a person's W-2s combined.
  National only.
- **Timing:** annual. 2024 is the latest as of 4 October 2026. 2025 is
  expected with the cost-of-living announcement after the 14 October CPI
  release; that is an inference, not verified. The lag is about 10
  months.
- **Fit:**
  - The closest non-IRS match to the W-2 wage distribution. It is per
    person, so it suits the per-spouse wage split.
  - Uncapped.
  - It includes non-filers' wages, and adds deferrals; SSA publishes the
    deferral total, so it can be backed out.
  - It excludes self-employment income and wage-line additions without a
    W-2.
  - It is the only administrative wage distribution available for 2024,
    and probably 2025, before SOI.
- **Observation.** The repo does not use it.

### 2026 Trustees Report and Office of the Chief Actuary files

- **Report:** released 9 June 2026, https://www.ssa.gov/oact/tr/2026/
  (verified by archive). Single-year tables `SingleYearTRTables_TR2026.xlsx`,
  annual to 2100 under three assumption sets:
  - fertility, mortality and immigration;
  - Social Security area population by broad age group;
  - average wage index, taxable payroll and covered workers;
  - beneficiaries;
  - CPI, GDP and interest rates.
- **Selected intermediate values read:**
  - The long-run total fertility rate is 1.75. The previous report used
    1.9, from memory, not verified.
  - Net immigration of 130,000 in 2026.
  - The average wage index is 69,846.57 for 2024 (actual) and 72,025.07
    for 2025 (estimate).
- **Downloadable files** (https://www.ssa.gov/OACT/Downloadables/CY/index.html;
  the old life-table page redirects there):
  - death probabilities and period and cohort life tables;
  - Social Security area population by single year of age, sex and
    marital status, 1941–2100.

  2025-report files are confirmed. 2026-report period life tables appear
  in a search index (`PerLifeTables_*_TR2026`) but were not opened. The
  repo's TR2024 files are one or two vintages old.
- **Fit:** persons, on the Social Security area universe, which is wider
  than the Census resident population. The Trustees and CBO make
  different demographic assumptions, so they are alternative long-run
  priors. The new SSA fertility assumption is notably lower.

### Annual Statistical Supplement, 2025 edition (completed March 2026)

- **Where:** https://www.ssa.gov/policy/docs/statcomps/supplement/
  (verified by archive).
- **Section 4.B:** earnings from a 1 percent sample of SSA earnings
  records:
  - wage workers by amount of taxable earnings, 1992–2023;
  - self-employed workers by age and by amount of taxable earnings;
  - taxable earnings by state, sex and age.
  - Social Security amounts are capped at the taxable maximum; Medicare
    amounts are uncapped.
- **Section 5:** beneficiaries and benefits by type, age, sex and
  monthly benefit amount, December 2024.
- **Fit:**
  - The self-employed tables by amount are a non-IRS distribution of
    self-employment earnings.
  - Benefit tables are December snapshots. Form SSA-1099 reports
    calendar-year payments, so calendar totals should come from the
    trust-fund tables.

### Other SSA series

- **Earnings and Employment Data by State and County** (2023 edition,
  January 2026): https://www.ssa.gov/policy/docs/statcomps/eedata_sc/
- **OASDI Beneficiaries by State and County** (2025 edition, August
  2026): https://www.ssa.gov/policy/docs/statcomps/oasdi_sc/
- **Monthly Statistical Snapshot** (August 2026: 71.5 million
  beneficiaries, $138.8 billion a month):
  https://www.ssa.gov/policy/docs/quickfacts/stat_snapshot/
- **Earnings Public-Use File:** only the 2006 edition exists. Of no use
  after 2015.

## BLS

- **Quarterly Census of Employment and Wages (QCEW).**
  - Where: https://www.bls.gov/cew/ (verified).
  - Coverage: 2025 annual averages are out, with total wages $12.3
    trillion and 155.7 million jobs. 2026Q1 was released 28 August 2026
    and 2026Q2 is due 2 December 2026. The lag is 5–6 months.
  - Detail: county, state and six-digit industry.
  - Wages include bonuses, stock options, severance and tips, and in some
    states employer contributions to deferred compensation plans.
    Employer payments for social insurance and benefits are excluded. It
    covers over 95 percent of jobs and excludes the self-employed.
  - Measured by employer location, not residence.
  - The best timely control total for the wage bill. It is closer to SSA
    net compensation than to Form W-2 box 1.
- **Current Employment Statistics** (https://www.bls.gov/ces/): monthly
  payroll employment, hours and average earnings. September 2026 data
  were released 2 October. The only overtime measure is manufacturing
  overtime hours.
- **Employment Cost Index and Employer Costs for Employee
  Compensation** (https://www.bls.gov/eci/, https://www.bls.gov/ecec/).
  - Quarterly. Employer costs per hour by wages, paid leave, supplemental
    pay (including overtime and bonuses), insurance and retirement. June
    2026, private industry: $32.82 an hour in wages and $14.07 in
    benefits.
  - Gives the ratio of employer health and retirement contributions to
    wages.
- **Occupational Employment and Wage Statistics** (May 2025 estimates,
  released 15 May 2026; https://www.bls.gov/oes/).
  - Employment and wage percentiles for about 830 occupations, by state
    and metro area.
  - Wages include tips but exclude overtime. So it can size tipped
    occupations, but cannot measure overtime pay.
- **Consumer Expenditure Survey published tables**
  (https://www.bls.gov/cex/tables.htm).
  - Means per consumer unit by income quintile and decile, age and other
    groups. 2024 is the latest; 2025 is due 29 October 2026.
  - Consumer units are not tax units. The CEX-to-PUF income gap noted in
    `CLAUDE.md` applies.
- **CPI** (https://www.bls.gov/cpi/): September 2026 is due 14 October.
  Used for indexing, through Macro-Projections.

## Census

- **Population Estimates, Vintage 2025.**
  https://www.census.gov/data/tables/time-series/demo/popest/2020s-national-detail.html
  (verified). Single year of age by sex to 1 July 2025, released June
  2026. Vintage 2026 totals come in December 2026, with age detail in
  June 2027. Resident universe.
- **National population projections.** The 2023 series is the latest
  (to 2060): https://www.census.gov/programs-surveys/popproj.html.
  Shorter than CBO's and SSA's.
- **CPS ASEC published income tables.** "Income in the United States:
  2025" (P60-289, 15 September 2026):
  https://www.census.gov/library/publications/2026/demo/p60-289.html.
  Household income distribution and historical tables. Households,
  money income, weak at the top.
- **American Community Survey.**
  - 2024 one-year tables are available. Relevant tables:
    - B25102 and B25103 (real estate taxes by mortgage status);
    - B25087 (owner costs);
    - B19061–B19070 (aggregate income by type);
    - B12001 (marital status).
  - **The 2025 one-year release is delayed:** a Census advisory of 4
    August 2026 says the date "is being determined". The API has no 2025
    one-year dataset.
- **Quarterly Summary of State and Local Tax Revenue**
  (https://www.census.gov/programs-surveys/qtax.html; verified).
  - 2026Q2 released 10 September 2026, a lag of about 2½ months.
  - State-government collections by state and tax type; property tax
    nationally.
  - For SALT: collections include payments by non-itemizers and by
    businesses, so they are a ceiling and a growth signal rather than a
    level target. Withholding plus estimated payments made during the
    year is close to the cash basis of the deduction.
- **Annual Survey of State and Local Government Finances.** 2024 data
  released July 2026:
  https://www.census.gov/programs-surveys/gov-finances.html. Property,
  income and sales taxes by state for state and local governments
  combined.
- **Nonemployer Statistics.** 2024 data released 29 September 2026:
  https://www.census.gov/programs-surveys/nonemployer-statistics.html
  (verified).
  - Firms and receipts by industry, state and county, and by legal form
    nationally and by state.
  - Built from IRS business returns. Receipts, not net profit.
  - Maps to Schedule C gross receipts.
- **Business Formation Statistics** (https://www.census.gov/econ/bfs/index.html):
  monthly applications for employer identification numbers. A signal of
  business counts, not income.
- **Families and living arrangements** (households by type, marital
  status by sex):
  https://www.census.gov/data/tables/time-series/demo/families/households.html
- **National Experimental Wellbeing Statistics (NEWS).** Survey income
  corrected with linked tax and program records, 2016–2021:
  https://www.census.gov/data/experimental-data-products/national-experimental-wellbeing-statistics.html.
  A local copy is in `shared/raw_data/Census-NEWS/`. Households;
  observation.

## CMS (Centers for Medicare & Medicaid Services)

- **National Health Expenditure Accounts.**
  - Historical through 2024, released 14 January 2026.
  - Projections for 2025–2034, page dated 24 June 2026:
    https://www.cms.gov/data-research/statistics-trends-and-reports/national-health-expenditure-data/projected
    (verified; tables in https://www.cms.gov/files/zip/nhe-projections-tables.zip).
  - Out-of-pocket spending: $556.6 billion in 2024, $883.2 billion in
    2034.
  - Household-paid private insurance premiums: $468 billion in 2024,
    $739 billion in 2034.
  - Employer premium contributions: $753 billion in 2024.
  - Relevant to the medical expense deduction, the self-employed health
    deduction, and employer-paid premiums excluded from wages.
- **Marketplace open-enrollment public use files** for 2024–2026, by
  state and county, with premium tax credit amounts:
  https://www.cms.gov/data-research/statistics-trends-reports/marketplace-products/2026-marketplace-open-enrollment-period-public-use-files.
  The effectuated-enrollment reports (people who actually paid) are the
  closer match to Form 8962 totals; no working URL was found.

## Private and other trackers

### Charitable giving

- **Giving USA** (https://givingusa.org/).
  - For 2025, released 7 July 2026: individuals $394.2 billion, bequests
    $62.2 billion, total $617.2 billion (verified through a summary by
    Candid).
  - Annual, about six months' lag. The full report is sold by
    subscription.
  - **Method:** the individual estimate starts from SOI itemized
    deductions and extends them with economic indicators. Non-itemizer
    giving comes from the Philanthropy Panel Study. As a check on the SOI
    charitable deduction it is partly circular.
  - **Concept differences from the deduction:** Giving USA counts
    non-itemizers and values gifts as given. The deduction is after AGI
    limits and includes carryovers.
- **Fundraising Effectiveness Project** (https://publications.fepreports.org/).
  Quarterly growth in dollars and donors by gift size; Q1 2026 is the
  latest. A growth signal only.
- **Donor-advised funds.** The DAF Research Collaborative's "Annual DAF
  Report 2025" covers fiscal 2024: contributions $89.6 billion, from a
  search snippet:
  https://johnsoncenter.org/resource/report-annual-daf-report-2025/.
  The deduction is taken when money goes into the fund, so contributions
  are the right analogue for high-income itemized giving.
- **Philanthropy Panel Study** (module of the Panel Study of Income
  Dynamics). Biennial household giving with income and itemizer status.
  It is the only public source on non-itemizers' giving, which the 2026
  non-itemizer deduction ($1,000 single, $2,000 joint, cash gifts only)
  needs.

### Wages

- **Atlanta Fed Wage Growth Tracker**
  (https://www.atlantafed.org/chcs/wage-growth-tracker; verified).
  - Median 12-month wage growth for the same individuals in the CPS, by
    wage quartile and other groups, through August 2026.
  - Growth for continuing workers, not the wage bill. CPS earnings are
    top-coded, so it says nothing about the top tail.
- **ADP Pay Insights** (https://payinsights.adp.com/). Median pay growth
  from payroll records, monthly, private sector. Base pay and gross pay
  (with bonuses and overtime) for job stayers and job changers.
  September 2026, stayers: gross pay +4.4 percent.
- **Indeed Wage Tracker, Gusto, Homebase.** Posted wages and small-firm
  data. Leading indicators only.

### Mortgage interest and property tax

- **Freddie Mac weekly mortgage rates** (https://www.freddiemac.com/pmms).
  Rates on new loans, so they matter only for the newly originated part
  of the mortgage stock.
- **Home Mortgage Disclosure Act data** (2025 originations, released June
  2026; https://ffiec.cfpb.gov/data-browser/). Loan amount, applicant
  income and location for new mortgages.
- **ATTOM property tax report.**
  - Tax year 2025: $396.8 billion levied on 89.6 million single-family
    homes, by state and county.
  - Free press release:
    https://www.attomdata.com/news/market-trends/home-sales-prices/2025-annual-tax-report
  - Includes investor-owned homes and excludes other property types.
- **Lincoln Institute 50-State Property Tax Comparison** (rates for
  typical properties in about 100 cities):
  https://www.lincolninst.edu/publications/other/50-state-property-tax-comparison-study-2025/

### Business income

- **Census Nonemployer Statistics** (above). BEA proprietors' income
  (above) carries a large misreporting adjustment relative to Schedule C
  and partnership income.
- **Census Quarterly Services Survey.** Quarterly revenue by service
  industry.

### Retirement distributions

- **Investment Company Institute quarterly retirement data**
  (https://www.ici.org/research/stats/retirement). 2026Q2: IRAs about
  $20 trillion, 401(k) plans $10.8 trillion. Asset stocks only, with no
  distributions.
- **Vanguard "How America Saves 2026."** Hardship-withdrawal rates for
  Vanguard plans.
- No non-IRS source gives total IRA or 401(k) distributions. The BEA
  pension tables cover employer plans only.

### Tips, overtime and car-loan interest (2025 law deductions)

- **Tipped occupations.** Treasury's list was proposed 22 September 2025
  (https://www.govinfo.gov/content/pkg/FR-2025-09-22/html/2025-18278.htm).
  Final regulations were reportedly issued April 2026 (search snippet
  only).
- **W-2 reporting.** The draft 2026 Form W-2 adds Box 12 codes for
  qualified tips (TP) and qualified overtime (TT):
  https://www.journalofaccountancy.com/news/2025/aug/draft-2026-form-w-2-includes-boxes-and-codes-for-tips-and-overtime/.
  2025 had transition relief. **The first administrative totals will
  come from tax year 2026 W-2s in 2027.**
- **Cost estimates.** JCT estimates of the deductions' cost were seen
  only as cited by others. The 2026 filing-season claim counts were not
  found.
- **Car loans.** Car-loan balances from the New York Fed. New-car loan
  amounts and rates from Experian, Cox and Edmunds (partly paywalled).
  No source gives the share of loans on US-assembled vehicles.

### Health

- **KFF Employer Health Benefits Survey 2025** (https://www.kff.org/ehbs/):
  family premium $26,993, of which workers pay $6,850. The 2026 edition
  is likely in October 2026.
- **Devenir HSA research** (https://www.devenir.com/category/research/):
  $174 billion in 41.7 million accounts at end-2025.

## How the sources map to tax-return variables

| Tax-Data variable | Best non-IRS level or shape source | Best timely growth signal (2024–2026) | Long-run (to 2097) | Main conceptual gap |
|---|---|---|---|---|
| Wages (`wages`, `wages1`, `wages2`) | SSA wage statistics (per-person brackets); CBO sheet 3 total | QCEW quarterly wages; BEA monthly wages; Treasury withholding; Atlanta Fed by quartile | CBO Social Security projections: covered earnings and share below taxable max to 2100; SSA wage index | Elective deferrals (in SSA, QCEW and BEA, not in box 1); non-filers' wages |
| Interest (`txbl_int`, `exempt_int`) | BEA Table 7.11 monetary interest received | BEA monthly; Z.1 interest-bearing assets | Macro-Projections (GDP share) | Interest inside IRAs and pensions; imputed interest |
| Dividends (`div_ord`, `div_pref`) | BEA Table 7.10 monetary dividends; CBO sheet 3 (qualified separately) | BEA; Z.1 equity holdings | GDP share | Imputed dividends; dividends inside retirement accounts |
| Capital gains (`kg_*`) | CBO sheet 6 realizations; CBO household income (by group, through 2022) | Treasury non-withheld receipts (April); S&P 500; Z.1 holding gains | CBO sheet 6 to 2036 only | Accrued vs realized; netting of losses |
| Business income (`sole_prop`, partnership, S corporation) | CBO sheet 3 net business income; Census Nonemployer receipts | BEA proprietors' income; Nonemployer 2024 | GDP share | Misreporting adjustment; receipts vs profit |
| Pensions and IRA distributions | CBO sheet 3 taxable pensions | BEA Tables 7.20–7.25 benefit payments (employer plans only) | CBO Social Security outlays, used as a proxy today | No non-IRS IRA distribution total |
| Social Security (`gross_ss`) | SSA trust-fund payments; Supplement benefit distributions; CBO sheet 3 taxable portion | SSA monthly snapshot | Trustees and CBO beneficiaries and costs to 2100 | December snapshot vs calendar-year payments |
| Unemployment compensation | BEA Table 3.12 | BEA monthly; Department of Labor claims | — | Calendar vs fiscal year |
| Mortgage interest | BEA owner-occupied monetary interest; JCT Table 3 (deduction, by class, 2025) | New York Fed balances; Freddie Mac rates | — | Itemization and $750,000 cap |
| State and local taxes | Census state and local finances; JCT Table 3 | Census quarterly tax collections; BEA Table 3.3 | — | Business share; non-itemizers; the cap |
| Charity | JCT Table 3; CBO household income does not cover it | Giving USA (partly SOI-based); Fundraising Effectiveness Project | — | Gifts vs deduction after limits; non-itemizers |
| Medical expenses | CMS out-of-pocket and household premiums; JCT Table 3 | CMS projections to 2034 | CMS to 2034 | 7.5 percent of AGI floor |
| Tips, overtime | BLS occupational counts; CBO "additional deductions" line | ADP gross vs base pay | — | No administrative data until 2027 |
| Consumption (`c_*`) | BEA PCE by product; BEA income–PCE joint distribution | BEA monthly PCE | GDP consumption share | Spending on behalf of households |
| Wealth (`value.*`) | Z.1 and DFA | DFA quarterly to 2026Q2 | — | Household vs tax unit |
| Population, marital status, age | CBO demographic projections; SSA Trustees files; Census estimates | Census Vintage 2025 | CBO to 2099; SSA to 2100 | Social Security area vs resident universe |
| Total tax units and returns | Treasury family counts (184.55 million, 2024); JCT (190.9 million incl. non-filers, 2025); CBO returns (168.0 million, 2025) | — | CBO returns to 2036 | Definitions of a non-filing unit differ |
| Top-end distribution | CBO AGI shares (2022–2036); CBO long-term AGI shares by rate (2056); Auten–Splinter; Realtime Inequality | Realtime Inequality monthly | CBO taxable-maximum share to 2100 | All partly SOI-based |

## Sources the repository does not use but should consider

In rough order of value for the targets framing in the brief:

1. **CBO revenue workbook, sheets 3 and 6, used in full:**
   - return and itemizer counts;
   - AGI shares of the top 1, 5, 10, 25 and 50 percent;
   - itemized and standard deduction totals;
   - the "additional deductions" line;
   - taxable income by bracket;
   - capital-gains realizations.

   These are soft targets for 2024–2036. They should carry modest
   precision, because they are CBO model output.
2. **SSA wage statistics:** a per-person wage distribution, observed
   through 2024 and probably 2025, ahead of SOI.
3. **JCT tax-expenditure Table 3 (JCX-45-25):** itemized deduction
   counts and amounts by income class for 2025. Matched by simulating
   who itemizes.
4. **CBO 2026 long-term Social Security projections:** covered earnings,
   taxable payroll and the share below the taxable maximum to 2100. The
   only long-run constraint on the top of the wage distribution.
5. **QCEW annual and quarterly wages,** for the 2025–2026 wage bill.
6. **Treasury 2024 family counts and Auten–Splinter Table C0:** total
   tax units and filing shares, for the population partition.
7. **CBO Distribution of Household Income, 2015–2022:** income
   components by group, including capital gains.
8. **Current BEA vintage (September 2026)** for the interest, dividend,
   mortgage-interest and SALT series. Use the monetary rather than the
   total interest and dividend lines.
9. **Census quarterly state and local tax collections,** for SALT
   growth in 2025–2026.
10. **Updated SSA life tables** (2025 confirmed, 2026 likely) and the
    2026 Trustees single-year tables, as a second long-run demographic
    path.
11. **CMS health expenditure projections,** for medical expenses and
    premiums.
12. **Treasury monthly receipts (non-withheld, April):** a first signal
    of 2025 capital gains and business income.

All of these sources should also be written down in a provenance file
next to the resource they feed. Today the source of each file has to be
reverse-engineered.

## Looked for and not found, or not confirmed

- A CBO baseline update after February 2026: none as of the archived 21
  September 2026 page.
- Any CBO, JCT or Treasury projection of Form 1040 lines after 2036.
- A current BEA reconciliation of personal income with AGI. None after
  2005.
- A non-IRS total of IRA distributions.
- The source of the `ui` column in `resources/cbo_1040.csv`, and of
  `sp500_total_return_index.csv`.
- How Macro-Projections carries its 2026 vintage from 2057 to 2099. The
  checked-out code is an older version.
- Whether the DFA's published income groups rank on realized or "usual"
  income.
- The 2026-report SSA population files and the opened 2026 period life
  tables.
- The date of the 2025 ACS one-year release.
- The 2025 SSA wage statistics, not yet out on 4 October 2026.
- IRS 2026 filing-season counts of claims for the new deductions.
- The contents of CBO's 2018 tax-model overview (publication 54096).

## Summary table

| Source | Measures | Unit | Granularity | Years available (Oct 2026) | Lag, frequency | Format | Projection or observation | Used by repo |
|---|---|---|---|---|---|---|---|---|
| CBO revenue workbook, sheet 3 | 1040 income lines, deductions, taxable income by bracket, returns, itemizers, AGI shares | Tax return | National; AGI shares by top groups | 2022–2036 (2022 actual) | Each baseline (Feb 2026 latest) | xlsx, csv on GitHub | Projection | 8 columns as growth rates |
| CBO revenue workbook, sheet 6 | Capital-gains realizations and receipts | Aggregate | National | 1996–2036 (CBO from 2024) | Each baseline | xlsx | Observation to 2023, then projection | Partly (`txbl_kg` from sheet 3) |
| CBO economic projections | NIPA income components, prices, labor | Aggregate | National, quarterly and annual | History to 2025, projection to 2036 | Each baseline | xlsx, csv | Projection | Via Macro-Projections |
| CBO long-term outlook | GDP, earnings, taxable-max share; AGI shares by statutory rate | Aggregate | National | To 2056 | Annual | xlsx | Projection | Via Macro-Projections (GDP growth) |
| CBO long-term Social Security projections | Covered earnings, taxable payroll, share below taxable max, beneficiaries | Aggregate | National | 2026–2100 | Annual (Sept 2026) | xlsx | Projection | No |
| CBO demographic projections | Population by age, sex, marital status; fertility, mortality, migration | Person | Single year of age | 2022–2099 | Annual (Jan 2026) | csv | Projection | Via Macro-Projections (married/unmarried only) |
| CBO Distribution of Household Income | Income components and taxes by group | Household | Quintiles, top 10/5/1 percent, household types | 1979–2022 | About 3 years, roughly annual | xlsx, csv | Observation | No |
| JCT tax expenditures (JCX-45-25) | Returns, itemizers, liability; deduction and credit counts and amounts | Tax return incl. non-filers | 12 expanded-income classes | 2025 (tables) | Annual, December | PDF | Projection | No |
| JCT distribution tables (JCX-37-25) | Federal taxes by class | Tax unit | 12 classes | 2027–2033 | Per bill | PDF | Projection | No |
| Treasury distribution tables | Cash income and taxes; income by source | Tax family incl. non-filers | Deciles, top percentiles | 2025 | Annual (none for 2026) | xlsx | Projection | No |
| Treasury family counts | Tax units by filing status, children, decile | Tax family | Deciles | 2024 | One-off (Jan 2025) | xlsx | Estimate | No |
| Treasury monthly and daily statements | Withheld and non-withheld receipts | Aggregate | National | 2015–Aug 2026 (monthly); daily to Oct 2026 | Days to weeks | API | Observation | No |
| BEA personal income | Wages, proprietors, rent, interest, dividends, transfers | Aggregate | National, monthly | 1929–Aug 2026 | About 1 month | API, txt, csv | Observation | Indirectly (CBO, Macro-Projections), plus mortgage and SALT lines |
| BEA PCE by product | Consumption by category | Aggregate | National; states annual | To 2025 (annual detail) | About 1 month | API, csv | Observation | Yes |
| BEA distribution of personal income | Income components by decile | Household | Deciles, top 1 and 5 percent | 2000–2024, 2025 nowcast | Annual (June) | csv, xlsx | Estimate | No |
| BEA misreporting tables (7.14, 7.18) | NIPA vs IRS business income and wages | Aggregate | National | To 2023 (7.14), 2025 (7.18) | Annual | API | Estimate | No |
| Federal Reserve Z.1 | Household balance sheet | Aggregate | National, quarterly | To 2026Q2 | About 10 weeks | csv | Observation | Via DFA |
| Federal Reserve DFA | Wealth by group | Household | Wealth, income, age and other groups | 1989Q3–2026Q2 | About 11 weeks | csv | Estimate | Yes (to 2025Q4) |
| New York Fed household debt | Mortgage, auto and other balances | Person (credit file) | Age, credit score, some states | To 2026Q2 | About 6 weeks | xlsx | Observation | No |
| Piketty–Saez–Zucman | Income and wealth by group | Equal-split adult | Fine percentiles | 1913–2019 | Stale | xlsx, dta | Estimate | No |
| Realtime Inequality | Income and wealth by group | Adult, tax unit | Fine percentiles, no components | 1976–Jun 2026 | Quarterly update | xlsx, json | Nowcast | No |
| Auten–Splinter | Top shares, sources, non-filers, tax-unit counts | Tax unit | Fine percentiles | 1960–2022 | Irregular | xlsx | Estimate | No |
| SSA wage statistics | W-2 workers and wages by bracket | Person | Brackets to $50 million+ | 1991–2024 (2025 due Oct 2026) | About 10 months, annual | html | Observation | No |
| SSA Trustees Report | Demography, wage index, taxable payroll, beneficiaries | Person, aggregate | Single-year tables; by age in OACT files | History to 2100 | Annual (June 2026) | xlsx, csv | Projection | Life tables (2024 report) |
| SSA Statistical Supplement | Earnings by amount, benefits by amount | Worker, beneficiary | Brackets, age, state | To 2023–2024 | About 15 months | xlsx | Observation | No |
| BLS QCEW | Wages and employment | Job, aggregate | County, state, industry | To 2026Q1 | 5–6 months | csv | Observation | No (state files in raw_data) |
| BLS CES, ECI, ECEC | Employment, earnings, compensation costs | Job | National, state, industry | To Sept 2026 | 1 week to 1 quarter | csv, API | Observation | No |
| BLS occupational wages (OEWS) | Employment and wages by occupation | Job | Occupation, state, metro | May 2025 | Annual | xlsx | Observation | No |
| BLS CEX published tables | Spending and income | Consumer unit | Quintiles, deciles, age | To 2024 (2025 due 29 Oct 2026) | About 10 months | xlsx | Observation | Microdata only |
| Census population estimates | Population by age and sex | Person | Single year of age, state | To July 2025 | About 11 months | csv | Observation | No (CBO used instead) |
| Census ACS tables | Income, housing costs, real estate taxes | Household | State and below | 2024 (2025 delayed) | About 9 months normally | API | Observation | Microdata elsewhere |
| Census quarterly tax collections | State tax collections by type | Aggregate | State | To 2026Q2 | About 2.5 months | xlsx, API | Observation | No |
| Census Nonemployer Statistics | Firms and receipts | Firm | Industry, state, county, legal form | To 2024 | About 21 months | API | Observation | No |
| Census NEWS | Survey income corrected with records | Household | Distribution | 2016–2021 | Irregular | xlsm | Estimate | No (local copy exists) |
| CMS health expenditures | Out-of-pocket, premiums by payer | Aggregate | National | History to 2024; projection to 2034 | Annual | xlsx | Both | No |
| Giving USA | Giving by source | Aggregate | National | To 2025 | About 6 months | Paid report | Estimate (partly SOI) | Yes (to 2024) |
| Atlanta Fed wage tracker | Median wage growth | Worker | Wage quartile and other groups | To Aug 2026 | About 1 month | xlsx | Observation | No |
| ADP Pay Insights | Median pay growth | Worker | Industry, size, region | To Sept 2026 | Under 1 month | Dashboard | Observation | No |
| ATTOM property tax | Property tax levied on single-family homes | Property | State, county | Tax year 2025 | About 4 months | Press release (data licensed) | Observation | No |
