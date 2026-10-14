---
title: "IRS information on individual income tax returns, tax years 2015 onward"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# IRS information on individual income tax returns, tax years 2015 onward

Background research for the Tax-Data rebuild (`research/rebuild/BRIEF.md`).
Question: how much does the IRS publish about the distribution of individual
returns after 2015, in aggregate or tabular form, that could serve as targets
or priors?

**Verified** below means I opened the page or file on 6 October 2026 and saw
the fact, or read it in the shared data store described in the section on
the repository's current use. **Not verified** means I believe it from a
search result, memory, or a secondary source, and did not open the primary
document. Release dates come from the Statistics of Income (SOI) Division's
2026 release calendar, revised 10 September 2026
(https://www.irs.gov/pub/irs-soi/soi-public-release-list.xlsx, verified).

## Summary

1. **The newest IRS sample-based detail is tax year 2023.** The Publication
   1304 tables for tax year 2023 came out on 26 March 2026, about 27 months
   after the end of the tax year. Tax year 2024 is not on the 2026 release
   calendar, so expect it in 2027.
2. **The newest IRS distributional data of any kind is tax year 2024**, from
   the "filing season statistics": counts of all returns processed by mid-
   November 2025, about 50 items in 17 AGI classes, no filing status or age.
   These are population counts, not a sample. Tax year 2025 versions were
   scheduled but are not posted as of 6 October 2026.
3. **There is no IRS microdata after 2015, and SOI has withdrawn the 2012–2015
   public use files** ("no longer available", page updated 22 May 2026). A
   synthetic public use file for tax year 2016, and a synthetic file of
   non-filers, were scheduled for 21 November 2025; as of May 2026 the page
   says the synthetic file "will be available in the next few months."
4. **SOI stopped publishing preliminary-year data** (tax year 2023 was
   cancelled), and the SOI Bulletin no longer carries the individual-returns
   article. The early look at a tax year is now only the filing season
   statistics.
5. **The repository uses a small slice of what exists.** Weighting targets
   are for 2017 only (188 targets from Pub 1304 Tables 1.4, 1.4A, 1.6, 2.1,
   2.3, 3.3 and Pub 4801). Table 1.4 per-return averages grow incomes for
   2018–2019, Table 1.6 counts grow weights through 2023, and CBO
   projections take over for income from 2019. Pub 1304 is published for
   every year 2016–2023 for every one of these tables. A hand-compiled
   version 3 of the tables interface with 2022 exists but the runscript
   reads version 2.
6. **Unused tables with high value for the rebuild:** Table 1.5 (every income
   source by age of primary taxpayer), Tables 1.2 and 1.3 (by filing
   status), Table 4.3 (percentile groups down to the top 0.001% with income
   by source), Table 2.5 (EITC by number of children and AGI), Tables 3.4 and
   3.6 (tax by marginal rate and filing status), the coefficient-of-variation
   and confidence-interval companions to Table 1.4, the IRA study (balances
   and contributions by age and AGI percentile), the W-2 study (wages,
   retirement deferrals and plan participation by age and sex, per earner),
   and Pub 5385 (totals from information returns such as 1098, 1099-R,
   5498 and Schedule K-1).
7. **Most of it is already downloaded.** The shared store
   `raw_data/IRS-Ind` (maintained in the IRS-Ind repository) mirrors the Pub
   1304 by-size tables for 2011–2023 with cleaned long-format panels, a
   Table 1.6 target view that handles SOI's merged disclosure cells, Pub 4801
   line items parsed for 2011–2023, and the IRA, W-2, sole proprietor,
   capital assets, state, county and ZIP series. Tax-Data does not read it.
   Not yet mirrored: Tables 1.3, 1.5, 1.4CV, 1.4CI, 3.4, 3.6, 4.1–4.3, the
   filing season statistics, and a parsed Pub 5385.
8. **Censoring by tax law runs through the tables.** Itemized deductions
   (Table 2.1) cover itemizers only, 15.1 million returns in 2023 against
   46.9 million in 2017, but for those itemizers Table 2.1 reports
   state and local taxes before the cap and medical expenses before the AGI
   floor. Social Security benefits are seen only for filers. Dependents are
   seen after 2017 only through credit claims. Pub 5385 information-return
   totals (for example all mortgage interest on Form 1098) are not censored
   by the deduction rules but carry no income dimension.
9. **The tables come with usable precision information.** The Pub 1304 sample
   is about 360,000 returns (tax year 2020, stratified, top incomes taken with
   certainty), and SOI publishes coefficients of variation for Table 1.4 and
   for the IRA tables. This maps directly onto the "targets with stated
   precision" framing.
10. **IRS projections are of return counts only**: Publications 6187, 6292,
    6149 and 6186 project numbers of returns and of selected schedules
    through 2032. No IRS publication projects incomes. Pub 6187 projects
    returns with Schedule A rising from 15.7 to 20.5 million between
    processing years 2025 and 2026, consistent with the higher state and
    local tax deduction cap.
11. **Coming in the next two months:** W-2 statistics for tax years
    2021–2023 and ZIP-code and congressional-district data for 2023 (22
    October 2026), Sales of Capital Assets for tax year 2018 (24 November
    2026; the series stopped at 2015 and this release has been postponed
    three times), and partnership tables for 2024 (17 December 2026).

## What the repository uses now

All paths are relative to the repository root unless they start with
`/nfs`.

| Input | IRS source behind it | Years in the file | How it is used |
|---|---|---|---|
| `Compiled-SOI-Tables` interface, version 2, vintage 2023100323 (`/nfs/roberts/project/pi_nrs36/shared/model_data/Compiled-SOI-Tables/v2/2023100323/historical/`) | Pub 1304 Tables 1.4, 1.4A (file `table_1_4_kg`), 1.6, 2.1, 2.3, 3.3; Pub 4801 partnership and S corporation active, passive and section 179 lines | 1.4: 2017–2019; 1.4A: 2015, 2017; 1.6: 2015, 2017; 2.1, 2.3: 2017; 3.3: 2017; Pub 4801: 2017 | `src/process_targets.R` reads them. 2017 values become the 188 weighting targets (`config/target_info/baseline.csv`) and the within-AGI-bin rescaling factors (`resources/variable_table_crosswalk.csv`, `scale_by_agi`). Table 1.6 for 2015 sets the age imputation (`src/process_puf.R`) and for 2017 the cell scaling (`src/create_2017_puf.R`). Table 1.4 per-return averages for 2017–2019 grow ten income items (`src/project_puf.R`, `irs_growth_factors_income`). |
| `Compiled-SOI-Tables` version 3, vintage 20250319 | Same tables | Adds 2022 to 1.4, 1.4A, 1.6, 3.3 and Pub 4801 | Not read: `config/interfaces/interface_versions.yaml` pins version 2. |
| `resources/return_counts_2023.csv` | Pub 1304 Table 1.6 totals by filing status × age group | 2017–2023 | Weight growth factors by demographic cell (`src/project_puf.R`, `FILER_OBS_LAST = 2023`). |
| `resources/return_counts_2019.csv` | Same, older | 2017–2019 | Still read in `src/project_puf.R` for the itemized-deduction and consumption per-return factors. |
| `resources/soca_*.csv` | SOI Sales of Capital Assets study | 1985, 1999, 2007–2015 | Holding period and basis-to-sales ratios for capital gains (`src/imputations/capital_gains.R`, `src/project_puf.R`). |
| `resources/cbo_1040.csv` | CBO projections of Form 1040 income items (CBO, built on SOI data; not an IRS product) | 2019–2036 | Near-term income growth from 2019. |
| `resources/itemized_deduction_projections.csv` | Source not stated in the file or code; not IRS as far as I can tell | 2017–2024 | Growth of mortgage interest, charity, state and local taxes. |
| `resources/qbi.csv` | Business tabulations by industry for partnerships and S corporations (source not stated in the file) | one year | Specified-service and wage imputation for the qualified business income deduction. |
| `research/state_weights/...` | Pub 1304 Table 1.6, Historic Table 2 (state), SOI IRA study Table 4, IRS Pub 5785 non-filer tax gap tables | 2014–2023 | Non-filer residual anchors and state weights (research corpus, feeding the `ASEC-Nonfilers` interface). |

Observations:

- **Only one year of targets.** The weighting step fits 2017. For 2018–2023
  the IRS published every table the targets come from, but the pipeline
  moves those years with single growth factors.
- **Hand compilation.** The interface's own readme says the files "were
  compiled by hand" and should ideally be generated by a script from
  downloaded SOI data.
- **The shared store `raw_data/IRS-Ind`**
  (`/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-Ind/`, notes in its
  `NOTES.md`, built 2026-09-27) is the scripted replacement. It holds:
  Pub 1304 Tables 1.1, 1.2, 1.4, 1.4A, 1.6, 1.7, 2.1, 2.3, 2.5, 2.7, 3.1,
  3.1A, 3.2, 3.3, 3.5 for 2011–2023 (2.3 from 1996 to its end in 2017);
  `aligned/bysize_*.csv` long panels with SOI's disclosure flags decoded;
  `aligned/targets_returns_marital_age.csv`, Table 1.6 as reweighting
  targets with merged cells grouped into blocks; `aligned/line_items.csv`,
  Pub 4801 parsed for 2011–2023 (24,838 values, checked against Pub 1304:
  152 of 156 comparisons exact, 4 differ by one in the last digit); Pub
  5385 PDFs 2017–2023 (not parsed); IRA tables 2000–2023; W-2 tables
  2014, 2016–2020; sole proprietor tables 1998–2023; Sales of Capital
  Assets 1985–2015; Historic Table 2 by state 2012–2023; state AGI
  percentiles 2013–2023; county data 2011–2023; ZIP data 2011–2022. Its
  notes say Table 1.6 status-by-age totals reproduce
  `resources/return_counts_2023.csv` exactly. Its only listed consumer is
  the Affordability-Index repository.

## Release timing as of October 2026

From the 2026 release calendar (verified) and the "What's new" page
(https://www.irs.gov/statistics/soi-tax-stats-whats-new, verified).

| Product | Newest tax year out | Released | Next |
|---|---|---|---|
| Filing season statistics (all returns processed by a date) | 2024 (mid-November 2025 cut) | Jan 2026 | Tax year 2025 late-May cut "pending"; mid-July cut scheduled 17 Sep 2026 but not posted on 6 Oct 2026 |
| Pub 1304 tables | 2023 | 26 Mar 2026 (first scheduled 11 Dec 2025) | 2024 not on the 2026 calendar |
| Pub 1304 complete report (PDF) | 2022 is the newest full PDF listed on the Pub 1304 page | — | — |
| Preliminary data (SOI Bulletin) | 2022 | Spring 2024 | 2023 cancelled |
| Pub 4801 line items | 2023 | Jun 2026 | — |
| Pub 5385 information-return line items | 2023 | Jun 2026 | — |
| Returns with business attributes | 2023 | Mar 2026 | — |
| Nonfarm sole proprietorships | 2023 | Apr 2026 | — |
| High-income returns study | 2022 | Nov 2025; Spring 2026 Bulletin article | — |
| IRA study | 2022 and 2023 (new method) | Jun 2026 | — |
| W-2 statistics | 2020 (2019–2020 reissued as one file, Jan 2026) | Jan 2026 | 2021, 2022, 2023 on 22 Oct 2026 |
| Noncash charitable contributions | 2023 | Jun 2026 | — |
| State data (Historic Table 2), county, metro, state percentiles | 2023 | Aug 2026 | — |
| ZIP code, congressional district | 2022 | — | 2023 on 22 Oct 2026 |
| State and county migration | 2022–2023 | Mar 2026 | 2023–2024 on 20 Nov 2026 |
| Sales of Capital Assets | 2015 | — | 2018 on 24 Nov 2026 (postponed from 26 May and 27 Aug) |
| Partnerships | 2023 | Dec 2025 | 2024 on 17 Dec 2026 |
| Estate tax, filing-year basis | filing year 2024 | Jul 2026 | — |
| Estate tax, year-of-death basis | 2022 due | — | 19 Nov 2026 |
| Personal wealth (estate multiplier) | 2019 | — | not scheduled |
| Tax gap estimates | 2023 due | — | 6 Nov 2026 |
| Return projections (Pubs 6187, 6292, 6149, 6186) | 2025–2032 edition | Dec 2025 | 2026 edition 17 Dec 2026 |
| IRS Data Book | fiscal year 2025 | Jun 2026 | — |

Rule of thumb: a tax year's population counts by AGI arrive about 14 months
after the year ends; the sample-based detail about 27 months after; state
detail about 32 months after.

## Sources, one by one

### Publication 1304 basic tables (the Complete Report)

**What it is.** Estimates from SOI's stratified sample of individual returns,
"selected before audit." For tax year 2020 the sample was about 360,000
returns out of roughly 167 million; returns with very high or very
negative income were taken with certainty (all returns with indexed
positive income of $8 million or more, in 2016 dollars) (Pub 1304 Rev.
11-2022, "Description of the Sample," https://www.irs.gov/pub/irs-prior/p1304--2022.pdf,
verified). This is the same sample from which the PUF was drawn.

**Where.** Basic tables part 1:
https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-returns-complete-report-publication-1304-basic-tables-part-1
(verified); by-size listing:
https://www.irs.gov/statistics/soi-tax-stats-individual-statistical-tables-by-size-of-adjusted-gross-income
(verified; newest year 2023 for every live table); tax rate and percentile
tables:
https://www.irs.gov/statistics/soi-tax-stats-individual-statistical-tables-by-tax-rate-and-income-percentile
(verified). Files are `https://www.irs.gov/pub/irs-soi/{yy}in{table}.xls`,
for example `23in14ar.xls`. All are Excel (`.xls`, a few `.xlsx`), with
multi-row headers and disclosure flags stored as number formats.

**Tables.** Table 1.4 has 19 AGI classes, from "no AGI" to $10 million or
more. "Filing status" below means single, married filing
jointly (with surviving spouses), married filing separately, head of
household.

| Table | Contents | Dimensions | Years (file probe or page) | In repo? |
|---|---|---|---|---|
| 1.1 | Selected income and tax items, with cumulative classes | AGI | 1996–2023 | No |
| 1.2 | AGI, exemptions, deductions, tax items | AGI × filing status | 1996–2023 | No |
| 1.3 | Every income source, adjustment, deduction, credit and tax | filing status (no AGI) | 2010, 2012, 2014–2023 confirmed by probe (other years not probed) | No |
| 1.4 | Every income source, adjustment, deduction and tax, count and amount | AGI (19 classes) | 1993–2023 | Yes, 2017–2019 |
| 1.4A | Capital gains and losses: short- and long-term gains, losses, carryovers, before the $3,000 loss limit | AGI | 2012–2023 | Yes, 2015 and 2017 |
| 1.4CV, 1.4CI | Coefficients of variation and confidence intervals for Table 1.4 | AGI | 1.4CV confirmed 2010, 2012, 2014–2023; 1.4CI 2023 (other years not checked) | No |
| 1.5 | Income sources, adjustments and tax items | age of primary taxpayer (under 18, 18–25, 26–34, 35–44, 45–54, 55–64, 65+) | 2010, 2012, 2014–2023 confirmed by probe | No |
| 1.6 | Number of returns | AGI × filing status × age | 2008–2023 | Yes, 2015, 2017, and totals 2017–2023 |
| 1.7 | Dependent filers' income, deductions, tax | AGI | 2012–2023 | No |
| 2.1 | Itemized deductions in detail, plus income items of itemizers | AGI | 1993–2023 | Yes, 2017 |
| 2.3 | Exemptions by type (dependents at home, away, parents, other) | AGI | ends 2017 (exemptions repealed) | Yes, 2017 |
| 2.5 | Earned income credit: earned income, credit, refundable part | AGI × number of qualifying children (0, 1, 2, 3+) | 1996–2023 | No |
| 2.7 | Affordable Care Act items (premium tax credit, mandate payment) | AGI | 2014–2023 | No |
| 3.1, 3.1A | Modified taxable income and tax by computation type; Form 8615 ("kiddie tax") | AGI | 1996–2023; 2008–2023 | No |
| 3.2 | Total income tax as a percent of AGI | AGI × filing status | 1996–2023 | No |
| 3.3 | Tax liability, every credit (child tax credit and its refundable part, education, foreign tax, etc.), payments | AGI | 1996–2023 | Yes, 2017 |
| 3.4 | Tax by marginal rate | rate × filing status | 1996–2023 | No |
| 3.5 | Tax generated by rate bracket | rate × AGI | 1996–2023 | No |
| 3.6 | Taxable income and tax by rate | rate × filing status | 1996–2023 | No |
| 4.1, 4.2 | Shares of AGI and tax, AGI floors | descending / ascending cumulative percentiles, returns excluding dependents | 2001–2023 | No |
| 4.3 | Returns, AGI share, wages, interest, dividends, business income, capital gains, partnership and S corporation income, deductions, credits, tax, AGI floors | top 0.001%, 0.01%, 0.1%, 1%, 2%, 3%, 4%, 5%, 10%, 20%, 25%, 30%, 40%, 50% | 2019–2023 confirmed by probe; page lists single years back to 2014 | No |

Verified by opening the files: Table 1.5 for 2015 and 2023 (age bands
above; "Age for joint returns was based on the primary taxpayer's age");
Table 1.3 for 2023; Table 1.4CV for 2023; Table 4.3 for 2023 (title "All
Individual Returns Excluding Dependents ... by Selected Expanded Descending
Cumulative Percentiles of Returns Based on AGI, Tax Year 2023," source line
"Publication 1304, March 2026"). Table 1.6 has merged disclosure cells that
move counts between age rows; the store's target view handles this.

**Censoring notes.** Table 2.1 covers itemizers only. For them it reports
total medical expenses before the AGI floor, and state and local income or
sales taxes, real estate taxes and personal property taxes before the cap,
alongside the capped amount (item list verified in the store's 2023
panel). Table 1.4's net capital gain line reflects the $3,000 loss limit;
Table 1.4A gives gains and losses before it. Partnership, S corporation and
rental amounts are after passive-loss and at-risk limits. Credits appear
only as claimed and as limited by tax liability, with the refundable parts
shown separately in Table 3.3. Tables 4.1–4.3 exclude dependent returns.

**Top 400 returns.** SOI's report on the 400 returns with the largest AGI
ended with tax year 2014; the replacement is the top 0.001% row of the
percentile tables (search result describing the SOI page; not verified;
https://www.irs.gov/pub/irs-soi/14intop400.pdf). Table 4.3's 0.001% group
was about 1,400 returns in 2014 by the same source.

### Filing season statistics

**What it is.** Aggregates of all Forms 1040 processed through the IRS
Individual Master File by three dates in the filing year: late May (week
21), mid-July (week 30), mid-November (week 47). SOI says late May covers
about 90% of returns, 84% of AGI and 80% of tax; mid-July about 95%, 87%
and 82% (page verified). Mid-November is nearly complete. Population
counts, so no sampling error, but processing-based and not edited the way
the SOI sample is.

**Dimensions.** 17 AGI classes from "no AGI" to $1 million or more. Verified
in `25inweek30.xlsx` (filing season 2025 = tax year 2024): number of
returns, AGI, total income, wages, taxable interest, ordinary and qualified
dividends, taxable IRA and pension income, taxable Social Security,
capital gains, state refunds, Schedule C, Schedule E, Schedule F,
unemployment, statutory adjustments and their main parts, standard
deduction (basic and additional), itemized deductions and their main
parts, qualified business income deduction, taxable income, tax, AMT,
credits (child, child care, education, saver's, premium, EITC), self-
employment tax, additional Medicare tax, net investment income tax,
payments, balance due, overpayments. No filing status, age or state.

**Years and format.** Files named by filing year: `{yy}inweek{21,30,47}`,
filing years 2018–2025 (tax years 2017–2024) on the page; `.xls` through
2024, `.xlsx` for 2025; 2018–2019 have "expanded" versions. Page:
https://www.irs.gov/node/9630 (verified; the descriptive slug URL returned
404). Example: https://www.irs.gov/pub/irs-soi/25inweek47.xlsx (verified to
exist). Filing-year 2026 files (`26inweek21.xlsx`, `26inweek30.xlsx`)
return 404 on 6 October 2026.

**Not used by the repository.** This is the only IRS distributional source
for tax year 2024 and will be the first for 2025.

### Preliminary data and the SOI Bulletin

The preliminary-data table (returns filed through September, weighted to a
full year) last appeared for tax year 2022 (Spring 2024 Bulletin). The
What's New page lists "Cancelled: Individuals, preliminary data, tax year
2023" (verified). The Winter and Summer 2026 Bulletins have no featured
article and point readers to Pub 1304
(https://www.irs.gov/statistics/soi-tax-stats-soi-bulletin-winter-2026,
https://www.irs.gov/statistics/soi-tax-stats-soi-bulletin-summer-2026,
both verified). The Spring 2026 issue has "High-Income Tax Returns for Tax
Year 2022" (search result; not opened:
https://www.irs.gov/statistics/soi-tax-stats-soi-bulletin-spring-2026).

### Line item estimates (Publication 4801)

**What it is.** For every line of Form 1040 and about 57 schedules and forms
(Schedules 1, 2, 3, A, B, C, D, E, F, SE, 8812; Forms 2441, 4562, 4797,
5329, 5695, 6251, 8582, 8606, 8615, 8829, 8863, 8889, 8936, 8959, 8960,
8962, 8995 and others), the estimated number of returns with an entry and
the amount, from the Pub 1304 sample. National only, no AGI dimension.
Matching items agree exactly with Pub 1304.

**Use.** Covers items Pub 1304 does not break out: health savings account
contributions, Form 8582 passive losses allowed and suspended, Form 6251
AMT adjustments, Form 8995 qualified business income components, Schedule
8812 child counts and credit components, Form 8962 premium tax credit
reconciliation, Schedule E partnership and S corporation active and passive
splits (the repository uses these for 2017).

**Years and format.** 2003–2023, PDF only, printed on facsimiles of the
forms (page verified:
https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-returns-line-item-estimates-publications-4801-and-5385).
The store's parser has extracted 2011–2023.

### Information returns line item estimates (Publication 5385)

**What it is.** For each information return linked to the Pub 1304 sample,
the number of forms, the number of individuals and the amount on each
line. Verified in the tax year 2023 edition (Rev. 5-2026, local copy
`national/line_items/p5385_2023.pdf`): Forms W-2 (279 million forms), W-2G,
1098 (mortgage interest), 1098-C, 1098-E (student loan interest), 1098-Q,
1098-T (tuition), 1099-A, B, C, DIV, G, INT, K, LTC, MISC, NEC, OID, PATR,
Q, R, S, SA, 3921, 3922, 5498, 5498-ESA, 5498-SA, and Schedules K-1 from
Forms 1065, 1120-S and 1041. I did not find Form SSA-1099 in it.

**Population.** Information returns for people in the sampled tax returns,
their dependents (counted with the claiming return), **and a simple random
sample, by Social Security number digits, of people who received
information returns but filed no tax return.** So the totals cover filers
and non-filers. I did not find a filer / non-filer split in the 2023
edition.

**Censoring.** None from deduction rules: Form 1098 totals are all mortgage
interest reported by lenders, whether or not anyone itemized; Form 5498
totals are IRA contributions, deductible or not; W-2 totals include
elective deferrals that never enter AGI. Thresholds for issuing a form
still apply (for example Form 1099-K reporting thresholds).

**Years and format.** 2017–2023, PDF only. Not parsed in the store; not used
by the repository.

### State and local area data

From the store's notes (verified against the files) and the 2026 calendar.
Not used by Tax-Data's pipeline; Historic Table 2 is used in the state
weights research.

- **Historic Table 2 (state).** State × 10 AGI classes (top class $1 million
  or more), 165 columns in 2023 (most items appear as a count and an amount): returns by filing status, number of
  individuals, elderly (primary 60+), every major income source, itemized
  deduction detail, credits including EITC by number of children (from
  2022), tax. 2012–2023 (2013 in per-state Excel only). CSV. Counts
  rounded to 10; small cells merged silently into neighbors.
- **State AGI percentiles.** Top 1, 5, 10, 25, 50, 75% within each state:
  counts, AGI floors, wages, interest, dividends, business income, capital
  gains, IRA, pensions, partnership plus S corporation income, tax.
  2013–2023. Excludes dependent and negative-AGI returns.
- **County, metropolitan area.** County × AGI classes (the class scheme
  changed in 2013); 2011–2023.
- **ZIP code.** ZIP × 6 AGI classes (top class $200,000+), negative AGI
  excluded; 2011–2022; 2023 due 22 October 2026.
- **Congressional district.** 2022; 2023 due 22 October 2026.
- **Migration.** Returns and AGI moving between states and counties,
  calendar years 2022–2023 out (March 2026).

A Congressional Research Service report on these tabulations, R49356
(September 2026), exists (https://www.everycrsreport.com/reports/R49356.html,
not opened).

### High-income returns study

Returns with AGI or "expanded income" of $200,000 or more, income under
alternative concepts, and high-income returns with no income tax and the
reasons. Newest is tax year 2022 (released November 2025; Bulletin article
Spring 2026, https://www.irs.gov/pub/irs-soi/soi-a-inhi-id2601.pdf, not
opened). By the search summary, 12.5 million returns had AGI of $200,000 or
more in 2022 and 9,997 had no worldwide income tax. Excel tables; I did not
open them. Not used.

### Business income reaching individuals

- **Returns with business attributes.** Returns with Schedule C, E or F
  income or loss: income items, adjustments, credits, taxes by AGI.
  2015–2023, Excel (https://www.irs.gov/node/70336, verified). Not used.
- **Nonfarm sole proprietorships.** Schedule C receipts, deductions,
  payroll and net income by industry, 1998–2023 (Tables 1–2). Table 4
  (Schedule C returns by AGI × filing status × age × industry) exists only
  for 2017–2020 and Table 3 (by size of receipts) for 2016–2020
  (https://www.irs.gov/statistics/soi-tax-stats-nonfarm-sole-proprietorship-statistics;
  store notes, verified). Not used.
- **Partnerships.** Tables by industry, 1993–2023, Excel
  (https://www.irs.gov/statistics/soi-tax-stats-partnership-statistics-by-sector-or-industry,
  verified). The annual partnership article (Publication 5338,
  https://www.irs.gov/pub/irs-pdf/p5338.pdf, not opened) reports income
  allocated by type of partner, including individuals; by the search
  summary, 2023 had 4.5 million returns and 30.2 million partners.
  Tax year 2024 due 17 December 2026. Not used.
- **S corporations.** Part of the corporation complete report (Publication
  16), tax year 2023 scheduled 30 September 2026. Not checked in detail.
- **Schedule K-1.** No standalone SOI K-1 tabulation found. Pub 5385 carries
  K-1 line totals (Forms 1065, 1120-S, 1041) linked to individuals.
- **Qualified business income deduction.** In Table 1.4 (by AGI), the
  filing season statistics, and Form 8995/8995-A lines in Pub 4801.

### Capital gains

- **Table 1.4A** (above): by AGI, 2012–2023.
- **Sales of Capital Assets study.** Transactions by asset type, holding
  period, month of sale, and AGI. Cross-sections 1985, 1997–1999,
  2007–2015, and panels 1999–2003 and 2004–2007
  (https://www.irs.gov/statistics/soi-tax-stats-sales-of-capital-assets-reported-on-individual-tax-returns;
  store notes, verified). Tax year 2018 scheduled for 24 November 2026.
  The repository uses 1985–2015.

### Retirement accounts and wages from information returns

- **IRA study.** From Forms 5498 and 1099-R matched to returns: IRA
  contributions (traditional, Roth, SEP, SIMPLE, rollovers), fair market
  value (balances), distributions. Tables by plan type (1), AGI (2, 3),
  age (4, 8), size of contribution × age (5, 6), filing status and sex
  (7), AGI percentile (9, 10). 2000–2023 except 2003; percentile tables
  from 2018; confidence intervals 2022–2023, coefficients of variation
  2018, 2020–2022. "Beginning with data for 2022, SOI introduced
  enhancements," so 2022 is published on both methods
  (https://www.irs.gov/statistics/soi-tax-stats-accumulation-and-distribution-of-individual-retirement-arrangements,
  verified). Excel. The repository's research uses Table 4 for an age
  split; the pipeline does not use it. No table on employer plans
  (401(k) balances).
- **W-2 statistics.** Per earner, from W-2s matched to returns: wages by
  age and sex, size of wages, size of AGI, return and earner type, and
  multiples of the Social Security wage cap (Table 1); elective deferrals
  to 401(k)-type plans, including for both spouses and those at the
  maximum (Table 2); retirement plan indicator (Table 3); two-earner joint
  returns (Table 4). Available for 2014 and 2016–2020 (no 2015); the
  2014–2018 workbook has seven tables and age-stratified sheets; 2019–2020
  reissued in January 2026 with age statistics
  (https://www.irs.gov/statistics/soi-tax-stats-individual-information-return-form-w2-statistics,
  verified). 2021–2023 due 22 October 2026. Not used. This is the only
  IRS source for the split of wages between spouses and for wage
  distributions by age and sex; the repository currently splits spouses'
  wages using Saez tabulations for 2014 (`resources/wagesplit.csv`).

### Charitable contributions

Table 2.1 (itemizers only); Table 1.4 for the 2020–2021 deduction for
non-itemizers; the noncash contributions study (Form 8283, donations over
$500: asset type, donee type, cost and fair market value), tax year 2023
released June 2026 (What's New, verified; tables not opened). After 2017
the itemizer slice is censored by the higher standard deduction; from
2026 law adds a deduction for non-itemizers, which will appear in future
tables (not verified).

### Credits

- **EITC.** Table 2.5 by AGI × number of children (1996–2023); Historic
  Table 2 by state × children (from 2022); EITC claims by state through
  tax year 2024 (processed 2025)
  (https://www.irs.gov/credits-deductions/individuals/earned-income-tax-credit/statistics-for-tax-returns-with-the-earned-income-tax-credit-eitc,
  verified); participation rates by state, estimated by Census and IRS
  from linked survey and tax records, through tax year 2022 (80.8%
  nationally) (https://www.irs.gov/tax-professionals/eitc-central/eitc-participation-rate-by-state,
  search summary, not opened).
- **Child tax credit.** Table 3.3 (nonrefundable and refundable parts, by
  AGI), Schedule 8812 lines in Pub 4801.
- **Premium tax credit.** Table 2.7, Form 8962 in Pub 4801.
- **Clean vehicle credits.** Tax year 2023 tables, December 2024 (What's
  New; not opened).

### Alternative minimum tax

AMT amount by AGI in Table 1.4 and the filing season statistics; Form 6251
lines (adjustments, exemption) in Pub 4801; Tables 3.1 by tax computation
type. These are computed amounts; the underlying AMT preference items are
reported only by people who file Form 6251.

### Social Security benefits

Total and taxable benefits by AGI (Table 1.4, filing season statistics), by
age (Table 1.5), by state (Historic Table 2). Seen only for beneficiaries
who file. Pub 5385 does not include Form SSA-1099, so the IRS publishes no
total for non-filing beneficiaries; that comes from SSA.

### Estate tax and wealth

Estate tax returns by filing year (2024 out July 2026), by year of death
(2022 due November 2026), and linked estate–Form 1040 tables (listed on
https://www.irs.gov/statistics/soi-tax-stats-estate-tax-statistics,
verified, contents not opened). Personal wealth estimates by the estate
multiplier method for 2019, 2016, 2013 and earlier, by net worth, sex, age
and state, Excel
(https://www.irs.gov/statistics/soi-tax-stats-personal-wealth-statistics,
verified). Covers only people above the estate filing threshold. Gift tax
2024 due February 2027.

### Non-filers

- **Pub 1304 and the filing season statistics exclude non-filers by
  definition.**
- **Publication 5785**, the non-filing tax gap for 2014–2016, with person-
  level counts of potential non-filers by income type from information
  returns (https://www.irs.gov/pub/irs-pdf/p5785.pdf; transcribed in
  `research/state_weights/nonfiler_residual/resources/`).
- **Tax gap estimates** for tax year 2023 due 6 November 2026, including the
  non-filing component (calendar; not opened).
- **Pub 5385** includes a sample of non-filers' information returns (above).
- **Synthetic supplemental file of non-filers.** Urban Institute and SOI
  built a synthetic file of people with information returns but no tax
  return
  (https://www.urban.org/research/publication/synthetic-supplemental-public-use-file-low-income-information-return-data-methodology-utility-and-privacy-implications,
  not opened); a tax year 2016 version was scheduled for 21 November 2025
  (What's New, verified) and I found no sign it is out.

### IRS Data Book

Fiscal year 2025, June 2026, Excel and PDF
(https://www.irs.gov/statistics/soi-tax-stats-irs-data-book, verified).
Returns filed by type, refunds, collections, audits, by fiscal year of
processing. I did not open the individual tables; the refundable-credit
and information-return-count tables I believe it contains are not
verified. Useful as a count cross-check, not as a distribution.

### Return filing projections

Publication 6187 (Rev. 12-2025, https://www.irs.gov/pub/irs-pdf/p6187.pdf,
verified): calendar-year (processing year) projections, 2024 actual and
2025–2032 projected, of Forms 1040 and 1040-SR (159.3 million in 2024,
163.2 million in 2027), paper and electronic, refund returns, Schedules 1,
2, 3, A, and C or F. Schedule A returns: 15.5 million (2024), 15.7 million
(2025), 20.5 million (2026), 21.1 million (2027). Companions: Pub 6292
(fiscal-year projections, spring and fall), Pub 6149 (by state), Pub 6186
(about 50 return types, by IRS campus), Pub 6961 (information and
withholding documents). None projects income amounts.

### New-law items to expect

Tax year 2025 returns carry Schedule 1-A for the new deductions for tips,
overtime, car loan interest and seniors (published by the IRS in March
2026, per search results; not opened). The tips and overtime deductions are
capped ($25,000 for tips; $12,500, or $25,000 joint, for overtime) and phase
out with income, and employer reporting was not required for 2025. So
these will be the first IRS data on tips and overtime, but censored by the
caps, phase-outs and take-up. The higher state and local tax cap from 2025
will move more returns into Table 2.1 and shrink the censoring of itemized
deductions. Neither will appear in Pub 1304 before roughly 2028; the filing
season statistics for tax year 2025 should carry Schedule 1-A totals sooner
(not verified; the tax year 2024 file has no such rows, as expected).

## Microdata access

- **Public use files.** SOI's page (updated 22 May 2026,
  https://www.irs.gov/statistics/soi-tax-stats-individual-public-use-microdata-files,
  verified) reads: "SOI is currently in the process of transitioning from
  the traditional, heavily redacted public-use files to a Synthetic Public-
  Use File ... Thus, SOI's Individual Public-Use Microdata Files for tax
  years 2012–2015 are no longer available. An alternative, Synthetic Public
  Use File, will be available in the next few months." No PUF after 2015 has
  been released. Whether the withdrawal affects existing licensees' terms of
  use I did not determine; worth checking against the 2015 license.
- **Synthetic PUF.** Built with the Urban Institute's Tax Policy Center
  (method:
  https://taxpolicycenter.org/publications/safely-expanding-research-access-administrative-tax-data-creating-synthetic-public-use,
  not opened). The What's New page lists "Nov. 21, 2025: Individuals,
  Synthetic Public Use File, tax year 2016" and the supplemental non-filer
  file (verified); the PUF page six months later still says "next few
  months." So the first synthetic file, when it arrives, is for tax year
  2016, one year past what we have, and contains no actual records.
- **Homogeneous synthetic files, 1962–2016.** Piketty, Saez and Zucman's
  synthetic micro-files, presented at an IRS research conference
  (https://www.irs.gov/pub/irs-soi/18rpsynthindtaxfiles.pdf, search
  result). The paper describes 2013–2016 as aged from earlier years, not
  observed. Copies are in
  `/nfs/roberts/project/pi_nrs36/shared/raw_data/Saez-Zucman-PUF/` (files
  `pufonline1962.dta`–`pufonline2016.dta`). Not an IRS product; covered in
  `research/rebuild/background/03_data_microdata.md`.
- **Joint Statistical Research Program.** Outside researchers work on IRS
  microdata for approved projects. The 2026 call ran 1–30 September 2026,
  with selections on 4 January 2027 and onboarding from 1 February 2027;
  the lead researcher must be a U.S. citizen eligible for the
  Intergovernmental Personnel Act mobility program
  (https://www.irs.gov/statistics/soi-tax-stats-joint-statistical-research-program,
  verified; dates from https://www.irs.gov/pub/irs-soi/26jsrpapplication.pdf
  via search summary). Output is aggregate and disclosure-reviewed; it does
  not give a team a microdata file to keep. The 2026 window has closed.
- **Other restricted routes.** Treasury's Office of Tax Analysis, the Joint
  Committee on Taxation and CBO work with the full SOI files; none is a
  route for the Budget Lab. Census research data centers hold some linked
  IRS records for approved projects (believed; not checked).

## Censoring by tax law, across sources

| Quantity | What IRS tables show after 2017 | Uncensored or less censored IRS source |
|---|---|---|
| Itemizable expenses (mortgage interest, state and local taxes, charity, medical) | Itemizers only (Table 2.1, Historic Table 2); for itemizers, state and local taxes before the cap and medical expenses before the floor | Pub 5385 Form 1098 mortgage interest, all borrowers, national total; nothing for state and local taxes or charity of non-itemizers |
| Capital losses | Net gain line after the $3,000 limit | Table 1.4A gains, losses and carryovers before the limit |
| Passive and at-risk losses | Allowed amounts | Pub 4801 Form 8582 suspended amounts |
| Social Security benefits | Filers only | None at the IRS (SSA publishes) |
| Dependents | Through 2017, exemptions (Table 2.3); after, only children and others claimed for credits (Table 3.3, Schedule 8812 in Pub 4801, Table 2.5) and "number of individuals" in Historic Table 2 | None |
| IRA contributions | Deductible contributions (Table 1.4) | IRA study (all contributions, balances) |
| Retirement deferrals | Not in AGI, not on Form 1040 | W-2 study, Pub 5385 W-2 lines |
| Tips, overtime (2025 on) | Deduction claimed, capped and phased out | W-2 reporting of these amounts is planned for later years (not verified) |
| Student loan interest, tuition | Deduction or credit claimed | Pub 5385 Forms 1098-E, 1098-T |
| Gambling | Winnings reported; losses only for itemizers | Pub 5385 Form W-2G |
| Credits | Claimed, limited by tax | EITC participation rates (Census–IRS) for take-up |

## Precision information for weighting targets

- Table 1.4CV and 1.4CI: coefficients of variation and confidence
  intervals for each Table 1.4 cell (CV files confirmed for 2010, 2012,
  2014–2023).
- IRA study: confidence intervals (2022–2023) and coefficients of
  variation (2018, 2020–2022).
- Pub 1304 "Description of the Sample": strata and sample counts, which
  allow approximate standard errors for other tables. Top strata are
  sampled with certainty, so top-income cells are near-exact apart from
  disclosure blurring.
- Filing season statistics: no sampling error, but processing differences
  from the edited sample (comparison of mid-November counts with Pub 1304
  for the same year would measure this; not done).
- Rounding and disclosure: Historic Table 2 counts rounded to 10 with silent
  merges; ZIP cells under 20 returns merged; Table 1.6 merged cells.

## What I looked for and did not find

- A Pub 1304 table for tax year 2024 (not out; not on the 2026 calendar).
- Any filing season statistics for tax year 2025 (scheduled, not posted).
- A preliminary-data table after tax year 2022 (cancelled for 2023).
- A PUF or synthetic PUF for any year after 2015 (none released).
- A standalone SOI Schedule K-1 tabulation (none found; Pub 5385 has K-1
  totals).
- Form SSA-1099 in Pub 5385 (not in the 2023 edition).
- A filer / non-filer split in Pub 5385 (not found).
- Employer-plan (401(k)) balances (no IRS source; the W-2 study has
  contributions only).
- The 400-highest-returns report after 2014 (discontinued, per search).
- Machine-readable Pub 4801 or 5385 (PDF only; the store's parser covers
  Pub 4801).
- The contents of the IRS Data Book tables, the high-income study tables,
  and the noncash charitable tables, which I did not open.
- The source of `resources/itemized_deduction_projections.csv` and
  `resources/qbi.csv`.

## Sources not used now that the rebuild should consider

In rough order of value for a multi-year fit:

1. Pub 1304 Tables 1.4, 1.4A, 1.6, 2.1, 3.3 for **every year 2016–2023**,
   not only 2017, read from the store's aligned panels rather than the
   hand-compiled interface.
2. Table 1.5 (income by age) and Tables 1.2/1.3 (by filing status): the
   only IRS joint information on income with age and with filing status.
3. Table 4.3 and Tables 4.1–4.2: top-tail targets down to the 0.001% group.
4. Filing season statistics for tax year 2024 (and 2025 when out): the
   newest year, by AGI.
5. Table 1.4CV: precision for each target.
6. W-2 study (2014, 2016–2023 by end of October): spouse wage split, wages by
   age and sex, deferrals.
7. IRA study: contributions and balances by age and AGI percentile.
8. Pub 5385 and Pub 4801: national totals for censored or unbroken-out
   items.
9. Table 2.5: EITC by children and AGI; Table 3.3 child credits by AGI.
10. Historic Table 2 and state percentiles (already used in research): state
    margins.
11. Sales of Capital Assets 2018 (November 2026) to update holding-period
    and basis inputs.

## Summary table

"Used now" means read by the Tax-Data pipeline (`src/main.R` and what it
sources); "research" means used only in `research/`.

| Source | Years available (Oct 2026) | Dimensions | Format | Used now |
|---|---|---|---|---|
| Pub 1304 Table 1.4 | 1993–2023 | AGI (19 classes) × ~70 items | xls | Yes (2017 targets; 2017–19 growth) |
| Pub 1304 Table 1.4A | 2012–2023 | AGI × capital gain items | xls | Yes (2015, 2017) |
| Pub 1304 Table 1.4CV / 1.4CI | 2010–2023 (probed 2010, 2012, 2014–23) / 2023 | AGI × items | xls | No |
| Pub 1304 Table 1.1 | 1996–2023 | AGI, cumulative | xls | No |
| Pub 1304 Table 1.2 | 1996–2023 | AGI × filing status | xls | No |
| Pub 1304 Table 1.3 | 2010–2023 (probed 2010, 2012, 2014–23) | filing status × all items | xls | No |
| Pub 1304 Table 1.5 | 2010–2023 (probed 2010, 2012, 2014–23) | age (7 bands) × all items | xls | No |
| Pub 1304 Table 1.6 | 2008–2023 | AGI × filing status × age, counts | xls | Yes (2015, 2017 cells; 2017–23 totals) |
| Pub 1304 Table 1.7 | 2012–2023 | AGI, dependent filers | xls | No |
| Pub 1304 Table 2.1 | 1993–2023 | AGI, itemizers | xls | Yes (2017) |
| Pub 1304 Table 2.3 | 1996–2017 (closed) | AGI × exemption type | xls | Yes (2017) |
| Pub 1304 Table 2.5 | 1996–2023 | AGI × number of children | xls | No |
| Pub 1304 Table 2.7 | 2014–2023 | AGI | xls | No |
| Pub 1304 Tables 3.1, 3.1A, 3.2 | 1996/2008–2023 | AGI (3.2 × filing status) | xls | No |
| Pub 1304 Table 3.3 | 1996–2023 | AGI × credits, tax | xls | Yes (2017) |
| Pub 1304 Tables 3.4, 3.5, 3.6 | 1996–2023 | marginal rate × filing status or AGI | xls | No |
| Percentile Tables 4.1, 4.2 | 2001–2023 | cumulative percentiles | xls/xlsx | No |
| Percentile Table 4.3 | 2019–2023 confirmed (page: from 2014) | top 0.001% … 50%, income by source | xls | No |
| Filing season statistics | tax years 2017–2024 | AGI (17 classes) × ~50 items, population | xls/xlsx | No |
| Preliminary data | through 2022; discontinued | AGI | xls | No |
| Pub 4801 line items | 2003–2023 | national, every line | PDF | Yes (2017 partnership/S corp lines) |
| Pub 5385 information returns | 2017–2023 | national, every line, filers and sampled non-filers | PDF | No |
| Historic Table 2 (state) | 2012–2023 | state × AGI (10) × ~165 items | csv | Research |
| State AGI percentiles | 2013–2023 | state × top-percent groups | csv | No |
| County / metro data | 2011–2023 | county × AGI | csv | No |
| ZIP code data | 2011–2022 (2023 due 22 Oct) | ZIP × AGI (6) | csv | No |
| Congressional district | 2022 (2023 due 22 Oct) | district × AGI | xlsx/csv | No |
| Migration | 2022–2023 | state/county flows | csv | No |
| High-income returns study | through 2022 | AGI and expanded income ≥ $200k, nontaxable reasons | xls/PDF | No |
| Returns with business attributes | 2015–2023 | AGI, returns with Sch. C/E/F | xls/xlsx | No |
| Nonfarm sole proprietorships | 1998–2023 (Table 4: 2017–2020) | industry (Table 4: AGI × filing status × age × industry) | xls | No |
| Partnerships, Pub 5338 | through 2023 (2024 due Dec) | industry; partner type | xlsx, PDF | No |
| Sales of Capital Assets | 1985…2015 (2018 due 24 Nov) | asset type, holding period, AGI | xls/xlsx | Yes (1985–2015) |
| IRA study | 2000–2023 (no 2003) | plan type, AGI, AGI percentile, age, sex, filing status | xls/xlsx | Research (Table 4) |
| W-2 study | 2014, 2016–2020 (2021–23 due 22 Oct) | per earner: age, sex, wage size, AGI, deferrals, plan indicator | xls/xlsx | No |
| Noncash charitable (Form 8283) | through 2023 | AGI, asset type, donee | xls | No |
| EITC statistics (EITC Central) | claims by state through 2024; participation through 2022 | state | web table | No |
| Estate tax, filing year | through filing year 2024 | gross estate size, age, sex, state | xls | No |
| Personal wealth | 2019, 2016, 2013, … | net worth, age, sex, state | xls/xlsx | No |
| Pub 5785 non-filing tax gap | 2014–2016 | income type, filing status | PDF | Research |
| Tax gap estimates | through 2022 (2023 due 6 Nov) | tax type, component | PDF | No |
| IRS Data Book | fiscal year 2025 | fiscal year totals | xlsx/PDF | No |
| Return projections, Pubs 6187/6292/6149/6186/6961 | 2024 actual, 2025–2032 projected | return type, schedule, state, campus | PDF | No |
| PUF (2012–2015) | withdrawn by SOI | microdata | — | Yes (2015, held locally) |
| Synthetic PUF (tax year 2016) | announced, not released | synthetic microdata | — | No |
| Joint Statistical Research Program | restricted, project-based | microdata on IRS systems | — | No |
| CBO Form 1040 projections (not IRS) | 2019–2036 | national items | csv | Yes |
