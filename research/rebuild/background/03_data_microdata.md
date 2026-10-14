---
title: "Record-level data sources that could supply the prior"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# Record-level data sources that could supply the prior

Background research for the Tax-Data rebuild (see `research/rebuild/BRIEF.md`).
Topic: microdata — files of individual records — that could supply the
starting joint distribution for variables and people the 2015 PUF lacks, and
for the years after 2015. IRS tables and non-IRS aggregates are covered by
other notes.

"Verified" below means I opened the page, document or file and saw the fact.
Everything else is marked as belief. The web search allowance for this session
ran out part-way through the work; the facts I could not check because of that
are listed in the section *What I looked for and could not find or verify*.

## Summary

1. **There is still no IRS microdata newer than the 2015 PUF, and the
   2012–2015 PUFs are no longer sold.** The IRS page for individual public-use
   files (last reviewed 22 May 2026) says SOI is moving to a fully synthetic
   PUF, that the 2012–2015 files "are no longer available", and that the
   synthetic file "will be available in the next few months". A May 2025
   notice on SOI's "What's new" page set 21 November 2025 as the release date
   for a tax-year 2016 synthetic PUF and a 2016 supplemental synthetic PUF
   (non-filers). I found no evidence that either was released. If one arrives
   it will contain no real returns: every record is drawn from a model fitted
   to the confidential data. It would be a second prior to compare against,
   not observed data.
2. **The 2015 PUF is heavily altered, and its demographic file covers only
   lower-income records.** Verified from the SOI booklets: 1,124 returns with
   extreme values are folded into four aggregate records; no stratum is
   sampled above 10 percent; high-sampling-rate records have wages, state and
   local income taxes and real estate taxes averaged in groups of three;
   non-itemizers' wages carry up to 10 percent random noise; all amounts are
   rounded. The demographic file (age bands, sex of primary filer, a
   three-way earnings split) covers 119,675 of the 207,692 sampled records,
   only those sampled at the 0.07 percent rate, which excludes every record
   with indexed income of $250,000 or more (1991 dollars).
3. **Every household survey under-reports capital and business income and
   misses the top.** Census's own comparison for 2007–2012 found CPS ASEC
   self-employment income at 32–44 percent of the national accounts total,
   rent and royalties at 25–30 percent, and dividends swinging between 27 and
   102 percent. Surveys can supply the shape of a variable conditional on
   income rank and demographics; they cannot supply levels at the top.
4. **The newest survey files the Budget Lab already holds** (verified on disk):
   CPS ASEC through the 2026 survey (income year 2025), SIPP through the 2025
   release (calendar 2024), CEX through 2024, SCF 2022 and 2019, ACS 1-year
   through 2024. Coming soon: the 2025 CEX on 29 October 2026 (with a
   two-month collection gap from the 2025 shutdown), the 2025 SCF "in late
   2026" (not out as of my check), and the 2025 ACS 1-year, delayed
   indefinitely by a new Commerce Department disclosure order (DAO 216-26).
5. **Several imputations are trained on older data than the Budget Lab
   holds.** Childcare expenses use the 2017 ASEC only; filer ages use the
   2015–2019 ASECs; tips use SIPP releases through 2023; the spouse wage split
   uses Saez's 2014 tabulation; sex and the state-and-local tax rate use DINA
   2017 and 2019; retirement-plan contributions and prior-year earnings are
   set to zero although the CPS carries both kinds of information.
6. **Two public projects already build what the rebuild describes, and are
   worth reading as precedents.** The Policy Simulation Library's
   tax-microdata file (TMD, version 2.2.1, 28 September 2026) ages the 2015
   PUF to 2022, adds CPS non-filers, reweights by quadratic programming to
   within ±0.5 percent of SOI targets, and imputes overtime and tips from SIPP
   2024 and auto-loan interest from CEX 2023. PolicyEngine's Enhanced CPS
   (public, 2024 base) imputes 67 PUF variables onto the CPS and calibrates to
   thousands of targets by gradient descent; its repository was archived in
   July 2026 and replaced by a project called microcosm.
7. **The SCF is the only credible wealth microdata.** 4,595 families in the
   2022 public file, five imputed copies of each record, Forbes 400 excluded by
   design. The public DINA microfiles (Piketty–Saez–Zucman, last updated
   February 2022; the Budget Lab copy runs 1962–2019) give capitalized wealth
   on tax-like records with a tax-unit identifier, and are the only public file
   joining tax-return income to wealth and to all taxes paid.
8. **For non-filers, the CPS ASEC remains the practical base;** the ACS is
   about 25 times larger and covers group quarters. The 2012 synthetic
   supplemental PUF (Urban–SOI) is the only public file built from IRS
   information returns for non-filers; a 2016 version is promised.
9. **Restricted routes give statistics, not microdata.** The IRS Joint
   Statistical Research Program's 2026 window closed 30 September 2026
   (selections 4 January 2027); work is on IRS equipment and only reviewed
   statistics leave. Census holds Form 1040 e-file data for 2011–2022 and
   W-2, 1099-R and 1099 information returns (August 2026 inventory), usable in
   research data centers only with IRS approval. The realistic payoff of
   either route is custom tabulations to use as targets.
10. **A finer post-2015 target set is already on disk.** The Saez–Zucman
    "agetable" files for 2013–2016 are tabulations of internal SOI data by 12
    demographic groups (married × itemizer × age and children) and 16 AGI
    groups up to the top 0.01 percent, with means of 47 variables per cell.
    They sit in `Saez-Zucman-PUF/v1/2023082914/historical/` in the shared raw
    data store and are not used by the pipeline.

## 1. What the repository uses now

Read from the code on 2026-10-06. "Newer on disk" means a newer file of the
same source is in `/nfs/roberts/project/pi_nrs36/shared/raw_data/`.

| Output variables | Module | Training source and year | Newer available |
|---|---|---|---|
| Base records, most income and deduction amounts | `process_puf.R` | 2015 PUF and its demographic file | None from IRS |
| Filer ages, spouse age gap | `imputations/ages.R` | CPS ASEC 2015–2019 surveys (IPUMS extract `cps_00027`, vintage 2024110517; years counted on disk) | ASEC 2020–2026 on disk |
| Sex of primary and secondary filer | `imputations/demographics.R` | DINA 2017 (male share among unmarried units) | Non-filer pool now carries observed sex from the ASEC |
| Blindness | `imputations/demographics.R` | One national share from SOI counts of blind standard deductions | ACS has a vision-difficulty question |
| Dependents' SSN status | `imputations/ssn.R` | One national share from ITEP (1.1 million of 72 million children) | ACS and CPS carry citizenship |
| Spouse wage split | `imputations/earnings_split.R` | Saez SOI tabulation `wagesplit.csv`, year 2014 | CPS ASEC person-level earnings to 2025 |
| Overtime | `imputations/overtime.R` | `resources/otdata2023.dta`, built September 2024; variable names (weekly earnings, overtime weekly earnings, FLSA eligibility, weeks worked) suggest CPS earnings data for 2023. Provenance is not documented in the repository. | CPS 2024–2025 |
| Tips | `imputations/tips.R` | SIPP releases 2018–2023 (income years 2017–2022), file `tip_ind_occ_full_split.csv`, interface vintage 2024090311 | SIPP 2024 and 2025 releases (calendar 2023, 2024) in `SIPP/v2/20260927` |
| Childcare expenses | `imputations/childcare.R` | CPS ASEC survey year 2017 only (income year 2016), SPM childcare expense | ASEC 2018–2026 |
| Auto-loan interest | `imputations/auto_loan.R` | SCF 2022 plus `scf2022_carinterest.dta` | SCF 2025 pending |
| Mortgage interest split | `imputations/mortgage.R` | SCF 2022 | SCF 2025 pending |
| Wealth, basis, accruals | `imputations/wealth.R`, `pool_scf_donors.R` | SCF 2022 tax units, with SCF 2019 pooled for donors | SCF 2025 pending |
| Consumption | `cex.R`, `imputations/consumption.R` | CEX interview survey 2022–2024 | CEX 2025 due 29 October 2026 |
| State and local tax rate | `imputations/state_local_tax.R` | DINA 2019 | None newer in the public DINA files I could find |
| Capital gains holding period and basis | `imputations/capital_gains.R` | SOI sales-of-capital-assets tables (not microdata) | Other notes |
| Parent income rank | `imputations/mobility.R` | Chetty et al. mobility matrix (not microdata) | PSID is the microdata alternative |
| Non-filers | `impute_nonfilers.R` | ASEC-built non-filer pool, tax years 2017–2023 | ASEC 2025 and 2026 (income years 2024, 2025) |
| Prior-year earnings, employer-plan contributions, divorce year | `imputations/placeholders.R` | Set to zero or a flag | CPS ASEC two-year panel; CPS retirement contributions; ACS year of last marriage |

The shared store also holds files the pipeline does not read: the SSA 2006
Earnings Public-Use File, the Saez–Zucman synthetic tax files 1962–2016 with
the 2013–2016 age tables, DINA microfiles 1962–2019, PSID test extracts, the
CPS ASEC two-year panel links, the National Survey of Early Care and Education
(directory `NSECE`), and SOI W-2 and IRA tables.

## 2. IRS microdata

### The 2015 Public Use File

Verified from the SOI general description booklets in
`IRS-PUF/v1/2023091310/historical/`.

- **Unit and size.** Tax return. 207,696 records, of which four are aggregate
  records (RECID 999996–999999) holding 1,124 returns with extreme values
  (generally within the top 30 amounts of any income item or the bottom 30 of
  any loss). Represents 151.2 million returns. National estimates only;
  the file's 222 columns include no state field (checked on disk).
- **Disclosure protections, in order.** Fiscal-year returns converted;
  extreme-value returns aggregated; high-income no-tax returns returned to
  their ordinary strata and subsampled; every stratum sampled above 10 percent
  subsampled to 10 percent; for records sampled above 0.07 percent, alimony
  and the state sales-tax deduction removed, marital status modified, and
  wages, state and local income taxes and real estate taxes replaced by the
  average of groups of three similar records ("multivariate blurring"); for
  records sampled at 0.07 percent, alimony blurred, itemizers blurred the same
  way, and non-itemizers' wages given up to 10 percent uniform noise
  (constrained not to change EITC or saver's credit eligibility); surviving
  spouses recoded as joint filers; dependents capped at 3 (joint, head of
  household), 2 (single), 1 (separate); amounts rounded (four significant
  digits above $100,000, nearest $100 from $10,000 to $100,000, nearest $10
  below); then "rebalanced" for internal consistency.
- **What it measures well.** Every 1040 line and many schedule lines, for
  filers, including the top of the distribution apart from the aggregate
  records. It is the only public source with credible levels for business,
  partnership, S corporation, capital gain and dividend income at the top.
- **What it lacks.** Non-filers; ages and sex for most of the high-income
  sample; anything not on the return (wealth, consumption, tips, overtime,
  employer-plan contributions); state; itemized-deduction detail for
  non-itemizers.
- **Access.** Sold by SOI until withdrawn; see the next subsection.
- **URL.** https://www.irs.gov/statistics/soi-tax-stats-individual-public-use-microdata-files

### The 2015 demographic file

Verified from its booklet (September 2022). 119,675 records, joined to the
PUF one-to-one on RECID. Only records in strata 7–13, the returns sampled at
0.07 percent; the booklet's Table A shows these are the records with indexed
positive income below $250,000 or indexed negative income above −$250,000
(indexed to 1991 dollars by a GDP price factor of 1.5874). Variables: age band
of the primary filer (six bands for non-dependents, three for dependent
filers), sex of the primary filer, ages of up to three dependents in seven
bands, and for joint returns a three-way split of earnings (wages plus
self-employment earnings) between spouses at 25 and 75 percent. Dependent ages
are suppressed on returns with farm income or a first-time homebuyer credit,
and wherever a combination of characteristics has 100 or fewer returns in the
population. The booklet's comparison with the full SOI sample shows the public
file understating dependents aged 19–23 and 24-plus by 33–58 percent in the
second and third dependent slots.

Consequence for the rebuild: ages, sex and the earnings split for high-income
records must come from a model, and the training data for that model (the
demographic file) contains no high-income records.

### Withdrawal of the 2012–2015 files and the synthetic PUF

Verified: the IRS page above, last reviewed 22 May 2026, reads: "the
Statistics of Income (SOI) Division is currently in the process of
transitioning from the traditional, heavily redacted public-use files to a
Synthetic Public-Use File (PUF) … Thus, SOI's Individual Public-Use Microdata
Files for tax years 2012–2015 are no longer available. An alternative,
Synthetic Public Use File, will be available in the next few months".

Verified: SOI's "What's new" page carries a "Notice of revised release date"
dated 29 May 2025 that lists "Nov. 21, 2025: Individuals, Synthetic Public
Use File, tax year 2016" and "Nov. 21, 2025: Individuals, Supplemental
Synthetic Public Use File, tax year 2016". I read the page's release list
for 2026 (through October) and did not find either file announced as
released. Given that the May 2026 page still says "in the next few months", I
believe neither has been released, but I could not confirm this directly.
https://www.irs.gov/statistics/soi-tax-stats-whats-new

Background on the method (verified from the 2020 Urban–TPC paper on the
supplemental file): a fully synthetic file is generated record by record from
a model fitted to the confidential data — here a sequence of classification
and regression trees (CART, a method that splits the data into groups by
repeated yes/no questions and draws values from within a group), with values
smoothed by drawing from a normal distribution around each donor value. No
synthetic record corresponds to a real return, and values in one synthetic
row come from many different real rows. That last property is the important
caution: joint relationships the model was not told to preserve may be
weakened.

Status of the Budget Lab's own 2015 PUF license under the withdrawal: not
something I could check. Worth confirming the terms before the rebuild
depends on the file for years.

### The synthetic supplemental PUF for 2012 (non-filers)

Verified from Bowen, Bryant, Burman et al. (2020, revised November 2020),
https://www.urban.org/sites/default/files/publication/102547/a-synthetic-supplemental-public-use-file-of-low-income-information-return-data_3.pdf.
Source: a 0.1 percent random sample of IRS information returns for 2012
(W-2, SSA-1099, 1099-INT and others), restricted to non-dependent people who
filed no return and whose income was below twice the joint filing threshold;
about 26,000 administrative observations after cleaning. Variables: sex, age
(top-coded at 85) and 17 information-return amounts. Unit: individual, not tax
unit. I did not find how the file is distributed or whether it is still
available.

### The Saez–Zucman synthetic tax files and age tables

Verified from Saez and Zucman, "Creating Homogeneous Synthetic Individual Tax
Files for Distributional Analysis" (August 2018),
https://www.irs.gov/pub/irs-soi/18rpsynthindtaxfiles.pdf, and from the files
on disk. Annual files for 1962, 1964 and 1966–2016, 11,000–37,000 records
each, 47 homogeneous variables (wages, pensions, dividends, interest, rents,
business income by type, capital gains, Social Security, unemployment
insurance, itemized deduction components, federal tax, EITC, CTC). Records are
averages of groups of five similar returns, with extreme returns aggregated,
so the files are public. The 2013–2016 files are the 2012 PUF reweighted and
rescaled to internal SOI tabulations. Those tabulations (`agetable2013.dta`
to `agetable2016.dta`) give, for 12 groups (married × itemizer × age 65+ or
under 65 with or without children) by 16 AGI groups (negative, nine deciles,
P90–95, P95–99, P99–99.5, P99.5–99.9, P99.9–99.99, top 0.01 percent), the
number of returns and the mean of every variable. For 2016 this is the
finest public cross-tabulation of internal data by demographic group and
top-income group I know of. As microdata the files are weaker than the PUF
(averaging by five destroys within-group variation) and are not a better
prior; the 2016 age table is a target.

## 3. Household surveys

Format notes: CPS, ACS and many others are most easily read through IPUMS
(https://www.ipums.org), which the Budget Lab already uses.

### CPS Annual Social and Economic Supplement (CPS ASEC)

- **Unit.** Person, household, family; Census also builds tax units for its
  tax model and IPUMS publishes the simulated tax variables (`FILESTAT`,
  `ADJGINC`, `FEDTAX`, `EITCRED`, `STATETAX`, `MARGTAX`, `TAXINC`; verified in
  the Budget Lab extract). The repository's own tax-unit builder
  (`src/nonfilers/asec_tax_units.R`) already forms units rule by rule.
- **Years.** Annual; income refers to the prior calendar year. The 2026
  survey (income year 2025) was released 15 September 2026 (verified, Census
  P60-289 and media advisory) and is on disk.
- **Sample size.** Person records in the Budget Lab's IPUMS extracts
  (verified by counting): 185,487 (2016 survey), 144,265 (2024), 142,125
  (2025), 134,729 (2026). The sample is shrinking, by about 27 percent since
  2016.
- **Measures well.** Household structure, relationships, ages, sex, marital
  status, citizenship, school enrollment, wages for most of the distribution,
  Social Security (about 90 percent of the national accounts total, per
  Rothbaum 2015), work and weeks, health coverage, childcare and
  out-of-pocket medical spending (from the Supplemental Poverty Measure
  module), state of residence. Since the 2014 redesign it asks about
  retirement-account interest and capital gains from stocks and mutual funds
  (`INCCAPG`); Census's public file also carries retirement-plan
  contributions (`RETCB_VAL`, used by TMD; not in the Budget Lab's IPUMS
  extract).
- **Measures badly.** Verified from Rothbaum, "Comparing Income Aggregates"
  (Census working paper SEHSD-WP2015-01), for 2007–2012 against the national
  accounts: total self-employment income fell from 44.0 to 31.7 percent of
  the benchmark; interest 53.7–74.9 percent; dividends 27.2–101.6 percent;
  rent and royalties 25.1–29.9 percent; unemployment compensation 65–80
  percent. Pension and retirement income is also low (Bee and Mitchell, Census
  working paper SEHSD-WP2017-39, believed to show retirement income roughly
  doubling when replaced by IRS 1099-R records; I did not reopen the paper).
  High values are protected by rank proximity swapping since 2011: values
  above a topcode are swapped among people of nearby rank (verified, IPUMS
  topcode documentation). No direct measure of itemized deductions: Census's
  tax model fills them by statistical match to the PUF (verified, Lin 2022,
  SEHSD-WP2022-18 — "statistical match" meaning each CPS unit receives the
  values of a similar PUF record). The 2020 tax model used the 2014 PUF. So
  any CPS itemized-deduction variable is PUF information, not new
  information.
- **Access.** Public; free. Census CSV and IPUMS.
- **URLs.** https://www.census.gov/programs-surveys/cps.html ;
  https://cps.ipums.org ;
  tax model: https://www.census.gov/content/dam/Census/library/working-papers/2022/demo/sehsd-wp2022-18.pdf

The CPS also has a two-year panel element (households in sample in
consecutive Marches, linked by IPUMS `CPSIDV`; on disk as `CPS-ASEC-Panel`),
which is the public source for prior-year earnings. The monthly CPS
outgoing-rotation earnings questions (usual weekly earnings, and whether
overtime, tips or commissions are received) are the source for the overtime
imputation.

### American Community Survey (ACS)

- **Unit.** Person and household; group quarters residents included. Tax
  units must be built (the non-filer pool already uses the ACS for group
  quarters).
- **Years.** Annual. 1-year files through 2024 on disk. The 2025 1-year
  release is delayed: Census says the date "is being determined" while it
  assesses Commerce Department Administrative Order 216-26 (effective 4 June
  2026), which bars noise infusion and prefers coarsening for all Census
  products (verified, Census media advisory of 4 August 2026 and the
  Director's blog of August 2026). Whether the order will change topcoding
  in future CPS or SIPP public files I could not determine.
- **Sample size.** 3,422,888 person records in the 2024 1-year IPUMS extract
  (verified by counting).
- **Measures well.** State and sub-state geography, property taxes, home
  value, mortgage payments and whether taxes are included, rent, utilities,
  vehicles, health coverage, disability (including vision), citizenship,
  year of last marriage (believed; the Census PUMS variable `MARHYP`).
- **Measures badly.** Income in eight broad categories only (interest,
  dividends and rent are one item), asked for the past 12 months rather than a
  calendar year, and worse than the CPS for wages and transfers (Rothbaum
  2015). No capital gains, no deductions.
- **Access.** Public PUMS; IPUMS.
- **URL.** https://www.census.gov/programs-surveys/acs/microdata.html

### Survey of Income and Program Participation (SIPP)

- **Unit.** Person-month records within households; panel with overlapping
  cohorts. Tax units must be built.
- **Years.** Annual releases since the 2018 redesign. The 2025 release
  (interviews in 2025 about calendar 2024) was published 14 July 2026
  (verified, Census 2025 SIPP page; the reference-year reading is from the
  Budget Lab store's notes). The 2024 and 2025 releases are on disk in
  `SIPP/v2/20260927`. Census notes "a general lower-than-average unit
  response rate" for 2025.
- **Sample size.** About 20,000 interviewed housing units a year under the
  overlapping-panel design (from a Census source I did not open directly;
  believed).
- **Measures well.** Monthly program participation, job-level earnings
  including tips (the repository's tips source), assets and debts in a
  wealth module, health insurance, child care, disability.
- **Measures badly.** Top incomes and wealth (no high-income oversample),
  capital income.
- **Access.** Public. Pipe-delimited files.
- **URL.** https://www.census.gov/programs-surveys/sipp/data/datasets.html

### Survey of Consumer Finances (SCF)

- **Unit.** The "primary economic unit" (the economically dominant person or
  couple and those financially dependent on them). Tax units can be formed;
  the repository does so in `stage1_scf_tax_units.R` following Moore's
  SCF-to-TAXSIM code, producing 29,290 tax-unit rows over five imputed
  copies.
- **Years.** Triennial. 2022 is the latest released (verified). The 2025
  survey began in March 2025 with about 13,000 households contacted, and
  results are promised for "late 2026" (Fed press release of 28 February
  2025; I did not find them released as of this note). The Budget Lab store
  has 2009, 2013, 2016, 2019 and 2022.
- **Sample size.** 2022: 4,602 families interviewed; the public file has
  22,975 records representing 4,595 families after seven were removed for
  disclosure, each in five copies because missing answers were imputed five
  times ("multiple imputation"; verified, SCF index page).
- **Measures well.** Every asset and debt category, home value and mortgage
  terms (balance, rate, year taken out), business equity, unrealized capital
  gains by asset type, retirement accounts, prior-year income by type
  including realized capital gains. A list sample drawn from SOI records
  oversamples wealthy families, so the top is far better covered than in any
  other survey.
- **Measures badly.** The Forbes 400 are excluded by design; the very top is
  thin (the repository's own lumpiness notes); income questions are asked
  about the prior calendar year; sample is small for cross-classified cells.
- **Access.** Public; free.
- **URL.** https://www.federalreserve.gov/econres/scfindex.htm

### Consumer Expenditure Survey (CEX)

- **Unit.** Consumer unit (roughly a household or financially independent
  group within one). Member file allows rough tax units, as `cex.R` does.
- **Years.** Annual public microdata; 2022–2024 on disk. 2025 public
  microdata will be published 29 October 2026 (verified, BLS shutdown
  notice). Because of the 1 October–12 November 2025 funding lapse, the
  interview survey lost about a third of reported expenditures for July and
  October and two-thirds for August and September, and the diary survey lost
  two months entirely; BLS will not adjust the public-file weights for this
  but will publish guidance.
- **Sample size.** About 4,600 consumer units per quarterly interview file in
  2024 (verified: 4,673, 4,612 and 4,601 rows in the 2024 Q2–Q4 files).
- **Measures well.** Spending by detailed category; also, relevant here,
  mortgage interest (`MRTINTCQ`), property taxes (`PROPTXCQ`), vehicle
  finance charges (`VEHFINCQ`), cash charitable contributions (`CASHCOCQ`),
  each for itemizers and non-itemizers alike (variable names verified in the
  2024 file).
- **Measures badly.** Income (the repository's notes record CEX income 22–48
  percent below PUF income at the same rank), the top (thin and attenuated),
  wealth.
- **Access.** Public. https://www.bls.gov/cex/pumd.htm

### Panel Study of Income Dynamics (PSID)

Not verified (the PSID site refused my requests). Believed: a family panel
since 1968, biennial since 1997, roughly 9,000–10,000 families per wave, with
income by source, wealth in every wave, broad consumption since 1999, and
charitable giving for all families (the Philanthropy Panel Study module). Its
value here is unique: parent–child links over decades (the microdata version
of `parent_rank`), and giving and medical spending measured for
non-itemizers. Weak at the top. Public files free with registration; the
Budget Lab store has test extracts. https://psidonline.isr.umich.edu

### Other surveys

Not verified in this session; listed for completeness with my understanding.

- **Medical Expenditure Panel Survey (MEPS), Household Component.** Person and
  household, annual, roughly 25,000–30,000 persons. Out-of-pocket medical
  spending and premiums by person, health savings accounts. The best source
  for the shape of medical expenses among non-itemizers. Public.
  https://meps.ahrq.gov
- **Health and Retirement Study (HRS).** People over 50 and spouses,
  biennial. Wealth, pensions, retirement income; linked SSA earnings and
  benefits are restricted. Useful for the joint distribution of retirement
  income and assets among older filers. https://hrs.isr.umich.edu
- **National Longitudinal Surveys of Youth (NLSY79, NLSY97).** Single
  birth cohorts; not representative of the population in any year. Low value
  for a cross-sectional prior.
- **National Health Interview Survey (NHIS).** Health and disability; no tax
  variables of note. Low value beyond disability shares.
- **National Survey of Early Care and Education (NSECE).** Household survey of
  child care arrangements and payments for families with young children; in
  the Budget Lab store. A possible cross-check for childcare expenses.

## 4. Public-use files built from administrative records

- **SSA Earnings Public-Use File, 2006.** Verified from the dictionary on disk
  (`SSA-EPUF/v1/2006/`). A 1 percent sample of Social Security numbers issued
  before 2007; about 4.3 million people, annual earnings 1951–2006, year of
  birth and sex. Earnings are Social Security covered earnings, top-coded at
  the taxable maximum and randomly rounded. No household or tax unit. Useful
  for earnings dynamics below the taxable maximum only, and dated. SSA's site
  refused my requests, so I could not check for a newer file; I believe 2006
  is still the latest. https://www.ssa.gov/policy/docs/microdata/epuf/
- **SIPP Synthetic Beta.** Verified (Census page): version 7.0, December
  2018; nine SIPP panels 1984–2008 linked to SSA and IRS earnings records
  (W-2 earnings through 2014) and benefits; partly synthetic; available by
  application, with results validated on the confidential "Gold Standard"
  file. Census's own description says there are no production plans for a
  new release (from the non-technical v7 description, seen in search results
  but not opened). Too old for a 2015+ prior.
  https://www.census.gov/programs-surveys/sipp/guidance/sipp-synthetic-beta-data-product.html
- **CMS public files.** The Medicare claims synthetic public-use files cover
  2008–2010 and carry no income. Not useful here.

## 5. Research files combining tax and survey data

### Piketty–Saez–Zucman DINA microfiles

Verified from the codebook (February 2022),
http://gabriel-zucman.eu/files/PSZCodebook.pdf, and the files on disk. One
file per year from 1962 (except 1963, 1965); the Budget Lab copy (interface
vintage 2023082913) runs to 2019. Unit: adult aged 20+ with a tax-unit
identifier (`id`), weights summing to the adult population; non-filers
included and flagged (`filer`). Built from the Saez–Zucman grouped-by-five
synthetic tax files, so within-group variation is averaged away. Variables:
fiscal income by type (wages and pensions, business by form, rents,
interest, dividends, capital gains), national-accounts income concepts,
wealth by asset class (capitalized from income flows), every tax (federal
income, state income, sales and excise, property, corporate, estate, payroll)
and transfers including Medicaid and SNAP. Age in three bands only. Use as a
prior: weak for income detail; distinctive for joint income–wealth–tax
structure and for non-filers' income. The page lists the February 2022 update
as current; I found no newer public microfiles. https://gabriel-zucman.eu/usdina/

### Realtime Inequality

Blanchet, Saez and Zucman. The replication package "constructs the synthetic
microfiles" behind the monthly series from DINA, the CPS, QCEW wage data, the
national accounts, the SCF and the Financial Accounts (verified, README).
I could not find a direct download of the microfiles themselves.
https://github.com/thomasblanchet/real-time-inequality ;
https://realtimeinequality.org

### PolicyEngine Enhanced CPS, and its successor

Verified from the documentation site and Hugging Face listing.

- **Construction.** Base: CPS ASEC 2024 (income year 2023), aged. The aged
  CPS is copied twice: one copy keeps CPS incomes and gains PUF-only
  variables (mortgage interest, charitable giving, state and local taxes,
  medical expenses, capital gains, dividends, credits); the second has its
  income variables replaced by values imputed from the 2015 PUF. Imputation
  is by quantile regression forests (a method that predicts the whole
  conditional distribution of a variable and draws from it, rather than a
  single average) using seven demographic predictors common to both files.
  Further imputations from SIPP 2023 (tips, vehicles, liquid assets), SCF
  2022 (net worth components, auto loans) and ACS 2022 (rent, real estate
  taxes). Weights are then calibrated to IRS, Census, CBO, JCT and state
  targets by gradient descent on squared relative error with a penalty that
  pushes many weights to zero; the paper cites 2,813 targets, the current
  methodology page about 13,000.
- **Access.** `enhanced_cps_2024.h5` is public on Hugging Face
  (https://huggingface.co/policyengine/policyengine-us-data).
- **Status.** The GitHub repository `policyengine-us-data` was archived on
  2 July 2026. Its successor, `PolicyEngine/microcosm` (created June 2026,
  active), is a general library for weighted "frames", imputation and
  calibration. Its issue #982 reports that in the current build the
  PUF-imputed half "reproduces ASEC income": because each income item was
  predicted from the recipient's own survey value of the same item, imputed
  wages never exceed the CPS maximum of $2,099,999. This is a concrete
  example of how a survey-based prior can carry survey top-coding into a
  tax file.
- **URLs.** https://policyengine.github.io/policyengine-us-data/ ;
  https://github.com/PolicyEngine/microcosm/issues/982

### Policy Simulation Library tax-microdata (TMD) and taxdata

Verified from the GitHub repository and the Tax-Calculator documentation.

- **TMD.** Version 2.2.1, released 28 September 2026; start year 2022 since
  Tax-Calculator 6.5.0. Requires the user's own licensed `puf_2015.csv` and
  `demographics_2015.csv`. Steps (from `tmd/datasets/tmd.py` and
  `tmd/utils/reweight.py`): uprate the 2015 PUF to the tax year; build CPS
  records from the public Census ASEC CSV (2023 file for tax year 2022) and
  keep only non-filers (income below a threshold, $9,990 for 2022, and no
  positive simulated income tax); append; reweight by quadratic programming —
  minimize the sum of squared changes in weight ratios, subject to each SOI
  target being hit within ±0.5 percent and each ratio lying in [0.1, 10],
  with penalized slack for infeasible targets. Targets are SOI counts and
  amounts by AGI bin and filing status. Pension contributions are imputed
  from the CPS `RETCB_VAL`; overtime and tips from SIPP 2024 and auto-loan
  interest from CEX 2023 by chained-equation imputation. Also produces state
  and congressional-district weight files.
- **taxdata.** The older PSL repository; its `cps.csv` is public CPS data
  reweighted to SOI targets and is shipped with Tax-Calculator; its PUF file
  is based on the 2011 PUF.
- **URLs.** https://github.com/PSLmodels/tax-microdata-benchmarking ;
  https://taxcalc.pslmodels.org/usage/data/

### Census CPS ASEC tax model and the NEWS project

- **Tax model.** Described above. Filing units are constructed within
  households; PUF items are statistically matched; the internal version uses
  higher topcodes than the public version (verified, Lin 2022).
- **National Experimental Wellbeing Statistics (NEWS).** Census links the CPS
  ASEC to administrative records including Form 1040 data to correct survey
  income. Latest release 14 July 2025, covering 2016–2021 (verified, Census
  NEWS page). Only tables are published; the linked microdata stay inside
  Census. Relevant as a source of corrected-income tabulations, not as a
  prior. The Budget Lab store holds the published workbooks in `Census-NEWS`.
  https://www.census.gov/data/experimental-data-products/national-experimental-wellbeing-statistics.html

### Urban–Brookings Tax Policy Center model

I could not open TPC's model description (requests refused). I believe the
model starts from the PUF statistically matched to the CPS and aged with
SOI and CBO data, but did not verify which PUF year it now uses.

## 6. Restricted-access routes

- **IRS Joint Statistical Research Program (JSRP).** Verified from the 2026
  call (https://www.irs.gov/pub/irs-soi/26jsrpapplication.pdf). The 2025
  window was postponed; the 2026 window ran 1–30 September 2026, selections
  4 January 2027, onboarding 1 February 2027. Lead researcher must be a U.S.
  citizen eligible for an Intergovernmental Personnel Act assignment
  (unpaid IRS employee status, background check). Data are custom extracts on
  IRS equipment only. Output: statistics reviewed for disclosure, published
  as SOI working papers. Selection weighs a tax-administration purpose,
  demonstrated need, and SOI staff capacity. "Producing new products for
  public dissemination" and tabulations are listed as acceptable purposes.
  For the rebuild, a JSRP project could produce fine cross-tabulations (in
  the style of the Saez–Zucman age tables, or joint distributions of
  deductions among non-itemizers from information returns) for use as
  targets, starting no earlier than 2027.
- **Federal Statistical Research Data Centers (FSRDC).** Verified from
  Census's administrative data inventory dated August 2026
  (https://www2.census.gov/about/linkage/data-file-inventory.pdf): Census
  holds Form 1040 e-file data for tax years 2011–2022, Form 1040 returns back
  to 1969 at intervals, W-2 forms, 1099-R and other 1099 information returns
  (some through tax year 2025), and SSA's Master Beneficiary Record through
  2025. The inventory notes these are for "approved internal projects for
  statistical purposes only" and that not all are available in research data
  centers; IRS data require IRS approval of the project (my understanding of
  the Title 26 rules, not checked against a current IRS document). Output is
  disclosure-reviewed statistics. Linked CPS–IRS work is possible here but
  slow to approve.
- **National Secure Data Service demonstration.** The National Science
  Foundation's demonstration (NORC, Coleridge Initiative and others) runs to
  August 2027 (search result summary of the NSF page; not opened). I found
  nothing indicating it offers IRS individual data.

## 7. Best record-level sources by variable group

Groups follow `config/variable_guide/baseline.csv`. "Prior" means the source
of the starting conditional distribution; levels at the top come from the PUF
and from targets in every case below.

| Variable group | Best source(s) | Notes |
|---|---|---|
| Filing status, dependents, dependent ages, CTC and EITC child counts (`filing_status`, `n_dep*`, `dep_age*`, `dep_ctc*`) | PUF (counts, status); PUF demographic file (age bands, below $250k indexed); CPS ASEC household rosters | PUF caps dependents at 3; the CPS has every child's exact age and relationship. ACS is the larger alternative. |
| Filer ages and sex (`age1`, `age2`, `male1`, `male2`) | PUF demographic file; CPS ASEC | No public microdata gives ages for high-income filers; the age split at the top must come from SOI tables or the SCF (which records ages and has high-income families). |
| Blindness, SSN status of dependents (`blind*`, `dep_ssn*`) | ACS (vision difficulty; citizenship of children and parents) | CPS also has citizenship. SSN status is not asked anywhere; citizenship and nativity are proxies only. |
| Wages and the spouse split (`wages`, `wages1`, `wages2`) | PUF (totals); PUF demographic EARNSPLIT (three bins); CPS ASEC (person earnings within couples) | The CPS split is reliable below the topcode; above it, the PUF earnings split bins do not exist. SOI W-2 tables are the other route. |
| Tips, overtime (`tips*`, `tips_lh*`, `ot*`) | SIPP 2024–2025 releases (tips); monthly CPS outgoing-rotation earnings (overtime, tips, commissions) | TMD uses SIPP 2024 for both. No IRS microdata until tax year 2025 returns with the new deductions exist. |
| Prior-year earnings (`ei_prior_yr`) | CPS ASEC two-year panel; SIPP longitudinal weights | Currently zero. SSA EPUF 2006 has long histories below the taxable maximum but is dated. |
| Employer-plan contributions (`trad_contr_er*`) | CPS ASEC `RETCB_VAL`; SCF | Currently zero. W-2 box 12 tables from SOI are the aggregate check (other note). |
| Interest, dividends (`txbl_int`, `exempt_int`, `div_*`) | PUF | Surveys capture roughly half or less; SCF holdings give the asset side. |
| Capital gains, holding period, basis (`kg_*`, `other_gains`) | PUF (amounts); SCF (unrealized gains by asset) | CPS `INCCAPG` exists since 2014 but is a survey answer with heavy under-reporting (not quantified here). |
| Business income by form, QBI items (`sole_prop*`, `part_*`, `scorp_*`, `farm*`, `wagebill_*`, `sstb_*`) | PUF | Surveys capture about a third of self-employment income. Spouse split: the PUF's self-employment earnings fields `E30400` and `E30500` (the demographic booklet uses them for the earnings split); CPS person-level self-employment earnings. SSTB status and wage bills have no public microdata; industry from the CPS or ACS is the closest proxy. |
| Pensions, IRA distributions, Social Security, UI (`*_pens_dist`, `txbl_ira_dist`, `gross_ss`, `ui`) | PUF | CPS Social Security is about 90 percent of the benchmark and could supply non-filers; retirement income is badly under-reported in the CPS. |
| Above-the-line deductions (`trad_contr_ira`, `hsa_contr`, `sl_int_ded`, `ed_exp`, `se_health`, `keogh_contr`, `alimony*`, `tuition_ded`) | PUF | SCF (education loans, IRAs) and MEPS (HSAs) for structure among non-filers. |
| Itemizable expenses (`med_exp`, `salt_*`, `int_exp`, `char_*`, `casualty_exp`, `job_exp`, misc.) | PUF for 2015 itemizers; for non-itemizers and post-2017: CEX (mortgage interest, property tax, cash gifts), ACS (property tax, mortgage payment), MEPS and CPS (medical out-of-pocket), PSID (giving) | The 2015 PUF already shows only itemizers' amounts. The underlying distribution of deductible spending among non-itemizers must come from surveys. Do not use CPS itemized-deduction variables: they are PUF statistical-match values. |
| Mortgage detail (`first_mort_*`, `second_mort_*`, `prim_mort_share`, `inv_int_exp`) | SCF | Balances, rates and origination years are only in the SCF; ACS for the count of owners with a mortgage. |
| Auto-loan interest (`auto_int_exp`) | SCF; CEX vehicle finance charges | TMD uses CEX 2023; the repository uses SCF 2022. |
| Childcare and education credit expenses (`care_exp`, `aoc_exp`, `llc_exp`) | PUF (claimants); CPS ASEC SPM childcare expenses 2018–2026; NSECE; CPS school enrollment | Repository trains on the 2017 ASEC only. I found no public microdata on tuition paid by family income beyond the CEX education categories. |
| Other credits and AMT items (`ftc`, `gbc`, `prior_yr_cred`, `res_energy_cred`, `net_ptc`, `excess_ptc`, `amt_*`) | PUF | No survey measures these. ACS and CPS marketplace coverage can inform premium tax credit eligibility among non-filers. |
| Consumption (`c_*`) | CEX 2022–2024, 2025 from 29 October 2026 | PSID has broader but coarser consumption; the 2025 CEX has the shutdown gap. |
| Wealth, basis, accruals (`value.*`, `basis.*`, `accruals.*`) | SCF 2022 (2025 pending), pooled with 2019 | DINA microfiles for income-capitalized wealth on tax-like records; Forbes list for the very top. PSID and SIPP are weak at the top. |
| State and local tax rate (`dina_other_taxes_rate`) | DINA 2019 | ACS property tax and CEX tax items are the survey alternatives; the CPS tax model's simulated state income tax is another. |
| Parent income rank, divorce year (`parent_rank`, `divorce_year`) | PSID (parent–child links); ACS year of last marriage and marital events | Both are currently filled without microdata. |
| Non-filers (all variables) | CPS ASEC 2018–2026; ACS for group quarters and size | Synthetic supplemental PUF (2012; 2016 promised) for information-return income of non-filers; DINA `filer == 0` records as a cross-check. |

## 8. Where the repository uses older data than it could

1. CPS ASEC: childcare uses only the 2017 survey; ages use 2015–2019; both
   could use 2018–2026, which are on disk.
2. SIPP: tips use releases through 2023 (income years through 2022); the 2024
   and 2025 releases (calendar 2023 and 2024) are on disk.
3. Spouse wage split: Saez's 2014 tabulation; CPS couples through 2025 are
   available, though only below the topcode.
4. DINA: sex uses 2017 and the state-and-local tax rate uses 2019. No newer
   public DINA microfiles appear to exist; the non-filer pool now observes
   sex directly.
5. Retirement-plan contributions and prior-year earnings are zero, though CPS
   data for both exist.
6. CEX 2025 arrives 29 October 2026; SCF 2025 is due late 2026. Both should
   be planned for, with the CEX shutdown gap handled explicitly.
7. Unused files already in the store that bear on the prior or targets: the
   Saez–Zucman 2013–2016 age tables, the SSA 2006 earnings file, the CPS
   ASEC panel links, NSECE, PSID extracts.

## 9. What I looked for and could not find or verify

- Whether the tax-year 2016 synthetic PUF or supplemental synthetic PUF was
  actually released after the November 2025 target date. I found no release
  announcement; the IRS microdata page in May 2026 still said "in the next
  few months".
- How the 2012 synthetic supplemental PUF is distributed, and whether it is
  still available.
- Whether the Budget Lab's license for the 2015 PUF is affected by the
  withdrawal.
- PSID latest wave and sample size; MEPS, HRS and NHIS latest years and
  sizes (sites refused requests, and the search allowance was exhausted).
- A current SSA list of public-use files (site refused requests).
- Any public DINA microfile later than the February 2022 update, and a direct
  download for Realtime Inequality microfiles.
- The current base data of the Tax Policy Center model (site refused
  requests).
- Whether Commerce Order 216-26 will change topcoding or swapping in future
  CPS ASEC and SIPP public files.
- The exact SIPP sample size per release and the 2019 SCF family count.
- A published quantification of CPS `INCCAPG` against SOI capital gains.

## Sources

IRS and SOI
- https://www.irs.gov/statistics/soi-tax-stats-individual-public-use-microdata-files (verified, reviewed 22 May 2026)
- https://www.irs.gov/statistics/soi-tax-stats-whats-new (verified)
- 2015 Public Use Tax File booklets, on disk at `/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/` (verified)
- https://www.irs.gov/pub/irs-soi/18rpsynthindtaxfiles.pdf (verified)
- https://www.irs.gov/pub/irs-soi/26jsrpapplication.pdf (verified)
- https://www.irs.gov/pub/irs-soi/14rfpufredesignrecommen.pdf (cited in the booklet; not opened)

Urban Institute and Tax Policy Center
- https://www.urban.org/sites/default/files/publication/102547/a-synthetic-supplemental-public-use-file-of-low-income-information-return-data_3.pdf (verified)
- https://www.urban.org/research/publication/safely-expanding-research-access-administrative-tax-data-creating-synthetic-public-use-file-and-validation-server (not opened)

Census
- https://www.census.gov/content/dam/Census/library/working-papers/2022/demo/sehsd-wp2022-18.pdf (verified)
- https://www.census.gov/content/dam/Census/library/working-papers/2015/demo/SEHSD-WP2015-01.pdf (verified)
- https://www.census.gov/content/dam/Census/library/working-papers/2017/demo/SEHSD-WP2017-39.pdf (not opened)
- https://cps.ipums.org/cps/topcodes_tables.shtml (search summary; not opened)
- https://www.census.gov/library/publications/2026/demo/p60-289.html (search summary)
- https://www.census.gov/newsroom/press-releases/2026/iphi-acs-media-advisory.html (verified)
- https://www.census.gov/newsroom/blogs/director/2026/08/understanding-the-new-disclosure-avoidance-policy.html (verified)
- https://www.census.gov/programs-surveys/sipp/data/datasets/2025-data/2025.html (verified)
- https://www.census.gov/programs-surveys/sipp/guidance/sipp-synthetic-beta-data-product.html (verified)
- https://www.census.gov/data/experimental-data-products/national-experimental-wellbeing-statistics.html (verified)
- https://www2.census.gov/about/linkage/data-file-inventory.pdf (verified, August 2026)

Federal Reserve, BLS, SSA
- https://www.federalreserve.gov/econres/scfindex.htm (verified)
- https://www.federalreserve.gov/newsevents/pressreleases/other20250228a.htm (search summary; not opened)
- https://www.bls.gov/cex/2025-federal-government-shutdown-impact-ce.htm (verified)
- SSA EPUF dictionary on disk at `SSA-EPUF/v1/2006/epuf_dictionary.pdf` (verified); https://www.ssa.gov/policy/docs/microdata/epuf/ (refused)

Research files
- http://gabriel-zucman.eu/files/PSZCodebook.pdf (verified); https://gabriel-zucman.eu/usdina/ (verified)
- https://github.com/thomasblanchet/real-time-inequality (verified)
- https://policyengine.github.io/policyengine-us-data/ and its `methodology` and `data` pages (verified)
- https://huggingface.co/policyengine/policyengine-us-data (verified)
- https://github.com/PolicyEngine/microcosm and issue 982 (verified)
- https://github.com/PSLmodels/tax-microdata-benchmarking (verified, including `tmd/datasets/tmd.py`, `tmd/utils/reweight.py`, `tmd/imputation_assumptions.py`, `tmd/create_taxcalc_imputed_variables.py`, `tmd/utils/pension_contributions.py`)
- https://taxcalc.pslmodels.org/usage/data/ (verified)

Other
- https://ncses.nsf.gov/about/national-secure-data-service-demo (search summary; not opened)
