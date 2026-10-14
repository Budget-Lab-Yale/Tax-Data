---
title: "Review of the IRS-data and microdata background reports (01, 03)"
role: review
workstream: rebuild
status: current
updated: 2026-10-07
true_as_of: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# Review of the IRS-data and microdata background reports

Reports reviewed: `research/rebuild/background/01_data_irs.md` (IRS sources
for 2015 onward) and `research/rebuild/background/03_data_microdata.md`
(record-level sources for the prior). Checked on 6–7 October 2026 against
the IRS and Census pages, the SOI 2026 release calendar (revised 10
September 2026), the downloaded SOI tables, the PUF booklets and files on
disk, the code in `src/`, and the GitHub repositories cited. Data were read
through Slurm job steps.

## Summary

**Claims checked: 54** (28 in report 01, 30 in report 03; 4 claims appear
in both and are counted once in each report's column). Grades:

| Grade | 01 (IRS) | 03 (microdata) |
|---|---|---|
| Confirmed | 21 | 21 |
| Overstated (true in part) | 3 | 6 |
| Wrong | 3 | 1 |
| Unsupported by the cited source | 0 | 1 |
| Uncheckable | 1 | 1 |

**Errors that would change a decision**

1. **The 2015 demographic file is not limited to low incomes in the sense
   the reports imply** (03, summary item 2 and §2). The booklet's cutoff is
   $250,000 of *indexed positive income in 1991 dollars*, which is about
   $397,000 in 2015 dollars. On disk the file covers 97.8 percent of
   weighted returns and contains 5,939 records with AGI of $250,000 or
   more (45 percent of the weighted returns above that line), up to an AGI
   of $584,200. The records without demographics are 3.3 million returns
   (2.2 percent) holding 23 percent of AGI. So the gap is real, but it is
   the top 1–2 percent of returns, not "every high-income record". This
   sets the size of the age, sex and earnings-split problem the rebuild has
   to solve.
2. **The current pipeline's ages for those records come from an SOI table,
   not from CPS** (03, §1 table). `src/process_puf.R` fills missing age
   bands from Pub 1304 Table 1.6 for 2015 by filing status × six AGI groups
   (top group $200,000 and over); `src/imputations/ages.R` then only draws
   a single year of age within the band from the CPS age distribution and
   the spouse's age gap from the 2015–2019 CPS. Report 03 lists CPS as the
   source for filer ages.
3. **The Saez–Zucman "age tables" are a weaker post-2015 target set than
   report 03 says** (03, summary item 10). Only the 2016 file is after
   2015. "Age" is a single split, 65 or over versus under 65 (the under-65
   group split by presence of children); cells are AGI percentile groups,
   not dollar classes; 186 cells survive suppression, with about 44 mean
   variables. Annual Pub 1304 Tables 1.5 and 1.6 already give finer age
   detail for every year 2015–2023. The tables remain useful for
   married × itemizer × top-percentile cells in 2016.
4. **The PolicyEngine top-coding example is out of date** (03, §5). Issue
   #982 in `PolicyEngine/microcosm` was closed on 22 September 2026 and the
   fix (#1033, conditioning on demographics and income rank) was reported
   on 26 September. The issue also says imputed wages exceeded the CPS
   maximum by no more than a few percent, not "never". The general lesson
   stands; the description of the current build does not.
5. **Top-tail percentile tables reach back to 2015** (01, Table 4.3). The
   same table as 4.3 is published for 2014–2018 under the name "Table 3"
   (`{yy}in03etr.xls`; checked for 2016). Report 01 lists these links but
   only "confirms" 2019–2023, so a reader could conclude there is no
   top-0.001-percent target for the 2015 base year. There is.

Smaller errors: the Pub 1304 sample for tax year 2020 was 385,971 returns,
not "about 360,000" (01); the high-income returns study for 2022 came out in
April 2026, not November 2025 (01); Pub 5385 for 2023 came out 26 May, not
June (01); the CPS capital gains item `INCCAPG` appears in the Budget Lab
extracts from the 2019 survey, not "since the 2014 redesign" (03);
`src/nonfilers/state_weights.R` in this repository already reads the
`IRS-Ind` store (01 says Tax-Data does not read it).

**Omissions.** (a) Commerce order DAO 216-26 bans record swapping and
synthetic data as well as noise infusion (Census Director's blog, August
2026). The CPS ASEC protects top values by rank-proximity swapping, so the
order bears directly on future CPS top-coding; report 03 mentions only noise
infusion. (b) Census's inventory lists Form 1040 returns annually from 1994
through at least 2018, and tax-year 2025 returns received in 2026; report 03
describes the holdings as "back to 1969 at intervals". (c) Neither report
gives the share of income held by records without demographics (23 percent
of AGI). (d) Table 1.5 has no AGI dimension; report 01 calls it "the only
IRS joint information on income with age" without saying that the joint
detail is age band × income item, with no income-size classes.

**Trust.** Report 01 is reliable on what exists and when: every table it
calls unused exists with the content it describes (1.3, 1.4CV, 1.4CI, 1.5,
2.5, 4.3, the W-2 and IRA studies, Pub 5385), and the release calendar facts
check out. Its errors are in secondary dates and one sample size. Report 03
is reliable on facts read from disk and code (record counts, survey years,
training years for tips, childcare, consumption and wealth), and on TMD and
the Urban synthetic file. Its interpretive summaries overstate in three
places that bear on rebuild design: demographic-file coverage, the value of
the Saez–Zucman tables, and the PolicyEngine example. Neither report invents
a table or publication. No contradiction between the two reports was found
on any checked fact; they differ only in how they label the JSRP dates
(01 "search summary", 03 "verified"; the PDF confirms them).

## Claims checked

Paths beginning `raw_data/` are under
`/nfs/roberts/project/pi_nrs36/shared/raw_data/`. "Calendar" is
https://www.irs.gov/pub/irs-soi/soi-public-release-list.xlsx (revised
9/10/2026). "What's New" is
https://www.irs.gov/statistics/soi-tax-stats-whats-new.

### Status of IRS microdata (both reports)

| Report | Section | Claim | Grade | Evidence |
|---|---|---|---|---|
| 01, 03 | Summary 3 / §2 | SOI's 2012–2015 public use files are "no longer available"; page dated 22 May 2026; synthetic file "will be available in the next few months". | CONFIRMED | Page text and "Last Reviewed or Updated: 22-May-2026", https://www.irs.gov/statistics/soi-tax-stats-individual-public-use-microdata-files |
| 01, 03 | Microdata access / §2 | Synthetic PUF and supplemental (non-filer) synthetic PUF, tax year 2016, scheduled for 21 November 2025; no sign of release. | CONFIRMED | What's New, "Notice of revised release date (May 2025)" lists both for Nov. 21, 2025; neither appears on the 2026 calendar; PUF page still says "next few months". |
| 03 | §2 | That notice is "dated 29 May 2025". | OVERSTATED | The notice is headed "May 2025". 29 May 2025 is the date it gives for the 2024 IRS Data Book, one of its items. |
| 01, 03 | Microdata access / §9 | The status of the Budget Lab's own 2015 PUF license under the withdrawal is unknown. | UNCHECKABLE | Not stated on any IRS page found; needs the license terms. Both reports flag this correctly. |

### Pub 1304 timing and tables (report 01)

| Report | Section | Claim | Grade | Evidence |
|---|---|---|---|---|
| 01 | Summary 1 | Pub 1304 tables for tax year 2023 released 26 March 2026; tax year 2024 not on the 2026 calendar. | CONFIRMED | Calendar: 2026-03-26, Publication 1304, program year 2023; no 2024 entry. What's New: "March 26, 2026 ... tax year 2023". Excel files saved 25–26 March 2026. |
| 01 | Tables | Table 1.5: income sources, adjustments and tax items by age of primary taxpayer, seven bands (under 18, 18–25, 26–34, 35–44, 45–54, 55–64, 65+). | CONFIRMED | `23in15ag.xls` and `15in15ag.xls` downloaded and read: bands "Under 18 … 65 and over"; columns run from AGI through income tax before credits. Part 1 page links `in15ag` for 2006–2023. No AGI-size dimension. |
| 01 | Tables | Table 1.3: every item by filing status. | CONFIRMED | `23in13ms.xls`, title "Table 1.3. All Returns: Sources of Income, Adjustments, Deductions…"; part 1 page links 1996–2023. |
| 01 | Tables | Table 1.4CV confirmed 2010, 2012, 2014–2023; 1.4CI 2023 only checked. | CONFIRMED | `23in14cv.xls` title "Table 1.4CV. Coefficients of Variation…". Coverage is wider than stated: part 1 page links CV files for 1996–2023 and CI files (`in14ci`) for 2018–2023 (17in14ci.xls returns 404). |
| 01 | Tables | Table 4.3: percentile groups down to the top 0.001 percent with income by source; 2019–2023 confirmed, page lists years back to 2014. | CONFIRMED | `23in43ts.xls`: 0.001 percent row (1,531 returns in 2023), columns for wages, interest, dividends, capital gains, business, partnership and S corporation income, deductions, credits, tax. Page links 2014–2018 to `{yy}in03etr.xls`; `16in03etr.xls` has the same title as "Table 3". https://www.irs.gov/statistics/soi-tax-stats-individual-statistical-tables-by-tax-rate-and-income-percentile |
| 01 | Tables | Table 2.5: EITC by AGI × number of qualifying children (0, 1, 2, 3+), 1996–2023. | CONFIRMED | `23in25ic.xls` title "Returns with Earned Income Credit, by Size of Adjusted Gross Income and Number of Qualifying Children"; by-size page lists `in25ic` files. |
| 01 | Pub 1304 | Sample about 360,000 returns for tax year 2020, out of about 167 million; certainty above $8 million indexed income. | WRONG (number) | Pub 1304 Rev. 11-2022, Table C: sample 385,971, population 166,615,394; $8 million and over strata sampled at 100 percent (confirmed). https://www.irs.gov/pub/irs-prior/p1304--2022.pdf |
| 01 | Censoring | Itemizers 15.1 million in 2023 against 46.9 million in 2017. | CONFIRMED | `raw_data/IRS-Ind/aligned/bysize_itemized_deductions.csv`: 15,106,257 (2023), 46,852,675 (2017). |
| 01 | Filing season | Newest is tax year 2024 (mid-November 2025 cut); tax year 2025 files not posted; "17 AGI classes". | CONFIRMED, one correction | `25inweek47.xlsx` returns 200, title "Tax Year 2024 (through Filing Season 2025 Cycle 47, November 20, 2025)"; `26inweek21.xlsx`, `26inweek30.xlsx` return 404; mid-July TY2025 on calendar for 17 Sep 2026. The file has 16 AGI classes plus a total column, not 17 classes. |
| 01 | Preliminary data | Preliminary data for tax year 2023 cancelled. | CONFIRMED | What's New: "Cancelled: Individuals, preliminary data, tax year 2023". |
| 01 | Release table | W-2 tables 2021–2023 and ZIP/district 2023 on 22 Oct 2026; Sales of Capital Assets 2018 on 24 Nov 2026; partnerships 2024 on 17 Dec 2026; Pub 6187 next edition 17 Dec 2026. | CONFIRMED | Calendar entries on those dates. |
| 01 | Release table | High-income returns study, tax year 2022, released November 2025. | WRONG | Calendar: 2026-04-23. What's New: released April 2026; the 14 Nov 2025 date was revised. |
| 01 | Release table | Pub 5385, tax year 2023, released June 2026. | WRONG (minor) | Calendar: 2026-05-26; What's New lists it under May 2026. Pub 4801 (4 June) is correct. |
| 01 | Return projections | Pub 6187 (Rev. 12-2025): Schedule A returns 15.5, 15.7, 20.5, 21.1 million for 2024–2027; 1040s 159.3 million (2024), 163.2 million (2027). | CONFIRMED | Pub 6187 text: Schedule A 15,546,066; 15,708,500; 20,540,900; 21,093,500; Forms 1040 159,266,861 and 163,226,400. https://www.irs.gov/pub/irs-pdf/p6187.pdf |

### Studies called unused (report 01)

| Report | Section | Claim | Grade | Evidence |
|---|---|---|---|---|
| 01 | Pub 5385 | Covers information returns for sampled filers, their dependents, and a simple random sample of non-filers by SSN digits; Form SSA-1099 absent; 279 million W-2s; PDF for 2017–2023. | CONFIRMED | `raw_data/IRS-Ind/national/line_items/p5385_2023.pdf` (Rev. 5-2026): "…but no tax return was filed", "simple random sample of recipients of information returns"; "Total of all forms filed = 279,180,928"; no "SSA" string in the text. Line-item page lists 5385 for 2017–2023. |
| 01 | W-2 study | Available 2014 and 2016–2020, no 2015; 2021–2023 due 22 Oct 2026. | CONFIRMED | Store: `raw_data/IRS-Ind/national/w2/` holds 2014, 2016–2018 single workbooks and 2019–2020 Tables 1–4; `NOTES.md` records 2015 absent under five filename shapes. Calendar: 22 Oct 2026, tax years 2021, 2022, 2023. |
| 01 | W-2 study | 2019–2020 reissued January 2026 with age statistics. | CONFIRMED, with a conflict | Calendar: W-2 tabulations 2019 and 2020 on 2026-01-22; What's New: "tables for tax years 2019 and 2020 are now available as a single file"; W-2 page (updated 27 Jan 2026) shows age in Tables 1–3. The store's `NOTES.md` (9 Sep 2026) says the 2019–2020 files have no age-stratified sheets, so the store copy may predate the reissue. https://www.irs.gov/statistics/soi-tax-stats-individual-information-return-form-w2-statistics |
| 01 | W-2 study | "The only IRS source for the split of wages between spouses." | OVERSTATED | The 2015 PUF demographic file carries `EARNSPLIT` (three-way split of earnings on joint returns), and `resources/wagesplit.csv` is itself a tabulation of SOI data. The W-2 study is the only current-year source. |
| 01 | IRA study | Tables by age, AGI percentile, filing status and sex; enhancements from 2022; CVs for 2018, 2020–2022. | OVERSTATED (minor) | Page lists Tables 1–10 with age (4, 5, 6, 8) and percentile (9, 10); "Beginning with data for 2022, SOI introduced enhancements" confirmed. Page says CV files cover 2018–2021; the store holds CV files for 2018, 2020, 2021, 2022. https://www.irs.gov/statistics/soi-tax-stats-accumulation-and-distribution-of-individual-retirement-arrangements |

### What the repository uses and what is on disk (report 01)

| Report | Section | Claim | Grade | Evidence |
|---|---|---|---|---|
| 01 | Repository use | 188 targets, all for 2017, from Tables 1.4, 1.4A, 1.6, 2.1, 2.3, 3.3 and Pub 4801. | CONFIRMED | `config/target_info/baseline.csv`: 188 rows, all year 2017; `src/process_targets.R` reads the seven files. |
| 01 | Repository use | Interface v2 years (1.4: 2017–19; 1.4A and 1.6: 2015, 2017; others 2017); v3 (vintage 20250319) adds 2022 to 1.4, 1.4A, 1.6, 3.3 and Pub 4801; runscript pins v2; files "compiled by hand". | CONFIRMED | `/nfs/roberts/project/pi_nrs36/shared/model_data/Compiled-SOI-Tables/v{2,3}/*/historical/*.csv` year columns; `config/interfaces/interface_versions.yaml` (version 2); `readme.txt` text. |
| 01 | Repository use | `resources/return_counts_2023.csv` (2017–2023) drives weight growth; `return_counts_2019.csv` still read. | CONFIRMED | File headers; `src/project_puf.R` lines 212–213, 301, 321. |
| 01 | Store | `raw_data/IRS-Ind` holds Pub 1304 by-size tables 2011–2023, Pub 4801 parsed (24,838 values; 152 of 156 checks exact, 4 differ by one), Table 1.6 target view reproducing `return_counts_2023.csv`, Pub 5385 PDFs unparsed. | CONFIRMED | `raw_data/IRS-Ind/NOTES.md` (built 2026-09-27) lines 590–600, 675–676, 1087–1097, 1194; directory listings. |
| 01 | Summary 7, store | "Tax-Data does not read it"; only listed consumer is Affordability-Index. | OVERSTATED | `src/nonfilers/state_weights.R` (lines 109, 588, 936, 1037, 1075) reads `IRS-Ind` HT2, by-size and IRA files for the non-filer and state-weight builds; `research/state_weights/` scripts call it. `src/main.R` does not. |
| 01, 03 | Microdata access / §6 | JSRP 2026 window 1–30 September 2026, selections 4 January 2027, onboarding 1 February 2027; lead researcher a U.S. citizen eligible for the IPA program; work on IRS equipment. | CONFIRMED | https://www.irs.gov/pub/irs-soi/26jsrpapplication.pdf |

### The 2015 PUF and demographic file (report 03)

| Report | Section | Claim | Grade | Evidence |
|---|---|---|---|---|
| 03 | §2 | 207,696 records including four aggregate records (RECID 999996–999999) holding 1,124 returns; 151.2 million returns; 222 columns, no state field. | CONFIRMED | `raw_data/IRS-PUF/v1/2023091310/historical/puf_2015.csv`: 207,696 × 222, no STATE or FIPS column; booklet: "A total of 1,124 returns were aggregated", "151.2 million". |
| 03 | §2 | Disclosure steps: 10 percent subsampling cap, blurring in groups of three above the 0.07 percent rate, up to 10 percent uniform noise on non-itemizers' wages, rounding. | CONFIRMED | `2015 Public Use Booklet.pdf` text, Section on disclosure ("subsampled at 10 percent", "groups of three", "up to 10 percent of random noise, drawn from a uniform distribution"). |
| 03 | §2 | Demographic file: 119,675 records, strata 7–13, sampled at 0.07 percent; booklet Table A shows indexed positive income below $250,000 (1991 dollars, deflator 1.5874). | CONFIRMED | `2015 Public Use Booklet demographic.pdf`, "Domain of the File" and Table A; `demographics_2015.csv` has 119,675 rows, all matching PUF RECIDs. |
| 03 | Summary 2, §2 | The file "covers only lower-income records" and "excludes every record with indexed income of $250,000 or more"; consequence: the training data "contains no high-income records". | OVERSTATED | In 2015 dollars the cutoff is about $397,000. Computed from the files: the file covers 97.8 percent of weighted returns; 5,939 records (44.8 percent of weighted returns) with AGI ≥ $250,000 are in it; 6 records above $400,000; maximum AGI $584,200. Records without demographics: 3.34 million weighted returns, 22.5 percent of total AGI. |
| 03 | §1 table | Filer ages come from CPS ASEC 2015–2019 (`ages.R`). | OVERSTATED | `cps_00027.csv.gz` YEAR = 2015–2019 (confirmed). But age bands come from the demographic file `AGERANGE`, and for records without it from Pub 1304 Table 1.6 for 2015 by filing status × AGI group (`src/process_puf.R` lines 28–41, 95–113); CPS supplies only the single year within the band and the spouse age gap. |

### Imputation training data and on-disk surveys (report 03)

| Report | Section | Claim | Grade | Evidence |
|---|---|---|---|---|
| 03 | §1 | Childcare trained on the 2017 ASEC only (income year 2016). | CONFIRMED | `src/imputations/childcare.R` line 13, `filter(YEAR == 2017 …)`, `SPMCHXPNS` present in `cps_00027`. |
| 03 | §1 | Tips trained on SIPP releases 2018–2023 (income years 2017–2022); the 2024 and 2025 releases are on disk in `SIPP/v2/20260927`. | CONFIRMED | `raw_data/SIPP/v1/2024090311/baseline/tip_ind_occ_full_split.csv` years 2018–2023; `tips.R` line 31 `year = year - 1`; `SIPP/v2/20260927/manifest.csv` lists `pu2024_csv.zip` (2023) and `pu2025_csv.zip` (2024). |
| 03 | §1 | Spouse wage split from Saez's tabulation for 2014; sex from DINA 2017; state and local tax rate from DINA 2019; prior-year earnings and employer contributions set to zero. | CONFIRMED | `earnings_split.R` line 12 `filter(year == 2014)` (file spans 1999–2014); `demographics.R` line 37; `state_local_tax.R` line 65; `placeholders.R` lines 12–17. Sex is the demographic file's `GENDER` where present, DINA only for the rest. |
| 03 | §1 | Overtime from `resources/otdata2023.dta`, CPS-like earnings data for 2023, provenance undocumented. | CONFIRMED | File: 242,582 rows, all year 2023; variables `total_weekly_earnings`, `ot_weekly_earnings`, `flsa_eligible`, `wkswork1`; no source note in the repository. |
| 03 | §1 | Wealth: SCF 2022 tax units (29,290 rows over five implicates) with SCF 2019 pooled for donors; consumption from CEX 2022–2024. | CONFIRMED | `wealth.R` line 132; `pool_scf_donors.R`; `main.R` line 106; `cex.R` line 31 `years = c(2022, 2023, 2024)`. |
| 03 | Summary 4, §3 | On disk: CPS ASEC through the 2026 survey; SIPP through the 2025 release; CEX through 2024; SCF 2009–2022; ACS 1-year through 2024. CPS person records 185,487 (2016), 144,265 (2024), 142,125 (2025), 134,729 (2026); ACS 2024 3,422,888. | CONFIRMED | Manifests in `raw_data/CPS-ASEC/cps_asec_common/cps20{15..26}_03s/` (`n_records`), `raw_data/ACS/acs_common_v2/us2024a/manifest.json`, `raw_data/SCF/v1/`, `raw_data/CEX/2024/`. |
| 03 | §3 CEX | 2024 interview files have 4,673, 4,612, 4,601 rows (Q2–Q4); `MRTINTCQ`, `PROPTXCQ`, `VEHFINCQ`, `CASHCOCQ` present; 2025 microdata 29 Oct 2026; shutdown losses of a third (July, October) and two-thirds (August, September); weights not adjusted in the public file. | CONFIRMED | Row counts read from `raw_data/CEX/2024/fmli24{2,3,4}.csv`; https://www.bls.gov/cex/2025-federal-government-shutdown-impact-ce.htm |
| 03 | §3 CPS | `INCCAPG` (capital gains) asked "since the 2014 redesign". | WRONG | In the Budget Lab extracts `INCCAPG` is absent for the 2015–2018 surveys and present from 2019 on, together with the other variables of the 2019 processing redesign (`INCRET1`, `SRCRET1`, …). |
| 03 | §3 CPS | Census's tax-model variables (`FILESTAT`, `ADJGINC`, `FEDTAX`, …) are in the extract; `RETCB_VAL` is not. | CONFIRMED | CPS manifests. |
| 03 | §3 CPS | Rothbaum (2015): self-employment 44.0 to 31.7 percent of NIPA, interest 53.7–74.9, dividends 27.2–101.6, rent and royalties 25.1–29.9. | CONFIRMED | https://www.census.gov/content/dam/Census/library/working-papers/2015/demo/SEHSD-WP2015-01.pdf, appendix table. |
| 03 | §3 ACS | 2025 ACS 1-year date "being determined" pending DAO 216-26, which bars noise infusion. | CONFIRMED, incomplete | Media advisory confirms the delay. The Director's blog says the order also bars record swapping and synthetic data ("Noise infusion is no longer allowed … record swapping and synthetic data methods"). https://www.census.gov/newsroom/blogs/director/2026/08/understanding-the-new-disclosure-avoidance-policy.html |
| 03 | §3 SCF | 2022 is the latest; 4,602 families interviewed; public file 22,975 records for 4,595 families. | CONFIRMED | https://www.federalreserve.gov/econres/scfindex.htm |

### Research files and outside projects (report 03)

| Report | Section | Claim | Grade | Evidence |
|---|---|---|---|---|
| 03 | Summary 10, §2 | Saez–Zucman files 2013–2016 are the 2012 PUF aged to internal SOI tabulations; the age tables give 12 groups (married × itemizer × 65+/under 65 with or without children) by 16 AGI percentile groups. | CONFIRMED | https://www.irs.gov/pub/irs-soi/18rpsynthindtaxfiles.pdf, pp. 8–9; `raw_data/Saez-Zucman-PUF/v1/2023082914/historical/agetable2016.dta`: 186 rows, `strata` 1–12, `agibin` −1 to 0.9999. |
| 03 | Summary 10 | The age tables are "a finer post-2015 target set" for 2013–2016 with "means of 47 variables per cell". | OVERSTATED | Only 2016 is after 2015; age is one 65+ indicator; 186 of 192 cells survive; about 44 mean variables (50 columns including identifiers). A 2012 table is also on disk. Pub 1304 Tables 1.5 and 1.6 give finer age detail annually. |
| 03 | §2 | Urban–SOI 2012 supplemental synthetic file: 0.1 percent sample of information returns, non-filers below twice the joint filing threshold, about 26,000 observations, CART synthesis. | CONFIRMED | https://www.urban.org/sites/default/files/publication/102547/a-synthetic-supplemental-public-use-file-of-low-income-information-return-data_3.pdf |
| 03 | Summary 6, §5 | TMD version 2.2.1 released 28 September 2026, tax year 2022; requires the licensed 2015 PUF and demographic file; CPS non-filers below $9,990 with no positive tax; quadratic-program reweighting within ±0.5 percent, ratios in [0.1, 10]; tips and overtime from SIPP 2024, auto-loan interest from CEX 2023; pensions from `RETCB_VAL`. | CONFIRMED | `README.md`, `setup.py` (version 2.2.1), `tmd/imputation_assumptions.py`, `tmd/datasets/tmd.py`, `tmd/create_taxcalc_imputed_variables.py`, `tmd/utils/pension_contributions.py` at https://github.com/PSLmodels/tax-microdata-benchmarking |
| 03 | Summary 6, §5 | `policyengine-us-data` archived 2 July 2026; successor `microcosm` created June 2026; Enhanced CPS imputes 67 PUF variables and calibrates to 2,813 targets by gradient descent. | CONFIRMED | GitHub API: archived true, last update 2026-07-02; microcosm created 2026-06-10. Methodology abstract: 67 variables, 2,813 targets, "dropout-regularized gradient descent". https://policyengine.github.io/policyengine-us-data/ |
| 03 | §5 | microcosm issue #982: "in the current build" the PUF-imputed half reproduces ASEC income and imputed wages "never exceed" $2,099,999. | OVERSTATED | Issue closed 2026-09-22; fix reported in #1033 on 2026-09-26. The issue says no clone's wages exceed the ASEC maximum "by more than a few percent". https://github.com/PolicyEngine/microcosm/issues/982 |
| 03 | Summary 9, §6 | Census holds Form 1040 e-file data 2011–2022 and Form 1040 returns "back to 1969 at intervals"; W-2 and 1099 data some through tax year 2025. | OVERSTATED | Inventory (August 2026): MeF 2011–2022 confirmed; Form 1040 returns listed for 1969, 1974, … then annually 1994–2018 (cell truncated), with tax year 2025 received in 2026; information returns for TY2025 confirmed. https://www2.census.gov/about/linkage/data-file-inventory.pdf |
| 03 | §6 | "The 2025 [JSRP] window was postponed." | UNSUPPORTED | Not in the cited 2026 call PDF; the JSRP page (reviewed 1 Oct 2026) says nothing about 2025. |

## Method notes

- Grade counts in the summary count each row once per report it covers.
- The demographic-file figures use `S006/100` as the weight and `E00100`
  as AGI, excluding the four aggregate records. Script:
  `rv/scripts/demo.R` in this session's scratchpad (not kept).
- Claims not checked, for lack of time or a primary source: the 2022
  Pub 1304 complete-report PDF being the newest; CRS report R49356; the
  NEWS release date; the SIPP Synthetic Beta production plans; the
  current PolicyEngine weight penalty (the abstract says dropout, report
  03 says a penalty pushing weights to zero); the EITC participation
  rate of 80.8 percent.
