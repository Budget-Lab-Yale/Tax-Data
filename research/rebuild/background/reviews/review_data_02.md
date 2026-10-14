---
title: "Review of 02_data_aggregates.md (non-IRS aggregate data)"
role: review
workstream: rebuild
status: current
updated: 2026-10-07
true_as_of: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# Review of `02_data_aggregates.md`

Reviewer's task: find what is wrong in the inventory of non-IRS aggregate data.
Every grade below rests on a file or page I opened myself on 6–7 October 2026.
Workbook and PDF contents were read through Slurm jobs; the working files are in
`/nfs/roberts/scratch/pi_nrs36/jar335/review_data02/` (outside the repository).

## Summary

Most of the inventory is accurate. The CBO, BEA, Social Security and Federal
Reserve numbers I checked match the sources, usually to the decimal. The errors
that would change a decision about targets are these.

1. **JCT Table 3 does not give deduction amounts.** The "amount" columns in
   JCX-45-25 Table 3 are tax expenditures: the tax saved because of the
   deduction, not the dollars deducted. The SALT amount is $57.5 billion and
   the charitable amount is $71.7 billion. Both are far below the deductions
   themselves. The return counts are also hard to read as counts of itemizers.
   Table 3 shows 23.4 million returns for SALT, but Table 2 shows only 21.1
   million itemizers in total. The SALT and charitable columns have identical
   totals. The table can still be matched by simulation, but only by computing
   each record's tax with and without the deduction. It is not a table of
   deduction counts and totals by income class, which is how the report
   describes it (summary item 6, recommendation 3).
2. **The DFA question can be settled. The Fed's income groups rank households
   on realized income, not "usual" income.** In the local DFA files, the
   income cutoffs for 2022Q3 are $31,247, $54,444, $90,796, $151,328 and
   $1,197,734. I computed weighted percentiles from the SCF 2022 summary
   extract. The SCF's realized-income variable (`INCOME`) gives $31,346,
   $54,046, $90,797, $151,328 and $1,199,812. The usual-income variable
   (`NORMINC`) gives $34,589, $59,450, $91,878, $153,489 and $1,066,860. 2019
   shows the same pattern. The project memory note
   (`project_wealth_aging.md`) is wrong, and so is the "usual-income gap" it
   describes. The Fed's published documentation does not state the definition
   either way.
3. **The `ui` column can be traced.** For 2025–2036 it equals, to the last
   digit, the "Unemployment compensation" line of CBO's February 2026 Table
   3-2 (mandatory outlays, fiscal years). The report lists it as
   unidentified.
4. **The Macro-Projections conclusions rest on a stale checkout.** The local
   copy is three commits behind GitHub. The current code (May 2026) runs
   2026 vintages. After 2056 it carries forward the long-term outlook's 2056
   growth rates. The interface vintage Tax-Data uses (2026022522) shows that
   behaviour: nominal GDP grows 3.52 percent a year from 2057 to 2099, and
   income shares of GDP are constant. One correction: interest income is
   not held at a fixed share of GDP. It moves with the 10-year Treasury rate
   until 2056.
5. **The "only long-run constraint on the top of the wage distribution" is
   overstated.** CBO's long-term outlook economic workbook already has the
   share of earnings below the taxable maximum to 2056. In the 17 September
   2026 Social Security file, the share is flat at 80.7 percent from 2056 to
   2100. After 2056 it adds a constant, not new information.
6. **SSA 2024 wage distribution: availability not shown.** The only archived
   copies of SSA's 2024 wage-statistics page (2 and 17 April 2026) say "Data
   for 2024 is not available." The 2024 average wage index (69,846.57) is
   published, so SSA has tabulated the 2024 data. I found no copy of the 2024
   bracket table. The 2023 figures quoted in the report are correct.
7. **The SALT series check is overstated.** The local NIPA file cited as
   verification ends in 2020Q4, so it cannot verify 2021–2024. Against
   current BEA data the resource file is an older vintage, and its growth
   rates for 2023–2024 differ materially (details in the table).
8. **Macro-Projections FRED key** (a claim the caller raised; it is not in
   `02`). It is confirmed in the local checkout. The current public GitHub
   version no longer contains it, but older public commits still do.

## Claims checked

Grades: CONFIRMED, WRONG, OVERSTATED, UNSUPPORTED (stated as verified or as
fact without adequate support), UNCHECKABLE.

### CBO revenue workbook (February 2026)

| # | Claim in `02` | Grade | Evidence |
|---|---|---|---|
| 1 | Sheet 3 covers calendar years 2022–2036, and only 2022 is labelled "Actual". | CONFIRMED | Local `CBO-Budget-Proj/v3/20260211/baseline/Revenue.xlsx`, tab `3.Individual Income Tax Details` (no space after "3."), table title "3. Detailed Individual Income Tax Projections in CBO's February 2026 Baseline". |
| 2 | Sheet 3 contents: income sources, AGI, standard and itemized deductions, QBI deduction, "additional deductions" (tips, overtime, car-loan interest, age 65+), seven brackets, AMT, credits, NIIT, returns, itemizers, AMT count, EITC and CTC, AGI shares of top 1/5/10/25/50 percent. | CONFIRMED | All present. The additional-deductions line is nonzero only for 2025–2028 ($416.3 billion in 2025, zero from 2029). Itemized deductions appear only as one total, with no split by type. |
| 3 | 168.0 million returns and 20.9 million itemizers in 2025; top 1 percent AGI share 23.3 percent in 2025. | CONFIRMED | Same sheet. |
| 4 | Net business income is "before the passive-loss and excess-business-loss limits". | OVERSTATED | Footnote b says "before disallowed losses on Form 461 and the deduction for qualifying business income". Form 461 covers the excess-business-loss limit. Passive losses are not mentioned. |
| 5 | Footnotes: the model "begins with a sample of tax returns…"; "over 300,000 tax returns filed in 2022"; return count includes dependents and future additional filers and ignores filing-threshold changes. | CONFIRMED | Footnote text and footnote l. |
| 6 | Sheet 6: realizations 1996–2036, Treasury through 2023, CBO from 2024; $1,286 billion in 2024, $1,635 billion in 2025; net gains on returns with a net gain. | CONFIRMED | Tab `6. Capital Gains Realizations`: 1,286.2 and 1,635.0, with notes as stated. |
| 7 | `resources/cbo_1040.csv` comes from sheet 3. | CONFIRMED for 2022–2036 | All eight CBO columns match sheet 3 exactly for 2022–2036 (for example wages 9739 … 17325.9; capital gains 1286 … 1851.5; taxable Social Security 459.3 … 1247.7). |
| 8 | Rows 2019–2021 "presumably come from earlier CBO vintages (not checked)". | Partly checked | The 2019 row equals the 2019 "Actual" column of CBO's February 2024 sheet 3 (12107, 8273.1, 236.9, 248.2, 884.1, 1102.8, 1125.4, 360.9). The 2021 row does not match the June 2024 vintage's 2021 actual: wages 9165 here vs 9022.4, total 14904 vs 14930. The source of 2020–2021 is still unknown. |
| 9 | The repo uses eight of the nine columns, as growth rates only; `txbl_ss` is read but dropped; `ss` grows with CBO Social Security outlays. | CONFIRMED | `src/project_puf.R` lines 64–74 convert every column to year-on-year growth and keep income, wages, int+div, qualified dividends, capital gains, business income, pensions and `ui`, dropping `txbl_ss`. `ss` takes `outlays_mand_oasdi` from Macro-Projections (line 97). Lines 141–163 also read `txbl_kg` levels, but only to compute growth for extending the S&P index. One nuance: of the eight columns used, only seven are from CBO sheet 3; `ui` is from a different CBO table (row 10). |
| 10 | "Thirty-two variables … grow with CBO total income." | CONFIRMED | 32 rows of `config/variable_guide/baseline.csv` have `grow_with = income`. |
| 11 | 2020–2036 growth comes from the CBO, BEA, NIPA and Giving USA series; growth after 2036 comes from Macro-Projections. | OVERSTATED | `rent`, `rent_loss`, `estate` and `estate_loss` grow with Macro-Projections rental income (`gdp_rent`) from 2020, because the near-term table has no rent series (`project_puf.R` lines 123–131). Mortgage interest grows with CBO interest and dividends from 2025, and charity and SALT grow with CBO total income from 2025. |
| 12 | No CBO baseline update since February 2026 (archive of 21 September 2026). | UNCHECKABLE | Not re-checked. cbo.gov returned 403 to direct requests. |

### `ui` column and other resource-file sources

| # | Claim in `02` | Grade | Evidence |
|---|---|---|---|
| 13 | The `ui` column's source is unknown; it "may be CBO fiscal-year unemployment compensation outlays". | WRONG (the source can be identified) | 2025–2036 values (38.735, 44.704, 47.247 … 57.819) equal the "Unemployment compensation" line in CBO February 2026 Budget Projections, Table 3-2 "Baseline Projections of Mandatory Outlays", fiscal years. The 2019–2024 values are probably earlier CBO actuals (not checked). So calendar-year unemployment income on returns is grown with fiscal-year federal outlays. |
| 14 | `ui` does not match NIPA Table 3.12 (2020: 471.5 vs 528.0). | CONFIRMED | Current BEA series A1589C, 2020 = 527,988 million. |
| 15 | Mortgage series is FRED W498RC1A027NBEA; 2017–2020 match exactly; 2024 is 474.7 vs 477.2 now. | CONFIRMED | FRED title "Monetary interest paid: Households: Owner-occupied housing". FRED values 334.815, 350.087, 365.823, 360.719 for 2017–2020 match the file. Later years: 2021 358.9 vs 358.2 in the file, 2022 384.1 vs 383.5, 2023 434.9 vs 433.2, 2024 477.2 vs 474.7. 2025 is 518.5. |
| 16 | SALT is from NIPA Table 3.3 (income plus sales taxes; property taxes); "2017 matches to the decimal"; verified against `shared/raw_data/NIPA/20240125/3-3.csv`. | OVERSTATED | The source is right, but the check is weaker than stated. (a) That local file covers 2009Q1–2020Q4 only, so it cannot verify 2021–2024, and its 2020 average (907.65) does not match the file (909.6). (b) 2017 income plus sales tax is 805.9 in both the local file and current BEA data; the resource file has 805.8. Property tax (535.2) matches. (c) Against current BEA data (series B245RC, B248RC, LA000355), the resource file is an older vintage that drifts away from the current figures. Income plus sales tax: 2023 1137.2 here vs 1106.0 now; 2024 1182.0 vs 1133.6. Property tax: 2023 728.3 vs 697.4; 2024 780.5 vs 765.8. Growth from 2023 to 2024 is +3.9% in the file vs +2.5% now for income plus sales, and +7.2% vs +9.8% for property. The property line also includes business property taxes, which the report says. |
| 17 | Charity is Giving USA individual giving; 2017 = $286.65 billion and 2024 = $392.45 billion. | CONFIRMED for 2024 | The Giving USA press release gives individuals $392.45 billion in 2024. I did not check 2017. The 2018–2023 values in the file (292.09, 309.66, 324.10, 326.87, 319.04, 374.40) look to me like the first-release value from each annual edition, not one consistently revised series. That is from memory and not verified. If true, the year-on-year growth rates mix revisions with real change. |
| 18 | Giving USA 2025 figures: individuals $394.2 billion, total $617.2 billion. | UNCHECKABLE | The figures are behind a paywall. The report itself cites a secondary summary. |
| 19 | PCE file last revised 9 April 2026. | CONFIRMED | File header of `resources/nipa_2_4_5u.csv`. |
| 20 | `sp500_total_return_index.csv` covers 1926–2025. | CONFIRMED | First and last rows. |

### CBO long-term projections

| # | Claim in `02` | Grade | Evidence |
|---|---|---|---|
| 21 | LTBO main workbook has 23 sheets; AGI shares by statutory rate 25/36/31/8 percent (2026) and 18/32/39/11 percent (2056). | CONFIRMED | Archived `62044-2026-LTBO.xlsx`: sheets 1–23 plus contents; sheet "9. Income Tax Rates". The local `CBO-LTBO/v3/20260225/baseline/LTBO-budget.xlsx` is a different, 3-sheet file and does not contain this table. |
| 22 | LTBO economic workbook includes the share of earnings below the taxable maximum, 1996–2056. | CONFIRMED | Local `LTBO-econ.xlsx`, sheet 1, row "Share of earnings below the taxable maximum amount". |
| 23 | CBO's 2026 Long-Term Projections for Social Security, released 17 September 2026. | CONFIRMED | Archived publication page 62556 shows "September 17, 2026". |
| 24 | Contents annually 2026–2100: covered workers by sex; covered earnings, taxable payroll and share below the maximum; OASI and DI beneficiaries; interest rates; GDP and taxable payroll from 1986. | CONFIRMED | `62556-2026-Additional-Info.xlsx` sheets 1–4 and the main workbook's Table 2 (1986–2100). |
| 25 | Values: 2026 $13.58 trillion covered, $11.28 trillion taxable, 83.1 percent; 2097 $166.8 trillion, $134.6 trillion, 80.7 percent. | CONFIRMED | 13.576 / 11.284 / 83.1 and 166.833 / 134.644 / 80.7. |
| 26 | "The only published CBO series that constrains the shape of the wage distribution after 2036"; "the only long-run constraint on the top of the wage distribution". | OVERSTATED | The LTBO economic workbook carries a share below the taxable maximum to 2056 (row 22). In the Social Security file the share is 80.7 percent every year from 2056 to 2100, so after 2056 it adds a constant, not information. The two series also differ: the outlook's share is 81.0 percent in 2056, against 80.7 percent of covered earnings in the Social Security file. |
| 27 | Macro-Projections holds income components after CBO's 10-year window at their average share of GDP over the last three years of the window. | OVERSTATED | True for consumption, investment, government, compensation, wages, proprietors' income, rent, dividends and corporate profits (`sh_vars` in the current GitHub code). Interest (`gdp_interest`) instead grows with GDP times the change in the 10-year rate. Its share of GDP rises from 8.69 percent (2036) to 8.77 percent (2056) in vintage 2026022522. Tax-Data grows interest and mortgage interest with this series. |
| 28 | "I did not find how the 2026 vintage was carried from 2057 to 2099"; the checked-out code names 2023 vintages. | WRONG (answerable) | The local checkout (`f0e94c2`, 1 April 2026) is three commits behind `origin/main` (`00184fc`, 19 July 2026). The current code sets `firstyr_proj = 2026` and `lastyr_proj = 2099`. It has the comment "For years after lastyr_ltbo, extend by assuming that all variables are in long-run steady-state as of lastyr_ltbo", and it repeats the 2056 growth rates. In vintage 2026022522's `projections.csv`, GDP growth is 3.52 percent every year 2056–2099, and every share of GDP is constant after 2056. |
| 29 | Macro-Projections inputs: CBO economic, budget, revenue, long-term and demographic projections; SSA average wage index; CPI from FRED; runs 1970–2099. | CONFIRMED, with one omission | `dependencies.csv` of vintage 2026022522 also lists SSA demographic data from the 2025 Trustees Report (`SSA_Demographic` 20250618). |

### SSA

| # | Claim in `02` | Grade | Evidence |
|---|---|---|---|
| 30 | Wage statistics give brackets of $5,000 to $200,000, then wider brackets up to "$50,000,000.00 and over". | CONFIRMED (2023 page) | Archived `netcomp.cgi?year=2023`: 293 workers in the top bracket in 2023. |
| 31 | 2023 reconciliation: $10.661 trillion subject to income tax, plus $446.0 billion deferrals, gives $11.103 trillion net compensation for 173.67 million earners. | CONFIRMED, slightly simplified | Net compensation also subtracts $3.75 billion of plan distributions included in wages. |
| 32 | "2024 is the latest as of 4 October 2026." | UNSUPPORTED | The only archive captures of `netcomp.cgi?year=2024` (17 April 2026) and of the default page (2 April 2026) read "Data for 2024 is not available." The November 2025 default page still showed 2023. The AWI page (archived 4 October 2026) gives the 2024 index of 69,846.57 and a raw 2024 average of $67,027.24 "based on our tabulated wage data". So the tabulation exists, but I found no published 2024 bracket table. Live ssa.gov returns 403. |
| 33 | 2025 wage statistics expected mid-October 2026. | UNCHECKABLE | The report itself labels this an inference. Given row 32, the timing of the 2024 table is itself unclear. |
| 34 | The repo's life tables are from the 2024 Trustees Report. | CONFIRMED | The header of `PerLifeTables_M_Alt2_TR2024.csv` says "Alternative 2 mortality probabilities used in the 2024 Trustees Report". `ssa_life_table_2022.csv` is used only by a test helper (`make_q_baseline_static_2022`, "For testing only"). Its source is not stated in the file. |
| 35 | 2026 Trustees Report released 9 June 2026; long-run fertility 1.75; 2024 AWI 69,846.57. | AWI CONFIRMED; others UNCHECKABLE | The AWI is confirmed on the archived AWI page. The release date and fertility rate were not checked. |

### BEA

| # | Claim in `02` | Grade | Evidence |
|---|---|---|---|
| 36 | Annual update of 30 September 2026: personal interest +250 / +315 / +421, dividends −116 / −84 / −97, proprietors' income −99 / −107 / −92, wages +3 / +23 / +18 (billions, 2023/2024/2025). | CONFIRMED | BEA SCB article, Table 7: +249.6 / +314.5 / +421.3, −116.0 / −83.8 / −96.9, −99.4 / −107.1 / −92.3, +2.7 / +22.7 / +17.6. The update covers 2021Q1–2026Q1. It uses SOI tax-year 2023 business tabulations, and extrapolations of them for 2024–2025. The interest revision rests mainly on those extrapolations, "informed by Compustat interest expense data", so the $421 billion is itself an extrapolation, not new IRS data. |
| 37 | Monetary interest received by persons $1,266 billion in 2025; monetary dividends $1,931 billion of $2,151 billion; owner-occupied rental income $898.5 billion. | CONFIRMED | Current BEA series B1138C, B1447C, B703RC, A2217C. |
| 38 | Pension benefit payments $1,925.8 billion (all employer plans), $1,037.5 billion (defined contribution plans), 2025. | CONFIRMED | Y235RC and Y353RC ("Benefit payments and withdrawals"). |
| 39 | Misreporting adjustments: Table 7.14 $611.8 billion (2023, last year 2023); Table 7.18 $172.6 billion (2025). | CONFIRMED | B2636C (2023 is the last year) and BA07RC. Table 7.18 compares wages with BLS data, and its adjustment is for misreporting on employment tax returns. |
| 40 | The last BEA personal income vs AGI reconciliation covers 2005 (Ledbetter, November 2007); gap $1,286 billion, 14.8 percent; misreporting explains $508 billion. | CONFIRMED | Article Tables 3–5: 1,285.9; 14.8; gap excluding misreporting 778.0, a difference of 507.9. |
| 41 | "NIPA Table 7.19 is discontinued" (presented as the reconciliation table). | UNSUPPORTED | Table 7.19 is indeed absent from the current register. But the AGI comparison was published as tables inside the SCB articles, and I found no evidence that it was ever NIPA Table 7.19. My recollection, not verified, is that 7.19 was a reconciliation for nonprofit institutions. |
| 42 | Treasury Working Paper 109: IRS AGI is 71.1 percent of adjusted personal income in 2012. | CONFIRMED | Paper text. |
| 43 | Distribution of Personal Income is official Table 2.10; 2024 provisional; experimental 2025 nowcast. | CONFIRMED | BEA register lists T21000; the BEA page describes the 2024 provisional data and the 2025 quintile nowcast. The 16 June release date was not checked. |

### Federal Reserve

| # | Claim in `02` | Grade | Evidence |
|---|---|---|---|
| 44 | DFA income groups: "the published income cutoffs appear to use realized income"; question left unresolved. | CONFIRMED, and resolvable | See summary item 2. The public DFA code builds both realized (`fincpctlecat`) and usual-income (`fnincpctlecat`) groups. The published cutoffs match SCF `INCOME` almost exactly in 2022 and 2019 (2019 after a constant price-level ratio of 0.863 between the extract's dollars and DFA's). They do not match `NORMINC`. The FEDS papers and notes I opened do not define the concept. |
| 45 | Local DFA copy ends 2025Q4. | CONFIRMED | `resources/dfa/raw/*.csv` and `dfa_totals.csv`, last observation 2025:Q4. Raw files are dated 23 April 2026. |
| 46 | Z.1 latest 11 September 2026 (2026Q2), next 10 December; table codes changed (B.101 → S1M.b, B.101.h → S14.b, R.101 → S1M.r). | CONFIRMED | Z.1 page and `z1_table_mapping.csv`. The 11 June date of the change was not checked. |

### JCT

| # | Claim in `02` | Grade | Evidence |
|---|---|---|---|
| 47 | JCX-45-25, 3 December 2025; law as of 31 August 2025; January 2025 CBO baseline. | CONFIRMED | PDF cover, Table 2 note [1], text at "based on the January 2025 Congressional Budget Office revenue baseline". |
| 48 | Table 2: 12 income classes; totals 190.9 million returns, 21.1 million itemizers, $2.204 trillion liability. | CONFIRMED | 190,856 thousand; 21,101 thousand; $2,204,292 million. JCT's own text calls them "nine income classes", but the table shows 12. |
| 49 | Table 3 has "counts and amounts" of SALT, charitable, mortgage and medical deductions by class, a "direct target for simulating who itemizes". | WRONG / OVERSTATED | The amounts are tax expenditures, defined in the report as "the difference between tax liability under present law and … without the provision": SALT $57.5 billion, charity $71.7 billion, mortgage $52.6 billion, medical $9.7 billion. The return counts also do not behave like counts of itemizers. SALT 23,417 thousand and charity 23,417 thousand are identical totals, and both exceed the 21,101 thousand itemizers in Table 2. In the $200,000–$500,000 class there are 10,203 thousand SALT returns against 8,709 thousand itemizers. A usable target would need Tax-Simulator to compute each deduction's tax value, record by record, and the count definition would need to be cleared up with JCT first. |
| 50 | Income classes use JCT expanded income (AGI plus listed items). | CONFIRMED | Table 2 note [2]. |

### Repository and GitHub (caller's item 9)

| # | Claim | Grade | Evidence |
|---|---|---|---|
| 51 | `Macro-Projections/src/macro-projections.R` contains a FRED API key in plain text. (This claim is not in `02`.) | CONFIRMED for the local checkout | Line 13 of the local file (commit `f0e94c2`) passes a literal 32-character key to `fredr_set_key`. The file is tracked in git, and the key was first committed in `790a16a` (29 November 2023). |
| 52 | Is the repository public? | Yes | The remote `Yale-Budget-Lab/Macro-Projections` redirects to `Budget-Lab-Yale/Macro-Projections`. The GitHub API reports `"private": false, "visibility": "public"`. Current `main` (since `17b8258`, 15 May 2026) reads the key from a file outside the repository (`parse_api_keys(...)`), so the current public file has no key. Older commits remain public: the raw file at commit `f0e94c2` on GitHub still contains the literal key. Removing it from the current file does not remove it from history. The key should be revoked and reissued. |

### Not checked

- JCX-37-25, JCX-48-23 and the JCT slides.
- Treasury distribution tables and family counts.
- CBO household-income report.
- Realtime Inequality and Auten–Splinter.
- BLS, Census, CMS, New York Fed and private-tracker release dates and figures.

## Omissions that matter

- **CBO sheet 3 has no itemized deductions by type.** Only the total after
  limits is shown. After 2025 there is no CBO projection of SALT, mortgage or
  charitable deductions separately. The multi-year figures by deduction that I
  know of are the JCT (fiscal years 2025–2029) and Treasury (2025–2035)
  tax-expenditure tables, and both are in tax-saving units.
- **DFA format changes affect a refresh.** In June 2026 the Fed relabelled
  "Private businesses" as "Unincorporated businesses". In September 2026 it
  added hedge fund and private debt fund shares to the household balance
  sheet. `resources/dfa/dfa_schema_crosswalk.csv` maps by DFA column label,
  so the recommended refresh needs the crosswalk checked first.
- **Tax-Data ages mortgage balances with interest paid.** In
  `config/variable_guide/baseline.csv`, `first_mort_bal` and `second_mort_bal` grow with the
  `mortgage` series, which is interest paid. Interest paid grows with rates as
  well as balances, so the balances inherit rate changes. The report lists the
  use but does not flag the mismatch.
- **The resource files mix vintages within one series.** This holds for SALT
  and mortgage (the BEA vintage changes partway through), probably for
  Giving USA, and for `cbo_1040.csv` (2019 from the February 2024 vintage,
  2022 onward from February 2026). Year-on-year growth at each join mixes
  data revisions with real change.

## Wording problems for John

- Summary item 6 should say "tax-expenditure amounts", not "counts and
  amounts of … deductions".
- Summary item 7 and the "Looked for and not found" list should drop `ui`.
- The DFA "unresolved question" should be closed.
- Summary item 2's "fixed shares of GDP" should mention the interest
  exception.
