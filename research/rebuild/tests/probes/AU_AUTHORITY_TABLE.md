---
title: "Test 4, probe AU: the authority table (joint model plan, step 0) — draft for review"
role: plan
workstream: rebuild
status: draft
updated: 2026-10-10
sot: research/rebuild/joint_model_plan.md
supersedes: []
superseded_by: null
---

# The authority table: draft

This is step 0 of `research/rebuild/joint_model_plan.md`: for every variable
in the joint model, the source that owns its concept, the exact concept, how
other sources' variables of the same name differ, and what D24's translation
does to it. It is a draft for John to review; the plan itself is unchanged.

**What was done.** Files were read; no jobs were run. Every number below is
quoted from a file named in the same cell. The 2015 Form 1040, Schedule A,
D and E line numbers are from the 2015 forms as I know them; the PUF codebook
and the SCF codebook are not on disk, so the line numbers of the Schedule E
pieces and a few SCF summary-file definitions (marked "to verify") have not
been checked against the documentation.

**Conventions in the tables.**

- "PUF" means the 2015 PUF without its four aggregate records and without
  dependent filers (141.6 million returns), the population the SCF's filer
  units correspond to, unless a cell says "PUF all".
- "SCF" means the SCF filer tax units built by test 3's builder
  (`research/rebuild/tests/posttrain/g_scf_tax_units.R`, a copy of
  `src/imputations/stage1_scf_tax_units.R`): 132.3 million units in the 2013
  wave (tax year 2012), 140.2 million in the 2016 wave (tax year 2015).
  SCF 2013 dollars are raised to 2015 by the CPI-U in the comparisons.
- Shares nonzero and totals for the 17 shared variables come from
  `/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/full_v2/g_tables.md`
  (written by `../posttrain/g_tables.py`), cited as `g_tables.md`.
- "ASEC" means the CPS Annual Social and Economic Supplement. Its comparisons
  with SOI are for tax year 2022, from
  `research/state_weights/nonfiler_residual/10_asec_tax_unit_design.md`
  (cited as `10_asec_tax_unit_design.md`).
- "D24 step" refers to the translation chain set out in the section "The
  order of D24's chain" below. "None" means the variable is not shared with
  any survey now in the model, so nothing is translated.

## Filing and return variables

| Variable | Authority and why | Concept in the file | Same name in other sources, and how they differ | D24 rule and step |
|---|---|---|---|---|
| `MARS` filing status | IRS. Filing status is a legal choice made on the return; no survey observes it directly. | Form 1040 lines 1–5: 1 single, 2 married filing jointly (SOI also counts qualifying surviving spouses here; to verify for the PUF), 3 married filing separately, 4 head of household. | **SCF**: the builder's status combines marital status (X8023), the reported filing answer (X5744 filed, X5746 jointly or separately) and children: married couples filing jointly or with one spouse filing stay one joint unit; couples filing separately and unmarried partners become two units; an unmarried unit with children is head of household (`g_scf_tax_units.R`, lines 285–395). Shares single / joint / separate / head: PUF 43.8 / 38.4 / 2.1 / 15.8%; SCF 2013 41.9 / 40.8 / 2.2 / 15.1%; SCF 2016 44.9 / 39.1 / 2.7 / 13.2% (`g_tables.md`). **ASEC**: Census's tax-model status (`FILESTAT`) has head of household at 0.585 of SOI and no separate filers in tax year 2022; the repo's ASEC builder applies the legal rule and meets the same shortfall (`10_asec_tax_unit_design.md`, sections 2.2 and D-A6). Marital status (married, cohabiting, separated) is a different variable; see "Demographics". | Step 1. Probabilistic crosswalk from the survey's status and marital status to filing status, within age group and children, set so the translated mix matches the PUF. Main moves: SCF joint to separate (few), SCF single to head of household for unmarried parents and for units with older or non-child dependents. |
| `DSI` dependent filer | IRS. Whether someone else can claim the filer is a tax-law status on the return. | The return's indicator that the filer can be claimed as a dependent on another return. 6.1% of PUF all returns, 9.3 million (`g_tables.md`, `G_DATA_REPORT.md`). | **SCF**: 0 for every unit; the builder never makes a financially dependent person a unit, so the SCF filer population has no dependent filers (`g_crosswalk.md`). **ASEC**: the repo's builder constructs dependent-headed units for dependents aged 15 and over and tags them (`10_asec_tax_unit_design.md`, D-A2 rule 5, D-A4). | Not translated: a coverage difference (D23 step 2). The SCF is in scope only for non-dependent filers; dependent filers are learned from the PUF (and later the ASEC). See open question 5. |
| `XTOT` total exemptions | IRS for the count claimed; the ages and relationships behind it are Census's (see dependents below). | Form 1040 line 6d: filer, spouse on a joint return, and dependents on line 6c; 0 for dependent filers; top-coded at 5. | **SCF**: 1, plus 1 if joint, plus children under 19 in the household, plus one if the household supports a child living elsewhere; split between the units of a split couple (`g_crosswalk.md`). The PUF also counts students aged 19–23, parents and other relatives. Shares 1 / 2 / 3 / 4 / 5+: PUF 43.4 / 29.5 / 12.5 / 9.9 / 4.7%; SCF 2016 46.6 / 29.6 / 7.7 / 10.4 / 5.7% (`g_tables.md`). Dependents 0 / 1 / 2 / 3+: PUF 62.5 / 17.3 / 13.1 / 7.0%; SCF 2016 69.2 / 12.8 / 8.8 / 9.3% (`a_crosswalk.md`). **ASEC**: the repo's builder applies the legal tests (qualifying child under 19, or under 24 if a full-time student; qualifying relative with the income limit and a support test) (`src/nonfilers/asec_tax_units.R`). | Step 3, within filing status and age group. First by definition: rebuild SCF dependents from the household roster with the legal tests where the roster allows (ages and relationships are there; school enrollment is not). Then a crosswalk for what remains. |
| `N24` children for the child tax credit | IRS for the count claimed; children's ages are Census's. | Number of children on line 6c checked as qualifying for the child tax credit (under 17 at year end), top-coded at 3. | **SCF**: children under 17 in the unit (the builder's count, never above its dependents), top-coded at 3; split-off units 0. Shares 0 / 1 / 2 / 3+: PUF 71.2 / 14.4 / 10.0 / 4.4%; SCF 2016 77.9 / 12.7 / 7.0 / 2.5% (`g_tables.md`). SCF ages are at the interview, a year after the tax year, so some children counted under 17 on the return are 17 in the SCF. | Step 3, with `XTOT`; the count of children under 17 should not exceed dependents after translation (the builder already enforces this: `G_DATA_REPORT.md`). |
| `EIC` children for the earned income credit | IRS. Defined only through the credit claim. | Number of qualifying children on Schedule EIC (0–3), recorded for returns that claim the credit. | No survey measures it. ASEC's `EITCRED` is a Census tax-model output and reaches 61% of SOI dollars (`10_asec_tax_unit_design.md`, section 2.6). | None. Learned from the PUF. |
| `FDED` form of deduction | IRS. A choice on the return. | 1 itemized (Schedule A), 2 standard deduction, 3 neither. | No survey. | None. Learned from the PUF. It decides whether the itemized-deduction amounts below are observed (see the table of missing values). |

## Demographics

The plan names Census as the authority for age, sex and household makeup.
The PUF's demographic file (SOI's, from Social Security records) is a fact
about the same people for 119,675 of 207,692 records (`g_tables.md`,
observed-rows table); where it exists it is the measurement of the file's
own records, so the proposal is: **the PUF demographic file where present,
Census tables as the targets for the whole file, and Census microdata
(with the SCF for top incomes) as what the model learns from where the PUF
has nothing.**

| Variable | Authority and why | Concept in the file | Same name in other sources, and how they differ | D24 rule and step |
|---|---|---|---|---|
| `AGERANGE`, then single-year age of the primary filer | Census for the population by age; SOI Table 1.6 for returns by age, filing status and AGI; the PUF demographic file for the records it covers. | Age of the primary filer (the first person named on the return) in years. The PUF gives six ranges (under 26, 26–34, 35–44, 45–54, 55–64, 65 and over; codes 1–3 mean other ranges for dependent filers) from Social Security records; the reference date is probably 31 December 2015, to verify in the demographic file's booklet. Tax-Simulator needs single years (`age1`; `src/imputations/ages.R` draws them from the CPS within the range). | **SCF**: age of the unit's head at the interview, about one year after the tax year; in a mixed-sex couple the SCF's head is usually the man (`g_crosswalk.md`, `a_crosswalk.md`). Shares under 26 / 26–34 / 35–44 / 45–54 / 55–64 / 65+: PUF 12.1 / 19.3 / 17.9 / 17.5 / 16.1 / 17.1%; SCF 2013 11.7 / 16.8 / 18.3 / 18.9 / 16.8 / 17.6%; SCF 2016 13.9 / 15.9 / 17.1 / 17.7 / 16.5 / 19.0% (`g_tables.md`; PUF among records with an age). **ASEC**: age at the March interview; `ages.R` takes the householder as the primary person, which is not the return's primary filer. **ACS**: age at an interview spread over the calendar year. **Census population estimates**: age on 1 July. | Step 2, within filing status: subtract a year from SCF ages (interview date), then for joint units choose which spouse is the primary filer (this moves age and sex together); what remains is a near-identity rank map within filing status. |
| Age of the spouse | Census. The PUF has no spouse age. | Age of the second person on a joint return. | **SCF**: spouse's age (X19), at the interview. **ASEC/ACS**: age of the householder's spouse; `ages.R` draws it from the ASEC distribution of spouse age given householder age. | Step 2, with the primary filer's age (the two are one draw for a couple). |
| `GENDER` sex of the primary filer | Census for the population by sex; the PUF demographic file for the records it covers. | Sex of the primary filer, from Social Security records. Tax-Simulator's `male1`, `male2` (`src/imputations/demographics.R`). | **SCF**: sex of the unit's head (X8021; X103 for a spouse's separate unit); unobserved for split-off units. Male: PUF 62.8%, SCF 2013 67.9%, SCF 2016 68.7% (`g_tables.md`). The gap is mostly who is listed first on a joint return (`G_DATA_REPORT.md`). Production fills missing sex from DINA's sex split for unmarried units (`demographics.R`). | Step 2, jointly with age: for joint units, the choice of primary filer. For other units, identity. |
| Sex of the spouse | Census. | Sex of the second person on a joint return. | Production assumes 1% of marriages are same-sex (`demographics.R`). The SCF and ASEC observe it. | Step 2. |
| `EARNSPLIT` earnings split on joint returns | IRS (SOI demographic file, from W-2s). | Banded share of the couple's earnings earned by the primary filer; 0 for non-joint returns. The band meanings are not confirmed (production reads codes 1, 2, 3 as the primary earning all, about half, none: `g_crosswalk.md`). | **SCF**: each spouse's wages from the main job; not used yet (`g_crosswalk.md`, open question 3). **ASEC**: each spouse's wages. Production's per-spouse wage split comes from Saez's W-2 tabulations (`resources/wagesplit.csv`, `src/imputations/earnings_split.R`). | Not translated now. If the SCF observes it later: step 4, after wages, by definition from the spouses' wages once the bands are confirmed. |
| Dependents by age | IRS for who is claimed (count and type: `XOCAH` children at home, `XOCAWH` children away, `XOPAR` parents, `XOODEP` other dependents); the PUF demographic file for age ranges (`AGEDP1`–`AGEDP3`: under 5, 5–12, 13–16, 17–18, 19–23, 24 and over, per `ages.R`); Census for ages where the PUF has none and for population totals. | Ages of up to three dependents claimed on the return. Not in the 40-variable model yet. | **SCF**: ages of children in the household roster; children under 19 only. **ASEC/ACS**: everyone's age and relationship; dependents only as constructed by a unit builder. | Step 3, with `XTOT`. |
| Marital status | Census. | Not a variable of the tax file now. Needed only as the bridge from surveys to filing status, and if Tax-Simulator ever needs cohabitation or legal marital status (for example, marriage penalties among couples filing as single or head of household). | **SCF**: X8023 (married, living with partner, separated, divorced, widowed, never married). **ASEC/ACS**: marital status and unmarried-partner relationship; 9.27 million unmarried partners in tax year 2022 (`10_asec_tax_unit_design.md`, section 2.9). | Input to step 1, not itself translated. See open question 2. |
| Household makeup | Census. | Not a variable of the file now. Candidate concept: for each tax unit, the household it lives in (number of adults and units, relationship to the householder). The SCF's wealth is a household amount split between units by our rule (see "Wealth"), so it matters for joining SCF wealth to PUF units. | **SCF**: the primary economic unit (the core family) and split-off units (financially independent adults; 13% of 2016 filer units, wealth unobserved: `a_crosswalk.md`). **ASEC**: households, families, subfamilies; 26.9% of people live in a household with a relationship the unit rules treat differently (`10_asec_tax_unit_design.md`, section 2.9). The PUF has no household. | Not translated. See open question 8. |
| Blindness | IRS. | Whether the filer and spouse claim the additional standard deduction for blindness. Production imputes it from the count of such deductions (`demographics.R`). | No survey in the model. | None; listed because Tax-Simulator uses it. |

## Incomes

| Variable | Authority and why | Concept in the file | Same name in other sources, and how they differ | D24 rule and step |
|---|---|---|---|---|
| `E00100` AGI | IRS. A tax-law construct. | Form 1040 line 37. | **SCF**: X5729 total income includes untaxed Social Security and transfers and is before adjustments; not used (`g_crosswalk.md`). **ASEC**: `ADJGINC` is a Census tax-model output statistically matched to the PUF itself; never an input (`10_asec_tax_unit_design.md`, section 2.5). | None. If a survey supplies all components later, build AGI from the translated components rather than mapping the survey's total. |
| `E00200` wages | IRS (W-2 box 1 as reported). | Form 1040 line 7. Excludes elective deferrals to 401(k)-type plans and pre-tax health premiums. | **SCF**: X5702, wages before deductions (probably includes elective deferrals; not sized). Share nonzero PUF 82.2%, SCF 2013 80.4%, SCF 2016 79.9%; totals $7,086bn, $6,948bn, $7,941bn (`g_tables.md`). **ASEC**: `INCWAGE`, earnings before deductions; 1.116 times SOI and 1.011 times SSA covered wages in tax year 2022 (`10_asec_tax_unit_design.md`, section 2.6). | Step 4, within filing status by age group. Definition first if the deferral can be estimated (the SCF asks contribution rates); then rank map. Few zero flips needed. |
| `E00900` Schedule C | IRS. | Form 1040 line 12, Schedule C net profit or loss. | **SCF**: X5704 is Schedule C and F together; set to 0 for farm-only households. Nonzero PUF 17.2%, SCF 2013 8.6%, SCF 2016 11.3%; losses 4.1% against 0.9% and 0.8%; totals $331bn, $393bn, $568bn (`g_tables.md`). Part of the SCF total is likely S-corporation or partnership income reported as business (`a_crosswalk.md`). The SCF masks some losses (-9 codes). **ASEC**: `INCBUS`. | Step 6: rank with zeros, zero flips chosen by likelihood (the SCF has a third to a half fewer holders than the PUF). Consider translating Schedule C, farm and Schedule E as a block (open question 9). |
| `E02000` total Schedule E | IRS. | Form 1040 line 17: rents, royalties, partnerships, S corporations, estates and trusts (Schedule E line 41). On the PUF its parts sum to $700bn against $704bn (`a_crosswalk.md`). | **SCF**: X5714, the same line by the SCF's wording, -9 masked losses kept as small losses. Nonzero PUF 12.4% (4.4% losses), SCF 2013 9.6% (1.0%), SCF 2016 10.2% (1.2%); totals $704bn, $980bn, $1,142bn (`g_tables.md`). **ASEC**: `INCRENT` covers rent, royalties, estates and trusts, not partnerships or S corporations. | Step 6, after Schedule C and farm. Loss flips are a large share of the moves (SCF losses are masked and rare). |
| `E26270` partnership and S corporation | IRS. | Schedule E Part II total (line 32). | None separately: inside SCF X5714. | None. Learned from the PUF given total Schedule E. |
| `E25850`, `E25860` rent and royalty income, loss | IRS. | Schedule E Part I, net income and net loss of rental real estate and royalties (lines 24 and 25 or 26; to verify in the codebook). Loss as a positive number. | Inside SCF X5714 and ASEC `INCRENT`. | None. |
| `E26390`, `E26400` estate and trust income, loss | IRS. | Schedule E Part III, estate and trust net income and loss (to verify the lines). | Inside SCF X5714 and ASEC `INCRENT`. | None. |
| `E02100` farm | IRS. | Form 1040 line 18, Schedule F net profit or loss. Net total -$15bn; 1.3% nonzero, 0.9% losses (`g_tables.md`). | **SCF**: X5704 for households that filed Schedule F but not C (X5749, X5747); unobserved for households filing both. Totals $17bn (2013), $8bn (2016); nonzero 0.5% and 0.4% (`g_tables.md`). **ASEC**: `INCFARM`. | Step 6, before Schedule C (the farm-only flag decides which line an SCF amount sits on). |
| `P23250` long-term gains | IRS. | Schedule D line 15, net long-term gain or loss, including carryover losses and capital gain distributions when Schedule D is filed; before the $3,000 loss limit. | **SCF**: X5712, net gains or losses of all holding periods including fund distributions, so it holds the PUF's long- and short-term gains and line-13 distributions together. Nonzero PUF 11.2%, SCF 2013 7.6%, SCF 2016 8.3%; totals $634bn, $494bn, $678bn (`g_tables.md`). **ASEC**: a capital gains item from 2014, and Census tax-model `CAPGAIN`/`CAPLOSS` (`10_asec_tax_unit_design.md`, D-A5). | Step 7. Definition first: the SCF amount is compared with a PUF sum (long- plus short-term plus `E01100`); test 2 used that sum, `full_v2` uses long-term alone (`a_crosswalk.md` against `g_crosswalk.md`). Then rank with zeros and likelihood flips. |
| `P22250` short-term gains | IRS. | Schedule D line 7, net short-term gain or loss, including carryover losses (the variable guide calls it a "carryover", which is only part of it). | Inside SCF X5712. | None. |
| `E01100` capital gain distributions | IRS. | Distributions reported directly on Form 1040 line 13 by filers not required to file Schedule D. Filers of Schedule D report theirs inside `P23250`. | Inside SCF X5712. | None. |
| `E00600` ordinary dividends | IRS (1099-DIV). | Form 1040 line 9a. | **SCF**: X5710. Nonzero PUF 18.9%, SCF 2013 12.0%, SCF 2016 11.9%; totals $238bn, $210bn, $250bn (`g_tables.md`). **ASEC**: `INCDIVID`, 0.449 of SOI dollars on a wider population (`10_asec_tax_unit_design.md`, section 2.6). | Step 7: rank with zeros; SCF zeros flipped to holders by likelihood given the whole row, wealth included (an SCF unit with stocks and no dividends). |
| `E00650` qualified dividends | IRS. | Form 1040 line 9b; part of `E00600`. | No survey separates it. | None. |
| `E00300` taxable interest | IRS (1099-INT). | Form 1040 line 8a. | **SCF**: X5708, interest other than tax-exempt. Nonzero PUF 29.6%, SCF 2013 16.3%, SCF 2016 12.4%; totals $88bn, $91bn, $90bn (`g_tables.md`). The worst-disagreeing target family in test 3 (`../posttrain/STATUS.md`, 9 October summary). **ASEC**: `INCINT`, 2.6 times SOI's taxable interest in tax year 2022 (all interest received; `10_asec_tax_unit_design.md`). | Step 7, the largest zero-to-positive translation: the SCF has 45% (2013) to 58% (2016) fewer holders than the PUF. |
| `E00400` tax-exempt interest | IRS. | Form 1040 line 8b. | **SCF**: X5706; split-off units 0 (not asked). Nonzero PUF 4.1%, SCF 2.3% and 2.2%; totals $60bn, $67bn, $81bn (`g_tables.md`). **ASEC**: not separate. | Step 7, after taxable interest. |
| `E02400` Social Security | IRS (SSA-1099), for filers only. | Form 1040 line 20a, gross benefits. Recipients who do not file are outside the PUF. | **SCF**: current monthly benefit at the interview, annualized (X5306/X5307, X5311/X5312), each person's to their own unit. Nonzero PUF 19.7%, SCF 20.6% and 21.6%; totals $603bn, $552bn, $654bn (`g_tables.md`). A current benefit stands for last year's. **ASEC**: `INCSS`. | Step 5, within filing status and age: rank map; few flips (benefits that began during the survey year are the main source of SCF positives that should be zero). |
| `E01700` taxable pensions | IRS. | Form 1040 line 16b, taxable pensions and annuities (excludes rollovers and the tax-free part). | **SCF**: X5722 (Social Security, pensions, annuities) less the current Social Security annualized, floored at zero: a gross pension amount. 31% of Social Security recipients report a current benefit above their X5722, and are floored (`G_DATA_REPORT.md`). Nonzero PUF 20.0%, SCF 19.6% and 20.5%; totals $692bn, $549bn, $663bn (`g_tables.md`). **ASEC**: `INCRETIR` and its 2019 split (`10_asec_tax_unit_design.md`, section 2.7). | Step 5, after Social Security. Rank map; gross against taxable is a level difference the rank absorbs, except for the floored records. |
| `E01500` total pensions | IRS. | Form 1040 line 16a, including direct rollovers between accounts. $1,178bn (`g_tables.md`). | Not used for the SCF: rollovers are not income in the SCF's sense (`a_crosswalk.md`). | None. |
| `E01400` taxable IRA distributions | IRS. | Form 1040 line 15b. $251bn, 9.4% nonzero (`g_tables.md`). | **SCF**: withdrawals from IRAs and account-type plans are asked separately but the builder's amount mixes plan types; not used (`g_crosswalk.md`). **ASEC**: `INCRET1`/`INCRET2` from tax year 2018. | None now; a candidate shared variable (`a_crosswalk.md`, open question 1), linked to the `dc` wealth variable. |
| `E02300` unemployment | IRS (1099-G). | Form 1040 line 19. Nonzero 4.3%, $27bn (`g_tables.md`). | **SCF**: X5716 is unemployment and workers' compensation together. Nonzero 6.7% (2013) and 3.1% (2016); $59bn and $20bn; 2012 was a high-unemployment year (`g_tables.md`, `G_DATA_REPORT.md`). **ASEC**: `INCUNEMP`, with workers' compensation separate. | Step 8 (last): rank with zeros; the SCF's workers' compensation recipients are the zeros to remove, chosen by likelihood. |

## Adjustments

| Variable | Authority and why | Concept in the file | Same name in other sources, and how they differ | D24 rule and step |
|---|---|---|---|---|
| `E03150` deductible IRA contributions | IRS. | Form 1040 line 32, the deductible amount after the income and plan-coverage limits. | No survey in the model. The SCF has IRA balances, not contributions. | None. |
| `E03210` student loan interest deduction | IRS. | Form 1040 line 33, capped at $2,500 and phased out by income; not the interest paid. | **SCF**: student loan balances and payments (inside `installment_debt`), not deductible interest (`g_crosswalk.md`). | None now. A possible bridge later: interest paid is the underlying amount. |
| `E03270` self-employed health insurance | IRS. | Form 1040 line 29. | No survey in the model. | None. |

## Itemized deductions

All six are recorded on the PUF only for returns that itemized in 2015
(`FDED` = 1); for everyone else the PUF holds zero, which means "not
reported", not "no expense" (D20).

| Variable | Authority and why | Concept in the file | Same name in other sources, and how they differ | D24 rule and step |
|---|---|---|---|---|
| `E17500` medical | IRS for itemizers; no authority for the underlying expense of others. | Schedule A line 1, medical and dental expenses before the AGI floor, as `src/process_puf.R` treats it (it computes line 4 from it); D20 notes this is unchecked. | No survey in the model. Candidates: MEPS, CEX (D20). | None. Underlying expense for non-itemizers: see the missing-values table. |
| `E18400` state and local income or sales tax | IRS for itemizers. | Schedule A line 5. | No survey in the model. Production has a DINA-based state and local tax rate (`src/imputations/state_local_tax.R`). | None. |
| `E18500` real estate tax | IRS for itemizers. | Schedule A line 6. | **SCF**: X721, property tax on the home, for every owner; not used because itemizing is unknown in the SCF (`g_crosswalk.md`). **ACS**: annual real estate taxes, for owner-occupied homes. Probe BR is testing it as a bridge variable. | Not yet. As a bridge: SCF and ACS amounts are the underlying tax; compare with the PUF only among records the model classes as itemizers. |
| `E19200` interest paid | IRS for itemizers. | Schedule A line 15: home mortgage interest and points, mortgage insurance premiums, investment interest. | **SCF**: mortgage balance, rate and payment for everyone (X805 and related items); interest is computed, not asked. **ACS**: mortgage payment, not interest. Probe BR is testing it. | Same as real estate tax. |
| `E19800` cash charity | IRS for itemizers. | Schedule A line 16. | Not in the SCF 2013 or 2016 (`g_crosswalk.md`). Candidates: CEX, PSID (D20). | None. |
| `E20100` non-cash charity | IRS for itemizers. | Schedule A line 17. | As above. | None. |

## Wealth

**Authority: the SCF for all 24**, because it is the only source that
measures each item of the household balance sheet for a sample that reaches
the top of the distribution (below the Forbes 400, which it excludes). The
Financial Accounts of the United States measure totals for households and
nonprofits together; the Distributional Financial Accounts spread those
totals using the SCF. The concept is the SCF's own: the Federal Reserve's
summary-file variables, measured at the interview date (2013 or 2016), in
the builder's mapping (`src/imputations/stage1_scf_tax_units.R`, lines
692–715). Totals and shares nonzero for both waves are in `g_tables.md`.

Two choices of ours sit inside every wealth variable. First, SCF wealth is a
household amount; for a couple split into two units, housing items are
divided by a housing share and everything else by the spouses' shares of
earnings (`stage1_scf_tax_units.R`, lines 640–655), and split-off units get
no wealth (unobserved). Second, the Forbes 400 are added from Forbes, a
coverage step. **D24 rule for every wealth variable: none.** Wealth is not
shared, so it keeps the SCF's concept; translation changes the incomes and
demographics it is conditioned on, never the wealth.

| Variable | SCF definition (summary-file items) | Other sources and how they differ; tax items that measure a flow from it |
|---|---|---|
| `cash` | `LIQ` (checking, savings, money market accounts and funds, call accounts) + `CDS` (certificates of deposit) | Financial Accounts deposits include nonprofits. Flow: taxable interest `E00300`. |
| `equities` | `STOCKS` (directly held) + `STMUTF` (stock funds) + `COMUTF` (combination funds). Excludes stock held in retirement accounts and managed accounts. | Flow: dividends `E00600`, `E00650`; gains. |
| `bonds` | `BOND` + `SAVBND` (savings bonds) + `TFBMUTF`, `GBMUTF`, `OBMUTF` (bond funds) | Flow: `E00300`, tax-exempt interest `E00400`. |
| `dc` | `IRAKH` (IRAs and Keoghs) + `THRIFT` (account-type plans on current jobs) | SOI's IRA statistics (Form 5498 values) cover IRAs only. Flows: `E01400`, `E03150`. |
| `db` | `FUTPEN` + `CURRPEN`. As I understand the Federal Reserve's definitions these are balances of account-type plans from past jobs and of accounts now paying out, not defined-benefit pension values, which the SCF's net worth leaves out (to verify). | Flow: pensions `E01500`, `E01700`. See open question 6. |
| `life_ins` | `CASHLI`, cash value of whole life insurance | Financial Accounts life insurance reserves. |
| `annuities` | `ANNUIT`, equity in annuities | Flow: part of `E01500`, `E01700`. |
| `trusts` | `TRUSTS`, trusts and managed investment accounts with an equity interest | Flow: estate and trust income `E26390`. |
| `other_fin` | `OTHFIN` (loans owed to the household, royalties, futures, other) + `OMUTF` (other funds) | |
| `pass_throughs` | `BUS`, value of privately held businesses, actively and not actively managed | Flows: `E00900`, `E26270`, `E02100`. |
| `primary_home` | `HOUSES`, value of the primary residence | ACS home value (owner-occupied, self-reported). Flows: real estate tax `E18500`, mortgage interest in `E19200` (itemizers). |
| `other_home` | `ORESRE`, other residential real estate (second homes, residential rentals), gross value | Flow: rents `E25850`, `E25860`. |
| `re_fund` | `NNRESRE`, nonresidential real estate; as I understand it, net of the debt on it (to verify). The name suggests a fund; it is property. | Flow: rents. See open question 7. |
| `other_nonfin` | `VEHIC` (vehicles) + `OTHNFIN` (art, jewelry, other) | |
| `primary_mortgage` | `MRTHEL`, mortgages and home equity loans and lines on the primary residence | Credit bureau data, Financial Accounts. Flow: `E19200`. |
| `other_mortgage` | `RESDBT`, debt on other residential property | |
| `credit_lines` | `OTHLOC`, other lines of credit | |
| `credit_cards` | `CCBAL`, card balances after the last payment | Credit bureau balances include amounts paid off monthly. |
| `installment_debt` | `INSTALL`, vehicle, education and other installment loans | Flow: student loan interest `E03210` (deduction). |
| `other_debt` | `ODEBT`, loans against pensions or life insurance, margin loans, other | |
| `kg_primary_home` | `KGHOUSE`, unrealized gain on the primary residence | |
| `kg_other_re` | `KGORE`, unrealized gain on other real estate | |
| `kg_pass_throughs` | `KGBUS`, unrealized gain on businesses | |
| `kg_other` | `KGSTMF`, unrealized gain on stocks and mutual funds | Flow: realized gains `P23250`, `P22250`, `E01100`. |

## Variables the PUF lacks or has for only some records

| Variable | What is missing on the PUF | Where the model learns it | Post-training targets |
|---|---|---|---|
| Age range, sex, earnings split (`AGERANGE`, `GENDER`, `EARNSPLIT`) | All 88,017 records outside the demographic file, every record with weight under 200, the high-income strata (`../puf_generator/DATA_REPORT.md`); plus 252 records with age range coded "not present" (`g_tables.md`). Dependent filers have coarser age codes. | PUF records below the cutoff (age given income, extrapolated upward); the SCF, which has ages at every income; the ASEC and ACS later (incomes top-coded, so weak at the top). Generation must draw demographics after incomes (plan, next step 4). | Age: SOI Pub 1304 Table 1.6, returns by age, filing status and AGI, which covers the top AGI classes (already read by `src/process_puf.R` to impute missing age groups); Census or Social Security area population by age for filers and non-filers together (the current partition in `src/project_puf.R`). Sex: Census population by age and sex; no SOI table by sex and AGI is in the repo. Earnings split: Saez's W-2 tabulations by wage percentile (`resources/wagesplit.csv`). |
| Single-year age of the primary filer | Every record (the PUF has ranges). | ASEC or ACS age within range, given filing status and income. | Census population by single year of age; Table 1.6 by range. |
| Spouse age and sex | Every joint return. | ASEC or ACS couples; the SCF. | Census couples by both ages (ASEC family tables). |
| Dependents' ages (`AGEDP1`–`AGEDP3`) | Records outside the demographic file; dependents beyond three. | PUF records with ages; ASEC dependents built by the legal rules. | Census children by age net of children in non-filer units; SOI counts of returns claiming the child tax credit. |
| Itemized deductions (`E17500`–`E20100`) | Non-itemizers (`FDED` 2 or 3): the PUF holds zero for an expense that is unreported. Itemizers' amounts are observed. | Bridge variables (SCF and ACS property tax and mortgage, probe BR); later CEX and PSID for charity, MEPS for medical; the DINA-based rate for state and local tax (D20). Constraint: amounts below the standard deduction for most non-itemizers. | SOI Table 2.1 for itemizers (observed, so the model must reproduce it); for everyone, candidate totals: ACS property taxes paid by owners, Census state and local government income tax revenue, charitable giving totals, MEPS out-of-pocket spending. |
| Adjustments (`E03150`, `E03210`, `E03270`) | Underlying amounts (contributions, interest paid, premiums) above the limits or for people who did not claim. | Not needed unless law changes the limits; then SCF (student loans), others. | SOI line-item totals. |
| `E01100` capital gain distributions | Present only for returns without Schedule D; for others they sit inside `P23250`. | PUF only. | None separate. |
| `EIC` qualifying children | Only for credit claimers; children of eligible non-claimers are not counted. | PUF. | SOI EITC counts by number of children. |
| Social Security and other incomes of non-filers | Non-filers are not on the PUF. | The ASEC non-filer pool (`research/state_weights/`), a coverage addition. | Social Security Administration benefit totals; the non-filer anchors. |
| Wealth (all 24) | Every PUF record. | SCF filer units; the Forbes list for the top. | D23: SCF shares within groups times the file's group totals; SCF totals for all U.S. units with the Forbes adjustment. |

## The order of D24's chain

D24 fixes the first four steps; the order of the remaining incomes is a
proposal.

0. **Coverage, before translation** (D23 step 2, reweighting allowed): SCF
   filers only; no SCF dependent filers (learned from the PUF); the Forbes
   400 added.
1. **Filing status**, by crosswalk from the SCF builder's status and
   marital status, within age group and children.
2. **Age and sex of the primary filer and spouse**, within filing status:
   remove the SCF's one-year interview lag; for joint units choose which
   spouse is primary (moves age and sex together); then a near-identity map.
3. **Dependents** (`XTOT`, `N24`), within filing status and age: rebuild
   from the roster by the legal tests where possible, then crosswalk.
4. **Wages**, by rank within filing status by age group; deferral
   arithmetic first if possible.
5. **Social Security, then taxable pensions**: closely matched in both
   sources (shares nonzero within 2 points), tied to age; small moves,
   and they sharpen the cells for later steps.
6. **Farm, Schedule C, total Schedule E**: farm first because the farm-only
   flag decides which line SCF business income sits on; these three
   exchange income by misclassification, so they go together.
7. **Taxable interest, dividends, tax-exempt interest, capital gains**: the
   largest zero-to-positive moves, chosen by likelihood given the whole row,
   wealth included; last among the large items so their cells hold all
   translated incomes.
8. **Unemployment**: small; removes workers' compensation.

Cells must stay coarse: about 6,000 SCF households per wave (D24).

## Open questions

1. **Dependents: tax-return dependents or household children?** The file's
   concept is dependents claimed on the return (`XTOT`, `N24`, the PUF's
   dependent types). The SCF counts children under 19 in the household plus
   one supported elsewhere; the ASEC builder applies the legal tests. Should
   the SCF's dependents be rebuilt from its roster by the ASEC builder's
   legal rules before any crosswalk, and who owns dependents' ages where the
   PUF has none?
2. **Filing status against marital status for cohabiting couples.** In tax
   terms they file as two single or head-of-household returns and one
   partner claims the children. The SCF builder splits a cohabiting couple
   into two units and splits the children evenly between them, so both can
   be heads of household. On the ASEC, Census's tax model has head of
   household 41.5% short of SOI in tax year 2022, and the repo's legal-rule
   builder is expected to be short too (`10_asec_tax_unit_design.md`).
   Should the file carry marital status (and cohabitation) as a separate
   Census-owned variable?
3. **Whose age.** PUF: the first-named filer. SCF: the head, usually the man
   in a mixed-sex couple. ASEC: the householder (used by `ages.R`). The
   share of joint returns whose primary filer is the man, by age, can be
   read from the PUF demographic file; it has not been computed here.
4. **When age is measured.** PUF probably at the end of the tax year; SCF a
   year later; ASEC three months later; ACS during the year; population
   estimates on 1 July. Is a one-year shift of SCF ages part of the
   definition step, or left to the rank map?
5. **Dependent filers.** 9.3 million PUF returns have no SCF counterpart.
   Some SCF split-off adults (students living at home who file) are
   probably dependent filers in tax terms. Coverage (PUF only) or
   translation (relabel some split-off units)?
6. **The `db` wealth variable** is `FUTPEN` + `CURRPEN`. If these are
   account-type balances, as I read the Federal Reserve's definitions, the
   variable is mislabelled and defined-benefit pensions are absent from the
   SCF's concept. To verify against the SCF documentation.
7. **The `re_fund` variable** is `NNRESRE`, nonresidential real estate,
   probably net of its debt. To verify, and the name may need changing.
8. **Household makeup and the wealth split.** SCF wealth belongs to the
   household; our builder divides it between split couples by earnings and
   housing shares, and gives split-off adults none. No source owns the
   split. Does the file need household identifiers so that wealth can be
   generated per household and then divided?
9. **Business income across lines.** The SCF's Schedule C amount is 72%
   above the PUF's in 2016 ($568bn against $331bn) and its Schedule E
   amount 62% above ($1,142bn against $704bn; `g_tables.md`), probably including S-corporation
   income reported as business. Translate Schedule C, farm and Schedule E
   as one block and split them afterwards by PUF shares?
10. **Capital gains concept.** `full_v2` places the SCF's all-gains amount
    against PUF long-term gains alone; test 2 used long- plus short-term
    plus distributions. Which is the definition step?
11. **Wages.** Survey wages probably include elective deferrals that W-2
    box 1 excludes. Estimate and subtract them, or leave the gap to the
    rank map?
12. **Population for age targets.** Census resident population or the
    Social Security area population that the non-filer work uses (they
    differ by territories, people abroad, and group quarters coverage).
13. **The earnings split bands** (`EARNSPLIT`) need the demographic
    booklet before the SCF or ASEC can supply them.
