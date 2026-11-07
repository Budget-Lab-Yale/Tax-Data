# The calculator contract

The columns the GPU tax calculator (tax-simulator-gpu, the commit in `pipeline/settings.json`, taxes.commit)
reads for each record, and where each comes from in the rebuild's year file. This is the list of what the rebuild
still has to supply before Tax-Simulator can run on it. An experiment of 28 October (John), on branch
`rebuild-calculator`; not part of the pipeline's default stages.

The list of record is `CONTRACT` in `pipeline/calculator.py`; `CALCULATOR_CONTRACT.csv` beside this file is written
from it. The calculator's batch command (`python -m taxmodel.run.batch`, called by `pipeline/taxes.py`; until 30 October
`pipeline/calculator_run.py`) checks at every run that every column the pinned calculator reads is in the frame (and names
the supplied columns it does not read), so the list cannot fall out of date when the commit moves. Since 30 October the
same contract feeds the taxes computed inside the tilt (`taxes.py`, FEATURES.md).

## Counts

165 columns, under Tax-Simulator's names, in Tax-Data's form (the overlapping wage columns wages, wages1, wages2,
tips, ot, sole_prop and farm as Tax-Data writes them; the calculator splits them itself):

| Where the column comes from | Columns |
|---|---:|
| the rebuild's column, same name | 62 (59 in a file without overtime: ot, ot1 and ot2 are then 0; the overtime run of 28 October has all 62) |
| the rebuild's column, another name | 50 |
| computed from the rebuild's columns | 17 |
| 0 for now | 36 |

Of the 36 at 0:
- 9 are constants production writes too, with no plan to fill them: `ei_prior_yr`, `other_above_ded`, `salt_pers`,
  the two mortgage balances and two mortgage years, `second_mort_int`, `inv_int_exp`. (The calculator overrides the
  two mortgage years and does not read them.)
- 27 are rule-based items still to build (NEXT_STEPS step 1): blindness (2), dependents' Social Security numbers
  (3), child tax credit eligibility of each dependent (3), the QBI wage bill and specified-service flag of each
  business (8), the basis and holding period of long-term gains (2), death probabilities (2), the yearly unrealized
  gains (7).

The ones that change the 2019 income tax are blindness (the extra standard deduction), the dependents' SSNs and
child credit eligibility (with both at 0 no child qualifies for the child tax credit: every dependent gets the
$500 credit for other dependents and nobody gets the refundable part), and the QBI wage bills (with no W-2 wages
paid, the QBI deduction is 0 above the taxable income threshold). The gain basis and holding period matter only
under an indexing reform; death probabilities only for the estate tax; the yearly unrealized gains only for
reporting.

## What each column is and where it comes from

Missing ages are written as missing (not -1), as Tax-Data writes them. Everything else has a value on every record.

### From the rebuild, same name (62)

| column          | from the rebuild                                              | what it is, or what would fill it                                       |
|:----------------|:--------------------------------------------------------------|:------------------------------------------------------------------------|
| weight          | weight (after the tilt); weight_start is the weight before it | tax units the record stands for                                         |
| filer           | filer                                                         | 1 files a return, 0 a non-filer                                         |
| n_dep           | n_dep (the rebuild caps it at 3)                              | number of dependents                                                    |
| wages           | wages                                                         | W-2 wages of the unit, box 1 (tips and overtime included)               |
| wages1          | wages1                                                        | primary filer's wages; wages1 + wages2 = wages                          |
| wages2          | wages2                                                        | spouse's wages                                                          |
| tips            | tips                                                          | tips of the unit; tips1 + tips2                                         |
| tips1           | tips1                                                         | primary filer's tips, inside wages1                                     |
| tips2           | tips2                                                         | spouse's tips, inside wages2                                            |
| tips_lh1        | tips_lh1                                                      | primary filer's tips are from leisure and hospitality                   |
| tips_lh2        | tips_lh2                                                      | spouse's tips are from leisure and hospitality                          |
| ot              | ot where the file has it (the overtime run); else 0           | overtime pay of the unit; ot1 + ot2                                     |
| ot1             | ot1 where the file has it; else 0                             | primary filer's overtime pay, inside wages1                             |
| ot2             | ot2 where the file has it; else 0                             | spouse's overtime pay, inside wages2                                    |
| trad_contr_er1  | trad_contr_er1                                                | primary filer's traditional elective deferrals (outside box 1)          |
| trad_contr_er2  | trad_contr_er2                                                | spouse's traditional elective deferrals                                 |
| farm            | farm                                                          | Schedule F net farm income of the unit                                  |
| part_179        | part_179                                                      | partnership section 179 deduction                                       |
| scorp_179       | scorp_179                                                     | S corporation section 179 deduction                                     |
| rent_loss       | rent_loss                                                     | rents and royalties, net loss, stored positive                          |
| estate_loss     | estate_loss                                                   | estate and trust net loss, stored positive                              |
| div_ord         | div_ord                                                       | ordinary dividends that are not qualified                               |
| div_pref        | div_pref                                                      | qualified dividends                                                     |
| alimony         | alimony                                                       | alimony received                                                        |
| other_gains     | other_gains                                                   | Form 4797 gains and losses                                              |
| kg_st           | kg_st                                                         | short-term gain or loss (PUF P22250)                                    |
| kg_1250         | kg_1250                                                       | unrecaptured section 1250 gain                                          |
| kg_collect      | kg_collect                                                    | 28 percent rate (collectibles) gain                                     |
| ed_exp          | ed_exp                                                        | educator expenses                                                       |
| trad_contr_ira  | trad_contr_ira                                                | deductible IRA contributions                                            |
| keogh_contr     | keogh_contr                                                   | Keogh, SEP and SIMPLE contributions                                     |
| se_health       | se_health                                                     | self-employed health insurance deduction                                |
| early_penalty   | early_penalty                                                 | penalty on early withdrawal of savings                                  |
| alimony_exp     | alimony_exp                                                   | alimony paid                                                            |
| tuition_ded     | tuition_ded                                                   | tuition and fees deduction (the law caps and repeals it)                |
| dpad            | dpad                                                          | domestic production activities deduction (the law repeals it from 2018) |
| sl_int_ded      | sl_int_ded                                                    | student loan interest deduction                                         |
| auto_int_exp    | auto_int_exp                                                  | auto loan interest (deductible only from 2025)                          |
| med_exp         | med_exp                                                       | medical expenses before the AGI floor                                   |
| salt_inc_sales  | salt_inc_sales                                                | state and local income or sales tax                                     |
| casualty_exp    | casualty_exp                                                  | casualty and theft losses                                               |
| job_exp         | job_exp                                                       | unreimbursed employee expenses                                          |
| tax_prep_exp    | tax_prep_exp                                                  | tax preparation fees                                                    |
| other_misc_exp  | other_misc_exp                                                | other miscellaneous deductions                                          |
| other_item_exp  | other_item_exp                                                | other itemized deductions                                               |
| char_cash       | char_cash                                                     | cash gifts to charity                                                   |
| char_noncash    | char_noncash                                                  | noncash gifts to charity                                                |
| ftc             | ftc                                                           | foreign tax credit                                                      |
| amt_ftc         | amt_ftc                                                       | AMT foreign tax credit                                                  |
| amt_nols        | amt_nols                                                      | AMT net operating loss and other adjustments                            |
| excess_ptc      | excess_ptc                                                    | excess advance premium tax credit repaid                                |
| net_ptc         | net_ptc                                                       | net premium tax credit                                                  |
| aoc_exp         | aoc_exp                                                       | American opportunity credit expenses                                    |
| llc_exp         | llc_exp                                                       | lifetime learning credit expenses                                       |
| savers_cred     | savers_cred                                                   | retirement savings credit                                               |
| old_cred        | old_cred                                                      | credit for the elderly or disabled                                      |
| res_energy_cred | res_energy_cred                                               | residential energy credits                                              |
| gbc             | gbc                                                           | general business credit                                                 |
| prior_yr_cred   | prior_yr_cred                                                 | prior-year minimum tax credit                                           |
| other_nonref    | other_nonref                                                  | other nonrefundable credits                                             |
| recapture_tax   | recapture_tax                                                 | recapture taxes                                                         |
| ira_penalty     | ira_penalty                                                   | additional tax on IRAs and other plans                                  |

### From the rebuild, another name (50)

| column                 | from the rebuild                                                                                                   | what it is, or what would fill it                                          |
|:-----------------------|:-------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------|
| id                     | record_id                                                                                                          | record id, unique in the file                                              |
| filing_status          | mars                                                                                                               | 1 single, 2 joint (and surviving spouses), 3 separate, 4 head of household |
| age1                   | age1; dependent filers (age1 -1) dep_filer_age, drawn by the non-microdata imputations, where the file has it (as taxsim_file); else NaN where the rebuild has -1 | age of the primary filer, 80 for 80 and over                               |
| age2                   | age2; NaN where the rebuild has -1 (none), as Tax-Data writes it                                             | age of the spouse                                                          |
| n_dep_eitc             | n_dep_eitc (PUF EIC, capped at 3)                                                                                      | number of EITC qualifying children                                         |
| dep_age1               | dep_age1; NaN where the rebuild has -1 (none), as Tax-Data writes it                                               | age of the first dependent                                                 |
| dep_age2               | dep_age2; NaN where the rebuild has -1 (none), as Tax-Data writes it                                               | age of the second dependent                                                |
| dep_age3               | dep_age3; NaN where the rebuild has -1 (none), as Tax-Data writes it                                               | age of the third dependent                                                 |
| sole_prop              | sole_prop                                                                                                              | Schedule C net income of the unit                                          |
| part_active            | part_active                                                                                                   | nonpassive partnership income                                              |
| part_active_loss       | part_active_loss                                                                                                  | nonpassive partnership loss, stored positive                               |
| part_passive           | part_passive                                                                                                      | passive partnership income                                                 |
| part_passive_loss      | part_passive_loss                                                                                                     | passive partnership loss, stored positive                                  |
| scorp_active           | scorp_active                                                                                                  | nonpassive S corporation income                                            |
| scorp_active_loss      | scorp_active_loss                                                                                                 | nonpassive S corporation loss, stored positive                             |
| scorp_passive          | scorp_passive                                                                                                     | passive S corporation income                                               |
| scorp_passive_loss     | scorp_passive_loss                                                                                                    | passive S corporation loss, stored positive                                |
| rent                   | rent                                                                                                           | rents and royalties, net income                                            |
| estate                 | estate                                                                                                         | estate and trust net income                                                |
| txbl_int               | interest                                                                                                           | taxable interest                                                           |
| exempt_int             | exempt_int                                                                                                | tax-exempt interest                                                        |
| state_ref              | state_ref                                                                                                      | taxable state income tax refunds                                           |
| txbl_ira_dist          | txbl_ira_dist                                                                                                        | taxable IRA distributions                                                  |
| txbl_pens_dist         | txbl_pens_dist                                                                                                   | taxable pensions and annuities                                             |
| gross_ss               | gross_ss                                                                                                           | gross Social Security benefits (the calculator computes the taxable part)  |
| ui                     | unemployment                                                                                                       | unemployment compensation                                                  |
| hsa_contr              | hsa_contr                                                                                                            | HSA deduction                                                              |
| salt_prop              | salt_prop                                                                                                    | real estate tax                                                            |
| first_mort_int         | first_mort_int (PUF E19200, all deductible interest, as production)                                             | mortgage interest paid                                                     |
| care_exp               | childcare (the CPS's childcare spending; the calculator applies the law's caps)                                    | child and dependent care expenses                                          |
| value.cash             | value.cash                                                                                                             | balance sheet: cash                                                        |
| value.equities         | value.equities                                                                                                         | balance sheet: equities                                                    |
| value.bonds            | value.bonds                                                                                                            | balance sheet: bonds                                                       |
| value.dc               | value.dc                                                                                                               | balance sheet: dc                                                          |
| value.db               | value.db                                                                                                               | balance sheet: db                                                          |
| value.life_ins         | value.life_ins                                                                                                         | balance sheet: life ins                                                    |
| value.annuities        | value.annuities                                                                                                        | balance sheet: annuities                                                   |
| value.trusts           | value.trusts                                                                                                           | balance sheet: trusts                                                      |
| value.other_fin        | value.other_fin                                                                                                        | balance sheet: other fin                                                   |
| value.pass_throughs    | value.pass_throughs                                                                                                    | balance sheet: pass throughs                                               |
| value.primary_home     | value.primary_home                                                                                                     | balance sheet: primary home                                                |
| value.other_home       | value.other_home                                                                                                       | balance sheet: other home                                                  |
| value.re_fund          | value.re_fund                                                                                                          | balance sheet: re fund                                                     |
| value.other_nonfin     | value.other_nonfin                                                                                                     | balance sheet: other nonfin                                                |
| value.primary_mortgage | value.primary_mortgage                                                                                                 | balance sheet: primary mortgage                                            |
| value.other_mortgage   | value.other_mortgage                                                                                                   | balance sheet: other mortgage                                              |
| value.credit_lines     | value.credit_lines                                                                                                     | balance sheet: credit lines                                                |
| value.credit_cards     | value.credit_cards                                                                                                     | balance sheet: credit cards                                                |
| value.installment_debt | value.installment_debt                                                                                                 | balance sheet: installment debt                                            |
| value.other_debt       | value.other_debt                                                                                                       | balance sheet: other debt                                                  |

### Computed from the rebuild's columns (17)

| column              | from the rebuild                                                               | what it is, or what would fill it                                                                                                                                                                                                                       |
|:--------------------|:-------------------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| dep_status          | age_range in 6, 7, 8 (the PUF's three dependent-filer ranges)                  | 1 if the return is filed by someone claimed as a dependent                                                                                                                                                                                              |
| male1               | sex_primary == 1; dependent filers dep_filer_sex == 1 where the file has it (else 0)   | primary filer is a man; the frame steps read it (chance of being a surviving spouse), not the tax chain                                                                                                                                                 |
| divorce_year        | 0 on every record (production: 0 where alimony paid or received, else missing) | year of divorce; before 2019 alimony counts in AGI. 0 and missing differ only in the capital gains module's chance of being a surviving spouse                                                                                                          |
| sole_prop1          | sole_prop - sole_prop2                                                             | primary filer's Schedule C income                                                                                                                                                                                                                       |
| sole_prop2          | sole_prop x spouse_se_share (production splits by one self-employment share too)   | spouse's Schedule C income                                                                                                                                                                                                                              |
| farm1               | farm - farm2                                                                   | primary filer's farm income                                                                                                                                                                                                                             |
| farm2               | farm x spouse_se_share                                                         | spouse's farm income                                                                                                                                                                                                                                    |
| part_se1            | part_se - part_se2                                                             | primary filer's Schedule SE earnings other than Schedules C and F                                                                                                                                                                                       |
| part_se2            | part_se x spouse_se_share                                                      | spouse's Schedule SE earnings other than Schedules C and F                                                                                                                                                                                              |
| gross_pens_dist     | txbl_pens_dist + nontxbl_pens_dist (the non-taxable rest, PUF E01500 - E01700; since 3 November) | gross pensions and annuities                                                                                                                                                                                                                            |
| other_inc           | other_agi_remainder + farm_rent + sche_other                                   | other income in AGI: AGI less every item the calculator gets in another column. The rebuild's remainder does not hold the adjustments (production's does); farm rental and the unnamed rest of Schedule E have no column of their own in the calculator |
| kg_lt               | kg_lt + kg_dist                                                                | long-term gain or loss (PUF P23250) plus capital gain distributions reported on Form 1040 (PUF E01100), which are long-term gains                                                                                                                       |
| basis.equities      | value.equities - w_kg_other (the SCF's unrealized gain on stocks and funds)        | cost basis of equities                                                                                                                                                                                                                                  |
| basis.pass_throughs | value.pass_throughs - w_kg_pass_throughs                                           | cost basis of businesses                                                                                                                                                                                                                                |
| basis.primary_home  | value.primary_home - w_kg_primary_home                                             | cost basis of the home                                                                                                                                                                                                                                  |
| basis.other_home    | value.other_home - w_kg_other_re x value.other_home / (value.other_home + value.re_fund)       | cost basis of other homes (the other real estate gain split by value, as production)                                                                                                                                                                    |
| basis.re_fund       | value.re_fund - w_kg_other_re x value.re_fund / (value.other_home + value.re_fund)             | cost basis of other real estate                                                                                                                                                                                                                         |

### 0 for now (36)

| column                 | from the rebuild            | what it is, or what would fill it                                                                                            |
|:-----------------------|:----------------------------|:-----------------------------------------------------------------------------------------------------------------------------|
| blind1                 | 0                           | primary filer is blind: the blindness rule (NEXT_STEPS step 1)                                                               |
| blind2                 | 0                           | spouse is blind: the blindness rule (NEXT_STEPS step 1)                                                                      |
| dep_ssn1               | 0                           | dependent 1 has a Social Security number: the dependents' SSN rule (NEXT_STEPS step 1)                                       |
| dep_ssn2               | 0                           | dependent 2 has a Social Security number: as dep_ssn1                                                                        |
| dep_ssn3               | 0                           | dependent 3 has a Social Security number: as dep_ssn1                                                                        |
| dep_ctc1               | 0                           | dependent 1 qualifies for the child tax credit apart from age and SSN: the child credit eligibility rule (NEXT_STEPS step 1) |
| dep_ctc2               | 0                           | dependent 2 qualifies for the child tax credit: as dep_ctc1                                                                  |
| dep_ctc3               | 0                           | dependent 3 qualifies for the child tax credit: as dep_ctc1                                                                  |
| ei_prior_yr            | 0 (production writes 0 too) | earned income last year (child credit look-back; off under 2019 law): no source planned                                      |
| wagebill_sole_prop     | 0                           | W-2 wages paid by the sole proprietorship (QBI wage limit): the QBI wage rule (NEXT_STEPS step 1)                            |
| wagebill_part          | 0                           | W-2 wages paid by the partnership (QBI): as wagebill_sole_prop                                                               |
| wagebill_scorp         | 0                           | W-2 wages paid by the S corporation (QBI): as wagebill_sole_prop                                                             |
| wagebill_farm          | 0                           | W-2 wages paid by the farm (QBI): as wagebill_sole_prop                                                                      |
| sstb_sole_prop         | 0                           | the sole proprietorship is a specified service business (QBI): the QBI specified-service rule (NEXT_STEPS step 1)            |
| sstb_part              | 0                           | the partnership is a specified service business: as sstb_sole_prop                                                           |
| sstb_scorp             | 0                           | the S corporation is a specified service business: as sstb_sole_prop                                                         |
| sstb_farm              | 0                           | the farm is a specified service business: as sstb_sole_prop                                                                  |
| kg_lt_basis            | 0                           | cost basis of long-term gains (indexing reforms only): basis from SOI's Sales of Capital Assets tables (NEXT_STEPS step 1)   |
| kg_lt_years_held       | 0                           | holding period of long-term gains (indexing reforms only): as kg_lt_basis                                                    |
| other_above_ded        | 0 (production writes 0 too) | other adjustments: none planned                                                                                              |
| salt_pers              | 0 (production writes 0 too) | personal property tax: none planned                                                                                          |
| first_mort_bal         | 0 (production writes 0 too) | first mortgage balance (the $750,000 limit): the balance sheet's value.primary_mortgage could fill it (a decision)               |
| second_mort_bal        | 0 (production writes 0 too) | second mortgage balance: as first_mort_bal                                                                                   |
| first_mort_year        | 0 (production writes 0 too) | year the first mortgage was taken out; the calculator overrides it                                                           |
| second_mort_year       | 0 (production writes 0 too) | as first_mort_year                                                                                                           |
| second_mort_int        | 0 (production writes 0 too) | interest on a second mortgage: inside first_mort_int                                                                         |
| inv_int_exp            | 0 (production writes 0 too) | investment interest: inside first_mort_int                                                                                   |
| q_death1               | 0                           | chance the primary filer dies this year: the death probabilities rule (NEXT_STEPS 1)                                         |
| q_death2               | 0                           | chance the spouse dies this year: as q_death1                                                                                |
| accruals.equities      | 0                           | yearly unrealized gain on equities (reporting only): the yearly unrealized gains rule (NEXT_STEPS step 1)                    |
| accruals.pass_throughs | 0                           | yearly unrealized gain on pass throughs (reporting only): the yearly unrealized gains rule (NEXT_STEPS step 1)               |
| accruals.primary_home  | 0                           | yearly unrealized gain on primary home (reporting only): the yearly unrealized gains rule (NEXT_STEPS step 1)                |
| accruals.other_home    | 0                           | yearly unrealized gain on other home (reporting only): the yearly unrealized gains rule (NEXT_STEPS step 1)                  |
| accruals.re_fund       | 0                           | yearly unrealized gain on re fund (reporting only): the yearly unrealized gains rule (NEXT_STEPS step 1)                     |
| accruals.dc            | 0                           | yearly unrealized gain on dc (reporting only): the yearly unrealized gains rule (NEXT_STEPS step 1)                          |
| accruals.trusts        | 0                           | yearly unrealized gain on trusts (reporting only): the yearly unrealized gains rule (NEXT_STEPS step 1)                      |
## Where this list differs from the column inventory of 28 October

The inventory is `explore/2026-10-28/tsim_columns/INVENTORY.md` in scratch.

1. **other_inc.** The inventory maps it to `other_agi`. That column holds every AGI item folded into it (business
   income, pensions, Social Security, the adjustments and the rest), which the calculator also reads in their own
   columns, so they would count twice. The right column is `other_agi_remainder`, plus farm rental and the unnamed
   rest of Schedule E, which have no column of their own in the calculator. The rebuild's remainder does not hold
   the adjustments; production's `other_inc` does (the residual of AGI), and the calculator subtracts the
   adjustments again (the double count of about $145 billion in NEXT_STEPS). On the rebuild there is no double
   count.
2. **kg_lt.** The inventory has it under the same name. Here it is `kg_lt + kg_dist`: capital gain distributions
   reported directly on Form 1040 (PUF E01100) are long-term gains. Production leaves them out of AGI altogether.
3. **first_mort_year, second_mort_year.** Listed as read; the GPU calculator sets both to infinity itself and does
   not read them.
4. **trad_contr_er1, trad_contr_er2.** The inventory found them only in a test file, with $2.55 trillion. In the
   big run's file (27-28 October) they are $251 billion and $78 billion after the tilt.
5. **divorce_year.** The inventory gives production's rule (0 where alimony is paid or received, missing
   otherwise). Here it is 0 on every record, as John asked for missing items. For the income tax the two are the
   same; they differ only in the capital gains module's chance of being a surviving spouse.
6. **dpad.** The calculator's 2019 law has the deduction repealed, so the $4.0 billion the file carries is not
   deducted (NEXT_STEPS step 2 had the repeal from 2020; the statute repealed it after 2017).

Columns Tax-Simulator's R model reads and the GPU calculator does not (not in the converter): `n_dep_ctc` (the
rebuild's `n_dep_ctc`), `male2` (`sex_spouse`), `parent_rank` (a rule to build), `part_se` (in the file), `nols`
(production writes 0), `prim_mort_share`, the eight consumption categories (in the file), the nine `forbes_*` tags
and `dina_other_taxes_rate`.
