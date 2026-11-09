# The calculator contract

The columns the GPU tax calculator (tax-simulator-gpu, at the commit in settings `taxes.commit`) reads for each
record, and where each comes from in the rebuild's year file.

The list is `CONTRACT` in `shared/calculator_input.py`, one row per column: the calculator's name, its status, where
it comes from, and what it is. `convert` in the same file turns a year file into the calculator's input frame, one row
per record, in that order. Three steps use it:
- the taxes computed inside the reweighting stage (`reweight/taxes.py`), on the start of every year;
- the Tax-Simulator file (`output/taxsim_file.py`), which adds 23 columns and a few rules of its own (below);
- the optional calculator report (`output/report_calculator.py`), which also writes the list as
  `calculator_contract.csv` beside its outputs. `CALCULATOR_CONTRACT.csv` beside this file is a copy.

The calculator's batch command (`python -m taxmodel.run.batch`) checks the frame at every run: it stops if a column
its tax chain or frame steps read is missing, and it lists the supplied columns it does not read. The contract
therefore cannot fall behind the pinned calculator without a run failing.

## Form of the input

- Column names are Tax-Simulator's. Amounts are in Tax-Data's form: the overlapping wage columns (`wages`, `wages1`,
  `wages2`, `tips`, `ot`, `sole_prop`, `farm`) are written as Tax-Data writes them, and the calculator splits them
  itself.
- `convert` checks that wages, tips, overtime, Schedule C and Schedule F each equal the sum of the two people's parts,
  that `id` is unique, and that the filing status is 1 to 4.
- Every column `convert` reads from the file must have no missing value. Missing ages are written as missing (not
  -1), as Tax-Data writes them; every other column has a value on every record.
- `convert` writes conversion notes: what it met and what it set (counts of missing ages, dependent filers given
  their drawn age, spouse's Schedule C plus F against the file's `se2`, and the like).

## Counts

165 columns:

| Status in `CONTRACT` | Columns | Where the value comes from |
|---|---:|---|
| same | 111 | the year file's column of the same name |
| renamed | 1 | `id` from `record_id` |
| computed | 17 | a formula on the year file's columns |
| zero | 36 | the year file's column when the file has it (27 columns, all present in files the pipeline makes); otherwise 0 (9 columns that are always 0) |

## Columns taken from the year file under the same name (111)

| Group | Columns |
|---|---|
| The record | `weight` (after the reweighting; `weight_start` in the file is the weight before it), `filer` (1 files, 0 does not), `filing_status` (1 single, 2 joint and surviving spouses, 3 separate, 4 head of household) |
| People | `age1`, `age2`, `dep_age1`, `dep_age2`, `dep_age3` (rules below); `n_dep` (number of dependents, capped at 3); `n_dep_eitc` (EITC qualifying children, the PUF's EIC, capped at 3) |
| Wages | `wages` (box 1 wages of the unit, tips and overtime included), `wages1`, `wages2` (each person's; `wages2` is wages times the spouse's drawn wage share, held inside the range the earnings split code allows), `tips`, `tips1`, `tips2` (inside each person's wages), `tips_lh1`, `tips_lh2` (tips from leisure and hospitality), `ot`, `ot1`, `ot2` (overtime pay inside each person's wages; 0 if the file lacks them), `trad_contr_er1`, `trad_contr_er2` (traditional elective deferrals, outside box 1) |
| Schedules C, E and F | `sole_prop`, `farm`; `part_active`, `part_passive`, `scorp_active`, `scorp_passive` and their `_loss` columns (losses stored positive); `part_179`, `scorp_179`; `rent`, `rent_loss`, `estate`, `estate_loss` |
| Other income | `txbl_int`, `exempt_int`, `div_ord` (ordinary dividends that are not qualified), `div_pref` (qualified dividends), `state_ref`, `txbl_ira_dist`, `txbl_pens_dist`, `gross_ss` (the calculator computes the taxable part), `ui`, `alimony`, `other_gains` (Form 4797) |
| Capital gains | `kg_st` (PUF P22250), `kg_1250` (unrecaptured section 1250 gain), `kg_collect` (28 percent rate gain) |
| Adjustments | `ed_exp`, `hsa_contr`, `trad_contr_ira`, `keogh_contr`, `se_health`, `early_penalty`, `alimony_exp`, `tuition_ded`, `dpad`, `sl_int_ded`, `auto_int_exp` (deductible only from 2025) |
| Schedule A | `med_exp` (before the AGI floor), `salt_inc_sales`, `salt_prop`, `first_mort_int` (PUF E19200, all deductible interest), `casualty_exp`, `job_exp`, `tax_prep_exp`, `other_misc_exp`, `other_item_exp`, `char_cash`, `char_noncash` |
| Credits and other taxes | `ftc`, `amt_ftc`, `amt_nols`, `excess_ptc`, `net_ptc`, `care_exp` (the CPS's childcare spending; the calculator applies the law's caps), `aoc_exp`, `llc_exp`, `savers_cred`, `old_cred`, `res_energy_cred`, `gbc`, `prior_yr_cred`, `other_nonref`, `recapture_tax`, `ira_penalty` |
| Balance sheet | the 20 items `value.cash`, `value.equities`, `value.bonds`, `value.dc`, `value.db`, `value.life_ins`, `value.annuities`, `value.trusts`, `value.other_fin`, `value.pass_throughs`, `value.primary_home`, `value.other_home`, `value.re_fund`, `value.other_nonfin`, `value.primary_mortgage`, `value.other_mortgage`, `value.credit_lines`, `value.credit_cards`, `value.installment_debt`, `value.other_debt` |

The calculator's law repeals the domestic production activities deduction (`dpad`) from 2018 and the tuition and
fees deduction (`tuition_ded`) after 2020. The reweighting stage sets both to zero in the file from those years
(settings `taxes.repealed_adjustments`).

## Computed columns (17)

| Column | Formula | What it is |
|---|---|---|
| `dep_status` | 1 where `age_range` is 6, 7 or 8 | the return is filed by someone claimed as a dependent |
| `male1` | `sex_primary` = 1; dependent filers: `dep_filer_sex` = 1 | the primary filer is a man (read by the frame steps for the chance of being a surviving spouse, not by the tax chain) |
| `divorce_year` | 0 on every record | year of divorce; before 2019 alimony counts in AGI |
| `sole_prop2`, `sole_prop1` | `sole_prop` x `spouse_se_share`; `sole_prop` less that | each person's Schedule C income |
| `farm2`, `farm1` | `farm` x `spouse_se_share`; `farm` less that | each person's farm income |
| `part_se2`, `part_se1` | `part_se` x `spouse_se_share`; `part_se` less that | each person's Schedule SE earnings other than Schedules C and F |
| `gross_pens_dist` | `txbl_pens_dist` + `nontxbl_pens_dist` | gross pensions and annuities (the non-taxable part is the PUF's E01500 less E01700, never below zero) |
| `other_inc` | `other_agi_remainder` + `farm_rent` + `sche_other` | other income in AGI: AGI less every item the calculator gets in another column; farm rental and the unnamed rest of Schedule E have no column of their own |
| `kg_lt` | `kg_lt` + `kg_dist` | long-term gain or loss (PUF P23250) plus capital gain distributions reported on Form 1040 (PUF E01100), which are long-term gains |
| `basis.equities` | `value.equities` less `w_kg_other` | cost basis of equities (the SCF's unrealized gain on stocks and funds) |
| `basis.pass_throughs` | `value.pass_throughs` less `w_kg_pass_throughs` | cost basis of businesses |
| `basis.primary_home` | `value.primary_home` less `w_kg_primary_home` | cost basis of the home |
| `basis.other_home`, `basis.re_fund` | each value less `w_kg_other_re` split in proportion to the two values | cost basis of other homes and of other real estate |

The rebuild's `other_agi_remainder` does not hold the adjustments, so the calculator, which subtracts the adjustments
from its own columns, does not count them twice.

## Columns zero in the contract (36)

`convert` takes each of these from the year file when the file has the column, and writes 0 otherwise.

Made in the year file (27):

| Columns | Made by | What it is |
|---|---|---|
| `blind1`, `blind2` | non-microdata imputations, blindness | each person is blind |
| `dep_ssn1`-`dep_ssn3` | the dollar step, from the drawn CPS Social Security number status; missing values set to 0 | the k-th dependent has a Social Security number |
| `dep_ctc1`-`dep_ctc3` | non-microdata imputations, child credit | the k-th dependent qualifies for the child tax credit apart from age and number |
| `wagebill_sole_prop`, `wagebill_part`, `wagebill_scorp`, `wagebill_farm` | non-microdata imputations, QBI | W-2 wages each kind of business pays (the QBI wage limit) |
| `sstb_sole_prop`, `sstb_part`, `sstb_scorp`, `sstb_farm` | non-microdata imputations, QBI | each kind of business is a specified service business |
| `kg_lt_basis`, `kg_lt_years_held` | non-microdata imputations, holding period of gains | cost basis and holding period of long-term gains (indexing reforms only) |
| `q_death1`, `q_death2` | non-microdata imputations, death probabilities | each person's chance of dying this year (the estate tax) |
| `accruals.equities`, `.pass_throughs`, `.primary_home`, `.other_home`, `.re_fund`, `.dc`, `.trusts` | non-microdata imputations, unrealized gains | yearly unrealized gain on each asset (reporting only) |

If a file lacked them, the ones that change the income tax are blindness (the extra standard deduction), the
dependents' Social Security numbers and child credit flags (with both at 0 no child qualifies for the child tax
credit), and the QBI wage bills (with none, the QBI deduction is 0 above the taxable income threshold).

Always 0 (9), as production's Tax-Data writes them:

| Column | What it is |
|---|---|
| `ei_prior_yr` | earned income last year (a child credit look-back, off under current law) |
| `other_above_ded` | other adjustments |
| `salt_pers` | personal property tax |
| `first_mort_bal`, `second_mort_bal` | mortgage balances (the $750,000 limit) |
| `first_mort_year`, `second_mort_year` | years the mortgages were taken out; the calculator sets these itself and does not read them |
| `second_mort_int`, `inv_int_exp` | interest on a second mortgage and investment interest, both inside `first_mort_int` |

## Rules `convert` applies

- `age1`: the file's `age1`; a dependent filer (`age1` -1) takes `dep_filer_age`, and `male1` takes `dep_filer_sex`
  (`AGES.md`).
- `age2`, `dep_age1`-`dep_age3`: missing where the file has -1 (no such person).
- `ot`, `ot1`, `ot2`: 0 where the file has no overtime columns.
- `dep_ssn1`-`dep_ssn3`: a missing value (no such dependent) is set to 0, and counted in the notes.
- `spouse_se_share` must lie in [0, 1].

## The Tax-Simulator file

`output/taxsim_file.py` writes `tax_units_<year>.csv` with production's 188 columns in production's order:
the 165 above, with production's conventions for missing values, and 23 more. Where each of the 188 comes from is
`TAXSIM_FILE_COLUMNS.csv`. What it changes from the calculator input:
- the spouse's age, sex, blindness and death probability are missing without a spouse; a dependent's age, Social
  Security number flag and child credit flag are missing where there is no such dependent;
- `divorce_year` is 0 where alimony is paid or received and missing otherwise (production's rule). For the income tax
  this is the same as 0 everywhere; the two differ only in the capital gains module's chance of being a surviving
  spouse;
- `kg_lt_years_held` and `kg_lt_basis` are missing where `kg_lt` is 0, and 0 where `kg_lt` is not 0 but the file has
  no holding period (capital gain distributions alone);
- the five `basis.*` columns are the non-microdata imputations' (value less unrealized gain, the gain first held to
  the value), not the formula above;
- flags are written TRUE or FALSE where production writes them so;
- records with a weight of 0 or less are left out.

The 23 more columns: `male2` (the spouse's sex), `n_dep_ctc` (the PUF's child credit children, N24, held to 3),
`parent_rank`, `part_se`, the eight consumption categories, `nols` (0, as production writes it), `prim_mort_share`
(the primary mortgage's share of all mortgage debt in the drawn balance sheet; 1 with no mortgage), and the nine
`forbes_*` columns (production's defaults, filled on the Forbes list's records). Of these, Tax-Simulator's R model
reads `male2` and `n_dep_ctc`; by a scan of its code it does not read `parent_rank`, `part_se`, `nols`,
`prim_mort_share`, the consumption categories, `q_death1-2`, or the `value.*`, `basis.*`, `accruals.*` and `forbes_*`
columns.
