"""The rebuild's year file as the input of the GPU tax calculator (convert, and the column list CONTRACT). taxes.py
(inside the tilt), taxsim_file.py, package.py and the optional calculator stage (output/calculator.py) use it.

The calculator in three steps:

1. The pinned calculator. The calculator's code is read from a copy of its repository at one commit (settings
   calculator.commit), written once with `git archive` under <out>/lib/tax-simulator-gpu_<first 12 characters of
   the commit>. Later edits to the repository do not reach a run; moving to a newer version is the one setting.
2. The converter (`convert`). The year file becomes a frame with Tax-Simulator's column names, one row per record,
   in Tax-Data's form (the overlapping wage columns wages, wages1, wages2, tips, ot, sole_prop, farm as Tax-Data
   writes them). Each column is the rebuild's own column under the same name, renamed, computed from the rebuild's
   columns, or 0 where the rebuild does not have it yet. CONTRACT below is the list, one row per column; it is
   written beside the outputs as calculator_contract.csv and is the source of research/rebuild/CALCULATOR_CONTRACT.csv.
3. The run. Since 30 October tax-simulator-gpu's own batch command (python -m taxmodel.run.batch, its local branch
   taxdata-batch-1103), called by taxes.run_batch with settings taxes (the pinned commit, the calculator's Python
   environment), in place of the experiment's own runner (calculator_run.py, removed): the calculator's own
   preparation (the wage split and the random draws keyed by record id, as its bridge does), its frame steps under
   the year's law and its tax chain; every computed column per record (calculator_<year>.parquet). The same command
   computes the taxes inside the tilt (taxes.py).
"""
import numpy as np
import pandas as pd

from shared import common


# ---- the contract ---------------------------------------------------------------------------------------------
# One row per column: (Tax-Simulator's name, status, where it comes from, what it is / what would fill it).
# status: same (the rebuild's column, same name; since the rename of 29-30 October, tools/rename_map.csv, every column
# that is the same item has the same name), renamed (the rebuild's column under another name: only id), computed (a
# formula on the rebuild's columns), zero (0 for now: the rebuild does not have it; the last field says what would
# fill it). The calculator reads every one of these; its batch command checks that against the pinned code.
SAME, RENAMED, COMPUTED, ZERO = 'same', 'renamed', 'computed', 'zero'
NO_AGE = 'NaN where the rebuild has -1 (none), as Tax-Data writes it'
CONTRACT = [
    # the record
    ('id', RENAMED, 'record_id', 'record id, unique in the file'),
    ('weight', SAME, 'weight (after the tilt); weight_start is the weight before it', 'tax units the record stands for'),
    ('filer', SAME, 'filer', '1 files a return, 0 a non-filer'),
    ('filing_status', SAME, 'filing_status', '1 single, 2 joint (and surviving spouses), 3 separate, 4 head of household'),
    ('dep_status', COMPUTED, 'age_range in 6, 7, 8 (the PUF\'s three dependent-filer ranges)',
     '1 if the return is filed by someone claimed as a dependent'),
    ('age1', SAME, f'age1; dependent filers (age1 -1) dep_filer_age, drawn by the non-microdata imputations, where '
     f'the file has it (as taxsim_file); else {NO_AGE}',
     'age of the primary filer, 80 for 80 and over'),
    ('age2', SAME, f'age2; {NO_AGE}', 'age of the spouse'),
    ('male1', COMPUTED, 'sex_primary == 1; dependent filers dep_filer_sex == 1 where the file has it (else 0)',
     'primary filer is a man; the frame steps read it (chance of being a surviving spouse), not the tax chain'),
    ('blind1', ZERO, '0', 'primary filer is blind: the blindness rule (NEXT_STEPS step 1)'),
    ('blind2', ZERO, '0', 'spouse is blind: the blindness rule (NEXT_STEPS step 1)'),
    ('n_dep', SAME, 'n_dep (the rebuild caps it at 3)', 'number of dependents'),
    ('n_dep_eitc', SAME, 'n_dep_eitc (PUF EIC, capped at 3)', 'number of EITC qualifying children'),
    ('dep_age1', SAME, f'dep_age1; {NO_AGE}', 'age of the first dependent'),
    ('dep_age2', SAME, f'dep_age2; {NO_AGE}', 'age of the second dependent'),
    ('dep_age3', SAME, f'dep_age3; {NO_AGE}', 'age of the third dependent'),
    ('dep_ssn1', ZERO, '0', 'dependent 1 has a Social Security number: the dependents\' SSN rule (NEXT_STEPS step 1)'),
    ('dep_ssn2', ZERO, '0', 'dependent 2 has a Social Security number: as dep_ssn1'),
    ('dep_ssn3', ZERO, '0', 'dependent 3 has a Social Security number: as dep_ssn1'),
    ('dep_ctc1', ZERO, '0', 'dependent 1 qualifies for the child tax credit apart from age and SSN: the child credit '
     'eligibility rule (NEXT_STEPS step 1)'),
    ('dep_ctc2', ZERO, '0', 'dependent 2 qualifies for the child tax credit: as dep_ctc1'),
    ('dep_ctc3', ZERO, '0', 'dependent 3 qualifies for the child tax credit: as dep_ctc1'),
    ('divorce_year', COMPUTED, '0 on every record (production: 0 where alimony paid or received, else missing)',
     'year of divorce; before 2019 alimony counts in AGI. 0 and missing differ only in the capital gains module\'s '
     'chance of being a surviving spouse'),
    # wages and self-employment, in Tax-Data's overlapping form (the calculator splits them itself)
    ('wages', SAME, 'wages', 'W-2 wages of the unit, box 1 (tips and overtime included)'),
    ('wages1', SAME, 'wages1', 'primary filer\'s wages; wages1 + wages2 = wages'),
    ('wages2', SAME, 'wages2 = wages x spouse_wage_share (the CPS label\'s draw, held inside the range the earnings '
     'split code allows, earn_split.py)', 'spouse\'s wages'),
    ('tips', SAME, 'tips', 'tips of the unit; tips1 + tips2'),
    ('tips1', SAME, 'tips1', 'primary filer\'s tips, inside wages1'),
    ('tips2', SAME, 'tips2', 'spouse\'s tips, inside wages2'),
    ('tips_lh1', SAME, 'tips_lh1', 'primary filer\'s tips are from leisure and hospitality'),
    ('tips_lh2', SAME, 'tips_lh2', 'spouse\'s tips are from leisure and hospitality'),
    ('ot', SAME, 'ot where the file has it (the overtime run); else 0', 'overtime pay of the unit; ot1 + ot2'),
    ('ot1', SAME, 'ot1 where the file has it; else 0', 'primary filer\'s overtime pay, inside wages1'),
    ('ot2', SAME, 'ot2 where the file has it; else 0', 'spouse\'s overtime pay, inside wages2'),
    ('trad_contr_er1', SAME, 'trad_contr_er1', 'primary filer\'s traditional elective deferrals (outside box 1)'),
    ('trad_contr_er2', SAME, 'trad_contr_er2', 'spouse\'s traditional elective deferrals'),
    ('sole_prop', SAME, 'sole_prop', 'Schedule C net income of the unit'),
    ('sole_prop1', COMPUTED, 'sole_prop - sole_prop2', 'primary filer\'s Schedule C income'),
    ('sole_prop2', COMPUTED, 'sole_prop x spouse_se_share (one self-employment share, as production; on filers the PUF '
     'label\'s draw, learned from E30500 / E30400, since 4 November)', 'spouse\'s Schedule C income'),
    ('farm', SAME, 'farm', 'Schedule F net farm income of the unit'),
    ('farm1', COMPUTED, 'farm - farm2', 'primary filer\'s farm income'),
    ('farm2', COMPUTED, 'farm x spouse_se_share', 'spouse\'s farm income'),
    ('part_se1', COMPUTED, 'part_se - part_se2', 'primary filer\'s Schedule SE earnings other than Schedules C and F'),
    ('part_se2', COMPUTED, 'part_se x spouse_se_share', 'spouse\'s Schedule SE earnings other than Schedules C and F'),
    ('ei_prior_yr', ZERO, '0 (production writes 0 too)', 'earned income last year (child credit look-back; off under '
     '2019 law): no source planned'),
    # Schedule E
    ('part_active', SAME, 'part_active', 'nonpassive partnership income'),
    ('part_active_loss', SAME, 'part_active_loss', 'nonpassive partnership loss, stored positive'),
    ('part_passive', SAME, 'part_passive', 'passive partnership income'),
    ('part_passive_loss', SAME, 'part_passive_loss', 'passive partnership loss, stored positive'),
    ('part_179', SAME, 'part_179', 'partnership section 179 deduction'),
    ('scorp_active', SAME, 'scorp_active', 'nonpassive S corporation income'),
    ('scorp_active_loss', SAME, 'scorp_active_loss', 'nonpassive S corporation loss, stored positive'),
    ('scorp_passive', SAME, 'scorp_passive', 'passive S corporation income'),
    ('scorp_passive_loss', SAME, 'scorp_passive_loss', 'passive S corporation loss, stored positive'),
    ('scorp_179', SAME, 'scorp_179', 'S corporation section 179 deduction'),
    ('rent', SAME, 'rent', 'rents and royalties, net income'),
    ('rent_loss', SAME, 'rent_loss', 'rents and royalties, net loss, stored positive'),
    ('estate', SAME, 'estate', 'estate and trust net income'),
    ('estate_loss', SAME, 'estate_loss', 'estate and trust net loss, stored positive'),
    ('wagebill_sole_prop', ZERO, '0', 'W-2 wages paid by the sole proprietorship (QBI wage limit): the QBI wage rule '
     '(NEXT_STEPS step 1)'),
    ('wagebill_part', ZERO, '0', 'W-2 wages paid by the partnership (QBI): with wagebill_scorp the group\'s wages, split by income'),
    ('wagebill_scorp', ZERO, '0', 'W-2 wages paid by the S corporation (QBI): with wagebill_part the group\'s wages, split by income'),
    ('wagebill_farm', ZERO, '0', 'W-2 wages paid by the farm (QBI): positive farm income, so the wage limit never binds'),
    ('sstb_sole_prop', ZERO, '0', 'the sole proprietorship is a specified service business (QBI): the QBI '
     'specified-service rule (NEXT_STEPS step 1)'),
    ('sstb_part', ZERO, '0', 'the partnership is a specified service business: as sstb_sole_prop'),
    ('sstb_scorp', ZERO, '0', 'the S corporation is a specified service business: as sstb_sole_prop'),
    ('sstb_farm', ZERO, '0', 'the farm is a specified service business: never'),
    # other income
    ('txbl_int', SAME, 'txbl_int', 'taxable interest'),
    ('exempt_int', SAME, 'exempt_int', 'tax-exempt interest'),
    ('div_ord', SAME, 'div_ord', 'ordinary dividends that are not qualified'),
    ('div_pref', SAME, 'div_pref', 'qualified dividends'),
    ('state_ref', SAME, 'state_ref', 'taxable state income tax refunds'),
    ('txbl_ira_dist', SAME, 'txbl_ira_dist', 'taxable IRA distributions'),
    ('txbl_pens_dist', SAME, 'txbl_pens_dist', 'taxable pensions and annuities'),
    ('gross_pens_dist', COMPUTED, 'txbl_pens_dist + nontxbl_pens_dist (the non-taxable rest of pensions, the PUF\'s '
     'E01500 - E01700, never below zero; since 3 November, before: retirement_gross - gross_ss)',
     'gross pensions and annuities'),
    ('gross_ss', SAME, 'gross_ss', 'gross Social Security benefits (the calculator computes the taxable part)'),
    ('ui', SAME, 'ui', 'unemployment compensation'),
    ('alimony', SAME, 'alimony', 'alimony received'),
    ('other_inc', COMPUTED, 'other_agi_remainder + farm_rent + sche_other',
     'other income in AGI: AGI less every item the calculator gets in another column. The rebuild\'s remainder '
     'does not hold the adjustments (production\'s does); farm rental and the unnamed rest of Schedule E have no '
     'column of their own in the calculator'),
    ('other_gains', SAME, 'other_gains', 'Form 4797 gains and losses'),
    # capital gains
    ('kg_st', SAME, 'kg_st', 'short-term gain or loss (PUF P22250)'),
    ('kg_lt', COMPUTED, 'kg_lt + kg_dist', 'long-term gain or loss (PUF P23250) plus capital gain distributions '
     'reported on Form 1040 (PUF E01100), which are long-term gains'),
    ('kg_1250', SAME, 'kg_1250', 'unrecaptured section 1250 gain'),
    ('kg_collect', SAME, 'kg_collect', '28 percent rate (collectibles) gain'),
    ('kg_lt_basis', ZERO, '0', 'cost basis of long-term gains (indexing reforms only): basis from SOI\'s Sales of '
     'Capital Assets tables (NEXT_STEPS step 1)'),
    ('kg_lt_years_held', ZERO, '0', 'holding period of long-term gains (indexing reforms only): as kg_lt_basis'),
    # above-the-line deductions
    ('ed_exp', SAME, 'ed_exp', 'educator expenses'),
    ('hsa_contr', SAME, 'hsa_contr', 'HSA deduction'),
    ('trad_contr_ira', SAME, 'trad_contr_ira', 'deductible IRA contributions'),
    ('keogh_contr', SAME, 'keogh_contr', 'Keogh, SEP and SIMPLE contributions'),
    ('se_health', SAME, 'se_health', 'self-employed health insurance deduction'),
    ('early_penalty', SAME, 'early_penalty', 'penalty on early withdrawal of savings'),
    ('alimony_exp', SAME, 'alimony_exp', 'alimony paid'),
    ('tuition_ded', SAME, 'tuition_ded', 'tuition and fees deduction (the law caps and repeals it)'),
    ('dpad', SAME, 'dpad', 'domestic production activities deduction (the law repeals it from 2018)'),
    ('other_above_ded', ZERO, '0 (production writes 0 too)', 'other adjustments: none planned'),
    ('sl_int_ded', SAME, 'sl_int_ded', 'student loan interest deduction'),
    ('auto_int_exp', SAME, 'auto_int_exp', 'auto loan interest (deductible only from 2025)'),
    # Schedule A
    ('med_exp', SAME, 'med_exp', 'medical expenses before the AGI floor'),
    ('salt_inc_sales', SAME, 'salt_inc_sales', 'state and local income or sales tax'),
    ('salt_prop', SAME, 'salt_prop', 'real estate tax'),
    ('salt_pers', ZERO, '0 (production writes 0 too)', 'personal property tax: none planned'),
    ('first_mort_bal', ZERO, '0 (production writes 0 too)', 'first mortgage balance (the $750,000 limit): the '
     'balance sheet\'s value.primary_mortgage could fill it (a decision)'),
    ('second_mort_bal', ZERO, '0 (production writes 0 too)', 'second mortgage balance: as first_mort_bal'),
    ('first_mort_year', ZERO, '0 (production writes 0 too)', 'year the first mortgage was taken out; the '
     'calculator overrides it'),
    ('second_mort_year', ZERO, '0 (production writes 0 too)', 'as first_mort_year'),
    ('first_mort_int', SAME, 'first_mort_int (PUF E19200, all deductible interest, as production)',
     'mortgage interest paid'),
    ('second_mort_int', ZERO, '0 (production writes 0 too)', 'interest on a second mortgage: inside first_mort_int'),
    ('inv_int_exp', ZERO, '0 (production writes 0 too)', 'investment interest: inside first_mort_int'),
    ('casualty_exp', SAME, 'casualty_exp', 'casualty and theft losses'),
    ('job_exp', SAME, 'job_exp', 'unreimbursed employee expenses'),
    ('tax_prep_exp', SAME, 'tax_prep_exp', 'tax preparation fees'),
    ('other_misc_exp', SAME, 'other_misc_exp', 'other miscellaneous deductions'),
    ('other_item_exp', SAME, 'other_item_exp', 'other itemized deductions'),
    ('char_cash', SAME, 'char_cash', 'cash gifts to charity'),
    ('char_noncash', SAME, 'char_noncash', 'noncash gifts to charity'),
    # credits and other taxes taken from the PUF
    ('ftc', SAME, 'ftc', 'foreign tax credit'),
    ('amt_ftc', SAME, 'amt_ftc', 'AMT foreign tax credit'),
    ('amt_nols', SAME, 'amt_nols', 'AMT net operating loss and other adjustments'),
    ('excess_ptc', SAME, 'excess_ptc', 'excess advance premium tax credit repaid'),
    ('net_ptc', SAME, 'net_ptc', 'net premium tax credit'),
    ('care_exp', SAME, 'care_exp (the CPS\'s childcare spending; the calculator applies the law\'s caps)',
     'child and dependent care expenses'),
    ('aoc_exp', SAME, 'aoc_exp', 'American opportunity credit expenses'),
    ('llc_exp', SAME, 'llc_exp', 'lifetime learning credit expenses'),
    ('savers_cred', SAME, 'savers_cred', 'retirement savings credit'),
    ('old_cred', SAME, 'old_cred', 'credit for the elderly or disabled'),
    ('res_energy_cred', SAME, 'res_energy_cred', 'residential energy credits'),
    ('gbc', SAME, 'gbc', 'general business credit'),
    ('prior_yr_cred', SAME, 'prior_yr_cred', 'prior-year minimum tax credit'),
    ('other_nonref', SAME, 'other_nonref', 'other nonrefundable credits'),
    ('recapture_tax', SAME, 'recapture_tax', 'recapture taxes'),
    ('ira_penalty', SAME, 'ira_penalty', 'additional tax on IRAs and other plans'),
    # mortality, the balance sheet and its cost bases (read by the frame steps and the estate and wealth taxes,
    # not by the income tax)
    ('q_death1', ZERO, '0', 'chance the primary filer dies this year: the death probabilities rule (NEXT_STEPS 1)'),
    ('q_death2', ZERO, '0', 'chance the spouse dies this year: as q_death1'),
    *[(f'value.{c}', SAME, f'value.{c}', f'balance sheet: {c.replace("_", " ")}')
      for c in ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin',
                'pass_throughs', 'primary_home', 'other_home', 're_fund', 'other_nonfin', 'primary_mortgage',
                'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt']],
    ('basis.equities', COMPUTED, 'value.equities - w_kg_other (the SCF\'s unrealized gain on stocks and funds)',
     'cost basis of equities'),
    ('basis.pass_throughs', COMPUTED, 'value.pass_throughs - w_kg_pass_throughs', 'cost basis of businesses'),
    ('basis.primary_home', COMPUTED, 'value.primary_home - w_kg_primary_home', 'cost basis of the home'),
    ('basis.other_home', COMPUTED, 'value.other_home - w_kg_other_re x value.other_home / (value.other_home + value.re_fund)',
     'cost basis of other homes (the other real estate gain split by value, as production)'),
    ('basis.re_fund', COMPUTED, 'value.re_fund - w_kg_other_re x value.re_fund / (value.other_home + value.re_fund)',
     'cost basis of other real estate'),
    *[(f'accruals.{c}', ZERO, '0', f'yearly unrealized gain on {c.replace("_", " ")} (reporting only): the '
       'yearly unrealized gains rule (NEXT_STEPS step 1)')
      for c in ['equities', 'pass_throughs', 'primary_home', 'other_home', 're_fund', 'dc', 'trusts']],
]
COLUMNS = [c[0] for c in CONTRACT]
assert len(COLUMNS) == len(set(COLUMNS)), 'a column twice in CONTRACT'
NO_AGE_COLUMNS = {'age1': 'age1', 'age2': 'age2', 'dep_age1': 'dep_age1', 'dep_age2': 'dep_age2',
                  'dep_age3': 'dep_age3'}


def convert(f):
    """The year file f as the calculator's input frame, columns in CONTRACT order, and a list of notes (counts of
    what the conversion met). Every rebuild column it reads must be present and have no missing value."""
    notes = []
    x = lambda c: f[c].to_numpy(float)
    # 29 October work copy (explore/2026-10-29/calculator): the retrained file carries columns this branch never read
    # (the survey items' amounts, which are missing where not in their universe, and ssn1 on dependent filers). The
    # check is kept for every column the converter reads; the others' missing values are counted in the notes.
    reads = {s.split(' ')[0].rstrip(';') for n, st, s, _ in CONTRACT if st in (SAME, RENAMED)} | set(NO_AGE_COLUMNS.values()) \
        | {'filer', 'age_range', 'filing_status', 'n_dep', 'spouse_se_share', 'married', 'sole_prop', 'farm', 'part_se', 'se2',
           'sex_primary', 'nontxbl_pens_dist', 'gross_ss', 'other_agi_remainder', 'farm_rent', 'sche_other', 'kg_lt',
           'kg_dist', 'value.other_home', 'value.re_fund', 'w_kg_other_re', 'value.equities', 'w_kg_other', 'value.pass_throughs',
           'w_kg_pass_throughs', 'value.primary_home', 'w_kg_primary_home', 'weight', 'ot', 'ot1', 'ot2'} \
        | {n for n, st, _, _ in CONTRACT if st == ZERO and n in f.columns}
    # dep_ssn1-3 in the retrained file are missing where the record has no k-th dependent (and on a few records
    # whose dependent count and dependent ages disagree): set to 0 here, as the contract's earlier default, and counted.
    f = f.copy()
    for k in (1, 2, 3):
        c = f'dep_ssn{k}'
        if c in f.columns and f[c].isna().any():
            na = f[c].isna().to_numpy()
            has = f['n_dep'].to_numpy() >= k
            notes.append(f'{c} missing: {na.sum():,} records ({(na & ~has).sum():,} with fewer than {k} dependents, '
                         f'{(na & has).sum():,} with a {k}-th dependent); set to 0')
            f[c] = f[c].fillna(0.0)
    gone = [c for c in f.columns if c in reads and f[c].isna().any()]
    assert not gone, f'missing values in the file: {gone}'
    other = {c: int(f[c].isna().sum()) for c in f.columns if c not in reads and f[c].isna().any()}
    notes.append('missing values in columns the converter does not read (records): '
                 + (', '.join(f'{c} {n:,}' for c, n in other.items()) or 'none'))
    out = {}
    for name, status, source, _ in CONTRACT:
        if status in (SAME, RENAMED) and name not in NO_AGE_COLUMNS and name not in ('ot', 'ot1', 'ot2'):
            col = source.split(' ')[0].rstrip(';')
            out[name] = f[col].to_numpy(np.int64) if name == 'id' else x(col)
    for name, col in NO_AGE_COLUMNS.items():
        a = x(col)
        assert ((a == -1) | (a >= 0)).all(), f'{col}: a value below -1'
        out[name] = np.where(a == -1, np.nan, a)
    filer = x('filer') == 1
    notes.append(f'age1 missing (age1 -1): {(np.isnan(out["age1"]) & filer).sum():,} filer records, '
                 f'{(np.isnan(out["age1"]) & ~filer).sum():,} non-filer records; of the filer records '
                 f'{(np.isnan(out["age1"]) & filer & (x("age_range") >= 6)).sum():,} are dependent filers')
    joint = x('filing_status') == 2
    notes.append(f'joint returns with no spouse age (age2 -1): {(joint & np.isnan(out["age2"])).sum():,} '
                 f'records; spouse age on a return that is not joint: {(~joint & ~np.isnan(out["age2"])).sum():,}')
    nd = x('n_dep')
    for k in (1, 2, 3):
        bad = (nd >= k) != ~np.isnan(out[f'dep_age{k}'])
        notes.append(f'dep_age{k} present exactly when n_dep >= {k}: {"yes" if not bad.any() else f"no, {bad.sum():,} records differ"}')
    for k in ('ot', 'ot1', 'ot2'):
        out[k] = x(k) if k in f else np.zeros(len(f))
    notes.append('overtime: ' + ('from the file (ot, ot1, ot2)' if 'ot1' in f else 'not in the file, set to 0'))
    share = x('spouse_se_share')
    assert ((share >= 0) & (share <= 1)).all(), 'spouse_se_share outside [0, 1]'
    married = x('married') == 1
    notes.append(f'spouse_se_share above 0 on a unit that is not married: {(~married & (share > 0)).sum():,} records')
    sole_prop, farm, part_se = x('sole_prop'), x('farm'), x('part_se')
    out['sole_prop2'] = sole_prop * share
    out['sole_prop1'] = sole_prop - out['sole_prop2']
    out['farm2'] = farm * share
    out['farm1'] = farm - out['farm2']
    out['part_se2'] = part_se * share
    out['part_se1'] = part_se - out['part_se2']
    se2 = out['sole_prop2'] + out['farm2']
    notes.append(f'spouse\'s Schedule C plus F against the file\'s se2: largest gap ${np.abs(se2 - x("se2")).max():,.4f}')
    out['dep_status'] = (x('age_range') >= common.ADULT_RANGES).astype(float)
    out['male1'] = (x('sex_primary') == 1).astype(float)
    # Dependent filers have no drawn age or sex (age1 -1); since 29 October the non-microdata imputations draw them
    # (dep_filer_age, dep_filer_sex) and the file Tax-Simulator reads carries them (taxsim_file.build). The taxes
    # computed inside the tilt take the same values, so they are the taxes of the published file (review of
    # rebuild-taxes-in-model, 30 October; until then age1 was missing here, which drops the 65-and-over standard
    # deduction of an older dependent filer).
    if 'dep_filer_age' in f.columns and 'dep_filer_sex' in f.columns:
        dep = (x('age_range') >= common.ADULT_RANGES) & np.isnan(out['age1']) & (x('dep_filer_age') >= 0)
        out['age1'] = np.where(dep, x('dep_filer_age'), out['age1'])
        out['male1'] = np.where(dep, (x('dep_filer_sex') == 1).astype(float), out['male1'])
        notes.append(f'dependent filers given dep_filer_age and dep_filer_sex: {dep.sum():,} records; primary age '
                     f'still missing on {np.isnan(out["age1"]).sum():,} records')
    out['divorce_year'] = np.zeros(len(f))
    # (3 November) gross pensions are taxable pensions plus the drawn non-taxable rest, so they are never below zero
    # and never below taxable pensions (until then retirement_gross - gross_ss, below zero on 15 million units in 2023)
    rest = x('nontxbl_pens_dist')
    assert (rest >= 0).all() and (out['txbl_pens_dist'] >= 0).all(), 'a pension part below zero'
    out['gross_pens_dist'] = out['txbl_pens_dist'] + rest
    if 'gross_pens_dist' in f.columns:
        gap = np.abs(out['gross_pens_dist'] - x('gross_pens_dist')).max()
        assert gap < 1e-6 * max(1.0, np.abs(out['gross_pens_dist']).max()), f'the file\'s gross_pens_dist is not its parts\' sum ({gap})'
    w = x('weight')
    notes.append(f'gross pensions = taxable pensions + the non-taxable rest on every record; the rest above zero on '
                 f'{(rest > 0).sum():,} records, {w[rest > 0].sum() / 1e6:,.3f} million units, '
                 f'${(w * rest).sum() / 1e9:,.2f} billion')
    out['other_inc'] = x('other_agi_remainder') + x('farm_rent') + x('sche_other')
    out['kg_lt'] = x('kg_lt') + x('kg_dist')
    other_re = x('value.other_home') + x('value.re_fund')
    part_home = np.divide(x('value.other_home'), other_re, out=np.zeros(len(f)), where=other_re != 0)
    nowhere = (other_re == 0) & (x('w_kg_other_re') != 0)
    notes.append(f'other real estate gain with no other real estate to hold it: {nowhere.sum():,} records '
                 '(basis.other_home takes all of it)')
    part_home = np.where(nowhere, 1.0, part_home)
    out['basis.equities'] = x('value.equities') - x('w_kg_other')
    out['basis.pass_throughs'] = x('value.pass_throughs') - x('w_kg_pass_throughs')
    out['basis.primary_home'] = x('value.primary_home') - x('w_kg_primary_home')
    out['basis.other_home'] = x('value.other_home') - x('w_kg_other_re') * part_home
    out['basis.re_fund'] = x('value.re_fund') - x('w_kg_other_re') * (1 - part_home)
    # 29 October work copy: a column the contract sets to 0 is taken from the file when the file now has it (the
    # retrain's non-microdata imputations: blindness, the child credit's eligibility, dependents' SSNs, the QBI wage
    # bills and specified-service flags, death chances, gains' basis and holding period, yearly unrealized gains).
    from_file = []
    for name, status, _, _ in CONTRACT:
        if status == ZERO:
            if name in f.columns:
                out[name] = x(name)
                from_file.append(name)
            else:
                out[name] = np.zeros(len(f))
    notes.append('set to 0 in the contract but taken from the file (29 October): ' + (', '.join(from_file) or 'none'))
    notes.append('still 0: ' + ', '.join(n for n, st, _, _ in CONTRACT if st == ZERO and n not in from_file))
    # identities the calculator relies on (Tax-Simulator's split-sum rule)
    for tot, a, b in [('wages', 'wages1', 'wages2'), ('tips', 'tips1', 'tips2'), ('ot', 'ot1', 'ot2'),
                      ('sole_prop', 'sole_prop1', 'sole_prop2'), ('farm', 'farm1', 'farm2')]:
        gap = np.abs(out[tot] - out[a] - out[b]).max()
        assert gap < 1e-6 * max(1.0, np.abs(out[tot]).max()), f'{tot} != {a} + {b} (largest gap {gap})'
    for k in (1, 2):
        base = out[f'wages{k}'] - out[f'tips{k}'] - out[f'ot{k}']
        notes.append(f'earner {k}: wages less tips less overtime below zero on {(base < -1e-6).sum():,} records')
    frame = pd.DataFrame({c: out[c] for c in COLUMNS})
    assert frame['id'].is_unique, 'record_id is not unique'
    assert frame['filing_status'].isin([1, 2, 3, 4]).all(), 'a filing status outside 1-4'
    return frame, notes
