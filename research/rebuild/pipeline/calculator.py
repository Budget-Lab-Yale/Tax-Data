"""The rebuild's year file as the input of the GPU tax calculator, and the calculator run on every record.

An experiment (John, 28 October): every record's federal income tax under the target year's law, from the GPU tax
calculator (repository tax-simulator-gpu) used as a library. Nothing here changes the file or feeds the tilt.

Three steps, all run by `main` (stage `calculator` of run.sbatch):

1. The pinned calculator. The calculator's code is read from a copy of its repository at one commit (settings
   calculator.commit), written once with `git archive` under <out>/lib/tax-simulator-gpu_<first 12 characters of
   the commit>. Later edits to the repository do not reach a run; moving to a newer version is the one setting.
2. The converter (`convert`). The year file becomes a frame with Tax-Simulator's column names, one row per record,
   in Tax-Data's form (the overlapping wage columns wages, wages1, wages2, tips, ot, sole_prop, farm as Tax-Data
   writes them). Each column is the rebuild's own column under the same name, renamed, computed from the rebuild's
   columns, or 0 where the rebuild does not have it yet. CONTRACT below is the list, one row per column; it is
   written beside the outputs as calculator_contract.csv and is the source of research/rebuild/CALCULATOR_CONTRACT.csv.
3. The run. calculator_run.py, under the calculator's own Python environment (settings calculator.python) with the
   pinned copy's source first on the path, applies the calculator's own preparation (the wage split and the random
   draws keyed by record id, as its bridge does), its frame steps under the year's law, and its tax chain, and
   writes every computed column per record (calculator_<year>.parquet).

Paths: input <tilt>/file_<year>.parquet (REBUILD_CALC_INPUT overrides it); outputs <out>/calculator/<run>/
(REBUILD_CALC_OUT overrides it).
"""
import json
import os
import subprocess
import sys

import numpy as np
import pandas as pd

import common

Y = common.TARGET_YEAR
S = common.SETTINGS['calculator']
INPUT = os.environ.get('REBUILD_CALC_INPUT', f'{common.TILT}/file_{Y}.parquet')
OUT = os.environ.get('REBUILD_CALC_OUT', f'{common.OUT}/calculator/{common.RUN}')
LIB = f'{common.OUT}/lib/tax-simulator-gpu_{S["commit"][:12]}'
HERE = os.path.dirname(os.path.abspath(__file__))

# ---- the contract ---------------------------------------------------------------------------------------------
# One row per column: (Tax-Simulator's name, status, where it comes from, what it is / what would fill it).
# status: same (the rebuild's column, same name), renamed (the rebuild's column under another name), computed (a
# formula on the rebuild's columns), zero (0 for now: the rebuild does not have it; the last field says what would
# fill it). The calculator reads every one of these; calculator_run.py checks that against the pinned code.
SAME, RENAMED, COMPUTED, ZERO = 'same', 'renamed', 'computed', 'zero'
NO_AGE = 'NaN where the rebuild has -1 (none), as Tax-Data writes it'
CONTRACT = [
    # the record
    ('id', RENAMED, 'record_id', 'record id, unique in the file'),
    ('weight', SAME, 'weight (after the tilt); weight_start is the weight before it', 'tax units the record stands for'),
    ('filer', SAME, 'filer', '1 files a return, 0 a non-filer'),
    ('filing_status', RENAMED, 'mars', '1 single, 2 joint (and surviving spouses), 3 separate, 4 head of household'),
    ('dep_status', COMPUTED, 'age_range in 6, 7, 8 (the PUF\'s three dependent-filer ranges)',
     '1 if the return is filed by someone claimed as a dependent'),
    ('age1', RENAMED, f'age_primary; {NO_AGE} (the dependent filers, about 6% of filers)',
     'age of the primary filer, 80 for 80 and over'),
    ('age2', RENAMED, f'age_spouse; {NO_AGE}', 'age of the spouse'),
    ('male1', COMPUTED, 'sex_primary == 1 (0 also where the rebuild has no sex: the dependent filers)',
     'primary filer is a man; the frame steps read it (chance of being a surviving spouse), not the tax chain'),
    ('blind1', ZERO, '0', 'primary filer is blind: the blindness rule (NEXT_STEPS step 1)'),
    ('blind2', ZERO, '0', 'spouse is blind: the blindness rule (NEXT_STEPS step 1)'),
    ('n_dep', SAME, 'n_dep (the rebuild caps it at 3)', 'number of dependents'),
    ('n_dep_eitc', RENAMED, 'n_eitc (PUF EIC, capped at 3)', 'number of EITC qualifying children'),
    ('dep_age1', RENAMED, f'age_dep1; {NO_AGE}', 'age of the first dependent'),
    ('dep_age2', RENAMED, f'age_dep2; {NO_AGE}', 'age of the second dependent'),
    ('dep_age3', RENAMED, f'age_dep3; {NO_AGE}', 'age of the third dependent'),
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
    ('wages2', SAME, 'wages2', 'spouse\'s wages'),
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
    ('sole_prop', RENAMED, 'sch_c', 'Schedule C net income of the unit'),
    ('sole_prop1', COMPUTED, 'sch_c - sole_prop2', 'primary filer\'s Schedule C income'),
    ('sole_prop2', COMPUTED, 'sch_c x spouse_se_share (production splits by one self-employment share too)',
     'spouse\'s Schedule C income'),
    ('farm', SAME, 'farm', 'Schedule F net farm income of the unit'),
    ('farm1', COMPUTED, 'farm - farm2', 'primary filer\'s farm income'),
    ('farm2', COMPUTED, 'farm x spouse_se_share', 'spouse\'s farm income'),
    ('part_se1', COMPUTED, 'part_se - part_se2', 'primary filer\'s partnership income subject to self-employment tax'),
    ('part_se2', COMPUTED, 'part_se x spouse_se_share', 'spouse\'s partnership income subject to self-employment tax'),
    ('ei_prior_yr', ZERO, '0 (production writes 0 too)', 'earned income last year (child credit look-back; off under '
     '2019 law): no source planned'),
    # Schedule E
    ('part_active', RENAMED, 'part_nonpass_inc', 'nonpassive partnership income'),
    ('part_active_loss', RENAMED, 'part_nonpass_loss', 'nonpassive partnership loss, stored positive'),
    ('part_passive', RENAMED, 'part_pass_inc', 'passive partnership income'),
    ('part_passive_loss', RENAMED, 'part_pass_loss', 'passive partnership loss, stored positive'),
    ('part_179', SAME, 'part_179', 'partnership section 179 deduction'),
    ('scorp_active', RENAMED, 'scorp_nonpass_inc', 'nonpassive S corporation income'),
    ('scorp_active_loss', RENAMED, 'scorp_nonpass_loss', 'nonpassive S corporation loss, stored positive'),
    ('scorp_passive', RENAMED, 'scorp_pass_inc', 'passive S corporation income'),
    ('scorp_passive_loss', RENAMED, 'scorp_pass_loss', 'passive S corporation loss, stored positive'),
    ('scorp_179', SAME, 'scorp_179', 'S corporation section 179 deduction'),
    ('rent', RENAMED, 'rent_inc', 'rents and royalties, net income'),
    ('rent_loss', SAME, 'rent_loss', 'rents and royalties, net loss, stored positive'),
    ('estate', RENAMED, 'estate_inc', 'estate and trust net income'),
    ('estate_loss', SAME, 'estate_loss', 'estate and trust net loss, stored positive'),
    ('wagebill_sole_prop', ZERO, '0', 'W-2 wages paid by the sole proprietorship (QBI wage limit): the QBI wage rule '
     '(NEXT_STEPS step 1)'),
    ('wagebill_part', ZERO, '0', 'W-2 wages paid by the partnership (QBI): as wagebill_sole_prop'),
    ('wagebill_scorp', ZERO, '0', 'W-2 wages paid by the S corporation (QBI): as wagebill_sole_prop'),
    ('wagebill_farm', ZERO, '0', 'W-2 wages paid by the farm (QBI): as wagebill_sole_prop'),
    ('sstb_sole_prop', ZERO, '0', 'the sole proprietorship is a specified service business (QBI): the QBI '
     'specified-service rule (NEXT_STEPS step 1)'),
    ('sstb_part', ZERO, '0', 'the partnership is a specified service business: as sstb_sole_prop'),
    ('sstb_scorp', ZERO, '0', 'the S corporation is a specified service business: as sstb_sole_prop'),
    ('sstb_farm', ZERO, '0', 'the farm is a specified service business: as sstb_sole_prop'),
    # other income
    ('txbl_int', RENAMED, 'interest', 'taxable interest'),
    ('exempt_int', RENAMED, 'tax_exempt_interest', 'tax-exempt interest'),
    ('div_ord', SAME, 'div_ord', 'ordinary dividends that are not qualified'),
    ('div_pref', SAME, 'div_pref', 'qualified dividends'),
    ('state_ref', RENAMED, 'state_refunds', 'taxable state income tax refunds'),
    ('txbl_ira_dist', RENAMED, 'ira_taxable', 'taxable IRA distributions'),
    ('txbl_pens_dist', RENAMED, 'pensions_taxable', 'taxable pensions and annuities'),
    ('gross_pens_dist', COMPUTED, 'retirement_gross - ss_gross (retirement_gross is the PUF\'s E02400 + E01500)',
     'gross pensions and annuities'),
    ('gross_ss', RENAMED, 'ss_gross', 'gross Social Security benefits (the calculator computes the taxable part)'),
    ('ui', RENAMED, 'unemployment', 'unemployment compensation'),
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
    ('hsa_contr', RENAMED, 'hsa_ded', 'HSA deduction'),
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
    ('salt_prop', RENAMED, 'real_estate_tax', 'real estate tax'),
    ('salt_pers', ZERO, '0 (production writes 0 too)', 'personal property tax: none planned'),
    ('first_mort_bal', ZERO, '0 (production writes 0 too)', 'first mortgage balance (the $750,000 limit): the '
     'balance sheet\'s w_primary_mortgage could fill it (a decision)'),
    ('second_mort_bal', ZERO, '0 (production writes 0 too)', 'second mortgage balance: as first_mort_bal'),
    ('first_mort_year', ZERO, '0 (production writes 0 too)', 'year the first mortgage was taken out; the '
     'calculator overrides it'),
    ('second_mort_year', ZERO, '0 (production writes 0 too)', 'as first_mort_year'),
    ('first_mort_int', RENAMED, 'mortgage_interest (PUF E19200, all deductible interest, as production)',
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
    ('care_exp', RENAMED, 'childcare (the CPS\'s childcare spending; the calculator applies the law\'s caps)',
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
    *[(f'value.{c}', RENAMED, f'w_{c}', f'balance sheet: {c.replace("_", " ")}')
      for c in ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin',
                'pass_throughs', 'primary_home', 'other_home', 're_fund', 'other_nonfin', 'primary_mortgage',
                'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt']],
    ('basis.equities', COMPUTED, 'w_equities - w_kg_other (the SCF\'s unrealized gain on stocks and funds)',
     'cost basis of equities'),
    ('basis.pass_throughs', COMPUTED, 'w_pass_throughs - w_kg_pass_throughs', 'cost basis of businesses'),
    ('basis.primary_home', COMPUTED, 'w_primary_home - w_kg_primary_home', 'cost basis of the home'),
    ('basis.other_home', COMPUTED, 'w_other_home - w_kg_other_re x w_other_home / (w_other_home + w_re_fund)',
     'cost basis of other homes (the other real estate gain split by value, as production)'),
    ('basis.re_fund', COMPUTED, 'w_re_fund - w_kg_other_re x w_re_fund / (w_other_home + w_re_fund)',
     'cost basis of other real estate'),
    *[(f'accruals.{c}', ZERO, '0', f'yearly unrealized gain on {c.replace("_", " ")} (reporting only): the '
       'yearly unrealized gains rule (NEXT_STEPS step 1)')
      for c in ['equities', 'pass_throughs', 'primary_home', 'other_home', 're_fund', 'dc', 'trusts']],
]
COLUMNS = [c[0] for c in CONTRACT]
assert len(COLUMNS) == len(set(COLUMNS)), 'a column twice in CONTRACT'
NO_AGE_COLUMNS = {'age1': 'age_primary', 'age2': 'age_spouse', 'dep_age1': 'age_dep1', 'dep_age2': 'age_dep2',
                  'dep_age3': 'age_dep3'}


def convert(f):
    """The year file f as the calculator's input frame, columns in CONTRACT order, and a list of notes (counts of
    what the conversion met). Every rebuild column it reads must be present and have no missing value."""
    notes = []
    x = lambda c: f[c].to_numpy(float)
    # 29 October work copy (explore/2026-10-29/calculator): the retrained file carries columns this branch never read
    # (the survey items' amounts, which are missing where not in their universe, and ssn1 on dependent filers). The
    # check is kept for every column the converter reads; the others' missing values are counted in the notes.
    reads = {s.split(' ')[0].rstrip(';') for n, st, s, _ in CONTRACT if st in (SAME, RENAMED)} | set(NO_AGE_COLUMNS.values()) \
        | {'filer', 'age_range', 'mars', 'n_dep', 'spouse_se_share', 'married', 'sch_c', 'farm', 'part_se', 'se2',
           'sex_primary', 'retirement_gross', 'ss_gross', 'other_agi_remainder', 'farm_rent', 'sche_other', 'kg_lt',
           'kg_dist', 'w_other_home', 'w_re_fund', 'w_kg_other_re', 'w_equities', 'w_kg_other', 'w_pass_throughs',
           'w_kg_pass_throughs', 'w_primary_home', 'w_kg_primary_home', 'weight', 'ot', 'ot1', 'ot2'} \
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
    notes.append(f'age1 missing (age_primary -1): {(np.isnan(out["age1"]) & filer).sum():,} filer records, '
                 f'{(np.isnan(out["age1"]) & ~filer).sum():,} non-filer records; of the filer records '
                 f'{(np.isnan(out["age1"]) & filer & (x("age_range") >= 6)).sum():,} are dependent filers')
    joint = x('mars') == 2
    notes.append(f'joint returns with no spouse age (age_spouse -1): {(joint & np.isnan(out["age2"])).sum():,} '
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
    sch_c, farm, part_se = x('sch_c'), x('farm'), x('part_se')
    out['sole_prop2'] = sch_c * share
    out['sole_prop1'] = sch_c - out['sole_prop2']
    out['farm2'] = farm * share
    out['farm1'] = farm - out['farm2']
    out['part_se2'] = part_se * share
    out['part_se1'] = part_se - out['part_se2']
    se2 = out['sole_prop2'] + out['farm2']
    notes.append(f'spouse\'s Schedule C plus F against the file\'s se2: largest gap ${np.abs(se2 - x("se2")).max():,.4f}')
    out['dep_status'] = (x('age_range') >= common.ADULT_RANGES).astype(float)
    out['male1'] = (x('sex_primary') == 1).astype(float)
    out['divorce_year'] = np.zeros(len(f))
    out['gross_pens_dist'] = x('retirement_gross') - x('ss_gross')
    neg = out['gross_pens_dist'] < 0
    w = x('weight')
    notes.append(f'gross pensions negative (retirement_gross below ss_gross): {neg.sum():,} records, '
                 f'{w[neg].sum() / 1e6:,.3f} million units, ${(w * out["gross_pens_dist"])[neg].sum() / 1e9:,.2f} billion '
                 '(passed as they are; the calculator reads gross pensions only for expanded income and the SALT '
                 'workaround\'s income shares)')
    over = out['txbl_pens_dist'] > out['gross_pens_dist'] + 0.5
    notes.append(f'taxable pensions above gross pensions: {over.sum():,} records, {w[over].sum() / 1e6:,.3f} million units')
    out['other_inc'] = x('other_agi_remainder') + x('farm_rent') + x('sche_other')
    out['kg_lt'] = x('kg_lt') + x('kg_dist')
    other_re = x('w_other_home') + x('w_re_fund')
    part_home = np.divide(x('w_other_home'), other_re, out=np.zeros(len(f)), where=other_re != 0)
    nowhere = (other_re == 0) & (x('w_kg_other_re') != 0)
    notes.append(f'other real estate gain with no other real estate to hold it: {nowhere.sum():,} records '
                 '(basis.other_home takes all of it)')
    part_home = np.where(nowhere, 1.0, part_home)
    out['basis.equities'] = x('w_equities') - x('w_kg_other')
    out['basis.pass_throughs'] = x('w_pass_throughs') - x('w_kg_pass_throughs')
    out['basis.primary_home'] = x('w_primary_home') - x('w_kg_primary_home')
    out['basis.other_home'] = x('w_other_home') - x('w_kg_other_re') * part_home
    out['basis.re_fund'] = x('w_re_fund') - x('w_kg_other_re') * (1 - part_home)
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


def rebuild_agi(f):
    """The rebuild's own AGI per record (dollars.agi: wages, interest, dividends, Schedule D net gain, other AGI)."""
    return f[['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']].to_numpy(float).sum(1)


def pin():
    """The pinned copy of the calculator: written once from the repository with git archive; checked every run."""
    stamp = f'{LIB}/PINNED_COMMIT'
    if not os.path.exists(stamp):
        os.makedirs(LIB, exist_ok=True)
        archive = subprocess.run(['git', '-C', S['repo'], 'archive', S['commit']], check=True, capture_output=True)
        subprocess.run(['tar', '-x', '-C', LIB], input=archive.stdout, check=True)
        with open(stamp, 'w') as fh:
            fh.write(S['commit'] + '\n')
    got = open(stamp).read().strip()
    assert got == S['commit'], f'{LIB} holds commit {got}, settings say {S["commit"]}'
    return LIB


def main():
    os.makedirs(OUT, exist_ok=True)
    lib = pin()
    common.say(f'calculator: tax-simulator-gpu at {S["commit"]}, copy {lib}')
    common.say(f'input {INPUT}\noutputs {OUT}')
    f = pd.read_parquet(INPUT)
    frame, notes = convert(f)
    frame.to_parquet(f'{OUT}/calculator_input_{Y}.parquet', index=False)
    pd.DataFrame(CONTRACT, columns=['column', 'status', 'from the rebuild', 'what it is, or what would fill it']) \
        .to_csv(f'{OUT}/calculator_contract.csv', index=False)
    with open(f'{OUT}/conversion_notes.txt', 'w') as fh:
        fh.write('\n'.join(notes) + '\n')
    common.say('conversion:\n  ' + '\n  '.join(notes))
    counts = pd.Series([c[1] for c in CONTRACT]).value_counts()
    common.say('columns by status: ' + ', '.join(f'{k} {v}' for k, v in counts.items()))
    # The run, in the calculator's own environment: module paths cleared so its GPU libraries are its own.
    env = {k: v for k, v in os.environ.items() if k not in ('PYTHONPATH', 'LD_LIBRARY_PATH', 'PYTHONHOME')}
    env.update(PYTHONPATH=f'{lib}/src', PYTHONUNBUFFERED='1')
    cmd = [S['python'], f'{HERE}/calculator_run.py', '--input', f'{OUT}/calculator_input_{Y}.parquet',
           '--out', OUT, '--year', str(S.get('law_year') or Y), '--lib', lib, '--seed', str(S['draw_seed']),
           '--commit', S['commit']]
    common.say('running: ' + ' '.join(cmd))
    subprocess.run(cmd, cwd=lib, env=env, check=True)


if __name__ == '__main__':
    sys.exit(main())
