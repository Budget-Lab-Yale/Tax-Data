"""The rebuild's year file as the file Tax-Simulator reads: tax_units_<year>.csv in production's form.

NEXT_STEPS item 2 (John, 29 October: "all the way through"). Production's Tax-Data writes one CSV per year with 188
columns (production vintage 2026100117_review_fixes); Tax-Simulator reads it by name. This script writes the rebuild's
file with the same 188 columns, in the same order, with production's conventions for what is missing, and the list of
where each column comes from (COLUMNS below, written as TAXSIM_FILE_COLUMNS.csv beside the output and, from it,
research/rebuild/TAXSIM_FILE_COLUMNS.csv).

How each column is made:

1. The 165 columns the GPU tax calculator reads come from the calculator experiment's converter (calculator.convert,
   CALCULATOR_CONTRACT.md). The 27 columns that experiment set to 0 as rules still to build (blindness, Social Security
   numbers and child credit eligibility of dependents, QBI wage bills and specified-service flags, the basis and
   holding period of long-term gains, death probabilities, yearly unrealized gains) are now the file's own columns,
   made by the non-microdata imputations and by dollars.py; the converter takes them from the file.
2. The other 23 columns of production's file: male2 (the spouse's sex), n_dep_ctc (the PUF's child credit count),
   parent_rank, part_se and the eight consumption categories come from the file; nols is 0 as production writes it;
   prim_mort_share is computed from the drawn balance sheet; the nine forbes_* columns take production's default for
   an ordinary record (forbes_flag 0, the rest missing), since the billionaire records are not in this file.
3. Production's conventions for missing values (checked on its 2019 file): the spouse's age, sex, blindness and death
   probability are missing (NA) without a spouse; a dependent's age, Social Security number and child credit flag
   are missing where there is no such dependent; divorce_year is 0 where alimony is paid or received and missing
   otherwise; the holding period and basis of long-term gains are missing where the long-term gain is 0. The primary
   filer always has an age: dependent filers take the age and sex the non-microdata imputations drew for them.
   Flags are written TRUE/FALSE where production writes them so (blindness, the dependents' flags, the specified-
   service flags). Missing values are written NA, as production writes them.

Every rule that departs from production, and every column set to a constant, is in COLUMNS with its reason, and the
counts each rule touched are written to taxsim_file_notes.txt.

Paths: input <tilt>/file_<year>.parquet (REBUILD_TAXSIM_INPUT overrides it); output folder
<out>/taxsim/<run>/ (REBUILD_TAXSIM_OUT overrides it).
"""
import os
import sys

import numpy as np
import pandas as pd

import calculator as C
import common

Y = common.TARGET_YEAR
INPUT = os.environ.get('REBUILD_TAXSIM_INPUT', f'{common.TILT}/file_{Y}.parquet')
OUT = os.environ.get('REBUILD_TAXSIM_OUT', f'{common.OUT}/taxsim/{common.RUN}')

# Production's 188 columns, in its order (Tax-Data vintage 2026100117_review_fixes, tax_units_2019.csv).
PRODUCTION_COLUMNS = (
    'id weight filer dep_status filing_status male1 male2 age1 age2 blind1 blind2 parent_rank n_dep dep_age1 dep_age2 '
    'dep_age3 dep_ssn1 dep_ssn2 dep_ssn3 n_dep_ctc dep_ctc1 dep_ctc2 dep_ctc3 n_dep_eitc wages wages1 wages2 ot ot1 '
    'ot2 tips tips1 tips2 tips_lh1 tips_lh2 ei_prior_yr trad_contr_er1 trad_contr_er2 txbl_int exempt_int div_ord '
    'div_pref state_ref alimony divorce_year sole_prop sole_prop1 sole_prop2 wagebill_sole_prop sstb_sole_prop kg_st '
    'kg_lt kg_lt_years_held kg_lt_basis kg_1250 kg_collect other_gains txbl_ira_dist gross_pens_dist txbl_pens_dist '
    'part_active part_active_loss part_passive part_passive_loss part_179 part_se part_se1 part_se2 wagebill_part '
    'sstb_part scorp_active scorp_active_loss scorp_passive scorp_passive_loss scorp_179 wagebill_scorp sstb_scorp '
    'rent rent_loss estate estate_loss farm farm1 farm2 wagebill_farm sstb_farm ui gross_ss nols other_inc ed_exp '
    'hsa_contr trad_contr_ira sl_int_ded keogh_contr se_health early_penalty alimony_exp tuition_ded dpad char_cash '
    'char_noncash other_above_ded auto_int_exp med_exp salt_inc_sales salt_prop salt_pers first_mort_bal '
    'second_mort_bal first_mort_year second_mort_year first_mort_int second_mort_int prim_mort_share inv_int_exp '
    'casualty_exp job_exp tax_prep_exp other_misc_exp other_item_exp amt_nols amt_ftc excess_ptc ftc care_exp aoc_exp '
    'llc_exp old_cred savers_cred res_energy_cred gbc prior_yr_cred other_nonref net_ptc recapture_tax ira_penalty '
    'c_clothing c_motor_vehicles c_durables c_other_nondurables c_food_off_premises c_gasoline c_housing_utilities '
    'c_other_services_health value.cash value.equities value.bonds value.dc value.db value.life_ins value.annuities '
    'value.trusts value.other_fin value.pass_throughs value.primary_home value.other_home value.re_fund '
    'value.other_nonfin value.primary_mortgage value.other_mortgage value.credit_lines value.credit_cards '
    'value.installment_debt value.other_debt basis.primary_home basis.other_home basis.re_fund basis.pass_throughs '
    'basis.equities accruals.equities accruals.pass_throughs accruals.primary_home accruals.other_home '
    'accruals.re_fund accruals.dc accruals.trusts q_death1 q_death2 forbes_flag forbes_source_year forbes_rank '
    'forbes_name forbes_source_category forbes_rank_group forbes_donor_id forbes_net_worth forbes_fiscal_income'
).split()
assert len(PRODUCTION_COLUMNS) == 188 and len(set(PRODUCTION_COLUMNS)) == 188

# Production columns Tax-Simulator's R model (main, 145a80eb1) never names in its code, comments excluded: a scan of
# src/ (tests excluded) on 29 October. Every other column of the 188 it reads. Its input step checks each calculation's
# required columns by name (parse_calc_fn_input), so a run that finishes has found every column it needs.
NOT_READ_BY_R = set(
    'parent_rank part_se nols prim_mort_share c_clothing c_motor_vehicles c_durables c_other_nondurables '
    'c_food_off_premises c_gasoline c_housing_utilities c_other_services_health q_death1 q_death2'.split()
    + [c for c in PRODUCTION_COLUMNS if c.startswith(('value.', 'basis.', 'accruals.', 'forbes_'))])

CONSUMPTION = ['c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables', 'c_food_off_premises',
               'c_gasoline', 'c_housing_utilities', 'c_other_services_health']
FORBES_DEFAULTS = {'forbes_flag': 0, 'forbes_source_year': np.nan, 'forbes_rank': np.nan, 'forbes_name': np.nan,
                   'forbes_source_category': np.nan, 'forbes_rank_group': np.nan, 'forbes_donor_id': np.nan,
                   'forbes_net_worth': np.nan, 'forbes_fiscal_income': np.nan}
FLAGS = ['blind1', 'blind2', 'dep_ssn1', 'dep_ssn2', 'dep_ssn3', 'dep_ctc1', 'dep_ctc2', 'dep_ctc3',
         'sstb_sole_prop', 'sstb_part', 'sstb_scorp', 'sstb_farm']   # written TRUE/FALSE, as production writes them
INTEGERS = ['id', 'filer', 'dep_status', 'filing_status', 'male1', 'age1', 'parent_rank', 'n_dep', 'n_dep_ctc',
            'n_dep_eitc', 'tips_lh1', 'tips_lh2', 'nols', 'forbes_flag']

# Columns made here (not by calculator.convert), and the rules this script applies on top of the converter.
EXTRA = {
    'male2': ('computed', 'sex_spouse == 1 (1 man, 0 woman); missing without a spouse, as production'),
    'parent_rank': ('same', 'parent_rank (non-microdata imputations, production\'s method)'),
    'n_dep_ctc': ('renamed', 'n_ctc (the PUF\'s child credit children, N24, held to 3)'),
    'part_se': ('same', 'part_se (production\'s formula on the PUF)'),
    'nols': ('zero', '0, as production writes it (no source in either)'),
    'prim_mort_share': ('computed', 'w_primary_mortgage / (w_primary_mortgage + w_other_mortgage) from the drawn '
                        'balance sheet, each balance read as at least 0; 1 where there is no mortgage (production\'s default); production imputes '
                        'it from the SCF by a forest. Tax-Simulator does not read it'),
    **{c: ('same', f'{c} (consumption, CEX)') for c in CONSUMPTION},
    **{c: ('default', 'production\'s value for an ordinary record (' + ('0' if c == 'forbes_flag' else 'missing') +
           '); the Forbes 400 records are on another branch, not in this file. Tax-Simulator does not read it')
       for c in FORBES_DEFAULTS},
}
RULES = {
    'age1': 'age_primary; for dependent filers (age_primary -1) dep_filer_age, drawn by the non-microdata imputations '
            '(production gives every record an age)',
    'male1': 'sex_primary == 1; for dependent filers dep_filer_sex == 1',
    'age2': 'age_spouse; missing without a spouse',
    'blind2': 'blind2; missing without a spouse, as production',
    'q_death2': 'q_death2; missing without a spouse (the file\'s -1), as production',
    'dep_ssn1': 'dep_ssn1 (dollars.py, from the drawn CPS status); missing where dep_age1 is missing, as production',
    'dep_ssn2': 'as dep_ssn1', 'dep_ssn3': 'as dep_ssn1',
    'dep_ctc1': 'dep_ctc1 (non-microdata imputations); missing where dep_age1 is missing, as production',
    'dep_ctc2': 'as dep_ctc1', 'dep_ctc3': 'as dep_ctc1',
    'divorce_year': '0 where alimony is received or paid, else missing (production\'s rule). Tax-Simulator counts '
                    'alimony only with a divorce year before 2019',
    'kg_lt_years_held': 'kg_lt_years_held; missing where kg_lt (with capital gain distributions) is 0, as '
                        'production; 0 where kg_lt is not 0 but the file has no holding period (capital gain '
                        'distributions alone): Tax-Simulator then makes no indexing adjustment, which current law '
                        'does not have',
    'kg_lt_basis': 'kg_lt_basis; missing where kg_lt is 0; 0 where the holding period is set to 0 (above)',
}


BASIS_FROM_FILE = ['basis.equities', 'basis.pass_throughs', 'basis.primary_home', 'basis.other_home', 'basis.re_fund']


def flag(v):
    """1/0/NaN as TRUE/FALSE/NA text."""
    return np.where(np.isnan(v), 'NA', np.where(v == 1, 'TRUE', 'FALSE')).astype(object)


def build(f):
    """The 188-column frame (numbers; flags still 1/0/NaN) and a list of notes."""
    frame, notes = C.convert(f)
    x = lambda c: f[c].to_numpy(float)
    n = len(f)
    out = {c: frame[c].to_numpy() for c in frame.columns}
    filer = x('filer') == 1
    w = x('weight')
    wm = lambda m: f'{int(m.sum()):,} records, {w[m].sum() / 1e6:,.3f} million units'

    # the primary filer: an age and a sex on every record
    dep = x('age_range') >= common.ADULT_RANGES
    assert (dep == (x('age_primary') == -1)).all(), 'age_primary is -1 exactly on the dependent filers: not so'
    a1 = np.where(dep, x('dep_filer_age'), x('age_primary'))
    assert (a1 >= 0).all(), f'records with no primary age after the dependent filers\' ages: {(a1 < 0).sum()}'
    out['age1'] = a1
    s1 = np.where(dep, x('dep_filer_sex'), x('sex_primary'))
    assert np.isin(s1, [1, 2]).all(), f'primary sex outside 1-2 on {(~np.isin(s1, [1, 2])).sum()} records'
    out['male1'] = (s1 == 1).astype(float)
    notes.append(f'dependent filers given dep_filer_age and dep_filer_sex: {wm(dep)}; ages {a1[dep].min():.0f} to '
                 f'{a1[dep].max():.0f}')

    # the spouse
    spouse = x('age_spouse') >= 0
    joint = x('mars') == 2
    notes.append(f'joint returns with no spouse age: {wm(joint & ~spouse)} (age2, male2, blind2, q_death2 missing '
                 'there; Tax-Simulator reads a missing age2 as under 65); spouse age on a return that is not joint: '
                 f'{wm(~joint & spouse)}')
    sx2 = x('sex_spouse')
    assert np.isin(sx2[spouse], [1, 2]).all(), 'a spouse with no sex'
    out['male2'] = np.where(spouse, (sx2 == 1).astype(float), np.nan)
    b2 = x('blind2')
    notes.append(f'blind2 set on records without a spouse (made missing): {wm(~spouse & (b2 == 1))}')
    out['blind2'] = np.where(spouse, b2, np.nan)
    qd2 = x('q_death2')
    notes.append(f'q_death2 at or above 0 on records without a spouse (made missing): {wm(~spouse & (qd2 >= 0))}')
    out['q_death2'] = np.where(spouse & (qd2 >= 0), qd2, np.nan)
    notes.append(f'q_death2 missing on records with a spouse: {wm(spouse & (qd2 < 0))}')
    qd1 = x('q_death1')
    assert ((qd1 >= 0) & (qd1 <= 1)).all(), 'q_death1 outside 0-1'

    # the dependents
    nd = x('n_dep')
    for k in (1, 2, 3):
        has = ~np.isnan(out[f'dep_age{k}'])
        for c in (f'dep_ssn{k}', f'dep_ctc{k}'):
            v = x(c)
            gap = has & ~np.isin(v, [0, 1])
            notes.append(f'{c}: dependent present (dep_age{k}) but no 0/1 value in the file: {wm(gap)}'
                         + ('; set to 1 (has a number), the status of nearly every dependent' if c.startswith('dep_ssn')
                            else '; set to 0 (not eligible)'))
            v = np.where(gap, 1.0 if c.startswith('dep_ssn') else 0.0, v)
            out[c] = np.where(has, v, np.nan)
        notes.append(f'n_dep >= {k} but no dep_age{k}: {wm((nd >= k) & ~has)}; dep_age{k} but n_dep < {k}: '
                     f'{wm((nd < k) & has)} (left as in the file)')
    out['n_dep_ctc'] = x('n_ctc')
    for c in ('n_dep_ctc', 'n_dep_eitc'):   # the PUF allows more EITC (or credit) children than dependents: kept
        notes.append(f'{c} above n_dep (left as drawn): {wm(out[c] > nd)}')

    # divorce year: production's rule
    al = (x('alimony') != 0) | (x('alimony_exp') != 0)
    out['divorce_year'] = np.where(al, 0.0, np.nan)
    notes.append(f'divorce_year 0 (alimony received or paid): {wm(al)}')

    # long-term gains' holding period and basis
    kg_lt = out['kg_lt']
    yrs, basis = x('kg_lt_years_held'), x('kg_lt_basis')
    none = (kg_lt != 0) & (yrs < 0)
    notes.append(f'kg_lt not 0 but no holding period in the file (holding period and basis set to 0): {wm(none)}, '
                 f'${(w * kg_lt)[none].sum() / 1e9:,.2f} billion of kg_lt; of these with kg_dist alone '
                 f'{int((none & (x("kg_lt") == 0)).sum()):,} records')
    stray = (kg_lt == 0) & (yrs >= 0)
    notes.append(f'kg_lt 0 but a holding period in the file (made missing): {wm(stray)}')
    out['kg_lt_years_held'] = np.where(kg_lt == 0, np.nan, np.where(none, 0.0, yrs))
    out['kg_lt_basis'] = np.where(kg_lt == 0, np.nan, np.where(none, 0.0, basis))

    # flags: 0/1 in the file
    for c in ('blind1', 'sstb_sole_prop', 'sstb_part', 'sstb_scorp', 'sstb_farm'):
        assert np.isin(out[c], [0, 1]).all(), f'{c}: a value other than 0/1'

    # the rest of production's columns
    out['parent_rank'] = x('parent_rank')
    assert ((out['parent_rank'] >= 1) & (out['parent_rank'] <= 100)).all()
    out['part_se'] = x('part_se')
    out['nols'] = np.zeros(n)
    pm, om = np.maximum(x('w_primary_mortgage'), 0), np.maximum(x('w_other_mortgage'), 0)   # a few drawn balances are just below 0
    tot = pm + om
    out['prim_mort_share'] = np.where(tot > 0, np.divide(pm, tot, out=np.ones(n), where=tot > 0), 1.0)
    notes.append(f'mortgage balances below 0 (read as 0 for prim_mort_share): {wm((x("w_primary_mortgage") < 0) | (x("w_other_mortgage") < 0))}')
    for c in CONSUMPTION:
        out[c] = x(c)
    for c, v in FORBES_DEFAULTS.items():
        out[c] = np.full(n, v, dtype=float)

    # cost bases: the file's own (non-microdata imputations), which hold each drawn gain to the asset's value first
    for c in BASIS_FROM_FILE:
        notes.append(f'{c}: the file\'s column in place of the converter\'s; records that differ: '
                     f'{wm(np.abs(out[c] - x(c)) > 0.5)}')
        out[c] = x(c)

    g = pd.DataFrame({c: out[c] for c in PRODUCTION_COLUMNS})
    return g, notes


def check(g):
    """The read checks on the finished frame: codes, identities, and missing values only where production has them."""
    msgs = []
    allowed_missing = {'male2', 'age2', 'blind2', 'q_death2', 'divorce_year', 'kg_lt_years_held', 'kg_lt_basis',
                       *[f'dep_{p}{k}' for p in ('age', 'ssn', 'ctc') for k in (1, 2, 3)],
                       *[c for c in FORBES_DEFAULTS if c != 'forbes_flag']}
    bad = [c for c in g.columns if c not in allowed_missing and g[c].isna().any()]
    assert not bad, f'missing values where production has none: {bad}'
    assert g['id'].is_unique
    for c, ok in [('filer', [0, 1]), ('dep_status', [0, 1]), ('filing_status', [1, 2, 3, 4]), ('male1', [0, 1]),
                  ('n_dep', [0, 1, 2, 3]), ('n_dep_ctc', [0, 1, 2, 3]), ('n_dep_eitc', [0, 1, 2, 3]),
                  ('tips_lh1', [0, 1]), ('tips_lh2', [0, 1])]:
        assert g[c].isin(ok).all(), f'{c}: a value outside {ok}'
    assert (g['weight'] > 0).all(), 'a weight of 0 or less'
    assert ((g['dep_status'] == 1) <= (g['filer'] == 1)).all(), 'a dependent filer who does not file'
    for tot, a, b in [('wages', 'wages1', 'wages2'), ('tips', 'tips1', 'tips2'), ('ot', 'ot1', 'ot2'),
                      ('sole_prop', 'sole_prop1', 'sole_prop2'), ('farm', 'farm1', 'farm2'),
                      ('part_se', 'part_se1', 'part_se2')]:
        gap = (g[tot] - g[a] - g[b]).abs().max()
        msgs.append(f'{tot} = {a} + {b}: largest gap ${gap:.2e}')
        assert gap <= 1e-6 * max(1.0, g[tot].abs().max()), f'{tot} != {a} + {b}'
    for k in (1, 2):
        assert (g[f'tips{k}'] <= g[f'wages{k}'] + 1e-6).all() and (g[f'ot{k}'] <= g[f'wages{k}'] + 1e-6).all()
    for k in (1, 2, 3):
        has = g[f'dep_age{k}'].notna()
        assert (g[f'dep_ssn{k}'].notna() == has).all() and (g[f'dep_ctc{k}'].notna() == has).all()
    spouse = g['age2'].notna()
    assert (g['male2'].notna() == spouse).all() and (g['blind2'].notna() == spouse).all()
    assert ((g['kg_lt'] != 0) == g['kg_lt_years_held'].notna()).all()
    msgs.append('codes, identities and missing-value pattern: as production')
    return msgs


def write_csv(g, path):
    """Production's text form: NA for missing, TRUE/FALSE flags, whole numbers without a decimal point."""
    t = g.copy()
    for c in FLAGS:
        t[c] = flag(t[c].to_numpy(float))
    for c in INTEGERS:
        assert t[c].notna().all() and (t[c] == t[c].round()).all(), f'{c}: not a whole number'
        t[c] = t[c].astype(np.int64)
    for c in ('age2', 'male2', 'dep_age1', 'dep_age2', 'dep_age3', 'divorce_year'):
        t[c] = t[c].astype('Int64')          # whole numbers with NA, as production's 23 and NA
    t.to_csv(path, index=False, na_rep='NA')


def column_table():
    """One row per production column: where it comes from in the rebuild's file, and whether Tax-Simulator reads it."""
    contract = {c[0]: c for c in C.CONTRACT}
    rows = []
    for c in PRODUCTION_COLUMNS:
        if c in contract:
            _, status, source, what = contract[c]
            if status == C.ZERO and c in C_FROM_FILE:
                status, source = 'same', f'{c} (made in the file since 29 October: {C_FROM_FILE[c]})'
            elif status == C.ZERO:
                source = '0, as production writes it'
        else:
            status, source = EXTRA[c]
            what = ''
        if c in BASIS_FROM_FILE:
            status, source = 'same', (f'{c} (non-microdata imputations: value less the SCF\'s unrealized gain, the gain '
                                      'first held to the value; the calculator converter\'s formula does not hold it)')
        if c in RULES:
            source = RULES[c] + (f' [converter: {source}]' if c not in ('divorce_year', 'age1', 'male1') else '')
        rows.append({'column': c, 'status': status, 'from the rebuild': source, 'what it is': what,
                     'read by Tax-Simulator (R, main)': 'no' if c in NOT_READ_BY_R else 'yes'})
    return pd.DataFrame(rows)


C_FROM_FILE = {
    **{c: 'blindness rule, non-microdata imputations' for c in ('blind1', 'blind2')},
    **{f'dep_ssn{k}': 'the drawn CPS status, dollars.py' for k in (1, 2, 3)},
    **{f'dep_ctc{k}': 'child credit eligibility, non-microdata imputations' for k in (1, 2, 3)},
    **{f'{p}_{b}': 'QBI rule, non-microdata imputations (dials a = b = 1, not yet solved)'
       for p in ('wagebill', 'sstb') for b in ('sole_prop', 'part', 'scorp', 'farm')},
    'kg_lt_basis': 'SOCA basis rule, non-microdata imputations', 'kg_lt_years_held': 'SOCA holding period rule',
    'q_death1': 'death probabilities, non-microdata imputations', 'q_death2': 'as q_death1',
    **{f'accruals.{a}': 'yearly unrealized gains, non-microdata imputations'
       for a in ('equities', 'pass_throughs', 'primary_home', 'other_home', 're_fund', 'dc', 'trusts')},
}


def main():
    os.makedirs(OUT, exist_ok=True)
    common.say(f'taxsim file: input {INPUT}\noutput {OUT}')
    f = pd.read_parquet(INPUT)
    g, notes = build(f)
    notes += check(g)
    table = column_table()
    table.to_csv(f'{OUT}/TAXSIM_FILE_COLUMNS.csv', index=False)
    common.say('columns by status: ' + ', '.join(f'{k} {v}' for k, v in table['status'].value_counts().items()))
    write_csv(g, f'{OUT}/tax_units_{Y}.csv')
    g.to_parquet(f'{OUT}/tax_units_{Y}.parquet', index=False)
    with open(f'{OUT}/taxsim_file_notes.txt', 'w') as fh:
        fh.write('\n'.join(notes) + '\n')
    common.say('notes:\n  ' + '\n  '.join(notes))
    common.say(f'written {OUT}/tax_units_{Y}.csv: {len(g):,} records, {g.shape[1]} columns')


if __name__ == '__main__':
    sys.exit(main())
