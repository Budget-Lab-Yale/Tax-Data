"""The run report (TARGETS.md section 14): one short page per year, and one table behind each of its sections.

Reads, in the tilt run's folder (common.TILT, or --run):
  file_<year>.parquet  every record, with weight_start (the weight before the tilt), weight (after) and, when the
                       prior pools several trained models, model (the training seed of the network that drew it)
  fit.csv              one row per target, keyed by (kind, group, target): value, before, after, and from the
                       solver se, miss_in_se, gave_way, effective_records, largest_weight_factor and flag
  targets.csv          the year's expanded target list (statistic, se, error_kind, source, ...), when it exists
Writes, in the same folder or --out (REBUILD_REPORT_OUT):
  report.md                  the summary page
  report_targets.csv         1. every target: value, error, where the file lands, the miss in standard errors
  report_thin_cells.csv      2. the effective number of records behind each target and its largest weight factor
  report_published.csv       3. SOI's published cells (Tables 1.4, 1.6 by AGI, 1.7): the file's value, the miss
  report_moves.csv           4. the standard statistics before and after the tilt
  report_models.csv          5. each trained model's value on the standard statistics, after the tilt
  report_overall.csv         6. effective sample kept, KL move, settings, code commit, vintages
  (section 6 of report.md is tilt.py's non_micro_checks.md: the non-microdata imputations' published checks)

A piece the run did not write (no standard errors, no model column, no solver metrics) is said on the page; it
is never filled in with a made-up number. When the fit table lacks the solver's thin-cell metrics, they are
computed here from the targets' linear conditions (target_list.conditions on targets.csv).

The standard statistics (sections 4 and 5): for every dollar item in the file, the number of units holding it
(nonzero) and its total, over all units, filers, non-filers, seven AGI groups and the primary filer's age ranges;
and net worth and total consumption by wage group. A statistic is listed only if at least report.min_records
records hold the item in the cell. Moves and gaps are ranked as a share of the cell's gross (the total of the
item's absolute values), so that a signed item whose total is near zero does not rank first on a small move.

Run: python report_tilt.py [--run DIR] [--out DIR] [--year YEAR]   (in a Slurm job, as every stage)
"""
import argparse
import datetime
import os
import re
import subprocess

import numpy as np
import pandas as pd

import common
import dollars as D
import targets as TG

R = common.SETTINGS.get('report', {})
TOP = R.get('rows_on_page', 8)                            # rows of each table shown on the summary page
MIN_RECORDS = R.get('min_records', 200)                   # records holding the item, for a statistic to be listed
HIT = R.get('hit_tolerance', 1e-6)                        # a target missed by more than this share gave way
THIN = {'warning': (R.get('thin_warning_records', 30), R.get('thin_warning_factor', 10)),
        'stop': (R.get('thin_stop_records', 10), R.get('thin_stop_factor', 100))}   # section 5's placeholders
SOI_CV = R.get('soi_cv', '/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-22/targets/irs_data/soi_cv.csv')
AGI_GROUPS = [(-np.inf if lo is None else lo, np.inf if hi is None else hi, lab) for lo, hi, lab in R.get(
    'agi_groups', [[None, 1, 'AGI below $1'], [1, 25e3, 'AGI $1-25k'], [25e3, 50e3, 'AGI $25k-50k'],
                   [50e3, 100e3, 'AGI $50k-100k'], [100e3, 200e3, 'AGI $100k-200k'], [200e3, 1e6, 'AGI $200k-1m'],
                   [1e6, None, 'AGI $1m and over']])]

# ---- plain names -------------------------------------------------------------------------------------------
LABELS = {
    'agi': 'adjusted gross income', 'wages': 'salaries and wages', 'wages1': "primary filer's wages",
    'wages2': "spouse's wages", 'interest': 'taxable interest', 'tax_exempt_interest': 'tax-exempt interest',
    'dividends': 'ordinary dividends', 'capital_gains': 'Schedule D net capital gain or loss',
    'capital_gains_line13': 'capital gain or loss with distributions (Form 1040 line 13)',
    'kg_st': 'short-term capital gain or loss', 'kg_lt': 'long-term capital gain or loss',
    'kg_dist': 'capital gain distributions on Form 1040', 'kg_other': 'other Schedule D gain or loss',
    'other_agi': 'all other income in AGI', 'business_income': 'business income (Schedules C, E and F)',
    'sch_cf_income': 'Schedule C and F net income', 'sch_e_income': 'Schedule E net income',
    'retirement_gross': 'Social Security and pensions, gross', 'ss_gross': 'Social Security benefits, total',
    'ss_taxable': 'Social Security benefits, taxable', 'pensions_taxable': 'pensions and annuities, taxable',
    'ira_taxable': 'taxable IRA distributions', 'other_gains': 'sales of property other than capital assets',
    'unemployment': 'unemployment compensation', 'alimony': 'alimony received',
    'state_refunds': 'state income tax refunds', 'adjustments': 'statutory adjustments',
    'sch_c': 'business or profession (Schedule C)', 'farm': 'farm (Schedule F)',
    'part_pass_inc': 'partnership passive income', 'part_nonpass_inc': 'partnership nonpassive income',
    'part_pass_loss': 'partnership passive loss', 'part_nonpass_loss': 'partnership nonpassive loss',
    'part_179': 'partnership section 179 deduction', 'scorp_pass_inc': 'S corporation passive income',
    'scorp_nonpass_inc': 'S corporation nonpassive income', 'scorp_pass_loss': 'S corporation passive loss',
    'scorp_nonpass_loss': 'S corporation nonpassive loss', 'scorp_179': 'S corporation section 179 deduction',
    'rent_inc': 'rent and royalty income', 'rent_loss': 'rent and royalty loss', 'estate_inc': 'estate and trust income',
    'estate_loss': 'estate and trust loss', 'farm_rent': 'farm rental income', 'sche_other': 'other Schedule E items',
    'se1': "primary filer's self-employment income", 'se2': "spouse's self-employment income",
    'mortgage_interest': 'home mortgage interest deduction', 'real_estate_tax': 'real estate tax deduction',
    'childcare': 'child care expenses', 'net_worth': 'net worth', 'total_consumption': 'total consumption',
    'w_cash': 'cash and transaction accounts', 'w_equities': 'stocks and stock funds', 'w_bonds': 'bonds and bond funds',
    'w_dc': 'IRAs and defined-contribution accounts', 'w_db': 'defined-benefit pensions',
    'w_life_ins': 'cash value of life insurance', 'w_annuities': 'annuities', 'w_trusts': 'trusts and managed accounts',
    'w_other_fin': 'other financial assets', 'w_pass_throughs': 'privately held businesses',
    'w_primary_home': 'primary home', 'w_other_home': 'other residential real estate',
    'w_re_fund': 'nonresidential real estate', 'w_other_nonfin': 'vehicles and other nonfinancial assets',
    'w_primary_mortgage': 'mortgage on the primary home', 'w_other_mortgage': 'other residential debt',
    'w_credit_lines': 'other lines of credit', 'w_credit_cards': 'credit card balances',
    'w_installment_debt': 'installment debt', 'w_other_debt': 'other debt',
    'w_kg_primary_home': 'unrealized gain on the primary home', 'w_kg_other_re': 'unrealized gain on other real estate',
    'w_kg_pass_throughs': 'unrealized gain on businesses', 'w_kg_other': 'unrealized gain on stocks and funds',
    'c_clothing': 'clothing and footwear', 'c_motor_vehicles': 'motor vehicles and parts',
    'c_durables': 'other durable goods', 'c_other_nondurables': 'other nondurable goods',
    'c_food_off_premises': 'food and beverages at home', 'c_gasoline': 'gasoline and other energy goods',
    'c_housing_utilities': 'housing and utilities', 'c_other_services_health': 'health care and other services'}
IRS_ITEM_LABELS = {'wages': 'salaries and wages', 'interest': 'taxable interest', 'dividends': 'ordinary dividends',
                   'capital_gains': 'Schedule D net gain less net loss'}
FAMILY_LABELS = {f: v['soi'] for f, v in TG.FAMILIES.items()}
AGE_PLAIN = dict(zip(common.AGE_LABELS, ['primary filer under 26', 'primary filer 26-34', 'primary filer 35-44',
                                         'primary filer 45-54', 'primary filer 55-64', 'primary filer 65 and over',
                                         'dependent filer under 18', 'dependent filer 18-25',
                                         'dependent filer 26 and over']))


def label(v):
    return LABELS.get(v, v.replace('_', ' '))


def describe(kind, group, target):
    """A target's plain name and its cell, from its key."""
    cell = group
    words = target.split(' ')
    if kind == 'IRS':
        what = {'returns': 'number of returns', 'joint returns': 'joint returns'}.get(target)
        if what is None:
            item, stat = target.rsplit(' ', 1)
            what = f"{IRS_ITEM_LABELS.get(item, label(item))}, {'number of returns' if stat == 'count' else 'amount'}"
            if item == 'capital_gains' and stat == 'count':
                what = 'returns with a Schedule D net gain or a net loss'
        cell = 'all returns' if group == 'all' else f'AGI {group}'
    elif kind == 'business' and len(words) == 3 and words[0] in FAMILY_LABELS:
        fam, side, stat = words
        what = f"{FAMILY_LABELS[fam]}, {side.replace('_', ' ')}, {'number of returns' if stat == 'count' else 'amount'}"
        cell = f'AGI {group}'
    elif kind == 'consumption' and target.endswith(', average'):
        what = f'{label(target[:-len(", average")])}, average per unit'
        cell = re.sub(r'^CEX \d{4}, ', '', group)
        cell = 'all units' if cell == 'all' else cell
    elif kind == 'wealth':
        what = 'share of units' if target == 'share of units' else 'net worth'
        cell = 'all units' if group == 'all' else f'net worth {group}'
    elif kind == 'non-filer people':
        what = 'people'
        cell = re.sub(r'aged (\d+)_(\d+)', r'aged \1-\2', group).replace('aged 75p', 'aged 75 and over')
    elif kind == 'non-filers':
        what, cell = 'non-filer units', 'all'
    elif kind == 'age':
        what = 'number of returns'
        cell = group.replace('non-joint', 'returns other than joint').replace('joint,', 'joint returns,') \
            .replace('primary', 'primary filer aged')
    else:
        what = target
    return what, cell


def statistic_of(kind, target):
    """The statistic a target fixes, when the target list does not say."""
    if 'average' in target:
        return 'average'
    if 'share' in target:
        return 'share'
    if target.endswith('amount') or target == 'net worth':
        return 'amount'
    return 'count'


def show(v, stat):
    """A number as the page prints it."""
    if v is None or not np.isfinite(v):
        return '' if v is None or np.isnan(v) else ('inf' if v > 0 else '-inf')
    if stat in ('share', 'worker share'):
        return f'{100 * v:.2f}%'
    if stat in ('amount', 'average', 'total'):
        a = abs(v)
        return f'${v / 1e9:,.1f} billion' if a >= 1e9 else f'${v / 1e6:,.1f} million' if a >= 1e6 else f'${v:,.0f}'
    return f'{v / 1e6:,.2f} million' if abs(v) >= 1e6 else f'{v:,.0f}'


def table(df, floatfmt=',.2f'):
    return df.to_markdown(index=False, floatfmt=floatfmt) if len(df) else '(none)'


# ---- reading the run ---------------------------------------------------------------------------------------
def read_run(run, year):
    df = pd.read_parquet(f'{run}/file_{year}.parquet')
    fit = pd.read_csv(f'{run}/fit.csv')
    tl = pd.read_csv(f'{run}/targets.csv') if os.path.exists(f'{run}/targets.csv') else None
    for c in ('weight_start', 'weight'):
        assert c in df, f'file_{year}.parquet has no {c} column'
        w = df[c].to_numpy(float)
        assert np.isfinite(w).all() and (w >= 0).all(), f'{c}: {(~np.isfinite(w)).sum()} records not finite or negative'
    assert (df['weight_start'] > 0).all(), 'a record with no starting weight: its weight factor is undefined'
    df = df.assign(agi=D.agi(df))
    return df, fit, tl


def as_bool(s):
    return s.astype(str).str.strip().str.lower().isin(['true', '1', '1.0'])


# ---- 1. every target -----------------------------------------------------------------------------------------
def target_table(fit, tl, notes):
    t = fit.copy()
    key = ['kind', 'group', 'target']
    if tl is not None:   # the solver's own columns (its se among them) win over the list's
        extra = [c for c in tl.columns if c not in key and c not in t.columns]
        t = t.merge(tl[key + extra], on=key, how='left', validate='one_to_one', indicator=True)
        missing = int((t.pop('_merge') == 'left_only').sum())
        if missing:
            notes.append(f'{missing} targets in fit.csv are not in targets.csv.')
    else:
        notes.append('The run has no targets.csv (the expanded target list): statistics are inferred from the '
                     'target names, and no source or vintage is shown.')
    if 'statistic' not in t:
        t['statistic'] = [statistic_of(k, g) for k, g in zip(t['kind'], t['target'])]
    if 'se' not in t:
        t['se'] = np.nan
        notes.append('No standard errors in the fit table or the target list: the miss in standard errors is not '
                     'available (shown blank).')
    elif t['se'].isna().any():
        notes.append(f"{t['se'].isna().sum()} targets have no standard error (shown blank).")
    t['miss'] = t['after'] - t['value']
    t['miss (%)'] = 100 * (t['after'] / t['value'] - 1)
    if 'miss_in_se' not in t:
        with np.errstate(divide='ignore', invalid='ignore'):
            t['miss_in_se'] = np.where(t['se'] > 0, t['miss'] / t['se'], np.nan)
    if 'gave_way' in t:
        t['gave_way'] = as_bool(t['gave_way'])
    else:
        t['gave_way'] = (t['miss (%)'].abs() > 100 * HIT).to_numpy()
        notes.append(f'The fit table has no gave_way column: a target is counted as having given way when the file '
                     f'misses it by more than {HIT:g} of its value.')
    plain = [describe(k, g, x) for k, g, x in zip(t['kind'], t['group'], t['target'])]
    t['what'], t['cell'] = [p[0] for p in plain], [p[1] for p in plain]
    t['fit_row'] = np.arange(len(t))   # the row's place in fit.csv, the order the conditions come in
    t = t.assign(_z=t['miss_in_se'].abs(), _p=t['miss (%)'].abs())
    t = t.sort_values(['gave_way', '_z', '_p', 'fit_row'], ascending=[False, False, False, True], na_position='last')
    first = ['kind', 'group', 'target', 'what', 'cell', 'statistic', 'value', 'se', 'before', 'after', 'miss',
             'miss (%)', 'miss_in_se', 'gave_way']
    return t[first + [c for c in t.columns if c not in first and not c.startswith('_')]]


def target_page(t, notes):
    by = t.groupby('kind', sort=False).agg(targets=('value', 'size'), gave_way=('gave_way', 'sum'),
                                           largest_miss_pct=('miss (%)', lambda s: s.abs().max()),
                                           largest_miss_se=('miss_in_se', lambda s: s.abs().max()))
    by = by.reset_index().rename(columns={'kind': 'kind of target', 'gave_way': 'gave way',
                                          'largest_miss_pct': 'largest miss (%)',
                                          'largest_miss_se': 'largest miss (standard errors)'})
    gave = t[t['gave_way']].head(TOP)
    shown = pd.DataFrame({'target': gave['what'], 'cell': gave['cell'],
                          'value': [show(v, s) for v, s in zip(gave['value'], gave['statistic'])],
                          'standard error': [show(v, s) for v, s in zip(gave['se'], gave['statistic'])],
                          'file': [show(v, s) for v, s in zip(gave['after'], gave['statistic'])],
                          'miss (%)': gave['miss (%)'].round(2), 'miss (standard errors)': gave['miss_in_se'].round(2)})
    out = [f"{len(t)} targets; {int(t['gave_way'].sum())} gave way.", '', table(by, ',.3g'), '']
    if len(gave):
        out += [f'The {len(gave)} that gave way most:', '', table(shown), '']
    return out + [f'- {n}' for n in notes] + ['', 'Full table: report_targets.csv.']


# ---- 2. thin cells -------------------------------------------------------------------------------------------
def list_conditions(df, tl, w0):
    """Per target, its key (kind, group, target) and each record's contribution (records x targets), from the expanded
    target list (target_list.conditions). For an average or a share the contribution is the group's records times the
    averaged variable, the total its average rests on."""
    import target_list
    F, T, averages = target_list.conditions(df, tl, w0)
    F = np.asarray(F.todense() if hasattr(F, 'todense') else F)
    a = np.column_stack([np.abs(F[:, j]) if m is None else m[0] * np.abs(m[1]) for j, m in enumerate(averages)])
    return tl[['kind', 'group', 'target']], a


def cell_metrics(a, w0, w1):
    """Per target: the records in its cell, the effective number of records its total rests on after the tilt
    ((sum of w a)^2 / sum of (w a)^2, a the record's contribution), and the largest weight factor in the cell."""
    f = w1 / w0
    out = []
    for j0 in range(0, a.shape[1], 50):
        b = a[:, j0:j0 + 50]
        wa = b * w1[:, None]
        with np.errstate(invalid='ignore', divide='ignore'):
            eff = wa.sum(0) ** 2 / (wa ** 2).sum(0)
        out.append(np.column_stack([(b > 0).sum(0), eff, np.where(b > 0, f[:, None], 0).max(0)]))
    return np.vstack(out)


def flag_of(eff, factor):
    """Section 5's flag and what tripped it."""
    for flag in ('stop', 'warning'):
        n, f = THIN[flag]
        why = ([f'fewer than {n} effective records'] if eff < n else []) + \
            ([f'a weight factor above {f}'] if factor > f else [])
        if why:
            return flag, ' and '.join(why)
    return '', ''


def thin_cells(t, df, tl, w0, w1, notes):
    cols = ['effective_records', 'largest_weight_factor', 'flag']
    if all(c in t for c in cols):
        th = t.copy()
        th['flag'] = th['flag'].fillna('').astype(str)
        if 'records' not in th:
            th['records'] = np.nan
    else:
        a = None
        in_order = t.sort_values('fit_row')
        for how, build in [('the expanded target list', lambda: list_conditions(df, tl, w0) if tl is not None else None)]:
            try:
                got = build()
            except Exception as e:   # said on the page; the next way is tried
                notes.append(f'Thin cells: could not rebuild the conditions from {how} ({type(e).__name__}: {e}).')
                continue
            if got is None:
                continue
            k, a_ = got
            if len(k) == len(t) and (k.reset_index(drop=True).astype(str) ==
                                     in_order[['kind', 'group', 'target']].reset_index(drop=True).astype(str)).all().all():
                a = a_
                notes.append(f'The fit table has no thin-cell metrics; they are computed here from {how}: effective '
                             f'records = (sum of weight x contribution)^2 / sum of (weight x contribution)^2, after '
                             f'the tilt; for an average or share, the contribution is the averaged variable.')
                break
            notes.append(f'Thin cells: the targets rebuilt from {how} do not match fit.csv row for row; not used.')
        if a is None:
            notes.append('Thin cells: no metrics in the fit table and no way to rebuild them; section left empty.')
            return t.iloc[:0].assign(records=[], effective_records=[], largest_weight_factor=[], flag=[], why=[])
        m = cell_metrics(a, w0, w1)
        th = in_order.copy()
        th['records'], th['effective_records'], th['largest_weight_factor'] = m[:, 0], m[:, 1], m[:, 2]
        got = [flag_of(e, f) for e, f in zip(th['effective_records'], th['largest_weight_factor'])]
        th['flag'], th['why'] = [g[0] for g in got], [g[1] for g in got]
    if 'why' not in th:
        th['why'] = ''
    rank = th['flag'].map({'stop': 0, 'warning': 1}).fillna(2)
    th = th.assign(_r=rank).sort_values(['_r', 'effective_records'])
    return th[['kind', 'group', 'target', 'what', 'cell', 'records', 'effective_records', 'largest_weight_factor',
               'flag', 'why']]


def thin_page(th):
    n = th['flag'].value_counts()
    out = [f"{n.get('stop', 0)} stops and {n.get('warning', 0)} warnings among {len(th)} targets "
           f"(warning: fewer than {THIN['warning'][0]} effective records or a weight factor above "
           f"{THIN['warning'][1]}; stop: fewer than {THIN['stop'][0]} or above {THIN['stop'][1]}).", '']
    if len(th):
        flagged = th[th['flag'] != '']
        why = flagged['why'].fillna('').astype(str)
        only = flagged[~why.str.contains('effective')] if why.ne('').all() else flagged.iloc[:0]
        if len(only):
            out += [f"{len(only)} of the {len(flagged)} flags are tripped by the weight factor alone, in cells resting "
                    f"on {only['effective_records'].min():,.0f} or more effective records.", '']
        s = th.head(TOP)
        out += ['The thinnest cells:', '', table(pd.DataFrame({
            'target': s['what'], 'cell': s['cell'],
            'records': [('' if pd.isna(v) else f'{v:,.0f}') for v in s['records']],
            'effective records': s['effective_records'].round(1),
            'largest weight factor': s['largest_weight_factor'].round(2), 'flag': s['flag'],
            'tripped by': s['why']}), ',.1f'), '']
    return out + ['Full table: report_thin_cells.csv.']


# ---- 3. published numbers ------------------------------------------------------------------------------------
def family_net(df, fam):
    f = TG.FAMILIES[fam]
    return df[f['income']].to_numpy(float).sum(1) - (df[f['loss']].to_numpy(float).sum(1) if f['loss'] else 0.0)


pos = lambda x: np.where(x > 0, x, 0.0)
neg = lambda x: np.where(x < 0, -x, 0.0)
# The published lines compared, Tables 1.4 and 1.7: (SOI line, as a pattern on the mirror's item, without
# ' > number of returns' or ' > amount'; the record's amount for it, positive where SOI's is; statistics; note).
LINES = [
    ('number of returns', lambda d: np.ones(len(d)), ['count'], ''),
    ('adjusted gross income less deficit', lambda d: d['agi'].to_numpy(float), ['amount'], ''),
    ('salaries and wages', lambda d: d['wages'].to_numpy(float), ['count', 'amount'], ''),
    ('taxable interest', lambda d: d['interest'].to_numpy(float), ['count', 'amount'], ''),
    ('tax exempt interest', lambda d: d['tax_exempt_interest'].to_numpy(float), ['count', 'amount'], ''),
    ('ordinary dividends', lambda d: d['dividends'].to_numpy(float), ['count', 'amount'], ''),
    ('state income tax refunds', lambda d: d['state_refunds'].to_numpy(float), ['count', 'amount'], ''),
    ('alimony received', lambda d: d['alimony'].to_numpy(float), ['count', 'amount'], ''),
    ('business or profession > net income', lambda d: pos(family_net(d, 'sch_c')), ['count', 'amount'], ''),
    ('business or profession > net loss', lambda d: neg(family_net(d, 'sch_c')), ['count', 'amount'], ''),
    ('capital gain distributions( reported on form 1040)?', lambda d: d['kg_dist'].to_numpy(float), ['count', 'amount'],
     'distributions reported directly on Form 1040, without Schedule D'),
    ('sales of capital assets( reported on form 1040, schedule d)? > taxable net gain',
     lambda d: pos(d['capital_gains'].to_numpy(float)), ['count', 'amount'],
     'the targets fix returns with a gain or a loss, and gains less losses, not each'),
    ('sales of capital assets( reported on form 1040, schedule d)? > taxable net loss',
     lambda d: neg(d['capital_gains'].to_numpy(float)), ['count', 'amount'],
     'the targets fix returns with a gain or a loss, and gains less losses, not each'),
    ('sales of property other than capital assets > net gain', lambda d: pos(d['other_gains'].to_numpy(float)),
     ['count', 'amount'], ''),
    ('sales of property other than capital assets > net loss', lambda d: neg(d['other_gains'].to_numpy(float)),
     ['count', 'amount'], ''),
    (r'(taxable ira distributions|taxable individual retirement arrangement \(ira\) distributions)',
     lambda d: d['ira_taxable'].to_numpy(float), ['count', 'amount'], ''),
    ('pensions and annuities > total', lambda d: (d['retirement_gross'] - d['ss_gross']).to_numpy(float),
     ['count', 'amount'], 'the file carries pensions as gross retirement income less Social Security'),
    ('pensions and annuities > taxable', lambda d: d['pensions_taxable'].to_numpy(float), ['count', 'amount'], ''),
    ('total rental and royalty > net income', lambda d: pos(family_net(d, 'rent_royalty')), ['count', 'amount'], ''),
    (r'total rental and royalty > net loss.*', lambda d: neg(family_net(d, 'rent_royalty')), ['count', 'amount'], ''),
    ('partnership and s corporation > net income', lambda d: pos(family_net(d, 'pship_scorp')), ['count', 'amount'], ''),
    ('partnership and s corporation > net loss', lambda d: neg(family_net(d, 'pship_scorp')), ['count', 'amount'], ''),
    ('estate and trust > net income', lambda d: pos(family_net(d, 'estate_trust')), ['count', 'amount'], ''),
    ('estate and trust > net loss', lambda d: neg(family_net(d, 'estate_trust')), ['count', 'amount'], ''),
    ('farm > net income', lambda d: pos(family_net(d, 'farm')), ['count', 'amount'], ''),
    ('farm > net loss', lambda d: neg(family_net(d, 'farm')), ['count', 'amount'], ''),
    ('farm rental > net income', lambda d: pos(family_net(d, 'farm_rental')), ['count', 'amount'], ''),
    ('farm rental > net loss', lambda d: neg(family_net(d, 'farm_rental')), ['count', 'amount'], ''),
    ('unemployment compensation', lambda d: d['unemployment'].to_numpy(float), ['count', 'amount'], ''),
    ('social security benefits > total', lambda d: d['ss_gross'].to_numpy(float), ['count', 'amount'], ''),
    ('social security benefits > taxable', lambda d: d['ss_taxable'].to_numpy(float), ['count', 'amount'], ''),
    ('(statutory adjustments > total|total statutory adjustments)', lambda d: d['adjustments'].to_numpy(float),
     ['count', 'amount'], '')]
NOT_COMPARED = ('qualified dividends, rent and royalty separately, other income, net operating loss, gambling, '
                'deductions, taxable income and tax: the file lacks the item or the calculator is needed')
T16_STATUS = {  # SOI's panel: (its short name, the PUF's filing status MARS; None for every return)
    'All returns': ('all returns', None),
    'Returns of married persons filing jointly and returns of surviving spouses': ('joint returns', 2),
    'Returns of single persons': ('single returns', 1),
    'Returns of married persons filing separately': ('married filing separately', 3),
    'Returns of heads of households': ('heads of households', 4)}
T16_AGE = {'Under 26': 0, '26 under 35': 1, '35 under 45': 2, '45 under 55': 3, '55 under 65': 4, '65 and over': 5}


def mirror(file, table_, year):
    t = pd.read_csv(os.path.join(TG.MIRROR, file), dtype=str, keep_default_na=False)
    return t[(t['tax_year'] == str(year)) & (t['table'] == table_)]


def bound(s):
    return -np.inf if s == '-Inf' else np.inf if s == 'Inf' else float(s)


def by_size_cells(table_, file, year, pop):
    """Tables 1.4 and 1.7, all returns: one row per published cell of the compared lines, AGI classes and total."""
    t = mirror(file, table_, year)
    t = t[t['panel'] == 'All returns']
    items = t['item'].str.lower().str.strip()
    rows = []
    for line, fn, stats, note in LINES:
        for st in stats:
            pat = line if (line, st) in [('number of returns', 'count'), ('adjusted gross income less deficit', 'amount')] \
                else f"{line} > {'number of returns' if st == 'count' else 'amount'}"
            hit = t[items.str.fullmatch(pat)]
            if hit['item'].nunique() != 1:
                continue                       # the line is not in this table or year
            for r in hit.itertuples():
                total = r.row_agi_lo == '' and r.row_agi_hi == ''
                if total and not (r.row_label.endswith('total') or r.row_label == 'All returns'):
                    continue
                if not total and (r.row_agi_lo == '' or r.row_agi_hi == ''):
                    continue
                rows.append({'table': table_, 'panel': 'all returns' if pop == 'filers' else 'dependent filers',
                             'row': 'all AGI' if total else r.row_label, 'lo': -np.inf if total else bound(r.row_agi_lo),
                             'hi': np.inf if total else bound(r.row_agi_hi), 'total': total,
                             'line': r.col_group or r.col_label,    # SOI's own name of the line
                             'item': r.item.lower().strip(), 'statistic': st, 'flag': r.flag, 'row_label': r.row_label,
                             'published': np.nan if r.value == '' else float(r.value) * (1000 if st == 'amount' else 1),
                             '_fn': fn, '_pop': pop, 'note': note})
    return rows


def table_16_cells(year):
    """Table 1.6: returns by filing status, the primary filer's age and AGI (the all-AGI column too). The All
    returns panel's under-18 and 18-25 rows are left out: the file does not split filers under 26 at 18."""
    t = mirror('bysize_returns_marital_age.csv', '1.6', year)
    rows = []
    for r in t[t['panel'].isin(T16_STATUS) & t['row_label'].isin(T16_AGE)].itertuples():
        total = r.col_agi_lo == '' and r.col_agi_hi == ''
        if not total and (r.col_agi_lo == '' or r.col_agi_hi == ''):
            continue
        rows.append({'table': '1.6', 'panel': T16_STATUS[r.panel][0], '_mars': T16_STATUS[r.panel][1], 'row': r.row_label, 'lo': -np.inf if total else bound(r.col_agi_lo),
                     'hi': np.inf if total else bound(r.col_agi_hi), 'total': total,
                     'line': f'Returns, primary filer {r.row_label.lower()}', 'item': r.item.lower().strip(), 'col': r.col_label,
                     'statistic': 'count', 'flag': r.flag, 'row_label': r.row_label,
                     'published': np.nan if r.value == '' else float(r.value), 'note': '', '_pop': 'filers'})
    return rows


def file_values(cells, df, weights):
    """The file's value of each published cell, under each set of weights, and the records it rests on."""
    filer = df['filer'].to_numpy() == 1
    dep = filer & (df['age_range'].to_numpy() >= common.ADULT_RANGES)
    a = df['agi'].to_numpy(float)
    rng, mars = df['age_range'].to_numpy(), df['mars'].to_numpy()
    fn_cache, bracket = {}, {}
    t16 = pd.DataFrame([c for c in cells if c['table'] == '1.6'])
    for c in cells:
        key = (c['lo'], c['hi'])
        if key not in bracket:
            bracket[key] = (a >= c['lo']) & (a < c['hi'])
        inside = bracket[key] & (filer if c['_pop'] == 'filers' else dep)
        if c['table'] == '1.6':
            status = c['_mars']
            inside = inside & (True if status is None else mars == status)
            k = T16_AGE[c['row']]
            # dependent filers: under 26 by their range; 26 and over spread over the five older ranges in proportion
            # to this table's counts in the same filing status and AGI column (TARGETS.md section 10). A cell SOI
            # blanked gets no share here; its neighbours carry its returns, and the merge below joins them.
            if k == 0:
                x = np.isin(rng, [0, common.DEPENDENT_RANGES[0], common.DEPENDENT_RANGES[1]]).astype(float)
            else:
                col = t16[(t16['panel'] == c['panel']) & (t16['lo'] == c['lo']) & (t16['hi'] == c['hi'])]
                older = col[col['row'] != 'Under 26']['published'].fillna(0.0)
                share = c['published'] / older.sum() if older.sum() > 0 and np.isfinite(c['published']) else 0.0
                x = (rng == k) + share * (rng == common.DEPENDENT_RANGES[-1])
        else:
            if id(c['_fn']) not in fn_cache:
                fn_cache[id(c['_fn'])] = c['_fn'](df)
            v = fn_cache[id(c['_fn'])]
            x = v if c['statistic'] == 'amount' else (v != 0).astype(float)
        xs = x * inside
        for name, w in weights.items():
            c[name] = float(xs @ w)
        c['records'] = int((xs != 0).sum())
    return cells


def soi_errors(year):
    """SOI's coefficients of variation for Table 1.4, all returns: {(row label, item): CV in percent}, and the
    cells another table borrows from (section 9: the Table 1.4 cell nearest in returns and amount)."""
    if not os.path.exists(SOI_CV):
        return {}, None
    cv = pd.read_csv(SOI_CV, dtype=str, keep_default_na=False)
    cv = cv[(cv['tax_year'] == str(year)) & (cv['table'] == '1.4') & (cv['panel'] == 'All returns')]
    cv = cv.assign(cv=pd.to_numeric(cv['cv_pct'], errors='coerce'), v=pd.to_numeric(cv['value'], errors='coerce'),
                   item=cv['item'].str.lower().str.strip())
    lookup = {(r.row_label, r.item): r.cv for r in cv.itertuples() if np.isfinite(r.cv)}
    d = cv[(cv['row_agi_lo'] != '') & (cv['cv'] > 0) & (cv['v'] > 0)]
    count = d[d['item'].str.endswith('number of returns')]
    n_of = {(r.row_label, r.item[:-len('number of returns')]): r.v for r in count.itertuples()}
    amount = d[d['item'].str.endswith('> amount')]
    paired = [n_of.get((r.row_label, r.item[:-len('amount')])) for r in amount.itertuples()]
    amount = amount.assign(n=paired).dropna(subset=['n'])     # an amount cell lends only with its count beside it
    return lookup, {'count': (np.log(count['v'].to_numpy(float)), count['cv'].to_numpy(float)),
                    'amount': (np.log(amount['v'].to_numpy(float) * 1000), np.log(amount['n'].to_numpy(float)),
                               amount['cv'].to_numpy(float))}


def series(cells):
    """The published cells in series along AGI: (the series' AGI classes, lowest first, as indices into cells; the
    indices of its all-AGI total). Tables 1.4 and 1.7: one series per line and statistic; Table 1.6: per filing
    status and age."""
    df = pd.DataFrame([{k: c[k] for k in ('table', 'panel', 'row', 'line', 'statistic', 'lo', 'total')} for c in cells])
    out = []
    for tab, part in df.groupby('table', sort=False):
        key = ['panel', 'row', 'statistic'] if tab == '1.6' else ['panel', 'line', 'statistic']
        for _, s in part.groupby(key, sort=False):
            out.append((s[~s['total']].sort_values('lo').index.tolist(), s.index[s['total']].tolist()))
    return out


def target_matcher(t):
    """A test of whether a published number (a count or an amount) is the value of a target of the same kind.
    Matched on the number itself, to half a dollar or half a return, so it holds whatever the target list calls
    its rows. Numbers under 1,000 are not matched; a match by coincidence remains possible."""
    tv = {}
    for st, kinds in [('count', ['units', 'count']), ('amount', ['amount'])]:
        v = t.loc[t['statistic'].isin(kinds), 'value'].to_numpy(float)
        tv[st] = v[np.isfinite(v) & (np.abs(v) >= 1000)]
    return lambda v, st: bool(np.isfinite(v) and abs(v) >= 1000 and np.isclose(tv[st], v, rtol=1e-9, atol=0.5).any())


def relation_to_targets(cells, is_target):
    """Each published cell is a target; part of a coarser target (in Table 1.4, a run of neighbouring AGI classes
    adds up to a target's value, as the merged top brackets and the business groups do; in any table, the cell's
    all-AGI total is a target); the sum of targets (a total whose classes are all covered); or not targeted. Blank
    cells count as zero in the runs, as SOI adds them in elsewhere."""
    for idx, tot in series(cells):
        st = cells[(idx or tot)[0]]['statistic']
        v = np.nan_to_num(np.array([cells[i]['published'] for i in idx], float))
        covered = np.zeros(len(idx), bool)
        if any(is_target(cells[i]['published'], st) for i in tot):
            covered[:] = True
        elif idx and cells[idx[0]]['table'] == '1.4':
            for i in range(len(idx)):
                for j, s in enumerate(np.cumsum(v[i:])):
                    if j > 0 and is_target(s, st):
                        covered[i:i + j + 1] = True
        for k, i in enumerate(idx):
            c = cells[i]
            c['relation'] = 'targeted' if is_target(c['published'], st) else \
                'part of a coarser target' if covered[k] else 'not targeted'
        for i in tot:
            c = cells[i]
            if is_target(c['published'], st):
                c['relation'] = 'targeted'
            else:
                whole = all(cells[i_]['relation'] != 'not targeted' for i_ in idx)
                c['relation'] = 'sum of targets' if idx and whole else 'not targeted'
    return cells


def merge_combined(cells, weights, is_target):
    """SOI blanks a cell for disclosure and adds it into a neighbour. Which neighbour is not always marked (Table
    1.4 marks the receiving cell, Table 1.6 does not), so a blank cell is merged with the cells on both sides along
    its AGI classes: the merged cell then holds the blank one's returns wherever SOI put them."""
    out = []
    for idx, tot in series(cells):
        out += [cells[i] for i in tot]
        group = list(range(len(idx)))
        for k, i in enumerate(idx):
            if np.isnan(cells[i]['published']):
                for j in (k - 1, k + 1):
                    if 0 <= j < len(idx):
                        old = group[j]
                        group = [group[k] if g == old else g for g in group]
        for g in dict.fromkeys(group):
            members = [cells[i] for i, gg in zip(idx, group) if gg == g]
            if len(members) == 1:
                out.append(members[0])
                continue
            m = dict(members[0])
            m.update(hi=members[-1]['hi'], published=float(np.nansum([x['published'] for x in members])),
                     records=sum(x['records'] for x in members), merged=len(members),
                     _members=[(x['row_label'], x['item'], x['published']) for x in members])
            if m['table'] == '1.6':
                m['col'] = f"{members[0]['col']} through {members[-1]['col']}"
            else:
                m['row'] = f"{members[0]['row']} through {members[-1]['row']}"
            for name in weights:
                m[name] = sum(x[name] for x in members)
            m['relation'] = 'targeted' if is_target(m['published'], m['statistic']) else 'not targeted' if \
                all(x['relation'] == 'not targeted' for x in members) else 'part of a coarser target'
            out.append(m)
    return out


def errors_for(cells, year, notes):
    """Each published cell's standard error: Table 1.4's from SOI's coefficients of variation (a merged cell's
    parts combined as independent); Tables 1.6 and 1.7 borrow the CV of the Table 1.4 cell nearest in number of
    returns (a count) or in amount and returns (an amount), section 9's rule."""
    lookup, donors = soi_errors(year)
    if not lookup:
        notes.append(f'No SOI coefficients of variation found ({SOI_CV}): no published cell has an error.')
    count_of = {(c['table'], c['panel'], c['row'], c['line']): c['published'] for c in cells if c['statistic'] == 'count'}
    for c in cells:
        c['se'], c['error source'] = np.nan, 'none'
        if not np.isfinite(c['published']) or c['published'] == 0:
            c['error source'] = 'none (published zero)' if c['published'] == 0 else 'none'
            continue
        if c['table'] == '1.4':
            parts = c.get('_members') or [(c['row_label'], c['item'], c['published'])]
            cvs = [(lookup.get((lab, it)), v) for lab, it, v in parts if np.isfinite(v) and v != 0]
            if cvs and all(cv is not None for cv, _ in cvs):
                c['se'] = float(np.sqrt(sum((cv / 100 * v) ** 2 for cv, v in cvs)))   # v already in dollars
                c['error source'] = 'SOI CV is 0 (sampled in full)' if c['se'] == 0 else \
                    'SOI CV' + (' (merged cells, combined as independent)' if len(cvs) > 1 else '')
            else:
                c['error source'] = 'none (SOI publishes no CV for this cell)'
        elif donors is not None:
            if c['statistic'] == 'count':
                lv, cv = donors['count']
                j = np.argmin(np.abs(lv - np.log(c['published'])))
            else:
                n = count_of.get((c['table'], c['panel'], c['row'], c['line']))
                if n is None or not np.isfinite(n) or n <= 0:
                    c['error source'] = 'none (no published count beside this amount)'
                    continue
                la, ln, cv = donors['amount']
                j = np.argmin((la - np.log(c['published'])) ** 2 + (ln - np.log(n)) ** 2)
            c['se'], c['error source'] = cv[j] / 100 * c['published'], 'CV borrowed from Table 1.4'
    return cells


def published(df, t, w0, w1, notes):
    """Section 3: SOI's published cells beside the file's values, before and after the tilt."""
    year = common.TARGET_YEAR
    weights = {'before': w0, 'after': w1}
    cells = by_size_cells('1.4', TG.FILES['1.4'], year, 'filers')
    if not cells:
        notes.append(f'Table 1.4 for {year} is not in the SOI mirror.')
    c17 = by_size_cells('1.7', 'bysize_dependent_returns.csv', year, 'dependent filers') \
        if os.path.exists(os.path.join(TG.MIRROR, 'bysize_dependent_returns.csv')) else []
    if not c17:
        notes.append(f'Table 1.7 (dependent filers) for {year} is not in the SOI mirror.')
    c16 = table_16_cells(year)
    if not c16:
        notes.append(f'Table 1.6 for {year} is not in the SOI mirror.')
    is_target = target_matcher(t)
    cells = file_values(cells + c17 + c16, df, weights)
    cells = merge_combined(relation_to_targets(cells, is_target), weights, is_target)
    cells = errors_for(cells, year, notes)
    p = pd.DataFrame(cells)
    p['miss'] = p['after'] - p['published']
    with np.errstate(divide='ignore', invalid='ignore'):
        p['miss (%)'] = 100 * (p['after'] / p['published'] - 1)
        p['miss_in_se'] = np.where(p['se'] > 0, p['miss'] / p['se'], np.nan)
    p['_z'] = p['miss_in_se'].abs()
    p['_o'] = (p['relation'] != 'not targeted').astype(int)
    p = p.sort_values(['_o', '_z'], ascending=[True, False], na_position='last')
    cols = ['table', 'panel', 'row', 'col', 'line', 'statistic', 'published', 'se', 'error source', 'before', 'after',
            'miss', 'miss (%)', 'miss_in_se', 'records', 'merged', 'relation', 'note']
    return p[[c for c in cols if c in p]]


def published_page(p):
    names = {'1.4': 'Table 1.4, all returns by AGI', '1.6': 'Table 1.6, returns by filing status, age and AGI',
             '1.7': 'Table 1.7, dependent filers by AGI'}
    nt = p[p['relation'] == 'not targeted']
    rows = []
    for tab, s in nt.groupby('table'):
        z = s['miss_in_se'].abs()
        rows.append({'table': names.get(tab, tab), 'cells not targeted': len(s), 'with an error': int(z.notna().sum()),
                     'median miss (standard errors)': z.median(), 'beyond 3 standard errors': int((z > 3).sum()),
                     'error from': ', '.join(sorted(s['error source'].unique()))})
    big = nt[(nt['records'] >= MIN_RECORDS) & nt['miss_in_se'].notna()].head(TOP)
    shown = pd.DataFrame({
        'table': big['table'], 'line': [f'{l}, {pn}' if t_ == '1.6' else l if l.lower() in (
                                            'number of returns', 'adjusted gross income less deficit') else
                                        f"{l} ({'number of returns' if s == 'count' else 'amount'})"
                                        for l, s, t_, pn in zip(big['line'], big['statistic'], big['table'], big['panel'])],
        'AGI': [r if t_ != '1.6' else c for r, c, t_ in zip(big['row'], big.get('col', big['row']), big['table'])],
        'published': [show(v, s) for v, s in zip(big['published'], big['statistic'])],
        'file': [show(v, s) for v, s in zip(big['after'], big['statistic'])],
        'miss (%)': big['miss (%)'].round(1), 'miss (standard errors)': big['miss_in_se'].round(1)})
    return [f"{len(p)} published cells compared; {len(nt)} are not targeted. Not compared: {NOT_COMPARED}.", '',
            table(pd.DataFrame(rows), ',.2f'), '',
            f'The largest misses among cells not targeted (at least {MIN_RECORDS} records in the cell):', '',
            table(shown, ',.1f'), '', 'Full table: report_published.csv.']


# ---- 4 and 5. the standard statistics -------------------------------------------------------------------------
def dollar_items(df):
    """Every dollar item in the file, in the generation order, then the computed totals."""
    skip = set(D.RATIOS) | set(D.AGE_SLOTS) | {'weeks_primary', 'weeks_spouse', 'hours_primary', 'hours_spouse',
                                              'business_income_drawn', 'other_agi_remainder'}
    extra = ['agi', 'capital_gains_line13', 'sch_cf_income', 'sch_e_income', 'wages1', 'wages2', 'se1', 'se2',
             'w_primary_mortgage', 'total_consumption', 'net_worth']
    items = [v for v in dict.fromkeys(D.ORDER + extra) if v in df and v not in skip]
    bad = {v: int(df[v].isna().sum()) for v in items if df[v].isna().any()}
    assert not bad, f'missing dollar values in the file: {bad}'
    return items


def standard_cells(df):
    a, rng = df['agi'].to_numpy(float), df['age_range'].to_numpy()
    filer = df['filer'].to_numpy() == 1
    cells = [('all tax units', np.ones(len(df), bool)), ('filers', filer), ('non-filers', ~filer)]
    cells += [(lab, (a >= lo) & (a < hi)) for lo, hi, lab in AGI_GROUPS]
    cells += [(AGE_PLAIN[lab], rng == k) for k, lab in enumerate(common.AGE_LABELS)]
    return cells


def standard_stats(df, weights):
    """Holders and total of every dollar item in every standard cell, under each set of weights; plus net worth
    and total consumption by wage group. A weight set whose name starts with 'records' counts records."""
    items = dollar_items(df)
    X = df[items].to_numpy(float)
    NZ = (X != 0).astype(float)
    rows = []

    def add(cell, m, cols):
        Xm, Nm = X[m][:, cols], NZ[m][:, cols]
        res = {name: (w[m] @ Nm, w[m] @ Xm, w[m] @ np.abs(Xm)) for name, w in weights.items()}
        rec = Nm.sum(0)
        for j, c in enumerate(cols):
            for s, k in (('holders', 0), ('total', 1)):
                r = {'statistic': s, 'item': label(items[c]), 'variable': items[c], 'cell': cell, 'records': int(rec[j])}
                r.update({name: v[k][j] for name, v in res.items()})
                # the gross: the total of the item's absolute values (for holders, the holders), the scale a move
                # of a signed item is measured against, so that a total near zero does not make a small move large
                r.update({f'{name} gross': v[0 if k == 0 else 2][j] for name, v in res.items()
                          if not name.startswith('records')})
                rows.append(r)
    allc = list(range(len(items)))
    for cell, m in standard_cells(df):
        add(cell, m, allc)
    wage = common.wage_group(df['wages'].to_numpy(float))
    some = [items.index(v) for v in ('net_worth', 'total_consumption') if v in items]
    for _, _, g in common.WAGE_GROUPS:
        add(f'wages {g}', wage == g, some)
    return pd.DataFrame(rows)


def targeted_variables(t):
    """The file's variables some target constrains, from the target list's variable column, else from the names."""
    if 'variable' in t and t['variable'].notna().any():
        return set(t['variable'].dropna().astype(str))
    out = set()
    for kind, target in zip(t['kind'], t['target']):
        first = target.split(' ')[0].rstrip(',')
        if kind == 'IRS':
            out |= {'agi'} if first in ('returns', 'joint') else {first}
        elif kind == 'business' and first in TG.FAMILIES:
            f = TG.FAMILIES[first]
            out |= set(f['income']) | set(f['loss'])
        elif kind == 'wealth':
            out.add('net_worth')
        elif kind == 'consumption':
            out.add(first)
    return out


def moves(st, tv):
    m = st.copy()
    with np.errstate(divide='ignore', invalid='ignore'):
        m['change (%)'] = 100 * (m['after'] / m['before'] - 1)
        m['ratio'] = m['after'] / m['before']
        m['change (% of gross)'] = 100 * (m['after'] - m['before']) / m['before gross']
    m['variable targeted somewhere'] = m['variable'].isin(tv)
    m['_k'] = m['change (% of gross)'].abs()
    m = m.sort_values(['variable targeted somewhere', '_k'], ascending=[True, False], na_position='last')
    return m.drop(columns='_k')


def moves_page(m):
    s = m[(~m['variable targeted somewhere']) & (m['records'] >= MIN_RECORDS)].head(TOP)
    shown = pd.DataFrame({'statistic': s['statistic'], 'item': s['item'], 'cell': s['cell'], 'records': s['records'],
                          'before': [show(v, 'count' if k == 'holders' else 'total') for v, k in zip(s['before'], s['statistic'])],
                          'after': [show(v, 'count' if k == 'holders' else 'total') for v, k in zip(s['after'], s['statistic'])],
                          'change (%)': s['change (%)'].round(1), 'ratio': s['ratio'].round(3),
                          'change (% of gross)': s['change (% of gross)'].round(1)})
    big = m[(m['records'] >= MIN_RECORDS) & (m['change (% of gross)'].abs() > 10)]
    return [f"{len(m)} standard statistics; {len(big)} with at least {MIN_RECORDS} records moved more than 10% "
            f"({int((~big['variable targeted somewhere']).sum())} of them on variables no target constrains).", '',
            'The largest moves on variables no target constrains:', '', table(shown, ',.3f'), '',
            'Full table: report_moves.csv (moves on targeted variables after the others).']


def model_table(df, w1, tv, notes):
    if 'model' not in df:
        notes.append('The file has no model column: it comes from one trained model, so there is nothing to compare.')
        return None
    model = df['model'].to_numpy()
    ks = sorted(pd.unique(model))
    if len(ks) < 2:
        notes.append(f'Every record comes from one model ({ks[0]}): nothing to compare.')
        return None
    W = w1.sum()
    weights = {'pooled': w1}
    for k in ks:
        mk = model == k
        weights[f'model {k}'] = np.where(mk, w1 * W / w1[mk].sum(), 0.0)
        weights[f'records model {k}'] = mk.astype(float)
    st = standard_stats(df, weights)
    vals = st[[f'model {k}' for k in ks]].to_numpy(float)
    with np.errstate(divide='ignore', invalid='ignore'):   # measured against the pooled gross, as in section 4
        st['gap (% of gross)'] = 100 * (vals.max(1) - vals.min(1)) / st['pooled gross'].to_numpy(float)
    rec = st[[f'records model {k}' for k in ks]].to_numpy(float)
    st['fewest records in a model'] = rec.min(1).astype(int)
    st = st.drop(columns=[f'records model {k}' for k in ks] + [f'model {k} gross' for k in ks])
    st['variable targeted somewhere'] = st['variable'].isin(tv)
    st['_g'] = st['gap (% of gross)']
    st = st.sort_values(['variable targeted somewhere', '_g'], ascending=[True, False], na_position='last')
    notes.append(f'Models: {", ".join(str(k) for k in ks)}; each model\'s records are scaled up to the whole '
                 f"file's weight after the tilt.")
    return st.drop(columns='_g')


def models_page(mt, notes):
    if mt is None:
        return [f'- {n}' for n in notes]
    ks = [c for c in mt.columns if c.startswith('model ')]
    s = mt[(~mt['variable targeted somewhere']) & (mt['fewest records in a model'] >= MIN_RECORDS)].head(TOP)
    shown = pd.DataFrame({'statistic': s['statistic'], 'item': s['item'], 'cell': s['cell'],
                          **{k: [show(v, 'count' if t_ == 'holders' else 'total') for v, t_ in zip(s[k], s['statistic'])]
                             for k in ks}, 'gap (% of gross)': s['gap (% of gross)'].round(1)})
    ok = mt[mt['fewest records in a model'] >= MIN_RECORDS]['gap (% of gross)']
    return [f'Median gap {ok.median():.1f}%; {int((ok > 10).sum())} of {len(ok)} statistics (at least {MIN_RECORDS} '
            f'records in each model) differ by more than 10% between models.', '',
            'The largest gaps on variables no target constrains:', '', table(shown, ',.1f'), ''] + \
        [f'- {n}' for n in notes] + ['', 'Full table: report_models.csv.']


# ---- 6. overall ----------------------------------------------------------------------------------------------
def git(*args):
    try:
        return subprocess.run(['git', '-C', common.HERE, *args], capture_output=True, text=True, timeout=60).stdout.strip()
    except (OSError, subprocess.SubprocessError):
        return ''


def overall(df, t, th, w0, w1, run, tl):
    p0, p1 = w0 / w0.sum(), w1 / w1.sum()
    ess = lambda w: w.sum() ** 2 / (w ** 2).sum()
    f = w1 / w0
    keep = p1 > 0
    commit = git('rev-parse', 'HEAD')
    dirty = git('status', '--porcelain', '--', '.')
    vint = 'not recorded in the target list'
    if tl is not None:
        for c in ('vintage', 'vintages'):
            if c in tl and tl[c].notna().any():
                vint = '; '.join(sorted(tl[c].dropna().astype(str).unique()))
    n = th['flag'].value_counts() if len(th) else pd.Series(dtype=int)
    rows = [('records', f'{len(df):,}'),
            ('units, after the tilt (millions)', f'{w1.sum() / 1e6:,.2f}'),
            ('targets', f'{len(t)}'), ('targets that gave way', f"{int(t['gave_way'].sum())}"),
            ('thin cells: stops, warnings', f"{n.get('stop', 0)}, {n.get('warning', 0)}"),
            ('effective sample kept (%)', f'{100 * ess(w1) / ess(w0):.2f}'),
            ('KL move (nats)', f'{float(np.sum(p1[keep] * np.log(p1[keep] / p0[keep]))):.4f}'),
            ('weight factor, 1st and 99th percentiles', f'{np.quantile(f, 0.01):.3f}, {np.quantile(f, 0.99):.3f}'),
            ('records moved more than tenfold up, down', f'{int((f > 10).sum())}, {int((f < 0.1).sum())}'),
            ('settings file', os.environ.get('REBUILD_SETTINGS', os.path.join(common.HERE, 'settings.json'))),
            ('target configuration', str(common.SETTINGS.get('targets_config', common.SETTINGS.get('tilt', {})
                                                             .get('targets_config', 'not named in the settings')))),
            ('expanded target list', f'{run}/targets.csv' if tl is not None else 'none written by this run'),
            ('code commit', (commit or 'unknown') + (' (with uncommitted changes in the pipeline)' if dirty else '')),
            ('vintages read', vint), ('run folder', run)]
    if 'model' in df:
        rows.insert(1, ('records by model', ', '.join(f'{k}: {v:,}' for k, v in df['model'].value_counts().sort_index().items())))
    return pd.DataFrame(rows, columns=['', 'value'])


# ---- the page --------------------------------------------------------------------------------------------------
def non_micro_page(run):
    """Section 6: the non-microdata imputations' checks, as tilt.py wrote them (non_micro_checks.md and its CSVs, on
    the weights after the tilt; non_micro_imputations.py)."""
    path = f'{run}/non_micro_checks.md'
    if not os.path.exists(path):
        return ['The run wrote no non-microdata imputations (settings non_micro_imputations.run_in_tilt).']
    lines = open(path).read().split('\n')[2:]               # the page less its title
    return [('#' + x) if x.startswith('## ') else x for x in lines]


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('--run', default=common.TILT, help='the tilt run folder (default common.TILT)')
    ap.add_argument('--out', default=os.environ.get('REBUILD_REPORT_OUT'), help='where to write (default: --run)')
    ap.add_argument('--year', type=int, default=common.TARGET_YEAR)
    a = ap.parse_args(argv)
    out = a.out or a.run
    os.makedirs(out, exist_ok=True)
    df, fit, tl = read_run(a.run, a.year)
    w0, w1 = df['weight_start'].to_numpy(float), df['weight'].to_numpy(float)

    n1, n2, n3, n5 = [], [], [], []
    t = target_table(fit, tl, n1)
    th = thin_cells(t, df, tl, w0, w1, n2)
    p = published(df, t, w0, w1, n3)
    tv = targeted_variables(tl if tl is not None and 'variable' in tl else t)
    mv = moves(standard_stats(df, {'before': w0, 'after': w1}), tv)
    mt = model_table(df, w1, tv, n5)
    ov = overall(df, t, th, w0, w1, a.run, tl)

    t.to_csv(f'{out}/report_targets.csv', index=False)
    th.to_csv(f'{out}/report_thin_cells.csv', index=False)
    p.to_csv(f'{out}/report_published.csv', index=False)
    mv.to_csv(f'{out}/report_moves.csv', index=False)
    if mt is not None:
        mt.to_csv(f'{out}/report_models.csv', index=False)
    ov.to_csv(f'{out}/report_overall.csv', index=False)
    notes = lambda ns: ([''] + [f'- {n}' for n in ns]) if ns else []
    text = [f'# The {a.year} run', '', f'Written {datetime.date.today()} from {a.run}. Dollars are nominal {a.year} '
            f'dollars. Each section has a full table beside this page.', '',
            '## Overall', '', ov.to_markdown(index=False), '',
            '## 1. Targets', ''] + target_page(t, n1) + ['', '## 2. Thin cells', ''] + thin_page(th) + notes(n2) + \
        ['', '## 3. Published numbers not targeted', ''] + published_page(p) + notes(n3) + \
        ['', '## 4. What the tilt moved', ''] + moves_page(mv) + \
        ['', '## 5. Model disagreement', ''] + models_page(mt, n5) + \
        ['', '## 6. Non-microdata imputations: published checks (reported only)', ''] + non_micro_page(a.run) + ['']
    open(f'{out}/report.md', 'w').write('\n'.join(text))
    common.say('\n'.join(text))


if __name__ == '__main__':
    main()
