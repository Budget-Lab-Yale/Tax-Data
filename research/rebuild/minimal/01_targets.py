#!/usr/bin/env python3
#-------------------------------------------------------------------------------
# 01_targets.py
#
# Builds the published IRS (SOI, Publication 1304) tables that the minimal
# experiment validates against (tax year 2015) and tilts to (tax year 2019).
# Six variables matter to the experiment: married (joint or not), number of
# dependents, wages, interest, dividends and net capital gain.
#
# What it reads (nothing is downloaded):
#   1. The SOI by-size-of-AGI tables mirrored in the shared raw-data store,
#      /nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-Ind/aligned/, which
#      holds every published cell of Pub 1304 Tables 1.2, 1.4 and 2.3 in long
#      form, with SOI's disclosure marks kept in a `flag` column:
#        Table 1.4 (bysize_income_sources.csv): returns and income items by AGI
#        Table 1.2 (bysize_marital_status.csv): returns and AGI by filing status
#        Table 2.3 (bysize_exemptions.csv): dependent exemptions, 2015 only
#   2. Tax-Data's own Compiled-SOI-Tables interface, used here only as a
#      cross-check: the hand-compiled Table 1.4 for 2015 (version 1) and 2019
#      (version 2, the version main.R reads), and Table 1.6 for 2015.
#      The repository's copies do not carry Table 1.2 or Table 2.3 for these
#      years, or Table 1.4 for 2015 in the pinned version, which is why the
#      raw-data mirror is the primary source.
#
# What it writes, to /nfs/roberts/scratch/pi_nrs36/jar335/minimal/targets/:
#   soi_2015_validation.csv   Table 1.4, 1.2 and 2.3 rows for 2015
#   soi_2019_targets.csv      Table 1.4 and 1.2 rows for 2019
#   soi_totals_2015_2019.csv  all-returns totals for both years
#   crosscheck_repo_copies.csv  every comparison against the repo's copies
#
# Units: SOI publishes money amounts in thousands of dollars. Every amount
# written here is in dollars (published value times 1,000). Counts are numbers
# of returns (or of exemptions, for Table 2.3's exemption counts).
#
# Missing values are never filled. A cell SOI left blank stays blank, with
# SOI's mark in the flag column, and the script prints how many there are.
#
# Run from anywhere (Python 3 standard library only):
#   python3 research/rebuild/minimal/01_targets.py
#-------------------------------------------------------------------------------

import csv
import math
import os
import sys
from collections import defaultdict

ALIGNED  = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-Ind/aligned'
COMPILED = '/nfs/roberts/project/pi_nrs36/shared/model_data/Compiled-SOI-Tables'
OUT      = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/targets'

# SOI's own file names for each table and year, for the source column
SOI_FILE = {
    ('1.4', 2015): '15in14ar.xls', ('1.4', 2019): '19in14ar.xls',
    ('1.2', 2015): '15in12ms.xls', ('1.2', 2019): '19in12ms.xls',
    ('2.3', 2015): '15in23ar.xls',
}
ALIGNED_FILE = {'1.4': 'bysize_income_sources.csv',
                '1.2': 'bysize_marital_status.csv',
                '2.3': 'bysize_exemptions.csv'}

# Table 1.4 columns kept: SOI item label -> (variable, statistic)
T14_ITEMS = {
    'number of returns':                       ('returns', 'count'),
    'adjusted gross income less deficit':      ('agi', 'amount'),
    'salaries and wages > number of returns':  ('wages', 'count'),
    'salaries and wages > amount':             ('wages', 'amount'),
    'taxable interest > number of returns':    ('taxable_interest', 'count'),
    'taxable interest > amount':               ('taxable_interest', 'amount'),
    'tax exempt interest > number of returns': ('tax_exempt_interest', 'count'),
    'tax exempt interest > amount':            ('tax_exempt_interest', 'amount'),
    'ordinary dividends > number of returns':  ('ordinary_dividends', 'count'),
    'ordinary dividends > amount':             ('ordinary_dividends', 'amount'),
    'qualified dividends > number of returns': ('qualified_dividends', 'count'),
    'qualified dividends > amount':            ('qualified_dividends', 'amount'),
    'capital gain distributions reported on form 1040 > number of returns':
        ('capital_gain_distributions_no_sch_d', 'count'),
    'capital gain distributions reported on form 1040 > amount':
        ('capital_gain_distributions_no_sch_d', 'amount'),
    'sales of capital assets reported on form 1040, schedule d > taxable net gain > number of returns':
        ('sch_d_net_gain', 'count'),
    'sales of capital assets reported on form 1040, schedule d > taxable net gain > amount':
        ('sch_d_net_gain', 'amount'),
    'sales of capital assets reported on form 1040, schedule d > taxable net loss > number of returns':
        ('sch_d_net_loss', 'count'),
    'sales of capital assets reported on form 1040, schedule d > taxable net loss > amount':
        ('sch_d_net_loss', 'amount'),
}

# Table 1.2 filing-status blocks -> code used here
T12_STATUS = {
    'all returns': 'all',
    'returns of married persons filing jointly and returns of surviving spouses': 'joint',
    'returns of married persons filing separately': 'separate',
    'returns of heads of households': 'head_of_household',
    'returns of single persons': 'single',
}
T12_MEASURES = {'number of returns': ('returns', 'count'),
                'adjusted gross income less deficit': ('agi', 'amount')}

# Table 2.3 columns kept (2015 only; the table ended with tax year 2017)
T23_ITEMS = {
    'number of returns': ('returns', 'count'),
    'total > number of returns': ('returns_with_dependent_exemptions', 'count'),
    'total > number of exemptions': ('dependent_exemptions', 'count'),
    'exemptions for children at home > number of returns': ('returns_with_children_at_home', 'count'),
    'exemptions for children at home > number of exemptions': ('exemptions_children_at_home', 'count'),
    'exemptions for children away from home > number of returns': ('returns_with_children_away', 'count'),
    'exemptions for children away from home > number of exemptions': ('exemptions_children_away', 'count'),
    'exemptions for parents > number of returns': ('returns_with_parent_dependents', 'count'),
    'exemptions for parents > number of exemptions': ('exemptions_parents', 'count'),
    'exemptions for other dependents > number of returns': ('returns_with_other_dependents', 'count'),
    'exemptions for other dependents > number of exemptions': ('exemptions_other_dependents', 'count'),
}
# Table 2.3 items that are counts of exemptions, not of returns: they stay
# as counts but the README says what they count.

FIELDS = ['year', 'table', 'filing_status', 'agi_bracket_lo', 'agi_bracket_hi',
          'agi_bracket_label', 'variable', 'statistic', 'value', 'flag', 'source']


def bound(x, is_total):
    """AGI bound as a plain number string; totals get -Inf / Inf."""
    if is_total:
        return None
    if x in ('-Inf', 'Inf'):
        return x
    return str(int(round(float(x))))


def to_value(raw, statistic):
    """Published cell -> value in dollars (amounts) or as-is (counts)."""
    if raw == '':
        return ''
    v = float(raw)
    if statistic == 'amount':
        v = v * 1000.0
    return str(int(round(v)))


def source(table, year, derived=None):
    s = (f'SOI Pub 1304 Table {table}, TY{year} ({SOI_FILE[(table, year)]}); '
         f'read from {ALIGNED}/{ALIGNED_FILE[table]}')
    if derived:
        s = 'DERIVED: ' + derived + '. Inputs: ' + s
    return s


def read_aligned(table, year):
    path = os.path.join(ALIGNED, ALIGNED_FILE[table])
    with open(path, newline='') as f:
        return [r for r in csv.DictReader(f)
                if r['tax_year'] == str(year) and r['table'] == table
                and r['panel'] == 'All returns']


def rows_from(table, year, items_map, status_of=None):
    """Long rows for one table-year. status_of maps an item to (status, item)."""
    out = []
    for r in read_aligned(table, year):
        item = r['item']
        status = 'all'
        if status_of is not None:
            hit = status_of(item)
            if hit is None:
                continue
            status, item = hit
        if item not in items_map:
            continue
        var, stat = items_map[item]
        is_total = r['row_label'].endswith('total')
        if is_total and r['row_label'] != 'All returns, total':
            continue
        out.append({
            'year': year, 'table': table, 'filing_status': status,
            'agi_bracket_lo': bound(r['row_agi_lo'], is_total) or '-Inf',
            'agi_bracket_hi': bound(r['row_agi_hi'], is_total) or 'Inf',
            'agi_bracket_label': 'all' if is_total else r['row_label'],
            'variable': var, 'statistic': stat,
            'value': to_value(r['value'], stat), 'flag': r['flag'],
            'source': source(table, year),
        })
    return out


def t12_status(item):
    for prefix, code in T12_STATUS.items():
        if item.startswith(prefix + ' > '):
            return code, item[len(prefix) + 3:]
    return None


def add_derived(rows, year):
    """Derived rows, amounts only: total interest, net capital gain, nonjoint."""
    out = []
    key = lambda r: (r['table'], r['filing_status'], r['agi_bracket_lo'],
                     r['agi_bracket_hi'], r['agi_bracket_label'])
    by = defaultdict(dict)
    for r in rows:
        by[key(r)][(r['variable'], r['statistic'])] = r

    def combine(cells, signs):
        if any(c['value'] == '' for c in cells):
            return '', 'missing_input'
        flag = ';'.join(sorted({c['flag'] for c in cells if c['flag']}))
        return str(sum(s * int(c['value']) for c, s in zip(cells, signs))), flag

    for k, d in by.items():
        table, status, lo, hi, label = k
        base = dict(year=year, table=table, filing_status=status,
                    agi_bracket_lo=lo, agi_bracket_hi=hi, agi_bracket_label=label)
        if table == '1.4':
            parts = [d[('taxable_interest', 'amount')], d[('tax_exempt_interest', 'amount')]]
            v, f = combine(parts, [1, 1])
            out.append({**base, 'variable': 'interest_total', 'statistic': 'amount',
                        'value': v, 'flag': f,
                        'source': source('1.4', year, 'taxable interest + tax-exempt interest')})
            parts = [d[('capital_gain_distributions_no_sch_d', 'amount')],
                     d[('sch_d_net_gain', 'amount')], d[('sch_d_net_loss', 'amount')]]
            v, f = combine(parts, [1, 1, -1])
            out.append({**base, 'variable': 'net_capital_gain', 'statistic': 'amount',
                        'value': v, 'flag': f,
                        'source': source('1.4', year,
                                         'capital gain distributions reported on Form 1040 '
                                         '+ Schedule D taxable net gain - Schedule D taxable net loss '
                                         '(= Form 1040 capital gain line, PUF E01000)')})
    # nonjoint = separate + head of household + single, from Table 1.2
    groups = defaultdict(dict)
    for r in rows:
        if r['table'] == '1.2' and r['filing_status'] in ('separate', 'head_of_household', 'single'):
            groups[(r['agi_bracket_lo'], r['agi_bracket_hi'], r['agi_bracket_label'],
                    r['variable'], r['statistic'])][r['filing_status']] = r
    for (lo, hi, label, var, stat), d in groups.items():
        if len(d) != 3:
            sys.exit(f'Table 1.2 {year}: status block missing for {label} {var}')
        v, f = combine(list(d.values()), [1, 1, 1])
        out.append(dict(year=year, table='1.2', filing_status='nonjoint',
                        agi_bracket_lo=lo, agi_bracket_hi=hi, agi_bracket_label=label,
                        variable=var, statistic=stat, value=v, flag=f,
                        source=source('1.2', year, 'separate + head of household + single')))
    return out


def write(path, rows):
    with open(path, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(rows)


def check_partition(rows, label):
    """Each published total against the sum of its brackets."""
    groups = defaultdict(lambda: [None, 0, 0])
    for r in rows:
        if r['value'] == '':
            continue
        k = (r['table'], r['filing_status'], r['variable'], r['statistic'])
        if r['agi_bracket_label'] == 'all':
            groups[k][0] = int(r['value'])
        else:
            groups[k][1] += int(r['value'])
            groups[k][2] += 1
    worst = 0.0
    for k, (tot, s, n) in groups.items():
        if tot is None:
            continue
        unit = 1000 if k[3] == 'amount' else 1
        gap = abs(tot - s) / unit
        if gap > n:   # more than one published unit per bracket
            print(f'  {label}: total vs brackets differ for {k}: {tot} vs {s}')
        worst = max(worst, gap)
    print(f'  {label}: largest total-minus-brackets gap {worst:.0f} published units')


#--------------------------- build the year files ------------------------------

all_rows = {}
for year, tables in [(2015, ['1.4', '1.2', '2.3']), (2019, ['1.4', '1.2'])]:
    rows = []
    rows += rows_from('1.4', year, T14_ITEMS)
    rows += rows_from('1.2', year, T12_MEASURES, status_of=t12_status)
    if '2.3' in tables:
        rows += rows_from('2.3', year, T23_ITEMS)
    rows += add_derived(rows, year)
    n_missing = sum(r['value'] == '' for r in rows)
    flags = defaultdict(int)
    for r in rows:
        if r['flag']:
            flags[r['flag']] += 1
    print(f'{year}: {len(rows)} rows, {n_missing} missing values, flags {dict(flags)}')
    check_partition([r for r in rows if not r['source'].startswith('DERIVED')], str(year))
    all_rows[year] = rows

write(os.path.join(OUT, 'soi_2015_validation.csv'), all_rows[2015])
write(os.path.join(OUT, 'soi_2019_targets.csv'), all_rows[2019])

#--------------------------- totals file ---------------------------------------

TOTAL_VARS = ['returns', 'agi', 'wages', 'taxable_interest', 'tax_exempt_interest',
              'interest_total', 'ordinary_dividends', 'qualified_dividends',
              'capital_gain_distributions_no_sch_d', 'sch_d_net_gain',
              'sch_d_net_loss', 'net_capital_gain']
totals = []
for year in (2015, 2019):
    for r in all_rows[year]:
        if r['agi_bracket_label'] != 'all':
            continue
        if r['table'] == '1.4' and r['variable'] in TOTAL_VARS:
            totals.append(r)
        if r['table'] == '1.2' and r['variable'] == 'returns':
            totals.append(r)
        if r['table'] == '2.3' and r['variable'] in ('returns_with_dependent_exemptions',
                                                     'dependent_exemptions'):
            totals.append(r)
write(os.path.join(OUT, 'soi_totals_2015_2019.csv'), totals)

#--------------------------- cross-check against the repo's copies -------------

def num(s):
    s = s.replace(',', '').strip()
    return None if s in ('', '-') else float(s)

checks = []
def chk(year, what, ours, theirs, src):
    diff = None if (ours is None or theirs is None) else ours - theirs
    checks.append(dict(year=year, comparison=what, this_file=ours, repo_copy=theirs,
                       difference=diff, repo_file=src))

def ours_lookup(year, table, status, lo, var, stat):
    for r in all_rows[year]:
        if (r['table'] == table and r['filing_status'] == status
                and r['agi_bracket_lo'] == lo and r['variable'] == var
                and r['statistic'] == stat and r['agi_bracket_label'] != 'all'):
            return None if r['value'] == '' else int(r['value'])
    return 'absent'

def lo_key(x):
    x = float(x)
    return '-Inf' if x < -1e90 else str(int(x))

# (a) 2015 Table 1.4 against Compiled-SOI-Tables v1 'SOI 1.4.csv'
v1 = os.path.join(COMPILED, 'v1/2023092813/historical/SOI 1.4.csv')
V1_MAP = {'adj_gross_less_def': ('returns', 'agi'),
          'Salaries and wages': ('wages', 'wages'),
          'Taxable interest': ('taxable_interest', 'taxable_interest'),
          'Tax-exempt interest': ('tax_exempt_interest', 'tax_exempt_interest'),
          'Ordinary dividends': ('ordinary_dividends', 'ordinary_dividends'),
          'Qualified dividends': ('qualified_dividends', 'qualified_dividends'),
          'Sales of capital assets reported on Form 1040 (net gain)': ('sch_d_net_gain', 'sch_d_net_gain'),
          'Sales of capital assets reported on Form 1040 (net loss)': ('sch_d_net_loss', 'sch_d_net_loss')}
LABEL_LO = {}
for r in read_aligned('1.4', 2015):
    if r['col_seq'] == '1' and not r['row_label'].endswith('total'):
        LABEL_LO[r['row_label'].strip()] = bound(r['row_agi_lo'], False)
with open(v1, newline='', encoding='utf-8-sig') as f:
    for r in csv.DictReader(f):
        if r['Year'] != '2015':
            continue
        var = r['Variable '].strip()
        if var not in V1_MAP:
            continue
        lo = LABEL_LO[r['AGI_Group'].strip()]
        cvar, avar = V1_MAP[var]
        c, a = num(r['#_of_returns']), num(r['$_amount'])
        o = ours_lookup(2015, '1.4', 'all', lo, cvar, 'count')
        chk(2015, f'T1.4 {cvar} count, AGI from {lo}', o, c, v1)
        o = ours_lookup(2015, '1.4', 'all', lo, avar, 'amount')
        chk(2015, f'T1.4 {avar} amount, AGI from {lo}', o, None if a is None else a * 1000, v1)

# (b) 2019 Table 1.4 against Compiled-SOI-Tables v2 table_1_4.csv (the pinned version)
v2 = os.path.join(COMPILED, 'v2/2023100323/historical/table_1_4.csv')
V2_MAP = {'wages': 'wages', 'txbl_int': 'taxable_interest', 'exempt_int': 'tax_exempt_interest',
          'div': 'ordinary_dividends', 'div_pref': 'qualified_dividends',
          'txbl_kg.income': 'sch_d_net_gain', 'txbl_kg.loss': 'sch_d_net_loss'}
with open(v2, newline='', encoding='utf-8-sig') as f:
    for r in csv.DictReader(f):
        if r['year'] != '2019':
            continue
        lo = lo_key(r['agi'])
        for k, var in V2_MAP.items():
            o = ours_lookup(2019, '1.4', 'all', lo, var, 'count')
            chk(2019, f'T1.4 {var} count, AGI from {lo}', o, num(r['count__' + k]), v2)
            a = num(r['amount__' + k])
            o = ours_lookup(2019, '1.4', 'all', lo, var, 'amount')
            chk(2019, f'T1.4 {var} amount, AGI from {lo}', o, None if a is None else a * 1000, v2)

# (c) 2015 returns by filing status: Table 1.2 here against Table 1.6 in the repo
v2_16 = os.path.join(COMPILED, 'v2/2023100323/historical/table_1_6.csv')
MARS = {'1': 'single', '2': 'joint', '3': 'separate', '4': 'head_of_household'}
t16 = defaultdict(float)
with open(v2_16, newline='', encoding='utf-8-sig') as f:
    for r in csv.DictReader(f):
        if r['year'] != '2015':
            continue
        for k, v in r.items():
            if k in ('year', 'filing_status', 'age_group'):
                continue
            t16[(MARS[r['filing_status']], lo_key(k))] += (num(v) or 0.0)
for (status, lo), v in sorted(t16.items()):
    o = ours_lookup(2015, '1.2', status, lo, 'returns', 'count')
    chk(2015, f'T1.2 returns ({status}, AGI from {lo}) vs repo T1.6 summed over age', o, v, v2_16)

with open(os.path.join(OUT, 'crosscheck_repo_copies.csv'), 'w', newline='') as f:
    w = csv.DictWriter(f, fieldnames=['year', 'comparison', 'this_file', 'repo_copy',
                                      'difference', 'repo_file'])
    w.writeheader()
    w.writerows(checks)

n_absent = sum(c['this_file'] == 'absent' for c in checks)
n_exact = sum(c['difference'] == 0 for c in checks)
n_diff = [c for c in checks if c['difference'] not in (0, None)]
print(f'cross-check: {len(checks)} comparisons, {n_exact} exact, '
      f'{len(n_diff)} differ, {n_absent} not found here')
for c in n_diff:
    print('   ', c['year'], c['comparison'], c['this_file'], c['repo_copy'], c['difference'])
