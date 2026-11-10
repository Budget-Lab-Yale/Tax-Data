"""How each dollar item is grown from one year to another.

Each item has one line in the target year's targets_config/<year>/growth.yaml. The line names the series the item
grows with, what the series is measured per (a return with the item, a holder, a household), how negative values are
grown, and the reason for the choice. An item's growth factor from one year to another is the series' level in the
later year over its level in the earlier year. For the series "soi 1.4" it is SOI's growth in dollars per return with
the item (prep/soi.py), carried past SOI's last year by CBO's projections. After the last year a series' data cover,
its level grows with personal income per person (BEA's, then CBO's projection, then Macro-Projections' GDP per person).
The SCF's series are applied by scale/maps.py. The series' data are in research/rebuild/resources/growth/.
FEATURES.md, "Dollars in the target year", describes the rules.
"""
import os

import numpy as np
import pandas as pd
import yaml

from shared import common
from prep import soi as TG

CONFIG = os.path.join(common.PIPELINE, 'targets_config')
RESOURCES = os.path.join(common.RESOURCES, 'growth')
LINE_KEYS = {'source', 'negative', 'measure', 'reason'}   # the keys every growth line has
# Each series: (what it measures, in words; its data files in RESOURCES, or None if it reads other sources)
SERIES = {
    'soi 1.4': ('dollars per return with the item, SOI Table 1.4', None),
    'agi per return': ('AGI per return, all returns (SOI Table 1.1), extended by BEA personal income per capita',
                       None),
    'bea mortgage interest per mortgaged owner': (
        'BEA NIPA Table 7.11 line 16 (mortgage interest paid by owner-occupants) over the ACS owner-occupied homes '
        'with a mortgage (B25081)', ('bea_mortgage_interest.csv', 'acs_mortgaged_owner_units.csv')),
    'acs real estate tax per paying owner': (
        'ACS aggregate real estate taxes paid by owner-occupants (B25090) over owner-occupied homes paying any '
        '(B25102)', ('acs_real_estate_taxes.csv',)),
    'state and local income tax per return': (
        "Census state and local individual income tax revenue over SOI's number of returns",
        ('census_sl_income_tax.csv',)),
    'meps out-of-pocket per person with any': (
        'MEPS out-of-pocket health spending (TOTSLF) per person with any', ('meps_oop_per_person.csv',)),
    'cpi tax preparation fees': ('BLS CPI-U, tax return preparation and other accounting fees (CUUR0000SS68023), '
                                 'annual average', ('cpi_tax_prep.csv',)),
    'college board tuition and fees': (
        'College Board published tuition and fees per full-time undergraduate, fixed 2015-16 sector weights',
        ('college_board_tuition_tax_year.csv',)),
    'nipa pce per return': ("the category's NIPA PCE over SOI's number of returns", None),
    'scf mean per unit between waves': ("the SCF wave's weighted mean per tax unit over another wave's", None),
    'scf net worth per unit between waves': ("the SCF wave's weighted mean net worth per tax unit over another wave's "
                                             "(after the last wave Z.1 household net worth per person)", None),
}


def _yaml(path):
    """Reads a YAML file."""
    with open(path) as f:
        return yaml.safe_load(f)


def path(year):
    """The growth.yaml of a target year."""
    return os.path.join(CONFIG, str(int(year)), 'growth.yaml')


def _is_none(spec):
    """Whether a source is 'none: <reason>': the amount is not grown."""
    return isinstance(spec, str) and spec.split(':')[0].strip() == 'none'


_LOADED = {}   # each year's growth lines, read once


def load(year=common.TARGET_YEAR):
    """{item: line} of the year's growth.yaml. Checks each line's keys and series, and stops with the full list of
    errors."""
    year = int(year)
    if year not in _LOADED:
        p = path(year)
        assert os.path.exists(p), f'no growth configuration for {year}: {p} (a new year starts as a copy of the ' \
                                  f'nearest year\'s folder)'
        d = _yaml(p)
        assert int(d.get('year', -1)) == year, f'{p}: says year {d.get("year")}'
        items = d.get('items') or {}
        bad = []
        for v, ln in items.items():
            if not isinstance(ln, dict) or set(ln) != LINE_KEYS:
                bad.append(f'{v}: needs exactly {sorted(LINE_KEYS)}, has {sorted(ln) if isinstance(ln, dict) else ln}')
                continue
            for k in ('measure', 'reason'):
                if not str(ln[k] or '').strip():
                    bad.append(f'{v}: empty {k}')
            for side in ('source', 'negative'):
                spec = ln[side]
                if side == 'negative' and spec == 'same':
                    continue
                if _is_none(spec):
                    if not spec.partition(':')[2].strip():
                        bad.append(f'{v}: {side} "none" needs its reason after a colon')
                    continue
                if not isinstance(spec, dict) or spec.get('series') not in SERIES:
                    bad.append(f'{v}: {side} {spec!r} is not a known series ({sorted(SERIES)}), "none: <reason>"'
                               + (' or "same"' if side == 'negative' else ''))
                elif spec['series'] == 'soi 1.4' and ('item' in spec) == ('rows' in spec):
                    bad.append(f'{v}: an SOI 1.4 source names an item or rows (not both)')
        assert not bad, f'{p}:\n  ' + '\n  '.join(bad)
        _LOADED[year] = items
    return _LOADED[year]


def line(v, year=common.TARGET_YEAR):
    """The growth line of item v in the target year's configuration."""
    items = load(year)
    assert v in items, f'{path(year)}: no growth line for {v} (every dollar item needs one)'
    return items[v]


# ---- The series ----
_CSV = {}   # each series file, read once


def _csv(name):
    """A series file in RESOURCES as {year: value}. A year may lack a value only with a note saying why; such a year
    is left out of the series."""
    if name not in _CSV:
        f = os.path.join(RESOURCES, name)
        assert os.path.exists(f), f'growth series file missing: {f}'
        d = pd.read_csv(f)
        assert not d['year'].isna().any() and d['year'].is_unique, f'{f}: a year missing or twice'
        gap = d['value'].isna()
        note = d['note'].fillna('').astype(str).str.strip() if 'note' in d else pd.Series('', index=d.index)
        assert not (gap & (note == '')).any(), f'{f}: a value missing with no note: {d.loc[gap & (note == ""), "year"].tolist()}'
        _CSV[name] = d[~gap].set_index('year')['value'].astype(float)
    return _CSV[name]


def _at(s, y, name):
    """A series' value in year y. Stops if the series has no value for y."""
    if y not in s.index:
        raise KeyError(f'growth series {name}: no value for {y} (it covers {int(s.index.min())}-{int(s.index.max())}; '
                       f'no extension is written for it)')
    return float(s.loc[y])


def population(y):
    """The Social Security area population in year y, from CBO's demographic projections."""
    return _at(_csv('ss_area_population.csv'), y, 'Social Security area population')


# The Financial Accounts (Z.1) category of each SCF balance-sheet item, used to grow it after the last SCF wave.
SCF_Z1_CATEGORIES = {
    **{f'value.{x}': 'deposits_bonds' for x in ('cash', 'bonds')},
    'value.equities': 'stocks_funds',
    **{f'value.{x}': 'retirement' for x in ('dc', 'db')},
    'value.pass_throughs': 'business',
    'value.primary_home': 'primary_home',
    **{f'value.{x}': 'other_real_estate' for x in ('other_home', 're_fund')},
    **{f'value.{x}': 'other_assets' for x in ('life_ins', 'annuities', 'trusts', 'other_fin', 'other_nonfin')},
    **{f'value.{x}': 'mortgages' for x in ('primary_mortgage', 'other_mortgage')},
    **{f'value.{x}': 'other_debt' for x in ('credit_lines', 'credit_cards', 'installment_debt', 'other_debt')},
}


def scf_extension(a, b, item=None):
    """The growth per person of an SCF item from year a to year b, after the last SCF wave, by the item's Financial
    Accounts (Z.1) category. An item outside the nine categories grows with household net worth."""
    category = SCF_Z1_CATEGORIES.get(item)
    name = f'z1_scf_{category}.csv' if category else 'z1_household_net_worth.csv'
    series = _csv(name)
    return (_at(series, b, name) / population(b)) / (_at(series, a, name) / population(a))


def agi_per_return(y):
    """AGI per return, all returns, from SOI's totals. After SOI's last year it grows with personal income per
    person."""
    tot = TG.totals()
    years = TG.total_years()
    last = years[-1]
    lev = lambda b: tot[(b, 'adjusted gross income less deficit')] / tot[(b, 'number of returns')]
    if y <= last:
        assert y in years, f'SOI: no totals for {y}'
        return lev(y)
    return lev(last) * income_per_person_growth(last, y)


def income_per_person_growth(a, b):
    """The growth of personal income per person from year a to year b: BEA's (NIPA Table 7.1 line 3) while it
    lasts, then CBO's projection through the last CBO year used, then nominal GDP per person from Macro-Projections."""
    from scale import projection_growth as PG
    L = PG.cbo_last_year()
    if b > L:
        x = max(a, L)
        return (income_per_person_growth(a, x) if x > a else 1.0) * PG.long_run_growth('gdp', x, b, per='population')
    bea = _csv('bea_personal_income_per_capita.csv')
    b_last = int(bea.index.max())
    if b <= b_last:
        return _at(bea, b, 'BEA personal income per capita') / _at(bea, a, 'BEA personal income per capita')
    cbo = _csv('cbo_personal_income_population.csv')
    mid = max(a, b_last)
    head = _at(bea, mid, 'BEA') / _at(bea, a, 'BEA') if mid > a else 1.0
    return head * _at(cbo, b, 'CBO personal income per capita') / _at(cbo, mid, 'CBO personal income per capita')


def level(spec, y):
    """The level of a series in year y (not SOI Table 1.4, whose growth is a ratio between years). After the last year
    its files cover, the last level is grown with personal income per person."""
    files = SERIES[spec['series']][1]
    if files:
        last = min(int(_csv(f).index.max()) for f in files)
        if y > last:
            return _level(spec, last) * income_per_person_growth(last, y)
    return _level(spec, y)


def _level(spec, y):
    """The level of a series in a year that its data cover."""
    s = spec['series']
    if s == 'agi per return':
        return agi_per_return(y)
    if s == 'bea mortgage interest per mortgaged owner':
        return _at(_csv('bea_mortgage_interest.csv'), y, s) / _at(_csv('acs_mortgaged_owner_units.csv'), y, s)
    if s == 'state and local income tax per return':
        return _at(_csv('census_sl_income_tax.csv'), y, s) / TG.all_returns(y)
    if s == 'nipa pce per return':
        if 'pce' not in _CSV:
            f = common.RAW['pce'] if os.path.isabs(common.RAW['pce']) else os.path.join(RESOURCES, common.RAW['pce'])
            _CSV['pce'] = pd.read_csv(f).set_index('year')
        p = _CSV['pce']
        if y in p.index:
            return p.loc[y, spec['category']] / TG.all_returns(y)
        # After the PCE file's last year, the last level per return is grown by CBO's personal income per person.
        last = int(p.index.max())
        assert y > last, f'PCE: no {y}'
        cbo = _csv('cbo_personal_income_population.csv')
        return p.loc[last, spec['category']] / TG.all_returns(last) * _at(cbo, y, 'CBO personal income per capita') \
            / _at(cbo, last, 'CBO personal income per capita')
    if s in ('acs real estate tax per paying owner', 'meps out-of-pocket per person with any',
             'cpi tax preparation fees', 'college board tuition and fees'):
        return _at(_csv(SERIES[s][1][0]), y, s)
    raise ValueError(f'growth series {s}: not a level series')


def _factor(spec, y):
    """The factor of one side's series from the PUF year to y."""
    if spec['series'] == 'soi 1.4':
        if 'item' in spec:
            return TG.per_return_growth(spec['item'], year=y)
        return TG.per_return_growth(None, rows=list(spec['rows']), absolute=bool(spec.get('absolute', False)), year=y)
    return level(spec, y) / level(spec, common.PUF_YEAR)


_FACTORS = {}   # each (item, year, target year)'s factors, computed once


def factors(v, y, year=common.TARGET_YEAR):
    """The growth factors of PUF amount v from the PUF year to year y, by the target year's growth line for v: (the
    factor for positive values, the factor for negative values)."""
    key = (v, int(y), int(year))
    if key not in _FACTORS:
        ln = line(v, year)
        assert not _is_none(ln['source']), f'{v}: a PUF amount with source "none" (only negative values may be none)'
        fp = _factor(ln['source'], y)
        neg = ln['negative']
        fn = fp if neg == 'same' else 1.0 if _is_none(neg) else _factor(neg, y)
        _FACTORS[key] = (fp, fn)
    return _FACTORS[key]


def between(v, s, t, year=common.TARGET_YEAR):
    """The factor of a CEX, CPS or SIPP amount v from year s to year t. These amounts are never negative."""
    ln = line(v, year)
    spec = ln['source']
    if _is_none(spec):
        return 1.0
    if spec['series'] == 'soi 1.4':
        f = _factor(spec, t) / _factor(spec, s)
    else:
        assert not spec['series'].startswith('scf '), f'{v}: the SCF is grown by maps.scf_growth'
        f = level(spec, t) / level(spec, s)
    # The dollar maps multiply by this without a check of their own.
    assert np.isfinite(f) and f > 0, f'{v}: growth factor {s} to {t} is {f} ({describe(v, year)})'
    return f


def describe(v, year=common.TARGET_YEAR):
    """The growth line of item v in words, for messages and reports."""
    ln = line(v, year)
    src = ln['source']
    if _is_none(src):
        return src
    out = src['series'] + (f": {src['item']}" if 'item' in src else f": {', '.join(src['rows'])}" if 'rows' in src
                           else f": {src['category']}" if 'category' in src else '')
    neg = ln['negative']
    return out + ('' if neg == 'same' else f'; negative values: {neg}' if isinstance(neg, str) else
                  f"; negative values: {neg['series']}")


# ---- The check before every run ----
_CHECKED = set()   # the years already checked


def check(year=common.TARGET_YEAR):
    """Checks the year's growth.yaml: every dollar item has a line, no line names an unknown item, each survey's lines
    name a series that its code applies, and every PUF item has a positive factor to the year. Stops with the full
    list of problems."""
    year = int(year)
    if year in _CHECKED:
        return
    from prep import build_table as BT
    items = load(year)
    groups = {'PUF': BT.PUF_AMOUNTS, 'SCF': BT.SCF_AMOUNTS, 'CEX': BT.CEX_AMOUNTS, 'CPS': BT.CPS_AMOUNTS,
              'SIPP': BT.SIPP_AMOUNTS, 'PUF and CPS shares': BT.PUF_SHARES}   # shares are never grown
    every = [v for vs in groups.values() for v in vs]
    bad = [f'{g} item {v}: no growth line' for g, vs in groups.items() for v in vs if v not in items]
    bad += [f'{v}: a growth line for an item the pipeline does not have' for v in items if v not in every]
    last_wave = max(int(w) for w in common.SETTINGS['paths']['raw']['scf_waves'])
    if year > last_wave:
        for v in SCF_Z1_CATEGORIES:
            try:
                factor = scf_extension(last_wave, year, v)
                assert np.isfinite(factor) and factor > 0
            except (OSError, KeyError, AssertionError, ValueError) as exc:
                bad.append(f'{v}: missing or invalid Financial Accounts category growth to {year}: {exc}')
    allowed = {'SCF': {'scf mean per unit between waves', 'scf net worth per unit between waves'}, 'CEX': {'nipa pce per return'},
               'CPS': {'soi 1.4'}, 'SIPP': {'soi 1.4'}}
    for g, ok in allowed.items():
        for v in groups[g]:
            if v not in items:
                continue
            src = items[v]['source']
            if not _is_none(src) and src['series'] not in ok:
                bad.append(f'{g} item {v}: series {src["series"]} is not one its code applies ({sorted(ok)})')
    for v in BT.PUF_SHARES:
        if v in items and not _is_none(items[v]['source']):
            bad.append(f'shared share {v}: a share (build_table.PUF_SHARES), its source must be none')
    for v in BT.SCF_RATIOS:
        if v in items and not _is_none(items[v]['source']):
            bad.append(f'SCF item {v}: a share or a count (build_table.SCF_RATIOS), its source must be none')
    cps_dollars = sorted(v for v in BT.CPS_AMOUNTS if v in items and not _is_none(items[v]['source']))
    if cps_dollars != sorted(common.SETTINGS['cps']['dollar_amounts']):
        bad.append(f'CPS items grown {cps_dollars} differ from settings cps.dollar_amounts '
                   f'{sorted(common.SETTINGS["cps"]["dollar_amounts"])}')
    for v in BT.PUF_AMOUNTS:
        if v not in items:
            continue
        try:
            f = factors(v, year, year)
            if not all(np.isfinite(x) and x > 0 for x in f):
                bad.append(f'PUF item {v}: factor {f} to {year} is not positive')
        except (KeyError, AssertionError, ValueError, ArithmeticError) as e:
            # An SOI line with no amount in the year, such as a repealed deduction, gives 0 / 0.
            bad.append(f'PUF item {v}: no factor to {year} ({type(e).__name__}: {e})')
    assert not bad, f'{path(year)}: the growth configuration is not complete:\n  ' + '\n  '.join(bad)
    _CHECKED.add(year)
