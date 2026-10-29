"""How every dollar item is grown from one year to another: the year's growth.yaml (targets_config/<year>/growth.yaml)
and the series it names (TARGETS.md section 12; NEXT_STEPS.md, 29 October).

Each dollar item the pipeline grows has one line in the target year's growth.yaml: its source (the series), the
measure (what the series is per: per return with the item, per holder, per household), how negative values are grown,
and a one-line reason. The series named there are used for every year the target year's run grows across: the PUF
year to the target year (the dollar maps, build_table.build_maps), the PUF year to each survey year (the tax data and
the survey cut points of the translation, build_table.translate_surveys) and, for a start from an earlier year's
file, that year to the target year (tilt.grow_dollars).

The series (SERIES below; their data in research/rebuild/resources/growth/, where sources.csv says where each came
from):
  soi 1.4           SOI Table 1.4, all returns: dollars per return with the item, the item given by `item` (a key of
                    targets.SOI_ITEMS) or by `rows` (SOI lines, with `absolute`), PUF year to y; past SOI's last
                    year, carried forward (targets.carried). This is the series every PUF item used before 29 October.
  agi per return    SOI's total AGI over all returns (Table 1.1; the same totals as Table 1.4's all-returns line, read
                    from the mirror); past SOI's last year, extended by BEA personal income per capita (NIPA Table
                    7.1 line 3: Table 2.1's personal income over population), and past BEA's last year by the growth
                    of CBO's projected personal income per person (TARGETS.md section 3's rule for a change).
  bea mortgage interest per mortgaged owner, acs real estate tax per paying owner, state and local income tax per
  return, meps out-of-pocket per person with any, cpi tax preparation fees, college board tuition and fees: a level
                    per year from the resources folder; past its last year the pipeline stops (no extension is written).
  nipa pce per return   a CEX category's NIPA PCE over SOI's number of returns (the CEX's rule since 22 October).
  scf mean per unit between waves   the SCF's own weighted mean per tax unit, one wave over another
                    (build_table.scf_growth computes it from the table; nothing here).
  none              not grown, for the reason given (a statutory nominal amount, a share, a count).

  load(year)        the year's growth lines, checked
  factors(v, y)     (factor for positive values, for negative values) of PUF amount v from the PUF year to year y
  between(v, s, t)  the factor of a CEX, CPS or SIPP amount from year s to year t
  line(v)           the target year's line for item v
  check(year)       every dollar item has a line, every line a known series, and every PUF item a positive factor
                    to the target year; stops with a list of what is missing otherwise
  describe(v)       the line in words, for reports
"""
import os

import numpy as np
import pandas as pd
import yaml

import common
import targets as TG

HERE = os.path.dirname(os.path.abspath(__file__))
CONFIG = common.SETTINGS.get('targets_config', os.path.join(HERE, 'targets_config'))
RESOURCES = os.path.join(os.path.dirname(HERE), 'resources', 'growth')
LINE_KEYS = {'source', 'negative', 'measure', 'reason'}
# series: (the measure in words, the data file(s) in RESOURCES)
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
}


def _yaml(path):
    with open(path) as f:
        return yaml.safe_load(f)


def path(year):
    return os.path.join(CONFIG, str(int(year)), 'growth.yaml')


def _is_none(spec):
    return isinstance(spec, str) and spec.split(':')[0].strip() == 'none'


_LOADED = {}


def load(year=common.TARGET_YEAR):
    """{item: line} of the year's growth.yaml, each line's keys and series checked."""
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
    items = load(year)
    assert v in items, f'{path(year)}: no growth line for {v} (every dollar item needs one)'
    return items[v]


# ---- the series --------------------------------------------------------------------------------------------------
_CSV = {}


def _csv(name):
    if name not in _CSV:
        f = os.path.join(RESOURCES, name)
        assert os.path.exists(f), f'growth series file missing: {f}'
        d = pd.read_csv(f)
        assert not d['year'].isna().any() and d['year'].is_unique, f'{f}: a year missing or twice'
        # a year with no value is allowed only with a note saying why (2026 of the Census revenue: two quarters
        # published); it is not in the series, so asking for it stops with "no value"
        gap = d['value'].isna()
        note = d['note'].fillna('').astype(str).str.strip() if 'note' in d else pd.Series('', index=d.index)
        assert not (gap & (note == '')).any(), f'{f}: a value missing with no note: {d.loc[gap & (note == ""), "year"].tolist()}'
        _CSV[name] = d[~gap].set_index('year')['value'].astype(float)
    return _CSV[name]


def _at(s, y, name):
    if y not in s.index:
        raise KeyError(f'growth series {name}: no value for {y} (it covers {int(s.index.min())}-{int(s.index.max())}; '
                       f'no extension is written for it)')
    return float(s.loc[y])


def _soi_per_capita_extension(y, last):
    """Growth of personal income per capita from SOI's last year to y: BEA's (NIPA Table 7.1 line 3) while BEA has the
    year, then CBO's projection."""
    bea = _csv('bea_personal_income_per_capita.csv')
    b_last = int(bea.index.max())
    if y <= b_last:
        return _at(bea, y, 'BEA personal income per capita') / _at(bea, last, 'BEA personal income per capita')
    cbo = _csv('cbo_personal_income_population.csv')   # CBO's level uses the Social Security area population: its growth only
    return (_at(bea, b_last, 'BEA personal income per capita') / _at(bea, last, 'BEA personal income per capita')) \
        * _at(cbo, y, 'CBO personal income per capita') / _at(cbo, b_last, 'CBO personal income per capita')


def population(y):
    """The Social Security area population in year y (resources/growth/ss_area_population.csv: Macro-Projections, CBO's
    January 2026 demographic projections; build_projection_series.py)."""
    return _at(_csv('ss_area_population.csv'), y, 'Social Security area population')


def scf_extension(a, b):
    """The SCF balance sheet's growth per person from stock year a to b after the last wave (30 October): household
    net worth (Z.1 S1M.b, FL152090005, fourth quarter; past 2025 held at its 2025 ratio to nominal GDP) over the
    Social Security area population. One factor for every balance-sheet item (build_table.scf_mean)."""
    nw = _csv('z1_household_net_worth.csv')
    return (_at(nw, b, 'Z.1 household net worth') / population(b)) / (_at(nw, a, 'Z.1 household net worth') / population(a))


def agi_per_return(y):
    tot = TG.totals()
    years = TG.total_years()
    last = years[-1]
    lev = lambda b: tot[(b, 'adjusted gross income less deficit')] / tot[(b, 'number of returns')]
    if y <= last:
        assert y in years, f'SOI: no totals for {y}'
        return lev(y)
    return lev(last) * _soi_per_capita_extension(y, last)


def income_per_person_growth(a, b):
    """Growth of personal income per person from year a to b: BEA's (NIPA Table 7.1 line 3) while BEA has both years,
    then CBO's projection (cbo_personal_income_population.csv, its growth only)."""
    bea = _csv('bea_personal_income_per_capita.csv')
    b_last = int(bea.index.max())
    if b <= b_last:
        return _at(bea, b, 'BEA personal income per capita') / _at(bea, a, 'BEA personal income per capita')
    cbo = _csv('cbo_personal_income_population.csv')
    mid = max(a, b_last)
    head = _at(bea, mid, 'BEA') / _at(bea, a, 'BEA') if mid > a else 1.0
    return head * _at(cbo, b, 'CBO personal income per capita') / _at(cbo, mid, 'CBO personal income per capita')


def level(spec, y):
    """The series' level in year y (a series given as a level; not SOI 1.4, which is a ratio between years). Past the
    last year its files cover (30 October, years after the data): the last year's level grown with personal income
    per person (BEA, then CBO's projection; income_per_person_growth), a placeholder recorded in the year's growth
    report."""
    files = SERIES[spec['series']][1]
    if files:
        last = min(int(_csv(f).index.max()) for f in files)
        if y > last:
            return _level(spec, last) * income_per_person_growth(last, y)
    return _level(spec, y)


def _level(spec, y):
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
        # past the PCE file's last year (30 October, years after the surveys): the last year's level per return grown
        # by CBO's personal income per person (the extension agi per return uses past BEA's last year), a placeholder
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


_FACTORS = {}


def factors(v, y, year=common.TARGET_YEAR):
    """(factor for positive values, for negative values) of PUF amount v from the PUF year to year y, by the target
    year's growth line for v."""
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
    """The factor of a CEX, CPS or SIPP amount v from year s to year t (both signs: these amounts are not negative)."""
    ln = line(v, year)
    spec = ln['source']
    if _is_none(spec):
        return 1.0
    if spec['series'] == 'soi 1.4':
        f = _factor(spec, t) / _factor(spec, s)
    else:
        assert spec['series'] != 'scf mean per unit between waves', f'{v}: the SCF is grown by build_table.scf_growth'
        f = level(spec, t) / level(spec, s)
    # the maps (build_table.build_maps, build_year_maps) multiply by this with no check of their own: a missing value
    # in a series (a blank cell in the NIPA PCE file) would otherwise reach them as a missing factor
    assert np.isfinite(f) and f > 0, f'{v}: growth factor {s} to {t} is {f} ({describe(v, year)})'
    return f


def grown_by(v, year=common.TARGET_YEAR):
    """True when the item is grown at all (its source is not "none")."""
    return not _is_none(line(v, year)['source'])


def describe(v, year=common.TARGET_YEAR):
    ln = line(v, year)
    src = ln['source']
    if _is_none(src):
        return src
    out = src['series'] + (f": {src['item']}" if 'item' in src else f": {', '.join(src['rows'])}" if 'rows' in src
                           else f": {src['category']}" if 'category' in src else '')
    neg = ln['negative']
    return out + ('' if neg == 'same' else f'; negative values: {neg}' if isinstance(neg, str) else
                  f"; negative values: {neg['series']}")


# ---- the check before every run ------------------------------------------------------------------------------------
_CHECKED = set()


def check(year=common.TARGET_YEAR):
    """Every dollar item the growth code touches has a line in the year's growth.yaml, no line names an item the
    pipeline does not have, the SCF, CEX, CPS and SIPP lines name the series their code applies, and every PUF item
    has a finite, positive factor for each sign from the PUF year to the year. Stops with the full list otherwise."""
    year = int(year)
    if year in _CHECKED:
        return
    import build_table as BT
    items = load(year)
    groups = {'PUF': BT.PUF_AMOUNTS, 'SCF': BT.SCF_AMOUNTS, 'CEX': BT.CEX_AMOUNTS, 'CPS': BT.CPS_AMOUNTS,
              'SIPP': BT.SIPP_AMOUNTS}
    every = [v for vs in groups.values() for v in vs]
    bad = [f'{g} item {v}: no growth line' for g, vs in groups.items() for v in vs if v not in items]
    bad += [f'{v}: a growth line for an item the pipeline does not have' for v in items if v not in every]
    allowed = {'SCF': {'scf mean per unit between waves'}, 'CEX': {'nipa pce per return'},
               'CPS': {'soi 1.4'}, 'SIPP': {'soi 1.4'}}
    for g, ok in allowed.items():
        for v in groups[g]:
            if v not in items:
                continue
            src = items[v]['source']
            if not _is_none(src) and src['series'] not in ok:
                bad.append(f'{g} item {v}: series {src["series"]} is not one its code applies ({sorted(ok)})')
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
            # ArithmeticError: an SOI line with no amount in the year (the domestic production activities deduction
            # from 2020, the tuition deduction from 2021, after their repeal) gives 0 / 0
            bad.append(f'PUF item {v}: no factor to {year} ({type(e).__name__}: {e})')
    assert not bad, f'{path(year)}: the growth configuration is not complete:\n  ' + '\n  '.join(bad)
    _CHECKED.add(year)
