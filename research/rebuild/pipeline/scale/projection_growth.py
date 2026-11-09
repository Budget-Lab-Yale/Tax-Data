"""Growth factors from CBO and Macro-Projections series for the projection years.

The functions here give CBO's growth in the number of returns, the growth in gross Social Security benefits per return,
and the growth of each series after CBO's last projection year. In those later years every series grows at the rate of
one Macro-Projections aggregate. FEATURES.md, "4. Scaling to the target year", "Projection years", describes the rules.
"""
import os

import numpy as np
import pandas as pd

from shared import common

from scale.projection_demographics import cbo_count

RESOURCES = common.RESOURCES


def cbo_return_growth(year):
    """CBO's growth in the total number of individual income tax returns from year - 1 to year (a ratio)."""
    return cbo_count(year, 'rev_iit_returns_total') / cbo_count(year - 1, 'rev_iit_returns_total')


def ssa_per_return(item, year):
    """The growth in gross Social Security benefits per return from year - 1 to year, for 2024 and 2025. It is the
    growth in SSA's OASDI benefits paid in the calendar year (Annual Statistical Supplement Table 4.A4 and Trustees
    Report Table III.A5) divided by CBO's growth in the number of returns. Returns the factor and a note on its source.
    These two years use SSA because CBO's fiscal-year outlays start at FY2025 and so cannot give their growth."""
    year = int(year)
    d = pd.read_csv(os.path.join(RESOURCES, 'ssa_oasdi_benefits.csv')).set_index('year')['oasdi_benefits_paid_millions']
    if year not in d.index or year - 1 not in d.index:
        raise ValueError(f'{item}: no SSA OASDI benefits paid for {year - 1} and {year} in ssa_oasdi_benefits.csv')
    a, b = float(d.loc[year - 1]), float(d.loc[year])
    factor = (b / a) / cbo_return_growth(year)
    return factor, f'SSA OASDI benefits paid in the calendar year / CBO total return growth, {year - 1} to {year}'


def gross_ss_between(a, b):
    """The growth in gross Social Security benefits per return from year a to year b. Both years are at or after SOI's
    last year. The growth is the product of the yearly factors: SSA's benefits through 2025, then CBO's outlays
    through CBO's last year used, then Macro-Projections' OASDI outlays per tax unit. A year with no source stops the
    run."""
    L = cbo_last_year()
    f = 1.0
    for y in range(int(a) + 1, min(int(b), L) + 1):
        f *= cbo_per_return('ss_gross', y)[0]
    if int(b) > L:
        f *= long_run_growth('outlays_mand_oasdi', max(int(a), L), int(b), per='units')
    return f


# ---- The years after CBO's projection ----
# After CBO's last projection year used (settings tilt.projection.cbo_last_year), every series grows at the rate of one
# Macro-Projections aggregate, read from resources/growth/macro_long_run.csv. The functions below extend the series
# that the growth.yaml rules read, and the count of returns. The series files of the items table
# (targets_config/projection_items.csv) already include their own extension.
_LR = {}   # the table macro_long_run.csv, read once


def cbo_last_year():
    """The last year a CBO projection is used (settings tilt.projection.cbo_last_year)."""
    return int(common.SETTINGS['tilt']['projection']['cbo_last_year'])


def long_run_level(name, year):
    """The level of a long-run aggregate in year. name is a column of resources/growth/macro_long_run.csv, or a sum
    of columns written 'a + b'. The columns are the Macro-Projections aggregates, 'units' (people aged 18 and over,
    with a married couple counted once) and 'population' (people of every age)."""
    if 'd' not in _LR:
        d = pd.read_csv(os.path.join(RESOURCES, 'growth', 'macro_long_run.csv'))
        assert d['year'].is_unique, 'macro_long_run.csv: a year twice'
        _LR['d'] = d.set_index('year')
    d = _LR['d']
    if int(year) not in d.index:
        raise ValueError(f'long-run aggregates: no {year} in macro_long_run.csv (it covers {d.index.min()}-{d.index.max()})')
    parts = [q.strip() for q in name.split('+')]
    missing = [q for q in parts if q not in d.columns]
    if missing:
        raise ValueError(f'long-run aggregates: no column {missing} in macro_long_run.csv')
    v = float(sum(float(d.loc[int(year), q]) for q in parts))
    if not (np.isfinite(v) and v > 0):
        raise ValueError(f'long-run aggregate {name} in {year}: {v}')
    return v


def long_run_growth(name, a, b, per=None):
    """The growth of the long-run aggregate name from year a to year b. When per is 'units' or 'population', the
    growth is divided by the growth of that count over the same years, which gives a growth per tax unit or per
    person."""
    g = long_run_level(name, b) / long_run_level(name, a)
    if per is not None:
        g /= long_run_level(per, b) / long_run_level(per, a)
    return g


def returns_growth(a, b):
    """The growth in the number of returns from year a to year b, with a <= b. It follows CBO's count through
    cbo_last_year, and after that the number of adults with a married couple counted once ('units')."""
    a, b, L = int(a), int(b), cbo_last_year()
    f = 1.0
    for y in range(a + 1, min(b, L) + 1):
        f *= cbo_return_growth(y)
    if b > L:
        f *= long_run_growth('units', max(a, L), b)
    return f


def cbo_per_return(item, year):
    """The growth per return of an item from year - 1 to year, and a note on its source. For gross Social Security
    (item 'ss_gross') through 2025 this is SSA's benefits paid (ssa_per_return). Otherwise it is the growth of the
    item's CBO series, divided by CBO's growth in the number of returns. CBO's fiscal-year levels stand in for the
    calendar year."""
    year = int(year)
    if item == 'ss_gross' and year <= 2025:
        return ssa_per_return(item, year)
    if not 2024 <= year <= 2036:
        raise ValueError(f'{item}: CBO-series growth is unavailable for {year - 1} to {year}')
    mapping = pd.read_csv(os.path.join(RESOURCES, 'projection_growth_mapping.csv'))
    rows = mapping[(mapping.old_item == item) & (mapping.source == 'CBO February 2026')]
    assert len(rows) == 1, f'{item}: expected one explicit CBO mapping; found {len(rows)}'
    series, source_path = rows.iloc[0]['series'], rows.iloc[0]['path']
    assert source_path.endswith('budget__ten_year_budget__annual_fy_2026-02.csv'), \
        f'{item}: CBO source {source_path} needs a rule'
    data = pd.read_csv(os.path.join(RESOURCES, 'cbo_budget_fiscal.csv'), dtype={'date': str})
    values = data[(data.variable == series) & data.date.isin([f'FY{year - 1}', f'FY{year}'])].set_index('date')['value']
    assert len(values) == 2, f'{item}: {series} lacks unique observations for {year - 1} and {year}'
    a, b = float(values[f'FY{year - 1}']), float(values[f'FY{year}'])
    assert np.isfinite(a) and np.isfinite(b) and a > 0 and b > 0, f'{item}: {series} has a missing or nonpositive level ({a}, {b})'
    factor = (b / a) / cbo_return_growth(year)
    return factor, f'{series} fiscal-year proxy aggregate growth / CBO total return growth, {year - 1} to {year}'
