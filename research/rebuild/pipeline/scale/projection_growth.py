"""Growth of CBO and Macro-Projections series for the projection years: CBO's count of returns, gross Social Security
per return, and the long-run continuation of the series past CBO's last year."""
import os

import numpy as np
import pandas as pd

from shared import common

from scale.projection_demographics import cbo_count

RESOURCES = common.RESOURCES


def cbo_return_growth(year):
    """CBO's growth in the total number of individual income tax returns from year - 1 to year."""
    return cbo_count(year, 'rev_iit_returns_total') / cbo_count(year - 1, 'rev_iit_returns_total')


def ssa_per_return(item, year):
    """Gross Social Security benefits, 2024 and 2025 (John, 2 November 2026): SSA's calendar-year OASDI benefits paid
    (resources/ssa_oasdi_benefits.csv: the Annual Statistical Supplement Table 4.A4 and the Trustees Report Table III.A5)
    over CBO's growth in the number of returns, year - 1 to year. CBO's fiscal-year outlays, the rule from 2026,
    cannot be used here: the repository's CBO file starts at FY2025."""
    year = int(year)
    d = pd.read_csv(os.path.join(RESOURCES, 'ssa_oasdi_benefits.csv')).set_index('year')['oasdi_benefits_paid_millions']
    if year not in d.index or year - 1 not in d.index:
        raise ValueError(f'{item}: no SSA OASDI benefits paid for {year - 1} and {year} in ssa_oasdi_benefits.csv')
    a, b = float(d.loc[year - 1]), float(d.loc[year])
    factor = (b / a) / cbo_return_growth(year)
    return factor, f'SSA OASDI benefits paid in the calendar year / CBO total return growth, {year - 1} to {year}'


def gross_ss_between(a, b):
    """Growth in gross Social Security benefits per return from year a to year b (a < b, both after SOI's last year or
    equal to it): the product of the adjacent years' factors, SSA through 2025 and CBO's outlays from 2026 to CBO's last
    year used (settings tilt.projection.cbo_last_year); after it (4 November 2026, the long-run years) Macro-Projections'
    OASDI outlays per return (long_run_growth). A year with no source stops the run."""
    L = cbo_last_year()
    f = 1.0
    for y in range(int(a) + 1, min(int(b), L) + 1):
        f *= cbo_per_return('ss_gross', y)[0]
    if int(b) > L:
        f *= long_run_growth('outlays_mand_oasdi', max(int(a), L), int(b), per='units')
    return f


# ---- the long-run years (4 November 2026, branch rebuild-longrun) -------------------------------------------------
# Past CBO's last projection year used (settings tilt.projection.cbo_last_year, 2035) every series continues at the
# growth of one Macro-Projections aggregate (production's long-run mapping, src/project_puf.R income_far). The
# aggregates are written once, with the counts they are divided by, to resources/growth/macro_long_run.csv
# (resources/growth/build_long_run.py). The items table's series files carry their own continuation (the same
# aggregates, resources/projection_long_run_mapping.csv); the functions here continue the series the growth.yaml rules
# read (targets.cbo_growth, growth.income_per_person_growth, gross_ss_between) and the count of returns
# (returns_growth, for the Forbes list grown past the last list).
_LR = {}


def cbo_last_year():
    """The last year a CBO projection is used (settings tilt.projection.cbo_last_year)."""
    from shared import common
    return int(common.SETTINGS['tilt']['projection']['cbo_last_year'])


def long_run_level(name, year):
    """The level of a long-run aggregate in year: a column of resources/growth/macro_long_run.csv, or a sum 'a + b'.
    Columns: the Macro-Projections aggregates, 'units' (people aged 18 and over, a married couple counted once) and
    'population' (every age)."""
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
    """Growth of the long-run aggregate name from year a to year b; with per ('units' or 'population'), divided by the
    growth of that count over the same years (a growth per return or per person)."""
    g = long_run_level(name, b) / long_run_level(name, a)
    if per is not None:
        g /= long_run_level(per, b) / long_run_level(per, a)
    return g


def returns_growth(a, b):
    """Growth in the number of returns from year a to year b (a <= b): CBO's count through cbo_last_year, then the
    growth of adults with a couple counted once ('units')."""
    a, b, L = int(a), int(b), cbo_last_year()
    f = 1.0
    for y in range(a + 1, min(b, L) + 1):
        f *= cbo_return_growth(y)
    if b > L:
        f *= long_run_growth('units', max(a, L), b)
    return f


def cbo_per_return(item, year):
    """Return the adjacent-year factor and its source for an explicit CBO mapping."""
    year = int(year)
    if item == 'ss_gross' and year <= 2025:
        return ssa_per_return(item, year)
    if not 2024 <= year <= 2036:
        raise ValueError(f'{item}: CBO-series growth is unavailable for {year - 1} to {year}')
    mapping = pd.read_csv(os.path.join(RESOURCES, 'projection_growth_mapping.csv'))
    rows = mapping[(mapping.old_item == item) & (mapping.source == 'CBO February 2026')]
    if len(rows) != 1:
        raise ValueError(f'{item}: expected one explicit CBO mapping; found {len(rows)}')
    series = rows.iloc[0]['series']
    if not isinstance(series, str) or not series.replace('_', '').isalnum():
        raise ValueError(f'{item}: CBO mapping is composite or unspecified: {series!r}')
    source_path = rows.iloc[0]['path']
    if source_path.endswith('revenue_detail__annual_cy_iit_2026-02.csv') and series.startswith('rev_iit_'):
        filename, prefix = 'cbo_revenue_detail.csv', 'CY'
    elif source_path.endswith('revenue_detail__annual_cy_2026-02.csv') and series == 'rev_capgains_realizations':
        filename, prefix = 'cbo_revenue_other.csv', 'CY'
    elif source_path.endswith('economic__economic_projections__calendar_2026-02.csv') and series in ('rental_inc_adj', 'interest_inc_pers'):
        filename, prefix = 'cbo_economic.csv', ''
    elif source_path.endswith('budget__ten_year_budget__annual_fy_2026-02.csv') and series in (
            'proj_mand_premium_tax_credits_timing_adj', 'proj_mand_social_security_timing_adj',
            'proj_mand_unemployment_timing_adj'):
        # The mapping explicitly uses fiscal-year levels as a proxy for calendar-year inputs.
        filename, prefix = 'cbo_budget_fiscal.csv', 'FY'
    else:
        raise ValueError(f'{item}: CBO source or aggregate units need a rule: {source_path}, {series}')
    data = pd.read_csv(os.path.join(RESOURCES, filename), dtype={'date': str})
    observations = data[(data.variable == series) & data.date.isin([f'{prefix}{year - 1}', f'{prefix}{year}'])]
    if len(observations) != 2 or observations.date.nunique() != 2:
        raise ValueError(f'{item}: {series} lacks unique observations for {year - 1} and {year}')
    values = observations.set_index('date')['value']
    a, b = float(values[f'{prefix}{year - 1}']), float(values[f'{prefix}{year}'])
    if not np.isfinite(a) or not np.isfinite(b) or a <= 0 or b <= 0:
        raise ValueError(f'{item}: {series} has a missing or nonpositive level ({a}, {b})')
    factor = (b / a) / cbo_return_growth(year)
    return factor, f'{series} {"fiscal-year proxy" if prefix == "FY" else "calendar-year"} aggregate growth / CBO total return growth, {year - 1} to {year}'
