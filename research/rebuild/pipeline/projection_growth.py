"""Selected direct CBO and macro-series growth for projection inputs.

It handles explicitly named CBO aggregates and some macro continuations.
Observed-source and other composite rules are not implemented and must stop.
It is not yet connected to the projection tilt.
"""
import os

import numpy as np
import pandas as pd

from projection_demographics import cbo_count

RESOURCES = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'resources')


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
    equal to it): the product of the adjacent years' factors, SSA through 2025 and CBO's outlays from 2026. A year
    with no source stops the run."""
    f = 1.0
    for y in range(int(a) + 1, int(b) + 1):
        f *= cbo_per_return('ss_gross', y)[0]
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


def macro_per_return(item, year, return_growth):
    """Growth of an explicitly named macro series after CBO tax data end.

    return_growth must come from the preceding final file's demographic cells.
    Supports named series, sums of series, and GDP times the Treasury rate.
    """
    year = int(year)
    if not 2037 <= year <= 2055:
        raise ValueError(f'{item}: mapped macro continuation is not specified for {year}')
    if not np.isfinite(return_growth) or return_growth <= 0:
        raise ValueError(f'{item}: invalid demographic return growth {return_growth}')
    mapping = pd.read_csv(os.path.join(RESOURCES, 'projection_growth_mapping.csv'))
    rows = mapping[(mapping.old_item == item) & (mapping.source == 'The production Macro-Projections interface.')]
    if len(rows) != 1:
        raise ValueError(f'{item}: expected one macro continuation; found {len(rows)}')
    series = rows.iloc[0]['series']
    if not isinstance(series, str):
        raise ValueError(f'{item}: missing macro series')
    if series == 'gdp * tsy_10y':
        columns = ['gdp', 'tsy_10y']
        operation = 'product'
    elif ' + ' in series and all(c.replace('_', '').isalnum() for c in series.split(' + ')):
        columns = series.split(' + ')
        operation = 'sum'
    elif series.replace('_', '').isalnum():
        columns = [series]
        operation = 'product'
    else:
        raise ValueError(f'{item}: unsupported macro description {series!r}')
    path = rows.iloc[0]['path']
    data = pd.read_csv(path, usecols=['year'] + columns)
    obs = data[data.year.isin([year - 1, year])]
    if len(obs) != 2 or obs.year.nunique() != 2:
        raise ValueError(f'{item}: {series} lacks unique observations for {year - 1} and {year}')
    obs = obs.set_index('year')
    values = obs[columns].astype(float)
    if not np.isfinite(values.to_numpy()).all() or not (values.to_numpy() > 0).all():
        raise ValueError(f'{item}: {series} has missing or nonpositive observations')
    a = values.loc[year - 1].sum() if operation == 'sum' else values.loc[year - 1].prod()
    b = values.loc[year].sum() if operation == 'sum' else values.loc[year].prod()
    return float((b / a) / return_growth), f'{series} macro growth / demographic return growth, {year - 1} to {year}'


def _plan_one_year(items, year, return_growth=None, strict=True):
    """Resolve all requested names or report every missing/ambiguous rule before growth.

    This preflight currently supports only direct CBO aggregates and the mapped
    macro continuation. It does not apply factors to records.
    """
    items = list(items)
    if len(items) != len(set(items)):
        raise ValueError('projection growth input columns contain duplicates')
    mapping = pd.read_csv(os.path.join(RESOURCES, 'projection_growth_mapping.csv'))
    planned, problems = [], []
    for item in items:
        names = mapping.loc[(mapping.renamed_item == item) | (mapping.old_item == item), 'old_item'].dropna().unique()
        if len(names) != 1:
            problems.append(f'{item}: {len(names)} mapping keys {list(names)}')
            continue
        key = names[0]
        try:
            if year <= 2036:
                sources = set(mapping.loc[mapping.old_item == key, 'source'])
                if key.startswith('c_'):
                    factor, source = consumption_per_return(key, year)
                elif 'Federal Reserve Z.1 and CBO' in sources:
                    factor, source = financial_accounts_per_return(key, year)
                elif key in ('elective_deferrals', 'tips_primary', 'tips_spouse'):
                    factor, source = cbo_per_return('wages', year)
                    source = f'{key}: CBO wages proxy after the 2023 W-2 observation; {source}'
                else:
                    factor, source = cbo_per_return(key, year)
            else:
                factor, source = macro_per_return(key, year, return_growth)
            planned.append({'column': item, 'mapping_key': key, 'factor': factor, 'source': source})
        except (ValueError, KeyError, FileNotFoundError) as exc:
            problems.append(f'{item}: {exc}')
    if strict and problems:
        raise ValueError(f'projection growth has {len(problems)} unresolved item(s):\n  ' + '\n  '.join(problems))
    # not strict: the resolved items, and the list of unresolved ones (item, reason) for the caller to deal with openly
    return pd.DataFrame(planned) if strict else (pd.DataFrame(planned, columns=['column', 'mapping_key', 'factor', 'source']), problems)


def plan_factors(items, year, return_growth=None, strict=True, from_year=None):
    """Growth factors per return from from_year to year (default: the year before). Since 1 November (decision 74) the
    projection years start from the 2023 file: each year's factor from CBO's and the macro series is the product of the
    adjacent-year factors, each already net of CBO's growth in the number of returns that year, so the compounded factor is
    the series' growth from from_year to year over CBO's growth in returns over the same years. An item with no rule in
    any one of the years is unresolved (listed in the problems)."""
    s = year - 1 if from_year is None else int(from_year)
    if s == year - 1:
        return _plan_one_year(items, year, return_growth, strict)
    items = list(items)
    total, problems, sources = None, {}, {}
    for y in range(s + 1, year + 1):
        got, probs = _plan_one_year(items, y, return_growth, strict=False)
        for q in probs:
            problems.setdefault(q.split(': ', 1)[0], q)
        g = got.set_index('column')
        if total is None:
            total = g[['mapping_key', 'factor']].copy()
        else:   # this year's factor times the product so far, for the items resolved in every year so far
            total = total.join(g[['factor']], how='inner', rsuffix='_y')
            total['factor'] = total['factor'] * total['factor_y']
            total = total.drop(columns='factor_y')
        sources = g['source'].to_dict()
    keep = total.drop(index=[k for k in problems if k in total.index])
    planned = pd.DataFrame({'column': keep.index, 'mapping_key': keep['mapping_key'].to_numpy(), 'factor': keep['factor'].to_numpy(),
                            'source': [f'{sources[c]} (compounded year by year, {s} to {year}; last year shown)' for c in keep.index]})
    if strict and problems:
        raise ValueError(f'projection growth has {len(problems)} unresolved item(s):\n  ' + '\n  '.join(problems.values()))
    return planned if strict else (planned, list(problems.values()))


def consumption_per_return(item, year):
    """NIPA category growth through 2025, then CBO total PCE growth through 2036."""
    year = int(year)
    permitted = {'c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables',
                 'c_food_off_premises', 'c_gasoline', 'c_housing_utilities', 'c_other_services_health'}
    if item not in permitted or not 2024 <= year <= 2036:
        raise ValueError(f'{item}: no supported consumption growth for {year}')
    if year <= 2025:
        data = pd.read_csv(os.path.join(RESOURCES, 'growth', 'nipa_pce_categories.csv'))
        obs = data[data.year.isin([year - 1, year])].set_index('year')[item]
        source = f'NIPA PCE {item}'
    else:
        data = pd.read_csv(os.path.join(RESOURCES, 'cbo_economic.csv'), dtype={'date': str})
        selected = data[(data.variable == 'pce') & data.date.isin([str(year - 1), str(year)])]
        obs = selected.assign(year=selected.date.astype(int)).set_index('year')['value']
        source = 'CBO total nominal PCE proxy'
    if len(obs) != 2 or not obs.index.is_unique:
        raise ValueError(f'{item}: {source} lacks unique levels for {year - 1} and {year}')
    a, b = float(obs.loc[year - 1]), float(obs.loc[year])
    if not np.isfinite(a) or not np.isfinite(b) or a <= 0 or b <= 0:
        raise ValueError(f'{item}: {source} has invalid levels ({a}, {b})')
    return (b / a) / cbo_return_growth(year), f'{source} / CBO total return growth, {year - 1} to {year}'


def financial_accounts_per_return(item, year):
    """Grow explicitly identified Z.1 stock series, then use CBO GDP growth."""
    import re
    import zipfile
    import common
    year = int(year)
    if not 2024 <= year <= 2036:
        raise ValueError(f'{item}: no supported Financial Accounts growth for {year}')
    mapping = pd.read_csv(os.path.join(RESOURCES, 'projection_growth_mapping.csv'))
    row = mapping[(mapping.old_item == item) & (mapping.source == 'Federal Reserve Z.1 and CBO')]
    if len(row) != 1:
        raise ValueError(f'{item}: expected one Financial Accounts mapping, found {len(row)}')
    description = row.iloc[0]['series']
    grammar = r'(?:FL|LM)\d+\.Q(?: (?:\+|minus) (?:FL|LM)\d+\.Q)*'
    match = re.fullmatch(f'({grammar}) through 2025; then CBO gdp', description)
    if not match:
        raise ValueError(f'{item}: Financial Accounts description requires an explicit rule: {description}')
    expr = match.group(1)
    parts = re.split(r' (\+|minus) ', expr)
    coefficients = [(parts[0], 1)] + [(parts[i + 1], 1 if parts[i] == '+' else -1)
                                       for i in range(1, len(parts), 2)]
    if year <= 2025:
        levels = {year - 1: 0., year: 0.}
        with zipfile.ZipFile(common.RAW['z1']) as archive:
            frames = [pd.read_csv(archive.open(name)) for name in ('csv/S1M_b.csv', 'csv/F4_3_s.csv')]
        for series, sign in coefficients:
            candidates = [df.set_index('date')[series] for df in frames if series in df.columns]
            if not candidates:
                raise ValueError(f'{item}: Z.1 series {series} is absent')
            for y in levels:
                date = f'{y}:Q4'
                vals = [float(s.loc[date]) for s in candidates if date in s.index]
                if not vals or not np.isfinite(vals).all() or not np.allclose(vals, vals[0]):
                    raise ValueError(f'{item}: missing or inconsistent {series} at {date}: {vals}')
                levels[y] += sign * vals[0]
        a, b = levels[year - 1], levels[year]
        source = f'Financial Accounts {expr}, fourth quarter'
    else:
        data = pd.read_csv(os.path.join(RESOURCES, 'cbo_economic.csv'), dtype={'date': str})
        obs = data[(data.variable == 'gdp') & data.date.isin([str(year - 1), str(year)])]
        if len(obs) != 2 or obs.date.nunique() != 2:
            raise ValueError(f'{item}: CBO GDP is missing for {year - 1} or {year}')
        values = obs.set_index('date')['value']
        a, b = float(values[str(year - 1)]), float(values[str(year)])
        source = 'CBO GDP proxy after the 2025 Financial Accounts observation'
    if not np.isfinite(a) or not np.isfinite(b) or a <= 0 or b <= 0:
        raise ValueError(f'{item}: {source} has missing or nonpositive levels ({a}, {b})')
    return (b / a) / cbo_return_growth(year), f'{source} / CBO total return growth, {year - 1} to {year}'
