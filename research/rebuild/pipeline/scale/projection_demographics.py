"""Population growth by demographic cell, used to grow the Forbes list's records into a projection year.

Each record's weight is multiplied by the population growth of its cell. A filer's cell is its marital status and the
primary filer's age band; the factors are in resources/population_factors.csv. A non-filer's cell is its age band; the
factors are in resources/nonfiler_factors.csv. Each factor is a population level relative to 2023. The growth from
one year to another is the ratio of the two years' factors. A record whose cell has no factor stops the run; no cell
gets a default. The module also reads CBO's published counts of returns. FEATURES.md, "4. Scaling to the target year",
"Projection years", describes the projection.
"""
import os

import numpy as np
import pandas as pd

from shared import common

RESOURCES = common.RESOURCES


def cbo_count(year, series):
    """A count of returns published by CBO (the total, itemizers or AMT payers) for one year. A year with no count
    stops the run."""
    allowed = {'rev_iit_returns_total', 'rev_iit_returns_itemized', 'rev_iit_returns_amt'}
    if series not in allowed:
        raise ValueError(f'unknown CBO return-count series {series!r}')
    counts = pd.read_csv(os.path.join(RESOURCES, 'cbo_counts.csv'))
    selected = counts[(counts.year == int(year)) & (counts.series == series)]
    if len(selected) != 1 or not np.isfinite(selected.count_millions.iloc[0]) or selected.count_millions.iloc[0] <= 0:
        raise ValueError(f'CBO {series}: no unique positive count for {year}')
    return float(selected.count_millions.iloc[0]) * 1_000_000


def _ratios(table, keys, base, year):
    """Each cell's population factor in year divided by its factor in base."""
    a = table[table.year == base].set_index(keys).factor
    b = table[table.year == year].set_index(keys).factor
    assert not a.empty and not b.empty and a.index.equals(b.index), \
        f'population factors do not cover the same cells in {base} and {year}'
    return b / a


def population_cells(previous, year, from_year):
    """Grows the weights of previous, the file of from_year, to year. Returns each record's weight times its cell's
    population growth, and a table with one row per cell (filer, age_range, married): the cell's weight in previous,
    its growth factor, and its target (weight times factor)."""
    # 1. Check that every record has a valid filer flag, marital status, filing status and age band.
    year, base = int(year), int(from_year)
    required = ['record_id', 'weight', 'filer', 'married', 'filing_status', 'age_range', 'age1']
    missing_columns = [c for c in required if c not in previous]
    assert not missing_columns, f'previous file lacks columns: {missing_columns}'
    d = previous[required].copy()
    assert not d.record_id.isna().any() and d.record_id.is_unique, 'previous file has missing or duplicate record IDs'
    w = d.weight.to_numpy(float)
    # A reweighted file keeps every record. A record with weight zero keeps weight zero.
    assert np.all(np.isfinite(w) & (w >= 0)), f'previous file has {int((~(np.isfinite(w) & (w >= 0))).sum())} negative or missing weights'
    assert (w > 0).any(), 'previous file has no positive weight'
    valid = (d.filer.isin([0, 1]) & d.married.isin([0, 1]) & d.filing_status.isin([1, 2, 3, 4]) &
             (d.married == (d.filing_status == 2).astype(int)) & d.age_range.isin(range(9)))
    age = d.age1
    # The age bands of the primary filer. The band "under 26" holds every primary filer below 26, a few of them under
    # 18. Bands 6 to 8 are the dependent filers' ranges. A dependent filer's own age is not kept (age1 is -1), and
    # those bands have no age check.
    bounds = [(0, 26), (26, 35), (35, 45), (45, 55), (55, 65), (65, 101), (-1, 0), (-1, 0), (-1, 0)]
    age_ok = pd.Series(False, index=d.index)
    for i, (lo, hi) in enumerate(bounds):
        age_ok |= (d.age_range == i) & (((age >= lo) & (age < hi)) if i < 6 else True)
    valid &= age_ok & ((d.filer == 1) | (d.age_range < 6))
    assert valid.all(), f'previous file has {int((~valid).sum())} records with missing or inconsistent filer, status, age or age band'
    d['age_band'] = d.age_range.map(dict(enumerate(common.AGE_LABELS)))

    # 2. Read the population factors and compute each cell's growth from from_year to year.
    filer = pd.read_csv(os.path.join(RESOURCES, 'population_factors.csv'))
    nonfiler = pd.read_csv(os.path.join(RESOURCES, 'nonfiler_factors.csv'))
    for table, keys, label in [(filer, ['year', 'married', 'age_band'], 'filers'),
                               (nonfiler, ['year', 'age_band'], 'nonfilers')]:
        assert not table.duplicated(keys).any() and np.all(np.isfinite(table.factor) & (table.factor > 0)), \
            f'{label}: duplicate cells or invalid population factors'
    fr = _ratios(filer, ['married', 'age_band'], base, year)
    nr = _ratios(nonfiler, ['age_band'], base, year)

    # 3. Give each record its cell's growth factor.
    d['factor'] = np.nan
    mf = d.filer == 1
    d.loc[mf, 'factor'] = pd.MultiIndex.from_frame(d.loc[mf, ['married', 'age_band']]).map(fr).to_numpy()
    d.loc[~mf, 'factor'] = d.loc[~mf, 'age_band'].map(nr)
    assert np.isfinite(d.factor).all(), f'{int(d.factor.isna().sum())} records have no population factor'

    # 4. The table of cells. It lists every cell, including cells with no records, which get weight zero and their
    #    factor from the factor tables.
    targets = d.groupby(['filer', 'age_range', 'married'], observed=True).agg(
        previous_weight=('weight', 'sum'), factor=('factor', 'first'), factor_count=('factor', 'nunique')).reset_index()
    assert (targets.factor_count == 1).all(), 'a demographic cell has more than one population factor'
    all_cells = pd.MultiIndex.from_tuples(
        [(f, age, married) for f, ages in [(1, range(9)), (0, range(6))]
         for age in ages for married in (0, 1)], names=['filer', 'age_range', 'married'])
    targets = targets.set_index(['filer', 'age_range', 'married']).reindex(all_cells).reset_index()
    targets['previous_weight'] = targets.previous_weight.fillna(0.0)
    for i in targets.index[targets.factor.isna()]:
        row = targets.loc[i]
        band = common.AGE_LABELS[int(row.age_range)]
        targets.loc[i, 'factor'] = fr.loc[(int(row.married), band)] if row.filer else nr.loc[band]
    targets['target'] = targets.previous_weight * targets.factor
    scaled = d.weight.to_numpy(float) * d.factor.to_numpy(float)
    return scaled, targets.drop(columns='factor_count')
