"""Population-cell growth for chained projection years.

Factors are ratios to 2023, so annual growth is the ratio of two adjacent
levels. Filer cells use joint status; nonfilers use the same age growth for
both joint statuses. No missing or unsupported cell receives a default.
"""
import os

import numpy as np
import pandas as pd

import common

RESOURCES = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'resources')


def cbo_count(year, series):
    """Published CBO count in returns; never extrapolate a missing year."""
    allowed = {'rev_iit_returns_total', 'rev_iit_returns_itemized', 'rev_iit_returns_amt'}
    if series not in allowed:
        raise ValueError(f'unknown CBO return-count series {series!r}')
    counts = pd.read_csv(os.path.join(RESOURCES, 'cbo_counts.csv'))
    selected = counts[(counts.year == int(year)) & (counts.series == series)]
    if len(selected) != 1 or not np.isfinite(selected.count_millions.iloc[0]) or selected.count_millions.iloc[0] <= 0:
        raise ValueError(f'CBO {series}: no unique positive count for {year}')
    return float(selected.count_millions.iloc[0]) * 1_000_000


def population_cells(previous, year, from_year=None):
    """Return scaled weights and independently constructed age/status targets. The previous file is the file of from_year
    (default: the year before; since 1 November, decision 74, every projection year starts from the 2023 file and the
    growth is the population's from from_year to year)."""
    year = int(year)
    base = year - 1 if from_year is None else int(from_year)
    required = ['record_id', 'weight', 'filer', 'married', 'filing_status', 'age_range', 'age1']
    missing_columns = [c for c in required if c not in previous]
    if missing_columns:
        raise ValueError(f'previous file lacks columns: {missing_columns}')
    d = previous[required].copy()
    if d.record_id.isna().any() or not d.record_id.is_unique:
        raise ValueError('previous file has missing or duplicate record IDs')
    w = d.weight.to_numpy(float)
    # a tilted file keeps every record, a weight of exactly zero for those the reweighting dropped; those stay at zero
    if not np.all(np.isfinite(w) & (w >= 0)):
        raise ValueError(f'previous file has {int((~(np.isfinite(w) & (w >= 0))).sum())} negative or missing weights')
    if not (w > 0).any():
        raise ValueError('previous file has no positive weight')
    valid = (d.filer.isin([0, 1]) & d.married.isin([0, 1]) & d.filing_status.isin([1, 2, 3, 4]) &
             (d.married == (d.filing_status == 2).astype(int)) & d.age_range.isin(range(9)))
    age = d.age1
    # the primary's age band: under 26 holds every primary below 26 (the file has 151 aged 15 to 17); a dependent filer's
    # primary age is not kept (age1 is -1), its band is the age_range itself, so no age check for bands 6 to 8
    bounds = [(0, 26), (26, 35), (35, 45), (45, 55), (55, 65), (65, 101), (-1, 0), (-1, 0), (-1, 0)]
    age_ok = pd.Series(False, index=d.index)
    for i, (lo, hi) in enumerate(bounds):
        age_ok |= (d.age_range == i) & (((age >= lo) & (age < hi)) if i < 6 else True)
    valid &= age_ok & ((d.filer == 1) | (d.age_range < 6))
    if not valid.all():
        raise ValueError(f'previous file has {int((~valid).sum())} records with missing or inconsistent filer, status, age or age band')
    d['age_band'] = d.age_range.map(dict(enumerate(common.AGE_LABELS)))
    filer = pd.read_csv(os.path.join(RESOURCES, 'population_factors.csv'))
    nonfiler = pd.read_csv(os.path.join(RESOURCES, 'nonfiler_factors.csv'))
    for table, keys, label in [(filer, ['year', 'married', 'age_band'], 'filers'),
                               (nonfiler, ['year', 'age_band'], 'nonfilers')]:
        if table.duplicated(keys).any() or not np.all(np.isfinite(table.factor) & (table.factor > 0)):
            raise ValueError(f'{label}: duplicate cells or invalid population factors')
    def ratios(table, keys):
        a = table[table.year == base].set_index(keys).factor
        b = table[table.year == year].set_index(keys).factor
        if a.empty or b.empty or not a.index.equals(b.index):
            raise ValueError(f'population factors do not cover the same cells in {base} and {year}')
        return b / a
    fr = ratios(filer, ['married', 'age_band'])
    nr = ratios(nonfiler, ['age_band'])
    d['factor'] = np.nan
    mf = d.filer == 1
    d.loc[mf, 'factor'] = pd.MultiIndex.from_frame(d.loc[mf, ['married', 'age_band']]).map(fr).to_numpy()
    d.loc[~mf, 'factor'] = d.loc[~mf, 'age_band'].map(nr)
    if not np.isfinite(d.factor).all():
        raise ValueError(f'{int(d.factor.isna().sum())} records have no population factor')
    # These targets use the unscaled file, separately from multiplication of each record's weight.
    targets = d.groupby(['filer', 'age_range', 'married'], observed=True).agg(
        previous_weight=('weight', 'sum'), factor=('factor', 'first'), factor_count=('factor', 'nunique')).reset_index()
    if (targets.factor_count != 1).any():
        raise ValueError('a demographic cell has more than one population factor')
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
