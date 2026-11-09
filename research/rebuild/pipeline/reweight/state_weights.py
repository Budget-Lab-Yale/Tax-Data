"""State weights: helpers for the integrated solve (state_joint.py), and two steps. --carry gives a projection year's
state weights from each record's 2023 shares; --check checks every year's state weights against its file.
See FEATURES.md, "State weights".
"""
import argparse
import json
import os

import numpy as np
import pandas as pd

from shared import common

from reweight import state_targets as ST

ROUNDING_SE = 10 / np.sqrt(12)     # Historic Table 2's counts are rounded to the nearest 10
BASE_YEAR = 2023                   # the projection years carry this year's shares


def say(*a):
    print(*a, flush=True)


def common_settings():
    """The run's settings "state_weights"."""
    return common.SETTINGS['state_weights']


def pins_of(df, year, areas):
    """For each record, the index of the area it is pinned to, or -1 if free. Only Forbes records are pinned."""
    pin = np.full(len(df), -1, dtype=int)
    f = ST.forbes_states(df, year)
    if len(f):
        where = {a: k for k, a in enumerate(areas)}
        for row, st in f.items():
            if st:
                assert st in where, f'{year}: list record in {st}'
                pin[row] = where[st]
        n_free = int((f == '').sum())
        if n_free:
            say(f'{year}: {n_free} list records with no state on any list and no residence abroad, not pinned: '
                + ', '.join(df['forbes_uri'].to_numpy(object)[f.index[f == '']].astype(str)[:10]))
    return pin


# ---- the starts ---------------------------------------------------------------------------------------------------------
def read_start(path, df):
    """state_start.py's probabilities [n, 51] in the file's record order."""
    z = np.load(path)
    idx = pd.Index(z['record_id']).get_indexer(df['record_id'].to_numpy())
    assert (idx >= 0).all(), f'{path}: {int((idx < 0).sum())} records of the file are not in the start'
    p = z['p'][idx].astype(float)
    assert np.isfinite(p).all() and (p >= 0).all()
    return p / p.sum(1, keepdims=True)


def cps_start(pi, group, ht2, areas):
    """Filers' starting shares: Other Areas and Puerto Rico at the IRS's share of returns in the record's AGI group, and
    the network's chances pi over the 51 scaled to the rest. Example: in 2019 in the $25-50k group the two hold 0.395% of
    returns, so a record with a 20.7% chance of California starts at 20.6% there."""
    v = ST.ht2_values(ht2, 'n_returns')[areas].to_numpy(float)       # [10, A]: each area's returns in each AGI group
    assert (v >= 0).all() and not np.isnan(v).any()
    s = v / v.sum(1, keepdims=True)
    p = np.zeros((len(group), len(areas)))
    other = [k for k, a in enumerate(areas) if a in ST.OTHER]
    rest = 1.0 - s[:, other].sum(1)
    p[:, :51] = pi * rest[group - 1][:, None]
    p[:, other] = s[group - 1][:, other]
    assert areas[:51] == ST.STATES
    return p


# ---- the tilt ---------------------------------------------------------------------------------------------------------
# A cell smaller than this share of the item's national total in the AGI group is held to the tolerance on this share of
# the total, not on its own size, so that a tiny cell the file cannot reach does not cost as much as a large state.
CELL_FLOOR = 0.001
LIST_FULL = 0.9     # a cell where the Forbes records hold this share of the target or more has no target


def tolerances(tg, tolerance):
    """A state target's standard error: the tolerance times the larger of the target and CELL_FLOOR times the item's
    national total in the group. For a count, the error from rounding Historic Table 2's counts to the nearest 10 is added
    in quadrature. Historic Table 2 has no sampling error, so the tolerance is a choice. Example, 2% tolerance: Connecticut's 2019 target of $12.21
    billion of wages in the $50-75k group has a standard error of $244 million."""
    t = tg['target'].abs().to_numpy()
    total = (tg['target'] / tg['share']).abs().to_numpy()
    se = tolerance * np.maximum(t, CELL_FLOOR * total)
    cnt = (tg['kind'] == 'count').to_numpy()
    scale = np.abs(tg['target'].to_numpy() / tg['published'].to_numpy())
    return np.where(cnt, np.sqrt(se ** 2 + (ROUNDING_SE * scale) ** 2), se)


def heldout_cells(f, group, P, ht2, areas):
    """The held-out items' scores by cell: one row per item, AGI group and area with a positive published value, where
    the target is the file's total times the area's share of the positive published values."""
    items = ST.HELD_OUT
    x = ST.item_vectors(f, items)
    w = f['weight'].to_numpy(float)
    rows = []
    for v in items:
        vals = ST.ht2_values(ht2, v)
        if vals is None:
            continue
        reach = None
        for g in range(1, 11):
            s = group == g
            if not (x[v][s] != 0).any():
                continue
            cells = vals.loc[g, areas]
            cells = cells[cells > 0]
            total = float((w[s] * x[v][s]).sum())
            if total <= 0 or not len(cells):
                continue
            sh = cells / cells.sum()
            a = [areas.index(c) for c in cells.index]
            reached = (w[s] * x[v][s]) @ P[s][:, a]
            rows.append(pd.DataFrame({'item': v, 'kind': ST.ITEMS[v][2], 'group': g, 'area': cells.index,
                                      'target': total * sh.to_numpy(), 'reached': reached}))
    d = pd.concat(rows, ignore_index=True)
    d['miss'] = (d['reached'] / d['target'] - 1).abs()
    return d


def score(d):
    """The average miss over all cells; the dollar-weighted miss, sum |reached - target| / sum target over the dollar
    items' cells; the same over the count items' cells; each in percent, overall and by item."""
    weighted = lambda c: 100 * float((c['reached'] - c['target']).abs().sum() / c['target'].abs().sum())
    out = {'cells': len(d), 'plain': 100 * float(d['miss'].mean()), 'dollar_weighted': weighted(d[d['kind'] == 'amount']),
           'return_weighted': weighted(d[d['kind'] == 'count'])}
    for v, c in d.groupby('item'):
        out[f'plain {v}'] = 100 * float(c['miss'].mean())
        out[f'weighted {v}'] = weighted(c)
    return out


def quality(f, P, areas):
    """Largest record shares and each area's effective number of records: (sum w q)^2 / sum (w q)^2."""
    w = f['weight'].to_numpy(float)
    wq = w[:, None] * P
    ess = wq.sum(0) ** 2 / (wq ** 2).sum(0)
    return {'smallest_state_effective_records': float(ess[:51].min()), 'state_with_smallest': areas[int(ess[:51].argmin())],
            'records_over_99pct_in_one_area': float((P.max(1) > 0.99).mean()), 'weight_over_99pct_in_one_area':
            float(w[P.max(1) > 0.99].sum() / w.sum())}


# ---- the year's file -----------------------------------------------------------------------------------------------------
def write_long(path, ids, weights, P, areas):
    """Write id, state and weight, one row per record and area with a share above zero. The weight is the record's
    national weight times its share."""
    import pyarrow as pa
    import pyarrow.parquet as pq
    n, A = P.shape
    r, c = np.nonzero(P > 0)
    t = pa.table({'id': pa.array(ids[r]), 'state': pa.DictionaryArray.from_arrays(pa.array(c, pa.int8()), pa.array(areas)),
                  'weight': pa.array(weights[r] * P[r, c])})
    pq.write_table(t, path, compression='zstd', row_group_size=2_000_000)
    return len(r)


def check_written(path, df, items_x):
    """Read the file back: every record's weights sum to its national weight, and every national total is unchanged."""
    d = pd.read_parquet(path, columns=['id', 'weight'])
    tot = d.groupby('id', sort=False)['weight'].sum()
    w = pd.Series(df['weight'].to_numpy(float), index=df['record_id'].to_numpy())
    assert set(tot.index) == set(w.index), f'{path}: the ids differ from the file'
    tot = tot.reindex(w.index)
    # weights too small to divide by are left out of the relative error
    rel = (tot - w).abs() / w.where(w > 1e-300, np.inf)
    out = {'records': len(w), 'rows': len(d), 'largest_share_sum_error': float(rel.max()),
           'bytes': os.path.getsize(path), 'national_weight': float(w.sum()), 'file_weight': float(d['weight'].sum())}
    worst = 0.0
    for v, x in items_x.items():
        a = float((w.to_numpy() * x).sum())
        b = float((tot.to_numpy() * x).sum())
        if a != 0:
            worst = max(worst, abs(b / a - 1))
    out['largest_item_total_error'] = worst
    return out


def carry(a):
    """A projection year's state weights: each record's national weight times its 2023 shares, found by record id. Forbes
    records, whose ids are their ranks on each year's list, are pinned by forbes_uri; one with no state takes the
    distribution of the year's pinned Forbes records. Any other record missing from the 2023 file stops the run."""
    year = a.year
    z = np.load(f'{a.out}/shares/shares_{BASE_YEAR}.npz')
    ids0, areas, P0 = z['record_id'], [str(x) for x in z['areas']], z['P']
    df = ST.read_file(f'{a.run}/{year}/tilt/y{year}/file_{year}.parquet')
    assert df['record_id'].is_unique and (df['weight'] >= 0).all()
    idx = pd.Index(ids0).get_indexer(df['record_id'].to_numpy())
    lst = df['billionaire'].to_numpy() == 1
    n_new = int(((idx < 0) & ~lst).sum())
    assert n_new == 0, f'{year}: {n_new} records neither on the list nor in the {BASE_YEAR} file'
    P = np.zeros((len(df), len(areas)))
    P[~lst] = P0[idx[~lst]]
    pin = pins_of(df, year, areas)
    pinned = lst & (pin >= 0)
    P[np.flatnonzero(pinned), pin[pinned]] = 1.0
    unpinned = lst & (pin < 0)
    if unpinned.any():
        d = np.bincount(pin[pinned], minlength=len(areas)) / pinned.sum()
        P[unpinned] = d
    os.makedirs(f'{a.out}/files', exist_ok=True)
    path = f'{a.out}/files/state_weights_{year}.parquet'
    assert not os.path.exists(path), f'{path} is already there'
    ids, wt = df['record_id'].to_numpy(), df['weight'].to_numpy(float)
    write_long(path, ids, wt, P, areas)
    items_x = ST.item_vectors(df, [v for v in ST.ITEMS])
    chk = check_written(path, df, items_x) | {'year': year, 'list_records': int(lst.sum()), 'list_pinned': int(pinned.sum()),
                                              'list_not_in_base': int((lst & (idx < 0)).sum()), 'newton_largest_group': 0,
                                              'newton_all_groups': 0, 'all_converged': True, 'carried_from': BASE_YEAR}
    say(json.dumps(chk))
    os.makedirs(f'{a.out}/checks', exist_ok=True)
    json.dump(chk, open(f'{a.out}/checks/check_{year}.json', 'w'), indent=1)


def check_all(a):
    """Check that every year has a file, that shares sum to 1 and national totals are unchanged, and that each past year's
    solve converged. A year within 10 times the stopping size is named in the log; a year beyond it stops the check.
    Writes checks/by_year.csv."""
    rows = []
    for y in [int(v) for v in a.years.split(',')]:
        f, c = f'{a.out}/files/state_weights_{y}.parquet', f'{a.out}/checks/check_{y}.json'
        r = dict(json.load(open(c)), file=os.path.exists(f)) if os.path.exists(c) else dict(year=y, file=os.path.exists(f))
        if r['file']:   # each file read back against the run's own weights
            d = pd.read_parquet(f, columns=['id', 'weight']).groupby('id', sort=False)['weight'].sum()
            w = pd.read_parquet(f'{a.run}/{y}/tilt/y{y}/file_{y}.parquet', columns=['record_id', 'weight']).set_index('record_id')['weight']
            assert d.index.nunique() == len(w) and set(d.index) == set(w.index), f'{y}: the ids differ'
            d = d.reindex(w.index)
            r['largest_share_sum_error'] = float(((d - w).abs() / w.where(w > 1e-300, np.inf)).max())
            r['weight_difference'] = float(d.sum() - w.sum())
            r['records_under_1e-6'] = int((w < 1e-6).sum())
        rows.append(r)
    d = pd.DataFrame(rows)
    d.to_csv(f'{a.out}/checks/by_year.csv', index=False)
    short = d[(d['all_converged'] == False) & d['within_near'].fillna(False).astype(bool)] if 'within_near' in d else d.iloc[:0]    # noqa: E712
    for _, r in short.iterrows():
        say(f"WARNING: {int(r['year'])} stopped short of the stopping size after {int(r['newton_largest_group'])} Newton steps, largest gradient {r['largest_gradient']:.2e}, "
            f"within 10 times the stopping size; the result is used")
    fin = d.drop(short.index)
    ok = {'every file there': bool(d['file'].all()), 'shares sum to 1 (largest error under 1e-9)': bool((d['largest_share_sum_error'] < 1e-9).all()),
          'national totals unchanged (largest error under 1e-9)': bool((d['largest_item_total_error'] < 1e-9).all()),
          'every past year converged within 150 Newton steps, or stopped short and is reported above': bool(fin['all_converged'].all() and (d['newton_largest_group'] <= 150).all())}
    say(json.dumps(ok, indent=1))
    say(f"largest share sum error {d['largest_share_sum_error'].max():.2e}; largest item total error {d['largest_item_total_error'].max():.2e}; "
        f"largest group's Newton steps {d['newton_largest_group'].max()}; largest year's, all groups {d['newton_all_groups'].max()}; bytes {d['bytes'].sum() / 1e9:.1f} GB in all")
    assert all(ok.values()), ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--run', required=True, help='the final folders: <run>/<year>/tilt/y<year>/file_<year>.parquet')
    ap.add_argument('--out', required=True, help='the output folder')
    ap.add_argument('--year', type=int, required=True)
    ap.add_argument('--carry', action='store_true', help='a projection year: the 2023 shares')
    ap.add_argument('--check', action='store_true', help='every year\'s file and check')
    ap.add_argument('--years', help='with --check: the years, comma-separated')
    a = ap.parse_args()
    assert a.check != a.carry, 'one of --carry and --check'
    return check_all(a) if a.check else carry(a)


if __name__ == '__main__':
    main()
