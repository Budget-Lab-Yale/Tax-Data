"""State weights, the CPU stage (FEATURES.md, "State weights"). For one year of a finished run: the starting shares, the tilt to
the targets (state_targets.py, state_tilt.py), the scores, and with --write the year's file of id, state, weight.

Run through run_state_weights.sh (one command for a whole run), or for one setting:
  python state_weights.py --run <run folder> --out <folder> --year 2019 --start cps --items comparison --tolerance 0.02 --tau 1000 --tag x

The starts.
  iselin  every record in an AGI group has the IRS's share of the group's returns in each area (Iselin's prior).
  cps     the network's probabilities (state_start.py) scaled to 1 - the IRS's share of Other Areas and Puerto Rico in the
          record's AGI group, those two areas at the IRS's share. Worked example: in 2019 in the group $25-50k, if Other Areas
          and Puerto Rico hold 0.8% of returns, a record with a 12% chance of California under the network starts at
          0.12 x 0.992 = 11.9% in California. Non-filers have only the 51: the CPS and Census have no other area.
Records on the Forbes list are pinned to their state in every start and held out of the shares (forbes_states).

The scores of the held-out items (validate_state_weights.R's rule, for any set of shares). For item v, AGI group g, areas with a
positive published value: target = the file's total of v in g x the area's share of the positive published values, reached =
sum of w x share x v; the miss is |reached / target - 1|. The plain score is the average miss over all cells. The dollar-weighted
miss is sum |reached - target| / sum target over the cells of the dollar items, pooled; the same over the count items is the
return-weighted miss.
"""
import argparse
import json
import os
import time

import numpy as np
import pandas as pd

import state_targets as ST
import state_tilt as TL

ROUNDING_SE = 10 / np.sqrt(12)     # Historic Table 2's counts are rounded to the nearest 10


def say(*a):
    print(*a, flush=True)


# ---- the file ---------------------------------------------------------------------------------------------------------
def load_year(run, year):
    """The year's records (state_targets.read_file), filers and non-filers, Historic Table 2, the areas and the list records."""
    path = f'{run}/{year}/tilt/y{year}/file_{year}.parquet'
    df = ST.read_file(path)
    assert df['record_id'].is_unique and (df['weight'] >= 0).all(), f'{path}: ids or weights'
    n0 = int((df['weight'] == 0).sum())
    if n0:   # the reweighting can take a record to weight zero: it keeps its starting shares, which multiply nothing
        say(f'{year}: {n0} records with national weight zero')
    ht2 = ST.read_ht2(year) if year <= 2023 else None
    return df, ht2


def pins_of(df, year, areas):
    """Per record the area index it is pinned to, -1 if free."""
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
def irs_return_shares(ht2, areas):
    """[10, A]: each area's share of the returns in each AGI group."""
    v = ST.ht2_values(ht2, 'n_returns')[areas].to_numpy(float)
    assert (v >= 0).all() and not np.isnan(v).any()
    return v / v.sum(1, keepdims=True)


def iselin_start(group, ht2, areas):
    return irs_return_shares(ht2, areas)[group - 1]


def read_start(path, df):
    """state_start.py's probabilities [n, 51] in the file's record order."""
    z = np.load(path)
    idx = pd.Index(z['record_id']).get_indexer(df['record_id'].to_numpy())
    assert (idx >= 0).all(), f'{path}: {int((idx < 0).sum())} records of the file are not in the start'
    p = z['p'][idx].astype(float)
    assert np.isfinite(p).all() and (p >= 0).all()
    return p / p.sum(1, keepdims=True)


def cps_start(pi, group, ht2, areas):
    """Filers: pi over the 51 x (1 - the IRS's share of the other areas in the group), the other areas at the IRS's share."""
    s = irs_return_shares(ht2, areas)
    p = np.zeros((len(group), len(areas)))
    other = [k for k, a in enumerate(areas) if a in ST.OTHER]
    rest = 1.0 - s[:, other].sum(1)
    p[:, :51] = pi * rest[group - 1][:, None]
    p[:, other] = s[group - 1][:, other]
    assert areas[:51] == ST.STATES
    return p


# ---- the tilt ---------------------------------------------------------------------------------------------------------
def tolerances(tg, tolerance, ht2_ratio=None):
    """A target's standard error: the relative tolerance x |target|, with the rounding of Historic Table 2's counts added in
    quadrature for a count item (10 / sqrt(12) returns, on the file's scale). Historic Table 2 has no sampling error."""
    se = tolerance * tg['target'].abs().to_numpy()
    cnt = (tg['kind'] == 'count').to_numpy()
    scale = np.abs(tg['target'].to_numpy() / tg['published'].to_numpy())
    return np.where(cnt, np.sqrt(se ** 2 + (ROUNDING_SE * scale) ** 2), se)


def tilt_filers(f, p0, group, ht2, areas, items, tolerance, tau, pin, budget=150, year=None, log=say, no_tilt=False):
    """Every AGI group's tilt. f: the filers; p0 [n, A] the start; pin: the filers' area index or -1. Returns the shares [n, A]
    (pinned records 1 in their area) and the fit table, one row per target: start, final, miss in standard errors."""
    tg, notes = ST.filer_targets(f, group, ht2, items, year)
    tg['se'] = tolerances(tg, tolerance)
    x = ST.item_vectors(f, items)
    w = f['weight'].to_numpy(float)
    P = p0.copy()
    free = pin < 0
    P[~free] = 0.0
    P[np.flatnonzero(~free), pin[~free]] = 1.0
    apos, ipos = {a: k for k, a in enumerate(areas)}, {v: k for k, v in enumerate(items)}
    X = np.column_stack([x[v] for v in items])
    tg['a'], tg['k'] = tg['area'].map(apos), tg['item'].map(ipos)
    infos, fits = [], []
    for g in range(1, 11):
        rows = np.flatnonzero(group == g)
        fr = rows[free[rows] & (w[rows] > 0)]
        pn = rows[~free[rows]]
        c = tg[tg['group'] == g]
        T = np.full((len(areas), len(items)), np.nan)
        se = np.full_like(T, np.nan)
        T[c['a'].to_numpy(), c['k'].to_numpy()] = c['target'].to_numpy()
        se[c['a'].to_numpy(), c['k'].to_numpy()] = c['se'].to_numpy()
        fixed = np.zeros_like(T)
        if len(pn):
            np.add.at(fixed, pin[pn], w[pn, None] * X[pn])
        start_reached = ((w[fr, None] * X[fr]).T @ p0[fr]).T + fixed
        t0 = time.time()
        if no_tilt:
            q, info = p0[fr], dict(steps=0, status='no tilt', kl=0.0, targets=len(c), records=len(fr), reached=start_reached,
                                   largest_gradient=np.nan, dropped_no_holder=0)
        else:
            q, info = TL.solve(w[fr], p0[fr], X[fr], T, se, fixed=fixed, tau=tau, budget=budget)
        P[fr] = q
        log(f'{year} group {g} ({ST.GROUP_NAMES[g - 1]}): {len(fr):,} free records, {len(pn)} pinned, {info["targets"]} targets; '
            f'{info["steps"]} Newton steps, {info["status"]}, largest gradient {info["largest_gradient"]:.2e}, move {info["kl"]:.4f} nats, '
            f'{time.time() - t0:.0f} s')
        infos.append({k: v for k, v in info.items() if k not in ('miss', 'reached')} | {'group': g, 'pinned': len(pn)})
        c = c.copy()
        c['start'] = start_reached[c['a'].to_numpy(), c['k'].to_numpy()]
        c['final'] = info['reached'][c['a'].to_numpy(), c['k'].to_numpy()]
        fits.append(c)
    fit = pd.concat(fits, ignore_index=True).drop(columns=['a', 'k'])
    fit['miss_se'] = (fit['final'] - fit['target']) / fit['se']
    fit['miss_rel'] = (fit['final'] / fit['target'] - 1).abs()
    fit['start_rel'] = (fit['start'] / fit['target'] - 1).abs()
    return P, fit, pd.DataFrame(infos), notes


def tilt_nonfilers(nf, pi, ht2, year, tolerance, tau, budget=150, no_tilt=False):
    """The non-filers over the 51 states. Returns shares [n, 51], fit, info, the state table."""
    tg, table, X = ST.nonfiler_targets(nf, ht2, year)
    T = tg.pivot(index='state', columns='band', values='target').loc[ST.STATES, ST.AGE_BAND_NAMES].to_numpy(float)
    se = tolerance * np.abs(T)
    w = nf['weight'].to_numpy(float)
    start = ((w[:, None] * X).T @ pi).T
    if no_tilt:
        q, info = pi, dict(steps=0, status='no tilt', kl=0.0, targets=T.size, records=len(w), reached=start, largest_gradient=np.nan)
    else:
        pos = w > 0
        q, info = TL.solve(w[pos], pi[pos], X[pos], T, se, tau=tau, budget=budget)
        q0, q = q, pi.copy()
        q[pos] = q0
    fit = tg.copy()
    r = pd.DataFrame(start, index=ST.STATES, columns=ST.AGE_BAND_NAMES).stack()
    fit['start'] = r.reindex(pd.MultiIndex.from_frame(fit[['state', 'band']])).to_numpy()
    r = pd.DataFrame(info['reached'], index=ST.STATES, columns=ST.AGE_BAND_NAMES).stack()
    fit['final'] = r.reindex(pd.MultiIndex.from_frame(fit[['state', 'band']])).to_numpy()
    return q, fit, {k: v for k, v in info.items() if k not in ('miss', 'reached')}, table


# ---- the scores ---------------------------------------------------------------------------------------------------------
def heldout_cells(f, group, P, ht2, areas, items=None):
    """validate_state_weights.R's --untargeted: one row per item, AGI group and area with a positive published value."""
    items = ST.HELD_OUT if items is None else items
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
    """The plain average miss over all cells, the dollar-weighted miss over the dollar items' cells, the return-weighted miss over
    the count items' cells, each in percent; and the same by item."""
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
    """id, state, weight: one row per record and area with a share above zero. Parquet with Zstandard compression and the state
    as a dictionary. weight = the record's national weight x its share, so the sum over areas is the national weight."""
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
    rel = (tot - w).abs() / w.where(w > 0, 1.0)
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


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--run', required=True, help='a finished run: <run>/<year>/tilt/y<year>/file_<year>.parquet')
    ap.add_argument('--out', required=True, help='the output folder')
    ap.add_argument('--year', type=int, required=True)
    ap.add_argument('--start', default='cps', choices=['cps', 'iselin'])
    ap.add_argument('--start-dir', default=None, help='the folder of start_<year>.npz (state_start.py); default <out>/starts')
    ap.add_argument('--items', default='comparison', help='comparison: Iselin\'s 21; or a comma-separated list; or final: state_final_items')
    ap.add_argument('--tolerance', type=float, default=0.02)
    ap.add_argument('--tau', type=float, default=1000.0)
    ap.add_argument('--nonfiler-tolerance', type=float, default=0.02)
    ap.add_argument('--budget', type=int, default=150)
    ap.add_argument('--tag', default=None)
    ap.add_argument('--no-tilt', action='store_true')
    ap.add_argument('--write', action='store_true', help='the year\'s final file, with the non-filers')
    ap.add_argument('--save-shares', action='store_true')
    a = ap.parse_args()
    year = a.year
    df, ht2 = load_year(a.run, year)
    areas = ST.areas(year)
    filer = df['filer'].to_numpy() == 1
    f = df[filer].reset_index(drop=True)
    group = ST.agi_group(f['calc_agi'])
    pin = pins_of(f, year, areas)
    items = ST.ISELIN_21 if a.items == 'comparison' else (ST.FINAL_ITEMS if a.items == 'final' else a.items.split(','))
    start_dir = a.start_dir or f'{a.out}/starts'
    tag = a.tag or f'{a.start}_{a.items}_t{a.tolerance:g}_tau{a.tau:g}' + ('_notilt' if a.no_tilt else '')
    od = f'{a.out}/fits/y{year}/{tag}'
    os.makedirs(od, exist_ok=True)
    if a.start == 'iselin':
        p0 = iselin_start(group, ht2, areas)
    else:
        pi_all = read_start(f'{start_dir}/start_{year}.npz', df)
        p0 = cps_start(pi_all[filer], group, ht2, areas)
    P, fit, info, notes = tilt_filers(f, p0, group, ht2, areas, items, a.tolerance, a.tau, pin, a.budget, year, no_tilt=a.no_tilt)
    fit.to_csv(f'{od}/fit.csv.gz', index=False)
    info.to_csv(f'{od}/newton.csv', index=False)
    notes.to_csv(f'{od}/no_target.csv', index=False)
    cells = heldout_cells(f, group, P, ht2, areas)
    cells.to_csv(f'{od}/heldout_cells.csv.gz', index=False)
    s = score(cells)
    r = dict(year=year, start=a.start, items=a.items, tolerance=a.tolerance, tau=a.tau, tilted=not a.no_tilt, tag=tag,
             targets=len(fit), within_2pct=100 * float((fit['miss_rel'] <= 0.02).mean()),
             within_2pct_start=100 * float((fit['start_rel'] <= 0.02).mean()), plain_targeted=100 * float(fit['miss_rel'].mean()),
             plain_targeted_start=100 * float(fit['start_rel'].mean()), largest_miss_se=float(fit['miss_se'].abs().max()),
             newton_largest_group=int(info['steps'].max()), newton_all_groups=int(info['steps'].sum()),
             all_converged=bool(info['status'].isin(['converged', 'no tilt']).all()), move_nats=float(np.average(info['kl'], weights=info['records'])),
             **quality(f, P, areas), **{f'heldout {k}': v for k, v in s.items()})
    say(json.dumps({k: (round(v, 3) if isinstance(v, float) else v) for k, v in r.items() if not k.startswith('heldout plain ') and not k.startswith('heldout weighted ')}))
    json.dump(r, open(f'{od}/summary.json', 'w'), indent=1)
    if a.write or a.save_shares:
        nf = df[~filer].reset_index(drop=True)
        assert a.start == 'cps', 'the final files start from the network'
        q, nfit, ninfo, ntable = tilt_nonfilers(nf, pi_all[~filer], ht2, year, a.nonfiler_tolerance, a.tau, a.budget)
        say(f'{year} non-filers: {ninfo["steps"]} Newton steps, {ninfo["status"]}, move {ninfo["kl"]:.4f} nats')
        nfit.to_csv(f'{od}/nonfiler_fit.csv', index=False)
        ntable.to_csv(f'{od}/nonfiler_states.csv')
        A = np.zeros((len(df), len(areas)))
        A[filer] = P
        A[~filer, :51] = q
        ids, wt = df['record_id'].to_numpy(), df['weight'].to_numpy(float)
        os.makedirs(f'{a.out}/shares', exist_ok=True)
        np.savez(f'{a.out}/shares/shares_{year}.npz', record_id=ids, areas=np.array(areas), P=A)
    if a.write:
        os.makedirs(f'{a.out}/files', exist_ok=True)
        path = f'{a.out}/files/state_weights_{year}.parquet'
        assert not os.path.exists(path), f'{path} is already there'
        n = write_long(path, ids, wt, A, areas)
        items_x = ST.item_vectors(df, [v for v in ST.ITEMS])
        chk = check_written(path, df, items_x) | {'year': year, 'newton_largest_group': int(max(info['steps'].max(), ninfo['steps'])),
                                                  'newton_all_groups': int(info['steps'].sum() + ninfo['steps']),
                                                  'all_converged': bool(info['status'].eq('converged').all() and ninfo['status'] == 'converged')}
        say(json.dumps(chk))
        os.makedirs(f'{a.out}/checks', exist_ok=True)
        json.dump(chk, open(f'{a.out}/checks/check_{year}.json', 'w'), indent=1)


if __name__ == '__main__':
    main()
