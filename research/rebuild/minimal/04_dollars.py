"""
04_dollars.py  (minimal example, step 4: the rank-dollar map)

What it does. The model works in rank bands, not dollars. This file turns
bands back into dollars.

  1. Builds the 2015 rank-dollar map for each source (PUF, SCF) and amount:
     for every band, the full sorted list of the real records' dollar values
     in that band, with their survey weights.
  2. Restores dollars for any banded file: for each record and amount, one
     value drawn at random from its band's list, with probability
     proportional to the weight of the real record that holds it. Band 0 is
     exactly zero; band -1 (amount absent) stays missing.
  3. Checks the map by a round trip on the held-out test records of each
     source: band them, draw dollars from a map built without them, and
     compare totals, the share at zero and four percentiles with the true
     dollars.
  4. Builds the crude 2019 PUF map: the 2015 PUF map with every value
     multiplied by one growth factor per amount, taken from SOI.
  5. Compares the real PUF with SOI 2015 by AGI bracket, and, if a generated
     file exists in gen/, gives its PUF records 2015 dollars and compares them too.

AGI is never a stored amount: it is the sum of wages, interest, dividends,
capital gains and other_agi, computed after the dollars are drawn.

Empty bands. A band is empty when one round dollar amount (a lump such as
$20 of interest) holds more weight than the band's width, so two cut points
coincide (step 1 report, section 4). The dollar amount at those ranks is the
lump value itself, which is the coinciding cut point. Rule: a record in an
empty band gets exactly that value. Negative bands of an amount that has
no negatives at all (wages, interest, dividends) have no cut points; a record
there gets zero, the nearest amount in dollar order. (A third rule, for
bands empty only in a subset of records, is in fill_row.) The rules are written
into the map as extra rows marked filled = True, so the draw has no special
cases and the count of draws that used a rule is a count of filled rows hit.

Writes, in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/map/:
  map_puf_2015.parquet, map_scf_2015.parquet, map_puf_2019.parquet
      columns: variable, band, value, weight, filled, rule
  growth_factors.csv      the 2015-to-2019 factor per amount and its inputs
  roundtrip.csv           the round-trip comparison, per source and amount
  roundtrip_seeds.csv     the round-trip total over 20 seeds (spread)
  empty_band_hits.csv     how often the empty-band rules fired in the round trip
  puf_vs_soi_2015.csv     real PUF, and the generated 2015 file when one
                          exists, against SOI 2015 by AGI bracket
"""
import glob
import json
import os
import numpy as np
import pandas as pd
import soi_cells

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{ROOT}/map'
AMOUNTS = ['wages', 'interest', 'dividends', 'capital_gains', 'other_agi', 'net_worth']
SEED = 20261104


# ---------------------------------------------------------------- the map

def layout(edge):
    """Band numbering read from step 1's edges: number of bands, first
    positive band, and how many negative band slots there are."""
    n_bands = edge['n_bands']
    pos0 = n_bands - (len(edge['positive']['cuts']) + 1)
    return n_bands, pos0, pos0 - 1


def band_cuts(edge, band):
    """Dollar cut points at a band's lower and upper edge (None where open).

    Step 1's rules (01_band.py): a positive value equal to a cut belongs to
    the band below the cut, so positive band i is (cut[i-1], cut[i]]. A
    negative value equal to a cut belongs to the less negative band, so
    negative band b is [cut[b-2], cut[b-1]), with band 1 open at the bottom
    and the last negative band ending just below zero.
    """
    n_bands, pos0, n_neg = layout(edge)
    if band >= pos0:
        pos, i = edge['positive']['cuts'], band - pos0
        return (pos[i - 1] if i >= 1 else 0.0), (pos[i] if i < len(pos) else None)
    neg = edge['negative']['cuts']
    return (neg[band - 2] if band >= 2 else None), (neg[band - 1] if band < n_neg else 0.0)


def fill_row(edge, band):
    """The map row for a band that holds no record (see the top note).

    A lump band has equal cuts, and the value is the lump. A band with
    distinct cuts can be empty only in a subset of the records (the round
    trip builds its map without the test records); it gets the cut that
    belongs to it, which is a real value in that band: the upper cut for a
    positive band, the lower cut for a negative one (the other cut where
    that edge is open).
    """
    n_bands, pos0, n_neg = layout(edge)
    if 1 <= band <= n_neg and edge['negative']['bands_used'] == 0:
        return 0.0, 'no negatives in data: zero'
    # Fewer negative bands than slots would need its own rule.
    assert edge['negative']['bands_used'] in (0, n_neg), 'unexpected negative band count'
    lo, hi = band_cuts(edge, band)
    if lo is not None and lo == hi:
        return float(lo), 'empty band: lump value at its cut'
    own = hi if band >= pos0 else lo
    other = lo if band >= pos0 else hi
    return float(own if own is not None else other), 'no records in this subset: band edge'


def build_map(df, edges, source):
    """Sorted dollar values and weights per amount and band, plus fill rows."""
    rows = []
    for v in AMOUNTS:
        x, b, w = df[v].to_numpy(float), df[f'{v}_band'].to_numpy(), df['weight'].to_numpy(float)
        present = b >= 0
        assert np.array_equal(present, ~np.isnan(x)), f'{v}: band -1 must match missing dollars'
        if not present.any():
            continue                      # PUF net worth: never observed, no map
        m = pd.DataFrame({'variable': v, 'band': b[present], 'value': x[present],
                          'weight': w[present], 'filled': False, 'rule': 'real records'})
        edge = edges[f'{source}/2015/{v}']
        missing = sorted(set(range(edge['n_bands'])) - set(m['band']))
        fills = [dict(zip(['value', 'rule'], fill_row(edge, k)), variable=v, band=k,
                      weight=1.0, filled=True) for k in missing]
        rows += [m, pd.DataFrame(fills, columns=m.columns)]
    out = pd.concat(rows, ignore_index=True)
    return out.sort_values(['variable', 'band', 'value'], kind='stable').reset_index(drop=True)


# ---------------------------------------------------------------- the draw

def draw_one(bands, vmap, rng):
    """Draw dollars for one amount. Returns values and the rule behind each draw."""
    u = rng.random(len(bands))            # one uniform per record, in row order
    values = np.full(len(bands), np.nan)
    rule = np.full(len(bands), '', dtype=object)
    for band, g in vmap.groupby('band', sort=True):
        idx = np.flatnonzero(bands == band)
        if len(idx) == 0:
            continue
        cum = np.cumsum(g['weight'].to_numpy())
        pick = np.searchsorted(cum, u[idx] * cum[-1], side='right')
        pick = np.minimum(pick, len(cum) - 1)
        values[idx] = g['value'].to_numpy()[pick]
        rule[idx] = g['rule'].to_numpy()[pick]
    absent = bands < 0
    unmapped = ~absent & np.isnan(values)
    assert not unmapped.any(), f'{unmapped.sum()} records in bands the map lacks'
    return values, rule


def draw_dollars(df, maps, seed=SEED):
    """Replace each amount's dollars with a draw from its band.

    maps: {variable: map rows for that variable}. A variable with no map is
    left untouched. The random stream for each amount is fixed by the seed
    and the amount's position, and each record's draw is set by its row.
    Returns the new table and, per amount, the count of draws that used a
    fill rule rather than real records, by rule.

    A map given as {year: map rows} (net worth by SCF wave, step 21) is
    applied by the record's year token: every year's map is drawn over all
    rows with the same random stream, and each row keeps the draw of its own
    year, so a record's draw still depends only on its row and its year.
    """
    out, hits = df.copy(), {}
    for i, v in enumerate(AMOUNTS):
        if v not in maps or f'{v}_band' not in df:
            continue
        bands = df[f'{v}_band'].to_numpy()
        if isinstance(maps[v], dict):
            # the variable's own year column if the table has one
            # (net_worth_year: step 21, second round), else the record's year
            year = df[f'{v}_year' if f'{v}_year' in df else 'year'].to_numpy()
            assert set(np.unique(year)) <= set(maps[v]), (v, np.unique(year), list(maps[v]))
            vals, rule = np.full(len(df), np.nan), np.full(len(df), '', dtype=object)
            for y, vmap in maps[v].items():
                here = year == y
                if here.any():
                    vy, ry = draw_one(bands, vmap, np.random.default_rng([seed, i]))
                    vals[here], rule[here] = vy[here], ry[here]
            out[v] = vals
        else:
            rng = np.random.default_rng([seed, i])
            out[v], rule = draw_one(bands, maps[v], rng)
        filled = rule[(rule != '') & (rule != 'real records')]
        hits[v] = pd.Series(filled, dtype=object).value_counts().to_dict()
    return out, hits


def split_by_variable(m):
    return {v: g for v, g in m.groupby('variable')}


# ---------------------------------------------------------------- growth to 2019

def growth_factors(totals_path):
    """2019-over-2015 growth in the amount per return that has the item.

    other_agi has no published item: its amount is AGI minus the four items
    (the same identity that defines it on the PUF, with capital gains as the
    Schedule D net) and it grows with that amount per return filed.

    Per return WITH the item, not per return filed: the 2019 map should hold
    the dollar level of those who have the item; how many have it is a
    separate target the tilt hits. Dividing by all returns would put the
    growth in participation into the map as well, and the tilt would then
    count it a second time.
    """
    t = pd.read_csv(totals_path)
    t = t[(t['filing_status'] == 'all') & (t['agi_bracket_label'] == 'all') & (t['table'] == 1.4)]
    val = {(r.year, r.variable, r.statistic): r.value for r in t.itertuples()}
    for y in (2015, 2019):
        # Schedule D net, the concept of the PUF's E01000; the 1040-line
        # distributions (E01100) are part of other_agi on the PUF.
        val[(y, 'sch_d_net', 'amount')] = (val[(y, 'sch_d_net_gain', 'amount')]
                                           - val[(y, 'sch_d_net_loss', 'amount')])
        val[(y, 'other_agi', 'amount')] = val[(y, 'agi', 'amount')] - sum(
            val[(y, k, 'amount')] for k in
            ['wages', 'taxable_interest', 'ordinary_dividends', 'sch_d_net'])
    spec = {  # amount item, count item: what each map amount grows with
        'wages': ('wages', 'wages'),
        'interest': ('taxable_interest', 'taxable_interest'),
        'dividends': ('ordinary_dividends', 'ordinary_dividends'),
        'capital_gains': ('sch_d_net_gain', 'sch_d_net_gain'),  # gains only; see grow_map
        'other_agi': ('other_agi', 'returns'),
    }
    rows = []
    for v, (amt, cnt) in spec.items():
        per = {y: val[(y, amt, 'amount')] / val[(y, cnt, 'count')] for y in (2015, 2019)}
        per_all = {y: val[(y, amt, 'amount')] / val[(y, 'returns', 'count')] for y in (2015, 2019)}
        rows.append({'variable': v, 'amount_item': amt, 'count_item': cnt,
                     'amount_2015': val[(2015, amt, 'amount')], 'amount_2019': val[(2019, amt, 'amount')],
                     'per_item_return_2015': per[2015], 'per_item_return_2019': per[2019],
                     'factor': per[2019] / per[2015],
                     'factor_if_per_all_returns': per_all[2019] / per_all[2015]})
    return pd.DataFrame(rows)


def grow_values(variable, x, factor):
    """2019 dollars from 2015 dollars: every value times the factor, with one
    exception. Negative capital gains are not grown: almost all are the loss
    deducted on the 1040, capped at $3,000 in nominal terms in both years.
    Negative other_agi (mostly business losses) is grown like the positives.
    Step 5 uses this same rule to grow the real PUF records.
    """
    x = np.array(x, dtype=float)
    grow = (x > 0) if variable == 'capital_gains' else (x != 0)
    x[grow] *= factor
    return x


def grow_map(m, factors):
    """The 2019 map: the 2015 map with each amount's values grown."""
    g = m.copy()
    for v, f in zip(factors['variable'], factors['factor']):
        rows = g['variable'] == v
        g.loc[rows, 'value'] = grow_values(v, g.loc[rows, 'value'], f)
    return g


# ---------------------------------------------------------------- round trip

def wquantile(x, w, probs):
    """Smallest x whose cumulative weight share reaches each probability (as step 1)."""
    o = np.argsort(x, kind='stable')
    xs, F = x[o], np.cumsum(w[o]) / w.sum()
    idx = np.minimum(np.searchsorted(F, np.asarray(probs) - 1e-12, side='left'), len(xs) - 1)
    return xs[idx]


def describe(x, w):
    """Weighted total, share zero and percentiles over records with the amount."""
    ok = ~np.isnan(x)
    x, w = x[ok], w[ok]
    q = wquantile(x, w, [0.5, 0.9, 0.99, 0.999])
    return {'total_bn': (x * w).sum() / 1e9, 'share_zero': w[x == 0].sum() / w.sum(),
            'share_negative': w[x < 0].sum() / w.sum(),
            'p50': q[0], 'p90': q[1], 'p99': q[2], 'p999': q[3]}


def round_trip(df, edges, source):
    """Held-out test records: true dollars against dollars drawn from their bands."""
    test, rest = df[df['split'] == 'test'], df[df['split'] != 'test']
    maps = split_by_variable(build_map(rest, edges, source))
    drawn, hits = draw_dollars(test, maps)
    w = test['weight'].to_numpy(float)
    rows, seeds = [], []
    pairs = {v: (test[v].to_numpy(float), drawn[v].to_numpy(float)) for v in maps}
    if 'other_agi' in maps:                  # AGI computed from the drawn parts
        pairs['agi (derived)'] = (soi_cells.agi(test), soi_cells.agi(drawn))
    for v, (x_real, x_new) in pairs.items():
        real, new = describe(x_real, w), describe(x_new, w)
        for stat in real:
            rows.append({'source': source, 'variable': v, 'statistic': stat,
                         'real': real[stat], 'drawn': new[stat]})
    for s in range(20):                      # spread of the total across seeds
        d, _ = draw_dollars(test, maps, seed=SEED + 1 + s)
        for v in maps:
            seeds.append({'source': source, 'variable': v, 'seed': s,
                          'total_bn': np.nansum(d[v].to_numpy(float) * w) / 1e9})
        if 'other_agi' in maps:
            seeds.append({'source': source, 'variable': 'agi (derived)', 'seed': s,
                          'total_bn': (soi_cells.agi(d) * w).sum() / 1e9})
    fired = [{'source': source, 'variable': v, 'records': len(test), 'rule': r, 'draws': n}
             for v in maps for r, n in (hits[v] or {'none fired': 0}).items()]
    return pd.DataFrame(rows), pd.DataFrame(seeds), pd.DataFrame(fired)


# ---------------------------------------------------------------- generated file

def generated_file():
    """Path of the generated file: GEN_FILE if set, else the only parquet in gen/."""
    if os.environ.get('GEN_FILE'):
        return os.environ['GEN_FILE']
    files = sorted(glob.glob(f'{ROOT}/gen/*.parquet'))
    assert len(files) <= 1, f'several generated files, set GEN_FILE: {files}'
    return files[0] if files else None


def puf_maps(year):
    """PUF maps for a year, plus the SCF net worth map: a PUF-labelled
    generated record that carries a net worth band was banded on SCF cuts.
    MINIMAL_MAP_DIR (environment, added 2026-10-15 for the v4 baseline): read
    the three map files from that folder instead of map/ (15_maps.py writes
    map/V4/ from table E4). Unset: map/, as before."""
    where = os.environ.get('MINIMAL_MAP_DIR', OUT)
    maps = split_by_variable(pd.read_parquet(f'{where}/map_puf_{year}.parquet'))
    maps['net_worth'] = split_by_variable(
        pd.read_parquet(f'{where}/map_scf_2015.parquet'))['net_worth']
    # MINIMAL_NW_MAP_YEARS (added 2026-10-16 for step 21, 21_PRIORS.md), e.g.
    # "2015,2018,2021": net worth gets dollars from the SCF wave of the
    # record's year token, map_scf_nw_<year>.parquet in the same folder
    # (21_maps.py, all in 2015 dollars). Unset: the one 2015 map, as before.
    years = [int(y) for y in os.environ.get('MINIMAL_NW_MAP_YEARS', '').split(',') if y.strip()]
    # MINIMAL_NW_TARGET_MAP_YEAR (added 2026-10-16, step 21 third round; plan
    # decision 10): every record's net worth rank is turned into dollars with
    # this one wave's map, whatever year token the rank was drawn under. The
    # target year's map: 2018 (the 2019 survey) for a 2019 file. Takes
    # precedence over MINIMAL_NW_MAP_YEARS.
    target = os.environ.get('MINIMAL_NW_TARGET_MAP_YEAR', '').strip()
    if target:
        maps['net_worth'] = split_by_variable(pd.read_parquet(
            f'{where}/map_scf_nw_{int(target)}.parquet'))['net_worth']
    elif years:
        maps['net_worth'] = {y: split_by_variable(pd.read_parquet(
            f'{where}/map_scf_nw_{y}.parquet'))['net_worth'] for y in years}
    return maps


def check_against_soi_2015(real_puf):
    """Real PUF, and generated PUF records if any, against SOI 2015 by AGI bracket.

    The real PUF's own gaps are the yardstick: the generated file cannot be
    expected to beat the data it learned from.
    """
    soi = soi_cells.read_soi(f'{ROOT}/targets/soi_2015_validation.csv', 2015)
    brackets = soi_cells.published_brackets(soi)
    out = soi_cells.compare(real_puf, real_puf['weight'], soi, brackets).rename(columns={
        'value': 'soi', 'file': 'real_puf', 'records': 'real_puf_records', 'gap_pct': 'real_puf_gap_pct'})
    path = generated_file()
    if path is None:
        print('no generated file in gen/: comparing the real PUF only')
    else:
        gen = pd.read_parquet(path)
        gen = gen[(gen['source'] == 'puf') & (gen['year'] == 2015)].reset_index(drop=True)
        gen, hits = draw_dollars(gen, puf_maps(2015))
        print(f'generated file {path}: {len(gen)} PUF records; fill-rule draws {hits}')
        # Equal weights scaled to SOI's 2015 return count, so brackets compare as shares.
        total = soi[(soi['variable'] == 'returns') & (soi['statistic'] == 'count')]['value'].sum()
        g = soi_cells.compare(gen, np.full(len(gen), total / len(gen)), soi, brackets)
        out['generated'], out['generated_records'] = g['file'], g['records']
        out['generated_gap_pct'] = g['gap_pct']
    out.to_csv(f'{OUT}/puf_vs_soi_2015.csv', index=False)
    print(out.to_string())


# ---------------------------------------------------------------- main

def main():
    os.makedirs(OUT, exist_ok=True)
    df = pd.read_parquet(f'{ROOT}/data/stacked.parquet')
    edges = json.load(open(f'{ROOT}/data/band_edges.json'))
    assert set(df['year']) == {2015}
    trips, seeds, fired = [], [], []
    for source in ['puf', 'scf']:
        d = df[df['source'] == source].reset_index(drop=True)
        m = build_map(d, edges, source)
        m.to_parquet(f'{OUT}/map_{source}_2015.parquet', index=False)
        filled = m[m['filled']].groupby(['variable', 'rule']).size()
        print(f'{source}: {len(d)} records; map rows {len(m)}; fill rows by rule:\n{filled}')
        t, s, f = round_trip(d, edges, source)
        trips.append(t), seeds.append(s), fired.append(f)
    factors = growth_factors(f'{ROOT}/targets/soi_totals_2015_2019.csv')
    factors.to_csv(f'{OUT}/growth_factors.csv', index=False)
    print(factors.to_string())
    m15 = pd.read_parquet(f'{OUT}/map_puf_2015.parquet')
    grow_map(m15, factors).to_parquet(f'{OUT}/map_puf_2019.parquet', index=False)
    trips = pd.concat(trips)
    trips['rel_diff'] = trips['drawn'] / trips['real'] - 1
    trips.to_csv(f'{OUT}/roundtrip.csv', index=False)
    seeds = pd.concat(seeds)
    seeds.to_csv(f'{OUT}/roundtrip_seeds.csv', index=False)
    pd.concat(fired).to_csv(f'{OUT}/empty_band_hits.csv', index=False)
    pd.set_option('display.width', 200)
    print(trips.to_string())
    print(seeds.groupby(['source', 'variable'])['total_bn'].describe().to_string())
    print(pd.concat(fired).to_string())
    check_against_soi_2015(df[df['source'] == 'puf'].reset_index(drop=True))


if __name__ == '__main__':
    main()
