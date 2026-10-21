"""
35_tables.py  (minimal example, step 35: business income, PUF against SCF.
Note: 35_BUSINESS_INCOME.md. Describes the data; changes no setting or model.)

What it does
  Weighted distributions of business income for tax year 2015:
  - PUF filers (raw 2015 PUF): Schedule C (E00900), Schedule E in total
    (E02000) and by component, Schedule F (E02100), and the sum C + E + F
    (step 33's business_income). Components of Schedule E, from the PUF
    fields named in config/variable_guide/baseline.csv:
      partnerships  = E25940 + E25980 - E25920 - E25960 - E26110
      S corporations = E26170 + E26190 - E26160 - E26180 - E26100
      (passive and nonpassive income minus losses minus the section 179
      deduction; E26270 is the PUF's own combined partnership and S
      corporation net figure and is shown too)
      rents and royalties = E25850 - E25860
      estates and trusts  = E26390 - E26400
      farm rental (Form 4835) = E27200
      remainder = E02000 - E26270 - rents and royalties - estates and trusts
                  - farm rental
  - PUF side as step 33 builds it: filers plus the crude CPS non-filers
    (stacked_E33.parquet, whose business_income for non-filers is CPS sole
    proprietorship + rent + rent loss in 2015 dollars).
  - SCF 2016 wave (2015 incomes): X5704 and X5714 separately and summed, at
    the household level (raw file, 35_scf_raw.R; weight X42001 / 5 over the
    five implicates) and at the tax-unit level as the production builder
    splits them (scf2016_units33.csv.gz from 33_build_scf.R, joined to
    stacked_E33's SCF rows for married and wages). Codes: -1 ("Nothing") is
    read as zero; -2 ("Negative value inferred from data on business") and
    -9 ("Other negative value and X5729 negative (public data set only)")
    are losses of unknown size: they count as losses in the shares but are
    left out of the loss percentiles and of the dollar totals (counted).
  Statistics: weighted count, share with a profit, a loss, nothing; total
  profits, total losses, net total; percentiles of profits and of the size
  of losses; for all units, by wage group and by married / not married.
  Also: where PUF losses come from; SCF loss reporters; SCF answers on
  filing Schedules C, E, F against the PUF; the SCF's business-section
  income for households that report no business loss.
Writes  step35/tables.md, step35/*.csv, step35/*.png
Run     sbatch -J tab35 research/rebuild/minimal/35_cpu.sbatch tables
"""
import os

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
D = f'{R}/data'
OUT = f'{R}/step35'
PUF_RAW = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/puf_2015.csv'
SCFP = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1/2016/historical/SCFP2016.csv'
POOL = ('/nfs/roberts/project/pi_nrs36/shared/model_data/ASEC-Nonfilers/v1/2026091201/'
        'historical/nonfiler_pool_2017.csv.gz')
CPI_2015, CPI_2017 = 237.017, 245.120
NF_OFFSET = 900_000_000
QS = [0.10, 0.25, 0.50, 0.75, 0.90, 0.99]
WAGE_GROUPS = [('none', None, 0), ('under $25k', 0, 25000), ('$25k-75k', 25000, 75000),
               ('$75k-200k', 75000, 200000), ('over $200k', 200000, None)]
MD = []


def say(*a):
    s = ' '.join(str(x) for x in a)
    print(s, flush=True)


def md(s=''):
    MD.append(s)


def wq(x, w, qs):
    o = np.argsort(x)
    x, w = x[o], w[o]
    c = np.cumsum(w) - 0.5 * w
    c /= w.sum()
    return np.interp(qs, c, x)


def wage_group(wages):
    g = np.full(len(wages), '', dtype=object)
    for name, lo, hi in WAGE_GROUPS:
        if lo is None:
            sel = wages <= 0
        elif hi is None:
            sel = wages > lo
        else:
            sel = (wages > lo) & (wages <= hi)
        g[sel] = name
    assert (g != '').all()
    return g


def stats(x, w, coded_loss=None):
    """x amounts, w weights; coded_loss: boolean, losses of unknown size."""
    x, w = np.asarray(x, float), np.asarray(w, float)
    cl = np.zeros(len(x), bool) if coded_loss is None else np.asarray(coded_loss, bool)
    pos, neg = (x > 0) & ~cl, (x < 0) & ~cl
    zero = (x == 0) & ~cl
    W = w.sum()
    r = {'units (m)': W / 1e6, 'profit %': 100 * w[pos].sum() / W,
         'loss %': 100 * (w[neg].sum() + w[cl].sum()) / W, 'none %': 100 * w[zero].sum() / W,
         'profits $bn': (w[pos] * x[pos]).sum() / 1e9, 'losses $bn': (w[neg] * x[neg]).sum() / 1e9}
    r['net $bn'] = r['profits $bn'] + r['losses $bn']
    if cl.any():
        r['loss % of unknown size'] = 100 * w[cl].sum() / W
    for lab, sel, sign in [('profit', pos, 1), ('loss size', neg, -1)]:
        q = wq(sign * x[sel], w[sel], QS) if sel.sum() >= 5 else [np.nan] * len(QS)
        for qq, v in zip(QS, q):
            r[f'{lab} P{int(round(qq * 100))}'] = v
    r['records'] = len(x)
    return r


def fmt(v, col):
    if pd.isna(v):
        return ''
    if '%' in col:
        return f'{v:.1f}'
    if '$bn' in col:
        return f'{v:,.1f}'
    if '(m)' in col:
        return f'{v:,.1f}'
    if col == 'records':
        return f'{int(v):,}'
    return f'{v:,.0f}'


def table(rows, title, cols=None):
    df = pd.DataFrame(rows).T
    cols = cols or list(df.columns)
    md(f'### {title}')
    md()
    md('| | ' + ' | '.join(cols) + ' |')
    md('|---|' + '---:|' * len(cols))
    for k, r in df.iterrows():
        md(f'| {k} | ' + ' | '.join(fmt(r.get(c, np.nan), c) for c in cols) + ' |')
    md()
    return df


SHORT = ['units (m)', 'profit %', 'loss %', 'none %', 'profits $bn', 'losses $bn', 'net $bn']
PCT = [f'profit P{int(q * 100)}' for q in QS] + [f'loss size P{int(q * 100)}' for q in QS]


def full_tables(name, items, w, wages, married, coded=None):
    """items: {label: array}. Writes all, by wage group, by married."""
    coded = coded or {}
    allrows = {k: stats(v, w, coded.get(k)) for k, v in items.items()}
    df = table(allrows, f'{name}: all units, counts and totals', SHORT + (['loss % of unknown size'] if coded else []))
    table(allrows, f'{name}: all units, percentiles (dollars; loss sizes as positive numbers)', PCT)
    out = [df.assign(group='all')]
    wg = wage_group(wages)
    for gname, sel in [(g[0], wg == g[0]) for g in WAGE_GROUPS] + [('married', married == 1), ('not married', married == 0)]:
        rows = {k: stats(v[sel], w[sel], None if coded.get(k) is None else coded[k][sel]) for k, v in items.items()}
        d = pd.DataFrame(rows).T.assign(group=gname)
        out.append(d)
    big = pd.concat(out)
    big.index.name = 'item'
    # by-group tables: one per statistic family, item x group
    for title, cols in [('counts and totals', SHORT), ('percentiles', PCT)]:
        md(f'### {name}: by wage group and marital status, {title}')
        md()
        md('| item | group | ' + ' | '.join(cols) + ' |')
        md('|---|---|' + '---:|' * len(cols))
        for item in items:
            for _, r in big.loc[[item]].iterrows():
                if r['group'] == 'all':
                    continue
                md(f'| {item} | {r["group"]} | ' + ' | '.join(fmt(r.get(c, np.nan), c) for c in cols) + ' |')
        md()
    return big


def main():
    os.makedirs(OUT, exist_ok=True)
    t = pd.read_parquet(f'{D}/stacked_E33.parquet')

    # ------------------------------------------------------------ PUF filers
    f = [c for c in pd.read_csv(PUF_RAW, nrows=0).columns]
    need = ['RECID', 'S006', 'MARS', 'E00200', 'E00900', 'E02000', 'E02100', 'E25850', 'E25860', 'E25920',
            'E25940', 'E25960', 'E25980', 'E26100', 'E26110', 'E26160', 'E26170', 'E26180', 'E26190', 'E26270',
            'E26390', 'E26400', 'E27200', 'E30400', 'E30500', 'SCHCF', 'SCHE', 'E25820', 'P25700', 'E25550',
            'P25350', 'P25380']
    need = [c for c in need if c in f]
    raw = pd.read_csv(PUF_RAW, usecols=need)
    raw = raw[raw.RECID < 999996]
    na = raw.isna().sum()
    say('PUF missing values per field:', na[na > 0].to_dict())
    assert na.sum() == 0
    w = raw.S006.to_numpy() / 100
    part = raw.E25940 + raw.E25980 - raw.E25920 - raw.E25960 - raw.E26110
    scorp = raw.E26170 + raw.E26190 - raw.E26160 - raw.E26180 - raw.E26100
    rents = raw.E25850 - raw.E25860
    estates = raw.E26390 - raw.E26400
    farmrent = raw.E27200
    rem = raw.E02000 - raw.E26270 - rents - estates - farmrent
    gap_ps = (raw.E26270 - part - scorp).to_numpy()
    md('# Step 35 tables: business income, 2015 PUF against the 2016 SCF wave')
    md()
    md('Generated by 35_tables.py. Weighted. Dollars of tax year 2015. "Loss size" percentiles are '
       'the size of the loss as a positive number, among units with a loss.')
    md()
    md('## Checks on the PUF component fields')
    md()
    for lab, g in [('E26270 minus (partnerships + S corporations as built from the passive/nonpassive fields)', gap_ps),
                   ('Schedule E remainder (E02000 minus E26270, rents, estates, farm rental)', rem.to_numpy())]:
        nz = np.abs(g) > 0.5
        md(f'- {lab}: nonzero on {nz.sum():,} records ({100 * w[nz].sum() / w.sum():.2f}% of weight); '
           f'weighted sum ${(w * g).sum() / 1e9:,.1f} billion; weighted sum of absolute values '
           f'${(w * np.abs(g)).sum() / 1e9:,.1f} billion.')
    for c in ['SCHCF', 'SCHE']:
        if c in raw:
            md(f'- {c} values (records, weighted millions): ' + '; '.join(
                f'{k}: {int((raw[c] == k).sum()):,}, {w[raw[c] == k].sum() / 1e6:.1f}'
                for k in sorted(raw[c].unique())))
    for c in ['E25820', 'P25700', 'E25550', 'P25350', 'P25380']:
        if c in raw:
            x = raw[c].to_numpy()
            md(f'- {c}: nonzero on {100 * w[x != 0].sum() / w.sum():.2f}% of returns, negative on '
               f'{100 * w[x < 0].sum() / w.sum():.2f}%, weighted sum ${(w * x).sum() / 1e9:,.1f} billion '
               f'(field meaning not documented in this repository)')
    md()
    puf_items = {
        'C: Schedule C (E00900)': raw.E00900.to_numpy(float),
        'E: Schedule E total (E02000)': raw.E02000.to_numpy(float),
        'E: partnerships': part.to_numpy(float),
        'E: S corporations': scorp.to_numpy(float),
        'E: partnerships and S corps (E26270)': raw.E26270.to_numpy(float),
        'E: rents and royalties': rents.to_numpy(float),
        'E: estates and trusts': estates.to_numpy(float),
        'E: farm rental (Form 4835)': farmrent.to_numpy(float),
        'E: remainder': rem.to_numpy(float),
        'F: Schedule F (E02100)': raw.E02100.to_numpy(float),
        'C + F': (raw.E00900 + raw.E02100).to_numpy(float),
        'C + E + F (step 33 business_income)': (raw.E00900 + raw.E02000 + raw.E02100).to_numpy(float),
    }
    md('## 1. PUF filers (207,692 records)')
    md()
    big_puf = full_tables('PUF filers', puf_items, w, raw.E00200.to_numpy(float), (raw.MARS == 2).astype(int).to_numpy())
    big_puf.to_csv(f'{OUT}/puf_filers.csv')
    say('PUF filers negative wages records:', int((raw.E00200 < 0).sum()))

    # where PUF losses come from
    md('## 2. Where PUF losses come from (filers)')
    md()
    tot = puf_items['C + E + F (step 33 business_income)']
    comps = {k: v for k, v in puf_items.items() if k.startswith(('C:', 'F:')) or k in
             ['E: partnerships', 'E: S corporations', 'E: rents and royalties', 'E: estates and trusts',
              'E: farm rental (Form 4835)', 'E: remainder']}
    lossu = tot < 0
    md(f'Returns with a net loss on C + E + F: {w[lossu].sum() / 1e6:.2f} million '
       f'({100 * w[lossu].sum() / w.sum():.1f}% of returns); net loss total ${(w[lossu] * tot[lossu]).sum() / 1e9:,.1f} billion.')
    md()
    md('| component | returns with a loss in it, all returns (m) | its losses, all returns ($bn) | '
       'among net-loss returns: share with a loss in it (%) | among net-loss returns: share for which it is the largest loss (%) | '
       'among net-loss returns: its losses ($bn) |')
    md('|---|---:|---:|---:|---:|---:|')
    M = np.column_stack([np.minimum(v, 0) for v in comps.values()])
    largest = np.argmin(M, axis=1)
    for j, (k, v) in enumerate(comps.items()):
        l = v < 0
        md(f'| {k} | {w[l].sum() / 1e6:.2f} | {(w[l] * v[l]).sum() / 1e9:,.1f} | '
           f'{100 * w[lossu & l].sum() / w[lossu].sum():.1f} | '
           f'{100 * w[lossu & (largest == j) & (M[:, j] < 0)].sum() / w[lossu].sum():.1f} | '
           f'{(w[lossu & l] * v[lossu & l]).sum() / 1e9:,.1f} |')
    md()
    # net loss returns by size
    md('Net-loss returns (C + E + F below zero) by size of the net loss:')
    md()
    md('| net loss | returns (m) | share of net-loss returns (%) | net loss ($bn) |')
    md('|---|---:|---:|---:|')
    edges = [0, 1000, 5000, 10000, 25000, 100000, np.inf]
    for lo, hi in zip(edges[:-1], edges[1:]):
        s = lossu & (-tot > lo) & (-tot <= hi)
        md(f'| ${lo:,.0f} to {"$" + format(hi, ",.0f") if np.isfinite(hi) else "more"} | {w[s].sum() / 1e6:.2f} | '
           f'{100 * w[s].sum() / w[lossu].sum():.1f} | {(w[s] * tot[s]).sum() / 1e9:,.1f} |')
    md()
    # by AGI-free view: by wage group share with net loss
    # ------------------------------------------------------------ PUF side with non-filers
    md('## 3. PUF side as in table E33: filers and crude non-filers (C + E + F)')
    md()
    p = t[(t.source == 'puf') & (t.year == 2015)]
    pool = pd.read_csv(POOL)
    pool['record_id'] = (NF_OFFSET + pool['id'].astype(np.int64)).astype(float)
    nfr = p[p.filer == 0]
    pl = pool.set_index('record_id').loc[nfr.record_id.to_numpy()]
    dfl = CPI_2015 / CPI_2017
    na = pl[['sole_prop', 'rent', 'rent_loss']].isna().sum()
    say('non-filer pool missing:', na.to_dict())
    assert na.sum() == 0
    rows = {
        'filers (E33)': stats(p.loc[p.filer == 1, 'business_income'], p.loc[p.filer == 1, 'weight']),
        'crude non-filers (E33)': stats(nfr.business_income, nfr.weight),
        'non-filers: sole proprietorship part': stats(pl.sole_prop.to_numpy() * dfl, nfr.weight.to_numpy()),
        'non-filers: rent + rent loss part': stats((pl.rent + pl.rent_loss).to_numpy() * dfl, nfr.weight.to_numpy()),
        'PUF side, filers + non-filers (E33)': stats(p.business_income, p.weight),
    }
    table(rows, 'PUF side: counts and totals', SHORT)
    table(rows, 'PUF side: percentiles', PCT)
    bigp = full_tables('PUF side with crude non-filers, C + E + F', {'C + E + F': p.business_income.to_numpy(float)},
                       p.weight.to_numpy(float), p.wages.to_numpy(float), p.married.to_numpy())
    bigp.to_csv(f'{OUT}/puf_side.csv')

    # ------------------------------------------------------------ SCF households
    h = pd.read_csv(f'{OUT}/scf2016_hh_items.csv.gz')
    h.columns = [c.upper() for c in h.columns]
    sc = pd.read_csv(SCFP, usecols=['Y1', 'MARRIED', 'ACTBUS', 'NONACTBUS', 'BUS', 'ORESRE', 'NNRESRE', 'WGT'])
    h = h.merge(sc, on='Y1', how='left', validate='1:1')
    assert h.MARRIED.notna().all()
    hw = h.X42001.to_numpy() / 5
    say('SCF hh rows', len(h), 'weight (m)', hw.sum() / 1e6)
    md('## 4. SCF 2016 wave, household (family) level')
    md()
    md('Codes of X5704 and X5714 (rows over the five implicates; weighted households, millions):')
    md()
    md('| item | -1 "Nothing" | -2 "negative inferred from business data" | -9 "negative, total income negative" | '
       'other negative (dollar loss) | 0 "Inap." (screener no) | positive |')
    md('|---|---:|---:|---:|---:|---:|---:|')
    codes = {}
    for v in ['X5704', 'X5714', 'X5712', 'X5724', 'X5729']:
        x = h[v].to_numpy()
        cells = [x == -1, x == -2, x == -9, (x < 0) & ~np.isin(x, [-1, -2, -9]), x == 0, x > 0]
        md(f'| {v} | ' + ' | '.join(f'{c.sum():,} ({hw[c].sum() / 1e6:.2f})' for c in cells) + ' |')
        codes[v] = cells
    md()
    def clean(v):
        x = h[v].to_numpy(float).copy()
        coded = np.isin(x, [-2, -9])
        x[x == -1] = 0
        x[coded] = -1e-9   # sign only; excluded from sizes and totals by stats()
        return x, coded
    b, bc = clean('X5704')
    r_, rc = clean('X5714')
    # sum: coded if the known part does not decide the sign (a coded loss and no positive on the other item)
    sc_coded = (bc | rc) & ~(np.where(bc, 0, b) + np.where(rc, 0, r_) > 0)
    s_known = np.where(bc, 0, b) + np.where(rc, 0, r_)
    hh_items = {'X5704 (sole proprietorship, farm)': b, 'X5714 (other businesses, net rent, trusts, royalties)': r_,
                'X5704 + X5714': np.where(sc_coded, -1e-9, s_known)}
    hh_coded = {'X5704 (sole proprietorship, farm)': bc, 'X5714 (other businesses, net rent, trusts, royalties)': rc,
                'X5704 + X5714': sc_coded}
    md('In the sum, a household with a -2 or -9 code is counted as a loss of unknown size unless its other '
       'item is positive, in which case the sum is the positive item alone (a lower bound on its sign is not knowable).')
    md()
    big_h = full_tables('SCF households', hh_items, hw, np.maximum(h.X5702.to_numpy(float), 0),
                        (h.MARRIED == 1).astype(int).to_numpy(), hh_coded)
    big_h.to_csv(f'{OUT}/scf_households.csv')

    # SCF loss reporters
    md('### SCF households reporting a loss: sizes (dollar losses only)')
    md()
    md('| item | households (m) | rows | P10 | P25 | P50 | P75 | P90 | P99 | total ($bn) | mean net worth of these households, SCFP ($m, 2022 dollars) |')
    md('|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|')
    nw = pd.read_csv(SCFP, usecols=['Y1', 'NETWORTH']).set_index('Y1').loc[h.Y1, 'NETWORTH'].to_numpy()
    for v in ['X5704', 'X5714']:
        x = h[v].to_numpy(float)
        l = (x < 0) & ~np.isin(x, [-1, -2, -9])
        q = wq(-x[l], hw[l], QS)
        md(f'| {v} | {hw[l].sum() / 1e6:.2f} | {l.sum():,} | ' + ' | '.join(f'{qq:,.0f}' for qq in q) +
           f' | {(hw[l] * x[l]).sum() / 1e9:,.1f} | {(hw[l] * nw[l]).sum() / hw[l].sum() / 1e6:.2f} |')
        l2 = np.isin(x, [-2, -9])
        md(f'| {v}, coded -2 or -9 | {hw[l2].sum() / 1e6:.2f} | {l2.sum():,} | | | | | | | | '
           f'{(hw[l2] * nw[l2]).sum() / max(hw[l2].sum(), 1) / 1e6:.2f} |')
    md()

    # Schedules filed (SCF self-report) vs PUF
    md('## 5. Schedules C, E, F: SCF answers against the PUF')
    md()
    def chk(cols):
        return np.zeros(len(h), bool) | np.any([h[c].to_numpy() == 1 for c in cols if c in h], axis=0)
    filed = h.X5744.isin([1, 6]).to_numpy() if 'X5744' in h else np.ones(len(h), bool)
    schC = chk(['X5747', 'X7758', 'X7762'])
    schE = chk(['X5748', 'X7759', 'X7763'])
    schF = chk(['X5749', 'X7760', 'X7764'])
    md(f'SCF households: filed or expect to file a 2015 return (X5744 = 1 or 6): {hw[filed].sum() / 1e6:.1f} million of '
       f'{hw.sum() / 1e6:.1f} million.')
    md()
    md('| | SCF households: say a return of theirs had it (m) | % of households | % of filing households | PUF returns with the schedule\'s amount nonzero (m) | % of returns |')
    md('|---|---:|---:|---:|---:|---:|')
    for lab, s, pv in [('Schedule C', schC, raw.E00900 != 0), ('Schedule E', schE, raw.E02000 != 0),
                       ('Schedule F', schF, raw.E02100 != 0), ('C or F', schC | schF, (raw.E00900 != 0) | (raw.E02100 != 0)),
                       ('any of C, E, F', schC | schE | schF, (raw.E00900 != 0) | (raw.E02000 != 0) | (raw.E02100 != 0))]:
        pv = pv.to_numpy()
        md(f'| {lab} | {hw[s].sum() / 1e6:.1f} | {100 * hw[s].sum() / hw.sum():.1f} | {100 * hw[s & filed].sum() / hw[filed].sum():.1f} | '
           f'{w[pv].sum() / 1e6:.1f} | {100 * w[pv].sum() / w.sum():.1f} |')
    md()
    md('SCF answers on X5704 and X5714 by whether the household said it filed the matching schedule:')
    md()
    md('| group | households (m) | X5704 profit % | loss % (incl. coded) | -1 "Nothing" % | 0 Inap. % | X5714 profit % | loss % (incl. coded) | -1 % | 0 Inap. % |')
    md('|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|')
    def dist(x, s):
        ww = hw[s]
        xs = x[s]
        return [100 * ww[xs > 0].sum() / ww.sum(), 100 * ww[(xs < 0) & (xs != -1)].sum() / ww.sum(),
                100 * ww[xs == -1].sum() / ww.sum(), 100 * ww[xs == 0].sum() / ww.sum()]
    X4, X14 = h.X5704.to_numpy(), h.X5714.to_numpy()
    for lab, s in [('said Schedule C or F', schC | schF), ('said Schedule E', schE), ('said none of C, E, F (filers)', filed & ~(schC | schE | schF)),
                   ('did not file', ~filed)]:
        md(f'| {lab} | {hw[s].sum() / 1e6:.1f} | ' + ' | '.join(f'{v:.1f}' for v in dist(X4, s) + dist(X14, s)) + ' |')
    md()

    # business section cross-check
    md('## 6. Business section and real estate answers for SCF households')
    md()
    def has(c):
        return c in h
    if has('X3132') and has('X3119'):
        md('Actively managed businesses #1 and #2: the business\'s own pre-tax net income (X3132, X3232; '
           'whole business, not the family\'s share) and its legal form (X3119, X3219).')
        md()
        md('| legal form | businesses (m, weighted) | with a loss (X3132 < 0, %) | -1 "Nothing" (%) | share of these households with X5704 < 0 or coded (%) | with X5714 < 0 or coded (%) | X5704 positive (%) | X5714 positive (%) |')
        md('|---|---:|---:|---:|---:|---:|---:|---:|')
        forms = {1: 'partnership', 2: 'sole proprietorship', 3: 'S corporation', 4: 'other corporation',
                 6: 'foreign', 11: 'limited partnership / LLP / LLC', 40: 'not a formal type', -7: 'other'}
        rows = []
        for fcol, icol in [('X3119', 'X3132'), ('X3219', 'X3232')]:
            rows.append(pd.DataFrame({'form': h[fcol].to_numpy(), 'inc': h[icol].to_numpy(), 'w': hw,
                                      'x4': X4, 'x14': X14}))
        bz = pd.concat(rows)
        bz = bz[bz.form != 0]
        for code in sorted(bz.form.unique()):
            s = bz[bz.form == code]
            ww = s.w.to_numpy()
            def pc(m):
                return 100 * ww[m].sum() / ww.sum()
            md(f'| {forms.get(code, code)} ({code}) | {ww.sum() / 1e6:.2f} | {pc((s.inc < 0) & (s.inc != -1)):.1f} | {pc(s.inc == -1):.1f} | '
               f'{pc((s.x4 < 0) & (s.x4 != -1)):.1f} | {pc((s.x14 < 0) & (s.x14 != -1)):.1f} | {pc(s.x4 > 0):.1f} | {pc(s.x14 > 0):.1f} |')
        md()
        anyloss = np.zeros(len(h), bool)
        for icol in ['X3132', 'X3232']:
            anyloss |= (h[icol].to_numpy() < 0) & (h[icol].to_numpy() != -1)
        inc_loss = ((X4 < 0) & (X4 != -1)) | ((X14 < 0) & (X14 != -1))
        md(f'- Households with an actively managed business (#1 or #2) whose own net income was a loss: '
           f'{hw[anyloss].sum() / 1e6:.2f} million; of these, {100 * hw[anyloss & inc_loss].sum() / hw[anyloss].sum():.1f}% '
           f'report a loss on X5704 or X5714 (including codes -2, -9), '
           f'{100 * hw[anyloss & ~inc_loss & ((X4 > 0) | (X14 > 0))].sum() / hw[anyloss].sum():.1f}% report a positive amount on one of them '
           f'and no loss, {100 * hw[anyloss & ~inc_loss & ~((X4 > 0) | (X14 > 0))].sum() / hw[anyloss].sum():.1f}% report neither.')
    for lab, cols in [('non-actively managed businesses: family net income received by type (X3410, X3414, X3454, X3418, X3422, X3430)',
                       ['X3410', 'X3414', 'X3454', 'X3418', 'X3422', 'X3430']),
                      ('remaining actively managed businesses: family net income received (X3337)', ['X3337']),
                      ('investment real estate, properties #1 and #2: net income received (X1730, X1830)', ['X1730', 'X1830']),
                      ('remaining vacation and other properties: net rental income (X2010, X2020)', ['X2010', 'X2020']),
                      ('labor section: business income personally received, respondent and spouse (X4131, X4731; may be per period, X4132/X4732)',
                       ['X4131', 'X4731'])]:
        cols = [c for c in cols if has(c)]
        if not cols:
            md(f'- {lab}: not extracted')
            continue
        M = h[cols].to_numpy(float)
        neg = ((M < 0) & (M != -1)).any(axis=1)
        pos = (M > 0).any(axis=1)
        m1 = (M == -1).any(axis=1)
        md(f'- {lab}: households with any positive {hw[pos].sum() / 1e6:.2f} million, any negative (not -1) '
           f'{hw[neg].sum() / 1e6:.3f} million ({neg.sum()} rows), any -1 "Nothing" {hw[m1].sum() / 1e6:.2f} million.')
    md()
    md('Household asset holdings (SCFP) against reported X5704 / X5714 sign:')
    md()
    md('| group | households (m) | X5704 loss % | X5704 none % | X5714 loss % | X5714 none % | X5714 profit % |')
    md('|---|---:|---:|---:|---:|---:|---:|')
    for lab, s in [('actively managed business (ACTBUS > 0)', h.ACTBUS.to_numpy() > 0),
                   ('non-actively managed business (NONACTBUS > 0)', h.NONACTBUS.to_numpy() > 0),
                   ('other residential real estate (ORESRE > 0)', h.ORESRE.to_numpy() > 0),
                   ('nonresidential real estate (NNRESRE > 0)', h.NNRESRE.to_numpy() != 0),
                   ('none of these', (h.ACTBUS.to_numpy() <= 0) & (h.NONACTBUS.to_numpy() <= 0) & (h.ORESRE.to_numpy() <= 0) & (h.NNRESRE.to_numpy() == 0))]:
        ww = hw[s]
        def pc(m):
            return 100 * ww[m[s]].sum() / ww.sum()
        md(f'| {lab} | {ww.sum() / 1e6:.1f} | {pc((X4 < 0) & (X4 != -1)):.1f} | {pc((X4 == 0) | (X4 == -1)):.1f} | '
           f'{pc((X14 < 0) & (X14 != -1)):.1f} | {pc((X14 == 0) | (X14 == -1)):.1f} | {pc(X14 > 0):.1f} |')
    md()

    # ------------------------------------------------------------ SCF tax units
    md('## 7. SCF 2016 wave, tax units as the production builder splits them (split-off units dropped, as in E33)')
    md()
    u = pd.read_csv(f'{D}/scf2016_units33.csv.gz')
    s33 = t[(t.source == 'scf') & (t.year == 2015)][['record_id', 'married', 'wages', 'weight', 'business_income']]
    u = u.merge(s33, on='record_id', how='inner', suffixes=('', '_e33'), validate='1:1')
    assert len(u) == len(s33)
    assert np.abs(u.business_income + u.rent_royalty_estate - u.business_income_e33).max() < 0.01
    u['Y1'] = (u.record_id // 100).astype(int)
    hc = h.set_index('Y1').loc[u.Y1, ['X5704', 'X5714']]
    ucb = np.isin(hc.X5704.to_numpy(), [-2, -9]) & (u.business_income.to_numpy() < 0)
    ucr = np.isin(hc.X5714.to_numpy(), [-2, -9]) & (u.rent_royalty_estate.to_numpy() < 0)
    ub = np.where(ucb, -1e-9, u.business_income.to_numpy(float))
    ur = np.where(ucr, -1e-9, u.rent_royalty_estate.to_numpy(float))
    known = np.where(ucb, 0, ub) + np.where(ucr, 0, ur)
    usc = (ucb | ucr) & ~(known > 0)
    md(f'Units: {len(u):,} rows, {u.weight.sum() / 1e6:.1f} million weighted. Unit amounts carry the household\'s '
       f'-2 / -9 codes split by the builder\'s shares (e.g. -9 x 0.5); these are treated as losses of unknown size.')
    md()
    big_u = full_tables('SCF tax units', {'X5704 split': ub, 'X5714 split': ur,
                                          'X5704 + X5714 (step 33 business_income)': np.where(usc, -1e-9, known)},
                        u.weight.to_numpy(float), u.wages.to_numpy(float), u.married.to_numpy(),
                        {'X5704 split': ucb, 'X5714 split': ucr, 'X5704 + X5714 (step 33 business_income)': usc})
    big_u.to_csv(f'{OUT}/scf_units.csv')
    md(f'Step 33 as built (codes -2 and -9 kept as dollar amounts): share of units with a loss on X5704 + X5714 '
       f'{100 * u.weight[u.business_income_e33 < 0].sum() / u.weight.sum():.2f}%, of which with |loss| <= $9: '
       f'{100 * u.weight[(u.business_income_e33 < 0) & (u.business_income_e33 >= -9)].sum() / u.weight.sum():.2f} points.')
    md()

    # ------------------------------------------------------------ side by side, concept-matched
    md('## 8. Side by side on matched concepts (all units)')
    md()
    side = {
        'PUF filers: C + F': stats(puf_items['C + F'], w),
        'SCF households: X5704': stats(b, hw, bc),
        'SCF tax units: X5704 split': stats(ub, u.weight, ucb),
        'PUF filers: Schedule E total': stats(puf_items['E: Schedule E total (E02000)'], w),
        'SCF households: X5714': stats(r_, hw, rc),
        'SCF tax units: X5714 split': stats(ur, u.weight, ucr),
        'PUF filers: C + E + F': stats(tot, w),
        'PUF side with non-filers: C + E + F': stats(p.business_income, p.weight),
        'SCF households: X5704 + X5714': stats(hh_items['X5704 + X5714'], hw, sc_coded),
        'SCF tax units: X5704 + X5714': stats(np.where(usc, -1e-9, known), u.weight, usc),
    }
    df = table(side, 'Counts and totals', SHORT + ['loss % of unknown size'])
    table(side, 'Percentiles', PCT)
    df.to_csv(f'{OUT}/side_by_side.csv')

    # ------------------------------------------------------------ charts
    fig, axes = plt.subplots(1, 3, figsize=(13, 4.2))
    pairs = [('Schedules C + F vs X5704', 'PUF filers: C + F', 'SCF tax units: X5704 split'),
             ('Schedule E vs X5714', 'PUF filers: Schedule E total', 'SCF tax units: X5714 split'),
             ('C + E + F vs X5704 + X5714', 'PUF side with non-filers: C + E + F', 'SCF tax units: X5704 + X5714')]
    for ax, (ttl, a, bb) in zip(axes, pairs):
        for i, (lab, key) in enumerate([('PUF', a), ('SCF', bb)]):
            r = side[key]
            ax.bar(i - 0.0, r['profit %'], color='#2b6cb0', width=0.6)
            ax.bar(i, r['loss %'], bottom=r['profit %'], color='#c05621', width=0.6)
            ax.text(i, r['profit %'] / 2, f"{r['profit %']:.1f}%", ha='center', color='white', fontsize=9)
            ax.text(i, r['profit %'] + r['loss %'] + 0.5, f"loss {r['loss %']:.1f}%", ha='center', fontsize=9)
        ax.set_xticks([0, 1], ['PUF', 'SCF units'])
        ax.set_title(ttl, fontsize=10)
        ax.set_ylabel('% of units')
        ax.spines[['top', 'right']].set_visible(False)
    fig.suptitle('Share of units with a profit (blue) and a loss (orange), tax year 2015', fontsize=11)
    fig.tight_layout()
    fig.savefig(f'{OUT}/shares_profit_loss.png', dpi=130)
    plt.close(fig)
    fig, axes = plt.subplots(1, 2, figsize=(12, 4.2))
    for ax, lab in zip(axes, ['profit', 'loss size']):
        for key, col, ls in [('PUF filers: C + F', '#2b6cb0', '-'), ('SCF tax units: X5704 split', '#2b6cb0', '--'),
                             ('PUF filers: Schedule E total', '#c05621', '-'), ('SCF tax units: X5714 split', '#c05621', '--')]:
            ys = [side[key][f'{lab} P{int(q * 100)}'] for q in QS]
            ax.plot([int(q * 100) for q in QS], ys, ls, color=col, marker='o', label=key)
        ax.set_yscale('log')
        ax.set_xlabel('percentile among units with a ' + ('profit' if lab == 'profit' else 'loss'))
        ax.set_ylabel('dollars (log scale)')
        ax.set_title(('Profits' if lab == 'profit' else 'Size of losses') + ', 2015 dollars', fontsize=10)
        ax.spines[['top', 'right']].set_visible(False)
    axes[0].legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(f'{OUT}/percentiles.png', dpi=130)
    plt.close(fig)

    open(f'{OUT}/tables.md', 'w').write('\n'.join(MD) + '\n')
    say('wrote', f'{OUT}/tables.md')


if __name__ == '__main__':
    main()
