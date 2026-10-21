"""
28_wages_cg.py  (step 28: wages and capital gains, SCF against PUF; 28_WAGES_CG.md)

The same comparison as 25_totals.py, for wages and capital gains: the 2016
SCF wave (tax year 2015; table E4N's SCF rows, 2015 dollars) against the 2015
PUF filers, and the PUF filers plus the crude non-filers of step 23, with the
IRS's published 2015 totals (SOI Pub 1304 table 1.4, all returns).

  1. Totals and the share of units with any amount (capital gains: positive,
     zero and negative separately; net total, gains alone, losses alone).
  2. Holders by dollar range, and the SCF's count as a share of the PUF side's.
  3. Weighted percentiles among holders, and the ratio SCF over PUF.
  4. Capital gains only: the table's PUF variable (E01000, which turns out
     to be the Schedule D result alone: capital gain distributions reported
     directly on line 13, E01100, are not in it) against PUF alternatives
     built from the raw PUF:
       full line 13           = E01000 + E01100
       before the loss limit  = P22250 + P23250 + E01100
                                (Schedule D lines 7 and 15, which include
                                carryover losses and, for Schedule D filers,
                                capital gain distributions; plus distributions
                                reported directly on line 13 by others)
       lines 13 and 14        = the above + E01200 (other gains, Form 4797)
     with checks that E01000 is the line-13 figure these imply.

Missing values are counted and reported, never dropped.
Writes scratch checks/step28/wages_cg.md.
"""
import os

import numpy as np
import pandas as pd

R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
PUF_RAW = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/puf_2015.csv'
P = [0.1, 0.25, 0.5, 0.75, 0.9, 0.99, 0.999]
PN = ['p10', 'p25', 'p50', 'p75', 'p90', 'p99', 'p99.9']
WAGE_RANGES = [(0.01, 5_000), (5_000, 20_000), (20_000, 50_000), (50_000, 100_000), (100_000, 200_000),
               (200_000, 500_000), (500_000, 1_000_000), (1_000_000, np.inf)]
CG_RANGES = [(0.01, 100), (100, 500), (500, 2_000), (2_000, 10_000), (10_000, 50_000), (50_000, np.inf)]
LOSS_RANGES = [(0.01, 100), (100, 500), (500, 2_000), (2_000, 2_999.995), (2_999.995, 3_000.005),
               (3_000.005, 10_000), (10_000, 50_000), (50_000, np.inf)]


def wq(x, w, p):
    o = np.argsort(x, kind='stable')
    c = np.cumsum(w[o]) / w.sum()
    return np.array([x[o][min(np.searchsorted(c, q), len(x) - 1)] for q in p])


def money(v):
    return f'${v:,.0f}'


def rng_label(lo, hi):
    if hi - lo < 0.1:
        return 'exactly $3,000'
    if hi == 2_999.995:
        return '$2,000 to just under $3,000'
    if lo == 3_000.005:
        return 'just over $3,000 to $10,000'
    return f'${lo:,.0f} to ' + ('more' if hi == np.inf else f'${hi:,.0f}')


def md(df, fmt='.1f'):
    return df.to_markdown(index=False, floatfmt=fmt)


def main():
    os.makedirs(f'{R}/checks/step28', exist_ok=True)
    t = pd.read_parquet(f'{R}/data/stacked_E4N.parquet')
    out = ['# Step 28: wages and capital gains, SCF 2016 wave against the 2015 PUF', '',
           'Dollars in 2015 dollars. SCF: 2016 wave (tax year 2015), all units in table E4N (split-off '
           'units dropped), five implicates with their weights. Non-filers: the crude stopgap of step 23.', '']

    # ---- missing values: counted, not dropped
    na = []
    for s in ['scf', 'puf']:
        g = t[t.source == s]
        for c in ['weight', 'wages', 'capital_gains']:
            na.append({'rows': s, 'column': c, 'missing': int(g[c].isna().sum()), 'rows in file': len(g)})
    na = pd.DataFrame(na)
    out += ['## Missing values in the columns used', '', md(na), '']
    assert na['missing'].sum() == 0, 'missing values in E4N; see the table'

    scf = t[t.source == 'scf']
    puf = t[(t.source == 'puf') & (t.filer == 1)]
    both = t[t.source == 'puf']
    nf = t[(t.source == 'puf') & (t.filer == 0)]

    # ---- raw PUF capital gain items, joined to the table's PUF filer rows
    raw = pd.read_csv(PUF_RAW, usecols=['RECID', 'E00200', 'E01000', 'E01100', 'E01200', 'P22250', 'P23250'])
    raw = raw[raw.RECID < 999996]
    rawna = {c: int(raw[c].isna().sum()) for c in raw.columns}
    out += ['Raw PUF columns, missing values: ' + ', '.join(f'{k} {v}' for k, v in rawna.items()), '']
    assert sum(rawna.values()) == 0
    pj = puf[['record_id', 'weight', 'wages', 'capital_gains']].merge(
        raw.assign(record_id=raw.RECID.astype(float)), on='record_id', how='left', validate='one_to_one')
    assert len(pj) == len(puf) and pj.RECID.notna().all()
    assert np.allclose(pj.wages, pj.E00200) and np.allclose(pj.capital_gains, pj.E01000)
    w_ = pj.weight.to_numpy(float)
    e1000 = pj.E01000.to_numpy(float)
    e1100 = pj.E01100.to_numpy(float)
    sd = (pj.P22250 + pj.P23250).to_numpy(float)           # Schedule D lines 7 + 15, before the limit
    sched = sd != 0
    line13 = e1000 + e1100                                  # the full Form 1040 line 13
    unl = sd + e1100                                        # line 13 before the loss limit
    l1314 = unl + pj.E01200.to_numpy(float)
    WM = lambda m: float(w_[m].sum() / 1e6)
    # E01000 against Schedule D lines 7 + 15 with the loss limit, on returns without E01100
    lim3, lim15 = np.where(sd < -3000, -3000.0, sd), np.where(sd < -1500, -1500.0, sd)
    nod = e1100 == 0
    exact = nod & ((np.abs(lim3 - e1000) < 0.5) | (np.abs(lim15 - e1000) < 0.5))
    near = nod & ~exact & (np.abs(lim3 - e1000) <= np.maximum(50, 0.02 * np.maximum(np.abs(e1000), np.abs(lim3))))
    conf = nod & ~exact & ~near
    chk = [
        ('PUF filer records', len(pj)),
        ('returns with E01100 nonzero (distributions on line 13, no Schedule D), millions', WM(e1100 != 0)),
        ('... of which E01000 is zero, millions', WM((e1100 != 0) & (e1000 == 0))),
        ('E01100 total ($ billion)', float((w_ * e1100).sum() / 1e9)),
        ('returns with E01100 nonzero and Schedule D amounts too, millions', WM((e1100 != 0) & sched)),
        ('returns without E01100: E01000 equals Schedule D lines 7 + 15 after the limit, millions', WM(exact)),
        ('... within rounding ($50 or 2%), millions', WM(near)),
        ('... in conflict, millions', WM(conf)),
        ('conflicts: Schedule D sum above zero but E01000 below zero, millions', WM(conf & (sd > 0) & (e1000 < 0))),
        ('conflicts: Schedule D sum zero but E01000 nonzero, millions', WM(conf & (sd == 0))),
        ('conflicts: other, millions', WM(conf & ~((sd > 0) & (e1000 < 0)) & (sd != 0))),
        ('conflicts: weighted E01000 dollars ($ billion)', float((w_ * e1000)[conf].sum() / 1e9)),
        ('conflicts: weighted Schedule D dollars after the limit ($ billion)', float((w_ * lim3)[conf].sum() / 1e9)),
        ('E01200 (other gains, Form 4797) nonzero, millions', WM(pj.E01200.to_numpy() != 0)),
        ('E01200 total ($ billion)', float((w_ * pj.E01200).sum() / 1e9)),
    ]
    out += ['## Capital gains: checks on the PUF items (weighted returns)', '',
            md(pd.DataFrame(chk, columns=['check', 'value']), '.2f'), '']

    # PUF alternatives, filers; non-filers carry zero capital gains (the CPS does not ask)
    nfz = np.zeros(len(nf))
    cg_alt = {'line 13 (E01000, the table)': (e1000, np.concatenate([e1000, nfz])),
              'full line 13 (E01000 + E01100)': (line13, np.concatenate([line13, nfz])),
              'before the loss limit': (unl, np.concatenate([unl, nfz])),
              'lines 13 and 14, before the limit': (l1314, np.concatenate([l1314, nfz]))}
    w_both = np.concatenate([w_, nf.weight.to_numpy(float)])

    irs = pd.read_csv(f'{R}/targets/soi_2015_validation.csv')
    irs = irs[(irs.year == 2015) & (irs.table == 1.4) & (irs.filing_status == 'all') & (irs.agi_bracket_label == 'all')]
    get = lambda var, stat: float(irs[(irs.variable == var) & (irs.statistic == stat)]['value'].iloc[0])
    n_ret = get('returns', 'count')

    # split-off units dropped from the SCF table (01_build_table.R checks)
    ck = pd.read_csv(f'{R}/data/checks_r.csv')
    so_w = float(ck[(ck.group == 'scf') & (ck.key == 'split_off_weighted_millions')].value.iloc[0])
    so_wages = float(ck[(ck.group == 'scf_split_off_weighted_total_billions') & (ck.key == 'wages')].value.iloc[0])

    sx = lambda f, v: (f[v].to_numpy(float), f['weight'].to_numpy(float))

    # ================= wages
    rows = []
    for name, f in [('SCF 2016 wave, all units', scf), ('PUF 2015, filers', puf), ('PUF 2015 + crude non-filers', both)]:
        x, w = sx(f, 'wages')
        rows.append({'file': name, 'units (millions)': w.sum() / 1e6, 'total ($ billion)': (x * w).sum() / 1e9,
                     'units with any (millions)': w[x > 0].sum() / 1e6,
                     'share with any (%)': 100 * w[x > 0].sum() / w.sum(),
                     'units below zero (millions)': w[x < 0].sum() / 1e6})
    rows.append({'file': 'SCF split-off units (dropped from the table)', 'units (millions)': so_w,
                 'total ($ billion)': so_wages, 'units with any (millions)': np.nan,
                 'share with any (%)': np.nan, 'units below zero (millions)': np.nan})
    rows.append({'file': 'IRS 2015, all returns', 'units (millions)': n_ret,
                 'total ($ billion)': get('wages', 'amount') / 1e9,
                 'units with any (millions)': get('wages', 'count') / 1e6,
                 'share with any (%)': 100 * get('wages', 'count') / n_ret, 'units below zero (millions)': np.nan})
    rows[-1]['units (millions)'] /= 1e6
    x, w = sx(nf, 'wages')
    out += ['## Wages: totals', '', md(pd.DataFrame(rows)), '',
            f'Crude non-filers alone: {w.sum() / 1e6:.1f} million units, ${(x * w).sum() / 1e9:.1f} billion of wages, '
            f'{100 * w[x > 0].sum() / w.sum():.1f}% with any.', '']

    rr = []
    for lo, hi in WAGE_RANGES:
        row = {'range': rng_label(lo, hi)}
        for name, f in [('SCF', scf), ('PUF filers', puf), ('PUF + non-filers', both)]:
            x, w = sx(f, 'wages')
            m = (x >= lo) & (x < hi)
            row[f'{name}: units (millions)'] = w[m].sum() / 1e6
            row[f'{name}: $ billion'] = (x[m] * w[m]).sum() / 1e9
        row['SCF as % of PUF filers'] = 100 * row['SCF: units (millions)'] / row['PUF filers: units (millions)']
        row['SCF as % of PUF + non-filers'] = 100 * row['SCF: units (millions)'] / row['PUF + non-filers: units (millions)']
        rr.append(row)
    out += ['## Wages: holders by dollar range', '', md(pd.DataFrame(rr), '.2f'), '']

    pr, qs = [], {}
    for name, f in [('SCF holders', scf), ('PUF holders (filers)', puf), ('PUF holders (filers + non-filers)', both)]:
        x, w = sx(f, 'wages')
        m = x > 0
        qs[name] = wq(x[m], w[m], P)
        pr.append({'who': name, **{k: money(v) for k, v in zip(PN, qs[name])}})
    for ref in ['PUF holders (filers)', 'PUF holders (filers + non-filers)']:
        pr.append({'who': f'ratio SCF / {ref}', **{k: f'{a / b:.2f}' for k, a, b in zip(PN, qs['SCF holders'], qs[ref])}})
    out += ['## Wages: weighted percentiles among holders (wages above zero)', '', pd.DataFrame(pr).to_markdown(index=False), '']

    # ================= capital gains
    def cg_row(name, x, w):
        return {'file': name, 'units (millions)': w.sum() / 1e6,
                'net total ($ billion)': (x * w).sum() / 1e9,
                'gains alone ($ billion)': (np.where(x > 0, x, 0) * w).sum() / 1e9,
                'losses alone ($ billion)': (np.where(x < 0, x, 0) * w).sum() / 1e9,
                'positive (%)': 100 * w[x > 0].sum() / w.sum(), 'zero (%)': 100 * w[x == 0].sum() / w.sum(),
                'negative (%)': 100 * w[x < 0].sum() / w.sum(),
                'positive (millions)': w[x > 0].sum() / 1e6, 'negative (millions)': w[x < 0].sum() / 1e6}

    x, w = sx(scf, 'capital_gains')
    rows = [cg_row('SCF 2016 wave, all units', x, w)]
    for k, (xf, xb) in cg_alt.items():
        rows.append(cg_row(f'PUF filers, {k}', xf, w_))
        if k.startswith('full line 13'):
            rows.append(cg_row(f'PUF + crude non-filers, {k}', xb, w_both))
    gpos = get('sch_d_net_gain', 'amount') + get('capital_gain_distributions_no_sch_d', 'amount')
    npos = get('sch_d_net_gain', 'count') + get('capital_gain_distributions_no_sch_d', 'count')
    nneg = get('sch_d_net_loss', 'count')
    rows.append({'file': 'IRS 2015, all returns, line 13', 'units (millions)': n_ret / 1e6,
                 'net total ($ billion)': get('net_capital_gain', 'amount') / 1e9,
                 'gains alone ($ billion)': gpos / 1e9, 'losses alone ($ billion)': -get('sch_d_net_loss', 'amount') / 1e9,
                 'positive (%)': 100 * npos / n_ret, 'zero (%)': 100 * (n_ret - npos - nneg) / n_ret,
                 'negative (%)': 100 * nneg / n_ret, 'positive (millions)': npos / 1e6, 'negative (millions)': nneg / 1e6})
    out += ['## Capital gains: totals', '',
            'IRS positive count = returns with a Schedule D net gain plus returns with capital gain distributions '
            'and no Schedule D (the two do not overlap).', '', md(pd.DataFrame(rows)), '']

    # SCF losses: how many beyond the $3,000 limit
    m = x < 0
    out += [f'SCF units with a loss: {w[m].sum() / 1e6:.2f} million; with a loss larger than $3,000: '
            f'{w[x < -3000].sum() / 1e6:.2f} million ({100 * w[x < -3000].sum() / max(w[m].sum(), 1):.0f}% of SCF loss units); '
            f'exactly -$3,000: {w[np.abs(x + 3000) < 0.005].sum() / 1e6:.2f} million.', '']

    def range_table(ranges, sign):
        rr = []
        for lo, hi in ranges:
            row = {'range': rng_label(lo, hi)}
            xs, ws = sx(scf, 'capital_gains')
            a = sign * xs
            mm = (a >= lo) & (a < hi)
            row['SCF: units (millions)'] = ws[mm].sum() / 1e6
            row['SCF: $ billion'] = sign * (xs[mm] * ws[mm]).sum() / 1e9
            for k, lab in [('line 13 (E01000, the table)', 'PUF E01000'), ('full line 13 (E01000 + E01100)', 'PUF full line 13'),
                           ('before the loss limit', 'PUF before limit')]:
                xb = cg_alt[k][1]
                a = sign * xb
                mm = (a >= lo) & (a < hi)
                row[f'{lab}: units (millions)'] = w_both[mm].sum() / 1e6
                row[f'{lab}: $ billion'] = sign * (xb[mm] * w_both[mm]).sum() / 1e9
                d = row[f'{lab}: units (millions)']
                row[f'SCF as % of {lab}'] = 100 * row['SCF: units (millions)'] / d if d > 0 else np.nan
            rr.append(row)
        return pd.DataFrame(rr)

    out += ['## Capital gains: holders of gains by dollar range (PUF side: filers + non-filers)', '',
            md(range_table(CG_RANGES, 1), '.2f'), '',
            '## Capital gains: holders of losses by size of loss (PUF side: filers + non-filers)', '',
            md(range_table(LOSS_RANGES, -1), '.2f'), '']

    for title, sign in [('gains (amounts above zero)', 1), ('losses (size of loss)', -1)]:
        pr, qs = [], {}
        xs, ws = sx(scf, 'capital_gains')
        mm = sign * xs > 0
        qs['SCF'] = wq(sign * xs[mm], ws[mm], P)
        pr.append({'who': 'SCF', **{k: money(v) for k, v in zip(PN, qs['SCF'])}})
        for k, lab in [('line 13 (E01000, the table)', 'PUF filers, E01000'),
                       ('full line 13 (E01000 + E01100)', 'PUF filers, full line 13'),
                       ('before the loss limit', 'PUF filers, before the limit'),
                       ('lines 13 and 14, before the limit', 'PUF filers, lines 13 and 14')]:
            xf = cg_alt[k][0]
            mm = sign * xf > 0
            qs[lab] = wq(sign * xf[mm], w_[mm], P)
            pr.append({'who': lab, **{k_: money(v) for k_, v in zip(PN, qs[lab])}})
        for lab in list(qs)[1:]:
            pr.append({'who': f'ratio SCF / {lab}', **{k: (f'{a / b:.2f}' if b else 'n/a')
                                                         for k, a, b in zip(PN, qs['SCF'], qs[lab])}})
        out += [f'## Capital gains: weighted percentiles among holders, {title}', '',
                '(Non-filers hold none, so filers alone and filers + non-filers are the same here.)', '',
                pd.DataFrame(pr).to_markdown(index=False), '']

    text = '\n'.join(out)
    open(f'{R}/checks/step28/wages_cg.md', 'w').write(text + '\n')
    print(text)


if __name__ == '__main__':
    main()
