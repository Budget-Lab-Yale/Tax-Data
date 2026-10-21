"""Post-training: the generated stack reweighted to 2019.

The start. Each record's importance weight from generation, scaled so filers
sum to SOI's 2019 return count and non-filers to the 2019 non-filer pool;
then filers are pre-scaled within joint and non-joint returns to SOI's 2019
counts of each (Table 1.2), so known population change is in the start.

The tilt. New weight = start weight x exp(sum over targets of a multiplier
times the record's contribution): the smallest move, in Kullback-Leibler
divergence, that meets the targets (Newton's method on the dual). Targets:
  - 136 IRS targets (SOI 2019 Table 1.4, filers, exact): in each AGI bracket
    (SOI's, the top ones merged until each would hold 500 records in a
    200,000-record file) the number of returns, and the number with and the
    amount of wages, taxable interest, ordinary dividends and Schedule D net
    capital gain; plus the number of joint returns.
  - the non-filer count (exact).
  - 140 business targets (SOI 2019 Table 1.4, filers, exact; 2% if exact
    fails): six families, the number of returns and dollars with a net income
    and with a net loss, in six AGI groups (merged where SOI combined cells).
    A return's family net is the signed sum of the family's PUF parts.
  - 8 wealth targets (SCF 2019, everyone, 2% tolerance): units and net worth
    above the SCF's 99th, 90th and 50th percentile lines, total net worth,
    units above $1 million. Counts are the SCF's share above each line times
    the file's units (returns plus non-filers); dollars are the SCF's own.
    Net worth is computed from the drawn balance sheet.
If the exact tilt fails even with the business fallback, every target is set
to 2% so the run still writes its tables; summary.md says so.

Writes, in common.TILT: file_2019.parquet (every record, 2019 dollars, net
worth in 2015 and 2019 dollars, each balance-sheet item and unrealized gain in
2019 dollars, weights before and after), fit.csv (every target, before and
after), summary.md.
"""
import os

import numpy as np
import pandas as pd

import common
import dollars as D
import targets as TG

S = common.SETTINGS['tilt']
NW_2016_WAVE_TO_2015 = 1.2493               # the 2016 wave's 2022-dollar factor, used as "2015 dollars"
NW_TO_2019 = 1.2493059 / 1.1592053           # net worth in those dollars to 2019 dollars
FAMILIES = {  # family: (income parts, loss parts stored positive)
    'sch_c': (['sch_c'], []),
    'pship_scorp': (['part_pass_inc', 'part_nonpass_inc', 'scorp_pass_inc', 'scorp_nonpass_inc'],
                    ['part_pass_loss', 'part_nonpass_loss', 'part_179', 'scorp_pass_loss', 'scorp_nonpass_loss',
                     'scorp_179']),
    'rent_royalty': (['rent_inc'], ['rent_loss']),
    'estate_trust': (['estate_inc'], ['estate_loss']),
    'farm': (['farm'], []),
    'farm_rental': (['farm_rent'], [])}
IRS_ITEMS = {'wages': ({'wages': 1}, ['wages']),
             'interest': ({'taxable_interest': 1}, ['taxable_interest']),
             'dividends': ({'ordinary_dividends': 1}, ['ordinary_dividends']),
             'capital_gains': ({'sch_d_net_gain': 1, 'sch_d_net_loss': -1}, ['sch_d_net_gain', 'sch_d_net_loss'])}


def agi(df):
    return df[['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']].to_numpy(float).sum(1)


def wquantile(x, w, probs):
    """Smallest x whose cumulative weight share reaches each probability."""
    o = np.argsort(x, kind='stable')
    xs, F = x[o], np.cumsum(w[o]) / w.sum()
    return xs[np.minimum(np.searchsorted(F, np.asarray(probs) - 1e-12, side='left'), len(xs) - 1)]


# ---- targets -------------------------------------------------------------------
def irs_brackets(soi):
    """SOI's 2019 AGI brackets, the top ones merged until each would hold enough records."""
    t = soi[(soi.year == 2019) & (soi.table.astype(str) == '1.4') & (soi.filing_status == 'all')
            & (soi.label != 'all')]
    ret = t[t.variable == 'returns'].sort_values('lo')
    out = list(ret[['lo', 'hi', 'label']].itertuples(index=False, name=None))
    count, total = dict(zip(ret['lo'], ret['value'])), ret['value'].sum()
    while len(out) > 1:
        lo, hi, label = out[-1]
        if sum(c for l_, c in count.items() if l_ >= lo) / total * S['bracket_file_size'] >= S['min_top_bracket_records']:
            break
        lo2, _, label2 = out[-2]
        out = out[:-2] + [(lo2, hi, f'{label2.split(" under")[0]} or more')]
    return out, t


def irs_targets(soi, df):
    """The 136 IRS targets and each record's contribution to them."""
    brackets, t = irs_brackets(soi)
    a = agi(df)
    cells, cols = [], []
    for lo, hi, label in brackets:
        s = t[(t.lo >= lo) & (t.hi <= hi)].groupby(['variable', 'statistic'])['value'].sum()
        inside = ((a >= lo) & (a < hi)).astype(float)
        cells.append({'group': label, 'target': 'returns', 'value': s[('returns', 'count')]})
        cols.append(inside)
        for item, (amount_rows, count_rows) in IRS_ITEMS.items():
            x = df[item].to_numpy(float)
            cells.append({'group': label, 'target': f'{item} count', 'value': sum(s[(c, 'count')] for c in count_rows)})
            cells.append({'group': label, 'target': f'{item} amount',
                          'value': sum(sg * s[(r, 'amount')] for r, sg in amount_rows.items())})
            cols += [inside * (x != 0), inside * x]
    joint = soi[(soi.year == 2019) & (soi.table.astype(str) == '1.2') & (soi.filing_status == 'joint')
                & (soi.label == 'all') & (soi.variable == 'returns')]['value'].item()
    cells.append({'group': 'all', 'target': 'joint returns', 'value': joint})
    cols.append(df['married'].to_numpy(float))
    return pd.DataFrame(cells), np.column_stack(cols)


def merge_groups(groups, cells):
    """AGI groups merged where a published cell crosses a group edge."""
    groups = list(groups)
    i = 0
    while i < len(groups) - 1:
        edge = groups[i][1]
        if any(lo < edge < hi for lo, hi, _ in cells):
            groups[i:i + 2] = [(groups[i][0], groups[i + 1][1], f'{groups[i][2]} to {groups[i + 1][2]}')]
            i = 0
        else:
            i += 1
    return groups


def business_targets(soi, df):
    """The business targets and each record's contribution; cells SOI publishes as zero, or that no record can reach, left out."""
    groups = [(-np.inf if lo is None else lo, np.inf if hi is None else hi, lab) for lo, hi, lab in S['business_agi_groups']]
    a = agi(df)
    cells, cols = [], []
    for fam, (inc, loss) in FAMILIES.items():
        net = df[inc].to_numpy(float).sum(1) - (df[loss].to_numpy(float).sum(1) if loss else 0.0)
        for side, x in [('net_income', np.where(net > 0, net, 0.0)), ('net_loss', np.where(net < 0, -net, 0.0))]:
            pc = {st: TG.published_cells(soi, 2019, f'{fam}_{side}', st) for st in ('count', 'amount')}
            for lo, hi, lab in merge_groups(groups, pc['count'] + pc['amount']):
                inside = (a >= lo) & (a < hi)
                for st, contrib in [('count', inside * (x > 0)), ('amount', inside * x)]:
                    cells.append({'group': lab, 'target': f'{fam} {side} {st}',
                                  'value': sum(v for l_, h_, v in pc[st] if l_ >= lo and h_ <= hi)})
                    cols.append(contrib.astype(float))
    cells, F = pd.DataFrame(cells), np.column_stack(cols)
    use = (cells['value'].to_numpy() != 0) & (F > 0).any(0)
    return cells[use].reset_index(drop=True), F[:, use]


def wealth_targets(x, file_units):
    """The eight SCF 2019 wealth targets (counts on the file's units) and each record's contribution."""
    u = pd.read_parquet(f'{common.DATA}/scf_units_2019.parquet')
    x19, w19 = u['net_worth_2015_dollars'].to_numpy(float), u['weight'].to_numpy(float)
    threshold = 1e6 * u['asset_factor'].iloc[0] / NW_2016_WAVE_TO_2015      # $1 million in 2019 dollars
    scale = file_units / w19.sum()
    cells, cols = [], []
    for q, lab in zip(wquantile(x19, w19, [0.99, 0.90, 0.50]), ['99th', '90th', '50th']):
        above = (x > q).astype(float)
        cells += [{'group': f'above the SCF {lab} percentile', 'target': 'units', 'value': w19[x19 > q].sum() * scale},
                  {'group': f'above the SCF {lab} percentile', 'target': 'net worth', 'value': (x19 * w19)[x19 > q].sum()}]
        cols += [above, x * above]
    cells += [{'group': 'all', 'target': 'net worth', 'value': (x19 * w19).sum()},
              {'group': 'above $1 million (2019 dollars)', 'target': 'units', 'value': w19[x19 > threshold].sum() * scale}]
    cols += [x, (x > threshold).astype(float)]
    return pd.DataFrame(cells), np.column_stack(cols)


def nonfiler_count():
    p = pd.read_csv(common.RAW['nonfiler_pool'].format(year=2019), usecols=['tax_year', 'weight', 'filer'])
    assert (p['tax_year'] == 2019).all() and (p['filer'] == 0).all()
    return float(p['weight'].sum())


# ---- the solver ------------------------------------------------------------------
def tilt(w, F, T, rel_tol, max_iter=200):
    """Newton's method on the dual. rel_tol per target: 0 exact, else a Gaussian
    penalty with that standard deviation as a share of the target."""
    W = w.sum()
    p = w / W
    X = F * (W / np.abs(T))
    t = np.sign(T)
    s2 = np.asarray(rel_tol, float) ** 2
    lam = np.zeros(len(T))

    def dual(lam):
        e = p * np.exp(X @ lam)
        return e.sum() - lam @ t + 0.5 * (s2 * lam ** 2).sum(), e
    D_, e = dual(lam)
    for it in range(max_iter):
        g = X.T @ e - t + s2 * lam
        if np.abs(g).max() < 1e-11:
            break
        step = np.linalg.solve(X.T @ (X * e[:, None]) + np.diag(s2) + 1e-12 * np.eye(len(T)), -g)
        a = 1.0
        while True:
            D_new, e_new = dual(lam + a * step)
            if np.isfinite(D_new) and D_new <= D_ + 1e-4 * a * (g @ step):
                break
            a /= 2
            if a < 1e-12:
                return W * e, False
        lam, D_, e = lam + a * step, D_new, e_new
    return W * e, bool(np.abs(X.T @ e - t + s2 * lam).max() < 1e-7)


def cost(w0, w1):
    p0, p1 = w0 / w0.sum(), w1 / w1.sum()
    ess = lambda w: w.sum() ** 2 / (w ** 2).sum()
    f = w1 / w0
    pos = p1 > 0
    return {'KL move (nats)': float(np.sum(p1[pos] * np.log(p1[pos] / p0[pos]))),
            'effective sample kept (%)': 100 * ess(w1) / ess(w0),
            'weight factor, 1st percentile': np.quantile(f, 0.01), 'weight factor, 99th percentile': np.quantile(f, 0.99),
            'records moved more than tenfold up': int((f > 10).sum()),
            'records moved more than tenfold down': int((f < 0.1).sum())}


# ---- net worth summary -------------------------------------------------------------
def top_share(x, w, p):
    o = np.argsort(-x, kind='stable')
    xs, ws = x[o], w[o]
    cum = np.cumsum(ws)
    take = np.clip(p * w.sum() - (cum - ws), 0, ws)
    return (xs * take).sum() / (x * w).sum()


def net_worth_table(files):
    rows = {}
    for name, (df, x, w) in files.items():
        r = {'units (millions)': w.sum() / 1e6, 'total ($ trillion)': (x * w).sum() / 1e12,
             'median ($)': wquantile(x, w, [0.5])[0], 'top 1% share (%)': 100 * top_share(x, w, 0.01)}
        wage = pd.cut(df['wages'], [-np.inf, 0, 25e3, 50e3, 100e3, 200e3, np.inf],
                      labels=['0', '1-25k', '25-50k', '50-100k', '100-200k', '200k+']).astype(str).to_numpy()
        for g in ['0', '1-25k', '25-50k', '50-100k', '100-200k', '200k+']:
            m = wage == g
            r[f'mean, wages {g} ($)'] = (x[m] * w[m]).sum() / w[m].sum()
        rows[name] = r
    return pd.DataFrame(rows)


def main():
    os.makedirs(common.TILT, exist_ok=True)
    soi = TG.load()
    raw = pd.read_parquet(f'{common.GEN}/stack.parquet')
    g19 = D.draw_dollars(raw, D.load_maps(2019))
    g15 = D.draw_dollars(raw, D.load_maps(2015))
    x = g15['net_worth'].to_numpy(float)
    filer = g19['filer'].to_numpy() == 1
    imp = g19['weight'].to_numpy(float)

    # the start
    returns = soi[(soi.year == 2019) & (soi.table.astype(str) == '1.4') & (soi.label == 'all')
                  & (soi.variable == 'returns')]['value'].item()
    n_nonfilers = nonfiler_count()
    w0 = np.where(filer, imp * returns / imp[filer].sum(), imp * n_nonfilers / imp[~filer].sum())
    w_plain = w0.copy()
    if S['prescale'] == 'filing_status':
        for married, status in [(1, 'joint'), (0, 'nonjoint')]:
            g = filer & (g19['married'].to_numpy() == married)
            count = soi[(soi.year == 2019) & (soi.table.astype(str) == '1.2') & (soi.filing_status == status)
                        & (soi.label == 'all') & (soi.variable == 'returns')]['value'].item()
            w0[g] *= count / w0[g].sum()

    # the targets
    irs, F_irs = irs_targets(soi, g19)
    bus, F_bus = business_targets(soi, g19)
    wl, F_w = wealth_targets(x, returns + n_nonfilers)
    F_irs, F_bus = F_irs * filer[:, None], F_bus * filer[:, None]
    cells = pd.concat([irs.assign(kind='IRS'), pd.DataFrame([{'group': 'all', 'target': 'non-filers',
                                                               'value': n_nonfilers, 'kind': 'non-filers'}]),
                       bus.assign(kind='business'), wl.assign(kind='wealth')], ignore_index=True)
    F = np.column_stack([F_irs, (~filer).astype(float), F_bus, F_w])
    T = cells['value'].to_numpy(float)
    tol = np.r_[np.zeros(len(irs) + 1 + len(bus)), np.full(len(wl), S['wealth_tolerance'])]
    w1, ok = tilt(w0, F, T, tol)
    form = 'exact'
    if not ok:
        tol[len(irs) + 1:len(irs) + 1 + len(bus)] = S['business_fallback_tolerance']
        w1, ok = tilt(w0, F, T, tol)
        form = f"{100 * S['business_fallback_tolerance']:.0f}% tolerance"
    if not ok:   # keep going so the run still writes its tables, and say so in the summary
        tol[:] = np.maximum(tol, 0.02)
        w1, ok = tilt(w0, F, T, tol)
        form = 'every target at 2% tolerance (exact did not converge)' if ok else \
            'DID NOT CONVERGE: weights are the last iterate'
        common.say('tilt fallback:', form)

    # outputs
    fit = cells.assign(before=F.T @ w0, after=F.T @ w1)
    fit['after / target'] = fit['after'] / fit['value']
    fit.to_csv(f'{common.TILT}/fit.csv', index=False)
    out = g19.assign(net_worth_2015_dollars=x, net_worth_2019_dollars=x * NW_TO_2019,
                     weight_start=w0, weight=w1).drop(columns=['net_worth'])
    for c in list(D.NET_WORTH_PARTS) + [v for v in D._META['scf_amounts'] if v.startswith('w_kg_')]:
        out[c + '_2019_dollars'] = g15[c].to_numpy(float) * NW_TO_2019   # drawn in the 2016 wave's dollars
        out = out.drop(columns=[c])
    out.to_parquet(f'{common.TILT}/file_2019.parquet', index=False)
    u = pd.read_parquet(f'{common.DATA}/scf_units_2019.parquet')
    nw = net_worth_table({
        'SCF 2019': (u.assign(wages=u['wages']), u['net_worth_2015_dollars'].to_numpy(float) * NW_TO_2019,
                     u['weight'].to_numpy(float)),
        'before the tilt': (g19, x * NW_TO_2019, w0), 'after the tilt': (g19, x * NW_TO_2019, w1)})
    summary = pd.Series({'targets': len(T), 'business targets': f'{len(bus)}, {form}',
                         'largest miss on exact IRS targets (%)': 100 * np.abs(fit['after / target'][:len(irs) + 1] - 1).max(),
                         **cost(w0, w1), 'KL move from the unscaled start (nats)': cost(w_plain, w1)['KL move (nats)']})
    by_kind = fit.groupby('kind')['after / target'].agg(['min', 'max'])
    text = ['# The 2019 tilt', '', summary.to_frame('').to_markdown(floatfmt=',.4f'), '',
            '## Targets after the tilt (ratio to target, range by kind)', '', by_kind.to_markdown(floatfmt='.4f'), '',
            '## Net worth (nominal 2019 dollars)', '', nw.to_markdown(floatfmt=',.1f'), '']
    open(f'{common.TILT}/summary.md', 'w').write('\n'.join(text))
    common.say('\n'.join(text))


if __name__ == '__main__':
    main()
