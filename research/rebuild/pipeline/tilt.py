"""Post-training: the generated stack reweighted to the target year (settings "years"; 2019 below).

The start. Each record's importance weight from generation, scaled so filers
sum to SOI's 2019 return count and non-filers to the CPS's expected
non-filers in 2019 (the sum over its units of weight x (1 - filer_p), the
same count the generated non-filer batch is sized by);
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
  - 8 non-filer people targets (exact; 2% if exact fails even with the
    business and age fallbacks): the people in non-filer units that step 1's
    calibration hit (settings nonfiler_targets_dir, its
    <year>_nonfilers_vs_targets.csv, read, not recomputed): heads and spouses
    in 7 adult age bands (18-25, 26-34, 35-44, 45-54, 55-64, 65-74, 75+) and
    children under 17. The file's side counts each head and spouse by their
    own exact age (step 1 bands a spouse by the head's age; in the CPS units
    the two ways differ by at most 0.09 million in a band), and children
    under 17 among the head, the spouse and the first three dependents (the
    youngest three; children beyond the third are not counted, about 0.13
    million in 2019).
  - age targets (SOI 2019 Table 1.6, filers, exact; 2% if exact fails even
    with the business fallback): the number of returns by the primary
    filer's age range, 26-34 to 65+, in joint and in non-joint returns
    (settings age_level "filing_status"; "all" for the five totals; "none"
    for no age targets). Under 26
    is not a target of its own: it is what the total and the joint count
    leave, and a sixth target would repeat them. Table 1.6 counts every
    return, a dependent filer's under its own age; so the file's side counts
    every filer return too, its dependent filers under 26 in under 26 (by
    the remainder) and its dependent filers 26 and over, whose exact age the
    file does not carry, spread over the five ranges 26 and over in
    proportion to Table 1.6's returns of their status.
  - business targets (SOI 2019 Table 1.4, filers, exact; 2% if exact
    fails): five families, the number of returns and dollars with a net
    income and with a net loss, in six AGI groups (merged where SOI combined
    cells). A return's family net is the signed sum of the family's PUF parts.
    Estates and trusts are left out until the targeting overhaul: their tiny
    cells could not always be met.
  - 8 wealth targets (SCF 2019, 2% tolerance): the share of units above the
    SCF's 99th, 90th and 50th percentile lines and above $1 million, among the
    file's units other than dependent filers (which hold no wealth); net
    worth above each percentile line and in total, in the SCF's own dollars.
    Net worth is computed from the drawn balance sheet.
  - 104 consumption targets (CEX 2019, within a band of consumption_band, 5%, of each average; free inside
    it): each category's average
    per unit, over all units and in each of 12 cells, the six adult age ranges
    of the primary filer by married (a joint unit) or not; separate returns are
    not married, on both sides. Dependent filers are left out on both sides.
    The overall averages are kept because the cells do not imply them: the
    file's units fall into the cells in other shares than the CEX's.
A share or an average is written as a linear condition on the weights
(average_condition), so it holds whatever the tilt does to the number of
units in its group.
If the exact tilt fails even with the business, age and non-filer fallbacks, every target is set
to 2% so the run still writes its tables; summary.md says so.

Writes, in common.TILT: file_<target year>.parquet (every record, every dollar
column in nominal target-year dollars, net worth and its parts included;
weights before and after), fit.csv (every target, before and after; for shares
and averages, the share or average itself), summary.md.
"""
import os

import numpy as np
import pandas as pd

import common
import dollars as D
import targets as TG
from common import TARGET_YEAR as Y, wquantile

S = common.SETTINGS['tilt']
FAMILIES = {f: (v['income'], v['loss']) for f, v in TG.FAMILIES.items() if v['target']}   # targeted ones
IRS_ITEMS = {'wages': ({'wages': 1}, ['wages']),
             'interest': ({'taxable_interest': 1}, ['taxable_interest']),
             'dividends': ({'ordinary_dividends': 1}, ['ordinary_dividends']),
             'capital_gains': ({'sch_d_net_gain': 1, 'sch_d_net_loss': -1}, ['sch_d_net_gain', 'sch_d_net_loss'])}


# ---- targets -------------------------------------------------------------------
def irs_brackets(soi):
    """SOI's AGI brackets, the top ones merged until each would hold enough records."""
    t = soi[(soi.year == Y) & (soi.table.astype(str) == '1.4') & (soi.filing_status == 'all')
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
    a = D.agi(df)
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
    cells.append({'group': 'all', 'target': 'joint returns', 'value': TG.returns(soi, Y, '1.2', 'joint')})
    cols.append(df['married'].to_numpy(float))
    return pd.DataFrame(cells), np.column_stack(cols)


def age_targets(df):
    """Returns by the primary filer's age range, 26-34 to 65+ (SOI Table 1.6), in joint and non-joint returns or over
    all statuses (settings age_level), and each record's contribution. Under 26 is left to the total returns and the
    joint count. Table 1.6 counts dependent filers' own returns under their own age: the file's dependent filers under
    26 fall in under 26, and those 26 and over (no exact age) are spread over the five ranges by Table 1.6's shares
    in their status."""
    t16 = TG.table_1_6(Y)
    rng, married = df['age_range'].to_numpy(), df['married'].to_numpy()
    assert ((rng >= 0) & (rng < common.N_RANGES)).all(), 'a record with no age range'
    nonjoint = ['single', 'separate', 'head_of_household']
    groups = {'filing_status': [('joint', married == 1, ['joint']), ('non-joint', married == 0, nonjoint)],
              'all': [('all', np.ones(len(df), bool), ['joint'] + nonjoint)], 'none': []}[S['age_level']]
    over_26 = range(1, common.ADULT_RANGES)
    cells, cols = [], []
    for lab, member, statuses in groups:
        n = np.array([sum(t16[(s, k)] for s in statuses) for k in over_26])
        dep_26 = member & (rng == common.DEPENDENT_RANGES[-1])   # dependent filers 26 and over
        for k, v in zip(over_26, n):
            cells.append({'group': f'{lab}, primary {AGES[k]}', 'target': 'returns', 'value': v})
            cols.append((member & (rng == k)) + dep_26 * v / n.sum())
    return pd.DataFrame(cells, columns=['group', 'target', 'value']), np.column_stack(cols) if cols else np.zeros((len(df), 0))


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
    a = D.agi(df)
    cells, cols = [], []
    for fam, (inc, loss) in FAMILIES.items():
        net = df[inc].to_numpy(float).sum(1) - (df[loss].to_numpy(float).sum(1) if loss else 0.0)
        for side, x in [('net_income', np.where(net > 0, net, 0.0)), ('net_loss', np.where(net < 0, -net, 0.0))]:
            pc = {st: TG.published_cells(soi, Y, f'{fam}_{side}', st) for st in ('count', 'amount')}
            for lo, hi, lab in merge_groups(groups, pc['count'] + pc['amount']):
                inside = (a >= lo) & (a < hi)
                for st, contrib in [('count', inside * (x > 0)), ('amount', inside * x)]:
                    cells.append({'group': lab, 'target': f'{fam} {side} {st}',
                                  'value': sum(v for l_, h_, v in pc[st] if l_ >= lo and h_ <= hi)})
                    cols.append(contrib.astype(float))
    cells, F = pd.DataFrame(cells), np.column_stack(cols)
    use = (cells['value'].to_numpy() != 0) & (F > 0).any(0)
    return cells[use].reset_index(drop=True), F[:, use]


def average_condition(member, x, m):
    """'The average of x over the group (member) is m' as a linear condition on the weights. It is
    sum over the group of w (x / m - 1) = 0; the tilt scales each target by its own size, so it is written as
    units + sum over the group of w (x / m - 1) = units, where units, the file's total (returns plus non-filers), is
    fixed exactly by the IRS and non-filer targets. A share is the average of a 0/1 indicator."""
    return 1.0 + member * (x / m - 1.0)


def average_tolerance(tolerance, member, w_start):
    """The tolerance of an average condition, as a share of units: tolerance times the group's share of the starting
    weight, so the group's average is within that share of m."""
    return tolerance * w_start[member].sum() / w_start.sum()


def wealth_targets(x, member, w_start, units):
    """The eight wealth targets from the target year's SCF wave: rows, each record's contribution, tolerances, and
    for the four shares the (group, indicator) whose average they set (None for the dollar targets). Shares are among
    the file's units other than dependent filers (member), which hold no wealth (generate.structural); dollars are
    the SCF's own. x and the SCF's net worth are both in the wave's (the target year's) dollars."""
    u = pd.read_parquet(f'{common.DATA}/scf_units_{Y}.parquet')
    x19, w19 = u['net_worth'].to_numpy(float), u['weight'].to_numpy(float)
    threshold = 1e6
    tol_dollars, tol_share = S['wealth_tolerance'], average_tolerance(S['wealth_tolerance'], member, w_start)
    rows, cols, tol, means = [], [], [], []

    def share(group, line):
        s = w19[x19 > line].sum() / w19.sum()
        above = (x > line).astype(float)
        rows.append({'group': group, 'target': 'share of units', 'value': units, 'average': s})
        cols.append(average_condition(member, above, s))
        tol.append(tol_share)
        means.append((member, above))

    def dollars(group, contribution, value):
        rows.append({'group': group, 'target': 'net worth', 'value': value})
        cols.append(contribution)
        tol.append(tol_dollars)
        means.append(None)
    for q, lab in zip(wquantile(x19, w19, [0.99, 0.90, 0.50]), ['99th', '90th', '50th']):
        share(f'above the SCF {lab} percentile', q)
        dollars(f'above the SCF {lab} percentile', x * (x > q), (x19 * w19)[x19 > q].sum())
    dollars('all', x, (x19 * w19).sum())
    share('above $1 million', threshold)
    return pd.DataFrame(rows), np.column_stack(cols), np.array(tol), means


AGES = common.AGE_LABELS[:common.ADULT_RANGES]


def consumption_targets(df, w_start, units):
    """CEX target-year spending per unit (build_table.cex_target_units): each category's average over all tax units,
    and in each cell of the primary filer's adult age range by married (a joint unit; separate returns are not
    married, on the CEX's units as on the file's), so the targets do not depend on how many units each source counts.
    Dependent filers are left out on both sides: the CEX folds them into the unit that claims them (build_cex.R),
    and the file gives them no consumption (generate.structural). Returns the target rows (with the average), the
    contributions, the tolerances, and per row the (group, spending) whose average it sets."""
    y = Y
    u = pd.read_parquet(f'{common.DATA}/cex_units_{y}_all.parquet')
    assert u['married'].isin([0, 1]).all() and (u['age_range'] < common.ADULT_RANGES).all()
    rows, cols, tol, means = [], [], [], []
    cells = [(f'age {a}, {"married" if m else "not married"}', k, m)
             for k, a in enumerate(AGES) for m in (1, 0)]
    for lab, k, m in [('all', None, None)] + cells:
        if k is None:
            mu = np.ones(len(u), bool)
            mf = (df['age_range'] < common.ADULT_RANGES).to_numpy()
        else:
            mu = ((u['age_range'] == k) & (u['married'] == m)).to_numpy()
            mf = ((df['age_range'] == k) & (df['married'] == m)).to_numpy()
        assert mu.any() and mf.any(), f'consumption cell {lab}: no CEX units or no records'
        for v in D.META['cex_amounts']:
            avg = float(np.average(u.loc[mu, v], weights=u.loc[mu, 'weight']))
            c = df[v].to_numpy(float)
            rows.append({'group': f'CEX {y}, {lab}', 'target': f'{v}, average', 'value': units, 'average': avg})
            cols.append(average_condition(mf, c, avg))
            tol.append(average_tolerance(S['consumption_band'], mf, w_start))
            means.append((mf, c))
    return pd.DataFrame(rows), np.column_stack(cols), np.array(tol), means


def nonfiler_count():
    """The CPS's expected non-filers in the target year: the sum over its units of weight x (1 - filer_p)
    (generate.cps_expected_nonfilers, repeated here so the tilt needs no network code)."""
    t = pd.read_parquet(common.TABLE, columns=['source', 'year', 'weight', 'filer_p'])
    t = t[(t.source == 'cps') & (t.year == Y)]
    assert len(t), f'no CPS units for {Y}'
    return float((t['weight'] * (1 - t['filer_p'])).sum())


NONFILER_BANDS = [('18_25', 18, 26), ('26_34', 26, 35), ('35_44', 35, 45), ('45_54', 45, 55), ('55_64', 55, 65),
                  ('65_74', 65, 75), ('75p', 75, np.inf)]


def nonfiler_people_targets(df):
    """The people in non-filer units, as step 1's calibration hit them (settings nonfiler_targets_dir,
    <year>_nonfilers_vs_targets.csv, column target_heads_spouses, in millions): heads and spouses by age band, and
    children under 17. Each record's contribution: its head and spouse, each by their own exact age, and its children
    under 17 among the head, the spouse and the first three dependents (the youngest three; any beyond are not counted).
    Ages are in years, -1 for none (dollars.py)."""
    t = pd.read_csv(f"{S['nonfiler_targets_dir']}/{Y}_nonfilers_vs_targets.csv").set_index('band')['target_heads_spouses']
    head, spouse = df['age_primary'].to_numpy(float), df['age_spouse'].to_numpy(float)
    nf = df['filer'].to_numpy() == 0
    assert (head[nf] >= 0).all() and ((spouse[nf] >= 0) == (df['married'].to_numpy()[nf] == 1)).all(), \
        'a non-filer with no head age, or a spouse age without a spouse or the reverse'
    cells, cols = [], []
    for b, lo, hi in NONFILER_BANDS:
        cells.append({'group': f'non-filers, heads and spouses aged {b}', 'target': 'people', 'value': 1e6 * t[b]})
        cols.append(((head >= lo) & (head < hi)).astype(float) + ((spouse >= lo) & (spouse < hi)))
    kids = (head >= 0) & (head < 17)
    kids = kids.astype(float) + ((spouse >= 0) & (spouse < 17))
    for k in (1, 2, 3):
        a = df[f'age_dep{k}'].to_numpy(float)
        kids += (a >= 0) & (a < 17)
    cells.append({'group': 'non-filers, children under 17', 'target': 'people', 'value': 1e6 * t['children under 17']})
    cols.append(kids)
    return pd.DataFrame(cells), np.column_stack(cols)


# ---- the solver ------------------------------------------------------------------
def tilt(w, F, T, rel_tol, max_iter=200, band=None):
    """Newton's method on the dual. rel_tol per target: 0 exact, else a Gaussian
    penalty with that standard deviation as a share of the target. band per target: 0 none, else the target need only
    be met within that share of it, with no cost inside the band (its dual term b |lambda|, smoothed at zero)."""
    W = w.sum()
    p = w / W
    X = F * (W / np.abs(T))
    t = np.sign(T)
    s2 = np.asarray(rel_tol, float) ** 2
    b = np.zeros(len(T)) if band is None else np.asarray(band, float)
    eps = 1e-6
    lam = np.zeros(len(T))

    def dual(lam):
        e = p * np.exp(X @ lam)
        return e.sum() - lam @ t + 0.5 * (s2 * lam ** 2).sum() + (b * np.sqrt(lam ** 2 + eps ** 2)).sum(), e

    def grad(lam, e):
        return X.T @ e - t + s2 * lam + b * lam / np.sqrt(lam ** 2 + eps ** 2)
    D_, e = dual(lam)
    for it in range(max_iter):
        g = grad(lam, e)
        if np.abs(g).max() < 1e-11:
            break
        h_band = b * eps ** 2 / (lam ** 2 + eps ** 2) ** 1.5
        step = np.linalg.solve(X.T @ (X * e[:, None]) + np.diag(s2 + h_band) + 1e-12 * np.eye(len(T)), -g)
        a = 1.0
        while True:
            D_new, e_new = dual(lam + a * step)
            if np.isfinite(D_new) and D_new <= D_ + 1e-4 * a * (g @ step):
                break
            a /= 2
            if a < 1e-12:
                return W * e, False
        lam, D_, e = lam + a * step, D_new, e_new
    return W * e, bool(np.abs(grad(lam, e)).max() < 1e-7)


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
        wage = common.wage_group(df['wages'].to_numpy(float))
        for _, _, g in common.WAGE_GROUPS:
            m = wage == g
            r[f'mean, wages {g} ($)'] = (x[m] * w[m]).sum() / w[m].sum()
        rows[name] = r
    return pd.DataFrame(rows)


def main():
    os.makedirs(common.TILT, exist_ok=True)
    soi = TG.load()
    raw = pd.read_parquet(f'{common.GEN}/stack.parquet')
    g19 = D.draw_dollars(raw, D.load_maps(Y))   # every dollar amount in target-year dollars
    x = g19['net_worth'].to_numpy(float)
    filer = g19['filer'].to_numpy() == 1
    imp = g19['weight'].to_numpy(float)

    # the start
    returns = TG.returns(soi, Y)
    n_nonfilers = nonfiler_count()
    w0 = np.where(filer, imp * returns / imp[filer].sum(), imp * n_nonfilers / imp[~filer].sum())
    w_plain = w0.copy()
    if S['prescale'] == 'filing_status':
        for married, status in [(1, 'joint'), (0, 'nonjoint')]:
            g = filer & (g19['married'].to_numpy() == married)
            w0[g] *= TG.returns(soi, Y, '1.2', status) / w0[g].sum()

    # the targets
    irs, F_irs = irs_targets(soi, g19)
    nfp, F_nfp = nonfiler_people_targets(g19)
    age, F_age = age_targets(g19)
    bus, F_bus = business_targets(soi, g19)
    units = returns + n_nonfilers
    nondep = g19['age_range'].to_numpy() < common.ADULT_RANGES   # wealth shares: units other than dependent filers
    wl, F_w, tol_w, means_w = wealth_targets(x, nondep, w0, units)
    cl, F_c, tol_c, means_c = consumption_targets(g19, w0, units)
    F_irs, F_age, F_bus = F_irs * filer[:, None], F_age * filer[:, None], F_bus * filer[:, None]
    F_nfp = F_nfp * ~filer[:, None]
    cells = pd.concat([irs.assign(kind='IRS'), pd.DataFrame([{'group': 'all', 'target': 'non-filers',
                                                               'value': n_nonfilers, 'kind': 'non-filers'}]),
                       nfp.assign(kind='non-filer people'), age.assign(kind='age'), bus.assign(kind='business'),
                       wl.assign(kind='wealth'), cl.assign(kind='consumption')], ignore_index=True)
    F = np.column_stack([F_irs, (~filer).astype(float), F_nfp, F_age, F_bus, F_w, F_c])
    T = cells['value'].to_numpy(float)
    n_exact = len(irs) + 1 + len(nfp) + len(age) + len(bus)
    on_nfp = slice(len(irs) + 1, len(irs) + 1 + len(nfp))
    on_age = slice(on_nfp.stop, on_nfp.stop + len(age))
    on_bus = slice(on_age.stop, n_exact)
    tol = np.r_[np.zeros(n_exact), tol_w, np.zeros(len(tol_c))]
    band = np.r_[np.zeros(n_exact + len(tol_w)), tol_c]
    means = [None] * n_exact + means_w + means_c
    # a tolerance's scale: 1 for a total; for a share or an average, its group's share of the starting weight
    scale = np.array([1.0 if m is None else w0[m[0]].sum() / w0.sum() for m in means])
    w1, ok = tilt(w0, F, T, tol, band=band)
    form = age_form = nfp_form = 'exact'
    if not ok:
        tol[on_bus] = S['business_fallback_tolerance']
        w1, ok = tilt(w0, F, T, tol, band=band)
        form = f"{100 * S['business_fallback_tolerance']:.0f}% tolerance"
    if not ok:
        tol[on_age] = S['age_fallback_tolerance']
        w1, ok = tilt(w0, F, T, tol, band=band)
        age_form = f"{100 * S['age_fallback_tolerance']:.0f}% tolerance"
    if not ok:
        tol[on_nfp] = S['nonfiler_fallback_tolerance']
        w1, ok = tilt(w0, F, T, tol, band=band)
        nfp_form = f"{100 * S['nonfiler_fallback_tolerance']:.0f}% tolerance"
    if not ok:   # keep going so the run still writes its tables, and say so in the summary
        tol[:] = np.maximum(tol, 0.02 * scale)    # 2% of each target; of the group's average for shares and averages
        w1, ok = tilt(w0, F, T, tol, band=band)
        form = 'every target at 2% tolerance (exact did not converge)' if ok else \
            'DID NOT CONVERGE: weights are the last iterate'
        common.say('tilt fallback:', form)

    # outputs
    fit = cells.assign(before=F.T @ w0, after=F.T @ w1)
    for i, mx in enumerate(means):                  # shares and averages: the share or average itself
        if mx is not None:
            member, v = mx
            mean = lambda w: (w * member * v).sum() / (w * member).sum()
            fit.loc[i, ['value', 'before', 'after']] = [fit.loc[i, 'average'], mean(w0), mean(w1)]
    fit['after / target'] = fit['after'] / fit['value']
    fit.to_csv(f'{common.TILT}/fit.csv', index=False)
    g19.assign(weight_start=w0, weight=w1).to_parquet(f'{common.TILT}/file_{Y}.parquet', index=False)
    u = pd.read_parquet(f'{common.DATA}/scf_units_{Y}.parquet')
    nw = net_worth_table({f'SCF {Y}': (u, u['net_worth'].to_numpy(float), u['weight'].to_numpy(float)),
                          'before the tilt': (g19, x, w0), 'after the tilt': (g19, x, w1)})
    summary = pd.Series({'targets': len(T), 'business targets': f'{len(bus)}, {form}',
                         'non-filer people targets': f'{len(nfp)}, {nfp_form}',
                         'age targets': f'{len(age)} ({S["age_level"]}), {age_form}',
                         'largest miss on exact IRS targets (%)': 100 * np.abs(fit['after / target'][:len(irs) + 1] - 1).max(),
                         'largest miss on non-filer people targets (%)': 100 * np.abs(fit['after / target'][on_nfp] - 1).max(),
                         'largest miss on age targets (%)': 100 * np.abs(fit['after / target'][on_age] - 1).max() if len(age) else np.nan,
                         **cost(w0, w1), 'KL move from the unscaled start (nats)': cost(w_plain, w1)['KL move (nats)']})
    by_kind = fit.groupby('kind')['after / target'].agg(['min', 'max'])
    text = [f'# The {Y} tilt', '', summary.to_frame('').to_markdown(floatfmt=',.4f'), '',
            '## Targets after the tilt (ratio to target, range by kind)', '', by_kind.to_markdown(floatfmt='.4f'), '',
            f'## Net worth (nominal {Y} dollars)', '', nw.to_markdown(floatfmt=',.1f'), '']
    open(f'{common.TILT}/summary.md', 'w').write('\n'.join(text))
    common.say('\n'.join(text))


if __name__ == '__main__':
    main()
