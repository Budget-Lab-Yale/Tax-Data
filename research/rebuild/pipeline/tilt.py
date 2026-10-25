"""Post-training: the generated stack reweighted to the target year (settings "years"; 2019 below).

One algorithm every year (TARGETS.md S7): build the start, build the targets, tilt. The year file
(targets_config/years/<year>.yaml, read by target_list.start_spec) says how the start is built:
  - start: generate (the default): the run's generated stack, its dollars drawn from the year's maps, as below;
  - start: {from_year: s, run: <name>}: year s's tilted file, <out>/tilt/<name>/file_<s>.parquet, its tilted
    weights in place of the importance weights, and (start_scaling dollars: per_return_growth) each record's
    amounts grown item by item from s to the target year (grow_dollars).
start_scaling weights (default settings tilt.prescale) is the scaling below, the same code for either start.
With hold_distribution in the year file, the start's distribution of each listed item across AGI rank groups
becomes targets too (target_list.hold_rows), built after the scaling from the scaled start.

The start. Each record's importance weight from generation, scaled so filers
sum to SOI's 2019 return count and non-filers to the CPS's expected
non-filers in 2019 (the sum over its units of weight x (1 - filer_p), the
same count the generated non-filer batch is sized by);
then filers are pre-scaled within joint and non-joint returns to SOI's 2019
counts of each (Table 1.2), so known population change is in the start
(settings prescale "filing_status").
With prescale "demographic" (TARGETS.md section 4), the population weights
are then scaled to the year's demographic cells before any other target,
each batch on its own, by the same solver and the same target rows the tilt
uses (so the cells are counted exactly as the targets count them):
  - filers: the smallest move (Kullback-Leibler) of the filers' weights that
    keeps the joint and non-joint totals and meets SOI Table 1.6's returns by
    the primary filer's age range, 26-34 to 65+, in joint and in non-joint
    returns (the "age" targets; under 26 is what each total leaves);
  - non-filers: the smallest move of the non-filers' weights that keeps their
    total and meets the 8 non-filer people targets (heads and spouses in 7
    age bands, children under 17). A unit carries several people, so this is
    a small fit, not a ratio.
Where a batch's cells cannot all be met, their standard errors decide, as in
the tilt. The cells stay targets in the tilt, so nothing later drags them
away. summary.md reports how far each prescaling moved its batch.

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
    business and age fallbacks): the people in non-filer units that the CPS
    filing stage's calibration hit (filing.py; settings nonfiler_targets_dir,
    inside the run's data folder, its <year>_nonfilers_vs_targets.csv, read,
    not recomputed): heads and spouses
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
    """The people in non-filer units, as the filing stage's calibration hit them (filing.py; settings
    nonfiler_targets_dir, inside the run's data folder, <year>_nonfilers_vs_targets.csv, column target_heads_spouses,
    in millions): heads and spouses by age band, and children under 17. Each record's contribution: its head and spouse, each by their own exact age, and its children
    under 17 among the head, the spouse and the first three dependents (the youngest three; any beyond are not counted).
    Ages are in years, -1 for none (dollars.py)."""
    t = pd.read_csv(f"{common.DATA}/{S['nonfiler_targets_dir']}/{Y}_nonfilers_vs_targets.csv").set_index('band')['target_heads_spouses']
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


# ---- the demographic prescaling (TARGETS.md section 4) ------------------------------------------------
PRESCALE_CELLS = {'filers': ['age'], 'non-filers': ['non-filers', 'non-filer people']}   # target kinds per batch


def prescale_demographic(w0, F, T, se, identity, averages, rows, filer, married, soi, SV):
    """The starting weights scaled to the year's demographic cells, each batch on its own: filers to SOI Table 1.6's
    returns by primary age range in joint and non-joint returns, keeping the joint and non-joint totals; non-filers
    to the non-filer people targets, keeping their total. Each is the smallest Kullback-Leibler move of that batch's
    weights that meets its cells (solver.solve on the batch alone, with the target rows' own conditions, standard
    errors and identities; the status totals are identities). Returns the new weights and, per batch, a dict for
    the summary: the solve, the batch's effective sample before and after, the factors, and each cell before and
    after."""
    w = w0.copy()
    out = []
    for batch, kinds in PRESCALE_CELLS.items():
        rec = filer if batch == 'filers' else ~filer
        cols = np.flatnonzero(rows['kind'].isin(kinds).to_numpy())
        if not len(cols):   # a year with no such cells (no CPS non-filer values in 2022): the batch is left as it is
            common.say(f'prescaling {batch}: the year has no targets of kinds {kinds}; left as scaled')
            continue
        assert all(averages[j] is None for j in cols), f'prescaling {batch}: an average among its cells'
        assert not (F[~rec][:, cols] != 0).any(), f'prescaling {batch}: a cell that records outside the batch reach'
        Fb, Tb, seb, idb = F[rec][:, cols], T[cols], se[cols], identity[cols]
        labels = (rows['kind'] + ' | ' + rows['group']).to_numpy()[cols].tolist()
        if batch == 'filers':
            # keep today's joint and non-joint scaling exactly: the two totals as identities
            extra = [((married[rec] == m).astype(float), TG.returns(soi, Y, '1.2', s), f'filers | {s} returns')
                     for m, s in [(1, 'joint'), (0, 'nonjoint')]]
            Fb = np.column_stack([Fb] + [c for c, _, _ in extra])
            Tb = np.append(Tb, [v for _, v, _ in extra])
            seb, idb = np.append(seb, [0.0, 0.0]), np.append(idb, [True, True])
            labels += [lab for _, _, lab in extra]
        wb0 = w[rec]
        wb1, info = SV.solve(wb0, Fb, Tb, seb, idb, [None] * len(Tb), S)
        ess = lambda v: v.sum() ** 2 / (v ** 2).sum()
        f = wb1 / wb0
        out.append({'batch': batch, 'records': int(rec.sum()), 'cell count': len(Tb), 'feasible': info['feasible'],
                    'how': info['how'], 'effective records before': ess(wb0), 'effective records after': ess(wb1),
                    'effective sample kept (%)': 100 * ess(wb1) / ess(wb0), 'KL move (nats)': info['kl_move'],
                    'largest factor': f.max(), 'smallest factor': f.min(),
                    'records moved more than twofold': int(((f > 2) | (f < 0.5)).sum()),
                    'cells': pd.DataFrame({'cell': labels, 'target': Tb, 'before': Fb.T @ wb0, 'after': Fb.T @ wb1})})
        common.say(f"prescaling {batch}: {info['how']}; effective sample kept {out[-1]['effective sample kept (%)']:.1f}%, "
                   f"factors {f.min():.3f} to {f.max():.3f}")
        w[rec] = wb1
    return w, out


def prescale_lines(prescaled):
    """summary.md's section on the prescaling."""
    if not prescaled:
        return []
    lines = ['## The demographic prescaling (before the tilt)', '']
    head = pd.DataFrame([{k: v for k, v in p.items() if k != 'cells'} for p in prescaled]).set_index('batch').T
    lines += [head.to_markdown(floatfmt=',.3f'), '']
    for p in prescaled:
        c = p['cells'].assign(**{'before - target (%)': lambda d: 100 * (d.before / d.target - 1)})
        lines += [f"### {p['batch']}: cells (people or returns)", '', c.to_markdown(index=False, floatfmt=',.1f'), '']
    return lines


# ---- the start from an earlier year's tilted file (TARGETS.md S7) ---------------------------------------------
def recombine(df):
    """The columns dollars.draw_dollars computes from the drawn amounts, computed again from the (grown) drawn
    amounts in the same order: the capital gains totals, business income and its two schedules from the eighteen
    parts, other AGI (the drawn remainder plus capital gain distributions plus the AGI items folded in), total
    consumption, the amounts made from a share (the spouse's wages, the home mortgage), the remainders (the primary
    filer's wages) and net worth. Returns the frame and the columns recomputed."""
    out, done = df.copy(), []

    def put(col, values):
        out[col] = values
        done.append(col)
    put('capital_gains_line13', out[D.KG_PIECES].sum(axis=1))
    put('capital_gains', out['kg_st'] + out['kg_lt'] + out['kg_other'])
    parts = D.META['computed_totals']['business_income']
    put('business_income', out[list(parts)].to_numpy(float) @ np.array(list(parts.values()), float))
    for sub in ['sch_cf_income', 'sch_e_income']:
        sp = D.META['computed_totals'][sub]
        put(sub, out[list(sp)].to_numpy(float) @ np.array(list(sp.values()), float))
    put('other_agi', out['other_agi_remainder'] + out['kg_dist']
        + out[list(D.FOLD)].to_numpy(float) @ np.array(list(D.FOLD.values()), float))
    cats = list(D.META['cex_amounts'])
    if all(c in out for c in cats):
        put('total_consumption', out[cats].to_numpy(float).sum(1))
    for slot, (item, base) in D.RATIOS.items():
        if f'{slot}_band' in out:
            put(item, out[slot] * out[base])
    for item, (total, other) in D.REMAINDERS.items():
        if other in out:
            put(item, out[total] - out[other])
    if all(c in out for c in D.NET_WORTH_PARTS):
        put('net_worth', out[list(D.NET_WORTH_PARTS)].to_numpy(float) @ np.array(list(D.NET_WORTH_PARTS.values()), float))
    return out, done


def grow_dollars(df, s, t):
    """Each record's amounts grown item by item from year s to year t (TARGETS.md S7, per-return growth), by the
    series the dollar maps are grown by (build_table.build_maps), as a ratio of year t's factor to year s's:
      - PUF amounts: SOI Table 1.4's dollars per return with the item (build_table.growth: positive and negative
        values separately; capital losses, mortgage interest and real estate tax not grown);
      - the balance sheet: the SCF waves' asset price factor, the wave of income year t - 1 over that of s - 1 (the
        maps' rule); not grown when year t - 1 has no wave;
      - consumption: NIPA PCE per return, by category;
      - the CPS-only dollars (childcare): SOI's wages per return; the CPS shares, weeks and hours not grown.
    Then every computed column is computed again from the grown parts (recombine; checked first to reproduce the
    file's own columns). Ages and every category are left as they are. Returns the grown frame and a table of the
    factors, one row per column."""
    table, edges_path = common.TABLE, common.EDGES
    import build_table as BT   # importing it points common.TABLE at the strata table (settings strata_labels)
    common.TABLE, common.EDGES = table, edges_path   # the tilt reads the shared table, as before
    same, done = recombine(df)
    for c in done:
        assert np.allclose(same[c].to_numpy(float), df[c].to_numpy(float), rtol=1e-9, atol=1e-6, equal_nan=True), \
            f'{c}: the year-{s} file\'s column is not what its parts give (dollars.draw_dollars changed?)'
    out, rows = df.copy(), []

    def grow(col, fp, fn, series):
        x = out[col].to_numpy(float)
        assert np.isfinite(fp) and fp > 0 and np.isfinite(fn) and fn > 0, f'{col}: growth factor {fp}, {fn}'
        out[col] = np.where(x > 0, x * fp, np.where(x < 0, x * fn, x))
        rows.append({'column': col, 'factor, positive values': fp, 'factor, negative values': fn, 'series': series,
                     'records with a value': int((x != 0).sum())})
    drawn_as = {'other_agi': 'other_agi_remainder', 'business_income': 'business_income_drawn'}
    for v in BT.PUF_AMOUNTS:
        if v in ('capital_gains', 'sch_cf_income', 'sch_e_income'):
            continue                                  # computed from their parts (recombine)
        col = drawn_as.get(v, v)
        if col not in out:
            continue
        gs, gt = BT.growth(v, s), BT.growth(v, t)
        series = 'none: no SOI line' if v in BT.NO_GROWTH else \
            'SOI 1.4 per return with the item' + ('; losses not grown' if v.startswith('kg_') or v == 'capital_gains' else '')
        grow(col, gt[0] / gs[0], gt[1] / gs[1], series)
    # the wave of income year y - 1 is the SCF of year y; its asset price factor is on every one of its units
    # (build_scf.R), the same factor the maps read from the table
    af = {}
    for y in (s, t):
        f = f'{common.DATA}/scf_units_{y}.csv.gz'
        af[y] = pd.read_csv(f, usecols=['asset_factor'])['asset_factor'] if os.path.exists(f) else pd.Series([], dtype=float)
    scf_ok = all(len(a) and a.nunique() == 1 for a in af.values())
    f_scf = af[s].iloc[0] / af[t].iloc[0] if scf_ok else 1.0
    for v in BT.SCF_AMOUNTS:
        if v in out:
            ratio = v == 'w_primary_ltv'
            grow(v, 1.0 if ratio else f_scf, 1.0 if ratio else f_scf,
                 'a ratio, not grown' if ratio else (f'SCF asset price factor, wave of {t - 1} over wave of {s - 1}'
                                                     if scf_ok else f'NOT GROWN: no SCF wave of income year {t - 1}'))
    pce = pd.read_csv(common.RAW['pce']).set_index('year')
    for v in BT.CEX_AMOUNTS:
        if v in out:
            f = (pce.loc[t, v] / TG.all_returns(t)) / (pce.loc[s, v] / TG.all_returns(s))
            grow(v, f, f, 'NIPA PCE per return (SOI returns)')
    for v in BT.CPS_AMOUNTS:
        if v in out:
            dollars_ = v in common.SETTINGS['cps']['dollar_amounts']
            f = BT.growth('wages', t)[0] / BT.growth('wages', s)[0] if dollars_ else 1.0
            grow(v, f, f, 'SOI wages per return with wages (a placeholder)' if dollars_ else 'not dollars, not grown')
    out, _ = recombine(out)
    return out, pd.DataFrame(rows)


def start_from_year(spec):
    """Year s's tilted file as the start: its records, its tilted weights (kept as weight_<s>), and its dollars grown
    to the target year (start_scaling dollars). Returns the frame, the weights and the growth table (None if not
    grown)."""
    s = spec['year']
    path = f"{common.OUT}/tilt/{spec['run']}/file_{s}.parquet"
    df = pd.read_parquet(path)
    assert df['weight'].notna().all() and (df['weight'] >= 0).all(), f'{path}: a missing or negative weight'
    w = df['weight'].to_numpy(float)
    df = df.drop(columns=['weight_start']).rename(columns={'weight': f'weight_{s}'})
    common.say(f'start: {path}, {len(df):,} records, {w.sum() / 1e6:,.2f} million units')
    if spec['dollars'] == 'per_return_growth':
        df, growth = grow_dollars(df, s, Y)
    else:
        growth = None
    df['weight'] = w
    return df, w, growth


def start_nonfilers():
    """The non-filer count the start's non-filer weights are scaled to: the CPS's expected non-filers in the year
    (nonfiler_count). A year with no CPS units (the CPS years are 2015, 2017, ..., 2023) takes the geometric mean of
    the nearest CPS years either side: a placeholder used only to scale the start, not a target. Returns the count
    and a note."""
    t = pd.read_parquet(common.TABLE, columns=['source', 'year', 'weight', 'filer_p'])
    t = t[t.source == 'cps']
    have = sorted(t['year'].unique())
    count = lambda y: float((t.loc[t.year == y, 'weight'] * (1 - t.loc[t.year == y, 'filer_p'])).sum())
    if Y in have:
        return nonfiler_count(), ''   # the count as before S7
    lo, hi = max([y for y in have if y < Y], default=None), min([y for y in have if y > Y], default=None)
    assert lo is not None and hi is not None, f'no CPS units for {Y}, and no CPS year on both sides'
    n = float(np.sqrt(count(lo) * count(hi)))
    note = (f'no CPS units for {Y}: the geometric mean of the CPS\'s expected non-filers in {lo} '
            f'({count(lo) / 1e6:.2f} million) and {hi} ({count(hi) / 1e6:.2f} million), a placeholder for scaling the '
            f'start only')
    common.say(note)
    return n, note


def hold_table(rows, df, w0, w1):
    """The hold_distribution targets' shares, on the starting and the final weights, computed directly (the fit
    table's are the solver's average form, equal on the start and close after)."""
    import target_list as TLS
    out, a = [], D.agi(df)
    for r in rows[rows['kind'] == 'hold distribution'].itertuples(index=False):
        x = TLS.record_value(df, r.variable)
        pop = TLS.population(df, r.population)
        g = pop & (a >= r.agi_lo) & (a < r.agi_hi)
        sh = lambda w: (w[g] * x[g]).sum() / (w[pop] * x[pop]).sum()
        out.append({'item': r.variable, 'group': r.group, 'share at the start (target, %)': 100 * r.value,
                    'after the tilt (%)': 100 * sh(w1), 'miss (share points)': 100 * (sh(w1) - r.value),
                    'error (share points)': 100 * r.se})
    return pd.DataFrame(out)


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
    import solver as SV
    import target_list as TLS
    spec = TLS.start_spec(Y)   # the year file's start and start_scaling (TARGETS.md S7)
    growth, start_lines = None, []
    if spec['mode'] == 'generate':
        raw = pd.read_parquet(f'{common.GEN}/stack.parquet')
        g19 = D.draw_dollars(raw, D.load_maps(Y))   # every dollar amount in target-year dollars
        imp = g19['weight'].to_numpy(float)
    else:
        g19, imp, growth = start_from_year(spec)   # every dollar amount grown to the target year
    x = g19['net_worth'].to_numpy(float)
    filer = g19['filer'].to_numpy() == 1

    # the start
    returns = TG.returns(soi, Y)
    n_nonfilers, nf_note = start_nonfilers()
    w0 = np.where(filer, imp * returns / imp[filer].sum(), imp * n_nonfilers / imp[~filer].sum())
    w_plain = w0.copy()
    prescale = spec['weights']
    assert prescale in ('none', 'filing_status', 'demographic'), f"start_scaling weights {prescale!r}"
    if prescale in ('filing_status', 'demographic'):
        for married, status in [(1, 'joint'), (0, 'nonjoint')]:
            g = filer & (g19['married'].to_numpy() == married)
            w0[g] *= TG.returns(soi, Y, '1.2', status) / w0[g].sum()

    # the targets (targets_config/, expanded by target_list.py, errors by errors.py) and the solve (solver.py):
    # every target hit exactly when they can all be hit; otherwise the standard errors decide which give way
    rows = TLS.expand(Y, start=g19)
    F, T, averages = TLS.conditions(g19, rows, w0, average_form=S.get('average_form', 'zero'))
    se, identity = rows['se'].to_numpy(float), rows['identity'].to_numpy(bool)
    missing = rows.loc[~identity & ~(se > 0), ['kind', 'group', 'target']]
    assert missing.empty, f'targets with no standard error:\n{missing.to_string()}'
    prescaled, w_status = [], w0.copy()
    if prescale == 'demographic':
        w0, prescaled = prescale_demographic(w0, F, T, se, identity, averages, rows, filer,
                                             g19['married'].to_numpy(), soi, SV)
    # hold_distribution: built from the start as scaled, so its rank groups and shares are the scaled start's
    hold = TLS.hold_rows(Y, g19, w0)
    if len(hold):
        rows = pd.concat([rows, hold], ignore_index=True)
        F, T, averages = TLS.conditions(g19, rows, w0, average_form=S.get('average_form', 'zero'))
        se, identity = rows['se'].to_numpy(float), rows['identity'].to_numpy(bool)
    TLS.write(rows, common.TILT, Y)
    w1, info = SV.solve(w0, F, T, se, identity, averages, S)
    if spec['mode'] == 'from_year':
        start_lines = [f"## The start: the {spec['year']} tilted file (TARGETS.md S7)", '',
                       f"- tilt/{spec['run']}/file_{spec['year']}.parquet: {len(g19):,} records; its tilted weights "
                       f"scaled by start_scaling weights '{prescale}'; dollars '{spec['dollars']}'.",
                       f"- non-filers scaled to {n_nonfilers / 1e6:.2f} million"
                       f"{' (' + nf_note + ')' if nf_note else ''}.", '']
        if growth is not None:
            start_lines += [f"Growth factors, {spec['year']} to {Y}, by column:", '',
                            growth.to_markdown(index=False, floatfmt='.4f'), '']
    elif nf_note:
        start_lines = [f'- non-filers scaled to {n_nonfilers / 1e6:.2f} million ({nf_note}).', '']
    if len(hold):
        start_lines += ['## Held distribution (hold_distribution targets): shares of each total by AGI rank group', '',
                        hold_table(rows, g19, w0, w1).to_markdown(index=False, floatfmt='.3f'), '']

    # outputs
    fit = SV.fit_table(w0, w1, F, T, se, averages, rows, S, identity)
    fit.to_csv(f'{common.TILT}/fit.csv', index=False)
    g19.assign(weight_start=w0, weight=w1).to_parquet(f'{common.TILT}/file_{Y}.parquet', index=False)
    scf_path = f'{common.DATA}/scf_units_{Y}.parquet'   # a year with no SCF wave has no column of its own
    u = pd.read_parquet(scf_path) if os.path.exists(scf_path) else None
    nw = net_worth_table({**({f'SCF {Y}': (u, u['net_worth'].to_numpy(float), u['weight'].to_numpy(float))}
                             if u is not None else {}),
                          'before the tilt': (g19, x, w0), 'after the tilt': (g19, x, w1)})
    summary = pd.Series({'targets': len(T), **cost(w0, w1),
                         'KL move from the unscaled start (nats)': cost(w_plain, w1)['KL move (nats)'],
                         **({'effective sample kept from the start before the demographic prescaling (%)':
                             cost(w_status, w1)['effective sample kept (%)']} if prescaled else {})})
    by_kind = fit.groupby('kind').agg(targets=('target', 'size'), gave_way=('gave_way', 'sum'),
                                      largest_miss_in_se=('miss_in_se', lambda m: m.abs().max()),
                                      warnings=('flag', lambda f: (f == 'warning').sum()),
                                      stops=('flag', lambda f: (f == 'stop').sum()))
    text = [f'# The {Y} tilt', '', *SV.summary_lines(info, fit), '', summary.to_frame('').to_markdown(floatfmt=',.4f'),
            '', *start_lines, *prescale_lines(prescaled), '## Targets by kind', '', by_kind.to_markdown(floatfmt='.3f'), '',
            f'## Net worth (nominal {Y} dollars)', '', nw.to_markdown(floatfmt=',.1f'), '']
    open(f'{common.TILT}/summary.md', 'w').write('\n'.join(text))
    common.say('\n'.join(text))


if __name__ == '__main__':
    main()
