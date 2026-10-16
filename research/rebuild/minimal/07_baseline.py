"""
07_baseline.py  (minimal example, step 7: the baseline)

Which stack. STACK_VERSION (environment; default v4) picks
gen/gen_puf_stack_<version>.parquet and writes to tilt/<version>/, with the
version in every file name below in place of <v>. v3 was the first baseline
(before the wealth fix); v4 is built the same way from the retrained model.

What it does. Tilts the oversampled stack gen/gen_puf_stack_<v>.parquet
(400,000 PUF-labelled generated records with net worth, oversampled at the
top on wages, capital gains, other_agi and net worth; importance weights in
`weight`, summing to 400,000) to 2019 targets, and writes the numbers for
07_BASELINE.md (archived: research/archive/minimal_07_BASELINE_2026-10-14_superseded.md;
the current baseline is 15_BASELINE_V4.md). This run is the baseline later changes are compared with.

  Start      the importance weights scaled to SOI's 2019 return count.
  Dollars    incomes from the step 4 2019 PUF map (as run B1), net worth from
             the 2015 SCF map (step 4's puf_maps); the tables group records
             by their 2015-dollar draws, as step 6 does.
  Targets    the 136 AGI-bracket targets of run B1, exact; plus eight soft
             wealth targets from the 2019 SCF tax units (step 6), all in
             dollars (see below), each with a standard deviation of 2% of
             the target.
  Compared   the same file tilted to the 136 targets alone.
  Redraw     200,000 equal-weight records drawn with probability proportional
             to the tilted weight (05_tilt.redraw, same seed), and every
             target checked on it.

Reuses 04_dollars.py, 05_tilt.py and 06_wealth_tables.py; changes none.

Settings added 2026-10-15 for the v4 baseline (15_BASELINE_V4.md), each
unset = the v3 behaviour:
  WEALTH_UNITS=filers  the six wealth targets come from the 2019 SCF units
             that would file (06_wealth_tables.would_file); the all-units
             targets are kept in wealth_targets_<v>.csv for comparison, and
             the tables get a filer column. Unset or 'all': every unit.
  MINIMAL_MAP_DIR      read by 04_dollars.puf_maps: the map folder (map/V4/
             for v4, built from table E4 by 15_maps.py).
  The wealth targets enter in dollars only (design/plan.md, decision 17,
  2026-10-15): the threshold form of 09_score.py. Eight targets: units and
  net worth above the SCF's 50th, 90th and 99th percentiles in dollars;
  total net worth; units above $1 million (2019 dollars). The rank
  iteration of 06_wealth_tables.tilt_with_wealth is no longer run: it did
  not settle in any v4 run, and each try cost about ten minutes before the
  threshold form was used anyway.
  The base of the count targets (bug fix, 2026-10-16, decision 17). Before
  this fix the counts were on two bases: units above $1 million was the
  SCF's own weighted count, a share of its 143 million units, while the
  percentile counts were the SCF's share times the IRS return count (157.8
  million returns for 2019). The $1 million line ($927,884 in 2015 dollars)
  is below the SCF's 90th percentile ($991,815), yet the old targets asked
  for fewer units above it (15.25 million) than above the 90th percentile
  (15.77 million). No set of weights can meet both, and the tilt emptied the
  band between the two lines (19_DIRECT.md, section 9). Now one base for
  every count, set by WEALTH_COUNT_BASE:
    irs_returns  every count is the SCF's weighted share of units
                 above the line times the IRS return count;
    scf_units    (default since 2026-10-16, John's decision; plan decision
                 17) every count is the SCF's own weighted count (shares of its
                 143 million units). The 15 million returns the IRS counts
                 beyond the SCF's units are then assumed to sit below every
                 line (for example, dependents filing their own returns);
    mixed_v4b    the old mixture, to reproduce v4 and v4b only.
  Dollar targets are the SCF's own amounts in all three. Which of the first
  two becomes the rule is John's decision (2026-10-16). The base is written
  in summary_<v>.csv, and consistency_<v>.csv checks that the eight targets
  can hold together (counts and dollars above a line fall as the line
  rises; the mean net worth of units between two lines lies between them).
  Extra outputs: holders_<v>.csv (share of returns with interest, dividends,
  capital gains: pretrain, posttrain, PUF 2015, IRS 2015 and 2019) and
  filing_rule_<v>.csv (what the filing rule drops).

Writes, in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/tilt/<v>/:
  weights_<v>.parquet       record_id, stratifier, weight before, after (136
                           targets only), after (with wealth targets)
  summary_<v>.csv           move, effective sample size, weight factors
  wealth_targets_<v>.csv    the rank-form wealth statistics (total, top 1%, next 9%,
                           next 40%, bottom 50%, units above $1 million) before and
                           after, and on the redraw; a description, not the targets
  wealth_lines_<v>.csv      the eight wealth targets the tilt uses (dollar lines):
                           target, before, after the 136 alone, after, redraw, misses
  consistency_<v>.csv       the checks that the eight targets agree with each other
  fit_<v>.csv               the 136 targets before and after
  untargeted_<v>.csv        the step 5 relationships: real PUF, pretrain, posttrain
  redraw_<v>.csv            the redraw against the 136 targets (05_tilt.redraw)
  redraw_<v>.parquet        the redrawn file: stack row, record_id, stratifier,
                           bands, 2019-dollar incomes, net worth, weight
  tables_<v>.md             the tables for the report
"""
import importlib.util
import os
import sys
import numpy as np
import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
V = os.environ.get('STACK_VERSION', 'v4')
OUT = f'{ROOT}/tilt/' + os.environ.get('TILT_TAG', V)   # TILT_TAG: another output folder (v4: v4_allunits)
STACK = f'{ROOT}/gen/gen_puf_stack_{V}.parquet'
META = f'{ROOT}/gen/gen_meta_{V}.json'
HERE = os.path.dirname(os.path.abspath(__file__))


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


wt = load('wt', '06_wealth_tables.py')
dollars, tiltmod = wt.dollars, wt.tiltmod


def table(index_name, cols, rows, data, pct, names=None):
    wt.COLS = cols
    return wt.md_table(index_name, rows, data, pct, names)


# 2019 SCF incomes are 2018 dollars; 2015 dollars = 2018 dollars x 1.1802 / 1.2493
# (09_score.py, WAGE_2018_TO_2015).
WAGE_2018_TO_2015 = 1.1802474966 / 1.2493


# The base of the wealth count targets (see the docstring).
COUNT_BASE = os.environ.get('WEALTH_COUNT_BASE', 'scf_units')
assert COUNT_BASE in ('irs_returns', 'scf_units', 'mixed_v4b'), COUNT_BASE
COUNT_BASE_TEXT = {
    'irs_returns': 'IRS return count for every count target (SCF share above the line times '
                   '{returns:.1f} million returns)',
    'scf_units': 'SCF units for every count target (the SCF weighted count above the line, '
                 'on its {units:.1f} million units)',
    'mixed_v4b': 'old mixed base (v4, v4b): units above $1 million as the SCF count, '
                 'percentile counts as SCF share times the return count'}


def threshold_form(x, returns, targets, threshold, units):
    """09_score.threshold_form, with the SCF units of the targets: units and
    net worth above the SCF's 99th, 90th and 50th percentiles, total net
    worth, units above $1 million (2019 dollars). Counts on the base set by
    COUNT_BASE; every amount is the SCF's own. Order as threshold_names."""
    _, x19, w19, _ = wt.scf2019(units)
    scale = {'irs_returns': returns / w19.sum(), 'scf_units': 1.0}

    def count(line, percentile):
        if COUNT_BASE == 'mixed_v4b':
            return w19[x19 > line].sum() * (returns / w19.sum() if percentile else 1.0)
        return w19[x19 > line].sum() * scale[COUNT_BASE]
    F, T = [], []
    for q in dollars.wquantile(x19, w19, [0.99, 0.90, 0.50]):
        a = (x > q).astype(float)
        F += [a, x * a]
        T += [count(q, True), (x19 * w19)[x19 > q].sum()]
    F += [x, (x > threshold).astype(float)]
    T += [targets['total'], count(threshold, False)]
    return np.column_stack(F), np.array(T)


def threshold_names(threshold, units):
    """For each column of threshold_form: its name, its line in 2015 dollars
    (None for total net worth) and whether it counts units or adds net worth."""
    _, x19, w19, _ = wt.scf2019(units)
    out = []
    for q, lab in zip(dollars.wquantile(x19, w19, [0.99, 0.90, 0.50]), ['99th', '90th', '50th']):
        out += [(f'units above the SCF {lab} percentile', q, 'units'),
                (f'net worth above the SCF {lab} percentile', q, 'net worth')]
    out += [('total net worth', None, 'net worth'),
            ('units above $1 million (2019 dollars)', threshold, 'units')]
    return out


def consistency(names, T):
    """Checks that the eight targets can all hold at once. Counts above a
    line must fall as the line rises, and so must net worth above a line
    (total net worth counts as the lowest line); where a line has both a
    count and an amount, the mean net worth above it must exceed the line,
    and between two such lines the mean of the units between them must lie
    between the lines. Returns one row per check."""
    count = {line: t for (_, line, kind), t in zip(names, T) if kind == 'units'}
    held = {line: t for (_, line, kind), t in zip(names, T) if kind == 'net worth' and line is not None}
    total = [t for (_, line, _), t in zip(names, T) if line is None][0]
    rows = []
    lines = sorted(count)
    for lo, hi in zip(lines[:-1], lines[1:]):
        rows.append({'check': f'units above ${lo:,.0f} > units above ${hi:,.0f}',
                     'left': count[lo], 'right': count[hi], 'holds': count[lo] > count[hi]})
    hl = sorted(held)
    rows.append({'check': f'total net worth >= net worth above ${hl[0]:,.0f}',
                 'left': total, 'right': held[hl[0]], 'holds': total >= held[hl[0]]})
    for lo, hi in zip(hl[:-1], hl[1:]):
        rows.append({'check': f'net worth above ${lo:,.0f} > net worth above ${hi:,.0f}',
                     'left': held[lo], 'right': held[hi], 'holds': held[lo] > held[hi]})
    for lo in hl:
        mean = held[lo] / count[lo]
        rows.append({'check': f'mean net worth above ${lo:,.0f} exceeds the line',
                     'left': mean, 'right': lo, 'holds': mean > lo})
    for lo, hi in zip(hl[:-1], hl[1:]):
        mean = (held[lo] - held[hi]) / (count[lo] - count[hi])
        rows.append({'check': f'mean net worth between ${lo:,.0f} and ${hi:,.0f} lies between them',
                     'left': mean, 'right': f'{lo:,.0f} to {hi:,.0f}', 'holds': lo < mean <= hi})
    return pd.DataFrame(rows)


def holders(g19, weights):
    """Share of returns with a nonzero amount of each investment item, under
    each set of weights, next to the 2015 PUF (survey weights), IRS 2015
    (Table 1.4) and IRS 2019 (the tilt's totals)."""
    puf = pd.read_parquet(f'{ROOT}/data/stacked.parquet')
    puf = puf[puf['source'] == 'puf']
    wp = puf['weight'].to_numpy(float)
    t = pd.read_csv(f'{ROOT}/targets/soi_2015_validation.csv')
    t = t[(t['year'] == 2015) & (t['table'] == 1.4) & (t['filing_status'] == 'all')
          & (t['agi_bracket_label'] == 'all') & (t['statistic'] == 'count')]
    c15 = dict(zip(t['variable'], t['value']))
    tot = tiltmod.target_set('totals')[1].set_index('cell')['value']
    irs15 = {'interest': c15['taxable_interest'], 'dividends': c15['ordinary_dividends'],
             'capital_gains': c15['sch_d_net_gain'] + c15['sch_d_net_loss']}
    rows = []
    for v in ['interest', 'dividends', 'capital_gains']:
        has = g19[v].to_numpy(float) != 0
        rows.append({'item': v, **{k: 100 * w[has].sum() / w.sum() for k, w in weights.items()},
                     'PUF 2015 (weighted)': 100 * wp[puf[v].to_numpy(float) != 0].sum() / wp.sum(),
                     'IRS 2015': 100 * irs15[v] / c15['returns'],
                     'IRS 2019': 100 * tot[f'{v} count'] / tot['returns']})
    return pd.DataFrame(rows)


def filing_rule():
    """What the filing rule drops from the 2019 SCF units: weighted share of
    units and of net worth, under the 2019 thresholds (used) and the 2018
    thresholds (the incomes' own year, for the record)."""
    u, x, w, _ = wt.scf2019('all')
    rows = []
    for name, th in [('2019 thresholds (used)', wt.FILING_THRESHOLD_2019),
                     ('2018 thresholds (for comparison)', wt.FILING_THRESHOLD_2018)]:
        f = wt.would_file(u, th)
        for grp, sel in [('all units', np.ones(len(u), bool)), ('joint', u['married'].to_numpy() == 1),
                         ('not joint', u['married'].to_numpy() == 0)]:
            rows.append({'thresholds': name, 'units': grp,
                         'units (millions)': w[sel].sum() / 1e6,
                         'filers (millions)': w[sel & f].sum() / 1e6,
                         'share of units dropped (%)': 100 * w[sel & ~f].sum() / w[sel].sum(),
                         'net worth ($ trillion, 2015 dollars)': (x * w)[sel].sum() / 1e12,
                         'share of net worth dropped (%)': 100 * (x * w)[sel & ~f].sum() / (x * w)[sel].sum(),
                         'records dropped': int((sel & ~f).sum())})
    out = pd.DataFrame(rows)
    out.to_csv(f'{OUT}/filing_rule_{V}.csv', index=False)
    print(out.to_string(), flush=True)


def main():
    os.makedirs(OUT, exist_ok=True)
    assert os.path.exists(STACK) and os.path.exists(META), f'{V} stack not finished'

    # ---- the stack, in 2019 and 2015 dollars (same seed: 2019 = 2015 grown)
    os.environ['GEN_FILE'] = STACK
    g19 = tiltmod.generated_2019()
    raw = pd.read_parquet(STACK)
    assert (raw['source'] == 'puf').all() and len(raw) == len(g19)
    g15, _ = dollars.draw_dollars(raw, dollars.puf_maps(2015))
    g15['record_id'] = g19['record_id'].to_numpy()
    assert (g19['net_worth_band'] >= 0).all()
    assert np.array_equal(g15['net_worth'].to_numpy(), g19['net_worth'].to_numpy())
    imp = g19['weight'].to_numpy(float)
    assert (imp > 0).all()
    print(f'stack: {len(g19)} records, importance weights sum {imp.sum():,.1f}; '
          f'by stratifier:\n{g19.groupby("stratifier")["weight"].agg(["size", "sum"])}')

    # ---- targets and start
    brackets, cells = tiltmod.target_set('brackets')
    returns = tiltmod.target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0]
    w0 = imp * returns / imp.sum()
    F = tiltmod.contributions(g19, brackets)
    T = cells['value'].to_numpy(float)
    tiltmod.check_reachable(F, cells)
    x = g15['net_worth'].to_numpy(float)
    units = os.environ.get('WEALTH_UNITS', 'all')
    targets, threshold = wt.wealth_targets(units)
    targets_all, _ = wt.wealth_targets('all')
    filing_rule()

    # ---- tilt to the 136 alone, then with the wealth targets
    w1, ok1, it1 = tiltmod.tilt(w0, F, T)
    assert ok1, '136-target tilt did not converge'
    Fq, Tq = threshold_form(x, returns, targets, threshold, units)
    names = threshold_names(threshold, units)
    cons = consistency(names, Tq)
    cons.to_csv(f'{OUT}/consistency_{V}.csv', index=False)
    print(cons.to_string(), flush=True)
    if COUNT_BASE != 'mixed_v4b':
        assert cons['holds'].all(), 'the eight wealth targets contradict each other'
    w2, ok2, _ = tiltmod.tilt(w0, np.column_stack([F, Fq]), np.r_[T, Tq],
                              rel_tol=np.r_[np.zeros(len(T)), np.full(len(Tq), wt.SIGMA)])
    method = 'threshold form (dollar lines; decision 17)'
    gap2 = 100 * (F.T @ w2 / T - 1)
    summary = pd.DataFrame([
        {'run': '136 targets', 'newton_steps': it1, 'converged': ok1, **tiltmod.move(w0, w1)},
        {'run': '136 exact + 8 soft wealth', 'converged': ok2,
         'wealth_targets_from': units, 'wealth_target_form': method,
         'wealth_count_base': COUNT_BASE,
         'wealth_count_base_meaning': COUNT_BASE_TEXT[COUNT_BASE].format(
             returns=returns / 1e6, units=wt.scf2019(units)[2].sum() / 1e6),
         'max_abs_gap_136_targets_pct': np.abs(gap2).max(), **tiltmod.move(w0, w2)}])
    summary.to_csv(f'{OUT}/summary_{V}.csv', index=False)
    print(summary.to_string(), flush=True)
    pd.DataFrame({'record_id': g19['record_id'], 'stratifier': g19['stratifier'],
                  'weight_before': w0, 'weight_after_136': w1, 'weight_after': w2}).to_parquet(
        f'{OUT}/weights_{V}.parquet', index=False)
    cells.assign(before=F.T @ w0, after=F.T @ w2,
                 gap_before_pct=100 * (F.T @ w0 / T - 1), gap_after_pct=gap2).to_csv(
        f'{OUT}/fit_{V}.csv', index=False)

    # ---- redraw: same draw as 05_tilt.redraw, which writes redraw_{V}.csv
    tiltmod.OUT = OUT
    tiltmod.redraw(V, w2, F, T, cells)
    rng = np.random.default_rng(tiltmod.SEED)
    idx = rng.choice(len(w2), size=tiltmod.REDRAW_SIZE, replace=True, p=w2 / w2.sum())
    wr = np.full(len(idx), w2.sum() / len(idx))
    rd = g19.iloc[idx].reset_index().rename(columns={'index': 'stack_row'}).assign(weight=wr)
    rd.to_parquet(f'{OUT}/redraw_{V}.parquet', index=False)
    rfit = pd.read_csv(f'{OUT}/redraw_{V}.csv')

    # the eight wealth targets the tilt used, before, after and on the redraw
    lines = pd.DataFrame({'target': [n for n, _, _ in names],
                          'line (2015 dollars)': [ln for _, ln, _ in names],
                          'target value': Tq, 'pretrain': Fq.T @ w0,
                          'posttrain 136 only': Fq.T @ w1, 'posttrain': Fq.T @ w2,
                          'redraw': Fq[idx].T @ wr})
    for c in ['pretrain', 'posttrain 136 only', 'posttrain', 'redraw']:
        lines[f'{c} miss %'] = 100 * (lines[c] / lines['target value'] - 1)
    lines['count base'] = COUNT_BASE
    lines.to_csv(f'{OUT}/wealth_lines_{V}.csv', index=False)
    print(lines.to_string(), flush=True)

    miss = wt.miss_table(x, targets, threshold, {'pretrain': w0, 'posttrain 136 only': w1,
                                                 'posttrain': w2})
    miss.insert(1, 'scf2019 all units', [targets_all[k] for k in miss['target']])
    red = wt.wealth_held(x[idx], wr, threshold)
    miss['redraw'] = [red[k] for k in miss['target']]
    miss['redraw miss %'] = 100 * (miss['redraw'] / miss['scf2019'] - 1)
    miss.to_csv(f'{OUT}/wealth_targets_{V}.csv', index=False)
    print(miss.to_string(), flush=True)

    # ---- untargeted relationships (step 5's), real PUF grown to 2019
    real = tiltmod.puf_2019_from_real()
    rel = tiltmod.relationships(real, real['weight'].to_numpy(float), brackets).rename(
        columns={'share': 'actual 2015 PUF'})
    rel['pretrain'] = tiltmod.relationships(g19, w0, brackets)['share'].to_numpy()
    rel['posttrain'] = tiltmod.relationships(g19, w2, brackets)['share'].to_numpy()
    p2 = tiltmod.relationships(g19, w0, brackets)
    assert (p2['group'].to_numpy() == rel['group'].to_numpy()).all()
    rel = rel[rel['relationship'] != 'any dividends, by AGI bracket']   # targeted
    rel.to_csv(f'{OUT}/untargeted_{V}.csv', index=False)

    # ---- wealth tables: actual SCF 2016 and 2019 waves, pretrain, posttrain
    scf16, w16 = wt.actual_scf()
    u19, x19, w19, _ = wt.scf2019()
    u19 = u19.assign(net_worth=x19)          # 2015 dollars; incomes are 2018 dollars
    files = {'actual SCF (2016 wave)': (scf16, w16), 'actual SCF (2019 wave)': (u19, w19)}
    if units == 'filers':
        # v4: the 2019 wave's incomes put in 2015 dollars for the groupings
        # (09_score.WAGE_2018_TO_2015), all units and filers.
        for name, kind in [('actual SCF (2019 wave)', 'all'), ('actual SCF (2019 wave, filers)', 'filers')]:
            u, xx, ww, _ = wt.scf2019(kind)
            u = u.assign(net_worth=xx, wages=u['wages'] * WAGE_2018_TO_2015,
                         dividends=u['dividends'] * WAGE_2018_TO_2015)
            files[name] = (u, ww)
    files.update({'pretrain': (g15, w0), 'posttrain': (g15, w2)})
    cols = list(files)
    t1, t2 = {}, {}
    for c, (df, w) in files.items():
        xx = df['net_worth'].to_numpy(float)
        t1[c], t2[c] = wt.unconditional(xx, w), wt.conditional(df, xx, w)

    hold = holders(g19, {'pretrain': w0, 'posttrain 136 only': w1, 'posttrain': w2})
    hold.to_csv(f'{OUT}/holders_{V}.csv', index=False)
    out = ['## Fit and move', '', summary.to_markdown(index=False), '',
           '## Holder shares (% of returns)', '', hold.to_markdown(index=False), '',
           '## Filing rule', '', pd.read_csv(f'{OUT}/filing_rule_{V}.csv').to_markdown(index=False), '',
           '## The eight wealth targets (dollar lines, as tilted)', '', lines.to_markdown(index=False), '',
           '## Consistency of the eight wealth targets', '', cons.to_markdown(index=False), '',
           '## Wealth statistics in rank form (description, not targets)', '', miss.to_markdown(index=False), '',
           '## Redraw against the 136 targets', '',
           f"within 2 standard errors: {(rfit['z'].abs() <= 2).sum()} of {rfit['z'].notna().sum()} "
           f"with a standard error; largest |z| {rfit['z'].abs().max():.2f}; "
           f"largest |% miss| {rfit['gap_pct'].abs().max():.1f} "
           f"({rfit.loc[rfit['gap_pct'].abs().idxmax(), 'bracket']}, "
           f"{rfit.loc[rfit['gap_pct'].abs().idxmax(), 'cell']}); "
           f"distinct records {rfit['distinct_records'].iloc[0]}", '']
    ucols = ['actual 2015 PUF', 'pretrain', 'posttrain']
    out += ['## Untargeted relationships (%)', '']
    for name, g in rel.groupby('relationship', sort=False):
        data = {c: dict(zip(g['group'], 100 * g[c])) for c in ucols}
        out += [f'### {name}', '', table('group', ucols, list(g['group']), data, False), '']
    labs1 = list(t1[cols[0]])
    out += ['## Table 1. Net worth, all units', '',
            table('', cols, labs1, t1, lambda k: '(%)' in k, [k.replace(' (%)', '') for k in labs1]), '']
    for stat, title in [('mean ($)', 'Table 2a. Mean net worth by group ($)'),
                        ('median ($)', 'Table 2b. Median net worth by group ($)'),
                        ('share of units (%)', 'Table 2c. Share of units in each group'),
                        ('share above $1m (%)', 'Table 3. Share of units with net worth above $1 million')]:
        out += [f'## {title}', '']
        for gname in (['wages'] if stat == 'share above $1m (%)' else wt.ORDER):
            data = {c: {r.group: r.value for r in t2[c][t2[c]['grouping'] == gname]
                        .rename(columns={stat: 'value'}).itertuples()} for c in cols}
            out += [table(gname, cols, wt.ORDER[gname], data, stat.endswith('(%)')), '']
    out += ['## Records per group', '']
    for gname in wt.ORDER:
        data = {c: {r.group: r.records for r in t2[c][t2[c]['grouping'] == gname].itertuples()}
                for c in cols}
        out += [f'| {gname} | ' + ' | '.join(cols) + ' |', '|---|' + '---:|' * len(cols)]
        out += [f'| {g} | ' + ' | '.join(f'{int(data[c][g]):,}' for c in cols) + ' |'
                for g in wt.ORDER[gname]] + ['']
    text = '\n'.join(out)
    open(f'{OUT}/tables_{V}.md', 'w').write(text)
    print(text)


if __name__ == '__main__':
    sys.exit(main())
