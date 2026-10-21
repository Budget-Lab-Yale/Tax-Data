"""
05_tilt.py  (minimal example, step 5: the tilt)

What it does. Moves a file's weights to hit published 2019 targets with the
smallest change, measured as Kullback-Leibler divergence (KL, the expected log
ratio of new to old weight, in nats). The new weights have the form
    w_new = w_old * exp(sum over targets k of lambda_k * f_k(record)),
where f_k(record) is the record's contribution to target k (1 or 0 for a
count, dollars for an amount). The lambdas solve
    sum_i w_new_i * f_k(i) = T_k   for every k,
found by Newton's method on the dual problem, which is convex.

Two target sets:
  totals    (runs A0, B0)  10 targets: returns, joint returns, and for each
            of wages, taxable interest, dividends and capital gains the
            number of returns with the item and its amount.
  brackets  (runs A1, B1)  returns, item counts and item amounts by AGI
            bracket (SOI Table 1.4, top brackets merged until each would
            hold 500 records in a 200,000-record file), plus joint returns.
AGI on a record is the sum of its five parts, in 2019 dollars.

Pre-scaled start (A1 and B1 again): before tilting, weights are scaled
within joint and non-joint returns to SOI's 2019 counts (Table 1.2), so
known population change is in the starting weights; the tilt's move is
measured from there.

Run A: the real 2015 PUF records, survey weights, dollars grown to 2019 with
the step 4 factors. Run B (only if a generated file exists): generated PUF
records, 2019 dollars drawn from the step 4 2019 map, equal starting weights
summing to SOI's 2019 return count; then a redraw of 200,000 records with
probability proportional to the tilted weight.

Writes, in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/tilt/:
  summary.csv            one row per run: fit, KL, effective sample size,
                         weight factor range
  fit_<run>.csv          every target: published, before, after
  untargeted_<run>.csv   four relationships before and after the tilt
  weights_<run>.parquet  record_id, weight before, weight after
  extremes_<run>.csv     the 15 records with the largest and smallest weight
                         factors: their AGI bracket and amounts
  redraw_<run>.csv       run B only: redrawn file's estimates against targets
"""
import importlib.util
import os
import sys
import numpy as np
import pandas as pd
import soi_cells

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{ROOT}/tilt'
SOI_2019 = f'{ROOT}/targets/soi_2019_targets.csv'
REDRAW_SIZE, MIN_TOP_RECORDS, SEED = 200_000, 500, 20261105

spec = importlib.util.spec_from_file_location(
    'dollars', os.path.join(os.path.dirname(os.path.abspath(__file__)), '04_dollars.py'))
dollars = importlib.util.module_from_spec(spec)
spec.loader.exec_module(dollars)


# ---------------------------------------------------------------- targets

def returns_2019(status):
    """2019 return count for a filing status group from Table 1.2 ('joint',
    or 'nonjoint' = separate + head of household + single)."""
    t = pd.read_csv(SOI_2019)
    row = t[(t['table'] == 1.2) & (t['filing_status'] == status) & (t['agi_bracket_label'] == 'all')
            & (t['variable'] == 'returns')]
    assert len(row) == 1
    return float(row['value'].iloc[0])


def prescale(df, w):
    """Pre-scaled start: within joint and non-joint returns, scale the
    weights so each group's count equals its 2019 count. Known population
    change goes into the starting weights; the tilt is measured from here,
    so this step is not counted in its move. (In the real version the groups
    would be age by filing status from the population projections.)"""
    w = w.copy()
    for married, status in [(1, 'joint'), (0, 'nonjoint')]:
        g = df['married'].to_numpy() == married
        w[g] *= returns_2019(status) / w[g].sum()
    return w


def target_set(name):
    """Brackets and the published target values, cell names included."""
    soi = soi_cells.read_soi(SOI_2019, 2019)
    if name == 'totals':
        brackets = [(-np.inf, np.inf, 'all')]
    else:
        brackets = soi_cells.collapse_top(soi, soi_cells.published_brackets(soi),
                                          REDRAW_SIZE, MIN_TOP_RECORDS)
    cells = soi_cells.soi_cells(soi, brackets)
    cells.loc[len(cells)] = {'bracket': 'all', 'cell': 'joint returns', 'value': returns_2019('joint')}
    return brackets, cells


def contributions(df, brackets):
    """Records-by-targets matrix, columns in the order of target_set's cells."""
    married = df['married'].to_numpy(float)   # PUF married = filing status 2 (joint)
    return np.column_stack([soi_cells.record_cells(df, brackets), married])


# ---------------------------------------------------------------- the tilt

def tilt(w, F, T, rel_tol=None, max_iter=200):
    """Solve for lambda; return new weights and whether it converged.

    rel_tol: optional tolerance per target, as a share of the target. A
    target with tolerance is soft: the solution trades its miss against the
    move (a Gaussian penalty with that standard deviation). None means every
    target is exact, which is what every run here uses.
    """
    W = w.sum()
    p = w / W                                  # work with weights summing to one
    X = F * (W / np.abs(T))                    # so each target becomes +1 or -1
    t = np.sign(T)
    s2 = np.zeros(len(T)) if rel_tol is None else np.asarray(rel_tol, float) ** 2
    lam = np.zeros(len(T))

    def dual(lam):
        e = p * np.exp(X @ lam)
        return e.sum() - lam @ t + 0.5 * (s2 * lam ** 2).sum(), e

    D, e = dual(lam)
    for it in range(max_iter):
        g = X.T @ e - t + s2 * lam             # achieved minus target, per target
        if np.abs(g).max() < 1e-11:
            break
        H = X.T @ (X * e[:, None]) + np.diag(s2)
        step = np.linalg.solve(H + 1e-12 * np.eye(len(T)), -g)
        a = 1.0                                 # halve the step until the dual falls
        while True:
            D_new, e_new = dual(lam + a * step)
            if np.isfinite(D_new) and D_new <= D + 1e-4 * a * (g @ step):
                break
            a /= 2
            if a < 1e-12:
                return W * e, False, it
        lam, D, e = lam + a * step, D_new, e_new
    # Converged: every target within one part in ten million.
    return W * e, bool(np.abs(X.T @ e - t + s2 * lam).max() < 1e-7), it


def move(w0, w1):
    """KL divergence of new from old (normalized weights, nats) and ESS ratio."""
    p0, p1 = w0 / w0.sum(), w1 / w1.sum()
    kl = float(np.sum(p1 * np.log(p1 / p0)))
    ess = lambda w: w.sum() ** 2 / (w ** 2).sum()
    factor = w1 / w0
    return {'kl_nats': kl, 'ess_before': ess(w0), 'ess_after': ess(w1),
            'ess_ratio': ess(w1) / ess(w0), 'factor_min': factor.min(),
            'factor_p01': np.quantile(factor, 0.01), 'factor_median': np.median(factor),
            'factor_p99': np.quantile(factor, 0.99), 'factor_max': factor.max(),
            'records_factor_over_10': int((factor > 10).sum()),
            'records_factor_under_0.1': int((factor < 0.1).sum())}


def extremes(df, w0, w1, brackets, n=15):
    """The records the tilt moved most, to see which targets did it."""
    a = soi_cells.agi(df)
    out = df[['record_id', 'married', 'n_dep'] + soi_cells.PARTS].assign(
        agi=a, bracket=np.select([(a >= lo) & (a < hi) for lo, hi, _ in brackets],
                                 [lab for _, _, lab in brackets], 'none'),
        weight_before=w0, factor=w1 / w0)
    out = out.sort_values('factor')
    return pd.concat([out.head(n), out.tail(n)])


def check_reachable(F, cells):
    """A target with no record able to contribute cannot be hit by any weights."""
    bad = [c for c, col in zip(cells.itertuples(), F.T)
           if c.value != 0 and not np.any(np.sign(col) == np.sign(c.value))]
    assert not bad, f'targets no record can reach: {bad}'


# ---------------------------------------------------------------- untargeted

def share(flag, w, groups):
    """Weighted share with the flag within each group."""
    keys = pd.MultiIndex.from_arrays(groups) if isinstance(groups, list) else groups
    num = pd.Series(np.asarray(flag, float) * w).groupby(keys).sum()
    return num / pd.Series(w).groupby(keys).sum()


def relationships(df, w, brackets):
    """Four relationships not (or not fully) targeted, in 2019 dollars."""
    wage = pd.cut(df['wages'], [-np.inf, 0, 25e3, 50e3, 100e3, 200e3, np.inf],
                  labels=['0', '1-25k', '25-50k', '50-100k', '100-200k', '200k+']).astype(str)
    div = pd.cut(df['dividends'], [-np.inf, 0, 1e3, 10e3, 100e3, np.inf],
                 labels=['0', '1-1k', '1k-10k', '10k-100k', '100k+']).astype(str)
    a = soi_cells.agi(df)
    agi_group = pd.Series(np.select([(a >= lo) & (a < hi) for lo, hi, _ in brackets],
                                    [lab for _, _, lab in brackets], 'none'))
    mar = df['married'].map({0: 'not joint', 1: 'joint'}).to_numpy()
    has_div = df['dividends'].to_numpy() != 0
    parts = [('any dividends, by wage group and joint', share(has_div, w, [wage.to_numpy(), mar])),
             ('any capital gains, by dividend group', share(df['capital_gains'] != 0, w, div.to_numpy())),
             ('any dividends, by AGI bracket', share(has_div, w, agi_group.to_numpy()))]
    for k in range(4):
        parts.append((f'share with {k}{"+" if k == 3 else ""} dependents, by joint',
                      share(df['n_dep'].to_numpy() == k, w, mar)))
    rows = [pd.DataFrame({'relationship': name, 'group': [str(i) for i in s.index], 'share': s.values})
            for name, s in parts]
    return pd.concat(rows, ignore_index=True)


# ---------------------------------------------------------------- runs

def run(name, df, w0, target_name, summary, w_plain=None):
    """Tilt w0 to a target set and write the run's files. For a pre-scaled
    start, w_plain is the weight before pre-scaling, so the summary also
    shows the pre-scaling's own move and the move from w_plain to the end."""
    brackets, cells = target_set(target_name)
    F = contributions(df, brackets)
    T = cells['value'].to_numpy(float)
    check_reachable(F, cells)
    w1, converged, iters = tilt(w0, F, T)
    fit = cells.assign(before=F.T @ w0, after=F.T @ w1)
    fit['gap_before_pct'] = 100 * (fit['before'] / fit['value'] - 1)
    fit['gap_after_pct'] = 100 * (fit['after'] / fit['value'] - 1)
    fit.to_csv(f'{OUT}/fit_{name}.csv', index=False)
    before, after = relationships(df, w0, brackets), relationships(df, w1, brackets)
    before.assign(after=after['share']).rename(columns={'share': 'before'}).to_csv(
        f'{OUT}/untargeted_{name}.csv', index=False)
    extremes(df, w0, w1, target_set('brackets')[0]).to_csv(f'{OUT}/extremes_{name}.csv', index=False)
    pd.DataFrame({'record_id': df['record_id'], 'weight_before': w0, 'weight_after': w1}).to_parquet(
        f'{OUT}/weights_{name}.parquet', index=False)
    row = {'run': name, 'records': len(df), 'targets': len(T), 'converged': converged,
           'newton_steps': iters, 'max_abs_gap_after_pct': fit['gap_after_pct'].abs().max(),
           **move(w0, w1)}
    if w_plain is not None:
        row.update({'prescale_kl_nats': move(w_plain, w0)['kl_nats'],
                    'kl_from_plain_start': move(w_plain, w1)['kl_nats'],
                    'ess_ratio_from_plain_start': move(w_plain, w1)['ess_ratio']})
    summary.append(row)
    print(pd.Series(row).to_string(), '\n', fit.to_string(), flush=True)
    return w1, F, T, cells


def redraw(name, w1, F, T, cells):
    """Equal-weight file of REDRAW_SIZE drawn with probability proportional to w1."""
    rng = np.random.default_rng(SEED)
    W, p = w1.sum(), w1 / w1.sum()
    idx = rng.choice(len(w1), size=REDRAW_SIZE, replace=True, p=p)
    est = F[idx].sum(axis=0) * W / REDRAW_SIZE
    sd = np.sqrt(np.maximum(p @ F ** 2 - (p @ F) ** 2, 0))      # spread under the tilted weights
    se = W * sd / np.sqrt(REDRAW_SIZE)
    # A target every record counts toward equally (the return count) has no
    # sampling error: the redraw hits it exactly, and z is left undefined.
    z = np.divide(est - T, se, out=np.full(len(T), np.nan), where=se > 0)
    out = cells.assign(redrawn=est, std_error=se, z=z,
                       gap_pct=100 * (est / T - 1), distinct_records=len(np.unique(idx)))
    out.to_csv(f'{OUT}/redraw_{name}.csv', index=False)
    print(out.to_string(), flush=True)


def puf_2019_from_real():
    """Run A input: real PUF records, dollars grown to 2019 by the step 4 factors."""
    df = pd.read_parquet(f'{ROOT}/data/stacked.parquet')
    df = df[df['source'] == 'puf'].reset_index(drop=True)
    need = soi_cells.PARTS + ['married', 'n_dep', 'weight']
    assert not df[need].isna().any().any(), 'missing values in the PUF columns used'
    factors = pd.read_csv(f'{ROOT}/map/growth_factors.csv')
    for v, f in zip(factors['variable'], factors['factor']):
        df[v] = dollars.grow_values(v, df[v], f)
    return df


def generated_2019():
    """Run B input: generated PUF records with dollars from the 2019 map."""
    path = dollars.generated_file()
    if path is None:
        return None
    gen = pd.read_parquet(path)
    gen = gen[gen['source'] == 'puf'].reset_index(drop=True)
    if 'record_id' not in gen:
        gen['record_id'] = np.arange(len(gen))   # generated rows: id = row number
    gen, hits = dollars.draw_dollars(gen, dollars.puf_maps(2019))
    print(f'run B input {path}: {len(gen)} PUF records; filled-band draws {hits}')
    return gen


def main():
    os.makedirs(OUT, exist_ok=True)
    summary = []
    puf = puf_2019_from_real()
    w = puf['weight'].to_numpy(float)
    for name, targets in [('A0', 'totals'), ('A1', 'brackets')]:
        run(name, puf, w, targets, summary)
    run('A1_prescaled', puf, prescale(puf, w), 'brackets', summary, w_plain=w)
    gen = generated_2019()
    if gen is None:
        print('no generated file in gen/: runs B0 and B1 skipped')
    else:
        returns = target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0]
        w = np.full(len(gen), returns / len(gen))
        for name, targets in [('B0', 'totals'), ('B1', 'brackets')]:
            w1, F, T, cells = run(name, gen, w, targets, summary)
            redraw(name, w1, F, T, cells)
        run('B1_prescaled', gen, prescale(gen, w), 'brackets', summary, w_plain=w)
    pd.DataFrame(summary).to_csv(f'{OUT}/summary.csv', index=False)
    print(pd.DataFrame(summary).to_string())


if __name__ == '__main__':
    sys.exit(main())
