"""The non-microdata imputations (named by John, 28 October 2026; NEXT_STEPS.md item 1, decisions Q1-Q9).

Items set from what the model has already drawn plus a published table, with no new microdata: dependent filers' exact
age and sex, child tax credit eligibility for each dependent, death probabilities, yearly unrealized gains on each
asset and the cost basis of each asset, the holding period and basis of long-term gains, parents' income rank, and
blindness (since the second retrain of 29 October: the PUF's blind rate by age times a factor that meets Publication
4801's blind boxes); and the QBI service flags and wage bills from the items the model draws for them (since 29 October;
dependents' Social Security numbers come from the model's own draws, dollars.py, ssn1, ssn2, dep_ssn1-3) (settings non_micro_imputations, which also says why each item is what it is).

How every item works (the settings section registers each one):
  - it reads columns of the file and published tables, CSVs under research/rebuild/resources/non_micro/ (built by
    build_non_micro_tables.py from their sources; sources.csv there). A table with a year column is read at the
    target year's row; with no such row, the last year before it, or the first year for a year before the table
    starts. Which row each item used is reported;
  - it writes new columns only: it never changes a drawn item or a weight (impute checks both);
  - every random number is ages.uniform_by_id(record_id, salt_base + 8 x stream + person): a fixed function of the
    record, the item's stream and the person (0 the unit, 1 and 2 the primary filer and the spouse, 3 to 5
    dependents 1 to 3), so a draw never moves when other code or other records change;
  - the two items that rank records (death probabilities, parents' rank) rank on the weights they are given: the
    start's, prescaled, inside the tilt stage, and the ranks are then held fixed through the tilt.

Where it runs (Q1): tilt.py calls impute() on the start, after every dollar amount is in target-year dollars and the
weights are prescaled, before the solve; report_tilt.py calls checks() on the final file with the weights after the
tilt (Q9: reported only, never a stop). It can also run on a finished file, for checks:

    python non_micro_imputations.py --run <tilt folder> --out <folder> [--year 2019] [--write-file]

which drops any of these columns already in the file, imputes them again with the file's weight_start, writes
non_micro_checks.md (and the file with the columns, with --write-file) to --out. In a Slurm job, as every stage.

Conventions, as in the rest of the file: -1 for an age or a person that does not exist, 0 for no sex; flags are 1 or
0, -1 where the dependent does not exist; dollars in nominal target-year dollars.
"""
import argparse
import os

import numpy as np
import pandas as pd

import common
from ages import uniform_by_id

S = common.SETTINGS.get('non_micro_imputations', {})
ITEMS = S.get('items', {})
RES = os.path.abspath(os.path.join(common.HERE, '..', 'resources', 'non_micro'))
CPS = common.SETTINGS['cps']
PERSONS = os.path.join(common.DATA, CPS['step1_dir'], CPS['step1_out'], 'persons_{year}.parquet')
DEP_AGES = ['age_dep1', 'age_dep2', 'age_dep3']
RANGE_AGES = {6: (0, 18), 7: (18, 26), 8: (26, 200)}   # the PUF's dependent-filer age ranges (common.AGE_LABELS)
assert sorted(RANGE_AGES) == common.DEPENDENT_RANGES


# ---- shared machinery ---------------------------------------------------------------------------------------------
def table(name):
    return pd.read_csv(os.path.join(RES, name))


def year_used(years, year):
    """The table year an item reads for a target year: the year itself; else the last year before it; else the first."""
    years = sorted(set(int(y) for y in years))
    if year in years:
        return year
    before = [y for y in years if y < year]
    return before[-1] if before else years[0]


def uniform(df, item, person):
    """Each record's uniform number for an item's stream and a person (module docstring)."""
    salt = int(S['salt_base']) + 8 * int(ITEMS[item]['stream']) + person
    return uniform_by_id(df['record_id'].to_numpy(), salt)


def fill_missing_sex(df):
    """The one drawn column this module writes back (John, 28 October): a primary filer with an age and no sex (2 records
    of the model 20261019's generation, both with the sex 0 = none) is given a sex, man or woman with even chances, by
    the death item's keyed draw (stream 8, person 1: the same number the item used before the write-back, so the death
    probabilities do not move). Generation now forces the primary filer's sex to a man or a woman (generate.py,
    cps_structure), so this finds nobody in a file made after that change. Returns the frame and the rows changed."""
    nosex = (df['age_primary'].to_numpy() >= 0) & (df['sex_primary'].to_numpy() <= 0)
    df = df.copy()
    if nosex.any():
        draw = np.where(uniform(df, 'death_probabilities', 1) < 0.5, 1, 2).astype(df['sex_primary'].dtype)
        df.loc[nosex, 'sex_primary'] = draw[nosex]
    return df, nosex


def primary_age_sex(df):
    """The primary filer's exact age and sex: the drawn ones, or a dependent filer's from dependent_filer_age_sex.
    The sex is never 0 after fill_missing_sex."""
    dep = df['age_primary'].to_numpy() < 0
    age = np.where(dep, df['dep_filer_age'].to_numpy(), df['age_primary'].to_numpy())
    sex = np.where(dep, df['dep_filer_sex'].to_numpy(), df['sex_primary'].to_numpy())
    assert not (age < 0).any(), f'{int((age < 0).sum())} records with no primary age after the dependent filers\' draw'
    assert not (sex <= 0).any(), f'{int((sex <= 0).sum())} primary filers with no sex (fill_missing_sex runs first)'
    assert not (dep & (sex <= 0)).any(), 'a dependent filer with no sex after the draw'
    return age.astype(int), sex.astype(int)


def has_spouse(df):
    return (df['married'].to_numpy() == 1) & (df['age_spouse'].to_numpy() >= 0) & (df['sex_spouse'].to_numpy() > 0)


def broad_income(df):
    """Production's broad income (src/imputations/helpers.R, compute_broad_income), on the rebuild's items: wages,
    Schedules C, E and F (business_income), taxable and tax-exempt interest, dividends, long- and short-term gains,
    gross Social Security, gross pensions and IRA distributions, unemployment compensation."""
    cols = ['wages', 'business_income', 'interest', 'tax_exempt_interest', 'dividends', 'kg_lt', 'kg_st', 'ss_gross',
            'retirement_gross', 'unemployment']
    x = df[cols].to_numpy(float).sum(1)
    assert np.isfinite(x).all(), 'broad income: a missing item'
    return x


# ---- 1. dependent filers' exact age and sex (Q2) ------------------------------------------------------------------
def cps_dependents(year):
    """The filing stage's persons of a CPS year who are someone's dependent, with their earnings."""
    p = pd.read_parquet(PERSONS.format(year=year), columns=['age', 'sex', 'weight', 'dep_type', 'p_own_return', 'wage',
                                                           'selfemp', 'farm_selfemp'])
    p = p[p['dep_type'].fillna('').astype(str) != ''].copy()
    p['earnings'] = p['wage'] + p['selfemp'] + p['farm_selfemp']
    return p


def dependent_filer_age_sex(df, year, w, ctx):
    it = ITEMS['dependent_filer_age_sex']
    rng, cy = df['age_range'].to_numpy(), df['cps_year'].to_numpy()
    dep = np.isin(rng, common.DEPENDENT_RANGES)
    age, sex = np.full(len(df), -1, int), np.zeros(len(df), int)
    u = uniform(df, 'dependent_filer_age_sex', 1)
    thr = table('dependent_filing_threshold.csv').set_index('year')['earned_income_threshold']
    pools = []
    for y in sorted(np.unique(cy[dep])):
        p = cps_dependents(int(y))
        ty = year_used(thr.index, int(y))
        for r, (lo, hi) in RANGE_AGES.items():
            inr = (p['age'] >= lo) & (p['age'] < hi)
            pool, used = inr & (p['earnings'] > thr[ty]), it['pool']
            first = int(pool.sum())
            if first < it['min_pool_records']:
                pool, used = inr & (p['p_own_return'] == 1), it['fallback_pool']
            assert pool.sum() > 0, f'no CPS dependents to draw from: CPS year {y}, range {common.AGE_LABELS[r]}'
            q = p[pool].assign(age=lambda d: np.minimum(d['age'], it['top_age']))
            cells = q.groupby(['age', 'sex'], as_index=False)['weight'].sum().sort_values(['age', 'sex'])
            cdf = np.cumsum(cells['weight'].to_numpy()) / cells['weight'].sum()
            m = dep & (rng == r) & (cy == y)
            k = np.minimum(np.searchsorted(cdf, u[m], side='right'), len(cells) - 1)
            age[m], sex[m] = cells['age'].to_numpy()[k], cells['sex'].to_numpy()[k]
            pools.append({'CPS year': int(y), 'range': common.AGE_LABELS[r], 'threshold year': ty,
                          'earnings threshold': float(thr[ty]),
                          'sample people earning above it': first, 'pool used': used,
                          'sample people in the pool': int(pool.sum()),
                          'pool (millions, CPS weights)': p.loc[pool, 'weight'].sum() / 1e6,
                          'file records drawn': int(m.sum())})
            ctx.setdefault('pool_cells', []).append(cells.assign(cps_year=int(y), range=r))
    ctx['dependent filer pools'] = pd.DataFrame(pools)
    ctx['notes'].append('dependent filers\' ages and sex: ' + '; '.join(
        f"CPS {r['CPS year']} {r['range']}: {r['pool used']} ({r['sample people in the pool']} people)" for r in pools))
    return {'dep_filer_age': age, 'dep_filer_sex': sex}


# ---- 2. child tax credit eligibility for each dependent ----------------------------------------------------------
def child_credit(df, year, w, ctx):
    lim = table('ctc_age_limit.csv').set_index('year')['under_age']
    ly = year_used(lim.index, year)
    law = int(lim[ly])
    ages = df[DEP_AGES].to_numpy(float)
    n = df['n_ctc'].to_numpy(float)
    exists = ages >= 0
    under17 = exists & (ages < 17)
    n16u = under17.sum(1)
    anyu = n16u > 0
    share = float(w[anyu & (n16u > n)].sum() / w[anyu].sum())   # production's share_17yo_ineligible, on the start
    flag = under17 & (n[:, None] >= np.arange(1, 4)[None, :])
    by_chance = exists & (ages >= 17) & (ages <= max(17, law - 1))
    u = np.column_stack([uniform(df, 'child_credit', 3 + j) for j in range(3)])
    flag = np.where(by_chance, u > share, flag)
    ctx['ctc'] = {'law_under_age': law, 'table_year': ly, 'share_17_not_eligible': share}
    ctx['notes'].append(f'child credit: law age under {law} (table year {ly}); 17-year-olds flagged with chance '
                        f'{1 - share:.4f}')
    return {f'dep_ctc{j + 1}': np.where(exists[:, j], flag[:, j].astype(int), -1) for j in range(3)}


# ---- 3. death probabilities ---------------------------------------------------------------------------------------
def life_table_q(year, top):
    """{(sex, age): q} for the table year; age top means top and over: the L-weighted average of q over top-119."""
    lt = table('ssa_period_life_table.csv')
    ly = year_used(lt['year'], year)
    t = lt[lt.year == ly]
    q = {(int(s), int(a)): float(v) for s, a, v in zip(t.sex, t.age, t.q) if a < top}
    for s in (1, 2):
        old = t[(t.sex == s) & (t.age >= top)]
        q[(s, top)] = float((old.q * old.L).sum() / old.L.sum())
    return q, ly


def chetty_bin(age):
    return np.select([age < 50, age < 60, age < 70], ['<50', '50-59', '60-69'], '70+')


def within_cell_percentile(sex, age, income, ids, w):
    """Each person's percentile (1-100) of income within their sex and single year of age, weighted, as production's
    build_chetty_pctile (ties broken by record id)."""
    d = pd.DataFrame({'sex': sex, 'age': age, 'income': income, 'id': ids, 'w': w, 'i': np.arange(len(sex))})
    d = d.sort_values(['sex', 'age', 'income', 'id'], kind='mergesort')
    cum = d.groupby(['sex', 'age'])['w'].cumsum() / d.groupby(['sex', 'age'])['w'].transform('sum')
    out = np.empty(len(sex), int)
    out[d['i'].to_numpy()] = np.clip(np.ceil(100 * cum.to_numpy() - 1e-9), 1, 100).astype(int)
    return out


def death_probabilities(df, year, w, ctx):
    it = ITEMS['death_probabilities']
    top = int(it['top_age'])
    q, ly = life_table_q(year, top)
    a1, s1 = primary_age_sex(df)
    a1 = np.minimum(a1, top)
    sp = has_spouse(df)
    a2 = np.minimum(df['age_spouse'].to_numpy().astype(int), top)
    s2 = df['sex_spouse'].to_numpy().astype(int)
    married = df['married'].to_numpy() == 1
    inc, ids = broad_income(df), df['record_id'].to_numpy()
    # one row per person: every primary filer, and every spouse
    P = pd.DataFrame({'rec': np.r_[np.arange(len(df)), np.flatnonzero(sp)], 'person': np.r_[np.ones(len(df), int),
                      np.full(sp.sum(), 2)], 'age': np.r_[a1, a2[sp]], 'sex': np.r_[s1, s2[sp]],
                      'married': np.r_[married, np.ones(sp.sum(), bool)]})
    P['w'], P['income'], P['id'] = w[P.rec], inc[P.rec], ids[P.rec]
    P['pctile'] = within_cell_percentile(P.sex.to_numpy(), P.age.to_numpy(), P.income.to_numpy(), P.id.to_numpy(),
                                         P.w.to_numpy())
    # the billionaires (top_billionaires.py, 29 October; PLAN.md step 17): income percentile 100, set explicitly, as
    # production does, for the primary filer and the spouse alike
    n_bil = 0
    if 'billionaire' in df:
        bil = df['billionaire'].to_numpy()[P.rec.to_numpy()] == 1
        P.loc[bil, 'pctile'] = 100
        n_bil = int(bil.sum())
    base = np.array([q[(s, a)] for s, a in zip(P.sex, P.age)])
    cr = table('chetty_mortality_ratio.csv').set_index(['sex', 'age_bin', 'pctile'])['ratio']
    rc = cr.reindex(pd.MultiIndex.from_arrays([P.sex, chetty_bin(P.age.to_numpy()), P.pctile])).to_numpy()
    assert np.isfinite(rc).all(), 'Chetty ratio: a person with no cell'
    jh = table('johnson_marital_factor.csv').set_index('age_band')
    band = np.where(P.age < 65, 'under_65', 'age_65_plus')
    raw = np.where(P.married, jh.loc[band, 'married'].to_numpy(), jh.loc[band, 'unmarried'].to_numpy())
    men = (P.sex == 1).to_numpy()
    rm = np.ones(len(P))
    rescale = {}
    for b in ['under_65', 'age_65_plus']:            # rescaled to average 1 among the file's men in the band
        m = men & (band == b)
        rescale[b] = float((P.w[m] * raw[m]).sum() / P.w[m].sum())
        rm[m] = raw[m] / rescale[b]
    qd = base * rc * rm
    assert (qd > 0).all() and (qd < 1).all(), 'a death probability outside (0, 1)'
    P['q'], P['q_ssa'] = qd, base
    q1 = np.full(len(df), -1.0)
    q2 = np.full(len(df), -1.0)
    one = P.person == 1
    q1[P.rec[one]] = qd[one]
    q2[P.rec[~one]] = qd[~one]
    ctx['deaths'] = P[['rec', 'person', 'age', 'sex', 'q', 'q_ssa']]
    ctx['notes'].append(f'death probabilities: SSA period life table {ly}; Johnson factors divided by the file\'s mean '
                        f'among men ({", ".join(f"{k} {v:.4f}" for k, v in rescale.items())}); '
                        f'{int((married & ~sp).sum())} married records with no spouse age or sex get no q_death2; '
                        f'primary filers without a sex were given one first (fill_missing_sex); '
                        f'{n_bil} billionaire people (Forbes list) placed at income percentile 100')
    return {'q_death1': q1, 'q_death2': q2}


# ---- 4. yearly unrealized gains and cost basis --------------------------------------------------------------------
ACCRUAL_ASSETS = {'equities': ('w_equities', 'equities'), 'pass_throughs': ('w_pass_throughs', 'pass_throughs'),
                  'primary_home': ('w_primary_home', 'real_estate'), 'other_home': ('w_other_home', 'real_estate'),
                  're_fund': ('w_re_fund', 'real_estate')}


def accrual_rates(year):
    """The year's revaluation rates by asset (stocks, businesses, real estate, bonds): Z.1 for a year it covers in full,
    else production's long-run rates. Returns the rates and where they came from."""
    z = table('z1_revaluation_rates.csv')
    need = ['equities', 'pass_throughs', 'real_estate', 'bonds']
    have = z[z.year == year].set_index('asset')['rate']
    if all(a in have for a in need):
        return {a: float(have[a]) for a in need}, f'Z.1 {year}'
    lr = table('accrual_long_run_rates.csv').set_index('asset')['rate']
    return {a: float(lr[a]) for a in need}, 'production\'s long-run rates (no full year of Z.1 data)'


def retirement_stock_share_placeholder(age_older):
    """PLACEHOLDER (Q6): the retirement accounts' stock share by production's age rule, 1.10 - age / 100 held to
    0.20-0.95, until the SCF's stock share is learned in pretraining."""
    r = ITEMS['unrealized_gains']['retirement_stock_share_rule']
    return np.clip(r['intercept'] + r['per_year_of_age'] * age_older, r['lowest'], r['highest'])


def unrealized_gains(df, year, w, ctx):
    it = ITEMS['unrealized_gains']
    rate, src = accrual_rates(year)
    if 'w_dc_stock_share' in df:     # (29 October, Q6) the SCF's stock share, drawn in generation
        share = df['w_dc_stock_share'].to_numpy(float)
        assert np.isfinite(share).all() and (share >= 0).all() and (share <= 1 + 1e-9).all(), 'stock share outside 0-1'
        dc = df['w_dc'].to_numpy(float)
        ctx['notes'].append(f'retirement accounts\' stock share: the SCF\'s, drawn; {100 * (w * share * dc).sum() / (w * dc).sum():.1f}% '
                            'of the dollars in w_dc (start weights)')
        ctx['stock share'] = 'drawn'
    else:                            # a file from a model before 29 October: production's age rule
        a1, _ = primary_age_sex(df)
        older = np.maximum(a1, df['age_spouse'].to_numpy())
        share = retirement_stock_share_placeholder(older)
        ctx['stock share'] = 'age rule'
    out = {f'accruals.{k}': rate[r] * df[col].to_numpy(float) for k, (col, r) in ACCRUAL_ASSETS.items()}
    ts = float(it['trust_stock_share'])
    out['accruals.dc'] = (share * rate['equities'] + (1 - share) * rate['bonds']) * df['w_dc'].to_numpy(float)
    out['accruals.trusts'] = (ts * rate['equities'] + (1 - ts) * rate['bonds']) * df['w_trusts'].to_numpy(float)
    ctx['accrual rates'] = {'source': src, **rate}
    ctx['notes'].append(f'unrealized gains: {src}: ' + ', '.join(f'{k} {100 * v:.2f}%' for k, v in rate.items()))
    return out


def cost_basis(df, year, w, ctx):
    w = np.asarray(w, float)
    oh, rf = df['w_other_home'].to_numpy(float), df['w_re_fund'].to_numpy(float)
    tot = oh + rf
    s = np.divide(oh, tot, out=np.full(len(df), 0.5), where=tot > 0)
    def held(g, v):    # a holding's share of the gain, held to its value exactly (the split's rounding can leave a hair over)
        return np.where(v > 0, np.minimum(g, v), g)

    gain = {'equities': df['w_kg_other'].to_numpy(float), 'pass_throughs': df['w_kg_pass_throughs'].to_numpy(float),
            'primary_home': df['w_kg_primary_home'].to_numpy(float)}
    value = {'equities': df['w_equities'].to_numpy(float), 'pass_throughs': df['w_pass_throughs'].to_numpy(float),
             'primary_home': df['w_primary_home'].to_numpy(float)}
    # other real estate: one gain on the two holdings together, held to their combined value (each share of it is then
    # at most that holding's value, because the split is by value)
    gain['other_re'], value['other_re'] = df['w_kg_other_re'].to_numpy(float), tot
    out, cut = {}, []
    for k in gain:     # hold each gain to its value: basis = value - min(gain, value), never below 0 (John, 28 October)
        g = np.minimum(gain[k], value[k])
        cut.append({'asset': 'other_homes and other real estate (one gain)' if k == 'other_re' else k,
                    'units capped': int((g < gain[k]).sum()), 'weighted units (millions)': w[g < gain[k]].sum() / 1e6,
                    'gain removed ($ billion, weights as given)': (w * (gain[k] - g)).sum() / 1e9})
        gain[k] = g
    ctx['basis capped'] = pd.DataFrame(cut)
    ctx['notes'].append('cost basis, drawn gains held to the value: ' + '; '.join(
        f'{r.asset} {r["units capped"]:,} records' for _, r in ctx['basis capped'].iterrows()))
    return {'basis.equities': value['equities'] - gain['equities'],
            'basis.pass_throughs': value['pass_throughs'] - gain['pass_throughs'],
            'basis.primary_home': value['primary_home'] - gain['primary_home'],
            'basis.other_home': oh - held(gain['other_re'] * s, oh),
            'basis.re_fund': rf - held(gain['other_re'] * (1 - s), rf)}


# ---- 5. holding period and basis of long-term gains (Q7) ----------------------------------------------------------
BUCKET_LO = np.array([1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 10.0, 15.0, 20.0])
BUCKET_HI = np.array([1.5, 2.0, 3.0, 4.0, 5.0, 10.0, 15.0, 20.0, np.inf])


def sp_index(x):
    """The S&P 500 total return index at (fractional) years x: linear between years, the first value before the
    series, and past its last year the last value grown at the series' geometric mean return."""
    s = table('sp500_total_return_index.csv')
    yr, ix = s['year'].to_numpy(float), s['index'].to_numpy(float)
    g = (ix[-1] / ix[0]) ** (1 / (yr[-1] - yr[0])) - 1
    x = np.asarray(x, float)
    return np.where(x > yr[-1], ix[-1] * (1 + g) ** (x - yr[-1]), np.interp(x, yr, ix))


def holding_model(year):
    """Production's pieces (capital_gains.R) at a year: the groups' gain and loss shares, the Weibull, the
    exponential tail's rate, the representative length of the top group, and the basis-to-sales ratio functions."""
    it = ITEMS['gains_holding_period']
    k, sc = it['weibull']['shape'], it['weibull']['scale']
    W = lambda x: 1 - np.exp(-(np.asarray(x, float) / sc) ** k)
    Winv = lambda p: sc * (-np.log1p(-np.asarray(p, float))) ** (1 / k)
    dens19 = k / sc * (19 / sc) ** (k - 1) * np.exp(-(19 / sc) ** k)
    g = table('soca_gain_holding_shares.csv')
    lo = table('soca_loss_holding_shares.csv')
    pi_g, pi_l = g['pi_g'].to_numpy(float), lo['pi_l'].to_numpy(float)
    lam = dens19 / (W(19) - W(14)) * pi_g[7] / pi_g[8]
    h_top = 20 + 1 / lam
    # the regression on SOCA's panel: log(ratio) = a[group] + b x h x log(1 + trailing return), no common constant
    sb = table('soca_basis_sales.csv')
    groups = list(g['h_bucket'])
    assert set(sb['bucket']) == set(groups), 'SOCA panel groups differ from the gain shares\' groups'
    X = np.column_stack([(sb['bucket'] == b).to_numpy(float) for b in groups] +
                        [np.log(sp_index(sb['year']) / sp_index(sb['year'] - sb['h']))])
    coef, *_ = np.linalg.lstsq(X, np.log(sb['basis_sales_ratio'].to_numpy(float)), rcond=None)
    knots = np.r_[g['h_midpoint'].to_numpy(float)[:8], h_top]
    pred = np.exp(coef[:9] + coef[9] * np.log(sp_index(year) / sp_index(year - knots)))
    g_ex = (1 / pred[8]) ** (1 / h_top) - 1
    gain_ratio = lambda h: np.where(h <= h_top, np.interp(h, knots, pred), 1 / (1 + g_ex) ** h)
    loss_ratio = lambda h: np.interp(h, np.r_[lo['h_midpoint'].to_numpy(float)[:8], h_top],
                                     lo['loss_bsr'].to_numpy(float))
    return {'groups': groups, 'pi_g': pi_g, 'pi_l': pi_l, 'W': W, 'Winv': Winv, 'lam': lam, 'h_top': h_top,
            'coef': coef, 'knots': knots, 'pred': pred, 'gain_ratio': gain_ratio, 'loss_ratio': loss_ratio}


def draw_holding(u1, u2, probs, M):
    cdf = np.cumsum(probs) / probs.sum()
    b = np.minimum(np.searchsorted(cdf, u1, side='right'), len(probs) - 1)
    lo, hi = BUCKET_LO[b], BUCKET_HI[b]
    top = lo >= 20
    flo, fhi = M['W'](lo - 1), M['W'](np.where(top, 30.0, hi) - 1)
    h_in = 1 + M['Winv'](flo + u2 * (fhi - flo))
    h_top = 20 - np.log1p(-u2) / M['lam']
    return np.where(top, h_top, h_in), b


def gains_holding_period(df, year, w, ctx):
    w = np.asarray(w, float)
    M = holding_model(year)
    kg = df['kg_lt'].to_numpy(float)
    u1, u2 = uniform(df, 'gains_holding_period', 1), uniform(df, 'gains_holding_period', 2)
    years_held, basis = np.full(len(df), -1.0), np.zeros(len(df))
    group = np.full(len(df), -1)
    # a holding period is at most the owner's age less 18 (John, 28 October). The owner is the older of the primary
    # filer and the spouse (the gain is the couple's; either may hold the asset); no spouse, or none with an age, is
    # the primary filer's age alone. The cap is held to at least 1 year, the shortest long-term holding period, so a
    # filer under 19 holds for a year.
    a1, _ = primary_age_sex(df)
    oldest = np.maximum(a1, df['age_spouse'].to_numpy().astype(int))
    cap = np.maximum(oldest - 18, 1).astype(float)
    drawn = np.full(len(df), np.nan)
    for sign, probs in [(1, M['pi_g']), (-1, M['pi_l'])]:
        m = np.sign(kg) == sign
        h, b = draw_holding(u1[m], u2[m], probs, M)
        drawn[m] = h
        h = np.minimum(h, cap[m])
        years_held[m], group[m] = h, b
        if sign == 1:
            r = M['gain_ratio'](h)
            basis[m] = kg[m] * r / (1 - r)
        else:
            r = M['loss_ratio'](h)
            basis[m] = -kg[m] * r / (r - 1)
    assert np.isfinite(basis).all() and (basis[kg != 0] > 0).all(), 'a long-term gain with no positive basis'
    capped = ~np.isnan(drawn) & (drawn > cap)
    ctx['holding capped'] = {'records': int(capped.sum()), 'units': float(w[capped].sum()),
                             'long-term records': int((kg != 0).sum()), 'units with long-term': float(w[kg != 0].sum()),
                             'longest before': float(np.nanmax(drawn)), 'longest after': float(years_held.max()),
                             'under 19 (cap 1 year)': int(((oldest < 19) & (kg != 0)).sum())}
    ctx['holding'] = {'model': M, 'group': group}
    ctx['notes'].append(f'holding period: basis-to-sales regression at {year}\'s trailing S&P 500 returns; ratio by '
                        f'group ' + ', '.join(f'{r:.3f}' for r in M['pred']) + f'; top group length {M["h_top"]:.1f} years')
    return {'kg_lt_years_held': years_held, 'kg_lt_basis': basis}


# ---- 6. parents' income rank (Q8) ---------------------------------------------------------------------------------
def mobility_income(df):
    """Production's income for the rank (src/imputations/mobility.R) on the rebuild's items: wages, interest,
    dividends, state refunds, taxable IRA and pension distributions, long- and short-term gains, other gains, Schedules
    C, E and F, unemployment, gross Social Security and other income."""
    cols = ['wages', 'interest', 'dividends', 'state_refunds', 'ira_taxable', 'pensions_taxable', 'kg_lt', 'kg_st',
            'other_gains', 'business_income', 'unemployment', 'ss_gross', 'other_agi_remainder']
    x = df[cols].to_numpy(float).sum(1)
    assert np.isfinite(x).all()
    return x


def own_rank(x, w):
    """Percentile of x among all units (1-100: the share of weight at or below x, times 100, rounded up), the
    bottom 10 merged into 10, as production's cut() at the weighted percentiles intends."""
    o = np.argsort(x, kind='mergesort')
    xs, cw = x[o], np.cumsum(w[o]) / w.sum()
    last = np.r_[xs[1:] != xs[:-1], True]                 # the cumulative share at the last of each tie
    F_sorted = cw[last][np.cumsum(np.r_[0, last[:-1].astype(int)])]
    F = np.empty(len(x))
    F[o] = F_sorted
    return np.clip(np.ceil(100 * F - 1e-9), 10, 100).astype(int)


def mobility_rows():
    """{child rank 10-100: the chances of parent ranks 1-100}: the matrix's row (child ranks under 10 summed into 10)."""
    m = pd.read_csv(os.path.join(RES, 'mobility_matrix.csv'), encoding='utf-8-sig')
    m = m[['child_rank'] + [str(i) for i in range(1, 101)]]
    m['child_rank'] = np.maximum(10, m['child_rank'])
    m = m.groupby('child_rank').sum()
    assert list(m.index) == list(range(10, 101)), 'mobility matrix: a child rank from 10 to 100 missing'
    return m.to_numpy(float)


def parents_rank(df, year, w, ctx):
    child = own_rank(mobility_income(df), w)
    rows = mobility_rows()
    cdf = np.cumsum(rows, 1) / rows.sum(1, keepdims=True)
    u = uniform(df, 'parents_rank', 0)
    parent = np.zeros(len(df), int)
    for c in np.unique(child):            # the first parent rank whose cumulative chance reaches u, as production
        m = child == c
        parent[m] = 1 + np.minimum(np.searchsorted(cdf[c - 10], u[m], side='left'), 99)
    ctx['child_rank'] = child
    return {'parent_rank': parent.astype(int)}


# ---- placeholders -------------------------------------------------------------------------------------------------
def blind_factor(key, filer, w, target):
    """The factor for one person type: each person is blind when key = u / rate is below the factor (u the person's
    keyed uniform number), so the chance is rate x factor. The factor is set so the filers' weighted count of blind
    people comes nearest the target (the drawn count, not only its expectation). Returns the factor and that count."""
    k, ww = key[filer], w[filer]
    o = np.argsort(k, kind='stable')
    cum = np.cumsum(ww[o])
    i = int(np.argmin(np.abs(cum - target)))
    lo = k[o][i]
    hi = k[o][i + 1] if i + 1 < len(o) else lo * (1 + 1e-9) + 1e-12
    return (lo + hi) / 2, float(cum[i])


def blindness(df, year, w, ctx):
    """Q3, as a rule (John, second retrain of 29 October; it replaces the PUF slot learned in pretraining). For each
    primary filer, and each spouse on a married-joint record (mars 2), a draw keyed by record id and person (stream 6,
    persons 1 and 2), blind with chance = the PUF's blind rate for the person's age range (puf_blind_rates.csv: the 2015
    PUF where the return shows blindness) times one factor per person type. Each factor makes the file's weighted count
    of blind people among filers, at the weights given (the start's, before reweighting), equal Publication 4801's blind
    boxes for the target year (pub4801_blind_boxes.csv: 'you are blind' for primary filers, 'spouse is blind' for
    spouses). Non-filers are drawn by the same rule and not counted. 1 blind, 0 not."""
    rates = table('puf_blind_rates.csv').sort_values('age_from')
    cuts = rates['age_from'].to_numpy(float)
    rate = rates['blind_rate'].to_numpy(float)
    assert len(rate) == common.ADULT_RANGES and (rate > 0).all() and (rate < 1).all()
    a1, _ = primary_age_sex(df)
    joint = (df['mars'].to_numpy() == 2) & has_spouse(df)
    a2 = df['age_spouse'].to_numpy()
    assert (a2[joint] >= 0).all()
    r1 = rate[np.searchsorted(cuts, a1, side='right') - 1]
    r2 = np.where(joint, rate[np.searchsorted(cuts, np.maximum(a2, 0), side='right') - 1], 0.0)
    filer = df['filer'].to_numpy() == 1
    p = table('pub4801_blind_boxes.csv')
    py = year_used(p['year'], year)
    box = p[p.year == py].iloc[0]
    out, info = {}, {'pub4801 year': py}
    for person, r, who, target, m in [(1, r1, 'primary', float(box.primary_blind), np.ones(len(df), bool)),
                                      (2, r2, 'spouse', float(box.spouse_blind), joint)]:
        u = uniform(df, 'blindness', person)
        key = np.where(m, u / np.where(m, r, 1.0), np.inf)
        f, drawn = blind_factor(key, filer & m, w, target)
        b = (key < f).astype(int)
        assert (r * f <= 1).all(), f'blindness: a {who} chance above 1 (factor {f:.3f})'
        expected = (w * r * f)[filer & m].sum()
        out[f'blind{person}'] = b
        info[who] = {'factor': f, 'target': target, 'drawn, filers': float(w[filer & (b == 1)].sum()),
                     'expected, filers': expected, 'drawn, non-filers': float(w[~filer & (b == 1)].sum())}
    ctx['blind'] = info
    ctx['notes'].append('blindness: PUF rate by age x factor (primary {:.3f}, spouse {:.3f}); blind filers at the start\'s '
                        'weights {:,.0f} primary and {:,.0f} spouses against Publication 4801 {} ({:,.0f}, {:,.0f})'.format(
                            info['primary']['factor'], info['spouse']['factor'], info['primary']['drawn, filers'],
                            info['spouse']['drawn, filers'], py, info['primary']['target'], info['spouse']['target']))
    return {'blind1': out['blind1'], 'blind2': out['blind2']}


QBI_KINDS = {  # file kind: (industry slot, workers slot, the PUF parts whose signed sum is the kind's income, person)
    'sole_prop': ('biz_industry_schc', 'biz_workers_schc', {'sch_c': 1}, 1),
    'part': ('biz_industry_part', 'biz_workers_part', {'part_pass_inc': 1, 'part_nonpass_inc': 1, 'part_pass_loss': -1,
                                                       'part_nonpass_loss': -1, 'part_179': -1}, 2),
    'scorp': ('biz_industry_scorp', 'biz_workers_scorp', {'scorp_pass_inc': 1, 'scorp_nonpass_inc': 1,
                                                          'scorp_pass_loss': -1, 'scorp_nonpass_loss': -1,
                                                          'scorp_179': -1}, 3)}


def qbi_income(df, parts):
    return sum(sg * df[p].to_numpy(float) for p, sg in parts.items())


def qbi(df, year, w, ctx):
    """Q5 (John, 28 October; the simple plan, scratch explore/2026-10-28/qbi_plan/SIMPLE_PLAN.md; built 29 October).
    For each record and kind of business (Schedule C, partnership, S corporation) with positive income of that kind:
      - specified service (sstb_<kind>): 1 when the record's keyed draw (stream 7, person 1-3 by kind) is below
        b x c(group), held to at most 1; c(group) per SCF industry group (settings c_by_scf_group), the group the
        model drew for the kind (biz_industry_<kind>); for group 8 (no industry code), and for none (0, which generation
        allows only without such income), the average of c over groups 1-7 weighted by the kind's positive income on
        the file (start weights), all three kinds together;
      - wage bill (wagebill_<kind>): a x the owner's share of outside workers drawn (biz_workers_<kind>) x the average
        pay of private employees in the group in the target year (cps_private_pay_by_scf_group.csv; group 8 and none:
        all private employees); for an S corporation plus the owner's own wages: in this first version all of the
        record's wages when it has S corporation income and no Schedule C or partnership income.
    Farms (positive farm income): never specified service; wage bill = the farm income, so the wage limit never binds.
    Otherwise 0. a and b are settings, 1 for now (two dials, set later against Treasury's and SOI's numbers).
    A file from a model before 29 October has no drawn industry: every item 0 (the placeholder of 28 October)."""
    it = ITEMS['qbi']
    writes = it['writes']
    if not all(QBI_KINDS[k][0] in df for k in QBI_KINDS):
        ctx['qbi'] = None
        ctx['notes'].append('QBI: the file has no drawn industry groups (a model from before 29 October): all 0')
        return {c: np.zeros(len(df), float if c.startswith('wagebill') else int) for c in writes}
    a, b = float(it['a']), float(it['b'])
    c = {int(g): float(v) for g, v in it['c_by_scf_group'].items()}
    assert sorted(c) == list(range(1, 8)), 'c_by_scf_group: groups 1-7'
    pay = table('cps_private_pay_by_scf_group.csv')
    py = year_used(pay['year'], year)
    pay = pay[pay.year == py].set_index('scf_industry_group')['average_pay']
    assert sorted(pay.index) == list(range(1, 9)), 'pay table: groups 1-8'
    inc = {k: qbi_income(df, parts) for k, (_, _, parts, _) in QBI_KINDS.items()}
    grp = {k: df[ind].to_numpy().astype(int) for k, (ind, _, _, _) in QBI_KINDS.items()}
    for k, g in grp.items():
        assert np.isin(g, range(9)).all(), f'{k}: industry group outside 0-8'
    # the chance for no industry code (and none): c over groups 1-7, weighted by positive income of the kind
    num = sum((w * np.maximum(inc[k], 0) * np.vectorize(lambda x: c.get(x, 0.0))(grp[k]) * (grp[k] >= 1) * (grp[k] <= 7)).sum()
              for k in QBI_KINDS)
    den = sum((w * np.maximum(inc[k], 0) * (grp[k] >= 1) * (grp[k] <= 7)).sum() for k in QBI_KINDS)
    c_other = float(num / den)
    out, rows = {}, []
    only_scorp = (inc['scorp'] > 0) & (df['sch_c'].to_numpy(float) == 0) & \
        np.column_stack([df[p].to_numpy(float) == 0 for p in QBI_KINDS['part'][2]]).all(1)
    for k, (ind, wk, parts, person) in QBI_KINDS.items():
        g = grp[k]
        on = inc[k] > 0
        chance = np.minimum(1.0, b * np.where((g >= 1) & (g <= 7), np.vectorize(lambda x: c.get(x, 0.0))(g), c_other))
        u = uniform(df, 'qbi', person)
        sstb = (on & (u < chance)).astype(int)
        p = np.where((g >= 1) & (g <= 8), pay.reindex(np.clip(g, 1, 8)).to_numpy(float), float(pay[8]))
        workers = df[wk].to_numpy(float)
        assert (workers >= 0).all() and np.isfinite(workers).all(), f'{wk}: below 0 or missing'
        bill = np.where(on, a * workers * p, 0.0)
        if k == 'scorp':
            bill = bill + np.where(on & only_scorp, df['wages'].to_numpy(float), 0.0)
        out[f'sstb_{k}'], out[f'wagebill_{k}'] = sstb, bill
        rows.append({'kind': k, 'units with positive income (millions)': w[on].sum() / 1e6,
                     'income ($ billion)': (w * np.maximum(inc[k], 0)).sum() / 1e9,
                     'industry none where income (millions, generation allows none only without it)': w[on & (g == 0)].sum() / 1e6,
                     'no industry code (millions)': w[on & (g == 8)].sum() / 1e6,
                     'specified service (millions)': w[sstb == 1].sum() / 1e6,
                     'specified service share of the income (%)': 100 * (w * np.maximum(inc[k], 0) * sstb).sum() / max((w * np.maximum(inc[k], 0)).sum(), 1),
                     'with outside workers (millions)': w[on & (workers > 0)].sum() / 1e6,
                     'wage bill ($ billion)': (w * bill).sum() / 1e9})
    farm = df['farm'].to_numpy(float)
    out['sstb_farm'] = np.zeros(len(df), int)
    out['wagebill_farm'] = np.maximum(farm, 0.0)
    rows.append({'kind': 'farm', 'units with positive income (millions)': w[farm > 0].sum() / 1e6,
                 'income ($ billion)': (w * np.maximum(farm, 0)).sum() / 1e9, 'specified service (millions)': 0.0,
                 'wage bill ($ billion)': (w * out['wagebill_farm']).sum() / 1e9})
    ctx['qbi'] = {'table': pd.DataFrame(rows), 'a': a, 'b': b, 'c_other': c_other, 'pay_year': py}
    ctx['notes'].append(f'QBI: a = {a:g}, b = {b:g}; chance for no industry code {c_other:.4f}; pay table year {py}; '
                        f'S corporation owners\' own wages added on {w[only_scorp].sum() / 1e6:.2f} million units')
    return {col: out[col] for col in writes}


PLACEHOLDER_VALUES = {'dependent_ssn': 'every dependent has a number (1)', 'blindness': 'nobody is blind (0)',
                      'qbi': 'no specified service business (0), no wages paid (0)'}

FUNCTIONS = {'dependent_filer_age_sex': dependent_filer_age_sex, 'child_credit': child_credit,
             'death_probabilities': death_probabilities,
             'unrealized_gains': unrealized_gains, 'cost_basis': cost_basis,
             'gains_holding_period': gains_holding_period, 'parents_rank': parents_rank, 'blindness': blindness,
             'qbi': qbi}


def written_columns():
    return [c for it in ITEMS.values() for c in it['writes']]


def impute(df, year, w):
    """Every registered item on the frame, in the settings' order, with weights w for the ranks. Returns the frame
    with the new columns and a context (notes for the summary, pieces for the checks)."""
    assert list(ITEMS) == list(FUNCTIONS), 'settings non_micro_imputations.items and FUNCTIONS differ'
    clash = [c for c in written_columns() if c in df]
    assert not clash, f'columns the non-microdata imputations write are already in the file: {clash}'
    w = np.asarray(w, float)
    assert len(w) == len(df) and np.isfinite(w).all() and (w >= 0).all()
    out, nosex = fill_missing_sex(df)      # the only drawn column changed: sex_primary, where it was none
    before = {c: out[c].to_numpy(copy=True) for c in df.columns}
    ctx = {'notes': [], 'year': year, 'sex filled': {'records': int(nosex.sum()), 'units': float(w[nosex].sum()),
                                                      'record_ids': df['record_id'].to_numpy()[nosex].tolist()}}
    ctx['notes'].append(f'primary filers with an age and no sex given one: {int(nosex.sum())} records '
                        f'({w[nosex].sum():,.0f} units at the start\'s weights)')
    for name, f in FUNCTIONS.items():
        cols = f(out, year, w, ctx)
        assert list(cols) == ITEMS[name]['writes'], f'{name}: wrote {list(cols)}, settings say {ITEMS[name]["writes"]}'
        for c, v in cols.items():
            v = np.asarray(v)
            assert len(v) == len(out) and not pd.isna(v).any(), f'{name}: {c} has missing values'
            out[c] = v
    for c in df.columns:   # nothing drawn changed
        assert np.array_equal(before[c], out[c].to_numpy(), equal_nan=before[c].dtype.kind == 'f'), f'{c} changed'
    common.say('non-microdata imputations: ' + ' | '.join(ctx['notes']))
    return out, ctx


# ---- checks (Q9: reported only) -----------------------------------------------------------------------------------
def _bn(x):
    return x / 1e9


def check_dependent_filers(df, w, ctx):
    dep = np.isin(df['age_range'].to_numpy(), common.DEPENDENT_RANGES)
    a, s = df['dep_filer_age'].to_numpy(), df['dep_filer_sex'].to_numpy()
    bands = [(0, 15, 'under 15'), (15, 16, '15'), (16, 17, '16'), (17, 18, '17'), (18, 20, '18-19'),
             (20, 22, '20-21'), (22, 26, '22-25'), (26, 35, '26-34'), (35, 55, '35-54'), (55, 200, '55 and over')]
    cells = pd.concat(ctx['pool_cells'])
    mix = pd.Series(w[dep], index=df['cps_year'].to_numpy()[dep]).groupby(level=0).sum()
    mix = mix / mix.sum()
    rows = []
    for lo, hi, lab in bands:
        m = dep & (a >= lo) & (a < hi)
        c = cells[(cells.age >= lo) & (cells.age < hi)]
        r = [k for k, (rl, rh) in RANGE_AGES.items() if rl <= lo < rh][0]
        tot = cells[cells.range == r].groupby('cps_year')['weight'].sum()
        pool_share = sum(mix.get(y, 0) * c[(c.cps_year == y)]['weight'].sum() / tot[y] for y in tot.index)
        pool_men = sum(mix.get(y, 0) * c[(c.cps_year == y) & (c.sex == 1)]['weight'].sum() / tot[y] for y in tot.index)
        rows.append({'age': lab, 'range': common.AGE_LABELS[r], 'file (millions)': w[m].sum() / 1e6,
                     'file share of its range (%)': 100 * w[m].sum() / w[dep & (df['age_range'].to_numpy() == r)].sum(),
                     'CPS pool share of its range (%)': 100 * pool_share,
                     'file men, share of the age (%)': 100 * w[m & (s == 1)].sum() / w[m].sum() if w[m].sum() else np.nan,
                     'CPS pool men, share of the age (%)': 100 * pool_men / pool_share if pool_share else np.nan})
    t = pd.DataFrame(rows)
    lines = [f'Dependent filers in the file: {w[dep].sum() / 1e6:.2f} million, {100 * w[dep & (s == 1)].sum() / w[dep].sum():.1f}% '
             f'men. The CPS pool is the year mix of the records\' CPS years.']
    soi = os.path.join(common.RAW['soi_mirror'], 'bysize_dependent_returns.csv')
    if os.path.exists(soi):
        d = pd.read_csv(soi)
        d = d[(d.tax_year == ctx['year']) & (d.row_label == 'All returns') & (d.item == 'number of returns')]
        if len(d):
            lines.append(f'SOI Table 1.7, {ctx["year"]}: {float(d.value.iloc[0]) / 1e6:.2f} million dependent returns '
                         '(the count is a target; SOI publishes no ages for them).')
    return t, lines


def check_child_credit(df, w, ctx):
    y, law = ctx['year'], ctx['ctc']['law_under_age']
    ages = df[DEP_AGES].to_numpy(float)
    flag = df[['dep_ctc1', 'dep_ctc2', 'dep_ctc3']].to_numpy() == 1
    exists = ages >= 0
    filer = df['filer'].to_numpy() == 1
    elig = flag & (ages < law)
    rows = []
    for lab, m in [('dependents (at most 3 per unit)', exists), ('dependents under 17', exists & (ages < 17)),
                   ('dependents under 17 flagged', flag & (ages < 17)), ('dependents aged 17', exists & (ages == 17)),
                   ('dependents aged 17 flagged', flag & (ages == 17)),
                   (f'children eligible under {y} law (flagged, under {law})', elig)]:
        rows.append({'count (millions)': lab, 'all units': (w[:, None] * m).sum() / 1e6,
                     'filers': (w[filer, None] * m[filer]).sum() / 1e6})
    anyc = elig.any(1)
    rows.append({'count (millions)': 'units with an eligible child', 'all units': w[anyc].sum() / 1e6,
                 'filers': w[anyc & filer].sum() / 1e6})
    rows.append({'count (millions)': 'n_ctc, the PUF\'s child credit children (2015 law, at most 3)',
                 'all units': (w * df['n_ctc'].to_numpy()).sum() / 1e6,
                 'filers': (w[filer] * df['n_ctc'].to_numpy()[filer]).sum() / 1e6})
    t = pd.DataFrame(rows)
    pop = table('census_population.csv')
    py = year_used(pop['year'], y)
    kids = pop[(pop.year == py) & (pop.age < law)]['population'].sum() / 1e6
    lines = [f'Census population under {law}, July {py}: {kids:.2f} million (an upper bound: some children are '
             'nobody\'s dependent, or have no number, or fail another test).']
    a = table('soi_actc_children.csv')
    if y in set(a.year):
        r = a[a.year == y].iloc[0]
        lines.append(f'SOI Pub 4801 {y}, Schedule 8812 line 4: {r.actc_children / 1e6:.2f} million children on '
                     f'{r.actc_returns / 1e6:.2f} million returns claiming the additional child tax credit (a lower '
                     'bound: returns with only the nonrefundable credit are not counted). SOI publishes no count of all '
                     'the credit\'s children for the year; the comparison that matters comes with the calculator.')
    lines.append(f'Share of units with dependents under 17 whose count of them exceeds n_ctc (start weights): '
                 f'{100 * ctx["ctc"]["share_17_not_eligible"]:.2f}%; 17-year-olds are flagged with one minus it.')
    return t, lines


def check_deaths(df, w, ctx):
    P = ctx['deaths'].copy()
    P['w'] = w[P.rec]
    y = ctx['year']
    bands = [(0, 25, 'under 25'), (25, 35, '25-34'), (35, 45, '35-44'), (45, 55, '45-54'), (55, 65, '55-64'),
             (65, 75, '65-74'), (75, 80, '75-79'), (80, 200, '80 and over')]
    pop = table('census_population.csv')
    py = year_used(pop['year'], y)
    pop = pop[pop.year == py]
    lt = table('ssa_period_life_table.csv')
    ly = year_used(lt['year'], y)
    lt = lt[lt.year == ly]
    rows = []
    for s, sl in [(1, 'men'), (2, 'women')]:
        qs = lt[lt.sex == s].set_index('age')
        old = qs[qs.index >= 85]
        q85 = float((old.q * old.L).sum() / old.L.sum())
        ps = pop[pop.sex == s].set_index('age')['population']
        ssa_deaths = pd.Series([ps[a] * (qs.loc[a, 'q'] if a < 85 else q85) for a in ps.index], index=ps.index)
        for lo, hi, lab in bands:
            m = (P.sex == s) & (P.age >= lo) & (P.age < hi)
            people, deaths = P.w[m].sum(), (P.w * P.q)[m].sum()
            at_ssa = (P.w * P.q_ssa)[m].sum()
            cp = ps[(ps.index >= max(lo, 18)) & (ps.index < hi)].sum() if hi > 18 else 0.0
            cd = ssa_deaths[(ssa_deaths.index >= max(lo, 18)) & (ssa_deaths.index < hi)].sum() if hi > 18 else 0.0
            rows.append({'sex': sl, 'age': lab, 'file people (millions)': people / 1e6,
                         'file expected deaths (thousands)': deaths / 1e3,
                         'file deaths at SSA rates (thousands)': at_ssa / 1e3,
                         'income and marital layers / SSA rates': deaths / at_ssa,
                         'file deaths per 1,000': 1e3 * deaths / people,
                         'population 18+ (millions)': cp / 1e6, 'SSA deaths, population 18+ (thousands)': cd / 1e3,
                         'SSA deaths per 1,000': 1e3 * cd / cp if cp else np.nan})
    t = pd.DataFrame(rows)
    tot = t[[c for c in t if 'millions' in c or 'thousands' in c]].sum()
    lines = [f'SSA period life table {ly} (2024 Trustees Report); Census population of July {py}, ages 18 and over in '
             'each band (the under-25 row compares the file\'s heads and spouses, dependent filers included, with '
             'everyone aged 18-24). The file counts heads and spouses only: dependents of 18 and over and people in '
             'group quarters are not its people, so the file\'s people and deaths are below the population\'s; the '
             'rates per 1,000 compare like with like.',
             f'All ages: file {tot["file people (millions)"]:.1f} million people, {tot["file expected deaths (thousands)"]:,.0f} '
             f'thousand expected deaths ({tot["file deaths at SSA rates (thousands)"]:,.0f} thousand at SSA\'s rates without '
             f'the income and marital layers); population 18+ {tot["population 18+ (millions)"]:.1f} million, SSA deaths '
             f'{tot["SSA deaths, population 18+ (thousands)"]:,.0f} thousand.']
    return t, lines


def check_accruals(df, w, ctx):
    y = ctx['year']
    rate = ctx['accrual rates']
    rows = []
    for k, col in [('equities', 'w_equities'), ('pass_throughs', 'w_pass_throughs'), ('primary_home', 'w_primary_home'),
                   ('other_home', 'w_other_home'), ('re_fund', 'w_re_fund'), ('dc', 'w_dc'), ('trusts', 'w_trusts')]:
        v, a = (w * df[col].to_numpy(float)).sum(), (w * df[f'accruals.{k}'].to_numpy(float)).sum()
        rows.append({'asset': k, 'file value ($ billion)': _bn(v), 'rate (%)': 100 * a / v if v else np.nan,
                     'file yearly gain ($ billion)': _bn(a)})
    t = pd.DataFrame(rows)
    z = table('z1_revaluation_rates.csv')
    z = z[z.year == y].set_index('asset')
    comp = []
    if len(z):
        def f(keys):
            return t[t.asset.isin(keys)][['file value ($ billion)', 'file yearly gain ($ billion)']].sum()
        for lab, keys, zk in [('stocks (corporate equities)', ['equities'], ['equities']),
                              ('stocks (corporate equities and mutual funds)', ['equities'], ['equities', 'mutual_funds']),
                              ('businesses (noncorporate equity)', ['pass_throughs'], ['pass_throughs']),
                              ('real estate (home, other homes, other real estate)',
                               ['primary_home', 'other_home', 're_fund'], ['real_estate'])]:
            fv = f(keys)
            zz = z.loc[zk]
            comp.append({'asset': lab, 'file value ($ billion)': fv.iloc[0],
                         'Z.1 held, end of year before ($ billion)': zz.held_end_of_year_before_millions.sum() / 1e3,
                         'Z.1 held, end of year ($ billion)': zz.held_end_of_year_millions.sum() / 1e3,
                         'file yearly gain ($ billion)': fv.iloc[1],
                         'Z.1 holding gain in the year ($ billion)': zz.revaluation_millions.sum() / 1e3,
                         'file / Z.1 gain': fv.iloc[1] / (zz.revaluation_millions.sum() / 1e3)})
    lines = [f'Rates: {rate["source"]}. The file\'s yearly gain is its value times the rate, so its gap from Z.1\'s is '
             'the gap in the amount held. Z.1 counts households and nonprofit organizations; the SCF counts '
             'households, and values its own stocks and funds (mutual funds included) at the stocks rate, as production. '
             'Retirement accounts and trusts have no Z.1 line to compare.']
    return t, pd.DataFrame(comp), lines


def check_basis(df, w, ctx):
    rows = []
    for k, v, g in [('equities', 'w_equities', None), ('pass_throughs', 'w_pass_throughs', 'w_kg_pass_throughs'),
                    ('primary_home', 'w_primary_home', 'w_kg_primary_home'), ('other_home', 'w_other_home', None),
                    ('re_fund', 'w_re_fund', None)]:
        b = df[f'basis.{k}'].to_numpy(float)
        val = df[v].to_numpy(float)
        rows.append({'asset': k, 'value ($ billion)': _bn((w * val).sum()),
                     'unrealized gain ($ billion)': _bn((w * (val - b)).sum()), 'basis ($ billion)': _bn((w * b).sum()),
                     'units with a negative basis (millions, should be 0)': w[b < 0].sum() / 1e6})
    return [pd.DataFrame(rows), ctx['basis capped']], [
        'basis = value - the SCF\'s unrealized gain (the gain on other real estate split by value), the gain first held '
        'to no more than the value so a basis is never below 0. The second table counts the records where the drawn gain '
        'exceeded the value and the gain removed (the gain and the value are drawn separately). A basis can still be '
        'below 0 where the drawn value itself is negative (other real estate in some records), which the rule does '
        'not touch.']


def check_holding(df, w, ctx):
    M = ctx['holding']['model']
    grp = ctx['holding']['group']
    kg, h = df['kg_lt'].to_numpy(float), df['kg_lt_years_held'].to_numpy(float)
    b = df['kg_lt_basis'].to_numpy(float)
    rows = []
    gain, loss = kg > 0, kg < 0
    G, L = (w * kg)[gain].sum(), -(w * kg)[loss].sum()
    for j, lab in enumerate(M['groups']):
        mg, ml = gain & (grp == j), loss & (grp == j)
        sales = (w * (b + kg))[mg].sum()
        rows.append({'holding period': lab, 'SOCA share of gain dollars (%)': 100 * M['pi_g'][j],
                     'file share of gain dollars (%)': 100 * (w * kg)[mg].sum() / G,
                     'file gains ($ billion)': _bn((w * kg)[mg].sum()),
                     'file mean years held': (w * h)[mg].sum() / w[mg].sum() if w[mg].sum() else np.nan,
                     'basis / sales at the group\'s length (regression)': M['pred'][j],
                     'file basis / sales': (w * b)[mg].sum() / sales if sales else np.nan,
                     'SOCA share of loss dollars (%)': 100 * M['pi_l'][j],
                     'file share of loss dollars (%)': 100 * -(w * kg)[ml].sum() / L})
    lines = [f'Long-term gains: {w[gain].sum() / 1e6:.2f} million units, ${_bn(G):,.1f} billion, basis '
             f'${_bn((w * b)[gain].sum()):,.1f} billion; losses: {w[loss].sum() / 1e6:.2f} million units, '
             f'${_bn(L):,.1f} billion, basis ${_bn((w * b)[loss].sum()):,.1f} billion. The group is drawn unconditionally '
             'with SOCA\'s shares, so the file\'s shares differ only by chance and by the tilt.']
    c = ctx['holding capped']
    lines.append(f'Each period is held to at most the older adult\'s age less 18 (at least 1 year): {c["records"]:,} of '
                 f'{c["long-term records"]:,} records with a long-term gain or loss changed ({c["units"] / 1e6:.2f} of '
                 f'{c["units with long-term"] / 1e6:.2f} million units at the start\'s weights); {c["under 19 (cap 1 year)"]:,} '
                 f'are under 19; the longest period fell from {c["longest before"]:.0f} to {c["longest after"]:.0f} years.')
    return pd.DataFrame(rows), lines


def check_blindness(df, w, ctx):
    """Blind people in the file by age, filers and all units, against SOI Publication 4801's blind boxes on all returns
    (pub4801_blind_boxes.csv; the nearest earlier year the table has), before reweighting (the rule's own count, at
    the start's weights) and after (weights w)."""
    filer = df['filer'].to_numpy() == 1
    a1, _ = primary_age_sex(df)
    a2 = df['age_spouse'].to_numpy()
    b1, b2 = df['blind1'].to_numpy() == 1, df['blind2'].to_numpy() == 1
    rows = []
    for lo, hi, lab in [(0, 26, 'under 26'), (26, 45, '26-44'), (45, 65, '45-64'), (65, 75, '65-74'), (75, 200, '75 and over'),
                        (0, 200, 'all ages')]:
        m1, m2 = (a1 >= lo) & (a1 < hi), (a2 >= lo) & (a2 < hi) & has_spouse(df)
        rows.append({'age': lab, 'primary filers, filers (millions)': w[m1 & filer].sum() / 1e6,
                     'blind primary filers, filers (thousands)': w[m1 & filer & b1].sum() / 1e3,
                     'blind share, primary filers (%)': 100 * w[m1 & b1].sum() / max(w[m1].sum(), 1),
                     'spouses, filers (millions)': w[m2 & filer].sum() / 1e6,
                     'blind spouses, filers (thousands)': w[m2 & filer & b2].sum() / 1e3,
                     'blind share, spouses (%)': 100 * w[m2 & b2].sum() / max(w[m2].sum(), 1),
                     'blind, non-filers (thousands)': (w[m1 & ~filer & b1].sum() + w[m2 & ~filer & b2].sum()) / 1e3})
    t = pd.DataFrame(rows)
    p = table('pub4801_blind_boxes.csv')
    py = year_used(p['year'], ctx['year'])
    r = p[p.year == py].iloc[0]
    info = ctx['blind']
    lines = [f'SOI Publication 4801, {py}, all returns: {r.primary_blind / 1e3:,.1f} thousand "you are blind" boxes and '
             f'{r.spouse_blind / 1e3:,.1f} thousand "spouse is blind" boxes. The file\'s blind filers before reweighting '
             f'(the start\'s weights, which the rule\'s factors are set on): {info["primary"]["drawn, filers"] / 1e3:,.1f} '
             f'thousand primary filers and {info["spouse"]["drawn, filers"] / 1e3:,.1f} thousand spouses; after: '
             f'{w[filer & b1].sum() / 1e3:,.1f} thousand and {w[filer & b2].sum() / 1e3:,.1f} thousand.',
             f'The rule: the 2015 PUF\'s blind rate by age range (puf_blind_rates.csv) times a factor of '
             f'{info["primary"]["factor"]:.3f} for primary filers and {info["spouse"]["factor"]:.3f} for spouses on '
             'married-joint returns; non-filers drawn by the same rule and not counted against the boxes.']
    return t, lines


def check_parents(df, w, ctx):
    child = ctx['child_rank']
    par = df['parent_rank'].to_numpy()
    rows_m = mobility_rows()
    mean_given = (rows_m * np.arange(1, 101)).sum(1) / rows_m.sum(1)
    rows = []
    for lo, hi in [(10, 10), (11, 20), (21, 30), (31, 40), (41, 50), (51, 60), (61, 70), (71, 80), (81, 90), (91, 100)]:
        m = (child >= lo) & (child <= hi)
        rows.append({'own rank': f'{lo}' if lo == hi else f'{lo}-{hi}', 'units (millions)': w[m].sum() / 1e6,
                     'file mean parents\' rank': (w * par)[m].sum() / w[m].sum(),
                     'matrix mean parents\' rank': (w[m] * mean_given[child[m] - 10]).sum() / w[m].sum()})
    return pd.DataFrame(rows), ['Own rank on the start\'s weights (rank 10 is the bottom 10%); the matrix column is '
                                'the mean the draw aims at, so this checks the draw, not the method.']


def checks(df, year, w, ctx=None):
    """Every item's published check, on the weights w (after the tilt). ctx: impute's context; if None (an older
    file, or a check on a finished file), the pieces the checks need are rebuilt by imputing again on the file's
    weight_start, and the result must equal the file's columns. Returns [(title, [tables], [lines])]."""
    w = np.asarray(w, float)
    if ctx is None:
        again, ctx = impute(df.drop(columns=[c for c in written_columns() if c in df]), year,
                            df['weight_start'].to_numpy(float))
        for c in written_columns():
            if c in df:
                assert np.allclose(again[c].to_numpy(float), df[c].to_numpy(float)), f'{c}: imputing again differs'
        df = again
    ctx['year'] = year
    out = []
    t, ln = check_dependent_filers(df, w, ctx)
    out.append(('Dependent filers\' exact age and sex', [ctx['dependent filer pools'], t], ln))
    t, ln = check_child_credit(df, w, ctx)
    out.append(('Child tax credit eligibility', [t], ln))
    t, ln = check_deaths(df, w, ctx)
    out.append(('Death probabilities: expected deaths against SSA', [t], ln))
    t, comp, ln = check_accruals(df, w, ctx)
    out.append(('Yearly unrealized gains against Z.1 holding gains', [t, comp], ln))
    t, ln = check_basis(df, w, ctx)
    out.append(('Cost basis of each asset', t, ln))
    t, ln = check_holding(df, w, ctx)
    out.append(('Holding period and basis of long-term gains against SOCA', [t], ln))
    t, ln = check_blindness(df, w, ctx)
    out.append(('Blindness against Publication 4801\'s blind boxes', [t], ln))
    if ctx.get('qbi'):
        q = ctx['qbi']
        out.append(('QBI service status and wage bill (first version, no QBI targets)', [q['table']],
                    [f'Dials a = {q["a"]:g} (wage bill) and b = {q["b"]:g} (specified service chance), set later; the chance for '
                     f'"no industry code" is {q["c_other"]:.4f}; average pay of private employees from the CPS of {q["pay_year"]}. '
                     'Start weights (before the tilt). The deduction itself needs the calculator (roadmap step 4).']))
    t, ln = check_parents(df, w, ctx)
    out.append(('Parents\' income rank', [t], ln))
    ph = [{'item': k, 'columns': ', '.join(it['writes']), 'value': PLACEHOLDER_VALUES[k]}
          for k, it in ITEMS.items() if it.get('placeholder')]
    if ctx.get('stock share') == 'age rule':
        ph.append({'item': 'unrealized_gains (retirement accounts)', 'columns': 'accruals.dc',
                   'value': 'stock share by production\'s age rule (a file from before the SCF item)'})
    out.append(('Placeholders', [pd.DataFrame(ph)], ['Each is replaced by its own item (NEXT_STEPS.md item 1).']))
    return out


def write_checks(sections, folder, year):
    """non_micro_checks.md (every table) and one CSV per table, in folder. Returns the page's lines."""
    text = [f'# The non-microdata imputations: checks, {year}', '',
            f'Weights after the tilt. Counts in millions, dollars in nominal {year} dollars. Reported only (Q9).', '']
    for i, (title, tables, lines) in enumerate(sections, 1):
        text += [f'## {i}. {title}', '']
        for j, t in enumerate(tables):
            text += [t.to_markdown(index=False, floatfmt=',.3f'), '']
            t.to_csv(os.path.join(folder, f'non_micro_check_{i}{"abcdefg"[j]}.csv'), index=False)
        text += [f'- {x}' for x in lines] + ['']
    open(os.path.join(folder, 'non_micro_checks.md'), 'w').write('\n'.join(text))
    return text


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('--run', default=common.TILT, help='the tilt folder holding file_<year>.parquet')
    ap.add_argument('--out', required=True, help='where to write the checks (and the file, with --write-file)')
    ap.add_argument('--year', type=int, default=common.TARGET_YEAR)
    ap.add_argument('--write-file', action='store_true')
    a = ap.parse_args(argv)
    os.makedirs(a.out, exist_ok=True)
    df = pd.read_parquet(os.path.join(a.run, f'file_{a.year}.parquet'))
    df = df.drop(columns=[c for c in written_columns() if c in df])
    df, ctx = impute(df, a.year, df['weight_start'].to_numpy(float))
    common.say('\n'.join(write_checks(checks(df, a.year, df['weight'].to_numpy(float), ctx), a.out, a.year)))
    if a.write_file:
        df.to_parquet(os.path.join(a.out, f'file_{a.year}.parquet'), index=False)


if __name__ == '__main__':
    main()
