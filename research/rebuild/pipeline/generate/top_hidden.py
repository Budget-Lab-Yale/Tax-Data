"""Part A: the 2015 PUF's four aggregate records rebuilt as synthetic returns made from generated records.

The four records (RECIDs 999996 to 999999, 1,232.78 returns) hold only group means for negative AGI, $0 to $10 million,
$10 to $100 million and over $100 million of AGI, and are left out of training. build() draws candidate filers, selects
the top ones by SOI's rule, weights them to the booklet's counts, scales their amounts to each record's group totals,
redraws their balance sheets and grows them to the target year, writing to gen/<run>/hidden. The method follows PWBM
(2020), section 2.1; FEATURES.md, "The top of the file", has the details.
"""
import os

import numpy as np
import pandas as pd

from prep import build_table as BT
from shared import common
from shared import dollars as D

S = common.SETTINGS['top_hidden']
Y = common.TARGET_YEAR
PY = common.PUF_YEAR
assert PY == 2015, 'the aggregate records rebuilt here are the 2015 PUF\'s'
BOOKLET = os.path.join(common.RESOURCES, 'top_hidden', 'soi_2015_aggregate_booklet.csv')

# ---- the four aggregate records ---------------------------------------------------------------------------------
RECIDS = [999996, 999997, 999998, 999999]
GROUPS = ['negative AGI', '$0 to $10 million', '$10 million to $100 million', 'over $100 million']
N_G = len(GROUPS)


def agi_group(agi):
    """The aggregate record group (0-3) of AGI in 2015 dollars: below 0; $0 to $10,000,000; $10,000,001 to
    $100,000,000; above $100,000,000. The lines are the booklet's disclosure text; its AGIR1 code labels, with a
    $1 million line, are a misprint."""
    a = np.asarray(agi, float)
    return np.where(a < 0, 0, np.where(a <= 10e6, 1, np.where(a <= 100e6, 2, 3)))


# ---- the parts the rebuild draws, from PUF fields (build_table.puf_rows) ----------------------------------------
SIGNED_ITEMS = {v: s for v, (s, _, _) in BT.ITEMS.items() if s != 0}   # AGI items other than wages through gains
PRIMITIVES = (['wages', 'txbl_int', 'div_pref', 'div_ord'] + BT.KG_PIECES + list(BT.PARTS)
              + ['other_gains', 'txbl_ira_dist', 'txbl_pens_dist', 'nontxbl_pens_dist', 'ui', 'alimony',
                 'state_ref', 'gross_ss', 'exempt_int', 'first_mort_int',
                 'salt_prop'] + list(BT.ADJ_PARTS) + list(BT.NEW_ITEMS) + ['other_agi_remainder'])
# Parts that are residuals in the PUF's translation (no PUF field or booklet line of their own) and may take both signs
RESIDUALS = ['div_ord', 'sche_other', 'part_se', 'other_item_exp', 'other_agi_remainder']


def puf_items(raw, itemizer=None):
    """The rebuild's parts from raw PUF fields, by the rules of build_table.puf_rows. itemizer says who gets
    other_item_exp (default FDED == 1; the aggregate records' FDED is zeroed, so the caller passes True). The medical
    floor is 10% of AGI."""
    f = {c: raw[c].to_numpy(float) for c in raw.columns}
    n = len(raw)
    out = {'wages': f['E00200'], 'txbl_int': f['E00300'], 'div_pref': f['E00650'], 'div_ord': f['E00600'] - f['E00650'],
           'kg_st': f['P22250'], 'kg_lt': f['P23250'], 'kg_dist': f['E01100']}   # before the loss limit
    # line 13's shortfall below the allowed net goes on the short-term piece
    mars = f['MARS'] if 'MARS' in f else None
    out['kg_st'] = f['P22250'] + np.minimum(f['E01000'] - common.allowed_capital_gain(f['P22250'] + f['P23250'], mars), 0)
    for p, (fld, _) in BT.PARTS.items():
        if fld:
            out[p] = f[fld]
    out['sche_other'] = f['E02000'] - sum(BT.PARTS[p][1] * out[p] for p in BT.SCHE_PARTS)
    out.update({'other_gains': f['E01200'], 'txbl_ira_dist': f['E01400'], 'txbl_pens_dist': f['E01700'],
                'nontxbl_pens_dist': np.maximum(f['E01500'] - f['E01700'], 0.0), 'ui': f['E02300'], 'alimony': f['E00800'],
                'state_ref': f['E00700'], 'gross_ss': f['E02400'],
                'exempt_int': f['E00400'], 'first_mort_int': f['E19200'], 'salt_prop': f['E18500']})
    out.update({p: f[fld] for p, fld in BT.ADJ_PARTS.items()})
    out.update({v: f[fld] for v, fld in BT.NEW_ITEMS.items() if fld})
    k1_part = (f['E25940'] + f['E25980'] - f['E25920'] - f['E25960']) != 0
    out['part_se'] = BT.ES.part_se_from_schedule_se(f['E09400'], f['E30400'], f['E00900'] + f['E02100'], k1_part)
    itemizer = (f['FDED'] == 1) if itemizer is None else np.broadcast_to(np.asarray(itemizer, bool), (n,))
    medical = np.maximum(0, f['E17500'] - np.maximum(0, f['E00100'] * 0.1))
    out['other_item_exp'] = np.where(itemizer, f['P04470'] + f['E21040'] - medical - f['E18400'] - f['E18500']
                                     - f['E19200'] - f['E19700'] - f['E20500'] - f['E20800'], 0.0)
    out['other_item_exp'] = np.maximum(out['other_item_exp'], 0.0)
    business = f['E00900'] + f['E02000'] + f['E02100']
    items = {'business_income': business, 'other_gains': out['other_gains'], 'txbl_ira_dist': out['txbl_ira_dist'],
             'txbl_pens_dist': out['txbl_pens_dist'],
             'ui': out['ui'], 'alimony': out['alimony'], 'state_ref': out['state_ref'],
             'adjustments': sum(f[c] for c in BT.ADJ_FIELDS)}
    assert set(items) == set(SIGNED_ITEMS), (set(items) ^ set(SIGNED_ITEMS))
    # line 13 in AGI: the allowed Schedule D net plus distributions
    allowed = common.allowed_capital_gain(out['kg_st'] + f['P23250'], mars)
    # taxable Social Security (E02500) comes off too: it is computed by the worksheet, not a part
    out['other_agi_remainder'] = f['E00100'] - f['E00200'] - f['E00300'] - f['E00600'] - allowed - f['E01100'] \
        - sum(SIGNED_ITEMS[v] * items[v] for v in items) - f['E02500']
    df = pd.DataFrame({v: np.asarray(out[v], float) for v in PRIMITIVES}, index=raw.index)
    return df


def puf_totals(df, ss_taxable=None):
    """The totals and AGI from the parts, as dollars.draw_dollars computes them. ss_taxable, if given, replaces the
    worksheet's taxable Social Security (held fixed while the residual is scaled)."""
    out = df.copy()
    out['capital_gains'] = common.allowed_capital_gain(out['kg_st'] + out['kg_lt'],
                                                       out['filing_status'] if 'filing_status' in out else None)
    out['capital_gains_line13'] = out['capital_gains'] + out['kg_dist']
    for tot, parts in D.META['computed_totals'].items():
        out[tot] = D.signed_sum(out, parts)
    out['business_income_drawn'] = out['business_income']
    out['other_agi'] = out['other_agi_remainder'] + out['kg_dist'] \
        + D.signed_sum(out, D.FOLD)
    if ss_taxable is None:
        D.retirement_items(out)
    else:
        out[common.GROSS_PENSIONS] = out[common.PENSION_PARTS].to_numpy(float).sum(1)
        out['ss_taxable'] = np.asarray(ss_taxable, float)
        out['other_agi'] = out['other_agi'] + out['ss_taxable']
    out['agi'] = D.agi(out)
    return out


def read_raw():
    """The raw 2015 PUF: the four aggregate records (by RECID order) and the ordinary records."""
    raw = pd.read_csv(common.RAW['puf'])
    agg = raw[raw.RECID >= 999996].set_index('RECID').loc[RECIDS].reset_index()
    assert len(agg) == 4
    return agg, raw[raw.RECID < 999996].reset_index(drop=True)


# Fields blank on all four aggregate records and absent from the booklet: suppressed, not known zeros. Their parts
# keep the donors' own 2015 amounts, and the residual moves so each group's AGI still equals the raw record's.
SUPPRESSED_FIELDS = {'E00800': 'alimony', 'E03500': 'alimony_exp'}
UNUSED_BLANK = {'E87521'}                                  # blank too, and not read here


def raw_targets(agg):
    """Per group: population returns (S006 / 100) and the net amount of every part (mean x returns), 2015 dollars. A
    suppressed field is read as 0 for the residual and its part's target is missing."""
    n = agg['S006'].to_numpy(float) / 100
    blank = {c for c in agg.columns if agg[c].isna().any()}
    assert blank <= set(SUPPRESSED_FIELDS) | UNUSED_BLANK, f'blank fields on the aggregate records: {sorted(blank)}'
    assert all(agg[c].isna().all() for c in blank), 'a field blank on some aggregate records only'
    common.say(f'aggregate records: fields blank on all four (suppressed): {sorted(blank)}')
    means = puf_items(agg.fillna({c: 0.0 for c in blank}), itemizer=True)
    for fld, part in SUPPRESSED_FIELDS.items():
        if fld in blank:
            means[part] = np.nan
    nets = means.mul(n, axis=0)
    # the raw record's taxable Social Security, which its AGI includes
    nets['ss_taxable'] = n * agg['E02500'].fillna(0.0).to_numpy(float)
    # each raw record's allowed Schedule D net (the loss limit on its mean), as its residual was made
    f = agg.fillna({c: 0.0 for c in blank})
    nets['sched_d_allowed'] = n * common.allowed_capital_gain(means['kg_st'].to_numpy(float) + means['kg_lt'].to_numpy(float),
                                                              f['MARS'].to_numpy() if 'MARS' in f else None)
    nets.index = GROUPS
    return n, means, nets


def check_translation(ordinary):
    """Checks that puf_items equals the training table's PUF rows on every part of the ordinary records
    (other_item_exp only where there are no medical expenses, whose floor there depends on age). Returns a table of
    the largest differences."""
    t = pd.read_parquet(common.TABLE, columns=['source', 'record_id'] + [('other_agi' if v == 'other_agi_remainder' else v)
                                                                          for v in PRIMITIVES])
    t = t[t.source == 'puf'].set_index('record_id')
    mine = puf_items(ordinary).set_index(ordinary['RECID'].astype(float))
    t = t.loc[mine.index]
    rows = []
    no_med = ordinary['E17500'].to_numpy() == 0
    for v in PRIMITIVES:
        a = mine[v].to_numpy(float)
        b = t['other_agi' if v == 'other_agi_remainder' else v].to_numpy(float)
        m = no_med if v == 'other_item_exp' else np.ones(len(a), bool)
        d = np.abs(a - b)[m]
        rows.append({'part': v, 'records compared': int(m.sum()), 'records differing': int((d > 1e-6).sum()),
                     'largest difference ($)': float(d.max())})
    out = pd.DataFrame(rows)
    assert (out['records differing'] == 0).all(), 'puf_items differs from build_table.puf_rows:\n' + out.to_string()
    return out


# ---- the booklet ---------------------------------------------------------------------------------------------
def booklet():
    """SOI's booklet table for the aggregate records, 2015 (printed pp. 32-36), in dollars and returns."""
    b = pd.read_csv(BOOKLET).set_index('field')
    for c in [c for c in b if c.endswith('_thousands')]:
        b[c.replace('_thousands', '')] = b[c] * 1000.0
    return b


# Booklet fields the rebuild carries, each as a column with the field's sign (a loss stored positive is the booklet's
# positive loss). Their counts are matched.
COUNT_FIELDS = {**{fld: p for p, (fld, _) in BT.PARTS.items() if fld},
                **{fld: p for p, fld in BT.ADJ_PARTS.items()},
                **{fld: v for v, fld in BT.NEW_ITEMS.items() if fld},
                'E00200': 'wages', 'E00300': 'txbl_int', 'E00400': 'exempt_int', 'E00600': 'dividends',
                'E00650': 'div_pref', 'E00700': 'state_ref', 'E00800': 'alimony', 'E01100': 'kg_dist',
                'E01200': 'other_gains', 'E01400': 'txbl_ira_dist', 'E01700': 'txbl_pens_dist',
                'E02300': 'ui', 'E02400': 'gross_ss', 'E18500': 'salt_prop',
                'E19200': 'first_mort_int', 'P22250': 'kg_st', 'P23250': 'kg_lt'}
# Booklet fields that are totals computed from the parts: checks only, since their signs can move with scaling
CHECK_FIELDS = {'E00100': 'agi', 'E01000': 'capital_gains', 'E02000': 'sch_e_income', 'E26270': 'pship_scorp_net',
                'E00900': 'sole_prop', 'E02100': 'farm', 'E02500': 'ss_taxable', 'E01500': 'gross_pens_dist'}
# Parts with both signs whose booklet line splits the combined amount into positive and negative dollars
SPLIT_PARTS = {'sole_prop': 'E00900', 'farm': 'E02100', 'other_gains': 'E01200', 'kg_st': 'P22250', 'kg_lt': 'P23250',
               'amt_nols': 'P60100'}

# ---- SOI's selection rule --------------------------------------------------------------------------------------
# SOI's rule (Bryant, Czajka, Ivsin and Nunns 2014), approximated: SOI does not publish it in full.
# (column, side, rule): side +1 takes the highest positive amounts, -1 the lowest negative ones.
SELECT = ([('agi', 1, 'top_agi')]
          + [(v, 1, 'income_items') for v in ['wages', 'txbl_int', 'dividends', 'state_ref', 'alimony', 'sole_prop',
                                               'capital_gains', 'other_gains', 'txbl_ira_dist', 'txbl_pens_dist',
                                               'sch_e_income', 'farm', 'ui']]
          + [(v, -1, 'loss_items') for v in ['sole_prop', 'other_gains', 'sch_e_income', 'farm']]
          + [(v, 1, 'other_fields') for v in ['exempt_int', 'div_pref', 'kg_st', 'kg_lt', 'kg_1250',
                                               'kg_collect', 'gross_pens_dist']
             + [p for p, (fld, _) in BT.PARTS.items() if fld and p not in ('sole_prop', 'farm')]
             + ['dpad', 'early_penalty', 'alimony_exp', 'first_mort_int', 'salt_prop', 'salt_inc_sales',
                'char_cash', 'char_noncash', 'med_exp', 'casualty_exp', 'job_exp', 'tax_prep_exp', 'other_misc_exp',
                'amt_nols', 'amt_ftc', 'ftc', 'gbc', 'prior_yr_cred', 'other_nonref', 'recapture_tax', 'ira_penalty',
                'res_energy_cred']]
          + [(v, -1, 'other_fields') for v in ['kg_st', 'kg_lt', 'amt_nols']])
# capped, income-limited and derived fields are left out of the rule, as SOI does
EXCLUDED_FROM_SELECTION = {
    'capped or income-limited': ['gross_ss', 'ss_taxable (computed by the worksheet)', 'trad_contr_ira', 'sl_int_ded', 'ed_exp', 'tuition_ded',
                                 'hsa_contr', 'keogh_contr', 'aoc_exp', 'llc_exp', 'savers_cred', 'old_cred', 'net_ptc',
                                 'excess_ptc', 'capital loss (the $3,000 cap)'],
    'derived': ['div_ord', 'kg_dist (zero on the aggregate records)', 'sche_other', 'part_se',
                'nontxbl_pens_dist (selected through gross pensions, E01500)',
                'other_item_exp', 'other_agi_remainder', 'se_tax_ded', 'se_health', 'business_income',
                'sch_cf_income', 'sche_rent', 'sche_rest', 'adjustments']}


def top_by_weight(x, w, n, side):
    """Indices of the n highest (side 1) or lowest (side -1) returns counted in weight: records in order until their
    weight reaches n, the record that crosses n included."""
    idx = np.flatnonzero(x > 0 if side > 0 else x < 0)
    if not len(idx):
        return idx
    o = idx[np.argsort(-side * x[idx], kind='stable')]
    k = int(np.searchsorted(np.cumsum(w[o]), n, side='left')) + 1
    return o[:k]


def select(pool, w):
    """SOI's rule on the pool: {record position: [rules that chose it]}."""
    sel = S['selection']
    reasons = {}
    for col, side, rule in SELECT:
        for i in top_by_weight(pool[col].to_numpy(float), w, sel[rule], side):
            reasons.setdefault(int(i), []).append(f"{rule}:{col}:{'high' if side > 0 else 'low'}")
    return reasons


# ---- scaling one part ------------------------------------------------------------------------------------------
def _one_group(P, N, T):
    """The positive and negative factors (a, b) of one group with both signs, a P + b N = T (P > 0 > N), closest in
    logs to one common factor c = |T| / (P - N)."""
    from scipy.optimize import minimize_scalar
    c = abs(T) / (P - N)
    lc = np.log(c)
    lo = np.log(T / N) + 1e-9 if T < 0 else lc - 40

    def f(u):
        a = (T - np.exp(u) * N) / P
        return (np.log(a) - lc) ** 2 + (u - lc) ** 2
    r = minimize_scalar(f, bounds=(lo, lc + 40), method='bounded', options={'xatol': 1e-12})
    b = float(np.exp(r.x))
    return (T - b * N) / P, b


def solve_field(P, N, T, B=None):
    """Scale factors for one part across the four groups: a[g] on positive amounts, b[g] on negative amounts, so that
    a P + b N = T in every group and, given B (the booklet's combined positive dollars), sum a P = B. P[g] and N[g] are
    the group's weighted positive and negative dollars, T[g] its net target. Among such factors, the closest in logs to
    one common factor per group. Returns a, b and notes on any target not met."""
    P, N, T = (np.asarray(x, float) for x in (P, N, T))
    assert np.isfinite(P).all() and np.isfinite(N).all() and np.isfinite(T).all(), (P, N, T)
    P, N = np.where(P < 1.0, 0.0, P), np.where(N > -1.0, 0.0, N)   # under a dollar of weighted amount: none
    a, b = np.zeros(N_G), np.zeros(N_G)
    notes, free = [], []
    for g in range(N_G):
        if T[g] == 0:
            continue                                          # nothing in the group: zero
        if P[g] > 0 and N[g] < 0:
            a[g], b[g] = _one_group(P[g], N[g], T[g])
            free.append(g)
        elif P[g] > 0 and T[g] > 0:
            a[g] = T[g] / P[g]
        elif N[g] < 0 and T[g] < 0:
            b[g] = T[g] / N[g]
        else:
            notes.append(f'group {g}: net {T[g]:,.0f} needs a sign its records lack (positive {P[g]:,.0f}, '
                         f'negative {N[g]:,.0f}); set to zero')
    if B is None:
        return a, b, notes
    fixed = sum(a[g] * P[g] for g in range(N_G) if g not in free)
    if not free:
        if abs(fixed - B) > 1e-6 * max(abs(B), 1):
            notes.append(f'booklet positive total {B:,.0f} not met: the groups\' nets fix it at {fixed:,.0f}')
        return a, b, notes
    # positives of a free group can be anything above max(0, T): the combined total must be at least their sum
    floor = fixed + sum(max(0.0, T[g]) for g in free)
    B_use = B
    if B <= floor:
        B_use = floor * (1 + 1e-6) + 1.0
        notes.append(f'booklet positive total {B:,.0f} below what the groups\' nets allow ({floor:,.0f}); '
                     f'set just above it')
    from scipy.optimize import minimize
    c = {g: abs(T[g]) / (P[g] - N[g]) for g in free}
    x0 = np.concatenate([[np.log(a[g]), np.log(b[g])] for g in free])

    def obj(x):
        return sum((x[2 * k] - np.log(c[g])) ** 2 + (x[2 * k + 1] - np.log(c[g])) ** 2 for k, g in enumerate(free))
    cons = [{'type': 'eq', 'fun': (lambda x, k=k, g=g: (np.exp(x[2 * k]) * P[g] + np.exp(x[2 * k + 1]) * N[g] - T[g])
                                   / max(abs(T[g]), 1.0))} for k, g in enumerate(free)]
    cons.append({'type': 'eq', 'fun': lambda x: (fixed + sum(np.exp(x[2 * k]) * P[g] for k, g in enumerate(free))
                                                 - B_use) / max(abs(B_use), 1.0)})
    r = minimize(obj, x0, constraints=cons, method='SLSQP', options={'maxiter': 500, 'ftol': 1e-14})
    if not r.success:
        notes.append(f'booklet split: the solver did not converge ({r.message}); the groups\' nets kept, the split not')
        return a, b, notes
    for k, g in enumerate(free):
        a[g], b[g] = np.exp(r.x[2 * k]), np.exp(r.x[2 * k + 1])
    return a, b, notes


def apply_factors(x, grp, a, b):
    return np.where(x > 0, x * a[grp], np.where(x < 0, x * b[grp], 0.0))


SS_ROUNDS = 50   # rounds of the residual against the worksheet's taxable Social Security


def scale_bounded(x, w, grp, T, lo, hi, rounds=400):
    """One part scaled group by group with each record held inside [lo, hi]: records outside are set to their bound
    and the rest scaled to the remaining net, until no record leaves its bound."""
    y = np.zeros(len(x))
    notes = []
    for g in range(N_G):
        m = grp == g
        fixed = np.zeros(len(x), bool)
        val = np.zeros(len(x))
        for _ in range(rounds):
            free = m & ~fixed
            Tf = T[g] - (w[m & fixed] * val[m & fixed]).sum()
            xf = np.where(free, x, 0.0)
            P = (w * np.where(xf > 0, xf, 0)).sum()
            N = (w * np.where(xf < 0, xf, 0)).sum()
            Tg = np.zeros(N_G)
            Tg[g] = Tf
            Pg, Ng = np.zeros(N_G), np.zeros(N_G)
            Pg[g], Ng[g] = P, N
            a, b, nt = solve_field(Pg, Ng, Tg)
            yg = np.where(free, apply_factors(x, np.full(len(x), g), a, b), val)
            below, above = free & (yg < lo - 1e-6), free & (yg > hi + 1e-6)
            if not (below | above).any():
                y[m] = yg[m]
                notes += nt
                break
            val[below], val[above] = lo[below], hi[above]
            fixed |= below | above
        else:
            y[m] = yg[m]
            notes.append(f'group {g}: the bound did not settle in {rounds} rounds')
        if fixed[m].any():
            notes.append(f'group {g}: {int(fixed[m].sum())} records held at their bound')
    return y, notes


# Each group's AGI range in 2015 dollars, a dollar or two inside the lines so that rounding cannot carry a record
# held at a line across it
AGI_RANGE = [(-np.inf, -2.0), (1.0, 10e6 - 1), (10e6 + 2, 100e6 - 1), (100e6 + 2, np.inf)]


def agi_without_ss_at(line, d):
    """Per record, the AGI without taxable Social Security, a, at which AGI with it equals line. AGI = a + ss(a), and
    ss rises by at most 85 cents a dollar, so AGI increases in a and bisection between line - 0.85 x gross benefits - 1
    and line + 1 finds it. An infinite line stays infinite."""
    line = np.asarray(line, float)
    gs = d['gross_ss'].to_numpy(float)
    add = d['exempt_int'].to_numpy(float) + d['sl_int_ded'].to_numpy(float)
    fs = d['filing_status'].to_numpy()
    fin = np.isfinite(line)
    lo = np.where(fin, line - 0.85 * gs - 1.0, 0.0)
    hi = np.where(fin, line + 1.0, 0.0)
    for _ in range(80):
        mid = 0.5 * (lo + hi)
        below = mid + common.taxable_social_security(gs, mid + add, fs) < np.where(fin, line, 0.0)
        lo, hi = np.where(below, mid, lo), np.where(below, hi, mid)
    return np.where(fin, 0.5 * (lo + hi), line)


def scale_all(base, w, grp, nets, book, hold=False):
    """Every part of base (2015 dollars) scaled to each group's net, then the totals. The residual other_agi_remainder
    (the PUF's AGI less every identified item) is scaled last; with hold, each record's residual is held so its AGI
    stays inside its group. Returns the scaled frame with totals and a table of factors per part."""
    out = base.copy()
    rows = []
    nets = nets.copy()
    for part in [p for p in SUPPRESSED_FIELDS.values() if nets[p].isna().all()]:
        own = np.array([(w * base[part].to_numpy(float))[grp == g].sum() for g in range(N_G)])
        sign = D.FOLD[part] if part in D.FOLD else -1.0      # alimony paid is an adjustment, subtracted from AGI
        nets['other_agi_remainder'] = nets['other_agi_remainder'] - sign * own
        rows.append({'part': part, 'notes': 'suppressed on the aggregate records: the donors\' own amounts, unscaled '
                     f'(${own.sum() / 1e6:,.1f} million); the residual moved to keep AGI'})
    suppressed = {r['part'] for r in rows}
    last = ['other_agi_remainder']                       # the residual: scaled last, after every other part
    for v in [p for p in PRIMITIVES if p not in suppressed and p not in last]:
        x = base[v].to_numpy(float)
        P = np.array([(w * np.where(x > 0, x, 0))[grp == g].sum() for g in range(N_G)])
        N = np.array([(w * np.where(x < 0, x, 0))[grp == g].sum() for g in range(N_G)])
        T = nets[v].to_numpy(float)
        fld = SPLIT_PARTS.get(v)
        B = float(book.loc[fld, 'amount_positive']) if fld else None
        a, b, notes = solve_field(P, N, T, B)
        out[v] = apply_factors(x, grp, a, b)
        rows.append({'part': v, **{f'positive factor, {GROUPS[g]}': a[g] for g in range(N_G)},
                     **{f'negative factor, {GROUPS[g]}': b[g] for g in range(N_G)}, 'notes': '; '.join(notes)})
    # each record's Schedule D net is held to the allowed loss; the gap between the group's allowed total and the raw
    # record's moves into the residual
    v = 'other_agi_remainder'
    fs = out['filing_status'] if 'filing_status' in out else None
    got = common.allowed_capital_gain(out['kg_st'].to_numpy(float) + out['kg_lt'].to_numpy(float), fs)
    short = nets['sched_d_allowed'].to_numpy(float) - np.array([(w * got)[grp == g].sum() for g in range(N_G)])
    note = ('held parts short of their nets, moved into it: ' + ', '.join(f'{GROUPS[g]} ${short[g] / 1e6:,.1f}m'
                                                                            for g in range(N_G) if abs(short[g]) > 1))
    # taxable Social Security comes from the worksheet on each record, so the residual moves by the raw record's
    # E02500 less the group's worksheet total. The worksheet reads AGI, which includes the residual, so the two are
    # repeated until the group totals settle within a dollar
    by_group = lambda x: np.array([(w * x)[grp == g].sum() for g in range(N_G)])
    raw_ss = nets['ss_taxable'].to_numpy(float)
    out[v] = base[v].to_numpy(float)
    ss = puf_totals(out)['ss_taxable'].to_numpy(float)
    for k in range(SS_ROUNDS):
        T = nets[v].to_numpy(float) + short + (raw_ss - by_group(ss))
        if hold:
            # the residual's bounds that keep each record's AGI, with taxable Social Security, inside its group
            without = D.agi(puf_totals(out.assign(other_agi_remainder=0.0), ss_taxable=np.zeros(len(out))))
            lo = agi_without_ss_at(np.array([AGI_RANGE[g][0] for g in grp]), out) - without
            hi = agi_without_ss_at(np.array([AGI_RANGE[g][1] for g in grp]), out) - without
            y, notes = scale_bounded(base[v].to_numpy(float), w, grp, T, lo, hi)
            out[v] = y
            row = {'part': v, 'notes': ['held so each record\'s AGI stays in its group', note] + notes}
        else:
            x = base[v].to_numpy(float)
            P = np.array([(w * np.where(x > 0, x, 0))[grp == g].sum() for g in range(N_G)])
            N = np.array([(w * np.where(x < 0, x, 0))[grp == g].sum() for g in range(N_G)])
            a, b, notes = solve_field(P, N, T)
            out[v] = apply_factors(x, grp, a, b)
            row = {'part': v, **{f'positive factor, {GROUPS[g]}': a[g] for g in range(N_G)},
                   **{f'negative factor, {GROUPS[g]}': b[g] for g in range(N_G)}, 'notes': [note] + notes}
        new = puf_totals(out)['ss_taxable'].to_numpy(float)
        moved = np.abs(by_group(new) - by_group(ss)).max()
        ss = new
        if moved < 1.0:
            break
    gap = raw_ss - by_group(ss)
    row['notes'] = '; '.join(row['notes'] + [
        f'taxable Social Security by the worksheet against the raw records\' E02500, moved into it: '
        + ', '.join(f'{GROUPS[g]} ${gap[g] / 1e6:,.2f}m' for g in range(N_G))
        + f' ({k + 1} rounds' + ('' if moved < 1.0 else f', not settled: last move ${moved:,.0f}') + ')'])
    rows.append(row)
    return puf_totals(out), pd.DataFrame(rows)


# ---- the counts -----------------------------------------------------------------------------------------------
def count_targets(book, frame):
    """(label, indicator column on frame, published count, tolerance) for the positive and negative count of each
    field in COUNT_FIELDS. The tolerance is the larger of a number of returns and a share of the count."""
    tol = S['count_tolerance']
    out = []
    for fld, col in COUNT_FIELDS.items():
        if fld not in book.index:
            continue
        x = frame[col].to_numpy(float)
        for side, cnt in [('positive', 'returns_positive'), ('negative', 'returns_negative')]:
            c = float(book.loc[fld, cnt])
            ind = (x > 0) if side == 'positive' else (x < 0)
            if c == 0 and not ind.any():
                continue                                       # nothing to match: zero and zero
            out.append((f'{fld} {col} {side}', ind.astype(float), c, max(float(tol['returns']), float(tol['share']) * c)))
    return out


def match_counts(frame, grp, n_ret, book):
    """Weights moved as little as possible from equal weights within each group so the booklet's counts are met
    within their tolerance, each group's returns exactly. Returns the weights, the starting weights, a table of every
    count before and after, and the solver's report."""
    from reweight import solver as SV
    w0 = np.zeros(len(frame))
    for g in range(N_G):
        m = grp == g
        assert m.any(), f'group {GROUPS[g]}: no record'
        w0[m] = n_ret[g] / m.sum()
    cts = count_targets(book, frame)
    F = np.column_stack([(grp == g).astype(float) for g in range(N_G)] + [c[1] for c in cts])
    T = np.array(list(n_ret) + [c[2] for c in cts])
    se = np.array([0.0] * N_G + [c[3] for c in cts])
    ident = np.array([True] * N_G + [False] * len(cts))
    w1, info = SV.solve(w0, F, T, se, ident, [None] * len(T), {'reweighting': {'tau': 1.0, 'newton_budget': 100}})
    tab = pd.DataFrame({'count': [f'returns, {g}' for g in GROUPS] + [c[0] for c in cts], 'booklet': T,
                        'tolerance': se, 'equal weights': F.T @ w0, 'after matching': F.T @ w1})
    tab['met'] = np.abs(tab['after matching'] - tab['booklet']) <= tab['tolerance'] * 1.001 + 1e-6   # solver rounding
    return w1, w0, tab, info


# ---- the candidates ----------------------------------------------------------------------------------------------
def folder():
    f = f'{common.GEN}/hidden'
    os.makedirs(f, exist_ok=True)
    return f


def draw_candidates(models, factors):
    """Each model's share of the candidate records by AGI group, drawn as generation draws filers but with a seed of
    their own. Each weight is the group's 2015 PUF returns over the records drawn in it, over the number of models."""
    from generate import generate as G
    alloc = np.asarray(S['candidate_records'], int)
    parts = []
    for k, mod in enumerate(models):
        n_k = np.array([G.equal_shares(int(a), len(models))[k] for a in alloc])
        part, _, _ = G.label_part(mod['network'], mod, k, len(models), factors[mod['run']], group_records=n_k,
                                  seed=int(S['candidate_seed']) + mod['seed_shift'])
        part['weight'] = part['weight'] / len(models)
        part['stratifier'] = 'label'
        part['model'] = mod['seed']
        part['record_id'] = k * G.ID_BLOCK + np.arange(len(part))
        parts.append(G.store_picks(part, k, stream=4343))   # the candidates' picks: a stream of their own
        common.say(f"{mod['run']}: {len(part):,} candidates")
    return pd.concat(parts, ignore_index=True)


def puf_maps(year):
    m = pd.read_parquet(f'{common.MAPS}/map_puf_{year}.parquet')
    return {v: g for v, g in m.groupby('variable')}


def dollars_2015(stack):
    """The PUF items of stack in 2015 dollars, from the 2015 map and the stored picks."""
    maps = puf_maps(PY)
    cols = [c for v in D.ORDER if v in maps for c in (f'{v}_band', f'u_{v}') if c in stack] + ['filing_status']
    d = D.draw_dollars(stack[cols].reset_index(drop=True), maps)   # filing status sets the capital loss limit
    missing = [v for v in PRIMITIVES if v not in d]
    assert not missing, f'parts with no 2015 dollars: {missing}'
    out = d[PRIMITIVES].copy()
    out['filing_status'] = stack['filing_status'].to_numpy()
    t = puf_totals(out)
    for c in ['capital_gains', 'capital_gains_line13', 'business_income', 'other_agi', 'dividends', 'adjustments']:
        assert np.allclose(t[c].to_numpy(float), d[c].to_numpy(float), rtol=1e-12, atol=1e-6), \
            f'{c}: puf_totals differs from dollars.draw_dollars'
    return t


# ---- wealth ---------------------------------------------------------------------------------------------------
def reband(frame2015):
    """Band of every PUF amount slot from 2015 dollars, on the 2015 PUF cut points."""
    from pretrain import model as M
    out = {}
    for v in M.PREDICTED:
        if v in M.CATEGORICAL or 'puf' not in M.PRESENT_ON[v] or v in M.REPORTED or v in M.PUF_SHARES:
            continue   # a share keeps the donor's own band
        col = 'other_agi_remainder' if v == 'other_agi' else v
        assert col in frame2015, f'{v}: no 2015 dollars to band'
        x = frame2015[col].to_numpy(float)
        assert not np.isnan(x).any(), f'{v}: missing 2015 dollars'
        out[f'{v}_band'], _ = BT.band_variable(x, np.ones(len(x)), fixed=common.edges()[f'puf/{PY}/{v}'])
    return pd.DataFrame(out)


def redraw_wealth(rows, models, factors, maps):
    """Each rebuilt return's balance sheet drawn again given its final income items (top_billionaires.redraw_records),
    keeping the first try below wealth_cap, else the lowest. rows are the rebuilt returns in the stack's coding, with
    PUF bands from their final 2015 dollars. Returns the new SCF slot values, the kept try's dollars, the try accepted
    (-1 where every try reached the cap), every try's net worth, and the try kept."""
    from generate import generate as G
    from pretrain import model as M
    from generate import top_billionaires as TB
    P = M.PREDICTED
    scf_cols = [M.column(v) for v in G.SCF_SLOTS]
    cap, tries = float(S['wealth_cap']), int(S['wealth_tries'])
    device = next(models[0]['network'].parameters()).device
    coded = np.column_stack([rows[M.column(v)].to_numpy().astype(np.int64) if M.column(v) in rows
                             else np.zeros(len(rows), np.int64) for v in P])
    scf_year_idx = rows['scf_year'].map(M.YEARS.index).to_numpy()
    model_of, waves, rids = rows['model'].to_numpy(), rows['scf_wave'].to_numpy(), rows['record_id'].to_numpy()
    tried, kept, ok, bands, dol = np.zeros((len(rows), tries)), np.zeros(len(rows), int), np.zeros(len(rows), bool), [], []
    for mod in models:
        m = np.flatnonzero(model_of == mod['seed'])
        if not len(m):
            continue
        got, dd, tried[m], kept[m], ok[m] = TB.redraw_records(
            mod['network'], factors[mod['run']], coded[m], np.full(len(m), M.SOURCES.index('puf')),
            np.full(len(m), M.YEARS.index(PY)), scf_year_idx[m], waves[m], rids[m], maps, cap, tries,
            int(S['wealth_seed']), int(S['wealth_salt']), device)
        bands.append(got.set_index(m))
        dol.append(dd.set_index(m))
    new = rows[scf_cols].copy()
    for c in scf_cols:
        new[c] = pd.concat(bands).sort_index()[c].to_numpy()
    dol = pd.concat(dol).sort_index().set_index(rows.index)
    assert not dol.isna().any().any(), 'a record with no balance sheet drawn'
    return new, dol, np.where(ok, kept, -1), tried, kept


# ---- the build -------------------------------------------------------------------------------------------------
def build():
    """Rebuilds the four records in 2015 dollars and then in the target year, in the numbered steps below. Writes the
    folder's files and returns the target-year rows."""
    import torch
    from generate import generate as G
    out = folder()
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    models = G.load_models(device)
    from generate import top_billionaires as TB
    by_seed, note = TB.offset_factors(models)
    common.say(note)
    factors = {m['run']: by_seed[m['seed']] for m in models}
    agg, ordinary = read_raw()
    common.say('translation check against build_table.puf_rows:\n' + check_translation(ordinary).to_string(index=False))
    n_ret, means, nets = raw_targets(agg)
    book = booklet()
    common.say(f'aggregate records: returns {n_ret.tolist()} (total {n_ret.sum():.2f})')

    # 1. candidates (reused if already drawn)
    cpath = f'{out}/candidates.parquet'
    if os.path.exists(cpath):
        cand = pd.read_parquet(cpath)
        common.say(f'candidates: {cpath} ({len(cand):,} records), reused')
    else:
        cand = draw_candidates(models, factors)
        cand.to_parquet(cpath, index=False)
    pool = dollars_2015(cand)
    w = cand['weight'].to_numpy(float)

    # 2. selection
    reasons = select(pool, w)
    chosen = np.array(sorted(reasons))
    common.say(f'selected {len(chosen):,} candidates, {w[chosen].sum():,.1f} returns in population weight')

    # 3. first groups by rank of the candidates' AGI: their largest returns fall short of the aggregated ones, so the
    # positive records are split in the shares of the three positive groups' returns
    a = pool['agi'].to_numpy(float)
    grp_all = np.full(len(pool), -1)
    neg = chosen[a[chosen] < 0]
    pos = chosen[a[chosen] >= 0]
    grp_all[neg] = 0
    o = pos[np.argsort(-a[pos], kind='stable')]
    share = (np.cumsum(w[o]) - w[o]) / w[o].sum()
    up = n_ret[1:][::-1]                                  # over $100M, $10-100M, $0-10M
    cut = np.cumsum(up) / up.sum()
    grp_all[o] = np.where(share < cut[0], 3, np.where(share < cut[1], 2, 1))
    lines = {3: a[o][share < cut[0]].min(), 2: a[o][(share >= cut[0]) & (share < cut[1])].min()}
    common.say(f'first groups by rank: candidates\' AGI lines {lines[3]:,.0f} (over $100M) and {lines[2]:,.0f} '
               f'($10-100M); records per group {np.bincount(grp_all[chosen], minlength=4).tolist()}')

    # a varied subsample per group, keeping some records chosen by every rule
    rng = np.random.default_rng(int(S['subsample_seed']))
    keep = []
    cap, floor_n = int(S['records_per_group']), int(S['min_per_rule'])
    for g in range(N_G):
        ids = chosen[grp_all[chosen] == g]
        if len(ids) > cap:
            key = rng.random(len(ids)) ** (1 / w[ids])        # weighted sampling without replacement
            take = set(ids[np.argsort(-key)[:cap]].tolist())
            rules = {}
            for i in ids:
                for r_ in reasons[int(i)]:
                    rules.setdefault(r_, []).append(int(i))
            for r_, members in rules.items():
                if not take.intersection(members):
                    take.update(rng.permutation(members)[:floor_n].tolist())
            ids = np.array(sorted(take))
        keep.append(ids)
    keep = np.concatenate(keep)

    # targeted donors where a group must hold a sign its records lack
    base_all = pool.copy()
    added = []
    lines_pool = np.where(a < 0, 0, np.where(a >= lines[3], 3, np.where(a >= lines[2], 2, 1)))
    for v in PRIMITIVES:
        x = base_all[v].to_numpy(float)
        for g in range(N_G):
            T = nets[v].iloc[g]
            m = keep[grp_all[keep] == g]
            need = (T > 0 and not (x[m] > 0).any()) or (T < 0 and not (x[m] < 0).any())
            if not need:
                continue
            side = 1 if T > 0 else -1
            pool_g = np.flatnonzero((lines_pool == g) & (side * x > 0))
            pool_g = np.setdiff1d(pool_g, keep)
            if not len(pool_g):
                added.append({'part': v, 'group': GROUPS[g], 'sign': 'positive' if side > 0 else 'negative',
                              'added': 0, 'note': 'no candidate in the pool has it'})
                continue
            top = pool_g[np.argsort(-side * x[pool_g])[:int(S['targeted_per_gap'])]]
            grp_all[top] = g
            for i in top:
                reasons.setdefault(int(i), []).append(f'targeted:{v}:{"positive" if side > 0 else "negative"}')
            keep = np.concatenate([keep, top])
            added.append({'part': v, 'group': GROUPS[g], 'sign': 'positive' if side > 0 else 'negative',
                          'added': len(top), 'note': ''})
    added = pd.DataFrame(added, columns=['part', 'group', 'sign', 'added', 'note'])
    common.say('targeted donors:\n' + (added.to_string(index=False) if len(added) else 'none needed'))
    keep = np.unique(keep)

    # structural zeros: a part a group's raw record has none of, or a sign the booklet has no return with
    base = pool.iloc[keep][PRIMITIVES + ['filing_status']].reset_index(drop=True)
    grp = grp_all[keep].copy()
    zeroed = []
    for v in PRIMITIVES:
        x = base[v].to_numpy(float)
        fld = next((f for f, c in COUNT_FIELDS.items() if c == v), None)
        no_pos = fld in book.index and book.loc[fld, 'returns_positive'] == 0
        no_neg = fld in book.index and book.loc[fld, 'returns_negative'] == 0
        by_sign = ((x > 0) & no_pos) | ((x < 0) & no_neg)
        if by_sign.any():
            zeroed.append({'part': v, 'records set to zero, booklet has no return with that sign': int(by_sign.sum()),
                           'weighted dollars of the donors set to zero (candidate weights)':
                           float((w[keep] * np.abs(x))[by_sign].sum())})
            base.loc[by_sign, v] = 0.0

    # the two pension parts are each at least zero, so gross pensions are never below taxable pensions
    assert (base[common.PENSION_PARTS].to_numpy(float) >= 0).all(), 'a pension part below zero on a donor'
    base_src = base                       # the donors before the per-group zeros, which follow each record's group

    def zero_groups(src, grp):
        """A part a group's raw record has none of is zero on the group's records (redone every round, as records
        move between groups)."""
        out_ = src.copy()
        for v in PRIMITIVES:
            kill = (nets[v].to_numpy(float)[grp] == 0) & (out_[v].to_numpy(float) != 0)
            out_.loc[kill, v] = 0.0
        return out_

    # 4-6. weights matched to the booklet's counts, amounts scaled to the groups' totals; a record whose final AGI is
    # outside its group is moved for move_iterations rounds, then held by the residual, and dropped if still outside
    donor = keep.copy()
    history = []

    def one_round(base_src, grp, hold):
        """Steps 4 to 6 once: the group zeros, the weights, the scaling; each record's group by its final AGI."""
        base = zero_groups(base_src, grp)
        wts, w_start, counts, info = match_counts(puf_totals(base), grp, n_ret, book)
        scaled, factors_tab = scale_all(base, wts, grp, nets, book, hold=hold)
        g_final = agi_group(scaled['agi'].to_numpy(float))
        history.append({'round': len(history) + 1, 'held in group by other income': hold, 'records': len(base),
                        **{f'records, {GROUPS[g]}': int((grp == g).sum()) for g in range(N_G)},
                        'outside their group after scaling': int((g_final != grp).sum()),
                        'counts met': int(counts['met'].sum()), 'counts': len(counts)})
        return base, wts, w_start, counts, info, scaled, factors_tab, g_final
    for it in range(int(S['group_iterations'])):
        hold = it >= int(S['move_iterations'])
        base, wts, w_start, counts, info, scaled, factors_tab, g_final = one_round(base_src, grp, hold)
        mis = g_final != grp
        common.say(f'round {it + 1}: {int(mis.sum())} records outside their group; counts met '
                   f'{int(counts["met"].sum())} of {len(counts)}')
        if not mis.any() or it == int(S['group_iterations']) - 1:
            break
        if it < int(S['move_iterations']):
            grp = np.where(mis, g_final, grp)
        else:
            base_src, grp, donor = base_src[~mis].reset_index(drop=True), grp[~mis], donor[~mis]
    extra = 0
    while mis.any():          # the records still outside their group are dropped, and steps 4-6 done again
        extra += 1
        assert extra <= 30, f'{int(mis.sum())} records still outside their group after 30 rounds of dropping'
        common.say(f'{int(mis.sum())} records still outside their group: dropped, weights and scaling done again '
                   f'(groups {grp[mis].tolist()}, AGI {np.round(scaled["agi"].to_numpy()[mis] / 1e6, 3).tolist()} million; '
                   f'other income: {factors_tab.loc[factors_tab.part == "other_agi_remainder", "notes"].tolist()})')
        base_src, grp, donor = base_src[~mis].reset_index(drop=True), grp[~mis], donor[~mis]
        base, wts, w_start, counts, info, scaled, factors_tab, g_final = one_round(base_src, grp, True)
        mis = g_final != grp
    history = pd.DataFrame(history)

    for v in PRIMITIVES:
        x = base_src[v].to_numpy(float)
        kill = (nets[v].to_numpy(float)[grp] == 0) & (x != 0)
        if kill.any():
            zeroed.append({'part': v, 'records set to zero, the group\'s raw record has none': int(kill.sum()),
                           'groups': ', '.join(GROUPS[g] for g in sorted(set(grp[kill])))})
    # the rebuilt 2015 returns
    h15 = scaled.copy()
    h15.insert(0, 'hidden_group', grp)
    h15.insert(1, 'weight', wts)
    h15.insert(2, 'weight_equal', w_start)
    h15.insert(3, 'donor_record_id', cand['record_id'].to_numpy()[donor])
    h15.insert(4, 'model', cand['model'].to_numpy()[donor])
    h15['rules'] = ['; '.join(reasons[int(i)]) for i in donor]
    for c in ['married', 'n_dep', 'age_range', 'filing_status', 'scf_wave', 'scf_year']:
        h15[c] = cand[c].to_numpy()[donor]
    donor_2015 = pool.iloc[donor].reset_index(drop=True)
    h15.to_parquet(f'{out}/hidden_2015.parquet', index=False)
    donor_2015.assign(hidden_group=grp, weight=wts).to_parquet(f'{out}/donors_2015.parquet', index=False)

    # checks in 2015 terms
    checks_2015(h15, donor_2015, wts, grp, nets, n_ret, book, counts, out, agg)
    history.to_csv(f'{out}/check_rounds.csv', index=False)
    factors_tab.to_csv(f'{out}/check_factors.csv', index=False)
    added.to_csv(f'{out}/check_targeted.csv', index=False)
    pd.DataFrame(zeroed).to_csv(f'{out}/check_zeroed.csv', index=False)
    pd.Series({'selected candidates': len(chosen), 'selected, population weight': w[chosen].sum(),
               'candidates': len(cand), 'kept records': len(h15), 'count solve': info['how'],
               'count solve converged': info['converged'], 'effective sample kept by the count matching (%)': 100 * info['effective_sample_kept'],
               'largest weight factor of the count matching': info['largest_weight_factor_overall'],
               'smallest weight factor of the count matching': info['smallest_weight_factor']}
              ).to_csv(f'{out}/check_selection.csv', header=['value'])

    # 7-8: the target year
    rows = target_year_rows(h15, cand.iloc[donor].reset_index(drop=True), models, factors, out)
    return rows


def checks_2015(h15, donor, w, grp, nets, n_ret, book, counts, out, agg):
    """Checks in 2015 dollars: counts and dollars against the booklet and the four raw records, the accounting rules,
    and each record inside its group."""
    counts.to_csv(f'{out}/check_counts.csv', index=False)
    # dollars by group against the raw records, every part and the totals
    rows = []
    tot = puf_totals(pd.DataFrame({v: nets[v].to_numpy() for v in PRIMITIVES}).fillna(0.0).assign(filing_status=0),
                     ss_taxable=nets['ss_taxable'].to_numpy(float))
    for part in SUPPRESSED_FIELDS.values():          # suppressed: no raw amount (the totals read them as 0)
        if nets[part].isna().all():
            tot[part] = np.nan
    for v in PRIMITIVES + ['capital_gains', 'business_income', 'sch_e_income', 'dividends', 'adjustments', 'other_agi',
                           'agi', 'ss_taxable', common.GROSS_PENSIONS]:
        x = h15[v].to_numpy(float)
        for g in range(N_G):
            m = grp == g
            rows.append({'item': v, 'group': GROUPS[g], 'raw record (mean x returns)': tot[v].iloc[g],
                         'rebuilt': (w * x)[m].sum(), 'donors before scaling': (w * donor[v].to_numpy(float))[m].sum()})
    t = pd.DataFrame(rows)
    t['difference'] = t['rebuilt'] - t['raw record (mean x returns)']
    t.to_csv(f'{out}/check_dollars_by_group.csv', index=False)
    # booklet: positive and negative counts and dollars of every booklet field the rebuild carries, and the totals
    rows = []
    fields = {**COUNT_FIELDS, **CHECK_FIELDS}
    from reweight import target_list as TLS
    for fld, col in fields.items():
        if fld not in book.index:
            continue
        x = TLS.record_value(h15, col) if col == 'pship_scorp_net' else h15[col].to_numpy(float)
        rows.append({'field': fld, 'column': col, 'kind': 'total (check)' if fld in CHECK_FIELDS and col not in
                     ('sole_prop', 'farm') else 'part',
                     'returns positive, booklet': book.loc[fld, 'returns_positive'], 'returns positive, rebuilt': w[x > 0].sum(),
                     'returns negative, booklet': book.loc[fld, 'returns_negative'], 'returns negative, rebuilt': w[x < 0].sum(),
                     'positive $bn, booklet': book.loc[fld, 'amount_positive'] / 1e9,
                     'positive $bn, rebuilt': (w * np.maximum(x, 0)).sum() / 1e9,
                     'negative $bn, booklet': book.loc[fld, 'amount_negative'] / 1e9,
                     'negative $bn, rebuilt': (w * np.minimum(x, 0)).sum() / 1e9})
    pd.DataFrame(rows).to_csv(f'{out}/check_booklet.csv', index=False)
    # accounting rules and groups
    cap = np.where(h15['filing_status'].to_numpy() == 3, -1500, -3000)
    rules = {
        'every record inside its AGI group (2015 dollars)': agi_group(h15['agi']) == grp,
        'qualified dividends at most ordinary dividends': h15['div_pref'] <= h15['dividends'] + 1e-6,
        'dividends = qualified + other': np.isclose(h15['dividends'], h15['div_pref'] + h15['div_ord']),
        'capital gain = short + long, a loss held to the limit': np.isclose(
            h15['capital_gains'], common.allowed_capital_gain(h15['kg_st'] + h15['kg_lt'], h15['filing_status'])),
        'capital gain at least the allowed loss': h15['capital_gains'] >= cap - 1e-6,
        'taxable Social Security (the worksheet) at most 85% of gross': h15['ss_taxable'] <= 0.85 * h15['gross_ss'] + 1e-6,
        'gross pensions = taxable pensions + the non-taxable rest': np.isclose(
            h15['gross_pens_dist'], h15['txbl_pens_dist'] + h15['nontxbl_pens_dist'], rtol=0, atol=1e-6),
        'the non-taxable rest of pensions at least zero': h15['nontxbl_pens_dist'] >= 0,
        'no negative amount in a positive-only part': np.ones(len(h15), bool),
        'AGI = wages + interest + dividends + capital gain + other AGI': np.isclose(
            h15['agi'], h15[D.AGI_PARTS].sum(axis=1)),
    }
    two_signed = set(SPLIT_PARTS) | set(RESIDUALS) | {'kg_dist'}
    for v in PRIMITIVES:
        if v not in two_signed:
            rules['no negative amount in a positive-only part'] &= h15[v].to_numpy() >= 0
    # the long-term gain pieces against the long-term gain (reported, not enforced: no strict rule in the law)
    for v in ['kg_1250', 'kg_collect']:
        rules[f'{v} at most the long-term gain, where positive (reported only)'] = \
            (h15[v] <= np.maximum(h15['kg_lt'], 0) + 1e-6) | (h15[v] == 0)
    rows = [{'rule': k, 'records breaking it': int((~np.asarray(v, bool)).sum()),
             'returns breaking it': float(w[~np.asarray(v, bool)].sum())} for k, v in rules.items()]
    pd.DataFrame(rows).to_csv(f'{out}/check_rules.csv', index=False)
    # charity: gifts against the deduction (E19700), by group
    rows = []
    for g in range(N_G):
        m = grp == g
        r = int(RECIDS[g])
        rows.append({'group': GROUPS[g], 'cash gifts $bn': (w * h15['char_cash'])[m].sum() / 1e9,
                     'other gifts $bn': (w * h15['char_noncash'])[m].sum() / 1e9,
                     'deduction (E19700, raw record) $bn': agg.set_index('RECID').loc[r, 'E19700'] * n_ret[g] / 1e9,
                     'cash gifts (E19800, raw record) $bn': agg.set_index('RECID').loc[r, 'E19800'] * n_ret[g] / 1e9,
                     'other gifts (E20100, raw record) $bn': agg.set_index('RECID').loc[r, 'E20100'] * n_ret[g] / 1e9,
                     'gifts above half of AGI (returns)': w[m & ((h15['char_cash'] + h15['char_noncash'])
                                                                 > 0.5 * np.maximum(h15['agi'], 0))].sum()})
    pd.DataFrame(rows).to_csv(f'{out}/check_charity.csv', index=False)
    item_mix(h15, out)


def target_year_rows(h15, donors, models, factors, out):
    """Steps 7 and 8: the balance sheet drawn again, the PUF items grown to the target year and every total
    recomputed, as rows in the generated file's schema."""
    from scale import start as START
    from pretrain import qgroups as Q
    from pretrain import model as M
    maps = D.load_maps(Y)
    stk = donors.reset_index(drop=True).copy()                    # the donors in the stack's own coding
    # PUF bands from the final 2015 dollars, and the computed flags from them
    bands = reband(h15)
    for c in bands:
        stk[c] = bands[c].to_numpy().astype(stk[c].dtype)
    for flag, parts in D.META.get('computed_flags', {}).items():
        stk[flag] = (stk[[f'{p}_band' for p in parts]] != 0).any(axis=1).astype(stk[flag].dtype)
    qg = Q.puf_group(h15['agi'].to_numpy(float), Q.puf_groups()['cuts'])
    stk['agi_group'], stk['q_group'], stk['label_ok'] = qg.astype(stk['agi_group'].dtype), qg, True
    stk['record_id'] = int(S['id_block']) + np.arange(len(stk))
    nw_before = D.draw_dollars(donors.reset_index(drop=True), maps, Y)['net_worth'].to_numpy(float)   # the donors' own
    # 7. wealth: the SCF slots drawn again; their stored picks are the kept try's (keyed by record id)
    new, dol, accepted, tried, kept = redraw_wealth(stk, models, factors, maps)
    for c in new:
        stk[c] = new[c].to_numpy().astype(stk[c].dtype)
    from generate import top_billionaires as TB
    for c, u in TB.kept_picks(stk['record_id'].to_numpy(), kept, int(S['wealth_salt'])).items():
        stk[c] = u                                                # float64: the exact picks the try used
    rows = D.draw_dollars(stk, maps, Y)                           # target-year dollars from the new bands and picks
    assert np.allclose(rows['net_worth'].to_numpy(float), dol['net_worth'].to_numpy(float), rtol=1e-9, atol=1e-3), \
        'the balance sheet kept differs from its try'
    rows['hidden_wealth_fallback'] = (accepted == -1).astype(np.int64)
    # 8. the PUF items grown from 2015 (start.grow_dollars on the PUF block alone), then every total recomputed
    puf15 = h15[PRIMITIVES + ['capital_gains_line13', 'capital_gains', 'business_income', 'business_income_drawn',
                             'other_agi', 'filing_status',   # filing status sets the capital loss limit
                             'ss_taxable', common.GROSS_PENSIONS]   # computed: recombine checks them
              + [k for k in D.META['computed_totals'] if k != 'business_income']].copy()
    puf15 = puf15.loc[:, ~puf15.columns.duplicated()]
    grown, growth = START.grow_dollars(puf15, PY, Y)
    # the gain pieces are before the loss limit, grown like any amount; recombine applies the limit
    grown, _ = START.recombine(grown, Y)
    for c in grown:
        rows[c] = grown[c].to_numpy()
    rows, _ = START.recombine(rows, Y)
    rows['synthetic_top'] = 1
    rows['hidden_group'] = h15['hidden_group'].to_numpy()
    rows['weight_2015'] = h15['weight'].to_numpy()
    rows['donor_record_id'] = h15['donor_record_id'].to_numpy()
    rows['weight'] = rows['weight_2015']                          # set in the file's units by rebuild_hidden_returns
    rows.to_parquet(f'{out}/hidden_{Y}.parquet', index=False)
    growth.to_csv(f'{out}/growth_{Y}.csv', index=False)
    # the income-wealth pattern, the new draws against the donors' own
    agi19 = D.agi(rows)
    w = h15['weight'].to_numpy(float)
    nw_after = rows['net_worth'].to_numpy(float)
    res = []
    for g in range(N_G):
        m = h15['hidden_group'].to_numpy() == g
        for name, nw in [('donor\'s own draw', nw_before), ('drawn again', nw_after)]:
            res.append({'group': GROUPS[g], 'balance sheet': name, 'returns': w[m].sum(),
                        f'mean AGI ({Y} $m)': (w * agi19)[m].sum() / w[m].sum() / 1e6,
                        f'mean net worth ({Y} $m)': (w * nw)[m].sum() / w[m].sum() / 1e6,
                        f'median net worth ({Y} $m)': common.wquantile(nw[m], w[m], [0.5])[0] / 1e6,
                        'median of net worth / AGI (AGI above 0)': common.wquantile((nw / agi19)[m & (agi19 > 0)],
                                                                                    w[m & (agi19 > 0)], [0.5])[0]
                        if (m & (agi19 > 0)).any() else np.nan,
                        'share with net worth $100m+ (%)': 100 * w[m & (nw >= 1e8)].sum() / w[m].sum(),
                        'share with net worth $1bn+ (%)': 100 * w[m & (nw >= 1e9)].sum() / w[m].sum(),
                        'records': int(m.sum())})
    pd.DataFrame(res).to_csv(f'{out}/check_wealth.csv', index=False)
    acc = pd.Series(accepted).value_counts().sort_index()
    acc.rename(index={-1: 'fallback (every try $1bn+)'}).to_csv(f'{out}/check_wealth_tries.csv', header=['records'])
    common.say('wealth: records accepted at each try:\n' + acc.to_string())
    return rows


def item_mix(h15, out):
    """The rebuilt returns' item mix by group, in 2015 dollars: every part's positive and negative dollars, their
    shares of the group's AGI, and the returns holding each. Writes item_mix_2015.csv."""
    w = h15['weight'].to_numpy(float)
    rows = []
    cols = PRIMITIVES + ['capital_gains', 'dividends', 'business_income', 'sch_cf_income', 'sch_e_income', 'adjustments',
                         'other_agi', 'agi']
    for g in range(N_G):
        m = h15['hidden_group'].to_numpy() == g
        agi = (w * h15['agi'].to_numpy(float))[m].sum()
        for v in cols:
            x = h15[v].to_numpy(float)
            pos, neg = (w * np.maximum(x, 0))[m].sum(), (w * np.minimum(x, 0))[m].sum()
            rows.append({'group': GROUPS[g], 'item': v, 'positive $m': pos / 1e6, 'negative $m': neg / 1e6,
                         'returns with positive': w[m & (x > 0)].sum(), 'returns with negative': w[m & (x < 0)].sum(),
                         'positive, share of the group\'s AGI': pos / agi, 'negative, share of the group\'s AGI': neg / agi})
    pd.DataFrame(rows).to_csv(f'{out}/item_mix_2015.csv', index=False)


# ---- the entry point ----------------------------------------------------------------------------------------------
def rebuild_hidden_returns(g19):
    """The rebuilt aggregate records as rows to append to the generated file g19, in target-year dollars; built first
    if hidden_<year>.parquet is not there. Each weight is the return's 2015 population weight times g19's filer weight
    over the 2015 PUF's retained returns, so scaling filers to SOI's returns gives it its share of returns."""
    path = f'{folder()}/hidden_{Y}.parquet'
    rows = pd.read_parquet(path) if os.path.exists(path) else build()
    missing = [c for c in g19.columns if c not in rows.columns]
    assert not missing, f'columns of the generated file the rebuilt returns lack: {missing}'
    assert not set(rows['record_id']) & set(g19['record_id']), 'a rebuilt return\'s record id is in the file'
    from generate import generate as G
    filer = g19['filer'].to_numpy() == 1
    r2015 = float(np.sum(G.LABEL_META['returns']))
    rows['weight'] = rows['weight_2015'] * g19['weight'].to_numpy(float)[filer].sum() / r2015
    rows = rows[list(g19.columns) + ['synthetic_top', 'hidden_group', 'weight_2015', 'donor_record_id',
                                     'hidden_wealth_fallback']].copy()
    # missing values only where the file has them too (ssn2 without a spouse, dep_ssn without dependents)
    bad = sorted(set(rows.columns[rows.isna().any()]) - set(g19.columns[g19.isna().any()]))
    assert not bad, f'missing values in the rebuilt returns in columns the file has none in: {bad}'
    for c in g19.columns:
        if rows[c].dtype != g19[c].dtype and g19[c].dtype != object and not rows[c].isna().any():
            rows[c] = rows[c].astype(g19[c].dtype)
    return rows
