"""The split of a married couple's earnings (settings "earn_split"; branch rebuild-earnsplit, 4 November 2026).

The PUF's two Schedule SE fields: E30400 is the couple's net earnings from self-employment (Schedule SE, after the
92.35% factor), E30500 the spouse's part of it. The PUF demographic booklet says so (the primary's earnings are
E30400 - E30500), and the data agree: E30400 is never below E30500, E30400 alone equals 0.9235 x Schedules C and F on
joint returns with no partnership income, and EARNSPLIT is reproduced on 99.9% of joint returns with no wages
(explore/2026-11-04/earnsplit/FINDINGS.md). Production (src/imputations/earnings_split.R) read E30400 as the
primary's alone.

Since 6 November E30400 is read as the booklet defines it: the lesser of net earnings and the room under the Social
Security wage base (explore/2026-11-05/se_tax_gap/RESULTS.md). part_se_from_schedule_se reads net earnings from the
reported tax, E09400, above that cap. EARNSPLIT and E30500 stay on the capped amounts.

The spouse's share of the couple's self-employment income (slot spouse_se_share, build_table.PUF_SHARES): on the PUF,
E30500 / E30400 on a joint return with E30400 above zero and some Schedule C, F or part_se; missing (not shown, not
scored) on a joint return with such income and E30400 zero: a loss, under the $400 floor, or wages at the cap (the
tax is then all Medicare, and a split matters only if one spouse has room under the cap). On the CPS (the non-filers' source), the
spouse's share of the couple's Schedule SE earnings computed the same way from each person's self-employment income
(se_earnings); where the couple has self-employment income but no Schedule SE earnings, the spouse's share of the
income itself, cut to 0-1, as before.

EARNSPLIT (slot earn_split, four codes) is the primary filer's share of the couple's earnings, wages plus Schedule SE
earnings: 1 the primary earned 75% or more, 2 more than 25% and less than 75%, 3 less than 25%; 0 not present (a
return that is not joint, or a joint return with no earnings). On the PUF and the CPS, absent on the SCF, CEX and SIPP.
  PUF: the demographic file's code; 0 on returns that are not joint. Joint returns with no demographics (most with AGI
       above $400,000): 0 where they have no earnings, else probabilities over codes 1-3 ("step 0" for the split, as
       ages.py does for the age range), and train.py draws a code from them at the start of every pass: a gradient
       boosted classifier (scikit-learn's HistGradientBoostingClassifier) fitted on the joint returns with
       demographics and earnings, on what the return shows about the split (the lowest and highest primary's share
       its wages and Schedule SE split allow, the wage share of earnings, wages, Schedule SE earnings, AGI, children,
       Social Security, pensions), then cut to the codes those bounds allow. Trees carry the pattern at the top of
       the known range (AGI up to about $400,000) flat beyond it; a multinomial logit, linear in log AGI, carried
       the middle's pattern upward instead. Checked by fitting below AGI $250,000 and predicting the known codes at
       $250,000-400,000 (data/earn_split_step0_check.csv; explore/2026-11-04/earnsplit/out/step0_models.csv
       compares the logit, a logit with splines and this model), and against the booklet's full-sample counts.
  CPS: the code of the couple's own earnings on the same definition (code_of, se_earnings).
At generation the wage share is drawn inside the range the code and the drawn self-employment split allow (in bands in
generate.py, exactly in dollars.py), with each person's earnings capped there as E30400 is (dollars.wage_share_bounds).
"""
import numpy as np
import pandas as pd
from sklearn.ensemble import HistGradientBoostingClassifier

from shared import common

S = common.SETTINGS['earn_split']
SE_FACTOR = float(S['se_factor'])          # Schedule SE: net earnings are 92.35% of the self-employment income
SE_FLOOR = float(S['se_floor'])            # and none are counted below $400
SE_RATE_SS = float(S['se_rate_social_security'])     # Schedule SE's Social Security rate, on E30400
SE_RATE_MEDICARE = float(S['se_rate_medicare'])      # and its Medicare rate, on all net earnings
SE_WAGE_BASE = float(S['se_wage_base'])              # the Social Security wage base of the PUF year
BEYOND_SHARE, BEYOND_DOLLARS = float(S['se_beyond_share']), float(S['se_beyond_dollars'])
N_CODES = 4
# the primary filer's share of the couple's earnings in each code (closed: a share of exactly 25% or 75% fits both)
RANGES = {1: (0.75, 1.0), 2: (0.25, 0.75), 3: (0.0, 0.25)}
LOW = np.array([np.nan, 0.75, 0.25, 0.0])
HIGH = np.array([np.nan, 1.0, 0.75, 0.25])
PUF_FIELDS = ['RECID', 'S006', 'MARS', 'E00100', 'E00200', 'E30400', 'E30500', 'E02400', 'E01500', 'XOCAH', 'XOCAWH',
              'XOPAR', 'XOODEP']


def se_earnings(x):
    """A person's Schedule SE net earnings from self-employment income x: 0.9235 x where that is $400 or more, else 0."""
    e = SE_FACTOR * np.asarray(x, float)
    return np.where(e >= SE_FLOOR, e, 0.0)


def half_rounding_unit(x):
    """Half the PUF's rounding unit for an amount (2015 booklet, ninth disclosure step): nearest $10 up to $10,000,
    nearest $100 up to $100,000, four significant digits above that; an amount below $5 is set to $2."""
    a = np.abs(np.asarray(x, float))
    out = np.full(a.shape, 5.0)
    out[a >= 1e4] = 50.0
    big = a >= 1e5
    out[big] = 0.5 * 10.0 ** (np.floor(np.log10(a[big])) - 3)
    out[a < 5] = 3.0
    return out


def schedule_se_earnings(tax, capped):
    """The couple's Schedule SE net earnings, read from the reported tax. tax: E09400; capped: E30400, the lesser of
    net earnings and the room under the Social Security wage base. Tax is 12.4% of E30400 plus 2.9% of net earnings, so
    net earnings are (tax - 12.4% x E30400) / 2.9%; where the tax is within rounding of 15.3% x E30400 the cap does not
    bind and net earnings are E30400 itself (dividing the tax by 2.9% would blow up its rounding). Example: a partner
    with no wages in 2015, tax $43,694, E30400 $118,500: (43,694 - 14,694) / 0.029 = $1,000,000."""
    tax, capped = np.asarray(tax, float), np.asarray(capped, float)
    both = SE_RATE_SS + SE_RATE_MEDICARE
    same = np.abs(tax - both * capped) <= half_rounding_unit(tax) + both * half_rounding_unit(capped)
    return np.where(same, capped, (tax - SE_RATE_SS * capped) / SE_RATE_MEDICARE)


def part_se_from_schedule_se(tax, capped, sch_cf, partnership):
    """part_se (fix B, explore/2026-11-05/se_tax_gap/RESULTS.md; John, 6 November 2026): Schedule SE earnings other
    than Schedules C and F, before the 92.35% factor, so that Schedules C and F plus part_se are the self-employment
    income whose tax is the reported tax. On a return with any partnership income or loss: net earnings / 0.9235 less
    C and F. On any other return the same difference where net earnings differ from 92.35% of C and F beyond rounding,
    else zero; also zero where net earnings are zero and C and F are at most $433 (no tax either way, a loss included).
    It can be negative: net earnings below 92.35% of C and F (a couple's profit and loss netted the other way, or
    C and F that Schedule SE did not tax). Not whole dollars. Arguments are E09400, E30400, E00900 + E02100, and whether
    the return has partnership income or loss."""
    sch_cf = np.asarray(sch_cf, float)
    net = schedule_se_earnings(tax, capped)
    total = np.where(net > 0, net / SE_FACTOR, 0.0)
    expected = SE_FACTOR * sch_cf
    beyond = np.abs(net - expected) > np.maximum(BEYOND_SHARE * np.abs(expected), BEYOND_DOLLARS)
    nothing = (net == 0) & (sch_cf <= SE_FLOOR / SE_FACTOR)
    return np.where(np.asarray(partnership, bool) | (beyond & ~nothing), total - sch_cf, 0.0)


def code_of(primary, total):
    """EARNSPLIT's code from the primary's earnings and the couple's: 0 where the total is not above zero."""
    primary, total = np.asarray(primary, float), np.asarray(total, float)
    share = np.where(total > 0, primary / np.where(total > 0, total, 1.0), np.nan)
    return np.where(total > 0, np.where(share >= 0.75, 1, np.where(share > 0.25, 2, 3)), 0).astype(np.int64)


def wage_share_bounds(wages, e1, e2, code):
    """The spouse's share of the couple's wages that puts the primary's share of earnings, (wages x (1 - share) + e1) /
    (wages + e1 + e2), in the code's range; e1, e2 each person's Schedule SE earnings. Returns (low, high, unreachable):
    low = high = NaN where there is no bound (code 0, or no wages); where no share reaches the range, the share that
    comes nearest (0 or 1), flagged unreachable."""
    w = np.maximum(np.asarray(wages, float), 0.0)
    e1, e2, code = np.asarray(e1, float), np.asarray(e2, float), np.asarray(code)
    T = w + e1 + e2
    on = (code >= 1) & (code <= 3) & (w > 0)
    c = np.where(on, code, 1)
    a, b = LOW[c], HIGH[c]
    ww = np.where(on, w, 1.0)
    raw_lo = 1 - (b * T - e1) / ww
    raw_hi = 1 - (a * T - e1) / ww
    lo, hi = np.clip(raw_lo, 0, 1), np.clip(raw_hi, 0, 1)
    unreachable = on & ((raw_hi < 0) | (raw_lo > 1))
    return np.where(on, lo, np.nan), np.where(on, hi, np.nan), unreachable


# ---- the PUF: observed codes and step 0 ---------------------------------------------------------------------------
def _frame(puf):
    p = puf[PUF_FIELDS].copy()
    g = pd.read_csv(common.SETTINGS['ages']['puf_demographics'], usecols=['RECID', 'EARNSPLIT'])
    p = p.merge(g, on='RECID', how='left', validate='one_to_one')
    p['w'] = p.S006 / 100
    p['W'] = p.E00200.clip(lower=0).astype(float)
    p['E'] = p.E30400.astype(float)
    p['e2'] = p.E30500.astype(float)
    p['e1'] = p.E - p.e2
    p['T'] = p.W + p.E
    T = p['T'].where(p['T'] > 0)
    p['lo'] = (p.e1 / T).fillna(0.0)                 # the primary's share with all the wages on the spouse
    p['hi'] = ((p.W + p.e1) / T).fillna(0.0)         # and with all of them on the primary
    return p


def _features(d):
    f = pd.DataFrame(index=d.index)
    a = d.E00100.astype(float)
    f['agi'] = np.sign(a) * np.log1p(np.abs(a))
    f['lo'], f['hi'] = d['lo'], d['hi']
    f['wage_part'] = d.W / d['T']
    f['wages'] = np.log1p(d.W)
    f['se'] = np.log1p(d.E)
    f['has_se'] = (d.E > 0).astype(float)
    f['se_spouse_part'] = np.where(d.E > 0, d.e2 / d.E.where(d.E > 0, 1.0), 0.0)
    f['ss'] = np.log1p(d.E02400.clip(lower=0))
    f['pens'] = np.log1p(d.E01500.clip(lower=0))
    f['kids_home'] = d.XOCAH.clip(upper=3)
    f['other_deps'] = (d.XOCAWH + d.XOPAR + d.XOODEP).clip(upper=3)
    return f


def _allowed(d):
    """[n, 3] whether codes 1-3 can be reached by some split of the wages (closed ranges)."""
    return np.column_stack([(d['hi'].to_numpy() >= RANGES[k][0] - 1e-12) & (d['lo'].to_numpy() <= RANGES[k][1] + 1e-12)
                            for k in (1, 2, 3)])


def _fit_predict(known, target):
    g = S['model']
    m = HistGradientBoostingClassifier(max_iter=int(g['max_iter']), learning_rate=float(g['learning_rate']),
                                       max_leaf_nodes=int(g['max_leaf_nodes']), min_samples_leaf=int(g['min_samples_leaf']),
                                       random_state=int(g['seed']))
    m.fit(_features(known), known.EARNSPLIT.astype(int), sample_weight=known.w)
    assert list(m.classes_) == [1, 2, 3], m.classes_
    p = m.predict_proba(_features(target)) * _allowed(target)
    s = p.sum(1, keepdims=True)
    assert (s > 0).all(), 'a record no code can reach'
    return p / s


def puf_codes(puf):
    """Per PUF record (puf: build_table.read_puf, aggregate records dropped): earn_split's code (-1 where step 0 gives
    probabilities instead) and an (n, 4) array of probabilities (zero rows where the code is known). Writes the
    step 0 check (data/earn_split_step0_check.csv) and logs a summary."""
    p = _frame(puf)
    joint = (p.MARS == 2).to_numpy()
    have = p.EARNSPLIT.notna().to_numpy()
    assert (p.loc[have & ~joint, 'EARNSPLIT'] == 0).all(), 'EARNSPLIT above 0 on a return that is not joint'
    earns = (p['T'] > 0).to_numpy()
    code = np.where(joint, p.EARNSPLIT.fillna(-1).to_numpy(), 0).astype(np.int64)
    missing = joint & ~have
    code[missing & ~earns] = 0                                     # no earnings: nothing to split
    todo = missing & earns
    code[todo] = -1
    w = p.w.to_numpy()
    # the known codes against the return's own earnings (FINDINGS.md: code 0 means no earnings)
    kj = joint & have
    off0 = kj & earns & (code == 0)
    off1 = kj & ~earns & (code > 0)
    unreach = kj & (code > 0) & earns & ~_allowed(p)[np.arange(len(p)), np.clip(code - 1, 0, 2)]
    common.say(f'earn_split: joint returns with demographics {w[kj].sum() / 1e6:.2f}M; code 0 with earnings '
               f'{w[off0].sum() / 1e6:.3f}M, a code above 0 with none {w[off1].sum() / 1e6:.3f}M, a code no split of '
               f'the wages reaches {w[unreach].sum() / 1e6:.3f}M ({int(unreach.sum())} records; kept as given)')
    known = p[kj & earns & (code > 0)]
    probs = np.zeros((len(p), N_CODES))
    probs[todo, 1:] = _fit_predict(known, p[todo])
    # the check: fitted below AGI $250,000, the known codes at $250,000-400,000 predicted
    lo_line, hi_line = S['check_agi']
    band = known.E00100.between(lo_line, hi_line, inclusive='left')
    fit, held = known[known.E00100 < lo_line], known[band]
    rows = []
    if len(held):
        ph = _fit_predict(fit, held)
        for lab, sel in [('all', np.ones(len(held), bool)), ('no Schedule SE earnings', (held.E == 0).to_numpy()),
                         ('with Schedule SE earnings', (held.E > 0).to_numpy())]:
            hw = held.w.to_numpy()[sel]
            for k in (1, 2, 3):
                rows.append({'returns at AGI $250,000-400,000': lab, 'code': k, 'records': int(sel.sum()),
                             'true share (%)': 100 * (hw * (held.EARNSPLIT.to_numpy()[sel] == k)).sum() / hw.sum(),
                             'predicted share (%)': 100 * (hw * ph[sel, k - 1]).sum() / hw.sum()})
    tw = w[todo]
    for k in (1, 2, 3):
        rows.append({'returns at AGI $250,000-400,000': 'joint returns with no demographics and earnings (step 0)',
                     'code': k, 'records': int(todo.sum()), 'true share (%)': np.nan,
                     'predicted share (%)': 100 * (tw * probs[todo, k]).sum() / tw.sum(),
                     'predicted returns (millions)': (tw * probs[todo, k]).sum() / 1e6,
                     "booklet: full sample less public file (millions)": S['booklet_full_less_public'][str(k)]})
    chk = pd.DataFrame(rows)
    chk.to_csv(f'{common.DATA}/earn_split_step0_check.csv', index=False)
    common.say(f'earn_split step 0: {int(todo.sum()):,} joint records with no demographics and some earnings '
               f'({tw.sum() / 1e6:.3f}M returns) get probabilities; {int((missing & ~earns).sum()):,} with no earnings '
               f'get 0. Check (fitted below AGI ${lo_line:,.0f}, predicted at ${lo_line:,.0f}-{hi_line:,.0f}):\n'
               + chk.round(2).to_string(index=False))
    assert np.allclose(probs[code == -1].sum(1), 1) and (probs[code != -1] == 0).all()
    return code, probs, p
