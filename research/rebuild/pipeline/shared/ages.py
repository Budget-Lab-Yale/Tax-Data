"""The primary filer's age range on every source (settings.json, "ages").

The slot age_range has nine categories: the PUF demographic file's six ranges
for filers who are not someone's dependent (0 under 26, 1 26-34, 2 35-44,
3 45-54, 4 55-64, 5 65+), and its three for dependent filers (6 under 18,
7 18-25, 8 26 and over), kept apart.

PUF. The observed range where the demographic file gives one. The records with
none (no demographics, or coded "not present") get probabilities instead, from
step 0, and train.py draws a range from them at the start of every pass:
  1. a prior: a multinomial logit of the range on what the return shows about
     age (filing status, AGI, wages, Social Security, pensions, IRA
     distributions, the early-withdrawal penalty, the standard deduction's
     65+/blind additions, children at home, other dependents; no capital or
     business income), fitted on the known ranges, one fit for joint returns
     and one for the rest;
  2. in each SOI Table 1.6 cell (filing status x AGI bracket) where at least
     min_missing_share of the weight has no range, one factor per range
     multiplies the prior until the missing records' weight in each range is
     the table's count less the known-range weight (negative residuals set to
     zero, the rest rescaled to the missing weight; where the table merges
     under 26 into 26-34, the pair has one target). Elsewhere the residual is
     mostly sampling noise and the prior stands;
  3. "not present" records lie outside Table 1.6 and keep the prior;
     dependent filers with no range get the known dependent filers' shares.

Surveys and non-filers. The range of the primary filer's age. Couples: in a
mixed-sex couple the man is the primary, except the woman with a chance that
depends on the man's range, set so the surveys' share of female primaries by
the primary's range is the PUF's (joint returns with demographics); in a
same-sex couple the older partner. The draw is keyed by household (SCF) or
record id (non-filers). Ages are taken as the survey reports them (no
adjustment for the interview date).
"""
import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler

from shared import common

S = common.SETTINGS['ages']
N_RANGES = common.N_RANGES
assert common.PUF_YEAR == 2015, 'the standard deduction amounts below are 2015 law'
ADULT_EDGES = [26, 35, 45, 55, 65]
BASIC = {1: 6300, 2: 12600, 3: 6300, 4: 9250}          # 2015 standard deduction
EXTRA = {1: 1550, 2: 1250, 3: 1250, 4: 1550}           # each 65+/blind addition
PANEL = {2: 'Returns of married persons filing jointly and returns of surviving spouses',
         3: 'Returns of married persons filing separately', 4: 'Returns of heads of households',
         1: 'Returns of single persons'}
ROWS = ['Under 26', '26 under 35', '35 under 45', '45 under 55', '55 under 65', '65 and over']
AGI_EDGES = [-np.inf, 1, 5e3, 10e3, 15e3, 20e3, 25e3, 30e3, 40e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, 1.5e6,
             2e6, 5e6, 10e6, np.inf]                    # Table 1.6's AGI columns 2-20, lower edge inclusive
PUF_FIELDS = ['RECID', 'S006', 'DSI', 'MARS', 'FDED', 'P04470', 'E00100', 'E00200', 'E02400', 'E01500', 'E01400',
              'E09900', 'XOCAH', 'XOCAWH', 'XOPAR', 'XOODEP']


def adult_range(age):
    return np.searchsorted(ADULT_EDGES, np.asarray(age, float), side='right')


def uniform_by_id(ids, salt):
    """A uniform draw per id (SplitMix64 of the id), the same on every run."""
    with np.errstate(over='ignore'):           # the arithmetic wraps on purpose
        x = np.asarray(ids, dtype=np.float64).astype(np.uint64) + np.uint64(salt) * np.uint64(0x9E3779B97F4A7C15)
        x = (x ^ (x >> np.uint64(30))) * np.uint64(0xBF58476D1CE4E5B9)
        x = (x ^ (x >> np.uint64(27))) * np.uint64(0x94D049BB133111EB)
        x = x ^ (x >> np.uint64(31))
    return (x >> np.uint64(11)).astype(np.float64) / 2.0 ** 53


# ---- PUF: observed ranges and step 0 ------------------------------------------------------------
def _puf_frame(puf):
    p = puf[PUF_FIELDS].copy()
    g = pd.read_csv(S['puf_demographics'])
    p = p.merge(g[['RECID', 'AGERANGE']], on='RECID', how='left')
    p['w'] = p.S006 / 100
    p['AGERANGE'] = p.AGERANGE.fillna(-1).astype(int)
    dep = (p.DSI == 1).to_numpy()
    p['age_range'] = -1
    p.loc[~dep & p.AGERANGE.between(1, 6), 'age_range'] = p.AGERANGE - 1
    p.loc[dep & p.AGERANGE.between(1, 3), 'age_range'] = p.AGERANGE + 5
    p['col'] = pd.cut(p.E00100, AGI_EDGES, right=False, labels=False) + 2
    add = (p.P04470 - p.MARS.map(BASIC)) / p.MARS.map(EXTRA)
    whole = (add - add.round()).abs() < 1e-6
    p['sd'] = np.select([p.FDED == 1, (p.FDED == 2) & whole & (add.round() == 0),
                         (p.FDED == 2) & whole & (add.round() == 1), (p.FDED == 2) & whole & (add.round() >= 2),
                         p.FDED == 2], ['itemizes', 'sd0', 'sd1', 'sd2', 'sd_other'], 'no deduction')
    return p


def _features(d):
    f = pd.DataFrame(index=d.index)
    a = d.E00100.astype(float)
    f['agi'] = np.sign(a) * np.log1p(np.abs(a))
    f['agi_le0'] = (a < 1).astype(float)
    for c, n in [('E00200', 'wages'), ('E02400', 'ss'), ('E01500', 'pens'), ('E01400', 'ira')]:
        f[n] = np.log1p(d[c].clip(lower=0))
        f['has_' + n] = (d[c] > 0).astype(float)
    f['penalty'] = (d.E09900 > 0).astype(float)
    for s in ['itemizes', 'sd1', 'sd2', 'sd_other', 'no deduction']:
        f['sd_' + s] = (d.sd == s).astype(float)
    f['kids_home'] = d.XOCAH.clip(upper=3)
    f['other_deps'] = (d.XOCAWH + d.XOPAR + d.XOODEP).clip(upper=3)
    for m in [3, 4]:
        f[f'mars{m}'] = (d.MARS == m).astype(float)
    return f


def _prior(known, target):
    """Six-range probabilities for target, from logits fitted on known (weighted), joint and the rest apart."""
    out = np.zeros((len(target), 6))
    for joint in [True, False]:
        t = known[(known.MARS == 2) == joint]
        X = _features(t)
        sc = StandardScaler().fit(X)
        m = LogisticRegression(C=1.0, max_iter=3000).fit(sc.transform(X), t.age_range, sample_weight=t.w)
        assert list(m.classes_) == list(range(6))
        sel = ((target.MARS == 2) == joint).to_numpy()
        if sel.any():
            out[sel] = m.predict_proba(sc.transform(_features(target[sel])))
    return out


def _calibrate(prior, w, groups, targets, iters=1000):
    f = np.ones(prior.shape[1])
    for _ in range(iters):
        q = prior * f
        q /= q.sum(1, keepdims=True)
        s = np.array([(w[:, None] * q[:, g]).sum() for g in groups])
        gap = np.max(np.abs(s - targets)) / w.sum()
        if gap < 1e-8:
            break
        for g, si, ti in zip(groups, s, targets):
            if si > 0:
                f[g] *= ti / si
    return q, gap


def puf_age_ranges(puf):
    """Per PUF record (puf: build_table.read_puf, RECID order as the PUF file, aggregate records dropped): the observed range (-1 where
    missing) and an (n, 9) array of probabilities (zero rows where observed). Logs a cell table."""
    p = _puf_frame(puf)
    t = pd.read_csv(S['table_1_6'])
    t = t[(t.tax_year == common.PUF_YEAR) & t.panel.isin(PANEL.values()) & t.row_label.isin(ROWS) & t.col_seq.between(2, 20)]
    probs = np.zeros((len(p), N_RANGES))
    missing = (p.age_range < 0).to_numpy()
    dep = (p.DSI == 1).to_numpy()
    # dependent filers with no range: the known dependent filers' weighted shares
    kd = p[dep & ~missing]
    shares = np.array([kd.w[kd.age_range == k].sum() for k in range(6, 9)])
    probs[np.ix_(dep & missing, range(6, 9))] = shares / shares.sum()
    # adults: prior, then Table 1.6
    known = p[~dep & ~missing]
    adult = ~dep & missing
    prior = _prior(known, p[adult])
    probs[np.ix_(adult, range(6))] = prior
    in_table = adult & (p.AGERANGE != 0).to_numpy()            # "not present" lies outside Table 1.6
    log = []
    for mars, panel in PANEL.items():
        for c in range(2, 21):
            cell = (p.MARS == mars).to_numpy() & (p.col == c).to_numpy()
            sel = cell & in_table
            if not sel.any():
                continue
            kn = p[cell & ~missing]
            known_w = np.array([kn.w[kn.age_range == a].sum() for a in range(6)])
            known_w[0] += kn.w[kn.age_range.isin([6, 7])].sum()   # dependent filers under 26 count as under 26
            w = p.w.to_numpy()[sel]
            share = w.sum() / (w.sum() + known_w.sum())
            row = dict(filing_status=mars, agi_from=AGI_EDGES[c - 2], missing_returns=w.sum(), missing_share=share)
            if share >= S['min_missing_share']:
                v = t[(t.panel == panel) & (t.col_seq == c)].set_index('row_label').reindex(ROWS)
                vals = v.value.to_numpy(float)
                if np.isnan(vals[0]):
                    assert v.flag.iloc[1] == 'combined', (mars, c)
                    groups, tab = [[0, 1], [2], [3], [4], [5]], np.r_[vals[1], vals[2:]]
                else:
                    groups, tab = [[a] for a in range(6)], vals
                assert not np.isnan(tab).any(), (mars, c)
                res = tab - np.array([known_w[g].sum() for g in groups])
                row.update(residual_sum=res.sum(), negative_residuals=-res[res < 0].sum())
                res = res.clip(min=0)
                res = res * w.sum() / res.sum()
                q, row['gap'] = _calibrate(prior[sel[adult]], w, groups, res)
                probs[np.ix_(sel, range(6))] = q
            log.append(row)
    log = pd.DataFrame(log)
    log.to_csv(f'{common.DATA}/age_step0_cells.csv', index=False)
    cal = log[log.missing_share >= S['min_missing_share']]
    common.say(f'step 0: {missing.sum()} PUF records with no range ({p.w[missing].sum() / 1e6:.3f}M returns); '
               f'{len(cal)} Table 1.6 cells calibrated, holding {cal.missing_returns.sum() / 1e6:.3f}M of them; '
               f'largest calibration gap {cal.gap.max():.1e}')
    assert np.allclose(probs[missing].sum(1), 1) and (probs[~missing] == 0).all()
    return p[['RECID', 'age_range']].copy(), probs


# ---- surveys and non-filers: whose age ----------------------------------------------------------
def puf_female_primary_share(puf):
    """Share of joint returns (with demographics, not dependents) whose primary is a woman, by range."""
    p = puf[['RECID', 'S006', 'DSI', 'MARS']]
    p = p.merge(pd.read_csv(S['puf_demographics'])[['RECID', 'AGERANGE', 'GENDER']], on='RECID')
    p = p[(p.MARS == 2) & (p.DSI == 0) & p.AGERANGE.between(1, 6) & p.GENDER.isin([1, 2])]
    w = p.S006 / 100
    return np.array([w[(p.AGERANGE == k + 1) & (p.GENDER == 2)].sum() / w[p.AGERANGE == k + 1].sum()
                     for k in range(6)])


def woman_first_chance(man_age, woman_age, weight, target, iters=200):
    """By the man's range: the chance the woman is the primary, so that the weighted share of female
    primaries by the primary's range equals target. Fixed point on mixed-sex couples."""
    mr, fr, w = adult_range(man_age), adult_range(woman_age), np.asarray(weight, float)
    pi = target.copy()
    for _ in range(iters):
        c = pi[mr]
        fem = np.bincount(fr, w * c, 6)
        male = np.bincount(mr, w * (1 - c), 6)
        share = fem / np.maximum(fem + male, 1e-12)
        pi = np.clip(pi * target / np.maximum(share, 1e-12), 0, 1)
    return pi, share


def primary_age(age1, age2, male1, male2, couple, key, pi):
    """The primary filer's age: age1 for one-adult units; in couples, the rule above."""
    age1, age2 = np.asarray(age1, float), np.asarray(age2, float)
    male1, male2 = np.asarray(male1, float), np.asarray(male2, float)
    mixed = couple & (male1 != male2)
    man_age = np.where(male1 == 1, age1, age2)
    woman_age = np.where(male1 == 1, age2, age1)
    woman_first = uniform_by_id(key, S['primary_rule_salt']) < pi[adult_range(np.where(mixed, man_age, 30))]
    out = age1.copy()
    out[mixed] = np.where(woman_first, woman_age, man_age)[mixed]
    same = couple & ~mixed
    out[same] = np.maximum(age1, age2)[same]
    return out, mixed & woman_first
