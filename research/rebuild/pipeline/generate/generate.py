"""Generation: draws PUF-style records from the trained networks and writes them to gen/<run>/stack.parquet.

Two batches are drawn, each in equal shares from every pooled network. The filers are drawn under the PUF label; each
has its AGI group set in advance and is weighted by 1 / q of its group (qgroups.py). The non-filers are drawn under the
CPS label at the target year. Each record is drawn one slot at a time, each slot from the network's probabilities given
the slots drawn so far. The slots come in phases: the record's own PUF items, the computed totals, the exact ages, and
then the SCF, CEX, CPS and SIPP items, each under that survey's label. Rules on the draw keep the items consistent with
each other. The consistency rules add holders of some assets. To offset them, the whole stack is drawn several
times, and after each draw the odds of those holdings are scaled down. FEATURES.md, "3. Generation", describes the
method.
"""
import json
import os

import numpy as np
import pandas as pd
import torch

from shared import common
from shared.ages import uniform_by_id
from pretrain import model as M
from pretrain import qgroups as Q
from prep import soi as TG

G = common.SETTINGS['generation']
C = common.SETTINGS['consistency']
NC = len(M.CONTEXT)
P = M.PREDICTED


def slots_of(names):
    """The PREDICTED indices of slot names, as a tensor."""
    return torch.tensor([P.index(v) for v in names], dtype=torch.long)


# ---- The slots by phase, and the rules' slots ----

COMPUTED = M.COMPUTED                     # {total: {part: sign}}
FLAGS = M.COMPUTED_FLAGS                  # {flag: parts}
PUF_SLOTS = [v for v in P if 'puf' in M.PRESENT_ON[v] and v not in COMPUTED and v not in FLAGS]
# The late slots: drawn after the PUF items and their totals, in this order, under the batch's label.
LATE_SLOTS = M.PUF_SHARES + list(M.SHARED_CATEGORIES)
# The QBI items under the SCF label. Each kind of business's outside workers are drawn after its industry group, and
# they are zero when the industry group is none. The industry group is never none on a record with any of that kind's
# PUF parts nonzero.
SCF_STRUCTURE = M.META['scf_structure']
SCF_AFTER = {P.index(v): P.index(b) for v, b in SCF_STRUCTURE['after'].items()}
INDUSTRY_PARTS = {P.index(v): slots_of(parts) for v, parts in SCF_STRUCTURE['industry_parts'].items()}
SCF_SLOTS = M.SCF_SLOTS
CEX_SLOTS = M.CEX_SLOTS
CPS_SLOTS = M.CPS_SLOTS
CPS_STRUCTURE = M.META['cps_structure']
SIPP_SLOTS = M.SIPP_SLOTS
SIPP_STRUCTURE = M.META['sipp_structure']
SIPP_SPOUSE = slots_of(SIPP_STRUCTURE['spouse'])
SIPP_FLAGS = list(SIPP_STRUCTURE['lh_of'])                    # the leisure and hospitality flags, drawn after the tips
SIPP_TIPS = [v for v in SIPP_SLOTS if v not in SIPP_FLAGS]
AGE_SLOTS = M.EXACT_AGES                   # the exact ages, drawn after the PUF items under the CPS label
SPOUSE = slots_of(CPS_STRUCTURE['spouse'])
# Each adult's overtime is drawn after the other CPS slots, when the spouse's share of wages is known.
CPS_OVERTIME = CPS_STRUCTURE['overtime']
CPS_FIRST = [v for v in CPS_SLOTS if v not in CPS_OVERTIME]
CPS_ZERO_WITHOUT = CPS_STRUCTURE['zero_without']
# The dependents' ages and Social Security number status: the k-th dependent's are present when n_dep >= k.
DEP_AGES = [P.index(v) for v in CPS_STRUCTURE['dependent_ages']]
DEP_SSN = [P.index(v) for v in CPS_STRUCTURE['dependent_ssn']]
PRIMARY_SSN = [P.index(v) for v in CPS_STRUCTURE['primary_ssn']]        # the primary filer's status: never none
PRIMARY_AGE = P.index(CPS_STRUCTURE['primary_age'])
SEX_PRIMARY = P.index('sex_primary')
# The spouse's exact age: value 0 is none and value a + 1 is age a. A spouse, and the head of a non-filer unit, is 18
# or over.
SPOUSE_AGE = P.index(next(v for v in CPS_STRUCTURE['spouse'] if v in CPS_STRUCTURE['ages']))
ADULT_AGE = 18
CPS_CATEGORICAL = slots_of(M.PERSON_CATEGORIES)
# {a gated share's slot index: [its bases' indices]}. A share is drawn only for a married couple with some of a base.
GATED = {s: list(bases) for s, (_, bases) in M.SHARE_GATES.items()}
ES_SLOT, WAGE_SHARE, SE_SHARE = P.index('earn_split'), P.index('spouse_wage_share'), P.index('spouse_se_share')
# The primary filer's age range for each value of the exact age (value 0 is none, value a + 1 is age a).
RANGE_OF_AGE_VALUE = torch.tensor([-1] + [int(r) for r in np.searchsorted([26, 35, 45, 55, 65],
                                                                          np.arange(M.PERSON_CATEGORIES[CPS_STRUCTURE['primary_age']] - 1),
                                                                          side='right')])
REP_SLOTS = slots_of(M.REPORTED)
RULES = C['rules']
for _r in RULES:
    assert all(M.OWNED_BY.get(v, 'puf') != 'scf' for v in _r['when_any_nonzero']), _r['name']
    assert all(v in SCF_SLOTS for v in _r['then_at_least_one_nonzero']), _r['name']
RULE_ITEMS = [slots_of(r['when_any_nonzero']) for r in RULES]
RULE_HOLDINGS = [slots_of(r['then_at_least_one_nonzero']) for r in RULES]
RULED = sorted({v for r in RULES for v in r['then_at_least_one_nonzero']}, key=P.index)   # the ruled holdings
DEPENDENT_RANGES = torch.tensor(common.DEPENDENT_RANGES)
SURVEY_ONLY = slots_of(AGE_SLOTS + SCF_SLOTS + CEX_SLOTS + CPS_SLOTS + SIPP_SLOTS)
CPS_VIEW = slots_of(AGE_SLOTS + CPS_SLOTS + [r for r in M.REPORTED if 'cps' in M.PRESENT_ON[r]])   # on every CPS row
TOTALS = slots_of(list(COMPUTED) + list(FLAGS))
GATED_SLOTS = torch.tensor(list(GATED), dtype=torch.long)
POINTS = 32                                # dollar points per band, for the computed totals (part_points)
TYPE_SLOTS = M.TYPE_SLOTS                  # drawn first, in a random order for each record
STATUS_SLOTS = list(M.TAX_STATUS)          # then these, in this order (mars, n_dep_ctc, n_dep_eitc)
MARS, MARRIED, AGE_RANGE = P.index('filing_status'), P.index('married'), P.index('age_range')
# The pooled networks: each is a run under OUT/model, with a shift added to its generation seeds.
MODELS = G['models']
LABEL_RECORDS = int(G['label_records'])     # the filer batch's number of records, over all networks
ID_BLOCK = 10_000_000                       # network k's record ids start at k x ID_BLOCK
REAL_PUF = common.REAL_PUF                 # settings generation.filers is 'real_puf': the filers are the real PUF
# The filer batch sets the AGI group label on every record in advance.
LABEL = 'agi_group'
assert M.PUF_LABELS == [LABEL], 'the label design needs the AGI group label in the table (build_table.agi_group_label)'
LABEL_META = M.META['labels'][LABEL]


# ---- The survey years and SCF waves of each record ----

def nonfiler_years():
    """The CPS years of the non-filer batch, as [(year, share)]: the target year alone when the CPS has it, otherwise
    the CPS years either side, half each."""
    from shared import survey_mix
    from reweight import target_list as TLS
    if common.TARGET_YEAR in TLS.cps_years():
        return [(common.TARGET_YEAR, 1.0)]
    return survey_mix.mix('cps')


def scf_waves():
    """The SCF waves the balance sheet is drawn at, from the target year's configuration (prior.yaml, "balance
    sheet"), as [(survey year of the wave, income year, share)] in order of survey year."""
    from reweight import target_list as TLS
    prior = TLS.resolve(common.TARGET_YEAR)['prior']['balance sheet']
    where = f'targets_config/{common.TARGET_YEAR}/prior.yaml, prior "balance sheet"'
    out = []
    for key, share in prior.items():
        src, wave = str(key).split()
        assert src == 'scf', f'{where}: {key} is not an SCF wave'
        income_year = int(wave) - 1
        assert income_year in M.YEARS, f'{where}: no {wave} wave (income year {income_year}) in the table'
        assert float(share) > 0, f'{where}: share {share} for {key}'
        out.append((int(wave), income_year, float(share)))
    total = sum(s for _, _, s in out)
    assert abs(total - 1) < 1e-6, f'{where}: shares sum to {total}'
    return sorted((w, y, s / total) for w, y, s in out)


SCF_WAVES = scf_waves()
SCF_YEAR_INDEX = np.array([M.YEARS.index(y) for _, y, _ in SCF_WAVES])


def assign_waves(n, seed):
    """Each of n records' SCF wave (an index into SCF_WAVES), drawn with the waves' shares by a random generator of
    its own."""
    u = np.random.default_rng([int(G['scf_wave_seed']), int(seed)]).random(n)
    k = np.searchsorted(np.cumsum([s for _, _, s in SCF_WAVES]), u, side='right')
    return np.minimum(k, len(SCF_WAVES) - 1)


def survey_years():
    """The years of the CEX, CPS and SIPP that records are drawn at, from the target year's configuration, as
    {survey: [(tax year, share)]}. Every year must be one the training table holds for that survey."""
    from shared import survey_mix
    t = pd.read_parquet(common.TABLE, columns=['source', 'year'])
    out = {}
    for src in survey_mix.SOURCES:
        out[src] = survey_mix.mix(src)
        seen = set(t.loc[t.source == src, 'year'])
        for y, share in out[src]:
            assert y in seen and y in M.YEARS, \
                f'{src} {y} is in the year mix but the training table has {src} years {sorted(seen)}: a year the model never saw cannot be drawn'
    return out


SURVEY_YEARS = survey_years()
SURVEY_YEAR_INDEX = {src: np.array([M.YEARS.index(y) for y, _ in m]) for src, m in SURVEY_YEARS.items()}


def assign_years(src, n, seed):
    """Each of n records' year of the survey src (an index into SURVEY_YEARS[src]), drawn with the years' shares. Each
    survey has a random generator of its own (settings generation.survey_year_seeds), and these draws leave every other
    draw unchanged."""
    u = np.random.default_rng([int(G['survey_year_seeds'][src]), int(seed)]).random(n)
    k = np.searchsorted(np.cumsum([s for _, s in SURVEY_YEARS[src]]), u, side='right')
    return np.minimum(k, len(SURVEY_YEARS[src]) - 1)


# ---- The computed totals ----

_points = {}


def part_points(total, device):
    """For each part of a total, 32 dollar points for each band [bands, 32]: the weighted quantiles of the PUF records
    in the band. Also the total's cut points. Computed once for each total."""
    if total in _points:
        return _points[total]
    parts = list(COMPUTED[total])
    t = pd.read_parquet(common.TABLE, columns=['source', 'weight'] + parts + [v + '_band' for v in parts])
    t = t[t.source == 'puf']
    w = t['weight'].to_numpy(float)
    out = {}
    for v in parts:
        x, b = t[v].to_numpy(float), t[v + '_band'].to_numpy()
        pts = np.zeros((M.N_CHOICES[v], POINTS))
        for k in range(M.N_CHOICES[v]):
            sel = b == k
            if sel.any():
                o = np.argsort(x[sel], kind='stable')
                xs, F = x[sel][o], np.cumsum(w[sel][o]) / w[sel].sum()
                pts[k] = xs[np.minimum(np.searchsorted(F, (np.arange(POINTS) + 0.5) / POINTS), len(xs) - 1)]
        out[v] = torch.tensor(pts, dtype=torch.float64, device=device)
    e = common.edges()[f'puf/{common.PUF_YEAR}/{total}']
    out['neg'] = torch.tensor(e['negative']['cuts'], dtype=torch.float64, device=device)
    out['pos'] = torch.tensor(e['positive']['cuts'], dtype=torch.float64, device=device)
    _points[total] = out
    return out


def computed_band(values, gen, total):
    """The band of a computed total. Each part is read as one of its band's dollar points, chosen at random, and the
    signed sum of the parts is placed on the total's PUF cut points."""
    pts = part_points(total, values.device)
    s = torch.zeros(len(values), dtype=torch.float64, device=values.device)
    for v, sign in COMPUTED[total].items():
        b = values[:, NC + P.index(v)]
        assert (b >= 0).all(), f'{v} not drawn before {total}'
        k = torch.randint(0, POINTS, (len(values),), generator=gen, device=values.device)
        s += float(sign) * pts[v][b, k]
    band = torch.zeros(len(values), dtype=torch.long, device=values.device)
    neg, pos = s < 0, s > 0
    band[neg] = 1 + torch.searchsorted(pts['neg'], s[neg].contiguous(), right=False)
    band[pos] = M.BAND_LAYOUT[total]['first_positive'] + torch.searchsorted(pts['pos'], s[pos].contiguous(),
                                                                             right=False)
    return band


# ---- Rules on a draw ----
# Each rule takes the probabilities of the slot being drawn on each record, rules out some values on some records, and
# rescales the rest.

def scf_order(size, gen):
    """A random order of the SCF-only slots for each record, with each ratio slot moved after its base and the QBI
    items moved last."""
    scf = slots_of(SCF_SLOTS)
    perm = scf[torch.rand(size, len(scf), generator=gen).argsort(1)]
    rows = torch.arange(size)
    pairs = [(P.index(slot), P.index(base)) for slot, (_, base) in M.RATIOS.items() if slot in SCF_SLOTS]
    # The SCF's ratio slots and their bases, and each kind of business's workers and its industry group. (The CPS's
    # ratio slots have a PUF base, which is drawn earlier.)
    for i, ib in pairs + list(SCF_AFTER.items()):
        pos, pos_b = (perm == i).int().argmax(1), (perm == ib).int().argmax(1)
        swap = pos < pos_b
        perm[rows[swap], pos[swap]], perm[rows[swap], pos_b[swap]] = ib, i
    # The QBI items come after every other SCF slot, in the same order among themselves. qbi_structure gives an
    # industry group to every record with that kind's PUF parts nonzero, while in the SCF only business owners have
    # one. Drawn earlier, an industry group would make the network treat the record as a business owner.
    qbi = torch.tensor(sorted(set(INDUSTRY_PARTS) | set(SCF_AFTER)))
    key = torch.arange(perm.shape[1]).expand(size, -1) + perm.shape[1] * torch.isin(perm, qbi).long()
    perm = torch.gather(perm, 1, key.argsort(1))
    return perm


def dependent(values):
    """Records whose primary filer is in a dependent-filer age range."""
    return torch.isin(values[:, NC + P.index('age_range')], DEPENDENT_RANGES.to(values.device))


def only(probs, rows, allowed=None):
    """On the selected rows, the probabilities limited to the allowed values (allowed is [rows, values], 1 or 0) and
    rescaled to sum to 1. With allowed None, the value 0 for certain. If every allowed value has probability zero,
    the allowed values are taken as equally likely."""
    if allowed is None:
        probs[rows] = 0.0
        probs[rows, 0] = 1.0
        return probs
    q = probs[rows] * allowed
    total = q.sum(1, keepdim=True)
    q = torch.where(total > 0, q / total.clamp_min(1e-30), allowed / allowed.sum(1, keepdim=True).clamp_min(1))
    probs[rows] = q
    return probs


def not_none(probs, rows):
    """On the selected rows, the value 0 (none) ruled out and the other values rescaled."""
    allowed = torch.ones(int(rows.sum()), probs.shape[1], device=probs.device)
    allowed[:, 0] = 0.0
    return only(probs, rows, allowed)


def cps_structure(values, slot, probs):
    """Rules that keep the CPS's person items in line with the record's PUF items. The spouse's items are none or
    zero unless the unit is married, and a married spouse has a sex and an age of 18 or over. The primary filer's sex
    and Social Security number status are never none. The k-th dependent has an age and a status exactly when there
    are at least k dependents. The primary filer's exact age lies in its age range. The head of a non-filer unit is 18
    or over, because the non-filer targets count adults from 18 and children under 17."""
    married = values[:, NC + P.index('married')] == 1
    n_dep = values[:, NC + P.index('n_dep')]
    on_spouse = torch.isin(slot, SPOUSE.to(slot.device))
    categorical = torch.isin(slot, CPS_CATEGORICAL.to(slot.device))
    probs = only(probs, on_spouse & ~married)
    probs = not_none(probs, on_spouse & married & categorical)
    probs = not_none(probs, slot == SEX_PRIMARY)
    sel = (slot == SPOUSE_AGE) & married
    adult = torch.zeros(probs.shape[1], device=probs.device)
    adult[ADULT_AGE + 1:M.PERSON_CATEGORIES[P[SPOUSE_AGE]]] = 1.0   # value a + 1 is age a
    probs = only(probs, sel, adult.expand(int(sel.sum()), -1))
    for k, j in list(enumerate(DEP_AGES, start=1)) + list(enumerate(DEP_SSN, start=1)):
        probs = only(probs, (slot == j) & (n_dep < k))
        probs = not_none(probs, (slot == j) & (n_dep >= k))
    for j in PRIMARY_SSN:
        probs = not_none(probs, slot == j)
    # Overtime is zero without wages; the spouse's is zero without a share of the couple's wages above zero.
    # dollars.py then holds each person's overtime to their wages less tips.
    for item, base in CPS_ZERO_WITHOUT.items():
        probs = only(probs, (slot == P.index(item)) & (values[:, NC + P.index(base)] <= 0))
    sel = slot == PRIMARY_AGE
    if sel.any():
        rng = RANGE_OF_AGE_VALUE.to(probs.device)
        allowed = torch.zeros(int(sel.sum()), probs.shape[1], device=probs.device)
        allowed[:, :len(rng)] = (rng[None, :] == values[sel, NC + P.index('age_range')][:, None]).float()
        nonfiler = values[sel, NC + P.index('filer')] == 0
        allowed[nonfiler, :ADULT_AGE + 1] = 0.0                       # rules out none and ages under 18
        probs = only(probs, sel, allowed)
    return probs


def positive(values, v):
    """Whether the amount v was drawn above zero (in a positive band)."""
    return values[:, NC + P.index(v)] >= M.BAND_LAYOUT[v]['first_positive']


def earn_structure(values, slot, probs):
    """Rules for the earnings split code. It is 0 unless the unit is married. A married couple with wages has a code
    of 1 to 3; on the PUF, code 0 with earnings occurs on only 0.02% of joint returns' weight. A married couple with
    neither wages nor positive self-employment income has code 0. Otherwise the network's probabilities stand."""
    sel = slot == ES_SLOT
    if not bool(sel.any()):
        return probs
    married = values[:, NC + MARRIED] == 1
    wages = positive(values, 'wages')
    se = torch.zeros_like(married)
    for b in GATED[SE_SHARE]:
        se = se | (values[:, NC + b] >= M.BAND_LAYOUT[P[b]]['first_positive'])
    probs = only(probs, sel & (~married | (~wages & ~se)))
    some = sel & married & wages
    allowed = torch.zeros(int(some.sum()), probs.shape[1], device=probs.device)
    allowed[:, 1:4] = 1.0
    return only(probs, some, allowed)


_BAND_TABLES = {}


def se_earnings_t(x):
    """Schedule SE net earnings from self-employment income x (a tensor): 92.35% of x where that is $400 or more,
    otherwise zero."""
    e = float(M.META['earn_split']['se_factor']) * x
    return torch.where(e >= float(M.META['earn_split']['se_floor']), e, torch.zeros_like(e))


def primary_earnings_share(s, W, e1, e2, cap):
    """The primary filer's share of the couple's earnings when the spouse's share of the couple's wages W is s. Each
    person's earnings are their wages plus their self-employment earnings (e1 for the primary filer, e2 for the
    spouse), the latter counted only up to the room left under the Social Security wage base cap."""
    w1, w2 = W * (1 - s), W * s
    p1 = w1 + torch.minimum(e1, (cap - w1).clamp(min=0))
    p2 = w2 + torch.minimum(e2, (cap - w2).clamp(min=0))
    return p1 / (p1 + p2).clamp(min=1e-9)


def capped_wage_share_range(W, e1, e2, a, b, cap):
    """The lowest and highest spouse's share of wages that put the primary filer's share of earnings between a and b
    (primary_earnings_share). The primary filer's share falls as the spouse's wage share rises. Each end is found by
    bisection: the interval is halved 40 times. Where no share reaches the range, the nearest share, 0 or 1, is
    returned."""
    share = lambda s: primary_earnings_share(s, W, e1, e2, cap)
    s0, s1 = torch.zeros_like(W), torch.ones_like(W)
    t0, t1 = s0.clone(), s1.clone()
    for _ in range(40):
        m = (s0 + s1) / 2
        ok = share(m) <= b                              # s1 ends as the smallest wage share that reaches b
        s1, s0 = torch.where(ok, m, s1), torch.where(ok, s0, m)
        m = (t0 + t1) / 2
        ok = share(m) >= a                              # t0 ends as the largest wage share that stays at or above a
        t0, t1 = torch.where(ok, m, t0), torch.where(ok, t1, m)
    lo = torch.where(share(torch.zeros_like(W)) <= b, torch.zeros_like(W), s1)
    hi = torch.where(share(torch.ones_like(W)) >= a, torch.ones_like(W), t0)
    return lo, hi


def band_tables(device):
    """The tables that wage_share_bands uses, made once for each device. They hold: each band's median in PUF-year
    dollars of wages, of the spouse's share of self-employment income and of its bases (for a band with no record,
    the middle of its cut points); the range of the primary filer's share of earnings for each earnings split code;
    and, for each year, each band's range of the spouse's share of wages on that CPS year's cut points."""
    key = str(device)
    if key in _BAND_TABLES:
        return _BAND_TABLES[key]
    E = common.edges()
    bases = [P[b] for b in GATED[SE_SHARE]]
    med = {}
    for name in ['wages', 'spouse_se_share'] + bases:
        e = E[f'puf/{common.PUF_YEAR}/{name}']
        n, fp = M.BAND_LAYOUT[name]['n_bands'], M.BAND_LAYOUT[name]['first_positive']
        pos, neg = list(e['positive']['cuts']), list(e['negative']['cuts'])
        x = np.zeros(n)
        for k, rec in enumerate(e['bands']):
            if rec['median'] is not None:
                x[k] = rec['median']
            elif k >= fp:
                i = k - fp
                lo = pos[i - 1] if i >= 1 else 0.0
                hi = pos[i] if i < len(pos) else lo
                x[k] = (lo + hi) / 2
            elif 1 <= k < fp and neg:
                i = k - 1
                lo = neg[i - 1] if i >= 1 else neg[0]
                hi = neg[i] if i < len(neg) else 0.0
                x[k] = (lo + hi) / 2
        med[name] = torch.tensor(x, dtype=torch.float64, device=device)
    rng = M.META['earn_split']['ranges']
    low = torch.tensor([0.0] + [rng[str(k)][0] for k in (1, 2, 3)], dtype=torch.float64, device=device)
    high = torch.tensor([0.0] + [rng[str(k)][1] for k in (1, 2, 3)], dtype=torch.float64, device=device)
    n = M.N_CHOICES['spouse_wage_share']
    fp = M.BAND_LAYOUT['spouse_wage_share']['first_positive']
    wlo = np.full((len(M.YEARS), n), 2.0)
    whi = np.full((len(M.YEARS), n), -1.0)
    for i, y in enumerate(M.YEARS):
        e = E.get(f'cps/{y}/spouse_wage_share')
        if e is None or 'positive' not in e:
            continue
        cuts = list(e['positive']['cuts'])
        wlo[i, 0] = whi[i, 0] = 0.0
        for k in range(fp, n):
            j = k - fp
            wlo[i, k] = cuts[j - 1] if j >= 1 else 0.0
            whi[i, k] = cuts[j] if j < len(cuts) else 1.0
    _BAND_TABLES[key] = {'median': med, 'se_bases': bases, 'low': low, 'high': high,
                         'wage_lo': torch.tensor(wlo, dtype=torch.float64, device=device),
                         'wage_hi': torch.tensor(whi, dtype=torch.float64, device=device)}
    return _BAND_TABLES[key]


def sipp_structure(values, slot, probs):
    """Rules for the SIPP's items. The spouse's tips and flag are zero unless the unit is married. The primary
    filer's tips are zero without wages, and the spouse's are zero without a share of wages above zero. A leisure and
    hospitality flag is 0 where its tips are zero."""
    married = values[:, NC + MARRIED] == 1
    probs = only(probs, torch.isin(slot, SIPP_SPOUSE.to(slot.device)) & ~married)
    for share, base in SIPP_STRUCTURE['zero_without'].items():
        probs = only(probs, (slot == P.index(share)) & (values[:, NC + P.index(base)] <= 0))
    for flag, share in SIPP_STRUCTURE['lh_of'].items():
        sel = slot == P.index(flag)
        if sel.any():
            assert (values[sel, NC + P.index(share)] >= 0).all(), f'{share} not drawn before {flag}'
        probs = only(probs, sel & (values[:, NC + P.index(share)] == 0))
    return probs


def deferral_structure(values, slot, probs):
    """Rules for elective deferrals. They are zero without wages, and dollars.py then holds each person's to their
    wages and the legal limit. The spouse's share of them is zero unless the unit is married."""
    probs = only(probs, (slot == P.index('elective_deferrals')) & (values[:, NC + P.index('wages')] <= 0))
    return only(probs, (slot == P.index('spouse_deferral_share')) & (values[:, NC + MARRIED] != 1))


def qbi_structure(values, slot, probs):
    """Rules for the QBI items. A kind of business's outside workers are zero where its industry group is none, and
    its industry group is not none where any of its PUF parts is nonzero."""
    for j, ind in SCF_AFTER.items():
        probs = only(probs, (slot == j) & (values[:, NC + ind] == 0))
    for j, parts in INDUSTRY_PARTS.items():
        sel = slot == j
        if not bool(sel.any()):
            continue
        probs = not_none(probs, (values[:, NC + parts.to(values.device)] != 0).any(1) & sel)
    return probs


def structural(values, slot, probs):
    """Every rule on a draw. A ratio slot whose base is zero is zero; then the rules above apply. A dependent filer
    holds no survey-only item, because the surveys count its wealth and spending in the household that claims it."""
    for slot_name, (_, base) in M.RATIOS.items():
        if P.index(slot_name) in GATED:     # a gated share is handled by Chunk.not_asked, not by one base
            continue
        probs = only(probs, (slot == P.index(slot_name)) & (values[:, NC + P.index(base)] == 0))
    probs = earn_structure(values, slot, probs)
    probs = cps_structure(values, slot, probs)
    probs = sipp_structure(values, slot, probs)
    probs = deferral_structure(values, slot, probs)
    probs = qbi_structure(values, slot, probs)
    return only(probs, dependent(values) & torch.isin(slot, SURVEY_ONLY.to(values.device)))


def scale_odds(probs, slot, factors):
    """On the records whose slot is a ruled holding, the odds of a nonzero value multiplied by the holding's offset
    factor."""
    for v, f in factors.items():
        if f != 1.0:
            s = probs.clone()
            s[:, 1:] *= f
            s /= s.sum(-1, keepdim=True)
            probs = torch.where((slot == P.index(v))[:, None], s, probs)
    return probs


def apply_must(slot, probs, must):
    """The slot's probabilities with zero ruled out on the records that a consistency rule says must hold it. Also
    returns which records those are."""
    rows = torch.arange(len(slot), device=slot.device)
    q = probs.clone()
    q[:, 0] = 0
    ok = must[rows, slot] & (q.sum(1) > 1e-12)
    return torch.where(ok[:, None], q / q.sum(1, keepdim=True).clamp_min(1e-30), probs), ok


# ---- One chunk of records ----

class Chunk:
    """A chunk of `size` records being drawn. Holds every slot's value (-1 where absent), which slots are drawn so far,
    and the consistency rules' bookkeeping. scf_year gives each record's year for its SCF phase (its wave's income
    year), as an index into model.YEARS; survey_year gives the same for the CEX, CPS and SIPP phases."""

    def __init__(self, model, size, order, preset, start, factors, gen, scf_year, survey_year, label='puf', year=None,
                 adult_only=False):
        # 1. The settings of the chunk.
        device = order.device
        self.adult_only = adult_only                                 # an adult age range under any label (real PUF)
        self.model, self.order, self.factors, self.gen = model, order, factors, gen
        # In the redraw of balance sheets (top_billionaires.redraw_records), records holds each record's rows and its
        # own random generator, [(rows, generator)]; see slot_probs.
        self.records = None
        self.scf_year = scf_year.to(device)
        self.survey_year = {src: y.to(device) for src, y in survey_year.items()}
        self.label = label                                           # the label for the batch's own items
        # 2. The starting values. The slots to be drawn are present and hidden (0, not yet filled). The survey-only
        #    slots are absent until their phase, except that under the CPS label the CPS's own and reported slots are
        #    present and hidden, as on a CPS training row. A share is absent until married and its bases are drawn
        #    (gate_shares, after_totals). The computed totals are present and hidden until they are computed. Then the
        #    preset values are set.
        self.rows = torch.arange(size, device=device)
        self.values = torch.full((size, len(M.SLOTS)), -1, dtype=torch.long, device=device)
        self.values[:, 0] = M.SOURCES.index(label)
        self.values[:, 1] = M.YEARS.index(common.PUF_YEAR if year is None else year)
        self.values[self.rows[:, None], NC + order] = 0
        self.values[:, NC + SURVEY_ONLY.to(device)] = -1
        if label == 'cps':
            self.values[:, NC + CPS_VIEW.to(device)] = 0
        self.values[:, NC + GATED_SLOTS.to(device)] = -1
        self.values[:, NC + TOTALS.to(device)] = 0
        self.filled = torch.zeros(size, len(P), dtype=torch.bool, device=device)
        for var, bands in preset.items():
            self.values[:, NC + P.index(var)] = bands[start:start + size].to(device)
            self.filled[:, P.index(var)] = True
            if P.index(var) in GATED:
                # With the real PUF, a gated share is preset on some records only. -1 means no preset value, and on
                # those records the share is drawn or not by the usual rule. A preset share stays as it is.
                self.filled[:, P.index(var)] = bands[start:start + size].to(device) >= 0
        # 3. The consistency rules' bookkeeping: which rules each record triggers and has resolved, which holdings it
        #    must hold, and, for each ruled holding, the network's own chance it is nonzero and whether it was forced.
        self.triggered = torch.zeros(size, len(RULES), dtype=torch.bool, device=device)
        self.resolved = self.triggered.clone()
        self.must = torch.zeros(size, len(P), dtype=torch.bool, device=device)
        self.own_chance = torch.zeros(size, len(RULED), dtype=torch.float64, device=device)
        self.forced = torch.zeros(size, len(RULED), dtype=torch.bool, device=device)
        self.ruled = torch.tensor([P.index(v) for v in RULED], device=device)
        # 4. For each source, the column of each slot in that source's packed records (-1 where the source lacks it).
        self.column = {}
        for src, slots in M.SOURCE_SLOTS.items():
            self.column[src] = torch.full((len(M.SLOTS),), -1, dtype=torch.long, device=device)
            self.column[src][list(slots)] = torch.arange(len(slots), device=device)
        self.not_asked = {}
        self.bounded = 0                                             # wage shares drawn inside an earnings split range
        self.gate_shares()

    def gate_shares(self):
        """Makes each share that is not yet drawn present and hidden once married and its bases are drawn, the unit is
        married, and it has some of a base. Otherwise the share stays absent. In the filer batch the base of the
        spouse's share of self-employment income, Schedules C and F, is a computed total, and the share stays absent
        until the totals are computed."""
        for s, (m, bases) in M.SHARE_GATES.items():
            known = self.filled[:, m].clone()
            some = torch.zeros_like(known)
            for b in bases:
                known &= self.filled[:, b]
                some |= self.values[:, NC + b] > 0
            asked = known & (self.values[:, NC + m] == 1) & some
            self.values[:, NC + s] = torch.where(self.filled[:, s], self.values[:, NC + s],
                                                 torch.where(asked, 0, -1))

    def own_rules(self, slot, probs):
        """Rules on the batch's own items. Full filing status is joint exactly when the unit is married; separate
        returns are not married, as in the data. Under the CPS label, or with adult_only, the age range is an adult
        one."""
        sel = slot == MARS
        if sel.any():
            assert self.filled[sel, MARRIED].all(), 'married is drawn before full filing status'
            married = self.values[sel, NC + MARRIED] == 1
            allowed = torch.zeros(int(sel.sum()), probs.shape[1], device=probs.device)
            allowed[married, 2] = 1.0
            allowed[~married, 1] = allowed[~married, 3] = allowed[~married, 4] = 1.0
            probs = only(probs, sel, allowed)
        sel = slot == AGE_RANGE
        if (self.label == 'cps' or self.adult_only) and sel.any():
            allowed = torch.zeros(int(sel.sum()), probs.shape[1], device=probs.device)
            allowed[:, :common.ADULT_RANGES] = 1.0
            probs = only(probs, sel, allowed)
        return probs

    def last_layer(self, values, hidden, src):
        """The network's last layer, run on the source's slots."""
        slots = M.SOURCE_SLOTS[src]
        return self.model.last_layer(*M.pack(values, hidden, slots), slots)

    def slot_probs(self, h, slot, groups=None):
        """Each row's probabilities for its slot, from its last-layer vector. In the redraw (self.records set), they are
        computed on each record's own rows. The last digits of the output layers' results depend on how many rows they
        run on, and this way a record's tries come out the same as when it is redrawn alone. groups gives, for each
        record, the rows of each distinct slot."""
        if self.records is None:
            return self.model.slot_probs(h, slot)
        if groups is None:                 # the same slot on every row (a ruled holding): one group per record
            same = torch.arange(len(self.records[0][0]), device=h.device)
            groups = [[(int(slot[0]), same)] for _ in self.records]
        return torch.cat([self.model.slot_probs(h[rows], slot[rows], groups[k]) for k, (rows, _) in enumerate(self.records)])

    def take(self, probs, slot):
        """Draws each record's slot from probs and records it as filled."""
        if self.records is None:
            draw = torch.multinomial(probs, 1, generator=self.gen).squeeze(1)
        else:
            draw = torch.cat([torch.multinomial(probs[rows], 1, generator=gen).squeeze(1) for rows, gen in self.records])
        self.values[self.rows, NC + slot] = draw
        self.filled[self.rows, slot] = True

    def force_picks(self, slot, nonzero):
        """The consistency rules' picks. When a record that triggers a rule first reaches one of the rule's holdings,
        one of the holdings not yet drawn is picked, and the record must hold it. The pick is in proportion to nonzero,
        the network's own chance that each holding is nonzero, before the offset's factors. The pick therefore does not
        depend on the factors."""
        for r in range(len(RULES)):
            g = RULE_HOLDINGS[r].to(slot.device)
            first = self.triggered[:, r] & ~self.resolved[:, r] & torch.isin(slot, g)
            if self.records is None and not first.any():
                continue
            self.resolved[:, r] |= first
            weight = (nonzero[:, g] * ~self.filled[:, g]).double().clamp_min(0) + 1e-12 * (~self.filled[:, g])
            if self.records is None:
                pick = torch.multinomial(weight[first], 1, generator=self.gen).squeeze(1)
                self.must[self.rows[first], g[pick]] = True
                continue
            hit = first.view(len(self.records), -1).any(1).tolist()
            for (rows, gen), h in zip(self.records, hit):
                if h:
                    f = first[rows]
                    pick = torch.multinomial(weight[rows][f], 1, generator=gen).squeeze(1)
                    self.must[rows[f], g[pick]] = True

    def puf_step(self, slot):
        """Draws one of the batch's own items for each record (a PUF variable), under the batch's label, given the
        slots drawn so far."""
        hidden = (self.values[:, NC:] >= 0) & ~self.filled
        h = self.last_layer(self.values, hidden, self.label)
        probs = self.model.slot_probs(h[self.rows, self.column[self.label][NC + slot]], slot)
        self.take(self.own_rules(slot, probs), slot)
        self.gate_shares()

    def compute_totals(self):
        """Computes the totals and flags from their parts, then does the bookkeeping in after_totals. Returns the
        number of records each rule applies to."""
        v = self.values
        for tot in COMPUTED:
            v[:, NC + P.index(tot)] = computed_band(v, self.gen, tot)
            self.filled[:, P.index(tot)] = True
        for flag, flag_parts in FLAGS.items():
            v[:, NC + P.index(flag)] = torch.stack([v[:, NC + P.index(p)] != 0 for p in flag_parts], 1).any(1).long()
            self.filled[:, P.index(flag)] = True
        return self.after_totals()

    def after_totals(self):
        """The bookkeeping after the totals, which depends only on the values drawn so far. It finds the rules each
        record triggers (none for a dependent filer, which holds no survey item), makes the survey slots present, and
        finds the records on which each share is not asked. Returns the number of records each rule applies to."""
        v = self.values
        dep = dependent(v)
        for r in range(len(RULES)):
            self.triggered[:, r] = (v[:, NC + RULE_ITEMS[r].to(v.device)] != 0).any(1) & ~dep
        v[:, NC + SURVEY_ONLY.to(v.device)] = 0                      # each survey's view leaves out the others' slots
        v[:, NC + REP_SLOTS.to(v.device)] = 0                        # reported slots: present and hidden, never drawn
        married = v[:, NC + P.index('married')] == 1
        # A share is not asked of a unit that is not married, has none of its bases, or is a dependent filer. A preset
        # share stays as it is. An asked share that is not yet drawn is present and hidden.
        self.not_asked = {}
        for j, bases in GATED.items():
            some = torch.zeros_like(married)
            for b in bases:
                some |= v[:, NC + b] > 0
            self.not_asked[j] = (~married | ~some | dep) & ~self.filled[:, j]
            v[~self.not_asked[j] & ~self.filled[:, j], NC + j] = 0
            v[self.not_asked[j], NC + j] = -1
        return {r['name']: int(self.triggered[:, k].sum()) for k, r in enumerate(RULES)}

    def survey_step(self, slot, label, year=None, groups=None):
        """Draws one survey-only slot for each record, as the survey `label` would see the record, under the rules.
        groups is used in the redraw (see slot_probs)."""
        # 1. Run the network on the records as the survey sees them, and get each record's probabilities for its slot.
        hidden = (self.values[:, NC:] >= 0) & ~self.filled
        view, view_hidden = M.seen_as(self.values, hidden, label, year)
        h, col = self.last_layer(view, view_hidden, label), self.column[label]
        slot_p = self.slot_probs(h[self.rows, col[NC + slot]], slot, groups)
        # 2. In the SCF phase, each ruled holding's chance of being nonzero, by the network alone. (No consistency rule
        #    applies in the other phases.)
        own_nonzero = torch.zeros(len(slot), len(P), device=slot.device)
        for v in (RULED if label == 'scf' else []):
            j = P.index(v)
            own_nonzero[:, j] = 1 - self.slot_probs(h[:, col[NC + j]], torch.full_like(slot, j))[:, 0]
        del h
        # 3. Apply the rules: the probabilities without the offset factors (kept for the offset), and with them. Then
        #    the consistency rules' picks, and zero ruled out where the record must hold the slot.
        own_probs = structural(self.values, slot, slot_p.clone())
        probs = structural(self.values, slot, scale_odds(slot_p.clone(), slot, self.factors))
        self.force_picks(slot, own_nonzero)
        probs, ok = apply_must(slot, probs, self.must)
        # 4. For records whose slot is a ruled holding, keep the network's own chance of nonzero and whether it was
        #    forced.
        k = (slot[:, None] == self.ruled[None, :])
        hit = k.any(1)
        col = k.int().argmax(1)
        self.own_chance[self.rows[hit], col[hit]] = 1 - own_probs[hit, 0].double()
        self.forced[self.rows[hit], col[hit]] = ok[hit]
        # 5. A share that is not asked is zero. In the CPS phase, the spouse's share of wages is held to the bands
        #    that fit the earnings split code.
        for j, m in self.not_asked.items():
            probs = only(probs, m & (slot == j))
        if label == 'cps':
            probs = self.wage_share_bands(slot, probs)
        # 6. Draw. A slot preset on some records only (a real PUF record's share) keeps its preset value. A share that
        #    is not asked is made absent again.
        keep = self.filled[self.rows, slot]
        before = self.values[self.rows, NC + slot].clone()
        self.take(probs, slot)
        if bool(keep.any()):
            self.values[self.rows[keep], NC + slot[keep]] = before[keep]
        for j, m in self.not_asked.items():
            self.values[m, NC + j] = -1

    def wage_share_bands(self, slot, probs):
        """A rule for the spouse's share of the couple's wages in the CPS phase. On a married couple with wages and an
        earnings split code of 1 to 3, only the bands that can put the primary filer's share of earnings in the code's
        range are allowed, on the record's CPS year's cut points. For this the couple's amounts are taken at their
        bands' medians in PUF-year dollars, and self-employment earnings count up to the PUF year's wage base.
        dollars.py later holds the exact share inside the exact range."""
        sel = (slot == WAGE_SHARE) & ~self.not_asked[WAGE_SHARE]
        code = self.values[:, NC + ES_SLOT]
        sel = sel & (code >= 1) & (code <= 3)
        if not bool(sel.any()):
            return probs
        B = band_tables(probs.device)
        v = self.values[sel]
        med = lambda name: B['median'][name][v[:, NC + P.index(name)].clamp(min=0)]
        W = med('wages')
        X = torch.zeros_like(W)
        for name in B['se_bases']:
            X = X + torch.where(v[:, NC + P.index(name)] >= 0, med(name), torch.zeros_like(W))
        sh = torch.where(v[:, NC + SE_SHARE] >= 0, med('spouse_se_share'), torch.zeros_like(W))
        e2, e1 = se_earnings_t(sh * X), se_earnings_t((1 - sh) * X)
        c = code[sel]
        lo, hi = capped_wage_share_range(W, e1, e2, B['low'][c], B['high'][c], float(M.META['earn_split']['se_wage_base']))
        yi = self.survey_year['cps'][sel]
        blo, bhi = B['wage_lo'][yi], B['wage_hi'][yi]                # [records, bands]: each band's range of shares
        allowed = torch.zeros(len(lo), probs.shape[1], device=probs.device)   # values past the share's bands stay 0
        allowed[:, :blo.shape[1]] = ((bhi >= lo[:, None] - 1e-9) & (blo <= hi[:, None] + 1e-9)).float()
        none = allowed.sum(1) == 0
        allowed[none] = 1.0                                          # not expected, since the bands cover 0 to 1
        self.bounded += int(sel.sum())
        return only(probs, sel, allowed)

    def finish(self):
        """Sets each share that is not asked of a record to zero (no spouse, none of the item, or a dependent filer).
        Called after the SIPP phase, which sees such shares as absent, as on the SIPP's rows."""
        for j, m in self.not_asked.items():
            self.values[m, NC + j] = 0


# ---- Drawing a batch of records ----

def complete_preset(preset, n, label, filer, totals_preset):
    """The values set on every record before anything is drawn: the caller's preset, the filing flag, and, for a
    batch under a survey's label, zero for each PUF amount that survey does not measure (interest on the CPS, for
    example). Nothing is drawn for those amounts."""
    preset = dict(preset or {})
    if totals_preset:
        missing = [v for v in list(COMPUTED) + list(FLAGS) if v not in preset]
        assert not missing, f'totals_preset: the computed totals and flags must be preset, missing {missing}'
    assert label != 'puf' or all(v in preset for v in M.PUF_LABELS), \
        'the filer batch presets the AGI group label (the network never sees it hidden on a PUF record)'
    preset['filer'] = torch.full((n,), filer, dtype=torch.long)
    if label != 'puf':
        # Only amounts can be missing from the batch's source; every category must be there.
        lacking = [v for v in PUF_SLOTS if label not in M.PRESENT_ON[v] and v not in preset and v not in M.PUF_LABELS]
        assert all(v in M.AMOUNTS for v in lacking), f'the {label} label lacks PUF categories: {lacking}'
        for v in lacking:
            preset[v] = torch.zeros(n, dtype=torch.long)
        if lacking:
            common.say(f'  {label} batch: zero on every record, not measured by the {label}: {lacking}')
    return preset


def slot_orders(size, first, then, rest, late, cpu_gen, device):
    """Each record's order of slots. The phases come in a fixed sequence; within most phases the slots come in a random
    order that differs from record to record."""
    shuffled = lambda x: x[torch.rand(size, len(x), generator=cpu_gen).argsort(1)]
    return torch.cat([shuffled(first), then.expand(size, -1), shuffled(rest), late.expand(size, -1),
                      shuffled(slots_of(AGE_SLOTS)), scf_order(size, cpu_gen), shuffled(slots_of(CEX_SLOTS)),
                      shuffled(slots_of(CPS_FIRST)), shuffled(slots_of(CPS_OVERTIME)),
                      shuffled(slots_of(SIPP_TIPS)), shuffled(slots_of(SIPP_FLAGS))], 1).to(device)


def draw_late_and_survey_slots(d, order, n_own, n_late):
    """Draws the rest of a chunk whose own items and computed totals are already drawn. First come the late slots (the
    spouse's share of self-employment income and the earnings split code) under the batch's label. Then come the exact
    ages under the CPS label, and then each survey's slots under that survey's label, at the record's year of that
    survey (for the SCF, the income year of the record's wave). Returns the values, each ruled holding's own chance of
    being nonzero, and whether a rule forced it."""
    k = n_own
    for count, label, year in [(n_late, d.label, None), (len(AGE_SLOTS), 'cps', d.survey_year['cps']),
                               (len(SCF_SLOTS), 'scf', d.scf_year), (len(CEX_SLOTS), 'cex', d.survey_year['cex']),
                               (len(CPS_SLOTS), 'cps', d.survey_year['cps']), (len(SIPP_SLOTS), 'sipp', d.survey_year['sipp'])]:
        for step in range(k, k + count):
            d.survey_step(order[:, step], label, year)
        k += count
    assert k == order.shape[1]
    d.finish()
    return d.values.cpu(), d.own_chance.cpu(), d.forced.cpu()


@torch.no_grad()
def generate(model, n, seed, preset=None, factors=None, chunk=50_000, label='puf', year=None, filer=1,
             totals_preset=False, adult_only=False):
    """Draws n PUF-style records from one network. Returns them as a table, and how many records each consistency
    rule applies to.

    label and year are the label and year under which the batch's own items are drawn; filer is the batch's filing
    flag. preset holds values set before anything is drawn ({variable: one band per record}). factors holds, for each
    ruled holding, the factor on its odds of being nonzero (the offset). With totals_preset, the computed totals and
    flags are preset (a real PUF record's own bands) and kept. With adult_only, an age range drawn under the PUF label
    is an adult one (used for the PUF's aggregate records). For each ruled holding the table also carries the network's
    own chance that it is nonzero (_own_<v>) and whether a rule forced it (_forced_<v>)."""
    # 1. Complete the preset values.
    preset, factors = complete_preset(preset, n, label, filer, totals_preset), factors or {}

    # 2. Seed the random generators. The slot orders are drawn on the CPU and the values on the device. Give each
    #    record its SCF wave and its CEX, CPS and SIPP years, each from a generator of its own.
    device = next(model.parameters()).device
    cpu_gen = torch.Generator().manual_seed(seed)
    gpu_gen = torch.Generator(device=device).manual_seed(seed)
    wave_k = assign_waves(n, seed)
    scf_year = torch.as_tensor(SCF_YEAR_INDEX[wave_k])
    svy_k = {src: assign_years(src, n, seed) for src in SURVEY_YEARS}
    svy_year = {src: torch.as_tensor(SURVEY_YEAR_INDEX[src][k]) for src, k in svy_k.items()}

    # 3. List the batch's own slots that are not preset, by phase: the type, then the filing status and credit
    #    children, then the other PUF items, then the late slots. A gated share that is preset on some records only is
    #    drawn among the late slots for the others.
    first = slots_of([v for v in TYPE_SLOTS if v not in preset])
    then = slots_of([v for v in STATUS_SLOTS if v not in preset])
    rest = slots_of([v for v in PUF_SLOTS if v not in preset and v not in TYPE_SLOTS
                     and v not in STATUS_SLOTS and v not in M.PUF_LABELS and v not in LATE_SLOTS])
    n_own = len(first) + len(then) + len(rest)
    late = slots_of([v for v in LATE_SLOTS if v not in preset or P.index(v) in GATED])
    rule_counts = {r['name']: 0 for r in RULES}

    # 4. Draw the records chunk by chunk. In each chunk: draw the own items one slot at a time, compute the totals
    #    (or, with totals_preset, only the bookkeeping that follows them), then draw the late and survey slots.
    parts = []
    for start in range(0, n, chunk):
        size = min(chunk, n - start)
        order = slot_orders(size, first, then, rest, late, cpu_gen, device)
        d = Chunk(model, size, order, preset, start, factors, gpu_gen, scf_year[start:start + size],
                  {src: y[start:start + size] for src, y in svy_year.items()}, label, year, adult_only)
        for step in range(n_own):
            d.puf_step(order[:, step])
        for name, count in (d.after_totals() if totals_preset else d.compute_totals()).items():
            rule_counts[name] += count
        parts.append(draw_late_and_survey_slots(d, order, n_own, len(late)))

    # 5. Put the chunks together as a table, with each record's SCF wave and the tax year of each survey phase.
    table = to_table(torch.cat([p[0] for p in parts]))
    oc, fc = torch.cat([p[1] for p in parts]).numpy(), torch.cat([p[2] for p in parts]).numpy()
    for i, v in enumerate(RULED):
        table[f'_own_{v}'], table[f'_forced_{v}'] = oc[:, i], fc[:, i]
    table['scf_wave'] = np.array([w for w, _, _ in SCF_WAVES])[wave_k]
    table['scf_year'] = np.array([y for _, y, _ in SCF_WAVES])[wave_k]
    for src, k in svy_k.items():
        table[f'{src}_year'] = np.array([y for y, _ in SURVEY_YEARS[src]])[k]
    return table, rule_counts


def to_table(values):
    """The drawn values as a data frame: source, year, one column per slot (bands for amounts), and weight 1."""
    v = values.numpy()
    out = pd.DataFrame({'source': [M.SOURCES[i] for i in v[:, 0]], 'year': [M.YEARS[i] for i in v[:, 1]]})
    for j, var in enumerate(P):
        out[M.column(var)] = v[:, NC + j]
    out['weight'] = 1.0
    return out


# ---- The filer batch, with each record's AGI group set in advance ----

def label_counts(n, records):
    """n records split across the groups in proportion to records, rounding by largest remainders."""
    share = np.asarray(records, float) / np.sum(records)
    base = np.floor(n * share).astype(int)
    rem = n - base.sum()
    base[np.argsort(-(n * share - base), kind='stable')[:rem]] += 1
    return base


def label_part(model, mod, k, n_models, fac, group_records=None, seed=None):
    """Network k's share of the filer batch (one of n_models shares). Its share of label_records is split across the
    12 AGI groups in proportion to the PUF's records in each group. Each record's group is set in advance and
    everything else is drawn given it. A record's weight is its group's PUF-year returns divided by the records drawn
    in the group (1 / q). The label check compares each record's band-AGI group, computed from its drawn bands, with
    its label (columns q_group and label_ok). For Part A's candidates (top_hidden.py), group_records and seed replace
    the split across groups and the seed label_seed. Returns the records, the label check's table and the rule
    counts."""
    rec, ret = np.asarray(LABEL_META['records'], float), np.asarray(LABEL_META['returns'], float)
    if group_records is None:
        n_k = equal_shares(LABEL_RECORDS, n_models)[k]
        n_g = label_counts(n_k, rec)
    else:
        n_g = np.asarray(group_records, dtype=int)
        assert n_g.shape == rec.shape and (n_g >= 0).all() and n_g.sum() > 0, f'label_part group_records {n_g}'
        n_k = int(n_g.sum())
    g_of = np.repeat(np.arange(len(rec)), n_g)
    seed = int(G['label_seed']) + mod['seed_shift'] if seed is None else int(seed)
    part, counts = generate(model, n_k, seed, preset={LABEL: torch.tensor(g_of)}, factors=fac)
    g = part[LABEL].to_numpy()
    assert (g == g_of).all()
    part['weight'] = ret[g] / n_g[g]
    qg = Q.puf_group(Q.band_agi(part), Q.puf_groups()['cuts'])
    part['q_group'], part['label_ok'] = qg, qg == g
    rows = []
    for j, name in enumerate(LABEL_META['labels']):
        s, x = g == j, qg[g == j]
        rows.append({'model': mod['seed'], 'group': name, 'records drawn': int(s.sum()),
                     'PUF records share': rec[j] / rec.sum(), 'weight per record (returns)': ret[j] / max(1, n_g[j]),
                     'band group differs (%)': 100 * float((x != j).mean()) if s.any() else np.nan,
                     'band group lower (%)': 100 * float((x < j).mean()) if s.any() else np.nan,
                     'band group higher (%)': 100 * float((x > j).mean()) if s.any() else np.nan,
                     'band group two or more away (%)': 100 * float((np.abs(x - j) >= 2).mean()) if s.any() else np.nan})
    check = pd.DataFrame(rows)
    common.say(f"{mod['run']}: the label check, the band-AGI group of the drawn bands against the label:\n"
               + check.to_string(index=False, float_format=lambda x: f'{x:.4g}'))
    return part, check, counts


# ---- The real PUF as the filers (settings generation.filers 'real_puf') ----
# In place of the filer batch, every 2015 PUF record keeps its own PUF items (their bands are preset), and each pooled
# network draws everything else given the record. Each record therefore appears once for each network (a "copy"),
# with its PUF weight divided by the number of networks. Its own 2015 amounts go with it (own_<item>), and
# dollars.own_values grows them to the target year.
REAL_ID = 50_000_000            # a real record's id is REAL_ID + copy index x REAL_COPY_BLOCK + its PUF RECID
REAL_COPY_BLOCK = 1_000_000
REAL_SEED_SHIFT = 500           # added to the filer batch's generation seed for a real-PUF copy
REAL_AGE_SALT = 2026110401      # key of the age-range draws where the PUF has none; the copy's index is added
REAL_EARN_SPLIT_SALT = 2026110501   # the same for the earnings split code
_REAL = {}


def own_items():
    """The PUF amounts for which a real record carries its own 2015 value: every PUF amount of the dollar map (the
    drawn items and the totals whose band is drawn), in the order of dollars.ORDER."""
    from prep import build_table as BT
    from shared import dollars as D
    return [v for v in D.ORDER if v in set(BT.PUF_AMOUNTS)]


def ordinary_puf_records(slots, own):
    """The training table's PUF rows: their bands of the given slots, age-range and earnings-split probabilities,
    weights, and own 2015 amounts (renamed own_<item>)."""
    from shared import dollars as D
    cols = list(dict.fromkeys(['source', 'record_id', 'weight', 'age_range'] + M.AGE_PROBS + M.ES_PROBS
                              + [M.column(v) for v in slots] + own))
    t = pd.read_parquet(common.TABLE, columns=cols)
    o = t[(t.source == 'puf').to_numpy()].reset_index(drop=True)
    del t
    na = o[cols].isna().sum()
    assert not na.any(), f'missing values on PUF rows: {na[na > 0].to_dict()}'
    # A slot may be missing (-1) on a PUF row only where it is missing in training too. The spouse's share of
    # self-employment income is missing where the PUF does not ask it, and it stays missing. The age range and the
    # earnings split code are missing on returns without demographic information; real_puf_part draws them from their
    # probabilities.
    may_lack = {'age_range', 'earn_split'} | set(M.PUF_SHARES)
    bad = {M.column(v): int((o[M.column(v)] < 0).sum()) for v in slots if v not in may_lack and (o[M.column(v)] < 0).any()}
    assert not bad, f'PUF rows with a slot absent: {bad}'
    if 'earn_split' in slots:
        none = o['earn_split'].to_numpy() < 0
        assert np.allclose(o.loc[none, M.ES_PROBS].sum(1), 1), 'earn_split: a PUF row with no code and no probabilities'
    return o.rename(columns={v: D.OWN + v for v in own})


def aggregate_puf_records(slots, own):
    """The PUF's four aggregate records: their items translated from the raw fields as Part A reads them (top_hidden),
    banded on the 2015 cut points, with their weights, computed flags, own 2015 amounts and AGI group label. The
    label comes from the bands, with the filing status taken as single for the label alone. The categories and the
    late slots are left out, because the networks draw them."""
    from shared import dollars as D
    from generate import top_hidden as TH
    agg_raw = TH.read_raw()
    # Alimony received and paid (E00800, E03500) are blank on the four records because SOI suppressed them. They are
    # read as zero, so that each record's AGI is still its E00100.
    sup = list(TH.SUPPRESSED_FIELDS)
    assert agg_raw[sup].isna().all().all(), 'the suppressed fields are not blank on the aggregate records'
    agg_raw = agg_raw.copy()
    agg_raw[sup] = 0.0
    f = TH.puf_items(agg_raw, itemizer=True)
    na = f.isna().sum()
    assert not na.any(), f'aggregate records: items missing after the translation: {na[na > 0].to_dict()}'
    # The totals as the training table defines them: capital gains before the loss limit, and other_agi as the
    # remainder of AGI.
    f['capital_gains'] = f['kg_st'] + f['kg_lt'] + f['kg_dist']
    for tot, parts in M.META['computed_totals'].items():
        f[tot] = D.signed_sum(f, parts)
    f['other_agi'] = f['other_agi_remainder']
    bands = TH.reband(f)
    a = pd.DataFrame({'record_id': agg_raw['RECID'].astype(float).to_numpy(),
                      'weight': agg_raw['S006'].to_numpy(float) / 100})
    for v in slots:
        if v in M.CATEGORICAL or v in M.PUF_LABELS or v in LATE_SLOTS:
            continue
        a[M.column(v)] = bands[f'{v}_band'].to_numpy()
    for flag, parts in FLAGS.items():
        a[flag] = (f[parts].to_numpy(float) != 0).any(1).astype(np.int64)
    lab = a[[c for c in a.columns if c.endswith('_band')]].assign(filing_status=1)
    a[LABEL] = Q.puf_group(Q.band_agi(lab), Q.puf_groups()['cuts'])
    return pd.concat([a, pd.DataFrame({D.OWN + v: f[v].to_numpy(float) for v in own})], axis=1)


def real_puf_records():
    """The 2015 PUF's records with their bands, own 2015 amounts and weights, read once. 'ord' holds the ordinary
    records (the training table's PUF rows) and 'agg' the four aggregate records. The aggregate records have zero
    filing status, dependents and age on the PUF; the networks draw those."""
    if _REAL:
        return _REAL
    own = own_items()
    # The preset slots: every PUF slot but the filing flag (which generate sets), and the computed totals and flags.
    slots = [v for v in PUF_SLOTS if v != 'filer'] + list(COMPUTED) + list(FLAGS)
    o = ordinary_puf_records(slots, own)
    a = aggregate_puf_records(slots, own)
    _REAL.update({'ord': o, 'agg': a, 'own': own, 'slots': slots})
    common.say(f'real PUF: {len(o):,} ordinary records ({o.weight.sum() / 1e6:,.3f} million returns), '
               f'{len(a)} aggregate records ({a.weight.sum():,.2f} returns); {int((o.age_range < 0).sum()):,} ordinary '
               f'records with no age range (drawn from the age-range probabilities); {len(own)} own amounts per record')
    return _REAL


def drawn_from_probs(o, column, probs, salt):
    """The column's values on the real PUF rows o. Where the PUF has none (-1), a value is drawn from the row's
    probabilities (the columns probs), with one uniform number per record keyed by record id and salt. Returns the
    values and which rows were drawn."""
    x = o[column].to_numpy().astype(np.int64)
    miss = x < 0
    u = uniform_by_id(o.loc[miss, 'record_id'].to_numpy(), salt)
    cp = np.cumsum(o.loc[miss, probs].to_numpy(float), 1)
    x[miss] = np.minimum((cp < u[:, None]).sum(1), len(probs) - 1)
    return x, miss


def real_puf_part(model, mod, k, n_models, fac):
    """Network k's copy of the real PUF (one of n_models copies). Each ordinary record has every PUF slot preset; the
    four aggregate records have their amounts preset, and their filing status, dependents and adult age range are
    drawn. Every survey item is then drawn given the record. Each record's weight is its PUF weight divided by
    n_models. Returns the records, the label check's table and the rule counts."""
    from shared import dollars as D
    R = real_puf_records()
    o, a, own, slots = R['ord'], R['agg'], R['own'], R['slots']
    seed = int(G['label_seed']) + mod['seed_shift'] + REAL_SEED_SHIFT

    # 1. The ordinary records. Where the PUF has no age range or earnings split code, one is drawn from the record's
    #    probabilities, once for each record and copy. Check that the draw left every preset value as it was.
    preset = {v: torch.as_tensor(o[M.column(v)].to_numpy().astype(np.int64)) for v in slots}
    rng, miss = drawn_from_probs(o, 'age_range', M.AGE_PROBS, REAL_AGE_SALT + k)
    es, es_miss = drawn_from_probs(o, 'earn_split', M.ES_PROBS, REAL_EARN_SPLIT_SALT + k)
    preset['age_range'], preset['earn_split'] = torch.as_tensor(rng), torch.as_tensor(es)
    common.say(f"{mod['run']}: real PUF copy {k + 1}: earnings split code drawn from its probabilities on "
               f"{int(es_miss.sum()):,} joint returns with no demographics (codes {np.bincount(es[es_miss], minlength=len(M.ES_PROBS)).tolist()})")
    part, counts = generate(model, len(o), seed, preset=preset, factors=fac, totals_preset=True)
    for v in slots:
        c = M.column(v)
        # A gated share that is -1 in the preset was drawn; every other preset value must be unchanged.
        on = preset[v].numpy() >= 0 if P.index(v) in GATED else np.ones(len(part), bool)
        assert np.array_equal(part[c].to_numpy()[on], preset[v].numpy()[on]), f'real PUF: the draw moved {c}'
        if P.index(v) in GATED:
            n_drawn = int((preset[v].numpy() < 0).sum())
            n_set = int((part[M.column(v)].to_numpy()[preset[v].numpy() < 0] > 0).sum())
            common.say(f"{mod['run']}: real PUF copy {k + 1}: {v} not on the PUF for {n_drawn:,} records, which the "
                       f"generator's rule then asks or not; {n_set:,} of them drew a share above the first band")
    part['age_range_drawn'] = miss.astype(np.int64)
    part['puf_aggregate'] = 0

    # 2. The four aggregate records, with a seed of their own. The type, filing status, credit children and late
    #    slots are drawn; the age range is an adult one.
    drawn_a = set(TYPE_SLOTS) | set(STATUS_SLOTS) | set(LATE_SLOTS)
    pre_a = {v: torch.as_tensor(a[M.column(v)].to_numpy().astype(np.int64)) for v in slots if v not in drawn_a}
    part_a, counts_a = generate(model, len(a), seed + 7, preset=pre_a, factors=fac, totals_preset=True,
                                adult_only=True)
    part_a['age_range_drawn'] = 1
    part_a['puf_aggregate'] = 1
    counts = {r: counts[r] + counts_a[r] for r in counts}

    # 3. Weights, the PUF record id and copy, and the own 2015 amounts on both kinds of record; then the label check,
    #    the band-AGI group of each record's bands against its AGI group label.
    for p, src in [(part, o), (part_a, a)]:
        p['weight'] = src['weight'].to_numpy(float) / n_models
        p['puf_recid'] = src['record_id'].to_numpy().astype(np.int64)
        p['puf_copy'] = k + 1
    part, part_a = [pd.concat([p, src[[D.OWN + v for v in own]].reset_index(drop=True)], axis=1)
                    for p, src in [(part, o), (part_a, a)]]
    part = pd.concat([part, part_a], ignore_index=True)
    g = part[LABEL].to_numpy()
    qg = Q.puf_group(Q.band_agi(part), Q.puf_groups()['cuts'])
    part['q_group'], part['label_ok'] = qg, qg == g
    check = pd.DataFrame([{'model': mod['seed'], 'group': name, 'records drawn': int((g == j).sum()),
                           'band group differs (%)': 100 * float((qg[g == j] != j).mean()) if (g == j).any() else np.nan}
                          for j, name in enumerate(LABEL_META['labels'])])
    common.say(f"{mod['run']}: real PUF copy {k + 1}: {len(part):,} records; the aggregate records were drawn with "
               f"filing status {part_a['filing_status'].tolist()}, age range {part_a['age_range'].tolist()} and "
               f"dependents {part_a['n_dep'].tolist()}")
    return part, check, counts


# ---- The stack: both batches from every network ----

def store_picks(s, k, stream=4242):
    """Adds the stored picks: for each record and amount, one uniform number between 0 and 1 in u_<variable>. Every
    later step uses it to pick the record's dollars within its band, and a record always gets the same dollars. The
    numbers are seeded by the dollars seed, the stream (4242 for the stack, 4343 for Part A's candidates), the
    network's position k and the variable's position, and are the same in every run."""
    from shared import dollars as D
    seed = int(common.SETTINGS['dollars']['seed'])
    cols = {f'u_{v}': np.random.default_rng([seed, stream, k, i]).random(len(s), dtype=np.float32)
            for i, v in enumerate(D.ORDER) if f'{v}_band' in s}
    return pd.concat([s, pd.DataFrame(cols, index=s.index)], axis=1)


def nonfiler_records():
    """The non-filer batch's number of records: the filer batch's number of records times the CPS's expected
    non-filers divided by SOI's returns."""
    from reweight import target_list as TLS
    # The CPS's expected non-filers in the year are the sum over its units of weight x (1 - filer_p). In a year with
    # no CPS units, they are the geometric mean of the CPS years either side.
    nf, returns = TLS.nonfiler_count(common.TARGET_YEAR), TG.returns(TG.load(), common.TARGET_YEAR)
    n = int(round(LABEL_RECORDS * nf / returns))
    common.say(f'non-filer batch: {n:,} records = {LABEL_RECORDS:,} x {nf / 1e6:.2f}M CPS {common.TARGET_YEAR} expected '
               f'non-filers / {returns / 1e6:.2f}M SOI returns')
    return n


def load_models(device):
    """The pooled networks: for each, its run, the network, its training seed and the shift added to its generation
    seeds."""
    assert len({e['run'] for e in MODELS}) == len(MODELS) and len({e['seed_shift'] for e in MODELS}) == len(MODELS), \
        'each pooled model is a different run with its own seed shift'
    out = []
    for e in MODELS:
        folder = f"{common.OUT}/model/{e['run']}"
        seed = int(json.load(open(os.path.join(folder, 'train_meta.json')))['seed'])
        out.append({'run': e['run'], 'seed': seed, 'seed_shift': int(e['seed_shift']),
                    'network': M.load_model(device, os.path.join(folder, 'model.pt'))})
        common.say(f"model {e['run']} (training seed {seed}), generation seeds shifted by {e['seed_shift']}")
    return out


def equal_shares(n, k):
    """n records split over k networks as evenly as possible, the first ones taking any remainder."""
    return [n // k + (i < n % k) for i in range(k)]


def nonfiler_part(model, mod, fac, n_nf_k, n_nf, n_models, nf_years):
    """One network's share of the non-filer batch: n_nf_k records under the CPS label, filing flag 0, split across the
    CPS years of nf_years in their shares, with a seed for each CPS year. Each network carries an equal share of the
    batch's n_nf records. Returns the records and the rule counts."""
    nf_parts, counts = [], {r['name']: 0 for r in RULES}
    sizes = [int(round(n_nf_k * s)) for _, s in nf_years]
    sizes[-1] = n_nf_k - sum(sizes[:-1])
    for j, ((ny, _), size) in enumerate(zip(nf_years, sizes)):
        part_nf, c = generate(model, size, G['nonfiler_seed'] + mod['seed_shift'] + 100 * j, factors=fac, label='cps',
                              year=ny, filer=0)
        if len(nf_years) > 1:
            common.say(f"  non-filers drawn at CPS {ny}: {size:,} records")
        nf_parts.append(part_nf)
        counts = {r: counts[r] + c[r] for r in counts}
    nf = pd.concat(nf_parts, ignore_index=True)
    nf['weight'], nf['stratifier'] = n_nf / n_models / n_nf_k, 'nonfiler'
    nf['q_group'], nf['label_ok'] = -1, True          # a non-filer has no AGI group label
    return nf, counts


def assign_record_ids(s, k):
    """Record ids for network k's records: k x ID_BLOCK plus the row number. With the real PUF, a real record's id is
    made from its RECID and copy instead; the CRS package matches records by them."""
    assert len(s) < ID_BLOCK
    s['record_id'] = k * ID_BLOCK + np.arange(len(s))
    if REAL_PUF:
        r = s['puf_recid'].to_numpy() >= 0
        s.loc[r, 'record_id'] = REAL_ID + k * REAL_COPY_BLOCK + s.loc[r, 'puf_recid'].to_numpy()
        assert s['record_id'].is_unique and (s.loc[~r, 'record_id'] < REAL_ID).all()


def build_stack(models, factors):
    """Draws the filer batch and the non-filer batch, each in equal shares from every network, and stacks them.
    factors holds each network's offset factors, by run. Returns the stack and the label check for each network and
    AGI group. Also logs how many records each consistency rule applied to."""
    n_nf = nonfiler_records()
    nf_years = nonfiler_years()
    rule_counts = {r['name']: 0 for r in RULES}
    stacks, checks = [], []
    for k, (mod, n_nf_k) in enumerate(zip(models, equal_shares(n_nf, len(models)))):
        model, fac = mod['network'], factors[mod['run']]
        # 1. The filers: every real 2015 PUF record with its survey items drawn (real_puf), or records drawn with
        #    their AGI group set in advance. Each network carries an equal share of the filers' weight.
        if REAL_PUF:
            part, check, counts = real_puf_part(model, mod, k, len(models), fac)
        else:
            part, check, counts = label_part(model, mod, k, len(models), fac)
        part['weight'] *= LABEL_RECORDS / len(models) / part['weight'].sum()
        part['stratifier'] = 'label'
        checks.append(check)
        rule_counts = {r: rule_counts[r] + counts[r] for r in rule_counts}
        common.say(f"{mod['run']}: filers done")
        # 2. The non-filers.
        nf, counts = nonfiler_part(model, mod, fac, n_nf_k, n_nf, len(models), nf_years)
        rule_counts = {r: rule_counts[r] + counts[r] for r in rule_counts}
        common.say(f"{mod['run']}: nonfiler done")
        if REAL_PUF:
            nf['puf_recid'], nf['puf_copy'], nf['puf_aggregate'], nf['age_range_drawn'] = -1, -1, 0, 0
        # 3. Stack the two batches, give each record an id, and store its picks.
        s = pd.concat([part, nf], ignore_index=True)
        s['model'] = mod['seed']
        assign_record_ids(s, k)
        stacks.append(store_picks(s, k))
    stack = pd.concat(stacks, ignore_index=True)
    # 4. Check that each network carries its equal share of each batch's weight, and log the shares of the SCF waves
    #    and the survey years against the shares set in the year's configuration.
    by = stack[['model', 'weight']].assign(batch=np.where(stack['filer'] == 1, 'filers', 'non-filers'))
    share = by.groupby(['batch', 'model'])['weight'].sum() / by.groupby('batch')['weight'].sum()
    common.say('records and share of each batch\'s weight, by model:\n' + pd.DataFrame(
        {'records': by.groupby(['batch', 'model']).size(), 'weight share': share}).to_string())
    assert np.allclose(share.to_numpy(), 1 / len(models)), 'a model does not carry its equal share of a batch'
    w = stack.groupby('scf_wave')['weight'].sum() / stack['weight'].sum()
    common.say('SCF waves of the balance sheet, share of records and of the starting weight against the prior:\n'
               + pd.DataFrame({'records': stack.groupby('scf_wave').size(),
                               'share of records': stack.groupby('scf_wave').size() / len(stack),
                               'share of weight': w,
                               'prior': pd.Series({wv: s for wv, _, s in SCF_WAVES})}).to_string())
    for src, m in SURVEY_YEARS.items():
        cnt, ws = stack.groupby(f'{src}_year').size(), stack.groupby(f'{src}_year')['weight'].sum() / stack['weight'].sum()
        common.say(f'{src.upper()} years, share of records and of the starting weight against the prior:\n'
                   + pd.DataFrame({'records': cnt, 'share of records': cnt / len(stack), 'share of weight': ws,
                                   'prior': pd.Series(dict(m))}).to_string())
    common.say('consistency rules: records each applies to, over the whole stack:')
    common.say(pd.Series(rule_counts).to_string())
    return stack, pd.concat(checks, ignore_index=True)


# ---- The offset for the consistency rules ----
# The consistency rules force some records to hold an asset that the network would have drawn as zero. This adds
# holders. The offset scales down the odds of holding on the records no rule forced, until the expected number of
# holders equals the network's own.

def held(m, f):
    """The chance of holding when the network's chance m has its odds multiplied by f."""
    return f * m / (f * m + (1 - m))


def offset(stack, factors, run):
    """For each ruled holding: the network's own expected holders, the expected holders with the rules, and the
    factor on the odds of the records no rule forced that brings the second back to the first. The factor is found by
    bisection on its log. Returns the table and the new factors."""
    w = stack['weight'].to_numpy(float)
    rows, new = [], {}
    for v in RULED:
        m, forced = stack[f'_own_{v}'].to_numpy(float), stack[f'_forced_{v}'].to_numpy(bool)
        f0 = factors.get(v, 1.0)
        target = (w * m).sum()
        expected = (w * np.where(forced, 1.0, held(m, f0))).sum()
        fixed, rest = w[forced].sum(), ~forced
        lo, hi = -30.0, 30.0
        for _ in range(100):
            mid = (lo + hi) / 2
            if fixed + (w[rest] * held(m[rest], np.exp(mid))).sum() > target:
                hi = mid
            else:
                lo = mid
        new[v] = float(np.exp((lo + hi) / 2))
        rows.append({'run': run, 'holding': v, "model's own expected holders": target,
                     'expected holders with the rules': expected, 'excess (%)': 100 * (expected / target - 1),
                     'forced draws (weighted)': fixed,
                     'forced draws the model would have made zero (weighted)': (w[forced] * (1 - m[forced])).sum(),
                     'factor used': f0, 'next factor': new[v]})
    return pd.DataFrame(rows), new


def main():
    # 1. Load the pooled networks and log the survey years and SCF waves the records will be drawn at.
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(common.SEED)
    models = load_models(device)
    os.makedirs(common.GEN, exist_ok=True)
    common.say('survey years: ' + '; '.join(f"{src} " + ', '.join(f'{y} {100 * s:.2f}%' for y, s in m)
                                           for src, m in SURVEY_YEARS.items()))
    common.say('SCF waves of the balance sheet: ' + ', '.join(f'{w} wave (income year {y}) {100 * s:.2f}%'
                                                              for w, y, s in SCF_WAVES))

    # 2. Draw the whole stack offset_generations times. The first run uses no offset factors. After each run but the
    #    last, each network's factors are recomputed from its own records and used in the next run. The last run's
    #    stack and label check are kept.
    factors, report = {m['run']: {} for m in models}, []
    runs = int(C['offset_generations'])
    for run in range(1, runs + 1):
        stack, check = build_stack(models, factors)
        check.insert(0, 'run', run)
        check.to_csv(os.path.join(common.GEN, 'label_check.csv'), index=False)
        nxt = {}
        for m in models:
            table, nxt[m['run']] = offset(stack[stack['model'] == m['seed']], factors[m['run']], run)
            table.insert(1, 'model', m['seed'])
            report.append(table)
            common.say(f"run {run} of {runs}, model {m['run']}:\n" + table.drop(columns=['run', 'model']).to_string(index=False))
        if run < runs:
            factors = nxt

    # 3. Write the offset report and the stack, without the offset's working columns.
    pd.concat(report).to_csv(os.path.join(common.GEN, 'consistency.csv'), index=False)
    stack.drop(columns=[c for c in stack if c.startswith(('_own_', '_forced_'))]) \
        .to_parquet(os.path.join(common.GEN, 'stack.parquet'), index=False)


if __name__ == '__main__':
    main()
