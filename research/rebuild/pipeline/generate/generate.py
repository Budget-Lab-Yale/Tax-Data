"""Generation: PUF-style records drawn from the trained networks, written to gen/<run>/stack.parquet.

Two batches are drawn in equal shares from each pooled network: filers under the PUF label, each with its AGI group
preset and weighted by 1 / q of the group, and non-filers under the CPS label at the target year. Each record is drawn
slot by slot in phases: its own PUF items, the computed totals, the exact ages, then the SCF, CEX, CPS and SIPP items,
each under that survey's label. Generation runs several times so the consistency rules' extra holders can be offset.
FEATURES.md, "3. Generation", has the method.
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


COMPUTED = M.COMPUTED                     # {total: {part: sign}}
FLAGS = M.COMPUTED_FLAGS                  # {flag: parts}
PUF_SLOTS = [v for v in P if 'puf' in M.PRESENT_ON[v] and v not in COMPUTED and v not in FLAGS]
# drawn after the PUF items and their totals, in this order, under the batch's label
LATE_SLOTS = M.PUF_SHARES + list(M.SHARED_CATEGORIES)
# the QBI items under the SCF label: each group's outside workers are drawn after its industry group and are zero with
# none; the industry group is never none on a record with any of the group's PUF parts nonzero
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
SIPP_FLAGS = list(SIPP_STRUCTURE['lh_of'])                    # drawn after the tips
SIPP_TIPS = [v for v in SIPP_SLOTS if v not in SIPP_FLAGS]
AGE_SLOTS = M.EXACT_AGES                   # drawn after the PUF items, under the CPS label
SPOUSE = slots_of(CPS_STRUCTURE['spouse'])
# each adult's overtime: drawn after the other CPS slots, so the spouse's share of wages is in place
CPS_OVERTIME = CPS_STRUCTURE['overtime']
CPS_FIRST = [v for v in CPS_SLOTS if v not in CPS_OVERTIME]
CPS_ZERO_WITHOUT = CPS_STRUCTURE['zero_without']
DEP_AGES = [P.index(v) for v in CPS_STRUCTURE['dependent_ages']]          # the k-th: present when n_dep >= k
# the dependents' Social Security number status: the k-th present when n_dep >= k
DEP_SSN = [P.index(v) for v in CPS_STRUCTURE['dependent_ssn']]
PRIMARY_SSN = [P.index(v) for v in CPS_STRUCTURE['primary_ssn']]        # the primary filer's: never none
PRIMARY_AGE = P.index(CPS_STRUCTURE['primary_age'])
SEX_PRIMARY = P.index('sex_primary')
# the spouse's exact age (value 0 none, value a + 1 age a). A spouse, and a non-filer unit's head, is 18 or over
SPOUSE_AGE = P.index(next(v for v in CPS_STRUCTURE['spouse'] if v in CPS_STRUCTURE['ages']))
ADULT_AGE = 18
CPS_CATEGORICAL = slots_of(M.PERSON_CATEGORIES)
# {gated share's slot index: [its bases' indices]}. A share is asked only of a married couple with some of a base.
GATED = {s: list(bases) for s, (_, bases) in M.SHARE_GATES.items()}
ES_SLOT, WAGE_SHARE, SE_SHARE = P.index('earn_split'), P.index('spouse_wage_share'), P.index('spouse_se_share')
# the primary filer's age range of each age value (value 0 is none, value a + 1 is age a)
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
RULED = sorted({v for r in RULES for v in r['then_at_least_one_nonzero']}, key=P.index)   # the offset's holdings
DEPENDENT_RANGES = torch.tensor(common.DEPENDENT_RANGES)
SURVEY_ONLY = slots_of(AGE_SLOTS + SCF_SLOTS + CEX_SLOTS + CPS_SLOTS + SIPP_SLOTS)
CPS_VIEW = slots_of(AGE_SLOTS + CPS_SLOTS + [r for r in M.REPORTED if 'cps' in M.PRESENT_ON[r]])   # present on a CPS row
TOTALS = slots_of(list(COMPUTED) + list(FLAGS))
GATED_SLOTS = torch.tensor(list(GATED), dtype=torch.long)
POINTS = 32
TYPE_SLOTS = M.TYPE_SLOTS                  # drawn first, in a random order per record
STATUS_SLOTS = list(M.TAX_STATUS)          # then these, in this order (mars, n_dep_ctc, n_dep_eitc)
MARS, MARRIED, AGE_RANGE = P.index('filing_status'), P.index('married'), P.index('age_range')
# The pooled networks: each a run under OUT/model and the shift added to its generation seeds.
MODELS = G['models']
LABEL_RECORDS = int(G['label_records'])     # the filer batch's records over all models
ID_BLOCK = 10_000_000                       # model k's record ids start at k x ID_BLOCK
REAL_PUF = common.REAL_PUF                 # settings generation.filers 'real_puf': real_puf_part in place of the filer batch
# The filer batch presets the AGI group label on every record.
LABEL = 'agi_group'
assert M.PUF_LABELS == [LABEL], 'the label design needs the AGI group label in the table (build_table.agi_group_label)'
LABEL_META = M.META['labels'][LABEL]


def nonfiler_years():
    """[(CPS year, share)] for the non-filer batch: the target year alone when the CPS has it, otherwise the CPS
    years either side, half each."""
    from shared import survey_mix
    from reweight import target_list as TLS
    if common.TARGET_YEAR in TLS.cps_years():
        return [(common.TARGET_YEAR, 1.0)]
    return survey_mix.mix('cps')


# ---- the SCF waves -----------------------------------------------------------------------------
def scf_waves():
    """The waves the balance sheet is drawn from, from the target year's configuration (prior, "balance sheet"):
    [(wave survey year, income year, share)], by survey year."""
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
    """Each of n records' wave (an index into SCF_WAVES), drawn with the shares by a generator of its own."""
    u = np.random.default_rng([int(G['scf_wave_seed']), int(seed)]).random(n)
    k = np.searchsorted(np.cumsum([s for _, _, s in SCF_WAVES]), u, side='right')
    return np.minimum(k, len(SCF_WAVES) - 1)


# ---- the CEX, CPS and SIPP years ----------------------------------------------------------------
def survey_years():
    """{survey: [(tax year, share)]} for the CEX, CPS and SIPP, from the target year's configuration. Every year must
    be one the training table holds for that survey."""
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
    """Each of n records' year of the survey src (an index into SURVEY_YEARS[src]), drawn with the shares by a generator
    of its own for that survey (settings generation.survey_year_seeds), so no other draw moves."""
    u = np.random.default_rng([int(G['survey_year_seeds'][src]), int(seed)]).random(n)
    k = np.searchsorted(np.cumsum([s for _, s in SURVEY_YEARS[src]]), u, side='right')
    return np.minimum(k, len(SURVEY_YEARS[src]) - 1)


# ---- computed totals ---------------------------------------------------------
_points = {}


def part_points(total, device):
    """[bands, 32] dollar points per band of each part of a total (weighted quantiles of the PUF records in the band),
    and the total's cut points."""
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
    """The band of a computed total: each part read as one of its band's dollar points at random, and the signed sum
    placed on the total's PUF cut points."""
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


# ---- rules on a draw -------------------------------------------------------------
def scf_order(size, gen):
    """A random order of the SCF-only slots per record, each ratio slot moved after its base."""
    scf = slots_of(SCF_SLOTS)
    perm = scf[torch.rand(size, len(scf), generator=gen).argsort(1)]
    rows = torch.arange(size)
    pairs = [(P.index(slot), P.index(base)) for slot, (_, base) in M.RATIOS.items() if slot in SCF_SLOTS]
    # (the CPS's ratios have a PUF base, drawn earlier); each QBI group's workers after its industry group
    for i, ib in pairs + list(SCF_AFTER.items()):
        pos, pos_b = (perm == i).int().argmax(1), (perm == ib).int().argmax(1)
        swap = pos < pos_b
        perm[rows[swap], pos[swap]], perm[rows[swap], pos_b[swap]] = ib, i
    # the QBI items after every other SCF slot, keeping their order among themselves: qbi_structure gives an industry
    # group to every record with the group's PUF parts nonzero, while in the SCF only business owners have one, so drawn
    # earlier an industry would make the record an owner
    qbi = torch.tensor(sorted(set(INDUSTRY_PARTS) | set(SCF_AFTER)))
    key = torch.arange(perm.shape[1]).expand(size, -1) + perm.shape[1] * torch.isin(perm, qbi).long()
    perm = torch.gather(perm, 1, key.argsort(1))
    return perm


def dependent(values):
    """Records whose primary filer is in a dependent-filer age range."""
    return torch.isin(values[:, NC + P.index('age_range')], DEPENDENT_RANGES.to(values.device))


def only(probs, rows, allowed=None):
    """On rows, the distribution cut to the allowed values ([rows, choices] booleans) and rescaled; with allowed None,
    value 0 for certain. If every allowed value has no chance, they are taken as equally likely."""
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
    """On rows, value 0 (none) ruled out and the other values rescaled."""
    allowed = torch.ones(int(rows.sum()), probs.shape[1], device=probs.device)
    allowed[:, 0] = 0.0
    return only(probs, rows, allowed)


def cps_structure(values, slot, probs):
    """The CPS's person items follow the record's PUF items: the spouse's items are none or zero unless married, and a
    married spouse has a sex and an age of 18 or over; the primary filer's sex and Social Security status are never
    none; the k-th dependent has an age and a status exactly when there are at least k dependents. The primary filer's
    exact age lies in its age range, and a non-filer head is 18 or over, since the non-filer targets count adults from
    18 and children under 17."""
    married = values[:, NC + P.index('married')] == 1
    n_dep = values[:, NC + P.index('n_dep')]
    on_spouse = torch.isin(slot, SPOUSE.to(slot.device))
    categorical = torch.isin(slot, CPS_CATEGORICAL.to(slot.device))
    probs = only(probs, on_spouse & ~married)
    probs = not_none(probs, on_spouse & married & categorical)
    probs = not_none(probs, slot == SEX_PRIMARY)    # the primary filer's sex is a man or a woman, never none
    sel = (slot == SPOUSE_AGE) & married
    adult = torch.zeros(probs.shape[1], device=probs.device)
    adult[ADULT_AGE + 1:M.PERSON_CATEGORIES[P[SPOUSE_AGE]]] = 1.0   # value a + 1 is age a
    probs = only(probs, sel, adult.expand(int(sel.sum()), -1))
    for k, j in list(enumerate(DEP_AGES, start=1)) + list(enumerate(DEP_SSN, start=1)):
        probs = only(probs, (slot == j) & (n_dep < k))
        probs = not_none(probs, (slot == j) & (n_dep >= k))
    for j in PRIMARY_SSN:
        probs = not_none(probs, slot == j)
    # overtime is zero without wages (the spouse's without a share of the couple's wages above zero); dollars.py then
    # holds it to the person's wages less tips
    for item, base in CPS_ZERO_WITHOUT.items():
        probs = only(probs, (slot == P.index(item)) & (values[:, NC + P.index(base)] <= 0))
    sel = slot == PRIMARY_AGE
    if sel.any():
        rng = RANGE_OF_AGE_VALUE.to(probs.device)
        allowed = torch.zeros(int(sel.sum()), probs.shape[1], device=probs.device)
        allowed[:, :len(rng)] = (rng[None, :] == values[sel, NC + P.index('age_range')][:, None]).float()
        nonfiler = values[sel, NC + P.index('filer')] == 0
        allowed[nonfiler, :ADULT_AGE + 1] = 0.0                       # value a + 1 is age a; value 0 is none
        probs = only(probs, sel, allowed)
    return probs


def positive(values, v):
    """Whether amount slot v is drawn above zero (a positive band)."""
    return values[:, NC + P.index(v)] >= M.BAND_LAYOUT[v]['first_positive']


def earn_structure(values, slot, probs):
    """The earnings split code: 0 unless married; on a married couple, 1-3 where it has wages (the PUF has code 0 with
    earnings on only 0.02% of joint returns' weight) and 0 where it has neither wages nor positive self-employment
    income; otherwise as the model draws."""
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
    """Schedule SE net earnings from self-employment income x (a tensor): 92.35% of it where that is $400 or more."""
    e = float(M.META['earn_split']['se_factor']) * x
    return torch.where(e >= float(M.META['earn_split']['se_floor']), e, torch.zeros_like(e))


def capped_wage_share_range(W, e1, e2, a, b, cap):
    """The spouse's wage shares (low, high) that put the primary filer's share of earnings in [a, b], self-employment
    earnings counted up to the room under the wage base. The primary's share falls as the spouse's wage share rises,
    so each end is found by halving. Where no share reaches the range, the nearest share, 0 or 1."""
    def primary_share(s):
        w1, w2 = W * (1 - s), W * s
        p1 = w1 + torch.minimum(e1, (cap - w1).clamp(min=0))
        p2 = w2 + torch.minimum(e2, (cap - w2).clamp(min=0))
        return p1 / (p1 + p2).clamp(min=1e-9)
    s0, s1 = torch.zeros_like(W), torch.ones_like(W)
    t0, t1 = s0.clone(), s1.clone()
    for _ in range(40):
        m = (s0 + s1) / 2
        ok = primary_share(m) <= b                      # the smallest share at or below the top of the range
        s1, s0 = torch.where(ok, m, s1), torch.where(ok, s0, m)
        m = (t0 + t1) / 2
        ok = primary_share(m) >= a                      # the largest share at or above the bottom of the range
        t0, t1 = torch.where(ok, m, t0), torch.where(ok, t1, m)
    lo = torch.where(primary_share(torch.zeros_like(W)) <= b, torch.zeros_like(W), s1)
    hi = torch.where(primary_share(torch.ones_like(W)) >= a, torch.ones_like(W), t0)
    return lo, hi


def band_tables(device):
    """For wage_share_bands: each band's median PUF-year dollars (an empty band: the middle of its cut points) of
    wages, the self-employment share and its bases; the earnings split codes' ranges of the primary filer's share; and,
    per year, each band's range of the spouse's wage share on that CPS year's cut points."""
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
    """The SIPP's items follow the record's: the spouse's tips and flag are zero unless married; the primary filer's
    tips are zero without wages, the spouse's without a share of wages above zero; a leisure and hospitality flag is 0
    where its tips are zero."""
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
    """Elective deferrals are zero without wages (dollars.py then holds each person's to their wages and the legal
    limit), and the spouse's share of them is zero unless married."""
    probs = only(probs, (slot == P.index('elective_deferrals')) & (values[:, NC + P.index('wages')] <= 0))
    return only(probs, (slot == P.index('spouse_deferral_share')) & (values[:, NC + MARRIED] != 1))


def qbi_structure(values, slot, probs):
    """The QBI items: a group's outside workers are zero where its industry group is none, and its industry group is
    not none where any of the group's PUF parts is nonzero."""
    for j, ind in SCF_AFTER.items():
        probs = only(probs, (slot == j) & (values[:, NC + ind] == 0))
    for j, parts in INDUSTRY_PARTS.items():
        sel = slot == j
        if not bool(sel.any()):
            continue
        probs = not_none(probs, (values[:, NC + parts.to(values.device)] != 0).any(1) & sel)
    return probs


def structural(values, slot, probs):
    """Every rule on a draw: a ratio slot whose base is zero is zero, then the rules above. A dependent filer holds no
    survey-only item, because the surveys count it in the household that claims it."""
    for slot_name, (_, base) in M.RATIOS.items():
        if P.index(slot_name) in GATED:     # a gated share: zero where not asked (Chunk.not_asked), not by one base
            continue
        probs = only(probs, (slot == P.index(slot_name)) & (values[:, NC + P.index(base)] == 0))
    probs = earn_structure(values, slot, probs)
    probs = cps_structure(values, slot, probs)
    probs = sipp_structure(values, slot, probs)
    probs = deferral_structure(values, slot, probs)
    probs = qbi_structure(values, slot, probs)
    return only(probs, dependent(values) & torch.isin(slot, SURVEY_ONLY.to(values.device)))


def scale_odds(probs, slot, factors):
    """On the records drawing a ruled holding, its odds of being nonzero multiplied by its factor."""
    for v, f in factors.items():
        if f != 1.0:
            s = probs.clone()
            s[:, 1:] *= f
            s /= s.sum(-1, keepdim=True)
            probs = torch.where((slot == P.index(v))[:, None], s, probs)
    return probs


def apply_must(slot, probs, must):
    """The current slot's distribution with zero ruled out where a consistency rule says the record must hold it, and
    which rows that is."""
    rows = torch.arange(len(slot), device=slot.device)
    q = probs.clone()
    q[:, 0] = 0
    ok = must[rows, slot] & (q.sum(1) > 1e-12)
    return torch.where(ok[:, None], q / q.sum(1, keepdim=True).clamp_min(1e-30), probs), ok


# ---- one chunk of records ----------------------------------------------------------
class Chunk:
    """size records being drawn: every slot's band (-1 absent), which slots are drawn, and the consistency rules'
    bookkeeping. scf_year is each record's index into model.YEARS for its SCF phase (its wave's income year);
    survey_year the same for the CEX, CPS and SIPP phases."""

    def __init__(self, model, size, order, preset, start, factors, gen, scf_year, survey_year, label='puf', year=None,
                 adult_only=False):
        device = order.device
        self.adult_only = adult_only                                 # an adult age range under any label (real PUF)
        self.model, self.order, self.factors, self.gen = model, order, factors, gen
        self.records = None   # the redraw (top_billionaires.redraw_records): [(rows, generator)] per record; see slot_probs
        self.scf_year = scf_year.to(device)
        self.survey_year = {src: y.to(device) for src, y in survey_year.items()}   # per survey: each record's year index
        self.label = label                                           # the batch's label for its own items
        self.rows = torch.arange(size, device=device)
        self.values = torch.full((size, len(M.SLOTS)), -1, dtype=torch.long, device=device)
        self.values[:, 0] = M.SOURCES.index(label)
        self.values[:, 1] = M.YEARS.index(common.PUF_YEAR if year is None else year)
        self.values[self.rows[:, None], NC + order] = 0              # present, not yet drawn
        self.values[:, NC + SURVEY_ONLY.to(device)] = -1             # absent until their turn
        if label == 'cps':        # the CPS's own and reported slots: present and hidden, as on a CPS training row
            self.values[:, NC + CPS_VIEW.to(device)] = 0
        self.values[:, NC + GATED_SLOTS.to(device)] = -1             # a share is absent until married and its bases are drawn (gate_shares, after_totals)
        self.values[:, NC + TOTALS.to(device)] = 0                   # present and hidden until computed
        self.filled = torch.zeros(size, len(P), dtype=torch.bool, device=device)
        for var, bands in preset.items():
            self.values[:, NC + P.index(var)] = bands[start:start + size].to(device)
            self.filled[:, P.index(var)] = True
            if P.index(var) in GATED:
                # (real PUF) a gated share preset on some records only: -1 means no preset, and those records are
                # asked or not by the generator's rule; a preset share stands
                self.filled[:, P.index(var)] = bands[start:start + size].to(device) >= 0
        self.triggered = torch.zeros(size, len(RULES), dtype=torch.bool, device=device)
        self.resolved = self.triggered.clone()
        self.must = torch.zeros(size, len(P), dtype=torch.bool, device=device)
        self.own_chance = torch.zeros(size, len(RULED), dtype=torch.float64, device=device)
        self.forced = torch.zeros(size, len(RULED), dtype=torch.bool, device=device)
        self.ruled = torch.tensor([P.index(v) for v in RULED], device=device)
        self.column = {}                                             # per source: each of SLOTS' column, -1 if absent
        for src, slots in M.SOURCE_SLOTS.items():
            self.column[src] = torch.full((len(M.SLOTS),), -1, dtype=torch.long, device=device)
            self.column[src][list(slots)] = torch.arange(len(slots), device=device)
        self.not_asked = {}
        self.bounded = 0                                             # wage shares drawn inside an earnings split range
        self.gate_shares()

    def gate_shares(self):
        """Each share not yet drawn is present (hidden) once married and its bases are drawn and the unit is married
        with some of a base; absent before that and where not asked. In the filer batch the self-employment share's base
        Schedules C and F is a computed total, so the share stays absent until the totals."""
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
        """Full filing status is joint exactly when married (separate returns are not married, as in the data); under
        the CPS label, or with adult_only, the age range is an adult one."""
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
        """The network on the source's slots."""
        slots = M.SOURCE_SLOTS[src]
        return self.model.last_layer(*M.pack(values, hidden, slots), slots)

    def slot_probs(self, h, slot, groups=None):
        """Each row's distribution of its slot from its last-layer vector. With self.records (the redraw), computed on
        each record's own rows, because the output layers' last digits depend on how many rows they run on; a record's
        tries then come out as when it is drawn alone. groups: per record, the rows of each distinct slot."""
        if self.records is None:
            return self.model.slot_probs(h, slot)
        if groups is None:                 # one slot on every row (a ruled holding): each record's rows are one group
            same = torch.arange(len(self.records[0][0]), device=h.device)
            groups = [[(int(slot[0]), same)] for _ in self.records]
        return torch.cat([self.model.slot_probs(h[rows], slot[rows], groups[k]) for k, (rows, _) in enumerate(self.records)])

    def take(self, probs, slot):
        """Draw slot from probs, and record it."""
        if self.records is None:
            draw = torch.multinomial(probs, 1, generator=self.gen).squeeze(1)
        else:
            draw = torch.cat([torch.multinomial(probs[rows], 1, generator=gen).squeeze(1) for rows, gen in self.records])
        self.values[self.rows, NC + slot] = draw
        self.filled[self.rows, slot] = True

    def force_picks(self, slot, nonzero):
        """The consistency rules' picks: a record that first reaches one of a triggered rule's holdings is given one
        holding it must hold, chosen in proportion to nonzero, the model's own chance that each is nonzero before the
        offset's factors, so the pick does not depend on the factors."""
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
        """One of the batch's own items per record (a PUF variable), under the batch's label, given the slots drawn
        so far."""
        hidden = (self.values[:, NC:] >= 0) & ~self.filled
        h = self.last_layer(self.values, hidden, self.label)
        probs = self.model.slot_probs(h[self.rows, self.column[self.label][NC + slot]], slot)
        self.take(self.own_rules(slot, probs), slot)
        self.gate_shares()

    def compute_totals(self):
        """The computed totals and flags from their parts; which rules each record triggers (none for a
        dependent filer, which holds no survey item); the survey slots made present. Returns the rule counts."""
        v = self.values
        for tot in COMPUTED:
            v[:, NC + P.index(tot)] = computed_band(v, self.gen, tot)
            self.filled[:, P.index(tot)] = True
        for flag, flag_parts in FLAGS.items():
            v[:, NC + P.index(flag)] = torch.stack([v[:, NC + P.index(p)] != 0 for p in flag_parts], 1).any(1).long()
            self.filled[:, P.index(flag)] = True
        return self.after_totals()

    def after_totals(self):
        """compute_totals' bookkeeping, which depends only on the values drawn so far: the rules each record triggers,
        the survey slots made present, the CPS shares' records with no question. Returns the rule counts."""
        v = self.values
        dep = dependent(v)
        for r in range(len(RULES)):
            self.triggered[:, r] = (v[:, NC + RULE_ITEMS[r].to(v.device)] != 0).any(1) & ~dep
        v[:, NC + SURVEY_ONLY.to(v.device)] = 0                      # each survey's view leaves out the others'
        v[:, NC + REP_SLOTS.to(v.device)] = 0                        # reported slots: present, never drawn (hidden)
        married = v[:, NC + P.index('married')] == 1
        self.not_asked = {}                                         # the shares' records with no question
        for j, bases in GATED.items():
            some = torch.zeros_like(married)
            for b in bases:
                some |= v[:, NC + b] > 0
            self.not_asked[j] = (~married | ~some | dep) & ~self.filled[:, j]   # a preset share stands
            v[~self.not_asked[j] & ~self.filled[:, j], NC + j] = 0   # asked, not yet drawn: present and hidden
            v[self.not_asked[j], NC + j] = -1
        return {r['name']: int(self.triggered[:, k].sum()) for k, r in enumerate(RULES)}

    def survey_step(self, slot, label, year=None, groups=None):
        """One survey-only slot per record, as the survey label would see the record, under the rules. groups: with
        self.records, per record the rows of each distinct slot (slot_probs)."""
        hidden = (self.values[:, NC:] >= 0) & ~self.filled
        view, view_hidden = M.seen_as(self.values, hidden, label, year)
        h, col = self.last_layer(view, view_hidden, label), self.column[label]
        slot_p = self.slot_probs(h[self.rows, col[NC + slot]], slot, groups)
        # each ruled holding's chance of being nonzero, the model's own (no rule applies in the CEX and CPS phases)
        own_nonzero = torch.zeros(len(slot), len(P), device=slot.device)
        for v in (RULED if label == 'scf' else []):
            j = P.index(v)
            own_nonzero[:, j] = 1 - self.slot_probs(h[:, col[NC + j]], torch.full_like(slot, j))[:, 0]
        del h
        own_probs = structural(self.values, slot, slot_p.clone())   # the model's own, before rules
        probs = structural(self.values, slot, scale_odds(slot_p.clone(), slot, self.factors))
        self.force_picks(slot, own_nonzero)
        probs, ok = apply_must(slot, probs, self.must)
        k = (slot[:, None] == self.ruled[None, :])                  # rows whose slot is a ruled holding
        hit = k.any(1)
        col = k.int().argmax(1)
        self.own_chance[self.rows[hit], col[hit]] = 1 - own_probs[hit, 0].double()
        self.forced[self.rows[hit], col[hit]] = ok[hit]
        for j, m in self.not_asked.items():                         # no draw where the share is not asked
            probs = only(probs, m & (slot == j))
        if label == 'cps':
            probs = self.wage_share_bands(slot, probs)
        # a slot preset on some records only (a real PUF record's share) keeps its preset value
        keep = self.filled[self.rows, slot]
        before = self.values[self.rows, NC + slot].clone()
        self.take(probs, slot)
        if bool(keep.any()):
            self.values[self.rows[keep], NC + slot[keep]] = before[keep]
        for j, m in self.not_asked.items():
            self.values[m, NC + j] = -1

    def wage_share_bands(self, slot, probs):
        """The spouse's share of the couple's wages, in the CPS phase: on a married couple with wages and an earnings
        split code of 1-3, only the bands of the record's CPS year that can put the primary filer's share of earnings in
        the code's range, with the couple's amounts at their bands' median PUF-year dollars and earnings counted up to
        the PUF year's wage base. dollars.py holds the exact share inside the exact range."""
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
        blo, bhi = B['wage_lo'][yi], B['wage_hi'][yi]                # [n, bands]: each band's share range, that year
        allowed = torch.zeros(len(lo), probs.shape[1], device=probs.device)   # (the choices past the share's bands: 0)
        allowed[:, :blo.shape[1]] = ((bhi >= lo[:, None] - 1e-9) & (blo <= hi[:, None] + 1e-9)).float()
        none = allowed.sum(1) == 0
        allowed[none] = 1.0                                          # (not expected: the bands cover 0 to 1)
        self.bounded += int(sel.sum())
        return only(probs, sel, allowed)

    def finish(self):
        """The CPS shares not asked of a record: zero (no spouse, or none of the item, or a dependent filer). Called
        after the SIPP phase, which sees them absent where not asked, as on the SIPP's rows."""
        for j, m in self.not_asked.items():
            self.values[m, NC + j] = 0


@torch.no_grad()
def generate(model, n, seed, preset=None, factors=None, chunk=50_000, label='puf', year=None, filer=1,
             totals_preset=False, adult_only=False):
    """n PUF-style records, and how many records each consistency rule applies to. label and year are the batch's
    label and year for its own items; filer is its filing flag. preset: {variable: bands, one per record}, set before
    anything is drawn. factors: {ruled holding: factor on its odds of being nonzero}. For each ruled holding the table
    carries the model's own chance it is nonzero (_own_<v>) and whether a rule forced it (_forced_<v>). totals_preset:
    the computed totals and flags are preset (a real PUF record's own bands) and kept. adult_only: an age range drawn
    under the PUF label is an adult one (the PUF's aggregate records)."""
    preset, factors = dict(preset or {}), factors or {}
    if totals_preset:
        missing = [v for v in list(COMPUTED) + list(FLAGS) if v not in preset]
        assert not missing, f'totals_preset: the computed totals and flags must be preset, missing {missing}'
    assert label != 'puf' or all(v in preset for v in M.PUF_LABELS), \
        'the filer batch presets the AGI group label (the network never sees it hidden on a PUF record)'
    preset['filer'] = torch.full((n,), filer, dtype=torch.long)
    if label != 'puf':
        # a PUF amount the batch's source does not measure (interest on the CPS) is zero on every
        # record of the batch, nothing drawn for it; a category must be there
        lacking = [v for v in PUF_SLOTS if label not in M.PRESENT_ON[v] and v not in preset and v not in M.PUF_LABELS]
        assert all(v in M.AMOUNTS for v in lacking), f'the {label} label lacks PUF categories: {lacking}'
        for v in lacking:
            preset[v] = torch.zeros(n, dtype=torch.long)
        if lacking:
            common.say(f'  {label} batch: zero on every record, not measured by the {label}: {lacking}')
    device = next(model.parameters()).device
    cpu_gen = torch.Generator().manual_seed(seed)                  # the orders
    gpu_gen = torch.Generator(device=device).manual_seed(seed)     # the draws
    wave_k = assign_waves(n, seed)                                 # each record's SCF wave
    scf_year = torch.as_tensor(SCF_YEAR_INDEX[wave_k])
    svy_k = {src: assign_years(src, n, seed) for src in SURVEY_YEARS}      # each record's CEX, CPS and SIPP year
    svy_year = {src: torch.as_tensor(SURVEY_YEAR_INDEX[src][k]) for src, k in svy_k.items()}
    first = slots_of([v for v in TYPE_SLOTS if v not in preset])
    then = slots_of([v for v in STATUS_SLOTS if v not in preset])
    rest = slots_of([v for v in PUF_SLOTS if v not in preset and v not in TYPE_SLOTS
                     and v not in STATUS_SLOTS and v not in M.PUF_LABELS and v not in LATE_SLOTS])
    n_own = len(first) + len(then) + len(rest)
    # the late slots; a gated share preset on some records only is drawn here for the others
    late = slots_of([v for v in LATE_SLOTS if v not in preset or P.index(v) in GATED])
    rule_counts = {r['name']: 0 for r in RULES}

    def orders(size):
        """Each record's slot order: the phases in sequence, the slots within most phases in a random order per
        record."""
        shuffled = lambda x: x[torch.rand(size, len(x), generator=cpu_gen).argsort(1)]
        return torch.cat([shuffled(first), then.expand(size, -1), shuffled(rest), late.expand(size, -1),
                          shuffled(slots_of(AGE_SLOTS)), scf_order(size, cpu_gen), shuffled(slots_of(CEX_SLOTS)),
                          shuffled(slots_of(CPS_FIRST)), shuffled(slots_of(CPS_OVERTIME)),
                          shuffled(slots_of(SIPP_TIPS)), shuffled(slots_of(SIPP_FLAGS))], 1).to(device)

    def finish(d, order):
        """The late slots and every survey phase, on a chunk whose own items and totals are drawn: the self-employment
        share and the earnings split code under the batch's label, the exact ages under the CPS label, then each
        survey's slots under its label, at the record's year of that survey (the SCF at its wave's income year)."""
        k = n_own
        for count, label, year in [(len(late), d.label, None), (len(AGE_SLOTS), 'cps', d.survey_year['cps']),
                                   (len(SCF_SLOTS), 'scf', d.scf_year), (len(CEX_SLOTS), 'cex', d.survey_year['cex']),
                                   (len(CPS_SLOTS), 'cps', d.survey_year['cps']), (len(SIPP_SLOTS), 'sipp', d.survey_year['sipp'])]:
            for step in range(k, k + count):
                d.survey_step(order[:, step], label, year)
            k += count
        assert k == order.shape[1]
        d.finish()
        return d.values.cpu(), d.own_chance.cpu(), d.forced.cpu()

    parts = []
    for start in range(0, n, chunk):           # chunk by chunk
        size = min(chunk, n - start)
        order = orders(size)
        d = Chunk(model, size, order, preset, start, factors, gpu_gen, scf_year[start:start + size],
                  {src: y[start:start + size] for src, y in svy_year.items()}, label, year, adult_only)
        for step in range(n_own):
            d.puf_step(order[:, step])
        for name, count in (d.after_totals() if totals_preset else d.compute_totals()).items():
            rule_counts[name] += count
        parts.append(finish(d, order))
    table = to_table(torch.cat([p[0] for p in parts]))
    oc, fc = torch.cat([p[1] for p in parts]).numpy(), torch.cat([p[2] for p in parts]).numpy()
    for i, v in enumerate(RULED):
        table[f'_own_{v}'], table[f'_forced_{v}'] = oc[:, i], fc[:, i]
    table['scf_wave'] = np.array([w for w, _, _ in SCF_WAVES])[wave_k]
    table['scf_year'] = np.array([y for _, y, _ in SCF_WAVES])[wave_k]
    for src, k in svy_k.items():       # the year each record's CEX, CPS and SIPP phases ran at (the tax year)
        table[f'{src}_year'] = np.array([y for y, _ in SURVEY_YEARS[src]])[k]
    return table, rule_counts


def to_table(values):
    v = values.numpy()
    out = pd.DataFrame({'source': [M.SOURCES[i] for i in v[:, 0]], 'year': [M.YEARS[i] for i in v[:, 1]]})
    for j, var in enumerate(P):
        out[M.column(var)] = v[:, NC + j]
    out['weight'] = 1.0
    return out


# ---- the filer batch, the AGI group preset ----------------------------------------------------
def label_counts(n, records):
    """n records split across the groups in proportion to records (largest remainders)."""
    share = np.asarray(records, float) / np.sum(records)
    base = np.floor(n * share).astype(int)
    rem = n - base.sum()
    base[np.argsort(-(n * share - base), kind='stable')[:rem]] += 1
    return base


def label_part(model, mod, k, n_models, fac, group_records=None, seed=None):
    """Model k's (of n_models) filer batch: its share of label_records split across the 12 groups in proportion to the
    PUF's records per group, each record's group preset and everything else drawn given it. The weight is the group's
    PUF-year returns over the records drawn in it (1 / q). Each record's band-AGI group from its drawn bands is checked
    against its label (q_group, label_ok). group_records and seed replace the split and label_seed for Part A's
    candidates (top_hidden.py). Returns the part, the check's table and the rule counts."""
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


# ---- the real PUF as the filers (settings generation.filers 'real_puf') ------------------------------------------
# In place of the filer batch, every 2015 PUF record keeps its own PUF items (their bands preset) and each pooled
# network draws everything else given the record, so each record appears once per network (a "copy") at its PUF
# weight over the number of networks. Its own 2015 amounts go with it (own_<item>); dollars.own_values grows them.
REAL_ID = 50_000_000            # a real record's id: REAL_ID + copy index x REAL_COPY_BLOCK + its PUF RECID
REAL_COPY_BLOCK = 1_000_000
REAL_SEED_SHIFT = 500           # added to the filer batch's generation seed for a real-PUF copy
REAL_AGE_SALT = 2026110401      # the draws of the age range where the PUF has none (uniform_by_id), plus the copy
REAL_EARN_SPLIT_SALT = 2026110501   # the same for the earnings split code
_REAL = {}


def own_items():
    """The PUF amounts a real record carries its own 2015 value of: every PUF amount of the dollar map (the drawn
    items and the totals whose band is drawn), in dollars.ORDER."""
    from prep import build_table as BT
    from shared import dollars as D
    return [v for v in D.ORDER if v in set(BT.PUF_AMOUNTS)]


def real_puf_records():
    """The 2015 PUF's records with their bands, own 2015 amounts and weights. 'ord' holds the training table's PUF
    rows; 'agg' the four aggregate records, translated from the raw fields as Part A reads them and banded on the 2015
    cut points, with the AGI group label from those bands (filing status taken as single for the label alone). Their
    filing status, dependents and age are zero on the PUF, so the networks draw them."""
    if _REAL:
        return _REAL
    from shared import dollars as D
    from generate import top_hidden as TH
    own = own_items()
    slots = [v for v in PUF_SLOTS if v != 'filer'] + list(COMPUTED) + list(FLAGS)   # preset: not the filing flag, which generate sets
    cols = list(dict.fromkeys(['source', 'record_id', 'weight', 'age_range'] + M.AGE_PROBS + M.ES_PROBS
                              + [M.column(v) for v in slots] + own))
    t = pd.read_parquet(common.TABLE, columns=cols)
    o = t[(t.source == 'puf').to_numpy()].reset_index(drop=True)
    del t
    na = o[cols].isna().sum()
    assert not na.any(), f'missing values on PUF rows: {na[na > 0].to_dict()}'
    # the earnings split's two slots may be -1 on a PUF row, as in training: the spouse's self-employment share where
    # the PUF does not ask it, kept absent, and the earnings split code on joint returns with no demographics, drawn
    # from its probabilities (real_puf_part)
    may_lack = {'age_range', 'earn_split'} | set(M.PUF_SHARES)
    bad = {M.column(v): int((o[M.column(v)] < 0).sum()) for v in slots if v not in may_lack and (o[M.column(v)] < 0).any()}
    assert not bad, f'PUF rows with a slot absent: {bad}'
    if 'earn_split' in slots:
        none = o['earn_split'].to_numpy() < 0
        assert np.allclose(o.loc[none, M.ES_PROBS].sum(1), 1), 'earn_split: a PUF row with no code and no probabilities'
    o = o.rename(columns={v: D.OWN + v for v in own})
    agg_raw = TH.read_raw()
    # alimony received and paid (E00800, E03500) are blank on the four records (suppressed): read as zero, so each
    # record's AGI is still its E00100
    sup = list(TH.SUPPRESSED_FIELDS)
    assert agg_raw[sup].isna().all().all(), 'the suppressed fields are not blank on the aggregate records'
    agg_raw = agg_raw.copy()
    agg_raw[sup] = 0.0
    f = TH.puf_items(agg_raw, itemizer=True)
    na = f.isna().sum()
    assert not na.any(), f'aggregate records: items missing after the translation: {na[na > 0].to_dict()}'
    f['capital_gains'] = f['kg_st'] + f['kg_lt'] + f['kg_dist']       # the table's total: before the loss limit
    for tot, parts in M.META['computed_totals'].items():
        f[tot] = D.signed_sum(f, parts)
    f['other_agi'] = f['other_agi_remainder']                         # the table's other_agi is the remainder
    bands = TH.reband(f)
    a = pd.DataFrame({'record_id': agg_raw['RECID'].astype(float).to_numpy(),
                      'weight': agg_raw['S006'].to_numpy(float) / 100})
    for v in slots:
        if v in M.CATEGORICAL or v in M.PUF_LABELS or v in LATE_SLOTS:   # the late slots are drawn (real_puf_part)
            continue
        a[M.column(v)] = bands[f'{v}_band'].to_numpy()
    for flag, parts in FLAGS.items():
        a[flag] = (f[parts].to_numpy(float) != 0).any(1).astype(np.int64)
    lab = a[[c for c in a.columns if c.endswith('_band')]].assign(filing_status=1)
    a[LABEL] = Q.puf_group(Q.band_agi(lab), Q.puf_groups()['cuts'])
    a = pd.concat([a, pd.DataFrame({D.OWN + v: f[v].to_numpy(float) for v in own})], axis=1)
    _REAL.update({'ord': o, 'agg': a, 'own': own, 'slots': slots})
    common.say(f'real PUF: {len(o):,} ordinary records ({o.weight.sum() / 1e6:,.3f} million returns), '
               f'{len(a)} aggregate records ({a.weight.sum():,.2f} returns); {int((o.age_range < 0).sum()):,} ordinary '
               f'records with no age range (drawn from step 0\'s probabilities); {len(own)} own amounts per record')
    return _REAL


def drawn_from_probs(o, column, probs, salt):
    """The column's values on the real PUF rows o, with a value drawn from the row's probabilities (columns probs) where
    the PUF has none (-1); one draw per record and copy, keyed by record id. Returns the values and the rows drawn."""
    x = o[column].to_numpy().astype(np.int64)
    miss = x < 0
    u = uniform_by_id(o.loc[miss, 'record_id'].to_numpy(), salt)
    cp = np.cumsum(o.loc[miss, probs].to_numpy(float), 1)
    x[miss] = np.minimum((cp < u[:, None]).sum(1), len(probs) - 1)
    return x, miss


def real_puf_part(model, mod, k, n_models, fac):
    """Model k's (of n_models) copy of the real PUF: the ordinary records with every PUF slot preset (a missing age
    range drawn once per record and copy from its probabilities), and the four aggregate records with their amounts
    preset and their filing status, dependents and adult age range drawn; then every survey item drawn given the
    record. Weight: the PUF weight over n_models. Returns the part, the label check's table and the rule counts."""
    from shared import dollars as D
    R = real_puf_records()
    o, a, own, slots = R['ord'], R['agg'], R['own'], R['slots']
    seed = int(G['label_seed']) + mod['seed_shift'] + REAL_SEED_SHIFT
    preset = {v: torch.as_tensor(o[M.column(v)].to_numpy().astype(np.int64)) for v in slots}
    # the age range, and the earnings split code on joint returns with no demographics, where the PUF has none
    rng, miss = drawn_from_probs(o, 'age_range', M.AGE_PROBS, REAL_AGE_SALT + k)
    es, es_miss = drawn_from_probs(o, 'earn_split', M.ES_PROBS, REAL_EARN_SPLIT_SALT + k)
    preset['age_range'], preset['earn_split'] = torch.as_tensor(rng), torch.as_tensor(es)
    common.say(f"{mod['run']}: real PUF copy {k + 1}: earnings split code drawn from step 0's probabilities on "
               f"{int(es_miss.sum()):,} joint returns with no demographics (codes {np.bincount(es[es_miss], minlength=len(M.ES_PROBS)).tolist()})")
    part, counts = generate(model, len(o), seed, preset=preset, factors=fac, totals_preset=True)
    for v in slots:
        c = M.column(v)
        on = preset[v].numpy() >= 0 if P.index(v) in GATED else np.ones(len(part), bool)   # -1: drawn
        assert np.array_equal(part[c].to_numpy()[on], preset[v].numpy()[on]), f'real PUF: the draw moved {c}'
        if P.index(v) in GATED:
            n_drawn = int((preset[v].numpy() < 0).sum())
            n_set = int((part[M.column(v)].to_numpy()[preset[v].numpy() < 0] > 0).sum())
            common.say(f"{mod['run']}: real PUF copy {k + 1}: {v} not on the PUF for {n_drawn:,} records, which the "
                       f"generator's rule then asks or not; {n_set:,} of them drew a share above the first band")
    part['age_range_drawn'] = miss.astype(np.int64)
    part['puf_aggregate'] = 0
    drawn_a = set(TYPE_SLOTS) | set(STATUS_SLOTS) | set(LATE_SLOTS)   # the late slots drawn, as the status is
    pre_a = {v: torch.as_tensor(a[M.column(v)].to_numpy().astype(np.int64)) for v in slots if v not in drawn_a}
    part_a, counts_a = generate(model, len(a), seed + 7, preset=pre_a, factors=fac, totals_preset=True,
                                adult_only=True)
    part_a['age_range_drawn'] = 1
    part_a['puf_aggregate'] = 1
    counts = {r: counts[r] + counts_a[r] for r in counts}
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


def store_picks(s, k, stream=4242):
    """The stored picks: for each record and amount, one uniform number in [0, 1) in u_<variable> that picks the
    record's dollars within its band in every later step. Seeded by the dollars seed, the stream (4242 for the stack,
    4343 for Part A's candidates), the model's position k and the variable's position, so the same in every run."""
    from shared import dollars as D
    seed = int(common.SETTINGS['dollars']['seed'])
    cols = {f'u_{v}': np.random.default_rng([seed, stream, k, i]).random(len(s), dtype=np.float32)
            for i, v in enumerate(D.ORDER) if f'{v}_band' in s}
    return pd.concat([s, pd.DataFrame(cols, index=s.index)], axis=1)


def nonfiler_records():
    """The non-filer batch's number of records: the filer batch's records times the CPS's expected non-filers over
    SOI's returns."""
    from reweight import target_list as TLS
    # the CPS's expected non-filers in the year, the sum over its units of weight x (1 - filer_p) (a year with no CPS
    # units: the geometric mean of the CPS years either side)
    nf, returns = TLS.nonfiler_count(common.TARGET_YEAR), TG.returns(TG.load(), common.TARGET_YEAR)
    n = int(round(LABEL_RECORDS * nf / returns))
    common.say(f'non-filer batch: {n:,} records = {LABEL_RECORDS:,} x {nf / 1e6:.2f}M CPS {common.TARGET_YEAR} expected '
               f'non-filers / {returns / 1e6:.2f}M SOI returns')
    return n


def load_models(device):
    """The pooled models: per model its run, network, training seed and generation-seed shift."""
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
    """n records split over k models as evenly as can be, the first ones taking any remainder."""
    return [n // k + (i < n % k) for i in range(k)]


def build_stack(models, factors):
    """The filer batch (the AGI group first) and the non-filer batch, each drawn in equal shares from every model and
    stacked; how many records each rule applied to; and the label check per model and group. factors: {model run: its
    offset factors}."""
    n_nf = nonfiler_records()
    nf_years = nonfiler_years()
    rule_counts = {r['name']: 0 for r in RULES}
    stacks, checks = [], []
    for k, (mod, n_nf_k) in enumerate(zip(models, equal_shares(n_nf, len(models)))):
        model, shift, fac = mod['network'], mod['seed_shift'], factors[mod['run']]
        if REAL_PUF:      # every 2015 PUF record, its survey items drawn given it
            part, check, counts = real_puf_part(model, mod, k, len(models), fac)
        else:
            part, check, counts = label_part(model, mod, k, len(models), fac)
        part['weight'] *= LABEL_RECORDS / len(models) / part['weight'].sum()   # each model: an equal share
        part['stratifier'] = 'label'
        checks.append(check)
        rule_counts = {r: rule_counts[r] + counts[r] for r in rule_counts}
        common.say(f"{mod['run']}: filers done")
        nf_parts, counts = [], {r: 0 for r in rule_counts}
        sizes = [int(round(n_nf_k * s)) for _, s in nf_years]
        sizes[-1] = n_nf_k - sum(sizes[:-1])
        for j, ((ny, _), size) in enumerate(zip(nf_years, sizes)):
            # a seed per CPS year of the batch
            part_nf, c = generate(model, size, G['nonfiler_seed'] + shift + 100 * j, factors=fac, label='cps',
                                  year=ny, filer=0)
            if len(nf_years) > 1:
                common.say(f"  non-filers drawn at CPS {ny}: {size:,} records")
            nf_parts.append(part_nf)
            counts = {r: counts[r] + c[r] for r in counts}
        nf = pd.concat(nf_parts, ignore_index=True)
        nf['weight'], nf['stratifier'] = n_nf / len(models) / n_nf_k, 'nonfiler'   # each model: an equal share
        nf['q_group'], nf['label_ok'] = -1, True                                     # no label on a non-filer
        rule_counts = {r: rule_counts[r] + counts[r] for r in rule_counts}
        common.say(f"{mod['run']}: nonfiler done")
        if REAL_PUF:
            nf['puf_recid'], nf['puf_copy'], nf['puf_aggregate'], nf['age_range_drawn'] = -1, -1, 0, 0
        s = pd.concat([part, nf], ignore_index=True)
        s['model'] = mod['seed']
        assert len(s) < ID_BLOCK
        s['record_id'] = k * ID_BLOCK + np.arange(len(s))
        if REAL_PUF:      # a real record's id from its RECID and copy, so the package can key on them
            r = s['puf_recid'].to_numpy() >= 0
            s.loc[r, 'record_id'] = REAL_ID + k * REAL_COPY_BLOCK + s.loc[r, 'puf_recid'].to_numpy()
            assert s['record_id'].is_unique and (s.loc[~r, 'record_id'] < REAL_ID).all()
        stacks.append(store_picks(s, k))
    stack = pd.concat(stacks, ignore_index=True)
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
    for src, m in SURVEY_YEARS.items():     # the CEX, CPS and SIPP years, records and starting weight against the prior
        cnt, ws = stack.groupby(f'{src}_year').size(), stack.groupby(f'{src}_year')['weight'].sum() / stack['weight'].sum()
        common.say(f'{src.upper()} years, share of records and of the starting weight against the prior:\n'
                   + pd.DataFrame({'records': cnt, 'share of records': cnt / len(stack), 'share of weight': ws,
                                   'prior': pd.Series(dict(m))}).to_string())
    common.say('consistency rules: records each applies to, over the whole stack:')
    common.say(pd.Series(rule_counts).to_string())
    return stack, pd.concat(checks, ignore_index=True)


# ---- the offset -----------------------------------------------------------------
def held(m, f):
    """The chance of holding, the model's chance m with its odds multiplied by f."""
    return f * m / (f * m + (1 - m))


def offset(stack, factors, run):
    """Per ruled holding: the model's own expected holders, the holders with the rules, and the factor
    on the untouched records' odds that brings the second back to the first."""
    w = stack['weight'].to_numpy(float)
    rows, new = [], {}
    for v in RULED:
        m, forced = stack[f'_own_{v}'].to_numpy(float), stack[f'_forced_{v}'].to_numpy(bool)
        f0 = factors.get(v, 1.0)
        target = (w * m).sum()
        expected = (w * np.where(forced, 1.0, held(m, f0))).sum()
        fixed, rest = w[forced].sum(), ~forced
        lo, hi = -30.0, 30.0                                # bisection on the log of the factor
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
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(common.SEED)
    models = load_models(device)
    os.makedirs(common.GEN, exist_ok=True)
    common.say('survey years: ' + '; '.join(f"{src} " + ', '.join(f'{y} {100 * s:.2f}%' for y, s in m)
                                           for src, m in SURVEY_YEARS.items()))
    common.say('SCF waves of the balance sheet: ' + ', '.join(f'{w} wave (income year {y}) {100 * s:.2f}%'
                                                              for w, y, s in SCF_WAVES))
    factors, report = {m['run']: {} for m in models}, []
    runs = int(C['offset_generations'])
    for run in range(1, runs + 1):
        stack, check = build_stack(models, factors)
        check.insert(0, 'run', run)
        check.to_csv(os.path.join(common.GEN, 'label_check.csv'), index=False)   # the last run's is kept
        nxt = {}
        for m in models:                    # each model's offset on its own records
            table, nxt[m['run']] = offset(stack[stack['model'] == m['seed']], factors[m['run']], run)
            table.insert(1, 'model', m['seed'])
            report.append(table)
            common.say(f"run {run} of {runs}, model {m['run']}:\n" + table.drop(columns=['run', 'model']).to_string(index=False))
        if run < runs:
            factors = nxt
    pd.concat(report).to_csv(os.path.join(common.GEN, 'consistency.csv'), index=False)
    stack.drop(columns=[c for c in stack if c.startswith(('_own_', '_forced_'))]) \
        .to_parquet(os.path.join(common.GEN, 'stack.parquet'), index=False)


if __name__ == '__main__':
    main()
