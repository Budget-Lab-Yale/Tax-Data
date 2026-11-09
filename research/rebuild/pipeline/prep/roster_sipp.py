"""The SIPP reader for the shared tax-unit rules (tax_units.py), and the SIPP's tax units (settings "sipp").

The Survey of Income and Program Participation (SIPP) has one public-use release per reference year, read by
sipp_read.py, with one row per person and month. The household is the sample unit as interviewed. Income is summed
over the reference year's 12 months, as the CPS asks it. This module builds the standard member list, runs the rules,
and splits some couples into separate returns. It writes sipp_units_<year>.csv.gz for build_table.py: one row per
unit, with each adult's wages, tips and elective deferrals and the unit's dividends, Social Security and pensions.
It also writes the rules' persons and units. FEATURES.md, "The SIPP", describes how the SIPP is used.
"""
import argparse
import os

import numpy as np
import pandas as pd

from shared import common
from prep import sipp_read as R
from prep import tax_units as T

S = common.SETTINGS['sipp']
LH = [tuple(r) for r in S['lh_industries']]    # the Census industry code ranges of leisure and hospitality
WAGE_CLASSES = [1, 2, 3, 4, 5, 6]              # classes of worker that work for an employer (7 and 8 are self-employed)
BLOOD = [7, 8, 9, 10, 13, 14]                  # the householder's relatives by blood (in-laws, codes 11 and 12, are not)
RELATIVES = [3, 5, 7, 8, 9, 10, 11, 12, 13, 14]
SHARED_ITEMS = ['dividends', 'gross_ss', 'pensions']   # the items the units file shares with the PUF (shared_items)
# Items that a release leaves blank for everyone although some people say they received them. Their amounts are read
# as zero. In reference year 2021, National Guard or reserve retirement is blank for everyone, and 3 people say they
# received it.
UNPUBLISHED = {(2021, 'TRET7AMT')}


# ---- Income items by person ----

def in_lh(codes):
    """Whether each Census industry code is in leisure and hospitality (settings sipp.lh_industries)."""
    c = pd.to_numeric(codes, errors='coerce').to_numpy(float)
    return np.logical_or.reduce([(c >= lo) & (c <= hi) for lo, hi in LH])


def explained(d, item, asked, label, unpublished=False):
    """The person-month values of `item`, with each missing value read as zero when the reason it is missing is known.

    A value is missing for a known reason when the person is under 15, or did not say they received the item (the
    columns in `asked` hold the receipt questions). A missing amount where the person said they received the item
    stops the run, except for an item listed in UNPUBLISHED."""
    v = d[item].to_numpy(float)
    na = np.isnan(v)
    q = d[asked].to_numpy(float)
    yes = (q == 1).any(axis=1)
    answered = (~np.isnan(q)).any(axis=1)
    young = d.TAGE.to_numpy(float) < 15
    n = [int((na & young).sum()), int((na & ~young & ~answered).sum()),
         int((na & ~young & answered & ~yes).sum()), int((na & ~young & yes).sum())]
    stray = ~na & ~yes
    common.say(f'    {label} ({item}): {len(v):,} person-month rows, {int(na.sum()):,} missing: under 15 {n[0]:,}, '
               f'no "received" answer {n[1]:,}, an answer other than "received" {n[2]:,}, said received but no amount '
               f'{n[3]:,}; an amount without "received": {int(stray.sum()):,} rows, ${np.nansum(v[stray]):,.0f}; '
               f'negative amounts {int((v < 0).sum())}')
    if n[3] and unpublished and na.all():
        who = d.loc[na & ~young & yes, ['SSUID', 'PNUM']].drop_duplicates()
        common.say(f'    UNPUBLISHED: {label} ({item}) is blank for everyone in this release; {n[3]} person-month rows '
                   f'({len(who)} people) say they received it; their amounts are unknown and read as zero')
    elif n[3]:
        bad = na & ~young & yes
        who = d.loc[bad, ['SSUID', 'PNUM']].drop_duplicates()
        blank = 'the release leaves the whole column blank (the item is not published in it); ' if na.all() else ''
        raise AssertionError(f'{label} ({item}): {blank}{n[3]} person-month rows ({len(who)} people) say they '
                             'received it but have no amount')
    return np.where(na, 0.0, v)


def shared_items(d, year):
    """Per person, the items other than wages that the SIPP shares with the PUF.

    They are dividends (income from stocks and mutual funds, a yearly amount repeated in every month), gross Social
    Security received by the person before Medicare premiums, and pensions (retirement income items 1-8). The public
    file has no amounts of IRA or 401(k) withdrawals. Also reports how often a person's monthly value changes across
    the months, to check that the dividends are a yearly amount."""
    common.say(f'SIPP {year}: the shared items\' missing values, by person-month row')
    key = [d.SSUID, d.PNUM]
    div = pd.Series(explained(d, 'TINC_STMF', R.OWNERSHIP, 'dividends'), index=d.index)
    ss = explained(d, 'TSSSAMT', ['ESSSMNYN'], 'gross Social Security')
    ret = sum(explained(d, f'TRET{k}AMT', [f'ERET{k}MNYN'], f'retirement income item {k}',
                        unpublished=(year, f'TRET{k}AMT') in UNPUBLISHED)
              for k in range(1, R.RET_ITEMS + 1))
    # The number of distinct values a person's dividends take across the months (one for a yearly amount repeated by
    # month), and whether they are missing in some months only
    nun = d.groupby(key, sort=False)['TINC_STMF'].nunique(dropna=True)
    mixed = d.TINC_STMF.isna().groupby(key, sort=False).agg(lambda s: 0 < s.sum() < len(s))
    y = pd.DataFrame({'own_dividends': div.groupby(key, sort=False).max(),
                      'own_gross_ss': pd.Series(ss, index=d.index).groupby(key, sort=False).sum(),
                      'own_pensions': pd.Series(ret, index=d.index).groupby(key, sort=False).sum()})
    holders = y.own_dividends > 0
    varies = float((nun.reindex(y.index) > 1)[holders].mean()) if holders.any() else 0.0
    common.say(f'  dividends (TINC_STMF): people with any whose value differs across months {100 * varies:.2f}%; '
               f'people with a value in some months and missing in others {int(mixed.sum())}')
    assert varies <= 0.01, f'SIPP {year}: TINC_STMF is not a yearly amount repeated by month ({100 * varies:.1f}% vary)'
    for c, s in [('gross Social Security (TSSSAMT)', 'TSSSAMT')] + \
            [(f'retirement income item {k} (TRET{k}AMT)', f'TRET{k}AMT') for k in range(1, R.RET_ITEMS + 1)]:
        g = d.groupby(key, sort=False)[s]
        h = g.max() > 0
        common.say(f'  {c}: people with any whose monthly value changes across months '
                   f'{100 * float((g.nunique(dropna=True) > 1)[h].mean()) if h.any() else 0:.1f}%')
    return y


def person_year(d, year):
    """Per person (SSUID, PNUM), the year's income items, December enrolment, earnings and weight, and the wages and
    tips on jobs for an employer.

    A job's earnings include the job's tips, so tips are part of wages, as on Form W-2. Tips in leisure and
    hospitality are those from jobs in the industries in settings sipp.lh_industries. Also returns the number of
    job-months whose tips are above the job's earnings."""
    # 1. Wages and tips on jobs for an employer, by person-month. Tips are published for the first four job lines.
    wages = np.zeros(len(d)); tips = np.zeros(len(d)); tips_lh = np.zeros(len(d))
    over = 0
    for j in range(1, R.JOBS + 1):
        emp = d[f'EJB{j}_CLWRK'].isin(WAGE_CLASSES).to_numpy()
        m = d[f'TJB{j}_MSUM'].fillna(0).to_numpy(float)
        wages += np.where(emp, m, 0.0)
        if j <= R.TIP_JOBS:
            t = d[f'TJB{j}_TXAMT'].fillna(0).to_numpy(float)
            over += int(((t > m + 0.5) & emp & (t > 0)).sum())
            tips += np.where(emp, t, 0.0)
            tips_lh += np.where(emp & in_lh(d[f'TJB{j}_IND']), t, 0.0)
    # 2. Sum the person's months. Self-employment income is total earnings less wages.
    f = lambda c: d[c].fillna(0).to_numpy(float)
    x = pd.DataFrame({'SSUID': d.SSUID, 'PNUM': d.PNUM, 'wage': wages, 'tips': tips, 'tips_lh_dollars': tips_lh,
                      'earn': f('TPEARN'), 'ssb': f('TSSSAMT'), 'ssi': f('TSSI_AMT'),
                      'uc': f('TUC1AMT') + f('TUC2AMT'), 'property': f('TPPRPINC'), 'other': f('TPOTHINC')})
    y = x.groupby(['SSUID', 'PNUM'], sort=False).sum()
    y['selfemp'] = y.earn - y.wage
    y = y.join(shared_items(d, year))
    assert np.abs(y.own_gross_ss - y.ssb).max() < 0.005, 'gross Social Security differs from ssb'
    # 3. Elective deferrals, where the release asks them: a yearly amount repeated by month. A person with no value
    #    in any month has none.
    item = common.SETTINGS['deferrals']['sipp_item']
    if item in d:
        g = d.groupby(['SSUID', 'PNUM'], sort=False)[item]
        y['elective_reported'] = g.max().clip(lower=0).reindex(y.index).fillna(0.0).to_numpy()
        y['elective_no_value'] = g.count().reindex(y.index).eq(0).to_numpy()
        y['elective_asked'] = True
    else:
        y['elective_reported'], y['elective_no_value'], y['elective_asked'] = 0.0, True, False
    # 4. December's enrolment and earnings, and the weight and residence of December (or of the person's last month
    #    in the survey).
    dec = d[d.MONTHCODE == 12].set_index(['SSUID', 'PNUM'])
    y['enrolled_dec'] = dec.RENROLL.reindex(y.index).eq(1).to_numpy()
    y['earn_dec'] = dec.TPEARN.reindex(y.index).fillna(0).to_numpy()
    last = d.sort_values('MONTHCODE').groupby(['SSUID', 'PNUM']).tail(1).set_index(['SSUID', 'PNUM'])
    y['weight'] = dec.WPFINWGT.reindex(y.index)
    y['weight'] = y.weight.fillna(last.WPFINWGT.reindex(y.index))
    y['resid_dec'] = dec.ERESIDENCEID.reindex(y.index).fillna(last.ERESIDENCEID.reindex(y.index)).to_numpy()
    return y.reset_index(), over


# ---- The member list ----

def read_roster(year):
    """The standard member list for one reference year, with each person's tips, elective deferrals and shared items
    carried along."""
    # 1. Read the release and sum each person's year.
    d = R.read_release(year)
    py, over = person_year(d, year)
    common.say(f'SIPP {year}: job-months whose tips are above the job\'s earnings: {over}')
    # 2. Keep the people present at the interview who have a weight. People with weight 0 are kept; they count for
    #    nothing.
    p = d.drop_duplicates(['SSUID', 'PNUM']).set_index(['SSUID', 'PNUM'])
    per = py.set_index(['SSUID', 'PNUM'])
    keep = p.THHLDSTATUS.isin([1, 2]).to_numpy()
    gone = p[~keep]
    common.say(f'  people not present at the interview (left out): {len(gone):,}, '
               f'{per.weight.reindex(gone.index).sum() / 1e6:.2f} m by their last weight')
    p = p[keep]
    assert not p.ERELRPE.isna().any(), 'an interview member without a relationship code'
    per = per.reindex(p.index)
    w0 = per.weight.isna()
    common.say(f'  interview members with no weight in any month (left out): {int(w0.sum())}')
    p, per = p[~w0.to_numpy()], per[~w0.to_numpy()]
    zero = (per.weight <= 0).to_numpy()
    common.say(f'  interview members with weight 0 (kept, they count for nothing): {int(zero.sum())}')
    # 3. Households. A household is the people of a sample unit present at the interview. A sample unit with more
    #    than one householder is split into one household per householder, and each person goes with the
    #    householder of their December residence. The household id is the sample unit's number x 10 plus which of
    #    its householders it is.
    P = p.reset_index()
    P = pd.concat([P, per.reset_index(drop=True)[[c for c in per.columns if c not in P]]], axis=1)
    hh_flag = P.ERELRPE.isin([1, 2])
    n_hh = hh_flag.groupby(P.SSUID).transform('sum').to_numpy()
    assert (n_hh >= 1).all(), f'{int((n_hh == 0).sum())} interview people in a sample unit with no householder'
    multi = n_hh > 1
    res_of_hh = P[hh_flag].set_index(['SSUID', 'resid_dec']).PNUM
    key = pd.MultiIndex.from_arrays([P.SSUID, P.resid_dec])
    own_hh = pd.Series(key.map(res_of_hh.groupby(level=[0, 1]).min()), index=P.index)
    first_hh = P.SSUID.map(P[hh_flag].groupby('SSUID').PNUM.min())
    hhpn = np.where(multi, own_hh.fillna(first_hh), first_hh).astype(int)
    hhpn = np.where(hh_flag, P.PNUM, hhpn)
    common.say(f'  sample units with more than one householder at the interview: '
               f'{P.loc[multi, "SSUID"].nunique()}, {int(multi.sum())} people')
    ssu = pd.factorize(P.SSUID)[0].astype(np.int64) + 1
    k = pd.Series(hhpn).groupby(P.SSUID.to_numpy()).rank(method='dense').astype(np.int64).to_numpy()
    assert k.max() < 10
    P['hh'] = ssu * 10 + k
    # 4. Person numbers: the householder is 1, then the others by PNUM. The spouse and parent pointers are
    #    translated to the new numbers. A spouse pointer counts only when the two people point at each other.
    P['is_hh'] = hh_flag.to_numpy()
    P = P.sort_values(['hh', 'is_hh', 'PNUM'], ascending=[True, False, True]).reset_index(drop=True)
    P['pn'] = P.groupby('hh').cumcount() + 1
    pn_of = P.set_index(['hh', 'PNUM']).pn
    ptr = lambda c: pd.MultiIndex.from_arrays([P.hh, P[c].fillna(-1).astype(int)]).map(pn_of).fillna(0).astype(int).to_numpy()
    sp = ptr('EPNSPOUSE'); m1 = ptr('EPNPAR1'); m2 = ptr('EPNPAR2')
    back = pd.Series(sp, index=pd.MultiIndex.from_arrays([P.hh, P.pn]))
    mutual = pd.MultiIndex.from_arrays([P.hh, sp]).map(back).fillna(-1).to_numpy() == P.pn.to_numpy()
    sp = np.where(mutual & (sp > 0), sp, 0)
    common.say(f'  spouse pointers not returned or outside the household (dropped): '
               f'{int(((ptr("EPNSPOUSE") > 0) & (sp == 0)).sum())}')
    r = pd.DataFrame({'survey': 'sipp', 'year': year, 'hh': P.hh.to_numpy(), 'pn': P.pn.to_numpy(),
                      'age': P.TAGE.astype(int).to_numpy(), 'sex': P.ESEX.astype(int).to_numpy(),
                      'weight': P.weight.to_numpy(float), 'gq': False})
    rel = P.ERELRPE.astype(int).to_numpy()
    hh = r.hh.to_numpy(); pn = r.pn.to_numpy(); age = r.age.to_numpy()
    # 5. Marital status in the CPS's terms (MARST). A person under 15 is never married.
    ems = P.EMS.fillna(6).astype(int).to_numpy()
    ems = np.where(age < 15, 6, ems)
    married_sp = sp > 0
    marst = np.where(married_sp, 1, np.select([ems == 1, ems == 2, ems == 3, ems == 4, ems == 5], [2, 2, 5, 4, 3], 6))
    r['sp_pn'] = sp; r['mom_pn'] = m1; r['pop_pn'] = m2; r['marst'] = marst
    r['married_sp'] = married_sp; r['married_absent'] = marst == 2
    # 6. Subfamilies. A subfamily is a married couple other than the householder's couple, or a lone parent of
    #    never-married children under 18. The people in the householder's couple are found first, then each
    #    person's number of such children.
    hsp = pd.Series(np.where(pn == 1, sp, 0)).groupby(hh).transform('max').to_numpy()
    in_hcouple = (pn == 1) | ((pn == hsp) & (hsp > 0))
    kid = (age < 18) & (marst == 6)
    kids_of = pd.concat([pd.DataFrame({'hh': hh[kid], 'par': m1[kid]}), pd.DataFrame({'hh': hh[kid], 'par': m2[kid]})])
    kids_of = kids_of[kids_of.par > 0]
    n_kids = pd.MultiIndex.from_arrays([hh, pn]).map(kids_of.groupby(['hh', 'par']).size()).fillna(0).to_numpy()
    relative = np.isin(rel, RELATIVES)
    couple = married_sp & ~in_hcouple
    lone = ~married_sp & ~in_hcouple & (n_kids > 0) & (pn != 1)
    sp_rel = pd.MultiIndex.from_arrays([hh, sp]).map(pd.Series(rel, index=pd.MultiIndex.from_arrays([hh, pn]))) \
        .fillna(0).to_numpy().astype(int)
    sp_sex = pd.MultiIndex.from_arrays([hh, sp]).map(pd.Series(r.sex.to_numpy(), index=pd.MultiIndex.from_arrays([hh, pn]))) \
        .fillna(0).to_numpy().astype(int)
    blood, sp_blood = np.isin(rel, BLOOD), np.isin(sp_rel, BLOOD)
    # A subfamily couple's reference person is the one who is the householder's blood relative. If both are or
    # neither is, it is the man; in a same-sex couple, the one with the lower person number.
    ref_couple = couple & ((blood & ~sp_blood) | ((blood == sp_blood) & ((r.sex.to_numpy() == 1) & (sp_sex == 2) |
                                                                          ((r.sex.to_numpy() == sp_sex) & (pn < sp)))))
    sf_ref = ref_couple | lone
    sf_head = np.where(sf_ref, pn, 0)
    sf_head = np.where(couple & ~ref_couple, sp, sf_head)                     # the couple's other member points at its reference person
    # The subfamily's children: never-married people under 18 whose parent is a subfamily member, who are not in the
    # householder's couple and are not the householder's own children
    head_of = pd.Series(sf_head, index=pd.MultiIndex.from_arrays([hh, pn]))
    par_head = np.maximum(pd.MultiIndex.from_arrays([hh, m1]).map(head_of).fillna(0).to_numpy(),
                          pd.MultiIndex.from_arrays([hh, m2]).map(head_of).fillna(0).to_numpy()).astype(int)
    sf_kid = kid & (par_head > 0) & (sf_head == 0) & ~in_hcouple & (rel != 7)
    sf_head = np.where(sf_kid, par_head, sf_head)
    in_sf = sf_head > 0
    ref_rel = pd.MultiIndex.from_arrays([hh, sf_head]).map(pd.Series(rel, index=pd.MultiIndex.from_arrays([hh, pn]))) \
        .fillna(0).to_numpy().astype(int)
    related_sf = in_sf & np.isin(ref_rel, RELATIVES)
    unrelated_sf = in_sf & ~related_sf
    # 7. Census's relationship code (PERRP) and family position code (HHDFMX), which CBO's rules read. A
    #    non-relative has relatives in the household when a spouse, parent or child pointer links them to someone
    #    there.
    linked = (sp > 0) | (m1 > 0) | (m2 > 0) | (n_kids > 0)
    family = pd.Series(relative).groupby(hh).transform('any').to_numpy()
    perrp = np.select(
        [pn == 1, rel == 3, rel == 5, rel == 4, rel == 6, rel == 7, rel == 8, rel == 9, rel == 10,
         np.isin(rel, [11, 12, 13, 14]), rel == 15, rel == 16, rel == 17, rel == 18],
        [np.where(family, 40, 41), 42, 45, np.where(linked, 43, 44), np.where(linked, 46, 47), 48, 49, 50, 51, 52, 53,
         np.where(linked, 54, 55), np.where(linked, 56, 57), np.where(linked, 58, 59)], -1)
    assert (perrp > 0).all(), pd.Series(rel[perrp < 0]).value_counts()
    assert ((pn == 1) == np.isin(rel, [1, 2])).all(), 'the householder is not person 1'
    kin = np.select([rel == 7, rel == 8], ['child', 'grandchild'], 'other')
    pos = np.select([in_sf & (sf_head == pn), in_sf & sf_kid, in_sf], ['ref', 'kid', 'sp'], 'none')
    code_pf = {('child', 'none'): 4, ('child', 'ref'): 3, ('child', 'sp'): 6, ('child', 'kid'): 36,
               ('grandchild', 'none'): 25, ('grandchild', 'ref'): 23, ('grandchild', 'sp'): 27, ('grandchild', 'kid'): 24,
               ('other', 'none'): 37, ('other', 'ref'): 35, ('other', 'sp'): 39, ('other', 'kid'): 36}
    # In a related subfamily, a member who is not the householder's relative (a partner by marriage, say) takes the
    # "other relative" codes.
    pf_code = np.array([code_pf[(k if rl else 'other', ps)] for k, rl, ps in zip(kin, relative | related_sf, pos)])
    hhdfmx = np.select(
        [(pn == 1) & family, pn == 1, (pn == hsp) & (hsp > 0) & np.isin(rel, [3, 5]), related_sf,
         unrelated_sf & (pos == 'ref'), unrelated_sf & (pos == 'sp'), unrelated_sf, relative],
        [1, 49, 2, pf_code, 46, 47, 48, np.array([code_pf[(k, 'none')] for k in kin])], 50)
    r['perrp'] = perrp; r['hhdfmx'] = hhdfmx
    sec = hhdfmx == 50
    sf_ids = pd.Series(np.where(in_sf, sf_head, 0)).groupby(hh).transform(lambda s: s.rank(method='dense')).to_numpy()
    sec_k = pd.Series(sec.astype(int)).groupby(hh).cumsum().to_numpy()
    n_sf = pd.Series(np.where(in_sf, sf_ids, 0)).groupby(hh).transform('max').to_numpy()
    r['ftype'] = np.select([hhdfmx == 49, related_sf, unrelated_sf, sec], [2, 3, 4, 5], 1)
    r['famrel'] = np.select([np.isin(hhdfmx, [1, 49]) | (pos == 'ref'), (hhdfmx == 2) | (pos == 'sp'),
                             (hhdfmx == 4) | (pos == 'kid'), sec], [1, 2, 3, 0], 4)
    r['famid'] = np.select([in_sf, sec], [1 + sf_ids, 1 + n_sf + sec_k], 1).astype(int)
    r['ffpos'] = r.famid
    r['fheadidx'] = np.select([in_sf, sec | (hhdfmx == 49)], [sf_head, pn], 1).astype(int)
    r['relate'] = rel
    # 8. The student and disability tests. A student is enrolled in December with no earnings that month. A person
    #    is disabled when any of the SIPP's core disability items says so.
    r['in_school'] = (P.enrolled_dec.to_numpy() & (P.earn_dec.to_numpy() <= 0))
    r['nilf_school'] = False; r['ill_disabled'] = False; r['has_disab_income'] = False
    r['diff_any'] = (P.RDIS == 1).to_numpy()
    # 9. Incomes (CBO's 3a). Property income counts as interest, and other income as pensions.
    for c in T.INCOME_ITEMS:
        r[c] = 0.0
    r['wage'] = P.wage.to_numpy(); r['selfemp'] = P.selfemp.to_numpy(); r['interest'] = P.property.to_numpy()
    r['uc'] = P.uc.to_numpy(); r['ssb'] = P.ssb.to_numpy(); r['ssi'] = P.ssi.to_numpy()
    r['pension'] = P.other.to_numpy()
    gi0 = r.wage + r.selfemp + r.interest + r.uc + r.pension
    ssb = r.ssb.to_numpy()
    r['gi'] = gi0 + T.taxable_social_security(gi0, ssb, marst == 1)
    r['support'] = -1
    r['gi_obs'] = True
    for c in T.ASSUMED_FLAGS:
        r[c] = False
    # 10. Carry the tips through.
    r['tips'] = P.tips.to_numpy(); r['tips_lh_dollars'] = P.tips_lh_dollars.to_numpy()
    # 11. Elective deferrals, held to the year's legal limit (with the higher catch-up limit from age 50) and then to
    #     the person's own wages.
    asked = bool(P.elective_asked.all())
    assert asked or not P.elective_asked.any()
    if asked:
        D = common.SETTINGS['deferrals']
        lim = D['limits'][str(year)]
        rep = P.elective_reported.to_numpy(float)
        limit = np.where(r.age.to_numpy() >= D['catch_up_age'], lim[1], lim[0])
        capped = np.minimum(rep, limit)
        own = np.minimum(capped, np.maximum(r.wage.to_numpy(float), 0.0))
        wt = r.weight.to_numpy(float)
        common.say(f'  elective deferrals ({D["sipp_item"]}): people with any {wt[rep > 0].sum() / 1e6:.2f} m, '
                   f'${(wt * rep).sum() / 1e9:.1f} billion reported; above the {year} limit '
                   f'{wt[rep > limit].sum() / 1e6:.2f} m people, ${(wt * (rep - capped)).sum() / 1e9:.2f} billion '
                   f'taken off; above their own wages {wt[capped > own].sum() / 1e6:.2f} m people, '
                   f'${(wt * (capped - own)).sum() / 1e9:.2f} billion taken off; no value in any month (read as none) '
                   f'{wt[P.elective_no_value.to_numpy(bool)].sum() / 1e6:.1f} m people')
        r['elective'] = own
        r['elective_reported'] = rep
    else:
        common.say(f'  elective deferrals: not asked in the {year} release (the units\' deferrals are missing)')
        r['elective'] = 0.0
        r['elective_reported'] = 0.0
    r['elective_asked'] = asked
    # 12. Carry the shared items and the SIPP's own person ids through.
    for c in ['own_dividends', 'own_gross_ss', 'own_pensions']:
        r[c] = P[c].to_numpy(float)
    r['SSUID'] = P.SSUID.to_numpy(); r['PNUM'] = P.PNUM.astype(int).to_numpy()
    assert not r.isna().any().any(), r.isna().sum()[r.isna().sum() > 0]
    assert (r.groupby('hh').pn.min() == 1).all()
    common.say(f'  {len(r):,} people in {r.hh.nunique():,} households, {r.weight.sum() / 1e6:.1f} m weighted; '
               f'related subfamilies {int((related_sf & (pos == "ref")).sum())}, unrelated {int((unrelated_sf & (pos == "ref")).sum())}')
    return r


# ---- The units file ----

def unit_rows(units, persons, mfs, year):
    """The units file: one row per unit, with the separate returns in place of the couples they split. Dependents'
    own returns are dropped, and their income goes nowhere. Person 1 and person 2 are ordered as
    tax_units.primary_and_spouse orders them."""
    # 1. List the units and their persons 1 and 2.
    main = units[~units.dep_return].copy()
    split = set(mfs.couple_uid) if len(mfs) else set()
    keep = main[~main.uid.isin(split)]
    P = persons.set_index(['hh', 'pn'])
    item = lambda h, p, c: pd.MultiIndex.from_arrays([h, p]).map(P[c]).to_numpy()
    joint = keep[keep.filing_status == 'joint']
    p1, p2 = T.primary_and_spouse(persons, joint.uid.to_numpy())
    one_pn = keep.head_pn.to_numpy().copy()
    two_pn = np.full(len(keep), -1)
    is_j = (keep.filing_status == 'joint').to_numpy()
    one_pn[is_j] = p1.pn.reindex(keep.uid[is_j]).to_numpy()
    two_pn[is_j] = p2.pn.reindex(keep.uid[is_j]).to_numpy()
    rows = pd.DataFrame({'uid': keep.uid.to_numpy(), 'hh': keep.hh.to_numpy(), 'weight': keep.weight.to_numpy(),
                         'married': is_j.astype(int), 'n_dep': keep.n_dep.to_numpy(), 'pn1': one_pn, 'pn2': two_pn})
    if len(mfs):
        m = mfs.copy()
        rows = pd.concat([rows, pd.DataFrame({'uid': m.uid.to_numpy(), 'hh': m.hh.to_numpy(), 'weight': m.weight.to_numpy(),
                                              'married': 0, 'n_dep': m.n_dep.to_numpy(), 'pn1': m.head_pn.to_numpy(),
                                              'pn2': -1})], ignore_index=True)
    # 2. Copy each person's age, sex, wages, tips and deferrals onto the unit, and add up the shared items over the
    #    two people. A unit's tips count as mostly from leisure and hospitality when that share is above
    #    settings sipp.lh_share.
    h = rows.hh.to_numpy()
    out = pd.DataFrame({'year': year, 'record_id': rows.uid.to_numpy(), 'household_id': h, 'weight': rows.weight,
                        'married': rows.married, 'n_dep': np.minimum(rows.n_dep, 3), 'dep_filer': 0})
    shared = {}
    for k, col in [(1, 'pn1'), (2, 'pn2')]:
        pnk = rows[col].to_numpy()
        has = pnk > 0
        g = lambda c, fill: np.where(has, item(h, np.where(has, pnk, 1), c), fill)
        out[f'age{k}'] = g('age', -1).astype(int)
        out[f'sex{k}'] = g('sex', 0).astype(int)
        out[f'male{k}'] = np.where(has, (out[f'sex{k}'] == 1).astype(int), -1)
        out[f'wages{k}'] = g('wage', 0.0).astype(float)
        tips = g('tips', 0.0).astype(float)
        lh = g('tips_lh_dollars', 0.0).astype(float)
        out[f'tips{k}'] = tips
        out[f'tips_lh{k}'] = ((tips > 0) & (lh > S['lh_share'] * np.where(tips > 0, tips, 1))).astype(int)
        out[f'elective{k}'] = g('elective', 0.0).astype(float)
        for c in SHARED_ITEMS:
            shared[c] = shared.get(c, 0.0) + g(f'own_{c}', 0.0).astype(float)
    out['wages'] = out.wages1 + out.wages2
    # 3. Elective deferrals are missing on every unit in a year whose release does not ask them.
    asked = bool(persons.elective_asked.all())
    assert asked or not persons.elective_asked.any()
    out['elective_deferrals'] = out.elective1 + out.elective2
    if not asked:
        out[['elective1', 'elective2', 'elective_deferrals']] = np.nan
    for c in SHARED_ITEMS:        # the primary filer's plus the spouse's
        out[c] = shared[c]
    assert (out.loc[out.married == 0, ['wages2', 'tips2', 'tips_lh2']] == 0).all().all()
    assert ((out.age2 >= 0) == (out.married == 1)).all()
    assert not out.drop(columns=['elective1', 'elective2', 'elective_deferrals']).isna().any().any()
    assert asked == out.elective_deferrals.notna().all() and not out.record_id.duplicated().any()
    return out


# ---- The units ----

def build(year):
    """The SIPP's units for one reference year. Couples are split into separate returns by the CEX's rule: every
    joint couple is a candidate, and couples are taken in the order of a keyed draw until their weight reaches half
    of SOI Table 1.6's separate returns."""
    from prep import soi as TG
    r = read_roster(year)
    persons, units = T.build_units(r, year)
    target = sum(TG.table_1_6(year)[('separate', k)] for k in range(6))
    mu = units[~units.dep_return].reset_index(drop=True)
    chosen, share, mfs = T.mfs_split(mu, persons, np.ones(len(mu), bool), target, salt=int(S['mfs_salt']))
    common.say(f'SIPP {year}: SOI Table 1.6 separate returns {target / 1e6:.3f} m; {len(chosen)} couples split '
               f'({share:.4f} of joint couples by weight)')
    units['mfs_split'] = units.uid.isin(chosen)
    return persons, units, mfs


# ---- Summaries ----

def summary(u, year):
    """One row of sipp_summary.csv: the year's units, wages and tips."""
    w = u.weight.to_numpy()
    tips = u.tips1 + u.tips2
    return {'year': year, 'rows': len(u), 'units (millions)': w.sum() / 1e6,
            'people in the units, heads and spouses (millions)': (w * (1 + u.married)).sum() / 1e6,
            'joint (millions)': (w * u.married).sum() / 1e6, 'wages ($ billion)': (w * u.wages).sum() / 1e9,
            'units with wages (millions)': w[u.wages > 0].sum() / 1e6,
            'tips ($ billion)': (w * tips).sum() / 1e9, 'units with tips (millions)': w[tips > 0].sum() / 1e6,
            'people with tips (millions)': (w * ((u.tips1 > 0).astype(int) + (u.tips2 > 0))).sum() / 1e6,
            'tips, leisure and hospitality ($ billion)': (w * (u.tips1 * u.tips_lh1 + u.tips2 * u.tips_lh2)).sum() / 1e9,
            'tips / wages of the people with tips': float((w * tips).sum() / (w * (u.wages1 * (u.tips1 > 0) + u.wages2 * (u.tips2 > 0))).sum())}


def shared_check(u, persons, year):
    """A summary of the shared items for one year, by unit and by person. The difference between the two totals is
    mostly the dependents' own amounts, which no unit carries."""
    w = u.weight.to_numpy(float)
    out = []
    for c in SHARED_ITEMS:
        x = u[c].to_numpy(float)
        h = x > 0
        out.append({'year': year, 'item': c, 'total ($ billion)': (w * x).sum() / 1e9,
                    'any (%)': 100 * w[h].sum() / w.sum(),
                    "holders' median ($)": float(common.wquantile(x[h], w[h], 0.5)) if h.any() else np.nan,
                    '$10k+ (m)': w[x >= 1e4].sum() / 1e6, '$100k+ (m)': w[x >= 1e5].sum() / 1e6,
                    "people's total ($ billion)": (persons.weight * persons[f'own_{c}']).sum() / 1e9})
    return out


# ---- The whole stage ----

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('years', type=int, nargs='*')
    ap.add_argument('--out', default=common.DATA)
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    rows, checks = [], []
    for y in a.years or [int(y) for y in S['years']]:
        # 1. Build the year's units and write the persons, the units and the separate returns.
        persons, units, mfs = build(y)
        persons.drop(columns=['SSUID']).to_parquet(f'{a.out}/persons_sipp_{y}.parquet')
        units.to_parquet(f'{a.out}/units_sipp_{y}.parquet')
        mfs.to_parquet(f'{a.out}/mfs_sipp_{y}.parquet')
        # 2. Write the units file that build_table.py reads, and keep the year's summaries.
        u = unit_rows(units, persons, mfs, y)
        u.to_csv(f'{a.out}/sipp_units_{y}.csv.gz', index=False)
        rows.append(summary(u, y))
        common.say(pd.Series(rows[-1]).to_string())
        checks += shared_check(u, persons, y)
    # 3. Write the summary of every year, and print the shared items by year.
    pd.DataFrame(rows).to_csv(f'{a.out}/sipp_summary.csv', index=False)
    with pd.option_context('display.width', 200, 'display.max_columns', 20, 'display.float_format', '{:,.2f}'.format):
        common.say('The shared items per unit (the primary filer\'s plus the spouse\'s), by year:\n' +
                   pd.DataFrame(checks).to_string(index=False))


if __name__ == '__main__':
    main()
