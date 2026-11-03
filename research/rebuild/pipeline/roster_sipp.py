"""roster_sipp.py: the SIPP reader for the shared tax-unit rules (tax_units.py), and the SIPP's tax units with each
adult's wages and tips (settings "sipp"; run.sbatch stage sipp). Everything is in Python: there is no R step.

Sources: one public-use release per reference year (settings sipp.releases; read by sipp_read.py, only the columns it
names). Each release is one row per person and month of the reference year.

Households and people (the CPS's convention: the people at the interview, income for the calendar year before):
  - the household is the sample unit (SSUID) as interviewed: everyone present at the interview (THHLDSTATUS 1
    returning or 2 new member), with ERELRPE, their relationship to the householder at the interview. People who
    left before the interview (partial-year members, movers, THHLDSTATUS 3-6) have no relationship code and are not
    household members (the CPS has no such people either);
  - a sample unit whose interview people hold more than one householder (ERELRPE 1 or 2: the unit split into two
    households by the interview) is divided into one household per householder, each person going with their
    December residence's householder (ERESIDENCEID); the count is printed;
  - each person's age is TAGE (at the interview, as the CPS's ages); weight, the person's December final weight
    (WPFINWGT, month 12; the reference year's last month), or their last month's weight if they have no December row.

What the SIPP observes, and the fallbacks (tax_units.FALLBACKS['sipp']):
  - relationship to the householder (ERELRPE, the same codes in every release from the 2014 panel): 1 householder
    with relatives, 2 householder without, 3 opposite-sex spouse, 4 opposite-sex partner, 5 same-sex spouse,
    6 same-sex partner, 7 child, 8 grandchild, 9 parent, 10 sibling, 11 parent or child in law, 12 brother or sister
    in law, 13 aunt, uncle, niece or nephew, 14 other relative, 15 foster child, 16 housemate or roommate, 17 roomer
    or boarder, 18 other nonrelative. Mapped to Census's PERRP below ("with relatives" for nonrelatives: a spouse,
    parent or child pointer links them to someone else in the household);
  - spouse pointer (EPNSPOUSE), parent pointers (EPNPAR1, EPNPAR2), all at the interview, each kept only when the
    person pointed to is in the household. Census's family codes (HHDFMX, family head and position) are rebuilt
    from them: a related subfamily is a married couple, or a parent with own children under 18 who have never
    married, among the householder's relatives other than the householder's own couple; an unrelated subfamily is
    the same among nonrelatives and partners; its reference person is the householder's blood relative, else the
    man, else the parent;
  - marital status EMS (1 married spouse present, 2 married spouse absent, 3 widowed, 4 divorced, 5 separated,
    6 never married) in the CPS's MARST codes; people under 15 never married;
  - student: enrolled (RENROLL 1) in the reference year's December and no earnings that month (the CPS's rule asks
    "in school and not in the labor force"); disability: any of the six core disability items (RDIS 1, the same six
    questions as the CPS's DIFFANY);
  - income per person over the reference year's 12 months (person-month rows summed): wages (TJBn_MSUM on jobs for
    an employer, class of worker 1-6), self-employment (TPEARN, all earnings, less those wages), investment and
    property income (TPPRPINC), unemployment (TUC1AMT + TUC2AMT), Social Security (TSSSAMT), SSI (TSSI_AMT), other
    income including pensions (TPOTHINC). Gross income is CBO's 3a formula (the taxable part of Social Security as
    the CEX's reader computes it);
  - support answer: none (-1). Every input observed; no assumed-input flag is set.

Tips (production's definition, src/imputations/tips.R and Kendall's SIPP-Data/annualize.R): each person's tips on jobs
for an employer (TJBn_TXAMT, job lines 1-4, class of worker 1-6), summed over the 12 months; tips on
self-employment are left out. Leisure and hospitality (tips_lh): more than settings sipp.lh_share of those tips from
jobs whose industry (TJBn_IND) is in settings sipp.lh_industries. The SIPP's job earnings (TJBn_MSUM) include the
job's tips (checked in every release: a job-month's tips are never above its earnings), so tips are a part of wages,
as on Form W-2.

Separate returns: joint couples split by the CEX's rule (tax_units.mfs_split, a keyed draw with settings
sipp.mfs_salt, dependents with person 1) until their weight reaches SOI Table 1.6's separate returns. Dependent
filers are folded into the unit that claims them: their own income goes nowhere (build_cps.R's rule).

Units file (sipp_units_<year>.csv.gz, one row per tax unit, read by build_table.py), no missing values except the
elective deferrals in years the release does not ask them:
  year, record_id (the unit's uid; a separate return's uid x 10 + person), household_id, weight (the head's),
  married (1 joint), n_dep (capped at 3), dep_filer (0), age1, age2, sex1, sex2, male1, male2 (person 1 and person 2;
  person 1 the man in a mixed-sex couple, the older in a same-sex one, as tax_units.primary_and_spouse; no spouse:
  age2 -1, sex2 0, male2 -1), wages1, wages2, wages (= wages1 + wages2), tips1, tips2, tips_lh1, tips_lh2 (0 or 1;
  0 without tips), elective1, elective2, elective_deferrals (= elective1 + elective2; since 28 October): each adult's
  elective deferrals, missing on every unit in a year the release does not ask them (2015-2019); dividends, gross_ss,
  pensions (since 28 October): the primary filer's plus the spouse's (below).

The shared non-wage items (since 28 October; shared_items), per person, then the primary filer's plus the spouse's on
the unit, as wages (a dependent's own amounts go nowhere; a separate return carries its one person's):
  - dividends: income from stocks and mutual funds (TINC_STMF), a yearly amount repeated in every month row, read
    once (checked: the run stops if more than 1% of the people with any have different values in different months);
  - gross_ss: Social Security received for oneself (TSSSAMT), monthly, before Medicare premiums, summed over the 12
    months; benefits received on behalf of children (TSSCAMT) are left out. The same dollars as the person's ssb;
  - pensions: retirement income items 1-8 (TRET1AMT-TRET8AMT: company or union, federal, state, local, military,
    railroad, reserve and other retirement income), monthly, summed over the months and the items. No IRA or 401(k)
    withdrawals (the public file has only whether, not how much), no lump sums or rollovers.
A missing amount on a person-month is read as none only with an explanation, and each kind is counted in the log: the
person is under 15, or did not say they received it (ESSSMNYN and ERETkMNYN, 1 for a month received and blank
otherwise; for dividends the six questions on owning stocks or mutual funds, alone or jointly). A missing amount where
the person said they received it stops the run.

Elective deferrals (settings deferrals): each person's own contributions to 401(k), 403(b), 503(b) and Thrift Savings
Plan accounts at the main employer (TSCNTAMT_401, a yearly amount repeated in every month: the largest monthly value;
no value in any month read as none, the person not in a plan there; traditional and Roth together), held to the
legal limit of the reference year (the catch-up limit from age 50 at the interview), then to the person's own wages
(the plan's money comes out of them).
Also writes persons_sipp_<year>.parquet, units_sipp_<year>.parquet and mfs_sipp_<year>.parquet (the rules' output, for
check_units.py) and sipp_summary.csv. Usage (through Slurm): python roster_sipp.py [year ...] [--out DIR].
"""
import argparse
import os

import numpy as np
import pandas as pd

import common
import sipp_read as R
import tax_units as T

S = common.SETTINGS['sipp']
REASSIGN = bool(common.SETTINGS.get('tax_units', {}).get('reassign', True))
LH = [tuple(r) for r in S['lh_industries']]
WAGE_CLASSES = [1, 2, 3, 4, 5, 6]              # class of worker: for an employer (7, 8 self-employed)
BLOOD = [7, 8, 9, 10, 13, 14]                  # the householder's relatives by blood (in-laws 11, 12 are not)
RELATIVES = [3, 5, 7, 8, 9, 10, 11, 12, 13, 14]
SHARED_ITEMS = ['dividends', 'gross_ss', 'pensions']   # the units file's shared non-wage items (shared_items)
# Items a release publishes as a wholly blank column although some people say they received them: their amounts are
# unknown and read as zero, counted in the log (28 October). Reference year 2021 (the 2022 release): National Guard or
# reserve retirement, TRET7AMT, blank for all; 3 people (36 person-months, about 20,000 people weighted) say received.
UNPUBLISHED = {(2021, 'TRET7AMT')}


def in_lh(codes):
    c = pd.to_numeric(codes, errors='coerce').to_numpy(float)
    return np.logical_or.reduce([(c >= lo) & (c <= hi) for lo, hi in LH])


def explained(d, item, asked, label, unpublished=False):
    """The person-month values of `item` with every missing value explained, counted and printed, then read as no
    income: the person is under 15 (the SIPP asks income of people 15 and over), the receipt questions (`asked`:
    1 = received; the monthly ones are 1 or blank in every release, never "no") have no answer for that row, or an
    answer other than "received". A missing amount where the person said they received it has no explanation: the
    run stops, saying whether the release left the whole column blank (the item not published)."""
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
    """Per person: the shared non-wage items (since 28 October). Dividends: income from stocks and mutual funds
    (TINC_STMF), a yearly amount repeated in every month row, read once (the person's largest monthly value; the run
    stops if more than 1% of the people with any have different values in different months). Gross Social Security:
    TSSSAMT, monthly, before Medicare premiums, summed over the months (benefits received on behalf of children,
    TSSCAMT, are left out). Pensions: retirement income items 1-8 (TRET1AMT-TRET8AMT), monthly, summed over the months
    and the items."""
    common.say(f'SIPP {year}: the shared items\' missing values, by person-month row')
    key = [d.SSUID, d.PNUM]
    div = pd.Series(explained(d, 'TINC_STMF', R.OWNERSHIP, 'dividends'), index=d.index)
    ss = explained(d, 'TSSSAMT', ['ESSSMNYN'], 'gross Social Security')
    ret = sum(explained(d, f'TRET{k}AMT', [f'ERET{k}MNYN'], f'retirement income item {k}',
                        unpublished=(year, f'TRET{k}AMT') in UNPUBLISHED)
              for k in range(1, R.RET_ITEMS + 1))
    # how often a person's value changes across the months (zero for a yearly amount repeated by month)
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
    """Per person (SSUID, PNUM): the year's income items, wages and tips on jobs for an employer, leisure and
    hospitality tips, December enrolment and earnings, the checks of tips against job earnings, and the shared items
    (shared_items)."""
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
    f = lambda c: d[c].fillna(0).to_numpy(float)
    x = pd.DataFrame({'SSUID': d.SSUID, 'PNUM': d.PNUM, 'wage': wages, 'tips': tips, 'tips_lh_dollars': tips_lh,
                      'earn': f('TPEARN'), 'ssb': f('TSSSAMT'), 'ssi': f('TSSI_AMT'),
                      'uc': f('TUC1AMT') + f('TUC2AMT'), 'property': f('TPPRPINC'), 'other': f('TPOTHINC')})
    y = x.groupby(['SSUID', 'PNUM'], sort=False).sum()
    y['selfemp'] = y.earn - y.wage
    y = y.join(shared_items(d, year))
    assert np.abs(y.own_gross_ss - y.ssb).max() < 0.005, 'gross Social Security differs from ssb'
    item = common.SETTINGS['deferrals']['sipp_item']
    if item in d:   # elective deferrals (28 October): a yearly amount, repeated by month
        g = d.groupby(['SSUID', 'PNUM'], sort=False)[item]
        y['elective_reported'] = g.max().clip(lower=0).reindex(y.index).fillna(0.0).to_numpy()
        y['elective_no_value'] = g.count().reindex(y.index).eq(0).to_numpy()   # no value in any month: none
        y['elective_asked'] = True
    else:
        y['elective_reported'], y['elective_no_value'], y['elective_asked'] = 0.0, True, False
    dec = d[d.MONTHCODE == 12].set_index(['SSUID', 'PNUM'])
    y['enrolled_dec'] = dec.RENROLL.reindex(y.index).eq(1).to_numpy()
    y['earn_dec'] = dec.TPEARN.reindex(y.index).fillna(0).to_numpy()
    last = d.sort_values('MONTHCODE').groupby(['SSUID', 'PNUM']).tail(1).set_index(['SSUID', 'PNUM'])
    y['weight'] = dec.WPFINWGT.reindex(y.index)
    y['weight'] = y.weight.fillna(last.WPFINWGT.reindex(y.index))
    y['resid_dec'] = dec.ERESIDENCEID.reindex(y.index).fillna(last.ERESIDENCEID.reindex(y.index)).to_numpy()
    return y.reset_index(), over


def read_roster(year):
    """The standard member list for one reference year, and the people's tips."""
    d = R.read_release(year)
    py, over = person_year(d, year)
    common.say(f'SIPP {year}: job-months whose tips are above the job\'s earnings: {over}')
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
    # households: the sample unit, split by householder where it holds more than one (see the top)
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
    P['hh'] = ssu * 10 + k                   # the sample unit's number x 10 + which of its householders
    # person numbers: the householder 1, then by PNUM; pointers translated
    P['is_hh'] = hh_flag.to_numpy()
    P = P.sort_values(['hh', 'is_hh', 'PNUM'], ascending=[True, False, True]).reset_index(drop=True)
    P['pn'] = P.groupby('hh').cumcount() + 1
    pn_of = P.set_index(['hh', 'PNUM']).pn
    ptr = lambda c: pd.MultiIndex.from_arrays([P.hh, P[c].fillna(-1).astype(int)]).map(pn_of).fillna(0).astype(int).to_numpy()
    sp = ptr('EPNSPOUSE'); m1 = ptr('EPNPAR1'); m2 = ptr('EPNPAR2')
    # a spouse pointer counts only when it is returned (both point at each other)
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
    ems = P.EMS.fillna(6).astype(int).to_numpy()
    ems = np.where(age < 15, 6, ems)
    married_sp = sp > 0
    marst = np.where(married_sp, 1, np.select([ems == 1, ems == 2, ems == 3, ems == 4, ems == 5], [2, 2, 5, 4, 3], 6))
    r['sp_pn'] = sp; r['mom_pn'] = m1; r['pop_pn'] = m2; r['marst'] = marst
    r['married_sp'] = married_sp; r['married_absent'] = marst == 2
    # the householder's couple
    hsp = pd.Series(np.where(pn == 1, sp, 0)).groupby(hh).transform('max').to_numpy()
    in_hcouple = (pn == 1) | ((pn == hsp) & (hsp > 0))
    # parents of own children under 18 who never married (by either pointer)
    kid = (age < 18) & (marst == 6)
    kids_of = pd.concat([pd.DataFrame({'hh': hh[kid], 'par': m1[kid]}), pd.DataFrame({'hh': hh[kid], 'par': m2[kid]})])
    kids_of = kids_of[kids_of.par > 0]
    n_kids = pd.MultiIndex.from_arrays([hh, pn]).map(kids_of.groupby(['hh', 'par']).size()).fillna(0).to_numpy()
    # subfamilies: couples outside the householder's couple, and lone parents of such children
    relative = np.isin(rel, RELATIVES)
    couple = married_sp & ~in_hcouple
    lone = ~married_sp & ~in_hcouple & (n_kids > 0) & (pn != 1)
    sp_rel = pd.MultiIndex.from_arrays([hh, sp]).map(pd.Series(rel, index=pd.MultiIndex.from_arrays([hh, pn]))) \
        .fillna(0).to_numpy().astype(int)
    sp_sex = pd.MultiIndex.from_arrays([hh, sp]).map(pd.Series(r.sex.to_numpy(), index=pd.MultiIndex.from_arrays([hh, pn]))) \
        .fillna(0).to_numpy().astype(int)
    blood, sp_blood = np.isin(rel, BLOOD), np.isin(sp_rel, BLOOD)
    # the couple's reference person: the householder's blood relative; if both or neither, the man; same sex, lower pn
    ref_couple = couple & ((blood & ~sp_blood) | ((blood == sp_blood) & ((r.sex.to_numpy() == 1) & (sp_sex == 2) |
                                                                          ((r.sex.to_numpy() == sp_sex) & (pn < sp)))))
    sf_ref = ref_couple | lone
    sf_head = np.where(sf_ref, pn, 0)
    sf_head = np.where(couple & ~ref_couple, sp, sf_head)                     # a couple's other member: its reference
    # children of a subfamily: never-married under 18 whose parent is a subfamily member, outside the householder's
    # couple's own children
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
    # "with relatives" for nonrelatives: linked by a spouse, parent or child pointer to someone else in the household
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
    # in a related subfamily a nonrelative member (an in-law partner, say) takes the "other relative" codes
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
    # student and disability
    r['in_school'] = (P.enrolled_dec.to_numpy() & (P.earn_dec.to_numpy() <= 0))
    r['nilf_school'] = False; r['ill_disabled'] = False; r['has_disab_income'] = False
    r['diff_any'] = (P.RDIS == 1).to_numpy()
    # incomes (CBO's 3a)
    for c in T.INCOME_ITEMS:
        r[c] = 0.0
    r['wage'] = P.wage.to_numpy(); r['selfemp'] = P.selfemp.to_numpy(); r['interest'] = P.property.to_numpy()
    r['uc'] = P.uc.to_numpy(); r['ssb'] = P.ssb.to_numpy(); r['ssi'] = P.ssi.to_numpy()
    r['pension'] = P.other.to_numpy()
    gi0 = r.wage + r.selfemp + r.interest + r.uc + r.pension
    ssb = r.ssb.to_numpy()
    combined = np.maximum(gi0 + 0.5 * ssb, 0)
    t1 = np.where(marst == 1, 32000, 25000); t2 = np.where(marst == 1, 44000, 34000)
    taxable = np.zeros(len(r))
    mid = (combined > t1) & (combined <= t2)
    taxable[mid] = np.minimum(0.5 * ssb, 0.5 * (combined - t1))[mid]
    tier1 = np.minimum(0.5 * ssb, 0.5 * (t2 - t1))
    hi = combined > t2
    taxable[hi] = np.minimum(0.85 * ssb, 0.85 * (combined - t2) + tier1)[hi]
    r['gi'] = gi0 + np.maximum(taxable, 0)
    r['support'] = -1
    r['gi_obs'] = True
    for c in T.ASSUMED_FLAGS:
        r[c] = False
    # passthrough: tips
    r['tips'] = P.tips.to_numpy(); r['tips_lh_dollars'] = P.tips_lh_dollars.to_numpy()
    # elective deferrals: held to the year's legal limit (catch-up from age 50), then to the person's own wages
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
    # passthrough: the shared items (since 28 October; shared_items)
    for c in ['own_dividends', 'own_gross_ss', 'own_pensions']:
        r[c] = P[c].to_numpy(float)
    r['SSUID'] = P.SSUID.to_numpy(); r['PNUM'] = P.PNUM.astype(int).to_numpy()
    assert not r.isna().any().any(), r.isna().sum()[r.isna().sum() > 0]
    assert (r.groupby('hh').pn.min() == 1).all()
    common.say(f'  {len(r):,} people in {r.hh.nunique():,} households, {r.weight.sum() / 1e6:.1f} m weighted; '
               f'related subfamilies {int((related_sf & (pos == "ref")).sum())}, unrelated {int((unrelated_sf & (pos == "ref")).sum())}')
    return r


def unit_rows(units, persons, mfs, year):
    """The units file (see the top): joint couples split into separate returns replaced by their two returns;
    dependents' own returns dropped (folded into the claiming unit). Person 1 and person 2 by
    tax_units.primary_and_spouse; each person's wages and tips."""
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
    # elective deferrals: missing on every unit in a year the release does not ask them
    asked = bool(persons.elective_asked.all())
    assert asked or not persons.elective_asked.any()
    out['elective_deferrals'] = out.elective1 + out.elective2
    if not asked:
        out[['elective1', 'elective2', 'elective_deferrals']] = np.nan
    for c in SHARED_ITEMS:        # the primary filer's plus the spouse's (since 28 October)
        out[c] = shared[c]
    assert (out.loc[out.married == 0, ['wages2', 'tips2', 'tips_lh2']] == 0).all().all()
    assert ((out.age2 >= 0) == (out.married == 1)).all()
    assert not out.drop(columns=['elective1', 'elective2', 'elective_deferrals']).isna().any().any()
    assert asked == out.elective_deferrals.notna().all() and not out.record_id.duplicated().any()
    return out


def build(year):
    import targets as TG
    r = read_roster(year)
    persons, units = T.build_units(r, year, reassign_pass=REASSIGN)
    target = sum(TG.table_1_6(year)[('separate', k)] for k in range(6))
    mu = units[~units.dep_return].reset_index(drop=True)
    chosen, share, mfs = T.mfs_split(mu, persons, np.ones(len(mu), bool), target, dep_rule='primary',
                                     salt=int(S['mfs_salt']))
    common.say(f'SIPP {year}: SOI Table 1.6 separate returns {target / 1e6:.3f} m; {len(chosen)} couples split '
               f'({share:.4f} of joint couples by weight)')
    units['mfs_split'] = units.uid.isin(chosen)
    return persons, units, mfs


def summary(u, year):
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
    """The shared items per year: weighted total, share of units with any, the holders' median, units at $10,000
    and over and at $100,000 and over; and the people's total (every person in a unit, by the person's own weight),
    whose difference from the units' total is mostly dependents' own amounts, which go nowhere."""
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


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('years', type=int, nargs='*')
    ap.add_argument('--out', default=common.DATA)
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    rows, checks = [], []
    for y in a.years or [int(y) for y in S['years']]:
        persons, units, mfs = build(y)
        persons.drop(columns=['SSUID']).to_parquet(f'{a.out}/persons_sipp_{y}.parquet')
        units.to_parquet(f'{a.out}/units_sipp_{y}.parquet')
        mfs.to_parquet(f'{a.out}/mfs_sipp_{y}.parquet')
        u = unit_rows(units, persons, mfs, y)
        u.to_csv(f'{a.out}/sipp_units_{y}.csv.gz', index=False)
        rows.append(summary(u, y))
        common.say(pd.Series(rows[-1]).to_string())
        checks += shared_check(u, persons, y)
    pd.DataFrame(rows).to_csv(f'{a.out}/sipp_summary.csv', index=False)
    with pd.option_context('display.width', 200, 'display.max_columns', 20, 'display.float_format', '{:,.2f}'.format):
        common.say('The shared items per unit (the primary filer\'s plus the spouse\'s), by year:\n' +
                   pd.DataFrame(checks).to_string(index=False))


if __name__ == '__main__':
    main()
