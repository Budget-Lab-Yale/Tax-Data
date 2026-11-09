"""The CPS reader for the shared tax-unit rules (tax_units.py). It turns the CPS ASEC, as IPUMS distributes it, into
the standard member list that the rules read.

The tax-unit rules are CBO's, and CBO's code reads the Census Bureau's own CPS files. Two of the Census codes it uses
are not in the IPUMS files: PERRP, each person's detailed relationship to the household's reference person, and
HHDFMX, each person's detailed position in a family. This reader rebuilds them, and the family pointers, from IPUMS's
relationship and family variables. It then computes each person's income items as CBO's 3a_compute_incomes.do does,
as far as the IPUMS files carry the income sources. The comments below name each input IPUMS lacks and what stands
in for it. TAX_UNITS.md describes the member list and the rules.
"""
import re

import numpy as np
import pandas as pd

from prep.tax_units import ASSUMED_FLAGS, taxable_social_security

RAW = '/nfs/roberts/project/pi_nrs36/shared/raw_data/CPS-ASEC/cps_asec_common'


# ---- Reading the IPUMS extract ----

def _read_fixed_width(survey_year):
    """The IPUMS CPS ASEC extract of March survey_year, with every variable read as a number."""
    d = f'{RAW}/cps{survey_year}_03s'
    v = pd.read_csv(f'{d}/variables.csv')
    colspecs = [(int(s) - 1, int(e)) for s, e in zip(v.start, v.end)]
    df = pd.read_fwf(f'{d}/cps_{survey_year}_03s.dat.gz', colspecs=colspecs, names=list(v.var_name), header=None,
                     dtype=str, compression='gzip')
    for n, dec in zip(v.var_name, v.imp_decim):
        x = pd.to_numeric(df[n], errors='raise')
        df[n] = x / 10 ** int(dec) if int(dec) else x
    return df


def _clean_income(z):
    """An IPUMS income column with its code for "not in universe" set to 0. That code is all nines, the column's
    largest value. A second code for "missing" (nines ending in 8) sits just below it and is also set to 0."""
    z = z.astype(float).copy()
    mx = z.max()
    if mx > 0 and re.fullmatch(r'9+8?', str(int(mx))):
        z[z == mx] = 0.0
        mx2 = z.max()
        if mx2 > 0 and re.fullmatch(r'9+8', str(int(mx2))):
            z[z == mx2] = 0.0
    return z


# ---- The member list ----

def read_roster(tax_year):
    """The member list for one tax year, from the ASEC of March tax_year + 1.

    Ages are as reported at the March interview, as in CBO's code. IPUMS's parent pointers name the same parents as
    Census's for 98% of people in 2024."""
    # 1. Read the extract and keep each person's household, person number, weight, demographics and pointers to
    #    their spouse and parents.
    x = _read_fixed_width(tax_year + 1)
    r = pd.DataFrame({'hh': x.SERIAL.astype(np.int64), 'pn': x.PERNUM.astype(int), 'age': x.AGE.astype(int),
                      'sex': x.SEX.astype(int), 'weight': x.ASECWT.astype(float), 'gq': (x.GQ == 2),
                      'relate': x.RELATE.astype(int), 'ftype': x.FTYPE.astype(int), 'famrel': x.FAMREL.astype(int),
                      'famid': x.FAMID.astype(int), 'sp_pn': x.SPLOC.astype(int), 'mom_pn': x.MOMLOC.astype(int),
                      'pop_pn': x.POPLOC.astype(int), 'marst': x.MARST.astype(int), 'hseq': x.HSEQ.astype(int),
                      'lineno': x.LINENO.astype(int)})
    assert not r[['hh', 'pn']].duplicated().any()
    assert (r.groupby('hh').pn.min() == 1).all()
    assert not r.isna().any().any(), r.isna().sum()[r.isna().sum() > 0]

    # 2. Each person's relationship to the reference person in Census's terms (PERRP). An unmarried partner
    #    (RELATE 1114, used before the 2020 survey) is split by sex as Census does: codes 43 and 44 for a partner of
    #    the opposite sex, 46 and 47 for one of the same sex. Census also splits partners, housemates, roomers and
    #    other nonrelatives by whether they have relatives in the household, and IPUMS does not. Here a person has
    #    relatives when a spouse or parent pointer links them to someone else in the household, or when they are in
    #    an unrelated subfamily. This matches Census exactly for partners in March 2024, and for about 90% of the
    #    others.
    head_sex = r.hh.map(r[r.pn == 1].set_index('hh').sex)
    linked = set()
    # IPUMS's spouse pointer (SPLOC) also points at unmarried partners, who are not relatives. Only a married
    # person's spouse pointer counts.
    for c in ['sp_pn', 'mom_pn', 'pop_pn']:
        t = r[(r[c] > 0) & ((c != 'sp_pn') | (r.marst == 1))]
        linked |= set(zip(t.hh, t.pn)) | set(zip(t.hh, t[c]))
    with_rel = np.array([(h, p) in linked for h, p in zip(r.hh, r.pn)]) | (r.ftype == 4).to_numpy()
    rel = r.relate.to_numpy()
    same_sex = (r.sex == head_sex).to_numpy()
    partner_os = (rel == 1116) | ((rel == 1114) & ~same_sex) | ((rel == 1113) & ~same_sex)
    partner_ss = (rel == 1117) | ((rel == 1114) & same_sex) | ((rel == 1113) & same_sex)
    perrp = np.select(
        [rel == 101, np.isin(rel, [201, 202]) & ~same_sex, np.isin(rel, [201, 202, 203]), partner_os, partner_ss,
         np.isin(rel, [301, 303]), rel == 901, rel == 501, rel == 701, rel == 1001, rel == 1242, rel == 1115,
         rel == 1241, rel == 1260],
        [np.where(with_rel, 40, 41), 42, 45, np.where(with_rel, 43, 44), np.where(with_rel, 46, 47), 48, 49, 50, 51,
         52, 53, np.where(with_rel, 54, 55), np.where(with_rel, 56, 57), np.where(with_rel, 58, 59)], -1)
    bad = perrp < 0
    if bad.any():
        raise ValueError(f'RELATE codes without a PERRP equivalent: {pd.Series(rel[bad]).value_counts().to_dict()}')
    r['perrp'] = perrp

    # 3. Each person's family position in Census's terms (HHDFMX), from the family type and the person's place in
    #    the family (FTYPE, FAMREL) and their relationship to the householder. Census splits some groups by whether
    #    the person is young or an adult and single or married. No rule uses those splits, so one code stands for
    #    each of CBO's groups. In March 2024 every person's group matches Census's HHDFMX.
    ft, fr = r.ftype.to_numpy(), r.famrel.to_numpy()
    kin = np.select([np.isin(rel, [301, 303]), rel == 901], ['child', 'grandchild'], 'other')
    in_sf = ft == 3
    code_pf = {('child', 'none'): 4, ('child', 'ref'): 3, ('child', 'sp'): 6,
               ('grandchild', 'none'): 25, ('grandchild', 'ref'): 23, ('grandchild', 'sp'): 27, ('grandchild', 'kid'): 24,
               ('other', 'none'): 37, ('other', 'ref'): 35, ('other', 'sp'): 39, ('other', 'kid'): 36}
    pos = np.select([in_sf & (fr == 1), in_sf & (fr == 2), in_sf & (fr == 3)], ['ref', 'sp', 'kid'], 'none')
    pf_code = np.array([code_pf.get((k, p), -1) for k, p in zip(kin, pos)])
    hhdfmx = np.select(
        [r.gq.to_numpy(), (ft == 1) & (fr == 1), (ft == 1) & (fr == 2), np.isin(ft, [1, 3]), ft == 4, ft == 2,
         ft == 5],
        [51, 1, 2, pf_code, np.select([fr == 1, fr == 2], [46, 47], 48), 49, 50], -1)
    bad = hhdfmx < 0
    if bad.any():
        raise ValueError(f'{bad.sum()} people without an HHDFMX equivalent')
    r['hhdfmx'] = hhdfmx

    # 4. Each person's family head (fheadidx) and family number within the household (ffpos). People in group
    #    quarters who are not in a family head their own.
    fref = r[r.famrel == 1].groupby(['hh', 'famid']).pn.min()
    r['fheadidx'] = pd.MultiIndex.from_arrays([r.hh, r.famid]).map(fref)
    r.loc[r.ftype.isin([2, 5]) & ~r.gq, 'fheadidx'] = r.pn
    r.loc[r.fheadidx.isna(), 'fheadidx'] = r.pn
    r['fheadidx'] = r.fheadidx.astype(int)
    r['ffpos'] = r.famid

    # 5. Marital status in CBO's terms: married with the spouse present (Census A_MARITL 1-2) and married with the
    #    spouse absent (A_MARITL 3).
    r['married_sp'] = (r.marst == 1) & (r.sp_pn > 0)
    r['married_absent'] = r.marst == 2

    # 6. The inputs to the student and disability tests (CBO's 4a_flag_dependents.do). A person is in school when
    #    enrolled in high school or college and not in the labor force. This is the nearest IPUMS item to Census's
    #    PRNLFSCH; 95% of the people Census marks in school match it (March 2024). Census's PYRSN (not working
    #    because of school) is not in the extract.
    in_school = x.SCHLCOLL.astype(int).isin([1, 2, 3, 4]) & (x.LABFORCE.astype(int) == 1)
    r['in_school'] = in_school.to_numpy()
    r['nilf_school'] = (x.WHYNWLY.astype(int) == 4).to_numpy()      # Census RSNNOTW 4
    r['ill_disabled'] = (x.WHYNWLY.astype(int) == 2).to_numpy()     # Census RSNNOTW 1
    r['diff_any'] = (x.DIFFANY.astype(int) == 2).to_numpy()         # any PEDIS* = 1 (exact in 2024)

    # 7. Each person's income items (CBO's 3a_compute_incomes.do). Survivor, disability and retirement income
    #    arrive in up to two amounts, each with a code for its source, and are sorted into CBO's items by source.
    inc = {c: _clean_income(x[c]) for c in x.columns if c.startswith('INC') and c not in ('INCTOT',)}
    z = lambda c: inc[c] if c in inc else pd.Series(0.0, index=x.index)
    src = lambda c: x[c].astype(int) if c in x.columns else pd.Series(0, index=x.index)
    surv = [(z('INCSURV1'), src('SRCSURV1')), (z('INCSURV2'), src('SRCSURV2'))]
    disa = [(z('INCDISA1'), src('SRCDISA1')), (z('INCDISA2'), src('SRCDISA2'))]
    by = lambda pairs, codes: sum(v * s.isin(codes) for v, s in pairs)
    wage = z('INCWAGE'); farm = z('INCFARM'); selfemp = z('INCBUS') + farm
    # From the March 2019 survey on, IPUMS's interest (INCINT) includes interest earned inside retirement accounts
    # (INCRINT1 and INCRINT2). CBO's interest (Census's TRDINT_VAL) does not, and it is taken off here.
    interest = z('INCINT') - z('INCRINT1') - z('INCRINT2'); dividends = z('INCDIVID'); rents = z('INCRENT')
    est_trust = by(surv, [8])
    if 'INCPEN1' in inc:            # the redesigned survey (March 2019 on)
        pens = [(z('INCPEN1'), src('SRCPEN1')), (z('INCPEN2'), src('SRCPEN2'))]
        annuity = z('INCRANN') + by(surv, [9])
        # CBO leaves out early distributions taken at age 57 or younger. IPUMS does not report them separately, so
        # retirement account distributions count only from age 58.
        distributions = (z('INCRET1') + z('INCRET2')) * (x.AGE >= 58)
        pension_items = by(pens, [1, 2, 3, 4, 5, 6, 8])
        rr_pen = by(pens, [7])
    else:                           # the earlier survey
        # Retirement income INCRETI1 and INCRETI2, each with its source: 1 company or union, ..., 5 railroad,
        # 6 annuities, 7 IRA, 8 other.
        rets = [(z('INCRETI1'), src('SRCRETI1')), (z('INCRETI2'), src('SRCRETI2'))]
        annuity = by(rets, [6]) + by(surv, [9])
        distributions = by(rets, [7])
        pension_items = by(rets, [1, 2, 3, 4, 8])
        rr_pen = by(rets, [5])
    pension = annuity + distributions + pension_items + by(surv, [1, 10, 2, 3, 4]) + by(disa, [2, 10, 3, 4, 5])
    rr_pension = rr_pen + by(surv, [5]) + by(disa, [6])
    uc = z('INCUNEMP') + by(disa, [9])
    ssb = z('INCSS'); ssi = z('INCSSI'); vet = z('INCVET')
    wc = z('INCWKCOM') + by(disa, [1, 7, 8]) + by(surv, [6, 7])
    # 8. Gross income by CBO's definition: the items above plus the taxable part of Social Security and railroad
    #    retirement benefits. IPUMS's other income has no source code. CBO moves some of it into wages, interest and
    #    the like by source; here it stays out of gross income.
    gi0 = wage + selfemp + interest + dividends + rents + est_trust + pension + uc
    ss_rr = ssb + rr_pension
    taxable = taxable_social_security(gi0, ss_rr, r.marst.to_numpy() == 1)
    for k, v in dict(wage=wage, selfemp=selfemp, farm_selfemp=farm, interest=interest, dividends=dividends,
                     rents=rents, est_trust=est_trust, pension=pension, uc=uc, ssb=ssb, rr_pension=rr_pension,
                     ssi=ssi, va_benefits=vet, wc=wc, other_income=z('INCOTHER')).items():
        r[k] = np.asarray(v, dtype=float)
    r['gi'] = np.asarray(gi0 + taxable, dtype=float)
    # 9. Whether the person has disability income (an input to the disability test, 4a): SSI paid for disability or
    #    blindness, Social Security paid for disability, or any other disability income with a source. Veterans'
    #    disability compensation is not in the extract.
    disab_income = (ssi * (ssi > 0) * (src('WHYSSI1').isin([1, 2]) | src('WHYSSI2').isin([1, 2]))
                    + ssb * (ssb > 0) * ((src('WHYSS1') == 2) | (src('WHYSS2') == 2))
                    + z('INCDISAB') * (z('INCDISAB') > 0) * ((src('SRCDISA1') > 0) | (src('SRCDISA2') > 0)))
    r['has_disab_income'] = np.asarray(disab_income > 0)
    r['year'] = tax_year
    # 10. The CPS has no question on financial support (-1 means not asked) and observes every input the rules use,
    #     so no input is flagged as assumed.
    r['survey'] = 'cps'
    r['support'] = -1
    r['gi_obs'] = True
    for c in ASSUMED_FLAGS:
        r[c] = False
    assert not r.isna().any().any(), r.isna().sum()[r.isna().sum() > 0]
    return r.reset_index(drop=True)
