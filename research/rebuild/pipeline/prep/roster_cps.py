"""roster_cps.py: the CPS reader for the shared tax-unit rules (tax_units.py). It turns an IPUMS CPS ASEC person file
into the standard member list the rules read (one row per person; the columns are listed in tax_units.py's header).

The CPS observes every input the rules use, so it needs no fallbacks (tax_units.FALLBACKS['cps']); the stand-ins for
the few Census variables IPUMS lacks are listed in tax_units.py's header and below. Moved out of tax_units.py on
24 October 2026 without any change to what it computes: step 1's units come out identical, unit for unit.

read_roster(tax_year)  the member list for one tax year (the ASEC of March tax_year + 1). CBO reads the Census Bureau's
                       CSV files; the member list rebuilds the Census codes CBO's rules read (HHDFMX, the detailed
                       family position; PERRP, the detailed relationship to the reference person; the family head and
                       family position pointers) from IPUMS's RELATE, FTYPE, FAMREL, FAMID and the spouse and parent
                       pointers. Incomes follow 3a_compute_incomes.do as far as IPUMS carries the source codes.
"""
import re

import numpy as np
import pandas as pd

from tax_units import ASSUMED_FLAGS, taxable_social_security

RAW = '/nfs/roberts/project/pi_nrs36/shared/raw_data/CPS-ASEC/cps_asec_common'

def _read_fixed_width(survey_year):
    """The IPUMS CPS ASEC extract of March survey_year, every variable as a number."""
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
    """An IPUMS income column with its not-in-universe sentinel (all nines, the column's maximum) set to 0."""
    z = z.astype(float).copy()
    mx = z.max()
    if mx > 0 and re.fullmatch(r'9+8?', str(int(mx))):
        z[z == mx] = 0.0
        mx2 = z.max()       # a second sentinel (…98 "missing") below the first
        if mx2 > 0 and re.fullmatch(r'9+8', str(int(mx2))):
            z[z == mx2] = 0.0
    return z


def read_roster(tax_year):
    """The standard person roster for one tax year (the ASEC of March tax_year + 1).

    Columns: hh, pn (person number, the pointers' key), age, sex, weight (ASECWT), gq, relate, ftype, famrel, famid,
    sp_pn, mom_pn, pop_pn, marst; CBO's codes hhdfmx, perrp, fheadidx (person number of the family's reference
    person), ffpos (= FAMID); student and disability flags; CBO's income items and gross income gi.
    """
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

    # --- CBO's family codes from IPUMS -------------------------------------------------------------------------
    # PERRP. RELATE 1114 (unmarried partner, before the 2020 survey) is split by sex as Census does (43/44 opposite
    # sex, 46/47 same sex); "with relatives" as described in the module header.
    head_sex = r.hh.map(r[r.pn == 1].set_index('hh').sex)
    linked = set()
    # IPUMS's SPLOC also points at unmarried partners (March 2024); a partner is not a relative, so only spouses count
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

    # HHDFMX: family type and position (FTYPE, FAMREL) and relationship to the householder. Codes stand for CBO's
    # groups (the young/adult and single/married splits are not used by any rule, so one code per group is used:
    # child 4 / subfamily reference 3 / its spouse 6; grandchild 25 / 23 / 27 / child of subfamily 24; other relative
    # 37 / 35 / 39 / 36). Measured on March 2024: every person's group matches Census's HHDFMX.
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
    # family head (fheadidx) and family position (ffpos)
    fref = r[r.famrel == 1].groupby(['hh', 'famid']).pn.min()
    r['fheadidx'] = pd.MultiIndex.from_arrays([r.hh, r.famid]).map(fref)
    r.loc[r.ftype.isin([2, 5]) & ~r.gq, 'fheadidx'] = r.pn
    r.loc[r.fheadidx.isna(), 'fheadidx'] = r.pn         # group quarters people outside a family
    r['fheadidx'] = r.fheadidx.astype(int)
    r['ffpos'] = r.famid
    # marital status in CBO's terms: married with spouse present (A_MARITL 1-2) and married spouse absent (3)
    r['married_sp'] = (r.marst == 1) & (r.sp_pn > 0)
    r['married_absent'] = r.marst == 2

    # --- school and disability (4a) ----------------------------------------------------------------------------
    # in school: enrolled in high school or college and not in the labor force, nearest to Census's PRNLFSCH (asked of
    # people not in the labor force)
    in_school = x.SCHLCOLL.astype(int).isin([1, 2, 3, 4]) & (x.LABFORCE.astype(int) == 1)
    r['in_school'] = in_school.to_numpy()
    r['nilf_school'] = (x.WHYNWLY.astype(int) == 4).to_numpy()      # RSNNOTW 4
    r['ill_disabled'] = (x.WHYNWLY.astype(int) == 2).to_numpy()     # RSNNOTW 1
    r['diff_any'] = (x.DIFFANY.astype(int) == 2).to_numpy()         # any PEDIS* = 1 (exact in 2024)

    # --- incomes (3a) ------------------------------------------------------------------------------------------
    inc = {c: _clean_income(x[c]) for c in x.columns if c.startswith('INC') and c not in ('INCTOT',)}
    z = lambda c: inc[c] if c in inc else pd.Series(0.0, index=x.index)
    src = lambda c: x[c].astype(int) if c in x.columns else pd.Series(0, index=x.index)
    surv = [(z('INCSURV1'), src('SRCSURV1')), (z('INCSURV2'), src('SRCSURV2'))]
    disa = [(z('INCDISA1'), src('SRCDISA1')), (z('INCDISA2'), src('SRCDISA2'))]
    by = lambda pairs, codes: sum(v * s.isin(codes) for v, s in pairs)
    wage = z('INCWAGE'); farm = z('INCFARM'); selfemp = z('INCBUS') + farm
    # from March 2019 IPUMS's INCINT includes interest earned inside retirement accounts (INCRINT1/2), which CBO's
    # interest (TRDINT_VAL) leaves out
    interest = z('INCINT') - z('INCRINT1') - z('INCRINT2'); dividends = z('INCDIVID'); rents = z('INCRENT')
    est_trust = by(surv, [8])
    if 'INCPEN1' in inc:            # the redesigned survey (March 2019 on)
        pens = [(z('INCPEN1'), src('SRCPEN1')), (z('INCPEN2'), src('SRCPEN2'))]
        annuity = z('INCRANN') + by(surv, [9])
        # CBO's distributions (DST_VAL1/2) leave out the early distributions of people 57 and under (DST_VAL*_YNG),
        # which IPUMS's INCRET1/2 include: here, distributions count from age 58
        distributions = (z('INCRET1') + z('INCRET2')) * (x.AGE >= 58)
        pension_items = by(pens, [1, 2, 3, 4, 5, 6, 8])
        rr_pen = by(pens, [7])
    else:                           # INCRETI1/2 by source (1 company/union ... 5 railroad, 6 annuities, 7 IRA, 8 other)
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
    gi0 = wage + selfemp + interest + dividends + rents + est_trust + pension + uc
    ss_rr = ssb + rr_pension
    taxable = taxable_social_security(gi0, ss_rr, r.marst.to_numpy() == 1)
    for k, v in dict(wage=wage, selfemp=selfemp, farm_selfemp=farm, interest=interest, dividends=dividends,
                     rents=rents, est_trust=est_trust, pension=pension, uc=uc, ssb=ssb, rr_pension=rr_pension,
                     ssi=ssi, va_benefits=vet, wc=wc, other_income=z('INCOTHER')).items():
        r[k] = np.asarray(v, dtype=float)
    r['gi'] = np.asarray(gi0 + taxable, dtype=float)
    # disability income (4a): SSI for disability or blindness, Social Security for disability, any other disability
    # income with a source; veterans' disability compensation is not in the extract
    disab_income = (ssi * (ssi > 0) * (src('WHYSSI1').isin([1, 2]) | src('WHYSSI2').isin([1, 2]))
                    + ssb * (ssb > 0) * ((src('WHYSS1') == 2) | (src('WHYSS2') == 2))
                    + z('INCDISAB') * (z('INCDISAB') > 0) * ((src('SRCDISA1') > 0) | (src('SRCDISA2') > 0)))
    r['has_disab_income'] = np.asarray(disab_income > 0)
    r['year'] = tax_year
    # the standard member list's survey columns: the CPS has no support answer of its own (-1, not asked), and
    # every input is observed, so no person carries an assumed input
    r['survey'] = 'cps'
    r['support'] = -1
    r['gi_obs'] = True
    for c in ASSUMED_FLAGS:
        r[c] = False
    assert not r.isna().any().any(), r.isna().sum()[r.isna().sum() > 0]
    return r.reset_index(drop=True)
