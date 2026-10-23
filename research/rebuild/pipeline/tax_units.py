"""tax_units.py: tax units from an IPUMS CPS ASEC person file, by CBO's public algorithm (US-CBO/CPS-tax-filing-units,
Stata), translated rule by rule (research/rebuild/TAX_UNITS.md, section 4).

Steps, with the Stata module each one translates:
  read_ipums_roster(tax_year)  the standard person roster from the IPUMS extract. CBO reads the Census Bureau's CSV
                               files; the roster rebuilds the Census codes CBO's rules read (HHDFMX, the detailed
                               family position; PERRP, the detailed relationship to the reference person; the family
                               head and family position pointers) from IPUMS's RELATE, FTYPE, FAMREL, FAMID and the
                               spouse and parent pointers. Incomes follow 3a_compute_incomes.do as far as IPUMS
                               carries the source codes.
  flag_dependents              4a_flag_dependents.do: qualifying child (QC) and qualifying relative (QR) tests.
  assign_units                 4b_identify_tax_unit_relationships.do: every person to one unit, as head (1), spouse
                               (2), qualifying child (3) or qualifying relative (4).
  reassign                     4c_reassign_relationships.do, the optional reassignment pass (settings: reassign).
  collapse_units               4e_collapse_to_tax_units.do: one row per unit, plus a dependent's own return for every
                               dependent with income (gross income, wages or self-employment not zero).
  puf_terms                    the units in the PUF's terms: filing status, dependents by type under the PUF's caps
                               and type order, their ages youngest first, child credit and EITC children.

Married filing separately (mfs_split, a post-filing step, off by default: settings cps.mfs_split): a uniform share
of joint filer couples, chosen by a draw keyed by unit id, become two separate returns, so that the count of
separate returns equals SOI's (Table 1.6), as nonfiler_pool/07_mfs_split.R does with weights.

Variants: reassign=True is CBO's default ("cbo"); reassign=False applies the first pass only ("strict rules").

Inputs IPUMS lacks, and what is used instead (listed in full in the 2023 reproduction report):
  - PRNLFSCH (in school, asked of people not in the labor force): SCHLCOLL 1-4 (enrolled in high school or college)
    and LABFORCE 1 (not in the labor force), setting student_rule ('nilf_enrolled', the default; 'enrolled' and
    'fulltime' drop the labor-force condition). Measured on March 2024: 95% of PRNLFSCH = 1 match. PYRSN (reason for not working the rest of the year = school): not in the extract, dropped.
  - VET_TYP1 (veterans' disability compensation): not in the extract; dropped from the disability-income test.
  - PERRP's "with relatives" split for unmarried partners, housemates, roomers and other nonrelatives: a person has
    relatives when a spouse or parent pointer links them to someone else in the household, or they are in an
    unrelated subfamily (measured on March 2024: exact for partners; about 90% for the others).
  - "Other income" (OI_VAL) with its source code (OI_OFF): IPUMS's INCOTHER has no source code, so the pieces CBO
    moves into wages, interest, Social Security and the like stay out of gross income.
  - Early retirement distributions (DST_VAL*_YNG, people 57 and under), which CBO leaves out of gross income: not
    separate in IPUMS; distributions (INCRET1/2) are counted from age 58.
  - Pointers: IPUMS's MOMLOC/POPLOC agree with Census's PEPAR1/PEPAR2 as sets for 98% of people in 2024.
Ages are as the ASEC reports them (at the March interview), as in CBO's code.

Usage (through Slurm): python tax_units.py <tax_year> [--strict] [--keep-gq] [--out DIR]
Writes persons_<year>_<variant>.parquet (one row per person: unit, role, dependent type) and
units_<year>_<variant>.parquet (one row per unit, dependents' own returns flagged and linked).
"""
import argparse
import gzip
import os
import re
import xml.etree.ElementTree as ET

import numpy as np
import pandas as pd

RAW = '/nfs/roberts/project/pi_nrs36/shared/raw_data/CPS-ASEC/cps_asec_common'

# CBO's 1b_config.do: the qualifying-relative income limit (the personal exemption) and the married-filing-separately
# standard deduction used as a support proxy. 2015-2017 added here from the Form 1040 instructions.
PARAMS = {2015: (4000, 6300), 2016: (4050, 6300), 2017: (4050, 6350), 2018: (4150, 12000), 2019: (4200, 12200),
          2020: (4300, 12400), 2021: (4300, 12550), 2022: (4400, 12950), 2023: (4700, 13850), 2024: (5050, 14600),
          2025: (5200, 15750)}
PRT_INC_FCT = 2          # head's gross income as a multiple of an unmarried partner's (support proxy)
INCOME_TOLERANCE = 0

# PUF caps on dependents shown, by filing status (AGES.md step 3)
PUF_CAP = {'single': 2, 'joint': 3, 'hoh': 3, 'separate': 1}
DEP_TYPES = ['child_home', 'child_away', 'other', 'parent']    # the PUF's type order


# ---------------------------------------------------------------------------------------------------------------
# Reading the IPUMS extract
# ---------------------------------------------------------------------------------------------------------------
def _read_fixed_width(survey_year, cache=None):
    if cache and os.path.exists(cache):
        return pd.read_parquet(cache)
    d = f'{RAW}/cps{survey_year}_03s'
    v = pd.read_csv(f'{d}/variables.csv')
    colspecs = [(int(s) - 1, int(e)) for s, e in zip(v.start, v.end)]
    df = pd.read_fwf(f'{d}/cps_{survey_year}_03s.dat.gz', colspecs=colspecs, names=list(v.var_name), header=None,
                     dtype=str, compression='gzip')
    for n, dec in zip(v.var_name, v.imp_decim):
        x = pd.to_numeric(df[n], errors='raise')
        df[n] = x / 10 ** int(dec) if int(dec) else x
    if cache:
        df.to_parquet(cache)
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


def read_ipums_roster(tax_year, student_rule='nilf_enrolled', cache_dir=None):
    """The standard person roster for one tax year (the ASEC of March tax_year + 1).

    Columns: hh, pn (person number, the pointers' key), age, sex, weight (ASECWT), gq, relate, ftype, famrel, famid,
    sp_pn, mom_pn, pop_pn, marst; CBO's codes hhdfmx, perrp, fheadidx (person number of the family's reference
    person), ffpos (= FAMID); student and disability flags; CBO's income items and gross income gi.
    """
    sy = tax_year + 1
    cache = f'{cache_dir}/ipums_{sy}.parquet' if cache_dir else None
    x = _read_fixed_width(sy, cache)
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
    schl = x.SCHLCOLL.astype(int)
    if student_rule == 'enrolled':
        in_school = schl.isin([1, 2, 3, 4])
    elif student_rule == 'fulltime':
        in_school = schl.isin([1, 3])
    elif student_rule == 'nilf_enrolled':        # nearest to PRNLFSCH (asked of people not in the labor force)
        in_school = schl.isin([1, 2, 3, 4]) & (x.LABFORCE.astype(int) == 1)
    else:
        raise ValueError(student_rule)
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
    combined = np.maximum(gi0 + 0.5 * ss_rr, 0)
    married_any = r.marst.to_numpy() == 1
    t1 = np.where(married_any, 32000, 25000); t2 = np.where(married_any, 44000, 34000)
    taxable = np.zeros(len(r))
    mid = (combined > t1) & (combined <= t2)
    taxable[mid] = np.minimum(0.5 * ss_rr, 0.5 * (combined - t1))[mid]
    tier1 = np.minimum(0.5 * ss_rr, 0.5 * (t2 - t1))
    hi = combined > t2
    taxable[hi] = np.minimum(0.85 * ss_rr, 0.85 * (combined - t2) + tier1)[hi]
    taxable = np.maximum(taxable, 0)
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
    assert not r.isna().any().any(), r.isna().sum()[r.isna().sum() > 0]
    return r.reset_index(drop=True)


# ---------------------------------------------------------------------------------------------------------------
# 4a: dependent tests
# ---------------------------------------------------------------------------------------------------------------
def _hh_max(r, values):
    return pd.Series(values, index=r.index).groupby(r.hh).transform('max').to_numpy()


def flag_dependents(r, tax_year):
    dep_cut, mfs_sd = PARAMS[tax_year]
    r = r.copy()
    gi_by = r.set_index(['hh', 'pn']).gi
    gi_s = pd.MultiIndex.from_arrays([r.hh, r.sp_pn]).map(gi_by).to_numpy()
    gi_s = np.where(r.married_sp & ~np.isnan(gi_s), gi_s, 0.0)
    studying = r.in_school | r.nilf_school
    disabled = (r.diff_any | r.has_disab_income | r.ill_disabled) & ~(r.wage + r.selfemp > dep_cut)
    qc = (r.age < 19) | ((r.age < 24) & studying) | disabled
    qc &= ~(r.married_sp & (np.maximum(r.gi, gi_s) > mfs_sd))
    qc &= ~(r.married_absent & (r.gi > mfs_sd))
    qr = r.gi < dep_cut + INCOME_TOLERANCE
    qr &= ~(r.married_sp & (np.maximum(r.gi, gi_s) > mfs_sd))
    qr &= ~(r.married_absent & (r.gi > mfs_sd))
    r['is_disabled'] = disabled.to_numpy()
    r['QC'] = qc.to_numpy(); r['QR'] = qr.to_numpy()
    proxy = r.hhdfmx == r.groupby('hh').hhdfmx.transform('min')
    r['hd_is_QR'] = _hh_max(r, (r.QR & proxy).astype(int)) == 1
    r['hd_gi'] = _hh_max(r, np.where(proxy, r.gi, -np.inf))
    return r


# ---------------------------------------------------------------------------------------------------------------
# 4b: first assignment
# ---------------------------------------------------------------------------------------------------------------
def _parents_of(r, who_mask):
    """For each person, the set {mom_pn, pop_pn} of the household's person selected by who_mask (one per household,
    else -1): returns two arrays."""
    sel = r[who_mask]
    m1 = r.hh.map(sel.groupby('hh').mom_pn.max()).fillna(-1).to_numpy()
    m2 = r.hh.map(sel.groupby('hh').pop_pn.max()).fillna(-1).to_numpy()
    return m1, m2


def assign_units(r):
    r = r.copy()
    h = r.hhdfmx.to_numpy(); perrp = r.perrp.to_numpy(); pn = r.pn.to_numpy(); fh = r.fheadidx.to_numpy()
    QC = r.QC.to_numpy(); QR = r.QR.to_numpy(); hd_is_QR = r.hd_is_QR.to_numpy(); famrel = r.famrel.to_numpy()
    in_pf = (h >= 1) & (h <= 45); householder = h == 1; hh_spouse = h == 2
    child = (h >= 3) & (h <= 12); grandchild = (h >= 23) & (h <= 34); oth_rltv = (h >= 35) & (h <= 45)
    in_pf_only = np.isin(h, [4, 7, 9, 12, 25, 29, 31, 34, 37, 40, 42, 45])
    in_pf_ref = np.isin(h, [3, 5, 8, 10, 23, 26, 30, 32, 35, 38, 41, 43])
    in_pf_sp = np.isin(h, [6, 11, 27, 33, 39, 44])
    in_uf = np.isin(h, [46, 47, 48])
    nofm_hh = h == 49; nofm_ind = h == 50; nofm_gq = h == 51
    foster = nofm_ind & (perrp == 53)
    tu_id = np.zeros(len(r), dtype=np.int64); tu_rel = np.zeros(len(r), dtype=np.int64)

    def put(mask, rel, uid):
        tu_rel[mask] = rel
        tu_id[mask] = uid[mask] if isinstance(uid, np.ndarray) else uid

    # A) primary family
    put(in_pf & householder, 1, 1)
    put(nofm_hh, 1, pn)
    put(in_pf & hh_spouse, 2, fh)
    cg = in_pf_only & (child | grandchild)
    put(cg & QC, 3, fh); put(QC & foster, 3, 1)
    put(cg & ~QC & QR, 4, fh); put(~QC & QR & foster, 4, 1)
    put(cg & ~QC & ~QR, 1, pn); put(~QC & ~QR & foster, 1, pn)
    # B) other relatives in the primary family; parents of the householder or spouse cannot be qualifying children
    p1h, p2h = _parents_of(r, r.hhdfmx == 1)
    p1s, p2s = _parents_of(r, r.hhdfmx == 2)
    is_par = (pn == p1h) | (pn == p2h) | (pn == p1s) | (pn == p2s)
    sib_oth = np.isin(perrp, [51, 52])
    put(in_pf_only & QC & sib_oth & oth_rltv & ~is_par, 3, fh)
    m = in_pf_only & QR & oth_rltv & ((~QC | is_par) | ~sib_oth)
    put(m, 4, fh)
    m = in_pf_only & ~QR & oth_rltv & ((~QC | is_par) | ~sib_oth)
    put(m, 1, pn)
    # C) unrelated subfamilies
    put(in_uf & (h == 46), 1, pn); put(in_uf & (h == 47), 2, fh); put(in_uf & (h == 48), 3, fh)
    # D) group quarters
    put(nofm_gq & np.isin(famrel, [0, 1]), 1, pn)
    put(nofm_gq & (famrel == 2), 2, fh)
    k = nofm_gq & np.isin(famrel, [3, 4])
    put(k & QC, 3, fh); put(k & ~QC & QR, 4, fh); put(k & ~QC & ~QR, 1, pn)
    # E) secondary individuals
    gi = r.gi.to_numpy(); hd_gi = r.hd_gi.to_numpy()
    p_no = nofm_ind & np.isin(perrp, [44, 47])
    m = p_no & QR & ~hd_is_QR & (hd_gi > gi * PRT_INC_FCT)
    put(m, 4, 1)
    tu_rel[p_no & (tu_rel == 0)] = 1
    tu_id[p_no & (tu_id == 0)] = pn[p_no & (tu_id == 0)]
    put(nofm_ind & np.isin(perrp, [43, 46]), 1, pn)
    put(nofm_ind & np.isin(perrp, [54, 55, 56, 57]), 1, pn)
    o58 = nofm_ind & (perrp == 58)
    put(o58 & ~QC & ~QR, 1, pn)
    tu_rel[o58 & QC] = 3
    tu_rel[o58 & ~QC & QR] = 4
    tu_id[o58 & (QC | QR) & (tu_id == 0)] = 999
    # who takes a dependent nonrelative with relatives (tu_id 999), in order of preference
    s = pd.DataFrame({'hh': r.hh, 'pn': pn, 'gi': gi})
    ctg = nofm_ind & np.isin(perrp, [43, 46])
    assert s[ctg].groupby('hh').size().max() <= 1 if ctg.any() else True

    def ptr_min(mask):
        return s.hh.map(s[mask].groupby('hh').pn.min()).to_numpy()

    def ptr_maxgi(mask):
        t = s[mask]
        if not len(t):
            return np.full(len(s), np.nan)
        mx = t.groupby('hh').gi.transform('max')
        return s.hh.map(t[t.gi == mx].groupby('hh').pn.min()).to_numpy()

    ptrs = [ptr_min(ctg), ptr_maxgi(h == 46), ptr_maxgi(nofm_ind & np.isin(perrp, [54, 56])),
            ptr_maxgi(nofm_ind & (perrp == 58) & ~QC & ~QR)]
    assignee = tu_id == 999
    mom = r.mom_pn.to_numpy(); pop = r.pop_pn.to_numpy()
    for ptr in ptrs:
        assigned = assignee & ~np.isnan(ptr) & (tu_id == 999)
        tu_id[assigned] = ptr[assigned].astype(np.int64)
        # an assignee who is the parent of the person they were given to is a qualifying relative, or their own unit
        selp = pd.Series(ptr).notna()
        tgt = pd.DataFrame({'hh': r.hh, 'pn': pn, 'mom': mom, 'pop': pop})
        par_of = tgt.set_index(['hh', 'pn'])
        key = pd.MultiIndex.from_arrays([r.hh, np.nan_to_num(ptr, nan=-1).astype(int)])
        pm = key.map(par_of['mom']).fillna(-2).to_numpy(); pp = key.map(par_of['pop']).fillna(-2).to_numpy()
        is_par_hd = (pm == pn) | (pp == pn)
        tu_rel[assigned & is_par_hd & QR] = 4
        tu_rel[assigned & is_par_hd & ~QR] = 1
        tu_id[assigned & is_par_hd & ~QR] = 999
    tu_id[tu_id == 999] = 1
    o59 = nofm_ind & (perrp == 59)
    m = o59 & QR & (r.age.to_numpy() < 19)
    put(m, 3, 1)
    tu_rel[o59 & (tu_rel == 0)] = 1
    tu_id[o59 & (tu_id == 0)] = pn[o59 & (tu_id == 0)]
    # F) subfamily reference people and spouses
    assert (tu_rel[in_pf_ref | in_pf_sp] == 0).all() and (tu_id[in_pf_ref | in_pf_sp] == 0).all()
    put(in_pf_ref, 1, pn); put(in_pf_sp, 2, fh)
    # G) children in subfamilies
    k = np.isin(h, [24, 36])
    put(k & QC, 3, fh); put(k & ~QC & QR, 4, fh)
    tu_rel[k & (tu_rel == 0)] = 1
    tu_id[k & (tu_id == 0)] = pn[k & (tu_id == 0)]
    assert ((tu_rel >= 1) & (tu_rel <= 4)).all(), pd.Series(h[tu_rel == 0]).value_counts()
    assert (tu_id >= 1).all()
    r['tu_id'] = tu_id; r['tu_rel'] = tu_rel
    r['in_pf_ref'] = in_pf_ref
    return r


# ---------------------------------------------------------------------------------------------------------------
# 4c: reassignment
# ---------------------------------------------------------------------------------------------------------------
def _move_youngest(r, from_mask_fn, eligible_hh, n_move, dest):
    """Up to n_move[hh] times, move the youngest qualifying child (lowest person number on a tie) selected by
    from_mask_fn() to unit dest[hh], in households where eligible_hh."""
    allocated = np.zeros(len(r))
    for _ in range(2):
        src_mask = from_mask_fn()
        age = np.where(src_mask, r.age.to_numpy(), np.inf)
        yng = pd.Series(age).groupby(r.hh.to_numpy()).transform('min').to_numpy()
        cand = src_mask & (age == yng) & eligible_hh & (n_move > allocated) & (n_move > 0)
        c = pd.DataFrame({'hh': r.hh.to_numpy(), 'pn': r.pn.to_numpy(), 'i': np.arange(len(r))})[cand]
        pick = c.sort_values(['hh', 'pn']).drop_duplicates('hh').i.to_numpy()
        tu = r.tu_id.to_numpy().copy()
        tu[pick] = dest[pick]
        r['tu_id'] = tu
        moved = np.zeros(len(r)); moved[pick] = 1
        allocated = allocated + pd.Series(moved).groupby(r.hh.to_numpy()).transform('sum').to_numpy()
    return r


def reassign(r, tax_year):
    dep_cut, mfs_sd = PARAMS[tax_year]
    r = r.copy()
    r['tu_id0'] = r.tu_id; r['tu_rel0'] = r.tu_rel
    g = r.hh.to_numpy()
    grp = lambda v: pd.Series(v).groupby([g, r.ffpos.to_numpy()])
    # Routine 1: a related subfamily whose head qualifies as a dependent joins the primary unit
    sf_has_ref = grp((r.ftype == 3) & r.in_pf_ref).transform('max').to_numpy()
    n_eitc = pd.Series((r.tu_rel == 3).astype(int)).groupby([g, r.tu_id.to_numpy()]).transform('sum').to_numpy()
    is_fh = (r.fheadidx == r.pn).to_numpy()
    earn = (r.wage + r.selfemp).to_numpy()
    pqc = sf_has_ref & is_fh & r.perrp.isin([48, 49, 51, 52]).to_numpy() & r.QC.to_numpy()
    pqc &= ~((earn > dep_cut) & (n_eitc > 0)); pqc &= ~((earn > mfs_sd) & (n_eitc == 0))
    pqr = sf_has_ref & is_fh & ~pqc & r.QR.to_numpy(); pqr &= ~(earn > dep_cut)
    PQC = grp(pqc).transform('max').to_numpy().astype(bool); PQR = grp(pqr).transform('max').to_numpy().astype(bool)
    flag = PQC | PQR
    tu_id = r.tu_id.to_numpy().copy(); tu_rel = r.tu_rel.to_numpy().copy()
    tu_id0 = r.tu_id0.to_numpy(); tu_rel0 = r.tu_rel0.to_numpy(); fh = r.fheadidx.to_numpy(); pn = r.pn.to_numpy()
    QC = r.QC.to_numpy(); QR = r.QR.to_numpy()
    tu_id[flag & is_fh] = 1
    tu_rel[flag & is_fh & PQC] = 3
    tu_rel[flag & is_fh & PQR] = 4
    m = flag & ~is_fh & np.isin(tu_rel0, [2, 4]) & (tu_id0 == fh)
    tu_id[m & QR] = 1; tu_rel[m & QR] = 4
    tu_id[m & ~QR] = pn[m & ~QR]; tu_rel[m & ~QR] = 1
    m = flag & ~is_fh & (tu_rel0 == 3) & (tu_id0 == fh)
    tu_id[m & QC] = 1; tu_rel[m & QC] = 3
    tu_id[m & ~QC & QR] = 1; tu_rel[m & ~QC & QR] = 4
    tu_id[m & ~QC & ~QR] = pn[m & ~QC & ~QR]; tu_rel[m & ~QC & ~QR] = 1
    r['tu_id'] = tu_id; r['tu_rel'] = tu_rel
    r['tu_id1'] = r.tu_id

    # Routine 2: spread the youngest qualifying children between the householder's unit and a related subfamily
    hsum = lambda v: pd.Series(np.asarray(v, dtype=float)).groupby(g).transform('sum').to_numpy()
    hmax = lambda v: pd.Series(np.asarray(v, dtype=float)).groupby(g).transform('max').to_numpy()
    for j in range(2, int(r.ffpos.max()) + 1):
        fhj = hmax(np.where((r.ffpos == j) & (r.fheadidx == r.pn), r.fheadidx, -np.inf))
        ctg = hmax(((r.ftype == 3) & (r.tu_rel == 1) & (r.ffpos == j) & (r.fheadidx == r.pn)).astype(int)) == 1
        if not ctg.any():
            continue
        hd_c = np.minimum(hsum((r.tu_rel == 3) & (r.tu_id == 1)), 5)
        sf_c = np.minimum(hsum((r.tu_rel == 3) & (r.tu_id == fhj)), 5)
        h2s = np.zeros(len(r)); s2h = np.zeros(len(r))
        h2s = np.where(hd_c >= 4, np.maximum(0, np.minimum(2, 2 - sf_c)), h2s)
        h2s = np.where((hd_c == 3) & (sf_c < 2), 1, h2s)
        h2s = np.where((hd_c == 2) & (sf_c == 0), 1, h2s)
        s2h = np.where(sf_c >= 4, np.maximum(0, np.minimum(2, 2 - hd_c)), s2h)
        s2h = np.where((sf_c == 3) & (hd_c < 2), 1, s2h)
        s2h = np.where((sf_c == 2) & (hd_c == 0), 1, s2h)
        assert not ((h2s > 0) & (s2h > 0)).any()
        ok = ctg & (fhj > 0)
        dest = np.where(np.isfinite(fhj), fhj, 0).astype(np.int64)
        r = _move_youngest(r, lambda: ((r.tu_rel == 3) & (r.tu_id == 1)).to_numpy(), ok, h2s, dest)
        r = _move_youngest(r, lambda: ((r.tu_rel == 3) & (r.tu_id == fhj)).to_numpy(), ok, s2h,
                           np.ones(len(r), dtype=np.int64))
    r['tu_id2'] = r.tu_id

    # Routine 3: the same between the householder and an unmarried partner
    prt = r.perrp.isin([43, 44, 46, 47]).to_numpy()
    has_partner_hh = hmax(prt & (r.hhdfmx == 50).to_numpy()) == 1
    pt_tu = hmax(np.where(prt, r.tu_id, -np.inf)); pt_rel = hmax(np.where(prt, r.tu_rel, -np.inf))
    pt_wage = hmax(np.where(prt, r.wage, -np.inf))
    hd_ln = hsum(np.where(r.pn == 1, r.pn, 0)); pt_ln = hsum(np.where(prt, r.pn, 0))
    hd_wage = hmax(np.where(r.pn == hd_ln, r.wage, -np.inf)); hd_tu = hmax(np.where(r.pn == hd_ln, r.tu_id, -np.inf))
    has_pt = np.isfinite(pt_tu)
    hd_c = np.minimum(hsum((r.tu_rel == 3) & (r.tu_id == 1)), 5)
    pt_c = np.minimum(hsum((r.tu_rel == 3) & (r.tu_id == np.where(has_pt, pt_tu, -1))), 5)
    momv = r.mom_pn.to_numpy(); popv = r.pop_pn.to_numpy()
    hd_max = hsum((r.tu_rel == 3).to_numpy() & ((momv == hd_ln) | (popv == hd_ln)))
    pt_max = hsum((r.tu_rel == 3).to_numpy() & ((momv == pt_ln) | (popv == pt_ln)))
    h2p = np.zeros(len(r))
    h2p = np.where((hd_c >= 4) & has_pt, np.maximum(0, np.minimum(np.minimum(2, pt_max), 2 - pt_c)), h2p)
    h2p = np.where((pt_c < 2) & (pt_max >= 1) & (hd_c == 3) & has_pt, 1, h2p)
    h2p = np.where((pt_c == 0) & (pt_max >= 1) & (hd_c == 2) & has_pt, 1, h2p)
    h2p = np.where(pt_rel != 1, 0, h2p)
    h2p = np.where(~(pt_wage > 0), 0, h2p)
    p2h = np.zeros(len(r))
    p2h = np.where((pt_c >= 4) & has_pt, np.maximum(0, np.minimum(np.minimum(2, hd_max), 2 - hd_c)), p2h)
    p2h = np.where((hd_c < 2) & (hd_max >= 1) & (pt_c == 3) & has_pt, 1, p2h)
    p2h = np.where((hd_c == 0) & (hd_max >= 1) & (pt_c == 2) & has_pt, 1, p2h)
    p2h = np.where(~(hd_wage > 0), 0, p2h)
    dest_p = np.where(has_pt, pt_tu, 0).astype(np.int64)
    r = _move_youngest(r, lambda: (has_partner_hh & (r.tu_rel == 3).to_numpy() & (r.tu_id == 1).to_numpy()),
                       np.ones(len(r), bool), h2p, dest_p)
    dest_h = np.where(np.isfinite(hd_tu), hd_tu, 0).astype(np.int64)
    r = _move_youngest(r, lambda: ((r.tu_rel == 3) & (r.tu_id == np.where(has_pt, pt_tu, -1))).to_numpy(),
                       np.ones(len(r), bool), p2h, dest_h)
    f = lambda a, b: pd.Series(np.where(a != b, '1', '0'), index=r.index)
    r['assignments'] = '1' + f(r.tu_id0, r.tu_id1) + f(r.tu_id1, r.tu_id2) + f(r.tu_id2, r.tu_id)
    assert ((r.tu_rel >= 1) & (r.tu_rel <= 4)).all() and (r.tu_id >= 1).all()
    return r


# ---------------------------------------------------------------------------------------------------------------
# Units, dependent types and the PUF's terms
# ---------------------------------------------------------------------------------------------------------------
def dependent_types(r):
    """Each dependent's type in the PUF's terms: 'parent' (a parent of the unit's head or spouse), 'child_home' (a
    qualifying child, tu_rel 3, living in the household), 'other' (any other qualifying relative). The CPS cannot
    see children living elsewhere, so 'child_away' is never assigned."""
    key = pd.MultiIndex.from_arrays([r.hh, r.tu_id])
    heads = r[r.tu_rel == 1].set_index(['hh', 'tu_id'])
    spouses = r[r.tu_rel == 2].drop_duplicates(['hh', 'tu_id']).set_index(['hh', 'tu_id'])
    hm, hp = key.map(heads.mom_pn).to_numpy(), key.map(heads.pop_pn).to_numpy()
    sm, sp = key.map(spouses.mom_pn).to_numpy(), key.map(spouses.pop_pn).to_numpy()
    head_is_householder = key.map(heads.pn).to_numpy() == 1
    pn = r.pn.to_numpy()
    is_par = (pn == hm) | (pn == hp) | (pn == sm) | (pn == sp) | (head_is_householder & (r.perrp.to_numpy() == 50))
    dep = r.tu_rel.isin([3, 4]).to_numpy()
    return np.where(~dep, '', np.where(is_par, 'parent', np.where(r.tu_rel.to_numpy() == 3, 'child_home', 'other')))


def collapse_units(r):
    """One row per unit (head and spouse incomes summed, as 4e does), plus dependents' own returns."""
    r = r.copy()
    r['uid'] = r.hh * 100 + r.tu_id
    r['dep_type'] = dependent_types(r)
    # family weight of the head (CBO's fmwt): the ASECWT of the head's family reference person
    fw = r.set_index(['hh', 'pn']).weight
    r['fam_weight'] = pd.MultiIndex.from_arrays([r.hh, r.fheadidx]).map(fw).to_numpy()
    r.loc[r.fam_weight.isna(), 'fam_weight'] = r.weight
    hs = r[r.tu_rel.isin([1, 2])]
    head = r[r.tu_rel == 1].set_index('uid')
    assert head.index.is_unique, 'two heads in one unit'
    assert set(r.uid) == set(head.index), 'a unit without a head'
    inc_cols = ['gi', 'wage', 'selfemp', 'farm_selfemp', 'interest', 'dividends', 'rents', 'est_trust', 'pension',
                'uc', 'ssb', 'rr_pension', 'ssi', 'va_benefits', 'wc', 'other_income']
    u = hs.groupby('uid')[inc_cols].sum()
    sp = r[r.tu_rel == 2].groupby('uid')
    assert sp.size().max() <= 1 if len(sp) else True
    u['has_spouse'] = u.index.isin(sp.size().index)
    deps = r[r.tu_rel.isin([3, 4])]
    nd = deps.groupby('uid').size()
    u['n_dep'] = nd.reindex(u.index, fill_value=0)
    u['filing_status'] = np.where(u.has_spouse, 'joint', np.where(u.n_dep > 0, 'hoh', 'single'))
    for c, src in [('hh', 'hh'), ('head_pn', 'pn'), ('head_age', 'age'), ('head_sex', 'sex'),
                   ('weight', 'weight'), ('fam_weight', 'fam_weight'), ('gq', 'gq')]:
        u[c] = head[src].reindex(u.index)
    spd = r[r.tu_rel == 2].set_index('uid')
    u['spouse_pn'] = spd.pn.reindex(u.index).fillna(-1).astype(int)
    u['spouse_age'] = spd.age.reindex(u.index).fillna(-1).astype(int)
    u['spouse_sex'] = spd.sex.reindex(u.index).fillna(0).astype(int)
    u['wage_hd'] = head.wage.reindex(u.index); u['wage_sp'] = spd.wage.reindex(u.index).fillna(0)
    u['selfemp_hd'] = head.selfemp.reindex(u.index); u['selfemp_sp'] = spd.selfemp.reindex(u.index).fillna(0)
    # counts as CBO defines them (uncapped)
    d = deps.assign(ctc=(deps.tu_rel == 3) & (deps.age < 17), eitc=deps.tu_rel == 3)
    for c, v in [('n_ctc', 'ctc'), ('n_eitc', 'eitc')]:
        u[c] = d.groupby('uid')[v].sum().reindex(u.index, fill_value=0).astype(int)
    for t in DEP_TYPES:
        u[f'n_{t}'] = (d.dep_type == t).groupby(d.uid).sum().reindex(u.index, fill_value=0).astype(int)
    # CBO's three youngest: qualifying children first, then qualifying relatives, by age (4e's sort)
    ds = d.sort_values(['uid', 'tu_rel', 'age', 'pn'])
    ds['k'] = ds.groupby('uid').cumcount()
    for k in range(3):
        t = ds[ds.k == k].set_index('uid')
        u[f'cbo_age_{k + 1}'] = t.age.reindex(u.index).fillna(-1).astype(int)
    # the PUF's caps and type order, then youngest first among the kept
    cap = u.filing_status.map(PUF_CAP)
    d2 = d.assign(type_rank=d.dep_type.map({t: i for i, t in enumerate(DEP_TYPES)}))
    d2 = d2.sort_values(['uid', 'type_rank', 'age', 'pn'])
    d2['k'] = d2.groupby('uid').cumcount()
    d2['kept'] = d2.k < d2.uid.map(cap)
    kept = d2[d2.kept].sort_values(['uid', 'age', 'pn'])
    kept['k2'] = kept.groupby('uid').cumcount()
    for t in DEP_TYPES:
        u[f'puf_{t}'] = (kept.dep_type == t).groupby(kept.uid).sum().reindex(u.index, fill_value=0).astype(int)
    u['puf_n_dep'] = kept.groupby('uid').size().reindex(u.index, fill_value=0).astype(int)
    for k in range(3):
        t = kept[kept.k2 == k].set_index('uid')
        u[f'dep_age{k + 1}'] = t.age.reindex(u.index).fillna(-1).astype(int)
    u['n24'] = u.n_ctc
    u['eic'] = np.minimum(u.n_eitc, 3)
    u['dep_return'] = False
    u['claimant_uid'] = -1
    u = u.reset_index()
    # dependents' own returns: every dependent with gross income, wages or self-employment income (4e)
    dr = deps[(deps.gi != 0) | (deps.wage != 0) | (deps.selfemp != 0)].copy()
    dr_u = pd.DataFrame({'uid': dr.hh * 100 + dr.pn + 10 ** 9 * 0, 'hh': dr.hh, 'head_pn': dr.pn,
                         'head_age': dr.age, 'head_sex': dr.sex, 'weight': dr.weight, 'fam_weight': dr.fam_weight,
                         'gq': dr.gq, 'spouse_pn': -1, 'spouse_age': -1, 'spouse_sex': 0,
                         'filing_status': np.where(dr.age < 14, 'dependent_under14', 'single'),
                         'dep_return': True, 'claimant_uid': dr.uid})
    for c in inc_cols:
        dr_u[c] = dr[c].to_numpy()
    dr_u['wage_hd'] = dr.wage.to_numpy(); dr_u['selfemp_hd'] = dr.selfemp.to_numpy()
    dr_u['uid'] = dr.hh.to_numpy() * 100 + dr.pn.to_numpy() + 10 ** 12     # own id block, disjoint from units
    u = pd.concat([u, dr_u], ignore_index=True)
    fill0 = [c for c in u.columns if c.startswith(('n_', 'puf_', 'n24', 'eic', 'wage_sp', 'selfemp_sp'))]
    u[fill0] = u[fill0].fillna(0).astype(int if True else float)
    for c in ['cbo_age_1', 'cbo_age_2', 'cbo_age_3', 'dep_age1', 'dep_age2', 'dep_age3']:
        u[c] = u[c].fillna(-1).astype(int)
    u['has_spouse'] = u.has_spouse.fillna(False).astype(bool)
    assert not u.isna().any().any(), u.isna().sum()[u.isna().sum() > 0]
    return r, u


# ---------------------------------------------------------------------------------------------------------------
# Married filing separately: a post-filing step (setting mfs_split, off by default)
# ---------------------------------------------------------------------------------------------------------------
INCOME_ITEMS = ['gi', 'wage', 'selfemp', 'farm_selfemp', 'interest', 'dividends', 'rents', 'est_trust', 'pension',
                'uc', 'ssb', 'rr_pension', 'ssi', 'va_benefits', 'wc', 'other_income']


def keyed_uniform(ids, salt):
    """A uniform draw in [0, 1) that depends only on the record id and the salt (splitmix64), never on which other
    records are present or their order (rule S23)."""
    with np.errstate(over='ignore'):         # the arithmetic wraps around 2^64 on purpose
        z = (np.asarray(ids, dtype=np.uint64) + np.uint64(salt) * np.uint64(0x9E3779B97F4A7C15))
        z = (z ^ (z >> np.uint64(30))) * np.uint64(0xBF58476D1CE4E5B9)
        z = (z ^ (z >> np.uint64(27))) * np.uint64(0x94D049BB133111EB)
        z = z ^ (z >> np.uint64(31))
    return (z >> np.uint64(11)).astype(np.float64) / float(2 ** 53)


def primary_and_spouse(persons, uids):
    """Person 1 and person 2 of each couple by the rebuild's rule before its random step (build_cps.R): in a
    mixed-sex couple the man, in a same-sex couple the older partner (the head on a tie)."""
    hs = persons[persons.uid.isin(uids) & persons.tu_rel.isin([1, 2])].copy()
    mixed = hs.groupby('uid').sex.transform('nunique') == 2
    hs['k1'] = np.where(mixed, hs.sex, 0); hs['k2'] = np.where(mixed, 0, -hs.age); hs['k3'] = hs.tu_rel
    hs = hs.sort_values(['uid', 'k1', 'k2', 'k3'])
    hs['pos'] = hs.groupby('uid').cumcount() + 1
    assert (hs.groupby('uid').size() == 2).all()
    return hs[hs.pos == 1].set_index('uid'), hs[hs.pos == 2].set_index('uid')


def mfs_split(units, persons, filer, target_returns, dep_rule='primary', salt=20261023):
    """Split joint filer couples into two married-filing-separately returns each.

    The couples are chosen by a uniform draw keyed by the unit id: couples are taken in order of their draw until
    their weighted count reaches target_returns / 2 (SOI's MFS return count; two returns per couple), so the share
    is uniform across couples and the result is a set of records, not a split of weights. Each couple becomes a
    return for person 1 and one for person 2, each with that person's own income items. Dependents:
      dep_rule 'primary': all on person 1's return;
      dep_rule 'even':    divided as evenly as possible, youngest first, alternating person 1, person 2, ...
    The PUF's cap for separate returns (1 dependent shown, type order, then youngest) is applied to each return.
    Non-filers are untouched: a non-filing couple has no filing status.
    units: main units (one row per uid, filing_status, weight); filer: boolean array aligned with units.
    Returns (chosen uids, the share of joint filer couples chosen, the new returns)."""
    j = units[filer & (units.filing_status.to_numpy() == 'joint')]
    u = keyed_uniform(j.uid.to_numpy(), salt)
    order = np.argsort(u, kind='stable')
    cw = np.cumsum(j.weight.to_numpy()[order])
    n = int(np.searchsorted(cw, target_returns / 2)) + 1
    chosen = j.uid.to_numpy()[order[:n]]
    share = cw[n - 1] / j.weight.sum()
    p1, p2 = primary_and_spouse(persons, chosen)
    deps = persons[persons.uid.isin(chosen) & persons.tu_rel.isin([3, 4])].sort_values(['uid', 'age', 'pn']).copy()
    deps['k'] = deps.groupby('uid').cumcount()
    deps['to'] = 1 if dep_rule == 'primary' else np.where(deps.k % 2 == 0, 1, 2)
    if dep_rule not in ('primary', 'even'):
        raise ValueError(dep_rule)
    rows = []
    for who, pp in [(1, p1), (2, p2)]:
        r = pd.DataFrame({'uid': chosen * 10 + who, 'couple_uid': chosen, 'person': who, 'hh': pp.hh.reindex(chosen).to_numpy(),
                          'head_pn': pp.pn.reindex(chosen).to_numpy(), 'head_age': pp.age.reindex(chosen).to_numpy(),
                          'head_sex': pp.sex.reindex(chosen).to_numpy(),
                          'weight': units.set_index('uid').weight.reindex(chosen).to_numpy(),
                          'filing_status': 'separate'})
        for c in INCOME_ITEMS:
            r[c] = pp[c].reindex(chosen).to_numpy()
        d = deps[deps.to == who].copy()
        d['rank'] = d.dep_type.map({t: i for i, t in enumerate(DEP_TYPES)})
        d = d.sort_values(['uid', 'rank', 'age', 'pn']); d['kk'] = d.groupby('uid').cumcount()
        kept = d[d.kk < PUF_CAP['separate']]
        r['n_dep'] = d.groupby('uid').size().reindex(chosen, fill_value=0).to_numpy()
        for t in DEP_TYPES:
            r[f'puf_{t}'] = (kept.dep_type == t).groupby(kept.uid).sum().reindex(chosen, fill_value=0).to_numpy()
        r['puf_n_dep'] = kept.groupby('uid').size().reindex(chosen, fill_value=0).to_numpy()
        r['dep_age1'] = kept.set_index('uid').age.reindex(chosen).fillna(-1).astype(int).to_numpy()
        r['dep_age2'] = -1; r['dep_age3'] = -1
        r['n24'] = ((d.tu_rel == 3) & (d.age < 17)).groupby(d.uid).sum().reindex(chosen, fill_value=0).to_numpy()
        r['n_eitc'] = (d.tu_rel == 3).groupby(d.uid).sum().reindex(chosen, fill_value=0).to_numpy()
        r['eic'] = 0                     # married filing separately cannot claim the EITC (2015 law)
        rows.append(r)
    out = pd.concat(rows, ignore_index=True)
    assert not out.isna().any().any(), out.isna().sum()[out.isna().sum() > 0]
    return chosen, share, out


def build(tax_year, reassign_pass=True, student_rule='nilf_enrolled', keep_gq=True, cache_dir=None):
    r = read_ipums_roster(tax_year, student_rule=student_rule, cache_dir=cache_dir)
    if not keep_gq:
        r = r[~r.gq].reset_index(drop=True)
    r = flag_dependents(r, tax_year)
    r = assign_units(r)
    if reassign_pass:
        r = reassign(r, tax_year)
    else:
        r['assignments'] = '1000'
    # CBO's code can leave a second head in the householder's unit: a nonrelative with relatives who is the parent
    # of the person first chosen to claim them, and fails the income test, is made a head with unit id 999, which
    # becomes the householder's unit at the end (4b, section E). Here that person heads their own unit instead.
    odd = (r.tu_rel == 1) & (r.tu_id != r.pn)
    print(f'second heads moved to their own unit: {odd.sum()} people, {r.weight[odd].sum() / 1e6:.3f} m weighted')
    r.loc[odd, 'tu_id'] = r.pn[odd]
    persons, units = collapse_units(r)
    # every person once; every spouse and dependent points at a unit with a head; no one claimed twice
    assert not persons[['hh', 'pn']].duplicated().any()
    assert persons.uid.isin(units.loc[~units.dep_return, 'uid']).all()
    return persons, units


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('tax_year', type=int)
    ap.add_argument('--strict', action='store_true', help='no reassignment pass')
    ap.add_argument('--drop-gq', action='store_true', help='leave out group quarters (the filing step does)')
    ap.add_argument('--student-rule', default='nilf_enrolled')
    ap.add_argument('--out', required=True)
    ap.add_argument('--cache', default=None)
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    variant = 'strict' if a.strict else 'cbo'
    persons, units = build(a.tax_year, reassign_pass=not a.strict, student_rule=a.student_rule,
                           keep_gq=not a.drop_gq, cache_dir=a.cache)
    tag = f'{a.tax_year}_{variant}' + ('_nogq' if a.drop_gq else '')
    keep = ['year', 'hh', 'pn', 'hseq', 'lineno', 'age', 'sex', 'weight', 'gq', 'relate', 'perrp', 'hhdfmx', 'ftype',
            'famrel', 'fheadidx', 'QC', 'QR', 'is_disabled', 'tu_id', 'tu_rel', 'uid', 'dep_type', 'assignments',
            'mom_pn', 'pop_pn'] + INCOME_ITEMS
    persons[keep].to_parquet(f'{a.out}/persons_{tag}.parquet')
    units.to_parquet(f'{a.out}/units_{tag}.parquet')
    # the roster for the R filing step (build_cps.R with cps.tax_units = "cbo"): one row per person
    fs = units[~units.dep_return].set_index('uid').filing_status
    ro = persons[['year', 'hh', 'pn', 'tu_id', 'tu_rel', 'dep_type']].assign(unit_fs=persons.uid.map(fs).to_numpy())
    assert not ro.isna().any().any()
    ro.to_csv(f'{a.out}/roster_{tag}.csv.gz', index=False)
    m = units[~units.dep_return]
    w = m.weight / 1e6
    print(f'tax year {a.tax_year}, {variant}: {len(persons)} people, {len(m)} units ({w.sum():.2f} m weighted), '
          f'{units.dep_return.sum()} dependents\' own returns')
    print((w.groupby(m.filing_status).sum()).round(2).to_string())
    print(f'dependents {(w * m.n_dep).sum():.2f} m; under the PUF caps {(w * m.puf_n_dep).sum():.2f} m; '
          f'child credit children {(w * m.n24).sum():.2f} m; reassignment codes '
          f'{persons.assignments.value_counts().to_dict()}')


if __name__ == '__main__':
    main()
