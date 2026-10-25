"""tax_units.py: the shared tax-unit rules for every survey (the CPS, SCF and CEX), by CBO's public algorithm
(US-CBO/CPS-tax-filing-units, Stata), translated rule by rule (research/rebuild/TAX_UNITS.md, section 4).

The rules run on a standard member list that each survey's reader produces (roster_cps.py, roster_scf.py,
roster_cex.py). Where a survey lacks an input, the rule uses a named fallback (FALLBACKS below), and every person
and unit carries flags saying which inputs were assumed (ASSUMED_FLAGS). Split from the CPS reader on 24 October 2026;
on the CPS the rules are unchanged, and step 1's units come out identical, unit for unit.

The standard member list, one row per person (CBO's Census codes are the shared code list; each reader translates its
own relationship codes into them):
  survey, year, hh, pn (person number, the pointers' key), weight, age, sex (1 man, 2 woman, 0 unknown), gq
  perrp       Census's detailed relationship to the household's reference person (40-59; listed in roster_cps.py)
  hhdfmx      Census's detailed family position (1-51), ftype, famrel, famid/ffpos, fheadidx (the family head's pn)
  sp_pn, mom_pn, pop_pn, marst, married_sp, married_absent   spouse and parent pointers (0 none), marital status
  in_school, nilf_school, ill_disabled, diff_any, has_disab_income   the student and disability inputs
  the income items INCOME_ITEMS and gross income gi (CBO's 3a definition)
  support     the survey's own answer to "does this person depend on the household's head for most of their
              support": 1 yes, 0 no, -1 not asked (the SCF's financial dependence; none in the CPS or CEX)
  gi_obs      the person's own gross income is measured (False: gi is 0 and the income test uses the fallback)
  the assumed-input flags ASSUMED_FLAGS (False where the input is observed)
Readers may add their own columns; the rules pass them through to the persons table.

Steps, with the Stata module each one translates:
  flag_dependents              4a_flag_dependents.do: qualifying child (QC) and qualifying relative (QR) tests.
  assign_units                 4b_identify_tax_unit_relationships.do: every person to one unit, as head (1), spouse
                               (2), qualifying child (3) or qualifying relative (4).
  claim_within_group           not CBO's: a survey's group of members outside the main family (the SCF's members
                               outside the primary economic unit) whose highest-income unit claims the group's other
                               relatives who pass the income test (Gale, Joshi, Pulliam and Sabelhaus 2022). Off
                               unless the survey's fallbacks turn it on.
  reassign                     4c_reassign_relationships.do, the optional reassignment pass (settings: reassign).
  collapse_units               4e_collapse_to_tax_units.do: one row per unit, plus a dependent's own return for every
                               dependent with income (gross income, wages or self-employment not zero).
  puf_terms                    the units in the PUF's terms: filing status, dependents by type under the PUF's caps
                               and type order, their ages youngest first, child credit and EITC children.
  build_units(roster, ...)     all of the above on one survey's member list.

Married filing separately (mfs_split, a post-filing step; the CPS's filing stage calls it, filing.py): a share of
joint filer couples, chosen in the order of a draw keyed by unit id, become two separate returns, so that the count of
separate returns equals SOI's (Table 1.6), as nonfiler_pool/07_mfs_split.R does with weights. The SCF instead splits
the couples that say they file separately (roster_scf.py).

Variants: reassign=True is CBO's default ("cbo"); reassign=False applies the first pass only ("strict rules").

Inputs IPUMS lacks, and what roster_cps.py uses instead (listed in full in the 2023 reproduction report):
  - PRNLFSCH (in school, asked of people not in the labor force): SCHLCOLL 1-4 (enrolled in high school or college)
    and LABFORCE 1 (not in the labor force), setting student_rule ('nilf_enrolled', the default; 'enrolled' and
    'fulltime' drop the labor-force condition). Measured on March 2024: 95% of PRNLFSCH = 1 match. PYRSN (reason for
    not working the rest of the year = school): not in the extract, dropped.
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

Usage (through Slurm): python tax_units.py <tax_year> [--strict] [--drop-gq] [--out DIR]   (the CPS)
Writes persons_<year>_<variant>.parquet (one row per person: unit, role, dependent type),
units_<year>_<variant>.parquet (one row per unit, dependents' own returns flagged and linked) and
roster_<year>_<variant>.csv.gz (save_cps; the pipeline's filing stage writes the same files through filing.py).
"""
import argparse
import os

import numpy as np
import pandas as pd

# CBO's 1b_config.do: the qualifying-relative income limit (the personal exemption) and the married-filing-separately
# standard deduction used as a support proxy. 2012-2017 added here from the Form 1040 instructions (2012-2014 for the
# SCF's 2013 wave).
PARAMS = {2012: (3800, 5950), 2013: (3900, 6100), 2014: (3950, 6200), 2015: (4000, 6300), 2016: (4050, 6300), 2017: (4050, 6350), 2018: (4150, 12000), 2019: (4200, 12200),
          2020: (4300, 12400), 2021: (4300, 12550), 2022: (4400, 12950), 2023: (4700, 13850), 2024: (5050, 14600),
          2025: (5200, 15750)}
PRT_INC_FCT = 2          # head's gross income as a multiple of an unmarried partner's (support proxy)
INCOME_TOLERANCE = 0

# PUF caps on dependents shown, by filing status (AGES.md step 3)
PUF_CAP = {'single': 2, 'joint': 3, 'hoh': 3, 'separate': 1}
DEP_TYPES = ['child_home', 'child_away', 'other', 'parent']    # the PUF's type order

# Inputs a reader may have to assume, person by person. A unit's flag is True when any of its members' is.
ASSUMED_FLAGS = ['age_assumed',      # exact age drawn within a reported age bracket (SCF members other than the couple)
                 'link_assumed',     # a child placed with the household's reference person, or a couple paired, with
                                     # no parent or spouse pointer to say so (SCF, CEX)
                 'income_assumed']   # own income not measured, or measured only for a group (SCF)

# The fallback table: for each survey, how each rule input is obtained. 'observed' means the rule runs as CBO wrote
# it; anything else names the fallback, which flag_dependents, assign_units and claim_within_group read.
FALLBACKS = pd.DataFrame.from_dict({
    'cps': {'relationship': 'observed', 'parent_links': 'observed', 'spouse_links': 'observed', 'age': 'observed',
            'student': 'observed', 'disability': 'observed', 'income_test': 'observed',
            'group_claim': 'off', 'children_away': 'none'},
    'scf': {'relationship': 'observed, merged codes', 'parent_links': "reference person's unit",
            'spouse_links': 'pair two married adults outside the PEU', 'age': 'drawn within the bracket',
            'student': 'support answer, ages 18-24', 'disability': 'none',
            'income_test': 'support answer where own income is not measured',
            'group_claim': 'highest-income unit outside the PEU', 'children_away': 'if child support is paid'},
    'cex': {'relationship': 'observed', 'parent_links': "reference person's unit",
            'spouse_links': 'pair a married child with a married in-law of the other sex; else the only two other '
                            'married adults of the other sex, both relatives or both not',
            'age': 'observed (seven 2015 children without one are given 8)',
            'student': 'enrolled in college (IN_COLL 1-2); no labor-force condition', 'disability': 'none',
            'income_test': 'member income only (wages, self-employment, Social Security, SSI)',
            'group_claim': 'off', 'children_away': 'none'},
}, orient='index')


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
    qr = r.gi < dep_cut + INCOME_TOLERANCE
    # Fallbacks where the survey has a support answer (the SCF), following Gale, Joshi, Pulliam and Sabelhaus (2022):
    # an adult child is a qualifying child only if aged 18-24 and financially dependent (the answer stands in for the
    # student test, and for the under-19 test at 18); where the person's own income is not measured, the answer
    # stands in for the income test of a qualifying relative, and a non-relative adult is never claimed.
    sup = r.support.to_numpy() if 'support' in r else np.full(len(r), -1)
    gi_obs = r.gi_obs.to_numpy() if 'gi_obs' in r else np.ones(len(r), bool)
    asked = sup >= 0
    qc_sup = (r.age < 18) | ((r.age <= 24) & (sup == 1)) | disabled
    qc = pd.Series(np.where(asked, qc_sup, qc), index=r.index)
    nonrel_adult = (r.perrp.to_numpy() >= 53) & (r.age.to_numpy() >= 18)
    no_inc = asked & ~gi_obs
    qr = pd.Series(np.where(no_inc, (sup == 1) & ~nonrel_adult, qr), index=r.index)
    # claimed on the answer: an adult qualifying child, or a qualifying relative with no income of their own measured
    # (a minor is a qualifying child by age, whatever the answer)
    r['dep_by_support'] = asked & (r.age.to_numpy() >= 18) & (qc.to_numpy() | (no_inc & qr.to_numpy()))
    qc &= ~(r.married_sp & (np.maximum(r.gi, gi_s) > mfs_sd))
    qc &= ~(r.married_absent & (r.gi > mfs_sd))
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


def claim_within_group(r, tax_year):
    """Not CBO's rule: Gale, Joshi, Pulliam and Sabelhaus (2022, TPC) for the SCF's members outside the primary
    economic unit. Within each household, the group's unit with the highest income (head and spouse gross income;
    the lowest head person number on a tie) claims the group's other relatives of the reference person (perrp 48-52)
    who are on their own (a unit of one), pass the qualifying-relative income test and are not married. The group is
    the reader's column `group` (> 0); the claimed are flagged claimed_in_group."""
    dep_cut, _ = PARAMS[tax_year]
    r = r.copy()
    r['claimed_in_group'] = False
    if 'group' not in r or not (r.group > 0).any():
        return r
    g = r[r.group > 0]
    size = pd.Series(1, index=r.index).groupby([r.hh, r.tu_id]).transform('sum')
    unit_gi = r.gi.where(r.tu_rel.isin([1, 2]), 0).groupby([r.hh, r.tu_id]).transform('sum')
    heads = g[g.tu_rel == 1]
    heads = heads.assign(ugi=unit_gi.loc[heads.index], size=size.loc[heads.index])
    best = heads.sort_values(['hh', 'ugi', 'pn'], ascending=[True, False, True]).drop_duplicates('hh')
    claimant = r.hh.map(best.set_index('hh').tu_id)
    cand = ((r.group > 0) & (r.tu_rel == 1) & (size == 1) & r.perrp.isin([48, 49, 50, 51, 52])
            & (r.gi < dep_cut + INCOME_TOLERANCE) & ~r.married_sp & claimant.notna() & (r.tu_id != claimant))
    r.loc[cand, 'tu_id'] = claimant[cand].astype(np.int64)
    r.loc[cand, 'tu_rel'] = 4
    r.loc[cand, 'claimed_in_group'] = True
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


def collapse_units(r, away=None):
    """One row per unit (head and spouse incomes summed, as 4e does), plus dependents' own returns.
    away: optional children living elsewhere whom a unit claims (columns uid, age): dependents of type child_away,
    counted with the unit's other dependents but not people of the household (the SCF; none in the CPS)."""
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
    deps_own = deps                          # dependents' own returns are built from the household's people only
    if away is not None and len(away):
        aw = pd.DataFrame({'uid': away.uid.to_numpy(), 'hh': away.uid.to_numpy() // 100, 'pn': -1, 'tu_rel': 3,
                           'age': away.age.to_numpy(), 'dep_type': 'child_away'})
        assert aw.uid.isin(head.index).all()
        deps = pd.concat([deps, aw], ignore_index=True)
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
    d = deps.assign(ctc=(deps.tu_rel == 3) & (deps.age < 17), eitc=(deps.tu_rel == 3) & (deps.dep_type != 'child_away'))
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
    dr = deps_own[(deps_own.gi != 0) | (deps_own.wage != 0) | (deps_own.selfemp != 0)].copy()
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
# Married filing separately: a post-filing step (filing.py calls it)
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


def mfs_split(units, persons, filer, target_returns, dep_rule='primary', salt=20261023, chance=None):
    """Split joint filer couples into two married-filing-separately returns each.

    The couples are chosen by a uniform draw keyed by the unit id: couples are taken in order of their draw until
    their weighted count reaches target_returns / 2 (SOI's MFS return count; two returns per couple), so the share
    is uniform across couples and the result is a set of records, not a split of weights.
    With chance (an array of filing chances aligned with units; step 1, 23 October evening), every joint couple is
    a candidate, filer is ignored, and couples are taken until weight x filing chance reaches target_returns / 2;
    each separate return then carries the couple's chance (column p_file). Each couple becomes a
    return for person 1 and one for person 2, each with that person's own income items. Dependents:
      dep_rule 'primary': all on person 1's return;
      dep_rule 'even':    divided as evenly as possible, youngest first, alternating person 1, person 2, ...
    The PUF's cap for separate returns (1 dependent shown, type order, then youngest) is applied to each return.
    Non-filers are untouched: a non-filing couple has no filing status.
    units: main units (one row per uid, filing_status, weight); filer: boolean array aligned with units.
    Returns (chosen uids, the share of joint filer couples chosen, the new returns)."""
    is_joint = units.filing_status.to_numpy() == 'joint'
    if chance is None:
        j = units[filer & is_joint]
        mass = j.weight.to_numpy()
    else:
        chance = np.asarray(chance, dtype=float)
        j = units[is_joint]
        mass = j.weight.to_numpy() * chance[is_joint]
    u = keyed_uniform(j.uid.to_numpy(), salt)
    order = np.argsort(u, kind='stable')
    cw = np.cumsum(mass[order])
    n = int(np.searchsorted(cw, target_returns / 2)) + 1
    chosen = j.uid.to_numpy()[order[:n]]
    share = cw[n - 1] / mass.sum()
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
        if chance is not None:
            r['p_file'] = pd.Series(chance[is_joint], index=j.uid.to_numpy()).reindex(chosen).to_numpy()
        rows.append(r)
    out = pd.concat(rows, ignore_index=True)
    assert not out.isna().any().any(), out.isna().sum()[out.isna().sum() > 0]
    return chosen, share, out


def build_units(r, tax_year, reassign_pass=True, away=None):
    """The rules on one survey's standard member list r (see the module header). away: children living elsewhere
    whom a unit claims, keyed by the claiming person (columns hh, pn, age; the SCF). Returns (persons, units): one
    row per person with unit id (uid), role (tu_rel) and dependent type, and one row per unit, with any_<flag>
    columns saying whether any member's input was assumed (ASSUMED_FLAGS), any_dep_by_support (a dependent claimed on
    the survey's support answer) and any_claimed_in_group."""
    for c in ASSUMED_FLAGS:
        if c not in r:
            r[c] = False
    survey = r.survey.iloc[0] if 'survey' in r else 'cps'
    r = flag_dependents(r, tax_year)
    r = assign_units(r)
    if FALLBACKS.loc[survey, 'group_claim'] != 'off':
        r = claim_within_group(r, tax_year)
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
    aw = None
    if away is not None and len(away):
        claim_uid = pd.MultiIndex.from_arrays([r.hh, r.pn]).to_frame(index=False).assign(uid=r.hh * 100 + r.tu_id)
        aw = away.merge(claim_uid, on=['hh', 'pn'], how='left')
        assert aw.uid.notna().all()
    persons, units = collapse_units(r, away=aw)
    # every person once; every spouse and dependent points at a unit with a head; no one claimed twice
    assert not persons[['hh', 'pn']].duplicated().any()
    assert persons.uid.isin(units.loc[~units.dep_return, 'uid']).all()
    # the assumed-input flags, by unit (a dependent's own return carries the dependent's own)
    fl = ASSUMED_FLAGS + ['dep_by_support', 'claimed_in_group']
    for c in fl:
        if c not in persons:
            persons[c] = False
    dep_only = ['dep_by_support', 'claimed_in_group']
    pf = persons[fl].copy()
    pf[dep_only] = pf[dep_only].to_numpy() & persons.tu_rel.isin([3, 4]).to_numpy()[:, None]
    anyf = pf.groupby(persons.uid.to_numpy()).max()
    own = persons.set_index(persons.hh * 100 + persons.pn + 10 ** 12)[ASSUMED_FLAGS]
    for c in fl:
        v = units.uid.map(anyf[c])
        if c in ASSUMED_FLAGS:
            v = v.fillna(units.uid.map(own[c]))
        units[f'any_{c}'] = v.fillna(False).astype(bool).to_numpy()
    return persons, units


def build(tax_year, reassign_pass=True, student_rule='nilf_enrolled', keep_gq=True, cache_dir=None):
    """The CPS: the member list from roster_cps.py, then the rules."""
    import roster_cps
    r = roster_cps.read_roster(tax_year, student_rule=student_rule, cache_dir=cache_dir)
    if not keep_gq:
        r = r[~r.gq].reset_index(drop=True)
    return build_units(r, tax_year, reassign_pass=reassign_pass)


def save_cps(persons, units, out, tag):
    """Write one CPS year's units (tax_units.py as a script, and filing.py's units step): the persons and units
    tables, and the roster the R steps read (one row per person; tax_units_cbo.R)."""
    os.makedirs(out, exist_ok=True)
    keep = ['year', 'hh', 'pn', 'hseq', 'lineno', 'age', 'sex', 'weight', 'gq', 'relate', 'perrp', 'hhdfmx', 'ftype',
            'famrel', 'fheadidx', 'QC', 'QR', 'is_disabled', 'tu_id', 'tu_rel', 'uid', 'dep_type', 'assignments',
            'mom_pn', 'pop_pn'] + INCOME_ITEMS
    persons[keep].to_parquet(f'{out}/persons_{tag}.parquet')
    units.to_parquet(f'{out}/units_{tag}.parquet')
    fs = units[~units.dep_return].set_index('uid').filing_status
    ro = persons[['year', 'hh', 'pn', 'tu_id', 'tu_rel', 'dep_type']].assign(unit_fs=persons.uid.map(fs).to_numpy())
    assert not ro.isna().any().any()
    ro.to_csv(f'{out}/roster_{tag}.csv.gz', index=False)
    m = units[~units.dep_return]
    w = m.weight / 1e6
    print(f'{tag}: {len(persons)} people, {len(m)} units ({w.sum():.2f} m weighted), '
          f'{units.dep_return.sum()} dependents\' own returns')
    print((w.groupby(m.filing_status).sum()).round(2).to_string())
    print(f'dependents {(w * m.n_dep).sum():.2f} m; under the PUF caps {(w * m.puf_n_dep).sum():.2f} m; '
          f'child credit children {(w * m.n24).sum():.2f} m; reassignment codes '
          f'{persons.assignments.value_counts().to_dict()}')


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('tax_year', type=int)
    ap.add_argument('--strict', action='store_true', help='no reassignment pass')
    ap.add_argument('--drop-gq', action='store_true', help='leave out group quarters (the filing step does)')
    ap.add_argument('--student-rule', default='nilf_enrolled')
    ap.add_argument('--out', required=True)
    ap.add_argument('--cache', default=None)
    a = ap.parse_args()
    variant = 'strict' if a.strict else 'cbo'
    persons, units = build(a.tax_year, reassign_pass=not a.strict, student_rule=a.student_rule,
                           keep_gq=not a.drop_gq, cache_dir=a.cache)
    save_cps(persons, units, a.out, f'{a.tax_year}_{variant}' + ('_nogq' if a.drop_gq else ''))


if __name__ == '__main__':
    main()
