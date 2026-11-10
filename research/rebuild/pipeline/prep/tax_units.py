"""The tax-unit rules shared by every survey: CBO's public algorithm for CPS tax filing units
(US-CBO/CPS-tax-filing-units), translated rule by rule from its Stata code.

A tax unit is the set of people who would appear on one tax return: a head, possibly a spouse, and the dependents
they claim. Each survey's reader (roster_cps.py, roster_scf.py, roster_cex.py, roster_sipp.py) builds a standard
member list of the survey's people. build_units then runs the rules on that list in four steps. It tests who could
be claimed as a dependent; assigns every person to one unit as head, spouse or dependent; moves some dependents
between units the way CBO's code does; and collapses the people into one row per unit, with a filing status and the
dependents counted the way the PUF shows them. It returns one row per person and one per unit. TAX_UNITS.md
describes the member list, the rules and each survey's fallbacks.

In the code a person's role in their unit (tu_rel) is 1 for the head, 2 for the spouse, 3 for a qualifying child
and 4 for a qualifying relative. A unit's number within the household (tu_id) is its head's person number.
"""
import os

import numpy as np
import pandas as pd

# By tax year: the income limit for a qualifying relative (the personal exemption amount), and the standard
# deduction for married filing separately. CBO uses the second as a sign that a married person supports themselves.
# The values come from CBO's 1b_config.do; 2012-2017 are from the Form 1040 instructions.
PARAMS = {2012: (3800, 5950), 2013: (3900, 6100), 2014: (3950, 6200), 2015: (4000, 6300), 2016: (4050, 6300), 2017: (4050, 6350), 2018: (4150, 12000), 2019: (4200, 12200),
          2020: (4300, 12400), 2021: (4300, 12550), 2022: (4400, 12950), 2023: (4700, 13850), 2024: (5050, 14600),
          2025: (5200, 15750)}
# The householder can claim an unmarried partner only if the householder's gross income is more than this multiple
# of the partner's.
PRT_INC_FCT = 2
INCOME_TOLERANCE = 0

# The most dependents the PUF shows on a return, by filing status (AGES.md)
PUF_CAP = {'single': 2, 'joint': 3, 'hoh': 3, 'separate': 1}
DEP_TYPES = ['child_home', 'child_away', 'other', 'parent']    # the PUF's order of dependent types

# Inputs a reader may have had to assume, flagged person by person. A unit's flag is True when any member's is.
ASSUMED_FLAGS = ['age_assumed',      # the exact age was drawn within a reported age range (SCF members other than
                                     # the couple)
                 'link_assumed',     # a child was placed with the household's reference person, or a couple was
                                     # paired, with no parent or spouse pointer in the survey to say so (SCF, CEX)
                 'income_assumed']   # the person's own income was not measured, or measured only for a group (SCF)


# ---- Dependent tests (CBO's 4a_flag_dependents.do) ----

def _hh_max(r, values):
    """The largest of `values` within each person's household, as an array over persons."""
    return pd.Series(values, index=r.index).groupby(r.hh).transform('max').to_numpy()


def flag_dependents(r, tax_year):
    """Marks each person who could be claimed as a qualifying child (QC) or a qualifying relative (QR).

    A qualifying child is under 19, or under 24 and a student, or disabled. A qualifying relative has gross income
    below the personal exemption amount. A married person cannot be either when their own or their spouse's gross
    income is above the standard deduction for married filing separately. Where the survey asks whether a person is
    financially dependent (the SCF), the answer replaces some of these tests (below). Also adds, for each household,
    whether its householder could be a qualifying relative (hd_is_QR) and the householder's gross income (hd_gi)."""
    dep_cut, mfs_sd = PARAMS[tax_year]
    r = r.copy()
    # The gross income of each married person's spouse (0 for others)
    gi_by = r.set_index(['hh', 'pn']).gi
    gi_s = pd.MultiIndex.from_arrays([r.hh, r.sp_pn]).map(gi_by).to_numpy()
    gi_s = np.where(r.married_sp & ~np.isnan(gi_s), gi_s, 0.0)
    # 1. The tests as CBO writes them. A person with earnings above the exemption amount is not counted as disabled.
    studying = r.in_school | r.nilf_school
    disabled = (r.diff_any | r.has_disab_income | r.ill_disabled) & ~(r.wage + r.selfemp > dep_cut)
    qc = (r.age < 19) | ((r.age < 24) & studying) | disabled
    qr = r.gi < dep_cut + INCOME_TOLERANCE
    # 2. Where the survey asks about financial support (the SCF), the rules of Gale, Joshi, Pulliam and Sabelhaus
    #    (2022). An adult child is a qualifying child only if aged 18 to 24 and financially dependent. The answer
    #    replaces the student test, and replaces the under-19 test at age 18. Where the person's own income is not
    #    measured, the answer replaces the income test of a qualifying relative, and a non-relative adult is never
    #    claimed.
    sup = r.support.to_numpy()
    gi_obs = r.gi_obs.to_numpy()
    asked = sup >= 0
    qc_sup = (r.age < 18) | ((r.age <= 24) & (sup == 1)) | disabled
    qc = pd.Series(np.where(asked, qc_sup, qc), index=r.index)
    nonrel_adult = (r.perrp.to_numpy() >= 53) & (r.age.to_numpy() >= 18)
    no_inc = asked & ~gi_obs
    qr = pd.Series(np.where(no_inc, (sup == 1) & ~nonrel_adult, qr), index=r.index)
    # Mark the people who are claimed because of the support answer: an adult qualifying child, or a qualifying
    # relative with no measured income of their own. A minor is a qualifying child by age, whatever the answer.
    r['dep_by_support'] = asked & (r.age.to_numpy() >= 18) & (qc.to_numpy() | (no_inc & qr.to_numpy()))
    # 3. The test for married people.
    qc &= ~(r.married_sp & (np.maximum(r.gi, gi_s) > mfs_sd))
    qc &= ~(r.married_absent & (r.gi > mfs_sd))
    qr &= ~(r.married_sp & (np.maximum(r.gi, gi_s) > mfs_sd))
    qr &= ~(r.married_absent & (r.gi > mfs_sd))
    r['is_disabled'] = disabled.to_numpy()
    r['QC'] = qc.to_numpy(); r['QR'] = qr.to_numpy()
    # 4. The householder's results. The householder is the person with the lowest family position code in the
    #    household.
    proxy = r.hhdfmx == r.groupby('hh').hhdfmx.transform('min')
    r['hd_is_QR'] = _hh_max(r, (r.QR & proxy).astype(int)) == 1
    r['hd_gi'] = _hh_max(r, np.where(proxy, r.gi, -np.inf))
    return r


# ---- First assignment of every person to a unit (CBO's 4b_identify_tax_unit_relationships.do) ----

def _parents_of(r, who_mask):
    """For each person, the mother and father pointers of the one household member selected by who_mask (-1 if
    there is none), as two arrays."""
    sel = r[who_mask]
    m1 = r.hh.map(sel.groupby('hh').mom_pn.max()).fillna(-1).to_numpy()
    m2 = r.hh.map(sel.groupby('hh').pop_pn.max()).fillna(-1).to_numpy()
    return m1, m2


def _lowest_pn(s, mask):
    """For each person, the lowest person number among the members of their household selected by mask (NaN if
    there is none)."""
    return s.hh.map(s[mask].groupby('hh').pn.min()).to_numpy()


def _highest_income_pn(s, mask):
    """For each person, the person number of the member of their household selected by mask with the highest gross
    income, the lowest person number on a tie (NaN if there is none)."""
    t = s[mask]
    if not len(t):
        return np.full(len(s), np.nan)
    mx = t.groupby('hh').gi.transform('max')
    return s.hh.map(t[t.gi == mx].groupby('hh').pn.min()).to_numpy()


def assign_units(r):
    """Assigns every person to one unit, as head, spouse, qualifying child or qualifying relative, by their family
    position (HHDFMX) and relationship to the householder (PERRP). The groups are taken in CBO's order: the primary
    family, its other relatives, unrelated subfamilies, group quarters, people living with no relatives, subfamily
    heads and spouses, and children in subfamilies. Adds tu_id (the unit) and tu_rel (the role)."""
    r = r.copy()
    h = r.hhdfmx.to_numpy(); perrp = r.perrp.to_numpy(); pn = r.pn.to_numpy(); fh = r.fheadidx.to_numpy()
    QC = r.QC.to_numpy(); QR = r.QR.to_numpy(); hd_is_QR = r.hd_is_QR.to_numpy(); famrel = r.famrel.to_numpy()
    # The family position groups CBO's code uses
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

    # A. The primary family. The householder heads unit 1, and the spouse joins the householder. A householder with
    #    no family heads their own unit. The family's children and grandchildren, and foster children, are qualifying
    #    children if they pass that test, else qualifying relatives if they pass that one, else heads of their own
    #    units.
    put(in_pf & householder, 1, 1)
    put(nofm_hh, 1, pn)
    put(in_pf & hh_spouse, 2, fh)
    cg = in_pf_only & (child | grandchild)
    put(cg & QC, 3, fh); put(QC & foster, 3, 1)
    put(cg & ~QC & QR, 4, fh); put(~QC & QR & foster, 4, 1)
    put(cg & ~QC & ~QR, 1, pn); put(~QC & ~QR & foster, 1, pn)
    # B. Other relatives in the primary family. A parent of the householder or of the spouse cannot be a qualifying
    #    child; only siblings and "other relatives" can.
    p1h, p2h = _parents_of(r, r.hhdfmx == 1)
    p1s, p2s = _parents_of(r, r.hhdfmx == 2)
    is_par = (pn == p1h) | (pn == p2h) | (pn == p1s) | (pn == p2s)
    sib_oth = np.isin(perrp, [51, 52])
    put(in_pf_only & QC & sib_oth & oth_rltv & ~is_par, 3, fh)
    m = in_pf_only & QR & oth_rltv & ((~QC | is_par) | ~sib_oth)
    put(m, 4, fh)
    m = in_pf_only & ~QR & oth_rltv & ((~QC | is_par) | ~sib_oth)
    put(m, 1, pn)
    # C. Unrelated subfamilies: the reference person heads a unit, with their spouse and children.
    put(in_uf & (h == 46), 1, pn); put(in_uf & (h == 47), 2, fh); put(in_uf & (h == 48), 3, fh)
    # D. Group quarters.
    put(nofm_gq & np.isin(famrel, [0, 1]), 1, pn)
    put(nofm_gq & (famrel == 2), 2, fh)
    k = nofm_gq & np.isin(famrel, [3, 4])
    put(k & QC, 3, fh); put(k & ~QC & QR, 4, fh); put(k & ~QC & ~QR, 1, pn)
    # E. People living with no relatives in the household. An unmarried partner is the householder's qualifying
    #    relative when they pass that test, the householder does not, and the householder's gross income is more
    #    than twice the partner's.
    gi = r.gi.to_numpy(); hd_gi = r.hd_gi.to_numpy()
    p_no = nofm_ind & np.isin(perrp, [44, 47])
    m = p_no & QR & ~hd_is_QR & (hd_gi > gi * PRT_INC_FCT)
    put(m, 4, 1)
    tu_rel[p_no & (tu_rel == 0)] = 1
    tu_id[p_no & (tu_id == 0)] = pn[p_no & (tu_id == 0)]
    put(nofm_ind & np.isin(perrp, [43, 46]), 1, pn)
    put(nofm_ind & np.isin(perrp, [54, 55, 56, 57]), 1, pn)
    # Other non-relatives who have relatives in the household (PERRP 58) and could be a dependent get the placeholder
    # unit 999 until a claimant is found below.
    o58 = nofm_ind & (perrp == 58)
    put(o58 & ~QC & ~QR, 1, pn)
    tu_rel[o58 & QC] = 3
    tu_rel[o58 & ~QC & QR] = 4
    tu_id[o58 & (QC | QR) & (tu_id == 0)] = 999
    # Who claims such a person, in order of preference: an unmarried partner with relatives in the household; the
    # reference person of an unrelated subfamily, the one with the highest income; a housemate or roomer with
    # relatives, the one with the highest income; another such non-relative who is not a dependent, the one with the
    # highest income.
    s = pd.DataFrame({'hh': r.hh, 'pn': pn, 'gi': gi})
    ctg = nofm_ind & np.isin(perrp, [43, 46])
    assert s[ctg].groupby('hh').size().max() <= 1 if ctg.any() else True
    ptrs = [_lowest_pn(s, ctg), _highest_income_pn(s, h == 46),
            _highest_income_pn(s, nofm_ind & np.isin(perrp, [54, 56])),
            _highest_income_pn(s, nofm_ind & (perrp == 58) & ~QC & ~QR)]
    assignee = tu_id == 999
    mom = r.mom_pn.to_numpy(); pop = r.pop_pn.to_numpy()
    for ptr in ptrs:
        assigned = assignee & ~np.isnan(ptr) & (tu_id == 999)
        tu_id[assigned] = ptr[assigned].astype(np.int64)
        # A person given to their own child cannot be that child's dependent. They are the child's qualifying
        # relative if they pass that test; otherwise they go back to the placeholder and the next claimant is tried.
        tgt = pd.DataFrame({'hh': r.hh, 'pn': pn, 'mom': mom, 'pop': pop})
        par_of = tgt.set_index(['hh', 'pn'])
        key = pd.MultiIndex.from_arrays([r.hh, np.nan_to_num(ptr, nan=-1).astype(int)])
        pm = key.map(par_of['mom']).fillna(-2).to_numpy(); pp = key.map(par_of['pop']).fillna(-2).to_numpy()
        is_par_hd = (pm == pn) | (pp == pn)
        tu_rel[assigned & is_par_hd & QR] = 4
        tu_rel[assigned & is_par_hd & ~QR] = 1
        tu_id[assigned & is_par_hd & ~QR] = 999
    # Anyone still without a claimant goes to the householder's unit.
    tu_id[tu_id == 999] = 1
    # Other non-relatives with no relatives in the household (PERRP 59): a person under 19 who passes the
    # qualifying-relative income test is a qualifying child of the householder; everyone else heads their own unit.
    o59 = nofm_ind & (perrp == 59)
    m = o59 & QR & (r.age.to_numpy() < 19)
    put(m, 3, 1)
    tu_rel[o59 & (tu_rel == 0)] = 1
    tu_id[o59 & (tu_id == 0)] = pn[o59 & (tu_id == 0)]
    # F. Reference people and spouses of related subfamilies.
    assert (tu_rel[in_pf_ref | in_pf_sp] == 0).all() and (tu_id[in_pf_ref | in_pf_sp] == 0).all()
    put(in_pf_ref, 1, pn); put(in_pf_sp, 2, fh)
    # G. Children in related subfamilies.
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
    """A claim among the SCF's members outside the primary economic unit (the reader's column `group` > 0). This
    rule is from Gale, Joshi, Pulliam and Sabelhaus (2022), not from CBO. Within the group, the unit whose head and
    spouse have the highest gross income claims the group's unmarried relatives of the reference person who are in
    units of one and pass the qualifying-relative income test."""
    dep_cut, _ = PARAMS[tax_year]
    r = r.copy()
    r['claimed_in_group'] = False
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


# ---- Reassignment (CBO's 4c_reassign_relationships.do) ----

def _move_youngest(r, from_mask_fn, eligible_hh, n_move, dest):
    """In each eligible household, moves up to n_move of the youngest qualifying children selected by
    from_mask_fn() to unit dest. On a tie in age the lower person number moves first. n_move is at most 2."""
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
    """CBO's three reassignment routines, applied in order within each household. They change who claims whom,
    never whether anyone files. The unit numbers before and after each routine are kept as tu_id0, tu_id1 and tu_id2,
    and `assignments` records which routines moved the person."""
    r = r.copy()
    r['tu_id0'] = r.tu_id; r['tu_rel0'] = r.tu_rel
    # 1. A related subfamily whose reference person could be the householder's dependent joins the householder's
    #    unit.
    r = _join_related_subfamilies(r, tax_year)
    r['tu_id1'] = r.tu_id
    # 2. Spread the youngest qualifying children between the householder's unit and each related subfamily.
    r = _spread_children_with_subfamilies(r)
    r['tu_id2'] = r.tu_id
    # 3. Spread them between the householder and an unmarried partner.
    r = _spread_children_with_partner(r)
    # 4. Record which routines moved each person: a code of four digits, '1' followed by one digit per routine
    #    (1 if the person's unit changed in that routine).
    f = lambda a, b: pd.Series(np.where(a != b, '1', '0'), index=r.index)
    r['assignments'] = '1' + f(r.tu_id0, r.tu_id1) + f(r.tu_id1, r.tu_id2) + f(r.tu_id2, r.tu_id)
    assert ((r.tu_rel >= 1) & (r.tu_rel <= 4)).all() and (r.tu_id >= 1).all()
    return r


def _join_related_subfamilies(r, tax_year):
    """Routine 1. A related subfamily whose reference person could be the householder's dependent joins the
    householder's unit.

    This applies when the reference person is the householder's child, grandchild, brother or sister, or other
    relative. The reference person becomes the householder's qualifying child (or qualifying relative). The
    subfamily's other members become the householder's dependents if they pass a test, and heads of their own units
    if not. The reference person cannot be claimed as a qualifying child when their earnings are above the exemption
    amount and they have qualifying children of their own, or above the separate-filing standard deduction and they
    have none. They cannot be claimed as a qualifying relative when their earnings are above the exemption amount."""
    dep_cut, mfs_sd = PARAMS[tax_year]
    g = r.hh.to_numpy()
    grp = lambda v: pd.Series(v).groupby([g, r.ffpos.to_numpy()])
    sf_has_ref = grp((r.ftype == 3) & r.in_pf_ref).transform('max').to_numpy()
    n_dep_eitc = pd.Series((r.tu_rel == 3).astype(int)).groupby([g, r.tu_id.to_numpy()]).transform('sum').to_numpy()
    is_fh = (r.fheadidx == r.pn).to_numpy()
    earn = (r.wage + r.selfemp).to_numpy()
    pqc = sf_has_ref & is_fh & r.perrp.isin([48, 49, 51, 52]).to_numpy() & r.QC.to_numpy()
    pqc &= ~((earn > dep_cut) & (n_dep_eitc > 0)); pqc &= ~((earn > mfs_sd) & (n_dep_eitc == 0))
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
    return r


def _spread_children_with_subfamilies(r):
    """Routine 2. Spreads the youngest qualifying children between the householder's unit and each related
    subfamily whose reference person still heads a unit.

    Each side's qualifying children are counted, up to 5. When one side has two or more and the other has fewer, up
    to two of the youngest move across. The number depends on both counts, as set out below."""
    g = r.hh.to_numpy()
    hsum = lambda v: pd.Series(np.asarray(v, dtype=float)).groupby(g).transform('sum').to_numpy()
    hmax = lambda v: pd.Series(np.asarray(v, dtype=float)).groupby(g).transform('max').to_numpy()
    for j in range(2, int(r.ffpos.max()) + 1):
        fhj = hmax(np.where((r.ffpos == j) & (r.fheadidx == r.pn), r.fheadidx, -np.inf))
        ctg = hmax(((r.ftype == 3) & (r.tu_rel == 1) & (r.ffpos == j) & (r.fheadidx == r.pn)).astype(int)) == 1
        if not ctg.any():
            continue
        hd_c = np.minimum(hsum((r.tu_rel == 3) & (r.tu_id == 1)), 5)
        sf_c = np.minimum(hsum((r.tu_rel == 3) & (r.tu_id == fhj)), 5)
        # h2s: how many move from the householder to the subfamily; s2h: how many move the other way
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
    return r


def _spread_children_with_partner(r):
    """Routine 3. Spreads the youngest qualifying children between the householder and an unmarried partner who
    heads their own unit, by the same counts as routine 2.

    The number moved toward a side is at most the number of the children who are that side's own children, and the
    children moved are that side's own children, youngest first. Nothing moves toward a side with no wages."""
    g = r.hh.to_numpy()
    hsum = lambda v: pd.Series(np.asarray(v, dtype=float)).groupby(g).transform('sum').to_numpy()
    hmax = lambda v: pd.Series(np.asarray(v, dtype=float)).groupby(g).transform('max').to_numpy()
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
    # hd_max and pt_max: how many of the qualifying children are the householder's and the partner's own
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
    # Only a child of the receiving side moves, youngest first. Here the code departs from CBO's, which moves the
    # youngest qualifying children whoever their parents are. A partner cannot claim a child who is not theirs.
    pt_own = (pt_ln > 0) & ((momv == pt_ln) | (popv == pt_ln))
    hd_own = (momv == hd_ln) | (popv == hd_ln)
    dest_p = np.where(has_pt, pt_tu, 0).astype(np.int64)
    r = _move_youngest(r, lambda: (has_partner_hh & (r.tu_rel == 3).to_numpy() & (r.tu_id == 1).to_numpy() & pt_own),
                       np.ones(len(r), bool), h2p, dest_p)
    dest_h = np.where(np.isfinite(hd_tu), hd_tu, 0).astype(np.int64)
    r = _move_youngest(r, lambda: ((r.tu_rel == 3) & (r.tu_id == np.where(has_pt, pt_tu, -1))).to_numpy() & hd_own,
                       np.ones(len(r), bool), p2h, dest_h)
    return r


# ---- One row per unit, dependent types and the PUF's terms (CBO's 4e_collapse_to_tax_units.do) ----

def dependent_types(r):
    """Each dependent's type in the PUF's terms: 'parent' (a parent of the head or spouse), 'child_home' (a
    qualifying child living in the household) or 'other'. Children living elsewhere ('child_away') are added by
    collapse_units."""
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


INCOME_ITEMS = ['gi', 'wage', 'selfemp', 'farm_selfemp', 'interest', 'dividends', 'rents', 'est_trust', 'pension',
                'uc', 'ssb', 'rr_pension', 'ssi', 'va_benefits', 'wc', 'other_income']   # a unit's income items


def collapse_units(r, away=None):
    """One row per unit, with the head's and spouse's incomes summed, plus one row for each dependent who has a
    return of their own.

    away holds children living elsewhere whom a unit claims (columns uid, age; the SCF only). They count as
    dependents of type child_away but are not people of the household. Returns the persons, with their unit id (uid)
    and dependent type, and the units."""
    # 1. Give each person their unit id and dependent type, and each person the family weight of their family's
    #    reference person (CBO's fmwt, the reference person's ASECWT).
    r = r.copy()
    r['uid'] = r.hh * 100 + r.tu_id
    r['dep_type'] = dependent_types(r)
    fw = r.set_index(['hh', 'pn']).weight
    r['fam_weight'] = pd.MultiIndex.from_arrays([r.hh, r.fheadidx]).map(fw).to_numpy()
    r.loc[r.fam_weight.isna(), 'fam_weight'] = r.weight
    # 2. Sum the head's and spouse's incomes by unit, and mark the units with a spouse.
    hs = r[r.tu_rel.isin([1, 2])]
    head = r[r.tu_rel == 1].set_index('uid')
    assert head.index.is_unique, 'two heads in one unit'
    assert set(r.uid) == set(head.index), 'a unit without a head'
    u = hs.groupby('uid')[INCOME_ITEMS].sum()
    sp = r[r.tu_rel == 2].groupby('uid')
    assert sp.size().max() <= 1 if len(sp) else True
    u['has_spouse'] = u.index.isin(sp.size().index)
    # 3. List the dependents, adding the children living elsewhere. Only the household's own people can have a
    #    return of their own.
    deps = r[r.tu_rel.isin([3, 4])]
    deps_own = deps
    if away is not None and len(away):
        aw = pd.DataFrame({'uid': away.uid.to_numpy(), 'hh': away.uid.to_numpy() // 100, 'pn': -1, 'tu_rel': 3,
                           'age': away.age.to_numpy(), 'dep_type': 'child_away'})
        assert aw.uid.isin(head.index).all()
        deps = pd.concat([deps, aw], ignore_index=True)
    # 4. Filing status: joint with a spouse, head of household with any dependent, single otherwise. Copy the
    #    head's and spouse's demographics, weights and earnings onto the unit.
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
    # 5. Count the dependents as CBO does, with no cap: child credit children (qualifying children under 17), EITC
    #    children (qualifying children living in the household) and each type. Record the ages of CBO's three
    #    youngest: qualifying children first, then qualifying relatives, each by age (4e's sort).
    d = deps.assign(ctc=(deps.tu_rel == 3) & (deps.age < 17), eitc=(deps.tu_rel == 3) & (deps.dep_type != 'child_away'))
    for c, v in [('n_dep_ctc', 'ctc'), ('n_dep_eitc', 'eitc')]:
        u[c] = d.groupby('uid')[v].sum().reindex(u.index, fill_value=0).astype(int)
    for t in DEP_TYPES:
        u[f'n_{t}'] = (d.dep_type == t).groupby(d.uid).sum().reindex(u.index, fill_value=0).astype(int)
    ds = d.sort_values(['uid', 'tu_rel', 'age', 'pn'])
    ds['k'] = ds.groupby('uid').cumcount()
    for k in range(3):
        t = ds[ds.k == k].set_index('uid')
        u[f'cbo_age_{k + 1}'] = t.age.reindex(u.index).fillna(-1).astype(int)
    # 6. The dependents the PUF would show. Keep the first ones in the PUF's type order, up to its cap for the
    #    filing status. Then order the kept dependents youngest first and record the first three ages.
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
    # The PUF's child credit children (N24) and EITC children (EIC, at most 3)
    u['n24'] = u.n_dep_ctc
    u['eic'] = np.minimum(u.n_dep_eitc, 3)
    u['dep_return'] = False
    u['claimant_uid'] = -1
    u = u.reset_index()
    # 7. Add a return of their own for every dependent with any gross income, wages or self-employment income
    #    (4e). These returns have ids in a block of their own, apart from the units' ids, and point to the unit that
    #    claims the dependent.
    dr = deps_own[(deps_own.gi != 0) | (deps_own.wage != 0) | (deps_own.selfemp != 0)].copy()
    dr_u = pd.DataFrame({'uid': dr.hh * 100 + dr.pn + 10 ** 12, 'hh': dr.hh, 'head_pn': dr.pn,
                         'head_age': dr.age, 'head_sex': dr.sex, 'weight': dr.weight, 'fam_weight': dr.fam_weight,
                         'gq': dr.gq, 'spouse_pn': -1, 'spouse_age': -1, 'spouse_sex': 0,
                         'filing_status': np.where(dr.age < 14, 'dependent_under14', 'single'),
                         'dep_return': True, 'claimant_uid': dr.uid})
    for c in INCOME_ITEMS:
        dr_u[c] = dr[c].to_numpy()
    dr_u['wage_hd'] = dr.wage.to_numpy(); dr_u['selfemp_hd'] = dr.selfemp.to_numpy()
    u = pd.concat([u, dr_u], ignore_index=True)
    # 8. On the dependents' own returns the counts are zero and the dependent ages -1 (none).
    fill0 = [c for c in u.columns if c.startswith(('n_', 'puf_', 'n24', 'eic', 'wage_sp', 'selfemp_sp'))]
    u[fill0] = u[fill0].fillna(0).astype(int)
    for c in ['cbo_age_1', 'cbo_age_2', 'cbo_age_3', 'dep_age1', 'dep_age2', 'dep_age3']:
        u[c] = u[c].fillna(-1).astype(int)
    u['has_spouse'] = u.has_spouse.fillna(False).astype(bool)
    assert not u.isna().any().any(), u.isna().sum()[u.isna().sum() > 0]
    return r, u


# ---- Helpers for the survey readers, and the split into separate returns ----

def taxable_social_security(other_income, benefits, married):
    """The taxable part of Social Security benefits by the Form 1040 worksheet, as CBO's 3a_compute_incomes.do
    computes it. Every survey reader uses it for gross income. married is a boolean array."""
    combined = np.maximum(other_income + 0.5 * benefits, 0)
    t1 = np.where(married, 32000, 25000); t2 = np.where(married, 44000, 34000)
    taxable = np.zeros(len(combined))
    mid = (combined > t1) & (combined <= t2)
    taxable[mid] = np.minimum(0.5 * benefits, 0.5 * (combined - t1))[mid]
    tier1 = np.minimum(0.5 * benefits, 0.5 * (t2 - t1))
    hi = combined > t2
    taxable[hi] = np.minimum(0.85 * benefits, 0.85 * (combined - t2) + tier1)[hi]
    return np.maximum(taxable, 0)


def keyed_uniform(ids, salt):
    """A uniform random number in [0, 1) for each id. The number depends only on the id and the salt, not on which
    other records are present or their order. It is computed with the splitmix64 hash."""
    with np.errstate(over='ignore'):         # the arithmetic wraps around at 2^64 by design
        z = (np.asarray(ids, dtype=np.uint64) + np.uint64(salt) * np.uint64(0x9E3779B97F4A7C15))
        z = (z ^ (z >> np.uint64(30))) * np.uint64(0xBF58476D1CE4E5B9)
        z = (z ^ (z >> np.uint64(27))) * np.uint64(0x94D049BB133111EB)
        z = z ^ (z >> np.uint64(31))
    return (z >> np.uint64(11)).astype(np.float64) / float(2 ** 53)


def primary_and_spouse(persons, uids):
    """Person 1 and person 2 of each couple in uids, by build_cps.R's rule without its random step. In a couple of
    a man and a woman, person 1 is the man. In a same-sex couple, person 1 is the older partner, and the head on a
    tie in age."""
    hs = persons[persons.uid.isin(uids) & persons.tu_rel.isin([1, 2])].copy()
    mixed = hs.groupby('uid').sex.transform('nunique') == 2
    hs['k1'] = np.where(mixed, hs.sex, 0); hs['k2'] = np.where(mixed, 0, -hs.age); hs['k3'] = hs.tu_rel
    hs = hs.sort_values(['uid', 'k1', 'k2', 'k3'])
    hs['pos'] = hs.groupby('uid').cumcount() + 1
    assert (hs.groupby('uid').size() == 2).all()
    return hs[hs.pos == 1].set_index('uid'), hs[hs.pos == 2].set_index('uid')


def mfs_split(units, persons, filer, target_returns, salt=20261023, chance=None):
    """Splits some joint couples into two married-filing-separately returns each.

    Each spouse gets a return with their own income, and every dependent goes on person 1's return. Couples are
    taken in the order of a random number keyed by unit id, until their weighted count reaches target_returns / 2.
    The weighted count uses the weight alone among the filer couples (`filer`), or, when `chance` is given, the
    weight times the filing chance among all joint couples; each new return then carries that chance as p_file.
    Returns the unit ids of the couples chosen, the share of couples chosen, and the new returns."""
    # 1. Choose the couples.
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
    # 2. Build person 1's and person 2's returns. Of the dependents on person 1's return, the PUF would show only one
    #    (its cap for separate returns).
    p1, p2 = primary_and_spouse(persons, chosen)
    deps = persons[persons.uid.isin(chosen) & persons.tu_rel.isin([3, 4])].sort_values(['uid', 'age', 'pn'])
    rows = []
    for who, pp in [(1, p1), (2, p2)]:
        r = pd.DataFrame({'uid': chosen * 10 + who, 'couple_uid': chosen, 'person': who, 'hh': pp.hh.reindex(chosen).to_numpy(),
                          'head_pn': pp.pn.reindex(chosen).to_numpy(), 'head_age': pp.age.reindex(chosen).to_numpy(),
                          'head_sex': pp.sex.reindex(chosen).to_numpy(),
                          'weight': units.set_index('uid').weight.reindex(chosen).to_numpy(),
                          'filing_status': 'separate'})
        for c in INCOME_ITEMS:
            r[c] = pp[c].reindex(chosen).to_numpy()
        d = (deps if who == 1 else deps.iloc[:0]).copy()      # every dependent goes on person 1's return
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
        r['n_dep_eitc'] = (d.tu_rel == 3).groupby(d.uid).sum().reindex(chosen, fill_value=0).to_numpy()
        r['eic'] = 0                     # married people filing separately cannot claim the EITC (2015 law)
        if chance is not None:
            r['p_file'] = pd.Series(chance[is_joint], index=j.uid.to_numpy()).reindex(chosen).to_numpy()
        rows.append(r)
    out = pd.concat(rows, ignore_index=True)
    assert not out.isna().any().any(), out.isna().sum()[out.isna().sum() > 0]
    return chosen, share, out


# ---- The whole set of rules ----

def build_units(r, tax_year, away=None):
    """Runs the rules on one survey's standard member list r.

    away holds children living elsewhere, keyed by the person who claims them (columns hh, pn, age; the SCF only).
    Returns (persons, units). Each unit has any_<flag> columns saying whether any member's input was assumed."""
    # 1. Test who could be a dependent, assign every person to a unit, apply the SCF's group claim where the survey
    #    has groups, and apply CBO's reassignment.
    r = flag_dependents(r, tax_year)
    r = assign_units(r)
    if 'group' in r:
        r = claim_within_group(r, tax_year)
    r = reassign(r, tax_year)
    # 2. CBO's code can leave a second head in the householder's unit: a non-relative who is the parent of the
    #    person first chosen to claim them and who fails the income test. That person heads their own unit instead.
    odd = (r.tu_rel == 1) & (r.tu_id != r.pn)
    print(f'second heads moved to their own unit: {odd.sum()} people, {r.weight[odd].sum() / 1e6:.3f} m weighted')
    r.loc[odd, 'tu_id'] = r.pn[odd]
    # 3. Find the unit of each claimant of a child living elsewhere, and collapse the people into units.
    aw = None
    if away is not None and len(away):
        claim_uid = pd.MultiIndex.from_arrays([r.hh, r.pn]).to_frame(index=False).assign(uid=r.hh * 100 + r.tu_id)
        aw = away.merge(claim_uid, on=['hh', 'pn'], how='left')
        assert aw.uid.notna().all()
    persons, units = collapse_units(r, away=aw)
    # Every person appears once, and every spouse and dependent is in a unit with a head.
    assert not persons[['hh', 'pn']].duplicated().any()
    assert persons.uid.isin(units.loc[~units.dep_return, 'uid']).all()
    # 4. The assumed-input flags by unit. A dependent's own return carries the dependent's own flags. The flags for a
    #    claim made because of the support answer or the group claim count only for dependents.
    fl = ASSUMED_FLAGS + ['dep_by_support', 'claimed_in_group']
    if 'claimed_in_group' not in persons:
        persons['claimed_in_group'] = False
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


def save_cps(persons, units, out, tag):
    """Writes one CPS year's persons and units, and the roster that the R steps read (tax_units_cbo.R)."""
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
