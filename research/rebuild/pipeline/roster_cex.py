"""roster_cex.py: the CEX reader for the shared tax-unit rules (tax_units.py). It turns a survey year's CEX interview
member files into the standard member list the rules read (one row per person; the columns are listed in
tax_units.py's header), runs the rules, and splits married couples into separate returns by the draw step 1 uses.

Sources: the same five quarterly member (MEMI) and family (FMLI) files per survey year as build_cex.R (Q1-Q4 of the
year and Q1 of the next), interviews with weight 0 dropped (weight FINLWT21 / 4 x MO_SCOPE / 3, as build_cex.R).
One "household" is one interview (NEWID): the consumer unit, the people sharing major expenses. Other consumer units
at the same address are other interviews.

What the CEX observes, and the fallbacks (tax_units.FALLBACKS['cex']):
  - relationship to the reference person (CU_CODE): 0 roommate or other non-relative, 1 reference person, 2 spouse,
    3 child, 4 grandchild, 5 in-law, 6 brother or sister, 7 parent, 8 other relative, 9 unmarried partner. Mapped to
    Census's codes (PERRP, HHDFMX) below.
  - no parent or spouse pointers. Children and grandchildren go to the reference person's unit (CBO's rule for a
    child in the primary family with no subfamily). A married child and a married in-law of the opposite sex are
    paired as a couple when exactly one such pair is in the consumer unit (a related subfamily; link_assumed on both).
    A grandchild's parent is never known (link_assumed on grandchildren under 24); nor is the parent of a child in a
    consumer unit where the reference person has an unmarried partner (link_assumed on those children under 24).
  - exact ages (AGE; seven children in 2015 have none and are given age 8, flagged) and sex; marital status MARITAL (1 married, 2 widowed, 3 divorced, 4 separated, 5 never
    married). A married person with no spouse found in the consumer unit is "married, spouse absent", as in the CPS.
  - student: IN_COLL 1 or 2 (enrolled in college full or part time). Not in the labor force is not asked per member,
    so the CPS's labor-force condition is dropped. Disability: not asked (all False).
  - income per member: wages SALARYXM, self-employment SEMPFRMM, Social Security and railroad retirement SOCRRXM,
    SSI SSIXM (missing = not asked = 0, as in build_cex.R). The consumer unit's interest, dividends, rents, pensions
    and other income are asked of the unit as a whole and are left out of the person's gross income for the income
    test. Gross income: wages + self-employment + the taxable part of Social Security (CBO's 3a formula).
  - support answer: none (-1). BLS's own tax unit codes are kept as passthrough columns and as a comparison
    (bls_unit: BLS's unit after build_cex.R's repairs and folding of dependent filers into the claiming unit).

Settings: tax_units.reassign (CBO's reassignment pass; true when absent). Run before build_cex.R.
Usage (through Slurm): python roster_cex.py [year ...] [--out DIR]   (no years: every year in settings cex.years)
Writes, to the pipeline's data folder ({REBUILD_OUT or paths.out}/data, as common.py resolves it) unless --out names
another: persons_cex_<year>.parquet and units_cex_<year>.parquet (the rules' output, with BLS's codes as comparison
columns, read by the checks), mfs_cex_<year>.parquet (the separate returns), and cex_persons_module_<year>.csv.gz,
the person file build_cex.R reads.
"""
import argparse
import os

import numpy as np
import pandas as pd

import common
import tax_units as T

CEX_RAW = common.RAW['cex']
MEMBER_COLS = ['NEWID', 'MEMBNO', 'AGE', 'SEX', 'CU_CODE', 'MARITAL', 'IN_COLL', 'TAX_UNIT', 'TU_CODE', 'TU_DPNDT',
               'SALARYXM', 'SEMPFRMM', 'SOCRRXM', 'SSIXM']
MFS_SALT = 20261023        # step 1's salt for the separate-return draw
REASSIGN = bool(common.SETTINGS.get('tax_units', {}).get('reassign', True))


def year_files(prefix, year):
    """build_cex.R's five quarterly files of survey year N: Q1 of N, Q2-Q4 of N, Q1 of N+1."""
    yy = year % 100
    q1 = f'{year}/{prefix}{yy:02d}1x.csv' if year <= 2019 else f'{year - 1}/{prefix}{yy:02d}1.csv'
    return [q1] + [f'{year}/{prefix}{yy:02d}{q}.csv' for q in (2, 3, 4)] + [f'{year}/{prefix}{(yy + 1) % 100:02d}1.csv']


def _read(prefix, year, cols):
    out = []
    for f in year_files(prefix, year):
        p = f'{CEX_RAW}/{f}'
        assert os.path.exists(p), p
        h = pd.read_csv(p, nrows=0).columns
        h = {c.upper(): c for c in h}
        miss = [c for c in cols if c not in h]
        assert not miss, (p, miss)
        d = pd.read_csv(p, usecols=[h[c] for c in cols], na_values=['', '.'], dtype={h['TU_DPNDT']: str}
                        if 'TU_DPNDT' in cols else None, low_memory=False)
        d.columns = [c.upper() for c in d.columns]
        d['src'] = os.path.basename(p)
        out.append(d)
    return pd.concat(out, ignore_index=True)


def interview_weights(year):
    """FMLI weight per interview: FINLWT21 / 4 x MO_SCOPE / 3 (build_cex.R's read_fmli)."""
    f = _read('fmli', year, ['NEWID', 'FINLWT21', 'QINTRVMO', 'QINTRVYR', 'FAM_SIZE'])
    assert not f.NEWID.duplicated().any() and not f.isna().any().any()
    y, m = f.QINTRVYR.to_numpy(), f.QINTRVMO.to_numpy()
    scope = np.select([(y == year) & (m <= 1), (y == year) & (m == 2), (y == year) & (m == 3), (y == year) & (m >= 4),
                       (y == year + 1) & (m == 1), (y == year + 1) & (m == 2), (y == year + 1) & (m == 3)],
                      [0, 1, 2, 3, 3, 2, 1], 0)
    f['weight'] = f.FINLWT21 / 4 * scope / 3
    return f[['NEWID', 'weight', 'FAM_SIZE']]


def bls_units(m):
    """BLS's unit for each person after build_cex.R's repairs: a dependent claimed by another unit moves to it, and a
    unit whose primary filer is claimed by another unit (a dependent filer) is folded into the claiming unit. The
    repairs only recode roles, so they matter here only through which unit a dependent filer's head is."""
    m = m.copy()
    m['claim'] = pd.to_numeric(m.TU_DPNDT, errors='coerce')
    m['claimed_elsewhere'] = m.claim.notna() & (m.claim != m.TAX_UNIT)
    tu = m.TU_CODE.to_numpy().copy()
    key = [m.NEWID, m.TAX_UNIT]
    no_payer = ~pd.Series(tu == 1).groupby(key).transform('any').to_numpy()
    has_ref = (m.CU_CODE == 1).groupby(key).transform('any').to_numpy()
    no_spouse = ~pd.Series(tu == 2).groupby(key).transform('any').to_numpy() & \
        (m.CU_CODE == 2).groupby(key).transform('any').to_numpy()
    tu = np.where(no_payer & (m.CU_CODE == 1), 1, np.where(no_spouse & (m.CU_CODE == 2) & has_ref & (tu != 2), 2, tu))
    still = ~pd.Series(tu == 1).groupby(key).transform('any').to_numpy()
    order = m.assign(negage=-m.AGE.fillna(-1)).sort_values(['NEWID', 'TAX_UNIT', 'negage', 'MEMBNO'])
    first = order.groupby(['NEWID', 'TAX_UNIT']).head(1).index
    promote = np.zeros(len(m), bool); promote[m.index.get_indexer(first)] = True
    tu = np.where(still & promote, 1, tu)
    home = np.where((tu == 3) & m.claimed_elsewhere, m.claim, m.TAX_UNIT).astype(int)
    m['bls_role'] = tu
    m['bls_home'] = home
    # dependent filers: heads (role 1) claimed by another unit; their whole home unit goes to the claimant
    df = m[(tu == 1) & m.claimed_elsewhere.to_numpy()]
    fold = dict(zip(zip(df.NEWID, df.bls_home), df.claim.astype(int)))
    m['bls_unit'] = [fold.get((n, h), h) for n, h in zip(m.NEWID, m.bls_home)]
    m['bls_dep_filer'] = [(n, h) in fold for n, h in zip(m.NEWID, m.bls_home)]
    return m


def read_roster(year):
    """The standard member list for one CEX survey year."""
    w = interview_weights(year)
    x = _read('memi', year, MEMBER_COLS).drop(columns='src')
    assert not x[['NEWID', 'MEMBNO']].duplicated().any()
    x = x.merge(w, on='NEWID', how='left')
    print(f'{year}: {len(x)} member rows, {x.NEWID.nunique()} interviews; interviews without an FMLI record: '
          f'{x.loc[x.weight.isna(), "NEWID"].nunique()}; with weight 0: {x.loc[x.weight == 0, "NEWID"].nunique()}')
    x = x[x.weight > 0].reset_index(drop=True)
    for c in ['SALARYXM', 'SEMPFRMM', 'SOCRRXM', 'SSIXM']:      # not asked = no such income (build_cex.R)
        x[c] = x[c].fillna(0.0)
    print('  missing AGE:', int(x.AGE.isna().sum()), '; SEX values', x.SEX.value_counts().to_dict(),
          '; MARITAL values', x.MARITAL.value_counts(dropna=False).to_dict(),
          '; IN_COLL values', x.IN_COLL.value_counts(dropna=False).to_dict())
    # BLS's '.' for AGE (2015: seven children, CU_CODE 3, in two interviews of one consumer unit): age 8, the middle
    # of the ages under 18, flagged as assumed. Any other missing age stops the run.
    age_missing = x.AGE.isna().to_numpy()
    if age_missing.any():
        print(x[age_missing][['NEWID', 'MEMBNO', 'CU_CODE', 'TU_CODE']].to_string())
        assert (x.CU_CODE[age_missing] == 3).all(), 'a missing age outside the children: decide a rule first'
        x['AGE'] = x.AGE.fillna(8)
    nref = (x.CU_CODE == 1).groupby(x.NEWID).sum()
    assert (nref == 1).all(), f'interviews without exactly one reference person: {(nref != 1).sum()}'
    x['age_missing'] = age_missing
    x = bls_units(x)

    # person numbers: the reference person is 1 (CBO's rules key the householder's unit on it), others by MEMBNO
    x['is_ref'] = x.CU_CODE == 1
    x = x.sort_values(['NEWID', 'is_ref', 'MEMBNO'], ascending=[True, False, True]).reset_index(drop=True)
    x['pn'] = x.groupby('NEWID').cumcount() + 1
    r = pd.DataFrame({'survey': 'cex', 'year': year, 'hh': x.NEWID.astype(np.int64), 'pn': x.pn, 'age': x.AGE.astype(int),
                      'sex': x.SEX.astype(int), 'weight': x.weight, 'gq': False})
    cu = x.CU_CODE.to_numpy()
    ref_sex = r.hh.map(r[cu == 1].set_index('hh').sex).to_numpy()
    same_sex = r.sex.to_numpy() == ref_sex
    mar = x.MARITAL.fillna(5).astype(int).to_numpy()
    hh = r.hh.to_numpy()

    # couples beyond the reference couple: one married child (3) and one married in-law (5) of the other sex, both 15+
    cand_c = (cu == 3) & (mar == 1) & (r.age >= 15).to_numpy()
    cand_l = (cu == 5) & (mar == 1) & (r.age >= 15).to_numpy()
    nc = pd.Series(cand_c).groupby(hh).transform('sum').to_numpy()
    nl = pd.Series(cand_l).groupby(hh).transform('sum').to_numpy()
    sex_c = pd.Series(np.where(cand_c, r.sex, 0)).groupby(hh).transform('max').to_numpy()
    sex_l = pd.Series(np.where(cand_l, r.sex, 0)).groupby(hh).transform('max').to_numpy()
    pair_hh = (nc == 1) & (nl == 1) & (sex_c != sex_l)
    sf_ref = cand_c & pair_hh
    sf_sp = cand_l & pair_hh
    # then any other two married people outside the reference couple, 15 or over, of the other sex, when exactly
    # two such remain and both are relatives (a parent couple, say) or both are not: the man is the subfamily's head
    rest = (mar == 1) & ~np.isin(cu, [1, 2]) & ~sf_ref & ~sf_sp & (r.age >= 15).to_numpy()
    n_rest = pd.Series(rest).groupby(hh).transform('sum').to_numpy()
    rel_rest = pd.Series(rest & np.isin(cu, [3, 4, 5, 6, 7, 8])).groupby(hh).transform('sum').to_numpy()
    men_rest = pd.Series(rest & (r.sex == 1).to_numpy()).groupby(hh).transform('sum').to_numpy()
    pair2 = rest & ~pair_hh & (n_rest == 2) & (men_rest == 1) & ((rel_rest == 2) | (rel_rest == 0))
    sf_ref = sf_ref | (pair2 & (r.sex == 1).to_numpy())
    sf_sp = sf_sp | (pair2 & (r.sex == 2).to_numpy())
    unrel_pair = pair2 & (cu == 0)
    pn = r.pn.to_numpy()
    sf_ref_pn = pd.Series(np.where(sf_ref, pn, 0)).groupby(hh).transform('max').to_numpy()
    sf_sp_pn = pd.Series(np.where(sf_sp, pn, 0)).groupby(hh).transform('max').to_numpy()
    sp_ref = pd.Series(np.where(cu == 2, pn, 0)).groupby(hh).transform('max').to_numpy()   # the reference's spouse

    # spouse pointers and marital status in the CPS's terms (MARST 1 married spouse present, 2 spouse absent)
    sp_pn = np.select([cu == 1, cu == 2, sf_ref, sf_sp], [sp_ref, 1, sf_sp_pn, sf_ref_pn], 0)
    married_sp = (sp_pn > 0)
    marst = np.where(married_sp, 1, np.where(mar == 1, 2, np.select([mar == 2, mar == 3, mar == 4], [5, 4, 3], 6)))
    r['sp_pn'] = sp_pn; r['mom_pn'] = 0; r['pop_pn'] = 0; r['marst'] = marst
    r['married_sp'] = married_sp; r['married_absent'] = marst == 2

    # Census codes. A family household: the reference person has a relative (codes 2-8) in the consumer unit.
    family = pd.Series(np.isin(cu, [2, 3, 4, 5, 6, 7, 8])).groupby(hh).transform('any').to_numpy()
    perrp = np.select(
        [cu == 1, (cu == 2) & ~same_sex, cu == 2, cu == 3, cu == 4, cu == 7, cu == 6, np.isin(cu, [5, 8]),
         (cu == 9) & ~same_sex, cu == 9, cu == 0],
        [np.where(family, 40, 41), 42, 45, 48, 49, 50, 51, 52, 44, 47, 59], -1)
    assert (perrp > 0).all(), pd.Series(cu[perrp < 0]).value_counts()
    in_pf = np.isin(cu, [2, 3, 4, 5, 6, 7, 8])
    kin_ref = np.select([cu == 3, cu == 4], [3, 23], 35)      # related subfamily head: child / grandchild / other
    kin_sp = np.select([cu == 3, cu == 4], [6, 27], 39)       # and spouse
    hhdfmx = np.select([(cu == 1) & family, cu == 1, unrel_pair & sf_ref, unrel_pair & sf_sp, sf_ref, sf_sp, cu == 2,
                        in_pf & (cu == 3), in_pf & (cu == 4), in_pf],
                       [1, 49, 46, 47, kin_ref, kin_sp, 2, 4, 25, 37], 50)
    r['perrp'] = perrp; r['hhdfmx'] = hhdfmx
    sec = hhdfmx == 50                                    # secondary individuals (partners, roommates)
    sec_k = pd.Series(sec.astype(int)).groupby(hh).cumsum().to_numpy()
    sf = sf_ref | sf_sp                                   # the one paired couple (subfamily) per consumer unit
    r['ftype'] = np.select([hhdfmx == 49, sf & ~unrel_pair, unrel_pair, sec], [2, 3, 4, 5], 1)
    r['famrel'] = np.select([np.isin(hhdfmx, [1, 49]) | sf_ref, (hhdfmx == 2) | sf_sp, hhdfmx == 4, sec],
                            [1, 2, 3, 0], 4)
    r['famid'] = np.select([sf, sec], [2, 2 + sec_k], 1)
    r['ffpos'] = r.famid
    r['fheadidx'] = np.select([sf, sec | (hhdfmx == 49)], [sf_ref_pn, pn], 1)
    r['relate'] = cu                                      # the survey's own relationship code

    # student and disability
    r['in_school'] = x.IN_COLL.isin([1, 2]).to_numpy()
    for c in ['nilf_school', 'ill_disabled', 'diff_any', 'has_disab_income']:
        r[c] = False

    # incomes (CBO's 3a, from the member items)
    wage = x.SALARYXM.to_numpy(float); se = x.SEMPFRMM.to_numpy(float); ssb = x.SOCRRXM.to_numpy(float)
    ssi = x.SSIXM.to_numpy(float)
    gi0 = wage + se
    for c in T.INCOME_ITEMS:
        r[c] = 0.0
    r['wage'] = wage; r['selfemp'] = se; r['ssb'] = ssb; r['ssi'] = ssi
    r['gi'] = gi0 + T.taxable_social_security(gi0, ssb, marst == 1)
    r['support'] = -1
    r['gi_obs'] = True

    # assumed inputs
    partner_hh = pd.Series(cu == 9).groupby(hh).transform('any').to_numpy()
    r['age_assumed'] = x.age_missing.to_numpy()
    r['link_assumed'] = sf_ref | sf_sp | ((cu == 4) & (r.age < 24).to_numpy()) | \
        ((cu == 3) & (r.age < 24).to_numpy() & partner_hh)
    r['income_assumed'] = False
    r['pair_assumed'] = sf_ref | sf_sp

    # BLS passthrough
    for c in ['NEWID', 'MEMBNO', 'CU_CODE', 'TAX_UNIT', 'TU_CODE', 'TU_DPNDT', 'IN_COLL', 'MARITAL', 'bls_unit',
              'bls_role', 'bls_dep_filer', 'FAM_SIZE']:
        r[c] = x[c].to_numpy()
    r['TU_DPNDT'] = r.TU_DPNDT.fillna('').astype(str)
    r['MARITAL'] = r.MARITAL.fillna(-1).astype(int)
    r['IN_COLL'] = r.IN_COLL.fillna(-1).astype(int)
    assert not r.drop(columns=['TU_DPNDT']).isna().any().any(), r.isna().sum()[r.isna().sum() > 0]
    assert (r.groupby('hh').pn.min() == 1).all()
    return r


def build(year):
    """Units by the shared rules (reassignment per the settings), the comparison with BLS's units, and the separate returns."""
    import targets as TG
    r = read_roster(year)
    persons, units = T.build_units(r, year, reassign_pass=REASSIGN)
    # BLS comparison: a module unit matches when its people are exactly the people of one BLS unit (after the
    # repairs and the folding of dependent filers); dependents' own returns are not separate units in either
    p = persons
    mod_sets = p.groupby('uid').apply(lambda g: frozenset(g.pn), include_groups=False)
    bls_sets = p.groupby(['hh', 'bls_unit']).pn.apply(frozenset)
    bls_of = p.set_index(['hh', 'pn']).bls_unit
    head = p[p.tu_rel == 1].set_index('uid')
    main = ~units.dep_return
    hk = list(zip(head.hh, head.pn))
    units.loc[main, 'bls_tax_unit'] = units.loc[main, 'uid'].map(pd.Series(bls_of.reindex(hk).to_numpy(), index=head.index))
    units.loc[main, 'bls_head_role'] = units.loc[main, 'uid'].map(
        pd.Series(p.set_index(['hh', 'pn']).bls_role.reindex(hk).to_numpy(), index=head.index))
    same = [mod_sets[u] == bls_sets.get((h, b), frozenset()) for u, h, b in
            zip(units.loc[main, 'uid'], units.loc[main, 'hh'], units.loc[main, 'bls_tax_unit'].astype(int))]
    units.loc[main, 'bls_same_members'] = np.array(same, dtype=int)
    units['any_pair_assumed'] = units.uid.map(p.groupby('uid').pair_assumed.max()).fillna(False).astype(bool)
    # separate returns: every joint couple a candidate (the CEX has no filing step), couples taken in the order of the
    # keyed draw until their weight reaches half of SOI Table 1.6's separate returns
    target = sum(TG.table_1_6(year)[('separate', k)] for k in range(6))
    mu = units[main].reset_index(drop=True)
    chosen, share, mfs = T.mfs_split(mu, persons, np.ones(len(mu), bool), target, salt=MFS_SALT)
    print(f'{year}: SOI Table 1.6 separate returns {target / 1e6:.3f} m; {len(chosen)} CEX couples split '
          f'({share:.4f} of joint couples by weight)')
    units['mfs_split'] = units.uid.isin(chosen)
    return persons, units, mfs, target


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('years', type=int, nargs='*', help='survey years (default: settings cex.years)')
    ap.add_argument('--out', default=common.DATA)
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    for y in a.years or [int(y) for y in common.SETTINGS['cex']['years']]:
        persons, units, mfs, target = build(y)
        persons.to_parquet(f'{a.out}/persons_cex_{y}.parquet')
        units.to_parquet(f'{a.out}/units_cex_{y}.parquet')
        mfs.to_parquet(f'{a.out}/mfs_cex_{y}.parquet')
        # the person file build_cex.R reads in module mode: each person's return (ret_tu, a unit number below 100
        # within the interview) and role on it (1 primary, 2 spouse, 3 dependent)
        p = persons
        ret = p.tu_id.to_numpy().copy()
        role = np.where(p.tu_rel == 1, 1, np.where(p.tu_rel == 2, 2, 3))
        split = p.uid.isin(units.loc[units.mfs_split, 'uid']).to_numpy()
        p1, p2 = T.primary_and_spouse(p, units.loc[units.mfs_split, 'uid'])
        k2 = set(zip(p2.hh, p2.pn))
        is2 = np.array([s and (h, n) in k2 for s, h, n in zip(split, p.hh, p.pn)])
        p1pn = p.uid.map(p1.pn).to_numpy()
        ret = np.where(split & ~is2, p1pn, ret)          # person 1 and the dependents: person 1's number
        ret = np.where(is2, p.pn, ret)                   # person 2: their own number
        role = np.where(split & ~is2 & (p.tu_rel.isin([1, 2])), 1, np.where(is2, 1, role))
        assert (ret < 100).all() and not pd.isna(ret).any()
        u = units.set_index('uid')
        out = pd.DataFrame({'NEWID': p.NEWID, 'MEMBNO': p.MEMBNO, 'ret_tu': ret.astype(int), 'role': role,
                            'married': ((p.uid.map(u.filing_status) == 'joint') & ~split).astype(int).to_numpy(),
                            'mfs': split.astype(int), 'any_link_assumed': p.uid.map(u.any_link_assumed).astype(int)})
        assert not out.isna().any().any()
        assert not out.duplicated(['NEWID', 'MEMBNO']).any()
        out.to_csv(f'{a.out}/cex_persons_module_{y}.csv.gz', index=False)
        m = units[~units.dep_return]
        w = m.weight / 1e6
        print(f'{y}: {len(persons)} people ({persons.weight.sum() / 1e6:.1f} m), {len(m)} units ({w.sum():.2f} m), '
              f'{units.dep_return.sum()} dependents\' own returns; by filing status',
              w.groupby(m.filing_status).sum().round(2).to_dict(),
              f'; same members as BLS {(w * m.bls_same_members).sum() / w.sum():.3f} of units; '
              f'link assumed {(w * m.any_link_assumed).sum():.2f} m units; couples paired by the fallback '
              f'{(w * m.any_pair_assumed).sum():.2f} m')


if __name__ == '__main__':
    main()
