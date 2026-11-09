"""The CEX reader for the shared tax-unit rules (tax_units.py). It runs before build_cex.R.

For each survey year it reads the five quarterly member and family files of the Consumer Expenditure interview
survey, as build_cex.R does. One interview is treated as one household. It builds the standard member list, runs
the rules, and splits some married couples into separate returns. It writes the persons, the units and the separate
returns, with the Bureau of Labor Statistics' own tax units beside them for comparison, and the person file that
build_cex.R reads. TAX_UNITS.md, "Each survey", describes how the CEX stands in for the inputs it lacks.
"""
import argparse
import os

import numpy as np
import pandas as pd

from shared import common
from prep import tax_units as T

CEX_RAW = common.RAW['cex']
MEMBER_COLS = ['NEWID', 'MEMBNO', 'AGE', 'SEX', 'CU_CODE', 'MARITAL', 'IN_COLL', 'TAX_UNIT', 'TU_CODE', 'TU_DPNDT',
               'SALARYXM', 'SEMPFRMM', 'SOCRRXM', 'SSIXM']
MFS_SALT = 20261023        # the salt of the draw that picks couples for separate returns; the CPS uses the same one


# ---- Reading the CEX ----

def year_files(prefix, year):
    """build_cex.R's five quarterly files of survey year N: the first quarter of N, the second to fourth quarters of
    N, and the first quarter of N+1."""
    yy = year % 100
    q1 = f'{year}/{prefix}{yy:02d}1x.csv' if year <= 2019 else f'{year - 1}/{prefix}{yy:02d}1.csv'
    return [q1] + [f'{year}/{prefix}{yy:02d}{q}.csv' for q in (2, 3, 4)] + [f'{year}/{prefix}{(yy + 1) % 100:02d}1.csv']


def _read(prefix, year, cols):
    """The columns cols of a year's five files of one kind (prefix 'memi' for members, 'fmli' for families), stacked,
    with the name of each row's file in column src."""
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
    """The weight of each interview: FINLWT21 / 4 x MO_SCOPE / 3, as build_cex.R's read_fmli computes it. MO_SCOPE
    is the number of the interview's three reference months that fall in the survey year."""
    f = _read('fmli', year, ['NEWID', 'FINLWT21', 'QINTRVMO', 'QINTRVYR', 'FAM_SIZE'])
    assert not f.NEWID.duplicated().any() and not f.isna().any().any()
    y, m = f.QINTRVYR.to_numpy(), f.QINTRVMO.to_numpy()
    scope = np.select([(y == year) & (m <= 1), (y == year) & (m == 2), (y == year) & (m == 3), (y == year) & (m >= 4),
                       (y == year + 1) & (m == 1), (y == year + 1) & (m == 2), (y == year + 1) & (m == 3)],
                      [0, 1, 2, 3, 3, 2, 1], 0)
    f['weight'] = f.FINLWT21 / 4 * scope / 3
    return f[['NEWID', 'weight', 'FAM_SIZE']]


def bls_units(m):
    """Each person's tax unit as the Bureau of Labor Statistics (BLS) records it, after build_cex.R's repairs.

    The repairs are two. A dependent claimed by another unit moves to that unit. A unit whose primary filer is
    claimed by another unit (a dependent filer) is folded into the claiming unit. The repairs only recode roles. Here
    they matter only for which unit a dependent filer's head belongs to."""
    m = m.copy()
    m['claim'] = pd.to_numeric(m.TU_DPNDT, errors='coerce')
    m['claimed_elsewhere'] = m.claim.notna() & (m.claim != m.TAX_UNIT)
    # A unit with no primary filer takes the reference person, if in the unit, as primary filer, and otherwise its
    # oldest member. A unit that holds the reference person and their spouse, but no one in the spouse role, gives
    # the spouse that role.
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
    # Dependent filers are primary filers (role 1) claimed by another unit. Their whole unit goes to the claimant.
    df = m[(tu == 1) & m.claimed_elsewhere.to_numpy()]
    fold = dict(zip(zip(df.NEWID, df.bls_home), df.claim.astype(int)))
    m['bls_unit'] = [fold.get((n, h), h) for n, h in zip(m.NEWID, m.bls_home)]
    m['bls_dep_filer'] = [(n, h) in fold for n, h in zip(m.NEWID, m.bls_home)]
    return m


# ---- The member list ----

def read_roster(year):
    """The standard member list for one CEX survey year."""
    # 1. Read the members of every interview with a positive weight. An income item that was not asked means no such
    #    income, as in build_cex.R.
    w = interview_weights(year)
    x = _read('memi', year, MEMBER_COLS).drop(columns='src')
    assert not x[['NEWID', 'MEMBNO']].duplicated().any()
    x = x.merge(w, on='NEWID', how='left')
    print(f'{year}: {len(x)} member rows, {x.NEWID.nunique()} interviews; interviews without an FMLI record: '
          f'{x.loc[x.weight.isna(), "NEWID"].nunique()}; with weight 0: {x.loc[x.weight == 0, "NEWID"].nunique()}')
    x = x[x.weight > 0].reset_index(drop=True)
    for c in ['SALARYXM', 'SEMPFRMM', 'SOCRRXM', 'SSIXM']:
        x[c] = x[c].fillna(0.0)
    print('  missing AGE:', int(x.AGE.isna().sum()), '; SEX values', x.SEX.value_counts().to_dict(),
          '; MARITAL values', x.MARITAL.value_counts(dropna=False).to_dict(),
          '; IN_COLL values', x.IN_COLL.value_counts(dropna=False).to_dict())
    # 2. A missing age. In 2015 BLS marks seven children's ages as missing ('.'). They are children of the
    #    reference person in two interviews of one consumer unit. They are given age 8, the middle of the ages under
    #    18, and flagged as assumed. Any other missing age stops the run.
    age_missing = x.AGE.isna().to_numpy()
    if age_missing.any():
        print(x[age_missing][['NEWID', 'MEMBNO', 'CU_CODE', 'TU_CODE']].to_string())
        assert (x.CU_CODE[age_missing] == 3).all(), 'a missing age outside the children: decide a rule first'
        x['AGE'] = x.AGE.fillna(8)
    nref = (x.CU_CODE == 1).groupby(x.NEWID).sum()
    assert (nref == 1).all(), f'interviews without exactly one reference person: {(nref != 1).sum()}'
    x['age_missing'] = age_missing
    x = bls_units(x)

    # 3. Person numbers. The reference person is 1, because CBO's rules key the householder's unit on it. The others
    #    follow in the order of their member numbers.
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

    # 4. Couples other than the reference person and spouse. The CEX has no spouse pointers. First, a household
    #    with exactly one married child (CU_CODE 3) and exactly one married in-law (CU_CODE 5), of the other sex and
    #    both 15 or over, has them as a couple.
    cand_c = (cu == 3) & (mar == 1) & (r.age >= 15).to_numpy()
    cand_l = (cu == 5) & (mar == 1) & (r.age >= 15).to_numpy()
    nc = pd.Series(cand_c).groupby(hh).transform('sum').to_numpy()
    nl = pd.Series(cand_l).groupby(hh).transform('sum').to_numpy()
    sex_c = pd.Series(np.where(cand_c, r.sex, 0)).groupby(hh).transform('max').to_numpy()
    sex_l = pd.Series(np.where(cand_l, r.sex, 0)).groupby(hh).transform('max').to_numpy()
    pair_hh = (nc == 1) & (nl == 1) & (sex_c != sex_l)
    sf_ref = cand_c & pair_hh
    sf_sp = cand_l & pair_hh
    # Otherwise, when exactly two married people of the other sex, 15 or over, remain outside the reference couple,
    # and both are relatives (a parent couple, say) or both are not, they are a couple. The man is the subfamily's
    # reference person.
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
    sp_ref = pd.Series(np.where(cu == 2, pn, 0)).groupby(hh).transform('max').to_numpy()   # the reference person's spouse

    # 5. Spouse pointers, and marital status in the CPS's terms (MARST 1 married with the spouse present, 2 married
    #    with the spouse absent).
    sp_pn = np.select([cu == 1, cu == 2, sf_ref, sf_sp], [sp_ref, 1, sf_sp_pn, sf_ref_pn], 0)
    married_sp = (sp_pn > 0)
    marst = np.where(married_sp, 1, np.where(mar == 1, 2, np.select([mar == 2, mar == 3, mar == 4], [5, 4, 3], 6)))
    r['sp_pn'] = sp_pn; r['mom_pn'] = 0; r['pop_pn'] = 0; r['marst'] = marst
    r['married_sp'] = married_sp; r['married_absent'] = marst == 2

    # 6. Census's relationship code (PERRP) and family position code (HHDFMX), which CBO's rules read. A household
    #    is a family household when the reference person has a relative (CU_CODE 2-8) in the consumer unit.
    family = pd.Series(np.isin(cu, [2, 3, 4, 5, 6, 7, 8])).groupby(hh).transform('any').to_numpy()
    perrp = np.select(
        [cu == 1, (cu == 2) & ~same_sex, cu == 2, cu == 3, cu == 4, cu == 7, cu == 6, np.isin(cu, [5, 8]),
         (cu == 9) & ~same_sex, cu == 9, cu == 0],
        [np.where(family, 40, 41), 42, 45, 48, 49, 50, 51, 52, 44, 47, 59], -1)
    assert (perrp > 0).all(), pd.Series(cu[perrp < 0]).value_counts()
    in_pf = np.isin(cu, [2, 3, 4, 5, 6, 7, 8])
    kin_ref = np.select([cu == 3, cu == 4], [3, 23], 35)      # a related subfamily's reference person: child, grandchild, other
    kin_sp = np.select([cu == 3, cu == 4], [6, 27], 39)       # and its spouse
    hhdfmx = np.select([(cu == 1) & family, cu == 1, unrel_pair & sf_ref, unrel_pair & sf_sp, sf_ref, sf_sp, cu == 2,
                        in_pf & (cu == 3), in_pf & (cu == 4), in_pf],
                       [1, 49, 46, 47, kin_ref, kin_sp, 2, 4, 25, 37], 50)
    r['perrp'] = perrp; r['hhdfmx'] = hhdfmx
    sec = hhdfmx == 50                                    # people with no relatives in the household (partners, roommates)
    sec_k = pd.Series(sec.astype(int)).groupby(hh).cumsum().to_numpy()
    sf = sf_ref | sf_sp                                   # the one paired couple (subfamily) of the consumer unit
    r['ftype'] = np.select([hhdfmx == 49, sf & ~unrel_pair, unrel_pair, sec], [2, 3, 4, 5], 1)
    r['famrel'] = np.select([np.isin(hhdfmx, [1, 49]) | sf_ref, (hhdfmx == 2) | sf_sp, hhdfmx == 4, sec],
                            [1, 2, 3, 0], 4)
    r['famid'] = np.select([sf, sec], [2, 2 + sec_k], 1)
    r['ffpos'] = r.famid
    r['fheadidx'] = np.select([sf, sec | (hhdfmx == 49)], [sf_ref_pn, pn], 1)
    r['relate'] = cu                                      # the survey's own relationship code

    # 7. The student and disability tests. A person enrolled in college is a student. The CEX has no disability
    #    items.
    r['in_school'] = x.IN_COLL.isin([1, 2]).to_numpy()
    for c in ['nilf_school', 'ill_disabled', 'diff_any', 'has_disab_income']:
        r[c] = False

    # 8. Incomes, from the member items (CBO's 3a): wages, self-employment income, Social Security and SSI.
    wage = x.SALARYXM.to_numpy(float); se = x.SEMPFRMM.to_numpy(float); ssb = x.SOCRRXM.to_numpy(float)
    ssi = x.SSIXM.to_numpy(float)
    gi0 = wage + se
    for c in T.INCOME_ITEMS:
        r[c] = 0.0
    r['wage'] = wage; r['selfemp'] = se; r['ssb'] = ssb; r['ssi'] = ssi
    r['gi'] = gi0 + T.taxable_social_security(gi0, ssb, marst == 1)
    r['support'] = -1
    r['gi_obs'] = True

    # 9. The flags for assumed inputs. A child's unit is flagged when another parent may be in the household: for a
    #    grandchild under 24, and for the reference person's child under 24 when the household has an unmarried
    #    partner. The paired couples are flagged too.
    partner_hh = pd.Series(cu == 9).groupby(hh).transform('any').to_numpy()
    r['age_assumed'] = x.age_missing.to_numpy()
    r['link_assumed'] = sf_ref | sf_sp | ((cu == 4) & (r.age < 24).to_numpy()) | \
        ((cu == 3) & (r.age < 24).to_numpy() & partner_hh)
    r['income_assumed'] = False
    r['pair_assumed'] = sf_ref | sf_sp

    # 10. Carry the BLS items through, for the comparison with BLS's units.
    for c in ['NEWID', 'MEMBNO', 'CU_CODE', 'TAX_UNIT', 'TU_CODE', 'TU_DPNDT', 'IN_COLL', 'MARITAL', 'bls_unit',
              'bls_role', 'bls_dep_filer', 'FAM_SIZE']:
        r[c] = x[c].to_numpy()
    r['TU_DPNDT'] = r.TU_DPNDT.fillna('').astype(str)
    r['MARITAL'] = r.MARITAL.fillna(-1).astype(int)
    r['IN_COLL'] = r.IN_COLL.fillna(-1).astype(int)
    assert not r.drop(columns=['TU_DPNDT']).isna().any().any(), r.isna().sum()[r.isna().sum() > 0]
    assert (r.groupby('hh').pn.min() == 1).all()
    return r


# ---- The units ----

def build(year):
    """The units by the shared rules, the comparison with BLS's units, and the separate returns."""
    from prep import soi as TG
    # 1. Run the rules.
    r = read_roster(year)
    persons, units = T.build_units(r, year)
    # 2. Compare with BLS's units. A unit matches when its people are exactly the people of one BLS unit, after the
    #    repairs and the folding of dependent filers. A dependent's own return is not a separate unit in either.
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
    # 3. Separate returns. Every joint couple is a candidate, because the CEX has no filing step. Couples are taken
    #    in the order of the keyed draw until their weight reaches half of SOI Table 1.6's separate returns.
    target = sum(TG.table_1_6(year)[('separate', k)] for k in range(6))
    mu = units[main].reset_index(drop=True)
    chosen, share, mfs = T.mfs_split(mu, persons, np.ones(len(mu), bool), target, salt=MFS_SALT)
    print(f'{year}: SOI Table 1.6 separate returns {target / 1e6:.3f} m; {len(chosen)} CEX couples split '
          f'({share:.4f} of joint couples by weight)')
    units['mfs_split'] = units.uid.isin(chosen)
    return persons, units, mfs, target


# ---- The person file for build_cex.R ----

def module_person_file(persons, units):
    """The person file build_cex.R reads. It gives each person's return (ret_tu, a unit number below 100 within the
    interview) and role on it (1 primary filer, 2 spouse, 3 dependent). On a couple split into separate returns,
    person 1 and the dependents are on person 1's return, and person 2 is the primary filer of their own."""
    p = persons
    ret = p.tu_id.to_numpy().copy()
    role = np.where(p.tu_rel == 1, 1, np.where(p.tu_rel == 2, 2, 3))
    split = p.uid.isin(units.loc[units.mfs_split, 'uid']).to_numpy()
    p1, p2 = T.primary_and_spouse(p, units.loc[units.mfs_split, 'uid'])
    k2 = set(zip(p2.hh, p2.pn))
    is2 = np.array([s and (h, n) in k2 for s, h, n in zip(split, p.hh, p.pn)])
    p1pn = p.uid.map(p1.pn).to_numpy()
    ret = np.where(split & ~is2, p1pn, ret)
    ret = np.where(is2, p.pn, ret)
    role = np.where(split & ~is2 & (p.tu_rel.isin([1, 2])), 1, np.where(is2, 1, role))
    assert (ret < 100).all() and not pd.isna(ret).any()
    u = units.set_index('uid')
    out = pd.DataFrame({'NEWID': p.NEWID, 'MEMBNO': p.MEMBNO, 'ret_tu': ret.astype(int), 'role': role,
                        'married': ((p.uid.map(u.filing_status) == 'joint') & ~split).astype(int).to_numpy(),
                        'mfs': split.astype(int), 'any_link_assumed': p.uid.map(u.any_link_assumed).astype(int)})
    assert not out.isna().any().any()
    assert not out.duplicated(['NEWID', 'MEMBNO']).any()
    return out


# ---- The whole stage ----

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('years', type=int, nargs='*', help='survey years (default: settings cex.years)')
    ap.add_argument('--out', default=common.DATA)
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    for y in a.years or [int(y) for y in common.SETTINGS['cex']['years']]:
        # 1. Build the units and write the persons, the units and the separate returns.
        persons, units, mfs, target = build(y)
        persons.to_parquet(f'{a.out}/persons_cex_{y}.parquet')
        units.to_parquet(f'{a.out}/units_cex_{y}.parquet')
        mfs.to_parquet(f'{a.out}/mfs_cex_{y}.parquet')
        # 2. Write the person file build_cex.R reads.
        out = module_person_file(persons, units)
        out.to_csv(f'{a.out}/cex_persons_module_{y}.csv.gz', index=False)
        # 3. Print a summary of the year's units.
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
