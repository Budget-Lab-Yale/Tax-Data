"""roster_scf.py: the SCF reader for the shared tax-unit rules (tax_units.py), and the SCF's units.

The SCF lists everyone in the household (positions 1-12 of the household listing) but measures in detail only the
primary economic unit (PEU): the respondent (R), a spouse or partner who shares finances, and everyone financially
dependent on them. The other adults who usually live there and are financially independent (outside the PEU) are
asked about as a group: their total wages, other income by kind, vehicles, bank accounts, other assets and debts.

read_roster(wave)   the standard member list (columns in tax_units.py's header) for one wave, every implicate a
                    household (hh = Y1). CBO's Census codes from the SCF's relationship codes:
                      R: the householder (hhdfmx 1 and perrp 40 when a relative lives there, else 49 and 41);
                      spouse (position 2 coded spouse): hhdfmx 2, perrp 42 (45 same sex), spouse pointers both ways;
                      partner (position 2 coded partner): a nonfamily member, perrp 44 (47 same sex), so CBO's
                        partner test applies, on the couple's earnings;
                      child (including in-law) 48 and hhdfmx 4; grandchild 49 and 25; parent 50; sibling 51;
                        grandparent, aunt or uncle and other relatives (cousins, nieces and nephews merged in the
                        public file) 52; all with hhdfmx 37;
                      boarder 57, paid help and other non-relatives (roommates, friends and foster children merged
                        in the public file) 59, hhdfmx 50.
                    Members who neither usually live there nor depend on the PEU are not household members (left
                    out, as the SCF's own household count does).
                    Fallbacks (tax_units.FALLBACKS['scf']), each flagged on the person:
                      age: members other than R and the spouse or partner are reported in brackets (0-3, 4-6, 7-12,
                        13-17, 18-27, then five-year groups, 93 and over); an exact age is drawn uniformly within the
                        bracket by a draw keyed by the household and position, the same in every implicate
                        (age_assumed);
                      support: the SCF's financial-dependence answer (1 for PEU members, 0 outside it) stands in for
                        the student and income tests (tax_units.flag_dependents), as Gale, Joshi, Pulliam and
                        Sabelhaus (2022) do; members other than the couple have no income of their own measured
                        (income_assumed, on adults);
                      links: no parent pointers, so every child goes to R's unit (link_assumed where another parent
                        may be present: grandchildren and other young relatives under 24, and R's children when R
                        lives with a partner); outside the PEU, two adults who both report being married or
                        living with a partner, and are the only two who do, are paired as a married couple filing
                        jointly (Gale and coauthors), as a related subfamily when one is R's relative, else an
                        unrelated one (link_assumed);
                      the group outside the PEU (column group = 1): its highest-income unit claims the group's other
                        relatives who pass the income test (tax_units.claim_within_group);
                      children living elsewhere: the number under 18 counts as dependents of R's unit only if the
                        family paid alimony or child support (X5731; the SCF does not separate the two), ages drawn
                        uniformly over 0-17.
units(wave)         the rules (tax_units.build_units, reassignment on), then separate returns from the couple's own
                    answer: a married couple living together files separately when it says it did (X5746 = 2) and
                    either spouse was married before (X7377 or X7392 = 2), Moore's condition. Each unit also says
                    how it takes the PEU's dollars in build_scf.R (peu_dollars: 'all', 'r', 'sp' or 'none'), and
                    the units outside the PEU carry their share of the group's income and balance sheet
                    (group_level = True).

Settings (settings.json "tax_units"): reassign (CBO's reassignment pass); npeu_split, 'equal' (the group's income
divided equally among its adults) or 'moore' (wages among the adults under 70 and Social Security and pensions among
those 62 and over, as Moore's code does); the group's balance sheet is divided equally either way.

Usage (through Slurm; run.sbatch's scf stage runs it before build_scf.R): python roster_scf.py [wave ...]
[--out DIR]; by default every wave in the settings, DIR {out}/data. Writes DIR/scf_persons_<wave>.parquet,
DIR/scf_units_module_<wave>.parquet and DIR/scf_npeu_dollars_<wave>.parquet.
"""
import argparse
import json
import os

import numpy as np
import pandas as pd

import tax_units as T

SETTINGS = os.environ.get('REBUILD_SETTINGS', os.path.join(os.path.dirname(__file__), 'settings.json'))
SALT_AGE = 20261024
SALT_AWAY = 20261025

# household listing positions 3-12: relationship, age, marital status, usually lives here, financially dependent
POS = {'rel': ['X108', 'X114', 'X120', 'X126', 'X132', 'X202', 'X208', 'X214', 'X220', 'X226'],
       'age': ['X110', 'X116', 'X122', 'X128', 'X134', 'X204', 'X210', 'X216', 'X222', 'X228'],
       'mar': ['X111', 'X117', 'X123', 'X129', 'X135', 'X205', 'X211', 'X217', 'X223', 'X229'],
       'lw': ['X112', 'X118', 'X124', 'X130', 'X136', 'X206', 'X212', 'X218', 'X224', 'X230'],
       'fd': ['X113', 'X119', 'X125', 'X131', 'X137', 'X207', 'X213', 'X219', 'X225', 'X231']}
HH = ['Y1', 'YY1', 'X42001', 'X8021', 'X103', 'X14', 'X19', 'X8023', 'X102', 'X7020', 'X106', 'X7050', 'X5744',
      'X5746', 'X7377', 'X7392', 'X5731', 'X5910', 'X5912', 'X4112', 'X4113', 'X4712', 'X4713', 'X4131', 'X4132',
      'X4731', 'X4732',
      # the group outside the PEU: wages, other income and its kinds, the balance sheet
      'X6403', 'X6415', 'X6406', 'X6407', 'X6408', 'X6409', 'X6410', 'X6411', 'X6412', 'X6413', 'X6414',
      'X6418', 'X6421', 'X6432', 'X6435', 'X6436', 'X6437', 'X6439']
REL_PERRP = {4: 48, 5: 49, 6: 50, 7: 52, 8: 52, 9: 52, 10: 52, 11: 51, 12: 49, 29: 52, 34: 57, 35: 59, 31: 59,
             32: 59, 36: 59, 39: 59}
RELATIVES = [4, 5, 6, 7, 8, 9, 10, 11, 12, 29]


def aconv(f):
    """Moore's annualizing factor for a period code (production builder's aconv)."""
    f = np.asarray(f)
    return np.select([f == 2, f == 3, f == 4, f == 5, np.isin(f, [6, 8, 14, 22]), f == 11, f == 12, f == 31],
                     [52.18, 26.09, 12, 4, 1, 2, 6, 24], 0.0)


def bracket(code):
    """The ages a public-file age code covers (codebook, household listing: 3 = 0-3, 6 = 4-6, 12 = 7-12,
    17 = 13-17, 25 = 18-27, c = c-2..c+2 from 30, 95 = 93 and over, here 93-97)."""
    code = np.asarray(code)
    lo = np.select([code <= 3, code <= 6, code <= 12, code <= 17, code <= 25, code >= 95], [0, 4, 7, 13, 18, 93],
                   code - 2)
    hi = np.select([code <= 3, code <= 6, code <= 12, code <= 17, code <= 25, code >= 95], [3, 6, 12, 17, 27, 97],
                   code + 2)
    return lo, hi


def read_raw(wave):
    s = json.load(open(SETTINGS))
    path = os.path.join(s['paths']['raw']['scf'].replace('{wave}', str(wave)), f'p{wave % 100:02d}i6.dta')
    want = [c.lower() for c in HH + sum(POS.values(), [])]
    with pd.read_stata(path, iterator=True) as rd:
        have = list(rd.variable_labels().keys())
    miss = [c.upper() for c in want if c not in have]
    if miss:
        print(f'wave {wave}: items not in the file (taken as 0): {miss}')
    d = pd.read_stata(path, columns=[c for c in want if c in have], convert_categoricals=False)
    d.columns = [c.upper() for c in d.columns]
    for c in miss:
        d[c] = 0
    d = d.astype(float)
    assert not d.isna().any().any(), d.isna().sum()[d.isna().sum() > 0]
    return d


def read_roster(wave, npeu_split='equal'):
    """The standard member list for one SCF wave (tax year wave - 1), and the household table it came from."""
    tax_year = wave - 1
    d = read_raw(wave)
    d['hh'] = d.Y1.astype(np.int64)
    d['w'] = d.X42001 / 5
    # --- R and the spouse or partner (positions 1 and 2) -------------------------------------------------------
    # Position 2 holds the spouse or partner (X102 > 0; 1 when that person, not the reference person, answered the
    # interview). Married or partner from the reference person's living arrangement (X8023: 1 married, 2 partner).
    has_sp = d.X102.to_numpy() > 0
    sp_code = np.where(has_sp, np.where(d.X8023.to_numpy() == 1, 2, 3), 0)
    sp_in_peu = has_sp & (d.X7020.to_numpy() == 2)
    sp_lives = has_sp & (sp_in_peu | (d.X106.to_numpy() == 1))       # a partner outside the PEU who lives there
    r_earn = np.maximum(0, d.X4112) * aconv(d.X4113) + np.maximum(0, d.X4131) * aconv(d.X4132)
    s_earn = np.maximum(0, d.X4712) * aconv(d.X4713) + np.maximum(0, d.X4731) * aconv(d.X4732)
    R = pd.DataFrame({'hh': d.hh, 'pn': 1, 'age': d.X14.astype(int), 'sex': d.X8021.astype(int), 'rel': 1,
                      'mar': d.X8023, 'lw': 1, 'fd': 1, 'gi': r_earn, 'gi_obs': True, 'support': -1,
                      'age_assumed': False})
    S = pd.DataFrame({'hh': d.hh.to_numpy(), 'pn': 2, 'age': d.X19.astype(int).to_numpy(), 'sex': d.X103.astype(int).to_numpy(), 'rel': sp_code,
                      'mar': np.where(sp_code == 2, 1, 2), 'lw': 1, 'fd': np.where(sp_in_peu, 1, 5),
                      'gi': np.where(sp_in_peu, s_earn, 0.0), 'gi_obs': sp_in_peu,
                      'support': np.where(sp_in_peu, -1, 0), 'age_assumed': False})[sp_lives]
    # --- positions 3-12 ----------------------------------------------------------------------------------------
    rows = []
    for i in range(10):
        x = pd.DataFrame({'hh': d.hh, 'yy1': d.YY1, 'pn': i + 3, **{k: d[v[i]] for k, v in POS.items()}})
        rows.append(x[x.rel > 0])
    O = pd.concat(rows, ignore_index=True)
    member = ~((O.lw == 5) & (O.fd == 5))           # neither usually lives here nor depends on the PEU: not a member
    print(f'wave {wave}: listed people who neither live there nor depend on the PEU, left out: '
          f'{(O.w if "w" in O else O.hh.map(d.set_index("hh").w))[~member].sum() / 1e6:.2f} m')
    O = O[member].copy()
    in_peu = (O.fd == 1) | (O.age <= 17)
    lo, hi = bracket(O.age.to_numpy())
    u = T.keyed_uniform(O.yy1.to_numpy().astype(np.int64) * 100 + O.pn.to_numpy(), SALT_AGE)
    O['age'] = (lo + np.floor(u * (hi - lo + 1))).astype(int)
    O['sex'] = 0
    O['gi'] = 0.0; O['gi_obs'] = False
    O['support'] = np.where(in_peu, 1, 0)
    O['age_assumed'] = True
    O['group'] = np.where(~in_peu, 1, 0)
    P = pd.concat([R, S, O.drop(columns=['yy1'])], ignore_index=True).sort_values(['hh', 'pn'], ignore_index=True)
    P['group'] = P.group.fillna(0).astype(int)
    P['weight'] = P.hh.map(d.set_index('hh').w)
    P['rel'] = P.rel.astype(int)
    # --- outside-PEU couples: exactly two group adults who report being married or living with a partner ---------
    grp = (P.group == 1)
    mar_g = grp & (P.mar == 1)
    nmar = mar_g.groupby(P.hh).transform('sum')
    pair = mar_g & (nmar == 2)
    first = pair & (P.pn == P.pn.where(pair).groupby(P.hh).transform('min'))
    P['sp_pn'] = 0
    pp = P[pair].groupby('hh').pn.agg(['min', 'max'])
    P.loc[pair, 'sp_pn'] = np.where(first[pair], P.hh[pair].map(pp['max']), P.hh[pair].map(pp['min']))
    # R and the spouse
    is_spouse = (P.pn == 2) & (P.rel == 2)
    hh_sp = set(P.hh[is_spouse])
    P.loc[is_spouse, 'sp_pn'] = 1
    P.loc[(P.pn == 1) & P.hh.isin(hh_sp), 'sp_pn'] = 2
    P['mom_pn'] = 0; P['pop_pn'] = 0
    P['married_sp'] = (P.sp_pn > 0)
    P['married_absent'] = False
    P['marst'] = np.where(P.married_sp, 1, 6)
    # --- CBO's Census codes ------------------------------------------------------------------------------------
    rel = P.rel.to_numpy()
    relative = np.isin(rel, RELATIVES)
    fam = P.hh.map(pd.Series(relative | (rel == 2)).groupby(P.hh).max()).to_numpy().astype(bool)
    r_sex = P.hh.map(P[P.pn == 1].set_index('hh').sex).to_numpy()
    same = P.sex.to_numpy() == r_sex
    perrp = np.select([rel == 1, (rel == 2) & ~same, rel == 2, (rel == 3) & ~same, rel == 3],
                      [np.where(fam, 40, 41), 42, 45, 44, 47], P.rel.map(REL_PERRP).fillna(-1).to_numpy())
    assert (perrp > 0).all(), pd.Series(rel[perrp <= 0]).value_counts()
    kin = np.select([rel == 4, np.isin(rel, [5, 12])], ['child', 'grandchild'], 'other')
    # outside-PEU couples: a related subfamily if the first member is R's relative, else an unrelated subfamily
    pair = pair.to_numpy(); first = first.to_numpy()
    rel_first = P.hh.map(pd.Series(np.where(first, relative, False)).groupby(P.hh).max()).to_numpy().astype(bool)
    in_sf = pair & rel_first
    in_uf = pair & ~rel_first
    pf = {('child', 'none'): 4, ('child', 'ref'): 3, ('child', 'sp'): 6, ('grandchild', 'none'): 25,
          ('grandchild', 'ref'): 23, ('grandchild', 'sp'): 27, ('other', 'none'): 37, ('other', 'ref'): 35,
          ('other', 'sp'): 39}
    posn = np.where(in_sf & first, 'ref', np.where(in_sf, 'sp', 'none'))
    # a non-relative married to a relative in a related subfamily is coded as that subfamily's spouse (other kin)
    pf_code = np.array([pf[(k if r else 'other', p)] for k, r, p in zip(kin, relative, posn)])
    hhdfmx = np.select([rel == 1, rel == 2, relative | in_sf, in_uf & first, in_uf],
                       [np.where(fam, 1, 49), 2, pf_code, 46, 47], 50)
    P['perrp'] = np.where(in_uf & np.isin(perrp, [55, 57, 59]), perrp - 1, perrp)   # "with relatives"
    P['hhdfmx'] = hhdfmx
    P['ftype'] = np.select([in_sf, in_uf, fam], [3, 4, 1], np.where(hhdfmx == 49, 2, 5))
    P['famrel'] = np.select([rel == 1, rel == 2, (in_sf | in_uf) & first, in_sf | in_uf, relative], [1, 2, 1, 2, 3], 0)
    P['famid'] = np.where(in_sf | in_uf, 2, 1)
    P['ffpos'] = P.famid
    sf_ref = P.hh.map(P[pair & first].set_index('hh').pn) if (pair & first).any() else pd.Series(np.nan, P.index)
    P['fheadidx'] = np.where(in_sf | in_uf, sf_ref.fillna(1), np.where(hhdfmx == 50, P.pn, 1)).astype(int)
    P['gq'] = False
    for c in ['in_school', 'nilf_school', 'ill_disabled', 'diff_any', 'has_disab_income']:
        P[c] = False
    # --- incomes: R and the spouse or partner have their earnings; the group outside the PEU its share -----------
    for c in T.INCOME_ITEMS:
        if c != 'gi':
            P[c] = 0.0
    P['wage'] = np.where(P.pn <= 2, P.gi, 0.0)
    shares = npeu_shares(P, d, npeu_split)
    g = P.group == 1
    P.loc[g, 'gi'] = shares.loc[P.index[g], 'gi'].to_numpy()
    P.loc[g, 'wage'] = shares.loc[P.index[g], 'wages'].to_numpy()
    # an adult's own income not measured (a minor's is taken as none, as everywhere); a child's unit uncertain where
    # another parent may be in the household: a grandchild, niece, nephew or other young relative, or R's child when
    # R lives with an unmarried partner (the code covers children of either); and the paired couples
    P['income_assumed'] = ~P.gi_obs & (P.age >= 18)
    r_partner = P.hh.map(P[P.pn == 2].set_index('hh').rel).fillna(0).to_numpy() == 3
    young_rel = (P.age < 24) & (P.pn > 2) & (np.isin(P.rel, [5, 7, 8, 9, 10, 11, 12, 29]) | ((P.rel == 4) & r_partner))
    P['link_assumed'] = young_rel.to_numpy() | pair
    P['relate'] = P.rel
    P['survey'] = 'scf'
    P['year'] = tax_year
    P['pair'] = pair
    P = P.drop(columns=['rel'])
    assert not P.isna().any().any(), P.isna().sum()[P.isna().sum() > 0]
    return P, d, shares


def npeu_shares(P, d, how):
    """Each outside-PEU adult's share of the group's income and balance sheet (interview-year dollars), as columns
    on P's index. The other income (X6415) is divided equally among the kinds the group reports, as Moore does."""
    g = P[P.group == 1]
    h = d.set_index('hh').loc[g.hh]
    n = g.groupby('hh').pn.transform('size').to_numpy()
    pos = lambda c: np.maximum(h[c].to_numpy(), 0)
    kinds = {'retirement_gross': ['X6406', 'X6407', 'X6408'], 'interest': ['X6410'], 'dividends': ['X6411'],
             'business_income': ['X6412'], 'rent_royalty_estate': ['X6413'], 'other_income': ['X6409', 'X6414']}
    flags = {k: sum(((h[c].to_numpy() == 1) if c != 'X6414' else (h[c].to_numpy() > 5)) for c in v)
             for k, v in kinds.items()}
    nk = sum(flags.values())
    other = np.where(nk > 0, pos('X6415') / np.maximum(nk, 1), 0.0)
    out = pd.DataFrame({'hh': g.hh.to_numpy(), 'pn': g.pn.to_numpy()}, index=g.index)
    eq = 1.0 / n
    if how == 'equal':
        w_wage = eq
        w_ret = eq
    elif how == 'moore':
        age = g.age.to_numpy()
        u70 = pd.Series(age < 70, index=g.index).groupby(g.hh).transform('sum').to_numpy()
        o62 = pd.Series(age >= 62, index=g.index).groupby(g.hh).transform('sum').to_numpy()
        w_wage = np.where(u70 > 0, (age < 70) / np.maximum(u70, 1), eq)
        w_ret = np.where(o62 > 0, (age >= 62) / np.maximum(o62, 1), eq)
    else:
        raise ValueError(how)
    out['wages'] = pos('X6403') * w_wage
    for k, f in flags.items():
        out[k] = other * f * (w_ret if k == 'retirement_gross' else eq)
    # the balance sheet, always equally; the home share is the group's share of R's home, with its mortgage
    home = np.minimum(pos('X6432'), pos('X6436') * pos('X6435') / 10000)
    out['w_cash'] = pos('X6421') * eq
    out['w_other_nonfin'] = pos('X6418') * eq
    out['w_primary_home'] = home * eq
    out['w_other_fin'] = (pos('X6432') - home) * eq
    out['w_primary_mortgage'] = pos('X6437') * pos('X6435') / 10000 * eq
    out['w_other_debt'] = pos('X6439') * eq
    # gross income for the income test (CBO's 3a items, Social Security counted as received, a fallback)
    out['gi'] = out[['wages', 'retirement_gross', 'interest', 'dividends', 'business_income',
                     'rent_royalty_estate', 'other_income']].sum(axis=1)
    return out


def units(wave, npeu_split='equal', reassign_pass=True):
    tax_year = wave - 1
    P, d, shares = read_roster(wave, npeu_split)
    # children living elsewhere: under 18, claimed by R's unit when the family paid alimony or child support
    n_away = np.where(d.X5912 > 0, d.X5912, np.where(d.X5912 == -2, np.maximum(d.X5910, 0), 0))
    n_away = np.where(d.X5731 == 1, n_away, 0).astype(int)
    aw = pd.DataFrame({'hh': np.repeat(d.hh.to_numpy(), n_away), 'pn': 1})
    aw['k'] = aw.groupby('hh').cumcount()
    yy1 = np.repeat(d.YY1.to_numpy().astype(np.int64), n_away)
    aw['age'] = np.floor(T.keyed_uniform(yy1 * 100 + aw.k.to_numpy(), SALT_AWAY) * 18).astype(int)
    persons, U = T.build_units(P, tax_year, reassign_pass=reassign_pass, away=aw[['hh', 'pn', 'age']])
    U['any_age_assumed'] |= U.uid.isin(aw.hh * 100 + 1) & (U.n_child_away > 0)
    # separate returns from the couple's own answer, with Moore's prior-marriage condition
    sep = d.set_index('hh').eval('X5746 == 2 and (X7377 == 2 or X7392 == 2)')
    main = U[~U.dep_return].reset_index(drop=True)
    pick = (main.filing_status == 'joint').to_numpy() & main.hh.map(sep).fillna(False).to_numpy() & \
        main.head_pn.isin([1, 2]).to_numpy() & main.spouse_pn.isin([1, 2]).to_numpy()
    if pick.any():
        tot = main.weight[pick].sum()
        chosen, share, mfs = T.mfs_split(main, persons, pick, 2 * (tot - 0.5 * main.weight[pick].min()))
        assert set(chosen) == set(main.uid[pick]), 'every answering couple must be split'
    else:
        chosen, mfs = np.array([], dtype=np.int64), pd.DataFrame()
    print(f'wave {wave}: couples filing separately by their own answer: {main.weight[pick].sum() / 1e6:.2f} m couples, '
          f'{2 * main.weight[pick].sum() / 1e6:.2f} m returns')
    # how each unit takes the PEU's dollars (build_scf.R): 'all' the whole PEU (R alone, a joint couple, R with the
    # partner claimed); 'r' and 'sp' R's and the spouse's or partner's shares when they are separate units; 'none'
    in_peu = persons.set_index(['hh', 'pn']).support.ne(0)          # R, a spouse or partner in the PEU, dependents
    head_in_peu = pd.MultiIndex.from_arrays([U.hh, U.head_pn]).map(in_peu).fillna(False).to_numpy().astype(bool)
    U['source'] = np.where(U.dep_return, 'dependent return',
                           np.where(U.head_pn.isin([1, 2]) & head_in_peu, 'peu', 'other'))
    U.loc[(U.source == 'other') & U.uid.map(persons.groupby('uid').group.max()).fillna(0).eq(1), 'source'] = 'npeu'
    sp_unit = U[(U.source == 'peu') & (U.head_pn == 2)].hh
    U['peu_dollars'] = np.select([U.source != 'peu', U.head_pn == 2, U.hh.isin(sp_unit)], ['none', 'sp', 'r'], 'all')
    U['group_level'] = U.source == 'npeu'
    U['separate'] = False
    if len(mfs):
        mfs = mfs.copy()
        mfs['source'] = 'peu'
        mfs['peu_dollars'] = np.where(mfs.head_pn == 1, 'r', 'sp')
        mfs['group_level'] = False
        mfs['separate'] = True
        for c in [c for c in U.columns if c.startswith('any_')]:
            mfs[c] = mfs.couple_uid.map(U.set_index('uid')[c]).to_numpy()
        mfs['dep_return'] = False; mfs['has_spouse'] = False; mfs['gq'] = False
        for c in ['spouse_pn', 'spouse_age', 'claimant_uid']:
            mfs[c] = -1
        U = pd.concat([U[~U.uid.isin(chosen)], mfs], ignore_index=True)
        for c in U.columns:
            if U[c].isna().any() and c.startswith(('n_', 'puf_', 'wage_', 'selfemp_', 'spouse_sex')):
                U[c] = U[c].fillna(0)
            elif U[c].isna().any() and c.startswith(('cbo_age', 'dep_age')):
                U[c] = U[c].fillna(-1)
    # each unit's share of the group's dollars: its own members' shares, plus those of the people it claims
    sh = shares.merge(persons[['hh', 'pn', 'uid']], on=['hh', 'pn'], how='left')
    assert sh.uid.notna().all()
    nd = sh.drop(columns=['hh', 'pn']).groupby('uid').sum()
    U['npeu_people'] = U.uid.map(persons[persons.group == 1].groupby('uid').size()).fillna(0).astype(int)
    return persons, U, nd


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('waves', type=int, nargs='*', help="default: the settings' SCF waves")
    ap.add_argument('--out', help='default: {REBUILD_OUT or paths.out}/data, where build_scf.R reads')
    a = ap.parse_args()
    s = json.load(open(SETTINGS))
    out = a.out or os.path.join(os.environ.get('REBUILD_OUT', s['paths']['out']), 'data')
    os.makedirs(out, exist_ok=True)
    tu = s.get('tax_units', {})
    split = tu.get('npeu_split', 'equal')
    for wave in a.waves or s['paths']['raw']['scf_waves']:
        persons, U, nd = units(wave, split, reassign_pass=tu.get('reassign', True))
        persons.to_parquet(f'{out}/scf_persons_{wave}.parquet')
        U.to_parquet(f'{out}/scf_units_module_{wave}.parquet')
        nd.reset_index().to_parquet(f'{out}/scf_npeu_dollars_{wave}.parquet')
        # for build_scf.R: the units (dependents' own returns left out; they are not SCF records) and the group's
        # dollars
        keep = ['uid', 'hh', 'head_pn', 'spouse_pn', 'head_age', 'head_sex', 'spouse_age', 'spouse_sex', 'weight',
                'filing_status', 'separate', 'n_dep', 'n_child_home', 'n_child_away', 'n_other', 'n_parent',
                'puf_n_dep', 'dep_age1', 'dep_age2', 'dep_age3', 'n24', 'n_eitc', 'source', 'peu_dollars',
                'group_level', 'npeu_people', 'any_age_assumed', 'any_link_assumed', 'any_income_assumed',
                'any_dep_by_support', 'any_claimed_in_group']
        U.loc[~U.dep_return, keep].to_csv(f'{out}/scf_units_module_{wave}.csv.gz', index=False)
        nd.reset_index().to_csv(f'{out}/scf_npeu_dollars_{wave}.csv.gz', index=False)
        m = U[~U.dep_return]
        w = m.weight / 1e6
        print(f'wave {wave} (npeu_split {split}): {len(persons)} people, {len(m)} unit rows, {w.sum():.2f} m units')
        print((w.groupby(m.filing_status).sum()).round(2).to_string())
        print((w.groupby(m.source).sum()).round(2).to_string())


if __name__ == '__main__':
    main()
