"""check_units.py: the standing tests every survey's tax units must pass, every year, before training (TAX_UNITS.md,
section 5, with the SCF report's additions of 24 October 2026). Run on the shared module's outputs (tax_units.py
through roster_cps.py, roster_scf.py or roster_cex.py); the benchmark is the CPS under the same rules, for the nearest
tax year built, and for 2019 also the tax data (SOI Table 1.6 less dependent filers, plus the CPS's expected
non-filers; explore/2026-10-24/spending_age, section 2).

Tests (millions, weighted):
  1. integrity: every person in exactly one unit; every unit has one head and at most one spouse; every spouse and
     dependent points at a unit that exists; no dependent heads a unit; every dependent's own return points at a
     claiming unit (exact; a failure stops the run).
  2. households, and people by age, against the CPS.
  3. units by the primary filer's age range, joint and not joint (separate returns counted as two units, not joint).
  4. units per household.
  5. units by number of dependents (0, 1, 2, 3 or more; dependents living elsewhere included).
  6. dependents living in the household by age, in the SCF's brackets (the coarsest common ones).
  7. adults 18 and over by role: primary filer, spouse, dependent.
  8. optional: the statistic a later stage takes from the survey, before and after (the SCF's net worth
     percentiles, the CEX's spending per unit by age range), from the survey's built files.
  The CEX also gets the share of units whose members are exactly one of BLS's own units (roster_cex.py keeps BLS's
  codes for this comparison only).

Usage (through Slurm): python check_units.py <survey> <year> --persons P --units U [--mfs M] --cps-persons CP
  --cps-units CU --out DIR [--stat networth|spending --stat-old OLD --stat-new NEW]
Writes DIR/checks_<survey>_<year>.md and DIR/checks_<survey>_<year>.csv (every number in the tables, long form).
"""
import argparse
import os

import numpy as np
import pandas as pd

EDGES = [26, 35, 45, 55, 65]
RANGES = ['under 26', '26-34', '35-44', '45-54', '55-64', '65+']
DEP_BANDS = ([-1, 3, 6, 12, 17, 27, 62, 200], ['0-3', '4-6', '7-12', '13-17', '18-27', '28-62', '63+'])
PEOPLE_BANDS = ([-1, 17, 25, 44, 64, 200], ['0-17', '18-25', '26-44', '45-64', '65+'])
# the tax data, 2019: SOI Table 1.6 by the primary filer's age, less the file's dependent filers, plus the CPS's
# expected non-filers; joint returns plus expected non-filing couples (explore/2026-10-24/spending_age, section 2)
TAX_DATA_2019 = pd.Series([16.42, 31.65, 29.80, 27.89, 28.17, 34.84, 60.27],
                          index=[f'primary {r}' for r in RANGES] + ['joint'])


def rng(age):
    return np.searchsorted(EDGES, np.asarray(age, float), side='right')


def wq(x, w, q):
    o = np.argsort(x); x = np.asarray(x)[o]; c = np.cumsum(np.asarray(w)[o])
    return x[np.searchsorted(c, q * c[-1])]


def md(df, fmt='{:,.2f}'):
    d = df.reset_index()
    f = lambda v: fmt.format(v) if isinstance(v, (float, np.floating)) and np.isfinite(v) else ('' if isinstance(v, float) else str(v))
    lines = ['| ' + ' | '.join(str(c) for c in d.columns) + ' |', '|' + '---|' * len(d.columns)]
    return '\n'.join(lines + ['| ' + ' | '.join(f(v) for v in row) + ' |' for row in d.itertuples(index=False)])


def load(persons, units, mfs=None):
    """Persons, the units before any separate split (for the integrity test) and the units after it (for counts)."""
    P = pd.read_parquet(persons)
    Uall = pd.read_parquet(units)
    if 'separate' in Uall and Uall.separate.any():          # the SCF: separate returns already in the units file
        after = Uall[~Uall.dep_return]
        sep = after[after.separate]
        before_ids = set(after.uid[~after.separate]) | set(sep.couple_uid)
    else:
        after = Uall[~Uall.dep_return]
        before_ids = set(after.uid)
        if mfs:
            M = pd.read_parquet(mfs)
            after = pd.concat([after[~after.uid.isin(M.couple_uid)], M.assign(dep_return=False)], ignore_index=True)
    return P, Uall, before_ids, after


def integrity(P, Uall, before_ids):
    r = {}
    r['people listed twice'] = int(P[['hh', 'pn']].duplicated().sum())
    r['people in no unit'] = int((~P.uid.isin(before_ids)).sum())
    heads = P[P.tu_rel == 1].groupby('uid').size()
    r['units without exactly one head'] = int(((heads != 1).sum()) + len(before_ids - set(heads.index)))
    r['units with two spouses'] = int((P[P.tu_rel == 2].groupby('uid').size() > 1).sum())
    r['dependents who also head a unit'] = int(P[P.tu_rel.isin([3, 4])].set_index(['hh', 'pn']).index
                                              .isin(P[P.tu_rel == 1].set_index(['hh', 'pn']).index).sum())
    dr = Uall[Uall.dep_return]
    r["dependents' own returns without a claiming unit"] = int((~dr.claimant_uid.isin(before_ids)).sum())
    if 'sp_pn' in P:
        sp = P[P.tu_rel == 2]
        hd = P[P.tu_rel == 1].set_index('uid').pn
        r['spouses whose pointer is not the head'] = int((sp.sp_pn.to_numpy() != sp.uid.map(hd).to_numpy()).sum())
    return pd.Series(r)


def unit_counts(after):
    w = after.weight
    nd = after.n_dep.clip(upper=3)
    r = {'all units': w.sum()}
    for j, n in enumerate(RANGES):
        r[f'primary {n}'] = w[rng(after.head_age) == j].sum()
    r['joint'] = w[after.filing_status == 'joint'].sum()
    r['not joint'] = w[after.filing_status != 'joint'].sum()
    r['  of which separate returns'] = w[after.filing_status == 'separate'].sum()
    r['  of which head of household'] = w[after.filing_status == 'hoh'].sum()
    for k, n in enumerate(['no dependents', '1 dependent', '2 dependents', '3 or more']):
        r[n] = w[nd == k].sum()
    r['dependents (at most 3 per unit)'] = (w * nd).sum()
    r['dependents (all)'] = (w * after.n_dep).sum()
    return pd.Series(r) / 1e6


def people(P, after):
    w = P.weight
    hh = P.drop_duplicates('hh').weight.sum()
    r = {'households': hh / 1e6, 'people': w.sum() / 1e6}
    for lab, s in P.groupby(pd.cut(P.age, PEOPLE_BANDS[0], labels=PEOPLE_BANDS[1]), observed=False).weight.sum().items():
        r[f'people {lab}'] = s / 1e6
    r['units per household'] = after.weight.sum() / hh
    a = P[P.age >= 18]
    for n, m in [('adults: primary filer', a.tu_rel == 1), ('adults: spouse', a.tu_rel == 2),
                 ('adults: dependent', a.tu_rel.isin([3, 4]))]:
        r[n] = a.weight[m].sum() / 1e6
    d = P[P.tu_rel.isin([3, 4])]
    for lab, s in d.groupby(pd.cut(d.age, DEP_BANDS[0], labels=DEP_BANDS[1]), observed=False).weight.sum().items():
        r[f'dependents {lab}'] = s / 1e6
    return pd.Series(r)


def stat(kind, old, new):
    rows = []
    for name, path in [('before (the survey as built today)', old), ('after (the shared rules)', new)]:
        x = pd.read_csv(path)
        if kind == 'networth':
            row = {'units (m)': x.weight.sum() / 1e6}
            for q, n in [(.25, '25th'), (.5, 'median'), (.75, '75th'), (.9, '90th'), (.99, '99th')]:
                row[n] = wq(x.net_worth.to_numpy(), x.weight.to_numpy(), q)
            row['total ($ tn)'] = (x.weight * x.net_worth).sum() / 1e12
        else:
            a = rng(x.age1)
            row = {'units (m)': x.weight.sum() / 1e6}
            for j, n in enumerate(RANGES):
                m = a == j
                row[n] = (x.weight[m] * x.total_consumption[m]).sum() / x.weight[m].sum()
            row['total ($ bn)'] = (x.weight * x.total_consumption).sum() / 1e9
        rows.append(pd.Series(row, name=name))
    return pd.DataFrame(rows)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('survey'); ap.add_argument('year', type=int)
    ap.add_argument('--persons', required=True); ap.add_argument('--units', required=True); ap.add_argument('--mfs')
    ap.add_argument('--cps-persons', required=True); ap.add_argument('--cps-units', required=True)
    ap.add_argument('--cps-year', type=int); ap.add_argument('--out', required=True)
    ap.add_argument('--stat'); ap.add_argument('--stat-old'); ap.add_argument('--stat-new')
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    P, Uall, ids, after = load(a.persons, a.units, a.mfs)
    cP, cUall, cids, cafter = load(a.cps_persons, a.cps_units)
    lines = [f'# Tax-unit checks: {a.survey.upper()} {a.year}', '',
             f'Benchmark: the CPS under the same rules, tax year {a.cps_year or "?"} (no separate split in the CPS '
             f'column; the CPS splits separate returns after its filing step).', '']
    integ = integrity(P, Uall, ids)
    lines += ['## 1. Integrity (counts of records; all must be 0)', '', md(integ.to_frame('records'), '{:,.0f}'), '']
    assert (integ == 0).all(), integ[integ != 0]
    t = pd.DataFrame({a.survey.upper(): unit_counts(after), 'CPS': unit_counts(cafter)})
    if a.year in (2018, 2019):
        t['tax data, 2019'] = TAX_DATA_2019.reindex(t.index)
    t['difference from the CPS'] = t.iloc[:, 0] - t.CPS
    lines += ['## 3 and 5. Units by the primary filer\'s age range, joint or not, and number of dependents (millions)',
              '', md(t), '']
    p = pd.DataFrame({a.survey.upper(): people(P, after), 'CPS': people(cP, cafter)})
    p['difference'] = p.iloc[:, 0] - p.CPS
    lines += ['## 2, 4, 6, 7. Households, people by age, units per household, adults by role, dependents by age '
              '(millions; units per household a ratio)', '', md(p, '{:,.3f}'), '']
    if 'bls_same_members' in after:          # the CEX: BLS's own units, kept only as a comparison
        m = after[after.bls_same_members.notna()]
        lines += [f'BLS comparison: {(m.weight * m.bls_same_members.astype(float)).sum() / m.weight.sum():.3f} of units '
                  f'(by weight) hold exactly the people of one of BLS\'s units (after build_cex.R\'s former repairs).', '']
    if a.stat:
        s = stat(a.stat, a.stat_old, a.stat_new)
        lines += [f'## 8. The statistic later stages take ({a.stat}; nominal dollars of the survey year)', '',
                  md(s, '{:,.0f}'), '']
    long = pd.concat([t.stack().rename('value').reset_index().assign(table='units'),
                      p.stack().rename('value').reset_index().assign(table='people'),
                      integ.rename('value').reset_index().assign(level_1='records', table='integrity')])
    long.to_csv(f'{a.out}/checks_{a.survey}_{a.year}.csv', index=False)
    open(f'{a.out}/checks_{a.survey}_{a.year}.md', 'w').write('\n'.join(lines))
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
