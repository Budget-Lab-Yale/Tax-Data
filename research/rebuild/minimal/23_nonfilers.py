"""
23_nonfilers.py  (minimal example, step 23: crude non-filers in the PUF side,
then decision 18 on the real SCF; 2026-10-16. Write-up: 23_NONFILERS.md.)

What it does
  Builds table E4N: v4c's table E4 (2015 PUF and the 2016 SCF wave) with the
  non-filers added to the PUF side as rows with a filing flag, as plan
  decision 7 says ("non-filers are rows in pretraining with a filing flag").
  A CRUDE STOPGAP, asked for by John on 16 October to test decision 18 on
  the real SCF before the non-filer work proper. It is to be replaced by a
  systematic non-filer method (plan decision 7), and results that use it
  rerun (23_NONFILERS.md lists what is crude):

  - The rows are the published CPS ASEC non-filer pool for 2017 (interface
    ASEC-Nonfilers, vintage 2026091201, historical), the nearest year built.
    Weights as published (the 2017 non-filer population stands in for 2015's).
  - Dollars put in 2015 dollars with CPI-U (2015 annual 237.017, 2017 annual
    245.120).
  - married = joint filing status (2); n_dep capped at 3; wages; interest =
    taxable interest (txbl_int, negatives set to zero: the PUF's interest is
    never negative); dividends = div_pref (the CPS's one dividends item);
    capital gains = 0 (the CPS does not ask; kg_lt is zero throughout);
    other income = sole proprietorship + taxable pensions + rent + rent
    losses + unemployment insurance + other income. Taxable Social Security
    is taken as zero (non-filers' incomes are mostly below the line where any
    of it is taxable). Nothing else.
  - source 'puf' (they stand for the part of the population the PUF does
    not cover), year 2015, filer = 0. PUF rows get filer = 1. SCF rows get
    no filer slot (-1): the model learns who files from the PUF side.
  - Every amount is placed on E4's slices (the PUF's 2015 cut points), so the
    band edges, and the dollar maps built from them (map/V4/), are E4's
    unchanged.

Writes (scratch data/): stacked_E4N.parquet, band_edges_E4N.json (E4's, with
a note), nonfilers_E4N_note.json (counts, clipped values, totals).
Usage: python research/rebuild/minimal/23_nonfilers.py
"""
import ast
import json

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
D = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data'
POOL = ('/nfs/roberts/project/pi_nrs36/shared/model_data/ASEC-Nonfilers/v1/2026091201/'
        'historical/nonfiler_pool_2017.csv.gz')
CPI_2015, CPI_2017 = 237.017, 245.120
ID_OFFSET = 900_000_000          # pool ids: 9e8 + pool id, disjoint from PUF and SCF ids

# 01_band.py's constants and functions only (as 10_distort.py loads them).
_band_file = f'{HERE}/01_band.py'
_tree = ast.parse(open(_band_file).read())
_stop = next(i for i, n in enumerate(_tree.body) if 'read_csv' in ast.unparse(n))
_keep = [n for n in _tree.body[:_stop] if isinstance(n, (ast.Import, ast.ImportFrom, ast.FunctionDef))
         or (isinstance(n, (ast.Assign, ast.Assert)) and 'sys.argv' not in ast.unparse(n)
             and 'SUF' not in ast.unparse(n))]
B = {}
exec(compile(ast.Module(body=_keep, type_ignores=[]), _band_file, 'exec'), B)
band_variable = B['band_variable']

PUF_AMOUNTS = ['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']


def pool_rows():
    p = pd.read_csv(POOL)
    assert (p['tax_year'] == 2017).all() and (p['filer'] == 0).all()
    deflate = CPI_2015 / CPI_2017
    note = {'pool file': POOL, 'records': len(p), 'weighted units (millions)': float(p['weight'].sum() / 1e6),
            'dollar factor 2017 to 2015 (CPI-U)': deflate,
            'interest below zero, set to zero (records)': int((p['txbl_int'] < 0).sum())}
    other = (p['sole_prop'] + p['txbl_pens_dist'] + p['rent'] + p['rent_loss'] + p['ui'] + p['other_inc'])
    out = pd.DataFrame({
        'source': 'puf', 'year': 2015,
        'record_id': ID_OFFSET + p['id'].astype(np.int64), 'household_id': ID_OFFSET + p['id'].astype(np.int64),
        'implicate': 0, 'split': 'train', 'weight': p['weight'].astype(float),
        'married': (p['filing_status'] == 2).astype(int), 'n_dep': p['n_dep'].clip(upper=3).astype(int),
        'wages': p['wages'] * deflate, 'interest': p['txbl_int'].clip(lower=0) * deflate,
        'dividends': p['div_pref'] * deflate, 'capital_gains': 0.0, 'other_agi': other * deflate,
        'net_worth': np.nan, 'filer': 0})
    assert not out.drop(columns=['net_worth']).isna().any().any()
    return out, note


def main():
    e4 = pd.read_parquet(f'{D}/stacked_E4.parquet')
    edges = json.load(open(f'{D}/band_edges_E4.json'))
    nf, note = pool_rows()
    w = nf['weight'].to_numpy()
    for v in PUF_AMOUNTS:
        band, _ = band_variable(nf[v].to_numpy(dtype=float), w, edges[f'puf/2015/{v}'])
        nf[f'{v}_band'] = band.astype('int16')
    nf['net_worth_band'] = np.int16(-1)
    for c in [c for c in e4.columns if c.endswith('_coarse_hi') or c.endswith('_coarse_neg')]:
        nf[c] = -1
    e4 = e4.copy()
    e4['filer'] = np.where(e4['source'] == 'puf', 1, -1)
    assert set(nf.columns) == set(e4.columns), set(nf.columns) ^ set(e4.columns)
    t = pd.concat([e4, nf[e4.columns]], ignore_index=True)
    for c in e4.columns:
        if str(e4[c].dtype) != 'object':
            t[c] = t[c].astype(e4[c].dtype)
    assert not t.duplicated(['source', 'record_id']).any()
    # checks: the PUF and SCF rows are E4's; pool rows have no negative interest or dividends
    assert t.iloc[:len(e4)].drop(columns=['filer']).equals(e4.drop(columns=['filer']))
    nfr = t['record_id'] >= ID_OFFSET
    for v in PUF_AMOUNTS:
        b = t.loc[nfr, f'{v}_band']
        assert b.between(0, 83).all() and ((b == 0) == (t.loc[nfr, v] == 0)).all(), v
    puf = (t['source'] == 'puf') & ~nfr
    for v in PUF_AMOUNTS + ['married']:
        note[f'share with any {v}: PUF filers / non-filers (%)'] = [
            round(100 * float(np.average(t.loc[m, v] != 0, weights=t.loc[m, 'weight'])), 1) for m in [puf, nfr]]
    note['PUF filers (weighted millions)'] = float(t.loc[puf, 'weight'].sum() / 1e6)
    note['wages 0: share of PUF side (%), filers only / with non-filers'] = [
        round(100 * float(np.average(t.loc[m, 'wages'] == 0, weights=t.loc[m, 'weight'])), 1)
        for m in [puf, t['source'] == 'puf']]
    scf = t['source'] == 'scf'
    note['wages 0: share of SCF units (%)'] = round(100 * float(np.average(t.loc[scf, 'wages'] == 0,
                                                                           weights=t.loc[scf, 'weight'])), 1)
    note['interest > 0: PUF side with non-filers / SCF (%)'] = [
        round(100 * float(np.average(t.loc[m, 'interest'] > 0, weights=t.loc[m, 'weight'])), 1)
        for m in [t['source'] == 'puf', scf]]
    t.to_parquet(f'{D}/stacked_E4N.parquet', index=False)
    edges['meta'] = dict(edges.get('meta', {}), E4N='E4 with the 2017 CPS non-filer pool added to the PUF side '
                         '(23_nonfilers.py); the cut points are E4\'s unchanged')
    json.dump(edges, open(f'{D}/band_edges_E4N.json', 'w'), indent=1)
    json.dump(note, open(f'{D}/nonfilers_E4N_note.json', 'w'), indent=1)
    print(json.dumps(note, indent=1))


if __name__ == '__main__':
    main()
