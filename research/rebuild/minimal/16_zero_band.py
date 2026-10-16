"""
16_zero_band.py  (minimal example, step 16: the SCF zero band, by wage group)

From data only, no model. Under the two assumptions of the reporting curve
(E9's report_curve.json: whether an SCF unit reports an item depends only on
its true amount, and the SCF and PUF units are the same population per head),
splits the SCF's reported zeros for interest, dividends and capital gains
into true non-holders and unreported holders, within each wage group, and
gives each part's mean net worth. Then asks what PUF-style records with a
zero would get if they were given the mean net worth of the SCF's true
non-holders instead of that of all SCF zero-reporters (what the source label
and the mapping, E4, give them), wage group by wage group.

Mean net worth only: a mixture's mean can be split into its parts' means; a
median cannot. Writes R/diag16/zero_band.json and prints it.
Run: sbatch research/rebuild/minimal/16_zero_band.sbatch
"""
import json
import numpy as np
import pandas as pd

R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
INV = ['interest', 'dividends', 'capital_gains']
GROUPS = ['0', '1-25k', '25-50k', '50-100k', '100-200k', '200k+']


def wage_group(w):
    return pd.cut(w, [-np.inf, 0, 25e3, 50e3, 100e3, 200e3, np.inf], labels=GROUPS).astype(str)


def split_zero(scf, v, g):
    w, nw, b = scf['weight'].to_numpy(float), scf['net_worth'].to_numpy(float), scf[v + '_band'].to_numpy()
    n = len(g)
    cnt = np.bincount(b, weights=w, minlength=n)
    ps = cnt / w.sum()
    mean_s = np.divide(np.bincount(b, weights=w * nw, minlength=n), cnt, out=np.zeros(n), where=cnt > 0)
    pos = (np.arange(n) > 0) & (cnt > 0)
    unrep = np.zeros(n)
    unrep[pos] = ps[pos] / g[pos] * (1 - g[pos])
    true0 = ps[0] - unrep.sum()
    return {'reported zero share': float(ps[0]), 'implied true zero share': float(true0),
            'mean NW, reported zeros': float(mean_s[0]),
            'mean NW, unreported holders': float((unrep * mean_s).sum() / unrep.sum()) if unrep.sum() > 0 else None,
            'mean NW, true non-holders': float((ps[0] * mean_s[0] - (unrep * mean_s).sum()) / true0) if true0 > 0 else None}


def main():
    t = pd.read_parquet(f'{R}/data/stacked_E4.parquet')
    curve = json.load(open(f'{R}/model/E9/report_curve.json'))
    scf, puf = t[t.source == 'scf'].copy(), t[t.source == 'puf'].copy()
    scf['wg'], puf['wg'] = wage_group(scf['wages']), wage_group(puf['wages'])
    out = {}
    for v in INV:
        g = np.asarray(curve[v]['g'])
        rows = {'all': split_zero(scf, v, g)}
        shift = 0.0
        pw = puf['weight'].to_numpy(float)
        for grp in GROUPS:
            s = split_zero(scf[scf.wg == grp], v, g)
            k = (puf.wg == grp).to_numpy()
            pz = float(pw[k & (puf[v + '_band'].to_numpy() == 0)].sum() / pw.sum())
            s['PUF share of all returns: this group with a zero'] = pz
            if s['mean NW, true non-holders'] is not None:
                s['change in PUF-style mean NW from this cell'] = pz * (s['mean NW, true non-holders'] - s['mean NW, reported zeros'])
                shift += s['change in PUF-style mean NW from this cell']
            rows[grp] = s
        rows['total change in PUF-style mean NW (sum of cells)'] = shift
        out[v] = rows
    print(json.dumps(out, indent=1))
    json.dump(out, open(f'{R}/diag16/zero_band.json', 'w'), indent=1)


if __name__ == '__main__':
    main()
