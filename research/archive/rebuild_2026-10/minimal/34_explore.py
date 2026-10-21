"""34_explore.py (step 34, 34_STATUS.md): one-off look at the 2015 PUF's
Schedule C, E and F fields: which exist, their signs, and which sums
reproduce E02000. Prints only; writes step34/explore.txt."""
import numpy as np, pandas as pd
PUF = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/puf_2015.csv'
cols = pd.read_csv(PUF, nrows=0).columns.tolist()
want = [c for c in cols if c[:3] in ('E25', 'E26', 'E27', 'P25', 'P26', 'P27')] + ['E00900', 'E02000', 'E02100', 'E00100']
t = pd.read_csv(PUF, usecols=['RECID', 'S006'] + want)
t = t[t.RECID < 999996]
w = t.S006 / 100
out = [f'all columns: {cols}', f'records {len(t)}']
for c in want:
    x = t[c]
    out.append(f'{c}: NA {int(x.isna().sum())} nonzero {(x!=0).mean():.4f} neg {(x<0).mean():.4f} '
               f'pos {(x>0).mean():.4f} wtd total $bn {(w*x).sum()/1e9:.2f} min {x.min():.0f} max {x.max():.0f}')
open('/nfs/roberts/scratch/pi_nrs36/jar335/minimal/step34/explore.txt', 'w').write('\n'.join(out) + '\n')
print('\n'.join(out))
