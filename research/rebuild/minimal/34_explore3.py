"""34_explore3.py (step 34, 34_STATUS.md): one-off look at the Schedule E
remainder (sche_other = E02000 minus the fifteen named Schedule E fields):
how much of it is the gap between the ten partnership and S corporation
fields and their combined net E26270, and what is left. Weighted (S006/100).
Writes step34/explore3.txt."""
import numpy as np, pandas as pd
PUF = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/puf_2015.csv'
F = ['E25940', 'E25980', 'E25920', 'E25960', 'E26110', 'E26170', 'E26190', 'E26160', 'E26180', 'E26100']
S = [1, 1, -1, -1, -1, 1, 1, -1, -1, -1]
t = pd.read_csv(PUF, usecols=['RECID', 'S006', 'E02000', 'E26270', 'E25850', 'E25860', 'E26390', 'E26400', 'E27200',
                              'P25700', 'E25820', 'F8582', 'SCHE'] + F)
t = t[t.RECID < 999996]
w = t.S006.to_numpy(float) / 100
ps = sum(s * t[f] for f, s in zip(F, S))
gap_ps = (t.E26270 - ps).to_numpy(float)
rest = (t.E02000 - t.E26270 - (t.E25850 - t.E25860) - (t.E26390 - t.E26400) - t.E27200).to_numpy(float)
rem = gap_ps + rest
out = []
def line(name, x):
    nz = x != 0
    out.append(f'{name}: weighted share nonzero {100*w[nz].sum()/w.sum():.2f}% (records {nz.sum()}), below zero '
               f'{100*w[x<0].sum()/w.sum():.2f}%, weighted total ${(w*x).sum()/1e9:.2f}bn, sum of |x| ${(w*np.abs(x)).sum()/1e9:.2f}bn')
line('remainder sche_other', rem)
line('  of which E26270 minus the ten partnership/S corp fields', gap_ps)
line('  of which E02000 minus E26270, rent net, estate net, farm rental', rest)
out.append(f'records with remainder nonzero where the E26270 gap explains it fully: {int(((rem != 0) & (rest == 0)).sum())}')
f8582 = t.F8582.to_numpy()
out.append(f'F8582 (passive activity loss form) values: {pd.Series(f8582).value_counts().to_dict()}')
for v in np.unique(f8582):
    m = f8582 == v
    out.append(f'  F8582={v}: share with E26270 gap nonzero {100*w[m & (gap_ps!=0)].sum()/w[m].sum():.1f}%, '
               f'gap total ${(w[m]*gap_ps[m]).sum()/1e9:.2f}bn')
pos = gap_ps > 0
out.append(f'E26270 gap above zero (combined net larger than the fields: losses limited?) weighted ${(w*gap_ps*pos).sum()/1e9:.2f}bn; '
           f'below zero ${(w*gap_ps*(gap_ps<0)).sum()/1e9:.2f}bn')
out.append(f'rent: P25700 minus (E25850 - E25860): nonzero records {int((t.P25700 - (t.E25850 - t.E25860) != 0).sum())}')
open('/nfs/roberts/scratch/pi_nrs36/jar335/minimal/step34/explore3.txt', 'w').write('\n'.join(out) + '\n')
print('\n'.join(out))
