"""
35_extra.py  (step 35, 35_BUSINESS_INCOME.md): a few supplementary counts.
  PUF filers: losses inside a schedule netted away by profits in another
  component; loss returns by size of loss per schedule; the PUF's F8582
  field (Form 8582, passive activity loss limitations, filed) by rental loss.
  SCF 2016 households: loss reporting by whether the respondent referred to
  income tax returns (X7451, interviewer's record); X7650 / X6766 (income
  unusually low, and why) among loss reporters and among business owners.
Writes step35/extra.md.  Run: sbatch -J extra35 research/rebuild/minimal/35_cpu.sbatch py research/rebuild/minimal/35_extra.py
"""
import numpy as np
import pandas as pd

OUT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/step35'
PUF_RAW = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/puf_2015.csv'
SCFP = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1/2016/historical/SCFP2016.csv'
L = []


def md(s=''):
    print(s, flush=True)
    L.append(s)


raw = pd.read_csv(PUF_RAW, usecols=['RECID', 'S006', 'E00900', 'E02000', 'E02100', 'E25850', 'E25860', 'E25920',
                                    'E25940', 'E25960', 'E25980', 'E26100', 'E26110', 'E26160', 'E26170', 'E26180',
                                    'E26190', 'E26390', 'E26400', 'E27200', 'F8582'])
raw = raw[raw.RECID < 999996]
assert raw.isna().sum().sum() == 0
w = raw.S006.to_numpy() / 100
W = w.sum()
part = (raw.E25940 + raw.E25980 - raw.E25920 - raw.E25960 - raw.E26110).to_numpy()
scorp = (raw.E26170 + raw.E26190 - raw.E26160 - raw.E26180 - raw.E26100).to_numpy()
rents = (raw.E25850 - raw.E25860).to_numpy()
est = (raw.E26390 - raw.E26400).to_numpy()
C, E, F = raw.E00900.to_numpy(), raw.E02000.to_numpy(), raw.E02100.to_numpy()
md('# Step 35 supplementary counts')
md()
md('## PUF filers: component losses netted away')
md()
md('| | % of returns |')
md('|---|---:|')
anyE = (part < 0) | (scorp < 0) | (rents < 0) | (est < 0) | (raw.E27200.to_numpy() < 0)
anyAll = anyE | (C < 0) | (F < 0)
for lab, s in [('net Schedule E below zero', E < 0), ('a loss in at least one Schedule E component', anyE),
               ('net C + F below zero', C + F < 0), ('a loss on C or on F', (C < 0) | (F < 0)),
               ('net C + E + F below zero', C + E + F < 0), ('a loss in at least one component of C, E, F', anyAll)]:
    md(f'| {lab} | {100 * w[s].sum() / W:.2f} |')
md()
md('## PUF filers: returns with a loss, by size of the loss')
md()
md('| item | returns with a loss (m) | loss under $1,000 (%) | under $2,500 (%) | under $5,000 (%) | under $10,000 (%) | $100,000 or more (%) |')
md('|---|---:|---:|---:|---:|---:|---:|')
for lab, x in [('Schedule C', C), ('Schedule F', F), ('C + F', C + F), ('Schedule E', E), ('rents and royalties', rents),
               ('partnerships', part), ('S corporations', scorp), ('C + E + F', C + E + F)]:
    l = x < 0
    wl = w[l]
    md(f'| {lab} | {wl.sum() / 1e6:.2f} | ' + ' | '.join(f'{100 * wl[(-x[l]) < c].sum() / wl.sum():.1f}' for c in [1000, 2500, 5000, 10000])
       + f' | {100 * wl[(-x[l]) >= 100000].sum() / wl.sum():.1f} |')
md()
md('## PUF filers: F8582 field by rents and royalties sign')
md()
md('F8582 values (weighted millions of returns):')
md()
md('| rents and royalties | ' + ' | '.join(f'F8582 = {v}' for v in sorted(raw.F8582.unique())) + ' |')
md('|---|' + '---:|' * raw.F8582.nunique())
for lab, s in [('loss', rents < 0), ('profit', rents > 0), ('none', rents == 0)]:
    md(f'| {lab} | ' + ' | '.join(f'{w[s & (raw.F8582.to_numpy() == v)].sum() / 1e6:.2f}' for v in sorted(raw.F8582.unique())) + ' |')
md()

# ------------------------------------------------------------ SCF
h = pd.read_csv(f'{OUT}/scf2016_hh_items.csv.gz')
h.columns = [c.upper() for c in h.columns]
hw = h.X42001.to_numpy() / 5
X4, X14 = h.X5704.to_numpy(), h.X5714.to_numpy()
loss = ((X4 < 0) & (X4 != -1)) | ((X14 < 0) & (X14 != -1))
pos = (X4 > 0) | (X14 > 0)
md('## SCF households: loss reporting by whether the respondent referred to income tax returns (X7451)')
md()
na = int(h.X7451.isna().sum())
md(f'X7451 missing on {na} rows ({hw[h.X7451.isna().to_numpy()].sum() / 1e6:.2f} million households); shown as a separate row, not dropped.')
md()
md('| X7451 | households (m) | X5704 or X5714 positive, no loss (%) | X5704 or X5714 a loss (%) | among those with a positive or a loss: loss (%) |')
md('|---|---:|---:|---:|---:|')
vals = h.X7451.to_numpy()
for lab, s in [('1 (referred to tax returns)', vals == 1), ('other value', (~np.isnan(vals)) & (vals != 1)), ('missing', np.isnan(vals))]:
    ww = hw[s]
    if ww.sum() == 0:
        continue
    md(f'| {lab} | {ww.sum() / 1e6:.2f} | {100 * ww[(pos & ~loss)[s]].sum() / ww.sum():.1f} | {100 * ww[loss[s]].sum() / ww.sum():.1f} | '
       f'{100 * ww[loss[s]].sum() / ww[(pos | loss)[s]].sum():.1f} |')
md(f'Values of X7451 present: {sorted(pd.unique(vals[~np.isnan(vals)]).tolist())}')
md()
md('## SCF households: income unusually low (X7650 = 2) and the reason (X6766)')
md()
sc = pd.read_csv(SCFP, usecols=['Y1', 'ACTBUS', 'NONACTBUS', 'ORESRE', 'NNRESRE'])
h = h.merge(sc, on='Y1', how='left', validate='1:1')
owner = ((h.ACTBUS > 0) | (h.NONACTBUS > 0) | (h.ORESRE > 0) | (h.NNRESRE != 0)).to_numpy()
md('| group | households (m) | income unusually low (%) | low because of low business or self-employment income, X6766 = 1 (%) | because of depreciation, X6766 = 10 (%) | took less out of business, X6766 = 13 (%) |')
md('|---|---:|---:|---:|---:|---:|')
for lab, s in [('X5704 or X5714 a loss', loss), ('X5704 or X5714 positive, no loss', pos & ~loss),
               ('owns business or investment real estate, X5704 and X5714 neither positive nor loss', owner & ~pos & ~loss),
               ('all households', np.ones(len(h), bool))]:
    ww = hw[s]
    lo = h.X7650.to_numpy()[s] == 2
    r = h.X6766.to_numpy()[s]
    md(f'| {lab} | {ww.sum() / 1e6:.2f} | {100 * ww[lo].sum() / ww.sum():.1f} | {100 * ww[r == 1].sum() / ww.sum():.1f} | '
       f'{100 * ww[r == 10].sum() / ww.sum():.2f} | {100 * ww[r == 13].sum() / ww.sum():.2f} |')
md()
open(f'{OUT}/extra.md', 'w').write('\n'.join(L) + '\n')
