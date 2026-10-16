"""
05_check_capital_gains.py  (minimal example, step 5: which SOI row the
capital gains target should use)

What it does. Reads the raw 2015 PUF and compares, by AGI bracket, its
capital gains fields with SOI 2015 Table 1.4:
  E01000  "Net capital gain or loss" (the PUF booklet's label)
          against SOI's Schedule D net gain minus Schedule D net loss, and
          against SOI's 1040 line figure (that, plus distributions reported
          on the 1040 without Schedule D)
  E01100  "Capital gain distributions reported on Form 1040"
          against SOI's distributions without Schedule D
It does this with and without the four aggregate records (each placed in
the bracket of its own AGI), to see whether they explain any gap below
$200,000. Writes nothing; prints the table (the step 5 report quotes it).
"""
import sys
import numpy as np
import pandas as pd
sys.path.insert(0, '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/minimal')
import soi_cells

PUF = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/puf_2015.csv'
SOI = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/targets/soi_2015_validation.csv'

puf = pd.read_csv(PUF, usecols=['RECID', 'S006', 'E00100', 'E01000', 'E01100'])
assert not puf.isna().any().any()
puf['weight'] = puf['S006'] / 100
aggregate = puf['RECID'] >= 999996
print('The four aggregate records:\n', puf[aggregate].to_string())

soi = soi_cells.read_soi(SOI, 2015)
brackets = soi_cells.published_brackets(soi)


def soi_value(lo, variable, statistic='amount'):
    row = soi[(soi['agi_bracket_lo'] == lo) & (soi['variable'] == variable) & (soi['statistic'] == statistic)]
    assert len(row) == 1
    return float(row['value'].iloc[0])


rows = []
for lo, hi, label in brackets:
    inside = (puf['E00100'] >= lo) & (puf['E00100'] < hi)
    real = inside & ~aggregate
    w = puf['weight']
    sch_d = soi_value(lo, 'sch_d_net_gain') - soi_value(lo, 'sch_d_net_loss')
    dist = soi_value(lo, 'capital_gain_distributions_no_sch_d')
    e01000 = (w * puf['E01000'])[real].sum()
    e01000_agg = (w * puf['E01000'])[inside].sum()
    e01100 = (w * puf['E01100'])[real].sum()
    rows.append({
        'bracket': label,
        'soi_sch_d_net_bn': sch_d / 1e9, 'soi_line13_bn': (sch_d + dist) / 1e9,
        'puf_E01000_bn': e01000 / 1e9,
        'gap_vs_sch_d_pct': 100 * (e01000 / sch_d - 1),
        'gap_vs_line13_pct': 100 * (e01000 / (sch_d + dist) - 1),
        'gap_vs_sch_d_with_aggregates_pct': 100 * (e01000_agg / sch_d - 1),
        'soi_distributions_bn': dist / 1e9, 'puf_E01100_bn': e01100 / 1e9,
        'puf_E01100_count': w[real & (puf['E01100'] != 0)].sum(),
        'soi_distributions_count': soi_value(lo, 'capital_gain_distributions_no_sch_d', 'count')})
out = pd.DataFrame(rows)
pd.set_option('display.width', 250)
print(out.round(2).to_string())
print('totals ($bn): SOI Schedule D net', out['soi_sch_d_net_bn'].sum(),
      'SOI line 13', out['soi_line13_bn'].sum(), 'PUF E01000', out['puf_E01000_bn'].sum(),
      'SOI distributions', out['soi_distributions_bn'].sum(), 'PUF E01100', out['puf_E01100_bn'].sum())
