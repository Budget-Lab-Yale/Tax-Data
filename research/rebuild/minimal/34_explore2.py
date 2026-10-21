"""34_explore2.py (step 34, 34_STATUS.md): one-off look at the SOI Table 1.4
item names for business income parts (2015, 2019) and at the signs of the
non-filer pool's sole_prop, rent and rent_loss. Prints; writes step34/explore2.txt."""
import pandas as pd
SOI = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-Ind/aligned/bysize_income_sources.csv'
POOL = ('/nfs/roberts/project/pi_nrs36/shared/model_data/ASEC-Nonfilers/v1/2026091201/'
        'historical/nonfiler_pool_2017.csv.gz')
t = pd.read_csv(SOI, dtype=str)
t = t[(t['table'] == '1.4') & (t['panel'] == 'All returns') & (t['row_label'] == 'All returns, total')
      & t['tax_year'].isin(['2015', '2019'])]
t['item'] = t['item'].str.lower().str.strip()
pat = 'business|partnership|corporation|rent|royalt|estate|trust|farm|remic|schedule e|section 179'
s = t[t['item'].str.contains(pat)][['tax_year', 'item', 'value']].sort_values(['item', 'tax_year'])
out = [s.to_string()]
p = pd.read_csv(POOL)
for c in ['sole_prop', 'rent', 'rent_loss', 'other_inc']:
    if c in p:
        x = p[c]
        out.append(f'pool {c}: NA {int(x.isna().sum())} neg {(x<0).sum()} pos {(x>0).sum()} min {x.min()} max {x.max()}')
out.append('pool columns: ' + ', '.join(p.columns))
open('/nfs/roberts/scratch/pi_nrs36/jar335/minimal/step34/explore2.txt', 'w').write('\n'.join(out) + '\n')
print('\n'.join(out))
