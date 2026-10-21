"""
36_business_targets.py  (minimal example, step 36: business income targets for the tilt)

John, 19 October: tilt a step-34 run to 2019 with targets for the business
income parts as well, from published IRS numbers where they exist, and learn
how many targets to use and how to build them where only some aggregates are
published.

What SOI publishes (Pub 1304 Table 1.4, by size of AGI, 2015 and 2019): for
six families, the number of returns and the amount with a net income, and
the same with a net loss. The PUF splits two of them further (partnerships
from S corporations, passive from nonpassive, the section 179 deduction);
SOI does not.

  family                   SOI rows                          parts on table E34
  Schedule C               business or profession            sch_c
  partnership + S corp     partnership and s corporation     part_* and scorp_* (ten)
  rent and royalty         total rental and royalty          rent_inc, rent_loss
  estate and trust         estate and trust                  estate_inc, estate_loss
  farm                     farm                              farm
  farm rental              farm rental                       farm_rent

What a return counts as. For a family with one signed part (Schedule C, farm,
farm rental) a return has net income if the part is positive, a net loss if
negative. For the three families made of several fields, two readings are
possible and the 2015 PUF against SOI 2015 decides between them (section B
of the output):
  "net"       the signed sum of the family's fields; income if above zero,
              loss if below (one net figure per return)
  "separate"  the income fields and the loss fields counted separately; a
              return with both counts in both
The reading chosen for each family is the one closer to SOI 2015 in counts
(written in the output and in FAMILY_RULE below, after the check).

Writes, in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/targets/:
  soi_business_2015_2019.csv   the published rows, both years, every AGI
                               bracket and the total, layout as soi_2019_targets.csv
and $R/step36/business_targets.md: the rows' totals, and the 2015 PUF
(filers, PUF weights) against SOI 2015 under each reading, in total and by
AGI bracket.
Usage (CPU job):
  sbatch -J bt36 --partition=day --time=0:30:00 --mem=32G --wrap \
    "module load ...; source research/rebuild/minimal/34_env.sh; python research/rebuild/minimal/36_business_targets.py"
"""
import importlib.util
import os

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT_CSV = f'{R}/targets/soi_business_2015_2019.csv'
OUT_MD = f'{R}/step36/business_targets.md'


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    mod = importlib.util.module_from_spec(s)
    s.loader.exec_module(mod)
    return mod


T01 = load('t01', '01_targets.py')

SOI_NAME = {'sch_c': 'business or profession', 'pship_scorp': 'partnership and s corporation',
            'rent_royalty': 'total rental and royalty', 'estate_trust': 'estate and trust',
            'farm': 'farm', 'farm_rental': 'farm rental'}
ITEMS = {}
for fam, name in SOI_NAME.items():
    for side in ('net income', 'net loss'):
        v = f'{fam}_{side.replace(" ", "_")}'
        ITEMS[f'{name} > {side} > number of returns'] = (v, 'count')
        ITEMS[f'{name} > {side} > amount'] = (v, 'amount')

PARTNERSHIP = ['part_pass_inc', 'part_nonpass_inc', 'part_pass_loss', 'part_nonpass_loss', 'part_179']
SCORP = ['scorp_pass_inc', 'scorp_nonpass_inc', 'scorp_pass_loss', 'scorp_nonpass_loss', 'scorp_179']
# fields of each family: (income fields, loss fields stored positive)
FIELDS = {'sch_c': (['sch_c'], []),
          'pship_scorp': (['part_pass_inc', 'part_nonpass_inc', 'scorp_pass_inc', 'scorp_nonpass_inc'],
                          ['part_pass_loss', 'part_nonpass_loss', 'part_179', 'scorp_pass_loss',
                           'scorp_nonpass_loss', 'scorp_179']),
          'rent_royalty': (['rent_inc'], ['rent_loss']),
          'estate_trust': (['estate_inc'], ['estate_loss']),
          'farm': (['farm'], []),
          'farm_rental': (['farm_rent'], [])}
SIGNED = {'sch_c', 'farm', 'farm_rental'}      # one signed field: the two readings agree


def family_net(df, fam):
    inc, loss = FIELDS[fam]
    return df[inc].to_numpy(float).sum(1) - (df[loss].to_numpy(float).sum(1) if loss else 0.0)


def family_sides(df, fam, rule):
    """(income amount, loss amount stored positive) per record under a reading."""
    if fam in SIGNED or rule == 'net':
        n = family_net(df, fam)
        return np.where(n > 0, n, 0.0), np.where(n < 0, -n, 0.0)
    inc, loss = FIELDS[fam]
    return df[inc].to_numpy(float).sum(1), df[loss].to_numpy(float).sum(1)


def build_rows():
    """The published rows as they are: a cell SOI combined with a neighbour
    for disclosure stays blank, with flag 'combined' (see published_cells)."""
    rows = []
    for y in (2015, 2019):
        rows += T01.rows_from('1.4', y, ITEMS)
    df = pd.DataFrame(rows)
    df['value'] = pd.to_numeric(df['value'])
    df['agi_bracket_lo'] = df['agi_bracket_lo'].astype(float)
    df['agi_bracket_hi'] = df['agi_bracket_hi'].astype(float)
    blank = df['value'].isna()
    assert (df.loc[blank, 'flag'] == 'combined').all(), df[blank & (df['flag'] != 'combined')]
    n = df.groupby(['year', 'variable', 'statistic']).size()
    assert (n == n.iloc[0]).all(), n          # every item has every bracket in both years
    df.to_csv(OUT_CSV, index=False)
    return df


def published_cells(soi, year, variable, stat):
    """The item's cells as published, lowest bracket first, as (lo, hi, value).
    SOI's combined cells: a non-blank cell flagged 'combined' holds its own
    bracket and the blank 'combined' brackets that follow it (checked on every
    item: each run of combined brackets starts with a non-blank one), so it
    becomes one cell spanning them."""
    s = soi[(soi['year'] == year) & (soi['variable'] == variable) & (soi['statistic'] == stat)
            & (soi['agi_bracket_label'] != 'all')].sort_values('agi_bracket_lo')
    out = []
    for r in s.itertuples():
        if pd.isna(r.value):
            assert out and out[-1][3], f'{year} {variable} {stat}: a blank combined cell with no cell to join'
            lo, _, v, _ = out[-1]
            out[-1] = (lo, r.agi_bracket_hi, v, True)
        else:
            out.append((r.agi_bracket_lo, r.agi_bracket_hi, float(r.value), r.flag == 'combined'))
    return [(lo, hi, v) for lo, hi, v, _ in out]


def merge_groups(groups, cells):
    """AGI groups (lo, hi, label), merged where a published cell crosses a group edge."""
    groups = list(groups)
    changed = True
    while changed:
        changed = False
        for i in range(len(groups) - 1):
            edge = groups[i][1]
            if any(lo < edge < hi for lo, hi, _ in cells):
                lo, _, lab = groups[i]
                _, hi, lab2 = groups[i + 1]
                groups[i:i + 2] = [(lo, hi, f'{lab} to {lab2}')]
                changed = True
                break
    return groups


def family_groups(soi, year, fam, side, groups):
    """The groups for one family and side, with the published count and
    amount in each: [(lo, hi, label, count, amount)]."""
    v = f'{fam}_{side}'
    cc, ca = published_cells(soi, year, v, 'count'), published_cells(soi, year, v, 'amount')
    g = merge_groups(groups, cc + ca)
    out = []
    for lo, hi, lab in g:
        inside = lambda c: [x for a, b, x in c if a >= lo and b <= hi]
        out.append((lo, hi, lab, sum(inside(cc)), sum(inside(ca))))
    assert abs(sum(o[3] for o in out) - sum(x for _, _, x in cc)) < 1, v   # every cell in some group
    return out


def main():
    os.makedirs(os.path.dirname(OUT_MD), exist_ok=True)
    soi = build_rows()
    rep = load('rep34', '34_report.py')
    real, w = rep.real_puf_side()
    filer = real['filer'].to_numpy() == 1
    real, w = real[filer].reset_index(drop=True), w[filer]
    agi = real['agi'].to_numpy(float)

    tot = soi[soi['agi_bracket_label'] == 'all']
    lines = ['# Business income targets: SOI rows and the 2015 PUF check', '',
             'Written by `36_business_targets.py`. SOI Pub 1304 Table 1.4, all returns. Amounts in dollars '
             '(losses as SOI publishes them, positive). PUF: table E34, filers, PUF weights, 2015 dollars.', '',
             '## A. SOI totals, 2015 and 2019', '',
             tot.pivot_table(index=['variable', 'statistic'], columns='year', values='value').reset_index()
                .to_markdown(index=False, floatfmt=',.0f'), '']

    # B. the 2015 PUF against SOI 2015 under each reading
    def soi_val(y, fam, side, stat):
        s = soi[(soi['year'] == y) & (soi['variable'] == f'{fam}_{side}') & (soi['statistic'] == stat)]
        return float(s.loc[s['agi_bracket_label'] == 'all', 'value'].iloc[0])

    rows, choice = [], {}
    for fam in FIELDS:
        best = None
        for rule in (['net'] if fam in SIGNED else ['net', 'separate']):
            a_inc, a_loss = family_sides(real, fam, rule)
            err = 0.0
            for side, a in [('net_income', a_inc), ('net_loss', a_loss)]:
                c_puf, m_puf = w[a > 0].sum(), (w * a).sum()
                c_soi, m_soi = soi_val(2015, fam, side, 'count'), soi_val(2015, fam, side, 'amount')
                rows.append({'family': fam, 'reading': rule, 'side': side,
                             'SOI 2015 returns': c_soi, 'PUF 2015 returns': c_puf,
                             'returns, PUF/SOI': c_puf / c_soi,
                             'SOI 2015 amount ($bn)': m_soi / 1e9, 'PUF 2015 amount ($bn)': m_puf / 1e9,
                             'amount, PUF/SOI': m_puf / m_soi})
                err += abs(np.log(c_puf / c_soi))
            if best is None or err < best[1]:
                best = (rule, err)
        choice[fam] = best[0]
    lines += ['## B. The 2015 PUF against SOI 2015, by reading', '',
              'Reading chosen for each family (closer in returns counted, income and loss together): '
              + ', '.join(f'{k}: {v}' for k, v in choice.items()) + '.', '',
              pd.DataFrame(rows).to_markdown(index=False, floatfmt=',.3f'), '']

    # C. by AGI bracket, under the chosen reading (brackets merged where SOI combined cells)
    brackets = sorted({(r.agi_bracket_lo, r.agi_bracket_hi, r.agi_bracket_label)
                       for r in soi[(soi['year'] == 2015) & (soi['agi_bracket_label'] != 'all')].itertuples()})
    rows = []
    for fam, rule in choice.items():
        sides = dict(zip(['net_income', 'net_loss'], family_sides(real, fam, rule)))
        for side, a in sides.items():
            for lo, hi, lab, c_soi, m_soi in family_groups(soi, 2015, fam, side, brackets):
                inside = (agi >= lo) & (agi < hi)
                sel = inside & (a > 0)
                rows.append({'family': fam, 'side': side, 'AGI': lab, 'PUF records': int(sel.sum()),
                             'returns, PUF/SOI': w[sel].sum() / c_soi if c_soi else np.nan,
                             'amount, PUF/SOI': (w * a)[inside].sum() / m_soi if m_soi else np.nan})
    lines += ['## C. By AGI bracket, chosen reading (PUF over SOI, 2015)', '',
              pd.DataFrame(rows).to_markdown(index=False, floatfmt=',.3f'), '']
    open(OUT_MD, 'w').write('\n'.join(lines) + '\n')
    pd.Series(choice).to_json(f'{R}/step36/family_rule.json')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
