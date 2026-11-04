"""The GPU tax calculator's results on the year file (calculator.py) against SOI, and the AGI check.

1. The AGI check. The calculator computes AGI itself from the record's items; the rebuild has its own AGI (the sum
   of its AGI parts, dollars.agi). The gap per record is split into the steps where the calculator computes
   something the rebuild draws or takes as given:
     capital gains        the calculator's net gain on the return (short- plus long-term, a net loss held to
                          $3,000) against the rebuild's line 13 (capital_gains_line13: since 29-30 October the
                          rebuild applies the same limit to kg_st + kg_lt, then adds kg_dist);
     taxable Social Security  computed from gross benefits, against the rebuild's ss_taxable (drawn until 3 November;
                          since then the same section 86 worksheet on the file's own items, so the gap is only where
                          the calculator's modified AGI differs from the file's: its own self-employment tax
                          deduction, the state tax workaround, the law's caps on adjustments);
     self-employment tax deduction  half the computed self-employment tax, against the drawn se_tax_ded;
     state tax workaround the calculator's frame step moves part of some owners' state income tax into the
                          partnership's or S corporation's loss (75% of eligible owners, a random draw);
     pass-through losses  the pass-through amount in the calculator's Schedule E against the rebuild's net
                          partnership and S corporation income (less the workaround): before the fix of 29 October
                          the calculator counted it only when the two net to a gain;
     other adjustments    the law's caps on the adjustments (tuition and fees, domestic production activities)
                          and any other gap in the deductions above the line;
     rest                 whatever is left (should be rounding only).
2. Levels by AGI group, filers only, against SOI 2019 Table 1.4 (returns, AGI, taxable income, itemized
   deductions, the QBI deduction, AMT, income tax before credits) and Table 3.3 (the child tax credit and the
   additional child tax credit, the EITC, income tax after credits, total income tax), read from the local SOI
   store (settings paths.raw.soi_tables). AGI groups are SOI's (Table 1.4's rows), by the calculator's AGI; a cell
   SOI combined with its neighbours is compared as one cell. Weights before the tilt (weight_start) and after
   (weight). Nominal dollars of the year. Nothing here is a target.

Reads <out>/calculator_<year>.parquet, the converter's input and the year file (calculator.py's paths). Writes
<out>/agi_check.csv, levels.csv, levels_by_rebuild_agi.csv, report.md.
"""
import os

import numpy as np
import pandas as pd

import calculator as C
import common

Y = C.Y
SOI = common.RAW['soi_tables']
FILES = {'1.4': 'bysize_income_sources.csv', '3.3': 'bysize_tax_liability.csv'}
# (table, SOI item without " > number of returns" / " > amount", the file's value per record, what it depends on
# that is 0 for now). Counts are returns with a value above zero (itemizers: returns that itemize).
AGI_ITEM = 'adjusted gross income less deficit'
SKIP = {('returns', 'amount'), (AGI_ITEM, 'count')}   # SOI publishes neither
_READ = {}   # each SOI file's rows of the year, read once
ZERO_QBI = 'QBI deduction above the income threshold (W-2 wages paid and specified-service flags are 0)'
ZERO_STD = 'blindness (0: no extra standard deduction for the blind)'
DOLLARS_2015 = ('itemized deduction items, AMT items and credits taken from the PUF are grown from 2015 by their growth.yaml '
                'lines (until 29 October they stayed in 2015 dollars)')
ITEMS = [
    ('1.4', 'adjusted gross income less deficit', 'agi', ''),
    ('1.4', 'total itemized deductions', 'item_ded', DOLLARS_2015 + '; mortgage balances 0 (no $750,000 limit)'),
    ('1.4', 'qualified business income deduction', 'qbi_ded', ZERO_QBI),
    ('1.4', 'taxable income', 'txbl_inc', '; '.join([ZERO_QBI, ZERO_STD, DOLLARS_2015])),
    ('1.4', 'alternative minimum tax', 'liab_amt', DOLLARS_2015),
    ('1.4', 'income tax before credits', 'liab_bc', '; '.join([ZERO_QBI, ZERO_STD, DOLLARS_2015])),
    ('3.3', 'tax credits > child and other dependent tax credit', 'ctc_nonref',
     'child credit eligibility and SSNs of dependents are 0, so every dependent gets only the $500 credit for other '
     'dependents'),
    ('3.3', 'tax credits > total refundable credits > additional child tax credit', 'ctc_ref',
     'as the child tax credit: with no qualifying child there is no refundable part'),
    ('3.3', 'tax credits > total refundable credits > earned income credit', 'eitc',
     'no column at 0 (EITC children are the PUF\'s count; ages missing for dependent filers)'),
    ('3.3', 'income tax after credits', 'liab_after_credits', 'the child tax credit (above); ' + DOLLARS_2015),
    ('3.3', 'total income tax', 'liab_iit', 'the child tax credit (above); ' + DOLLARS_2015),
]


def soi_cells(table, item, stat):
    """SOI's cells of one item by AGI, all returns, as (lo, hi, value), lowest first; value in dollars or returns.
    A non-blank 'combined' cell holds its own bracket and the blank ones after it (targets.published_cells)."""
    if table not in _READ:
        t = pd.read_csv(os.path.join(SOI, FILES[table]), dtype=str, keep_default_na=False)
        _READ[table] = t[(t.tax_year == str(Y)) & (t.table == table) & (t.panel == 'All returns')
                         & ~t.row_label.str.endswith('total')]
    t = _READ[table]
    # AGI is published as one amount with no " > amount" in its name
    name = 'number of returns' if item == 'returns' else item if item == AGI_ITEM else f'{item} > {stat}'
    t = t[t.item == name]
    assert len(t) == 19, f'{table} {name}: {len(t)} AGI rows'
    out = []
    t = t.assign(lo=[-np.inf if v in ('', '-Inf') else float(v) for v in t.row_agi_lo]).sort_values('lo')
    for r in t.itertuples():
        lo = r.lo
        hi = np.inf if r.row_agi_hi in ('', 'Inf') else float(r.row_agi_hi)
        if r.value == '':
            assert out and out[-1][3], f'{table} {name}: a blank cell with no cell to join'
            out[-1] = (out[-1][0], hi, out[-1][2], True)
        else:
            v = float(r.value) * (1000 if stat == 'amount' else 1)
            out.append((lo, hi, v, r.flag == 'combined'))
    return [(lo, hi, v) for lo, hi, v, _ in out]


def label(lo, hi):
    if lo == -np.inf:
        return 'no AGI'
    if hi == np.inf:
        return f'${lo / 1e6:g}M or more' if lo >= 1e6 else f'${lo:,.0f} or more'
    f = lambda v: f'${v / 1e6:g}M' if v >= 1e6 else f'${v:,.0f}'
    return f'{f(lo)} to {f(hi)}'


def agi_check(f, c, inp):
    """Per-record gap between the calculator's AGI and the rebuild's, split by step (module docstring)."""
    x = lambda d, k: d[k].to_numpy(float)
    rebuild = C.rebuild_agi(f)
    line13 = x(f, 'capital_gains_line13')
    p0 = (x(inp, 'part_active') + x(inp, 'part_passive') - x(inp, 'part_active_loss') - x(inp, 'part_passive_loss')
          - x(inp, 'part_179') + x(inp, 'scorp_active') + x(inp, 'scorp_passive') - x(inp, 'scorp_active_loss')
          - x(inp, 'scorp_passive_loss') - x(inp, 'scorp_179'))
    workaround = x(c, 'salt_workaround_part') + x(c, 'salt_workaround_scorp')
    seca = x(c, 'liab_seca_er')
    terms = pd.DataFrame({
        'capital gains': x(c, 'txbl_kg') - line13,
        'taxable Social Security': x(c, 'txbl_ss') - x(f, 'ss_taxable'),
        'self-employment tax deduction': -(seca - x(f, 'se_tax_ded')),
        'state tax workaround': -workaround,
        # calculator_fix work copy (29 October): the pass-through amount the calculator puts in Schedule E, read as
        # Schedule E less net rent and net estate income, so the step is right with or without the pass-through fix
        'pass-through losses': (x(c, 'sch_e') - x(c, 'net_rent') - x(c, 'net_estate')) - (p0 - workaround),
        'other adjustments': -((x(c, 'above_ded') - seca) - (x(f, 'adjustments') - x(f, 'se_tax_ded'))),
    })
    gap = x(c, 'agi') - rebuild
    terms['rest'] = gap - terms.sum(axis=1)
    return rebuild, gap, terms


def summarise_gap(gap, terms, w, filer, tol=1.0):
    rows = []
    for name, sel in [('filers', filer), ('non-filers', ~filer)]:
        d = np.abs(gap) > tol
        rows.append({'records': name, 'term': 'all (AGI gap)', 'records with a gap over $1': int((d & sel).sum()),
                     'share of records (%)': 100 * (d & sel).sum() / sel.sum(),
                     'units with a gap (m)': w[d & sel].sum() / 1e6,
                     'gap, weighted ($bn)': (w * gap)[sel].sum() / 1e9,
                     'gap, absolute, weighted ($bn)': (w * np.abs(gap))[sel].sum() / 1e9})
        for t in terms.columns:
            v = terms[t].to_numpy()
            d = np.abs(v) > tol
            rows.append({'records': name, 'term': t, 'records with a gap over $1': int((d & sel).sum()),
                         'share of records (%)': 100 * (d & sel).sum() / sel.sum(),
                         'units with a gap (m)': w[d & sel].sum() / 1e6,
                         'gap, weighted ($bn)': (w * v)[sel].sum() / 1e9,
                         'gap, absolute, weighted ($bn)': (w * np.abs(v))[sel].sum() / 1e9})
    return pd.DataFrame(rows)


def levels(v, agi, weights, filer):
    """Every item's SOI cells against the file's, by the AGI given, filers only. Long form."""
    rows = []
    for table, item, col, zero in [('1.4', 'returns', None, '')] + ITEMS:
        for stat in ('count', 'amount'):
            if (item, stat) in SKIP:
                continue
            cells = soi_cells(table, item, 'number of returns' if stat == 'count' else 'amount')
            if col is None:
                val = np.ones(len(v))
            elif col == 'item_ded' and stat == 'count':
                val = v['itemizing'].to_numpy(float)
            else:
                val = v[col].to_numpy(float)
            per = (val > 0).astype(float) if (stat == 'count' and col is not None) else val
            for lo, hi, soi in cells + [(-np.inf, np.inf, None)]:
                sel = filer & (agi >= lo) & (agi < hi)
                r = {'table': table, 'item': item, 'statistic': stat, 'lo': lo, 'hi': hi,
                     'AGI group': 'all returns' if soi is None else label(lo, hi),
                     'SOI': sum(c[2] for c in cells) if soi is None else soi, 'depends on 0 columns': zero}
                for wname, w in weights.items():
                    r[wname] = (w * per)[sel].sum()
                rows.append(r)
    return pd.DataFrame(rows)


def fmt_table(d, stat):
    """A levels table for the report: returns in millions, dollars in billions."""
    div = 1e6 if stat == 'count' else 1e9
    out = pd.DataFrame({'AGI group': d['AGI group']})
    out['SOI'] = d['SOI'] / div
    out['file, before the tilt'] = d['before the tilt'] / div
    out['file, after the tilt'] = d['after the tilt'] / div
    out['after / SOI'] = np.where(d['SOI'] != 0, d['after the tilt'] / d['SOI'], np.nan)
    return out.to_markdown(index=False, floatfmt=',.3f')


def main():
    out = C.OUT
    f = pd.read_parquet(C.INPUT)
    c = pd.read_parquet(f'{out}/calculator_{Y}.parquet')
    inp = pd.read_parquet(f'{out}/calculator_input_{Y}.parquet')
    assert (c['id'].to_numpy() == f['record_id'].to_numpy()).all() and (inp['id'] == c['id']).all(), 'record order'
    c['liab_after_credits'] = c['liab_bc'] - c['nonref'] - c['ref_iit']
    filer = f['filer'].to_numpy() == 1
    weights = {'before the tilt': f['weight_start'].to_numpy(float), 'after the tilt': f['weight'].to_numpy(float)}
    w = weights['after the tilt']
    rebuild, gap, terms = agi_check(f, c, inp)
    s = summarise_gap(gap, terms, w, filer)
    s.to_csv(f'{out}/agi_check.csv', index=False)
    per = pd.DataFrame({'id': c['id'], 'filer': filer.astype(int), 'weight': w, 'agi_rebuild': rebuild,
                        'agi_calculator': c['agi'], 'gap': gap}).join(terms)
    per.to_parquet(f'{out}/agi_check_records.parquet', index=False)
    agi = c['agi'].to_numpy(float)
    lv = levels(c, agi, weights, filer)
    lv.to_csv(f'{out}/levels.csv', index=False)
    levels(c, rebuild, weights, filer).to_csv(f'{out}/levels_by_rebuild_agi.csv', index=False)

    man = pd.read_json(f'{out}/calculator_manifest.json', typ='series')
    lines = [f'# The GPU tax calculator on the {Y} file, against SOI', '',
             f'File: `{C.INPUT}` ({len(f):,} records, {filer.sum():,} filers). Calculator: tax-simulator-gpu at '
             f'commit {man["commit"]}, {Y} law, {", ".join(man["devices"])}. Nominal {Y} dollars; filers only; AGI '
             'groups by the calculator\'s AGI. Returns in millions, dollars in billions.', '',
             '## The AGI check', '',
             'The calculator\'s AGI against the rebuild\'s own (the sum of its AGI parts), split by the step where '
             'they part (the docstring of report_calculator.py says what each step is). Weighted by the weights '
             'after the tilt.', '',
             s.to_markdown(index=False, floatfmt=',.3f'), '']
    tot = lv[lv['AGI group'] == 'all returns']
    t = tot[['table', 'item', 'statistic', 'SOI', 'before the tilt', 'after the tilt']].copy()
    t['table'] = 'Table ' + t['table']   # as text, so the table number is not printed as a number
    div = np.where(t['statistic'] == 'count', 1e6, 1e9)
    for k in ('SOI', 'before the tilt', 'after the tilt'):
        t[k] = t[k] / div
    t['after / SOI'] = t['after the tilt'] / t['SOI']
    t['depends on columns set to 0'] = tot['depends on 0 columns'].str.len().gt(0).map({True: 'yes', False: ''})
    lines += ['## All returns', '', t.to_markdown(index=False, floatfmt=',.3f'), '',
              'What each line depends on that is 0 for now:', '']
    lines += [f'- {item}: {zero}' for _, item, _, zero in ITEMS if zero]
    lines += ['']
    for table, item, col, zero in [('1.4', 'returns', None, '')] + ITEMS:
        for stat in ('count', 'amount'):
            d = lv[(lv['item'] == item) & (lv['statistic'] == stat)]
            if d.empty:
                continue
            what = 'returns (millions)' if stat == 'count' else '$ billions'
            lines += [f'## {item}: {what} (SOI Table {table})' + (' [depends on columns set to 0]' if zero else ''),
                      '', fmt_table(d, stat), '']
    nf = ~filer
    lines += ['## Non-filers', '',
              f'The calculator also runs on the {nf.sum():,} non-filer records ({w[nf].sum() / 1e6:,.2f} million '
              f'units after the tilt). On them, after the tilt: income tax before credits '
              f'${(w * c["liab_bc"])[nf].sum() / 1e9:,.2f} billion; EITC ${(w * c["eitc"])[nf].sum() / 1e9:,.2f} '
              f'billion on {w[nf & (c["eitc"] > 0)].sum() / 1e6:,.2f} million units; additional child tax credit '
              f'${(w * c["ctc_ref"])[nf].sum() / 1e9:,.2f} billion. SOI counts filed returns only, so none of this '
              'is in the tables above.', '']
    with open(f'{out}/report.md', 'w') as fh:
        fh.write('\n'.join(lines))
    common.say('\n'.join(lines[:40]))


if __name__ == '__main__':
    main()
