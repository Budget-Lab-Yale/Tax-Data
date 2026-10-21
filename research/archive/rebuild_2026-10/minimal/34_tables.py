"""
34_tables.py  (minimal example, step 34: business income in its parts on the
PUF side. Status: 34_STATUS.md; settings: 34_settings_scf.json.)

What it does
  Builds table E34 from table E33 (33_tables.py), new files only. Table E33
  has one PUF item business_income = E00900 + E02000 + E02100. Table E34 adds
  its parts as PUF-owned model variables (absent on SCF rows). Every loss
  field the PUF stores as a positive number stays a positive number here and
  carries the sign -1 in the sum (PARTS below); the fields the PUF stores
  with their sign (Schedule C, farm, farm rental, the Schedule E remainder)
  keep it.
    sch_c               E00900  Schedule C net profit or loss          +1 (signed)
    part_pass_inc       E25940  partnership passive income             +1
    part_nonpass_inc    E25980  partnership nonpassive income          +1
    part_pass_loss      E25920  partnership passive loss               -1 (stored positive)
    part_nonpass_loss   E25960  partnership nonpassive loss            -1 (stored positive)
    part_179            E26110  partnership section 179 deduction      -1 (stored positive)
    scorp_pass_inc      E26170  S corporation passive income           +1
    scorp_nonpass_inc   E26190  S corporation nonpassive income        +1
    scorp_pass_loss     E26160  S corporation passive loss             -1 (stored positive)
    scorp_nonpass_loss  E26180  S corporation nonpassive loss          -1 (stored positive)
    scorp_179           E26100  S corporation section 179 deduction    -1 (stored positive)
    rent_inc            E25850  rent and royalty net income            +1
    rent_loss           E25860  rent and royalty net loss              -1 (stored positive)
    estate_inc          E26390  estate and trust net income            +1
    estate_loss         E26400  estate and trust net loss              -1 (stored positive)
    farm_rent           E27200  farm rental net income or loss (Form 4835)  +1 (signed)
    sche_other          E02000 minus the fifteen Schedule E fields above: the rest of
                        Schedule E (REMICs and whatever else the PUF does not
                        carry separately)                       +1 (signed remainder)
    farm                E02100  Schedule F net profit or loss          +1 (signed)
  The signs are checked on the data (a field said to be stored positive
  must have no value below zero), and the partnership and S corporation
  fields are checked against E26270 (their combined net).
  Identity checks, in whole dollars, on every PUF filer record: the parts
  with their signs add to E00900 + E02000 + E02100 (= E33's
  business_income), and AGI (E00100) = wages + interest + dividends + the
  four capital gains pieces + the other step-33 items with their signs +
  the parts with their signs + other_agi (unchanged from E33).
  Crude non-filers (step 23's CPS rows): sch_c = sole_prop (signed), rent_inc =
  rent (the pool stores it at zero or above), rent_loss = minus rent_loss (the
  pool stores it at zero or below; here a positive number), every other part zero; they add to
  E33's business_income (asserted).
  SCF rows: the parts are absent (band -1, no dollars). Their business_income
  is E33's, unchanged, and so are its bands and settings.
  Bands: each part cut on the PUF filers and applied to the whole PUF side,
  as 33_tables.py does.
  The model learns the parts from the band-edge file's meta.step33_items
  (E33's items, then the parts as 'puf'). meta.computed_totals says that
  business_income on a generated PUF record is computed from the parts
  (03_generate.py MINIMAL_COMPUTED_EXTRA, 04_dollars.py MINIMAL_SUM_TOTAL).

Writes
  data/stacked_E34.parquet, data/band_edges_E34.json
  map/V34/map_puf_2015.parquet, map_puf_2019.parquet (V33's rows plus the
          parts), map_scf_2015.parquet (V33's), growth_factors_34.csv
  step34/tables_checks.md
Usage (Slurm): sbatch research/rebuild/minimal/34_cpu.sbatch tables
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
D = f'{R}/data'
OUT = f'{R}/step34'
MAPS = f'{R}/map/V34'
MAPS33 = f'{R}/map/V33'


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    mod = importlib.util.module_from_spec(s)
    s.loader.exec_module(mod)
    return mod


T33 = load('t33', '33_tables.py')
PUF_RAW, POOL, CPI_2015, CPI_2017, NF_OFFSET = T33.PUF_RAW, T33.POOL, T33.CPI_2015, T33.CPI_2017, T33.NF_OFFSET
band_variable, fixed_edge, wquantile, N_BANDS = T33.band_variable, T33.fixed_edge, T33.wquantile, T33.N_BANDS

# part: (PUF field, sign in the sum, how the PUF stores it)
PARTS = {
    'sch_c': ('E00900', 1, 'signed'),
    'part_pass_inc': ('E25940', 1, 'positive'),
    'part_nonpass_inc': ('E25980', 1, 'positive'),
    'part_pass_loss': ('E25920', -1, 'positive'),
    'part_nonpass_loss': ('E25960', -1, 'positive'),
    'part_179': ('E26110', -1, 'positive'),
    'scorp_pass_inc': ('E26170', 1, 'positive'),
    'scorp_nonpass_inc': ('E26190', 1, 'positive'),
    'scorp_pass_loss': ('E26160', -1, 'positive'),
    'scorp_nonpass_loss': ('E26180', -1, 'positive'),
    'scorp_179': ('E26100', -1, 'positive'),
    'rent_inc': ('E25850', 1, 'positive'),
    'rent_loss': ('E25860', -1, 'positive'),
    'estate_inc': ('E26390', 1, 'positive'),
    'estate_loss': ('E26400', -1, 'positive'),
    'farm_rent': ('E27200', 1, 'signed'),
    'sche_other': (None, 1, 'signed remainder'),
    'farm': ('E02100', 1, 'signed'),
}
SIGN = {p: s for p, (_, s, _) in PARTS.items()}
SCHE_FIELDS = [f for p, (f, _, _) in PARTS.items() if f and p not in ('sch_c', 'farm')]
PARTNERSHIP = ['part_pass_inc', 'part_nonpass_inc', 'part_pass_loss', 'part_nonpass_loss', 'part_179']
SCORP = ['scorp_pass_inc', 'scorp_nonpass_inc', 'scorp_pass_loss', 'scorp_nonpass_loss', 'scorp_179']
RAW = ['RECID', 'E00100', 'E00900', 'E02000', 'E02100', 'E26270'] + SCHE_FIELDS

# 2019 growth of each part's dollars: the IRS (SOI Table 1.4) amount per
# return with the item, 2019 over 2015. (patterns for positive values,
# patterns for negative values); a loss field stored positive grows with the
# loss rows. Where the IRS has no row the closest aggregate is used (GROWTH_NOTE).
NI, NL = ' > net income', r' > net loss.*'
GROWTH = {
    'sch_c': (['business or profession' + NI], ['business or profession' + NL]),
    'farm': (['farm' + NI], ['farm' + NL]),
    'rent_inc': (['total rental and royalty' + NI], None),
    'rent_loss': (['total rental and royalty' + NL], None),
    'estate_inc': (['estate and trust' + NI], None),
    'estate_loss': (['estate and trust' + NL], None),
    'farm_rent': (['farm rental' + NI], ['farm rental' + NL]),
    **{p: (['partnership and s corporation' + NI], None)
       for p in ['part_pass_inc', 'part_nonpass_inc', 'scorp_pass_inc', 'scorp_nonpass_inc', 'part_179', 'scorp_179']},
    **{p: (['partnership and s corporation' + NL], None)
       for p in ['part_pass_loss', 'part_nonpass_loss', 'scorp_pass_loss', 'scorp_nonpass_loss']},
    'sche_other': ('business_income', None),
}
GROWTH_NOTE = {
    'part_179': 'no IRS row for the section 179 deduction: partnership and S corporation net income',
    'scorp_179': 'no IRS row for the section 179 deduction: partnership and S corporation net income',
    **{p: 'the IRS publishes partnerships and S corporations together, passive and nonpassive together'
       for p in PARTNERSHIP + SCORP if not p.endswith('179')},
    'rent_inc': 'IRS total rental and royalty net income (the PUF field holds both)',
    'rent_loss': 'IRS total rental and royalty net loss (the PUF field holds both)',
    'sche_other': 'no IRS row: step 33\'s business income factor (Schedules C, E and F together)',
}
SHEET = json.load(open(f"{HERE}/34_settings_scf.json"))
assert {k: v for k, v in SHEET.items() if k not in ("_what", "step34_parts")} == \
    {k: v for k, v in json.load(open(f"{HERE}/33_settings_scf.json")).items() if k != "_what"}, "SCF settings differ from step 33"
assert {p: (d["sign"], d["stored"]) for p, d in SHEET["step34_parts"].items() if p != "_what"} == \
    {p: (s, how) for p, (_, s, how) in PARTS.items()}, "34_settings_scf.json step34_parts disagrees with PARTS"
LOG = []


def say(*a):
    s = ' '.join(str(x) for x in a)
    print(s, flush=True)
    LOG.append(s)


def soi_rows(val, y, patterns, base):
    """Amount and count summed over the component rows present in both years."""
    amt = cnt = 0.0
    for p in patterns:
        try:
            a = abs(T33.soi_get(val, y, [p], 'amount'))
            c = T33.soi_get(val, y, [p], 'number of returns')
            T33.soi_get(val, base, [p], 'amount')
        except KeyError as ex:
            say(f'SOI: {p!r} missing in {y} or {base} ({ex}); left out')
            continue
        amt, cnt = amt + a, cnt + c
    return amt, cnt


def factor(val, patterns):
    if patterns == 'business_income':
        (a15, c15), (a19, c19) = T33.soi_item(val, 2015, 'business_income', base=2019), \
            T33.soi_item(val, 2019, 'business_income', base=2015)
    else:
        (a15, c15), (a19, c19) = soi_rows(val, 2015, patterns, 2019), soi_rows(val, 2019, patterns, 2015)
    assert a15 > 0 and c15 > 0 and c19 > 0, (patterns, a15, c15, a19, c19)
    return (a19 / c19) / (a15 / c15), a15, c15, a19, c19


def describe(x, w):
    x = np.asarray(x, float)
    nz = x != 0
    out = {'share nonzero (%)': 100 * w[nz].sum() / w.sum(), 'share negative (%)': 100 * w[x < 0].sum() / w.sum(),
           'total ($ billion)': (w * x).sum() / 1e9}
    for lab, sel in [('gains', x > 0), ('losses', x < 0)]:
        if sel.any():
            q = wquantile(x[sel], w[sel], [0.1, 0.5, 0.9, 0.99])
            out.update({f'{lab}: P10': q[0], f'{lab}: P50': q[1], f'{lab}: P90': q[2], f'{lab}: P99': q[3]})
    return out


def main():
    os.makedirs(OUT, exist_ok=True)
    os.makedirs(MAPS, exist_ok=True)
    t = pd.read_parquet(f'{D}/stacked_E33.parquet')
    edges = json.load(open(f'{D}/band_edges_E33.json'))
    say('E33 rows by source and year:', t.groupby(['source', 'year']).size().to_dict())
    filers = ((t.source == 'puf') & (t.filer == 1)).to_numpy()
    nfr = ((t.source == 'puf') & (t.filer == 0)).to_numpy()
    scf = (t.source == 'scf').to_numpy()
    pside = filers | nfr

    # ---- PUF filers
    raw = pd.read_csv(PUF_RAW, usecols=RAW)
    raw = raw[raw.RECID < 999996]
    na = {c: int(raw[c].isna().sum()) for c in raw.columns}
    say('raw PUF fields, missing values per field:', na)
    assert sum(na.values()) == 0, 'missing values in the raw PUF fields'
    I = {c: raw[c].round().astype(np.int64).to_numpy() for c in RAW if c != 'RECID'}
    for c in I:
        assert np.array_equal(I[c], raw[c].to_numpy()), f'{c}: not whole dollars'
    signs = []
    for p, (f, s, how) in PARTS.items():
        if f is None:
            continue
        neg, pos = int((I[f] < 0).sum()), int((I[f] > 0).sum())
        signs.append({'part': p, 'PUF field': f, 'stored as': how, 'sign in the sum': s,
                      'records below zero': neg, 'records above zero': pos})
        if how == 'positive':
            assert neg == 0, f'{f}: said to be stored positive but has {neg} values below zero'
    signs = pd.DataFrame(signs)
    say(signs.to_string())
    x = {p: I[f] for p, (f, _, _) in PARTS.items() if f}
    sche_named = sum(SIGN[p] * x[p] for p in PARTS if PARTS[p][0] in SCHE_FIELDS)
    x['sche_other'] = I['E02000'] - sche_named
    rem = x['sche_other']
    say(f'Schedule E remainder (sche_other): {int((rem != 0).sum())} records nonzero of {len(rem)}; '
        f'{int((rem < 0).sum())} below zero; unweighted sum ${rem.sum():,}; largest |value| ${np.abs(rem).max():,}')
    ps_net = sum(SIGN[p] * x[p] for p in PARTNERSHIP + SCORP)
    gap = ps_net - I['E26270']
    say(f'partnership + S corporation parts against E26270 (their combined net): {int((gap != 0).sum())} records differ; '
        f'largest gap ${np.abs(gap).max():,}')
    total = sum(SIGN[p] * x[p] for p in PARTS)
    target = I['E00900'] + I['E02000'] + I['E02100']
    bad = int((total != target).sum())
    say(f'parts against E00900 + E02000 + E02100 on {len(total):,} PUF records, whole dollars: {bad} records off')
    assert bad == 0
    pf = pd.DataFrame({'record_id': raw['RECID'].astype(float).to_numpy(), **x}).set_index('record_id')
    rid = t.loc[filers, 'record_id'].to_numpy()
    pf = pf.loc[rid]
    assert len(pf) == filers.sum()
    b33 = t.loc[filers, 'business_income'].to_numpy()
    assert np.array_equal(np.round(b33), b33)
    tot = sum(SIGN[p] * pf[p].to_numpy() for p in PARTS)
    bad = int((tot != b33.astype(np.int64)).sum())
    say(f'parts against table E33 business_income on {len(tot):,} PUF filer records: {bad} records off')
    assert bad == 0
    # AGI identity with the parts in place of business_income
    fold33 = {v: s for v, s in T33.AGI_SIGN.items() if s != 0 and v != 'business_income'}
    pieces = ['wages', 'interest', 'dividends'] + T33.PIECES
    agi = sum(t.loc[filers, v].round().astype(np.int64).to_numpy() for v in pieces) + \
        sum(s * t.loc[filers, v].round().astype(np.int64).to_numpy() for v, s in fold33.items()) + tot + \
        t.loc[filers, 'other_agi'].round().astype(np.int64).to_numpy()
    e00100 = raw.set_index(raw.RECID.astype(float)).loc[rid, 'E00100'].round().astype(np.int64).to_numpy()
    bad = int((agi != e00100).sum())
    say(f'AGI identity (all modelled parts + remainder) on {len(agi):,} PUF filer records, whole dollars: {bad} records off')
    assert bad == 0
    for p in PARTS:
        t[p] = np.nan
        t.loc[filers, p] = pf[p].to_numpy().astype(float)

    # ---- non-filers
    pool = pd.read_csv(POOL)
    pool['record_id'] = (NF_OFFSET + pool['id'].astype(np.int64)).astype(float)
    pool = pool.set_index('record_id').loc[t.loc[nfr, 'record_id'].to_numpy()]
    cols = ['sole_prop', 'rent', 'rent_loss']
    say('non-filer pool fields, missing values:', pool[cols].isna().sum().to_dict())
    assert not pool[cols].isna().any().any()
    for c in cols:
        say(f'non-filer pool {c}: {int((pool[c] < 0).sum())} below zero, {int((pool[c] > 0).sum())} above zero')
    dfl = CPI_2015 / CPI_2017
    assert (pool['rent'] >= 0).all() and (pool['rent_loss'] <= 0).all(), 'pool rent / rent_loss signs'
    nf = {'sch_c': pool['sole_prop'].to_numpy() * dfl, 'rent_inc': pool['rent'].to_numpy() * dfl,
          'rent_loss': -pool['rent_loss'].to_numpy() * dfl}
    for p in PARTS:
        t.loc[nfr, p] = nf.get(p, np.zeros(nfr.sum()))
    gap = np.abs(sum(SIGN[p] * t.loc[nfr, p].to_numpy() for p in PARTS) - t.loc[nfr, 'business_income'].to_numpy()).max()
    say(f'non-filers: parts against E33 business_income, largest gap ${gap:.6f}')
    assert gap < 0.01

    # ---- bands
    w_all = t['weight'].to_numpy(float)
    for p in PARTS:
        xp = t.loc[filers, p].to_numpy(float)
        assert (xp != 0).any(), f'{p}: no nonzero values on the PUF'
        b, rec = band_variable(xp, w_all[filers])
        edges[f'puf/2015/{p}'] = rec
        b2, _ = band_variable(t.loc[pside, p].to_numpy(float), w_all[pside], fixed_edge(rec, 1.0))
        t[f'{p}_band'] = np.int16(-1)
        t.loc[pside, f'{p}_band'] = b2
        assert np.array_equal(b2[t.loc[pside, 'filer'].to_numpy() == 1], b), p
        t[f'{p}_band'] = t[f'{p}_band'].astype('int16')
    # the SCF's years need an edge record for every amount (02_model reads the band count);
    # absent items in E33 have none, so none is needed here either
    for p in PARTS:
        bb, xx = t[f'{p}_band'], t[p]
        assert ((bb == -1) == xx.isna()).all() and ((bb == 0) == (xx == 0)).all(), p
        assert (bb[pside] >= 0).all() and (bb[scf] == -1).all(), p
    absent_ok = T33.PIECES + ['other_agi', 'net_worth', 'capital_gains_reported', 'other_agi_e27'] + \
        [v for v in T33.NEW if T33.ITEMS[v]['absent_on_scf']] + [v + '_reported' for v in T33.SHARED_NEW] + list(PARTS)
    na = t.drop(columns=absent_ok).isna().sum()
    say('missing values outside the absent slots:', int(na.sum()), na[na > 0].to_dict())
    assert na.sum() == 0
    na_p = {p: int(t.loc[pside, p].isna().sum()) for p in PARTS}
    say('missing values of the parts on the PUF side:', na_p)
    assert sum(na_p.values()) == 0
    t.to_parquet(f'{D}/stacked_E34.parquet', index=False)
    items = dict(edges['meta']['step33_items'], **{p: 'puf' for p in PARTS})
    edges['meta'] = dict(edges['meta'], E34='step 34 (34_tables.py): E33 plus the parts of business income '
                         '(PUF-owned); business_income on a generated PUF record is computed from them',
                         step33_items=items,
                         computed_totals={'business_income': SIGN})
    json.dump(edges, open(f'{D}/band_edges_E34.json', 'w'), indent=1)
    say('written', f'{D}/stacked_E34.parquet', len(t), 'rows; slots added:', list(PARTS))

    # ---- maps: V33's rows unchanged plus the parts
    dollars = load('dollars34', '04_dollars.py')
    keep = dollars.AMOUNTS
    try:
        dollars.AMOUNTS = list(PARTS)
        mp = dollars.build_map(t[pside].reset_index(drop=True), edges, 'puf')
    finally:
        dollars.AMOUNTS = keep
    m33 = pd.read_parquet(f'{MAPS33}/map_puf_2015.parquet')
    pd.concat([m33, mp], ignore_index=True).sort_values(['variable', 'band', 'value'], kind='stable') \
        .to_parquet(f'{MAPS}/map_puf_2015.parquet', index=False)
    pd.read_parquet(f'{MAPS33}/map_scf_2015.parquet').to_parquet(f'{MAPS}/map_scf_2015.parquet', index=False)
    val = T33.soi_table([2015, 2019])
    rows, fac = [pd.read_parquet(f'{MAPS33}/map_puf_2019.parquet')], []
    for p in PARTS:
        pos_pat, neg_pat = GROWTH[p]
        fp, a15, c15, a19, c19 = factor(val, pos_pat)
        fn = factor(val, neg_pat)[0] if neg_pat else fp
        fac.append({'part': p, 'PUF field': PARTS[p][0] or 'remainder', 'IRS rows': str(pos_pat) + (f' / losses {neg_pat}' if neg_pat else ''),
                    'factor, positive values': fp, 'factor, negative values': fn,
                    'note': GROWTH_NOTE.get(p, '')})
        g = mp[mp.variable == p].copy()
        v = g['value'].to_numpy(float).copy()
        v[v > 0] *= fp
        v[v < 0] *= fn
        g['value'] = v
        rows.append(g)
    g19 = pd.concat(rows, ignore_index=True).sort_values(['variable', 'band', 'value'], kind='stable')
    g19.to_parquet(f'{MAPS}/map_puf_2019.parquet', index=False)
    fac = pd.DataFrame(fac)
    fac.to_csv(f'{MAPS}/growth_factors_34.csv', index=False)
    say(fac.to_string())

    # ---- checks for the notes
    w = w_all[filers]
    desc = pd.DataFrame([{'part': p, 'PUF field': PARTS[p][0] or 'remainder', 'sign': SIGN[p],
                          **describe(t.loc[filers, p].to_numpy(float), w)} for p in PARTS])
    nf_tab = pd.DataFrame([{'part': p, 'non-filers with it (millions)': w_all[nfr][t.loc[nfr, p].to_numpy() != 0].sum() / 1e6,
                            'amount ($ billion)': (w_all[nfr] * t.loc[nfr, p].to_numpy()).sum() / 1e9} for p in PARTS])
    md = ['# Step 34 tables: checks', '', '## Log', '', '```', *LOG, '```', '',
          '## How the PUF stores each field', '', signs.to_markdown(index=False), '',
          '## The parts on PUF filers, 2015 (weighted; signed as they enter the sum is NOT applied here: '
          'loss fields are positive amounts)', '', desc.to_markdown(index=False, floatfmt=',.1f'), '',
          '## Crude non-filers: the parts', '', nf_tab.to_markdown(index=False, floatfmt='.2f'), '',
          '## Growth to 2019', '', fac.to_markdown(index=False, floatfmt='.4f'), '']
    open(f'{OUT}/tables_checks.md', 'w').write('\n'.join(md) + '\n')
    open(f'{OUT}/TABLES_DONE', 'w').close()


if __name__ == '__main__':
    main()
