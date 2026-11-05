"""The package for a holder of the 2015 PUF (stage package; settings generation.filers real_puf; PACKAGE.md).

With filers real_puf every 2015 PUF record is in the year's file, its own PUF items grown and the networks' items
drawn given it (generate.real_puf_part). This stage writes the files that let someone with their own copy of the
2015 PUF (CRS) rebuild the year's Tax-Simulator file (taxsim_file.py's tax_units_<year>) without receiving any PUF
value, and tests that they do.

    python package.py build      the package and the round trip of the Python merge script
    python package.py paths      prints the package folder, the PUF and the check folder (for run.sbatch)
    python package.py check_r    the round trip of the R merge script, whose output run.sbatch writes to the check
                                 folder

The package (<out>/package/<run>/, REBUILD_PACKAGE_OUT overrides it; chmod 700), always relative to the 2015 PUF:
  factors.parquet (and factors.csv to read)  one row per PUF item and step: the factor for positive amounts and the
        factor for negative amounts, the same for every record. The steps are the year's chain of starts
        (target_list.start_spec): the growth from 2015 to the year the records were drawn (growth.factors, as
        dollars.own_values applies it), that year's Schedule A multipliers on six itemized items (taxes.
        greedy_schedule_a, one number per category and year), then each projection step's factors (tilt.grow_dollars,
        start_growth.csv of each year's tilt folder). Every step is checked against the files: each item of every
        PUF record copy must be exactly its 2015 amount times the factors so far, or the stage stops.
  records.parquet  one row per PUF record copy in the year's Tax-Simulator file: RECID and copy; row, its place in the
        file; weight_factor (the weight over the PUF weight S006 / 100); medical_floor (0.075 or 0.1, the floor the
        pipeline used for the 2015 value of other itemized deductions); state_ref_kept (0 where the tax benefit rule
        removed the taxable state refund, in any year of the chain, else 1); the aggregate records' drawn filing status
        and dependent counts (-1 on the other records); the imputed items the merge needs, as values; and the
        Tax-Simulator columns that are not the merge's own columns, as values (prefix ts.).
  extra_records.parquet  the file's records that are not PUF records (non-filers, the Forbes list), every column.
  spec.json, merge_puf.py, merge_puf.R, README.md.
The AMT net operating loss is shipped as the cap rule the merge applies (merge_puf.amt_cap), not as values; capital
gains after the loss limit, other income and taxable Social Security are computed by the merge. Outside the package,
with counts only (no record values): <out>/package/roundtrip_<run>_python.csv and _r.csv, factors_<run>.csv,
summary_<run>.md. The round trip fails the stage unless every column of the merged file equals the Tax-Simulator file
on every record, or differs by rounding only (at most one millionth of a dollar, or one part in 10^12).
"""
import json
import os
import shutil
import subprocess
import sys

import numpy as np
import pandas as pd

import common

HERE = os.path.dirname(os.path.abspath(__file__))
Y = common.TARGET_YEAR
PKG = os.environ.get('REBUILD_PACKAGE_OUT', f'{common.OUT}/package/{common.RUN}')
CHECK = PKG.rstrip('/') + '_check'
SUMMARY = os.path.dirname(PKG.rstrip('/'))
TAXSIM = os.environ.get('REBUILD_TAXSIM_OUT', f'{common.OUT}/taxsim/{common.RUN}')
say = common.say

# The Tax-Simulator columns made by a formula of the record's items (merge_puf.formula), and those that may be shipped
# as values because they depend only on imputed items (the dependent filers' and spouses' ages and flags, the
# dependents' columns, the mortgage share): any other column that is not one of the merge's own columns stops the stage
FORMULAS = ['id', 'weight', 'sole_prop1', 'sole_prop2', 'farm1', 'farm2', 'part_se1', 'part_se2', 'other_inc', 'kg_lt',
            'gross_pens_dist', 'divorce_year', 'kg_lt_years_held', 'kg_lt_basis']
FORMULA_INPUTS = ['spouse_se_share', 'kg_lt_years_held', 'kg_lt_basis', 'record_id']
SHIPPED_OK = {'age1', 'male1', 'male2', 'age2', 'blind2', 'q_death2', 'dep_status', 'prim_mort_share'} | \
    {f'dep_{p}{k}' for p in ('age', 'ssn', 'ctc') for k in (1, 2, 3)}
SCHEDULE_A_STEP = 'Schedule A multiplier'
# Imputed items that are a PUF amount times or plus a part that is not one: shipped as values they would carry a grown
# PUF amount, so the merge computes them from the PUF items (merge_puf.puf_based) and the package ships only the other
# part (the columns listed). kg_lt_basis = the long-term gain times r / (1 - r), r the basis-to-sales ratio of the
# drawn holding period (non_micro_imputations.gains_holding_period); wagebill_farm = the farm income, at least 0;
# wagebill_scorp = the outside workers' pay, plus the record's wages where it has S corporation income and no Schedule
# C or partnership income (non_micro_imputations.qbi)
PUF_BASED = {'kg_lt_basis': ['kg_lt_basis_ratio'], 'wagebill_farm': [],
             'wagebill_scorp': ['wagebill_scorp_outside', 'wagebill_scorp_owner_wages']}
TOL_ABS, TOL_REL = 1e-6, 1e-12


def chmod_private(path):
    """The folder and everything in it readable by the owner only."""
    for root, dirs, files in os.walk(path):
        os.chmod(root, 0o700)
        for f in files:
            os.chmod(os.path.join(root, f), 0o600)


def chain():
    """[(year, its tilt folder)] from the year the records were drawn (a generated start) to the target year."""
    import target_list as TLS
    years, y = [(Y, common.TILT)], Y
    while True:
        spec = TLS.start_spec(y)
        if spec['mode'] == 'generate':
            return years
        y = spec['year']
        years.insert(0, (y, f"{common.OUT}/tilt/{spec['run']}"))


def factor_columns():
    """{file column: 2015 item}: the PUF items carried by a factor (generate.own_items, less the totals the file
    computes from their parts, and less the AMT adjustment, which the merge holds to the loss by its rule; the drawn
    business income, other income's remainder and the AMT adjustment before the cap carry their item's factor)."""
    import generate as G
    totals = ('capital_gains', 'dividends', 'adjustments', 'business_income', 'other_agi', 'amt_nols')
    cols = {v: v for v in G.own_items() if v not in totals}
    cols.update({'business_income_drawn': 'business_income', 'other_agi_remainder': 'other_agi',
                 'amt_nols_drawn': 'amt_nols'})
    return cols


GROWTH_COLUMN = {'amt_nols_drawn': 'amt_nols'}   # the column tilt.grow_dollars grows for a file column


def apply(v, p, n):
    v = np.asarray(v, float)
    return np.where(v > 0, v * p, np.where(v < 0, v * n, v))


def exact_factor(x, y):
    """y / x, moved by the last binary digit where needed so that x * factor is exactly y; and the count still off."""
    q = y / x
    for _ in range(3):
        bad = x * q != y
        if not bad.any():
            break
        up, dn = np.nextafter(q, np.inf), np.nextafter(q, -np.inf)
        q = np.where(bad & (x * up == y), up, np.where(bad & (x * dn == y), dn, q))
    return q, int((x * q != y).sum())


def medical_floor(recid):
    """The floor build_table.puf_rows used for other itemized deductions: 7.5% for the 65+ range (or no range and step
    0's 65+ chance over one half), 10% otherwise; the aggregate records 10%, as Part A reads them."""
    t = pd.read_parquet(common.TABLE, columns=['source', 'record_id', 'age_range', 'age_p5'])
    t = t[(t.source == 'puf').to_numpy()].set_index('record_id')
    old = ((t['age_range'] == 5) | (t['age_p5'] > 0.5)).reindex(np.asarray(recid, float)).to_numpy()
    agg = np.asarray(recid) >= 999996
    assert not pd.isna(old[~agg]).any() and pd.isna(old[agg]).all(), 'a PUF record not in the table'
    return np.where(agg, 0.1, np.where(old == True, 0.075, 0.1))  # noqa: E712


def compare(m, t, cols):
    """One row per column: records that differ, the largest absolute and relative difference and the largest in units
    of the last binary digit, missing on one side only, and whether the column passes (identical, or rounding only)."""
    rows = []
    for c in cols:
        a, b = m[c], t[c]
        if pd.api.types.is_numeric_dtype(a) and pd.api.types.is_numeric_dtype(b):
            x, y = a.to_numpy(float), b.to_numpy(float)
            one = np.isnan(x) ^ np.isnan(y)
            both = ~np.isnan(x) & ~np.isnan(y)
            diff = both & (x != y)
            ad = np.where(diff, np.abs(x - y), 0.0)
            big = np.maximum(np.abs(x), np.abs(y))
            rel = np.where(diff, ad / np.maximum(big, 1.0), 0.0)
            ulp = np.where(diff, ad / np.spacing(np.where(big > 0, big, 1.0)), 0.0)
            ok = not one.any() and bool((~diff | (ad <= np.maximum(TOL_ABS, TOL_REL * np.where(both, big, 0.0)))).all())
            rows.append({'column': c, 'records that differ': int(diff.sum()),
                         'largest absolute difference': float(ad.max()) if len(ad) else 0.0,
                         'largest relative difference': float(rel.max()) if len(rel) else 0.0,
                         'largest difference in last-digit units': float(ulp.max()) if len(ulp) else 0.0,
                         'missing on one side only': int(one.sum()), 'passes': ok})
        else:
            na_a, na_b = pd.isna(a).to_numpy(), pd.isna(b).to_numpy()
            same = (na_a & na_b) | (~na_a & ~na_b & (a.astype(str).to_numpy() == b.astype(str).to_numpy()))
            rows.append({'column': c, 'records that differ': int((~same).sum()), 'largest absolute difference': np.nan,
                         'largest relative difference': np.nan, 'largest difference in last-digit units': np.nan,
                         'missing on one side only': int((na_a ^ na_b).sum()), 'passes': bool(same.all())})
    return pd.DataFrame(rows)


def puf_based(Fr, rec, MG):
    """In place: the parts of the PUF_BASED items that are not PUF amounts, into rec, each checked to give the file's
    item exactly by the merge's own function."""
    import non_micro_imputations as NMI
    M = NMI.holding_model(Y)
    kg, h = Fr['kg_lt'].to_numpy(float), Fr['kg_lt_years_held'].to_numpy(float)
    r = np.zeros(len(Fr))
    r[kg > 0] = M['gain_ratio'](h[kg > 0])
    r[kg < 0] = M['loss_ratio'](h[kg < 0])
    rec['kg_lt_basis_ratio'] = r
    farm, wages = Fr['farm'].to_numpy(float), Fr['wages'].to_numpy(float)
    inc = NMI.qbi_income(Fr, NMI.QBI_KINDS['scorp'][2])
    only = (inc > 0) & (Fr['sole_prop'].to_numpy(float) == 0) & \
        np.column_stack([Fr[p].to_numpy(float) == 0 for p in NMI.QBI_KINDS['part'][2]]).all(1)
    bill = Fr['wagebill_scorp'].to_numpy(float)
    o = np.where(only, bill - wages, bill)
    for _ in range(3):   # the outside part o such that o + wages is the file's bill exactly
        bad = only & (o + wages != bill)
        if not bad.any():
            break
        up, dn = np.nextafter(o, np.inf), np.nextafter(o, -np.inf)
        o = np.where(bad & (up + wages == bill), up, np.where(bad & (dn + wages == bill), dn, o))
    rec['wagebill_scorp_outside'] = o
    rec['wagebill_scorp_owner_wages'] = only.astype(np.int8)
    d = {'kg_lt': kg, 'farm': farm, 'wages': wages}
    MG.puf_based(d, rec, {'puf_based': list(PUF_BASED)})
    for c in PUF_BASED:
        bad = d[c] != Fr[c].to_numpy(float)
        assert not bad.any(), f'{c}: the merge\'s rule differs from the file on {int(bad.sum())} PUF record copies'
    say(f'computed by the merge from the PUF items, not shipped: {list(PUF_BASED)} (each checked exactly); S corporation '
        f'wage bills with the owner\'s wages: {int(only.sum()):,} record copies')


def read_file(path, columns=None):
    import pyarrow.parquet as pq
    names = pq.read_schema(path).names
    cols = columns or [c for c in names if not (c.endswith('_band') or c.startswith(('u_', 'calc_')))]
    return pd.read_parquet(path, columns=cols)


def build():
    assert common.REAL_PUF, ('package: settings generation.filers is synthetic; the package is made only for a file '
                             'built on the real PUF (generation.filers real_puf)')
    import build_table as BT
    import calculator as C
    import dollars as D
    import generate as G
    import growth as GR
    import taxsim_file as TS
    import translate as TR
    sys.path.insert(0, HERE)
    import merge_puf as MG

    os.makedirs(SUMMARY, exist_ok=True)
    links = chain()
    y0 = links[0][0]
    say(f'package for {Y}: the chain of starts {" -> ".join(str(y) for y, _ in links)} (records drawn in {y0})')
    T = pd.read_parquet(f'{TAXSIM}/tax_units_{Y}.parquet')
    F = read_file(f'{common.TILT}/file_{Y}.parquet')
    live = F['weight'].to_numpy(float) > 0
    F = F.loc[live].reset_index(drop=True)
    assert np.array_equal(F['record_id'].to_numpy(np.int64), T['id'].to_numpy(np.int64)), \
        'the year file\'s live records and the Tax-Simulator file differ'
    rid = F['record_id'].to_numpy(np.int64)
    n_copies = len(G.MODELS)
    is_real = lambda i: (i >= G.REAL_ID) & (i < G.REAL_ID + n_copies * G.REAL_COPY_BLOCK)   # the Forbes list from 95e6
    real = is_real(rid)
    Fr = F.loc[real].reset_index(drop=True)
    recid = (rid[real] - G.REAL_ID) % G.REAL_COPY_BLOCK
    copy = (rid[real] - G.REAL_ID) // G.REAL_COPY_BLOCK + 1
    say(f'{len(F):,} live records: {int(real.sum()):,} PUF record copies ({len(np.unique(recid)):,} PUF records), '
        f'{int((~real).sum()):,} other records; {int((~live).sum()):,} records of weight 0 left out, as the '
        f'Tax-Simulator file leaves them out')

    # ---- the definitions (spec), and the 2015 items from the raw PUF by the merge's own function ----------------
    lim = common.SETTINGS['deferrals']['limits'][str(Y)]
    D_meta = D.META
    ratio_items_made = [item for slot, (item, base) in D.RATIOS.items()
                        if item not in ('value.primary_mortgage', 'w_dc_stocks')]   # those two are balance sheet values
    spec = {
        'year': Y, 'drawn_in': y0, 'chain': [y for y, _ in links], 'real_id_base': G.REAL_ID,
        'copy_block': G.REAL_COPY_BLOCK, 'copies': n_copies,
        'raw_fields': list(dict.fromkeys(BT.PUF_FIELDS)),
        'items': {'parts': BT.PARTS, 'sche_parts': BT.SCHE_PARTS, 'adj_parts': BT.ADJ_PARTS, 'adj_fields': BT.ADJ_FIELDS,
                  'new_items': BT.NEW_ITEMS, 'agi_item_signs': {v: s for v, (s, _, _) in BT.ITEMS.items() if s != 0},
                  'suppressed_on_aggregate_records': ['E00800', 'E03500']},
        'derive': {'computed_totals': D_meta['computed_totals'], 'k1_parts': BT.K1_PARTS, 'agi_fold': D.FOLD,
                   'pension_parts': common.PENSION_PARTS,
                   'social_security': {'single': common.SS_BRACKETS['single'], 'joint': common.SS_BRACKETS['joint'],
                                       'rates': common.SS_RATES, 'magi_rate': common.SS_MAGI_RATE},
                   'ratios': D.RATIOS, 'ratio_items_made': ratio_items_made, 'remainders': D.REMAINDERS,
                   'person_caps': D.PERSON_CAPS, 'sums': D.SUMS, 'flags_with': D.FLAGS_WITH, 'deferral_limit': lim,
                   'catch_up_age': common.SETTINGS['deferrals']['catch_up_age'],
                   'roth_share': {'joint': float(TR.roth_share(Y, [1])[0]), 'nonjoint': float(TR.roth_share(Y, [0])[0])}},
    }
    assert set(spec['items']['agi_item_signs']) == set(D.FOLD) and all(
        spec['items']['agi_item_signs'][k] == D.FOLD[k] for k in D.FOLD), 'the AGI items and their signs differ'
    raw = pd.read_csv(common.RAW['puf'], usecols=lambda c: c in set(spec['raw_fields']))   # every PUF record
    assert raw['RECID'].is_unique and np.isin(np.unique(recid), raw['RECID']).all(), 'a PUF record of the file is not in the raw PUF'
    floor_by = pd.Series(medical_floor(raw['RECID'].to_numpy()), index=raw['RECID'].to_numpy())
    x15 = MG.items_2015(raw, spec, floor_by.loc[raw['RECID']].to_numpy())
    cols = factor_columns()
    missing = [c for c in cols if c not in Fr]
    assert not missing, f'factor columns not in the file: {missing}'

    # ---- the factors, step by step, each step checked against the files -----------------------------------------
    by_id = lambda frame: frame.set_index('record_id')
    prev = {}
    steps = []           # rows: column, puf_item, step, what, positive, negative
    checks = []          # rows: step, column, records checked, records that differ
    for k, (y, folder) in enumerate(links):
        fy = by_id(read_file(f'{folder}/file_{y}.parquet', ['record_id'] + list(cols)))
        fy = fy.loc[is_real(fy.index.to_numpy(np.int64))]
        ids = fy.index.to_numpy(np.int64)
        if k == 0:
            rr = (ids - G.REAL_ID) % G.REAL_COPY_BLOCK
            assert np.isin(rr, x15.index).all(), 'a PUF record copy whose RECID is not in the PUF'
            x0 = x15.loc[rr]
            sa = {}
            sa_path = f'{folder}/taxes/schedule_a_factors.csv'
            if os.path.exists(sa_path):
                s = pd.read_csv(sa_path)
                for _, r in s.iterrows():
                    for c in str(r['input columns']).split(', '):
                        sa[c] = (float(r['multiplier s on the prior']), r['category'])
        else:
            gtab = pd.read_csv(f'{folder}/start_growth.csv').set_index('column')
        for c, b in cols.items():
            if k == 0:
                x = x0[b].to_numpy(float)
                assert np.isfinite(x).all(), f'{b}: a 2015 item missing on a PUF record'
                fp, fn = (1.0, 1.0) if y0 == common.PUF_YEAR else GR.factors(b, y0, y0)
                steps.append({'column': c, 'puf_item': b, 'what': f'growth from 2015 to {y0}', 'positive': fp,
                              'negative': fn})
                v = apply(x, fp, fn)
                if c in sa:
                    steps.append({'column': c, 'puf_item': b, 'what': f'{SCHEDULE_A_STEP} {y0} ({sa[c][1]})',
                                  'positive': sa[c][0], 'negative': sa[c][0]})
                    v = v * sa[c][0]
                made = (x == 0) & (fy[c].to_numpy(float) != 0)
                assert not made.any(), f'{c}: {int(made.sum())} PUF record copies with a 2015 amount of zero are not zero'
            else:
                g = GROWTH_COLUMN.get(c, c)
                assert g in gtab.index, f'{c}: no growth factor from {links[k - 1][0]} to {y} in {folder}/start_growth.csv'
                fp, fn = float(gtab.loc[g, 'factor, positive values']), float(gtab.loc[g, 'factor, negative values'])
                steps.append({'column': c, 'puf_item': b, 'what': f'growth from {links[k - 1][0]} to {y}',
                              'positive': fp, 'negative': fn})
                assert prev[c].index.is_unique and np.isin(ids, prev[c].index).all(), f'{y}: a PUF record copy not in {links[k - 1][0]}'
                v = apply(prev[c].loc[ids].to_numpy(float), fp, fn)
            got = fy[c].to_numpy(float)
            assert np.isfinite(got).all(), f'{c}: missing on a PUF record copy in {y}'
            bad = v != got
            if c == 'state_ref':            # the tax benefit rule removes refunds: those records are zero in the file
                removed = bad & (got == 0)
                bad = bad & ~removed
                checks.append({'year': y, 'column': c, 'records checked': len(v), 'records that differ': int(bad.sum()),
                               'refunds removed by the tax benefit rule': int(removed.sum())})
            else:
                checks.append({'year': y, 'column': c, 'records checked': len(v), 'records that differ': int(bad.sum())})
            prev[c] = pd.Series(got, index=ids)
        del fy
    checks = pd.DataFrame(checks)
    bad = checks[checks['records that differ'] > 0]
    checks.to_csv(f'{SUMMARY}/factor_checks_{common.RUN}.csv', index=False)
    assert bad.empty, ('PUF items that are not their 2015 amount times the factors (counts only):\n'
                       + bad.to_string(index=False))
    fac = pd.DataFrame(steps)
    fac.insert(2, 'step', fac.groupby('column').cumcount() + 1)
    say(f'factors: {len(cols)} PUF items, {fac.step.max()} steps at most; every PUF record copy of every year of the '
        f'chain is its 2015 amount times the factors exactly (factor_checks_{common.RUN}.csv)')

    # ---- the record copies: weight factor, refund flag, categories ------------------------------------------------
    xr = x15.loc[recid]
    w15 = raw.set_index('RECID')['S006'].loc[recid].to_numpy(float) / 100
    wf, n_inexact = exact_factor(w15, Fr['weight'].to_numpy(float))
    ref15, refY = xr['state_ref'].to_numpy(float), Fr['state_ref'].to_numpy(float)
    kept = np.where((ref15 != 0) & (refY == 0), 0, 1).astype(np.int8)
    agg = recid >= 999996
    rec = pd.DataFrame({'RECID': recid, 'copy': copy, 'row': np.flatnonzero(real), 'weight_factor': wf,
                        'medical_floor': floor_by.loc[recid].to_numpy(), 'state_ref_kept': kept})
    cats = ['filing_status', 'married', 'n_dep', 'n_dep_ctc', 'n_dep_eitc']
    pc = MG.puf_categories(raw).loc[recid].reset_index(drop=True)
    for c in cats:
        diff = (Fr[c].to_numpy() != pc[c].to_numpy()) & ~agg
        assert not diff.any(), f'{c}: the file differs from the PUF on {int(diff.sum())} ordinary record copies'
        rec[c] = np.where(agg, Fr[c].to_numpy(), -1).astype(np.int64)

    # ---- the Tax-Simulator columns: the merge's own, a formula, a constant, or shipped ----------------------------
    derived = ['capital_gains', 'capital_gains_line13', *D_meta['computed_totals'], 'k1_owner', 'gross_pens_dist',
               'ss_taxable', 'other_agi', *ratio_items_made, *D.REMAINDERS, *[i for i, *_ in D.PERSON_CAPS.values()],
               *D.SUMS, *D.FLAGS_WITH, 'elective1', 'elective2', 'trad_contr_er1', 'trad_contr_er2', 'trad_deferrals',
               'medicare_wages']
    made = set(cols) | set(derived) | set(cats) | {'state_ref', 'amt_nols', 'record_id', 'weight'} | set(PUF_BASED)
    contract = {n: (st, src) for n, st, src, _ in C.CONTRACT}

    def source(c):
        st, src = contract.get(c, (None, None))
        if c in ('ot', 'ot1', 'ot2'):
            return c
        if st in (C.SAME, C.RENAMED) and c not in C.NO_AGE_COLUMNS:
            return src.split(' ')[0].rstrip(';')
        if (st == C.ZERO and c in F) or c in TS.BASIS_FROM_FILE or (c in TS.EXTRA and TS.EXTRA[c][0] == 'same'):
            return c
        return None
    Tr = T.loc[real].reset_index(drop=True)
    rules, shipped = {}, []
    for c in TS.PRODUCTION_COLUMNS:
        if c in FORMULAS:
            rules[c] = ('formula', None)
            continue
        s = source(c)
        tv = Tr[c]
        if s is not None and s in Fr and pd.api.types.is_numeric_dtype(tv):
            a, b = tv.to_numpy(float), Fr[s].to_numpy(float)
            if ((a == b) | (np.isnan(a) & np.isnan(b))).all():
                rules[c] = ('file', s)
                continue
        vals = tv.dropna().unique()
        if len(vals) == 0 or (len(vals) == 1 and not tv.isna().any()):
            v = None if len(vals) == 0 else vals[0]
            rules[c] = ('constant', v.item() if hasattr(v, 'item') else v)
            continue
        assert c in SHIPPED_OK, (f'Tax-Simulator column {c} is not one of the merge\'s columns, a formula or a constant, '
                                 'and is not on the list that may be shipped as values')
        rules[c] = ('shipped', None)
        shipped.append(c)
    need = set(MG.derive_inputs(spec)) | {s for how, s in rules.values() if how == 'file'} | set(FORMULA_INPUTS)
    need |= {'farm_rent', 'sche_other', 'kg_dist', 'alimony', 'alimony_exp', 'txbl_pens_dist', 'nontxbl_pens_dist',
             'other_agi_remainder', 'sole_prop', 'farm', 'part_se'}
    need -= made
    constants, imputed = {}, []
    for c in sorted(need, key=list(Fr.columns).index):
        assert c in Fr, f'{c}: needed by the merge, not in the file'
        u = Fr[c].unique()
        if len(u) == 1 and not pd.isna(u[0]):
            constants[c] = u[0].item() if hasattr(u[0], 'item') else u[0]
        else:
            imputed.append(c)
    for c in imputed:
        rec[c] = Fr[c].to_numpy()
    puf_based(Fr, rec, MG)
    for c in shipped:
        rec['ts.' + c] = Tr[c].to_numpy()
    extra = T.loc[~real].copy()
    extra.insert(0, 'row', np.flatnonzero(~real))
    spec.update({'factor_columns': list(cols), 'imputed': imputed, 'constants': constants, 'puf_based': list(PUF_BASED),
                 'taxsim': {'columns': list(TS.PRODUCTION_COLUMNS), 'rules': rules, 'records': len(T),
                            'flags': TS.FLAGS, 'integers': TS.INTEGERS,
                            'integers_with_missing': ['age2', 'male2', 'dep_age1', 'dep_age2', 'dep_age3', 'divorce_year']}})
    how = pd.Series([h for h, _ in rules.values()]).value_counts().to_dict()
    say(f'Tax-Simulator columns: {how}; shipped as values: {shipped}; imputed items in records.parquet: {len(imputed)}; '
        f'constants: {constants}')

    # ---- write -----------------------------------------------------------------------------------------------------
    if os.path.exists(PKG):
        shutil.rmtree(PKG)
    os.makedirs(PKG)
    os.makedirs(CHECK, exist_ok=True)
    chmod_private(PKG)
    chmod_private(CHECK)
    fac.to_parquet(f'{PKG}/factors.parquet', index=False)
    fac.to_csv(f'{PKG}/factors.csv', index=False)
    rec.to_parquet(f'{PKG}/records.parquet', index=False)
    extra.to_parquet(f'{PKG}/extra_records.parquet', index=False)
    json.dump(spec, open(f'{PKG}/spec.json', 'w'), indent=1, default=str)
    for f in ('merge_puf.py', 'merge_puf.R'):
        shutil.copy(os.path.join(HERE, f), os.path.join(PKG, f))
    fac.to_csv(f'{SUMMARY}/factors_{common.RUN}.csv', index=False)   # the factors: no record values

    # ---- the round trip: the package's own Python script on our PUF ---------------------------------------------
    sys.path.insert(0, PKG)
    import importlib
    MGp = importlib.import_module('merge_puf')
    assert os.path.dirname(os.path.abspath(MGp.__file__)) in (os.path.abspath(PKG), HERE)
    out, d = MGp.merge(common.RAW['puf'], PKG, file_frame=True)
    rt = compare(out, T, TS.PRODUCTION_COLUMNS)
    rt.to_csv(f'{SUMMARY}/roundtrip_{common.RUN}_python.csv', index=False)
    file_cols = [c for c in list(cols) + derived + ['state_ref', 'amt_nols'] if c in Fr and c in d]
    keep = d['weight'].to_numpy() > 0
    ff = compare(d.loc[keep].reset_index(drop=True), Fr, list(dict.fromkeys(file_cols)))
    ff.to_csv(f'{SUMMARY}/roundtrip_{common.RUN}_python_file_columns.csv', index=False)
    removed = int((kept == 0).sum())
    capped = int((Fr['amt_nols'].to_numpy(float) < Fr['amt_nols_drawn'].to_numpy(float)).sum())
    summary = {'year': Y, 'chain': spec['chain'], 'records': len(T), 'PUF record copies': int(real.sum()),
               'other records': int((~real).sum()), 'factor rows': len(fac), 'weight factors not exact': n_inexact,
               'refunds removed (record copies)': removed, 'AMT adjustments held to the loss (record copies)': capped,
               'Tax-Simulator columns by rule': how, 'shipped': shipped, 'imputed items': len(imputed)}
    json.dump(summary, open(f'{SUMMARY}/summary_{common.RUN}.json', 'w'), indent=1, default=str)
    write_readme(spec, summary, rt)
    chmod_private(PKG)
    report('Python', rt)


def report(name, rt):
    bad = rt[~rt['passes']]
    diff = rt[rt['records that differ'] > 0]
    say(f'round trip ({name} merge script on our 2015 PUF) against the Tax-Simulator file: {len(rt)} columns, '
        f'{len(rt) - len(diff)} identical on every record, {len(diff)} with rounding differences only'
        if bad.empty else f'round trip ({name}): {len(bad)} columns FAIL')
    if len(diff):
        say(diff.to_string(index=False))
    assert bad.empty, f'the {name} merge does not reproduce the Tax-Simulator file:\n{bad.to_string(index=False)}'


def check_r():
    assert common.REAL_PUF, 'package: settings generation.filers is synthetic'
    import taxsim_file as TS
    path = f'{CHECK}/merged_by_r.parquet'
    m = pd.read_parquet(path)
    T = pd.read_parquet(f'{TAXSIM}/tax_units_{Y}.parquet')
    assert len(m) == len(T) and np.array_equal(m['id'].to_numpy(np.int64), T['id'].to_numpy(np.int64)), \
        'the R merge gives other records, or another order'
    rt = compare(m, T, TS.PRODUCTION_COLUMNS)
    rt.to_csv(f'{SUMMARY}/roundtrip_{common.RUN}_r.csv', index=False)
    os.remove(path)
    readme = f'{PKG}/README.md'
    diff = rt[rt['records that differ'] > 0]
    text = open(readme).read().replace('R_ROUNDTRIP', roundtrip_sentence(rt))
    open(readme, 'w').write(text)
    chmod_private(PKG)
    report('R', rt)


def roundtrip_sentence(rt):
    diff = rt[rt['records that differ'] > 0]
    if diff.empty:
        return f'all {len(rt)} columns identical on every record.'
    worst = diff.sort_values('largest absolute difference').iloc[-1]
    unit = '' if worst['column'] == 'weight' else '$'
    return (f'{len(rt) - len(diff)} of {len(rt)} columns identical on every record; {len(diff)} '
            f'{"differs" if len(diff) == 1 else "differ"} by rounding only ({", ".join(diff["column"])}, on at most '
            f'{int(diff["records that differ"].max()):,} records; the largest difference is '
            f'{unit}{worst["largest absolute difference"]:.2g}, in {worst["column"]}, '
            f'{worst["largest difference in last-digit units"]:.0f} in the last binary digit).')


def write_readme(spec, summary, rt):
    y, y0 = spec['year'], spec['drawn_in']
    steps = ' then '.join(['the growth from 2015 to ' + str(y0)]
                          + [f'the growth from {a} to {b}' for a, b in zip(spec['chain'][:-1], spec['chain'][1:])])
    shipped = ', '.join(summary['shipped'])
    text = f"""# The Budget Lab's {y} Tax-Simulator file, as a merge onto the 2015 IRS Public Use File

This package turns your own copy of the 2015 IRS Public Use File (PUF) into the Budget Lab's {y} tax file, the file
our tax model (Tax-Simulator) reads, `tax_units_{y}.csv`. It contains no PUF value. It holds the numbers that carry
each PUF record to {y}, the items we added to each record, and the records that are not PUF records.
Written by the pipeline's package stage (`package.py`, branch `rebuild-realpuf`); what it is for is in `PACKAGE.md`
of that branch.

## How the file was made

1. Every 2015 PUF record keeps all its own PUF items. Each of our three trained networks drew, for every record and
   given all its PUF items, the items the PUF lacks: exact ages, the household balance sheet (from the Survey of
   Consumer Finances), consumption (from the Consumer Expenditure Survey), and person items from the Current
   Population Survey and the Survey of Income and Program Participation. So each PUF record appears three times
   (column `copy`, 1 to 3), at a third of its weight. The records were drawn for {y0}.
2. The PUF's four aggregate records (RECID 999996 to 999999) are records like the others. Their filing status,
   dependents and age range are blank on the PUF, so the networks drew them; their blank alimony fields are read as
   zero.
3. Non-filers and the Forbes list of US billionaires are added as records of their own.
4. Every dollar amount was grown to {y} ({steps}), and the weights were adjusted to the year's published totals.

## What is in the package

| File | What it holds |
|---|---|
| `factors.parquet` (`factors.csv` the same, to read) | one row per PUF item and step: the factor for positive amounts and the factor for negative amounts. The same for every record. Steps: {steps}; for six itemized deductions (medical expenses, state and local income or sales tax, real estate tax, mortgage interest, cash and other gifts) one more step, a multiplier for {y0} that brings the average deduction per itemizer to the IRS's |
| `records.parquet` | one row per PUF record copy in the file ({summary['PUF record copies']:,}): `RECID`, `copy`; `row`, its place in the file; `weight_factor`, the weight over the PUF weight (S006 / 100); `medical_floor`, the share of AGI (0.075 or 0.1) used for the 2015 value of other itemized deductions; `state_ref_kept`, 0 where the taxable state refund is removed (below), else 1; the four aggregate records' drawn filing status and dependents (-1 on other records); the items we added, as values ({summary['imputed items']} columns); and these Tax-Simulator columns as values, columns `ts.<name>`: {shipped} |
| `extra_records.parquet` | the {summary['other records']:,} records that are not PUF records, every Tax-Simulator column |
| `spec.json` | the definitions the scripts use: how each 2015 item is made from PUF fields, how the computed columns are made, and where each Tax-Simulator column comes from |
| `merge_puf.py`, `merge_puf.R` | the merge, in Python and in R; the two do the same steps |

## How to run it

    python merge_puf.py puf_2015.csv <this folder> tax_units_{y}.csv
    Rscript merge_puf.R puf_2015.csv <this folder> tax_units_{y}.csv

(an output name ending in .parquet writes Parquet; no extension writes both). Python needs numpy, pandas and pyarrow;
R needs data.table, jsonlite and arrow. About 16 GB of memory and a few minutes. The steps:

1. Each record's 2015 items from the PUF fields, by the Budget Lab's definitions (for example, `kg_st` is P22250
   adjusted so that line 13 equals the allowed Schedule D net).
2. Each item times its factors, step by step; the weight is S006 / 100 times the weight factor.
3. The items we added, joined on RECID and copy.
4. The computed columns: capital gains after the $3,000 loss limit, business income and its schedules, dividends,
   the adjustments, other income in AGI, taxable Social Security (the section 86 worksheet), gross pensions, each
   spouse's wages, tips, overtime pay and elective deferrals (held to the {y} legal limits and to each person's
   wages), Medicare wages.
5. Two rules that change a PUF item by more than its growth. The taxable state refund is removed where
   `state_ref_kept` is 0 (a refund is taxable only to a return that itemized; we compute who itemizes, and this flag
   is that result); other income and taxable Social Security are then computed again. The AMT net operating loss
   adjustment (P60100, grown) is held to the record's net operating loss, the negative part of its other income.
   {summary['refunds removed (record copies)']:,} record copies have the refund removed; on
   {summary['AMT adjustments held to the loss (record copies)']:,} the AMT adjustment is held to the loss.
6. The Tax-Simulator columns, then the other records, in the file's order.

Every dollar amount in the output is in nominal {y} dollars.

## Test

Applied to our copy of the 2015 PUF: the Python script, {roundtrip_sentence(rt)} The R script, R_ROUNDTRIP
Rounding differences come from adding the same numbers in a different order (for example, the PUF weight times the
weight factor is not always exactly our weight in the last binary digit); a different numerical library on your
machine can give more of them, of the same size.

## Confidentiality

No PUF amount is in the package for any PUF record. Three items we add are a PUF amount times or plus something
else, so the merge computes them from your PUF and the package holds only the other part: the basis of long-term
gains (the gain times a ratio set by the drawn holding period; the package holds the ratio), the farm's wage bill for
the qualified business income deduction (the farm income), and the S corporation's wage bill (the outside workers'
pay, plus the record's wages for an owner with no Schedule C or partnership income; the package holds the pay and a
0/1 flag). Parts of the package still depend on PUF values: the refund flag (whether the record itemizes in our
model), whether a record has a long-term gain (its holding period is missing where it has none), and items computed
from the record's income rank (death probabilities, parents' income rank). The non-filers' tax items are amounts
picked from PUF records with similar items, grown, as in every file the pipeline makes. Treat the package as
PUF-derived data under your PUF license.
"""
    open(f'{PKG}/README.md', 'w').write(text)


if __name__ == '__main__':
    cmd = sys.argv[1] if len(sys.argv) > 1 else 'build'
    if cmd == 'paths':
        print(PKG, common.RAW['puf'], CHECK)
    elif cmd == 'build':
        build()
    elif cmd == 'check_r':
        check_r()
    else:
        sys.exit(__doc__)
