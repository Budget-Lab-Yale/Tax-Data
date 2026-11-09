"""The package for a holder of the 2015 PUF. It lets someone with their own copy of the 2015 PUF (CRS) rebuild the
year's Tax-Simulator file without receiving any PUF value. This module builds the package and checks that it works.

The package is made only for a file built on the real PUF (settings generation.filers real_puf). In such a file, every
PUF record appears once per trained network ("record copies"), with its PUF items grown to the target year and the
items the networks added. The package holds: the growth factors of each PUF item, the same for every record; one row
per record copy with its weight factor and added items; the records that are not PUF records; and two merge scripts,
in Python and R, that put these together with the PUF. Step `build` writes the package to <out>/package/<run>/ and
runs the Python merge script on our own PUF (the "round trip"); step `check_r` checks the R merge script's result,
which run.py produces. Every merged column must equal the Tax-Simulator file on every record, to rounding. See
PACKAGE.md.
"""
import json
import os
import shutil
import subprocess
import sys

import numpy as np
import pandas as pd

from shared import common

HERE = os.path.dirname(os.path.abspath(__file__))
Y = common.TARGET_YEAR
PKG = f'{common.OUT}/package/{common.RUN}'
CHECK = PKG.rstrip('/') + '_check'
SUMMARY = os.path.dirname(PKG.rstrip('/'))
TAXSIM = f'{common.OUT}/taxsim/{common.RUN}'
say = common.say

# FORMULAS: the Tax-Simulator columns the merge computes from the record's items. SHIPPED_OK: columns that may be shipped
# as values, because they depend only on imputed items. Any other column that the merge cannot make stops the stage.
FORMULAS = ['id', 'weight', 'sole_prop1', 'sole_prop2', 'farm1', 'farm2', 'part_se1', 'part_se2', 'other_inc', 'kg_lt',
            'gross_pens_dist', 'divorce_year', 'kg_lt_years_held', 'kg_lt_basis']
FORMULA_INPUTS = ['spouse_se_share', 'kg_lt_years_held', 'kg_lt_basis', 'record_id']
SHIPPED_OK = {'age1', 'male1', 'male2', 'age2', 'blind2', 'q_death2', 'dep_status', 'prim_mort_share'} | \
    {f'dep_{p}{k}' for p in ('age', 'ssn', 'ctc') for k in (1, 2, 3)}
SCHEDULE_A_STEP = 'Schedule A multiplier'
# Imputed items built from a PUF amount. Shipped as values they would reveal the grown PUF amount, so the merge computes
# them from the PUF items and the package ships only the other part (the columns listed): kg_lt_basis is the long-term
# gain times r / (1 - r), with r the basis-to-sales ratio of the drawn holding period; wagebill_farm is farm income,
# at least 0.
PUF_BASED = {'kg_lt_basis': ['kg_lt_basis_ratio'], 'wagebill_farm': []}
TOL_ABS, TOL_REL = 1e-6, 1e-12    # the largest absolute and relative difference a round trip may show
# PUF items not shipped: the file holds the calculator's value, no Tax-Simulator column holds it, and the recipient's
# calculator computes it from the other items. Its factor rows are the plain growth and are not checked.
NOT_SHIPPED = {'se_tax_ded': 'the deductible half of self-employment tax'}
REPEAL_STEP = 'repealed from'


# ---- Small helpers ----

def repealed():
    """{item: first tax year without it}, for the repealed adjustments the pipeline sets to zero."""
    rep = common.SETTINGS.get('taxes', {}).get('repealed_adjustments', {})
    return {k: int(v) for k, v in rep.items() if not k.startswith('_')}


def chmod_private(path):
    """The folder and everything in it readable by the owner only."""
    for root, dirs, files in os.walk(path):
        os.chmod(root, 0o700)
        for f in files:
            os.chmod(os.path.join(root, f), 0o600)


def chain():
    """[(year, its tilt folder)] from the year the records were drawn (a generated start) to the target year."""
    from reweight import target_list as TLS
    years, y = [(Y, common.TILT)], Y
    while True:
        spec = TLS.start_spec(y)
        if spec['mode'] == 'generate':
            return years
        y = spec['year']
        years.insert(0, (y, f"{common.OUT}/tilt/{spec['run']}"))


def factor_columns():
    """{file column: 2015 item}: the PUF items carried by growth factors. Totals the file computes from their parts are
    left out. The drawn business income, other income's remainder and the AMT adjustment before its cap carry their
    item's factors."""
    from generate import generate as G
    totals = ('capital_gains', 'dividends', 'adjustments', 'business_income', 'other_agi', 'amt_nols')
    cols = {v: v for v in G.own_items() if v not in totals}
    cols.update({'business_income_drawn': 'business_income', 'other_agi_remainder': 'other_agi',
                 'amt_nols_drawn': 'amt_nols'})
    return cols


GROWTH_COLUMN = {'amt_nols_drawn': 'amt_nols'}   # {file column: the column whose growth the projection steps apply to it}


def apply(v, p, n):
    """v with its positive values times p and its negative values times n."""
    v = np.asarray(v, float)
    return np.where(v > 0, v * p, np.where(v < 0, v * n, v))


def exact_factor(x, y):
    """The factor y / x, moved by one in its last binary digit where needed so that x times the factor is exactly y;
    and the number of records where it still is not."""
    q = y / x
    for _ in range(3):
        bad = x * q != y
        if not bad.any():
            break
        up, dn = np.nextafter(q, np.inf), np.nextafter(q, -np.inf)
        q = np.where(bad & (x * up == y), up, np.where(bad & (x * dn == y), dn, q))
    return q, int((x * q != y).sum())


def nearest_exact(x, y, f):
    """The factor as the pipeline applied it: f, or the value within four last binary digits of f that gives y = x * f
    on the most records (a factor read back from a text file can be one digit off). Used only for the steps after the
    first, whose factor is a single multiplication; the check that follows must then find every record exact."""
    if not len(x):
        return f
    cands, g = [f], f
    for _ in range(4):
        g = np.nextafter(g, np.inf); cands.append(g)
    g = f
    for _ in range(4):
        g = np.nextafter(g, -np.inf); cands.append(g)
    hits = [int((x * c == y).sum()) for c in cands]
    return float(cands[int(np.argmax(hits))])


def medical_floor(recid):
    """The medical expense floor (a share of AGI) the training table used for other itemized deductions: 7.5% for the
    65+ age range (or no range and a chance of 65+ over one half), 10% otherwise and on the aggregate records."""
    t = pd.read_parquet(common.TABLE, columns=['source', 'record_id', 'age_range', 'age_p5'])
    t = t[(t.source == 'puf').to_numpy()].set_index('record_id')
    old = ((t['age_range'] == 5) | (t['age_p5'] > 0.5)).reindex(np.asarray(recid, float)).to_numpy()
    agg = np.asarray(recid) >= 999996
    assert not pd.isna(old[~agg]).any() and pd.isna(old[agg]).all(), 'a PUF record not in the table'
    return np.where(agg, 0.1, np.where(old == True, 0.075, 0.1))  # noqa: E712


def compare(m, t, cols):
    """One row per column of m against t: the records that differ, the largest absolute and relative difference, the
    largest in units of the last binary digit, the values missing on one side only, and whether the column passes
    (identical, or rounding only)."""
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
    """Put into rec the parts of the PUF_BASED items that are not PUF amounts. Check that the merge's own function
    gives the file's item exactly from them."""
    from scale import non_micro_imputations as NMI
    M = NMI.holding_model(Y)
    kg, h = Fr['kg_lt'].to_numpy(float), Fr['kg_lt_years_held'].to_numpy(float)
    r = np.zeros(len(Fr))
    r[kg > 0] = M['gain_ratio'](h[kg > 0])
    r[kg < 0] = M['loss_ratio'](h[kg < 0])
    rec['kg_lt_basis_ratio'] = r
    d = {'kg_lt': kg, 'farm': Fr['farm'].to_numpy(float)}
    MG.puf_based(d, rec, {'puf_based': list(PUF_BASED)})
    for c in PUF_BASED:
        bad = d[c] != Fr[c].to_numpy(float)
        assert not bad.any(), f'{c}: the merge\'s rule differs from the file on {int(bad.sum())} PUF record copies'
    say(f'computed by the merge from the PUF items, not shipped: {list(PUF_BASED)} (each checked exactly)')


def read_file(path, columns=None):
    """A year file's columns (by default every column but the bands, the u_ columns and the calculator's)."""
    import pyarrow.parquet as pq
    names = pq.read_schema(path).names
    cols = columns or [c for c in names if not (c.endswith('_band') or c.startswith(('u_', 'calc_')))]
    return pd.read_parquet(path, columns=cols)


# ---- Reading the files ----

def is_puf_copy(i):
    """Whether each record id is a copy of a PUF record. The copies' ids run from G.REAL_ID, one block of
    G.REAL_COPY_BLOCK ids per network; the Forbes list's ids start above them, from 95e6."""
    from generate import generate as G
    n_copies = len(G.MODELS)
    return (i >= G.REAL_ID) & (i < G.REAL_ID + n_copies * G.REAL_COPY_BLOCK)


def recid_of(i):
    """A PUF record copy's PUF RECID, from its record id."""
    from generate import generate as G
    return (i - G.REAL_ID) % G.REAL_COPY_BLOCK


def read_year():
    """The Tax-Simulator file T, the year file's live records F, and of these the PUF record copies Fr, with which
    rows of F they are (real), which are the PUF's aggregate records (agg), their PUF RECIDs and their copy numbers."""
    from generate import generate as G
    T = pd.read_parquet(f'{TAXSIM}/tax_units_{Y}.parquet')
    F = read_file(f'{common.TILT}/file_{Y}.parquet')
    live = F['weight'].to_numpy(float) > 0
    F = F.loc[live].reset_index(drop=True)
    assert np.array_equal(F['record_id'].to_numpy(np.int64), T['id'].to_numpy(np.int64)), \
        'the year file\'s live records and the Tax-Simulator file differ'
    rid = F['record_id'].to_numpy(np.int64)
    real = is_puf_copy(rid)
    agg = recid_of(rid[real]) >= 999996
    Fr = F.loc[real].reset_index(drop=True)
    recid = recid_of(rid[real])
    copy = (rid[real] - G.REAL_ID) // G.REAL_COPY_BLOCK + 1
    say(f'{len(F):,} live records: {int(real.sum()):,} PUF record copies ({len(np.unique(recid)):,} PUF records), '
        f'{int((~real).sum()):,} other records; {int((~live).sum()):,} records of weight 0 left out, as the '
        f'Tax-Simulator file leaves them out')
    return T, F, Fr, real, agg, recid, copy


# ---- The definitions and the 2015 items ----

def package_spec(links, y0):
    """The definitions the merge scripts use (spec.json): which PUF fields they read, how each 2015 item and each
    computed column is made, and the record id scheme. Also the items made from a ratio."""
    from prep import build_table as BT
    from shared import dollars as D
    from generate import generate as G
    from shared import translate as TR
    lim = common.SETTINGS['deferrals']['limits'][str(Y)]
    ratio_items_made = [item for slot, (item, base) in D.RATIOS.items()
                        if item not in ('value.primary_mortgage', 'w_dc_stocks')]   # those two are balance sheet values
    spec = {
        'year': Y, 'drawn_in': y0, 'chain': [y for y, _ in links], 'real_id_base': G.REAL_ID,
        'copy_block': G.REAL_COPY_BLOCK, 'copies': len(G.MODELS),
        'raw_fields': list(dict.fromkeys(BT.PUF_FIELDS)),
        'items': {'parts': BT.PARTS, 'sche_parts': BT.SCHE_PARTS, 'adj_parts': BT.ADJ_PARTS, 'adj_fields': BT.ADJ_FIELDS,
                  'new_items': BT.NEW_ITEMS, 'agi_item_signs': {v: s for v, (s, _, _) in BT.ITEMS.items() if s != 0},
                  'suppressed_on_aggregate_records': ['E00800', 'E03500']},
        'derive': {'computed_totals': D.META['computed_totals'], 'k1_parts': BT.K1_PARTS, 'agi_fold': D.FOLD,
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
    return spec, ratio_items_made


def puf_items_2015(spec, recid):
    """Every PUF record's raw fields, the medical expense floor each record used, and its 2015 items made by the merge
    script's own function (merge_puf.items_2015)."""
    from output import merge_puf as MG
    raw = pd.read_csv(common.RAW['puf'], usecols=lambda c: c in set(spec['raw_fields']))   # every PUF record
    assert raw['RECID'].is_unique and np.isin(np.unique(recid), raw['RECID']).all(), 'a PUF record of the file is not in the raw PUF'
    floor_by = pd.Series(medical_floor(raw['RECID'].to_numpy()), index=raw['RECID'].to_numpy())
    x15 = MG.items_2015(raw, spec, floor_by.loc[raw['RECID']].to_numpy())
    return raw, floor_by, x15


# ---- The growth factors ----

def factor_steps(links, cols, x15, y0):
    """The growth factors, step by step along the chain of years, each step checked against that year's file. Every
    PUF record copy's item must be its 2015 amount times the factors exactly, except the state refunds that the rules
    cut and the items not shipped. Writes factor_checks_<run>.csv. Returns the factors (one row per item and step),
    the repealed adjustments, and the ones a step set to zero."""
    from shared import growth as GR
    rep = repealed()
    zeroed = set()       # repealed items already set to zero by a step
    prev = {}
    steps = []           # rows: column, puf_item, step, what, positive, negative
    checks = []          # rows: step, column, records checked, records that differ
    for k, (y, folder) in enumerate(links):
        fy = read_file(f'{folder}/file_{y}.parquet', ['record_id'] + list(cols)).set_index('record_id')
        fy = fy.loc[is_puf_copy(fy.index.to_numpy(np.int64))]
        ids = fy.index.to_numpy(np.int64)
        if k == 0:
            assert np.isin(recid_of(ids), x15.index).all(), 'a PUF record copy whose RECID is not in the PUF'
            x0 = x15.loc[recid_of(ids)]
            sa = {}
            sa_path = f'{folder}/taxes/schedule_a_factors.csv'
            if os.path.exists(sa_path):
                s = pd.read_csv(sa_path, float_precision='round_trip')   # the C parser's default is not exact
                for _, r in s.iterrows():
                    for c in str(r['input columns']).split(', '):
                        sa[c] = (float(r['multiplier s on the prior']), r['category'])
        else:
            gtab = pd.read_csv(f'{folder}/start_growth.csv', float_precision='round_trip').set_index('column')
        for c, b in cols.items():
            if k == 0:
                # The first step: the growth from 2015 to the year the records were drawn, then that year's Schedule A
                # multiplier where the item has one.
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
                made = (x == 0) & (fy[c].to_numpy(float) != 0) & (c not in NOT_SHIPPED)
                assert not made.any(), f'{c}: {int(made.sum())} PUF record copies with a 2015 amount of zero are not zero'
            else:
                # A projection step: the growth from the previous year of the chain, as the projection applied it.
                g = GROWTH_COLUMN.get(c, c)
                assert g in gtab.index, f'{c}: no growth factor from {links[k - 1][0]} to {y} in {folder}/start_growth.csv'
                fp, fn = float(gtab.loc[g, 'factor, positive values']), float(gtab.loc[g, 'factor, negative values'])
                steps.append({'column': c, 'puf_item': b, 'what': f'growth from {links[k - 1][0]} to {y}',
                              'positive': fp, 'negative': fn})
                assert prev[c].index.is_unique and np.isin(ids, prev[c].index).all(), f'{y}: a PUF record copy not in {links[k - 1][0]}'
                x = prev[c].loc[ids].to_numpy(float)
                if c not in NOT_SHIPPED:    # the factor exactly as the pipeline applied it
                    fp = nearest_exact(x[x > 0], fy[c].to_numpy(float)[x > 0], fp)
                    fn = nearest_exact(x[x < 0], fy[c].to_numpy(float)[x < 0], fn)
                    steps[-1].update(positive=fp, negative=fn)
                v = apply(x, fp, fn)
            # A repealed adjustment: one step with a factor of zero from the repeal year.
            if c in rep and c not in zeroed and int(y) >= rep[c]:
                steps.append({'column': c, 'puf_item': b, 'what': f'{REPEAL_STEP} {rep[c]}', 'positive': 0.0,
                              'negative': 0.0})
                v = apply(v, 0.0, 0.0) + 0.0     # + 0.0: no negative zero
                zeroed.add(c)
            got = fy[c].to_numpy(float)
            assert np.isfinite(got).all(), f'{c}: missing on a PUF record copy in {y}'
            bad = v != got
            row = {'year': y, 'column': c, 'records checked': len(v)}
            if c == 'state_ref':
                # The refund rule and the tax benefit rule remove a refund or keep part of it, so the file's refund
                # lies between 0 and the grown amount; records.parquet carries the share kept.
                cut = bad & (got >= 0) & (got < v)
                bad = bad & ~cut
                row.update({'refunds removed by the rules': int((cut & (got == 0)).sum()),
                            'refunds kept in part by the tax benefit rule': int((cut & (got > 0)).sum())})
            if c in NOT_SHIPPED:
                # The file holds the calculator's value: counted, not checked.
                row.update({'not shipped: record copies that differ from the grown PUF amount': int(bad.sum())})
                bad = np.zeros(len(v), bool)
            row['records that differ'] = int(bad.sum())
            checks.append(row)
            prev[c] = pd.Series(v if c in NOT_SHIPPED else got, index=ids)
        del fy
    checks = pd.DataFrame(checks)
    bad = checks[checks['records that differ'] > 0]
    checks.to_csv(f'{SUMMARY}/factor_checks_{common.RUN}.csv', index=False)
    assert bad.empty, ('PUF items that are not their 2015 amount times the factors (counts only):\n'
                       + bad.to_string(index=False))
    fac = pd.DataFrame(steps)
    fac.insert(2, 'step', fac.groupby('column').cumcount() + 1)
    say(f'factors: {len(cols)} PUF items, {fac.step.max()} steps at most; every PUF record copy of every year of the '
        f'chain is its 2015 amount times the factors exactly, except the refunds the rules cut (a multiplier per '
        f'record copy, below) and the items not shipped ({list(NOT_SHIPPED)}) (factor_checks_{common.RUN}.csv)')
    return fac, rep, zeroed


# ---- The record copies ----

def record_copies(raw, x15, recid, copy, real, agg, Fr, fac, floor_by):
    """One row per PUF record copy (records.parquet): its RECID, copy and row in the file; its weight factor (the file's
    weight over the PUF weight); its medical expense floor; its state refund multiplier; and, on the aggregate records
    only, the drawn filing status and dependents. Also which grown refunds are positive, the multipliers, and the
    counts of weight factors and multipliers that are not exact."""
    from output import merge_puf as MG
    xr = x15.loc[recid]
    w15 = raw.set_index('RECID')['S006'].loc[recid].to_numpy(float) / 100
    wf, n_inexact = exact_factor(w15, Fr['weight'].to_numpy(float))
    # The taxable state refund has one multiplier per record copy between 0 and 1: the file's refund over the grown 2015
    # refund, adjusted in the last binary digit where needed so that the merge's product is exactly the file's.
    refG = MG.grow(xr.reset_index(drop=True)[['state_ref']], fac[fac.column == 'state_ref'])['state_ref']
    refY = Fr['state_ref'].to_numpy(float)
    assert (refG >= 0).all() and (refY >= 0).all() and (refY <= refG * (1 + TOL_REL)).all(), \
        'state_ref: a refund above its grown 2015 amount, or a negative one'
    mult = np.ones(len(refY))
    pos = refG > 0
    mult[pos], n_mult_inexact = exact_factor(refG[pos], refY[pos])
    mult = np.clip(mult, 0.0, 1.0)
    rec = pd.DataFrame({'RECID': recid, 'copy': copy, 'row': np.flatnonzero(real), 'weight_factor': wf,
                        'medical_floor': floor_by.loc[recid].to_numpy(), 'state_ref_multiplier': mult})
    # The categories come from the PUF's fields, except on the aggregate records, whose categories were drawn.
    cats = ['filing_status', 'married', 'n_dep', 'n_dep_ctc', 'n_dep_eitc']
    pc = MG.puf_categories(raw).loc[recid].reset_index(drop=True)
    for c in cats:
        diff = (Fr[c].to_numpy() != pc[c].to_numpy()) & ~agg
        assert not diff.any(), f'{c}: the file differs from the PUF on {int(diff.sum())} ordinary record copies'
        rec[c] = np.where(agg, Fr[c].to_numpy(), -1).astype(np.int64)
    return rec, cats, pos, mult, n_inexact, n_mult_inexact


# ---- The Tax-Simulator columns ----

def taxsim_source(c, contract, F):
    """The year file's column that the Tax-Simulator column c is copied from, if any."""
    from shared import calculator_input as C
    from output import taxsim_file as TS
    st, src = contract.get(c, (None, None))
    if st in (C.SAME, C.RENAMED) and c not in C.NO_AGE_COLUMNS:
        return src.split(' ')[0].rstrip(';')
    if (st == C.ZERO and c in F) or c in TS.BASIS_FROM_FILE or (c in TS.EXTRA and TS.EXTRA[c][0] == 'same'):
        return c
    return None


def taxsim_rules(spec, ratio_items_made, cols, cats, rec, F, Fr, T, real, rep, zeroed):
    """How the merge makes each Tax-Simulator column: as the merge's own column ('file'), by a formula, as a constant,
    or shipped as values in records.parquet. Adds to rec the imputed items the merge needs and the shipped columns,
    and to spec the rules. Returns the computed columns, the other records (extra_records.parquet), the shipped
    columns, the imputed items, and the count of columns by rule."""
    from shared import calculator_input as C
    from shared import dollars as D
    from output import taxsim_file as TS
    from output import merge_puf as MG
    derived = ['capital_gains', 'capital_gains_line13', *D.META['computed_totals'], 'k1_owner', 'gross_pens_dist',
               'ss_taxable', 'other_agi', *ratio_items_made, *D.REMAINDERS, *[i for i, *_ in D.PERSON_CAPS.values()],
               *D.SUMS, *D.FLAGS_WITH, 'elective1', 'elective2', 'trad_contr_er1', 'trad_contr_er2', 'trad_deferrals',
               'medicare_wages']
    made = set(cols) | set(derived) | set(cats) | {'state_ref', 'amt_nols', 'record_id', 'weight'} | set(PUF_BASED)
    contract = {n: (st, src) for n, st, src, _ in C.CONTRACT}
    Tr = T.loc[real].reset_index(drop=True)
    # 1. Each column's rule. A column copied from the file is used only if it equals the Tax-Simulator file exactly.
    rules, shipped = {}, []
    for c in TS.PRODUCTION_COLUMNS:
        if c in FORMULAS:
            rules[c] = ('formula', None)
            continue
        s = taxsim_source(c, contract, F)
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
    # 2. The items the merge needs that it does not make: a constant goes into spec, the rest into records.parquet.
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
    # 3. The records that are not PUF records, with every Tax-Simulator column, and the rules in spec.
    extra = T.loc[~real].copy()
    extra.insert(0, 'row', np.flatnonzero(~real))
    spec.update({'factor_columns': list(cols), 'imputed': imputed, 'constants': constants, 'puf_based': list(PUF_BASED),
                 'state_ref_multiplier': ('records.parquet column state_ref_multiplier: the grown taxable state refund '
                                          'times it is the year\'s refund (0 removed, 1 kept whole, between where the '
                                          'tax benefit rule keeps part); the merge only multiplies'),
                 'repealed': {c: {'from': y, 'step': f'{REPEAL_STEP} {y}'} for c, y in rep.items() if c in zeroed},
                 'not_shipped': {c: (f'{w}: the file\'s value is the tax calculator\'s and no Tax-Simulator column '
                                     'holds it; the recipient\'s calculator computes it from the other items. Its '
                                     'factor rows are the plain growth, so the working frame carries the PUF\'s own '
                                     'amount grown') for c, w in NOT_SHIPPED.items()},
                 'taxsim': {'columns': list(TS.PRODUCTION_COLUMNS), 'rules': rules, 'records': len(T),
                            'flags': TS.FLAGS, 'integers': TS.INTEGERS,
                            'integers_with_missing': ['age2', 'male2', 'dep_age1', 'dep_age2', 'dep_age3', 'divorce_year']}})
    how = pd.Series([h for h, _ in rules.values()]).value_counts().to_dict()
    say(f'Tax-Simulator columns: {how}; shipped as values: {shipped}; imputed items in records.parquet: {len(imputed)}; '
        f'constants: {constants}')
    return derived, extra, shipped, imputed, how


# ---- Writing the package and the Python round trip ----

def write_package(fac, rec, extra, spec):
    """Write the package folder (readable by the owner only), with copies of the two merge scripts, and the factors
    beside it (they hold no record values)."""
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


def python_round_trip(T, Fr, cols, derived):
    """Run the package's own Python merge script on our copy of the PUF. Compare its output with the Tax-Simulator file,
    and its working frame with the year file's columns. Writes both comparisons; returns the first."""
    from output import taxsim_file as TS
    sys.path.insert(0, PKG)
    import importlib
    MGp = importlib.import_module('merge_puf')
    assert os.path.dirname(os.path.abspath(MGp.__file__)) in (os.path.abspath(PKG), HERE)
    out, d = MGp.merge(common.RAW['puf'], PKG, file_frame=True)
    rt = compare(out, T, TS.PRODUCTION_COLUMNS)
    rt.to_csv(f'{SUMMARY}/roundtrip_{common.RUN}_python.csv', index=False)
    file_cols = [c for c in list(cols) + derived + ['state_ref', 'amt_nols'] if c in Fr and c in d and c not in NOT_SHIPPED]
    keep = d['weight'].to_numpy() > 0
    ff = compare(d.loc[keep].reset_index(drop=True), Fr, list(dict.fromkeys(file_cols)))
    ff.to_csv(f'{SUMMARY}/roundtrip_{common.RUN}_python_file_columns.csv', index=False)
    return rt


# ---- The build step ----

def build():
    assert common.REAL_PUF, ('package: settings generation.filers is synthetic; the package is made only for a file '
                             'built on the real PUF (generation.filers real_puf)')

    # 1. Find the chain of years from the year the records were drawn to the target year. Read the Tax-Simulator file
    #    and the year file, and find the PUF record copies.
    os.makedirs(SUMMARY, exist_ok=True)
    links = chain()
    y0 = links[0][0]
    say(f'package for {Y}: the chain of starts {" -> ".join(str(y) for y, _ in links)} (records drawn in {y0})')
    T, F, Fr, real, agg, recid, copy = read_year()

    # 2. Write the definitions (spec), and make each record's 2015 items from the raw PUF with the merge's own function.
    spec, ratio_items_made = package_spec(links, y0)
    raw, floor_by, x15 = puf_items_2015(spec, recid)
    cols = factor_columns()
    missing = [c for c in cols if c not in Fr]
    assert not missing, f'factor columns not in the file: {missing}'

    # 3. The growth factors, step by step, each step checked against the files.
    fac, rep, zeroed = factor_steps(links, cols, x15, y0)

    # 4. The record copies: weight factor, refund multiplier, categories.
    rec, cats, pos, mult, n_inexact, n_mult_inexact = record_copies(raw, x15, recid, copy, real, agg, Fr, fac, floor_by)

    # 5. The Tax-Simulator columns: the merge's own, a formula, a constant, or shipped.
    derived, extra, shipped, imputed, how = taxsim_rules(spec, ratio_items_made, cols, cats, rec, F, Fr, T, real, rep, zeroed)

    # 6. Write the package.
    write_package(fac, rec, extra, spec)

    # 7. The round trip: the package's own Python script on our PUF. Then write the summary and the README, and stop if
    #    the merge does not reproduce the Tax-Simulator file.
    rt = python_round_trip(T, Fr, cols, derived)
    removed = int((pos & (mult == 0)).sum())
    part = int((pos & (mult > 0) & (mult < 1)).sum())
    capped = int((Fr['amt_nols'].to_numpy(float) < Fr['amt_nols_drawn'].to_numpy(float)).sum())
    summary = {'year': Y, 'chain': spec['chain'], 'records': len(T), 'PUF record copies': int(real.sum()),
               'aggregate record copies': int(agg.sum()),
               'other records': int((~real).sum()), 'factor rows': len(fac), 'weight factors not exact': n_inexact,
               'refunds removed (record copies)': removed, 'refunds kept in part (record copies)': part,
               'refund multipliers whose product is not exactly the file\'s refund': n_mult_inexact,
               'repealed adjustments (a factor of zero)': {c: y for c, y in rep.items() if c in zeroed},
               'not shipped': list(NOT_SHIPPED),
               'AMT adjustments held to the loss (record copies)': capped,
               'Tax-Simulator columns by rule': how, 'shipped': shipped, 'imputed items': len(imputed)}
    json.dump(summary, open(f'{SUMMARY}/summary_{common.RUN}.json', 'w'), indent=1, default=str)
    write_readme(spec, summary, rt)
    chmod_private(PKG)
    report('Python', rt)


# ---- Reporting the round trips, the R round trip and the README ----

def report(name, rt):
    """Log a round trip's result, and stop if a column fails."""
    bad = rt[~rt['passes']]
    diff = rt[rt['records that differ'] > 0]
    say(f'round trip ({name} merge script on our 2015 PUF) against the Tax-Simulator file: {len(rt)} columns, '
        f'{len(rt) - len(diff)} identical on every record, {len(diff)} with rounding differences only'
        if bad.empty else f'round trip ({name}): {len(bad)} columns FAIL')
    if len(diff):
        say(diff.to_string(index=False))
    assert bad.empty, f'the {name} merge does not reproduce the Tax-Simulator file:\n{bad.to_string(index=False)}'


def check_r():
    """Compare the R merge script's output (run by run.py) with the Tax-Simulator file, write the comparison, and
    complete the README's sentence on the R round trip."""
    assert common.REAL_PUF, 'package: settings generation.filers is synthetic'
    from output import taxsim_file as TS
    path = f'{CHECK}/merged_by_r.parquet'
    m = pd.read_parquet(path)
    T = pd.read_parquet(f'{TAXSIM}/tax_units_{Y}.parquet')
    assert len(m) == len(T) and np.array_equal(m['id'].to_numpy(np.int64), T['id'].to_numpy(np.int64)), \
        'the R merge gives other records, or another order'
    rt = compare(m, T, TS.PRODUCTION_COLUMNS)
    rt.to_csv(f'{SUMMARY}/roundtrip_{common.RUN}_r.csv', index=False)
    os.remove(path)
    readme = f'{PKG}/README.md'
    text = open(readme).read().replace('R_ROUNDTRIP', roundtrip_sentence(rt))
    open(readme, 'w').write(text)
    chmod_private(PKG)
    report('R', rt)


def roundtrip_sentence(rt):
    """The README's sentence on a round trip's result."""
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
    """Write the package's README.md. The R round trip's sentence is filled in later by check_r."""
    y, y0 = spec['year'], spec['drawn_in']
    steps = ' then '.join(['the growth from 2015 to ' + str(y0)]
                          + [f'the growth from {a} to {b}' for a, b in zip(spec['chain'][:-1], spec['chain'][1:])])
    shipped = ', '.join(summary['shipped'])
    names = {'dpad': 'the domestic production activities deduction', 'tuition_ded': 'the tuition and fees deduction'}
    rp = summary.get('repealed adjustments (a factor of zero)', {})
    repealed_text = '; '.join(f'{names.get(c, c)} from {yy}' for c, yy in rp.items()) or 'none'
    if summary['aggregate record copies']:
        aggregates_text = ("""2. The PUF's four aggregate records (RECID 999996 to 999999) are records like the others. Their filing status,
   dependents and age range are blank on the PUF, so the networks drew them; their blank alimony fields are read as
   zero.""")
    else:
        aggregates_text = ("""2. The PUF's four aggregate records (RECID 999996 to 999999) are not used: the merge ignores them. In their place
   `extra_records.parquet` holds synthetic returns made from our networks' draws, scaled so that each of the four AGI
   groups has its aggregate record's net amount of each item and the counts follow SOI's booklet table.""")
    text = f"""# The Budget Lab's {y} Tax-Simulator file, as a merge onto the 2015 IRS Public Use File

This package turns your own copy of the 2015 IRS Public Use File (PUF) into the Budget Lab's {y} tax file, the file
our tax model (Tax-Simulator) reads, `tax_units_{y}.csv`. It contains no PUF value. It holds the numbers that carry
each PUF record to {y}, the items we added to each record, and the records that are not PUF records.
Written by the pipeline's package stage (`package.py`, branch `rebuild-joint-model`); what it is for is in
`research/rebuild/PACKAGE.md` of that branch.

## How the file was made

1. Every 2015 PUF record keeps all its own PUF items. Each of our three trained networks drew, for every record and
   given all its PUF items, the items the PUF lacks: exact ages, the household balance sheet (from the Survey of
   Consumer Finances), consumption (from the Consumer Expenditure Survey), and person items from the Current
   Population Survey and the Survey of Income and Program Participation. So each PUF record appears three times
   (column `copy`, 1 to 3), at a third of its weight. The records were drawn for {y0}.
{aggregates_text}
3. Non-filers and the Forbes list of US billionaires are added as records of their own.
4. Every dollar amount was grown to {y} ({steps}), and the weights were adjusted to the year's published totals.

## What is in the package

| File | What it holds |
|---|---|
| `factors.parquet` (`factors.csv` the same, to read) | one row per PUF item and step: the factor for positive amounts and the factor for negative amounts. The same for every record. Steps: {steps}; for six itemized deductions (medical expenses, state and local income or sales tax, real estate tax, mortgage interest, cash and other gifts) one more step, a multiplier for {y0} that brings the average deduction per itemizer to the IRS's; for a deduction the law has repealed by {y} ({repealed_text}) one more step, a factor of zero from the repeal year |
| `records.parquet` | one row per PUF record copy in the file ({summary['PUF record copies']:,}): `RECID`, `copy`; `row`, its place in the file; `weight_factor`, the weight over the PUF weight (S006 / 100); `medical_floor`, the share of AGI (0.075 or 0.1) used for the 2015 value of other itemized deductions; `state_ref_multiplier`, the share of the grown taxable state refund the file keeps, between 0 and 1 (below); the four aggregate records' drawn filing status and dependents (-1 on other records); the items we added, as values ({summary['imputed items']} columns); and these Tax-Simulator columns as values, columns `ts.<name>`: {shipped} |
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
5. Two rules that change a PUF item by more than its growth. The taxable state refund, grown, is multiplied by
   `state_ref_multiplier`: 0 where it is removed (a refund is taxable only to a return that itemized, and we compute
   who itemizes), 1 where it is kept whole, and the share kept where the tax benefit rule keeps part of it (a refund
   is taxable only as far as deducting the tax lowered tax); about 15% of the refunds that rule would cut are kept
   whole, as returns report them anyway. The multiplier is that result: the merge only multiplies and does no tax
   calculation. Other income and taxable Social Security are then computed again. The AMT net operating loss
   adjustment (P60100, grown) is held to the record's net operating loss, the negative part of its other income.
   {summary['refunds removed (record copies)']:,} record copies have the refund removed and
   {summary['refunds kept in part (record copies)']:,} keep part of it; on
   {summary['AMT adjustments held to the loss (record copies)']:,} the AMT adjustment is held to the loss.
   The deductible half of self-employment tax is not in the package: no column of the Tax-Simulator file holds it,
   and your tax calculator computes it from the self-employment income and wages, as ours does. The merge's working
   items carry the PUF's own amount grown (E03260), which no output column reads.
6. The Tax-Simulator columns, then the other records, in the file's order.

Every dollar amount in the output is in nominal {y} dollars.

## Test

Applied to our copy of the 2015 PUF: the Python script, {roundtrip_sentence(rt)} The R script, R_ROUNDTRIP
Rounding differences come from adding the same numbers in a different order (for example, the PUF weight times the
weight factor is not always exactly our weight in the last binary digit); a different numerical library on your
machine can give more of them, of the same size.

## Confidentiality

No PUF amount is in the package for any PUF record. Two items we add are a PUF amount times something
else, so the merge computes them from your PUF and the package holds only the other part: the basis of long-term
gains (the gain times a ratio set by the drawn holding period; the package holds the ratio) and the farm's wage bill for
the qualified business income deduction (the farm income). Parts of the package still depend on PUF values: the other
wage bills (zero where the record has no positive income of the type, a partnership's and an S corporation's split by
their income), the refund multiplier (whether the record itemizes in our
model, and the share of the refund the tax benefit rule keeps), whether a record has a long-term gain (its holding period is missing where it has none), and items computed
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
