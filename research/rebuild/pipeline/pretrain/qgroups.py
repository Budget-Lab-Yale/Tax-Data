"""Grouped weighting (q) and the AGI group label.

In training, each record counts by its survey weight times q, where q is the number of records in the record's group
divided by the group's total survey weight. Within a group, records then count by their weight; across groups, they
count by their number. This gives the rare top groups more influence on the network than their weight alone would.
On the PUF the groups are 12 groups of band-AGI (AGI computed from the record's bands); on the SCF they are 5 classes
of band-income within each wave; on the other sources q is 1. The groups are computed from bands because the network
sees only bands. Generation sets each record's AGI group label in advance and weights the record by 1 / q, which undoes
the grouping. FEATURES.md, "2. Pretraining", describes the method.
"""
import numpy as np
import pandas as pd

from shared import common

# ---- The groups ----

# The PUF's groups: three for zero or negative AGI, and nine above zero, cut at these weighted percentiles of
# band-AGI.
PUF_LINES = [0.5, 0.75, 0.9, 0.95, 0.99, 0.995, 0.999, 0.9999]
PUF_LABELS = ['AGI below -$1M', 'AGI -$1M to -$50k', 'AGI -$50k to 0', 'below p50', 'p50-75', 'p75-90', 'p90-95',
              'p95-99', 'p99-99.5', 'p99.5-99.9', 'p99.9-99.99', 'p99.99+']
# The SCF's classes, cut at these weighted percentiles of band-income within each wave, and the income items that
# make up band-income.
SCF_LINES = [0.5, 0.9, 0.99, 0.999]
SCF_LABELS = ['below p50', 'p50-90', 'p90-99', 'p99-99.9', 'p99.9+']
SCF_INCOME = ['wages', 'txbl_int', 'dividends', 'capital_gains', 'gross_ss', 'sch_cf_income', 'sche_rent', 'sche_rest']


def puf_group(agi, cuts):
    """The group (0 to 11, as in PUF_LABELS) of each AGI in PUF-year dollars. cuts holds the 8 lines above zero."""
    agi = np.asarray(agi, float)
    pos = 3 + np.searchsorted(np.asarray(cuts, float), agi, side='right')
    neg = np.where(agi >= -50e3, 2, np.where(agi >= -1e6, 1, 0))
    return np.where(agi > 0, pos, neg)


# ---- AGI and income from bands ----

_REP = {}


def rep_maps():
    """A dollar map with one row per band of each variable ({variable: rows}), read once. Each band's amount is the
    weighted mean of the real PUF-year amounts in it; a band with no real record takes the map's filled value. Every
    row has weight 1, so dollars.draw_dollars on these maps returns these amounts exactly."""
    if not _REP:
        m = pd.read_parquet(f'{common.MAPS}/map_puf_{common.PUF_YEAR}.parquet')
        for v, g in m.groupby('variable'):
            rows = []
            for b, x in g.groupby('band', sort=True):
                real = x[~x['filled'].to_numpy(bool)]
                if len(real):
                    w = real['weight'].to_numpy(float)
                    val = float((real['value'].to_numpy(float) * w).sum() / w.sum())
                else:
                    assert len(x) == 1, (v, b)
                    val = float(x['value'].iloc[0])
                rows.append((v, int(b), val, 1.0))
            _REP[v] = pd.DataFrame(rows, columns=['variable', 'band', 'value', 'weight'])
    return _REP


def band_agi(df):
    """Band-AGI: each record's AGI computed from the representative amounts of its bands (rep_maps), in PUF-year
    dollars."""
    from shared import dollars as D
    # Taxable Social Security is computed by the section 86 worksheet, so the worksheet's inputs are needed too.
    need = set(D.AGI_PARTS) | set(D.KG_PIECES) | set(D.FOLD) | set(D.META['computed_totals']['business_income']) \
        | set(D.SS_INPUTS)
    cols = [f'{v}_band' for v in D.ORDER if v in need]
    bands = df[cols].reset_index(drop=True)
    assert (bands >= 0).all().all(), 'a part with no band on a record'
    # The filing status sets the capital loss limit and the worksheet's base amounts.
    assert 'filing_status' in df, 'band-AGI needs the filing status (the loss limit and taxable Social Security)'
    bands['filing_status'] = df['filing_status'].to_numpy()
    return D.agi(D.draw_dollars(bands, rep_maps()))


def _scale(e_to, e_from, side):
    """The factor that carries the PUF year's band cut points (e_from) to another year's (e_to) on one side (negative
    or positive), as the median ratio of the cut points. Also returns the spread of the ratios, which is zero up to
    rounding when every cut point grows by the same factor."""
    a, b = np.asarray(e_to[side]['cuts'], float), np.asarray(e_from[side]['cuts'], float)
    if len(a) == 0 or len(b) == 0:
        return 1.0, 0.0
    assert len(a) == len(b), (side, len(a), len(b))
    ok = b != 0
    r = a[ok] / b[ok]
    return (float(np.median(r)), float(r.max() - r.min())) if len(r) else (1.0, 0.0)


def scf_band_income(d):
    """Band-income of SCF rows: the sum of the income items in SCF_INCOME, each read from the representative amount
    of the row's reported band and grown from the PUF year to the wave by the growth of the item's cut points."""
    rep, edges = rep_maps(), common.edges()
    inc = np.zeros(len(d))
    yr = d['year'].to_numpy()
    for v in SCF_INCOME:
        col = f'{v}_reported_band' if f'{v}_reported_band' in d else f'{v}_band'
        b = d[col].to_numpy()
        assert (b >= 0).all(), f'SCF rows with no band for {col}'
        base = rep[v].set_index('band')['value'].reindex(b).to_numpy(float)
        assert not np.isnan(base).any(), f'{v}: SCF bands the PUF map lacks'
        e_puf = edges[f'puf/{common.PUF_YEAR}/{v}']
        for y in np.unique(yr):
            m = yr == y
            e = edges[f'scf/{int(y)}/{v}']
            assert e['n_bands'] == e_puf['n_bands'], (v, y)
            (fp, sp), (fn, sn) = _scale(e, e_puf, 'positive'), _scale(e, e_puf, 'negative')
            assert sp < 1e-6 * max(1.0, fp) and sn < 1e-6 * max(1.0, fn), \
                f'{v}, SCF {y}: the wave\'s cut points are not the PUF\'s times one factor'
            x = base[m]
            inc[m] += np.where(x > 0, x * fp, x * fn)
    return inc


# ---- q for each row ----

_PUF = {}


def puf_groups(table=None):
    """The PUF's band-AGI group lines (PUF-year dollars), and each group's records, returns and q, from the training
    table's PUF rows. Computed once and kept."""
    if not _PUF:
        if table is None:
            table = pd.read_parquet(common.TABLE)
        d = table[(table['source'] == 'puf').to_numpy()]
        assert (d['year'] == common.PUF_YEAR).all()
        agi, w = band_agi(d), d['weight'].to_numpy(float)
        assert not np.isnan(agi).any(), 'band-AGI missing on a PUF record'
        cuts = common.wquantile(agi, w, PUF_LINES)
        assert cuts[0] > 0
        g = puf_group(agi, cuts)
        rec = np.bincount(g, minlength=12).astype(float)
        ret = np.bincount(g, weights=w, minlength=12)
        assert (rec > 0).all(), f'a PUF band-AGI group with no record: {rec}'
        _PUF.update(cuts=[float(c) for c in cuts], q=list(rec / ret), records=list(rec), returns=list(ret),
                    group=pd.Series(g, index=d.index))
    return _PUF


def scf_groups(table):
    """Each SCF row's band-income class (0 to 4) within its wave, with its year, weight and band-income."""
    d = table[(table['source'] == 'scf').to_numpy()]
    inc = scf_band_income(d)
    w = d['weight'].to_numpy(float)
    yr = d['year'].to_numpy()
    cls = np.zeros(len(d), dtype=np.int64)
    for y in np.unique(yr):
        m = yr == y
        cuts = common.wquantile(inc[m], w[m], SCF_LINES)
        cls[m] = np.searchsorted(cuts, inc[m], side='right')
    return pd.DataFrame({'year': yr, 'cls': cls, 'weight': w, 'income': inc}, index=d.index)


def q_of(table):
    """q for each row of the table (1 on the sources without groups)."""
    q = np.ones(len(table))
    pos = pd.Series(np.arange(len(table)), index=table.index)
    ref = puf_groups(table)
    g = ref['group']
    q[pos[g.index].to_numpy()] = np.asarray(ref['q'])[g.to_numpy()]
    s = scf_groups(table)
    key = s['year'].astype(np.int64) * 10 + s['cls']
    rec = key.groupby(key).transform('size').to_numpy(float)
    tot = s['weight'].groupby(key).transform('sum').to_numpy(float)
    q[pos[s.index].to_numpy()] = rec / tot
    return q


def describe(table):
    """A short table of the groups with their records, weighted units and q, for the training log."""
    rows = []
    ref = puf_groups(table)
    for k, lab in enumerate(PUF_LABELS):
        rows.append({'source': 'puf', 'group': lab, 'records': int(ref['records'][k]),
                     'weighted (m)': ref['returns'][k] / 1e6, 'q': ref['q'][k]})
    s = scf_groups(table)
    for (y, c), x in s.groupby(['year', 'cls']):
        rows.append({'source': f'scf {y}', 'group': SCF_LABELS[c], 'records': len(x),
                     'weighted (m)': x['weight'].sum() / 1e6, 'q': len(x) / x['weight'].sum()})
    return pd.DataFrame(rows)
