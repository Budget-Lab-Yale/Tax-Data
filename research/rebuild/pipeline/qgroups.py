"""Grouped weighting (q) and the AGI group label (agreed 26 October; tested in explore/2026-10-25/q_build).

Training counts each record by its survey weight times q, q constant within a group:
  q = (records in the group) / (total survey weight in the group).
Within a group records count by weight as before; across groups they count by number, so the few records at the top
of the PUF count as much as the many in the middle. Each source's total share of training is unchanged
(model.training_weights rescales within each source and split after multiplying by q).

The groups are computed from BANDS, not exact dollars, because the network sees only bands: two records with the same
bands always fall in the same group. Each band of each amount has one fixed representative amount, the weighted mean
of the real PUF-year amounts in that band (the PUF year's dollar map; a band with no real record: the map's filled
value).
  PUF "agi12": band-AGI, the AGI formula of dollars.draw_dollars (the capital gains pieces, capital gain distributions
    and the folded items into other AGI, business income from its parts) applied to the representative amounts of the
    record's bands. Positive band-AGI in 9 groups at the weighted rank lines p50, p75, p90, p95, p99, p99.5, p99.9 and
    p99.99 of band-AGI over the PUF year's returns; band-AGI zero or negative in 3 groups: -$50,000 to 0 (0
    included), -$1,000,000 to below -$50,000, below -$1,000,000. A record exactly at a line is in the group above.
  SCF "income5": within each wave, 5 classes cut at the weighted p50, p90, p99 and p99.9 of band-income, the sum of
    wages, interest, dividends, capital gains, gross retirement income, Schedules C and F and Schedule E, each from the
    representative amount of the household's band. The SCF's bands are the PUF's with the cut points grown to the wave,
    so a band's representative amount is the PUF one times that growth. For interest, dividends and capital gains the
    band is the household's reported one (model.REPORTED), not the filled-in PUF-concept slot (fillin.py), which
    changes every pass.
  CEX, CPS: q = 1.
q is computed on all of a source's rows (training and held back together), so a group has one q. Settings "training",
q_groups: {"puf": "agi12", "scf": "income5"}; null or missing gives q = 1 everywhere.

The AGI group label. The PUF record's band-AGI group is also a category in the training table (agi_group,
build_table.agi_group_label) that the network learns, always shown on PUF rows in training (train.py). Generation
presets it on every filer record and draws everything else given it (generate.py), so the number of records drawn in
each group is set in advance; each record is weighted by its group's PUF returns over the records drawn in it, which is
1 / q of the group and undoes the up-counting exactly.
"""
import numpy as np
import pandas as pd

import common

Q_GROUPS = common.SETTINGS['training'].get('q_groups') or {}
assert set(Q_GROUPS) <= {'puf', 'scf'}, Q_GROUPS
assert Q_GROUPS.get('puf') in (None, 'agi12') and Q_GROUPS.get('scf') in (None, 'income5'), Q_GROUPS

PUF_LINES = [0.5, 0.75, 0.9, 0.95, 0.99, 0.995, 0.999, 0.9999]
PUF_LABELS = ['AGI below -$1M', 'AGI -$1M to -$50k', 'AGI -$50k to 0', 'below p50', 'p50-75', 'p75-90', 'p90-95',
              'p95-99', 'p99-99.5', 'p99.5-99.9', 'p99.9-99.99', 'p99.99+']
SCF_LINES = [0.5, 0.9, 0.99, 0.999]
SCF_LABELS = ['below p50', 'p50-90', 'p90-99', 'p99-99.9', 'p99.9+']
SCF_INCOME = ['wages', 'interest', 'dividends', 'capital_gains', 'retirement_gross', 'sch_cf_income', 'sch_e_income']


def puf_group(agi, cuts):
    """Group index 0..11 (PUF_LABELS) of AGI in PUF-year dollars, on the 8 positive lines cuts."""
    agi = np.asarray(agi, float)
    pos = 3 + np.searchsorted(np.asarray(cuts, float), agi, side='right')
    neg = np.where(agi >= -50e3, 2, np.where(agi >= -1e6, 1, 0))
    return np.where(agi > 0, pos, neg)


_REP = {}


def rep_maps():
    """{variable: one row per band}: the representative amount of each band of each PUF-map variable, the weighted
    mean of the real amounts in that band in map_puf_<PUF year>, or the map's filled value for a band with no real
    record. Weight 1, so dollars.draw_dollars on these maps returns the representative amounts exactly."""
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
    """AGI from the representative amounts of each record's bands, through the combination dollars.draw_dollars uses.
    PUF-year dollars."""
    import dollars as D
    need = set(D.AGI_PARTS) | set(D.KG_PIECES) | set(D.FOLD) | set(D.META['computed_totals']['business_income'])
    cols = [f'{v}_band' for v in D.ORDER if v in need]
    bands = df[cols].reset_index(drop=True)
    assert (bands >= 0).all().all(), 'a part with no band on a record'
    return D.agi(D.draw_dollars(bands, rep_maps()))


def _scale(e_to, e_from, side):
    """The factor that carries the PUF year's band cuts to another year's (the SCF wave's cuts are the PUF's grown),
    and the spread of the ratios (zero to rounding)."""
    a, b = np.asarray(e_to[side]['cuts'], float), np.asarray(e_from[side]['cuts'], float)
    if len(a) == 0 or len(b) == 0:
        return 1.0, 0.0
    assert len(a) == len(b), (side, len(a), len(b))
    ok = b != 0
    r = a[ok] / b[ok]
    return (float(np.median(r)), float(r.max() - r.min())) if len(r) else (1.0, 0.0)


def scf_band_income(d):
    """Band-income of SCF rows (see the top): the 7 income items from the representative amounts of each row's bands,
    each band's PUF amount times the growth of the item's cut points from the PUF year to the wave (positive and
    negative bands separately)."""
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


_PUF = {}


def puf_groups(table=None):
    """The PUF's band-AGI lines (PUF-year dollars), groups and q, from the table's PUF rows (computed once).
    Returns dict(cuts=[8 lines], q=[12], records=[12], returns=[12], group=Series by table index)."""
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
    """Per SCF row of table: its band-income class 0..4 within its wave (index aligned to the SCF rows)."""
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
    """q per row of table (1 where no group applies)."""
    q = np.ones(len(table))
    pos = pd.Series(np.arange(len(table)), index=table.index)
    if Q_GROUPS.get('puf'):
        ref = puf_groups(table)
        g = ref['group']
        q[pos[g.index].to_numpy()] = np.asarray(ref['q'])[g.to_numpy()]
    if Q_GROUPS.get('scf'):
        s = scf_groups(table)
        key = s['year'].astype(np.int64) * 10 + s['cls']
        rec = key.groupby(key).transform('size').to_numpy(float)
        tot = s['weight'].groupby(key).transform('sum').to_numpy(float)
        q[pos[s.index].to_numpy()] = rec / tot
    return q


def describe(table):
    """A short table of the groups, records, weighted units and q, for the training log."""
    rows = []
    if Q_GROUPS.get('puf'):
        ref = puf_groups(table)
        for k, lab in enumerate(PUF_LABELS):
            rows.append({'source': 'puf', 'group': lab, 'records': int(ref['records'][k]),
                         'weighted (m)': ref['returns'][k] / 1e6, 'q': ref['q'][k]})
    if Q_GROUPS.get('scf'):
        s = scf_groups(table)
        for (y, c), x in s.groupby(['year', 'cls']):
            rows.append({'source': f'scf {y}', 'group': SCF_LABELS[c], 'records': len(x),
                         'weighted (m)': x['weight'].sum() / 1e6, 'q': len(x) / x['weight'].sum()})
    return pd.DataFrame(rows)
