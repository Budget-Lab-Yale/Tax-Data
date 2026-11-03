"""check_tables.py: the check tool's tabulation (README.md). It reads the tables of records prepare.py wrote for the year
in settings.json and makes, for one request, the table of an item Y by a variable X in the item's source and in the
posttrained file, side by side. The notebook (check_tool.ipynb, through check_notebook.py) and the web server
(server.py) both use it; neither has a tabulation of its own. Needs only numpy, pandas and pyarrow.
"""
import json
import os
import re
import time

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
SETTINGS = json.load(open(os.path.join(HERE, 'settings.json')))
SIDES = ('source', 'file')


def default_folder():
    """The folder prepare.py wrote for the year and source definition in settings.json."""
    return os.path.join(SETTINGS['cache'], f'{SETTINGS["year"]}_{SETTINGS["puf_source"]}')

# ---- the data -----------------------------------------------------------------------------------------------------
class Data:
    def __init__(self, folder):
        t0 = time.time()
        self.folder = folder
        self.catalog = json.load(open(os.path.join(folder, 'catalog.json')))
        self.frames = {}
        for s in self.catalog['sources']:
            d = pd.read_parquet(os.path.join(folder, f'{s}.parquet'))
            self.frames[s] = {c: d[c].to_numpy() for c in d.columns}
            self.frames[s]['_n'] = len(d)
        self.load_seconds = time.time() - t0
        self.thin = int(self.catalog['thin_cell_records'])


def wquantile(x, w, probs):
    """Smallest x whose cumulative weight share reaches each probability (the pipeline's common.wquantile)."""
    o = np.argsort(x, kind='stable')
    xs, F = x[o], np.cumsum(w[o]) / w.sum()
    return xs[np.minimum(np.searchsorted(F, np.asarray(probs) - 1e-12, side='left'), len(xs) - 1)]


class QueryError(Exception):
    pass


def number_list(v, what):
    if isinstance(v, str):
        v = [p for p in re.split(r'[,\s;]+', v.strip()) if p]
    try:
        out = sorted(set(float(str(p).replace('$', '').replace('_', '')) for p in v))
    except ValueError:
        raise QueryError(f'{what}: not a list of numbers')
    if not out:
        raise QueryError(f'{what}: give at least one number')
    return out


def population(d, units, forbes, side):
    n = d['_n']
    m = np.ones(n, bool)
    if units == 'nodep' and 'dep_filer' in d:
        m &= d['dep_filer'] == 0
    elif units == 'filers':
        m &= d['filer'] == 1
    elif units == 'nonfilers':
        m &= d['filer'] == 0
    if forbes == 'drop' and side == 'file':
        m &= d['forbes'] == 0
    return m


def apply_filters(d, base, filters, cat):
    """The records of base that pass every filter; and for each filter the records and units it leaves out because its
    variable is missing on them."""
    m = base.copy()
    notes = []
    for f in filters:
        x = d[f['var']]
        miss = np.isnan(x)
        nmiss = int((base & miss).sum())
        op = f['op']
        with np.errstate(invalid='ignore'):
            if op == 'gt':
                ok = x > float(f['a'])
            elif op == 'lt':
                ok = x < float(f['a'])
            elif op == 'ge':
                ok = x >= float(f['a'])
            elif op == 'le':
                ok = x <= float(f['a'])
            elif op == 'between':
                lo, hi = sorted([float(f['a']), float(f['b'])])
                ok = (x >= lo) & (x <= hi)
            elif op == 'is':
                ok = np.isin(x, [float(v) for v in f['values']])
            else:
                raise QueryError(f'filter on {f["var"]}: unknown condition {op}')
        m &= ok & ~miss
        if nmiss:
            notes.append({'var': f['var'], 'records': nmiss, 'units': float(d['w'][base & miss].sum())})
    return m, notes


def groups_for(d, keep, xspec, cat, src_cuts=None, rank_base=None):
    """Each record's X group (an integer; -1 where X is missing), the group labels, and the cutoffs used."""
    n = d['_n']
    g = np.full(n, -1, np.int64)
    if xspec['var'] is None:
        g[:] = 0
        return g, ['all units'], None
    x = d[xspec['var']]
    ok = ~np.isnan(x)
    mode = xspec['mode']
    if mode == 'category':
        codes = sorted(int(k) for k in cat['categories']) if cat['categories'] else \
            sorted(set(np.unique(x[ok & keep]).astype(int).tolist()))
        lookup = {c: i for i, c in enumerate(codes)}
        vals = x[ok].astype(int)
        gi = np.array([lookup.get(v, -2) for v in vals], np.int64) if len(vals) else np.zeros(0, np.int64)
        g[ok] = gi
        labels = [cat['categories'].get(str(c), str(c)) for c in codes]
        if (g == -2).any():                 # a code not in the list of categories: its own group, never dropped
            extra = sorted(set(x[g == -2].astype(int).tolist()))
            for c in extra:
                g[(g == -2) & (x == c)] = len(labels)
                labels.append(f'code {c}')
        return g, labels, None
    if mode == 'dollars':
        cuts = number_list(xspec['cuts'], 'dollar cutoffs')
        g[ok] = np.searchsorted(np.array(cuts), x[ok], side='right')
        labels = [f'below {fmt_cut(cuts[0])}'] + [f'{fmt_cut(a)} to below {fmt_cut(b)}' for a, b in zip(cuts, cuts[1:])] \
            + [f'{fmt_cut(cuts[-1])} and over']
        return g, labels, cuts
    if mode == 'percentiles':
        p = number_list(xspec['cuts'], 'percentiles')
        if p[0] <= 0 or p[-1] >= 100:
            raise QueryError('percentiles: each must be above 0 and below 100')
        bounds = [0] + p + [100]
        labels = [f'p{fmt_p(a)} to p{fmt_p(b)}' for a, b in zip(bounds, bounds[1:])]
        if xspec['rank'] == 'own':
            # (a) each file ranked within itself: a record's place is the weight share of the base population below
            # it plus half its own (report_cex.deciles); records tied at one value are split by their order
            b = rank_base & ok
            idx = np.flatnonzero(b)
            o = idx[np.argsort(x[idx], kind='stable')]
            w = d['w'][o]
            cum = (np.cumsum(w) - w / 2) / w.sum() if len(o) and w.sum() > 0 else np.zeros(len(o))
            g[o] = np.searchsorted(np.array(p) / 100, cum, side='right')
            g[~b & ok] = -3                 # outside the base (never in the table: the base holds the filtered units)
            return g, labels, None
        g[ok] = np.searchsorted(np.array(src_cuts), x[ok], side='right')
        return g, labels, src_cuts
    raise QueryError(f'unknown way to cut X: {mode}')


def fmt_cut(v):
    return f'{v:,.0f}' if abs(v) >= 100 or v == int(v) else f'{v:,.3g}'


def fmt_p(v):
    return f'{v:g}'


def cell_stats(d, sel, y, kind):
    """The statistics of Y over the records sel."""
    w = d['w'][sel]
    yv = y[sel]
    miss = np.isnan(yv)
    rec = d['rec'][sel]
    out = {'records': int(len(np.unique(rec))), 'units': float(w.sum()),
           'y_missing_records': int(len(np.unique(rec[miss]))), 'y_missing_units': float(w[miss].sum())}
    wy, yy = w[~miss], yv[~miss]
    out['units_with_y'] = float(wy.sum())
    out['nonzero_units'] = float(wy[yy != 0].sum())
    out['nonzero_share'] = float(out['nonzero_units'] / out['units_with_y']) if out['units_with_y'] > 0 else None
    out['total'] = float((wy * yy).sum())
    out['mean'] = float(out['total'] / out['units_with_y']) if out['units_with_y'] > 0 else None
    return out


def run_query(data, q):
    t0 = time.time()
    cat = data.catalog
    V = cat['variables']
    y = q.get('y')
    if y not in V or not V[y].get('y_source'):
        raise QueryError(f'Y: {y!r} is not an item the tool can tabulate')
    src = V[y]['y_source']
    allowed = set(cat['sources'][src]['x'])
    xspec = dict(q.get('x') or {})
    xspec.setdefault('var', None)
    if xspec['var'] in ('', 'none'):
        xspec['var'] = None
    if xspec['var'] is not None:
        if xspec['var'] not in allowed:
            raise QueryError(f'X: {xspec["var"]} is not available for {y}, whose source is the {cat["sources"][src]["name"]}')
        if V[xspec['var']]['kind'] in ('category', 'flag'):
            xspec['mode'] = 'category'
        elif xspec.get('mode') not in ('percentiles', 'dollars'):
            raise QueryError('X: choose percentiles or dollar cutoffs')
        xspec.setdefault('rank', 'own')
        xspec.setdefault('base', 'filtered')
        if xspec['rank'] not in ('own', 'source') or xspec['base'] not in ('filtered', 'all'):
            raise QueryError('X: unknown percentile definition')
    filters = q.get('filters') or []
    for f in filters:
        if f.get('var') not in allowed:
            raise QueryError(f'filter: {f.get("var")} is not available for {y}, whose source is the '
                             f'{cat["sources"][src]["name"]}')
    units = q.get('units', 'all')
    if units not in ('all', 'nodep', 'filers', 'nonfilers'):
        raise QueryError(f'units: {units}')
    if units in ('filers', 'nonfilers') and not cat['sources'][src]['has_filer']:
        raise QueryError(f'the {cat["sources"][src]["name"]} does not say who files, so filers and non-filers cannot be '
                         'chosen for this item')
    forbes = q.get('forbes', 'keep')
    frames = {'source': data.frames[src], 'file': data.frames['file']}
    pop, keep, fnotes = {}, {}, {}
    for side, d in frames.items():
        pop[side] = population(d, units, forbes, side)
        keep[side], fnotes[side] = apply_filters(d, pop[side], filters, V)
    xinfo = V[xspec['var']] if xspec['var'] else None
    src_cuts = None
    if xspec['var'] and xspec['mode'] == 'percentiles' and xspec['rank'] == 'source':
        d = frames['source']
        x = d[xspec['var']]
        b = (keep['source'] if xspec['base'] == 'filtered' else pop['source']) & ~np.isnan(x)
        if not b.any():
            raise QueryError('no source records to take percentiles from')
        p = number_list(xspec['cuts'], 'percentiles')
        src_cuts = wquantile(x[b], d['w'][b], np.array(p) / 100).astype(float).tolist()
    res = {}
    for side, d in frames.items():
        rb = keep[side] if xspec.get('base', 'filtered') == 'filtered' else pop[side]
        g, labels, cuts = groups_for(d, keep[side], xspec, xinfo, src_cuts, rb)
        res[side] = (g, labels, cuts)
    labels = res['source'][1] if len(res['source'][1]) >= len(res['file'][1]) else res['file'][1]
    # category codes found on one side only: the longer list is a superset for the fixed lists; for open lists use
    # the union of codes
    if xspec['var'] and xspec['mode'] == 'category' and not xinfo['categories']:
        codes = sorted(set().union(*[set(np.unique(frames[s][xspec['var']][keep[s] & ~np.isnan(frames[s][xspec['var']])])
                                         .astype(int).tolist()) for s in SIDES]))
        labels = [str(c) for c in codes]
        for side, d in frames.items():
            x = d[xspec['var']]
            ok = ~np.isnan(x)
            g = np.full(d['_n'], -1, np.int64)
            g[ok] = np.searchsorted(np.array(codes), x[ok].astype(int))
            res[side] = (g, labels, None)
    kind = V[y]['kind']
    rows = []
    any_missing_x = any(((res[s][0] == -1) & keep[s]).any() for s in SIDES)
    order = list(range(len(labels))) + ([-1] if any_missing_x else [])
    tot = {s: cell_stats(frames[s], keep[s], frames[s][y], kind) for s in SIDES}
    for k in order:
        row = {'label': labels[k] if k >= 0 else f'{V[xspec["var"]]["label"]} missing', 'missing_x': k < 0}
        for s in SIDES:
            d = frames[s]
            sel = keep[s] & (res[s][0] == k)
            st = cell_stats(d, sel, d[y], kind)
            st['share_of_total'] = st['total'] / tot[s]['total'] if tot[s]['total'] != 0 else None
            st['share_of_units'] = st['units'] / tot[s]['units'] if tot[s]['units'] > 0 else None
            if sel.any() and xspec['var'] and xspec['mode'] != 'category':
                xv = d[xspec['var']][sel]
                xv = xv[~np.isnan(xv)]
                st['x_min'], st['x_max'] = (float(xv.min()), float(xv.max())) if len(xv) else (None, None)
            row[s] = st
        row['thin'] = min(row['source']['records'], row['file']['records']) < data.thin
        rows.append(row)
    for s in SIDES:
        tot[s]['share_of_total'] = 1.0 if tot[s]['total'] != 0 else None
        tot[s]['share_of_units'] = 1.0
    total = {'label': 'all units shown', **tot, 'thin': min(tot['source']['records'], tot['file']['records']) < data.thin}
    out = {'y': y, 'y_label': V[y]['label'], 'y_kind': kind, 'y_unit': V[y].get('unit'), 'source_key': src,
           'source': {k: cat['sources'][src][k] for k in ('name', 'what', 'count')},
           'file': {k: cat['sources']['file'][k] for k in ('name', 'what', 'count')},
           'x': xspec, 'x_label': V[xspec['var']]['label'] if xspec['var'] else None,
           'cuts': {s: res[s][2] for s in SIDES}, 'rows': rows, 'total': total, 'thin_cell_records': data.thin,
           'filter_missing': fnotes,
           'population': {s: {'records': int(len(np.unique(frames[s]['rec'][pop[s]]))), 'units': float(frames[s]['w'][pop[s]].sum())}
                          for s in SIDES},
           'forbes_in_file': int(frames['file']['forbes'][keep['file']].sum())}
    out['seconds'] = time.time() - t0
    return out


def jsonable(o):
    if isinstance(o, float) and not np.isfinite(o):
        return None
    if isinstance(o, dict):
        return {k: jsonable(v) for k, v in o.items()}
    if isinstance(o, (list, tuple)):
        return [jsonable(v) for v in o]
    if isinstance(o, (np.floating,)):
        return jsonable(float(o))
    if isinstance(o, (np.integer,)):
        return int(o)
    return o
