"""The year's targets, from the configuration in targets_config/.

The configuration says what to target in words: each entry is a block of targets, every statistic x every variable
x every cell, with its population, where its value comes from and the kind of its error. The catalog
(targets_config/catalog.yaml) says what the words mean; it is a dictionary shared by every year. Each year has a
folder of its own, targets_config/<year>/, with three files written out in full (nothing is inherited): targets.yaml
(the entries), prior.yaml (the year's prior) and growth.yaml (how every dollar item is grown, growth.py). A new year
starts as a copy of the nearest year's folder.

  resolve(year)                 the year's entries, prior and growth lines, checked: every entry has a value source
                                the catalog defines, every dollar item a growth line (growth.check); stops otherwise
  expand(year, start, which)    one row per target (the columns in ROW_COLUMNS): its key (kind, group, target),
                                its parts in words, its value, its error kind and standard error, whether it is an
                                identity, its source lines, the entry's reason, and for a survey target the tax-unit
                                rules its value was computed on. A cell no record of the starting file can reach is
                                left out (listed, with the cells published as zero, in LAST['excluded'])
  conditions(df, rows, w_start) the linear condition each row puts on the file's weights: F (records x targets) and
                                T, so that the target is met when F.T @ w == T; and for an average or a share,
                                averages[j] = (member, v, the published average), else None
  write(rows, folder, year)     targets.csv and the resolved configuration, saved with the run
  start_spec(year)              how the year's start is built: the year file's start and start_scaling

Values. Published values come from the run's SOI numbers (prep/soi.py: Tables 1.4 and 1.2 in targets/soi.csv, Table
1.6 by soi.table_1_6); survey values from the survey's own tax units in the run's data folder, the SCF's from
the year's mix of SCF waves (scf_targets.py, which computes their standard errors with them); the non-filer values
from the CPS filing stage of the same run.

Standard errors. errors.standard_errors(rows, year) (errors.py) gives each target's standard error, in the units of
the published statistic (on an average, on the average itself); 0 for an identity.

Averages and shares. "The average of x over the group is m" is written as the sum over the group of w (x / m - 1) = 0;
solver.py takes a zero target.
"""
import copy
import json
import os
import subprocess

import numpy as np
import pandas as pd
import yaml

from shared import common
from prep import soi as TG

CONFIG = os.path.join(common.PIPELINE, 'targets_config')
ROW_COLUMNS = ['kind', 'group', 'target', 'statistic', 'population', 'variable', 'value', 'error_kind', 'se',
               'identity', 'source', 'reason', 'tax_unit_rules']
# Columns after the contract's, which conditions() reads: the entry, the year of the value, the cell in machine
# terms (an AGI range, married, the age range, a line and the variable it is on), the dependent-filer rule, the error's
# reason.
CELL_COLUMNS = ['entry', 'year', 'agi_lo', 'agi_hi', 'married', 'age_range', 'line', 'line_variable',
                'dependent_filers', 'error_reason',
                # an income rank group (its variable, its percentiles, and the survey's dollar
                # lines at those percentiles, for the record; the file's lines are its own, set in conditions())
                'rank_variable', 'rank_lo', 'rank_hi', 'rank_line_lo', 'rank_line_hi',
                # a band's upper line (net worth at or below it), on line_variable; NaN for none. A
                # band's lower line is 'line' (net worth above it), NaN for a band with no lower line.
                'line_hi',
                # a dollar range of a variable other than AGI (catalog groupings with edges): the unit
                # is in the cell when range_lo <= the variable < range_hi; '' and NaN for none
                'range_variable', 'range_lo', 'range_hi']
TARGETS_KEYS = {'year', 'kind', 'targets', 'report', 'start', 'start_scaling'}
PRIOR_KEYS = {'year', 'prior'}
ENTRY_KEYS = {'id', 'kind', 'statistic', 'population', 'variable', 'cells', 'value', 'error', 'key', 'outside_line',
              'dependent_filers', 'note', 'reason', 'percentiles', 'must_reach', 'needs_calculator'}
# reason: why the entry is as it is in this year where it departs from the general rule or from other years; it fills
# targets.csv's reason column.
# must_reach: an entry's cell labels that expand must keep; a cell among them that no record can reach is an error,
# not a quiet drop (Table 4.3's top 0.001%).
VALUE_KINDS = {'published', 'survey', 'pipeline', 'outside'}
ERROR_KEYS = {'kind', 'relative', 'reason'}


# ---- reading and resolving ------------------------------------------------------------------------------------
class _Loader(yaml.SafeLoader):
    """The safe loader, refusing anchors and aliases (writing rules, catalog.yaml)."""
    def compose_node(self, parent, index):
        if self.check_event(yaml.AliasEvent):
            raise ValueError('targets_config: YAML aliases are not allowed')
        event = self.peek_event()
        if getattr(event, 'anchor', None):
            raise ValueError(f'targets_config: YAML anchor &{event.anchor} is not allowed')
        return super().compose_node(parent, index)


def _yaml(path):
    with open(path) as f:
        return yaml.load(f, Loader=_Loader)


CAT = _yaml(os.path.join(CONFIG, 'catalog.yaml'))


def _no_scientific(x, where):
    """YAML reads 1e6 as text: a number-like string in a numeric place is an error."""
    if isinstance(x, str):
        try:
            float(x)
        except ValueError:
            return
        raise ValueError(f'{where}: {x!r} is text; write numbers out in full')


def folder(year):
    return os.path.join(CONFIG, str(int(year)))


def read(year, name):
    """One file of the year's folder, its top-level keys checked."""
    p = os.path.join(folder(year), f'{name}.yaml')
    assert os.path.exists(p), f'no {name}.yaml for {year} in {folder(year)} (a new year starts as a copy of the ' \
                              f'nearest year\'s folder)'
    d = _yaml(p)
    allowed = {'targets': TARGETS_KEYS, 'prior': PRIOR_KEYS}[name]
    bad = set(d) - allowed
    assert not bad, f'{p}: unknown keys {bad}'
    assert int(d.get('year', -1)) == int(year), f'{p} says year {d.get("year")}'
    return d


def value_source(e):
    """(kind, source) of an entry's value, checked: one of VALUE_KINDS, a source the catalog defines."""
    v = e.get('value')
    assert isinstance(v, dict), f'entry {e.get("id")}: no value source'
    kinds = [k for k in v if k != 'year']
    assert len(kinds) == 1 and kinds[0] in VALUE_KINDS, \
        f'entry {e.get("id")}: value must name one of {sorted(VALUE_KINDS)} (and optionally a year), not {v}'
    assert v[kinds[0]] in CAT['sources'], f'entry {e.get("id")}: value source {v[kinds[0]]!r} is not in the catalog'
    return kinds[0], v[kinds[0]]


_RESOLVED = {}


def resolve(year):
    """The year's configuration: its targets.yaml and prior.yaml as written, with the growth lines of its growth.yaml
    (under 'growth'). Checked before any use (TARGETS.md section 12): every entry has its parts and a value source
    the catalog defines, and every dollar item has a growth line for the year (growth.check). Stops otherwise."""
    year = int(year)
    if year in _RESOLVED:
        return copy.deepcopy(_RESOLVED[year])
    t, p = read(year, 'targets'), read(year, 'prior')
    r = {k: t[k] for k in ['year', 'kind', 'report', 'start', 'start_scaling'] if k in t}
    r['prior'] = p['prior']
    r['targets'] = t.get('targets') or []
    ids = [e.get('id') for e in r['targets']]
    assert len(ids) == len(set(ids)), f'{folder(year)}/targets.yaml: an entry id twice'
    for e in r['targets']:
        bad = set(e) - ENTRY_KEYS
        assert not bad, f'entry {e.get("id")}: unknown keys {bad}'
        missing = {'id', 'kind', 'statistic', 'population', 'cells', 'value', 'error', 'key'} - set(e)
        assert not missing, f'entry {e["id"]}: missing {missing}'
        bad = set(e['error']) - ERROR_KEYS
        assert not bad, f'entry {e["id"]}: unknown keys in error {bad}'
        _no_scientific(e['error'].get('relative'), f'entry {e["id"]}, error')
        value_source(e)
        assert e['population'] in CAT['populations'], f'entry {e["id"]}: no population {e["population"]}'
        assert e['error']['kind'] in CAT['error_kinds'], f'entry {e["id"]}: no error kind {e["error"]["kind"]}'
        if e['error']['kind'] == 'judgment':
            assert 'relative' in e['error'] and e['error'].get('reason'), \
                f'entry {e["id"]}: a judgment error needs its number (relative) and a reason'
        for st in _as_list(e['statistic']):
            assert st in CAT['statistics'], f'entry {e["id"]}: no statistic {st}'
    from shared import growth as GR
    GR.check(year)
    r['growth'] = GR.load(year)
    _RESOLVED[year] = r
    return copy.deepcopy(r)


def _as_list(x):
    return list(x) if isinstance(x, list) else [x]


# ---- variables ------------------------------------------------------------------------------------------------
def variable_names(spec):
    if spec is None:
        return [None]
    if isinstance(spec, str):
        return list(CAT['variable_lists'][spec]) if spec in CAT['variable_lists'] else [spec]
    return list(spec)


def variable_def(v):
    """The catalog's definition of a variable; a business family's net income or loss from soi.FAMILIES; a
    projection year's people, returns group or item side (projection_steps)."""
    if v in CAT['variables']:
        return CAT['variables'][v]
    from scale import projection_steps as PS
    if PS.handles(v):
        return PS.variable_def(v)
    for fam in CAT['families']:
        for side in ('net_income', 'net_loss'):
            if v == f'{fam}_{side}':
                f = TG.FAMILIES[fam]
                return {'family': fam, 'side': side, 'short': f'{fam} {side}',
                        'soi 1.4': {'count': [v], 'amount': [v]}, 'what': f'{f["soi"]} > {side.replace("_", " ")}'}
    raise KeyError(f'no variable {v} in the catalog')


AGE_COLUMNS = {'head': 'age1', 'spouse': 'age2', 'dep1': 'dep_age1', 'dep2': 'dep_age2',
               'dep3': 'dep_age3'}


def person_values(df, v):
    """A variable defined person by person (catalog ss_tips): an array (records x people slots), each person's value.
    W-2 box 7: each person's tips held so that wages other than tips plus tips are at most the wage base. The record's
    value (record_value) is the sum over the slots; the worker-level statistics count and rank the slots."""
    d = variable_def(v)
    assert 'ss_tips' in d, f'{v}: not defined person by person'
    base = d['wage_base'].get(common.TARGET_YEAR)
    assert base, f'{v}: no Social Security wage base for {common.TARGET_YEAR} in the catalog'
    out = np.zeros((len(df), len(d['ss_tips'])))
    for k, (wcol, tcol) in enumerate(d['ss_tips']):
        wg, tp = df[wcol].to_numpy(float), df[tcol].to_numpy(float)
        assert np.isfinite(wg).all() and np.isfinite(tp).all(), f'{v}: {wcol} or {tcol} missing'
        assert (tp <= wg + 1e-6).all(), f'{v}: {tcol} above {wcol} on {int((tp > wg + 1e-6).sum())} records'
        out[:, k] = np.minimum(tp, np.maximum(0.0, float(base) - (wg - tp)))
    return out


def record_value(df, v):
    """A record's value of variable v (in the file's dollars)."""
    d = variable_def(v)
    if 'projection' in d:
        from scale import projection_steps as PS
        return PS.record_value(df, v)
    if 'family' in d:
        f = TG.FAMILIES[d['family']]
        net = df[f['income']].to_numpy(float).sum(1) - (df[f['loss']].to_numpy(float).sum(1) if f['loss'] else 0.0)
        if d['side'] == 'net':
            assert np.isfinite(net).all(), f'{v}: a missing or nonfinite business part'
            return net
        return np.where(net > 0, net, 0.0) if d['side'] == 'net_income' else np.where(net < 0, -net, 0.0)
    if 'transform' in d:
        assert d['transform'] in ('positive', 'loss', 'loss_allowed', 'any'), f'{v}: unknown transform {d["transform"]}'
        x = record_value(df, d['base'])
        if d['transform'] == 'any':
            assert np.isfinite(x).all(), f'{v}: {int((~np.isfinite(x)).sum())} records with {d["base"]} missing'
            return (x != 0).astype(float)
        if d['transform'] == 'loss_allowed':
            # The loss allowed on the return: $3,000, or $1,500 if married filing separately (mars 3).
            return np.minimum(np.maximum(-x, 0), np.where(df['filing_status'].to_numpy() == 3, 1500.0, 3000.0))
        return np.maximum(x, 0) if d['transform'] == 'positive' else np.maximum(-x, 0)
    if 'ss_tips' in d:
        return person_values(df, v).sum(1)
    if 'people' in d:
        p = d['people']
        hi = np.inf if p['to'] is None else p['to']
        n = np.zeros(len(df))
        for m in p['members']:
            a = df[AGE_COLUMNS[m]].to_numpy(float)          # -1 for no such member
            n += (a >= p['from']) & (a < hi)
        return n
    if 'gain_held_to_value' in d:
        # an unrealized gain held to its asset's value and never below zero, as the simulator has it
        g = df[d['gain_held_to_value']['gain']].to_numpy(float)
        val = df[d['gain_held_to_value']['value']].to_numpy(float).sum(1)
        assert np.isfinite(g).all() and np.isfinite(val).all(), \
            f'{v}: the gain or a value missing on {int((~np.isfinite(g) | ~np.isfinite(val)).sum())} records'
        return np.clip(np.minimum(g, val), 0, None)
    if 'parts' in d:
        x = df[d['parts']].to_numpy(float)
        assert not np.isnan(x).any(), f'{v}: a part missing on {int(np.isnan(x).any(1).sum())} records'
        return x.sum(1)
    return df[d['record']].to_numpy(float)


# ---- cells ----------------------------------------------------------------------------------------------------
def group_cells(spec):
    """A cells spec as a list of cells: {'label', 'parts': {grouping: label}, 'filters': [(column, value)],
    'agi': grouping name or None, 'line': (kind, number, label) or None}. AGI groupings are resolved later, with
    the published cells (merging needs them)."""
    if spec == 'all':
        return [{'label': 'all', 'parts': {}, 'filters': [], 'agi': None, 'line': None}]
    if isinstance(spec, list):
        return [c for s in spec for c in group_cells(s)]
    per_dim = []
    for name, sel in spec.items():
        g = CAT['groupings'][name]
        if g['variable'] == 'agi' and 'ranks' not in g:
            assert sel == 'all', f'{name}: AGI groupings take all their cells'
            per_dim.append([{'parts': {}, 'filters': [], 'agi': name, 'line': None}])
        elif 'values' in g:
            vals = [v for v in g['values'] if sel == 'all' or v[1] in sel]
            assert sel == 'all' or len(vals) == len(sel), f'{name}: unknown cells in {sel}'
            per_dim.append([{'parts': {name: lab}, 'filters': [(g['variable'], val)], 'agi': None, 'line': None}
                            for val, lab in vals])
        elif 'lines' in g:
            lines = [ln for ln in g['lines'] if sel == 'all' or ln[2] in sel]
            assert sel == 'all' or len(lines) == len(sel), f'{name}: unknown cells in {sel}'
            per_dim.append([{'parts': {name: ln[2]}, 'filters': [], 'agi': None, 'line': (g['variable'], *ln)}
                            for ln in lines])
        elif 'ranks' in g and g.get('cutoffs') == 'published':
            # a published percentile group at the source's own published dollar line for the year: the AGI cutoff the
            # source prints for the group (catalog cutoff_item), not the file's percentile. The percentiles name the
            # group only; published_grid sets the cell's AGI range from the cutoff.
            assert g['variable'] == 'agi', f'{name}: published lines are AGI cutoffs'
            ranks = [rk for rk in g['ranks'] if sel == 'all' or rk[2] in sel]
            assert sel == 'all' or len(ranks) == len(sel), f'{name}: unknown cells in {sel}'
            assert all(float(rk[1]) == 100 for rk in ranks), f'{name}: published lines are for top groups only'
            per_dim.append([{'parts': {name: rk[2]}, 'filters': [], 'agi': None, 'line': None,
                             'published_line': name} for rk in ranks])
        elif 'ranks' in g:
            # a range of a variable's weighted percentiles, each side its own
            ranks = [rk for rk in g['ranks'] if sel == 'all' or rk[2] in sel]
            assert sel == 'all' or len(ranks) == len(sel), f'{name}: unknown cells in {sel}'
            per_dim.append([{'parts': {name: rk[2]}, 'filters': [], 'agi': None, 'line': None,
                             'rank': (g['variable'], float(rk[0]), float(rk[1]))} for rk in ranks])
        elif 'bands' in g:
            # a band of the variable between two lines, [lower, upper, label]; each line is
            # [percentile, p] (the survey's p-th percentile, in dollars) or [dollars, d] or null (none). A unit is in
            # the band when it is above the lower line and at or below the upper one, as "above the line" is
            # strictly above.
            bands = [bd for bd in g['bands'] if sel == 'all' or bd[2] in sel]
            assert sel == 'all' or len(bands) == len(sel), f'{name}: unknown cells in {sel}'
            per_dim.append([{'parts': {name: bd[2]}, 'filters': [], 'agi': None, 'line': None,
                             'band': (g['variable'], bd[0], bd[1])} for bd in bands])
        elif 'edges' in g:
            # dollar ranges of a variable other than AGI, [low, high, label], low inclusive, null for no limit: each
            # range is a cell, on the file and on a survey's units alike (record_value)
            ranges = [ed for ed in g['edges'] if sel == 'all' or ed[2] in sel]
            assert sel == 'all' or len(ranges) == len(sel), f'{name}: unknown cells in {sel}'
            per_dim.append([{'parts': {name: ed[2]}, 'filters': [], 'agi': None, 'line': None,
                             'range': (g['variable'], -np.inf if ed[0] is None else float(ed[0]),
                                       np.inf if ed[1] is None else float(ed[1]))} for ed in ranges])
        else:
            raise NotImplementedError(name)
    out = [{'parts': {}, 'filters': [], 'agi': None, 'line': None, 'rank': None, 'band': None, 'range': None,
            'published_line': None}]
    for dim in per_dim:
        for a in out:
            assert not (a.get('range') and any(b.get('range') for b in dim)), 'a cell with two dollar ranges'
        out = [{'parts': {**a['parts'], **b['parts']}, 'filters': a['filters'] + b['filters'],
                'agi': a['agi'] or b['agi'], 'line': a['line'] or b['line'],
                'rank': a.get('rank') or b.get('rank'), 'band': a.get('band') or b.get('band'),
                'range': a.get('range') or b.get('range'),
                'published_line': a.get('published_line') or b.get('published_line')}
               for a in out for b in dim]
    assert not any(c['line'] and c['band'] for c in out), 'a cell with both a line and a band'
    assert not any(c['published_line'] and (c['agi'] or c['rank']) for c in out), \
        'a cell with a published AGI line and another AGI grouping'
    for c in out:
        c['label'] = ', '.join(c['parts'].values()) or 'all'
    return out


def merge_groups(groups, cells):
    """AGI groups merged where a published cell crosses a group edge."""
    groups = list(groups)
    i = 0
    while i < len(groups) - 1:
        edge = groups[i][1]
        if any(lo < edge < hi for lo, hi, _ in cells):
            groups[i:i + 2] = [(groups[i][0], groups[i + 1][1], f'{groups[i][2]} to {groups[i + 1][2]}')]
            i = 0
        else:
            i += 1
    return groups


# ---- values ---------------------------------------------------------------------------------------------------
def soi_lines(v, st, table='1.4'):
    """[(soi.csv variable, statistic, sign)] behind statistic st of variable v in Table 1.4 (or the table named)."""
    if st == 'units':
        return [('returns', 'count', 1)]
    lines = variable_def(v)[f'soi {table}'][st]
    return [(ln[len('minus '):], st, -1) if ln.startswith('minus ') else (ln, st, 1) for ln in lines]


def soi_table_lines(v, st):
    """(table, soi_lines) of a variable published by size of AGI in Table 1.4 or Table 3.3 (the one its catalog entry
    names), or of the number of returns (statistic units, Table 1.4), for errors.py."""
    if st == 'units':
        return '1.4', soi_lines(v, st)
    d = variable_def(v)
    (table,) = [t for t in ('1.4', '3.3') if f'soi {t}' in d]
    return table, soi_lines(v, st, table)


def published_14(soi, year, cells, v, stats, table='1.4'):
    """Rows from Table 1.4 (or Table 3.3, table '3.3') by AGI: our groups summed from the published cells, the groups
    merged where a published cell crosses an edge, over all the variable's statistics (merge_groups)."""
    (c,) = cells
    rows = []
    # every line the variable names must be in the year's table: a line not found would give zero in every AGI group,
    # and a cell published as zero is left out (expand), so a misnamed line would silently drop all its targets
    t14 = soi[(soi.year == year) & (soi.table.astype(str) == table) & (soi.filing_status == 'all')]
    have = set(zip(t14['variable'], t14['statistic']))
    for st in stats:
        missing = [(var, stt) for var, stt, _ in soi_lines(v, st, table) if (var, stt) not in have]
        assert not missing, f'Table {table} {year}, variable {v}: no line {missing} in the SOI table (targets/soi.csv); ' \
                            'a misnamed variable or family, or a line SOI does not publish that year'
    if c['agi'] is None:   # cells: all, the all-returns total row
        assert c['label'] == 'all' and not c['filters'], f'Table {table}: cells {c["label"]} need an AGI grouping'
        t = soi[(soi.year == year) & (soi.table.astype(str) == table) & (soi.filing_status == 'all') & (soi.label == 'all')]
        s = t.groupby(['variable', 'statistic'])['value'].sum()
        for st in stats:
            ln = soi_lines(v, st, table)
            rows.append({'cell': 'all', 'statistic': st, 'value': sum(sg * s[(var, stt)] for var, stt, sg in ln),
                         'source': f'soi {table} {year}, all returns, total: ' + ' | '.join(
                             f'{"minus " if sg < 0 else ""}{var} {stt}' for var, stt, sg in ln)})
        return rows
    g = CAT['groupings'][c['agi']]
    groups = [(-np.inf if lo is None else lo, np.inf if hi is None else hi, lab) for lo, hi, lab in g['edges']]
    pc = {st: [(sg, TG.published_cells(soi, year, var, stt, table)) for var, stt, sg in soi_lines(v, st, table)]
          for st in stats}
    if g.get('merge_across_published'):
        groups = merge_groups(groups, [x for st in stats for _, cl in pc[st] for x in cl])
    for lo, hi, lab in groups:
        for st in stats:
            rows.append({'cell': lab, 'agi': (lo, hi), 'statistic': st,
                         'value': sum(sg * sum(val for l_, h_, val in cl if l_ >= lo and h_ <= hi) for sg, cl in pc[st]),
                         'source': f'soi {table} {year}: ' + ' | '.join(f'{"minus " if sg < 0 else ""}{var} {stt}'
                                                                      for var, stt, sg in soi_lines(v, st, table))})
    return rows


def table_17(year):
    """Table 1.7 of a year from the SOI mirror, all returns, by AGI bracket: one row per published cell, with lo, hi
    (the bracket; -inf and inf at the ends, NaN for the all-AGI row), label, item, value (as published: amounts in
    thousands) and flag."""
    t = pd.read_csv(os.path.join(TG.MIRROR, CAT['sources']['soi 1.7']['file']), dtype=str, keep_default_na=False)
    t = t[(t['tax_year'] == str(year)) & (t['table'] == '1.7') & (t['panel'] == 'All returns')]
    assert len(t), f'Table 1.7 {year}: not in the SOI mirror'
    edge = lambda a, inf: np.nan if a == '' else inf if a in ('-Inf', 'Inf') else float(a)
    return pd.DataFrame({'lo': [edge(a, -np.inf) for a in t['row_agi_lo']],
                         'hi': [edge(a, np.inf) for a in t['row_agi_hi']],
                         'label': t['row_label'].to_numpy(), 'item': t['item'].to_numpy(),
                         'value': t['value'].to_numpy(), 'flag': t['flag'].to_numpy()})


def table_17_item(v, st):
    """The Table 1.7 mirror item behind statistic st of variable v: the number of returns for 'units', else the
    item the variable's catalog entry names under 'soi 1.7'."""
    if st == 'units':
        return 'number of returns'
    items = variable_def(v).get('soi 1.7')
    assert items and st in items, f'Table 1.7: no item for {v} {st} in the catalog'
    return items[st]


def published_17(soi, year, cells, v, stats):
    """Table 1.7, dependent filers by AGI, read from the SOI mirror (targets/soi.csv has Tables 1.4 and 1.2 only):
    the number of returns (statistic units) and the amount (and count) of an item the variable names under "soi 1.7"
    in the catalog (the net capital gain). Amounts are published in thousands and returned in dollars. A blank or
    flagged cell would need joining to its neighbour; none is blank among the cells targeted, so one is an error."""
    (c,) = cells
    t = table_17(year)
    out = []
    for st in stats:
        item = table_17_item(v, st)
        scale = 1000.0 if st == 'amount' else 1.0
        what = 'number of returns' if st == 'units' else item
        ti = t[t['item'] == item]
        assert len(ti), f'Table 1.7 {year}: no item {item!r}'
        if c['agi'] is None:   # cells: all, the all-AGI total
            (val,) = ti.loc[ti['lo'].isna(), 'value']
            assert val != '', f'Table 1.7 {year}: a blank all-AGI cell for {item}'
            out.append({'cell': 'all', 'statistic': st, 'value': float(val) * scale,
                        'source': f'soi 1.7 {year}: dependent filers, {what}, all AGI'})
            continue
        # our own AGI groups: each sums the published brackets inside it, merged where a published bracket crosses an
        # edge (merge_groups), as Table 1.4's groups are. A grouping that covers part of the AGI range (covers_part)
        # uses the published brackets inside its span, which must each lie in one group.
        g = CAT['groupings'][c['agi']]
        assert 'edges' in g, f'Table 1.7: the AGI grouping {c["agi"]} has no edges'
        cell = ti[ti['lo'].notna()].sort_values('lo')
        groups = [(-np.inf if lo is None else lo, np.inf if hi is None else hi, lab) for lo, hi, lab in g['edges']]
        span = (groups[0][0], groups[-1][1])
        inside = cell[(cell.lo >= span[0]) & (cell.hi <= span[1])]
        assert not ((cell.lo < span[0]) & (cell.hi > span[0])).any() and \
            not ((cell.lo < span[1]) & (cell.hi > span[1])).any(), \
            f'Table 1.7 {year}: a published bracket crosses the ends of {c["agi"]}'
        assert g.get('covers_part') or len(inside) == len(cell), \
            f'Table 1.7 {year}: published brackets outside {c["agi"]}'
        if g.get('merge_across_published'):
            groups = merge_groups(groups, list(inside[['lo', 'hi', 'label']].itertuples(index=False, name=None)))
        covered = sum(int(((inside.lo >= lo) & (inside.hi <= hi)).sum()) for lo, hi, _ in groups)
        assert covered == len(inside), f'Table 1.7 {year}: a published bracket in none of the groups of {c["agi"]}'
        for lo, hi, lab in groups:
            x = inside[(inside.lo >= lo) & (inside.hi <= hi)]
            assert len(x) and (x['value'] != '').all() and (x['flag'] == '').all(), \
                f'Table 1.7 {year}, {lab}: {item} blank or flagged ({list(x["flag"])}); it would need joining'
            out.append({'cell': lab, 'agi': (lo, hi), 'statistic': st,
                        'value': float(x['value'].astype(float).sum()) * scale,
                        'source': f'soi 1.7 {year}: dependent filers, {what}'})
    return out


def published_25(year, cells, v, stats):
    """SOI Table 2.5, returns with the earned income credit by the number of qualifying children, all AGI (the
    mirror's bysize_eitc.csv, row Total): the returns with the credit and its amount in each children group (grouping
    eitc_children on n_dep_eitc). Errors borrow Table 1.4's nearest cells (errors.borrow_cv), as the other tables'
    counts and amounts do."""
    from reweight import errors
    lab = {'none': 'no qualifying children', 'one': 'one qualifying child', 'two': 'two qualifying children',
           'three or more': 'three or more qualifying children'}
    t = pd.read_csv(os.path.join(TG.MIRROR, 'bysize_eitc.csv'), dtype=str, keep_default_na=False)
    t = t[(t.tax_year == str(year)) & (t.row_label == 'Total')]
    assert len(t), f'Table 2.5 {year}: no Total row in bysize_eitc.csv'
    donors, _ = errors.soi_cells(year)
    rows = []
    for c in cells:
        (g,) = [n for n in c['parts'] if CAT['groupings'][n]['variable'] == 'n_dep_eitc']
        group = lab[c['parts'][g]]
        vals = {}
        for st in ('count', 'amount'):
            item = f'returns with {group} > ' + ('number of returns' if st == 'count' else 'total earned income credit > amount')
            x = t[t['item'] == item]['value']
            assert len(x) == 1 and x.iloc[0] != '', f'Table 2.5 {year}: no value for {item}'
            vals[st] = float(x.iloc[0]) * (1000.0 if st == 'amount' else 1.0)
        for st in stats:
            val = vals[st]
            cv, what = (errors.borrow_cv(donors, count=val) if st == 'count' else
                        errors.borrow_cv(donors, amount=val, returns=vals['count']))
            rows.append({'cell': c['label'], 'cell_spec': c, 'statistic': st, 'value': val, 'se': abs(val) * cv / 100,
                         'error_kind': 'soi_borrowed', 'se_reason': f'Table 2.5 {st}, CV {cv:.2f}% borrowed from Table 1.4: {what}',
                         'source': f'soi 2.5 {year}: returns with {group}, the earned income credit ({st}), all AGI; bysize_eitc.csv'})
    return rows


def published_status(soi, year, source, cells, stats):
    """Rows of returns by filing status (Table 1.2) or by filing status and age (Table 1.6), all AGI."""
    assert stats == ['units'], f'{source}: returns only'
    rows = []
    t16 = TG.table_1_6(year) if source == 'soi 1.6' else None
    four = {'single': ['single'], 'joint': ['joint'], 'separate': ['separate'], 'head of household': ['head_of_household']}
    for c in cells:
        (gname,) = [n for n in c['parts'] if CAT['groupings'][n]['variable'] in ('married', 'filing_status')]
        if CAT['groupings'][gname]['variable'] == 'filing_status':
            assert source == 'soi 1.6', f'{source}: by filing status only for Table 1.6'
            statuses = four[c['parts'][gname]]
        else:
            statuses = CAT['groupings'][gname][f'in {source}'][c['parts'][gname]]
        if source == 'soi 1.2':
            v = sum(TG.returns(soi, year, '1.2', s) for s in statuses)
            src = f'soi 1.2 {year}: returns of ' + ' + '.join(statuses)
        else:
            (aname,) = [n for n in c['parts'] if CAT['groupings'][n]['variable'] == 'age_range']
            (k,) = [val for val, lab in CAT['groupings'][aname]['values'] if lab == c['parts'][aname]]
            v = sum(t16[(s, k)] for s in statuses)
            src = f'soi 1.6 {year}: returns of ' + ' + '.join(statuses) + f', primary {c["parts"][aname]}'
        rows.append({'cell': c['label'], 'cell_spec': c, 'statistic': 'units', 'value': v, 'source': src})
    return rows


# Table 1.6 by AGI: the All returns panel's returns by the primary filer's age range within each AGI group. A row whose
# AGI columns do not add to its own all-AGI total by more than this many returns has a misprinted cell (2018, 26 under
# 35: SOI's workbook prints $40,000-50,000 and $50,000-75,000 2.21 million too high, both flagged combined in the
# mirror; the four filing-status panels confirm it).
T16_ROW_SLACK = 50


def table_16_by_agi(year):
    """Table 1.6's All returns panel for a year: {age row label: (all-AGI total, [(lo, hi, value, flag), ...])}, the
    AGI columns lowest first. Values are returns. Rows under 26 are left out (the targets start at 26)."""
    t = pd.read_csv(common.SETTINGS['ages']['table_1_6'], dtype=str, keep_default_na=False)
    t = t[(t.tax_year == str(year)) & (t.panel == 'All returns') & t.row_label.isin(TG.T16_ROWS[1:])]
    assert len(t), f'Table 1.6 {year}: no All returns panel'
    out = {}
    for row, g in t.groupby('row_label'):
        tot = g[(g.col_agi_lo == '') & (g.col_agi_hi == '')]
        assert len(tot) == 1 and tot.value.iloc[0] != '', f'Table 1.6 {year}, {row}: no all-AGI total'
        cols = g[(g.col_agi_lo != '') & (g.col_agi_hi != '')]
        assert (cols.value != '').all(), f'Table 1.6 {year}, {row}: a blank AGI cell in the All returns panel ' \
                                         f'({cols.loc[cols.value == "", "col_label"].tolist()}); it would need joining'
        cells = sorted((float(r.col_agi_lo), float(r.col_agi_hi), float(r.value), r.flag) for r in cols.itertuples())
        out[row] = (float(tot.value.iloc[0]), cells)
    assert sorted(out) == sorted(TG.T16_ROWS[1:]), f'Table 1.6 {year}: age rows {sorted(out)}'
    return out


def published_12_agi(soi, year, cells, stats):
    """Table 1.2's returns by filing status within AGI groups (targets/soi.csv's Table 1.2 rows): each group sums the
    published cells inside it, the groups merged where a published cell of any of the entry's statuses crosses an edge
    (merge_groups), as Table 1.4's groups are."""
    assert stats == ['units'], 'Table 1.2 by AGI: returns only'
    four = {'single': 'single', 'joint': 'joint', 'separate': 'separate', 'head of household': 'head_of_household'}
    (gname,) = {c['agi'] for c in cells}
    g = CAT['groupings'][gname]
    groups = [(-np.inf if lo is None else float(lo), np.inf if hi is None else float(hi), lab) for lo, hi, lab in g['edges']]
    st_of = {}
    for c in cells:
        (fname,) = [n for n in c['parts'] if CAT['groupings'][n]['variable'] == 'filing_status']
        st_of[c['label']] = four[c['parts'][fname]]
    pc = {st: TG.published_cells(soi, year, 'returns', 'count', '1.2', st) for st in set(st_of.values())}
    groups = merge_groups(groups, [(lo, hi, '') for cl in pc.values() for lo, hi, _ in cl])
    rows = []
    for c in cells:
        st = st_of[c['label']]
        cl = pc[st]
        for lo, hi, lab in groups:
            inside = [v for l_, h_, v in cl if l_ >= lo and h_ <= hi]
            assert inside, f'Table 1.2 {year}, {st}: no published cell in {lab}'
            spec = {**c, 'label': f'{lab}, {c["label"]}'}
            rows.append({'cell': spec['label'], 'cell_spec': spec, 'agi': (lo, hi), 'statistic': 'units',
                         'value': float(sum(inside)),
                         'source': f'soi 1.2 {year}: returns of {st}, {len(inside)} AGI cell(s) summed'})
    return rows


def published_16_agi(year, cells, stats):
    """Rows of returns by the primary filer's age range (26 and over) within AGI groups, Table 1.6's All returns
    panel. Each AGI group sums the published columns inside it (Table 1.6's columns are Table 1.4's brackets, so the
    standard grid's edges fall on them). Where a row's columns do not add to its own all-AGI total, the run of adjacent
    cells it flags 'combined' is the misprint: the groups that run touches are merged for every age (as a published
    cell crossing an edge merges groups, merge_groups), and in the misprinted row the merged group is the row's total
    less its other columns. Every row's groups must then add to its total."""
    assert stats == ['units'], 'Table 1.6 by AGI: returns only'
    t16 = table_16_by_agi(year)
    (gname,) = {c['agi'] for c in cells}
    g = CAT['groupings'][gname]
    groups = [(-np.inf if lo is None else float(lo), np.inf if hi is None else float(hi), lab) for lo, hi, lab in g['edges']]
    bad = {}
    for row, (tot, cl) in t16.items():
        if abs(sum(v for _, _, v, _ in cl) - tot) <= T16_ROW_SLACK:
            continue
        runs, cur = [], []
        for lo, hi, v, flag in cl:
            if flag == 'combined':
                cur.append((lo, hi))
            else:
                runs, cur = runs + ([cur] if len(cur) > 1 else []), []
        runs += [cur] if len(cur) > 1 else []
        assert len(runs) == 1, f'Table 1.6 {year}, {row}: AGI columns add to {sum(v for _, _, v, _ in cl):,.0f}, the ' \
                               f'total is {tot:,.0f}, and {len(runs)} runs of adjacent combined cells: a decision is needed'
        bad[row] = (runs[0][0][0], runs[0][-1][1])
        common.say(f'Table 1.6 {year}, {row}: AGI columns add to {sum(v for _, _, v, _ in cl):,.0f} against the '
                   f'total {tot:,.0f}; the combined cells from {bad[row][0]:,.0f} to {bad[row][1]:,.0f} are taken as the '
                   'total less the other columns')
    groups = merge_groups(groups, [(lo, hi, row) for row, (lo, hi) in bad.items()])
    rows = []
    for c in cells:
        (aname,) = [n for n in c['parts'] if CAT['groupings'][n]['variable'] == 'age_range']
        (k,) = [val for val, lab in CAT['groupings'][aname]['values'] if lab == c['parts'][aname]]
        row = TG.T16_ROWS[k]
        tot, cl = t16[row]
        vals = []
        for lo, hi, lab in groups:
            inside = [v for l_, h_, v, _ in cl if l_ >= lo and h_ <= hi]
            assert all((l_ >= lo and h_ <= hi) or h_ <= lo or l_ >= hi for l_, h_, _, _ in cl), \
                f'Table 1.6 {year}, {row}: a published column crosses the edge of {lab}'
            if row in bad and lo <= bad[row][0] and bad[row][1] <= hi:
                v = tot - sum(v for l_, h_, v, _ in cl if not (l_ >= lo and h_ <= hi))
                src = f'soi 1.6 {year}, All returns, {row}: the all-AGI total less the other AGI columns (the ' \
                      'published cells here are misprinted)'
            else:
                v = float(sum(inside))
                src = f'soi 1.6 {year}, All returns, {row}: ' + ('one AGI column' if len(inside) == 1 else
                                                                  f'{len(inside)} AGI columns summed')
            vals.append(v)
            spec = {**c, 'parts': {**c['parts'], gname: lab}, 'label': f'{lab}, primary {c["parts"][aname]}'}
            rows.append({'cell': spec['label'], 'cell_spec': spec, 'agi': (lo, hi), 'statistic': 'units', 'value': v,
                         'source': src})
        assert abs(sum(vals) - tot) <= T16_ROW_SLACK, \
            f'Table 1.6 {year}, {row}: the AGI groups add to {sum(vals):,.0f}, the total is {tot:,.0f}'
    return rows


_CPS_YEARS = None


def cps_years():
    """The tax years with CPS units in the training table (the CPS's years: 2015, 2017, ..., 2023)."""
    global _CPS_YEARS
    if _CPS_YEARS is None:
        t = pd.read_parquet(common.TABLE, columns=['source', 'year'])
        _CPS_YEARS = sorted(int(y) for y in t.loc[t.source == 'cps', 'year'].unique())
    return _CPS_YEARS


def cps_neighbours(year):
    """For a year with no CPS units (an even year): the nearest CPS years either side, (lo, hi). A year with CPS
    units, or one outside their range, is an error: the caller handles those."""
    have = cps_years()
    lo, hi = max([y for y in have if y < year], default=None), min([y for y in have if y > year], default=None)
    assert year not in have and lo is not None and hi is not None, \
        f'{year}: no CPS units and no CPS year on both sides (CPS years {have})'
    return lo, hi


def taxdata_nonfiler_rows(year, cells, v, stats):
    """An item of the non-filers in the pipeline's tax data for the year (data/tax_data_<year>.parquet, the CPS units
    weighted by their chance of not filing, their amounts in PUF terms before the training's fill-in): its amount, or
    the units with it (statistic count). A year with no tax data file takes the geometric mean of the CPS years either
    side, as the non-filer count does. Cells all."""
    assert cells == [group_cells('all')[0]], 'tax data non-filers: cells all only'
    def one(y):
        t = pd.read_parquet(f'{common.DATA}/tax_data_{y}.parquet')
        t = t[t['filer'] == 0]
        x = record_value(t, v); w = t['weight'].to_numpy(float)
        assert np.isfinite(x).all() and np.isfinite(w).all(), f'tax data {y}: {v} or weight missing on a non-filer'
        return {'amount': float((w * x).sum()), 'count': float(w[x != 0].sum())}
    if os.path.exists(f'{common.DATA}/tax_data_{year}.parquet'):
        vals, how = one(year), f'tax data {year}'
    else:
        lo, hi = cps_neighbours(year)
        a, b = one(lo), one(hi)
        vals = {k: float(np.sqrt(a[k] * b[k])) if a[k] > 0 and b[k] > 0 else (a[k] + b[k]) / 2 for k in a}
        how = f'geometric mean of the tax data of {lo} and {hi}'
    return [{'cell': 'all', 'statistic': st, 'value': vals[st],
             'source': f'{how}: non-filers (CPS units weighted by their chance of not filing), {v}, {st}, before the fill-in'}
            for st in stats]


def nonfiler_count(year):
    """The CPS's expected non-filers in the year: the sum over its units of weight x (1 - filer_p), the same count
    the generated non-filer batch is sized by (generate.cps_expected_nonfilers). A year with no CPS units (an even
    year) takes the geometric mean of the nearest CPS years either side (cps_neighbours), as the start's scaling does
    (start.start_nonfilers); its targets.yaml gives the count a judgment error instead of identity."""
    t = pd.read_parquet(common.TABLE, columns=['source', 'year', 'weight', 'filer_p'])
    t = t[t.source == 'cps']
    if year > max(cps_years()):   # past the last CPS year: grown with the population
        from shared import growth as GR
        last = max(cps_years())
        return nonfiler_count(last) * GR.population(year) / GR.population(last)
    if year not in set(t['year']):
        lo, hi = cps_neighbours(year)
        return float(np.sqrt(nonfiler_count(lo) * nonfiler_count(hi)))
    t = t[t.year == year]
    assert len(t), f'no CPS units for {year}'
    return float((t['weight'] * (1 - t['filer_p'])).sum())


def filing_checks(year):
    path = f'{common.DATA}/filing/checks/{year}_nonfilers_vs_targets.csv'   # the filing stage's checks
    return pd.read_csv(path).set_index('band')['target_heads_spouses'], path


def cex_mix_rows(year, cells, v, stats):
    """The CEX's statistics over the year's mix of CEX years (survey_mix.py): each year's units
    in the target year's dollars (the maps' factors), the statistic computed on each year alone, then pooled, value =
    the sum over years of share x the year's value. Only averages: an average does not depend on a year's number of
    units. The standard error is pooled in errors.consumption_errors (variance = sum of share^2 x the year's)."""
    from shared import survey_mix
    assert stats == ['average'], f'CEX {year}: the year mix is built for averages, not {stats}'
    per_year = []
    for y, share in survey_mix.mix('cex', year):
        per_year.append((y, share, _survey_averages('cex', y, cells, v, survey_frame_grown(y))))
    rows = []
    for k in range(len(per_year[0][2])):
        base = dict(per_year[0][2][k])
        base['value'] = sum(share * r[k]['value'] for _, share, r in per_year)
        base['source'] = (f'cex over the year mix {", ".join(f"{y} {100 * sh:.2f}%" for y, sh, _ in per_year)}, all units, '
                          f'each year\'s spending in {common.TARGET_YEAR} dollars by NIPA PCE per return '
                          f'(survey_mix.py); pooled: sum of share x the year\'s average')
        rows.append(base)
    return rows


_GROWN_CEX = {}


def survey_frame_grown(y):
    """The CEX year y's units with spending in the target year's dollars (read once)."""
    if y not in _GROWN_CEX:
        from shared import survey_mix
        u = survey_mix.grown_units('cex', y)
        assert u['weight'].notna().all() and (u['weight'] > 0).all(), f'CEX {y}: a unit with no positive weight'
        _GROWN_CEX[y] = u
    return _GROWN_CEX[y]


_CPS_OT = {}


def cps_overtime_units(y):
    """The CPS year y's tax units for the overtime targets (read once): survey_mix.grown_units('cps', y), every dollar
    column in the target year's dollars (the overtime slots and the translated wages by SOI's wages per return, the
    maps' factors), restricted to the units asked the earnings questions.

    Universe. Overtime pay needs weekly pay and hours, asked only of the households in months 4 and 8 of the CPS's
    main sample (not of the ASEC's extra sample); on every other unit, and on the few asked units with a worker's pay
    missing, build_cps.R leaves it missing. Those units are left out here and counted in the printout; each statistic
    is an average over the asked units in the cell, with their own ASEC weights (the head's ASECWT), so it does not
    depend on how many units were asked. Dependent filers are folded into the unit that claims them (build_cps.R), as
    in the CEX, so the population matches the file's units other than dependent filers.
    Columns added: ot = ot_primary + ot_spouse (the unit's overtime, as the file's ot = ot1 + ot2) and wages =
    wages_translated (the tax data's wages at the same percentile among earners, so the wage groups mean the same as
    on the file)."""
    if y not in _CPS_OT:
        from shared import survey_mix
        u = survey_mix.grown_units('cps', y)
        assert u['weight'].notna().all() and (u['weight'] > 0).all(), f'CPS {y}: a unit with no positive weight'
        na1, na2 = u['ot_primary'].isna().to_numpy(), u['ot_spouse'].isna().to_numpy()
        assert (na1 == na2).all(), f'CPS {y}: overtime missing on one adult of a unit only'
        assert 'wages' not in u, f'CPS {y}: the unit file already has a column wages'
        w = u['weight'].to_numpy(float)
        common.say(f'CPS {y} overtime targets: {int((~na1).sum()):,} of {len(u):,} units asked the earnings questions '
                   f'({100 * w[~na1].sum() / w.sum():.1f}% of the weight); the other {int(na1.sum()):,} have overtime '
                   'missing (not asked, or a worker\'s pay missing) and are left out')
        a = u[~na1].copy()   # the universe: the units asked the earnings questions (see above)
        a['ot'] = a['ot_primary'].to_numpy(float) + a['ot_spouse'].to_numpy(float)
        a['wages'] = a['wages_translated'].to_numpy(float)
        _CPS_OT[y] = a
    return _CPS_OT[y]


def cps_mix_rows(year, cells, v, stats):
    """The CPS overtime targets over the year's mix of CPS years (survey_mix.py), as the CEX's
    (cex_mix_rows): each year's asked units in the target year's dollars (cps_overtime_units), the average computed on
    each year alone, value = the sum over years of share x the year's average. The standard error is pooled in
    errors.overtime_errors from the CPS's replicate weights (variance = sum of share^2 x the year's)."""
    from shared import survey_mix
    assert stats == ['average'], f'CPS {year}: the year mix is built for averages, not {stats}'
    per_year = []
    for y, share in survey_mix.mix('cps', year):
        per_year.append((y, share, _survey_averages('cps', y, cells, v, cps_overtime_units(y))))
    rows = []
    for k in range(len(per_year[0][2])):
        base = dict(per_year[0][2][k])
        base['value'] = sum(share * r[k]['value'] for _, share, r in per_year)
        base['source'] = (f'cps over the year mix {", ".join(f"{y} {100 * sh:.2f}%" for y, sh, _ in per_year)}, the '
                          'units asked the earnings questions, weighted; each year\'s overtime and wages in '
                          f'{common.TARGET_YEAR} dollars by SOI wages per return (survey_mix.py); pooled: sum of share x '
                          'the year\'s average; by year: ' + ', '.join(f'{y} {r[k]["value"]:,.6g}' for y, _, r in per_year))
        rows.append(base)
    return rows


_CPS_AGE = {}


def cps_age_units(y):
    """The CPS year y's tax units of the training table, filers and non-filers, for the counts of units by
    the primary's age (catalog source cps units): record_id, weight, age_range and age1, the primary filer's age in years
    (the table stores age plus 1; ages 80 and over held at 80, as on the file). Dependent filers are folded into the
    unit that claims them, as in every survey, so the count matches the file's units other than dependent filers."""
    if y not in _CPS_AGE:
        t = pd.read_parquet(common.TABLE, columns=['source', 'year', 'record_id', 'weight', 'age_range', 'age1'])
        u = t[(t.source == 'cps') & (t.year == y)].reset_index(drop=True)
        assert len(u), f'no CPS units for {y} in the training table'
        na = u.isna().sum()
        assert na.sum() == 0, f'CPS {y}: missing values\n{na[na > 0]}'
        assert (u['weight'] > 0).all(), f'CPS {y}: a unit with no positive weight'
        assert (u['age_range'] < common.ADULT_RANGES).all(), f'CPS {y}: a dependent filer among the units'
        assert (u['age1'] > 0).all(), f'CPS {y}: {int((u["age1"] <= 0).sum())} units with no primary filer age'
        u['age1'] = u['age1'].to_numpy(float) - 1
        _CPS_AGE[y] = u
    return _CPS_AGE[y]


def cps_age_rows(year, cells, v, stats):
    """The number of tax units, filers and non-filers, in each cell of the primary's age, from the CPS's units over the
    year's CPS year mix (prior "cps items"; 2022: 2021 and 2023, half each): value = sum over years of
    share x the year's count. The standard error is pooled in errors.cps_age_errors from the CPS's replicate weights."""
    from shared import survey_mix
    assert stats == ['units'] and v is None, f'CPS units {year}: counts of units only, not {stats} of {v}'
    rows = []
    mix = survey_mix.mix('cps', year)
    for c in cells:
        assert not c['filters'] and c.get('rank') is None and c.get('band') is None and c['line'] is None and c['agi'] is None, \
            f'CPS units {year}, cell {c["label"]}: only ranges of the primary\'s age are built'
        per = []
        for y, share in mix:
            u = cps_age_units(y)
            m = np.ones(len(u), bool)
            if c.get('range'):
                rv, lo, hi = c['range']
                xg = record_value(u, rv)
                m &= (xg >= lo) & (xg < hi)
            assert m.any(), f'CPS {y}, cell {c["label"]}: no units'
            per.append((y, share, float(u['weight'].to_numpy(float)[m].sum()), int(m.sum())))
        rows.append({'cell': c['label'], 'cell_spec': c, 'statistic': 'units', 'value': sum(s * n for _, s, n, _ in per),
                     'line': None, 'line_variable': '', 'rank_lines': (np.nan, np.nan), 'line_hi': None,
                     'source': 'CPS tax units of the training table, filers and non-filers, over the year mix '
                               + ', '.join(f'{y} {100 * s:.2f}%: {n:,.0f} units ({k:,} sample units)' for y, s, n, k in per)
                               + '; pooled: sum of share x the year\'s count'})
    return rows


def survey_rows(source, year, cells, v, stats, population=None):
    """The statistic computed on the survey's own units in each cell. The SCF's come from the year's mix of SCF
    waves, with their standard errors (scf_targets.py), on the population's units (its net worth ceiling, if any)."""
    if source == 'scf':
        from reweight import scf_targets
        return scf_targets.rows(cells, v, stats, year, ceiling=net_worth_ceiling(population))
    assert net_worth_ceiling(population) is None, f'{source}: a net worth ceiling on a survey other than the SCF'
    if source == 'cex':
        return cex_mix_rows(year, cells, v, stats)
    if source == 'cps':
        return cps_mix_rows(year, cells, v, stats)
    assert source == 'cps units', f'survey source {source}: no targets are built from it'
    return cps_age_rows(year, cells, v, stats)


def _survey_averages(source, year, cells, v, u):
    """The average of v over one year's units u of a survey in each cell (the CEX's and the CPS's; cex_mix_rows and
    cps_mix_rows pool the years)."""
    path = f'{common.DATA}/{source}_units_{year}_all.parquet'
    if 'age_range' in u:
        assert (u['age_range'] < common.ADULT_RANGES).all(), f'{path}: a dependent filer among the survey units'
    w = u['weight'].to_numpy(float)
    x = record_value(u, v)
    rows = []
    for c in cells:
        m = np.ones(len(u), bool)
        for col, val in c['filters']:
            m &= (u[col] == val).to_numpy()
        assert c.get('rank') is None and c.get('band') is None, \
            f'{source} {year}, cell {c["label"]}: income rank groups and bands are built for the SCF only (scf_targets.py)'
        if c.get('range'):
            # a dollar range of a variable other than AGI, low inclusive (the unit's wages for the overtime targets)
            rv, lo, hi = c['range']
            xg = record_value(u, rv)
            assert np.isfinite(xg).all(), f'{source} {year}: {rv} missing on {int((~np.isfinite(xg)).sum())} units'
            m &= (xg >= lo) & (xg < hi)
        assert m.any(), f'{source} {year}, cell {c["label"]}: no survey units'
        assert c['line'] is None, f'{source} {year}, cell {c["label"]}: an average has no line'
        rows.append({'cell': c['label'], 'cell_spec': c, 'statistic': 'average',
                     'value': float(np.average(x[m], weights=w[m])), 'line': None, 'line_variable': '',
                     'rank_lines': (np.nan, np.nan), 'line_hi': None})
    return rows


# ---- income rank groups (the income x net worth grid) -----------------------------------------------------
def rank_dollar_lines(x, w, lo, hi):
    """The dollar lines of the lo-th and hi-th weighted percentiles of x (common.wquantile: the smallest x whose
    cumulative weight share reaches the share); 0 and 100 are no line."""
    a = -np.inf if lo <= 0 else float(common.wquantile(x, w, [lo / 100])[0])
    b = np.inf if hi >= 100 else float(common.wquantile(x, w, [hi / 100])[0])
    return a, b


FILE_RANK_LINES = {}   # (variable, population, percentile) -> the file's dollar line, as last set by conditions()


def pipeline_rows(year, cells, v, stats):
    assert cells == [group_cells('all')[0]] and stats in (['units'], ['amount'])
    if year > max(cps_years()):   # past the last CPS year: the last year's grown with the population
        from shared import growth as GR
        last = max(cps_years())
        f = GR.population(year) / GR.population(last)
        if v is None:
            return [{'cell': 'all', 'statistic': 'units', 'value': nonfiler_count(year),
                     'source': f'past the last CPS year: the filing stage\'s expected non-filers in {last} times the '
                               f'growth of the Social Security area population, {f:.4f}'}]
        return [{'cell': 'all', 'statistic': 'amount', 'value': cps_nonfiler_people(last, v) * f,
                 'source': f'past the last CPS year: the filing stage\'s people among the expected non-filers in {last}, '
                           f'counted as the file counts them, times the growth of the Social Security area population, '
                           f'{f:.4f}'}]
    if year not in cps_years():   # an even year: the geometric mean of the CPS years either side
        lo, hi = cps_neighbours(year)
        if v is None:
            return [{'cell': 'all', 'statistic': 'units', 'value': nonfiler_count(year),
                     'source': f'no CPS units for {year}: the geometric mean of the filing stage\'s expected non-filers '
                               f'in {lo} ({nonfiler_count(lo):,.0f}) and {hi} ({nonfiler_count(hi):,.0f})'}]
        a, b = cps_nonfiler_people(lo, v), cps_nonfiler_people(hi, v)
        return [{'cell': 'all', 'statistic': 'amount', 'value': float(np.sqrt(a * b)) if a > 0 and b > 0 else (a + b) / 2,
                 'source': f'no CPS units for {year}: the geometric mean of the filing stage\'s people among the '
                           f'expected non-filers in {lo} ({a:,.0f}) and {hi} ({b:,.0f}), counted as the file counts them'}]
    if v is None:
        return [{'cell': 'all', 'statistic': 'units', 'value': nonfiler_count(year),
                 'source': f'filing stage {year}: CPS units of the training table, sum of weight x (1 - filer_p)'}]
    t, path = filing_checks(year)
    band = variable_def(v)['filing_band']
    assert isinstance(band, str), f'{v}: filing_band {band!r} is not text (quote it in catalog.yaml)'
    return [{'cell': 'all', 'statistic': 'amount', 'value': cps_nonfiler_people(year, v),
             'source': f'filing stage {year}: CPS units of the training table, sum of weight x (1 - filer_p) x the '
                       f'people counted as the file counts them (record_value); the calibration\'s own number, by the '
                       f'head\'s band and every child, {1e6 * t[band]:,.0f} ({os.path.relpath(path, common.OUT)}, band '
                       f'{band})'}]


def outside_rows(src, year, cells, v, stats, e):
    """A target from outside information that comes only as a change: the start year's own file's total (its final
    weights, its dollars of that year) times the outside series' growth from that year to this one (Z.1 household net
    worth, growth.py). Only for a year whose start is an earlier year's file (from_year); cells all. The projection
    years' own sources are projection_steps.outside_rows, CBO's counts cbo_count_rows."""
    if src == 'cbo counts':
        return cbo_count_rows(year, v, stats, e)
    if src in ('projection persons', 'projection returns', 'projection items', 'projection filing season'):
        from scale import projection_steps as PS
        assert stats == ['amount'], f'entry {e["id"]}: projection targets are amounts'
        return PS.outside_rows(src, year)
    assert src == 'z1 household net worth', f'outside source {src}'
    assert cells == [group_cells('all')[0]], f'entry {e["id"]}: an outside target has cells all'
    spec = start_spec(year)
    assert spec['mode'] == 'from_year', f'entry {e["id"]}: an outside target needs a start from an earlier year\'s file'
    s = spec['year']
    path = f"{common.OUT}/tilt/{spec['run']}/file_{s}.parquet"
    df = pd.read_parquet(path)
    w = df['weight'].to_numpy(float)
    pop = population(df, e['population'])
    from shared import growth as GR
    nw = GR._csv('z1_household_net_worth.csv')
    f = float(nw.loc[year]) / float(nw.loc[s])
    out = []
    for st in stats:
        assert st in ('units', 'amount'), f'entry {e["id"]}: outside targets are units or amounts'
        lev = float(w[pop].sum()) if st == 'units' else float((w[pop] * record_value(df, v)[pop]).sum())
        out.append({'cell': 'all', 'statistic': st, 'value': lev * f,
                    'source': f'{src}: {e["outside_line"]!r}, {s} to {year}, growth {f:.4f}, on the {s} file\'s own level '
                              f'{lev:,.0f} ({os.path.relpath(path, common.OUT)})'})
    return out


def cbo_count_rows(year, v, stats, e):
    """CBO's published counts of returns (all returns, itemizers, returns affected by the AMT), anchored to our own
    level like every other projection target: the start file's count (its final weights; the count as the targets
    count it: filers, and for itemizers and AMT returns the filers with a nonzero calculator value) times CBO's growth
    in that count from the start year to this one. After settings tilt.projection.cbo_last_year no CBO number is
    used: no row (the entry is then left out and the file's count follows from the demographic cells). The standard
    error is the entry's."""
    from scale import projection_demographics as PD
    from scale import projection_growth as PG
    series = e['outside_line']
    if year > PG.cbo_last_year():
        return []
    assert len(stats) == 1
    spec = start_spec(year)
    assert spec['mode'] == 'from_year', f'entry {e["id"]}: a CBO count target needs a start from an earlier year\'s file'
    s = spec['year']
    path = f"{common.OUT}/tilt/{spec['run']}/file_{s}.parquet"
    df = pd.read_parquet(path)
    w = df['weight'].to_numpy(float)
    pop = population(df, e['population'])
    if v is None:
        member = pop
    else:
        d = variable_def(v)
        xc = record_value(df, d.get('count_variable') or v)
        assert np.isfinite(xc).all(), f'entry {e["id"]}: {v} missing on {int((~np.isfinite(xc)).sum())} records of the {s} file'
        member = pop & (xc != 0)
    lev = float(w[member].sum())
    a, b = PD.cbo_count(s, series), PD.cbo_count(year, series)
    return [{'cell': 'all', 'statistic': stats[0], 'value': lev * b / a,
             'source': f'CBO February 2026 individual income tax projections, {series}: the {s} file\'s own count {lev:,.0f} '
                       f'({os.path.relpath(path, common.OUT)}) times CBO\'s growth of the count from {s} to {year}, '
                       f'{b / 1e6:.3f} / {a / 1e6:.3f} million = {b / a:.5f} (CBO\'s level in {year}: {b:,.0f})',
             'cell_spec': {'parts': {}, 'filters': [], 'label': 'all', 'agi': None, 'line': None}}]


_CPS_AGES = {}


def check_nonfiler_bands(df, rows):
    """Every non-filer head and spouse in the file (filer 0; a member with an age) must fall in exactly one of the
    non-filer people targets' bands (rows of kind 'non-filer people'). A head or spouse in no band (a 17-year-old:
    adults are counted from 18, children under 17) or in two would let the tilt move the non-filer count through
    records the people targets cannot see. Fails loudly, with the ages that break it."""
    people = rows.loc[rows['kind'] == 'non-filer people', 'variable'].unique()
    if not len(people):
        return
    nf = (df['filer'] == 0).to_numpy()
    for m in ('head', 'spouse'):
        a = df[AGE_COLUMNS[m]].to_numpy(float)[nf]
        has = a >= 0                                      # -1: no such member
        n = np.zeros(int(nf.sum()))
        for v in people:
            p = variable_def(v)['people']
            if m in p['members']:
                n += (a >= p['from']) & (a < (np.inf if p['to'] is None else p['to']))
        bad = has & (n != 1)
        assert not bad.any(), (f'{int(bad.sum())} non-filer {m}s fall in {sorted(set(n[bad].astype(int)))} of the '
                               f'non-filer people bands, not exactly one; their ages: '
                               f'{pd.Series(a[bad]).value_counts().sort_index().to_dict()}')
    common.say(f'non-filer people bands: every non-filer head and spouse falls in exactly one ({int(nf.sum()):,} '
               'non-filer records)')


def cps_nonfiler_people(year, v):
    """The people of variable v (a catalog "people" variable) among the CPS's expected non-filers in the year: the sum
    over its units in the training table of weight x (1 - filer_p) x the unit's count of them by record_value, the
    rule the file's records are counted by, so target and file count people one way. Heads and spouses by their own
    exact age; children under 17 among the head, the spouse and the three dependents the table carries (the youngest
    three of those kept under the PUF's caps). The filing stage's calibration hit its own numbers, with heads and
    spouses in the band of the unit's CPS head and with every child; the file can do neither (its primary filer is
    not always the CPS head, and it carries at most three dependents). In 2019 the two ways differ by up to 0.09
    million people in a band, and by 0.20 million children (2.16 million against 1.96 million)."""
    if year not in _CPS_AGES:
        t = pd.read_parquet(common.TABLE, columns=['source', 'year', 'weight', 'filer_p'] + list(AGE_COLUMNS.values()))
        t = t[(t.source == 'cps') & (t.year == year)]
        assert len(t), f'no CPS units for {year}'
        assert not t.isna().any().any(), f'CPS {year}: a missing weight, chance of filing or age in the table'
        # the table's ages are categories, 0 for none and a + 1 for age a (build_table.AGE_SLOTS); the file's are
        # years, -1 for none (dollars.py)
        ages = pd.DataFrame({c: np.where(t[c].to_numpy(float) > 0, t[c].to_numpy(float) - 1, -1.0)
                             for c in AGE_COLUMNS.values()})
        _CPS_AGES[year] = (ages, (t['weight'] * (1 - t['filer_p'])).to_numpy(float))
    ages, q = _CPS_AGES[year]
    return float(q @ record_value(ages, v))


# ---- the tax-unit rules a survey value was computed on -------------------------------------------------------------
def _commit():
    try:
        h = subprocess.run(['git', '-C', HERE, 'rev-parse', '--short', 'HEAD'], capture_output=True, text=True,
                           check=True).stdout.strip()
        dirty = subprocess.run(['git', '-C', HERE, 'status', '--porcelain', '--', '.'], capture_output=True,
                               text=True, check=True).stdout.strip()
        return h + (' with uncommitted changes' if dirty else '')
    except (OSError, subprocess.CalledProcessError):
        return 'unknown'


_COMMIT = None


def tax_unit_rules(source):
    """The rules a source's units were built with, the settings that select their variant, and the code commit (the
    rules as the settings and code name them now; the units on disk were built by the run's own scf, cex and filing
    stages)."""
    global _COMMIT
    s = CAT['sources'][source]
    if 'tax_unit_rules' not in s:
        return ''
    if _COMMIT is None:
        _COMMIT = _commit()
    r = CAT['tax_unit_rules'][s['tax_unit_rules']]
    picks = []
    for name in r['settings'][source]:
        sec, key = name.split('.')
        picks.append(f'{name} = {common.SETTINGS.get(sec, {}).get(key, "(not set)")}')
    return (f'{s["tax_unit_rules"]}: {r["module"]} ({r["what"]}), units by {", ".join(s["built_by"])}; '
            f'{"; ".join(picks)}; code commit {_COMMIT}')


# ---- published grids ----------------------------------------------------------------------------------------
def published_grid(source, year, cells, v, stats):
    """A tidy published table, selected by catalog item, measure and group labels.

    Group labels map to one or several published panels (several panels are added). Rank cells map to published
    cumulative groups. A cell of a grouping with published lines (catalog cutoffs: published; Table 4.3's top groups)
    is the returns with AGI at or above the group's published cutoff for the year (catalog cutoff_item), in nominal
    dollars of the year; a missing cutoff is an error. Errors borrow errors.py's nearest Table 1.4 cells using the
    paired holder count. A published AGI cutoff at or above the catalog's full-sample line gets errors.FULL_SAMPLE_SE
    instead.
    """
    from reweight import errors
    definition = CAT['sources'][source]
    t = pd.read_csv(os.path.join(common.RAW['soi_tables'], definition['file']), keep_default_na=False, dtype=str)
    t = t[(t.tax_year == str(year)) & (t.table == str(definition['table']))]
    donors, donor_year = errors.soi_cells(year)
    spec = definition['units'] if v is None else variable_def(v)['published'][source]
    out = []
    for c in cells:
        selected = t
        cutoff = None
        for grouping, label in c['parts'].items():
            g = CAT['groupings'][grouping]
            selected = selected[selected[g['published_column']].isin(g['published_labels'][label])]
        if definition.get('cutoff_item'):
            cut = selected[selected.item == definition['cutoff_item']]
            assert len(cut) == 1, f'{source} {c["label"]}: expected one AGI cutoff'
            if cut.iloc[0].value:
                cutoff = float(cut.iloc[0].value)
        if c.get('published_line'):
            # the group is the returns at or above SOI's published AGI cutoff for the year
            assert definition.get('cutoff_item'), f'{source}: no cutoff_item for the published lines of {c["label"]}'
            assert cutoff is not None and np.isfinite(cutoff) and cutoff > 0, \
                f'{source} {year} {c["label"]}: no published AGI cutoff ({definition["cutoff_item"]})'
        def value(measure, item=None):
            # an item SOI names differently in some years (catalog item_aliases: salaries and wages is "total wages"
            # in Table 4.3 from tax year 2022) is read under the name the year publishes
            name = item or spec['item']
            for alt in [name] + list(CAT.get('item_aliases', {}).get(name, [])):
                if ((selected.item == alt) & (selected.measure == measure)).any():
                    name = alt
                    break
            z = selected[(selected.item == name) & (selected.measure == measure)]
            expected = 1
            for grouping, label in c['parts'].items():
                expected *= len(CAT['groupings'][grouping]['published_labels'][label])
            assert len(z) == expected, f'{source} {c["label"]} {spec["item"]} {measure}: {len(z)} cells, expected {expected}'
            assert z.value.ne('').all(), f'{source} {c["label"]} {spec["item"]}: {int(z.value.eq("").sum())} missing values: {z.flag.tolist()}'
            scales = z.unit.map({'returns': 1, '$ thousands': 1000, 'whole dollars': 1})
            assert scales.notna().all(), f'{source}: unknown units {z.unit.tolist()}'
            x = pd.to_numeric(z.value) * scales
            if 'net loss' in name and measure != spec.get('count', 'returns'):
                # Table 4.3 prints a net loss's amount as a negative number in some years and as a positive one in
                # others; the file's loss variables are sizes, so the published amount is read as its size
                x = x.abs()
            return float(x.sum())
        for st in stats:
            val = value(spec[st])
            count = value(spec.get('count', spec.get('units', 'returns')), spec.get('returns_item'))
            if cutoff is not None and cutoff >= definition.get('full_sample_above', np.inf):
                se, kind = errors.FULL_SAMPLE_SE * abs(val), 'judgment'
                reason = (f'{100 * errors.FULL_SAMPLE_SE:g}% of value (errors.full_sample_se): published AGI cutoff '
                          f'${cutoff:,.0f} is in the fully sampled AGI range; allowance for definition differences')
            elif c.get('published_line') and st in ('count', 'units'):
                # the count above a published AGI cutoff borrows only from Table 1.4's count cells in the AGI classes
                # at or above the cutoff (a class reaching above it counts), as Table 1.6's age-by-AGI cells do
                # (errors.soi_errors): SOI samples the top far more densely than the middle
                same = donors[~donors.total_row & (donors.hi > cutoff)]
                cv, what = errors.borrow_cv(same, count=abs(val))
                assert np.isfinite(cv), f'{source} {year} {c["label"]}: no Table 1.4 count cell with a CV above ${cutoff:,.0f}'
                se, kind = abs(val) * cv / 100, 'soi_borrowed'
                reason = (f'CV {cv:g}% from the nearest Table 1.4 count cells in AGI classes reaching above the cutoff '
                          f'${cutoff:,.0f} ({donor_year}): {what}')
            else:
                cv, what = errors.borrow_cv(donors, count=abs(val)) if st in ('count', 'units') else \
                    errors.borrow_cv(donors, amount=abs(val), returns=count)
                se, kind = abs(val) * cv / 100, 'soi_borrowed'
                reason = f'CV {cv:g}% from nearest Table 1.4 cells ({donor_year}): {what}'
            row = {'cell': c['label'], 'cell_spec': c, 'statistic': st, 'value': val,
                   'source': f'{source} {year}: {spec["item"]}, {c["label"]}; {definition["file"]}',
                   'se': se, 'error_kind': kind, 'se_reason': reason}
            if c.get('published_line'):
                row['agi'] = (cutoff, np.inf)
                row['source'] += f'; AGI at or above SOI\'s published cutoff ${cutoff:,.0f}'
            out.append(row)
    return out


def _w2_groups(heading):
    """Each column's group in a W-2 table's heading row (merged cells carried right, footnote marks dropped)."""
    groups, g = [], None
    for c in heading[1:]:
        g = c.split(' [')[0].strip() if isinstance(c, str) and c.strip() else g
        groups.append(g)
    return groups


def _w2_less_dependents(definition, share, cell, st, published, returns, donors, donor_year):
    """A W-2 target's value and standard error: (value, se, se_reason). Where the population leaves dependent filers
    out (share, from w2_dependent_share or w2_deferral_dependent_share, is not None) and the cell is non-joint returns,
    the published value less the SIPP's dependent share of it (of the people for a count, of the dollars for an
    amount). The error borrows the CV of the nearest Table 1.4 cells (a count by its size; an amount by its size and
    its count of returns), and gap_share of the reduction is added as a definition gap (square root of the sum of
    squares)."""
    from reweight import errors
    gap_share = definition['dependent_share']['gap_share']
    reduction, gap_why = 0.0, ''
    if share is not None and cell['filters'] == [('married', 0)]:
        sh = share[1] if st == 'amount' else share[0]
        reduction = published * sh   # a non-joint return is one taxpayer, so its workers and its returns are the same count
        gap_why = (f'; less the dependent filers, {100 * sh:.1f}% ({share[2]}); plus '
                   f'{100 * gap_share:g}% of that reduction as a definition gap')
    val = published - reduction
    cv, what = errors.borrow_cv(donors, amount=val, returns=returns) if st == 'amount' else \
        errors.borrow_cv(donors, count=val)
    se = float(np.sqrt((val * cv / 100) ** 2 + (gap_share * reduction) ** 2))
    reason = f'CV {cv:g}% from nearest Table 1.4 cells ({donor_year}): {what}{gap_why}' + \
        (f'; published {published:,.0f}' if reduction else '')
    return val, se, reason


def _w2_table(path, sheet):
    """A W-2 statistics summary table (Table 4.B): {item label: {(column group, measure): value}}, the column groups
    read from the heading row (merged cells carried right, footnote marks dropped), money amounts in dollars."""
    import openpyxl
    rows = [list(r) for r in openpyxl.load_workbook(path, read_only=True, data_only=True)[sheet].iter_rows(values_only=True)]
    (hr,) = [i for i, r in enumerate(rows) if any(isinstance(c, str) and c.startswith('All taxpayers') for c in r)]
    assert any('thousands of dollars' in str(c) for r in rows[:hr] for c in r if c), f'{path}: amounts not in thousands'
    groups = _w2_groups(rows[hr])
    measures = [str(c).strip() if c is not None else None for c in rows[hr + 1][1:]]
    out = {}
    for r in rows[hr + 2:]:
        if not isinstance(r[0], str) or not r[0].startswith('Box'):
            continue
        cells = {}
        for gname, meas, val in zip(groups, measures, r[1:]):
            if gname is None or meas is None or val is None:
                continue
            num = float(val) if isinstance(val, (int, float)) else np.nan   # 'd' (withheld) or 'N/A'
            cells[(gname, meas)] = num * (1000.0 if meas == 'Amount' else 1.0)
        out[r[0].strip()] = cells
    return out


def w2_dependent_share(source, year):
    """The SIPP's share of dependents among the year's non-joint people with tips, over the year's mix of SIPP years
    (survey_mix.py): the sum over years of share x the year's share, for people and for tips
    (a share of the people or the dollars does not depend on a year's population). Returns (share of people, share of
    tips, description)."""
    from shared import survey_mix
    parts = [(y, s, w2_dependent_share_one(source, y)) for y, s in survey_mix.mix('sipp', year)]
    return (sum(s * r[0] for _, s, r in parts), sum(s * r[1] for _, s, r in parts),
            'pooled over the SIPP year mix, sum of share x the year\'s share; ' + '; '.join(
                f'{100 * s:.2f}% x [{r[2]}]' for _, s, r in parts))


def w2_dependent_share_one(source, year):
    """One SIPP year's share of dependents among the year's non-joint people with tips: (share of people, share of tips,
    description). Dependents are the people the shared tax-unit rules made dependents (persons file, dep_type);
    the other non-joint people with tips are the primary filers with tips of the SIPP's units that are not joint."""
    d = CAT['sources'][source]['dependent_share']
    p = pd.read_parquet(f"{common.OUT}/{d['persons'].format(year=year)}", columns=['weight', 'tips', 'dep_type'])
    u = pd.read_parquet(f"{common.OUT}/{d['units'].format(year=year)}",
                        columns=['weight', 'married', 'tips_primary_reported'])
    for name, x in [('persons', p[['weight', 'tips']]), ('units', u)]:
        assert x.notna().all().all(), f'{source}: SIPP {name} {year} with missing values'
    dep = p['dep_type'].fillna('').ne('').to_numpy() & (p['tips'] > 0).to_numpy()
    nj = (u['married'] == 0).to_numpy() & (u['tips_primary_reported'] > 0).to_numpy()
    n_dep, t_dep = float(p['weight'][dep].sum()), float((p['weight'] * p['tips'])[dep].sum())
    n_nj, t_nj = float(u['weight'][nj].sum()), float((u['weight'] * u['tips_primary_reported'])[nj].sum())
    why = (f'SIPP {year}: dependents with tips {n_dep / 1e6:.3f} million people ({int(dep.sum())} records), '
           f'${t_dep / 1e9:.2f} billion; other non-joint people with tips {n_nj / 1e6:.3f} million ({int(nj.sum())} '
           f'records), ${t_nj / 1e9:.2f} billion')
    return n_dep / (n_dep + n_nj), t_dep / (t_dep + t_nj), why


def published_w2(source, year, cells, v, stats, population):
    """Rows from SOI's W-2 summary table (catalog source with layout w2_summary): the variable's item (its
    'published' entry), in each cell's column group (the grouping's 'in <source>' labels); a count is the column's
    number of returns (or of taxpayers where the group has one per return), an amount its amount. Where the
    population leaves dependent filers out, the non-joint value is reduced by the SIPP's dependent share
    (w2_dependent_share), and gap_share of the reduction is added to the borrowed error (square root of the sum of
    squares)."""
    from reweight import errors
    definition = CAT['sources'][source]
    path = os.path.join(common.RAW['soi_w2'], definition['file'].format(year=year))
    table = _w2_table(path, definition['sheet'])
    item = variable_def(v)['published'][source]['item']
    assert item in table, f'{path}: no row {item!r}'
    donors, donor_year = errors.soi_cells(year)
    no_dependents = 'age_range < 6' in CAT['populations'][population]['rule']   # dependent filers left out
    share = w2_dependent_share(source, year) if no_dependents else None
    out = []
    for c in cells:
        ((gname, label),) = c['parts'].items()
        group = CAT['groupings'][gname][f'in {source}'][label]
        have = {m: x for (g, m), x in table[item].items() if g == group}
        count_col = 'Number of returns' if 'Number of returns' in have else 'Number of taxpayers'
        count, amount = have[count_col], have['Amount']
        assert np.isfinite(count) and np.isfinite(amount), f'{path}: {item}, {group}: withheld or missing'
        workers = have.get('Number of taxpayers', np.nan)
        for st in stats:
            published = {'count': count, 'amount': amount, 'workers': workers}[st]
            assert np.isfinite(published), f'{path}: {item}, {group}: no {st}'
            src = f'{source} {year}: {item}, {group}, {"amount" if st == "amount" else "number of taxpayers" if st == "workers" else count_col} ({path})'
            val, se, reason = _w2_less_dependents(definition, share, c, st, published, count, donors, donor_year)
            out.append({'cell': c['label'], 'cell_spec': c, 'statistic': st, 'value': val, 'source': src,
                        'se': se, 'error_kind': 'soi_borrowed', 'se_reason': reason})
    return out


# ---- W-2 Table 2.D: elective deferrals by AGI and return type -------------------------------------------
def _agi_bounds(label):
    """(lo, hi) of a W-2 table's AGI row label: 'No adjusted gross income', '$1 under $5,000', '$10,000,000 and over'."""
    s = label.replace(',', '').replace('$', '').strip()
    if s.lower().startswith('no adjusted gross income'):
        return -np.inf, 1.0
    if ' under ' in s:
        lo, hi = s.split(' under ')
        return float(lo), float(hi)
    assert s.endswith(' and over'), f'an AGI row label not understood: {label!r}'
    return float(s[:-len(' and over')]), np.inf


def _w2_by_agi(path, sheet):
    """A W-2 statistics table by size of AGI (Table 2.D): {(lo, hi, row label): {(column group, measure): value}},
    the column groups read from the heading row (merged cells carried right, footnote marks dropped), money amounts
    in dollars. Only the sheet's first table: its rows stop at the first row after the data that is not an AGI row
    (the sheet also holds Table 2.D.1 below)."""
    import openpyxl
    rows = [list(r) for r in openpyxl.load_workbook(path, read_only=True, data_only=True)[sheet].iter_rows(values_only=True)]
    hr = next(i for i, r in enumerate(rows) if any(isinstance(c, str) and c.startswith('All taxpayers') for c in r))
    assert any('thousands of dollars' in str(c) for r in rows[:hr] for c in r if c), f'{path}: amounts not in thousands'
    groups = _w2_groups(rows[hr])
    measures = [str(c).split(' [')[0].strip() if c is not None else None for c in rows[hr + 1][1:]]
    out, started = {}, False
    for r in rows[hr + 2:]:
        lab = r[0].strip() if isinstance(r[0], str) else ''
        is_agi = lab.startswith('$') or lab.lower().startswith('no adjusted gross income')
        if not is_agi:
            if started:
                break
            continue
        started = True
        cells = {}
        for gname, meas, val in zip(groups, measures, r[1:]):
            if gname is None or meas is None or val is None:
                continue
            num = float(val) if isinstance(val, (int, float)) else np.nan   # 'd' (withheld) or a flag
            cells[(gname, meas)] = num * (1000.0 if meas == 'Contributions' else 1.0)
        out[(*_agi_bounds(lab), lab)] = cells
    assert out, f'{path} {sheet}: no AGI rows'
    return out


def w2_deferral_dependent_share(source, year):
    """The SIPP's share of dependents among the non-joint people with elective deferrals, (share of people, share of
    dollars, description), in the SIPP year nearest year that asks deferrals (catalog dependent_share years; the
    SIPP asks them from 2021). Dependents are the people the shared tax-unit rules made dependents (persons file,
    dep_type); the other non-joint people with deferrals are the primary filers with deferrals (elective1) of the
    SIPP's units that are not joint. Deferrals after the legal limit and the person's own wages (roster_sipp.py)."""
    d = CAT['sources'][source]['dependent_share']
    sy = min(d['years'], key=lambda c: (abs(c - year), c))
    p = pd.read_parquet(f"{common.OUT}/{d['persons'].format(year=sy)}", columns=['weight', 'elective', 'dep_type'])
    u = pd.read_csv(f"{common.OUT}/{d['units'].format(year=sy)}", usecols=['weight', 'married', 'elective1'])
    for name, x in [('persons', p[['weight', 'elective']]), ('units', u)]:
        assert x.notna().all().all(), f'{source}: SIPP {name} {sy} with missing values (deferrals not asked?)'
    dep = p['dep_type'].fillna('').ne('').to_numpy() & (p['elective'] > 0).to_numpy()
    nj = (u['married'] == 0).to_numpy() & (u['elective1'] > 0).to_numpy() & (u['weight'] > 0).to_numpy()
    n_dep, t_dep = float(p['weight'][dep].sum()), float((p['weight'] * p['elective'])[dep].sum())
    n_nj, t_nj = float(u['weight'][nj].sum()), float((u['weight'] * u['elective1'])[nj].sum())
    why = (f'SIPP {sy}: dependents with deferrals {n_dep / 1e6:.3f} million people ({int(dep.sum())} records), '
           f'${t_dep / 1e9:.2f} billion; other non-joint people with deferrals {n_nj / 1e6:.3f} million '
           f'({int(nj.sum())} records), ${t_nj / 1e9:.2f} billion')
    return n_dep / (n_dep + n_nj), t_dep / (t_dep + t_nj), why


def published_w2_agi(source, year, cells, v, stats, population):
    """Rows from SOI's W-2 Table 2.D (catalog source with layout w2_by_agi): the variable's measures (its
    'published' entry: count, the number of taxpayers; amount, the contributions) in each cell's column group (the
    grouping's 'in <source>' labels), summed over the table's AGI rows inside each of the cell's AGI groups (an AGI
    grouping with edges; every AGI row must fall inside one group). On non-joint returns a taxpayer is a return. Where
    the population leaves dependent filers out, the non-joint value is reduced by the SIPP's dependent share
    (w2_deferral_dependent_share) and gap_share of the reduction is added to the borrowed error, as for the tips
    targets (published_w2)."""
    from reweight import errors
    definition = CAT['sources'][source]
    path = os.path.join(common.RAW['soi_w2'], definition['file'].format(year=year))
    table = _w2_by_agi(path, definition['sheet'])
    spec = variable_def(v)['published'][source]
    donors, donor_year = errors.soi_cells(year)
    no_dependents = 'age_range < 6' in CAT['populations'][population]['rule']
    share = w2_deferral_dependent_share(source, year) if no_dependents else None
    out = []
    for c in cells:
        assert c['agi'] and 'edges' in CAT['groupings'][c['agi']], f'{source}: cells need an AGI grouping with edges'
        ((gname, label),) = c['parts'].items()
        group = CAT['groupings'][gname][f'in {source}'][label]
        agi_groups = [(-np.inf if lo is None else lo, np.inf if hi is None else hi, lab)
                      for lo, hi, lab in CAT['groupings'][c['agi']]['edges']]
        for lo_r, hi_r, lab_r in table:
            n_in = sum(lo <= lo_r and hi_r <= hi for lo, hi, _ in agi_groups)
            assert n_in == 1, f'{path}: the AGI row {lab_r!r} lies in {n_in} of the groups of {c["agi"]}'
        for lo, hi, lab in agi_groups:
            inside = [cells_ for (lo_r, hi_r, _), cells_ in table.items() if lo <= lo_r and hi_r <= hi]
            got = {m: sum(x[(group, spec[m])] for x in inside) for m in ('count', 'amount')}
            assert all(np.isfinite(x) for x in got.values()), f'{path}: {group}, {lab}: withheld or missing'
            spec_c = {**c, 'parts': {c['agi']: lab, **c['parts']}, 'label': f'{lab}, {label}', 'agi': c['agi']}
            for st in stats:
                val, se, reason = _w2_less_dependents(definition, share, c, st, got[st], got['count'], donors, donor_year)
                out.append({'cell': spec_c['label'], 'cell_spec': spec_c, 'agi': (lo, hi), 'statistic': st,
                            'value': val, 'error_kind': 'soi_borrowed', 'se': se,
                            'source': f'{source} {year}: {group}, {spec[st]}, AGI {lab} ({path})', 'se_reason': reason})
    return out


def _w2_percentiles(path, sheet):
    """A W-2 statistics percentile table (Table 4.C): {item label: {percentile: value in dollars}}, the percentile
    columns read from the heading row ('10th' is 10)."""
    import openpyxl
    rows = [list(r) for r in openpyxl.load_workbook(path, read_only=True, data_only=True)[sheet].iter_rows(values_only=True)]
    (hr,) = [i for i, r in enumerate(rows) if any(isinstance(c, str) and c.strip() == '10th' for c in r)]
    cols = {int(c.strip()[:-2]): k for k, c in enumerate(rows[hr]) if isinstance(c, str) and c.strip()[-2:] in ('th', 'st', 'nd', 'rd')}
    return {r[0].strip(): {p: float(r[k]) for p, k in cols.items()} for r in rows[hr + 1:]
            if isinstance(r[0], str) and r[0].startswith('Box') and all(isinstance(r[k], (int, float)) for k in cols.values())}


def published_w2_shares(source, year, cells, v, e):
    """Worker share rows from SOI's W-2 percentile table (catalog source with layout w2_percentiles): for each
    percentile p of the entry's list, the share of workers (individuals with box 7 tips) above the p-th percentile
    line is 1 - p/100, the line the table's dollar value. The workers are on filed returns other than dependent
    filers'; the table covers every individual with a box 7 value, dependent filers included.
    Error: the borrowed CV (as for any count) of the count above the line (share times the workers' number) and of
    the workers' number, added in quadrature, times the share (the two are positively related, so this is not
    small); plus half the most the dependent filers could move the share (all of them below the line: s d / (1 - d),
    at most 1 - s, d the dependents' share of all workers, from the SIPP as in w2_dependent_share), in quadrature."""
    from reweight import errors
    definition = CAT['sources'][source]
    path = os.path.join(common.RAW['soi_w2'], definition['file'].format(year=year))
    item = variable_def(v)['published'][source]['item']
    pct = _w2_percentiles(path, definition['sheet'])[item]
    summary = _w2_table(path, definition['summary_sheet'])[item]
    n_all, n_nonjoint = summary[('All taxpayers', 'Number of taxpayers')], summary[('Nonjoint returns, wage earner', 'Number of taxpayers')]
    d = w2_dependent_share(source, year)[0] * n_nonjoint / n_all
    donors, donor_year = errors.soi_cells(year)
    cv_n, _ = errors.borrow_cv(donors, count=n_all)
    out = []
    for c in cells:
        for p in e['percentiles']:
            s, line = 1.0 - p / 100.0, pct[p]
            cv_a, what = errors.borrow_cv(donors, count=s * n_all)
            gap = 0.5 * min(s * d / (1 - d), 1 - s)
            se = float(np.sqrt((s * np.hypot(cv_a, cv_n) / 100) ** 2 + gap ** 2))
            out.append({'cell': c['label'], 'cell_spec': c, 'statistic': 'worker share', 'value': s, 'line': line,
                        'line_variable': v, 'error_kind': 'soi_borrowed', 'se': se,
                        'source': f'{source} {year}: {item}, {p}th percentile ${line:,.0f}, so {100 * s:g}% of workers above ({path})',
                        'se_reason': f'{100 * s:g}% of {n_all:,.0f} workers; CV {cv_a:g}% on the count above and {cv_n:g}% on the workers '
                                     f'(borrowed from the nearest Table 1.4 cells, {donor_year}: {what}), in quadrature; plus half the most the '
                                     f'dependent filers (SIPP: {100 * d:.1f}% of workers) could move the share, {gap:.4f}'})
    return out


# ---- expansion ------------------------------------------------------------------------------------------------
def _fill(template, cell, v, st, year, line=None):
    parts = dict(cell['parts'])
    d = variable_def(v) if v else {}
    return template.format(cell=cell['label'], variable=d.get('short', ''), label=d.get('label', ''), statistic=st,
                           year=year, line=line, **parts)


def published_21(year, cells, v, stats):
    """Table 2.1's already combined groups; do not infer errors for its residual."""
    (cell,) = cells
    assert cell['agi'] == 'itemized_agi_groups'
    path = os.path.join(common.RESOURCES, 'table21_2015_2023.csv')
    data = pd.read_csv(path)
    category = v.removeprefix('itemized_')
    # the taxes line is Table 2.1's state and local taxes (limited, 2018 on), not the taxes paid total, which adds
    # other taxes the calculator does not compute (resources/build_table21_state_local.py)
    source_category = {'other': 'other_residual', 'taxes': 'taxes_state_local'}.get(category, category)
    source = data[(data.year == year) & (data.category == source_category)]
    groups = CAT['groupings']['itemized_agi_groups']['edges']
    # Table 2.1's published groups, with their lower and upper edges. A group of ours can hold several published groups
    # (under $25,000 and $25,000-$50,000 are one group, under $50,000): their counts and amounts add, and their standard
    # errors add in quadrature (the published groups are treated as independent, as the combined source cells already
    # were).
    published = [('under $25,000', -np.inf, 25000), ('$25,000–$50,000', 25000, 50000), ('$50,000–$75,000', 50000, 75000),
                 ('$75,000–$100,000', 75000, 100000), ('$100,000–$200,000', 100000, 200000),
                 ('$200,000–$500,000', 200000, 500000), ('$500,000 and over', 500000, np.inf)]
    rows, used = [], []
    for lo, hi, label in groups:
        glo, ghi = (-np.inf if lo is None else lo), (np.inf if hi is None else hi)
        inside = [p for p in published if p[1] >= glo and p[2] <= ghi]
        assert inside, (label, 'no published group of Table 2.1 inside')
        used += [p[0] for p in inside]
        for st in stats:
            value, var, reasons, srcs = 0.0, 0.0, [], []
            for published_label, _, _ in inside:
                matches = source[source.agi_group == published_label]
                assert len(matches) == 1, (year, category, published_label, len(matches))
                record = matches.iloc[0]
                v_ = float(record[st])
                assert np.isfinite(v_), (year, category, published_label, st, 'missing value')
                cv = record[st + '_cv']
                if category == 'other':
                    assert st == 'amount' and year >= 2018
                    cv, reason = .10, '10% judgment: residual CV and covariance are unavailable'
                elif pd.isna(cv):
                    donors = data[(data.category == category) & (data.agi_group == published_label) & data[st + '_cv'].notna()].copy()
                    assert len(donors), (category, published_label, st, 'no CV donor')
                    donors['distance'] = abs(donors.year - year)
                    donor = donors.sort_values(['distance', 'year']).iloc[0]
                    cv = float(donor[st + '_cv'])
                    reason = f'missing grouped CV: borrowed same category and AGI group from {int(donor.year)}'
                else:
                    reason = 'Table 2.1 grouped CV; combined source cells treated as independent'
                assert np.isfinite(cv) and cv >= 0
                value += v_
                var += (abs(v_) * cv) ** 2
                reasons.append(reason)
                srcs.append(f'SOI Table 2.1 {year}: {category}, {published_label}; {record.source_file}, {record.source_rows}')
            rows.append({'cell': label, 'agi': (glo, ghi), 'statistic': st, 'value': value, 'se': float(np.sqrt(var)),
                         'error_kind': 'table21', 'se_reason': '; '.join(dict.fromkeys(reasons))
                         + ('; groups added, errors in quadrature' if len(inside) > 1 else ''),
                         'source': ' + '.join(srcs)})
    assert sorted(used) == sorted(p[0] for p in published), 'our groups do not cover Table 2.1\'s exactly once'
    return rows


def expand_entry(e, year, soi):
    stats = _as_list(e['statistic'])
    cells = group_cells(e['cells'])
    (vkind, src), = [(k, v) for k, v in e['value'].items() if k != 'year']
    vyear = int(e['value'].get('year', year))
    err = e['error']
    reasons = e.get('reason', '')
    out = []
    for v in variable_names(e.get('variable')):
        if vkind == 'published' and src == 'soi 2.1':
            rows = published_21(vyear, cells, v, stats)
            for r in rows:
                r['cell_spec'] = {'parts': {}, 'filters': [], 'line': None, 'label': r['cell']}
        elif vkind == 'published' and src in ('soi 1.4', 'soi 1.7', 'soi 3.3'):
            rows = published_17(soi, vyear, cells, v, stats) if src == 'soi 1.7' else \
                published_14(soi, vyear, cells, v, stats, table=src.split(' ')[1])
            for r in rows:
                r['cell_spec'] = {'parts': {}, 'filters': [], 'line': None, 'label': r['cell']}
        elif vkind == 'published' and CAT['sources'][src].get('layout') == 'tidy_grid':
            rows = published_grid(src, vyear, cells, v, stats)
        elif vkind == 'published' and CAT['sources'][src].get('layout') == 'w2_summary':
            rows = published_w2(src, vyear, cells, v, stats, e['population'])
        elif vkind == 'published' and CAT['sources'][src].get('layout') == 'w2_percentiles':
            rows = published_w2_shares(src, vyear, cells, v, e)
        elif vkind == 'published' and CAT['sources'][src].get('layout') == 'w2_by_agi':
            rows = published_w2_agi(src, vyear, cells, v, stats, e['population'])
        elif vkind == 'published' and src == 'soi 2.5':
            rows = published_25(vyear, cells, v, stats)
        elif vkind == 'published' and src == 'soi 1.2' and any(c['agi'] for c in cells):
            rows = published_12_agi(soi, vyear, cells, stats)
        elif vkind == 'published' and src == 'soi 1.6' and any(c['agi'] for c in cells):
            rows = published_16_agi(vyear, cells, stats)
        elif vkind == 'published':
            rows = published_status(soi, vyear, src, cells, stats)
        elif vkind == 'survey':
            rows = survey_rows(src, vyear, cells, v, stats, e['population'])
        elif vkind == 'pipeline' and src == 'tax_data_nonfilers':
            rows = taxdata_nonfiler_rows(vyear, cells, v, stats)
            for r in rows:
                r['cell_spec'] = cells[0]
        elif vkind == 'pipeline':
            rows = pipeline_rows(vyear, cells, v, stats)
            for r in rows:
                r['cell_spec'] = cells[0]
        elif vkind == 'outside':
            rows = outside_rows(src, vyear, cells, v, stats, e)
            for r in rows:
                r.setdefault('cell_spec', cells[0])   # the demographic cells carry their own
        else:
            raise NotImplementedError(vkind)
        for r in rows:
            c, st = r['cell_spec'], r['statistic']
            assert not c.get('published_line') or 'agi' in r, \
                f'{e["id"]} {c["label"]}: a published AGI line the value source ({src}) does not set'
            filt = dict(c['filters'])
            se = {'identity': 0.0, 'judgment': err.get('relative', np.nan) * abs(r['value'])}.get(err['kind'], np.nan)
            line = r.get('line')
            vr = r.get('variable', v)   # a row may carry its own variable (projection_steps)
            group, target = _fill(e['key']['group'], c, vr, st, vyear, line), _fill(e['key']['target'], c, vr, st, vyear, line)
            if 'pooled' in r:   # an SCF target: its standard error, computed with its value, for errors.py
                from reweight import scf_targets
                scf_targets.RESULTS[(e['kind'], group, target)] = r['pooled']
            out.append({
                'kind': e['kind'], 'group': group, 'target': target,
                'statistic': st, 'population': e['population'], 'variable': vr or '', 'value': float(r['value']),
                'error_kind': r.get('error_kind', err['kind']), 'se': r.get('se', se), 'identity': err['kind'] == 'identity',
                'source': r['source'], 'reason': reasons,
                'tax_unit_rules': tax_unit_rules(src) if vkind in ('survey', 'pipeline') else '',
                'entry': e['id'], 'year': vyear,
                'agi_lo': r['agi'][0] if 'agi' in r else np.nan, 'agi_hi': r['agi'][1] if 'agi' in r else np.nan,
                'married': filt.get('married', np.nan), 'age_range': filt.get('age_range', np.nan),
                'filters': json.dumps([(col, val) for col, val in c['filters'] if col not in ('married', 'age_range')]),
                'line': r.get('line', np.nan) if r.get('line') is not None else np.nan,
                'line_variable': r.get('line_variable', ''),
                'dependent_filers': e.get('dependent_filers', ''), 'error_reason': r.get('se_reason', err.get('reason', '')),
                'rank_variable': c['rank'][0] if c.get('rank') else '',
                'rank_lo': c['rank'][1] if c.get('rank') else np.nan,
                'rank_hi': c['rank'][2] if c.get('rank') else np.nan,
                'rank_line_lo': r.get('rank_lines', (np.nan, np.nan))[0],
                'rank_line_hi': r.get('rank_lines', (np.nan, np.nan))[1],
                'line_hi': r.get('line_hi') if r.get('line_hi') is not None else np.nan,
                'range_variable': c['range'][0] if c.get('range') else '',
                'range_lo': c['range'][1] if c.get('range') else np.nan,
                'range_hi': c['range'][2] if c.get('range') else np.nan})
    return out


LAST = {}   # the last expand(): the cells left out and the resolved configuration


CALCULATOR_KIND = 'calculator'   # targets on columns the tax calculator computes (taxes.py)


def expand(year, start, which):
    """One row per target for the year (ROW_COLUMNS, then CELL_COLUMNS). A cell published as zero is left out, and so
    is a cell no record of the starting file (a DataFrame with the file's columns) can reach. Both are listed in
    LAST['excluded'], and the resolved configuration is LAST['config'] (kept outside the frame, so pandas operations
    on the rows never carry them).
    which: 'without calculator' (every entry but those of kind calculator, whose columns the tilt computes after the
    non-microdata imputations, taxes.py) or 'calculator' (those alone; LAST['excluded'] then keeps what the call
    before left out and adds to it)."""
    cfg = resolve(year)
    soi = TG.load()
    assert which in ('without calculator', 'calculator'), which
    is_calc = lambda e: e['kind'] == CALCULATOR_KIND or bool(e.get('needs_calculator'))   # needs_calculator: a CBO count of the calculator's itemizers or AMT
    entries = [e for e in cfg['targets'] if is_calc(e) == (which == 'calculator')]
    expanded = [r for e in entries for r in expand_entry(e, int(year), soi)]
    # Save additional discrete groupings only when a configuration uses them. The default list keeps its schema.
    extra = ['filters'] if any(r.get('filters', '[]') != '[]' for r in expanded) else []
    rows = pd.DataFrame(expanded, columns=ROW_COLUMNS + CELL_COLUMNS + extra)
    zero = (rows['value'] == 0) & rows['statistic'].isin(['count', 'amount', 'units'])
    excluded = rows[zero].assign(why='published as zero').reset_index(drop=True)
    if which == 'calculator':
        excluded = pd.concat([LAST['excluded'], excluded], ignore_index=True)
    rows = rows[~zero].reset_index(drop=True)
    must = {(e['id'], str(c)) for e in cfg['targets'] for c in e.get('must_reach', [])}
    is_must = np.array([(en, str(g)) in must for en, g in zip(rows['entry'], rows['group'])], bool)
    if len(rows):   # a year with no calculator entries has nothing to reach
        F, _, _ = conditions(start, rows, None, check=False)
        reach = (F != 0).any(0) | rows['statistic'].isin(['average', 'share', 'worker share']).to_numpy()
        excluded = pd.concat([excluded, rows[~reach].assign(why='no record can reach it')], ignore_index=True)
        dead = rows[~reach & is_must]
        assert dead.empty, f'targets that must be kept (must_reach) and no record can reach:\n{dead[["kind", "group", "target"]]}'
        rows = rows[reach].reset_index(drop=True)
    keys = rows[['kind', 'group', 'target']].apply(tuple, axis=1)
    assert keys.is_unique, f'two entries give the same target: {keys[keys.duplicated()].tolist()}'
    from reweight import errors
    e = errors.standard_errors(rows, int(year))
    m = rows[['kind', 'group', 'target']].merge(e, on=['kind', 'group', 'target'], how='left', validate='1:1')
    got = m['se'].notna().to_numpy() & ~rows['identity'].to_numpy()
    rows.loc[got, 'se'] = m.loc[got, 'se'].to_numpy()
    rows.loc[got, 'error_kind'] = m.loc[got, 'error_kind'].to_numpy()
    rows['se_reason'] = np.where(got | rows['identity'].to_numpy(),
                                 m['se_reason'].fillna('').to_numpy(), rows['error_reason'].fillna(''))
    LAST.update(excluded=excluded, config=cfg, year=int(year))
    return rows


# ---- the conditions on the weights ----------------------------------------------------------------------------
def net_worth_ceiling(name):
    """A population's net worth ceiling in nominal target-year dollars (catalog net_worth_below; the SCF targets'
    population), or None: none set."""
    c = CAT['populations'][name].get('net_worth_below')
    return None if c is None else float(c)


def population(df, name):
    rule = CAT['populations'][name]['rule']
    m = np.ones(len(df), bool) if rule == 'all' else df.eval(rule).to_numpy()
    c = net_worth_ceiling(name)
    if c is not None:   # units with net worth at or above the ceiling are outside (the Forbes list's range)
        nw = df['net_worth'].to_numpy(float)
        assert np.isfinite(nw[m]).all(), f'population {name}: net worth missing on {int((~np.isfinite(nw[m])).sum())} units'
        m &= nw < c
    return m


def conditions(df, rows, w_start, check=True):
    """F (records x targets), T, averages for the file df: the target in row j is met when F[:, j] @ w == T[j]. For
    an average or a share, averages[j] = (member, v, published average): the average of v over the records in
    member, written as the sum over member of w (v / m - 1) = 0 (a share is the average of a 0/1 indicator); else
    None. With check, a target no record with a positive starting weight can reach is an error."""
    rows = rows.assign(line_hi=rows['line_hi'].astype(float))
    years = rows['year'].unique()
    assert len(years) == 1, f'rows of several years: {years}'
    from shared import dollars as D   # here, not at the top: dollars reads band_edges.json on import, and build_table needs this module (through survey_mix) before it writes that file
    a = D.agi(df)
    rng = df['age_range'].to_numpy()
    assert ((rng >= 0) & (rng < common.N_RANGES)).all(), 'a record with no age range'
    married = df['married'].to_numpy()
    pops = {p: population(df, p) for p in rows['population'].unique()}
    values = {}
    F = np.zeros((len(df), len(rows)))
    T = rows['value'].to_numpy(float).copy()
    averages = [None] * len(rows)

    def put_average(j, member, v, m):
        # 'the average of v over member is m': the sum over member of w (v / m - 1) = 0
        F[:, j] = member * (v / m - 1.0)
        T[j] = 0.0
        averages[j] = (member, v, m)

    for j, r in enumerate(rows.itertuples(index=False)):
        cell = pops[r.population].copy()
        extra = getattr(r, 'filters', '')
        if isinstance(extra, str) and extra:
            for column, value in json.loads(extra):
                if value is not None:
                    assert df[column].notna().all(), f'{column}: missing group values'
                    cell &= df[column].to_numpy() == value
        if not np.isnan(r.agi_lo):
            cell &= (a >= r.agi_lo) & (a < r.agi_hi)
        if r.range_variable:
            # a dollar range of a variable other than AGI (the unit's wages for the overtime targets), low inclusive
            if r.range_variable not in values:
                values[r.range_variable] = record_value(df, r.range_variable)
            xg = values[r.range_variable]
            assert np.isfinite(xg[cell]).all(), f'{r.group} {r.target}: {r.range_variable} missing in the cell\'s population'
            cell &= (xg >= r.range_lo) & (xg < r.range_hi)
        if not np.isnan(r.married):
            cell &= married == r.married
        if not np.isnan(r.age_range):
            cell &= rng == r.age_range
        if r.rank_variable:
            # the income rank group on the file, by the file's own weighted percentiles
            # among the entry's population on the starting weights w_start (the importance weights when there are
            # none, for expand's reach check only); fixed as dollar lines, so the group is the same records through
            # the tilt
            if r.rank_variable not in values:
                values[r.rank_variable] = record_value(df, r.rank_variable)
            xr, pop = values[r.rank_variable], pops[r.population]
            ws = np.asarray(w_start, float) if w_start is not None else \
                df['weight'].to_numpy(float) if 'weight' in df else np.ones(len(df))
            lo_line, hi_line = rank_dollar_lines(xr[pop], ws[pop], r.rank_lo, r.rank_hi)
            FILE_RANK_LINES[(r.rank_variable, r.population, r.rank_lo)] = lo_line
            FILE_RANK_LINES[(r.rank_variable, r.population, r.rank_hi)] = hi_line
            cell &= (xr >= lo_line) & (xr < hi_line)
        for name in (r.variable, r.line_variable):
            if name and name not in values:
                values[name] = record_value(df, name)
        x = values.get(r.variable)
        # the line is on its own variable: net worth above the SCF's 99th percentile, for a target in stocks
        assert (np.isnan(r.line) and np.isnan(r.line_hi)) or r.line_variable, f'{r.group} {r.target}: a line with no variable'
        if np.isnan(r.line_hi):
            line = None if np.isnan(r.line) else (values[r.line_variable] > r.line).astype(float)
        else:
            # a band, above the lower line (if any) and at or below the upper one
            xl = values[r.line_variable]
            line = ((xl <= r.line_hi) & ((xl > r.line) if not np.isnan(r.line) else True)).astype(float)
        if r.statistic == 'units':
            F[:, j] = cell
            if r.dependent_filers:
                # Table 1.6's dependent filers 26 and over: spread over this status's ranges by the entry's own values;
                # for an entry by AGI group and not by status, over the ranges of the same AGI group
                same = rows['entry'] == r.entry
                dep_26 = pops[r.population] & (rng == common.DEPENDENT_RANGES[-1])
                if not np.isnan(r.married):
                    same &= rows['married'] == r.married
                    dep_26 &= married == r.married
                if not np.isnan(r.agi_lo):
                    same &= (rows['agi_lo'] == r.agi_lo) & (rows['agi_hi'] == r.agi_hi)
                    dep_26 &= (a >= r.agi_lo) & (a < r.agi_hi)
                F[:, j] += dep_26 * r.value / rows.loc[same, 'value'].sum()
        elif r.statistic == 'count':
            count_var = variable_def(r.variable).get('count_variable')
            xc = record_value(df, count_var) if count_var else x
            # SOI Table 2.1 counts returns claiming a positive category amount.
            pos = r.variable in ('itemized_medical', 'itemized_taxes', 'itemized_interest', 'itemized_charity',
                                     'itemized_charity_cash', 'itemized_charity_noncash')
            F[:, j] = cell * ((xc > 0) if pos else (xc != 0)) * (1.0 if line is None else line)
        elif r.statistic == 'amount':
            F[:, j] = cell * x * (1.0 if line is None else line)
        elif r.statistic == 'workers':
            # a worker-level count: the people (primary and spouse slots) with a positive value of the variable
            F[:, j] = cell * (person_values(df, r.variable) > 0).sum(1)
        elif r.statistic == 'worker share':
            # the share of the workers (people with a positive value) whose value is above the dollar line r.line.
            # With n the record's workers and a those above the line, the share s is met when sum w (a - s n) = 0.
            # In the solver's average form (member, v, m = s) that is member = the records with workers and
            # v = s + (a - s n) W_member / W_workers, W the starting weights' totals, so the average of v over member
            # is the share itself on the starting weights and moves with the miss on the share.
            assert not np.isnan(r.line), f'{r.group} {r.target}: a worker share needs a line'
            px = person_values(df, r.variable)
            n_w, n_above = (px > 0).sum(1).astype(float), (px > r.line).sum(1).astype(float)
            member = cell & (n_w > 0)
            ws = np.asarray(w_start, float) if w_start is not None else np.ones(len(df))
            assert member.any(), f'{r.group} {r.target}: no workers'
            k = ws[member].sum() / (ws * n_w * cell).sum()
            v = r.value + (n_above - r.value * n_w) * k
            put_average(j, member, v, r.value)
        elif r.statistic in ('average', 'share'):
            v = line if r.statistic == 'share' else x
            assert v is not None, f'{r.group} {r.target}: a share needs a line'
            put_average(j, cell, v, r.value)
        else:
            raise NotImplementedError(r.statistic)
    if check:
        live = np.ones(len(df), bool) if w_start is None else np.asarray(w_start) > 0
        dead = [f'{k} | {g} | {t}' for k, g, t, f in zip(rows['kind'], rows['group'], rows['target'], (F[live] != 0).any(0))
                if not f]
        assert not dead, f'targets no record can reach (expand with start= leaves them out): {dead}'
    return F, T, averages


# ---- the start and the targets built from it ----------------------------------------------------
def start_spec(year):
    """How the year's start is built, from the year file:
      start: generate                      the run's generated stack (the default): start_scaling {weights:
                                           demographic, dollars: none}: filers scaled to SOI's returns and non-filers
                                           to the year's non-filer count, then joint and non-joint returns to Table
                                           1.2, then the demographic cells (tilt.prescale_demographic); dollars from
                                           the year's maps
      start: {from_year: s, run: <name>}   year s's tilted file, <out>/tilt/<name>/file_<s>.parquet: start_scaling
                                           {weights: persons, dollars: item_factor}: the weights calibrated to people
                                           by age and marital status (projection_steps.step1), each record's amounts
                                           grown item by item from s to the year (start.grow_dollars)
    A start_scaling in the year file must say the same."""
    cfg = resolve(year)
    st = cfg.get('start', 'generate')
    if st == 'generate':
        spec = {'mode': 'generate', 'weights': 'demographic', 'dollars': 'none'}
    else:
        assert isinstance(st, dict) and set(st) == {'from_year', 'run'}, \
            f'{year}/targets.yaml: start is generate or {{from_year: <year>, run: <tilt run>}}, not {st!r}'
        assert int(st['from_year']) < int(year), f'{year}/targets.yaml: from_year {st["from_year"]} is not earlier'
        spec = {'mode': 'from_year', 'year': int(st['from_year']), 'run': str(st['run']), 'weights': 'persons',
                'dollars': 'item_factor'}
        from scale import projection_steps as PS   # the jump-off year (settings tilt.projection.filing_season)
        j = PS.jump_off(year, spec['year'])
        if j != spec['year']:
            spec.update(year=j, run=f'y{j}', jump_off=True)
    sc = dict(cfg.get('start_scaling') or {})
    for k, v in sc.items():
        assert spec.get(k) == v, f'{year}/targets.yaml: start_scaling {k} is {spec.get(k)!r} for this start, not {v!r}'
    return spec


# ---- saving with the run --------------------------------------------------------------------------------------
def write(rows, folder, year=common.TARGET_YEAR):
    """targets.csv (one row per target) and resolved_<year>.yaml (the configuration as resolved), and the cells left
    out, if any, in targets_excluded.csv."""
    os.makedirs(folder, exist_ok=True)
    rows.to_csv(f'{folder}/targets.csv', index=False)
    assert LAST['year'] == int(year), 'write before expand'
    with open(f'{folder}/resolved_{year}.yaml', 'w') as f:
        yaml.safe_dump(LAST['config'], f, sort_keys=False, width=110, allow_unicode=True)
    if len(LAST['excluded']):
        LAST['excluded'].to_csv(f'{folder}/targets_excluded.csv', index=False)
