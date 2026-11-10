"""The year's targets: the published and survey statistics the reweighting aims at, each with its value and standard
error.

The targets are written down in targets_config/. catalog.yaml holds the definitions that every year shares: the
variables, the groupings that divide records into cells, the populations and the sources. Each year's folder holds
targets.yaml (the year's entries), prior.yaml and growth.yaml, written out in full.

1. resolve reads the year's folder and checks it.
2. expand turns each entry into one row per target: a variable, a cell and a statistic (a count, an amount, an
   average or a share), with its value from the entry's source and its standard error (errors.py).
3. conditions writes each target as a linear condition on the weights, for the solver.
4. write saves targets.csv and the resolved configuration with the run.

TARGETS.md, "How a year's targets are written down", describes the format.
"""
import copy
import json
import os
import subprocess
from functools import lru_cache

import numpy as np
import openpyxl
import pandas as pd
import yaml

from shared import common
from shared import growth as GR
from shared import survey_mix
from prep import soi as TG
from reweight import errors
from scale import projection_demographics as PD
from scale import projection_growth as PG
from scale import projection_steps as PS

CONFIG = os.path.join(common.PIPELINE, 'targets_config')
ROW_COLUMNS = ['kind', 'group', 'target', 'statistic', 'population', 'variable', 'value', 'error_kind', 'se',
               'identity', 'source', 'reason', 'tax_unit_rules']
# The columns of targets.csv. ROW_COLUMNS are the columns other programs read. CELL_COLUMNS follow them and are read by
# conditions(): the entry, the year of the value, the cell's definition (an AGI range, marital status, the age range,
# a dollar line and the variable it is on), the rule for dependent filers, and the reason for the error.
CELL_COLUMNS = ['entry', 'year', 'agi_lo', 'agi_hi', 'married', 'age_range', 'line', 'line_variable',
                'dependent_filers', 'error_reason',
                # An income rank group: its variable, its percentiles, and the survey's dollar lines at those
                # percentiles. The survey's lines are reported only; conditions() sets the file's own lines.
                'rank_variable', 'rank_lo', 'rank_hi', 'rank_line_lo', 'rank_line_hi',
                # A band's upper line, on line_variable (a unit is in the band when at or below it); NaN for none.
                # A band's lower line is 'line' (a unit is in the band when above it); NaN for none.
                'line_hi',
                # A dollar range of a variable other than AGI (catalog groupings with edges). A unit is in the cell
                # when range_lo <= the variable < range_hi. '' and NaN for none.
                'range_variable', 'range_lo', 'range_hi']
# The keys allowed in targets.yaml, prior.yaml, an entry and an entry's error, and the kinds of value source.
TARGETS_KEYS = {'year', 'kind', 'targets', 'report', 'start', 'start_scaling'}
PRIOR_KEYS = {'year', 'prior'}
ENTRY_KEYS = {'id', 'kind', 'statistic', 'population', 'variable', 'cells', 'value', 'error', 'key', 'outside_line',
              'dependent_filers', 'note', 'reason', 'percentiles', 'must_reach', 'needs_calculator'}
# An entry's reason says why the entry departs, in this year, from the general rule or from other years. It fills
# targets.csv's reason column.
# An entry's must_reach lists cell labels that expand must keep. If no record can reach one of them, the run stops
# instead of dropping it (Table 4.3's top 0.001%).
VALUE_KINDS = {'published', 'survey', 'pipeline', 'outside'}
ERROR_KEYS = {'kind', 'relative', 'reason'}
CALCULATOR_KIND = 'calculator'   # the kind of target on columns the tax calculator computes (taxes.py)
AGE_COLUMNS = {'head': 'age1', 'spouse': 'age2', 'dep1': 'dep_age1', 'dep2': 'dep_age2',
               'dep3': 'dep_age3'}
# The four filing statuses: the catalog's names, and the names of soi.py's Table 1.2 and 1.6 panels.
FOUR = {'single': 'single', 'joint': 'joint', 'separate': 'separate', 'head of household': 'head_of_household'}


# ---- Reading and checking the year's configuration ----

class _Loader(yaml.SafeLoader):
    """PyYAML's safe loader, refusing anchors and aliases (YAML's shorthand for repeating a block). The writing rules
    in catalog.yaml forbid them."""
    def compose_node(self, parent, index):
        if self.check_event(yaml.AliasEvent):
            raise ValueError('targets_config: YAML aliases are not allowed')
        event = self.peek_event()
        if getattr(event, 'anchor', None):
            raise ValueError(f'targets_config: YAML anchor &{event.anchor} is not allowed')
        return super().compose_node(parent, index)


def _yaml(path):
    """A YAML file read with _Loader."""
    with open(path) as f:
        return yaml.load(f, Loader=_Loader)


CAT = _yaml(os.path.join(CONFIG, 'catalog.yaml'))


def _no_scientific(x, where):
    """Stops the run if x is text that looks like a number. YAML reads 1e6 as text, not as a number."""
    if isinstance(x, str):
        try:
            float(x)
        except ValueError:
            return
        raise ValueError(f'{where}: {x!r} is text; write numbers out in full')


def folder(year):
    """The year's folder in targets_config/."""
    return os.path.join(CONFIG, str(int(year)))


def read(year, name):
    """One file of the year's folder (targets or prior), with its top-level keys and its year checked."""
    p = os.path.join(folder(year), f'{name}.yaml')
    assert os.path.exists(p), f'no {name}.yaml for {year} in {folder(year)} (a new year starts as a copy of the ' \
                              f'nearest year\'s folder)'
    d = _yaml(p)
    allowed = {'targets': TARGETS_KEYS, 'prior': PRIOR_KEYS}[name]
    bad = set(d) - allowed
    assert not bad, f'{p}: unknown keys {bad}'
    assert int(d.get('year', -1)) == int(year), f'{p} says year {d.get("year")}'
    return d


def _as_list(x):
    """x as a list: itself if it is one, else a list holding x."""
    return list(x) if isinstance(x, list) else [x]


def value_source(e):
    """The kind and source of an entry's value, as (kind, source). The kind is one of VALUE_KINDS and the source is
    one the catalog defines. The value may also name a year."""
    v = e['value']
    kinds = [k for k in v if k != 'year']
    assert len(kinds) == 1 and kinds[0] in VALUE_KINDS, \
        f'entry {e.get("id")}: value must name one of {sorted(VALUE_KINDS)} (and optionally a year), not {v}'
    assert v[kinds[0]] in CAT['sources'], f'entry {e.get("id")}: value source {v[kinds[0]]!r} is not in the catalog'
    return kinds[0], v[kinds[0]]


@lru_cache(maxsize=None)
def _resolve(year):
    """The checks and the reading behind resolve, done once per year."""
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
        assert isinstance(e['value'], dict), f'entry {e.get("id")}: no value source'
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
    GR.check(year)
    r['growth'] = GR.load(year)
    return r


def resolve(year):
    """The year's configuration: its targets.yaml and prior.yaml as written, with its growth lines under 'growth'.
    Stops the run unless every entry has all its parts and a value source the catalog defines, and every dollar item
    has a growth line for the year."""
    return copy.deepcopy(_resolve(int(year)))


# ---- Variables ----

def variable_names(spec):
    """The variables of an entry: None for none, a list from the catalog's variable_lists, or the names given."""
    if spec is None:
        return [None]
    if isinstance(spec, str):
        return list(CAT['variable_lists'][spec]) if spec in CAT['variable_lists'] else [spec]
    return list(spec)


def variable_def(v):
    """The definition of variable v. It comes from the catalog; for a business family's net income or net loss, from
    soi.FAMILIES; and for a projection year's people, returns groups and item sides, from projection_steps."""
    if v in CAT['variables']:
        return CAT['variables'][v]
    if PS.handles(v):
        return PS.variable_def(v)
    for fam in CAT['families']:
        for side in ('net_income', 'net_loss'):
            if v == f'{fam}_{side}':
                f = TG.FAMILIES[fam]
                return {'family': fam, 'side': side, 'short': f'{fam} {side}',
                        'soi 1.4': {'count': [v], 'amount': [v]}, 'what': f'{f["soi"]} > {side.replace("_", " ")}'}
    raise KeyError(f'no variable {v} in the catalog')


def person_values(df, v):
    """The values of a variable defined person by person (catalog ss_tips), as an array of records by people. The
    variable is W-2 box 7, Social Security tips: each person's tips, held so that wages other than tips plus tips are
    at most the Social Security wage base. A record's value (record_value) is the sum over its people. The worker-level
    statistics count and rank the people."""
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
    """Each record's value of variable v, in the file's dollars. The catalog defines a variable in one of these ways:
    a projection variable, a business family's net income or loss, a transform of another variable, Social Security
    tips by person, a count of people in an age range, an unrealized gain held to its asset's value, a sum of parts, or
    a column of the file."""
    d = variable_def(v)
    if 'projection' in d:
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
            # The capital loss allowed on the return: up to $3,000, or $1,500 if married filing separately.
            return np.minimum(np.maximum(-x, 0), np.where(df['filing_status'].to_numpy() == 3, 1500.0, 3000.0))
        return np.maximum(x, 0) if d['transform'] == 'positive' else np.maximum(-x, 0)
    if 'ss_tips' in d:
        return person_values(df, v).sum(1)
    if 'people' in d:
        p = d['people']
        hi = np.inf if p['to'] is None else p['to']
        n = np.zeros(len(df))
        for m in p['members']:
            a = df[AGE_COLUMNS[m]].to_numpy(float)          # -1 when the record has no such member
            n += (a >= p['from']) & (a < hi)
        return n
    if 'gain_held_to_value' in d:
        # An unrealized gain, held to its asset's value and never below zero, as in Tax-Simulator.
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


# ---- Cells ----
# A cell is a group of records that a target covers. It is a dict with:
#   'parts': {grouping: label}, the cell's label in each grouping;
#   'filters': [(column, value)], conditions on the file's columns;
#   'label': the cell's name;
# and each of the following, or None:
#   'agi': an AGI grouping's name. Its AGI ranges are set when the published cells are read, because groups may be
#          merged where a published cell crosses an edge;
#   'line': (variable, kind, number, label), a dollar line;
#   'rank': (variable, lo, hi), an income rank group between two percentiles;
#   'band': (variable, lower line, upper line);
#   'range': (variable, lo, hi), a dollar range of a variable other than AGI;
#   'published_line': the grouping whose published AGI cutoff is the cell's lower bound.
CELL_KEYS = ('agi', 'line', 'rank', 'band', 'range', 'published_line')


def new_cell(parts, label=None, **keys):
    """A cell with the given parts and label, and the keys given; every other key is None."""
    c = {'parts': parts, 'filters': [], 'label': label, **dict.fromkeys(CELL_KEYS)}
    c.update(keys)
    return c


def edge_groups(name):
    """A grouping's edges as a list of (lo, hi, label). lo is included and hi is not; -inf and inf mean no limit."""
    return [(-np.inf if lo is None else float(lo), np.inf if hi is None else float(hi), lab)
            for lo, hi, lab in CAT['groupings'][name]['edges']]


def group_cells(spec):
    """The cells of an entry's cells spec, as a list. The spec is 'all', a dict of grouping: 'all' or [labels], or a
    list of these. There is one cell for each combination of the groupings' cells."""
    if spec == 'all':
        return [new_cell({}, 'all')]
    if isinstance(spec, list):
        return [c for s in spec for c in group_cells(s)]
    per_dim = []
    for name, sel in spec.items():
        g = CAT['groupings'][name]
        v = g['variable']
        if v == 'agi' and 'ranks' not in g:
            assert sel == 'all', f'{name}: AGI groupings take all their cells'
            per_dim.append([new_cell({}, agi=name)])
            continue
        (kind,) = [k for k in ('values', 'lines', 'ranks', 'bands', 'edges') if k in g]
        picked = [x for x in g[kind] if sel == 'all' or x[-1] in sel]   # the label is the last element
        assert sel == 'all' or len(picked) == len(sel), f'{name}: unknown cells in {sel}'
        if kind == 'values':
            cells = [new_cell({name: lab}, filters=[(v, val)]) for val, lab in picked]
        elif kind == 'lines':
            cells = [new_cell({name: lab}, line=(v, kind_, num, lab)) for kind_, num, lab in picked]
        elif kind == 'ranks' and g.get('cutoffs') == 'published':
            # A published percentile group, cut at the AGI cutoff the source prints for the group in the year (catalog
            # cutoff_item), not at the file's own percentile. The percentiles only name the group; published_grid sets
            # the cell's AGI range from the cutoff.
            assert v == 'agi', f'{name}: published lines are AGI cutoffs'
            assert all(float(hi) == 100 for _, hi, _ in picked), f'{name}: published lines are for top groups only'
            cells = [new_cell({name: lab}, published_line=name) for _, _, lab in picked]
        elif kind == 'ranks':
            # A range of a variable's weighted percentiles, computed separately on the survey and on the file.
            cells = [new_cell({name: lab}, rank=(v, float(lo), float(hi))) for lo, hi, lab in picked]
        elif kind == 'bands':
            # A band of the variable between two lines, written [lower, upper, label]. Each line is [percentile, p]
            # (the survey's p-th percentile, in dollars), [dollars, d], or null for none. A unit is in the band when it
            # is strictly above the lower line and at or below the upper one.
            cells = [new_cell({name: lab}, band=(v, lo, hi)) for lo, hi, lab in picked]
        else:
            # Dollar ranges of a variable other than AGI, written [low, high, label], with low included and null for no
            # limit. Each range is a cell, on the file and on a survey's units alike (record_value).
            cells = [new_cell({name: lab}, range=(v, -np.inf if lo is None else float(lo), np.inf if hi is None else float(hi)))
                     for lo, hi, lab in picked]
        per_dim.append(cells)
    out = [new_cell({})]
    for dim in per_dim:
        for a in out:
            assert not (a['range'] and any(b['range'] for b in dim)), 'a cell with two dollar ranges'
        out = [{**{k: a[k] or b[k] for k in CELL_KEYS}, 'parts': {**a['parts'], **b['parts']},
                'filters': a['filters'] + b['filters'], 'label': None} for a in out for b in dim]
    assert not any(c['line'] and c['band'] for c in out), 'a cell with both a line and a band'
    assert not any(c['published_line'] and (c['agi'] or c['rank']) for c in out), \
        'a cell with a published AGI line and another AGI grouping'
    for c in out:
        c['label'] = ', '.join(c['parts'].values()) or 'all'
    return out


def merge_groups(groups, cells):
    """The AGI groups, with two neighbouring groups merged wherever a published cell crosses the edge between them."""
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


# ---- Values from SOI's tables ----

def soi_lines(v, st, table='1.4'):
    """The published lines behind statistic st of variable v in Table 1.4 (or the table named), as a list of
    (soi.csv variable, statistic, sign). A line with sign -1 is subtracted."""
    if st == 'units':
        return [('returns', 'count', 1)]
    lines = variable_def(v)[f'soi {table}'][st]
    return [(ln[len('minus '):], st, -1) if ln.startswith('minus ') else (ln, st, 1) for ln in lines]


def soi_table_lines(v, st):
    """The table and published lines of a variable published by size of AGI, in Table 1.4 or Table 3.3 (the one its
    catalog entry names), or of the number of returns (statistic units, Table 1.4). Used by errors.py."""
    if st == 'units':
        return '1.4', soi_lines(v, st)
    d = variable_def(v)
    (table,) = [t for t in ('1.4', '3.3') if f'soi {t}' in d]
    return table, soi_lines(v, st, table)


def published_14(soi, year, cells, v, stats, table='1.4'):
    """Target rows from Table 1.4 (or Table 3.3, with table '3.3') by AGI, for each of the variable's statistics. Each
    of our AGI groups is the sum of the published cells inside it. Groups are merged where a published cell crosses an
    edge between them (merge_groups)."""
    (c,) = cells
    rows = []
    # Every line the variable names must be in the year's table. A line not found would give zero in every AGI group.
    # expand leaves out cells published as zero, and a misnamed line would otherwise drop all its targets unseen.
    t14 = soi[(soi.year == year) & (soi.table.astype(str) == table) & (soi.filing_status == 'all')]
    have = set(zip(t14['variable'], t14['statistic']))
    for st in stats:
        missing = [(var, stt) for var, stt, _ in soi_lines(v, st, table) if (var, stt) not in have]
        assert not missing, f'Table {table} {year}, variable {v}: no line {missing} in the SOI table (targets/soi.csv); ' \
                            'a misnamed variable or family, or a line SOI does not publish that year'
    if c['agi'] is None:   # cells all: the total row of all returns
        assert c['label'] == 'all' and not c['filters'], f'Table {table}: cells {c["label"]} need an AGI grouping'
        s = t14[t14.label == 'all'].groupby(['variable', 'statistic'])['value'].sum()
        for st in stats:
            ln = soi_lines(v, st, table)
            rows.append({'cell': 'all', 'statistic': st, 'value': sum(sg * s[(var, stt)] for var, stt, sg in ln),
                         'source': f'soi {table} {year}, all returns, total: ' + ' | '.join(
                             f'{"minus " if sg < 0 else ""}{var} {stt}' for var, stt, sg in ln)})
        return rows
    groups = edge_groups(c['agi'])
    pc = {st: [(sg, TG.published_cells(soi, year, var, stt, table)) for var, stt, sg in soi_lines(v, st, table)]
          for st in stats}
    if CAT['groupings'][c['agi']].get('merge_across_published'):
        groups = merge_groups(groups, [x for st in stats for _, cl in pc[st] for x in cl])
    for lo, hi, lab in groups:
        for st in stats:
            rows.append({'cell': lab, 'agi': (lo, hi), 'statistic': st,
                         'value': sum(sg * sum(val for l_, h_, val in cl if l_ >= lo and h_ <= hi) for sg, cl in pc[st]),
                         'source': f'soi {table} {year}: ' + ' | '.join(f'{"minus " if sg < 0 else ""}{var} {stt}'
                                                                      for var, stt, sg in soi_lines(v, st, table))})
    return rows


@lru_cache(maxsize=None)
def table_17(year):
    """Table 1.7 (dependent filers) of a year from the SOI mirror, all returns, by AGI bracket. One row per published
    cell, with lo and hi (the bracket: -inf and inf at the ends, NaN for the all-AGI row), label, item, value (as
    published, amounts in thousands) and flag."""
    t = pd.read_csv(os.path.join(TG.MIRROR, CAT['sources']['soi 1.7']['file']), dtype=str, keep_default_na=False)
    t = t[(t['tax_year'] == str(year)) & (t['table'] == '1.7') & (t['panel'] == 'All returns')]
    assert len(t), f'Table 1.7 {year}: not in the SOI mirror'
    edge = lambda a, inf: np.nan if a == '' else inf if a in ('-Inf', 'Inf') else float(a)
    return pd.DataFrame({'lo': [edge(a, -np.inf) for a in t['row_agi_lo']],
                         'hi': [edge(a, np.inf) for a in t['row_agi_hi']],
                         'label': t['row_label'].to_numpy(), 'item': t['item'].to_numpy(),
                         'value': t['value'].to_numpy(), 'flag': t['flag'].to_numpy()})


def table_17_item(v, st):
    """The Table 1.7 item behind statistic st of variable v: the number of returns for 'units', or else the item that
    the variable's catalog entry names under 'soi 1.7'."""
    if st == 'units':
        return 'number of returns'
    items = variable_def(v).get('soi 1.7')
    assert items and st in items, f'Table 1.7: no item for {v} {st} in the catalog'
    return items[st]


def published_17(soi, year, cells, v, stats):
    """Target rows from Table 1.7, dependent filers by AGI, read from the SOI mirror (targets/soi.csv holds Tables 1.4
    and 1.2 only). The statistics are the number of returns (units), and the amount and count of the item the
    variable names under "soi 1.7" in the catalog (the net capital gain). Amounts are published in thousands and
    returned in dollars. A blank or flagged cell would need to be joined to its neighbour. No targeted cell is blank,
    so a blank one stops the run."""
    (c,) = cells
    t = table_17(year)
    out = []
    for st in stats:
        item = table_17_item(v, st)
        scale = 1000.0 if st == 'amount' else 1.0
        what = 'number of returns' if st == 'units' else item
        ti = t[t['item'] == item]
        assert len(ti), f'Table 1.7 {year}: no item {item!r}'
        if c['agi'] is None:   # cells all: the all-AGI total
            (val,) = ti.loc[ti['lo'].isna(), 'value']
            assert val != '', f'Table 1.7 {year}: a blank all-AGI cell for {item}'
            out.append({'cell': 'all', 'statistic': st, 'value': float(val) * scale,
                        'source': f'soi 1.7 {year}: dependent filers, {what}, all AGI'})
            continue
        # Our own AGI groups. Each is the sum of the published brackets inside it, with groups merged where a
        # published bracket crosses an edge (merge_groups), as for Table 1.4. A grouping that covers only part of the
        # AGI range (covers_part) uses the published brackets inside its span, and each of those must lie in one
        # group.
        g = CAT['groupings'][c['agi']]
        assert 'edges' in g, f'Table 1.7: the AGI grouping {c["agi"]} has no edges'
        published = ti[ti['lo'].notna()].sort_values('lo')
        groups = edge_groups(c['agi'])
        span = (groups[0][0], groups[-1][1])
        inside = published[(published.lo >= span[0]) & (published.hi <= span[1])]
        assert not ((published.lo < span[0]) & (published.hi > span[0])).any() and \
            not ((published.lo < span[1]) & (published.hi > span[1])).any(), \
            f'Table 1.7 {year}: a published bracket crosses the ends of {c["agi"]}'
        assert g.get('covers_part') or len(inside) == len(published), \
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
    """Target rows from SOI Table 2.5: returns with the earned income credit by the number of qualifying children,
    all AGI (the mirror's bysize_eitc.csv, row Total). For each group of children (grouping eitc_children, on
    n_dep_eitc), the number of returns with the credit and the credit's amount. The standard errors borrow the CVs of
    Table 1.4's nearest cells (errors.borrow_cv), as for the other tables' counts and amounts."""
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


def published_21(year, cells, v, stats):
    """Target rows from Table 2.1 (itemized deductions) in the itemized-deduction AGI groups, with standard errors
    from the table's CVs by group. A missing CV is borrowed from the nearest year. The residual 'other' category gets
    a judgment error of 10%."""
    (c,) = cells
    assert c['agi'] == 'itemized_agi_groups'
    data = pd.read_csv(os.path.join(common.RESOURCES, 'table21_2015_2023.csv'))
    category = v.removeprefix('itemized_')
    # The taxes line is Table 2.1's state and local taxes (limited from 2018), not the total of taxes paid. That total
    # adds other taxes the calculator does not compute (resources/build_table21_state_local.py).
    source_category = {'other': 'other_residual', 'taxes': 'taxes_state_local'}.get(category, category)
    source = data[(data.year == year) & (data.category == source_category)]
    # Table 2.1's published groups, with their lower and upper edges. One of our groups can hold several published
    # groups: under $25,000 and $25,000-$50,000 form our group under $50,000. Their counts and amounts add, and their
    # standard errors combine as the square root of the sum of squares (the published groups are treated as
    # independent).
    published = [('under $25,000', -np.inf, 25000), ('$25,000–$50,000', 25000, 50000), ('$50,000–$75,000', 50000, 75000),
                 ('$75,000–$100,000', 75000, 100000), ('$100,000–$200,000', 100000, 200000),
                 ('$200,000–$500,000', 200000, 500000), ('$500,000 and over', 500000, np.inf)]
    rows, used = [], []
    for glo, ghi, label in edge_groups('itemized_agi_groups'):
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


def _part_of(c, variables):
    """The (grouping, label) of the cell's part whose grouping is on one of the variables."""
    (gname,) = [n for n in c['parts'] if CAT['groupings'][n]['variable'] in variables]
    return gname, c['parts'][gname]


def _age_row(c):
    """The Table 1.6 row (an index into soi.T16_ROWS) and label of the cell's age range."""
    aname, label = _part_of(c, ('age_range',))
    (k,) = [val for val, lab in CAT['groupings'][aname]['values'] if lab == label]
    return k, label


def published_status(soi, year, source, cells, stats):
    """Target rows of returns by filing status (Table 1.2) or by filing status and age (Table 1.6), all AGI."""
    assert stats == ['units'], f'{source}: returns only'
    rows = []
    t16 = TG.table_1_6(year) if source == 'soi 1.6' else None
    for c in cells:
        gname, label = _part_of(c, ('married', 'filing_status'))
        if CAT['groupings'][gname]['variable'] == 'filing_status':
            assert source == 'soi 1.6', f'{source}: by filing status only for Table 1.6'
            statuses = [FOUR[label]]
        else:
            statuses = CAT['groupings'][gname][f'in {source}'][label]
        if source == 'soi 1.2':
            v = sum(TG.returns(soi, year, '1.2', s) for s in statuses)
            src = f'soi 1.2 {year}: returns of ' + ' + '.join(statuses)
        else:
            k, age = _age_row(c)
            v = sum(t16[(s, k)] for s in statuses)
            src = f'soi 1.6 {year}: returns of ' + ' + '.join(statuses) + f', primary {age}'
        rows.append({'cell': c['label'], 'cell_spec': c, 'statistic': 'units', 'value': v, 'source': src})
    return rows


# In Table 1.6 by AGI, a row whose AGI columns differ from its own all-AGI total by more than this many returns has a
# misprinted cell. An example is 2018, ages 26-34, where SOI's workbook prints two columns 2.21 million too high.
T16_ROW_SLACK = 50


def table_16_by_agi(year):
    """Table 1.6's All returns panel for a year, as a dict of age row label: (all-AGI total, [(lo, hi, value, flag),
    ...]), with the AGI columns lowest first. Values are numbers of returns. Rows under 26 are left out, because the
    targets start at 26."""
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
    """Target rows from Table 1.2: returns by filing status within AGI groups (the Table 1.2 rows of
    targets/soi.csv). Each group is the sum of the published cells inside it. As for Table 1.4, groups are merged where
    a published cell of any of the entry's statuses crosses an edge (merge_groups)."""
    assert stats == ['units'], 'Table 1.2 by AGI: returns only'
    (gname,) = {c['agi'] for c in cells}
    st_of = {c['label']: FOUR[_part_of(c, ('filing_status',))[1]] for c in cells}
    pc = {st: TG.published_cells(soi, year, 'returns', 'count', '1.2', st) for st in set(st_of.values())}
    groups = merge_groups(edge_groups(gname), [(lo, hi, '') for cl in pc.values() for lo, hi, _ in cl])
    rows = []
    for c in cells:
        st = st_of[c['label']]
        for lo, hi, lab in groups:
            inside = [v for l_, h_, v in pc[st] if l_ >= lo and h_ <= hi]
            assert inside, f'Table 1.2 {year}, {st}: no published cell in {lab}'
            spec = {**c, 'label': f'{lab}, {c["label"]}'}
            rows.append({'cell': spec['label'], 'cell_spec': spec, 'agi': (lo, hi), 'statistic': 'units',
                         'value': float(sum(inside)),
                         'source': f'soi 1.2 {year}: returns of {st}, {len(inside)} AGI cell(s) summed'})
    return rows


def published_16_agi(year, cells, stats):
    """Target rows of returns by the primary filer's age range (26 and over) within AGI groups, from Table 1.6's All
    returns panel. Each AGI group is the sum of the published columns inside it. Table 1.6's columns are Table 1.4's
    brackets, and the standard grid's edges fall on them.

    When a row's columns do not add up to its own all-AGI total, the misprint is taken to be the run of adjacent cells
    the row flags 'combined'. The groups that run touches are merged for every age (as when a published cell crosses an
    edge, merge_groups). In the misprinted row, the merged group is the row's total less its other columns. Every row's
    groups must then add up to its total."""
    assert stats == ['units'], 'Table 1.6 by AGI: returns only'
    t16 = table_16_by_agi(year)
    (gname,) = {c['agi'] for c in cells}
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
    groups = merge_groups(edge_groups(gname), [(lo, hi, row) for row, (lo, hi) in bad.items()])
    rows = []
    for c in cells:
        k, age = _age_row(c)
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
            spec = {**c, 'parts': {**c['parts'], gname: lab}, 'label': f'{lab}, primary {age}'}
            rows.append({'cell': spec['label'], 'cell_spec': spec, 'agi': (lo, hi), 'statistic': 'units', 'value': v,
                         'source': src})
        assert abs(sum(vals) - tot) <= T16_ROW_SLACK, \
            f'Table 1.6 {year}, {row}: the AGI groups add to {sum(vals):,.0f}, the total is {tot:,.0f}'
    return rows


# ---- Non-filers, their people and counts by age, from the CPS's tax units ----

@lru_cache(maxsize=None)
def cps_table():
    """The CPS tax units of the training table, filers and non-filers: year, record_id, weight, filer_p (the
    probability of filing), age_range, and the ages as the table holds them (0 for none, a + 1 for age a)."""
    t = pd.read_parquet(common.TABLE, columns=['source', 'year', 'record_id', 'weight', 'filer_p', 'age_range']
                                              + list(AGE_COLUMNS.values()))
    return t[t.source == 'cps'].drop(columns='source')


def cps_years():
    """The tax years with CPS units in the training table (2015, 2017, ..., 2023)."""
    return sorted(int(y) for y in cps_table()['year'].unique())


def cps_neighbours(year):
    """For a year with no CPS units (an even year), the nearest CPS years on either side, as (lo, hi). The caller
    handles a year with CPS units, or one outside their range; either one here stops the run."""
    have = cps_years()
    lo, hi = max([y for y in have if y < year], default=None), min([y for y in have if y > year], default=None)
    assert year not in have and lo is not None and hi is not None, \
        f'{year}: no CPS units and no CPS year on both sides (CPS years {have})'
    return lo, hi


def nonfiler_count(year):
    """The CPS's expected number of non-filers in the year: the sum over its units of weight x (1 - filer_p). The
    generated non-filers are scaled to this count. A year between CPS years takes the geometric mean of the nearest CPS
    years on either side. A year after the last CPS year takes the last count, grown with the population."""
    have = cps_years()
    if year > max(have):   # after the last CPS year: grown with the population
        return nonfiler_count(max(have)) * GR.population(year) / GR.population(max(have))
    if year not in have:
        lo, hi = cps_neighbours(year)
        return float(np.sqrt(nonfiler_count(lo) * nonfiler_count(hi)))
    t = cps_table()
    t = t[t.year == year]
    assert len(t), f'no CPS units for {year}'
    return float((t['weight'] * (1 - t['filer_p'])).sum())


@lru_cache(maxsize=None)
def cps_people(year):
    """The CPS year's units, for the counts of people: their ages as the file holds them (in years, -1 for none;
    dollars.py), and each unit's expected number of non-filers, weight x (1 - filer_p)."""
    t = cps_table()
    t = t[t.year == year]
    assert len(t), f'no CPS units for {year}'
    assert not t.isna().any().any(), f'CPS {year}: a missing weight, chance of filing or age in the table'
    ages = pd.DataFrame({c: np.where(t[c].to_numpy(float) > 0, t[c].to_numpy(float) - 1, -1.0)
                         for c in AGE_COLUMNS.values()})
    return ages, (t['weight'] * (1 - t['filer_p'])).to_numpy(float)


def cps_nonfiler_people(year, v):
    """The number of people of variable v (a catalog "people" variable) among the CPS's expected non-filers in the
    year: the sum over its units of weight x (1 - filer_p) x the unit's count of them by record_value, the same rule
    that counts the file's records. This differs a little from the filing stage's own calibration, which counts heads
    and spouses in the band of the unit's CPS head, and every child. The file carries at most three dependents."""
    ages, q = cps_people(year)
    return float(q @ record_value(ages, v))


def _taxdata_nonfiler_totals(y, v):
    """The amount of variable v among the non-filers of the tax data of year y, and the weighted number of non-filers
    with a nonzero amount."""
    t = pd.read_parquet(f'{common.DATA}/tax_data_{y}.parquet')
    t = t[t['filer'] == 0]
    x = record_value(t, v); w = t['weight'].to_numpy(float)
    assert np.isfinite(x).all() and np.isfinite(w).all(), f'tax data {y}: {v} or weight missing on a non-filer'
    return {'amount': float((w * x).sum()), 'count': float(w[x != 0].sum())}


def taxdata_nonfiler_rows(year, cells, v, stats):
    """Target rows for an item of the non-filers in the pipeline's tax data for the year: its amount, or the number
    of units with it (statistic count). The tax data (data/tax_data_<year>.parquet) are the CPS units weighted by their
    probability of not filing, with amounts in PUF terms before the training's fill-in. A year with no tax data file
    takes the geometric mean of the CPS years on either side, as the non-filer count does. Cells all only."""
    assert cells == group_cells('all'), 'tax data non-filers: cells all only'
    if os.path.exists(f'{common.DATA}/tax_data_{year}.parquet'):
        vals, how = _taxdata_nonfiler_totals(year, v), f'tax data {year}'
    else:
        lo, hi = cps_neighbours(year)
        a, b = _taxdata_nonfiler_totals(lo, v), _taxdata_nonfiler_totals(hi, v)
        vals = {k: float(np.sqrt(a[k] * b[k])) if a[k] > 0 and b[k] > 0 else (a[k] + b[k]) / 2 for k in a}
        how = f'geometric mean of the tax data of {lo} and {hi}'
    return [{'cell': 'all', 'statistic': st, 'value': vals[st],
             'source': f'{how}: non-filers (CPS units weighted by their chance of not filing), {v}, {st}, before the fill-in'}
            for st in stats]


def pipeline_rows(year, cells, v, stats):
    """The target row for the filing stage's non-filers in the year (statistic units), or for the people among them
    (a catalog "people" variable). A year between CPS years takes the geometric mean of the years on either side. A
    year after the last CPS year takes the last year's value, grown with the population."""
    assert cells == group_cells('all') and stats in (['units'], ['amount'])
    have = cps_years()
    if v is None:
        value = nonfiler_count(year)
        if year > max(have):
            last = max(have)
            f = GR.population(year) / GR.population(last)
            source = (f'past the last CPS year: the filing stage\'s expected non-filers in {last} times the '
                      f'growth of the Social Security area population, {f:.4f}')
        elif year not in have:
            lo, hi = cps_neighbours(year)
            source = (f'no CPS units for {year}: the geometric mean of the filing stage\'s expected non-filers '
                      f'in {lo} ({nonfiler_count(lo):,.0f}) and {hi} ({nonfiler_count(hi):,.0f})')
        else:
            source = f'filing stage {year}: CPS units of the training table, sum of weight x (1 - filer_p)'
    elif year > max(have):   # after the last CPS year: the last year's value grown with the population
        last = max(have)
        f = GR.population(year) / GR.population(last)
        value = cps_nonfiler_people(last, v) * f
        source = (f'past the last CPS year: the filing stage\'s people among the expected non-filers in {last}, '
                  f'counted as the file counts them, times the growth of the Social Security area population, '
                  f'{f:.4f}')
    elif year not in have:   # an even year: the geometric mean of the CPS years on either side
        lo, hi = cps_neighbours(year)
        a, b = cps_nonfiler_people(lo, v), cps_nonfiler_people(hi, v)
        value = float(np.sqrt(a * b)) if a > 0 and b > 0 else (a + b) / 2
        source = (f'no CPS units for {year}: the geometric mean of the filing stage\'s people among the '
                  f'expected non-filers in {lo} ({a:,.0f}) and {hi} ({b:,.0f}), counted as the file counts them')
    else:
        path = f'{common.DATA}/filing/checks/{year}_nonfilers_vs_targets.csv'   # the filing stage's checks
        t = pd.read_csv(path).set_index('band')['target_heads_spouses']
        band = variable_def(v)['filing_band']
        assert isinstance(band, str), f'{v}: filing_band {band!r} is not text (quote it in catalog.yaml)'
        value = cps_nonfiler_people(year, v)
        source = (f'filing stage {year}: CPS units of the training table, sum of weight x (1 - filer_p) x the '
                  f'people counted as the file counts them (record_value); the calibration\'s own number, by the '
                  f'head\'s band and every child, {1e6 * t[band]:,.0f} ({os.path.relpath(path, common.OUT)}, band '
                  f'{band})')
    return [{'cell': 'all', 'statistic': 'units' if v is None else 'amount', 'value': value, 'source': source}]


def check_nonfiler_bands(df, rows):
    """Checks that every non-filer head and spouse in the file falls in exactly one of the age bands of the targets
    for non-filer people. A head or spouse in no band (a 17-year-old: adults are counted from 18, children under 17),
    or in two, would let the reweighting change the number of non-filers through records the people targets do not
    count. Stops the run with the ages that fail."""
    people = rows.loc[rows['kind'] == 'non-filer people', 'variable'].unique()
    if not len(people):
        return
    nf = (df['filer'] == 0).to_numpy()
    for m in ('head', 'spouse'):
        a = df[AGE_COLUMNS[m]].to_numpy(float)[nf]
        has = a >= 0                                      # -1 means no such member
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


# ---- Survey averages over the mix of survey years (the CEX and the CPS) ----

@lru_cache(maxsize=None)
def cex_units(y):
    """The units of CEX year y, with spending in target-year dollars."""
    u = survey_mix.grown_units('cex', y)
    assert u['weight'].notna().all() and (u['weight'] > 0).all(), f'CEX {y}: a unit with no positive weight'
    return u


@lru_cache(maxsize=None)
def cps_overtime_units(y):
    """The tax units of CPS year y for the overtime targets, with every dollar column in target-year dollars, limited
    to the units asked the earnings questions. The CPS asks weekly pay and hours only of households in months 4 and 8
    of its sample. Overtime is missing on every other unit. Each statistic is an average over the units asked, with
    their own weights. Adds the columns ot (the unit's overtime) and wages (the wages translated to the PUF's
    definition, which gives the wage groups the same meaning as on the file)."""
    u = survey_mix.grown_units('cps', y)
    assert u['weight'].notna().all() and (u['weight'] > 0).all(), f'CPS {y}: a unit with no positive weight'
    na1, na2 = u['ot_primary'].isna().to_numpy(), u['ot_spouse'].isna().to_numpy()
    assert (na1 == na2).all(), f'CPS {y}: overtime missing on one adult of a unit only'
    assert 'wages' not in u, f'CPS {y}: the unit file already has a column wages'
    w = u['weight'].to_numpy(float)
    common.say(f'CPS {y} overtime targets: {int((~na1).sum()):,} of {len(u):,} units asked the earnings questions '
               f'({100 * w[~na1].sum() / w.sum():.1f}% of the weight); the other {int(na1.sum()):,} have overtime '
               'missing (not asked, or a worker\'s pay missing) and are left out')
    a = u[~na1].copy()   # the units asked the earnings questions
    a['ot'] = a['ot_primary'].to_numpy(float) + a['ot_spouse'].to_numpy(float)
    a['wages'] = a['wages_translated'].to_numpy(float)
    return a


@lru_cache(maxsize=None)
def cps_age_units(y):
    """The tax units of CPS year y in the training table, filers and non-filers, for the counts of units by the
    primary filer's age: record_id, weight, age_range and age1 in years (80 and over held at 80, as on the file)."""
    t = cps_table()
    u = t.loc[t.year == y, ['record_id', 'weight', 'age_range', 'age1']].reset_index(drop=True)
    assert len(u), f'no CPS units for {y} in the training table'
    na = u.isna().sum()
    assert na.sum() == 0, f'CPS {y}: missing values\n{na[na > 0]}'
    assert (u['weight'] > 0).all(), f'CPS {y}: a unit with no positive weight'
    assert (u['age_range'] < common.ADULT_RANGES).all(), f'CPS {y}: a dependent filer among the units'
    assert (u['age1'] > 0).all(), f'CPS {y}: {int((u["age1"] <= 0).sum())} units with no primary filer age'
    u['age1'] = u['age1'].to_numpy(float) - 1
    return u


def in_range(u, c, source, year):
    """A mask of the survey units in the cell's range of a variable other than AGI (the unit's wages for the overtime
    targets, the primary filer's age for the counts by age), with the low end included. Every unit when the cell has
    no range."""
    m = np.ones(len(u), bool)
    if c['range']:
        rv, lo, hi = c['range']
        xg = record_value(u, rv)
        assert np.isfinite(xg).all(), f'{source} {year}: {rv} missing on {int((~np.isfinite(xg)).sum())} units'
        m &= (xg >= lo) & (xg < hi)
    return m


def cps_age_rows(year, cells, v, stats):
    """Target rows for the number of tax units, filers and non-filers, in each cell of the primary filer's age, from
    the CPS's units over the year's mix of CPS years: the sum over years of share x the year's count."""
    assert stats == ['units'] and v is None, f'CPS units {year}: counts of units only, not {stats} of {v}'
    rows = []
    mix = survey_mix.mix('cps', year)
    for c in cells:
        assert not c['filters'] and c['rank'] is None and c['band'] is None and c['line'] is None and c['agi'] is None, \
            f'CPS units {year}, cell {c["label"]}: only ranges of the primary\'s age are built'
        per = []
        for y, share in mix:
            u = cps_age_units(y)
            m = in_range(u, c, 'CPS', y)
            assert m.any(), f'CPS {y}, cell {c["label"]}: no units'
            per.append((y, share, float(u['weight'].to_numpy(float)[m].sum()), int(m.sum())))
        rows.append({'cell': c['label'], 'cell_spec': c, 'statistic': 'units', 'value': sum(s * n for _, s, n, _ in per),
                     'source': 'CPS tax units of the training table, filers and non-filers, over the year mix '
                               + ', '.join(f'{y} {100 * s:.2f}%: {n:,.0f} units ({k:,} sample units)' for y, s, n, k in per)
                               + '; pooled: sum of share x the year\'s count'})
    return rows


def survey_averages(source, year, cells, v, u):
    """The average of v over one year's units u of the CEX or the CPS, in each cell. survey_mix_rows pools the
    years."""
    if 'age_range' in u:
        assert (u['age_range'] < common.ADULT_RANGES).all(), \
            f'{common.DATA}/{source}_units_{year}_all.parquet: a dependent filer among the survey units'
    w = u['weight'].to_numpy(float)
    x = record_value(u, v)
    rows = []
    for c in cells:
        assert c['rank'] is None and c['band'] is None, \
            f'{source} {year}, cell {c["label"]}: income rank groups and bands are built for the SCF only (scf_targets.py)'
        assert c['line'] is None, f'{source} {year}, cell {c["label"]}: an average has no line'
        m = in_range(u, c, source, year)
        for col, val in c['filters']:
            m &= (u[col] == val).to_numpy()
        assert m.any(), f'{source} {year}, cell {c["label"]}: no survey units'
        rows.append({'cell': c['label'], 'cell_spec': c, 'statistic': 'average',
                     'value': float(np.average(x[m], weights=w[m]))})
    return rows


def survey_mix_rows(source, year, cells, v, stats):
    """Target rows for the CEX's or the CPS's averages over the year's mix of survey years. Each year's units are put
    in target-year dollars and the average is computed on each year alone. The years are then pooled as the sum over
    years of share x the year's average. Only averages are built this way, because an average does not depend on the
    year's number of units."""
    assert stats == ['average'], f'{source.upper()} {year}: the year mix is built for averages, not {stats}'
    units = cex_units if source == 'cex' else cps_overtime_units
    per_year = [(y, share, survey_averages(source, y, cells, v, units(y))) for y, share in survey_mix.mix(source, year)]
    rows = []
    for k in range(len(cells)):
        base = dict(per_year[0][2][k])
        base['value'] = sum(share * r[k]['value'] for _, share, r in per_year)
        if source == 'cex':
            base['source'] = (f'cex over the year mix {", ".join(f"{y} {100 * sh:.2f}%" for y, sh, _ in per_year)}, all units, '
                              f'each year\'s spending in {common.TARGET_YEAR} dollars by NIPA PCE per return '
                              f'(survey_mix.py); pooled: sum of share x the year\'s average')
        else:
            base['source'] = (f'cps over the year mix {", ".join(f"{y} {100 * sh:.2f}%" for y, sh, _ in per_year)}, the '
                              'units asked the earnings questions, weighted; each year\'s overtime and wages in '
                              f'{common.TARGET_YEAR} dollars by SOI wages per return (survey_mix.py); pooled: sum of share x '
                              'the year\'s average; by year: ' + ', '.join(f'{y} {r[k]["value"]:,.6g}' for y, _, r in per_year))
        rows.append(base)
    return rows


def survey_rows(source, year, cells, v, stats, population):
    """Target rows for a statistic computed on the survey's own units in each cell. The SCF's come from the year's mix
    of SCF waves, with their standard errors (scf_targets.py), on the population's units (below its net worth ceiling,
    if it has one)."""
    if source == 'scf':
        from reweight import scf_targets
        return scf_targets.rows(cells, v, stats, year, ceiling=net_worth_ceiling(population))
    assert net_worth_ceiling(population) is None, f'{source}: a net worth ceiling on a survey other than the SCF'
    if source in ('cex', 'cps'):
        return survey_mix_rows(source, year, cells, v, stats)
    assert source == 'cps units', f'survey source {source}: no targets are built from it'
    return cps_age_rows(year, cells, v, stats)


# ---- Income rank groups (the grid of income by net worth) ----

def rank_dollar_lines(x, w, lo, hi):
    """The dollar values of the lo-th and hi-th weighted percentiles of x (common.wquantile: the smallest x whose
    cumulative share of the weight reaches the percentile). Percentiles 0 and 100 give no line (-inf and inf)."""
    a = -np.inf if lo <= 0 else float(common.wquantile(x, w, [lo / 100])[0])
    b = np.inf if hi >= 100 else float(common.wquantile(x, w, [hi / 100])[0])
    return a, b


FILE_RANK_LINES = {}   # (variable, population, percentile): the file's dollar line, as last set by conditions()


# ---- Targets of a projection year from outside sources: a growth rate applied to the start file's own level ----

def _start_file(spec, pop_name):
    """The start's file (the reweighted file of year spec['year']): its path, the records, their final weights, and a
    mask of the population's records."""
    path = f"{common.OUT}/tilt/{spec['run']}/file_{spec['year']}.parquet"
    df = pd.read_parquet(path)
    return path, df, df['weight'].to_numpy(float), population(df, pop_name)


def outside_rows(src, year, cells, v, stats, e):
    """Target rows from an outside source that gives only a growth rate. The value is the start file's own total
    (with its final weights and in its year's dollars) times the outside series' growth from the start's year to this
    one (Z.1 household net worth, growth.py). Only for a year whose start is an earlier year's file (from_year), and
    cells all only. The projection years' other sources are handled by projection_steps.outside_rows, and CBO's
    counts by cbo_count_rows."""
    if src == 'cbo counts':
        return cbo_count_rows(year, v, stats, e)
    if src.startswith('projection '):
        assert stats == ['amount'], f'entry {e["id"]}: projection targets are amounts'
        return PS.outside_rows(src, year)
    assert src == 'z1 household net worth', f'outside source {src}'
    assert cells == group_cells('all'), f'entry {e["id"]}: an outside target has cells all'
    spec = start_spec(year)
    assert spec['mode'] == 'from_year', f'entry {e["id"]}: an outside target needs a start from an earlier year\'s file'
    s = spec['year']
    path, df, w, pop = _start_file(spec, e['population'])
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
    """The target row for one of CBO's published counts of returns (all returns, itemizers, or returns paying the
    AMT). Like every other projection target, it starts from the file's own level: the start file's count, with its
    final weights, times CBO's growth in the count from the start's year to this one. The start file's count is of
    filers, and for itemizers and AMT returns, of the filers with a nonzero calculator value. After settings
    tilt.projection.cbo_last_year no CBO number is used, and there is no row. The file's count then follows from the
    demographic cells. The standard error is the entry's."""
    series = e['outside_line']
    if year > PG.cbo_last_year():
        return []
    assert len(stats) == 1
    spec = start_spec(year)
    assert spec['mode'] == 'from_year', f'entry {e["id"]}: a CBO count target needs a start from an earlier year\'s file'
    s = spec['year']
    path, df, w, member = _start_file(spec, e['population'])
    if v is not None:
        xc = record_value(df, variable_def(v).get('count_variable') or v)
        assert np.isfinite(xc).all(), f'entry {e["id"]}: {v} missing on {int((~np.isfinite(xc)).sum())} records of the {s} file'
        member &= xc != 0
    lev = float(w[member].sum())
    a, b = PD.cbo_count(s, series), PD.cbo_count(year, series)
    return [{'cell': 'all', 'statistic': stats[0], 'value': lev * b / a,
             'source': f'CBO February 2026 individual income tax projections, {series}: the {s} file\'s own count {lev:,.0f} '
                       f'({os.path.relpath(path, common.OUT)}) times CBO\'s growth of the count from {s} to {year}, '
                       f'{b / 1e6:.3f} / {a / 1e6:.3f} million = {b / a:.5f} (CBO\'s level in {year}: {b:,.0f})'}]


# ---- The tax-unit rules behind a survey value ----

@lru_cache(maxsize=None)
def _commit():
    """The code's git commit, noting uncommitted changes, for targets.csv."""
    try:
        h = subprocess.run(['git', '-C', common.PIPELINE, 'rev-parse', '--short', 'HEAD'], capture_output=True, text=True,
                           check=True).stdout.strip()
        dirty = subprocess.run(['git', '-C', common.PIPELINE, 'status', '--porcelain', '--', '.'], capture_output=True,
                               text=True, check=True).stdout.strip()
        return h + (' with uncommitted changes' if dirty else '')
    except (OSError, subprocess.CalledProcessError):
        return 'unknown'


def tax_unit_rules(source):
    """A description, for targets.csv, of the rules that built a source's tax units, the settings that choose their
    variant, and the code's commit."""
    s = CAT['sources'][source]
    if 'tax_unit_rules' not in s:
        return ''
    r = CAT['tax_unit_rules'][s['tax_unit_rules']]
    picks = []
    for name in r['settings'][source]:
        sec, key = name.split('.')
        picks.append(f'{name} = {common.SETTINGS.get(sec, {}).get(key, "(not set)")}')
    return (f'{s["tax_unit_rules"]}: {r["module"]} ({r["what"]}), units by {", ".join(s["built_by"])}; '
            f'{"; ".join(picks + [f"code commit {_commit()}"])}')


# ---- Published tables in tidy form ----

def _grid_value(selected, c, spec, source, measure, item=None):
    """The sum of the published cells of one item and measure among the cells selected for cell c, in returns or
    dollars. An item that SOI names differently in some years (catalog item_aliases: salaries and wages is "total
    wages" in Table 4.3 from tax year 2022) is read under the name the year publishes."""
    name = item or spec['item']
    for alt in [name] + list(CAT.get('item_aliases', {}).get(name, [])):
        if ((selected.item == alt) & (selected.measure == measure)).any():
            name = alt
            break
    z = selected[(selected.item == name) & (selected.measure == measure)]
    expected = int(np.prod([len(CAT['groupings'][g]['published_labels'][l]) for g, l in c['parts'].items()]))
    assert len(z) == expected, f'{source} {c["label"]} {spec["item"]} {measure}: {len(z)} cells, expected {expected}'
    assert z.value.ne('').all(), f'{source} {c["label"]} {spec["item"]}: {int(z.value.eq("").sum())} missing values: {z.flag.tolist()}'
    scales = z.unit.map({'returns': 1, '$ thousands': 1000, 'whole dollars': 1})
    assert scales.notna().all(), f'{source}: unknown units {z.unit.tolist()}'
    x = pd.to_numeric(z.value) * scales
    if 'net loss' in name and measure != spec.get('count', 'returns'):
        # Table 4.3 prints a net loss's amount as a negative number in some years and as a positive one in others.
        # The file's loss variables are sizes, and the published amount is read as its size.
        x = x.abs()
    return float(x.sum())


def published_grid(source, year, cells, v, stats, e):
    """Target rows from a published table in tidy form, selected by the catalog's item, measure and group labels.

    - A group label stands for one or more published panels, which are added together.
    - A rank cell stands for a published cumulative group.
    - A cell of a grouping with published lines (catalog cutoffs: published; Table 4.3's top groups) is the returns
      with AGI at or above the group's published cutoff for the year (catalog cutoff_item), in nominal dollars of the
      year. A missing cutoff stops the run.
    - Standard errors borrow the CVs of the nearest Table 1.4 cells (errors.borrow_cv), using the published number of
      returns with the item. A group whose published AGI cutoff is at or above the catalog's full-sample line gets
      errors.FULL_SAMPLE_SE instead.
    """
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
        if c['published_line']:
            # The group is the returns at or above SOI's published AGI cutoff for the year.
            assert definition.get('cutoff_item'), f'{source}: no cutoff_item for the published lines of {c["label"]}'
            assert cutoff is not None and np.isfinite(cutoff) and cutoff > 0, \
                f'{source} {year} {c["label"]}: no published AGI cutoff ({definition["cutoff_item"]})'
        for st in stats:
            val = _grid_value(selected, c, spec, source, spec[st])
            count = _grid_value(selected, c, spec, source, spec.get('count', spec.get('units', 'returns')),
                                spec.get('returns_item'))
            if cutoff is not None and cutoff >= definition.get('full_sample_above', np.inf):
                se, kind = errors.FULL_SAMPLE_SE * abs(val), 'judgment'
                reason = (f'{100 * errors.FULL_SAMPLE_SE:g}% of value (errors.full_sample_se): published AGI cutoff '
                          f'${cutoff:,.0f} is in the fully sampled AGI range; allowance for definition differences')
            elif c['published_line'] and st in ('count', 'units'):
                # The count above a published AGI cutoff borrows only from Table 1.4's count cells in the AGI classes
                # that reach above the cutoff, as Table 1.6's cells by age and AGI do (errors.soi_errors). SOI samples
                # the top far more densely than the middle.
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
            if c['published_line']:
                row['agi'] = (cutoff, np.inf)
                row['source'] += f'; AGI at or above SOI\'s published cutoff ${cutoff:,.0f}'
            out.append(row)
    return out


# ---- SOI's tables of W-2 statistics (Excel workbooks) ----

def _w2_sheet(path, sheet, heading):
    """The rows of a sheet of a W-2 workbook, and the index of its heading row: the first row with a cell that starts
    with heading (Table 2.D's sheet holds a second table below the first). The sheet's amounts must be in
    thousands."""
    rows = [list(r) for r in openpyxl.load_workbook(path, read_only=True, data_only=True)[sheet].iter_rows(values_only=True)]
    hr = next(i for i, r in enumerate(rows) if any(isinstance(c, str) and c.startswith(heading) for c in r))
    assert any('thousands of dollars' in str(c) for r in rows[:hr] for c in r if c), f'{path}: amounts not in thousands'
    return rows, hr


def _w2_groups(heading):
    """The group of each column in a W-2 table's heading row. A merged cell's group is carried to the columns on its
    right, and footnote marks are dropped."""
    groups, g = [], None
    for c in heading[1:]:
        g = c.split(' [')[0].strip() if isinstance(c, str) and c.strip() else g
        groups.append(g)
    return groups


def _w2_cells(groups, measures, values, money):
    """One data row as a dict of (column group, measure): value. The money measure is converted from thousands to
    dollars. 'd' (withheld) and other marks become NaN."""
    cells = {}
    for gname, meas, val in zip(groups, measures, values):
        if gname is None or meas is None or val is None:
            continue
        num = float(val) if isinstance(val, (int, float)) else np.nan
        cells[(gname, meas)] = num * (1000.0 if meas == money else 1.0)
    return cells


def _w2_table(path, sheet):
    """A W-2 summary table (Table 4.B), as a dict of item label: {(column group, measure): value}, with amounts in
    dollars."""
    rows, hr = _w2_sheet(path, sheet, 'All taxpayers')
    groups = _w2_groups(rows[hr])
    measures = [str(c).strip() if c is not None else None for c in rows[hr + 1][1:]]
    return {r[0].strip(): _w2_cells(groups, measures, r[1:], 'Amount') for r in rows[hr + 2:]
            if isinstance(r[0], str) and r[0].startswith('Box')}


def _w2_less_dependents(definition, share, cell, st, published, returns, donors, donor_year):
    """A W-2 target's value and standard error, as (value, se, se_reason).

    The W-2 tables count dependent filers. When the population leaves them out (share is not None; it comes from
    w2_dependent_share or w2_deferral_dependent_share) and the cell is non-joint returns, the value is the published
    value less the SIPP's dependent share of it: of the people for a count, of the dollars for an amount.

    The standard error borrows the CV of the nearest Table 1.4 cells (for a count, by its size; for an amount, by its
    size and its number of returns). gap_share of the reduction is added as an allowance for the difference in
    definitions, combined as the square root of the sum of squares."""
    gap_share = definition['dependent_share']['gap_share']
    reduction, gap_why = 0.0, ''
    if share is not None and cell['filters'] == [('married', 0)]:
        sh = share[1] if st == 'amount' else share[0]
        reduction = published * sh   # a non-joint return has one taxpayer: its workers and returns are the same count
        gap_why = (f'; less the dependent filers, {100 * sh:.1f}% ({share[2]}); plus '
                   f'{100 * gap_share:g}% of that reduction as a definition gap')
    val = published - reduction
    cv, what = errors.borrow_cv(donors, amount=val, returns=returns) if st == 'amount' else \
        errors.borrow_cv(donors, count=val)
    se = float(np.sqrt((val * cv / 100) ** 2 + (gap_share * reduction) ** 2))
    reason = f'CV {cv:g}% from nearest Table 1.4 cells ({donor_year}): {what}{gap_why}' + \
        (f'; published {published:,.0f}' if reduction else '')
    return val, se, reason


def w2_dependent_share(source, year):
    """The SIPP's share of dependents among the non-joint people with tips, over the year's mix of SIPP years
    (survey_mix.py): the sum over years of share x the year's share, for people and for dollars of tips. Pooling shares
    this way is valid because a share does not depend on a year's population. Returns (share of people, share of tips,
    description)."""
    parts = [(y, s, w2_dependent_share_one(source, y)) for y, s in survey_mix.mix('sipp', year)]
    return (sum(s * r[0] for _, s, r in parts), sum(s * r[1] for _, s, r in parts),
            'pooled over the SIPP year mix, sum of share x the year\'s share; ' + '; '.join(
                f'{100 * s:.2f}% x [{r[2]}]' for _, s, r in parts))


def w2_dependent_share_one(source, year):
    """One SIPP year's share of dependents among the non-joint people with tips, as (share of people, share of tips,
    description). Dependents are the people the shared tax-unit rules made dependents (the persons file's dep_type).
    The other non-joint people with tips are the primary filers with tips in the SIPP's units that are not joint."""
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


def _leaves_dependents_out(population):
    """Whether the population leaves dependent filers out. The W-2 tables count them."""
    return 'age_range < 6' in CAT['populations'][population]['rule']


def published_w2(source, year, cells, v, stats, e):
    """Target rows from SOI's W-2 summary table (a catalog source with layout w2_summary): the variable's item (its
    'published' entry) in each cell's column group (the grouping's 'in <source>' labels). A count is the column group's
    number of returns (or of taxpayers, where the group has one taxpayer per return), and an amount is its amount.
    Where the population leaves dependent filers out, the non-joint value is reduced as described in
    _w2_less_dependents."""
    definition = CAT['sources'][source]
    path = os.path.join(common.RAW['soi_w2'], definition['file'].format(year=year))
    table = _w2_table(path, definition['sheet'])
    item = variable_def(v)['published'][source]['item']
    assert item in table, f'{path}: no row {item!r}'
    donors, donor_year = errors.soi_cells(year)
    share = w2_dependent_share(source, year) if _leaves_dependents_out(e['population']) else None
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


# ---- W-2 Table 2.D: elective deferrals by AGI and type of return ----

def _agi_bounds(label):
    """The (lo, hi) of an AGI row label of a W-2 table, such as 'No adjusted gross income', '$1 under $5,000' or
    '$10,000,000 and over'."""
    s = label.replace(',', '').replace('$', '').strip()
    if s.lower().startswith('no adjusted gross income'):
        return -np.inf, 1.0
    if ' under ' in s:
        lo, hi = s.split(' under ')
        return float(lo), float(hi)
    assert s.endswith(' and over'), f'an AGI row label not understood: {label!r}'
    return float(s[:-len(' and over')]), np.inf


def _w2_by_agi(path, sheet):
    """A W-2 table by size of AGI (Table 2.D), as a dict of (lo, hi, row label): {(column group, measure): value},
    with amounts in dollars. Only the sheet's first table is read: it ends at the first row after the data that is not
    an AGI row (the sheet also holds Table 2.D.1 below it)."""
    rows, hr = _w2_sheet(path, sheet, 'All taxpayers')
    groups = _w2_groups(rows[hr])
    measures = [str(c).split(' [')[0].strip() if c is not None else None for c in rows[hr + 1][1:]]
    out = {}
    for r in rows[hr + 2:]:
        lab = r[0].strip() if isinstance(r[0], str) else ''
        if not (lab.startswith('$') or lab.lower().startswith('no adjusted gross income')):
            if out:
                break
            continue
        out[(*_agi_bounds(lab), lab)] = _w2_cells(groups, measures, r[1:], 'Contributions')
    assert out, f'{path} {sheet}: no AGI rows'
    return out


def w2_deferral_dependent_share(source, year):
    """The SIPP's share of dependents among the non-joint people with elective deferrals, as (share of people, share
    of dollars, description). It uses the SIPP year nearest to year among those that ask about deferrals (catalog
    dependent_share years; the SIPP asks from 2021). Dependents are the people the shared tax-unit rules made
    dependents (the persons file's dep_type). The other non-joint people with deferrals are the primary filers with
    deferrals (elective1) in the SIPP's units that are not joint. Deferrals are held to the legal limit and to the
    person's own wages (roster_sipp.py)."""
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


def published_w2_agi(source, year, cells, v, stats, e):
    """Target rows from SOI's W-2 Table 2.D (a catalog source with layout w2_by_agi). The variable's 'published'
    entry names the measures: for a count, the number of taxpayers; for an amount, the contributions. Each is taken in
    the cell's column group (the grouping's 'in <source>' labels) and summed over the table's AGI rows inside each of
    the cell's AGI groups. The AGI grouping must have edges, and every AGI row must fall inside one group. On a
    non-joint return a taxpayer is a return. Where the population leaves dependent filers out, the non-joint value is
    reduced as described in _w2_less_dependents, with the shares from w2_deferral_dependent_share."""
    definition = CAT['sources'][source]
    path = os.path.join(common.RAW['soi_w2'], definition['file'].format(year=year))
    table = _w2_by_agi(path, definition['sheet'])
    spec = variable_def(v)['published'][source]
    donors, donor_year = errors.soi_cells(year)
    share = w2_deferral_dependent_share(source, year) if _leaves_dependents_out(e['population']) else None
    out = []
    for c in cells:
        assert c['agi'] and 'edges' in CAT['groupings'][c['agi']], f'{source}: cells need an AGI grouping with edges'
        ((gname, label),) = c['parts'].items()
        group = CAT['groupings'][gname][f'in {source}'][label]
        agi_groups = edge_groups(c['agi'])
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
    """A W-2 percentile table (Table 4.C), as a dict of item label: {percentile: value in dollars}. The percentile
    columns are read from the heading row ('10th' is 10)."""
    rows = [list(r) for r in openpyxl.load_workbook(path, read_only=True, data_only=True)[sheet].iter_rows(values_only=True)]
    (hr,) = [i for i, r in enumerate(rows) if any(isinstance(c, str) and c.strip() == '10th' for c in r)]
    cols = {int(c.strip()[:-2]): k for k, c in enumerate(rows[hr]) if isinstance(c, str) and c.strip()[-2:] in ('th', 'st', 'nd', 'rd')}
    return {r[0].strip(): {p: float(r[k]) for p, k in cols.items()} for r in rows[hr + 1:]
            if isinstance(r[0], str) and r[0].startswith('Box') and all(isinstance(r[k], (int, float)) for k in cols.values())}


def published_w2_shares(source, year, cells, v, stats, e):
    """Target rows for shares of workers, from SOI's W-2 percentile table (a catalog source with layout
    w2_percentiles). For each percentile p in the entry's list, the share of workers (people with tips in box 7)
    above the p-th percentile line is 1 - p/100, and the line is the table's dollar value. The file's workers are on
    returns other than dependent filers', while the table covers everyone with a box 7 value, dependent filers
    included.

    The standard error has two parts, combined as the square root of the sum of squares:
    - the share times the borrowed CVs (as for any count) of the number of workers above the line and of the number
      of all workers, themselves combined as the square root of the sum of squares. The two counts are positively
      related, which makes this part larger than for independent counts;
    - half the most the dependent filers could move the share, which is when all of them are below the line:
      s d / (1 - d), at most 1 - s, where d is the dependents' share of all workers, from the SIPP as in
      w2_dependent_share."""
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


# The function that reads each layout of a published source in the catalog.
LAYOUTS = {'tidy_grid': published_grid, 'w2_summary': published_w2, 'w2_percentiles': published_w2_shares,
           'w2_by_agi': published_w2_agi}


# ---- Turning entries into targets ----

def _fill(template, cell, v, st, year, line=None):
    """An entry's group or target label, filled in from its template for one cell, variable and statistic."""
    parts = dict(cell['parts'])
    d = variable_def(v) if v else {}
    return template.format(cell=cell['label'], variable=d.get('short', ''), label=d.get('label', ''), statistic=st,
                           year=year, line=line, **parts)


def entry_rows(e, year, soi, v):
    """The value rows of one entry for one of its variables, from the entry's value source."""
    stats = _as_list(e['statistic'])
    cells = group_cells(e['cells'])
    vkind, src = value_source(e)
    if vkind == 'published':
        layout = CAT['sources'][src].get('layout')
        by_agi = any(c['agi'] for c in cells)
        if layout:
            return LAYOUTS[layout](src, year, cells, v, stats, e)
        if src == 'soi 2.1':
            return published_21(year, cells, v, stats)
        if src == 'soi 1.7':
            return published_17(soi, year, cells, v, stats)
        if src in ('soi 1.4', 'soi 3.3'):
            return published_14(soi, year, cells, v, stats, table=src.split(' ')[1])
        if src == 'soi 2.5':
            return published_25(year, cells, v, stats)
        if src == 'soi 1.2' and by_agi:
            return published_12_agi(soi, year, cells, stats)
        if src == 'soi 1.6' and by_agi:
            return published_16_agi(year, cells, stats)
        return published_status(soi, year, src, cells, stats)
    if vkind == 'survey':
        return survey_rows(src, year, cells, v, stats, e['population'])
    if vkind == 'pipeline':
        if src == 'tax_data_nonfilers':
            return taxdata_nonfiler_rows(year, cells, v, stats)
        return pipeline_rows(year, cells, v, stats)
    return outside_rows(src, year, cells, v, stats, e)


def expand_entry(e, year, soi):
    """One row per target of the entry, for each variable, cell and statistic, with the columns ROW_COLUMNS and then
    CELL_COLUMNS. Each row has its value and, where the source gives one, its standard error."""
    vkind, src = value_source(e)
    vyear = int(e['value'].get('year', year))
    err = e['error']
    out = []
    for v in variable_names(e.get('variable')):
        for r in entry_rows(e, vyear, soi, v):
            r.setdefault('cell_spec', new_cell({}, r['cell']))   # a source that sets no cell has the one cell named
            c, st = r['cell_spec'], r['statistic']
            assert not c.get('published_line') or 'agi' in r, \
                f'{e["id"]} {c["label"]}: a published AGI line the value source ({src}) does not set'
            filt = dict(c['filters'])
            rank = c.get('rank') or ('', np.nan, np.nan)
            rng = c.get('range') or ('', np.nan, np.nan)
            rank_lines = r.get('rank_lines', (np.nan, np.nan))
            se = {'identity': 0.0, 'judgment': err.get('relative', np.nan) * abs(r['value'])}.get(err['kind'], np.nan)
            line = r.get('line')
            vr = r.get('variable', v)   # a row may carry its own variable (projection_steps)
            group, target = _fill(e['key']['group'], c, vr, st, vyear, line), _fill(e['key']['target'], c, vr, st, vyear, line)
            if 'pooled' in r:   # an SCF target: keep its standard error, computed with its value, for errors.py
                from reweight import scf_targets
                scf_targets.RESULTS[(e['kind'], group, target)] = r['pooled']
            out.append({
                'kind': e['kind'], 'group': group, 'target': target,
                'statistic': st, 'population': e['population'], 'variable': vr or '', 'value': float(r['value']),
                'error_kind': r.get('error_kind', err['kind']), 'se': r.get('se', se), 'identity': err['kind'] == 'identity',
                'source': r['source'], 'reason': e.get('reason', ''),
                'tax_unit_rules': tax_unit_rules(src) if vkind in ('survey', 'pipeline') else '',
                'entry': e['id'], 'year': vyear,
                'agi_lo': r['agi'][0] if 'agi' in r else np.nan, 'agi_hi': r['agi'][1] if 'agi' in r else np.nan,
                'married': filt.get('married', np.nan), 'age_range': filt.get('age_range', np.nan),
                'filters': json.dumps([(col, val) for col, val in c['filters'] if col not in ('married', 'age_range')]),
                'line': line if line is not None else np.nan,
                'line_variable': r.get('line_variable', ''),
                'dependent_filers': e.get('dependent_filers', ''), 'error_reason': r.get('se_reason', err.get('reason', '')),
                'rank_variable': rank[0], 'rank_lo': rank[1], 'rank_hi': rank[2],
                'rank_line_lo': rank_lines[0], 'rank_line_hi': rank_lines[1],
                'line_hi': r['line_hi'] if r.get('line_hi') is not None else np.nan,
                'range_variable': rng[0], 'range_lo': rng[1], 'range_hi': rng[2]})
    return out


LAST = {}   # from the last call of expand(): the cells left out and the resolved configuration


def expand(year, start, which):
    """One row per target for the year, with the columns ROW_COLUMNS and then CELL_COLUMNS.

    A cell published as zero is left out, and so is a cell that no record of the start can reach (start is a
    DataFrame with the file's columns). Both are listed in LAST['excluded']. The resolved configuration is kept in
    LAST['config']. They are kept outside the returned table, where pandas operations on the rows cannot carry them.

    which is 'without calculator' or 'calculator'. The first gives every entry except those of kind calculator, whose
    columns the reweighting computes after the non-microdata imputations (taxes.py). The second gives those entries
    alone; LAST['excluded'] then keeps what the call before left out and adds to it."""
    # 1. Read the configuration and expand the entries asked for.
    cfg = resolve(year)
    soi = TG.load()
    assert which in ('without calculator', 'calculator'), which
    # An entry with needs_calculator (a CBO count of the calculator's itemizers or AMT returns) goes with the calculator
    # entries.
    entries = [e for e in cfg['targets']
               if (e['kind'] == CALCULATOR_KIND or bool(e.get('needs_calculator'))) == (which == 'calculator')]
    expanded = [r for e in entries for r in expand_entry(e, int(year), soi)]
    # The filters column is written only when some entry uses it, so that targets.csv otherwise keeps its usual columns.
    extra = ['filters'] if any(r.get('filters', '[]') != '[]' for r in expanded) else []
    rows = pd.DataFrame(expanded, columns=ROW_COLUMNS + CELL_COLUMNS + extra)
    # 2. Leave out the cells published as zero, and those no record of the start can reach.
    zero = (rows['value'] == 0) & rows['statistic'].isin(['count', 'amount', 'units'])
    excluded = rows[zero].assign(why='published as zero').reset_index(drop=True)
    if which == 'calculator':
        excluded = pd.concat([LAST['excluded'], excluded], ignore_index=True)
    rows = rows[~zero].reset_index(drop=True)
    must = {(e['id'], str(c)) for e in cfg['targets'] for c in e.get('must_reach', [])}
    is_must = np.array([(en, str(g)) in must for en, g in zip(rows['entry'], rows['group'])], bool)
    if len(rows):   # a year with no calculator entries has no rows to check
        F, _, _ = conditions(start, rows, None, check=False)
        reach = (F != 0).any(0) | rows['statistic'].isin(['average', 'share', 'worker share']).to_numpy()
        excluded = pd.concat([excluded, rows[~reach].assign(why='no record can reach it')], ignore_index=True)
        dead = rows[~reach & is_must]
        assert dead.empty, f'targets that must be kept (must_reach) and no record can reach:\n{dead[["kind", "group", "target"]]}'
        rows = rows[reach].reset_index(drop=True)
    # 3. Check that no two entries give the same target, and attach the standard errors.
    keys = rows[['kind', 'group', 'target']].apply(tuple, axis=1)
    assert keys.is_unique, f'two entries give the same target: {keys[keys.duplicated()].tolist()}'
    e = errors.standard_errors(rows, int(year))
    m = rows[['kind', 'group', 'target']].merge(e, on=['kind', 'group', 'target'], how='left', validate='1:1')
    got = m['se'].notna().to_numpy() & ~rows['identity'].to_numpy()
    rows.loc[got, 'se'] = m.loc[got, 'se'].to_numpy()
    rows.loc[got, 'error_kind'] = m.loc[got, 'error_kind'].to_numpy()
    rows['se_reason'] = np.where(got | rows['identity'].to_numpy(),
                                 m['se_reason'].fillna('').to_numpy(), rows['error_reason'].fillna(''))
    LAST.update(excluded=excluded, config=cfg, year=int(year))
    return rows


# ---- The conditions on the weights ----

def net_worth_ceiling(name):
    """A population's net worth ceiling, in nominal target-year dollars (catalog net_worth_below; the population of the
    SCF targets), or None when there is none."""
    c = CAT['populations'][name].get('net_worth_below')
    return None if c is None else float(c)


def population(df, name):
    """A mask of the records in a population: those that meet its rule in the catalog and are below its net worth
    ceiling, if it has one."""
    rule = CAT['populations'][name]['rule']
    m = np.ones(len(df), bool) if rule == 'all' else df.eval(rule).to_numpy()
    c = net_worth_ceiling(name)
    if c is not None:   # units with net worth at or above the ceiling (the Forbes list's range) are outside
        nw = df['net_worth'].to_numpy(float)
        assert np.isfinite(nw[m]).all(), f'population {name}: net worth missing on {int((~np.isfinite(nw[m])).sum())} units'
        m &= nw < c
    return m


def _cached_value(values, df, name):
    """record_value(df, name), computed once and kept in the dict values."""
    if name not in values:
        values[name] = record_value(df, name)
    return values[name]


def _put_average(F, T, averages, j, member, v, m):
    """Writes target j, "the average of v over member is m", as the condition: the sum over member of w (v / m - 1)
    equals 0."""
    F[:, j] = member * (v / m - 1.0)
    T[j] = 0.0
    averages[j] = (member, v, m)


def conditions(df, rows, w_start, check=True):
    """Writes each target as a linear condition on the weights of the file df. Returns F (records by targets), T and
    averages. The target in row j is met when F[:, j] @ w == T[j].

    For an average or a share, averages[j] = (member, v, m), meaning "the average of v over the records in member is
    m", and the condition is written as: the sum over member of w (v / m - 1) equals 0. A share is the average of a
    0/1 indicator. For other targets averages[j] is None. With check, a target that no record with a positive
    starting weight can reach stops the run."""
    # 1. What every condition needs: each record's AGI, age range, marital status, populations and starting weight.
    rows = rows.assign(line_hi=rows['line_hi'].astype(float))
    years = rows['year'].unique()
    assert len(years) == 1, f'rows of several years: {years}'
    # Imported here, not at the top: dollars reads band_edges.json when imported, and build_table needs this module
    # before it writes that file.
    from shared import dollars as D
    a = D.agi(df)
    rng = df['age_range'].to_numpy()
    assert ((rng >= 0) & (rng < common.N_RANGES)).all(), 'a record with no age range'
    married = df['married'].to_numpy()
    pops = {p: population(df, p) for p in rows['population'].unique()}
    # The starting weights. For expand's check of which cells records can reach, the frame's weight column.
    ws = np.asarray(w_start, float) if w_start is not None else \
        df['weight'].to_numpy(float) if 'weight' in df else np.ones(len(df))
    values = {}
    F = np.zeros((len(df), len(rows)))
    T = rows['value'].to_numpy(float).copy()
    averages = [None] * len(rows)

    # 2. For each target, find the records in its cell, then write its column of F by its statistic.
    for j, r in enumerate(rows.itertuples(index=False)):
        cell = pops[r.population].copy()
        extra = getattr(r, 'filters', '')
        if isinstance(extra, str) and extra:
            for column, val in json.loads(extra):
                if val is not None:
                    assert df[column].notna().all(), f'{column}: missing group values'
                    cell &= df[column].to_numpy() == val
        if not np.isnan(r.agi_lo):
            cell &= (a >= r.agi_lo) & (a < r.agi_hi)
        if r.range_variable:
            # A dollar range of a variable other than AGI (the unit's wages for the overtime targets), low end
            # included.
            xg = _cached_value(values, df, r.range_variable)
            assert np.isfinite(xg[cell]).all(), f'{r.group} {r.target}: {r.range_variable} missing in the cell\'s population'
            cell &= (xg >= r.range_lo) & (xg < r.range_hi)
        if not np.isnan(r.married):
            cell &= married == r.married
        if not np.isnan(r.age_range):
            cell &= rng == r.age_range
        if r.rank_variable:
            # The income rank group on the file, by the file's own weighted percentiles among the entry's population,
            # on the starting weights. The percentiles are fixed as dollar lines. The group therefore holds the same
            # records throughout the reweighting.
            xr, pop = _cached_value(values, df, r.rank_variable), pops[r.population]
            lo_line, hi_line = rank_dollar_lines(xr[pop], ws[pop], r.rank_lo, r.rank_hi)
            FILE_RANK_LINES[(r.rank_variable, r.population, r.rank_lo)] = lo_line
            FILE_RANK_LINES[(r.rank_variable, r.population, r.rank_hi)] = hi_line
            cell &= (xr >= lo_line) & (xr < hi_line)
        x = _cached_value(values, df, r.variable) if r.variable else None
        # A line is on its own variable. For example, a target for stocks among units with net worth above the SCF's
        # 99th percentile.
        assert (np.isnan(r.line) and np.isnan(r.line_hi)) or r.line_variable, f'{r.group} {r.target}: a line with no variable'
        if np.isnan(r.line_hi):
            line = None if np.isnan(r.line) else (_cached_value(values, df, r.line_variable) > r.line).astype(float)
        else:
            # A band: above the lower line (if any) and at or below the upper one.
            xl = _cached_value(values, df, r.line_variable)
            line = ((xl <= r.line_hi) & ((xl > r.line) if not np.isnan(r.line) else True)).astype(float)
        if r.statistic == 'units':
            F[:, j] = cell
            if r.dependent_filers:
                # Table 1.6's dependent filers aged 26 and over are spread over this status's age ranges in proportion
                # to the entry's own values. For an entry by AGI group and not by status, they are spread over the
                # ranges of the same AGI group.
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
            xc = _cached_value(values, df, count_var) if count_var else x
            # SOI Table 2.1 counts returns claiming a positive category amount.
            pos = r.variable in ('itemized_medical', 'itemized_taxes', 'itemized_interest', 'itemized_charity',
                                     'itemized_charity_cash', 'itemized_charity_noncash')
            F[:, j] = cell * ((xc > 0) if pos else (xc != 0)) * (1.0 if line is None else line)
        elif r.statistic == 'amount':
            F[:, j] = cell * x * (1.0 if line is None else line)
        elif r.statistic == 'workers':
            # A count of workers: the people (primary filer and spouse) with a positive value of the variable.
            F[:, j] = cell * (person_values(df, r.variable) > 0).sum(1)
        elif r.statistic == 'worker share':
            # The share of workers (people with a positive value) whose value is above the dollar line r.line. Let n be
            # a record's number of workers and a the number above the line. The share s is met when
            # sum w (a - s n) = 0. In the solver's form for an average (member, v, m = s), member is the records with
            # workers and v = s + (a - s n) W_member / W_workers, where W is a total of starting weights. The average of
            # v over member then equals the share on the starting weights, and it moves with the miss on the share.
            assert not np.isnan(r.line), f'{r.group} {r.target}: a worker share needs a line'
            px = person_values(df, r.variable)
            n_w, n_above = (px > 0).sum(1).astype(float), (px > r.line).sum(1).astype(float)
            member = cell & (n_w > 0)
            assert member.any(), f'{r.group} {r.target}: no workers'
            k = ws[member].sum() / (ws * n_w * cell).sum()
            _put_average(F, T, averages, j, member, r.value + (n_above - r.value * n_w) * k, r.value)
        elif r.statistic in ('average', 'share'):
            v = line if r.statistic == 'share' else x
            assert v is not None, f'{r.group} {r.target}: a share needs a line'
            _put_average(F, T, averages, j, cell, v, r.value)
        else:
            raise NotImplementedError(r.statistic)
    # 3. Check that every target can be reached by a record with a positive starting weight.
    if check:
        live = np.ones(len(df), bool) if w_start is None else np.asarray(w_start) > 0
        dead = [f'{k} | {g} | {t}' for k, g, t, f in zip(rows['kind'], rows['group'], rows['target'], (F[live] != 0).any(0))
                if not f]
        assert not dead, f'targets no record can reach (expand with start= leaves them out): {dead}'
    return F, T, averages


# ---- How the year's start is built ----

def start_spec(year):
    """How the year's start is built, from the start key of targets.yaml:
    - 'generate': the generated records, with weights prescaled to the demographic cells and dollars from the year's
      maps;
    - {from_year: s, run: <name>}: year s's reweighted file, with weights calibrated by projection_steps.step1 and
      dollars grown item by item.
    A year after the jump-off year starts from the jump-off year's file. A start_scaling key in targets.yaml must agree
    with this."""
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
        j = PS.jump_off(year, spec['year'])   # the jump-off year (settings tilt.projection.filing_season)
        if j != spec['year']:
            spec.update(year=j, run=f'y{j}', jump_off=True)
    for k, v in (cfg.get('start_scaling') or {}).items():
        assert spec.get(k) == v, f'{year}/targets.yaml: start_scaling {k} is {spec.get(k)!r} for this start, not {v!r}'
    return spec


# ---- Saving the targets with the run ----

def write(rows, folder, year):
    """Writes targets.csv (one row per target), resolved_<year>.yaml (the configuration as resolved) and, if any cells
    were left out, targets_excluded.csv."""
    os.makedirs(folder, exist_ok=True)
    rows.to_csv(f'{folder}/targets.csv', index=False)
    assert LAST['year'] == int(year), 'write before expand'
    with open(f'{folder}/resolved_{year}.yaml', 'w') as f:
        yaml.safe_dump(LAST['config'], f, sort_keys=False, width=110, allow_unicode=True)
    if len(LAST['excluded']):
        LAST['excluded'].to_csv(f'{folder}/targets_excluded.csv', index=False)
