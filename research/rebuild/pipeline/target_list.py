"""The year's targets, from the configuration in targets_config/ (TARGETS.md section 12).

The configuration says what to target in words: each entry is a block of targets, every statistic x every variable
x every cell, with its population, where its value comes from and the kind of its error. The catalog
(targets_config/catalog.yaml) says what the words mean. A year's file (years/<year>.yaml) inherits from
historical.yaml or projection.yaml, which inherit from defaults.yaml; a year can add an entry, drop one or override
fields of one, and every drop or override carries a written reason.

  resolve(year)                 the year's entries, inheritance applied, each with the file it came from and the
                                reason for every field a later file overrode
  expand(year, start=None)      one row per target (the columns in ROW_COLUMNS): its key (kind, group, target),
                                the parts of TARGETS.md section 2 in words, its value, its error kind and standard
                                error, whether it is an identity, its source lines, the reasons for any override, and
                                for a survey target the tax-unit rules its value was computed on. With a starting
                                file, a cell no record can reach is left out (listed, with the cells published as zero, in
                                LAST['excluded'])
  conditions(df, rows, w_start) the linear condition each row puts on the file's weights: F (records x targets) and
                                T, so that the target is met when F.T @ w == T; and for an average or a share,
                                averages[j] = (member, v, the published average), else None
  write(rows, folder, year)     targets.csv and the resolved configuration, saved with the run
  start_spec(year)              how the year's start is built (TARGETS.md S7): the year file's start and
                                start_scaling, with their defaults
  hold_rows(year, df, w)        the year's hold_distribution targets, built from the start file df and its weights
                                w (TARGETS.md S7)

Values. Published values come from the run's SOI numbers (targets.py: Tables 1.4 and 1.2 in targets/soi.csv, Table
1.6 by targets.table_1_6); survey values from the survey's own tax units in the run's data folder; the non-filer
values from the CPS filing stage of the same run. The 383 targets of 24 October are reproduced exactly; the builders
they came from are tilt.py's, whose helpers are reused here (tilt.merge_groups, tilt.average_condition).

Standard errors. errors.standard_errors(rows, year) (errors.py) gives each target's standard error, in the units of
the published statistic (on an average, on the average itself). Until errors.py exists, se is the configuration's
own number for a judgment, 0 for an identity, and NaN otherwise.

Averages and shares. "The average of x over the group is m" is written as tilt.average_condition writes it:
units + sum over the group of w (x / m - 1) = units, where units is the file's total (SOI's returns plus the
non-filers). The condition holds exactly only while the file's total weight equals that number, which the bracket
return counts and the non-filer count fix; average_form='zero' gives the plain form, sum over the group of
w (x / m - 1) = 0, for a solver that can take a zero target.
"""
import copy
import importlib.util
import os
import subprocess

import numpy as np
import pandas as pd
import yaml

import common
import dollars as D
import targets as TG
import tilt as TL

HERE = os.path.dirname(os.path.abspath(__file__))
CONFIG = os.path.join(HERE, 'targets_config')
ROW_COLUMNS = ['kind', 'group', 'target', 'statistic', 'population', 'variable', 'value', 'error_kind', 'se',
               'identity', 'source', 'reason', 'tax_unit_rules']
# Columns after the contract's, which conditions() reads: the entry, the year of the value, the cell in machine
# terms (an AGI range, married, the age range, a line and the variable it is on), the dependent-filer rule, the error's
# reason.
CELL_COLUMNS = ['entry', 'year', 'agi_lo', 'agi_hi', 'married', 'age_range', 'line', 'line_variable',
                'dependent_filers', 'error_reason']
TOP_KEYS = {'year', 'kind', 'inherits', 'prior', 'targets', 'drop', 'report', 'start', 'start_scaling',
            'hold_distribution'}
YEAR_KEYS = ['year', 'kind', 'prior', 'report', 'start', 'start_scaling', 'hold_distribution']   # a later file replaces
ENTRY_KEYS = {'id', 'kind', 'statistic', 'population', 'variable', 'cells', 'value', 'error', 'key',
              'dependent_filers', 'note', 'override'}
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


def read(name):
    d = _yaml(os.path.join(CONFIG, f'{name}.yaml'))
    bad = set(d) - TOP_KEYS
    assert not bad, f'{name}: unknown keys {bad}'
    for e in d.get('targets') or []:
        bad = set(e) - ENTRY_KEYS
        assert not bad, f'{name}, entry {e.get("id")}: unknown keys {bad}'
        if 'error' in e:
            bad = set(e['error']) - ERROR_KEYS
            assert not bad, f'{name}, entry {e["id"]}: unknown keys in error {bad}'
            _no_scientific(e['error'].get('relative'), f'{name}, entry {e["id"]}, error')
    return d


def _merge_entries(base, child, name):
    """Entries by id, in order: a new id is added at the end; an inherited id must carry 'override' (the reason),
    and the fields it gives replace the inherited ones."""
    out = {e['id']: e for e in base}
    for e in child:
        e = dict(e)
        if e['id'] in out:
            assert e.get('override'), f'{name}: entry {e["id"]} is inherited; say why with override'
            reason = e.pop('override')
            merged = {**out[e['id']], **{k: v for k, v in e.items() if k != 'id'}}
            merged['_overridden'] = {**out[e['id']].get('_overridden', {}),
                                     **{k: f'{name}: {reason}' for k in e if k != 'id'}}
            out[e['id']] = merged
        else:
            assert 'override' not in e, f'{name}: entry {e["id"]} overrides nothing'
            out[e['id']] = {**e, '_from': name}
    return list(out.values())


def _resolve(name):
    d = read(name)
    r = copy.deepcopy(_resolve(d['inherits'])) if d.get('inherits') else {'targets': []}
    for k in YEAR_KEYS:
        if k in d:
            r[k] = d[k]
    r['targets'] = _merge_entries(r['targets'], d.get('targets') or [], name)
    for dr in d.get('drop') or []:
        assert dr.get('reason'), f'{name}: dropping {dr["id"]} needs a reason'
        n = len(r['targets'])
        r['targets'] = [e for e in r['targets'] if e['id'] != dr['id']]
        assert len(r['targets']) == n - 1, f'{name}: nothing to drop: {dr["id"]}'
        r.setdefault('dropped', []).append({**dr, '_in': name})
    return r


def resolve(year):
    """The year's resolved configuration: years/<year>.yaml with its inheritance applied."""
    r = _resolve(f'years/{year}')
    assert int(r['year']) == int(year), f'years/{year}.yaml says year {r["year"]}'
    for e in r['targets']:
        missing = {'id', 'kind', 'statistic', 'population', 'cells', 'value', 'error', 'key'} - set(e)
        assert not missing, f'entry {e["id"]}: missing {missing}'
        assert e['population'] in CAT['populations'], f'entry {e["id"]}: no population {e["population"]}'
        assert e['error']['kind'] in CAT['error_kinds'], f'entry {e["id"]}: no error kind {e["error"]["kind"]}'
        if e['error']['kind'] == 'judgment':
            assert 'relative' in e['error'] and e['error'].get('reason'), \
                f'entry {e["id"]}: a judgment error needs its number (relative) and a reason'
        for st in _as_list(e['statistic']):
            assert st in CAT['statistics'], f'entry {e["id"]}: no statistic {st}'
    return r


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
    """The catalog's definition of a variable; a business family's net income or loss from targets.FAMILIES."""
    if v in CAT['variables']:
        return CAT['variables'][v]
    for fam in CAT['families']:
        for side in ('net_income', 'net_loss'):
            if v == f'{fam}_{side}':
                f = TG.FAMILIES[fam]
                return {'family': fam, 'side': side, 'short': f'{fam} {side}',
                        'soi 1.4': {'count': [v], 'amount': [v]}, 'what': f'{f["soi"]} > {side.replace("_", " ")}'}
    raise KeyError(f'no variable {v} in the catalog')


AGE_COLUMNS = {'head': 'age_primary', 'spouse': 'age_spouse', 'dep1': 'age_dep1', 'dep2': 'age_dep2',
               'dep3': 'age_dep3'}


def record_value(df, v):
    """A record's value of variable v (in the file's dollars)."""
    d = variable_def(v)
    if 'family' in d:
        f = TG.FAMILIES[d['family']]
        net = df[f['income']].to_numpy(float).sum(1) - (df[f['loss']].to_numpy(float).sum(1) if f['loss'] else 0.0)
        return np.where(net > 0, net, 0.0) if d['side'] == 'net_income' else np.where(net < 0, -net, 0.0)
    if 'people' in d:
        p = d['people']
        hi = np.inf if p['to'] is None else p['to']
        n = np.zeros(len(df))
        for m in p['members']:
            a = df[AGE_COLUMNS[m]].to_numpy(float)          # -1 for no such member
            n += (a >= p['from']) & (a < hi)
        return n
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
        if g['variable'] == 'agi':
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
        else:
            raise NotImplementedError(name)
    out = [{'parts': {}, 'filters': [], 'agi': None, 'line': None}]
    for dim in per_dim:
        out = [{'parts': {**a['parts'], **b['parts']}, 'filters': a['filters'] + b['filters'],
                'agi': a['agi'] or b['agi'], 'line': a['line'] or b['line']} for a in out for b in dim]
    for c in out:
        c['label'] = ', '.join(c['parts'].values()) or 'all'
    return out


def soi_brackets(soi, year, rule, kept_separate=()):
    """SOI's Table 1.4 AGI brackets, the top ones merged until each would hold enough records (tilt.irs_brackets
    with the year and the rule as arguments). Brackets whose lower edge is in kept_separate are never merged; each
    must be published that year, they must be the top brackets, and the rule applies to the brackets below them,
    each counted on its own records."""
    t = soi[(soi.year == year) & (soi.table.astype(str) == '1.4') & (soi.filing_status == 'all') & (soi.label != 'all')]
    ret = t[t.variable == 'returns'].sort_values('lo')
    out = list(ret[['lo', 'hi', 'label']].itertuples(index=False, name=None))
    kept = sorted(float(x) for x in kept_separate)
    top = [b for b in out if b[0] in kept]
    assert [b[0] for b in top] == kept, f'Table 1.4 {year}: brackets kept separate {kept} not all published'
    assert not kept or out[-len(top):] == top, f'Table 1.4 {year}: the brackets kept separate are not the top ones'
    out = out[:len(out) - len(top)]
    ceiling = kept[0] if kept else np.inf
    count, total = dict(zip(ret['lo'], ret['value'])), ret['value'].sum()
    while len(out) > 1:
        lo, hi, label = out[-1]
        if sum(c for l_, c in count.items() if lo <= l_ < ceiling) / total * rule['in_a_file_of'] >= rule['records']:
            break
        lo2, _, label2 = out[-2]
        # below brackets kept separate a merged bracket still has a top: "$200,000 under $1,000,000"
        end = f' under {label.split(" under ")[1]}' if kept and ' under ' in label else ' or more'
        out = out[:-2] + [(lo2, hi, f'{label2.split(" under")[0]}{end}')]
    return out + top, t


# ---- values ---------------------------------------------------------------------------------------------------
def soi_lines(v, st):
    """[(soi.csv variable, statistic, sign)] behind statistic st of variable v in Table 1.4."""
    if st == 'units':
        return [('returns', 'count', 1)]
    lines = variable_def(v)['soi 1.4'][st]
    return [(ln[len('minus '):], st, -1) if ln.startswith('minus ') else (ln, st, 1) for ln in lines]


def published_14(soi, year, cells, v, stats):
    """Rows from Table 1.4 by AGI: our groups summed from the published cells. As-published brackets sum the
    non-blank cells inside each bracket (tilt.irs_targets); edge groups are merged where a published cell crosses an
    edge, over all the variable's statistics (tilt.business_targets)."""
    (c,) = cells
    rows = []
    if c['agi'] is None:   # cells: all, the all-returns total row
        assert c['label'] == 'all' and not c['filters'], f'Table 1.4: cells {c["label"]} need an AGI grouping'
        t = soi[(soi.year == year) & (soi.table.astype(str) == '1.4') & (soi.filing_status == 'all') & (soi.label == 'all')]
        s = t.groupby(['variable', 'statistic'])['value'].sum()
        for st in stats:
            ln = soi_lines(v, st)
            rows.append({'cell': 'all', 'statistic': st, 'value': sum(sg * s[(var, stt)] for var, stt, sg in ln),
                         'source': f'soi 1.4 {year}, all returns, total: ' + ' | '.join(
                             f'{"minus " if sg < 0 else ""}{var} {stt}' for var, stt, sg in ln)})
        return rows
    g = CAT['groupings'][c['agi']]
    if 'as_published' in g:
        brackets, t = soi_brackets(soi, year, g['merge_top_until'], g.get('kept_separate', ()))
        # a cell SOI combined with its neighbour must lie inside one of our brackets, or the bracket's sum would hold
        # dollars of the next one (none does in 2019; the top brackets kept separate make it possible)
        for st in stats:
            for var, stt, _ in soi_lines(v, st):
                for l_, h_, _ in TG.published_cells(soi, year, var, stt):
                    assert any(lo <= l_ and h_ <= hi for lo, hi, _ in brackets), \
                        f'Table 1.4 {year}, {var} {stt}: a combined cell [{l_}, {h_}) crosses a bracket edge'
        for lo, hi, lab in brackets:
            s = t[(t.lo >= lo) & (t.hi <= hi)].groupby(['variable', 'statistic'])['value'].sum()
            for st in stats:
                ln = soi_lines(v, st)
                rows.append({'cell': lab, 'agi': (lo, hi), 'statistic': st,
                             'value': sum(sg * s[(var, stt)] for var, stt, sg in ln),
                             'source': f'soi 1.4 {year}: ' + ' | '.join(f'{"minus " if sg < 0 else ""}{var} {stt}'
                                                                       for var, stt, sg in ln)})
        return rows
    groups = [(-np.inf if lo is None else lo, np.inf if hi is None else hi, lab) for lo, hi, lab in g['edges']]
    pc = {st: [(sg, TG.published_cells(soi, year, var, stt)) for var, stt, sg in soi_lines(v, st)] for st in stats}
    if g.get('merge_across_published'):
        groups = TL.merge_groups(groups, [x for st in stats for _, cl in pc[st] for x in cl])
    for lo, hi, lab in groups:
        for st in stats:
            rows.append({'cell': lab, 'agi': (lo, hi), 'statistic': st,
                         'value': sum(sg * sum(val for l_, h_, val in cl if l_ >= lo and h_ <= hi) for sg, cl in pc[st]),
                         'source': f'soi 1.4 {year}: ' + ' | '.join(f'{"minus " if sg < 0 else ""}{var} {stt}'
                                                                   for var, stt, sg in soi_lines(v, st))})
    return rows


def published_17(soi, year, cells, stats):
    """Table 1.7, dependent filers' returns by AGI, read from the SOI mirror (targets/soi.csv has Tables 1.4 and 1.2
    only). Its brackets, the top ones merged until each would hold enough records in a file of filers: a bracket's
    expected records are its returns as a share of all Table 1.4 returns, times the file size (the IRS brackets'
    rule). A blank cell would need joining to its neighbour; none is blank in the years seen, so one is an error."""
    (c,) = cells
    assert stats == ['units'], 'Table 1.7: returns only'
    t = pd.read_csv(os.path.join(TG.MIRROR, CAT['sources']['soi 1.7']['file']), dtype=str, keep_default_na=False)
    t = t[(t['tax_year'] == str(year)) & (t['table'] == '1.7') & (t['panel'] == 'All returns')
          & (t['item'] == 'number of returns')]
    if c['agi'] is None:   # cells: all, the all-AGI total
        (v,) = t.loc[t['row_agi_lo'] == '', 'value'].astype(float)
        return [{'cell': 'all', 'statistic': 'units', 'value': v,
                 'source': f'soi 1.7 {year}: dependent filers, number of returns, all AGI'}]
    g = CAT['groupings'][c['agi']]
    t = t[t['row_agi_lo'] != '']
    assert len(t), f'Table 1.7 {year}: not in the SOI mirror'
    assert (t['value'] != '').all(), f'Table 1.7 {year}: a blank returns cell'
    cell = pd.DataFrame({'lo': [-np.inf if a == '-Inf' else float(a) for a in t['row_agi_lo']],
                         'hi': [np.inf if a == 'Inf' else float(a) for a in t['row_agi_hi']],
                         'label': t['row_label'].to_numpy(), 'value': t['value'].astype(float).to_numpy()}).sort_values('lo')
    rule, total = g['merge_top_until'], TG.returns(soi, year)
    out = list(cell[['lo', 'hi', 'label']].itertuples(index=False, name=None))
    count = dict(zip(cell['lo'], cell['value']))
    while len(out) > 1:
        lo, hi, label = out[-1]
        if sum(v for l_, v in count.items() if l_ >= lo) / total * rule['in_a_file_of'] >= rule['records']:
            break
        lo2, _, label2 = out[-2]
        out = out[:-2] + [(lo2, hi, f'{label2.split(" under")[0]} or more')]
    return [{'cell': lab, 'agi': (lo, hi), 'statistic': 'units',
             'value': float(cell.loc[(cell.lo >= lo) & (cell.hi <= hi), 'value'].sum()),
             'source': f'soi 1.7 {year}: dependent filers, number of returns'} for lo, hi, lab in out]


def published_status(soi, year, source, cells, stats):
    """Rows of returns by filing status (Table 1.2) or by filing status and age (Table 1.6), all AGI."""
    assert stats == ['units'], f'{source}: returns only'
    rows = []
    t16 = TG.table_1_6(year) if source == 'soi 1.6' else None
    for c in cells:
        (gname,) = [n for n in c['parts'] if CAT['groupings'][n]['variable'] == 'married']
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


def nonfiler_count(year):
    """The CPS's expected non-filers in the year: the sum over its units of weight x (1 - filer_p)
    (tilt.nonfiler_count with the year as an argument)."""
    t = pd.read_parquet(common.TABLE, columns=['source', 'year', 'weight', 'filer_p'])
    t = t[(t.source == 'cps') & (t.year == year)]
    assert len(t), f'no CPS units for {year}'
    return float((t['weight'] * (1 - t['filer_p'])).sum())


def filing_checks(year):
    path = f"{common.DATA}/{common.SETTINGS['tilt'].get('nonfiler_targets_dir', 'filing/checks')}/{year}_nonfilers_vs_targets.csv"
    return pd.read_csv(path).set_index('band')['target_heads_spouses'], path


_SURVEY = {}


def survey_frame(source, year):
    key = (source, year)
    if key not in _SURVEY:
        path = f"{common.OUT}/{CAT['sources'][source]['file'].format(year=year)}"
        u = pd.read_parquet(path)
        assert u['weight'].notna().all() and (u['weight'] > 0).all(), f'{path}: a unit with no positive weight'
        _SURVEY[key] = (u, path)
    return _SURVEY[key]


def survey_rows(source, year, cells, v, stats):
    """The statistic computed on the survey's own units in each cell (tilt.wealth_targets,
    tilt.consumption_targets)."""
    u, path = survey_frame(source, year)
    if 'age_range' in u:
        assert (u['age_range'] < common.ADULT_RANGES).all(), f'{path}: a dependent filer among the survey units'
    w = u['weight'].to_numpy(float)
    x = None if v is None else record_value(u, v)
    rows = []
    for c in cells:
        m = np.ones(len(u), bool)
        for col, val in c['filters']:
            m &= (u[col] == val).to_numpy()
        assert m.any(), f'{source} {year}, cell {c["label"]}: no survey units'
        line, above, line_var = None, m, ''
        if c['line'] is not None:
            # the line is on the grouping's variable (net worth), which may differ from the target's (stocks)
            line_var, kind, num, _ = c['line']
            xl = record_value(u, line_var)
            line = float(common.wquantile(xl, w, [num / 100])[0]) if kind == 'percentile' else float(num)
            above = m & (xl > line)
        for st in stats:
            if st == 'share':
                assert line is not None, f'{source} {year}, cell {c["label"]}: a share needs a line'
                val = w[above].sum() / w[m].sum()
            elif st == 'amount':
                val = (x * w)[above].sum()
            elif st == 'average':
                assert line is None
                val = float(np.average(x[m], weights=w[m]))
            else:
                raise NotImplementedError(f'{source}: statistic {st}')
            rows.append({'cell': c['label'], 'cell_spec': c, 'statistic': st, 'value': val, 'line': line,
                         'line_variable': line_var,
                         'source': f'{source} {year} ({os.path.relpath(path, common.OUT)}), all units'})
    return rows


def pipeline_rows(year, cells, v, stats):
    assert cells == [group_cells('all')[0]] and stats in (['units'], ['amount'])
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


_CPS_AGES = {}


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


# ---- expansion ------------------------------------------------------------------------------------------------
def _fill(template, cell, v, st, year):
    parts = dict(cell['parts'])
    d = variable_def(v) if v else {}
    return template.format(cell=cell['label'], variable=d.get('short', ''), label=d.get('label', ''), statistic=st,
                           year=year, **parts)


def expand_entry(e, year, soi):
    stats = _as_list(e['statistic'])
    cells = group_cells(e['cells'])
    (vkind, src), = [(k, v) for k, v in e['value'].items() if k != 'year']
    vyear = int(e['value'].get('year', year))
    err = e['error']
    reasons = '; '.join(f'{k}: {r}' for k, r in e.get('_overridden', {}).items())
    out = []
    for v in variable_names(e.get('variable')):
        if vkind == 'published' and src in ('soi 1.4', 'soi 1.7'):
            rows = published_14(soi, vyear, cells, v, stats) if src == 'soi 1.4' else \
                published_17(soi, vyear, cells, stats)
            for r in rows:
                r['cell_spec'] = {'parts': {}, 'filters': [], 'line': None, 'label': r['cell']}
        elif vkind == 'published':
            rows = published_status(soi, vyear, src, cells, stats)
        elif vkind == 'survey':
            rows = survey_rows(src, vyear, cells, v, stats)
        elif vkind == 'pipeline':
            rows = pipeline_rows(vyear, cells, v, stats)
            for r in rows:
                r['cell_spec'] = cells[0]
        else:
            raise NotImplementedError(vkind)
        for r in rows:
            c, st = r['cell_spec'], r['statistic']
            filt = dict(c['filters'])
            se = {'identity': 0.0, 'judgment': err.get('relative', np.nan) * abs(r['value'])}.get(err['kind'], np.nan)
            out.append({
                'kind': e['kind'], 'group': _fill(e['key']['group'], c, v, st, vyear),
                'target': _fill(e['key']['target'], c, v, st, vyear),
                'statistic': st, 'population': e['population'], 'variable': v or '', 'value': float(r['value']),
                'error_kind': err['kind'], 'se': se, 'identity': err['kind'] == 'identity',
                'source': r['source'], 'reason': reasons,
                'tax_unit_rules': tax_unit_rules(src) if vkind in ('survey', 'pipeline') else '',
                'entry': e['id'], 'year': vyear,
                'agi_lo': r['agi'][0] if 'agi' in r else np.nan, 'agi_hi': r['agi'][1] if 'agi' in r else np.nan,
                'married': filt.get('married', np.nan), 'age_range': filt.get('age_range', np.nan),
                'line': r.get('line', np.nan) if r.get('line') is not None else np.nan,
                'line_variable': r.get('line_variable', ''),
                'dependent_filers': e.get('dependent_filers', ''), 'error_reason': err.get('reason', '')})
    return out


LAST = {}   # the last expand(): the cells left out and the resolved configuration


def _errors_module():
    """errors.py (agent B's) if it exists; its own import errors are not hidden."""
    if importlib.util.find_spec('errors') is None:
        return None
    import errors
    return errors


def expand(year=common.TARGET_YEAR, start=None):
    """One row per target for the year (ROW_COLUMNS, then CELL_COLUMNS). A cell published as zero is left out; with
    a starting file (a DataFrame with the file's columns), so is a cell no record can reach. Both are listed in
    LAST['excluded'], and the resolved configuration is LAST['config'] (kept outside the frame, so pandas operations
    on the rows never carry them)."""
    cfg = resolve(year)
    soi = TG.load()
    rows = pd.DataFrame([r for e in cfg['targets'] for r in expand_entry(e, int(year), soi)],
                        columns=ROW_COLUMNS + CELL_COLUMNS)
    zero = (rows['value'] == 0) & rows['statistic'].isin(['count', 'amount', 'units'])
    excluded = rows[zero].assign(why='published as zero')
    rows = rows[~zero].reset_index(drop=True)
    if start is not None:
        F, _, _ = conditions(start, rows, None, check=False)
        reach = (F != 0).any(0) | rows['statistic'].isin(['average', 'share']).to_numpy()
        excluded = pd.concat([excluded, rows[~reach].assign(why='no record can reach it')], ignore_index=True)
        rows = rows[reach].reset_index(drop=True)
    keys = rows[['kind', 'group', 'target']].apply(tuple, axis=1)
    assert keys.is_unique, f'two entries give the same target: {keys[keys.duplicated()].tolist()}'
    errors = _errors_module()
    if errors is not None:
        e = errors.standard_errors(rows, int(year))
        m = rows[['kind', 'group', 'target']].merge(e, on=['kind', 'group', 'target'], how='left', validate='1:1')
        got = m['se'].notna().to_numpy() & ~rows['identity'].to_numpy()
        rows.loc[got, 'se'] = m.loc[got, 'se'].to_numpy()
        rows.loc[got, 'error_kind'] = m.loc[got, 'error_kind'].to_numpy()
        rows['se_reason'] = m['se_reason'].fillna('').to_numpy()
    LAST.update(excluded=excluded, config=cfg, year=int(year))
    return rows


# ---- the conditions on the weights ----------------------------------------------------------------------------
def population(df, name):
    rule = CAT['populations'][name]['rule']
    return np.ones(len(df), bool) if rule == 'all' else df.eval(rule).to_numpy()


def file_units(year):
    """The file's total number of units: SOI's returns (Table 1.4, all AGI) plus the non-filers, as tilt.py has it."""
    return TG.returns(TG.load(), year) + nonfiler_count(year)


def conditions(df, rows, w_start, units=None, average_form='units', check=True):
    """F (records x targets), T, averages for the file df: the target in row j is met when F[:, j] @ w == T[j]. For
    an average or a share, averages[j] = (member, v, published average): the average of v over the records in
    member; else None. average_form 'units' writes it as tilt.average_condition does (T = units, the file's total);
    'zero' as sum over member of w (v / m - 1) = 0. With check, a target no record with a positive starting weight
    can reach is an error."""
    rows = rows.fillna({'variable': '', 'dependent_filers': ''})   # as read back from targets.csv
    if 'line_variable' not in rows:   # a target list written before 25 October: every line was on the target's variable
        rows = rows.assign(line_variable=np.where(rows['line'].notna(), rows['variable'], ''))
    rows = rows.fillna({'line_variable': ''})
    years = rows['year'].unique()
    assert len(years) == 1, f'rows of several years: {years}'
    if units is None and average_form == 'units' and rows['statistic'].isin(['average', 'share', 'share of total']).any():
        units = file_units(int(years[0]))
    a = D.agi(df)
    rng = df['age_range'].to_numpy()
    assert ((rng >= 0) & (rng < common.N_RANGES)).all(), 'a record with no age range'
    married = df['married'].to_numpy()
    pops = {p: population(df, p) for p in rows['population'].unique()}
    values = {}
    F = np.zeros((len(df), len(rows)))
    T = rows['value'].to_numpy(float).copy()
    averages = [None] * len(rows)
    for j, r in enumerate(rows.itertuples(index=False)):
        cell = pops[r.population].copy()
        if not np.isnan(r.agi_lo):
            cell &= (a >= r.agi_lo) & (a < r.agi_hi)
        if not np.isnan(r.married):
            cell &= married == r.married
        if not np.isnan(r.age_range):
            cell &= rng == r.age_range
        for name in (r.variable, r.line_variable):
            if name and name not in values:
                values[name] = record_value(df, name)
        x = values.get(r.variable)
        # the line is on its own variable: net worth above the SCF's 99th percentile, for a target in stocks
        assert np.isnan(r.line) or r.line_variable, f'{r.group} {r.target}: a line with no variable'
        line = None if np.isnan(r.line) else (values[r.line_variable] > r.line).astype(float)
        if r.statistic == 'units':
            F[:, j] = cell
            if r.dependent_filers:
                # Table 1.6's dependent filers 26 and over: spread over this status's ranges by the entry's own values
                same = (rows['entry'] == r.entry) & (rows['married'] == r.married)
                dep_26 = pops[r.population] & (married == r.married) & (rng == common.DEPENDENT_RANGES[-1])
                F[:, j] += dep_26 * r.value / rows.loc[same, 'value'].sum()
        elif r.statistic == 'count':
            F[:, j] = cell * (x != 0)
        elif r.statistic == 'amount':
            F[:, j] = cell * x * (1.0 if line is None else line)
        elif r.statistic == 'share of total':
            # a hold_distribution target (hold_rows): the item's share of its total in an AGI rank group, s =
            # sum over the group of w x / sum over the population of w x, written sum over the population of
            # w x (1[in the group] - s) = 0. In the solver's average form (averages[j] = (member, v, m), the condition
            # sum over member of w (v - m) = 0) that is member = the population's holders of the item, m = s and
            # v = s + x (1[in the group] - s) / xbar, xbar the item's average over member on the starting weights.
            # The solver's "average" of v is then the share itself on the starting weights, and after the tilt the
            # share up to the change in xbar (a few percent where the totals move), so it measures the miss on the
            # share as it does on any average. The thin-cell measures see every holder of the item.
            pop = pops[r.population]
            member = pop & (x != 0)
            w_avg = np.ones(len(df)) if w_start is None else np.asarray(w_start, float)
            xbar = (w_avg[member] * x[member]).sum() / w_avg[member].sum()
            assert member.any() and xbar != 0, f'{r.group} {r.target}: no holders, or the item averages zero'
            v = r.value + x * (cell.astype(float) - r.value) / xbar
            F[:, j] = TL.average_condition(member, v, r.value) if average_form == 'units' else member * (v / r.value - 1.0)
            T[j] = units if average_form == 'units' else 0.0
            averages[j] = (member, v, r.value)
        elif r.statistic in ('average', 'share'):
            v = line if r.statistic == 'share' else x
            assert v is not None, f'{r.group} {r.target}: a share needs a line'
            F[:, j] = TL.average_condition(cell, v, r.value) if average_form == 'units' else cell * (v / r.value - 1.0)
            T[j] = units if average_form == 'units' else 0.0
            averages[j] = (cell, v, r.value)
        else:
            raise NotImplementedError(r.statistic)
    if check:
        live = np.ones(len(df), bool) if w_start is None else np.asarray(w_start) > 0
        dead = [f'{k} | {g} | {t}' for k, g, t, f in zip(rows['kind'], rows['group'], rows['target'], (F[live] != 0).any(0))
                if not f]
        assert not dead, f'targets no record can reach (expand with start= leaves them out): {dead}'
    return F, T, averages


# ---- the start and the targets built from it (TARGETS.md S7) ----------------------------------------------------
START_SCALING = {'weights': ('none', 'filing_status', 'demographic'), 'dollars': ('per_return_growth', 'none')}


def start_spec(year):
    """How the year's start is built, from the year file (TARGETS.md S7):
      start: generate                      the run's generated stack (the default; today's behaviour)
      start: {from_year: s, run: <name>}   year s's tilted file, <out>/tilt/<name>/file_<s>.parquet
    and start_scaling: {weights: none | filing_status | demographic, dollars: per_return_growth | none}. weights:
    how the start's weights are scaled to year t before the tilt (tilt.py: filers to SOI's returns and non-filers to
    the year's non-filer count, then joint and non-joint returns to Table 1.2, then the demographic cells); its
    default is settings tilt.prescale, as before S7. dollars (from_year only): each record's amounts grown item by
    item from s to t by SOI's growth per return with the item (tilt.grow_dollars)."""
    cfg = resolve(year)
    st = cfg.get('start', 'generate')
    if st == 'generate':
        spec = {'mode': 'generate'}
    else:
        assert isinstance(st, dict) and set(st) == {'from_year', 'run'}, \
            f'years/{year}.yaml: start is generate or {{from_year: <year>, run: <tilt run>}}, not {st!r}'
        assert int(st['from_year']) < int(year), f'years/{year}.yaml: from_year {st["from_year"]} is not earlier'
        spec = {'mode': 'from_year', 'year': int(st['from_year']), 'run': str(st['run'])}
    sc = dict(cfg.get('start_scaling') or {})
    bad = set(sc) - set(START_SCALING)
    assert not bad, f'years/{year}.yaml: unknown keys in start_scaling {bad}'
    spec['weights'] = sc.get('weights', common.SETTINGS['tilt']['prescale'])
    spec['dollars'] = sc.get('dollars', 'per_return_growth' if spec['mode'] == 'from_year' else 'none')
    for k, allowed in START_SCALING.items():
        assert spec[k] in allowed, f'years/{year}.yaml: start_scaling {k} {spec[k]!r} is not one of {allowed}'
    assert spec['mode'] == 'from_year' or spec['dollars'] == 'none', \
        f'years/{year}.yaml: a generated start has its dollars from the year\'s maps; start_scaling dollars is for from_year'
    return spec


HOLD_KEYS = {'population', 'ranks', 'items', 'statistic', 'error', 'note'}


def hold_rows(year, df, w):
    """The year's hold_distribution targets (TARGETS.md S7), built from the start file df with its weights w (after
    the start's scaling). The year file says:
      hold_distribution:
        population: filers                     whose records are ranked and whose totals are shared out
        ranks: {variable: agi, groups: [[0, 50, p0-50], ..., [99.9, 100, p99.9+]]}   percentile ranges of the
                                               variable on the start's weights, lower edge inclusive
        items: [agi, wages, ...]               catalog variables
        statistic: share of total
        error: {kind: judgment, relative: 0.01, reason: ...}   or absolute: <share points>, with a reason
    Each item's share of its total in each rank group is a target at its value on the start (kind 'hold
    distribution'). The groups are fixed as AGI ranges at the start's percentile lines (agi_lo, agi_hi), so they stay
    the same records through the tilt. Empty when the year has no hold_distribution."""
    cfg = resolve(year)
    h = cfg.get('hold_distribution')
    cols = ROW_COLUMNS + CELL_COLUMNS + ['se_reason']
    if not h:
        return pd.DataFrame(columns=cols)
    bad = set(h) - HOLD_KEYS
    assert not bad, f'years/{year}.yaml: unknown keys in hold_distribution {bad}'
    assert h.get('statistic', 'share of total') == 'share of total', 'hold_distribution: statistic share of total only'
    assert h['ranks']['variable'] == 'agi', 'hold_distribution: ranks by AGI only (the groups are stored as AGI ranges)'
    err = h['error']
    assert err['kind'] == 'judgment' and err.get('reason') and (('relative' in err) != ('absolute' in err)), \
        'hold_distribution: a judgment error with relative or absolute (not both) and a reason'
    pop_name = h.get('population', 'filers')
    pop = population(df, pop_name)
    w = np.asarray(w, float)
    a = D.agi(df)
    assert not np.isnan(a[pop]).any(), 'hold_distribution: a record with no AGI'
    groups = h['ranks']['groups']
    assert groups[0][0] == 0 and groups[-1][1] == 100 and all(g[1] == n[0] for g, n in zip(groups, groups[1:])), \
        'hold_distribution: the rank groups must run from 0 to 100 without gaps'
    lines = [float(x) for x in common.wquantile(a[pop], w[pop], [g[0] / 100 for g in groups[1:]])]
    edges = [-np.inf] + lines + [np.inf]
    out = []
    for v in h['items']:
        d = variable_def(v) if v in CAT['variables'] else {'short': v}
        x = record_value(df, v)
        total = (w[pop] * x[pop]).sum()
        assert total > 0, f'hold_distribution, {v}: its total on the start is not positive ({total:,.0f})'
        for (p_lo, p_hi, lab), lo, hi in zip(groups, edges[:-1], edges[1:]):
            g = pop & (a >= lo) & (a < hi)
            s = float((w[g] * x[g]).sum() / total)
            se = err['relative'] * abs(s) if 'relative' in err else float(err['absolute'])
            assert se > 0, f'hold_distribution, {v} in {lab}: a share of 0 with a relative error'
            out.append({
                'kind': 'hold distribution', 'group': f'AGI {lab}', 'target': f'{d.get("short", v)} share of total',
                'statistic': 'share of total', 'population': pop_name, 'variable': v, 'value': s,
                'error_kind': 'judgment', 'se': se, 'identity': False,
                'source': f'the start file, {pop_name} ranked by AGI on its weights: percentiles {p_lo}-{p_hi}, AGI '
                          f'{lo:,.0f} to {hi:,.0f}',
                'reason': '', 'tax_unit_rules': '', 'entry': 'hold_distribution', 'year': int(year),
                'agi_lo': lo, 'agi_hi': hi, 'married': np.nan, 'age_range': np.nan, 'line': np.nan,
                'line_variable': '', 'dependent_filers': '', 'error_reason': err['reason'],
                'se_reason': f'judgment ({err["reason"]})'})
    return pd.DataFrame(out, columns=cols)


# ---- saving with the run --------------------------------------------------------------------------------------
def write(rows, folder, year=common.TARGET_YEAR):
    """targets.csv (one row per target) and resolved_<year>.yaml (the configuration as resolved), and the cells left
    out, if any, in targets_excluded.csv."""
    os.makedirs(folder, exist_ok=True)
    rows.to_csv(f'{folder}/targets.csv', index=False)
    with open(f'{folder}/resolved_{year}.yaml', 'w') as f:
        yaml.safe_dump(LAST['config'] if LAST.get('year') == int(year) else resolve(year), f, sort_keys=False,
                       width=110, allow_unicode=True)
    ex = LAST.get('excluded') if LAST.get('year') == int(year) else None
    if ex is not None and len(ex):
        ex.to_csv(f'{folder}/targets_excluded.csv', index=False)


if __name__ == '__main__':
    rows = expand(common.TARGET_YEAR)
    out = os.environ.get('REBUILD_TARGETS_OUT', common.TILT)
    write(rows, out)
    common.say(f'{len(rows)} targets for {common.TARGET_YEAR} written to {out}/targets.csv')
    common.say(rows.groupby(['kind', 'error_kind']).size().to_string())
