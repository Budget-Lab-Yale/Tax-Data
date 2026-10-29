"""One table of every setting of the targets configuration by year (TARGETS.md section 12; 29 October): rows are the
settings, columns the years with a folder in targets_config/, and a cell that differs from the year before it is
marked. Written to targets_config/OVERVIEW.csv and targets_config/OVERVIEW.md (generated; do not edit by hand).

Rows:
  target <id>                 the entry's value source (published table, survey, pipeline stage; with its year if fixed)
  target <id>, definition     statistic | population | variable | cells | error kind (and its relative size), and
                              the reason, if any
  growth <item>               the item's source; negative values; measure (growth.yaml)
  prior <entry>: <source>     the share of that source in the prior entry (prior.yaml)
  year kind, start, start scaling, hold distribution   the year's other settings (targets.yaml)
A cell is "-" where the year has no such setting. In the markdown view a cell that differs from the year before is
marked "(changed)"; in the CSV each year has a column "changed <year>" (yes or empty).

Regenerate after any change to a year's folder (it reads YAML only; standard library and PyYAML):
    python research/rebuild/pipeline/config_overview.py
Settings targets_config (REBUILD_SETTINGS or the pipeline's settings.json) names another configuration folder.
"""
import csv
import json
import os

import yaml

HERE = os.path.dirname(os.path.abspath(__file__))
_S = json.load(open(os.environ.get('REBUILD_SETTINGS', os.path.join(HERE, 'settings.json'))))
CONFIG = _S.get('targets_config', os.path.join(HERE, 'targets_config'))


def _flat(x):
    """A setting as one line of text."""
    if isinstance(x, dict):
        return '{' + ', '.join(f'{k}: {_flat(v)}' for k, v in x.items()) + '}'
    if isinstance(x, list):
        return '[' + ', '.join(_flat(v) for v in x) + ']'
    return ' '.join(str(x).split())


def settings(year):
    """{row label: text} for one year's folder."""
    d = os.path.join(CONFIG, str(year))
    t = yaml.safe_load(open(os.path.join(d, 'targets.yaml')))
    p = yaml.safe_load(open(os.path.join(d, 'prior.yaml')))
    g = yaml.safe_load(open(os.path.join(d, 'growth.yaml')))
    out = {'year kind': _flat(t.get('kind', '-')), 'start': _flat(t.get('start', 'generate')),
           'start scaling': _flat(t.get('start_scaling', 'settings tilt.prescale')),
           'hold distribution': _flat(t.get('hold_distribution', 'none'))}
    for e in t.get('targets') or []:
        out[f'target {e["id"]}'] = _flat(e['value'])
        err = e['error']
        err_txt = err['kind'] + (f" {err['relative']}" if 'relative' in err else '')
        out[f'target {e["id"]}, definition'] = ' | '.join(
            [_flat(e['statistic']), e['population'], _flat(e.get('variable', '-')), _flat(e['cells']), err_txt]
            + ([f"reason: {_flat(e['reason'])}"] if e.get('reason') else []))
    for entry, shares in (p.get('prior') or {}).items():
        for src, s in shares.items():
            out[f'prior {entry}: {src}'] = f'{float(s):.4f}'
    for v, ln in (g.get('items') or {}).items():
        neg = ln['negative']
        out[f'growth {v}'] = f"{_flat(ln['source'])}; negative: {_flat(neg)}; {_flat(ln['measure'])}"
    return out


def main():
    years = sorted(int(x) for x in os.listdir(CONFIG) if x.isdigit() and os.path.isdir(os.path.join(CONFIG, x)))
    assert years, f'no year folders in {CONFIG}'
    by_year = {y: settings(y) for y in years}
    rows = []
    for y in years:                      # every row, in the order it first appears
        rows += [r for r in by_year[y] if r not in rows]
    order = {'year': 0, 'start': 0, 'hold': 0, 'target': 1, 'prior': 2, 'growth': 3}
    rows.sort(key=lambda r: order.get(r.split()[0], 4))
    table, changed = [], 0
    for r in rows:
        cells = [by_year[y].get(r, '-') for y in years]
        flags = [i > 0 and cells[i] != cells[i - 1] for i in range(len(years))]
        changed += sum(flags)
        table.append((r, cells, flags))
    with open(os.path.join(CONFIG, 'OVERVIEW.csv'), 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow(['setting'] + [c for y in years for c in (str(y), f'changed {y}')])
        for r, cells, flags in table:
            w.writerow([r] + [c for v, fl in zip(cells, flags) for c in (v, 'yes' if fl else '')])
    esc = lambda s: s.replace('|', '\\|')
    md = ['# The targets configuration by year (generated)', '',
          'Written by `pipeline/config_overview.py` from the year folders in `pipeline/targets_config/`; do not edit by '
          'hand. Regenerate after any change to a year\'s folder: `python research/rebuild/pipeline/config_overview.py`.',
          '', f'Years: {", ".join(map(str, years))}. Rows: {len(rows)} settings (each target entry\'s value source and '
          'definition, each growth line, each prior share, and the year\'s start settings). "(changed)" marks a cell that '
          f'differs from the year before; {changed} cells do. "-" means the year has no such setting. The same table is '
          'OVERVIEW.csv.', '',
          '| setting | ' + ' | '.join(map(str, years)) + ' |', '|---|' + '---|' * len(years)]
    for r, cells, flags in table:
        md.append(f'| {esc(r)} | ' + ' | '.join(esc(c) + (' (changed)' if fl else '') for c, fl in zip(cells, flags))
                  + ' |')
    open(os.path.join(CONFIG, 'OVERVIEW.md'), 'w').write('\n'.join(md) + '\n')
    print(f'{len(rows)} settings x {len(years)} years ({changed} changed cells) written to {CONFIG}/OVERVIEW.md and .csv')


if __name__ == '__main__':
    main()
