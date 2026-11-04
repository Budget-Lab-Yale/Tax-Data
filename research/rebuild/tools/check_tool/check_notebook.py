"""check_notebook.py: the controls of the check tool's notebook, check_tool.ipynb (README.md). The notebook calls show(),
which reads the tables prepare.py wrote and puts the controls, the table and the plot on the page. Every change to a
control makes them again. The tabulation itself is check_tables.run_query, the same code the web server uses; this
file only turns the controls into a request and the answer into a table of one statistic, with the source, the file
and their difference, and a plot of it. The plot is drawn here as SVG, because the two modules hold no plotting package
(no matplotlib).

Needs numpy, pandas, pyarrow and ipywidgets: on Bouchet, the modules Jupyter-bundle/20250530-GCCcore-13.3.0 and
Arrow/17.0.0-gfbf-2024a.
"""
import html
import math
import time
from contextlib import contextmanager

MODULES = 'Jupyter-bundle/20250530-GCCcore-13.3.0 Arrow/17.0.0-gfbf-2024a'
UNITS = [('all units', 'all'), ('all units except dependent filers', 'nodep'), ('filers only (returns)', 'filers'),
         ('non-filers only', 'nonfilers')]
SOURCE_ORDER = ['puf', 'scf', 'cex', 'cps', 'sipp']
WIDE = {'description_width': 'initial'}


def missing_packages():
    """The packages the notebook needs that this Python cannot import."""
    out = []
    for p in ('numpy', 'pandas', 'pyarrow', 'ipywidgets'):
        try:
            __import__(p)
        except Exception:
            out.append(p)
    return out


def show(folder=None):
    """Read the tables and show the controls and the table. Returns the tool (its .last holds the last table)."""
    miss = missing_packages()
    if miss:
        import sys
        print(f'This notebook cannot start: Python cannot find {", ".join(miss)}.\n'
              f'It is running the Python at {sys.executable}.\n'
              'Close this Jupyter session (in Open OnDemand, My Interactive Sessions, Delete) and start a new one with\n'
              f'this in the field "Additional modules":\n\n    {MODULES}\n\nREADME.md, "Starting it", says which '
              'fields to fill.')
        return None
    from IPython.display import display
    import check_tables
    folder = folder or check_tables.default_folder()
    try:
        data = check_tables.Data(folder)
    except PermissionError:
        print(f'This notebook cannot read the tables in {folder}: permission denied. The folder is readable by the group '
              'pi_nrs36 only; ask John to check that your account is in it.')
        return None
    tool = CheckTool(data)
    display(tool.box)
    return tool


# ---- formatting, as on the web page ---------------------------------------------------------------------------------
def esc(s):
    return html.escape(str(s), quote=True)


def pct(v):
    return '' if v is None else f'{100 * v:.1f}%'


def mil(v):
    return '' if v is None else f'{v / 1e6:,.3f}'


def count(v):
    return '' if v is None else f'{int(round(v)):,}'


def short(v):
    """A number with at most three decimals, as the page's toLocaleString."""
    s = f'{v:,.3f}'.rstrip('0').rstrip('.')
    return '0' if s in ('-0', '') else s


def money(v, unit):
    if v is None:
        return ''
    if unit and unit != 'dollars':
        return short(v)
    return ('-' if v < 0 else '') + '$' + f'{int(abs(v) + 0.5):,}'


def bn(v, unit):
    if v is None:
        return ''
    if unit and unit != 'dollars':
        return short(v / 1e6)
    return f'{v / 1e9:,.2f}'


def x_range(st, unit):
    if st.get('x_min') is None:
        return ''
    return money(st['x_min'], unit) + ' to ' + money(st['x_max'], unit)


CELL = 'border:1px solid #ddd;padding:3px 7px;text-align:right;white-space:nowrap;'
HEAD = CELL + 'background:#f3f3f5;font-weight:600;'
THIN = 'background:#fdecc8;'
SRC = 'background:#f6f8fa;border-left:3px solid #4a7;padding:0.4em 0.8em;font-size:12px;color:#444;'


def heading(r):
    """The title (Y by X) and the box naming the source and the file."""
    return (f'<h3 style="margin:0.8em 0 0.3em 0">{esc(r["y"] + ": " + r["y_label"])}'
            + (f' by {esc(r["x"]["var"] + ": " + r["x_label"])}' if r['x_label'] else '') + '</h3>'
            f'<div style="{SRC}"><b>Source: {esc(r["source"]["name"])}.</b> {esc(r["source"]["what"])}<br>'
            f'<b>File: {esc(r["file"]["name"])}.</b> {esc(r["file"]["what"])}</div>')


def full_table(r, V):
    """Every statistic of one answer of check_tables.run_query, as HTML: the source's columns, then the file's."""
    flag, unit = r['y_kind'] == 'flag', r['y_unit']
    ymiss = any(r['total'][s]['y_missing_records'] > 0 for s in ('source', 'file'))
    xu = V[r['x']['var']]['unit'] if r['x']['var'] else None
    show_range = r['x']['var'] and r['x'].get('mode') != 'category'
    cols = [('sample records', lambda st: count(st['records'])),
            ('units (millions)', lambda st: mil(st['units'])),
            ('share of units', lambda st: pct(st['share_of_units'])),
            ('share yes' if flag else 'share with a nonzero value', lambda st: pct(st['nonzero_share'])),
            ('units with yes (millions)' if flag else ('total ($ billions)' if unit == 'dollars'
                                                        else f'total (millions of {unit})'),
             lambda st: mil(st['total']) if flag else bn(st['total'], unit))]
    if not flag:
        cols.append(('mean per unit', lambda st: money(st['mean'], unit)))
    cols.append(('share of all units with yes' if flag else 'share of the total', lambda st: pct(st['share_of_total'])))
    if ymiss:
        cols.append(('records missing Y', lambda st: count(st['y_missing_records'])))
    if show_range:
        cols.append(('X range in the group', lambda st: x_range(st, xu)))
    n = len(cols)
    h = ['<div style="overflow-x:auto"><table style="border-collapse:collapse;margin-top:0.6em;'
         'font-variant-numeric:tabular-nums;font-size:13px">',
         f'<tr><th rowspan=2 style="{HEAD}">{esc(r["x"]["var"] if r["x_label"] else "")}</th>'
         f'<th colspan={n} style="{HEAD}text-align:center">source: {esc(r["source"]["name"])}</th>'
         f'<th colspan={n} style="{HEAD}text-align:center">file: {esc(r["file"]["name"])}</th></tr><tr>'
         + ''.join(f'<th style="{HEAD}">{esc(c[0])}</th>' for _ in range(2) for c in cols) + '</tr>']

    def line(row, total=False):
        extra = 'font-weight:600;border-top:2px solid #999;' if total else ''
        lab = esc(row['label']) + (' (thin)' if row['thin'] else '')
        s = f'<tr><td style="{CELL}{extra}text-align:left;{THIN if row["thin"] else ""}">{lab}</td>'
        for side in ('source', 'file'):
            thin = row[side]['records'] < r['thin_cell_records']
            for c in cols:
                s += f'<td style="{CELL}{extra}{THIN if thin else ""}">{c[1](row[side])}</td>'
        return s + '</tr>'

    h += [line(row) for row in r['rows']] + [line(r['total'], True), '</table></div>']
    return '\n'.join(h)


def notes(r, V):
    """The notes under the table: how it was made, as a list of sentences."""
    xu = V[r['x']['var']]['unit'] if r['x']['var'] else None
    out = [f'Shaded cells rest on fewer than {r["thin_cell_records"]} sample records in that file; a row marked thin '
           'has fewer in either file. In the plot, hollow points or pale bars mark the same groups.',
           'Shares with a nonzero value, totals and means are over the units whose Y is not missing. Weights are each '
           "file's own."]
    x = r['x']
    if r['cuts']['source'] and x.get('mode') == 'percentiles':
        out.append('Cutoffs from the source, applied to both files: '
                   + ', '.join(money(v, xu) for v in r['cuts']['source']) + '.')
    if x.get('mode') == 'percentiles' and x.get('rank') == 'own':
        out.append('Each file is ranked within itself; units tied at one value (zero, say) are split between groups '
                   'by their order in the file. The full tables show the lowest and highest X in each group.')
    if x.get('mode') == 'percentiles':
        out.append('Percentiles of ' + ('all units of the chosen units, before the other filters'
                                        if x.get('base') == 'all' else 'the filtered units') + '.')
    for side in ('source', 'file'):
        for f in r['filter_missing'][side]:
            out.append(f'The {side} has {count(f["records"])} records ({mil(f["units"])} million units) missing '
                       f'{esc(f["var"])}; the filter on it leaves them out.')
    if r['forbes_in_file']:
        out.append(f"The file's part of the table holds {r['forbes_in_file']} Forbes-list records.")
    p = r['population']
    out.append(f'Population before the filters: source {count(p["source"]["records"])} records, '
               f'{mil(p["source"]["units"])} million units; file {count(p["file"]["records"])} records, '
               f'{mil(p["file"]["units"])} million units.')
    return out


def notes_html(items):
    return '<ul style="font-size:12px;color:#555">' + ''.join(f'<li>{t}</li>' for t in items) + '</ul>'


def render(r, V):
    """The full view of one answer: the title, both files' tables with every statistic, and the notes."""
    return heading(r) + '\n' + full_table(r, V) + '\n' + notes_html(notes(r, V))


# ---- one statistic at a time: the compact table and the plot --------------------------------------------------------
# The statistics the dropdown offers: the key, its name for an amount, its name for a yes-or-no item (None: not offered
# for one, because there it repeats another: a yes-or-no item's total is its number of units answering yes).
STATS = [('nonzero_share', 'share of units with a nonzero value', 'share of units answering yes'),
         ('nonzero_units', 'number of units with a nonzero value', 'number of units answering yes'),
         ('total', 'weighted total', None),
         ('mean', 'mean per unit (zeros included)', None),
         ('mean_nonzero', 'mean among units with a nonzero value', None),
         ('share_of_total', "each group's share of the total", "each group's share of all units answering yes")]
SHARES = ('nonzero_share', 'share_of_total')
SOURCE_COLOR, FILE_COLOR = '#2a78d6', '#eb6834'      # blue and orange: the first two of a palette checked for colour blindness
INK, INK2, GRID = '#0b0b0b', '#52514e', '#e6e5e0'
MUTED = 'color:#666;font-size:12px;'


def stat_options(flag):
    return [(fl if flag else am, k) for k, am, fl in STATS if not (flag and fl is None)]


def stat_name(key, flag):
    return dict((k, fl if flag else am) for k, am, fl in STATS)[key]


def stat_value(st, key):
    """One statistic of one cell, from check_tables.cell_stats (None where it does not exist)."""
    if key == 'mean_nonzero':
        return st['total'] / st['nonzero_units'] if st['nonzero_units'] > 0 else None
    return st[key]


def stat_scale(key, unit, flag):
    """How a statistic is shown: the number it is divided by, its unit in words, and whether it is in dollars."""
    dollars = (unit or 'dollars') == 'dollars' and not flag
    if key in SHARES:
        return 0.01, 'percent', False
    if key == 'nonzero_units':
        return 1e6, 'millions of units', False
    if key == 'total':
        return (1e9, '$ billions', False) if dollars else (1e6, f'millions of {unit}', False)
    per = 'per unit' if key == 'mean' else 'per unit with a nonzero value'
    return (1, f'dollars {per}', True) if dollars else (1, f'{unit} {per}', False)


def decimals(big):
    """Decimals for totals in millions or billions, from the largest number shown (one for the column)."""
    return 1 if big >= 100 else 2 if big >= 1 else 3


def stat_cell(v, key, unit, flag, dec=None):
    """A statistic as the compact table shows it, in the unit stat_scale names. dec: the decimals of counts and
    totals (decimals() of the column's largest number); by default, from the number itself."""
    if v is None:
        return ''
    div, _, dollars = stat_scale(key, unit, flag)
    s = v / div
    if key in SHARES:
        return f'{s:.1f}%'
    if dollars:
        return ('-' if s < 0 else '') + '$' + f'{int(abs(s) + 0.5):,}'
    if key == 'nonzero_units':           # millions of units to the thousand, as in the full tables
        return f'{s:,.3f}'
    if key == 'total':
        return f'{s:,.{decimals(abs(s)) if dec is None else dec}f}'
    return short(s)


def pct_diff(s, f):
    """(file - source) / source; divided by the source's size where it is negative, so plus means the file is higher."""
    if s is None or f is None or s == 0:
        return None
    return (f - s) / abs(s)


def compact_table(r, key):
    """One statistic by X group: source, file, the difference, and each file's sample records."""
    flag, unit = r['y_kind'] == 'flag', r['y_unit']
    name = stat_name(key, flag)
    _, words, _ = stat_scale(key, unit, flag)
    ymiss = any(r['total'][s]['y_missing_records'] > 0 for s in ('source', 'file'))
    share = key in SHARES
    thin_at = r['thin_cell_records']
    div = stat_scale(key, unit, flag)[0]
    dec = decimals(max([abs(stat_value(rw[sd], key) or 0) / div for rw in r['rows'] + [r['total']]
                        for sd in ('source', 'file')]))
    heads = [f'source: {r["source"]["name"]}', r['file']['name']]
    heads += (['difference (percentage points, file minus source)'] if share else [])
    heads += ['percent difference, (file − source) ÷ source']
    small = ['sample records: source', 'sample records: file']
    if ymiss:
        small += ['records missing Y: source', 'records missing Y: file']
    h = [f'<div style="margin:0.8em 0 0.2em 0;font-size:14px"><b>{esc(name[0].upper() + name[1:])}</b>, '
         f'{esc(words)}</div>',
         '<div style="overflow-x:auto"><table style="border-collapse:collapse;margin-top:0.3em;'
         'font-variant-numeric:tabular-nums;font-size:13px">',
         f'<tr><th style="{HEAD}text-align:left;white-space:normal;max-width:18em">{esc(r["x"]["var"] + ": " + r["x_label"] if r["x_label"] else "")}</th>'
         + ''.join(f'<th style="{HEAD}white-space:normal;max-width:12em">{esc(t)}</th>' for t in heads)
         + ''.join(f'<th style="{HEAD}{MUTED}font-weight:400;white-space:normal;max-width:7em">{esc(t)}</th>'
                   for t in small) + '</tr>']

    def line(row, total=False):
        extra = 'font-weight:600;border-top:2px solid #999;' if total else ''
        thin = {s: row[s]['records'] < thin_at for s in ('source', 'file')}
        v = {s: stat_value(row[s], key) for s in ('source', 'file')}
        cells = [(stat_cell(v['source'], key, unit, flag, dec), thin['source']),
                 (stat_cell(v['file'], key, unit, flag, dec), thin['file'])]
        if share:
            d = None if v['source'] is None or v['file'] is None else 100 * (v['file'] - v['source'])
            cells.append(('' if d is None else f'{d:+.1f}', False))
        d = pct_diff(v['source'], v['file'])
        cells.append(('' if d is None else f'{100 * d:+.1f}%', False))
        s = (f'<tr><td style="{CELL}{extra}text-align:left;{THIN if row["thin"] else ""}">'
             f'{esc(row["label"])}{" (thin)" if row["thin"] else ""}</td>')
        s += ''.join(f'<td style="{CELL}{extra}{THIN if t else ""}">{c}</td>' for c, t in cells)
        s += ''.join(f'<td style="{CELL}{extra}{MUTED}{THIN if thin[side] else ""}">{count(row[side]["records"])}</td>'
                     for side in ('source', 'file'))
        if ymiss:
            s += ''.join(f'<td style="{CELL}{extra}{MUTED}">{count(row[side]["y_missing_records"])}</td>'
                         for side in ('source', 'file'))
        return s + '</tr>'

    h += [line(row) for row in r['rows']] + [line(r['total'], True), '</table></div>']
    return '\n'.join(h)


def compact_notes(r, key):
    """The notes the compact table adds to notes()."""
    out = ['Percent difference: the file\'s value minus the source\'s, divided by the source\'s; a plus sign means the '
           'file is higher. Where the source\'s value is negative it is divided by its size, so plus still means '
           'higher. It is blank where the source\'s value is zero.']
    if key in SHARES:
        out.append('For a share, the difference in percentage points is the file\'s share minus the source\'s: a '
                   'share of 12.0% in the source and 15.0% in the file is +3.0 points, or +25.0%.')
    if key == 'share_of_total' and any(r['total'][s]['total'] < 0 for s in ('source', 'file')):
        out.append('The total of all units shown is negative in a file, so its shares of the total have the opposite '
                   'sign to the groups\' totals.')
    if ordered(r) and any(row['missing_x'] for row in r['rows']):
        out.append('The plot leaves out the row of units whose X is missing; it is in the table.')
    return out


def ordered(r):
    """Whether the groups are in an order (percentiles or cutoffs), so the plot draws lines rather than bars."""
    return bool(r['x']['var']) and r['x'].get('mode') in ('percentiles', 'dollars')


def nice_ticks(lo, hi, n=5):
    """Round tick values spanning lo to hi, and the number of decimals they need."""
    if hi <= lo:
        hi = lo + 1
    raw = (hi - lo) / n
    mag = 10 ** math.floor(math.log10(raw))
    step = next(m * mag for m in (1, 2, 2.5, 5, 10) if m * mag >= raw * (1 - 1e-9))
    t0, t1 = math.floor(lo / step + 1e-9) * step, math.ceil(hi / step - 1e-9) * step
    ticks = [t0 + i * step for i in range(int(round((t1 - t0) / step)) + 1)]
    dec = 0
    while dec < 6 and abs(step * 10 ** dec - round(step * 10 ** dec)) > 1e-6:
        dec += 1
    return ticks, dec


def clip(s, n):
    s = str(s)
    return s if len(s) <= n else s[:n - 1] + '…'


def svg_plot(r, key):
    """The statistic by X group, source and file side by side, as an SVG drawing: two lines over groups in an order
    (percentiles, cutoffs), grouped bars otherwise. Hovering over a point or bar shows its value."""
    flag, unit = r['y_kind'] == 'flag', r['y_unit']
    div, words, dollars = stat_scale(key, unit, flag)
    rows = [row for row in r['rows'] if not (ordered(r) and row['missing_x'])]
    if not rows:
        return ''
    sides = [('source', f'source: {r["source"]["name"]}', SOURCE_COLOR), ('file', r['file']['name'], FILE_COLOR)]
    val = {s: [None if stat_value(row[s], key) is None else stat_value(row[s], key) / div for row in rows]
           for s, _, _ in sides}
    allv = [v for s, _, _ in sides for v in val[s] if v is not None]
    lo, hi = min([0.0] + allv), max([0.0] + allv)
    if hi == lo:
        hi = lo + 1
    ticks, dec = nice_ticks(lo, hi)
    lo, hi = ticks[0], ticks[-1]

    def tick(v):
        s = f'{abs(v):,.{dec}f}'
        return ('-' if v < 0 and float(s.replace(',', '')) else '') + ('$' if dollars else '') + s \
            + ('%' if key in SHARES else '')

    n = len(rows)
    labels = [clip(row['label'], 30) for row in rows]
    turn = n > 6 or max(len(t) for t in labels) > 14
    W_, ml, mr, mt = 780, 84, 18, 66
    mb = 46 + (int(max(len(t) for t in labels) * 6.2 * 0.6) + 8 if turn else 16)
    ph = 250
    H = mt + ph + mb
    pw = W_ - ml - mr
    Y = lambda v: mt + ph * (hi - v) / (hi - lo)
    band = pw / n
    cx = lambda i: ml + band * (i + 0.5)
    font = 'font-family="sans-serif"'
    o = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W_}" height="{H}" viewBox="0 0 {W_} {H}" '
         f'style="max-width:100%;height:auto">',
         f'<rect width="{W_}" height="{H}" fill="#ffffff"/>',
         f'<text x="{ml}" y="18" {font} font-size="14" font-weight="600" fill="{INK}">'
         f'{esc(clip(r["y"] + ": " + stat_name(key, flag), 95))}</text>']
    lx = ml
    for _, lab, col in sides:                 # the legend
        o.append(f'<rect x="{lx}" y="29" width="12" height="12" rx="2" fill="{col}"/>'
                 f'<text x="{lx + 17}" y="39.5" {font} font-size="12" fill="{INK2}">{esc(lab)}</text>')
        lx += 17 + int(len(lab) * 6.8) + 22
    o.append(f'<text x="{ml - 8}" y="{mt - 9}" {font} font-size="11" fill="{INK2}" text-anchor="end">'
             f'{esc(words)}</text>')
    for t in ticks:                           # the grid and the Y axis's values
        o.append(f'<line x1="{ml}" x2="{W_ - mr}" y1="{Y(t):.1f}" y2="{Y(t):.1f}" stroke="{GRID}" stroke-width="1"/>'
                 f'<text x="{ml - 8}" y="{Y(t) + 4:.1f}" {font} font-size="11" fill="{INK2}" text-anchor="end">'
                 f'{esc(tick(t))}</text>')
    o.append(f'<line x1="{ml}" x2="{W_ - mr}" y1="{Y(0):.1f}" y2="{Y(0):.1f}" stroke="#8a8984" stroke-width="1"/>')
    for i, t in enumerate(labels):            # the X groups
        x, y = cx(i), mt + ph + 16
        if turn:
            o.append(f'<text x="{x:.1f}" y="{y}" {font} font-size="11" fill="{INK2}" text-anchor="end" '
                     f'transform="rotate(-35 {x:.1f} {y})">{esc(t)}</text>')
        else:
            o.append(f'<text x="{x:.1f}" y="{y}" {font} font-size="11" fill="{INK2}" text-anchor="middle">{esc(t)}</text>')
    how = ''                                  # how X was cut, kept whole; the label is shortened to fit instead
    if r['x'].get('mode') == 'percentiles':
        how = (', percentile groups, each file ranked within itself' if r['x'].get('rank') == 'own'
               else ", groups cut at the source's percentiles")
    elif r['x'].get('mode') == 'dollars':
        how = ', groups between the cutoffs typed'
    xtitle = (r['x']['var'] + ': ' + clip(r['x_label'], max(20, 108 - len(r['x']['var']) - len(how))) + how
              if r['x_label'] else 'all units shown')
    o.append(f'<text x="{ml + pw / 2:.1f}" y="{H - 10}" {font} font-size="12" fill="{INK}" text-anchor="middle">'
             f'{esc(xtitle)}</text>')

    def hover(i, s, lab):
        row, v = rows[i], val[s][i]
        st = row[s]
        txt = f'{row["label"]}, {lab}: ' + (stat_cell(stat_value(st, key), key, unit, flag) if v is not None else 'none')
        txt += f'; {count(st["records"])} sample records' + (' (thin)' if st['records'] < r['thin_cell_records'] else '')
        return f'<title>{esc(txt)}</title>'

    thin = {s: [row[s]['records'] < r['thin_cell_records'] for row in rows] for s, _, _ in sides}
    if ordered(r):                            # two lines
        for s, lab, col in sides:
            seg = []
            for i, v in enumerate(val[s] + [None]):
                if v is not None:
                    seg.append(f'{cx(i):.1f},{Y(v):.1f}')
                elif seg:
                    o.append(f'<polyline points="{" ".join(seg)}" fill="none" stroke="{col}" stroke-width="2" '
                             'stroke-linejoin="round"/>')
                    seg = []
        for s, lab, col in sides:
            for i, v in enumerate(val[s]):
                if v is None:
                    continue
                fill = '#ffffff' if thin[s][i] else col
                o.append(f'<circle cx="{cx(i):.1f}" cy="{Y(v):.1f}" r="4.5" fill="{fill}" stroke="{col if thin[s][i] else "#ffffff"}" '
                         f'stroke-width="2">{hover(i, s, lab)}</circle>')
    else:                                     # grouped bars, a 2-pixel gap between the two
        bw = max(4.0, min(34.0, band * 0.36))
        for k, (s, lab, col) in enumerate(sides):
            for i, v in enumerate(val[s]):
                if v is None:
                    continue
                x0 = cx(i) - bw - 1 if k == 0 else cx(i) + 1
                y0, y1 = Y(0), Y(v)
                hgt = abs(y0 - y1)
                rad = min(4.0, bw / 2, hgt)
                sg = -1 if v >= 0 else 1          # the rounded end points away from zero
                path = (f'M{x0:.1f},{y0:.1f} V{y1 - sg * rad:.1f} Q{x0:.1f},{y1:.1f} {x0 + rad:.1f},{y1:.1f} '
                        f'H{x0 + bw - rad:.1f} Q{x0 + bw:.1f},{y1:.1f} {x0 + bw:.1f},{y1 - sg * rad:.1f} V{y0:.1f} Z')
                style = (f'fill="{col}" fill-opacity="0.3" stroke="{col}" stroke-width="1.5"' if thin[s][i]
                         else f'fill="{col}"')
                o.append(f'<path d="{path}" {style}>{hover(i, s, lab)}</path>')
    o.append('</svg>')
    return '\n'.join(o)


# ---- the controls ---------------------------------------------------------------------------------------------------
class FilterRow:
    """One filter: a variable, a condition (greater than, less than, between; or is, for a variable with categories)
    and its value or values."""

    def __init__(self, tool):
        import ipywidgets as W
        self.tool = tool
        self.var = W.Dropdown(layout=W.Layout(width='34em'))
        self.op = W.Dropdown(layout=W.Layout(width='11em'))
        self.a = W.Text(placeholder='a number', continuous_update=False, layout=W.Layout(width='10em'))
        self.b = W.Text(placeholder='a number', continuous_update=False, layout=W.Layout(width='10em'))
        self.and_ = W.HTML(' and ')
        self.cat = W.Dropdown(layout=W.Layout(width='20em'))
        self.remove = W.Button(description='remove', layout=W.Layout(width='7em'))
        self.box = W.HBox([self.var, self.op, self.a, self.and_, self.b, self.cat, self.remove])
        with tool.hold():
            self.set_vars()
        self.var.observe(self.on_var, 'value')
        self.op.observe(self.on_op, 'value')
        for w in (self.a, self.b, self.cat):
            w.observe(tool.changed, 'value')
        self.remove.on_click(lambda _: tool.remove_filter(self))

    def set_vars(self, keep=None):
        V = self.tool.V
        xs = sorted(v for v in self.tool.allowed() if v != 'filing_status')
        self.var.options = [(self.tool.var_label(v), v) for v in xs]
        if keep in xs:
            self.var.value = keep
        elif 'age1' in xs:
            self.var.value = 'age1'
        elif 'agi' in xs:
            self.var.value = 'agi'
        self.set_ops()

    def is_category(self):
        return self.tool.V[self.var.value]['kind'] != 'amount'

    def set_ops(self):
        v = self.var.value
        if self.is_category():
            self.op.options = [('is', 'is')]
            cats = self.tool.V[v]['categories']
            self.cat.options = [(lab, k) for k, lab in cats.items()] if cats else [('0', '0'), ('1', '1')]
        else:
            keep = self.op.value if self.op.value in ('gt', 'lt', 'between') else 'gt'
            self.op.options = [('greater than', 'gt'), ('less than', 'lt'), ('between', 'between')]
            self.op.value = keep
        self.draw()

    def draw(self):
        cat, between = self.is_category(), self.op.value == 'between'
        self.cat.layout.display = '' if cat else 'none'
        self.a.layout.display = 'none' if cat else ''
        self.and_.layout.display = self.b.layout.display = '' if (between and not cat) else 'none'

    def on_var(self, _):
        with self.tool.hold():
            self.set_ops()
        self.tool.changed()

    def on_op(self, _):
        self.draw()
        self.tool.changed()

    def spec(self):
        """The filter as the tabulation reads it."""
        import check_tables
        v, op = self.var.value, self.op.value
        if op == 'is':
            return {'var': v, 'op': 'is', 'values': [self.cat.value]}

        def num(w):
            t = w.value.strip().replace(',', '').replace('$', '')
            if t == '':
                raise check_tables.QueryError(f'A filter on {v} has no value.')
            try:
                return float(t)
            except ValueError:
                raise check_tables.QueryError(f'A filter on {v}: "{w.value}" is not a number.')
        return {'var': v, 'op': op, 'a': num(self.a), 'b': num(self.b) if op == 'between' else None}


class CheckTool:
    """The notebook's controls and table. Every change to a control calls refresh(), which makes the table again."""

    def __init__(self, data):
        import ipywidgets as W
        self.data, self.cat, self.V = data, data.catalog, data.catalog['variables']
        self.filters, self._held = [], 0
        self.last = self.last_query = self.last_error = None
        C = self.cat
        L = lambda w: W.Layout(width=w)
        self.head = W.HTML(
            f'<b>The posttrained file against its sources.</b> Year {C["year"]}. File: {esc(C["file"])}. Source for '
            f'PUF items: <b>{esc(C["sources"]["puf"]["name"])}</b> (setting puf_source = {esc(C["puf_source"])}). '
            f'Tables built {esc(C["built"])}. ' + ' '.join(esc(n) for n in C['notes']))
        ys = []
        for s in SOURCE_ORDER:
            ys += [(f'{C["sources"][s]["name"]}: {self.var_label(v)}', v)
                   for v in sorted(v for v in self.V if self.V[v].get('y_source') == s)]
        self.y = W.Dropdown(options=ys, value='wages' if 'wages' in self.V else ys[0][1], description='Y, the item:',
                            style=WIDE, layout=L('60em'))
        self.ysrc = W.HTML()
        self.units = W.Dropdown(options=UNITS, value='all', description='Units:', style=WIDE, layout=L('28em'))
        self.forbes = W.Checkbox(description="leave out the file's Forbes-list records", indent=False, layout=L('30em'))
        self.fs = W.Dropdown(description='Filing status:', style=WIDE, layout=L('28em'))
        self.fsnote = W.HTML()
        self.filter_box = W.VBox([])
        self.addf = W.Button(description='Add a filter', layout=L('10em'))
        self.x = W.Dropdown(description='X, the groups:', style=WIDE, layout=L('60em'))
        self.xmode = W.RadioButtons(options=[('percentiles', 'percentiles'),
                                             ("cutoffs I type (in the variable's own units)", 'dollars')],
                                    value='percentiles', layout=L('auto'))
        self.pcts = W.Text(value=', '.join(f'{p:g}' for p in C['default_percentiles']), description='Percentiles:',
                           continuous_update=False, style=WIDE, layout=L('40em'))
        self.rank = W.RadioButtons(options=[('(a) each file ranked within itself', 'own'),
                                            ('(b) cutoffs taken from the source, applied to both files', 'source')],
                                   value='own', layout=L('auto'))
        self.base = W.RadioButtons(options=[('percentiles of the filtered units', 'filtered'),
                                            ('percentiles of all units (the units choice above, before the other '
                                             'filters)', 'all')], value='filtered', layout=L('auto'))
        self.cuts = W.Text(value=', '.join(f'{c:g}' for c in C['default_dollar_cutoffs']), description='Cutoffs:',
                           continuous_update=False, style=WIDE, layout=L('40em'))
        self.pctrow = W.VBox([self.pcts, self.rank, self.base])
        self.cutrow = W.VBox([self.cuts, W.HTML('<span style="font-size:12px;color:#555">A unit exactly at a cutoff '
                                                'goes to the group above it.</span>')])
        self.xamount = W.VBox([self.xmode, self.pctrow, self.cutrow])
        self.stat = W.Dropdown(options=stat_options(False), value='nonzero_share', description='Statistic:',
                               style=WIDE, layout=L('34em'))
        self.full = W.Checkbox(description='also show the full tables (every statistic, each file)', indent=False,
                               layout=L('32em'))
        self.status = W.HTML()
        self.result = W.HTML()           # the title, the source box and the compact table
        self.plot = W.HTML()             # the plot, an SVG drawing
        self.notes = W.HTML()            # the notes, and the full tables when asked for
        note = lambda t: W.HTML(f'<span style="font-size:12px;color:#555">{t}</span>')
        section = lambda t: W.HTML(f'<b style="font-size:14px">{t}</b>')
        self.box = W.VBox([
            self.head,
            section('Y: the item tabulated'), self.y, self.ysrc,
            section('Filters (applied to both files)'), W.HBox([self.units, self.forbes]), W.HBox([self.fs, self.fsnote]),
            self.filter_box,
            W.HBox([self.addf, note('Each filter keeps the units whose value is greater than, less than, or between '
                                    'two values (both ends included), or in a category. A unit missing the '
                                    "filter's variable is left out, and the table says how many. Type a number "
                                    'and press Enter.')]),
            section('X: the groups'), self.x, self.xamount,
            section('The statistic shown'), W.HBox([self.stat, self.full]),
            note('The table and the plot are made again whenever a control changes. After typing in a box, press '
                 'Enter.'),
            self.status, self.result, self.plot, self.notes])
        with self.hold():
            self.on_y()
        self.y.observe(self._on_y, 'value')
        self.x.observe(self._on_x, 'value')
        self.xmode.observe(self._on_x, 'value')
        for w in (self.units, self.forbes, self.fs, self.pcts, self.rank, self.base, self.cuts):
            w.observe(self.changed, 'value')
        self.addf.on_click(lambda _: self.add_filter())
        self.stat.observe(self._on_stat, 'value')
        self.full.observe(self._on_stat, 'value')
        self.refresh()

    # -- helpers
    def var_label(self, v):
        u = self.V[v].get('unit')
        return f'{v}: {self.V[v]["label"]}' + (f' ({u})' if u and u != 'dollars' else '')

    def source(self):
        return self.cat['sources'][self.V[self.y.value]['y_source']]

    def allowed(self):
        return self.source()['x']

    @contextmanager
    def hold(self):
        """Changes made inside do not make the table; the caller makes it once at the end."""
        self._held += 1
        try:
            yield
        finally:
            self._held -= 1

    def changed(self, *_):
        if not self._held:
            self.refresh()

    # -- what each control does to the others, as on the web page
    def _on_y(self, _):
        with self.hold():
            self.on_y()
        self.refresh()

    def _on_stat(self, _):
        if not self._held:               # the statistic and the full tables need no new tabulation, only a new drawing
            self.draw()

    def _on_x(self, _):
        self.on_x()
        self.changed()

    def on_y(self):
        y, s = self.y.value, self.source()
        ysrc = self.V[y]['y_source']
        self.ysrc.value = (f'<div style="{SRC}"><b>Source for this item: {esc(s["name"])}.</b> {esc(s["what"])} '
                           f'Sample records are counted as {esc(s["count"])}. '
                           + ('' if ysrc == 'puf' else 'X and the filters offer only the variables the network was '
                                                       'given for this survey when it drew its items.') + '</div>')
        xs = self.allowed()
        keep = self.x.value
        opts = [v for v in sorted(xs) if v != y]
        self.x.options = [('none (one row: every unit)', '')] + [(self.var_label(v), v) for v in opts]
        self.x.value = keep if keep in opts else next((v for v in ('agi', 'wages') if v in opts), '')
        self.on_x()
        keep = self.fs.value
        if 'filing_status' in xs:
            cats = self.V['filing_status']['categories']
            self.fs.options = [('any', '')] + [(lab, k) for k, lab in cats.items()]
            self.fs.value = keep if keep in cats else ''
            self.fs.disabled, self.fsnote.value = False, ''
        else:
            self.fs.options = [('any', '')]
            self.fs.value, self.fs.disabled = '', True
            self.fsnote.value = '<span style="font-size:12px;color:#555">This source has no filing status.</span>'
        flag = self.V[y]['kind'] == 'flag'
        keep = self.stat.value
        self.stat.options = stat_options(flag)
        self.stat.value = keep if keep in [k for _, k in self.stat.options] else 'nonzero_share'
        keep = self.units.value
        self.units.options = UNITS if s['has_filer'] else UNITS[:2]
        self.units.value = keep if keep in [u for _, u in self.units.options] else 'all'
        if ysrc not in ('puf', 'cps') and self.units.value == 'all':
            self.units.value = 'nodep'
        for row in list(self.filters):
            if row.var.value not in xs:
                self.remove_filter(row, refresh=False)
            else:
                row.set_vars(row.var.value)

    def on_x(self):
        x = self.x.value
        self.xamount.layout.display = '' if (x and self.V[x]['kind'] == 'amount') else 'none'
        self.pctrow.layout.display = '' if self.xmode.value == 'percentiles' else 'none'
        self.cutrow.layout.display = '' if self.xmode.value == 'dollars' else 'none'

    def add_filter(self):
        row = FilterRow(self)
        self.filters.append(row)
        self.filter_box.children = [r.box for r in self.filters]
        self.changed()
        return row

    def remove_filter(self, row, refresh=True):
        self.filters.remove(row)
        self.filter_box.children = [r.box for r in self.filters]
        if refresh:
            self.changed()

    # -- the request and the table
    def query(self):
        """The controls as a request to check_tables.run_query (the same request the web page sends)."""
        y, x = self.y.value, self.x.value
        filters = []
        if self.fs.value and not self.fs.disabled:
            filters.append({'var': 'filing_status', 'op': 'is', 'values': [self.fs.value]})
        filters += [row.spec() for row in self.filters]
        q = {'y': y, 'units': self.units.value, 'forbes': 'drop' if self.forbes.value else 'keep', 'filters': filters,
             'x': {'var': x or None}}
        if x and self.V[x]['kind'] == 'amount':
            q['x'].update(mode=self.xmode.value, cuts=self.pcts.value if self.xmode.value == 'percentiles'
                          else self.cuts.value, rank=self.rank.value, base=self.base.value)
        return q

    def refresh(self):
        import check_tables
        t0 = time.time()
        try:
            q = self.query()
            r = check_tables.run_query(self.data, q)
        except check_tables.QueryError as e:
            return self.fail(str(e))
        except Exception as e:           # a bug: say what, so it can be reported
            return self.fail(f'{type(e).__name__}: {e}')
        self.last, self.last_query, self.last_error = r, q, None
        self.draw()
        self.status.value = f'<span style="font-size:12px;color:#555">Table made in {time.time() - t0:.2f} seconds.</span>'

    def draw(self):
        """The compact table of the chosen statistic, its plot and the notes, from the last answer."""
        r, key = self.last, self.stat.value
        if r is None:
            return
        self.result.value = heading(r) + '\n' + compact_table(r, key)
        self.plot.value = svg_plot(r, key)
        full = ('<div style="margin-top:1em;font-size:14px"><b>Every statistic, each file</b></div>\n'
                + full_table(r, self.V)) if self.full.value else ''
        self.notes.value = notes_html(compact_notes(r, key) + notes(r, self.V)) + full

    def fail(self, msg):
        self.last, self.last_error = None, msg
        self.result.value = self.plot.value = self.notes.value = ''
        self.status.value = f'<span style="color:#b00020;font-weight:600">{esc(msg)}</span>'
