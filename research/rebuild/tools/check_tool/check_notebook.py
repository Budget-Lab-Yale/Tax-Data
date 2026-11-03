"""check_notebook.py: the controls of the check tool's notebook, check_tool.ipynb (README.md). The notebook calls show(),
which reads the tables prepare.py wrote and puts the controls and the table on the page. Every change to a control
makes the table again. The tabulation itself is check_tables.run_query, the same code the web server uses; this file
only turns the controls into a request and the answer into a table.

Needs numpy, pandas, pyarrow and ipywidgets: on Bouchet, the modules Jupyter-bundle/20250530-GCCcore-13.3.0 and
Arrow/17.0.0-gfbf-2024a.
"""
import html
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


def render(r, V):
    """The table of one answer of check_tables.run_query, as HTML: the source's columns, then the file's."""
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
    h = [f'<h3 style="margin:0.8em 0 0.3em 0">{esc(r["y"] + ": " + r["y_label"])}'
         + (f' by {esc(r["x"]["var"] + ": " + r["x_label"])}' if r['x_label'] else '') + '</h3>',
         f'<div style="{SRC}"><b>Source: {esc(r["source"]["name"])}.</b> {esc(r["source"]["what"])}<br>'
         f'<b>File: {esc(r["file"]["name"])}.</b> {esc(r["file"]["what"])}</div>',
         '<div style="overflow-x:auto"><table style="border-collapse:collapse;margin-top:0.6em;'
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
    notes = [f'Shaded cells rest on fewer than {r["thin_cell_records"]} sample records in that file; a row marked thin '
             'has fewer in either file.',
             'Shares with a nonzero value, totals and means are over the units whose Y is not missing. Weights are each '
             "file's own."]
    x = r['x']
    if r['cuts']['source'] and x.get('mode') == 'percentiles':
        notes.append('Cutoffs from the source, applied to both files: '
                     + ', '.join(money(v, xu) for v in r['cuts']['source']) + '.')
    if x.get('mode') == 'percentiles' and x.get('rank') == 'own':
        notes.append('Each file is ranked within itself; units tied at one value (zero, say) are split between groups '
                     'by their order in the file.')
    if x.get('mode') == 'percentiles':
        notes.append('Percentiles of ' + ('all units of the chosen units, before the other filters'
                                          if x.get('base') == 'all' else 'the filtered units') + '.')
    for side in ('source', 'file'):
        for f in r['filter_missing'][side]:
            notes.append(f'The {side} has {count(f["records"])} records ({mil(f["units"])} million units) missing '
                         f'{esc(f["var"])}; the filter on it leaves them out.')
    if r['forbes_in_file']:
        notes.append(f"The file's part of the table holds {r['forbes_in_file']} Forbes-list records.")
    p = r['population']
    notes.append(f'Population before the filters: source {count(p["source"]["records"])} records, '
                 f'{mil(p["source"]["units"])} million units; file {count(p["file"]["records"])} records, '
                 f'{mil(p["file"]["units"])} million units.')
    h.append('<ul style="font-size:12px;color:#555">' + ''.join(f'<li>{t}</li>' for t in notes) + '</ul>')
    return '\n'.join(h)


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
        self.status = W.HTML()
        self.result = W.HTML()
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
            note('The table is made again whenever a control changes. After typing in a box, press Enter.'),
            self.status, self.result])
        with self.hold():
            self.on_y()
        self.y.observe(self._on_y, 'value')
        self.x.observe(self._on_x, 'value')
        self.xmode.observe(self._on_x, 'value')
        for w in (self.units, self.forbes, self.fs, self.pcts, self.rank, self.base, self.cuts):
            w.observe(self.changed, 'value')
        self.addf.on_click(lambda _: self.add_filter())
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
        self.result.value = render(r, self.V)
        self.status.value = f'<span style="font-size:12px;color:#555">Table made in {time.time() - t0:.2f} seconds.</span>'

    def fail(self, msg):
        self.last, self.last_error = None, msg
        self.result.value = ''
        self.status.value = f'<span style="color:#b00020;font-weight:600">{esc(msg)}</span>'
