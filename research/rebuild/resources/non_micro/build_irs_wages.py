"""irs_wages_by_business_type.csv: wages paid by type of business, from three SOI tables, every year published. Not a
stage; run by hand when SOI posts a new year (6 November 2026: 2015-2023, all three tables).

Data only: each file is downloaded into its own new empty folder <folder>/<file stem>/, and read as data; nothing in a
download is run. Run with python -I from another folder, with the Python modules on PYTHONPATH:
    python -I build_irs_wages.py <folder with the downloads> <output csv>

  sole_prop  SOI Sole Proprietorship Table 1 (<yy>sp01br.xls): Payroll, all nonfarm industries, businesses with and
             without net income. Payroll is salaries and wages plus the cost of labor in cost of sales (SOI's footnote).
             Schedule C only: SOI publishes no such line for Schedule F.
  part       SOI Partnership Table 1 (<yy>pa01.xlsx): Salaries and wages, all industries. Guaranteed payments to
             partners are a separate line in the table, kept in the CSV with used = 0: they are not W-2 wages.
  scorp      SOI Corporation Table 9 (<yy>co09ccr.xlsx, Form 1120-S): Salaries and wages, and Compensation of officers,
             the total column.
Amounts in thousands of dollars, as SOI prints them. Each cell is checked by its row label and its table's title and
tax year."""
import csv
import glob
import os
import re
import sys

sys.path += os.environ['PYTHONPATH'].split(':') + [os.path.expanduser('~/.local/lib/python3.12/site-packages')]
import openpyxl
import xlrd

BASE = 'https://www.irs.gov/pub/irs-soi/'


def grid(p):
    if p.endswith('.xls'):
        sh = xlrd.open_workbook(p).sheet_by_index(0)
        return [sh.row_values(r) for r in range(sh.nrows)]
    sh = openpyxl.load_workbook(p, read_only=True, data_only=True).worksheets[0]
    return [list(r) for r in sh.iter_rows(values_only=True)]


def clean(x):
    return re.sub(r'\s+', ' ', str(x if x is not None else '')).strip()


def num(x):
    assert isinstance(x, (int, float)), f'not a number: {x!r}'
    return float(x)


def one(g, label, col):
    r = [r for r in g if clean(r[0]).lower().startswith(label)]
    assert r, f'no row {label}'
    return num(r[0][col])


def read(folder, yy):
    y, rows = 2000 + yy, []
    def row(kind, line, value, p, used=1):
        rows.append({'year': y, 'business_type': kind, 'line': line, 'thousands': value, 'used': used,
                     'source': f'SOI {BASE}{os.path.basename(p)}'})
    ps = glob.glob(f'{folder}/{yy}sp01br/*')
    if ps:
        g = grid(ps[0])
        assert str(y) in clean(g[0][0]) and 'Nonfarm Sole Proprietorships' in clean(g[0][0]), g[0][0]
        col = [i for r in g[2:7] for i, c in enumerate(r) if clean(c).lower().startswith('payroll')][0]
        assert col == 7
        allr = [r for r in g if re.match(r'all (nonfarm )?industries', clean(r[0]), re.I)][0]
        row('sole_prop', 'Payroll, all nonfarm industries, with and without net income', num(allr[col]), ps[0])
    ps = glob.glob(f'{folder}/{yy}pa01/*')
    if ps:
        g = grid(ps[0])
        assert str(y) in clean(g[0][0]) and 'All Partnerships' in clean(g[0][0]), g[0][0]
        col = [i for r in g[1:6] for i, c in enumerate(r) if clean(c).lower().startswith('all industries')][0]
        row('part', 'Salaries and wages, all industries', one(g, 'salaries and wages', col), ps[0])
        row('part', 'Guaranteed payments to partners, all industries (memo)', one(g, 'guaranteed payments', col), ps[0], 0)
    ps = glob.glob(f'{folder}/{yy}co09ccr/*')
    if ps:
        g = grid(ps[0])
        t = ' '.join(clean(r[0]) for r in g[:3])
        assert str(y) in t and re.search('1120-?S', t), t
        col = [i for r in g[2:6] for i, c in enumerate(r) if clean(c).lower().startswith('total')][0]
        row('scorp', 'Salaries and wages', one(g, 'salaries and wages', col), ps[0])
        row('scorp', 'Compensation of officers', one(g, 'compensation of officers', col), ps[0])
    return rows


if __name__ == '__main__':
    rows = [r for yy in range(15, 30) for r in read(sys.argv[1], yy)]
    with open(sys.argv[2], 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=['year', 'business_type', 'line', 'thousands', 'used', 'source'])
        w.writeheader()
        w.writerows(rows)
    print(f'{len(rows)} rows')
