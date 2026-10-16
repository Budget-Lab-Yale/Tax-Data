"""
soi_cells.py  (minimal example, shared by steps 4 and 5)

What it does. Puts published SOI numbers and our records on the same
footing, as "cells": for each AGI bracket, the number of returns, and for
each of the four income items, the number of returns with a nonzero amount
and the total amount. Step 4 uses it to compare a generated 2015 file with
SOI 2015; step 5 uses it to build the 2019 targets for the tilt and each
record's contribution to them. Writes nothing.

How the items line up with SOI Pub 1304 Table 1.4:
  wages          amount: salaries and wages      count: returns with wages
  interest       amount: taxable interest        count: returns with it
  dividends      amount: ordinary dividends      count: returns with them
  capital_gains  amount: Schedule D net gain minus the deducted Schedule D
                 net loss. Not the 1040 line, which also holds capital gain
                 distributions reported without Schedule D: the PUF keeps
                 those in a separate field (E01100), and its E01000 matches
                 the Schedule D figure bracket by bracket (step 5 report,
                 note of 2026-10-14; 05_check_capital_gains.py).
                 count: returns with a Schedule D net gain plus returns with
                 a Schedule D net loss.
AGI on a record is computed, never stored: the sum of the five parts.
"""
import numpy as np
import pandas as pd

PARTS = ['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']
ITEMS = {  # item: (SOI amount rows with their signs, SOI count rows)
    'wages': ({'wages': 1}, ['wages']),
    'interest': ({'taxable_interest': 1}, ['taxable_interest']),
    'dividends': ({'ordinary_dividends': 1}, ['ordinary_dividends']),
    'capital_gains': ({'sch_d_net_gain': 1, 'sch_d_net_loss': -1},
                      ['sch_d_net_gain', 'sch_d_net_loss']),
}


def agi(df):
    """AGI as the sum of its five parts. Missing parts are an error, not zero."""
    parts = df[PARTS].to_numpy(float)
    assert not np.isnan(parts).any(), 'a record is missing an AGI part'
    return parts.sum(axis=1)


def read_soi(path, year):
    """Table 1.4 rows by AGI bracket (the published total row excluded)."""
    t = pd.read_csv(path)
    t = t[(t['year'] == year) & (t['table'] == 1.4) & (t['filing_status'] == 'all')
          & (t['agi_bracket_label'] != 'all')]
    return t[['agi_bracket_lo', 'agi_bracket_hi', 'agi_bracket_label',
              'variable', 'statistic', 'value']].reset_index(drop=True)


def published_brackets(soi):
    """The brackets as published: a list of (lo, hi, label), lowest first."""
    b = soi[['agi_bracket_lo', 'agi_bracket_hi', 'agi_bracket_label']].drop_duplicates()
    b = b.sort_values('agi_bracket_lo')
    return list(b.itertuples(index=False, name=None))


def collapse_top(soi, brackets, file_size, min_records):
    """Merge brackets from the top down until the top one would hold at
    least min_records records in an equal-weight file of file_size records.
    Thin brackets make targets that a sample can only hit with huge weights.
    """
    ret = soi[(soi['variable'] == 'returns') & (soi['statistic'] == 'count')]
    count = dict(zip(ret['agi_bracket_lo'], ret['value']))
    total = ret['value'].sum()
    out = list(brackets)
    while len(out) > 1:
        lo, hi, label = out[-1]
        top = sum(count[b[0]] for b in brackets if b[0] >= lo)
        if top / total * file_size >= min_records:
            break
        lo2, _, label2 = out[-2]
        out = out[:-2] + [(lo2, hi, f'{label2.split(" under")[0]} or more')]
    return out


def soi_cells(soi, brackets):
    """Published value of every cell, summing published brackets that were merged."""
    rows = []
    for lo, hi, label in brackets:
        s = soi[(soi['agi_bracket_lo'] >= lo) & (soi['agi_bracket_hi'] <= hi)]
        val = s.groupby(['variable', 'statistic'])['value'].sum()
        rows.append({'bracket': label, 'cell': 'returns', 'value': val[('returns', 'count')]})
        for item, (amount_rows, count_rows) in ITEMS.items():
            rows.append({'bracket': label, 'cell': f'{item} count',
                         'value': sum(val[(c, 'count')] for c in count_rows)})
            rows.append({'bracket': label, 'cell': f'{item} amount',
                         'value': sum(sign * val[(a, 'amount')] for a, sign in amount_rows.items())})
    return pd.DataFrame(rows)


def record_cells(df, brackets):
    """Each record's contribution to every cell, in the order soi_cells uses.

    Returns a records-by-cells matrix: 1 or 0 for counts, dollars for amounts.
    The weighted column sums are the file's estimate of each cell.
    """
    a = agi(df)
    cols = []
    for lo, hi, _ in brackets:
        inside = ((a >= lo) & (a < hi)).astype(float)
        cols.append(inside)
        for item in ITEMS:
            x = df[item].to_numpy(float)
            cols.append(inside * (x != 0))
            cols.append(inside * x)
    return np.column_stack(cols)


def compare(df, weight, soi, brackets):
    """Published cells against a file's weighted estimates, with the gap."""
    cells = soi_cells(soi, brackets)
    contrib = record_cells(df, brackets)
    cells['file'] = contrib.T @ np.asarray(weight, float)
    cells['records'] = (contrib != 0).sum(axis=0)
    cells['gap_pct'] = 100 * (cells['file'] / cells['value'] - 1)
    return cells
