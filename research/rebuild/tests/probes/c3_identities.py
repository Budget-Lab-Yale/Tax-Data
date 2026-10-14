"""Probe C (test 4): the totals that are computed from their parts (D25).
Shared by c3_prepare.py and c3_compare.py.

Model variables that replace totals, each in the total's column:
  AGI_REMAINDER   AGI less the income items plus the adjustments (probe AS,
                  as_agi.py; the income items use the totals below, computed
                  first)
  SCHE_REMAINDER  total Schedule E (E02000) less partnership and S corporation
                  (E26270), rent and royalty income less loss (E25850 - E25860)
                  and estate and trust income less loss (E26390 - E26400):
                  farm rental, REMICs and anything else on Schedule E line 26
                  that the model does not carry
  DIV_NONQUAL     ordinary dividends (E00600) less qualified dividends (E00650)
  PENS_NONTAX     total pensions and annuities (E01500) less the taxable part
                  (E01700)
Computed after generation:
  E02000 = E26270 + E25850 - E25860 + E26390 - E26400 + SCHE_REMAINDER
  E00600 = E00650 + DIV_NONQUAL
  E01500 = E01700 + PENS_NONTAX
  E00100 = items less adjustments (as_agi.py) + AGI_REMAINDER
"""
import numpy as np
import as_agi

SCHE_PIECES = [("E26270", 1.0), ("E25850", 1.0), ("E25860", -1.0), ("E26390", 1.0), ("E26400", -1.0)]
REPLACE = {"E00100": "AGI_REMAINDER", "E02000": "SCHE_REMAINDER", "E00600": "DIV_NONQUAL", "E01500": "PENS_NONTAX"}


def col(df, c):
    return df[c].to_numpy(float)


def sche_pieces(df):
    return sum(s * col(df, c) for c, s in SCHE_PIECES)


def add_totals(df):
    """Compute E02000, E00600, E01500 and E00100 from the parts, in place."""
    df["E02000"] = sche_pieces(df) + col(df, "SCHE_REMAINDER")
    df["E00600"] = col(df, "E00650") + col(df, "DIV_NONQUAL")
    df["E01500"] = col(df, "E01700") + col(df, "PENS_NONTAX")
    df["E00100"] = as_agi.items_less_adjustments(df) + col(df, "AGI_REMAINDER")
    return df
