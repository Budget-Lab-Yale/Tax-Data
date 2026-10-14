"""Probe AS: AGI as the sum of its income items, less its adjustments, plus a
remainder. Shared by as_prepare.py, as_compare.py and as_check.py.

2015 Form 1040 lines, as far as the 40 PUF-side model variables carry them:

Income items (added):
  line 7   wages                      E00200
  line 8a  taxable interest           E00300
  line 9a  ordinary dividends         E00600
  line 12  Schedule C                 E00900
  line 13  capital gain or loss       E01100 (distributions reported without
           Schedule D) + max(P22250 + P23250, -3,000), the limit -1,500 for
           married filing separately (MARS 3)
  line 15b taxable IRA distributions  E01400
  line 16b taxable pensions           E01700
  line 17  Schedule E total           E02000 (its pieces E26270, E25850,
           E25860, E26390, E26400 are inside it and are not added again)
  line 18  Schedule F                 E02100
  line 19  unemployment compensation  E02300
Adjustments (subtracted):
  line 29  self-employed health insurance  E03270
  line 32  IRA deduction                   E03150
  line 33  student loan interest           E03210
Remainder = AGI - income items + adjustments: taxable refunds (line 10),
alimony received (11), Form 4797 gains (14), taxable Social Security (20b),
other income (21), less the adjustments not in the model (half of
self-employment tax, SEP and similar plans, HSA, moving, educator, early
withdrawal penalty, alimony paid, tuition, domestic production activities),
plus any inconsistency in the file itself.
"""
import numpy as np

ITEMS = ["E00200", "E00300", "E00600", "E00900", "E01400", "E01700", "E02000", "E02100", "E02300"]
CAPGAIN = ["E01100", "P22250", "P23250"]
ADJ = ["E03270", "E03150", "E03210"]
REM = "AGI_REMAINDER"


def capital_line13(df):
    lim = np.where(df["MARS"].to_numpy(float) == 3, -1500.0, -3000.0)
    sd = df["P22250"].to_numpy(float) + df["P23250"].to_numpy(float)
    return df["E01100"].to_numpy(float) + np.maximum(sd, lim)


def items_less_adjustments(df):
    s = capital_line13(df)
    for c in ITEMS:
        s = s + df[c].to_numpy(float)
    for c in ADJ:
        s = s - df[c].to_numpy(float)
    return s


def agi_from_parts(df):
    return items_less_adjustments(df) + df[REM].to_numpy(float)
