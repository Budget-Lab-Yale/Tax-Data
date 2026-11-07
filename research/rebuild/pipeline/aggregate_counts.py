"""(Exploration of 5 November 2026; settings generation.real_puf_aggregates 'holder_share') In a real-PUF file the four
aggregate records hold group means, so nearly every item is non-zero on them and each copy looks like a holder of it. In
a count target (statistic 'count', target_list.conditions) each copy therefore counts as a share of a holder, by one rule
for every variable with a line in SOI's booklet table for the aggregate records (resources/top_hidden/
soi_2015_aggregate_booklet.csv):

  share of record r, field f, sign s = min(1, N(f, s) / sum of n(q) over the records q whose mean of f is not zero)
                                       if the mean of f on record r is not zero, otherwise 0

N(f, s): the booklet's returns with that sign of the field, for all four records together. n(q): the record's returns
(S006 / 100). The booklet has no count by record, so the holders are spread evenly over the returns of the records that
can hold them; a record whose mean is exactly zero is taken to have no holder. The share does not depend on the sign of
the record's own mean (a positive mean does not mean every return has a profit, and a group with a positive mean has
returns with losses). Where a variable is made of several fields, the share is the largest of its fields' shares: the
booklet does not say how the fields' holders overlap, and the largest is the fewest holders there can be of any of them.

Only the count changes: each copy still counts as a whole return in every returns target and in the group its AGI puts
it in, and still carries its whole mean in every dollar target. Count targets on variables with no line here keep the
whole count: items the PUF does not hold (balance sheet, tips, deferrals), alimony (suppressed on the four records and
read as zero) and the calculator's credits."""
import numpy as np
import pandas as pd

import common

ANY, POS, NEG = 'returns_nonzero', 'returns_positive', 'returns_negative'
LINES = {
    'wages': [('E00200', ANY)], 'taxable_interest': [('E00300', ANY)], 'exempt_int': [('E00400', ANY)],
    'ordinary_dividends': [('E00600', ANY)], 'qualified_dividends': [('E00650', ANY)], 'state_ref': [('E00700', ANY)],
    'capital_gain_distributions': [('E01100', ANY)], 'sch_d_net_capital_gain': [('E01000', ANY)],
    'capital_gain_total': [('E01000', POS)], 'capital_gain_positive': [('E01000', POS)], 'capital_gain_loss': [('E01000', NEG)],
    'other_gains_net_gain': [('E01200', POS)], 'other_gains_net_loss': [('E01200', NEG)],
    'txbl_ira_dist': [('E01400', ANY)], 'total_pensions': [('E01500', ANY)], 'txbl_pens_dist': [('E01700', ANY)],
    'ui': [('E02300', ANY)], 'gross_ss': [('E02400', ANY)],
    'sole_prop_net_income': [('E00900', POS)], 'sole_prop_net_loss': [('E00900', NEG)],
    'farm_net_income': [('E02100', POS)], 'farm_net_loss': [('E02100', NEG)],
    'sch_cf_positive': [('E00900', POS), ('E02100', POS)], 'sch_cf_loss': [('E00900', NEG), ('E02100', NEG)],
    'farm_rental_net_income': [('E27200', POS)], 'farm_rental_net_loss': [('E27200', NEG)],
    'rent_royalty_net_income': [('E25850', ANY)], 'rent_royalty_net_loss': [('E25860', ANY)],
    'estate_trust_net_income': [('E26390', ANY)], 'estate_trust_net_loss': [('E26400', ANY)],
    'pship_scorp_net': [('E26270', ANY)], 'pship_scorp_net_income': [('E26270', POS)], 'pship_scorp_net_loss': [('E26270', NEG)],
    'partnership_net_income': [('E25940', ANY), ('E25980', ANY)],
    'partnership_net_loss': [('E25920', ANY), ('E25960', ANY), ('E26110', ANY)],
    'scorp_net_income': [('E26170', ANY), ('E26190', ANY)],
    'scorp_net_loss': [('E26160', ANY), ('E26180', ANY), ('E26100', ANY)],
    'statutory_adjustments': [(f, ANY) for f in ('E03150', 'E03210', 'E03220', 'E03230', 'E03240', 'E03260', 'E03270',
                                                 'E03290', 'E03300', 'E03400')],
    'income_tax_before_credits': [('E05800', ANY)], 'taxable_income': [('E04800', ANY)],
    'alternative_minimum_tax': [('E09600', ANY)], 'self_employment_tax': [('E09400', ANY)],
    'itemized_medical': [('E17500', ANY)], 'itemized_taxes': [('E18400', ANY), ('E18500', ANY)],
    'itemized_interest': [('E19200', ANY)], 'itemized_charity': [('E19700', ANY)],
    'itemized_charity_cash': [('E19800', ANY)], 'itemized_charity_noncash': [('E20100', ANY)],
    'itemized_deductions_claimed': [(f, ANY) for f in ('E17500', 'E18400', 'E18500', 'E19200', 'E19700', 'E20400', 'E20500')],
}
_TABLE = {}


def table():
    """One row per variable and aggregate record: the share of a holder a copy of that record counts as, and the field
    the share came from. The rule is in the module's docstring."""
    if len(_TABLE):
        return _TABLE['table']
    import top_hidden as TH
    book = pd.read_csv(TH.BOOKLET).set_index('field')
    agg, _ = TH.read_raw()
    agg = agg.set_index('RECID')
    n = agg['S006'].to_numpy(float) / 100
    rows = []
    for v, lines in LINES.items():
        best = np.zeros(len(n))
        came = [''] * len(n)
        for f, side in lines:
            m = agg[f].to_numpy(float)
            assert np.isfinite(m).all(), f'{f}: a blank mean on an aggregate record'
            has = m != 0
            assert has.any() or book.loc[f, side] == 0, f'{f}: the booklet counts holders and no record has a mean'
            s = np.where(has, min(1.0, float(book.loc[f, side]) / n[has].sum()) if has.any() else 0.0, 0.0)
            for i in range(len(n)):
                if s[i] > best[i] or not came[i]:
                    best[i], came[i] = s[i], f'{f} {side}'
        rows += [{'variable': v, 'puf_recid': int(r), 'share': best[i], 'field': came[i]} for i, r in enumerate(agg.index)]
    t = pd.DataFrame(rows)
    assert t['share'].between(0, 1).all()
    _TABLE['table'] = t
    return t


class Holders:
    """For one file: the rows of the aggregate record copies, and each variable's shares in the order of those rows."""

    def __init__(self, df):
        self.rows = np.flatnonzero(df['puf_aggregate'].to_numpy() == 1)
        self.recid = df['puf_recid'].to_numpy()[self.rows]
        assert np.isin(self.recid, table()['puf_recid'].unique()).all()
        self.by = table().set_index(['variable', 'puf_recid'])['share']

    def has(self, v):
        return v in LINES

    def share(self, v):
        return self.by.loc[[(v, r) for r in self.recid]].to_numpy(float)
