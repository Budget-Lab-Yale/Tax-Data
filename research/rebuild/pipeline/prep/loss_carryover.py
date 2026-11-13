"""Capital loss carryovers on the PUF's records.

The PUF's two Schedule D fields leave out the loss carried over from earlier years. The 2012 PUF description
booklet defines P22250 as "all short-term capital gains less losses shown in column (h) of Schedule D except for the
short-term capital loss carryover", and P23250 the same way for long-term. The file's kg_st and kg_lt are Schedule D
lines 7 and 15, which include the carryovers: the lines that IRS Table 1.4A counts as net short-term and long-term
gain or loss, and the lines the $3,000 limit applies to. In 2015 the carryovers were $171 billion short-term and $292
billion long-term (Table 1.4A).

The PUF has no carryover field, but it shows where a carryover exists: Form 1040 line 13 (E01000) is the allowed net
of lines 7 and 15, so where line 13 is below the allowed sum of the two fields, the difference (the gap) is carryover.
Where line 13 is above the $3,000 limit, the carryover is exactly the gap. Where line 13 is at the limit, the
carryover is at least the gap, and the rest is not observed.

The carryover of each PUF record:
1. The holders are the records with a gap. Each holder's carryover is all short-term or all long-term, drawn with the
   share of carryover returns that are long-term in its AGI group (Table 1.4A, the PUF year).
2. A holder above the limit carries the gap. A holder at the limit carries the gap plus an unobserved rest, drawn from
   an exponential distribution and scaled so that, in each AGI group and for each term, the weighted carryover equals
   Table 1.4A's amount.
3. The draws come from one generator with a fixed seed over the records in RECID order, so a record's carryover
   depends only on the PUF.
"""
from functools import lru_cache

import numpy as np
import pandas as pd

from shared import common
from shared.common import PUF_YEAR

SEED = 20261112
# The PUF's aggregate records (RECIDs 999996-999999) are means of many returns; they keep the gap alone.
AGGREGATE_RECID = 999996
ITEMS = {('st', 'amount'): 'short term loss carryover > amount',
         ('st', 'returns'): 'short term loss carryover > number of returns',
         ('lt', 'amount'): 'long term loss carryover > amount',
         ('lt', 'returns'): 'long term loss carryover > number of returns'}


def irs_carryovers(year=PUF_YEAR):
    """Table 1.4A's short-term and long-term loss carryovers by size of AGI for one tax year: one row per AGI group
    with its lower and upper edge, and the returns and dollars of each carryover."""
    t = pd.read_csv(f"{common.RAW['soi_mirror']}/bysize_capital_assets.csv")
    t = t[(t.table == '1.4A') & (t.tax_year == year) & (t.panel == 'All returns') & t.row_agi_lo.notna()]
    out = None
    for (term, measure), item in ITEMS.items():
        s = t[t['item'] == item][['row_seq', 'row_agi_lo', 'row_agi_hi', 'value']].rename(
            columns={'value': f'{term}_{measure}'})
        out = s if out is None else out.merge(s, on=['row_seq', 'row_agi_lo', 'row_agi_hi'], validate='1:1')
    assert out is not None and len(out) == 19, f'Table 1.4A {year}: expected 19 AGI groups, found {0 if out is None else len(out)}'
    out = out.sort_values('row_seq').reset_index(drop=True)
    for c in ('st_amount', 'lt_amount'):
        out[c] *= 1000.0            # the table's dollars are in thousands
    assert not out.isna().any().any()
    return out


def gap(p_st, p_lt, line13, mars):
    """The carryover that line 13 shows: the allowed sum of the two Schedule D fields less line 13, where positive."""
    return np.maximum(common.allowed_capital_gain(p_st + p_lt, mars) - line13, 0.0)


@lru_cache(maxsize=1)
def puf_carryovers():
    """Each PUF record's short-term and long-term carryover (positive dollars), indexed by RECID, made once from the
    raw PUF by the rule in the module's description."""
    raw = pd.read_csv(common.RAW['puf'], usecols=['RECID', 'S006', 'MARS', 'E00100', 'E01000', 'P22250', 'P23250'])
    raw = raw[raw.RECID < AGGREGATE_RECID].sort_values('RECID').reset_index(drop=True)
    w = raw.S006.to_numpy(float) / 100
    mars = raw.MARS.to_numpy()
    line13 = raw.E01000.to_numpy(float)
    g = gap(raw.P22250.to_numpy(float), raw.P23250.to_numpy(float), line13, mars)
    limit = -np.where(mars == 3, common.CAPITAL_LOSS_LIMIT_SEPARATE, common.CAPITAL_LOSS_LIMIT)
    holder = g > 0
    at_limit = holder & (line13 <= limit)
    # 1. The AGI group of each record, on Table 1.4A's edges (lower edge included).
    irs = irs_carryovers()
    group = np.searchsorted(irs.row_agi_hi.to_numpy(), raw.E00100.to_numpy(float), side='right')
    assert (group >= 0).all() and (group < len(irs)).all()
    # 2. Short-term or long-term, drawn with the group's long-term share of carryover returns.
    rng = np.random.default_rng(SEED)
    u_term, e_rest = rng.random(len(raw)), rng.exponential(1.0, len(raw))
    share_lt = (irs.lt_returns / (irs.lt_returns + irs.st_returns)).to_numpy()
    long_term = holder & (u_term < share_lt[group])
    # 3. The rest at the limit, scaled in each group and term to Table 1.4A's dollars.
    carry = g.copy()
    for k in range(len(irs)):
        for term, is_term in (('st', ~long_term), ('lt', long_term)):
            sel = (group == k) & holder & is_term
            rest_sel = sel & at_limit
            short = irs.at[k, f'{term}_amount'] - (w[sel] * g[sel]).sum()
            if short <= 0 or not rest_sel.any():
                continue
            carry[rest_sel] += short * e_rest[rest_sel] / (w[rest_sel] * e_rest[rest_sel]).sum()
    return pd.DataFrame({'co_st': np.where(holder & ~long_term, carry, 0.0),
                         'co_lt': np.where(long_term, carry, 0.0)}, index=raw.RECID.to_numpy())


def schedule_d_lines(p_st, p_lt, line13, mars, recid=None):
    """Schedule D lines 7 and 15 (kg_st, kg_lt), the two PUF fields less each record's carryover. A record with no
    RECID, or an aggregate record, has its gap taken off the short-term field, so its line 13 is still restored."""
    p_st, p_lt = np.asarray(p_st, float), np.asarray(p_lt, float)
    g = gap(p_st, p_lt, np.asarray(line13, float), np.asarray(mars))
    co_st, co_lt = g.copy(), np.zeros_like(g)
    if recid is not None:
        recid = np.asarray(recid)
        co = puf_carryovers()
        known = np.isin(recid, co.index.to_numpy())
        assert (known | (recid >= AGGREGATE_RECID)).all(), 'a PUF record without a carryover row'
        r = co.reindex(recid[known])
        co_st[known], co_lt[known] = r.co_st.to_numpy(), r.co_lt.to_numpy()
    return p_st - co_st, p_lt - co_lt
