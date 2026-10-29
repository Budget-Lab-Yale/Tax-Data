"""The 2015 PUF's four aggregate records rebuilt as synthetic returns made from generated records (Part A of
explore/2026-10-29/top_research/PLAN.md, approved by John on 29 October 2026; PWBM 2020, section 2.1, is the method).

In the 2015 PUF, 1,232.78 returns (in population weight) sit in four records that hold only group means: negative AGI
(RECID 999996, 140.31 returns), $0 to $10 million (999997, 233.47), over $10 million to $100 million (999998, 394) and
over $100 million (999999, 465). build_table.read_puf() drops them, so the model never saw them. This module rebuilds
them after generation, without retraining, in 2015 dollars, and then grows them to the target year:

  1. Candidates. An extra batch of filer records drawn from each pooled network under the PUF label for 2015, with
     its own allocation of draws across the 12 band-AGI groups (settings top_hidden.candidate_records, through
     generate.label_part's group_records). Each candidate keeps its population weight: its group's 2015 PUF returns
     over the records drawn in it, divided by the number of pooled models. Candidates are donors of which items go
     together; they are not part of the file.
  2. Selection, as SOI did (Bryant, Czajka, Ivsin and Nunns 2014; SELECT below): the top 400 returns by AGI, the 30
     highest of each income item, the 30 lowest of each loss, the 10 highest (and lowest, for two-signed fields) of
     most other amount fields; ranks counted in population weight; duplicates removed. Capped, income-limited and
     derived fields are left out of the rule (EXCLUDED_FROM_SELECTION), as SOI does. This is an approximation of a
     rule SOI does not publish in full.
  3. Groups by AGI, computed from the items. The candidates' own AGI is in the world of the retained PUF, whose largest
     returns fall short of the aggregated ones, so the first assignment is by rank: the selected returns with negative
     AGI form the negative group; the others, ranked by AGI, are split in the shares of the three positive groups'
     population counts. After the dollars are scaled (step 6), each record must lie in its group by its final AGI in
     2015 dollars ($0 to $10,000,000; $10,000,001 to $100,000,000; above): for settings move_iterations rounds a record
     outside it is moved to the group its AGI is in and steps 4 to 6 are repeated; after that, each record is held
     inside its group through the residual part other_agi_remainder (other income: the PUF's AGI less every identified
     item), set at the group's line where needed and the rest of the part scaled to keep the group's net; a record
     still outside is dropped (settings group_iterations).
     Each group keeps a manageable, varied set (settings records_per_group, min_per_rule). Where a group must hold a
     sign of a field that its records lack (its raw record's net amount needs it), the largest such candidates from
     the rest of the pool are added (settings targeted_per_gap).
  4. Starting weights: each group's population count from the raw record (S006 / 100, unrounded), divided equally.
  5. Counts. SOI's booklet table for the aggregate records (2015 Public Use File booklet, printed pp. 32-36,
     resources/top_hidden/soi_2015_aggregate_booklet.csv) gives, for all four records together, the returns with a
     positive and with a negative amount of each field. The weights move as little as possible (solver.solve, the
     Kullback-Leibler move) so that the combined positive and negative counts of every field the rebuild carries
     (COUNT_FIELDS) come within the larger of 2 returns and 2% (settings count_tolerance); each group's total count is
     an identity. Counts that cannot be met give way and are reported.
  6. Dollars. Each group's net amount of each field the rebuild draws (a "part": PRIMITIVES) is the raw record's mean
     times its unrounded weight, through the same translation from PUF fields as build_table.puf_rows (puf_items).
     Positive and negative amounts of a part are scaled by one factor each per group (never the totals), so that every
     group's net amount and, where the booklet splits a field, the combined positive total (hence the negative total)
     match. The split the published figures leave open is chosen closest to the candidates: the factors as close as
     possible, in logs, to one common factor per group and field (solve_field). Totals are then computed from the parts
     exactly as dollars.draw_dollars computes them. The accounting rules kept: qualified dividends at most ordinary
     dividends (the parts are qualified and other dividends, both at least 0); the capital gain on the return is short
     plus long term (both before the loss limit since 29-30 October), a net loss held to the allowed loss (-$3,000,
     -$1,500 married filing separately) where AGI is made (common.allowed_capital_gain), the difference between each
     group's allowed gain and the raw record's moved into other_agi_remainder; taxable Social Security at most 85% of gross; taxable
     pensions at most total pensions, total pensions (gross retirement less gross Social Security, E01500) being the
     part scaled (BOUNDED). A bounded part is scaled on the records inside its bound, iterated; what a bounded part
     falls short of its group's net moves into other_agi_remainder, so each group's AGI equals the raw record's.
     Donors whose total pensions are below their taxable pensions (the network draws them as separate items) are
     raised first. A part a group's raw record has none of is zero on that group's records, redone each round.
     Charity: char_cash is E19800 (cash gifts), char_noncash E20100 (gifts other than cash); E19700 (the deduction)
     is a check only.
  7. Wealth drawn again after the scaling (redraw_wealth): every rebuilt return's whole balance sheet (every SCF slot:
     assets, debts, the mortgage share, accumulated gains, ownership flags, deferrals, auto loan interest, the QBI
     items) is drawn again under the SCF label given its final income items (bands on the 2015 cut points):
     settings wealth_tries tries, drawn as one batch from generators seeded by wealth_seed and the record id, each try's
     dollar picks keyed by record id; the first try with net worth in target-year dollars below settings wealth_cap
     ($1 billion; Part B, step 10) is kept. A record whose every try lands at or above the cap keeps its lowest try and
     is flagged (hidden_wealth_fallback = 1). Since the merge with Part B (29 October) this is one function for both
     parts, top_billionaires.redraw_record. This is an extrapolation: the network has never seen incomes this large
     (they sit in the top bands either way).
  8. Into the file (rebuild_hidden_returns): each record's PUF amounts grown from 2015 to the target year by the file's
     per-return growth (tilt.grow_dollars, build_table.growth), every total recomputed (tilt.recombine), the balance
     sheet from step 7, every other survey item (ages, consumption, the CPS's and SIPP's items) the donor's own, flagged
     synthetic_top = 1, record ids from settings id_block. Its weight is its share of the 2015 PUF's returns in the
     generated file's own units, so the tilt's scaling of filers to SOI's returns grows it with the count of returns.
     Each item is grown by its line in the target year's growth.yaml (growth.py; since 29 October the items with no
     Table 1.4 line, which until then stayed in 2015 dollars, by their declared series); capital losses are not grown;
     written to growth_<year>.csv.

Writes, in the folder (settings top_hidden.folder, else <out>/gen/<run>/hidden; REBUILD_HIDDEN_DIR overrides):
candidates.parquet (the pool, bands and 2015 dollars), hidden_2015.parquet (the rebuilt returns in 2015 dollars, with
their group, weight, the rules that chose them and the donor's record id), hidden_<year>.parquet (the rows in the
generated file's schema, target-year dollars), and the check tables (check_*.csv).

Entry point for tilt.main: rebuild_hidden_returns(g19) -> the rows to append (see its docstring).
Run: python top_hidden.py build    (the whole build; a GPU node; the candidate pool is reused if it is already there)
     python top_hidden.py check    (the Table 4.3 checks for 2015 and the target year, on the run's generated stack)
"""
import json
import os
import sys

import numpy as np
import pandas as pd

import build_table as BT
import common
import dollars as D
import targets as TG

S = common.SETTINGS['top_hidden']
Y = common.TARGET_YEAR
PY = common.PUF_YEAR
assert PY == 2015, 'the aggregate records rebuilt here are the 2015 PUF\'s'
HERE = os.path.dirname(os.path.abspath(__file__))
BOOKLET = os.path.join(HERE, '..', 'resources', 'top_hidden', 'soi_2015_aggregate_booklet.csv')

# ---- the four aggregate records ---------------------------------------------------------------------------------
RECIDS = [999996, 999997, 999998, 999999]
GROUPS = ['negative AGI', '$0 to $10 million', '$10 million to $100 million', 'over $100 million']
N_G = len(GROUPS)


def agi_group(agi):
    """The aggregate record group (0-3) of AGI in 2015 dollars: below 0; $0 to $10,000,000; $10,000,001 to
    $100,000,000; above $100,000,000 (the booklet's disclosure text; its AGIR1 code labels with a $1 million line are
    a misprint: REPORT.md section 1.1)."""
    a = np.asarray(agi, float)
    return np.where(a < 0, 0, np.where(a <= 10e6, 1, np.where(a <= 100e6, 2, 3)))


# ---- the parts the rebuild draws, from PUF fields (build_table.puf_rows) ----------------------------------------
SIGNED_ITEMS = {v: s for v, (s, _, _) in BT.ITEMS.items() if s != 0}   # AGI items besides wages ... capital gains
PRIMITIVES = (['wages', 'txbl_int', 'div_pref', 'div_ord'] + BT.KG_PIECES + list(BT.PARTS)
              + ['other_gains', 'txbl_ira_dist', 'txbl_pens_dist', 'ss_taxable', 'ui', 'alimony',
                 'state_ref', 'retirement_gross', 'gross_ss', 'exempt_int', 'first_mort_int',
                 'salt_prop'] + list(BT.ADJ_PARTS) + list(BT.NEW_ITEMS) + ['other_agi_remainder'])
# Parts that are residuals in the PUF's translation (no PUF field of their own; no booklet line)
RESIDUALS = ['div_ord', 'sche_other', 'part_se', 'other_item_exp', 'other_agi_remainder', 'retirement_gross']


def puf_items(raw, itemizer=None, old=None):
    """The rebuild's parts from raw PUF fields (floats; means on the aggregate records), by build_table.puf_rows'
    rules. itemizer: who gets other_item_exp (default FDED == 1; the aggregate records' FDED is zeroed, so the caller
    passes True); old: the 7.5% medical floor (default False, the 10% floor)."""
    f = {c: raw[c].to_numpy(float) for c in raw.columns}
    n = len(raw)
    out = {'wages': f['E00200'], 'txbl_int': f['E00300'], 'div_pref': f['E00650'], 'div_ord': f['E00600'] - f['E00650'],
           'kg_st': f['P22250'], 'kg_lt': f['P23250'], 'kg_dist': f['E01100']}   # before the loss limit (29-30 October)
    for p, (fld, _) in BT.PARTS.items():
        if fld:
            out[p] = f[fld]
    out['sche_other'] = f['E02000'] - sum(BT.PARTS[p][1] * out[p] for p in BT.SCHE_PARTS)
    out.update({'other_gains': f['E01200'], 'txbl_ira_dist': f['E01400'], 'txbl_pens_dist': f['E01700'],
                'ss_taxable': f['E02500'], 'ui': f['E02300'], 'alimony': f['E00800'],
                'state_ref': f['E00700'], 'retirement_gross': f['E02400'] + f['E01500'], 'gross_ss': f['E02400'],
                'exempt_int': f['E00400'], 'first_mort_int': f['E19200'], 'salt_prop': f['E18500']})
    out.update({p: f[fld] for p, fld in BT.ADJ_PARTS.items()})
    out.update({v: f[fld] for v, fld in BT.NEW_ITEMS.items() if fld})
    k1_part = (f['E25940'] + f['E25980'] - f['E25920'] - f['E25960']) != 0
    out['part_se'] = np.where(k1_part, (f['E30400'] + f['E30500']) / 0.9235 - f['E00900'] - f['E02100'], 0.0)
    itemizer = (f['FDED'] == 1) if itemizer is None else np.broadcast_to(np.asarray(itemizer, bool), (n,))
    old = np.zeros(n, bool) if old is None else np.broadcast_to(np.asarray(old, bool), (n,))
    medical = np.maximum(0, f['E17500'] - np.maximum(0, f['E00100'] * np.where(old, 0.075, 0.1)))
    out['other_item_exp'] = np.where(itemizer, f['P04470'] + f['E21040'] - medical - f['E18400'] - f['E18500']
                                     - f['E19200'] - f['E19700'] - f['E20500'] - f['E20800'], 0.0)
    business = f['E00900'] + f['E02000'] + f['E02100']
    items = {'business_income': business, 'other_gains': out['other_gains'], 'txbl_ira_dist': out['txbl_ira_dist'],
             'txbl_pens_dist': out['txbl_pens_dist'], 'ss_taxable': out['ss_taxable'],
             'ui': out['ui'], 'alimony': out['alimony'], 'state_ref': out['state_ref'],
             'adjustments': sum(f[c] for c in BT.ADJ_FIELDS)}
    assert set(items) == set(SIGNED_ITEMS), (set(items) ^ set(SIGNED_ITEMS))
    # line 13 in AGI as build_table.puf_rows has it: the allowed Schedule D net plus distributions (29-30 October)
    allowed = common.allowed_capital_gain(f['P22250'] + f['P23250'], f['MARS'] if 'MARS' in f else None)
    out['other_agi_remainder'] = f['E00100'] - f['E00200'] - f['E00300'] - f['E00600'] - allowed - f['E01100'] \
        - sum(SIGNED_ITEMS[v] * items[v] for v in items)
    df = pd.DataFrame({v: np.asarray(out[v], float) for v in PRIMITIVES}, index=raw.index)
    return df


def puf_totals(df):
    """The totals from the parts (2015 frame), exactly as dollars.draw_dollars computes them; plus agi."""
    out = df.copy()
    out['capital_gains'] = common.allowed_capital_gain(out['kg_st'] + out['kg_lt'],
                                                       out['filing_status'] if 'filing_status' in out else None)
    out['capital_gains_line13'] = out['capital_gains'] + out['kg_dist']
    for tot, parts in D.META['computed_totals'].items():
        out[tot] = out[list(parts)].to_numpy(float) @ np.array(list(parts.values()), float)
    out['business_income_drawn'] = out['business_income']
    out['other_agi'] = out['other_agi_remainder'] + out['kg_dist'] \
        + out[list(D.FOLD)].to_numpy(float) @ np.array(list(D.FOLD.values()), float)
    out['agi'] = D.agi(out)
    return out


def read_raw():
    """The raw 2015 PUF: the four aggregate records (by RECID order) and the ordinary records."""
    raw = pd.read_csv(common.RAW['puf'])
    agg = raw[raw.RECID >= 999996].set_index('RECID').loc[RECIDS].reset_index()
    assert len(agg) == 4
    return agg, raw[raw.RECID < 999996].reset_index(drop=True)


# Fields blank on all four aggregate records (and absent from the booklet): disclosure-suppressed, not known zeros.
# The parts read from them keep the donors' own 2015 amounts, unscaled; the residual other_agi_remainder's target is
# moved by the donors' amounts so each group's AGI still equals the raw record's (scale_all).
SUPPRESSED_FIELDS = {'E00800': 'alimony', 'E03500': 'alimony_exp'}
UNUSED_BLANK = {'E87521'}                                  # blank too, and read by nothing here


def raw_targets(agg):
    """Per group: population returns (S006 / 100) and the net amount of every part (mean x returns), 2015 dollars. A
    suppressed field (SUPPRESSED_FIELDS) is read as 0 for the residual and its part's target is missing (NaN)."""
    n = agg['S006'].to_numpy(float) / 100
    blank = {c for c in agg.columns if agg[c].isna().any()}
    assert blank <= set(SUPPRESSED_FIELDS) | UNUSED_BLANK, f'blank fields on the aggregate records: {sorted(blank)}'
    assert all(agg[c].isna().all() for c in blank), 'a field blank on some aggregate records only'
    common.say(f'aggregate records: fields blank on all four (suppressed): {sorted(blank)}')
    means = puf_items(agg.fillna({c: 0.0 for c in blank}), itemizer=True)
    for fld, part in SUPPRESSED_FIELDS.items():
        if fld in blank:
            means[part] = np.nan
    nets = means.mul(n, axis=0)
    # the allowed Schedule D net of each raw record (the loss limit on its mean), as its residual was made (puf_items)
    f = agg.fillna({c: 0.0 for c in blank})
    nets['sched_d_allowed'] = n * common.allowed_capital_gain(f['P22250'].to_numpy(float) + f['P23250'].to_numpy(float),
                                                              f['MARS'].to_numpy() if 'MARS' in f else None)
    nets.index = GROUPS
    return n, means, nets


def check_translation(ordinary):
    """puf_items against the training table's PUF rows (build_table.puf_rows): every part equal on the ordinary
    records (other_item_exp where no medical expenses, since puf_rows' medical floor uses the age range). Returns a
    table of the largest differences."""
    t = pd.read_parquet(common.TABLE, columns=['source', 'record_id'] + [('other_agi' if v == 'other_agi_remainder' else v)
                                                                          for v in PRIMITIVES])
    t = t[t.source == 'puf'].set_index('record_id')
    mine = puf_items(ordinary).set_index(ordinary['RECID'].astype(float))
    t = t.loc[mine.index]
    rows = []
    no_med = ordinary['E17500'].to_numpy() == 0
    for v in PRIMITIVES:
        a = mine[v].to_numpy(float)
        b = t['other_agi' if v == 'other_agi_remainder' else v].to_numpy(float)
        m = no_med if v == 'other_item_exp' else np.ones(len(a), bool)
        d = np.abs(a - b)[m]
        rows.append({'part': v, 'records compared': int(m.sum()), 'records differing': int((d > 1e-6).sum()),
                     'largest difference ($)': float(d.max())})
    out = pd.DataFrame(rows)
    assert (out['records differing'] == 0).all(), 'puf_items differs from build_table.puf_rows:\n' + out.to_string()
    return out


# ---- the booklet ---------------------------------------------------------------------------------------------
def booklet():
    """SOI's booklet table for the aggregate records, 2015 (printed pp. 32-36), in dollars and returns."""
    b = pd.read_csv(BOOKLET).set_index('field')
    for c in [c for c in b if c.endswith('_thousands')]:
        b[c.replace('_thousands', '')] = b[c] * 1000.0
    return b


# Booklet fields the rebuild carries, each as a column whose sign is the field's sign (loss parts stored positive are
# the booklet's positive losses). Counts of these are matched in step 5.
COUNT_FIELDS = {**{fld: p for p, (fld, _) in BT.PARTS.items() if fld},
                **{fld: p for p, fld in BT.ADJ_PARTS.items()},
                **{fld: v for v, fld in BT.NEW_ITEMS.items() if fld},
                'E00200': 'wages', 'E00300': 'txbl_int', 'E00400': 'exempt_int', 'E00600': 'dividends',
                'E00650': 'div_pref', 'E00700': 'state_ref', 'E00800': 'alimony', 'E01100': 'kg_dist',
                'E01200': 'other_gains', 'E01400': 'txbl_ira_dist', 'E01700': 'txbl_pens_dist',
                'E02300': 'ui', 'E02400': 'gross_ss', 'E02500': 'ss_taxable', 'E18500': 'salt_prop',
                'E19200': 'first_mort_int', 'P22250': 'kg_st', 'P23250': 'kg_lt'}
# Booklet fields that are totals the rebuild computes from its parts: checks only (their signs can move with scaling)
CHECK_FIELDS = {'E00100': 'agi', 'E01000': 'capital_gains', 'E02000': 'sch_e_income', 'E26270': 'pship_scorp_net',
                'E00900': 'sole_prop', 'E02100': 'farm'}
# Parts with both signs whose booklet line splits the combined amount into positive and negative dollars
SPLIT_PARTS = {'sole_prop': 'E00900', 'farm': 'E02100', 'other_gains': 'E01200', 'kg_st': 'P22250', 'kg_lt': 'P23250',
               'amt_nols': 'P60100'}

# ---- step 2: SOI's selection rule ------------------------------------------------------------------------------
# (column, side, rule) with side +1 the highest positive amounts, -1 the lowest negative ones; the number of returns
# from settings top_hidden.selection.
SELECT = ([('agi', 1, 'top_agi')]
          + [(v, 1, 'income_items') for v in ['wages', 'txbl_int', 'dividends', 'state_ref', 'alimony', 'sole_prop',
                                               'capital_gains', 'other_gains', 'txbl_ira_dist', 'txbl_pens_dist',
                                               'sch_e_income', 'farm', 'ui']]
          + [(v, -1, 'loss_items') for v in ['sole_prop', 'other_gains', 'sch_e_income', 'farm']]
          + [(v, 1, 'other_fields') for v in ['exempt_int', 'div_pref', 'kg_st', 'kg_lt', 'kg_1250',
                                               'kg_collect', 'retirement_gross']
             + [p for p, (fld, _) in BT.PARTS.items() if fld and p not in ('sole_prop', 'farm')]
             + ['dpad', 'early_penalty', 'alimony_exp', 'first_mort_int', 'salt_prop', 'salt_inc_sales',
                'char_cash', 'char_noncash', 'med_exp', 'casualty_exp', 'job_exp', 'tax_prep_exp', 'other_misc_exp',
                'amt_nols', 'amt_ftc', 'ftc', 'gbc', 'prior_yr_cred', 'other_nonref', 'recapture_tax', 'ira_penalty',
                'res_energy_cred']]
          + [(v, -1, 'other_fields') for v in ['kg_st', 'kg_lt', 'amt_nols']])
EXCLUDED_FROM_SELECTION = {
    'capped or income-limited': ['gross_ss', 'ss_taxable', 'trad_contr_ira', 'sl_int_ded', 'ed_exp', 'tuition_ded',
                                 'hsa_contr', 'keogh_contr', 'aoc_exp', 'llc_exp', 'savers_cred', 'old_cred', 'net_ptc',
                                 'excess_ptc', 'capital loss (the $3,000 cap)'],
    'derived': ['div_ord', 'kg_dist (zero on the aggregate records)', 'sche_other', 'part_se',
                'other_item_exp', 'other_agi_remainder', 'se_tax_ded', 'se_health', 'business_income',
                'sch_cf_income', 'sche_rent', 'sche_rest', 'adjustments']}


def top_by_weight(x, w, n, side):
    """Indices of the n highest (side 1, among positive x) or lowest (side -1, among negative x) returns counted in
    weight: records in order until their weight reaches n, the record that crosses n included."""
    idx = np.flatnonzero(x > 0 if side > 0 else x < 0)
    if not len(idx):
        return idx
    o = idx[np.argsort(-side * x[idx], kind='stable')]
    k = int(np.searchsorted(np.cumsum(w[o]), n, side='left')) + 1
    return o[:k]


def select(pool, w):
    """SOI's rule on the pool (2015 frame with totals): {record position: [rules]}."""
    sel = S['selection']
    reasons = {}
    for col, side, rule in SELECT:
        for i in top_by_weight(pool[col].to_numpy(float), w, sel[rule], side):
            reasons.setdefault(int(i), []).append(f"{rule}:{col}:{'high' if side > 0 else 'low'}")
    return reasons


# ---- step 6: scaling one part ----------------------------------------------------------------------------------
def _one_group(P, N, T):
    """The positive and negative factors (a, b) of one group with both signs, a P + b N = T (P > 0 > N), closest in
    logs to one common factor c = |T| / (P - N)."""
    from scipy.optimize import minimize_scalar
    c = abs(T) / (P - N)
    lc = np.log(c)
    lo = np.log(T / N) + 1e-9 if T < 0 else lc - 40

    def f(u):
        a = (T - np.exp(u) * N) / P
        return (np.log(a) - lc) ** 2 + (u - lc) ** 2
    r = minimize_scalar(f, bounds=(lo, lc + 40), method='bounded', options={'xatol': 1e-12})
    b = float(np.exp(r.x))
    return (T - b * N) / P, b


def solve_field(P, N, T, B=None):
    """Scale factors for one part across the four groups: a[g] on positive amounts, b[g] on negative amounts, so that
    a P + b N = T in every group and, with B (the booklet's combined positive dollars), sum a P = B. P[g] >= 0 and
    N[g] <= 0 are the group's weighted positive and negative dollars before scaling, T[g] its net target. Among the
    factors that do it, the closest in logs to one common factor per group (|T| / (P - N)). Returns a, b and a list of
    notes (unmet groups, an unmet booklet split)."""
    P, N, T = (np.asarray(x, float) for x in (P, N, T))
    assert np.isfinite(P).all() and np.isfinite(N).all() and np.isfinite(T).all(), (P, N, T)
    P, N = np.where(P < 1.0, 0.0, P), np.where(N > -1.0, 0.0, N)   # under a dollar of weighted amount: none
    a, b = np.zeros(N_G), np.zeros(N_G)
    notes, free = [], []
    for g in range(N_G):
        if T[g] == 0:
            continue                                          # nothing in the group: zero
        if P[g] > 0 and N[g] < 0:
            a[g], b[g] = _one_group(P[g], N[g], T[g])
            free.append(g)
        elif P[g] > 0 and T[g] > 0:
            a[g] = T[g] / P[g]
        elif N[g] < 0 and T[g] < 0:
            b[g] = T[g] / N[g]
        else:
            notes.append(f'group {g}: net {T[g]:,.0f} needs a sign its records lack (positive {P[g]:,.0f}, '
                         f'negative {N[g]:,.0f}); set to zero')
    if B is None:
        return a, b, notes
    fixed = sum(a[g] * P[g] for g in range(N_G) if g not in free)
    if not free:
        if abs(fixed - B) > 1e-6 * max(abs(B), 1):
            notes.append(f'booklet positive total {B:,.0f} not met: the groups\' nets fix it at {fixed:,.0f}')
        return a, b, notes
    # positives of a free group can be anything above max(0, T): the combined total must be at least their sum
    floor = fixed + sum(max(0.0, T[g]) for g in free)
    B_use = B
    if B <= floor:
        B_use = floor * (1 + 1e-6) + 1.0
        notes.append(f'booklet positive total {B:,.0f} below what the groups\' nets allow ({floor:,.0f}); '
                     f'set just above it')
    from scipy.optimize import minimize
    c = {g: abs(T[g]) / (P[g] - N[g]) for g in free}
    x0 = np.concatenate([[np.log(a[g]), np.log(b[g])] for g in free])

    def obj(x):
        return sum((x[2 * k] - np.log(c[g])) ** 2 + (x[2 * k + 1] - np.log(c[g])) ** 2 for k, g in enumerate(free))
    cons = [{'type': 'eq', 'fun': (lambda x, k=k, g=g: (np.exp(x[2 * k]) * P[g] + np.exp(x[2 * k + 1]) * N[g] - T[g])
                                   / max(abs(T[g]), 1.0))} for k, g in enumerate(free)]
    cons.append({'type': 'eq', 'fun': lambda x: (fixed + sum(np.exp(x[2 * k]) * P[g] for k, g in enumerate(free))
                                                 - B_use) / max(abs(B_use), 1.0)})
    r = minimize(obj, x0, constraints=cons, method='SLSQP', options={'maxiter': 500, 'ftol': 1e-14})
    if not r.success:
        notes.append(f'booklet split: the solver did not converge ({r.message}); the groups\' nets kept, the split not')
        return a, b, notes
    for k, g in enumerate(free):
        a[g], b[g] = np.exp(r.x[2 * k]), np.exp(r.x[2 * k + 1])
    return a, b, notes


def apply_factors(x, grp, a, b):
    return np.where(x > 0, x * a[grp], np.where(x < 0, x * b[grp], 0.0))


# Parts held to a bound on each record: (part, lower bound, upper bound), each a function of the frame after the
# parts it depends on are scaled (BOUNDED is scaled last, in this order).
BOUNDED = [('ss_taxable', None, lambda d: 0.85 * d['gross_ss'].to_numpy(float)),
           ('txbl_pens_dist', None, lambda d: np.maximum(0.0, d['retirement_gross'].to_numpy(float)
                                                            - d['gross_ss'].to_numpy(float)))]


def scale_bounded(x, w, grp, T, lo, hi, rounds=400):
    """One part scaled group by group (no booklet split) with each record held inside [lo, hi]: the records outside
    are set to their bound and the rest scaled to the remaining net, until no record leaves its bound."""
    y = np.zeros(len(x))
    notes = []
    for g in range(N_G):
        m = grp == g
        fixed = np.zeros(len(x), bool)
        val = np.zeros(len(x))
        for _ in range(rounds):
            free = m & ~fixed
            Tf = T[g] - (w[m & fixed] * val[m & fixed]).sum()
            xf = np.where(free, x, 0.0)
            P = (w * np.where(xf > 0, xf, 0)).sum()
            N = (w * np.where(xf < 0, xf, 0)).sum()
            Tg = np.zeros(N_G)
            Tg[g] = Tf
            Pg, Ng = np.zeros(N_G), np.zeros(N_G)
            Pg[g], Ng[g] = P, N
            a, b, nt = solve_field(Pg, Ng, Tg)
            yg = np.where(free, apply_factors(x, np.full(len(x), g), a, b), val)
            below = free & (yg < lo - 1e-6) if lo is not None else np.zeros(len(x), bool)
            above = free & (yg > hi + 1e-6) if hi is not None else np.zeros(len(x), bool)
            if not (below | above).any():
                y[m] = yg[m]
                notes += nt
                break
            if lo is not None:
                val[below] = lo[below]
            if hi is not None:
                val[above] = hi[above]
            fixed |= below | above
        else:
            y[m] = yg[m]
            notes.append(f'group {g}: the bound did not settle in {rounds} rounds')
        if fixed[m].any():
            notes.append(f'group {g}: {int(fixed[m].sum())} records held at their bound')
    return y, notes


# Each group's AGI range in 2015 dollars, as agi_group has it, a dollar or two inside the lines so rounding in the
# sums cannot carry a record held at a line across it
AGI_RANGE = [(-np.inf, -2.0), (1.0, 10e6 - 1), (10e6 + 2, 100e6 - 1), (100e6 + 2, np.inf)]


def scale_all(base, w, grp, nets, book, hold=False):
    """Step 6 on base (2015 parts, structural zeros applied) with weights w and groups grp: every part scaled, then
    the totals. hold: the residual other_agi_remainder (other income, the PUF's AGI less every identified item) is
    scaled last, each record's held so its AGI stays inside its group's range (as the bounded parts are held), the
    rest of the part scaled to keep each group's net. Returns the scaled frame (with totals) and a table per part."""
    out = base.copy()
    rows = []
    bounded = {v for v, _, _ in BOUNDED}
    nets = nets.copy()
    for part in [p for p in SUPPRESSED_FIELDS.values() if nets[p].isna().all()]:
        own = np.array([(w * base[part].to_numpy(float))[grp == g].sum() for g in range(N_G)])
        sign = D.FOLD[part] if part in D.FOLD else -1.0      # alimony paid is an adjustment, subtracted from AGI
        nets['other_agi_remainder'] = nets['other_agi_remainder'] - sign * own
        rows.append({'part': part, 'notes': 'suppressed on the aggregate records: the donors\' own amounts, unscaled '
                     f'(${own.sum() / 1e6:,.1f} million); the residual moved to keep AGI'})
    suppressed = {r['part'] for r in rows}
    last = ['other_agi_remainder']                       # the residual: scaled last, after every other part
    later = ['retirement_gross']                         # after gross Social Security (below)
    for v in [p for p in PRIMITIVES if p not in bounded and p not in suppressed and p not in last + later]:
        x = base[v].to_numpy(float)
        P = np.array([(w * np.where(x > 0, x, 0))[grp == g].sum() for g in range(N_G)])
        N = np.array([(w * np.where(x < 0, x, 0))[grp == g].sum() for g in range(N_G)])
        T = nets[v].to_numpy(float)
        fld = SPLIT_PARTS.get(v)
        B = float(book.loc[fld, 'amount_positive']) if fld else None
        a, b, notes = solve_field(P, N, T, B)
        out[v] = apply_factors(x, grp, a, b)
        rows.append({'part': v, **{f'positive factor, {GROUPS[g]}': a[g] for g in range(N_G)},
                     **{f'negative factor, {GROUPS[g]}': b[g] for g in range(N_G)}, 'notes': '; '.join(notes)})
    # gross retirement income is gross Social Security plus total pensions (E02400 + E01500): total pensions is the
    # part scaled (at least 0, to E01500's net), so gross retirement is never below gross Social Security and taxable
    # pensions can be held to total pensions
    x = base['retirement_gross'].to_numpy(float) - base['gross_ss'].to_numpy(float)
    assert (x >= -1e-6).all(), 'gross retirement below gross Social Security on a donor (made consistent in build)'
    x = np.maximum(x, 0.0)
    T = nets['retirement_gross'].to_numpy(float) - nets['gross_ss'].to_numpy(float)
    P = np.array([(w * x)[grp == g].sum() for g in range(N_G)])
    a, b, notes = solve_field(P, np.zeros(N_G), T)
    out['retirement_gross'] = out['gross_ss'] + apply_factors(x, grp, a, b)
    rows.append({'part': 'retirement_gross', **{f'positive factor, {GROUPS[g]}': a[g] for g in range(N_G)},
                 'notes': '; '.join(['scaled as total pensions (gross retirement less gross Social Security)'] + notes)})
    for v, lo_f, hi_f in BOUNDED:
        x = base[v].to_numpy(float)
        lo = lo_f(out) if lo_f else None
        hi = hi_f(out) if hi_f else None
        y, notes = scale_bounded(x, w, grp, nets[v].to_numpy(float), lo, hi)
        out[v] = y
        rows.append({'part': v, 'notes': '; '.join(notes)})
    # A held part in AGI that falls short of its group's net (its records' bounds cannot carry it) moves the
    # residual's target by the shortfall, so each group's AGI still equals the raw record's
    v = 'other_agi_remainder'
    short = np.zeros(N_G)
    for b_part, _, _ in BOUNDED:
        got = np.array([(w * out[b_part].to_numpy(float))[grp == g].sum() for g in range(N_G)])
        short += nets[b_part].to_numpy(float) - got
    # the capital gain in AGI (29-30 October): each rebuilt record's Schedule D net held to the allowed loss; what the
    # group's records allow differs from the raw record's allowed net, and the difference moves into the residual
    if 'sched_d_allowed' in nets:
        fs = out['filing_status'] if 'filing_status' in out else None
        got = common.allowed_capital_gain(out['kg_st'].to_numpy(float) + out['kg_lt'].to_numpy(float), fs)
        short += nets['sched_d_allowed'].to_numpy(float) - np.array([(w * got)[grp == g].sum() for g in range(N_G)])
    T = nets[v].to_numpy(float) + short
    note = ('held parts short of their nets, moved into it: ' + ', '.join(f'{GROUPS[g]} ${short[g] / 1e6:,.1f}m'
                                                                            for g in range(N_G) if abs(short[g]) > 1))
    if hold:
        without = D.agi(puf_totals(out.assign(other_agi_remainder=0.0)))
        lo = np.array([AGI_RANGE[g][0] for g in grp]) - without
        hi = np.array([AGI_RANGE[g][1] for g in grp]) - without
        y, notes = scale_bounded(base[v].to_numpy(float), w, grp, T, lo, hi)
        out[v] = y
        rows.append({'part': v, 'notes': '; '.join(['held so each record\'s AGI stays in its group', note] + notes)})
    else:
        x = base[v].to_numpy(float)
        P = np.array([(w * np.where(x > 0, x, 0))[grp == g].sum() for g in range(N_G)])
        N = np.array([(w * np.where(x < 0, x, 0))[grp == g].sum() for g in range(N_G)])
        a, b, notes = solve_field(P, N, T)
        out[v] = apply_factors(x, grp, a, b)
        rows.append({'part': v, **{f'positive factor, {GROUPS[g]}': a[g] for g in range(N_G)},
                     **{f'negative factor, {GROUPS[g]}': b[g] for g in range(N_G)}, 'notes': '; '.join([note] + notes)})
    return puf_totals(out), pd.DataFrame(rows)


# ---- step 5: the counts ---------------------------------------------------------------------------------------
def count_targets(book, frame):
    """(label, indicator column on frame, published count, tolerance) for every booklet count the rebuild carries:
    each field of COUNT_FIELDS, its positive and its negative count."""
    tol = S['count_tolerance']
    out = []
    for fld, col in COUNT_FIELDS.items():
        if fld not in book.index:
            continue
        x = frame[col].to_numpy(float)
        for side, cnt in [('positive', 'returns_positive'), ('negative', 'returns_negative')]:
            c = float(book.loc[fld, cnt])
            ind = (x > 0) if side == 'positive' else (x < 0)
            if c == 0 and not ind.any():
                continue                                       # nothing to match: zero and zero
            out.append((f'{fld} {col} {side}', ind.astype(float), c, max(float(tol['returns']), float(tol['share']) * c)))
    return out


def match_counts(frame, grp, n_ret, book):
    """Step 5: the weights moved as little as possible (solver.solve) so the booklet's combined counts are met within
    their tolerance; each group's returns an identity. Starting weights equal within each group (step 4). Returns the
    weights, the starting weights, and a table of every count before and after."""
    import solver as SV
    w0 = np.zeros(len(frame))
    for g in range(N_G):
        m = grp == g
        assert m.any(), f'group {GROUPS[g]}: no record'
        w0[m] = n_ret[g] / m.sum()
    cts = count_targets(book, frame)
    F = np.column_stack([(grp == g).astype(float) for g in range(N_G)] + [c[1] for c in cts])
    T = np.array(list(n_ret) + [c[2] for c in cts])
    se = np.array([0.0] * N_G + [c[3] for c in cts])
    ident = np.array([True] * N_G + [False] * len(cts))
    w1, info = SV.solve(w0, F, T, se, ident, [None] * len(T), {'range_in_se': 1.0})
    tab = pd.DataFrame({'count': [f'returns, {g}' for g in GROUPS] + [c[0] for c in cts], 'booklet': T,
                        'tolerance': se, 'equal weights': F.T @ w0, 'after matching': F.T @ w1})
    tab['met'] = np.abs(tab['after matching'] - tab['booklet']) <= tab['tolerance'] * 1.001 + 1e-6   # solver rounding
    return w1, w0, tab, info


# ---- the candidates ----------------------------------------------------------------------------------------------
def folder():
    f = os.environ.get('REBUILD_HIDDEN_DIR') or S.get('folder') or f'{common.GEN}/hidden'
    os.makedirs(f, exist_ok=True)
    return f


def offset_factors(models):
    """Each model's consistency-rule factors as the run's own generation last used them (<gen>/consistency.csv, the
    last run's 'factor used'); none (1) if the file is missing."""
    path = f'{common.GEN}/consistency.csv'
    out = {m['run']: {} for m in models}
    if not os.path.exists(path):
        common.say(f'no {path}: candidates drawn with no offset factors')
        return out
    c = pd.read_csv(path)
    c = c[c['run'] == c['run'].max()]
    for m in models:
        r = c[c['model'] == m['seed']]
        out[m['run']] = dict(zip(r['holding'], r['factor used'].astype(float)))
    return out


def store_picks(s, k):
    """Each candidate's stored dollar picks (as generate.store_picks), on a stream of their own (4343)."""
    seed = int(common.SETTINGS['dollars']['seed'])
    cols = {f'u_{v}': np.random.default_rng([seed, 4343, k, i]).random(len(s), dtype=np.float32)
            for i, v in enumerate(D.ORDER) if f'{v}_band' in s}
    return pd.concat([s, pd.DataFrame(cols, index=s.index)], axis=1)


def draw_candidates(models, factors):
    """Step 1: each model's share of settings candidate_records, by group, through generate.label_part with its own
    seed. Weight: the group's 2015 PUF returns over the records drawn in it, over the number of models."""
    import generate as G
    alloc = np.asarray(S['candidate_records'], int)
    parts = []
    for k, mod in enumerate(models):
        n_k = np.array([G.equal_shares(int(a), len(models))[k] for a in alloc])
        part, _, _ = G.label_part(mod['network'], mod, k, len(models), factors[mod['run']], group_records=n_k,
                                  seed=int(S['candidate_seed']) + mod['seed_shift'])
        part['weight'] = part['weight'] / len(models)
        part['stratifier'] = 'label'
        part['model'] = mod['seed']
        part['record_id'] = k * G.ID_BLOCK + np.arange(len(part))
        parts.append(store_picks(part, k))
        common.say(f"{mod['run']}: {len(part):,} candidates")
    return pd.concat(parts, ignore_index=True)


def puf_maps(year):
    m = pd.read_parquet(f'{common.MAPS}/map_puf_{year}.parquet')
    return {v: g for v, g in m.groupby('variable')}


def dollars_2015(stack):
    """The PUF items of stack in 2015 dollars (the 2015 map, the stored picks), as dollars.draw_dollars puts them."""
    maps = puf_maps(PY)
    cols = [c for v in D.ORDER if v in maps for c in (f'{v}_band', f'u_{v}') if c in stack]
    d = D.draw_dollars(stack[cols].reset_index(drop=True), maps)
    missing = [v for v in PRIMITIVES if v not in d]
    assert not missing, f'parts with no 2015 dollars: {missing}'
    out = d[PRIMITIVES].copy()
    out['filing_status'] = stack['filing_status'].to_numpy()
    t = puf_totals(out)
    for c in ['capital_gains', 'capital_gains_line13', 'business_income', 'other_agi', 'dividends', 'adjustments']:
        assert np.allclose(t[c].to_numpy(float), d[c].to_numpy(float), rtol=1e-12, atol=1e-6), \
            f'{c}: puf_totals differs from dollars.draw_dollars'
    return t


# ---- step 7: wealth -------------------------------------------------------------------------------------------
def reband(frame2015):
    """Band of every PUF amount slot from 2015 dollars, on the 2015 PUF cut points (build_table.band_variable)."""
    import model as M
    out = {}
    for v in M.PREDICTED:
        if v in M.CATEGORICAL or 'puf' not in M.PRESENT_ON[v] or v in M.REPORTED:
            continue
        col = 'other_agi_remainder' if v == 'other_agi' else v
        assert col in frame2015, f'{v}: no 2015 dollars to band'
        x = frame2015[col].to_numpy(float)
        assert not np.isnan(x).any(), f'{v}: missing 2015 dollars'
        out[f'{v}_band'], _ = BT.band_variable(x, np.ones(len(x)), fixed=common.edges()[f'puf/{PY}/{v}'])
    return pd.DataFrame(out)


def scf_dollars(rows, maps, salt):
    """The balance sheet in target-year dollars from rows' SCF bands (top_billionaires.balance_sheet_dollars: each
    amount from the row's own wave's map with a pick keyed by record id and salt + the amount's position; the items
    made from a share and net worth)."""
    import top_billionaires as TB
    return TB.balance_sheet_dollars(rows, maps, salt)


def redraw_wealth(rows, models, factors, maps):
    """Step 7 (see the top), by the one redraw function the generated file's records use too
    (top_billionaires.redraw_record, merged 29 October): for each rebuilt return, wealth_tries tries of its whole SCF
    block drawn as one batch from generators seeded by wealth_seed and its record id, try t's dollars from picks keyed
    by its id and wealth_salt + 100 t; the first try below wealth_cap is kept, else the lowest. rows: the rebuilt
    returns in the stack's own coding (generate.to_table: bands, categories, ages as value 0 none and a + 1 for age a),
    PUF bands from their final 2015 dollars, record ids final. Returns the new SCF slot values (stack columns), the
    balance-sheet dollars of the try kept, per record the try accepted (-1: every try at or above the cap, the lowest
    kept), the net worth of every try, and the try kept."""
    import generate as G
    import model as M
    import top_billionaires as TB
    P = M.PREDICTED
    scf_cols = [M.column(v) for v in G.SCF_SLOTS]
    cap, tries = float(S['wealth_cap']), int(S['wealth_tries'])
    device = next(models[0]['network'].parameters()).device
    by_seed = {m['seed']: m for m in models}
    new = rows[scf_cols].copy()
    dol, accepted = [], np.full(len(rows), -1)
    tried, kept = np.full((len(rows), tries), np.nan), np.zeros(len(rows), int)
    for i in range(len(rows)):
        r = rows.iloc[i]
        mod = by_seed[int(r['model'])]
        coded = [int(r[M.column(v)]) if M.column(v) in rows else 0 for v in P]
        slots, dd, nw, k, ok = TB.redraw_record(mod['network'], factors[mod['run']], coded, 'puf', M.YEARS.index(PY),
                                                M.YEARS.index(int(r['scf_year'])), int(r['scf_wave']),
                                                int(r['record_id']), maps, cap, tries, int(S['wealth_seed']),
                                                int(S['wealth_salt']), device)
        new.iloc[i] = [slots[c] for c in scf_cols]
        dol.append(dd)
        tried[i], kept[i] = nw, k
        accepted[i] = k if ok else -1
    dol = pd.DataFrame(dol, index=rows.index)
    assert not dol.isna().any().any(), 'a record with no balance sheet drawn'
    return new, dol, accepted, tried, kept


# ---- the build -------------------------------------------------------------------------------------------------
def build():
    """Steps 1 to 8 (see the top). Writes the folder's files and returns the target-year rows."""
    import torch
    import generate as G
    out = folder()
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    models = G.load_models(device)
    factors = offset_factors(models)
    agg, ordinary = read_raw()
    common.say('translation check against build_table.puf_rows:\n' + check_translation(ordinary).to_string(index=False))
    n_ret, means, nets = raw_targets(agg)
    book = booklet()
    common.say(f'aggregate records: returns {n_ret.tolist()} (total {n_ret.sum():.2f})')

    # 1. candidates (reused if already drawn)
    cpath = f'{out}/candidates.parquet'
    if os.path.exists(cpath):
        cand = pd.read_parquet(cpath)
        common.say(f'candidates: {cpath} ({len(cand):,} records), reused')
    else:
        cand = draw_candidates(models, factors)
        cand.to_parquet(cpath, index=False)
    pool = dollars_2015(cand)
    w = cand['weight'].to_numpy(float)

    # 2. selection
    reasons = select(pool, w)
    chosen = np.array(sorted(reasons))
    common.say(f'selected {len(chosen):,} candidates, {w[chosen].sum():,.1f} returns in population weight')

    # 3. first groups, by rank of the candidates' own AGI
    a = pool['agi'].to_numpy(float)
    grp_all = np.full(len(pool), -1)
    neg = chosen[a[chosen] < 0]
    pos = chosen[a[chosen] >= 0]
    grp_all[neg] = 0
    o = pos[np.argsort(-a[pos], kind='stable')]
    share = (np.cumsum(w[o]) - w[o]) / w[o].sum()
    up = n_ret[1:][::-1]                                  # over $100M, $10-100M, $0-10M
    cut = np.cumsum(up) / up.sum()
    grp_all[o] = np.where(share < cut[0], 3, np.where(share < cut[1], 2, 1))
    lines = {3: a[o][share < cut[0]].min(), 2: a[o][(share >= cut[0]) & (share < cut[1])].min()}
    common.say(f'first groups by rank: candidates\' AGI lines {lines[3]:,.0f} (over $100M) and {lines[2]:,.0f} '
               f'($10-100M); records per group {np.bincount(grp_all[chosen], minlength=4).tolist()}')

    # varied subsample per group
    rng = np.random.default_rng(int(S['subsample_seed']))
    keep = []
    cap, floor_n = int(S['records_per_group']), int(S['min_per_rule'])
    for g in range(N_G):
        ids = chosen[grp_all[chosen] == g]
        if len(ids) > cap:
            key = rng.random(len(ids)) ** (1 / w[ids])        # weighted sampling without replacement
            take = set(ids[np.argsort(-key)[:cap]].tolist())
            rules = {}
            for i in ids:
                for r_ in reasons[int(i)]:
                    rules.setdefault(r_, []).append(int(i))
            for r_, members in rules.items():
                if not take.intersection(members):
                    take.update(rng.permutation(members)[:floor_n].tolist())
            ids = np.array(sorted(take))
        keep.append(ids)
    keep = np.concatenate(keep)

    # targeted donors where a group must hold a sign its records lack
    base_all = pool.copy()
    added = []
    lines_pool = np.where(a < 0, 0, np.where(a >= lines[3], 3, np.where(a >= lines[2], 2, 1)))
    for v in PRIMITIVES:
        x = base_all[v].to_numpy(float)
        for g in range(N_G):
            T = nets[v].iloc[g]
            m = keep[grp_all[keep] == g]
            need = (T > 0 and not (x[m] > 0).any()) or (T < 0 and not (x[m] < 0).any())
            if not need:
                continue
            side = 1 if T > 0 else -1
            pool_g = np.flatnonzero((lines_pool == g) & (side * x > 0))
            pool_g = np.setdiff1d(pool_g, keep)
            if not len(pool_g):
                added.append({'part': v, 'group': GROUPS[g], 'sign': 'positive' if side > 0 else 'negative',
                              'added': 0, 'note': 'no candidate in the pool has it'})
                continue
            top = pool_g[np.argsort(-side * x[pool_g])[:int(S['targeted_per_gap'])]]
            grp_all[top] = g
            for i in top:
                reasons.setdefault(int(i), []).append(f'targeted:{v}:{"positive" if side > 0 else "negative"}')
            keep = np.concatenate([keep, top])
            added.append({'part': v, 'group': GROUPS[g], 'sign': 'positive' if side > 0 else 'negative',
                          'added': len(top), 'note': ''})
    added = pd.DataFrame(added, columns=['part', 'group', 'sign', 'added', 'note'])
    common.say('targeted donors:\n' + (added.to_string(index=False) if len(added) else 'none needed'))
    keep = np.unique(keep)

    # structural zeros: a part a group's raw record has none of, or a sign the booklet has no return with
    base = pool.iloc[keep][PRIMITIVES + ['filing_status']].reset_index(drop=True)
    grp = grp_all[keep].copy()
    zeroed = []
    for v in PRIMITIVES:
        x = base[v].to_numpy(float)
        fld = next((f for f, c in COUNT_FIELDS.items() if c == v), None)
        no_pos = fld in book.index and book.loc[fld, 'returns_positive'] == 0
        no_neg = fld in book.index and book.loc[fld, 'returns_negative'] == 0
        by_sign = ((x > 0) & no_pos) | ((x < 0) & no_neg)
        if by_sign.any():
            zeroed.append({'part': v, 'records set to zero, booklet has no return with that sign': int(by_sign.sum()),
                           'weighted dollars of the donors set to zero (candidate weights)':
                           float((w[keep] * np.abs(x))[by_sign].sum())})
            base.loc[by_sign, v] = 0.0

    # Donors drawn by the network can break the pension identities (each item is its own slot): total pensions (gross
    # retirement less gross Social Security) made at least taxable pensions and never below zero, before any scaling
    tot_p = base['retirement_gross'].to_numpy(float) - base['gross_ss'].to_numpy(float)
    fixed_p = np.maximum(tot_p, np.maximum(base['txbl_pens_dist'].to_numpy(float), 0.0))
    changed = fixed_p != tot_p
    common.say(f'donors with total pensions below taxable pensions or below zero, made consistent: {int(changed.sum())}')
    zeroed.append({'part': 'retirement_gross', 'records raised so total pensions are at least taxable pensions and at '
                   'least zero': int(changed.sum())})
    base['retirement_gross'] = base['gross_ss'].to_numpy(float) + fixed_p
    base_src = base                       # the donors before the per-group zeros, which follow each record's group

    def zero_groups(src, grp):
        """A part a group's raw record has none of is zero on the group's records (redone every round, as records
        move between groups, so the counts are matched on what the scaling will leave)."""
        out_ = src.copy()
        for v in PRIMITIVES:
            kill = (nets[v].to_numpy(float)[grp] == 0) & (out_[v].to_numpy(float) != 0)
            out_.loc[kill, v] = 0.0
        return out_

    # 4-6 with the group check
    donor = keep.copy()
    history = []
    for it in range(int(S['group_iterations'])):
        base = zero_groups(base_src, grp)
        frame = puf_totals(base)
        wts, w_start, counts, info = match_counts(frame, grp, n_ret, book)
        hold = it >= int(S['move_iterations'])
        scaled, factors_tab = scale_all(base, wts, grp, nets, book, hold=hold)
        g_final = agi_group(scaled['agi'].to_numpy(float))
        mis = g_final != grp
        history.append({'round': it + 1, 'held in group by other income': hold, 'records': len(base), **{f'records, {GROUPS[g]}': int((grp == g).sum())
                                                                    for g in range(N_G)},
                        'outside their group after scaling': int(mis.sum()), 'counts met': int(counts['met'].sum()),
                        'counts': len(counts)})
        common.say(f'round {it + 1}: {int(mis.sum())} records outside their group; counts met '
                   f'{int(counts["met"].sum())} of {len(counts)}')
        if not mis.any() or it == int(S['group_iterations']) - 1:
            break
        if it < int(S['move_iterations']):
            grp = np.where(mis, g_final, grp)
        else:
            base_src, grp, donor = base_src[~mis].reset_index(drop=True), grp[~mis], donor[~mis]
    hold = True
    history = pd.DataFrame(history)
    extra = 0
    while mis.any():          # the records still outside their group are dropped, and steps 4-6 done again
        extra += 1
        assert extra <= 30, f'{int(mis.sum())} records still outside their group after 30 rounds of dropping'
        common.say(f'{int(mis.sum())} records still outside their group: dropped, weights and scaling done again '
                   f'(groups {grp[mis].tolist()}, AGI {np.round(scaled["agi"].to_numpy()[mis] / 1e6, 3).tolist()} million; '
                   f'other income: {factors_tab.loc[factors_tab.part == "other_agi_remainder", "notes"].tolist()})')
        base_src, grp, donor = base_src[~mis].reset_index(drop=True), grp[~mis], donor[~mis]
        base = zero_groups(base_src, grp)
        frame = puf_totals(base)
        wts, w_start, counts, info = match_counts(frame, grp, n_ret, book)
        scaled, factors_tab = scale_all(base, wts, grp, nets, book, hold=hold)
        mis = agi_group(scaled['agi'].to_numpy(float)) != grp
        history = pd.concat([history, pd.DataFrame([{
            'round': len(history) + 1, 'held in group by other income': hold, 'records': len(base),
            **{f'records, {GROUPS[g]}': int((grp == g).sum()) for g in range(N_G)},
            'outside their group after scaling': int(mis.sum()), 'counts met': int(counts['met'].sum()),
            'counts': len(counts)}])], ignore_index=True)

    for v in PRIMITIVES:
        x = base_src[v].to_numpy(float)
        kill = (nets[v].to_numpy(float)[grp] == 0) & (x != 0)
        if kill.any():
            zeroed.append({'part': v, 'records set to zero, the group\'s raw record has none': int(kill.sum()),
                           'groups': ', '.join(GROUPS[g] for g in sorted(set(grp[kill])))})
    # the rebuilt 2015 returns
    h15 = scaled.copy()
    h15.insert(0, 'hidden_group', grp)
    h15.insert(1, 'weight', wts)
    h15.insert(2, 'weight_equal', w_start)
    h15.insert(3, 'donor_record_id', cand['record_id'].to_numpy()[donor])
    h15.insert(4, 'model', cand['model'].to_numpy()[donor])
    h15['rules'] = ['; '.join(reasons[int(i)]) for i in donor]
    for c in ['married', 'n_dep', 'age_range', 'filing_status', 'scf_wave', 'scf_year']:
        h15[c] = cand[c].to_numpy()[donor]
    donor_2015 = pool.iloc[donor].reset_index(drop=True)
    h15.to_parquet(f'{out}/hidden_2015.parquet', index=False)
    donor_2015.assign(hidden_group=grp, weight=wts).to_parquet(f'{out}/donors_2015.parquet', index=False)

    # checks in 2015 terms
    checks_2015(h15, donor_2015, wts, grp, nets, n_ret, book, counts, out, agg)
    history.to_csv(f'{out}/check_rounds.csv', index=False)
    factors_tab.to_csv(f'{out}/check_factors.csv', index=False)
    added.to_csv(f'{out}/check_targeted.csv', index=False)
    pd.DataFrame(zeroed).to_csv(f'{out}/check_zeroed.csv', index=False)
    pd.Series({'selected candidates': len(chosen), 'selected, population weight': w[chosen].sum(),
               'candidates': len(cand), 'kept records': len(h15), 'count solve': info['how'],
               'count solve feasible': info['feasible'], 'effective sample kept by the count matching (%)': 100 * info['effective_sample_kept'],
               'largest weight factor of the count matching': info['largest_weight_factor_overall'],
               'smallest weight factor of the count matching': info['smallest_weight_factor']}
              ).to_csv(f'{out}/check_selection.csv', header=['value'])

    # 7-8: the target year
    rows = target_year_rows(h15, cand.iloc[donor].reset_index(drop=True), models, factors, out)
    return rows


def checks_2015(h15, donor, w, grp, nets, n_ret, book, counts, out, agg):
    """Part A's checks in 2015 terms: counts and dollars against the booklet and the four raw records, the
    accounting rules, each record inside its group."""
    counts.to_csv(f'{out}/check_counts.csv', index=False)
    # dollars by group against the raw records, every part and the totals
    rows = []
    tot = puf_totals(pd.DataFrame({v: nets[v].to_numpy() for v in PRIMITIVES}).fillna(0.0).assign(filing_status=0))
    for part in SUPPRESSED_FIELDS.values():          # suppressed: no raw amount (the totals read them as 0)
        if nets[part].isna().all():
            tot[part] = np.nan
    for v in PRIMITIVES + ['capital_gains', 'business_income', 'sch_e_income', 'dividends', 'adjustments', 'other_agi',
                           'agi']:
        x = h15[v].to_numpy(float)
        for g in range(N_G):
            m = grp == g
            rows.append({'item': v, 'group': GROUPS[g], 'raw record (mean x returns)': tot[v].iloc[g],
                         'rebuilt': (w * x)[m].sum(), 'donors before scaling': (w * donor[v].to_numpy(float))[m].sum()})
    t = pd.DataFrame(rows)
    t['difference'] = t['rebuilt'] - t['raw record (mean x returns)']
    t.to_csv(f'{out}/check_dollars_by_group.csv', index=False)
    # booklet: positive and negative counts and dollars of every booklet field the rebuild carries, and the totals
    rows = []
    fields = {**COUNT_FIELDS, **CHECK_FIELDS}
    import target_list as TLS
    for fld, col in fields.items():
        if fld not in book.index:
            continue
        x = TLS.record_value(h15, col) if col == 'pship_scorp_net' else h15[col].to_numpy(float)
        rows.append({'field': fld, 'column': col, 'kind': 'total (check)' if fld in CHECK_FIELDS and col not in
                     ('sole_prop', 'farm') else 'part',
                     'returns positive, booklet': book.loc[fld, 'returns_positive'], 'returns positive, rebuilt': w[x > 0].sum(),
                     'returns negative, booklet': book.loc[fld, 'returns_negative'], 'returns negative, rebuilt': w[x < 0].sum(),
                     'positive $bn, booklet': book.loc[fld, 'amount_positive'] / 1e9,
                     'positive $bn, rebuilt': (w * np.maximum(x, 0)).sum() / 1e9,
                     'negative $bn, booklet': book.loc[fld, 'amount_negative'] / 1e9,
                     'negative $bn, rebuilt': (w * np.minimum(x, 0)).sum() / 1e9})
    pd.DataFrame(rows).to_csv(f'{out}/check_booklet.csv', index=False)
    # accounting rules and groups
    cap = np.where(h15['filing_status'].to_numpy() == 3, -1500, -3000)
    rules = {
        'every record inside its AGI group (2015 dollars)': agi_group(h15['agi']) == grp,
        'qualified dividends at most ordinary dividends': h15['div_pref'] <= h15['dividends'] + 1e-6,
        'dividends = qualified + other': np.isclose(h15['dividends'], h15['div_pref'] + h15['div_ord']),
        'capital gain = short + long, a loss held to the limit': np.isclose(
            h15['capital_gains'], common.allowed_capital_gain(h15['kg_st'] + h15['kg_lt'], h15['filing_status'])),
        'capital gain at least the allowed loss': h15['capital_gains'] >= cap - 1e-6,
        'taxable Social Security at most 85% of gross': h15['ss_taxable'] <= 0.85 * h15['gross_ss'] + 1e-6,
        'taxable pensions at most total pensions': h15['txbl_pens_dist'] <= h15['retirement_gross'] - h15['gross_ss'] + 1e-6,
        'gross retirement at least gross Social Security': h15['retirement_gross'] >= h15['gross_ss'] - 1e-6,
        'no negative amount in a positive-only part': np.ones(len(h15), bool),
        'AGI = wages + interest + dividends + capital gain + other AGI': np.isclose(
            h15['agi'], h15[D.AGI_PARTS].sum(axis=1)),
    }
    two_signed = set(SPLIT_PARTS) | set(RESIDUALS) | {'kg_dist'}
    for v in PRIMITIVES:
        if v not in two_signed and v != 'retirement_gross':
            rules['no negative amount in a positive-only part'] &= h15[v].to_numpy() >= 0
    # the long-term gain pieces against the long-term gain (reported, not enforced: no strict rule in the law)
    for v in ['kg_1250', 'kg_collect']:
        rules[f'{v} at most the long-term gain, where positive (reported only)'] = \
            (h15[v] <= np.maximum(h15['kg_lt'], 0) + 1e-6) | (h15[v] == 0)
    rows = [{'rule': k, 'records breaking it': int((~np.asarray(v, bool)).sum()),
             'returns breaking it': float(w[~np.asarray(v, bool)].sum())} for k, v in rules.items()]
    pd.DataFrame(rows).to_csv(f'{out}/check_rules.csv', index=False)
    # charity: gifts against the deduction (E19700), by group
    rows = []
    for g in range(N_G):
        m = grp == g
        r = int(RECIDS[g])
        rows.append({'group': GROUPS[g], 'cash gifts $bn': (w * h15['char_cash'])[m].sum() / 1e9,
                     'other gifts $bn': (w * h15['char_noncash'])[m].sum() / 1e9,
                     'deduction (E19700, raw record) $bn': agg.set_index('RECID').loc[r, 'E19700'] * n_ret[g] / 1e9,
                     'cash gifts (E19800, raw record) $bn': agg.set_index('RECID').loc[r, 'E19800'] * n_ret[g] / 1e9,
                     'other gifts (E20100, raw record) $bn': agg.set_index('RECID').loc[r, 'E20100'] * n_ret[g] / 1e9,
                     'gifts above half of AGI (returns)': w[m & ((h15['char_cash'] + h15['char_noncash'])
                                                                 > 0.5 * np.maximum(h15['agi'], 0))].sum()})
    pd.DataFrame(rows).to_csv(f'{out}/check_charity.csv', index=False)
    item_mix(h15, out)


def target_year_rows(h15, donors, models, factors, out):
    """Steps 7 and 8: the balance sheet drawn again, the PUF items grown to the target year, every total recomputed;
    rows in the generated file's schema. Writes hidden_<year>.parquet, check_wealth.csv and growth_<year>.csv."""
    import tilt as TL
    import qgroups as Q
    import model as M
    maps = D.load_maps(Y)
    stk = donors.reset_index(drop=True).copy()                    # the donors in the stack's own coding
    # PUF bands from the final 2015 dollars (the network's view of the rebuilt return), the computed flags from them
    bands = reband(h15)
    for c in bands:
        stk[c] = bands[c].to_numpy().astype(stk[c].dtype)
    for flag, parts in D.META.get('computed_flags', {}).items():
        stk[flag] = (stk[[f'{p}_band' for p in parts]] != 0).any(axis=1).astype(stk[flag].dtype)
    qg = Q.puf_group(h15['agi'].to_numpy(float), Q.puf_groups()['cuts'])
    stk['agi_group'], stk['q_group'], stk['label_ok'] = qg.astype(stk['agi_group'].dtype), qg, True
    stk['record_id'] = int(S['id_block']) + np.arange(len(stk))
    nw_before = D.draw_dollars(donors.reset_index(drop=True), maps, Y)['net_worth'].to_numpy(float)   # the donors' own
    # 7. wealth: the SCF slots drawn again; their stored picks are the kept try's (keyed by record id)
    new, dol, accepted, tried, kept = redraw_wealth(stk, models, factors, maps)
    for c in new:
        stk[c] = new[c].to_numpy().astype(stk[c].dtype)
    import ages
    for i, v in enumerate(M.META['scf_amounts']):
        u = np.array([ages.uniform_by_id(np.array([rid]), int(S['wealth_salt']) + 100 * int(t) + i)[0]
                      for rid, t in zip(stk['record_id'].to_numpy(), kept)])
        stk[f'u_{v}'] = u                                         # float64: the exact picks the try used
    rows = D.draw_dollars(stk, maps, Y)                           # target-year dollars from the new bands and picks
    assert np.allclose(rows['net_worth'].to_numpy(float), dol['net_worth'].to_numpy(float), rtol=1e-9, atol=1e-3), \
        'the balance sheet kept differs from its try'
    rows['hidden_wealth_fallback'] = (accepted == -1).astype(np.int64)
    # 8. the PUF items grown from 2015 (tilt.grow_dollars on the PUF block alone), then every total recomputed
    puf15 = h15[PRIMITIVES + ['capital_gains_line13', 'capital_gains', 'business_income', 'business_income_drawn',
                             'other_agi'] + [k for k in D.META['computed_totals'] if k != 'business_income']].copy()
    puf15 = puf15.loc[:, ~puf15.columns.duplicated()]
    grown, growth = TL.grow_dollars(puf15, PY, Y)
    # since 29-30 October the short and long term pieces are the gains and losses before the loss limit, grown like any
    # amount; recombine() applies the nominal limit in the target year (before, a piece kg_other held the limit and a
    # rule here kept returns at it)
    grown, _ = TL.recombine(grown, Y)
    for c in grown:
        rows[c] = grown[c].to_numpy()
    rows, _ = TL.recombine(rows, Y)
    rows['synthetic_top'] = 1
    rows['hidden_group'] = h15['hidden_group'].to_numpy()
    rows['weight_2015'] = h15['weight'].to_numpy()
    rows['donor_record_id'] = h15['donor_record_id'].to_numpy()
    rows['weight'] = rows['weight_2015']                          # set in the file's units by rebuild_hidden_returns
    rows.to_parquet(f'{out}/hidden_{Y}.parquet', index=False)
    growth.to_csv(f'{out}/growth_{Y}.csv', index=False)
    # the income-wealth pattern, the new draws against the donors' own
    agi19 = D.agi(rows)
    w = h15['weight'].to_numpy(float)
    nw_after = rows['net_worth'].to_numpy(float)
    res = []
    for g in range(N_G):
        m = h15['hidden_group'].to_numpy() == g
        for name, nw in [('donor\'s own draw', nw_before), ('drawn again', nw_after)]:
            res.append({'group': GROUPS[g], 'balance sheet': name, 'returns': w[m].sum(),
                        f'mean AGI ({Y} $m)': (w * agi19)[m].sum() / w[m].sum() / 1e6,
                        f'mean net worth ({Y} $m)': (w * nw)[m].sum() / w[m].sum() / 1e6,
                        f'median net worth ({Y} $m)': common.wquantile(nw[m], w[m], [0.5])[0] / 1e6,
                        'median of net worth / AGI (AGI above 0)': common.wquantile((nw / agi19)[m & (agi19 > 0)],
                                                                                    w[m & (agi19 > 0)], [0.5])[0]
                        if (m & (agi19 > 0)).any() else np.nan,
                        'share with net worth $100m+ (%)': 100 * w[m & (nw >= 1e8)].sum() / w[m].sum(),
                        'share with net worth $1bn+ (%)': 100 * w[m & (nw >= 1e9)].sum() / w[m].sum(),
                        'records': int(m.sum())})
    pd.DataFrame(res).to_csv(f'{out}/check_wealth.csv', index=False)
    acc = pd.Series(accepted).value_counts().sort_index()
    acc.rename(index={-1: 'fallback (every try $1bn+)'}).to_csv(f'{out}/check_wealth_tries.csv', header=['records'])
    common.say('wealth: records accepted at each try:\n' + acc.to_string())
    return rows


def item_mix(h15, out):
    """The rebuilt returns' item mix by group (2015 dollars, starting weights), for Part B's split of billionaires'
    income kinds into tax items (PLAN.md step 15): every part's positive and negative dollars separately, as a share of
    the group's AGI, with the returns holding each. Writes item_mix_2015.csv."""
    w = h15['weight'].to_numpy(float)
    rows = []
    cols = PRIMITIVES + ['capital_gains', 'dividends', 'business_income', 'sch_cf_income', 'sch_e_income', 'adjustments',
                         'other_agi', 'agi']
    for g in range(N_G):
        m = h15['hidden_group'].to_numpy() == g
        agi = (w * h15['agi'].to_numpy(float))[m].sum()
        for v in cols:
            x = h15[v].to_numpy(float)
            pos, neg = (w * np.maximum(x, 0))[m].sum(), (w * np.minimum(x, 0))[m].sum()
            rows.append({'group': GROUPS[g], 'item': v, 'positive $m': pos / 1e6, 'negative $m': neg / 1e6,
                         'returns with positive': w[m & (x > 0)].sum(), 'returns with negative': w[m & (x < 0)].sum(),
                         'positive, share of the group\'s AGI': pos / agi, 'negative, share of the group\'s AGI': neg / agi})
    pd.DataFrame(rows).to_csv(f'{out}/item_mix_2015.csv', index=False)


# ---- the entry point for tilt.main ---------------------------------------------------------------------------------
def rebuild_hidden_returns(g19=None, year=Y, path=None, details=False):
    """The rebuilt aggregate records as rows to append to the generated file (see the top). g19: the generated file
    as tilt.main has it after dollars.draw_dollars (target-year dollars, importance weights in column weight). Reads
    hidden_<year>.parquet from the folder (top_hidden.folder; built by `python top_hidden.py build`, which needs the
    trained networks and is best run on a GPU node); builds it first if it is not there.
    Returns a frame with exactly g19's columns plus synthetic_top (1), in target-year dollars, its weight in g19's
    units: each rebuilt return's 2015 population weight times g19's total filer weight over the 2015 PUF's retained
    returns (the population the generated filers stand for), so tilt.main's scaling of filers to SOI's returns gives
    each rebuilt return its share of the year's returns. The caller sets synthetic_top = 0 on g19's own rows. With
    details, also hidden_group (0-3), weight_2015, donor_record_id and hidden_wealth_fallback."""
    assert year == Y, f'the rebuilt returns are built for the run\'s target year {Y}'
    path = path or f'{folder()}/hidden_{year}.parquet'
    rows = pd.read_parquet(path) if os.path.exists(path) else build()
    extra = ['hidden_group', 'weight_2015', 'donor_record_id', 'hidden_wealth_fallback']
    if g19 is not None:
        missing = [c for c in g19.columns if c not in rows.columns]
        assert not missing, f'columns of the generated file the rebuilt returns lack: {missing}'
        assert not set(rows['record_id']) & set(g19['record_id']), 'a rebuilt return\'s record id is in the file'
        import generate as G
        filer = g19['filer'].to_numpy() == 1
        r2015 = float(np.sum(G.LABEL_META['returns']))
        rows['weight'] = rows['weight_2015'] * g19['weight'].to_numpy(float)[filer].sum() / r2015
        cols = list(g19.columns) + ['synthetic_top'] + (extra if details else [])
        rows = rows[cols].copy()
        # missing values only where the file has them too (ssn2 without a spouse, dep_ssn without dependents, ...)
        bad = sorted(set(rows.columns[rows.isna().any()]) - set(g19.columns[g19.isna().any()]))
        assert not bad, f'missing values in the rebuilt returns in columns the file has none in: {bad}'
        for c in g19.columns:
            if rows[c].dtype != g19[c].dtype and g19[c].dtype != object and not rows[c].isna().any():
                rows[c] = rows[c].astype(g19[c].dtype)
    return rows


# ---- the Table 4.3 checks -----------------------------------------------------------------------------------------
T43_ITEMS = {'returns': None, 'agi': 'adjusted gross income less deficit', 'wages': 'salaries and wages',
             'taxable_interest': 'taxable interest', 'ordinary_dividends': 'ordinary dividends',
             'capital_gain_total': 'sales of capital assets, net gain less loss',
             'pship_scorp_net_income': 'partnership and s corporation > net income',
             'pship_scorp_net_loss': 'partnership and s corporation > net loss'}
T43_GROUPS = {'top 0.001 percent': 99.999, 'top 0.01 percent': 99.99, 'top 0.1 percent': 99.9}


def table_43(year):
    """SOI Table 4.3 for the year: per group, the AGI floor, returns and the items' amounts."""
    t = pd.read_csv(f"{common.RAW['soi_tables']}/pctile_top_items.csv")
    t = t[(t.tax_year == year) & (t['table'].astype(str) == '4.3') & t.pctile_group.isin(T43_GROUPS)]
    out = {}
    for gname in T43_GROUPS:
        x = t[t.pctile_group == gname]
        r = {'returns': float(x[(x.item == 'all') & (x.measure == 'returns')]['value'].iloc[0]),
             'AGI floor': float(x[x.item.str.startswith('adjusted gross income floor')]['value'].iloc[0])}
        for k, item in T43_ITEMS.items():
            if item is None:
                continue
            meas = 'value' if k == 'agi' else 'amount'
            r[k] = 1000.0 * float(x[(x.item == item) & (x.measure == meas)]['value'].iloc[0])
        out[gname] = r
    return out


def top_groups(df, w, label):
    """The file's top 0.001%, 0.01% and 0.1% by AGI among returns other than dependent filers: returns, AGI floor,
    AGI and the items (target_list.record_value, the tilt's own definitions)."""
    import target_list as TLS
    pop = (df['filer'].to_numpy() == 1) & (df['age_range'].to_numpy() < common.ADULT_RANGES)
    a = D.agi(df)
    rows = []
    for gname, p in T43_GROUPS.items():
        line = float(common.wquantile(a[pop], w[pop], [p / 100])[0])
        m = pop & (a >= line)
        r = {'file': label, 'group': gname, 'returns': w[m].sum(), 'AGI floor': line, 'agi': (w * a)[m].sum()}
        for k in T43_ITEMS:
            if k in ('returns', 'agi'):
                continue
            r[k] = (w * TLS.record_value(df, k))[m].sum()
        rows.append(r)
    return pd.DataFrame(rows)


def check():
    """The top 0.001%, 0.01% and 0.1% against Table 4.3: in 2015 (the run's generated filers in 2015 dollars at their
    2015 population weights, before and after adding the rebuilt returns at theirs), and in the target year at the
    tilt's starting scaling (filers to SOI's returns), before any reweighting. Then target_list.expand with the file
    plus the rebuilt returns as the start: the top 0.001% must be kept."""
    import generate as G
    import target_list as TLS
    out = folder()
    stack = pd.read_parquet(f'{common.GEN}/stack.parquet')
    h15 = pd.read_parquet(f'{out}/hidden_2015.parquet')
    # 2015
    filers = stack[stack['filer'] == 1].reset_index(drop=True)
    d15 = dollars_2015(filers)
    for c in ['filer', 'age_range', 'filing_status']:
        d15[c] = filers[c].to_numpy()
    w15 = filers['weight'].to_numpy(float) * float(np.sum(G.LABEL_META['returns'])) / filers['weight'].sum()
    h = h15.assign(filer=1)
    both = pd.concat([d15, h[d15.columns]], ignore_index=True)
    wb = np.concatenate([w15, h15['weight'].to_numpy(float)])
    pub = table_43(PY)
    res = [top_groups(d15, w15, 'generated filers'), top_groups(both, wb, 'with the rebuilt returns')]
    res.append(pd.DataFrame([{'file': f'SOI Table 4.3, {PY}', 'group': k, **v} for k, v in pub.items()]))
    t15 = pd.concat(res, ignore_index=True)
    t15.to_csv(f'{out}/check_table43_{PY}.csv', index=False)
    common.say(f'Table 4.3, {PY}:\n' + t15.to_string(index=False))
    # the target year, the tilt's starting scaling
    maps = D.load_maps(Y)
    g19 = D.draw_dollars(stack, maps, Y)
    have = pd.read_parquet(f'{out}/hidden_{Y}.parquet').columns
    lack = [c for c in g19.columns if c not in have]
    if lack:   # a stack drawn by older code (the retrain of 29 October drew blindness in generation)
        common.say(f'the run\'s stack has columns the rebuilt returns lack (older generation code), left out of this '
                   f'check: {lack}')
        g19 = g19.drop(columns=lack)
    hid = rebuild_hidden_returns(g19)
    soi = TG.load()
    full = pd.concat([g19, hid.drop(columns=['synthetic_top'])], ignore_index=True)
    res = []
    for name, df in [('generated file', g19), ('with the rebuilt returns', full)]:
        imp = df['weight'].to_numpy(float)
        filer = df['filer'].to_numpy() == 1
        w0 = np.where(filer, imp * TG.returns(soi, Y) / imp[filer].sum(), imp)
        res.append(top_groups(df, w0, name))
    pub = table_43(Y)
    res.append(pd.DataFrame([{'file': f'SOI Table 4.3, {Y}', 'group': k, **v} for k, v in pub.items()]))
    t19 = pd.concat(res, ignore_index=True)
    t19.to_csv(f'{out}/check_table43_{Y}.csv', index=False)
    common.say(f'Table 4.3, {Y}, before any reweighting:\n' + t19.to_string(index=False))
    # the restored target is kept by expand
    rows = TLS.expand(Y, start=full)
    top = rows[(rows['kind'] == 'soi 4.3') & (rows['group'] == 'top 0.001 percent')]
    ex = TLS.LAST['excluded']
    common.say(f'target_list.expand: {len(top)} top 0.001% targets kept; excluded among them: '
               f'{int(((ex["kind"] == "soi 4.3") & (ex["group"] == "top 0.001 percent")).sum()) if len(ex) else 0}')
    top[['kind', 'group', 'target', 'value', 'se', 'error_kind']].to_csv(f'{out}/check_top_targets_{Y}.csv', index=False)
    hid.to_parquet(f'{out}/hidden_{Y}_rows.parquet', index=False)


if __name__ == '__main__':
    what = sys.argv[1] if len(sys.argv) > 1 else 'build'
    if what == 'build':
        build()
    elif what == 'check':
        check()
    elif what == 'mix':
        item_mix(pd.read_parquet(f'{folder()}/hidden_2015.parquet'), folder())
    else:
        raise SystemExit(f'unknown: {what} (build or check)')
