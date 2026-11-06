"""The state weights' targets (FEATURES.md, "State weights"): IRS Historic Table 2 and Census adults, read for one year's
file.

What this module gives the next stage (state_weights.py), and a later joint solve:
  read_ht2(year)              Historic Table 2 as a table: one row per area and AGI group, amounts in dollars
  read_file(path)             the columns of a year's file that the targets read
  item_vectors(df)            {item: each record's value of the item}: the file's side of every item in ITEMS
  agi_group(agi)              each record's Historic Table 2 AGI group (1-10) from the file's calc_agi
  filer_targets(...)          target = the file's national total for the group and item x the IRS's share of the area
  forbes_states(df, year)     the list records' states
  nonfiler_targets(...)       each state's Census adults less the adults on its IRS returns, as shares of the file's
                              non-filer adults, by age band

The rule for a filer target. For area s, AGI group g, item v:
    target = (the file's total of v in g, every record, the list included) x (s's value of v in g) / (the sum over areas)
Worked example, 2019, wages in the group $50,000-75,000 in Connecticut: if the file has $1,200 billion of wages in the
group and the IRS has Connecticut at $12 billion of a national $1,180 billion, the target is $1,200 billion x 12 / 1,180
= $12.2 billion. Only the IRS's geography is used, not its level, so a gap between the file and the IRS nationally
cannot make the targets impossible to meet.

Cells that are merged, not positive or signed. The rule for each area, group and item (cell_rule below):
  - Historic Table 2 merges a cell with fewer than 10 returns into a neighbouring AGI group of the same state without a
    flag, so a merged cell reads zero. A zero cell in a group whose national total is not zero has no target: the state's
    true value there is unknown. Its neighbour keeps its published value (the file is told nothing about the merge).
  - A signed item (the IRS's AGI in the group under $1, capital gains, business income) keeps its sign: the target is the
    file's total x the area's signed share. The shares are only meaningful if the areas' values do not cancel, so in a
    group where |sum of values| is under half of the sum of |values| the whole item has no targets in that group.
  - An item whose national total in a group has opposite signs in the file and the IRS has no targets in that group.
"""
import os

import numpy as np
import pandas as pd

HT2_DIR = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-Ind/state/HT2'
PEP_DIR = '/nfs/roberts/project/pi_nrs36/shared/raw_data/Census-PEP'
HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, '..', '..', '..'))
FORBES = os.path.join(HERE, '..', 'resources', 'top', 'forbes_billionaires_{year}.csv')

# The 50 states and DC by FIPS code: the CPS's 51 values, the order of build_table.STATE_FIPS.
STATE_FIPS = [1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
              34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56]
STATES = ['AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME',
          'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC',
          'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY']
STATE_NAMES = ['Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware',
               'District of Columbia', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas',
               'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi',
               'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York',
               'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island',
               'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington',
               'West Virginia', 'Wisconsin', 'Wyoming']
assert len(STATE_FIPS) == len(STATES) == len(STATE_NAMES) == 51
# Outside the 51: the IRS's Other Areas (armed forces and foreign addresses, territories), and Puerto Rico from 2018.
OTHER = ['OA', 'PR']


def areas(year):
    """The areas of a year's Historic Table 2: the 51, Other Areas, and Puerto Rico from 2018."""
    return STATES + (['OA', 'PR'] if year >= 2018 else ['OA'])


GROUP_LOWER = np.array([-np.inf, 1, 10e3, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6])
GROUP_NAMES = ['under $1', '$1-10k', '$10-25k', '$25-50k', '$50-75k', '$75-100k', '$100-200k', '$200-500k', '$500k-1m', '$1m+']


def agi_group(agi):
    """Historic Table 2's AGI group, 1 to 10, lower edge inclusive."""
    a = np.asarray(agi, float)
    assert np.isfinite(a).all(), 'a record with no AGI'
    return np.searchsorted(GROUP_LOWER, a, side='right')


def read_ht2(year):
    """Historic Table 2 of a tax year: columns state and group (1-10; the US row and the area totals, group 0, kept in a
    second frame), every other column a number, amounts (A...) in dollars. Numbers are quoted with thousands separators."""
    d = pd.read_csv(f'{HT2_DIR}/ht2_{year}.csv.gz', dtype=str)
    d.columns = [c.upper() for c in d.columns]
    d['STATE'] = d['STATE'].str.strip().str.upper()
    d['AGI_STUB'] = d['AGI_STUB'].astype(int)
    num = [c for c in d.columns if c not in ('STATE', 'AGI_STUB')]
    n_blank = d[num].isna().sum()
    # An item Historic Table 2 leaves blank in a row is not a zero: it stays missing and the item's users see it.
    if n_blank.any():
        print(f'Historic Table 2, {year}: blank cells {n_blank[n_blank > 0].to_dict()}', flush=True)
    for c in num:
        x = pd.to_numeric(d[c].str.replace(',', '', regex=False), errors='coerce')
        n_bad = int(x.isna().sum() - d[c].isna().sum())
        if n_bad:
            print(f'Historic Table 2, {year}: {c} has {n_bad} entries that are not numbers', flush=True)
        d[c] = x * 1000.0 if c.startswith('A') else x
    d = d.rename(columns={'STATE': 'state', 'AGI_STUB': 'group'})
    want = areas(year)
    got = sorted(set(d['state']) - {'US'})
    assert sorted(want) == got, f'{year}: areas {got} against {sorted(want)}'
    assert d.groupby('state').size().eq(11).all()
    return d


# The file's columns the items read.
FILE_COLUMNS = ['record_id', 'weight', 'filer', 'filing_status', 'n_dep', 'married', 'age1', 'age2', 'age_range', 'billionaire',
                'forbes_uri', 'calc_agi', 'wages', 'txbl_int', 'div_ord', 'div_pref', 'capital_gains_line13', 'kg_st', 'kg_lt',
                'salt_inc_sales', 'salt_prop', 'first_mort_int', 'calc_eitc', 'sole_prop', 'txbl_pens_dist', 'ss_taxable',
                'calc_liab_after_credits', 'calc_liab_iit', 'calc_itemizing', 'calc_claimed_taxes', 'calc_claimed_interest',
                'calc_claimed_charity', 'txbl_ira_dist', 'ui', 'part_passive', 'part_active', 'part_passive_loss',
                'part_active_loss', 'part_179', 'scorp_passive', 'scorp_active', 'scorp_passive_loss', 'scorp_active_loss',
                'scorp_179', 'exempt_int', 'gross_ss', 'state_ref', 'char_cash', 'char_noncash', 'calc_txbl_inc',
                'calc_liab_bc', 'calc_liab_niit', 'calc_liab_seca', 'calc_ctc_ref', 'calc_ctc_nonref', 'calc_qbi_ded',
                'adjustments', 'farm', 'calc_std_ded', 'calc_item_ded_claimed', 'calc_item_ded', 'calc_nonref', 'calc_liab_amt',
                'gross_pens_dist', 'net_ptc', 'rent', 'rent_loss', 'estate', 'estate_loss']


def read_file(path, extra=()):
    cols = [c for c in FILE_COLUMNS]
    have = set(__import__('pyarrow.parquet').parquet.ParquetFile(path).schema.names)
    missing = [c for c in cols if c not in have]
    assert not missing, f'{path}: no columns {missing}'
    df = pd.read_parquet(path, columns=cols + list(extra))
    na = df.drop(columns=['forbes_uri']).isna().sum()
    if na.any():
        print(f'{path}: missing values in the columns the targets read: {na[na > 0].to_dict()}', flush=True)
    return df


# The items. name: (the Historic Table 2 columns, summed; how the file's side is made; 'count' or 'amount'; a note).
# Iselin's 21 series (HT2_TARGET_MAP, puf_series_x, HT2_SERIES_FOR; src/nonfilers/state_weights.R) come first. A count is
# a count of returns with the item not zero, as Historic Table 2's N columns are.
def _item_table():
    nz = lambda v: (lambda d: (d[v].to_numpy(float) != 0).astype(float))
    amt = lambda v: (lambda d: d[v].to_numpy(float))
    fs = lambda k: (lambda d: (d['filing_status'].to_numpy() == k).astype(float))
    div = lambda d: d['div_ord'].to_numpy(float) + d['div_pref'].to_numpy(float)
    itm = lambda d: d['calc_itemizing'].to_numpy(float) > 0
    retire = lambda d: d['txbl_pens_dist'].to_numpy(float) + d['txbl_ira_dist'].to_numpy(float)
    # the partnership and S corporation line of Schedule E as the file draws it: gains less losses less section 179
    parts = ['part_passive', 'part_active', 'scorp_passive', 'scorp_active']
    loss = ['part_passive_loss', 'part_active_loss', 'part_179', 'scorp_passive_loss', 'scorp_active_loss', 'scorp_179']
    pship = lambda d: sum(d[c].to_numpy(float) for c in parts) - sum(np.abs(d[c].to_numpy(float)) for c in loss)
    return {
        'n_returns': (['N1'], lambda d: np.ones(len(d)), 'count'),
        'n_single': (['MARS1'], fs(1), 'count'),
        'n_joint': (['MARS2'], fs(2), 'count'),
        'n_hoh': (['MARS4'], fs(4), 'count'),
        'n_indiv': (['N2'], lambda d: 1.0 + (d['filing_status'].to_numpy() == 2) + d['n_dep'].to_numpy(float), 'count'),
        'agi_amt': (['A00100'], amt('calc_agi'), 'amount'),
        'n_wages': (['N00200'], nz('wages'), 'count'),
        'wages_amt': (['A00200'], amt('wages'), 'amount'),
        'n_int': (['N00300'], nz('txbl_int'), 'count'),
        'int_amt': (['A00300'], amt('txbl_int'), 'amount'),
        'n_div': (['N00600'], lambda d: (div(d) != 0).astype(float), 'count'),
        'div_amt': (['A00600'], div, 'amount'),
        'n_kg': (['N01000'], nz('capital_gains_line13'), 'count'),
        'kg_amt': (['A01000'], amt('capital_gains_line13'), 'amount'),   # not in Iselin's 21: added by the plan
        # Schedule A items are the IRS's among itemizers: the file draws them on other records too (2.5 times the IRS's count of
        # returns with property tax in 2019), so the file's side counts them on records the calculator itemizes
        'n_salt': (['N18425', 'N18450'], lambda d: ((d['salt_inc_sales'].to_numpy(float) != 0) & itm(d)).astype(float), 'count'),
        'salt_amt': (['A18425', 'A18450'], lambda d: d['salt_inc_sales'].to_numpy(float) * itm(d), 'amount'),
        'n_re_tax': (['N18500'], lambda d: ((d['salt_prop'].to_numpy(float) != 0) & itm(d)).astype(float), 'count'),
        're_tax_amt': (['A18500'], lambda d: d['salt_prop'].to_numpy(float) * itm(d), 'amount'),
        'n_mort_int': (['N19300'], lambda d: ((d['first_mort_int'].to_numpy(float) != 0) & itm(d)).astype(float), 'count'),
        'mort_int_amt': (['A19300'], lambda d: d['first_mort_int'].to_numpy(float) * itm(d), 'amount'),
        'n_eitc': (['N59660'], lambda d: (d['calc_eitc'].to_numpy(float) > 0).astype(float), 'count'),
        'eitc_amt': (['A59660'], amt('calc_eitc'), 'amount'),
        # the nine held-out items of validate_state_weights.R
        'n_bus': (['N00900'], nz('sole_prop'), 'count'),
        'bus_amt': (['A00900'], amt('sole_prop'), 'amount'),
        'n_pens': (['N01700'], nz('txbl_pens_dist'), 'count'),
        'pens_amt': (['A01700'], amt('txbl_pens_dist'), 'amount'),
        'n_ss': (['N02500'], lambda d: (d['ss_taxable'].to_numpy(float) > 0).astype(float), 'count'),
        'ss_amt': (['A02500'], amt('ss_taxable'), 'amount'),
        'n_fedtax': (['N06500'], lambda d: (d['calc_liab_after_credits'].to_numpy(float) > 0).astype(float), 'count'),
        'fedtax_amt': (['A06500'], lambda d: np.maximum(d['calc_liab_after_credits'].to_numpy(float), 0.0), 'amount'),
        'kg_amt_pos': (['A01000'], lambda d: np.maximum(d['kg_st'].to_numpy(float) + d['kg_lt'].to_numpy(float), 0.0), 'amount'),
        # other items, for the list of what the final files use (state_weights settings; RESULTS.md says which and why)
        'n_ira': (['N01400'], nz('txbl_ira_dist'), 'count'),
        'ira_amt': (['A01400'], amt('txbl_ira_dist'), 'amount'),
        'n_ui': (['N02300'], nz('ui'), 'count'),
        'ui_amt': (['A02300'], amt('ui'), 'amount'),
        'n_qdiv': (['N00650'], nz('div_pref'), 'count'),
        'qdiv_amt': (['A00650'], amt('div_pref'), 'amount'),
        'n_pship': (['N26270'], lambda d: (pship(d) != 0).astype(float), 'count'),
        'pship_amt': (['A26270'], pship, 'amount'),
        'n_txinc': (['N04800'], lambda d: (d['calc_txbl_inc'].to_numpy(float) > 0).astype(float), 'count'),
        'txinc_amt': (['A04800'], amt('calc_txbl_inc'), 'amount'),
        'n_itemize': (['N04470'], lambda d: (d['calc_itemizing'].to_numpy(float) > 0).astype(float), 'count'),
        'itemize_amt': (['A04470'], lambda d: d['calc_item_ded_claimed'].to_numpy(float), 'amount'),
        'n_char': (['N19700'], lambda d: (d['calc_claimed_charity'].to_numpy(float) > 0).astype(float), 'count'),
        'char_amt': (['A19700'], amt('calc_claimed_charity'), 'amount'),
        'n_setax': (['N09400'], lambda d: (d['calc_liab_seca'].to_numpy(float) > 0).astype(float), 'count'),
        'setax_amt': (['A09400'], amt('calc_liab_seca'), 'amount'),
        'n_refund_state': (['N00700'], nz('state_ref'), 'count'),
        'refund_state_amt': (['A00700'], amt('state_ref'), 'amount'),
        'n_stdded': (['N04450'], lambda d: (d['calc_std_ded'].to_numpy(float) > 0).astype(float), 'count'),
        'qbi_amt': (['A04475'], amt('calc_qbi_ded'), 'amount'),
        'n_qbi': (['N04475'], lambda d: (d['calc_qbi_ded'].to_numpy(float) > 0).astype(float), 'count'),
        'n_niit': (['N85300'], lambda d: (d['calc_liab_niit'].to_numpy(float) > 0).astype(float), 'count'),
        'niit_amt': (['A85300'], amt('calc_liab_niit'), 'amount'),
        'n_actc': (['N11070'], lambda d: (d['calc_ctc_ref'].to_numpy(float) > 0).astype(float), 'count'),
        'actc_amt': (['A11070'], amt('calc_ctc_ref'), 'amount'),
        'n_exempt': (['N00400'], nz('exempt_int'), 'count'),
        'exempt_amt': (['A00400'], amt('exempt_int'), 'amount'),
        # 2018 only: the IRS published taxable IRA distributions and pensions as one line
        'n_retire': (['N01750'], lambda d: (retire(d) != 0).astype(float), 'count'),
        'retire_amt': (['A01750'], retire, 'amount'),
    }


ITEMS = _item_table()
ISELIN_21 = ['n_returns', 'n_single', 'n_joint', 'n_hoh', 'n_indiv', 'agi_amt', 'n_wages', 'wages_amt', 'n_int', 'int_amt', 'n_div',
             'div_amt', 'n_kg', 'n_salt', 'salt_amt', 'n_re_tax', 're_tax_amt', 'n_mort_int', 'mort_int_amt', 'n_eitc', 'eitc_amt']
HELD_OUT = ['n_bus', 'bus_amt', 'n_pens', 'pens_amt', 'n_ss', 'ss_amt', 'n_fedtax', 'fedtax_amt', 'kg_amt_pos']
# The items the final files use: Iselin's 21, capital gains dollars, his nine held-out items' own lines (with the capital gains
# line in place of its positive part), and the other lines of the return the file holds on the IRS's definition. A line the year's
# table does not have is left out that year (pensions and IRA distributions are one line in 2018; the QBI deduction starts in
# 2018; tax-exempt interest in 2022). In the final files an item has no targets in an AGI group where its national total
# in the file is outside TRUST times the IRS's (filer_targets, trust=True): the file's item then is not the IRS's item in that group.
EXTRA_ITEMS = ['kg_amt', 'n_ira', 'ira_amt', 'n_retire', 'retire_amt', 'n_ui', 'ui_amt', 'n_qdiv', 'qdiv_amt', 'n_pship', 'pship_amt',
               'n_txinc', 'txinc_amt', 'n_itemize', 'itemize_amt', 'n_char', 'char_amt', 'n_setax', 'setax_amt', 'n_refund_state',
               'refund_state_amt', 'n_qbi', 'qbi_amt', 'n_niit', 'niit_amt', 'n_actc', 'actc_amt', 'n_exempt', 'exempt_amt']
FINAL_ITEMS = ISELIN_21 + [v for v in HELD_OUT if v != 'kg_amt_pos'] + EXTRA_ITEMS
TRUST = (0.67, 1.5)
# ... and no targets in an AGI group where fewer than MIN_EFFECTIVE records stand behind the item: (sum |w x|)^2 / sum (w x)^2, the list
# records left out. State income tax deducted in the group under $1 rests on 4 effective records in 2019 and cannot be spread over 53
# areas; in Newton's method such an item takes the whole budget and moves nothing that matters.
MIN_EFFECTIVE = 100
assert len(ISELIN_21) == 21 and set(ISELIN_21 + HELD_OUT + FINAL_ITEMS) <= set(ITEMS)


def ht2_values(ht2, item):
    """A frame: index group (1-10), columns the areas, the item's Historic Table 2 values, in dollars or returns. None if a
    column the item needs is not in the year's table (the table's columns change by year, NOTES.md)."""
    codes = ITEMS[item][0]
    if any(c not in ht2.columns for c in codes):
        return None
    d = ht2[(ht2['state'] != 'US') & (ht2['group'] > 0)]
    v = d.pivot(index='group', columns='state', values=codes[0]).astype(float)
    for c in codes[1:]:
        v = v + d.pivot(index='group', columns='state', values=c).astype(float)
    return v


def us_values(ht2, item):
    """The item in Historic Table 2's US row, by group."""
    codes = ITEMS[item][0]
    d = ht2[(ht2['state'] == 'US') & (ht2['group'] > 0)].set_index('group')
    return sum(d[c] for c in codes).astype(float)


def item_vectors(df, items):
    """{item: one value per record}, every value finite."""
    out = {}
    for v in items:
        x = np.asarray(ITEMS[v][1](df), float)
        n_bad = int((~np.isfinite(x)).sum())
        assert n_bad == 0, f'item {v}: {n_bad} records with no value'
        out[v] = x
    return out


def cell_rule(values, file_total):
    """For one item in one AGI group: values, the areas' published values (a Series); file_total, the file's national total
    of the item in the group. Returns the areas' signed shares (NaN where the cell has no target), and a note if the item
    has no targets in the group. Rules: this module's header."""
    v = values.astype(float)
    n_na = int(v.isna().sum())
    if n_na == len(v):
        return pd.Series(np.nan, index=v.index), 'blank in the table'
    if n_na:
        return pd.Series(np.nan, index=v.index), f'{n_na} areas blank'
    total, size = v.sum(), v.abs().sum()
    if size == 0:
        return pd.Series(np.nan, index=v.index), 'zero in every area'
    if abs(total) < 0.5 * size:
        return pd.Series(np.nan, index=v.index), f'areas cancel: sum {total:,.0f} of sizes {size:,.0f}'
    if file_total == 0:
        return pd.Series(np.nan, index=v.index), 'zero in the file'
    if np.sign(file_total) != np.sign(total):
        return pd.Series(np.nan, index=v.index), 'opposite signs in the file and the table'
    s = v / total
    s[v == 0] = np.nan           # zero in a group with a national total: a merged cell, value unknown
    return s, ''


def filer_targets(df, group, ht2, items, year, weight=None, trust=False):
    """The filer targets of one year: one row per area, AGI group and item with a target, columns area, group, item, kind,
    share (the IRS's signed share), target (dollars or returns), published (the IRS's value); and the notes on what has no
    target. df, group: the file's filers and their AGI group."""
    w = df['weight'].to_numpy(float) if weight is None else np.asarray(weight, float)
    x = item_vectors(df, items)
    rows, notes = [], []
    for v in items:
        vals = ht2_values(ht2, v)
        if vals is None:
            notes.append(dict(year=year, item=v, group=0, note='not in the table this year'))
            continue
        for g in range(1, 11):
            total = float((w[group == g] * x[v][group == g]).sum())
            s, note = cell_rule(vals.loc[g], total)
            if trust and not note:
                ratio = total / float(vals.loc[g].sum())
                a = np.abs(w * x[v])[(group == g) & (df['billionaire'].to_numpy() != 1)]
                eff = a.sum() ** 2 / (a ** 2).sum() if a.sum() > 0 else 0.0
                if not TRUST[0] <= ratio <= TRUST[1]:
                    s, note = None, f'the file is {ratio:.2f} times the IRS in the group'
                elif eff < MIN_EFFECTIVE:
                    s, note = None, f'{eff:.0f} effective records in the group'
            if note:
                notes.append(dict(year=year, item=v, group=g, note=note))
                continue
            n_merged = int(s.isna().sum())
            if n_merged:
                notes.append(dict(year=year, item=v, group=g, note=f'{n_merged} areas zero (merged cells, no target)'))
            s = s.dropna()
            rows.append(pd.DataFrame({'area': s.index, 'group': g, 'item': v, 'kind': ITEMS[v][2], 'share': s.to_numpy(),
                                      'target': total * s.to_numpy(), 'published': vals.loc[g, s.index].to_numpy()}))
    return pd.concat(rows, ignore_index=True), pd.DataFrame(notes)


# ---- the Forbes list ----------------------------------------------------------------------------------------------
def _forbes_lists():
    """Every list, 2015-2026: uri -> [(list year, state's two letters or '', country)]."""
    out = {}
    names = dict(zip(STATE_NAMES, STATES))
    for y in range(2015, 2027):
        lst = pd.read_csv(FORBES.format(year=y), dtype=str, keep_default_na=False)
        for u, st, c in zip(lst['uri'], lst['residence_state'], lst['residence_country']):
            out.setdefault(u, []).append((y, names.get(st, ''), c))
    return out


_LISTS = {}


def forbes_states(df, year):
    """Each list record's area, by its forbes_uri (a list record's id is its rank on the year's list, so the id does not
    follow a person from year to year; the uri does). The rule: the person's residence_state on the list nearest in time
    that gives one (the year's own list first; ties to the earlier list). A person with no state on any list: if the
    nearest list gives a residence abroad, Other Areas, which is where the IRS counts foreign addresses; otherwise not
    pinned. Returns a Series, index the rows of df that are list records, values the area or '' for not pinned."""
    if not _LISTS:
        _LISTS.update(_forbes_lists())
    rows = np.flatnonzero(df['billionaire'].to_numpy() == 1)
    out = []
    for u in df['forbes_uri'].to_numpy(object)[rows]:
        got = sorted(_LISTS.get(str(u), []), key=lambda r: (abs(r[0] - year), r[0]))
        st = next((r[1] for r in got if r[1]), '')
        if not st and got and got[0][2] not in ('', 'United States'):
            st = 'OA'
        out.append(st)
    return pd.Series(out, index=rows, dtype=object)


# ---- Census adults ------------------------------------------------------------------------------------------------
AGE_BANDS = [(18, 26), (26, 35), (35, 45), (45, 55), (55, 65), (65, 200)]
AGE_BAND_NAMES = ['18-25', '26-34', '35-44', '45-54', '55-64', '65+']


def pep_adults(year):
    """Census resident population estimates, July 1 of the year: a frame, index the 51 states by two letters, one column per
    age band of AGE_BANDS (group quarters included: Census's estimates have them). Files: sc-est2020int (2015-2019) and
    sc-est2024 (2020 on). Rows SEX 0 (both), ORIGIN 0 (total), summed over the six race groups; AGE 85 is 85 and over."""
    f = 'sc-est2020int-alldata6.csv' if year <= 2019 else 'sc-est2024-alldata6.csv'
    c = f'POPESTIMATE{year}'
    p = pd.read_csv(f'{PEP_DIR}/{f}', dtype={'STATE': str}, encoding='latin-1', usecols=['STATE', 'SEX', 'ORIGIN', 'RACE', 'AGE', c])
    p = p[(p['SEX'] == 0) & (p['ORIGIN'] == 0)]
    p['fips'] = p['STATE'].astype(int)
    assert set(STATE_FIPS) <= set(p['fips'])
    p = p[p['fips'].isin(STATE_FIPS)]
    out = pd.DataFrame(index=STATES, columns=AGE_BAND_NAMES, dtype=float)
    for name, (lo, hi) in zip(AGE_BAND_NAMES, AGE_BANDS):
        s = p[(p['AGE'] >= lo) & (p['AGE'] < hi)].groupby('fips')[c].sum()
        out[name] = s.reindex(STATE_FIPS).to_numpy(float)
    assert not out.isna().any().any()
    return out


def filing_adults(ht2):
    """Adults on returns by area, as Iselin's ht2_filing_persons: 2 x joint returns + every other return (a separate return
    or a surviving spouse's is one adult, single and head of household one), summed over AGI groups 1 to 10."""
    d = ht2[(ht2['state'] != 'US') & (ht2['group'] > 0)]
    g = d.groupby('state')[['N1', 'MARS2']].sum()
    return (g['N1'] + g['MARS2']).astype(float)


def nonfiler_state_table(year, ht2):
    """Per state: Census adults, adults on returns (Historic Table 2), and the residual. Residual = Census adults less
    adults on returns; a residual at or below zero is kept and reported, and the target there is a floor (state_weights.py
    says how)."""
    pep = pep_adults(year)
    fa = filing_adults(ht2).reindex(STATES)
    t = pd.DataFrame({'pep_adults': pep.sum(axis=1), 'filing_adults': fa})
    t['residual'] = t['pep_adults'] - t['filing_adults']
    return t, pep


def age_band_national(year):
    """The national residual by age band, Census adults less adults on returns in the band (SOI Table 1.6), from Iselin's
    nonfiler_age_shape_<year>.csv: resources for 2014-2023. Its 65-74 and 75+ rows are one band here."""
    f = os.path.join(REPO, 'research/state_weights/nonfiler_residual/resources', f'nonfiler_age_shape_{year}.csv')
    a = pd.read_csv(f)
    a['band'] = a['band'].replace({'65_74': '65+', '75p': '65+'}).str.replace('_', '-')
    # the file's 65+ rows split one residual by a share: pep_adults and filing_adults are by the split band
    return a.groupby('band')[['pep_adults', 'filing_adults', 'residual_nonfiling_adults']].sum().reindex(AGE_BAND_NAMES)


def rake(seed, row, col, steps=200):
    """Iterative proportional fitting of a positive table to row and column totals with the same sum."""
    t = np.array(seed, float)
    for _ in range(steps):
        t *= (row / t.sum(1))[:, None]
        t *= (col / t.sum(0))[None, :]
    return t


def nonfiler_targets(df_nf, ht2, year, floor=0.02):
    """The non-filers' targets: state x age band, adults. The IRS publishes filing adults by state and (Table 1.6) by age
    band nationally, not by state and band, so the table is built by one rule:
      1. residual(s) = Census adults(s) - adults on returns(s) (Historic Table 2). A residual below floor x Census adults(s)
         is set to that floor (reported in the state table; a state whose returns outnumber its residents has no usable
         residual).
      2. national residual(b) = Census adults(b) - adults on returns(b) (SOI Table 1.6, Iselin's nonfiler_age_shape).
      3. The state x band table starts at Census adults(s, b) x residual(b) / Census adults(b), the band's national
         non-filing rate, and is raked to the shares of 1 and 2.
      4. target(s, b) = the file's non-filer adults in band b (each adult at own age) x the table's share of s within b.
    Worked example, 2019, band 18-25: the national residual is 11.06 million of 35.75 million Census adults, a rate of 0.309;
    a state with 1 million Census adults aged 18-25 starts at 309,000 before raking; if the file has 8 million non-filer
    adults in the band and the raked table gives the state 2.8% of the band, its target is 224,000.
    The file has no group quarters population and Census does (state_weights.py reports the size of that difference).
    Returns target frame (state, band, target, share), the state table, and each record's adults by band (X)."""
    table, pep = nonfiler_state_table(year, ht2)
    table['floored'] = table['residual'] < floor * table['pep_adults']
    use = table['residual'].where(~table['floored'], floor * table['pep_adults']).to_numpy(float)
    nat = age_band_national(year)
    assert (nat['residual_nonfiling_adults'] > 0).all(), nat
    seed = pep.to_numpy(float) * (nat['residual_nonfiling_adults'] / nat['pep_adults']).to_numpy(float)[None, :]
    col = nat['residual_nonfiling_adults'].to_numpy(float)
    col = col / col.sum() * use.sum()
    t = rake(seed, use, col)
    a1, a2 = df_nf['age1'].to_numpy(float), df_nf['age2'].to_numpy(float)
    married = df_nf['married'].to_numpy() == 1
    assert (a1 >= 18).all(), 'a non-filer head under 18'
    X = np.zeros((len(df_nf), len(AGE_BANDS)))
    for k, (lo, hi) in enumerate(AGE_BANDS):
        X[:, k] = ((a1 >= lo) & (a1 < hi)).astype(float) + (married & (a2 >= lo) & (a2 < hi)).astype(float)
    n_outside = (1 + married).sum() - X.sum()
    assert n_outside == 0, f'{n_outside} non-filer adults outside the age bands'
    w = df_nf['weight'].to_numpy(float)
    file_b = (w[:, None] * X).sum(0)
    share = t / t.sum(0, keepdims=True)
    tgt = pd.DataFrame(share * file_b[None, :], index=STATES, columns=AGE_BAND_NAMES).stack().rename('target').reset_index()
    tgt.columns = ['state', 'band', 'target']
    tgt['share'] = (pd.DataFrame(share, index=STATES, columns=AGE_BAND_NAMES).stack()).to_numpy()
    table['file_share_target'] = (share * file_b[None, :]).sum(1) / file_b.sum()
    table['residual_share'] = use / use.sum()
    return tgt, table, X
