"""The billionaires.

Above $1 billion of net worth the file is the Forbes list alone; below it, the SCF (a labelled assumption, John's
choice: Forbes covers the people above $1 billion better than a handful of heavily weighted SCF households). Two pieces:

1. redraw_balance_sheets: every generated record whose net worth, in nominal target-year dollars, comes
   out at $1 billion or more has its whole SCF block drawn again (every SCF-only slot: assets, debts, the home's loan
   share, accumulated gains, ownership flags and industries, elective deferrals, auto loan interest, the QBI workers),
   under the SCF label given everything else on the record, as generation drew it (generate.Chunk.survey_step, the
   record's own SCF wave, its model, the consistency rules with that model's offset factors). Each record gets a fixed
   number of tries (settings tilt.top.redraw_tries) whose random numbers come from
   generators seeded by the record's id alone (the slot order, the draws, and the stored picks that turn bands into
   dollars), so a record's tries never depend on which other records are redrawn. The first try below $1 billion is
   kept; if every try lands at $1 billion or more, the try with the lowest net worth is kept and the record flagged
   (wealth_redraw_fallback). The test is on grown dollars (dollars.draw_dollars with the target year's maps).
   Then net worth, the mortgage, the retirement accounts' stocks and the deferral items are made again from the new
   dollars; nothing else on the record changes.

2. build_billionaires: the Forbes list as tax units, weight 1, flag billionaire = 1.
   - The list (resources/top/forbes_billionaires_<year>.csv; settings tilt.top.billionaire_lists): one dated list.
     Spouses listed separately become one joint return (their fortunes added); a family fortune or a couple named in
     one entry stays one record. Units are ranked by their net worth.
   - Rank groups, BSYZ's (Balkir, Saez, Yagan and Zucman 2025, Table 1; the IRS study's Table 1 behind it): the
     top 92 units ("top 100", the top 0.00005% of tax units), the next 276 ("next 300", the next 0.00015%), and the
     rest, which take the next group's figures (an extrapolation).
   - Wealth: net worth is Forbes's. Public holdings are the person's public share (Forbes's tracked share holdings over
     net worth, from production's later 2025 profiles: not dated 2019) times their 2019 net worth; where it is
     unknown, the rank group's wealth-weighted average share among people with a known one (not assumed private).
     The rest is private: real estate (value.re_fund) when Forbes's industry is real estate, otherwise a private business
     (value.pass_throughs). No home, no debt (net worth right, gross assets understated). Basis: zero for self-made
     fortunes (Forbes's 2019 self_made flag), equal to value for inherited ones, written as accumulated gains
     (w_kg_other on public stock, w_kg_pass_throughs, w_kg_other_re) so the non-microdata step's cost_basis gives it.
   - Income: each rank group's dollars of each kind = the group's Forbes wealth x the IRS study's
     amount per dollar of wealth (= wealth x BSYZ's fiscal-income ratio x BSYZ's share, on the underlying amounts),
     kinds: capital gains, dividends, taxable interest, tax-exempt interest, business profits, business losses,
     rental and royalty profits, rental losses, estate and trust income, other income (net operating losses left
     out), wages and pensions. Each kind is shared among the group's units by their holdings of the matching asset:
     gains and dividends by public stock; business profits, and separately business losses, by private holdings;
     the rest by total wealth. Group totals hold exactly; a unit's income is not wealth x the ratio.
   - Tax items: within each kind, the split follows the item mix of the top group (item_mix: Part A's
     rebuilt $100 million-plus returns when present, else the generated records with AGI of $100 million or more,
     a stand-in), except where the IRS study gives the split: partnership against S corporation profits and losses.
   - Charity: gifts of 0.7% of wealth in the top group, 0.6% in the rest (BSYZ Table 2), cash and other
     gifts as in the mix; the calculator computes the deduction.
   - People: age from Forbes's 2019 age (held to 80, the file's top age), sex from Forbes, marital status
     from the later profiles (married, or "widowed, remarried"; missing: married, production's rule); a spouse of the
     same age and the other sex (a separately listed spouse keeps their own age and sex). Dependents by production's
     rule (only under 45, at most three, ages 12, 9, 6, from the later profiles' children). Earnings all to the primary
     filer. Wages and pensions are shared among wages, taxable pensions and taxable IRA distributions as in the mix;
     the non-taxable rest of pensions is zero, so gross pensions are the taxable pensions and IRA distributions are not
     in them; with no Social Security, taxable Social Security computes to zero.
     Items beyond the kinds (state income tax, foreign tax credits, business credits, self-employment items,
     the other itemized deductions in RATIO_ITEMS) at their ratio to AGI in the mix; consumption at the mix's average;
     everything else zero, listed in ZERO_ITEMS. Ids from 95,000,000 (ID_BASE) up, by rank (Part A's from 90,000,000).
"""
import os

import numpy as np
import pandas as pd

from shared import common
from shared import dollars as D

S = common.SETTINGS['tilt']['top']
RES = os.path.join(common.RESOURCES, 'top')
ID_BASE = 95_000_000             # Part A's rebuilt returns take 90,000,000 up (top_hidden id_block)
TOP_AGE = 80                      # the file's top age (age1, age2): 80 means 80 and over
GROUPS = [('top', 92), ('next', 276)]   # BSYZ's top 0.00005% and next 0.00015% of tax units (IRS study units)

# kinds of income: (IRS study item(s), the holdings that share it out)
KINDS = {'gains': ('public'), 'dividends': ('public'), 'txbl_int': ('total'), 'exempt': ('total'),
         'business profits': ('private'), 'business losses': ('private'), 'rent profits': ('total'),
         'rent losses': ('total'), 'estate': ('total'), 'other income': ('total'), 'wages and pensions': ('total')}
# items beyond the kinds, at their ratio to AGI in the mix
RATIO_ITEMS = ['salt_inc_sales', 'salt_prop', 'ftc', 'amt_ftc', 'gbc', 'prior_yr_cred', 'other_item_exp',
               'other_misc_exp', 'tax_prep_exp', 'part_se', 'se_tax_ded', 'se_health', 'keogh_contr']
# items set to zero, with the reason (for the report)
ZERO_ITEMS = {
    'gross_ss': 'not market income; capped per person, so not scaled with income',
    'nontxbl_pens_dist': 'the IRS study\'s pensions are the taxable amounts on the return',
    'ui': 'none', 'alimony': 'none', 'alimony_exp': 'none',
    'trad_contr_ira': 'capped, negligible', 'sl_int_ded': 'phased out', 'ed_exp': 'phased out',
    'tuition_ded': 'phased out', 'hsa_contr': 'capped, negligible', 'early_penalty': 'none', 'med_exp': 'none',
    'casualty_exp': 'none', 'job_exp': 'none', 'first_mort_int': 'no home, no debt',
    'aoc_exp': 'phased out', 'llc_exp': 'phased out', 'old_cred': 'none', 'savers_cred': 'phased out',
    'res_energy_cred': 'none', 'other_nonref': 'none', 'net_ptc': 'none', 'excess_ptc': 'none',
    'recapture_tax': 'none', 'ira_penalty': 'none', 'amt_nols': 'net operating losses left out',
    'dpad': 'repealed after 2017', 'farm_rent': 'none', 'sche_other': 'none', 'care_exp': 'none',
    'auto_int_exp': 'no debt', 'elective_deferrals': 'none', 'tips_primary': 'none', 'tips_spouse': 'none',
    'ot_primary': 'none', 'ot_spouse': 'none'}


# ---- the list as tax units ------------------------------------------------------------------------------------
def wealth_parts(p):
    """Each person's net worth in three parts (module docstring): public holdings = the public share (known,
    else the rank group's wealth-weighted average among people with a known share) x net worth; the rest private, real
    estate (private_re) when Forbes's industry is real estate, else a private business (private_pt). p: the people
    table with group, net_worth, public_share and real_estate. Returns p with public_share_used, public, private,
    private_re, private_pt, and the mask of people whose share is known."""
    p = p.copy()
    known = p['public_share'].notna()
    avg = (p[known].assign(x=lambda d: d.public_share * d.net_worth).groupby('group')['x'].sum()
           / p[known].groupby('group')['net_worth'].sum())
    p['public_share_used'] = p['public_share'].fillna(p['group'].map(avg))
    assert p['public_share_used'].notna().all(), 'a rank group with no known public share'
    p['public'] = p['public_share_used'] * p['net_worth']
    p['private'] = p['net_worth'] - p['public']
    p['private_re'] = np.where(p['real_estate'], p['private'], 0.0)
    p['private_pt'] = p['private'] - p['private_re']
    return p, known


def last_list_year():
    """The latest year with a Forbes list in settings tilt.top.billionaire_lists, or None."""
    years = [int(y) for y in S['billionaire_lists']]
    return max(years) if years else None


def list_for_year(year):
    """The billionaires' list for tax year year and how it is used: ('list', the year's own list, year) when settings
    tilt.top.billionaire_lists has one (the records then have weight 1, held fixed); ('grown', the last list grown to
    the year by grow_list, the last list's year) for a year after the last list (the records then start at weight 1
    times their cell's population growth and are held fixed, top_records.add_list); (None, None, None) otherwise."""
    lst = read_list(year)
    if lst is not None:
        return 'list', lst, year
    last = last_list_year()
    if last is not None and year > last:
        return 'grown', grow_list(read_list(last), last, year), last
    return None, None, None


def growth_factors(s, t):
    """The growth per return from s to t of the three kinds of holding the billionaires have: public stock as
    value.equities and a private business as value.pass_throughs, private real estate as value.re_fund, which keeps
    growth.yaml's rule (the SCF's mean per tax unit, wave of income year t - 1 over that of s - 1, after the last wave
    by the Financial Accounts per person: maps.scf_growth). The first two are the items table's series for the column
    (resources/projection_series_current.csv through 2035, then nominal GDP) over the growth in
    the number of returns (projection_growth.returns_growth: CBO's through settings tilt.projection.cbo_last_year, then
    adults with a couple counted once). No add factors apply
    (none may be set on these two columns). Returns {part: (factor, source)}."""
    from prep import build_table as BT
    from scale import maps as MP
    from scale import projection_growth as PG
    from scale import projection_steps as PS
    assert 2024 <= s < t, f'billionaires grown from {s} to {t}: only forward from a list year after 2023'
    cols = {'public': 'value.equities', 'private_pt': 'value.pass_throughs', 'private_re': 'value.re_fund'}
    tab, add = PS.items_table(), PS.add_factors_table()
    R = PG.returns_growth(s, t)
    out, scf = {}, None
    for k, c in cols.items():
        if k != 'private_re':
            spec = tab.loc[c, 'series']
            assert spec.startswith('file:') and tab.loc[c, 'kind'] == 'total', f'{c}: the items table row is {spec}, not a file series of kind total'
            assert not (add['column'] == c).any(), f'{c}: add factors on a column the billionaires are grown by'
            g, what = PS.series_growth(spec, s, t)
            out[k] = (g / R, f'{c}: {what} (growth in total) over the growth in returns {R:.5f}')
            continue
        if scf is None:   # as grow_dollars: the SCF rows of the training table
            scf = pd.read_parquet(common.TABLE, columns=['source', 'year', 'weight'] + BT.SCF_AMOUNTS)
            scf = scf[scf.source == 'scf']
        f = MP.scf_growth(scf, s - 1, t - 1)[c]
        assert np.isfinite(f) and f > 0, f'{c}: no SCF growth factor {s} to {t}'
        out[k] = (float(f), f'{c}: SCF mean per tax unit, wave of {t - 1} over wave of {s - 1} (growth.yaml, as grow_dollars)')
    return out


def grow_list(lst, s, t, factor=None):
    """The year-s list carried to year t (a year after the last list): each person's net worth
    split into public stock, private business and private real estate as build_billionaires splits it (wealth_parts,
    on the year-s ranking), each part grown by growth_factors(s, t), or, with factor (a pair (factor, source)),
    every part by that one factor, net worth their
    sum, and the public share made
    the grown public part over the grown net worth (so build_billionaires splits the grown fortune into the same
    grown parts); ages raised by t - s (a missing age stays missing). Spouses, couples, families and every other column
    stay as on the year-s list. Income is not carried: build_billionaires computes it from the grown wealth by the IRS
    study's ratios, as in a list year."""
    units, people = tax_units(lst)
    p = people_attributes(people)
    p, _ = wealth_parts(p)
    f = growth_factors(s, t) if factor is None else {k: factor for k in ('public', 'private_pt', 'private_re')}
    grown = {k: p[k].to_numpy(float) * f[k][0] for k in f}
    nw = grown['public'] + grown['private_pt'] + grown['private_re']
    by_uri = pd.DataFrame({'uri': p['uri'].to_numpy(), 'nw': nw, 'share': grown['public'] / nw}).set_index('uri')
    out = lst.copy()
    assert out['uri'].isin(by_uri.index).all()
    out['net_worth_list'] = out['net_worth']
    out['net_worth'] = np.round(out['uri'].map(by_uri['nw']).to_numpy(float))
    out['public_share_later'] = out['uri'].map(by_uri['share']).to_numpy(float)
    age = pd.to_numeric(out['age'], errors='coerce')
    out['age'] = (age + (t - s)).astype('Int64')
    out.attrs['growth'] = {k: v for k, v in f.items()}
    return out


def read_list(year):
    """The year's Forbes list (settings tilt.top.billionaire_lists: {year: file in resources/top}), or None."""
    name = S['billionaire_lists'].get(str(year))
    if not name:
        return None
    lst = pd.read_csv(os.path.join(RES, name), keep_default_na=False, na_values={'net_worth': [''], 'age': ['']})
    assert lst['uri'].is_unique and (lst['net_worth'] >= 1e9).all(), f'{name}: duplicate entries or one below $1 billion'
    return lst


def tax_units(lst):
    """The list's entries as tax units: spouses listed separately joined into one joint return (net worth added, the
    richer as the primary filer); every other entry one unit. Each person keeps a row in the people table, with the
    unit's id. Returns (units, people), units ranked by net worth (ties by Forbes rank, then uri)."""
    p = lst.copy()
    p['person'] = 1
    pair = p['joint_with'] != ''
    assert set(p.loc[pair, 'joint_with']) <= set(p['uri']), 'a spouse not on the list'
    # the primary filer of a pair: the richer (ties: the uri first in order)
    other = p.set_index('uri')['net_worth']
    richer = np.where(pair, (p['net_worth'] > p['joint_with'].map(other).fillna(-1)) |
                      ((p['net_worth'] == p['joint_with'].map(other).fillna(-1)) & (p['uri'] < p['joint_with'])), True)
    p['unit_uri'] = np.where(richer, p['uri'], p['joint_with'])
    p.loc[~richer, 'person'] = 2
    u = p.groupby('unit_uri', sort=False).agg(net_worth=('net_worth', 'sum'), forbes_rank=('forbes_rank', 'min'),
                                             people=('uri', 'size')).reset_index()
    assert (u['people'] <= 2).all()
    u = u.sort_values(['net_worth', 'forbes_rank', 'unit_uri'], ascending=[False, True, True], kind='stable')
    u['unit_rank'] = np.arange(1, len(u) + 1)
    edges = np.cumsum([n for _, n in GROUPS])
    u['group'] = np.select([u['unit_rank'] <= edges[0], u['unit_rank'] <= edges[1]], [GROUPS[0][0], GROUPS[1][0]],
                           'rest')
    p = p.merge(u[['unit_uri', 'unit_rank', 'group']], on='unit_uri', how='left', validate='many_to_one')
    return u.reset_index(drop=True), p


# ---- the IRS study's amounts per rank group ------------------------------------------------------------------
def bsyz_groups():
    """Per rank group, per tax unit, in dollars: wealth, fiscal income and the kinds (KINDS), from the IRS study's
    Table 1 (resources/top/irs_wealth_groups_2018_2020.csv). Top group: the top 0.00005% column. Next group: (4 x top
    0.0002% - top 0.00005%) / 3, the percentile widths (BSYZ Table 1's next-300 column). Rest: the next group's
    (an extrapolation). Kinds add up to fiscal income exactly: business losses = net business income less positive
    business income; rental profits = net rental income plus royalties less rental losses."""
    t = pd.read_csv(os.path.join(RES, 'irs_wealth_groups_2018_2020.csv')).set_index('item')
    cols = {'top': t['top_00005pct'] * 1e3, 'next': (4 * t['top_0002pct'] - t['top_00005pct']) / 3 * 1e3}
    out = {}
    for g, a in cols.items():
        k = {'gains': a['realized_capital_gains'], 'dividends': a['dividends'], 'txbl_int': a['txbl_int'],
             'exempt': a['exempt_int'], 'business profits': a['positive_business_income'],
             'business losses': a['business_income'] - a['positive_business_income'],
             'rent losses': a['negative_rental_income'],
             'rent profits': a['rental_income'] + a['royalties'] - a['negative_rental_income'],
             'estate': a['estate_and_trust_income'], 'other income': a['other_income_excl_nols'],
             'wages and pensions': a['wages_and_pensions']}
        fiscal = (a['wages_and_pensions'] + a['realized_capital_gains'] + a['dividends'] + a['txbl_int']
                  + a['exempt_int'] + a['rental_income'] + a['estate_and_trust_income'] + a['royalties']
                  + a['business_income'] + a['other_income_excl_nols'])
        assert abs(sum(k.values()) - fiscal) < 1e-6 * abs(fiscal)
        out[g] = {'wealth': a['wealth'], 'fiscal': fiscal, 'kinds': k, 'agi': a['agi'],
                  'partnership profits': a['positive_partnership_income'], 'scorp profits': a['positive_scorp_income'],
                  'partnership losses': -a['negative_partnership_income'], 'scorp losses': -a['negative_scorp_income'],
                  'gifts': a['current_charitable_contributions'], 'gifts deducted': a['charitable_contributions_deducted'],
                  'federal income and payroll tax': a['federal_individual_income_and_fica_taxes'],
                  'units': a['tax_units']}
    out['rest'] = out['next']
    return out


# ---- the item mix of the top group ---------------------------------------------------------------------------
def _share(num, den, fallback):
    return float(num / den) if den > 0 else fallback


def item_mix(df, w, notes):
    """The split of each kind into tax items, the ratios of the other items to AGI, gifts' cash share, workers per
    dollar and industry by kind of business, consumption and work, from the mix group: Part A's rebuilt returns of
    $100 million or more of AGI where present (synthetic_top = 1), else the
    generated records with AGI of at least settings tilt.top.stand_in_agi (target-year dollars), or the top 0.01% of
    filers by AGI if fewer than stand_in_min_records. Weighted by w. Returns the mix and the group's description."""
    agi = D.agi(df)
    filer = df['filer'].to_numpy() == 1
    hidden = df['synthetic_top'].to_numpy() == 1
    bil = df['billionaire'].to_numpy() == 1
    line = float(S['stand_in_agi'])
    if hidden.any():
        g = hidden & (df['hidden_group'].to_numpy() == 3)
        what = (f"Part A's rebuilt returns of the over-$100-million aggregate record (hidden_group 3; {int(g.sum())} "
                f"records, in target-year dollars, weighted by their starting weights)")
    else:
        g = filer & ~bil & ~hidden & (agi >= line)
        what = (f"STAND-IN: the generated filers with AGI of ${line / 1e6:,.0f} million or more in target-year dollars "
                f"({int(g.sum())} records; weighted by the generation weights)")
        if g.sum() < int(S['stand_in_min_records']):
            o = np.argsort(-agi[filer & ~bil & ~hidden], kind='stable')
            idx = np.flatnonzero(filer & ~bil & ~hidden)[o]
            cum = np.cumsum(w[idx])
            g = np.zeros(len(df), bool)
            g[idx[cum <= 1e-4 * w[filer].sum()]] = True
            what = f'STAND-IN: the top 0.01% of generated filers by AGI ({int(g.sum())} records)'
    assert g.sum() >= 10, f'the item mix group has {int(g.sum())} records'
    x = df.loc[g]
    ww = w[g]
    tot = lambda c: float((ww * x[c].to_numpy(float)).sum())
    pos = lambda c: float((ww * np.maximum(x[c].to_numpy(float), 0)).sum())
    neg = lambda c: float((ww * np.maximum(-x[c].to_numpy(float), 0)).sum())
    m = {}
    line13 = sum(tot(c) for c in D.KG_PIECES)
    m['gains'] = {c: _share(tot(c), line13, 1.0 if c == 'kg_lt' else 0.0) for c in D.KG_PIECES}
    m['kg_1250 per kg_lt'] = _share(tot('kg_1250'), pos('kg_lt'), 0.0)
    m['kg_collect per kg_lt'] = _share(tot('kg_collect'), pos('kg_lt'), 0.0)
    m['div_pref share'] = _share(tot('div_pref'), tot('div_pref') + tot('div_ord'), 1.0)
    wp = {c: pos(c) for c in ['wages', 'txbl_pens_dist', 'txbl_ira_dist']}
    m['wages and pensions'] = {c: _share(v, sum(wp.values()), 1.0 if c == 'wages' else 0.0) for c, v in wp.items()}
    # business profits and losses: the Schedule C and farm part from the mix, partnership against S corporation from
    # the IRS study (build_billionaires), and within each the mix's split
    prof = {'part_passive': tot('part_passive'), 'part_active': tot('part_active'),
            'scorp_passive': tot('scorp_passive'), 'scorp_active': tot('scorp_active'),
            'sole_prop': pos('sole_prop'), 'farm': pos('farm')}
    loss = {'part_passive_loss': tot('part_passive_loss'), 'part_active_loss': tot('part_active_loss'),
            'part_179': tot('part_179'), 'scorp_passive_loss': tot('scorp_passive_loss'),
            'scorp_active_loss': tot('scorp_active_loss'), 'scorp_179': tot('scorp_179'),
            'sole_prop': neg('sole_prop'), 'farm': neg('farm')}
    m['schedule C and farm share of profits'] = _share(prof['sole_prop'] + prof['farm'], sum(prof.values()), 0.0)
    m['schedule C and farm share of losses'] = _share(loss['sole_prop'] + loss['farm'], sum(loss.values()), 0.0)
    # the share of the group's returns with a Schedule C or farm profit, and with a loss (weighted): only that share of
    # the billionaires holds one (build_billionaires)
    cf = x['sole_prop'].to_numpy(float) + x['farm'].to_numpy(float)
    m['holders of schedule C and farm profits'] = float(ww[cf > 0].sum() / ww.sum())
    m['holders of schedule C and farm losses'] = float(ww[cf < 0].sum() / ww.sum())
    part = lambda d, keys, first: {k: _share(d[k], sum(d[q] for q in keys), 1.0 if k == first else 0.0) for k in keys}
    m['partnership profits'] = part(prof, ['part_passive', 'part_active'], 'part_active')
    m['scorp profits'] = part(prof, ['scorp_passive', 'scorp_active'], 'scorp_active')
    m['schedule C and farm profits'] = part(prof, ['sole_prop', 'farm'], 'sole_prop')
    m['partnership losses'] = part(loss, ['part_passive_loss', 'part_active_loss', 'part_179'], 'part_active_loss')
    m['scorp losses'] = part(loss, ['scorp_passive_loss', 'scorp_active_loss', 'scorp_179'], 'scorp_active_loss')
    m['schedule C and farm losses'] = part(loss, ['sole_prop', 'farm'], 'sole_prop')
    oth = {c: pos(c) for c in ['other_gains', 'state_ref', 'other_agi_remainder']}
    m['other income'] = {c: _share(v, sum(oth.values()), 1.0 if c == 'other_agi_remainder' else 0.0)
                         for c, v in oth.items()}
    m['char_cash share'] = _share(tot('char_cash'), tot('char_cash') + tot('char_noncash'), 0.5)
    agi_pos = float((ww * np.maximum(agi[g], 0)).sum())
    m['ratio to AGI'] = {c: tot(c) / agi_pos for c in RATIO_ITEMS}
    # QBI: each group's outside workers per dollar of the group's positive income, and its industry with the most income
    kinds = {'cf': {'sole_prop': 1, 'farm': 1},
             'ps': {'part_passive': 1, 'part_active': 1, 'scorp_passive': 1, 'scorp_active': 1}}
    m['workers per dollar'], m['industry'] = {}, {}
    for k, parts in kinds.items():
        inc = np.maximum(sum(s * x[p].to_numpy(float) for p, s in parts.items()), 0)
        m['workers per dollar'][k] = _share((ww * x[f'biz_workers_{k}'].to_numpy(float)).sum(), (ww * inc).sum(), 0.0)
        ind = x[f'biz_industry_{k}'].to_numpy()
        by = pd.Series(ww * inc).groupby(ind).sum().drop(index=0, errors='ignore')
        m['industry'][k] = int(by.idxmax()) if len(by) and by.max() > 0 else 8
    m['consumption'] = {c: tot(c) / ww.sum() for c in D.META['cex_amounts']}
    earn = x['wages'].to_numpy(float) > 0
    med = lambda c: float(np.round(common.wquantile(x[c].to_numpy(float)[earn], ww[earn], [0.5])[0])) if earn.any() else 0.0
    m['weeks_primary'], m['hours_primary'] = med('weeks_primary'), med('hours_primary')
    notes.append(f'item mix: {what}')
    return m, what


# ---- the records ---------------------------------------------------------------------------------------------
def people_attributes(people):
    """Per person: age (held to 18-80; missing: 65, production's rule), sex (1 man, 2 woman; missing: man), married
    (from the later profiles; missing: married), children, public share (NaN unknown), real estate, self-made."""
    p = people.copy()
    age = pd.to_numeric(p['age'], errors='coerce')
    p['age_used'] = np.clip(np.where(age.isna() | (age < 18) | (age > 100), 65, age), 18, TOP_AGE).astype(int)
    p['sex'] = np.where(p['gender'].astype(str).str.upper() == 'F', 2, 1)
    ms = p['marital_status_later'].astype(str).str.lower()
    p['married_later'] = (ms == '') | ms.str.contains('married')   # "married", "widowed, remarried"; missing: married
    p['married_later'] &= ~ms.str.contains('^divorced|^widowed$|^single|^separated|^engaged|^in relationship', regex=True)
    p['children'] = pd.to_numeric(p['children_later'], errors='coerce').fillna(0).astype(int)
    p['public_share'] = pd.to_numeric(p['public_share_later'], errors='coerce')
    p['real_estate'] = p['industries'].astype(str).str.contains('Real Estate', case=False)
    p['founder'] = p['self_made'].astype(str).str.lower() == 'true'
    return p


def build_billionaires(lst, df, w, year, notes):
    """The list as records in the file's schema (columns of df; see the module docstring), weight 1, and a table of
    the units for the report. w: the start's weights of df, for the item mix."""
    units, people = tax_units(lst)
    p = people_attributes(people)
    groups = bsyz_groups()
    p, known = wealth_parts(p)
    for c in ['public', 'private_re', 'private_pt']:
        p[f'gain_{c}'] = np.where(p['founder'], p[c], 0.0)
    agg = p.groupby('unit_uri').agg(public=('public', 'sum'), private=('private', 'sum'), private_re=('private_re', 'sum'),
                                    private_pt=('private_pt', 'sum'), gain_public=('gain_public', 'sum'),
                                    gain_private_re=('gain_private_re', 'sum'), gain_private_pt=('gain_private_pt', 'sum'))
    u = units.merge(agg, left_on='unit_uri', right_index=True, how='left', validate='one_to_one')
    assert np.allclose(u['public'] + u['private'], u['net_worth'], rtol=1e-12)
    # the people: primary (person 1) and spouse
    prim = p[p.person == 1].set_index('unit_uri')
    spou = p[p.person == 2].set_index('unit_uri')
    u = u.join(prim[['uri', 'name', 'age_used', 'sex', 'married_later', 'children', 'couple_entry', 'family_entry']],
               on='unit_uri')
    u = u.join(spou[['uri', 'age_used', 'sex']].add_prefix('sp_'), on='unit_uri')
    u['joint_pair'] = u['sp_uri'].notna()
    u['married'] = (u['joint_pair'] | (u['couple_entry'].astype(str) == 'True') | u['married_later']).astype(int)
    u['age2'] = np.where(u['married'] == 1, np.where(u['joint_pair'], u['sp_age_used'], u['age_used']), -1).astype(int)
    u['sex_spouse'] = np.where(u['married'] == 1, np.where(u['joint_pair'], u['sp_sex'], 3 - u['sex']), 0).astype(int)
    u['n_dep'] = np.where(u['age_used'] < 45, np.minimum(u['children'], 3), 0).astype(int)

    # income: each group's kinds, shared out by holdings
    key = {'public': u['public'].to_numpy(float), 'private': u['private'].to_numpy(float),
           'total': u['net_worth'].to_numpy(float)}
    kinds = {k: np.zeros(len(u)) for k in KINDS}
    group_rows = []
    for g, prm in groups.items():
        m = (u['group'] == g).to_numpy()
        if not m.any():
            continue
        Wg = float(u.loc[m, 'net_worth'].sum())
        for k, by in KINDS.items():
            dollars = Wg * prm['kinds'][k] / prm['wealth']
            kk = key[by][m]
            assert kk.sum() > 0, f'group {g}: no {by} holdings to share {k} by'
            kinds[k][m] = dollars * kk / kk.sum()
        group_rows.append({'group': g, 'units': int(m.sum()), 'wealth ($ billion)': Wg / 1e9,
                           'fiscal income ($ billion)': Wg * prm['fiscal'] / prm['wealth'] / 1e9,
                           'ratio of fiscal income to wealth (%)': 100 * prm['fiscal'] / prm['wealth']})
    mix, what = item_mix(df, w, notes)
    out = pd.DataFrame(index=range(len(u)))
    # every column of the file, at a default: band -1, stored pick missing, numbers 0
    for c in df.columns:
        if c.endswith('_band'):
            out[c] = -1
        elif c.startswith('u_'):
            out[c] = np.nan
        elif pd.api.types.is_bool_dtype(df[c]):
            out[c] = True
        elif pd.api.types.is_numeric_dtype(df[c]):
            out[c] = 0
        else:
            out[c] = ''
    put = lambda c, v: out.__setitem__(c, v)
    put('source', 'puf'); put('year', common.PUF_YEAR); put('filer', 1); put('stratifier', 'forbes'); put('model', 0)
    put('weight', 1.0); put('agi_group', -1); put('q_group', -1); put('label_ok', True)
    put('record_id', ID_BASE + u['unit_rank'].to_numpy())
    put('scf_wave', year); put('scf_year', year - 1)
    put('hidden_group', -1)           # Part A's group label: none (-1) on every record not rebuilt by Part A
    for src in ['cex', 'cps', 'sipp']:
        put(f'{src}_year', year)
    # people
    put('married', u['married'].to_numpy())
    put('n_dep', u['n_dep'].to_numpy())
    put('filing_status', np.where(u['married'] == 1, 2, np.where(u['n_dep'] > 0, 4, 1)))
    put('n_dep_ctc', u['n_dep'].to_numpy()); put('n_dep_eitc', 0)
    age = u['age_used'].to_numpy()
    put('age1', age)
    put('age_range', np.searchsorted([26, 35, 45, 55, 65], age, side='right'))
    put('age2', u['age2'].to_numpy())
    put('sex_primary', u['sex'].to_numpy()); put('sex_spouse', u['sex_spouse'].to_numpy())
    for k, a in enumerate([12, 9, 6], start=1):
        has = u['n_dep'].to_numpy() >= k
        put(f'dep_age{k}', np.where(has, a, -1))
        put(f'ssn_status_dep{k}', np.where(has, 1, 0))
        put(f'dep_ssn{k}', np.where(has, 1.0, np.nan))
    put('ssn_status_primary', 1); put('ssn_status_spouse', np.where(u['married'] == 1, 1, 0))
    put('ssn1', 1.0)
    put('ssn2', np.where(u['married'] == 1, 1.0, np.nan))
    # income items
    g = mix['gains']
    for c in D.KG_PIECES:
        put(c, kinds['gains'] * g[c])
    put('kg_1250', np.maximum(out['kg_lt'], 0) * mix['kg_1250 per kg_lt'])
    put('kg_collect', np.maximum(out['kg_lt'], 0) * mix['kg_collect per kg_lt'])
    put('div_pref', kinds['dividends'] * mix['div_pref share'])
    put('div_ord', kinds['dividends'] * (1 - mix['div_pref share']))
    put('txbl_int', kinds['txbl_int']); put('exempt_int', kinds['exempt'])
    for c, s in mix['wages and pensions'].items():
        put(c, kinds['wages and pensions'] * s)
    # pensions are taxable pensions alone (the non-taxable rest is zero, ZERO_ITEMS); gross pensions
    # and taxable Social Security are computed by recombine below. IRA distributions stay in txbl_ira_dist and are not
    # pensions
    # business: partnership against S corporation from the IRS study (by group), Schedule C and farm from the mix
    grp = u['group'].to_numpy()
    irs = lambda k: np.array([groups[x][k] for x in grp])
    pr, ls = kinds['business profits'], -kinds['business losses']        # both 0 or more
    assert (pr >= 0).all() and (ls >= 0).all()
    cf_p, cf_l = mix['schedule C and farm share of profits'], mix['schedule C and farm share of losses']
    part_p = irs('partnership profits') / (irs('partnership profits') + irs('scorp profits'))
    part_l = irs('partnership losses') / (irs('partnership losses') + irs('scorp losses'))
    for c, s in mix['partnership profits'].items():
        put(c, pr * (1 - cf_p) * part_p * s)
    for c, s in mix['scorp profits'].items():
        put(c, pr * (1 - cf_p) * (1 - part_p) * s)
    for c, s in mix['partnership losses'].items():
        put(c, ls * (1 - cf_l) * part_l * s)
    for c, s in mix['scorp losses'].items():
        put(c, ls * (1 - cf_l) * (1 - part_l) * s)
    # Schedule C and farm: every unit with private holdings would otherwise hold
    # a small Schedule C profit and loss, and the list alone then exceeded Table 4.3's count of top-0.001% returns with
    # a Schedule C or F profit. So, in each rank group, the group's Schedule C and farm profits go to
    # the mix group's share of units holding them (units chosen by a draw keyed by record id), shared among those by
    # private holdings; losses the same, with their own share and draw. Group dollars are unchanged.
    from shared import ages
    rid = ID_BASE + u['unit_rank'].to_numpy()
    privk = u['private'].to_numpy(float)
    cf_amount = {}
    for side, total, share_key, salt in [('profits', pr * cf_p, 'holders of schedule C and farm profits', 811),
                                         ('losses', ls * cf_l, 'holders of schedule C and farm losses', 812)]:
        hold = ages.uniform_by_id(rid, salt) < mix[share_key]
        amt = np.zeros(len(u))
        for gname in np.unique(grp):
            m_ = grp == gname
            h_ = m_ & hold & (privk > 0)
            if not h_.any():   # none drawn: the unit with the largest private holdings
                h_ = m_ & (privk == privk[m_].max())
            amt[h_] = total[m_].sum() * privk[h_] / privk[h_].sum()
        cf_amount[side] = amt
    for c in ['sole_prop', 'farm']:
        put(c, cf_amount['profits'] * mix['schedule C and farm profits'][c]
            - cf_amount['losses'] * mix['schedule C and farm losses'][c])
    put('rent', kinds['rent profits']); put('rent_loss', -kinds['rent losses'])
    put('estate', np.maximum(kinds['estate'], 0)); put('estate_loss', np.maximum(-kinds['estate'], 0))
    for c, s in mix['other income'].items():
        put(c, kinds['other income'] * s)
    assert (out['rent_loss'] >= 0).all()
    # the computed business total, as drawn (no draw here)
    parts = D.META['computed_totals']['business_income']
    put('business_income_drawn', D.signed_sum(out, parts))
    # charity
    rate = np.where(grp == 'top', float(S['gift_rate_top']), float(S['gift_rate_rest']))
    gifts = rate * u['net_worth'].to_numpy(float)
    put('char_cash', gifts * mix['char_cash share']); put('char_noncash', gifts * (1 - mix['char_cash share']))
    # the balance sheet and basis as accumulated gains
    put('value.equities', u['public'].to_numpy(float)); put('value.pass_throughs', u['private_pt'].to_numpy(float))
    put('value.re_fund', u['private_re'].to_numpy(float))
    put('w_kg_other', u['gain_public'].to_numpy(float)); put('w_kg_pass_throughs', u['gain_private_pt'].to_numpy(float))
    put('w_kg_other_re', u['gain_private_re'].to_numpy(float))
    put('owns_business', (u['private_pt'] > 0).astype(int).to_numpy())
    # QBI groups: an industry where the group has income, workers per dollar of its profits
    kp = {'cf': np.maximum(out['sole_prop'] + out['farm'], 0),
          'ps': out['part_passive'] + out['part_active'] + out['scorp_passive'] + out['scorp_active']}
    anyk = {'cf': (out[['sole_prop', 'farm']] != 0).any(axis=1),
            'ps': (out[['part_passive', 'part_active', 'part_passive_loss', 'part_active_loss', 'part_179',
                        'scorp_passive', 'scorp_active', 'scorp_passive_loss', 'scorp_active_loss', 'scorp_179']] != 0).any(axis=1)}
    for k in kp:
        put(f'biz_industry_{k}', np.where(anyk[k], mix['industry'][k], 0))
        put(f'biz_workers_{k}', np.where(anyk[k], kp[k] * mix['workers per dollar'][k], 0.0))
    put('k1_owner', anyk['ps'].astype(int).to_numpy())
    # consumption and work
    for c, v in mix['consumption'].items():
        put(c, v)
    wages_pos = out['wages'].to_numpy(float) > 0
    put('weeks_primary', np.where(wages_pos, mix['weeks_primary'], 0.0))
    put('hours_primary', np.where(wages_pos, mix['hours_primary'], 0.0))
    # the totals, from the parts (start.recombine), then the items at a ratio to AGI before them
    from scale import start as START
    out, _ = START.recombine(out, year)
    agi0 = np.maximum(D.agi(out), 0)
    for c, r in mix['ratio to AGI'].items():
        put(c, agi0 * r)
    out, _ = START.recombine(out, year)
    # flags
    put('billionaire', 1)
    put('billionaire_rank', u['unit_rank'].to_numpy())
    put('forbes_uri', u['unit_uri'].to_numpy())
    # checks: nothing missing that the file never has missing; dtypes as the file's
    for c in df.columns:
        may_be_missing = c.startswith('u_') or bool(df[c].isna().any())
        assert may_be_missing or not out[c].isna().any(), f'billionaires: {c} missing on {int(out[c].isna().sum())} records'
        if pd.api.types.is_integer_dtype(df[c]):
            assert np.array_equal(out[c].to_numpy(float), np.round(out[c].to_numpy(float))), f'billionaires: {c} not whole'
            out[c] = out[c].astype(df[c].dtype)
    assert np.allclose(out['net_worth'], u['net_worth'].to_numpy(float), rtol=1e-12), 'net worth is not Forbes\'s'
    units_table = u.assign(fiscal_income=sum(kinds.values()), agi=D.agi(out), gifts=gifts,
                           **{f'kind: {k}': v for k, v in kinds.items()})
    notes.append(f'billionaires: {len(lst)} list entries, {len(u)} tax units, ${u.net_worth.sum() / 1e12:.3f} trillion; '
                 f'public share known for {int(known.sum())} people, the rank group\'s average for {int((~known).sum())}')
    return out, {'units': units_table, 'people': p, 'groups': pd.DataFrame(group_rows), 'mix': mix, 'mix group': what,
                 'bsyz': groups}


# ---- the redraw of balance sheets at $1 billion or more (one function for Part A and Part B) ------------------
def offset_factors(models):
    """Each model's consistency offset factors as generation last used them (gen/consistency.csv, the last run's
    'factor used'), by the model's training seed, and a note. Part A's build (top_hidden.build) reads them too."""
    path = os.path.join(common.GEN, 'consistency.csv')
    c = pd.read_csv(path)
    c = c[c['run'] == c['run'].max()]
    return {m['seed']: dict(zip(c.loc[c.model == m['seed'], 'holding'], c.loc[c.model == m['seed'], 'factor used']))
            for m in models}, f'offset factors from {path}, run {int(c.run.max())}'


def balance_sheet_dollars(rows, maps, salt):
    """The balance sheet in target-year dollars from rows' SCF bands (columns <amount>_band, scf_wave, record_id): each
    amount from the row's own wave's map, with a pick keyed by record id (ages.uniform_by_id(record_id, salt + the
    amount's position in the table's SCF amounts); salt may be one number per row); the items made from a share; net
    worth. Returns a frame of the SCF amounts, the share items and net_worth."""
    from shared import ages
    out = pd.DataFrame(index=rows.index)
    salt = np.asarray(salt, dtype=np.uint64) if np.ndim(salt) else int(salt)
    for i, v in enumerate(D.META['scf_amounts']):
        u = ages.uniform_by_id(rows['record_id'].to_numpy(), salt + np.uint64(i) if np.ndim(salt) else salt + i)
        out[v] = D.draw_by_wave(rows, v, maps, None, u)
    for slot, (item, base) in D.RATIOS.items():
        if slot in out and base in out:
            out[item] = out[slot] * out[base]
    out['net_worth'] = D.signed_sum(out, D.NET_WORTH_PARTS)
    return out


def kept_picks(rids, kept, salt):
    """The stored picks (u_<amount>) of each record's kept try: the same keyed numbers balance_sheet_dollars used."""
    from shared import ages
    return {f'u_{v}': ages.uniform_by_id(rids, int(salt) + 100 * np.asarray(kept) + i)
            for i, v in enumerate(D.META['scf_amounts'])}


def redraw_records(network, factors, coded, label_idx, year_idx, scf_year_idx, waves, rids, maps, ceiling, tries, seed,
                   salt, device):
    """The balance sheets of many records drawn again (Part B; Part A's step 7 calls it too): every SCF-only slot,
    under the SCF label at each record's SCF wave, given everything else on the record, as generation draws it
    (generate.Chunk: the record's own coded values, after_totals for the consistency rules each record triggers, the
    exact ages put back, the SCF slots in scf_order, the model's offset factors). Each record gets tries rows, from
    generators seeded by (seed, its id) alone. The network runs on every record's rows at once; the output layers and
    the draws run record by record (Chunk.records), so each record's tries come out exactly as when it is redrawn on
    its own. Try t's dollars come from picks keyed by the record's id and salt + 100 t (balance_sheet_dollars).
    coded: [records, len(model.PREDICTED)], the records' slot values in the stack's coding; label_idx, year_idx,
    scf_year_idx: per record, the index of its own label, its year and its SCF income year; waves, rids: per record.
    The first try below the ceiling is kept, else the lowest. Returns, one row per record: the kept try's SCF slot
    values (the stack's columns), its balance-sheet dollars, every try's net worth [records, tries], the try kept, and
    whether a try was below the ceiling."""
    import torch
    from generate import generate as G
    from pretrain import model as M
    P, NC = M.PREDICTED, len(M.CONTEXT)
    R, n = len(rids), len(rids) * tries
    orders, records = [], []
    for r, rid in enumerate(rids):
        gen_seed = int(np.random.default_rng([int(seed), int(rid)]).integers(2 ** 62))
        orders.append(G.scf_order(tries, torch.Generator().manual_seed(gen_seed)))
        records.append((torch.arange(r * tries, (r + 1) * tries, device=device),
                        torch.Generator(device=device).manual_seed(gen_seed + 1)))
    order = torch.cat(orders)
    rep = lambda x: torch.as_tensor(np.repeat(np.asarray(x, dtype=np.int64), tries))
    v = torch.full((n, len(M.SLOTS)), -1, dtype=torch.long)
    v[:, 0], v[:, 1] = rep(label_idx), rep(year_idx)
    v[:, NC:] = torch.as_tensor(np.repeat(np.asarray(coded, dtype=np.int64), tries, axis=0))
    v = v.to(device)
    syear = {src: torch.zeros(n, dtype=torch.long) for src in G.SURVEY_YEARS}   # not used by the SCF phase
    d = G.Chunk(network, n, order.to(device), {}, 0, factors, None, rep(scf_year_idx), syear)
    d.records = records
    d.values = v.clone()
    d.filled = torch.ones(n, len(P), dtype=torch.bool, device=device)
    for j in G.SURVEY_ONLY.tolist() + G.REP_SLOTS.tolist():
        d.filled[:, j] = False                      # hidden until drawn (the reported slots never drawn)
    d.after_totals()                                # the rules each record triggers; every survey slot present
    for a in G.AGE_SLOTS:                           # the exact ages are drawn before the SCF phase: kept
        d.values[:, NC + P.index(a)] = v[:, NC + P.index(a)]
        d.filled[:, P.index(a)] = True
    slot_at = order.numpy().reshape(R, tries, -1)   # [record, try, step]
    with torch.no_grad():
        for step in range(order.shape[1]):
            # per record, the rows (positions among its tries) of each distinct slot at this step
            flat, groups = [], []
            for r in range(R):
                g = []
                for jj in np.unique(slot_at[r, :, step]):
                    pos = np.flatnonzero(slot_at[r, :, step] == jj)
                    g.append((int(jj), len(flat), len(flat) + len(pos)))
                    flat.extend(pos.tolist())
                groups.append(g)
            flat = torch.as_tensor(flat, device=device)
            d.survey_step(d.order[:, step], 'scf', d.scf_year, [[(jj, flat[a:b]) for jj, a, b in g] for g in groups])
    vals = d.values.cpu().numpy()
    got = pd.DataFrame({M.column(x): vals[:, NC + P.index(x)] for x in G.SCF_SLOTS})
    got['record_id'], got['scf_wave'] = np.repeat(np.asarray(rids), tries), np.repeat(np.asarray(waves), tries)
    dd = balance_sheet_dollars(got, maps, int(salt) + 100 * np.tile(np.arange(tries), R))
    nw = dd['net_worth'].to_numpy(float).reshape(R, tries)
    below = nw < ceiling
    ok = below.any(1)
    kept = np.where(ok, below.argmax(1), nw.argmin(1))
    at = np.arange(R) * tries + kept
    return (got.drop(columns=['record_id', 'scf_wave']).iloc[at].reset_index(drop=True),
            dd.iloc[at].reset_index(drop=True), nw, kept, ok)


SCF_MADE = ['net_worth', 'value.primary_mortgage', 'w_dc_stocks', 'elective1', 'elective2', 'trad_contr_er1',
            'trad_contr_er2', 'trad_deferrals', 'medicare_wages']   # made from the SCF amounts (draw_dollars)


def redraw_balance_sheets(df, raw, year, ceiling, notes):
    """The redraw on the generated records, see the module docstring. df: the start in target-year dollars; raw: the
    same records with their bands and stored picks (the generated stack, in df's order). Every record at or above the
    ceiling: redraw_records, model by model, then the records' dollars made again from the kept tries' bands and picks
    (dollars.draw_dollars), of which the SCF amounts and the items made from them are kept. Returns the new frame and a
    table, one row per record redrawn."""
    import torch
    from generate import generate as G
    from pretrain import model as M
    nw = df['net_worth'].to_numpy(float)
    todo = np.flatnonzero(nw >= ceiling)
    if 'billionaire' in df:
        assert not (df['billionaire'].to_numpy()[todo] == 1).any()
    if not len(todo):
        notes.append('redraw: no generated record at or above the ceiling')
        return df, pd.DataFrame()
    assert np.array_equal(raw['record_id'].to_numpy()[todo], df['record_id'].to_numpy()[todo]), 'raw and df out of order'
    tries, seed, salt = int(S['redraw_tries']), int(S['redraw_seed']), int(S['redraw_salt'])
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    models = G.load_models(device)
    factors, fnote = offset_factors(models)
    maps = D.load_maps(year)
    P = M.PREDICTED
    scf_cols = [M.column(x) for x in G.SCF_SLOTS]
    r = raw.iloc[todo].reset_index(drop=True)
    rids = r['record_id'].to_numpy().astype(np.int64)
    coded = np.column_stack([r[M.column(x)].to_numpy().astype(np.int64) for x in P])
    label_idx, year_idx = r['source'].map(M.SOURCES.index).to_numpy(), r['year'].map(M.YEARS.index).to_numpy()
    scf_year_idx, waves, model_of = r['scf_year'].map(M.YEARS.index).to_numpy(), r['scf_wave'].to_numpy(), r['model'].to_numpy()
    nw_tries, kept, ok, bands = np.zeros((len(r), tries)), np.zeros(len(r), int), np.zeros(len(r), bool), []
    for mod in models:
        m = np.flatnonzero(model_of == mod['seed'])
        if not len(m):
            continue
        got, _, nw_tries[m], kept[m], ok[m] = redraw_records(
            mod['network'], factors[mod['seed']], coded[m], label_idx[m], year_idx[m], scf_year_idx[m], waves[m], rids[m],
            maps, ceiling, tries, seed, salt, device)
        bands.append(got.set_index(m))
    bands = pd.concat(bands).sort_index()
    for c in scf_cols:
        r[c] = bands[c].to_numpy()
    picks = {c: u for c, u in kept_picks(rids, kept, salt).items() if c in r}
    for c, u in picks.items():
        r[c] = u
    dd = D.draw_dollars(r, maps, year)
    after = nw_tries[np.arange(len(r)), kept]
    assert np.allclose(dd['net_worth'].to_numpy(float), after, rtol=1e-9, atol=1e-3), 'a kept try differs'
    # the redraw's other dollars are the records' own
    same = [c for c in D.ORDER if c in dd and c not in D.SCF_SET and c in df and c not in SCF_MADE]
    moved = [c for c in same if not np.isclose(dd[c].to_numpy(float), df[c].to_numpy(float)[todo], rtol=1e-9, atol=1e-6).all()]
    assert not moved, f'the redraw moved {moved}'
    out = df.copy()
    keep = [c for c in dict.fromkeys(list(D.META['scf_amounts']) + SCF_MADE + scf_cols) if c in out and c in dd]
    for c in keep:
        out.iloc[todo, out.columns.get_loc(c)] = dd[c].to_numpy()
    for c, u in picks.items():
        if c in out:
            out[c] = out[c].astype(np.float64)      # the picks are kept in full precision
            out.iloc[todo, out.columns.get_loc(c)] = u
    tab = pd.DataFrame({'record_id': rids, 'net worth before': nw[todo], 'tries below the ceiling': (nw_tries < ceiling).sum(1),
                        'try kept': kept + 1, 'net worth after': after, 'fallback': ~ok})
    out['wealth_redraw_fallback'] = 0
    out.loc[out['record_id'].isin(tab.loc[tab.fallback, 'record_id']), 'wealth_redraw_fallback'] = 1
    out['wealth_redrawn'] = out['record_id'].isin(tab['record_id']).astype(int)
    notes.append(f'redraw: {len(tab)} generated records at or above ${ceiling / 1e9:g} billion, {tries} tries each '
                 f'({fnote}); {int((~tab.fallback).sum())} kept a try below it, {int(tab.fallback.sum())} fell back to their '
                 f'lowest try; {tab["tries below the ceiling"].sum() / (tries * len(tab)):.1%} of all tries were below it')
    return out, tab
