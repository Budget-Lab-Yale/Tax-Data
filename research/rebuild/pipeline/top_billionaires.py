"""The billionaires (PLAN.md Part B, explore/2026-10-29/top_research; approved by John, 29 October 2026).

Above $1 billion of net worth the file is the Forbes list alone; below it, the SCF (a labelled assumption, John's
choice: Forbes covers the people above $1 billion better than a handful of heavily weighted SCF households). Two pieces:

1. redraw_balance_sheets (step 10): every generated record whose net worth, in nominal target-year dollars, comes
   out at $1 billion or more has its whole SCF block drawn again (every SCF-only slot: assets, debts, the home's loan
   share, accumulated gains, ownership flags and industries, elective deferrals, auto loan interest, the QBI workers),
   under the SCF label given everything else on the record, as generation drew it (generate.Chunk.survey_step, the
   record's own SCF wave, its model, the consistency rules with that model's offset factors). Each record gets a fixed
   number of tries (settings tilt.top.redraw_tries), drawn together as one batch whose random numbers come from
   generators seeded by the record's id alone (the slot order, the draws, and the stored picks that turn bands into
   dollars), so a record's tries never depend on which other records are redrawn. The first try below $1 billion is
   kept; if every try lands at $1 billion or more, the try with the lowest net worth is kept and the record flagged
   (wealth_redraw_fallback). The test is on grown dollars (dollars.draw_dollars with the target year's maps).
   Then net worth, the mortgage, the retirement accounts' stocks and the deferral items are made again from the new
   dollars; nothing else on the record changes.

2. build_billionaires (steps 11-18): the Forbes list as tax units, weight 1, flag billionaire = 1.
   - The list (resources/top/forbes_billionaires_<year>.csv; settings tilt.top.billionaire_lists): one dated list.
     Spouses listed separately become one joint return (their fortunes added); a family fortune or a couple named in
     one entry stays one record. Units are ranked by their net worth.
   - Rank groups, BSYZ's (Balkir, Saez, Yagan and Zucman 2025, Table 1; the IRS study's Table 1 behind it): the
     top 92 units ("top 100", the top 0.00005% of tax units), the next 276 ("next 300", the next 0.00015%), and the
     rest, which take the next group's figures (an extrapolation).
   - Wealth: net worth is Forbes's. Public holdings are the person's public share (Forbes's tracked share holdings over
     net worth, from production's later 2025 profiles: not dated 2019) times their 2019 net worth; where it is
     unknown, the rank group's wealth-weighted average share among people with a known one (not assumed private).
     The rest is private: real estate (w_re_fund) when Forbes's industry is real estate, otherwise a private business
     (w_pass_throughs). No home, no debt (net worth right, gross assets understated). Basis: zero for self-made
     fortunes (Forbes's 2019 self_made flag), equal to value for inherited ones, written as accumulated gains
     (w_kg_other on public stock, w_kg_pass_throughs, w_kg_other_re) so the non-microdata step's cost_basis gives it.
   - Income (steps 13-14): each rank group's dollars of each kind = the group's Forbes wealth x the IRS study's
     amount per dollar of wealth (= wealth x BSYZ's fiscal-income ratio x BSYZ's share, on the underlying amounts),
     kinds: capital gains, dividends, taxable interest, tax-exempt interest, business profits, business losses,
     rental and royalty profits, rental losses, estate and trust income, other income (net operating losses left
     out), wages and pensions. Each kind is shared among the group's units by their holdings of the matching asset:
     gains and dividends by public stock; business profits, and separately business losses, by private holdings;
     the rest by total wealth. Group totals hold exactly; a unit's income is not wealth x the ratio.
   - Tax items (step 15): within each kind, the split follows the item mix of the top group (item_mix: Part A's
     rebuilt $100 million-plus returns when present, else the generated records with AGI of $100 million or more,
     a stand-in), except where the IRS study gives the split: partnership against S corporation profits and losses.
   - Charity (step 16): gifts of 0.7% of wealth in the top group, 0.6% in the rest (BSYZ Table 2), cash and other
     gifts as in the mix; the calculator computes the deduction.
   - People (step 17): age from Forbes's 2019 age (held to 80, the file's top age), sex from Forbes, marital status
     from the later profiles (married, or "widowed, remarried"; missing: married, production's rule); a spouse of the
     same age and the other sex (a separately listed spouse keeps their own age and sex). Dependents by production's
     rule (only under 45, at most three, ages 12, 9, 6, from the later profiles' children). Earnings all to the primary
     filer. Items beyond the kinds (state income tax, foreign tax credits, business credits, self-employment items,
     the other itemized deductions in RATIO_ITEMS) at their ratio to AGI in the mix; consumption at the mix's average;
     everything else zero, listed in ZERO_ITEMS. Ids from 95,000,000 (ID_BASE) up, by rank (Part A's from 90,000,000).
"""
import contextlib
import io
import os

import numpy as np
import pandas as pd

import common
import dollars as D

S = common.SETTINGS['tilt'].get('top', {})
RES = os.path.abspath(os.path.join(common.HERE, '..', 'resources', 'top'))
ID_BASE = 95_000_000             # Part A's rebuilt returns take 90,000,000 up (top_hidden id_block)
TOP_AGE = 80                      # the file's top age (age_primary, age_spouse): 80 means 80 and over
GROUPS = [('top', 92), ('next', 276)]   # BSYZ's top 0.00005% and next 0.00015% of tax units (IRS study units)
SCF_SET = list(D.META['scf_amounts'])

# kinds of income: (IRS study item(s), the holdings that share it out)
KINDS = {'gains': ('public'), 'dividends': ('public'), 'interest': ('total'), 'exempt': ('total'),
         'business profits': ('private'), 'business losses': ('private'), 'rent profits': ('total'),
         'rent losses': ('total'), 'estate': ('total'), 'other income': ('total'), 'wages and pensions': ('total')}
# items beyond the kinds, at their ratio to AGI in the mix (step 17)
RATIO_ITEMS = ['salt_inc_sales', 'real_estate_tax', 'ftc', 'amt_ftc', 'gbc', 'prior_yr_cred', 'other_item_exp',
               'other_misc_exp', 'tax_prep_exp', 'part_se', 'se_tax_ded', 'se_health', 'keogh_contr']
# items set to zero, with the reason (for the report)
ZERO_ITEMS = {
    'ss_gross': 'not market income; capped per person, so not scaled with income',
    'ss_taxable': 'as ss_gross', 'unemployment': 'none', 'alimony': 'none', 'alimony_exp': 'none',
    'trad_contr_ira': 'capped, negligible', 'sl_int_ded': 'phased out', 'ed_exp': 'phased out',
    'tuition_ded': 'phased out', 'hsa_ded': 'capped, negligible', 'early_penalty': 'none', 'med_exp': 'none',
    'casualty_exp': 'none', 'job_exp': 'none', 'mortgage_interest': 'no home, no debt (step 12)',
    'aoc_exp': 'phased out', 'llc_exp': 'phased out', 'old_cred': 'none', 'savers_cred': 'phased out',
    'res_energy_cred': 'none', 'other_nonref': 'none', 'net_ptc': 'none', 'excess_ptc': 'none',
    'recapture_tax': 'none', 'ira_penalty': 'none', 'amt_nols': 'net operating losses left out (step 13)',
    'dpad': 'repealed after 2017', 'farm_rent': 'none', 'sche_other': 'none', 'childcare': 'none',
    'auto_int_exp': 'no debt', 'elective_deferrals': 'none', 'tips_primary': 'none', 'tips_spouse': 'none',
    'ot_primary': 'none', 'ot_spouse': 'none'}


# ---- the list as tax units ------------------------------------------------------------------------------------
def read_list(year):
    """The year's Forbes list (settings tilt.top.billionaire_lists: {year: file in resources/top}), or None."""
    name = (S.get('billionaire_lists') or {}).get(str(year))
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
        k = {'gains': a['realized_capital_gains'], 'dividends': a['dividends'], 'interest': a['interest'],
             'exempt': a['tax_exempt_interest'], 'business profits': a['positive_business_income'],
             'business losses': a['business_income'] - a['positive_business_income'],
             'rent losses': a['negative_rental_income'],
             'rent profits': a['rental_income'] + a['royalties'] - a['negative_rental_income'],
             'estate': a['estate_and_trust_income'], 'other income': a['other_income_excl_nols'],
             'wages and pensions': a['wages_and_pensions']}
        fiscal = (a['wages_and_pensions'] + a['realized_capital_gains'] + a['dividends'] + a['interest']
                  + a['tax_exempt_interest'] + a['rental_income'] + a['estate_and_trust_income'] + a['royalties']
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


# ---- the item mix of the top group (step 15) -----------------------------------------------------------------
def _share(num, den, fallback):
    return float(num / den) if den > 0 else fallback


def item_mix(df, w, notes):
    """The split of each kind into tax items, the ratios of the other items to AGI, gifts' cash share, workers per
    dollar and industry by kind of business, consumption and work, from the mix group: Part A's rebuilt returns of
    $100 million or more of AGI where present (synthetic_top = 1; settings tilt.top.mix_from_hidden), else the
    generated records with AGI of at least settings tilt.top.stand_in_agi (target-year dollars), or the top 0.01% of
    filers by AGI if fewer than stand_in_min_records. Weighted by w. Returns the mix and the group's description."""
    agi = D.agi(df)
    filer = df['filer'].to_numpy() == 1
    hidden = df['synthetic_top'].to_numpy() == 1 if 'synthetic_top' in df else np.zeros(len(df), bool)
    bil = df['billionaire'].to_numpy() == 1 if 'billionaire' in df else np.zeros(len(df), bool)
    line = float(S.get('stand_in_agi', 1e8))
    if hidden.any() and S.get('mix_from_hidden', True):
        g = hidden & (df['hidden_group'].to_numpy() == 3)
        what = (f"Part A's rebuilt returns of the over-$100-million aggregate record (hidden_group 3; {int(g.sum())} "
                f"records, in target-year dollars, weighted by their starting weights)")
    else:
        g = filer & ~bil & ~hidden & (agi >= line)
        what = (f"STAND-IN: the generated filers with AGI of ${line / 1e6:,.0f} million or more in target-year dollars "
                f"({int(g.sum())} records; weighted by the generation weights)")
        if g.sum() < int(S.get('stand_in_min_records', 100)):
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
    wp = {c: pos(c) for c in ['wages', 'pensions_taxable', 'ira_taxable']}
    m['wages and pensions'] = {c: _share(v, sum(wp.values()), 1.0 if c == 'wages' else 0.0) for c, v in wp.items()}
    # business profits and losses: the Schedule C and farm part from the mix, partnership against S corporation from
    # the IRS study (build_billionaires), and within each the mix's split
    prof = {'part_pass_inc': tot('part_pass_inc'), 'part_nonpass_inc': tot('part_nonpass_inc'),
            'scorp_pass_inc': tot('scorp_pass_inc'), 'scorp_nonpass_inc': tot('scorp_nonpass_inc'),
            'sch_c': pos('sch_c'), 'farm': pos('farm')}
    loss = {'part_pass_loss': tot('part_pass_loss'), 'part_nonpass_loss': tot('part_nonpass_loss'),
            'part_179': tot('part_179'), 'scorp_pass_loss': tot('scorp_pass_loss'),
            'scorp_nonpass_loss': tot('scorp_nonpass_loss'), 'scorp_179': tot('scorp_179'),
            'sch_c': neg('sch_c'), 'farm': neg('farm')}
    m['schedule C and farm share of profits'] = _share(prof['sch_c'] + prof['farm'], sum(prof.values()), 0.0)
    m['schedule C and farm share of losses'] = _share(loss['sch_c'] + loss['farm'], sum(loss.values()), 0.0)
    part = lambda d, keys, first: {k: _share(d[k], sum(d[q] for q in keys), 1.0 if k == first else 0.0) for k in keys}
    m['partnership profits'] = part(prof, ['part_pass_inc', 'part_nonpass_inc'], 'part_nonpass_inc')
    m['scorp profits'] = part(prof, ['scorp_pass_inc', 'scorp_nonpass_inc'], 'scorp_nonpass_inc')
    m['schedule C and farm profits'] = part(prof, ['sch_c', 'farm'], 'sch_c')
    m['partnership losses'] = part(loss, ['part_pass_loss', 'part_nonpass_loss', 'part_179'], 'part_nonpass_loss')
    m['scorp losses'] = part(loss, ['scorp_pass_loss', 'scorp_nonpass_loss', 'scorp_179'], 'scorp_nonpass_loss')
    m['schedule C and farm losses'] = part(loss, ['sch_c', 'farm'], 'sch_c')
    oth = {c: pos(c) for c in ['other_gains', 'state_refunds', 'other_agi_remainder']}
    m['other income'] = {c: _share(v, sum(oth.values()), 1.0 if c == 'other_agi_remainder' else 0.0)
                         for c, v in oth.items()}
    m['char_cash share'] = _share(tot('char_cash'), tot('char_cash') + tot('char_noncash'), 0.5)
    agi_pos = float((ww * np.maximum(agi[g], 0)).sum())
    m['ratio to AGI'] = {c: tot(c) / agi_pos for c in RATIO_ITEMS}
    # QBI: each kind's outside workers per dollar of the kind's positive income, and its industry with the most income
    kinds = {'schc': {'sch_c': 1}, 'part': {'part_pass_inc': 1, 'part_nonpass_inc': 1},
             'scorp': {'scorp_pass_inc': 1, 'scorp_nonpass_inc': 1}}
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
def _people_attributes(people):
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
    p = _people_attributes(people)
    groups = bsyz_groups()
    # wealth: public share known, else the rank group's wealth-weighted average among people with a known share
    known = p['public_share'].notna()
    avg = (p[known].assign(x=lambda d: d.public_share * d.net_worth).groupby('group')['x'].sum()
           / p[known].groupby('group')['net_worth'].sum())
    p['public_share_used'] = p['public_share'].fillna(p['group'].map(avg))
    assert p['public_share_used'].notna().all(), 'a rank group with no known public share'
    p['public'] = p['public_share_used'] * p['net_worth']
    p['private'] = p['net_worth'] - p['public']
    p['private_re'] = np.where(p['real_estate'], p['private'], 0.0)
    p['private_pt'] = p['private'] - p['private_re']
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
    u['age_spouse'] = np.where(u['married'] == 1, np.where(u['joint_pair'], u['sp_age_used'], u['age_used']), -1).astype(int)
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
    n = len(u)
    put = lambda c, v: out.__setitem__(c, v)
    put('source', 'puf'); put('year', common.PUF_YEAR); put('filer', 1); put('stratifier', 'forbes'); put('model', 0)
    put('weight', 1.0); put('agi_group', -1); put('q_group', -1); put('label_ok', True)
    put('record_id', ID_BASE + u['unit_rank'].to_numpy())
    put('scf_wave', year); put('scf_year', year - 1)
    for src in ['cex', 'cps', 'sipp']:
        if f'{src}_year' in df:
            put(f'{src}_year', year)
    # people
    put('married', u['married'].to_numpy())
    put('n_dep', u['n_dep'].to_numpy())
    put('mars', np.where(u['married'] == 1, 2, np.where(u['n_dep'] > 0, 4, 1)))
    put('n_ctc', u['n_dep'].to_numpy()); put('n_eitc', 0)
    age = u['age_used'].to_numpy()
    put('age_primary', age)
    put('age_range', np.searchsorted([26, 35, 45, 55, 65], age, side='right'))
    put('age_spouse', u['age_spouse'].to_numpy())
    put('sex_primary', u['sex'].to_numpy()); put('sex_spouse', u['sex_spouse'].to_numpy())
    for k, a in enumerate([12, 9, 6], start=1):
        has = u['n_dep'].to_numpy() >= k
        put(f'age_dep{k}', np.where(has, a, -1))
        put(f'ssn_status_dep{k}', np.where(has, 1, 0))
        if f'dep_ssn{k}' in df:
            put(f'dep_ssn{k}', np.where(has, 1.0, np.nan))
    put('ssn_status_primary', 1); put('ssn_status_spouse', np.where(u['married'] == 1, 1, 0))
    if 'ssn1' in df:
        put('ssn1', 1.0)
    if 'ssn2' in df:
        put('ssn2', np.where(u['married'] == 1, 1.0, np.nan))
    if 'blind' in df:                 # a stack from before the blindness rule (the retrain of 28 October): not blind
        put('blind', 1)
    if 'blind_chance2' in df:
        put('blind_chance2', np.where(u['married'] == 1, 0.0, -1.0))
    # income items
    g = mix['gains']
    for c in D.KG_PIECES:
        put(c, kinds['gains'] * g[c])
    put('kg_1250', np.maximum(out['kg_lt'], 0) * mix['kg_1250 per kg_lt'])
    put('kg_collect', np.maximum(out['kg_lt'], 0) * mix['kg_collect per kg_lt'])
    put('div_pref', kinds['dividends'] * mix['div_pref share'])
    put('div_ord', kinds['dividends'] * (1 - mix['div_pref share']))
    put('interest', kinds['interest']); put('tax_exempt_interest', kinds['exempt'])
    for c, s in mix['wages and pensions'].items():
        put(c, kinds['wages and pensions'] * s)
    put('retirement_gross', out['pensions_taxable'] + out['ira_taxable'])
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
    for c in ['sch_c', 'farm']:
        put(c, pr * cf_p * mix['schedule C and farm profits'][c] - ls * cf_l * mix['schedule C and farm losses'][c])
    put('rent_inc', kinds['rent profits']); put('rent_loss', -kinds['rent losses'])
    put('estate_inc', np.maximum(kinds['estate'], 0)); put('estate_loss', np.maximum(-kinds['estate'], 0))
    for c, s in mix['other income'].items():
        put(c, kinds['other income'] * s)
    assert (out['rent_loss'] >= 0).all()
    # the computed business total, as drawn (no draw here)
    parts = D.META['computed_totals']['business_income']
    put('business_income_drawn', out[list(parts)].to_numpy(float) @ np.array(list(parts.values()), float))
    # charity (step 16)
    rate = np.where(grp == 'top', float(S.get('gift_rate_top', 0.007)), float(S.get('gift_rate_rest', 0.006)))
    gifts = rate * u['net_worth'].to_numpy(float)
    put('char_cash', gifts * mix['char_cash share']); put('char_noncash', gifts * (1 - mix['char_cash share']))
    # the balance sheet (step 12) and basis as accumulated gains
    put('w_equities', u['public'].to_numpy(float)); put('w_pass_throughs', u['private_pt'].to_numpy(float))
    put('w_re_fund', u['private_re'].to_numpy(float))
    put('w_kg_other', u['gain_public'].to_numpy(float)); put('w_kg_pass_throughs', u['gain_private_pt'].to_numpy(float))
    put('w_kg_other_re', u['gain_private_re'].to_numpy(float))
    put('owns_business', (u['private_pt'] > 0).astype(int).to_numpy())
    # QBI kinds: an industry where the kind has income, workers per dollar of its profits
    kp = {'schc': np.maximum(out['sch_c'], 0), 'part': out['part_pass_inc'] + out['part_nonpass_inc'],
          'scorp': out['scorp_pass_inc'] + out['scorp_nonpass_inc']}
    anyk = {'schc': out['sch_c'] != 0,
            'part': (out[['part_pass_inc', 'part_nonpass_inc', 'part_pass_loss', 'part_nonpass_loss', 'part_179']] != 0).any(axis=1),
            'scorp': (out[['scorp_pass_inc', 'scorp_nonpass_inc', 'scorp_pass_loss', 'scorp_nonpass_loss', 'scorp_179']] != 0).any(axis=1)}
    for k in kp:
        put(f'biz_industry_{k}', np.where(anyk[k], mix['industry'][k], 0))
        put(f'biz_workers_{k}', np.where(anyk[k], kp[k] * mix['workers per dollar'][k], 0.0))
    put('k1_owner', (anyk['part'] | anyk['scorp']).astype(int).to_numpy())
    # consumption and work
    for c, v in mix['consumption'].items():
        put(c, v)
    wages_pos = out['wages'].to_numpy(float) > 0
    put('weeks_primary', np.where(wages_pos, mix['weeks_primary'], 0.0))
    put('hours_primary', np.where(wages_pos, mix['hours_primary'], 0.0))
    # the totals, from the parts (tilt.recombine), then the items at a ratio to AGI before them (step 17)
    import tilt as TL
    out, _ = TL.recombine(out, year)
    agi0 = np.maximum(D.agi(out), 0)
    for c, r in mix['ratio to AGI'].items():
        put(c, agi0 * r)
    out, _ = TL.recombine(out, year)
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


# ---- the redraw of balance sheets at $1 billion or more (step 10; one function for Part A and Part B) ---------
def _factors(models):
    """Each model's consistency offset factors as generation last used them (gen/consistency.csv, the last run's
    'factor used'); {} with a note when the file is missing."""
    path = os.path.join(common.GEN, 'consistency.csv')
    if not os.path.exists(path):
        return {m['seed']: {} for m in models}, f'{path} missing: no offset factors in the redraw'
    c = pd.read_csv(path)
    c = c[c['run'] == c['run'].max()]
    return {m['seed']: dict(zip(c.loc[c.model == m['seed'], 'holding'], c.loc[c.model == m['seed'], 'factor used']))
            for m in models}, f'offset factors from {path}, run {int(c.run.max())}'


def balance_sheet_dollars(rows, maps, salt):
    """The balance sheet in target-year dollars from rows' SCF bands (columns <amount>_band, scf_wave, record_id): each
    amount from the row's own wave's map, with a pick keyed by record id (ages.uniform_by_id(record_id, salt + the
    amount's position in the table's SCF amounts); salt may be one number per row); the items made from a share; net
    worth. Returns a frame of the SCF amounts, the share items and net_worth."""
    import ages
    out = pd.DataFrame(index=rows.index)
    salt = np.asarray(salt, dtype=np.uint64) if np.ndim(salt) else int(salt)
    for i, v in enumerate(D.META['scf_amounts']):
        u = ages.uniform_by_id(rows['record_id'].to_numpy(), salt + np.uint64(i) if np.ndim(salt) else salt + i)
        out[v] = D.draw_by_wave(rows, v, maps, None, u)
    for slot, (item, base) in D.RATIOS.items():
        if slot in out and base in out:
            out[item] = out[slot] * out[base]
    out['net_worth'] = out[list(D.NET_WORTH_PARTS)].to_numpy(float) @ np.array(list(D.NET_WORTH_PARTS.values()), float)
    return out


def redraw_record(network, factors, coded, label, year_idx, scf_year_idx, wave, rid, maps, ceiling, tries, seed, salt,
                  device):
    """One record's balance sheet drawn again (step 10; Part A's step 7 calls it too): every SCF-only slot, under the
    SCF label at the record's SCF wave, given everything else on the record, as generation draws it (generate.Chunk:
    the record's own coded values, after_totals for the consistency rules each record triggers, the exact ages put
    back, the SCF slots in scf_order, the model's offset factors). tries rows drawn as one batch from generators seeded
    by (seed, rid) alone; try t's dollars from picks keyed by rid and salt + 100 t (balance_sheet_dollars). coded: the
    record's slot values in the stack's coding, len(model.PREDICTED). The first try below the ceiling is kept, else the
    lowest. Returns (the kept try's SCF slot values as {column: value}, its dollars as a Series, every try's net worth,
    the try kept, whether one was below the ceiling)."""
    import torch
    import generate as G
    import model as M
    P, NC = M.PREDICTED, len(M.CONTEXT)
    gen_seed = int(np.random.default_rng([int(seed), int(rid)]).integers(2 ** 62))
    cpu_gen = torch.Generator().manual_seed(gen_seed)
    dev_gen = torch.Generator(device=device).manual_seed(gen_seed + 1)
    v = torch.full((tries, len(M.SLOTS)), -1, dtype=torch.long)
    v[:, 0], v[:, 1] = M.SOURCES.index(label), year_idx
    v[:, NC:] = torch.as_tensor(np.asarray(coded, dtype=np.int64))[None, :]
    v = v.to(device)
    order = G.scf_order(tries, cpu_gen).to(device)
    scf_year = torch.full((tries,), int(scf_year_idx), dtype=torch.long)
    syear = {src: torch.zeros(tries, dtype=torch.long) for src in G.SURVEY_YEARS}   # not used by the SCF phase
    d = G.Chunk(network, tries, order, {}, 0, factors, dev_gen, scf_year, syear, label,
                None if label == 'puf' else M.YEARS[year_idx])
    d.values = v.clone()
    d.filled = torch.ones(tries, len(P), dtype=torch.bool, device=device)
    for j in G.SURVEY_ONLY.tolist() + G.REP_SLOTS.tolist():
        d.filled[:, j] = False                      # hidden until drawn (the reported slots never drawn)
    d.after_totals()                                # the rules each record triggers; every survey slot present
    for a in G.AGE_SLOTS:                           # the exact ages are drawn before the SCF phase: kept
        d.values[:, NC + P.index(a)] = v[:, NC + P.index(a)]
        d.filled[:, P.index(a)] = True
    with torch.no_grad():
        for step in range(order.shape[1]):
            d.survey_step(order[:, step], 'scf', d.scf_year)
    vals = d.values.cpu().numpy()
    got = pd.DataFrame({M.column(x): vals[:, NC + P.index(x)] for x in G.SCF_SLOTS})
    got['record_id'], got['scf_wave'] = int(rid), int(wave)
    dd = balance_sheet_dollars(got, maps, int(salt) + 100 * np.arange(tries))
    nw = dd['net_worth'].to_numpy(float)
    ok = np.flatnonzero(nw < ceiling)
    k = int(ok[0]) if len(ok) else int(np.argmin(nw))
    return got.drop(columns=['record_id', 'scf_wave']).iloc[k].to_dict(), dd.iloc[k], nw, k, bool(len(ok))


def kept_picks(rid, k, salt):
    """The stored picks (u_<amount>) of try k: the same keyed numbers balance_sheet_dollars used."""
    import ages
    return {f'u_{v}': float(ages.uniform_by_id(np.array([rid]), int(salt) + 100 * int(k) + i)[0])
            for i, v in enumerate(D.META['scf_amounts'])}


SCF_MADE = ['net_worth', 'w_primary_mortgage', 'w_dc_stocks', 'elective1', 'elective2', 'trad_contr_er1',
            'trad_contr_er2', 'trad_deferrals', 'medicare_wages']   # made from the SCF amounts (draw_dollars)


def redraw_balance_sheets(df, raw, year, ceiling, notes, check_same=True):
    """Step 10 on the generated records, see the module docstring. df: the start in target-year dollars; raw: the same
    records with their bands and stored picks (the generated stack, in df's order, or the start file itself). Each
    record at or above the ceiling: redraw_record, then the record's dollars made again from the kept try's bands and
    picks (dollars.draw_dollars on the one record), of which the SCF amounts and the items made from them are kept.
    Returns the new frame and a table, one row per record redrawn."""
    import torch
    import generate as G
    import model as M
    nw = df['net_worth'].to_numpy(float)
    todo = np.flatnonzero(nw >= ceiling)
    if 'billionaire' in df:
        assert not (df['billionaire'].to_numpy()[todo] == 1).any()
    if not len(todo):
        notes.append('redraw: no generated record at or above the ceiling')
        return df, pd.DataFrame()
    assert np.array_equal(raw['record_id'].to_numpy()[todo], df['record_id'].to_numpy()[todo]), 'raw and df out of order'
    tries, seed = int(S.get('redraw_tries', 20)), int(S.get('redraw_seed', 20261029))
    salt = int(S.get('redraw_salt', 2026102901))
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    models = {m['seed']: m for m in G.load_models(device)}
    factors, fnote = _factors(list(models.values()))
    maps = D.load_maps(year)
    P = M.PREDICTED
    scf_cols = [M.column(x) for x in G.SCF_SLOTS]
    out = df.copy()
    rows = []
    for i in todo:
        r = raw.iloc[[i]].copy()
        rid = int(r['record_id'].iloc[0])
        mod = models[int(r['model'].iloc[0])]
        coded = [int(r[M.column(x)].iloc[0]) for x in P]
        slots, _, x, k, ok = redraw_record(mod['network'], factors[mod['seed']], coded, r['source'].iloc[0],
                                           M.YEARS.index(int(r['year'].iloc[0])), M.YEARS.index(int(r['scf_year'].iloc[0])),
                                           int(r['scf_wave'].iloc[0]), rid, maps, ceiling, tries, seed, salt, device)
        for c, val in slots.items():
            r[c] = val
        for c, val in kept_picks(rid, k, salt).items():
            if c in r:
                r[c] = np.float64(val)
        with contextlib.redirect_stdout(io.StringIO()):   # draw_dollars' own report, once per record: not needed
            dd = D.draw_dollars(r, maps, year)
        assert np.isclose(dd['net_worth'].iloc[0], x[k], rtol=1e-9, atol=1e-3), f'record {rid}: kept try differs'
        keep = [c for c in dict.fromkeys(list(D.META['scf_amounts']) + SCF_MADE + scf_cols) if c in out and c in dd]
        for c in keep:
            out.iat[i, out.columns.get_loc(c)] = dd[c].iloc[0]
        for c in kept_picks(rid, k, salt):
            if c in out:
                out.iat[i, out.columns.get_loc(c)] = r[c].iloc[0]
        if check_same:   # a generated stack (not a grown start): the redraw's other dollars are the record's own
            same = [c for c in D.ORDER if c in dd and c not in D.SCF_SET and c in df and c not in SCF_MADE]
            moved = [c for c in same if not np.isclose(dd[c].iloc[0], df[c].iloc[i], rtol=1e-9, atol=1e-6)]
            assert not moved, f'record {rid}: the redraw moved {moved}'
        rows.append({'record_id': rid, 'net worth before': nw[i], 'tries below the ceiling': int((x < ceiling).sum()),
                     'try kept': k + 1, 'net worth after': x[k], 'fallback': not ok})
    tab = pd.DataFrame(rows)
    out['wealth_redraw_fallback'] = 0
    out.loc[out['record_id'].isin(tab.loc[tab.fallback, 'record_id']), 'wealth_redraw_fallback'] = 1
    out['wealth_redrawn'] = out['record_id'].isin(tab['record_id']).astype(int)
    notes.append(f'redraw: {len(tab)} generated records at or above ${ceiling / 1e9:g} billion, {tries} tries each '
                 f'({fnote}); {int((~tab.fallback).sum())} kept a try below it, {int(tab.fallback.sum())} fell back to their '
                 f'lowest try; {tab["tries below the ceiling"].sum() / (tries * len(tab)):.1%} of all tries were below it')
    return out, tab
