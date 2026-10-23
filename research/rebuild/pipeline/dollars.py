"""Dollars for generated records.

The model works in bands. A record's dollars in a band are drawn from that
band's real records ("the map", built by build_table.py): the PUF's dollar
values for PUF variables, the SCF's for the balance sheet, the CEX's for
consumption, each record's chance of being picked proportional to its weight.
The target year's maps are the PUF year's grown to the target year
(build_table.build_maps); negative capital gains are not grown (the $3,000 cap
is nominal).

After the draws, the pieces are put on the definitions the IRS targets use:
  capital_gains   the Schedule D net, kg_st + kg_lt + kg_other (the line-13
                  total, with kg_dist, is kept as capital_gains_line13)
  business_income the exact signed sum of its eighteen parts
  other_agi       AGI minus wages, interest, dividends and capital gains:
                  the drawn remainder plus kg_dist plus the AGI items folded
                  back in (FOLD); the remainder alone is other_agi_remainder
  net_worth       the 14 assets less the 6 debts; the home mortgage is its
                  drawn share of the home's value times the home's dollars
  wages2, se2     the spouse's wages and self-employment income: the drawn
                  share times the couple's total (wages; Schedules C and F as
                  computed above); wages1 and se1, the primary filer's, are the
                  total less the spouse's, so the two add up to the total
  CPS ages        age_primary, age_spouse, age_dep1-3 in years, -1 for none
"""
import numpy as np
import pandas as pd

import common

# The order fixes each amount's random stream; it must not change.
ORDER = ['wages', 'interest', 'dividends', 'capital_gains', 'other_agi', 'net_worth',
         'kg_st', 'kg_lt', 'kg_dist', 'kg_other',
         'business_income', 'retirement_gross', 'tax_exempt_interest', 'other_gains', 'ira_taxable',
         'pensions_taxable', 'ss_taxable', 'unemployment', 'alimony', 'state_refunds', 'adjustments',
         'sch_c', 'part_pass_inc', 'part_nonpass_inc', 'part_pass_loss', 'part_nonpass_loss', 'part_179',
         'scorp_pass_inc', 'scorp_nonpass_inc', 'scorp_pass_loss', 'scorp_nonpass_loss', 'scorp_179',
         'rent_inc', 'rent_loss', 'estate_inc', 'estate_loss', 'farm_rent', 'sche_other', 'farm']
# Added after the first list, so its streams are unchanged ('net_worth' stays as a placeholder): the
# itemized deductions, then the SCF-only amounts. Net worth is computed from the balance sheet.
META = common.meta()
ORDER = ORDER + ['mortgage_interest', 'real_estate_tax'] + list(META['scf_amounts']) + list(META['cex_amounts']) + ['ss_gross'] \
    + list(META['cps_amounts'])
NET_WORTH_PARTS = META['net_worth_parts']
RATIOS = META['ratios']                           # slot: [dollar item, its base]
REMAINDERS = META['remainders']                   # item: [total, the other part]: the total less the other part
AGE_SLOTS = META['cps_structure']['ages']         # the CPS's exact ages: value 0 none, else the age + 1
KG_PIECES = META['kg_pieces']
FOLD = META['agi_signs']                          # the AGI items folded into other_agi, with their signs
AGI_PARTS = ['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']


def agi(df):
    """AGI of records whose dollars draw_dollars has put on the IRS targets' definitions."""
    return df[AGI_PARTS].to_numpy(float).sum(1)


def load_maps(year):
    """{variable: map rows} for a year: the PUF's, the SCF's balance sheet and the CEX's consumption."""
    split = lambda m: {v: g for v, g in m.groupby('variable')}
    maps = {}
    for source in ['puf', 'scf', 'cex', 'cps']:
        maps.update(split(pd.read_parquet(f'{common.MAPS}/map_{source}_{year}.parquet')))
    return maps


def draw_one(bands, vmap, rng):
    u = rng.random(len(bands))
    values = np.full(len(bands), np.nan)
    for band, g in vmap.groupby('band', sort=True):
        idx = np.flatnonzero(bands == band)
        if len(idx):
            cum = np.cumsum(g['weight'].to_numpy())
            pick = np.minimum(np.searchsorted(cum, u[idx] * cum[-1], side='right'), len(cum) - 1)
            values[idx] = g['value'].to_numpy()[pick]
    assert not (np.isnan(values) & (bands >= 0)).any(), 'records in bands the map lacks'
    return values


def draw_dollars(df, maps):
    """Dollars for every banded amount, then the pieces combined (see the top)."""
    seed = common.SETTINGS['dollars']['seed']
    out = df.copy()
    for i, v in enumerate(ORDER):
        if f'{v}_band' in df:
            assert v in maps, f'no dollar map for {v}'
            out[v] = draw_one(df[f'{v}_band'].to_numpy(), maps[v], np.random.default_rng([seed, i]))
    remainder = out['other_agi'].to_numpy(float).copy()
    out['capital_gains_line13'] = out[KG_PIECES].sum(axis=1)
    out['capital_gains'] = out['kg_st'] + out['kg_lt'] + out['kg_other']
    out['other_agi'] = out['other_agi'] + out['kg_dist']
    parts = META['computed_totals']['business_income']
    out['business_income_drawn'] = out['business_income']
    out['business_income'] = out[list(parts)].to_numpy(float) @ np.array(list(parts.values()), float)
    for sub in ['sch_cf_income', 'sch_e_income']:
        sp = META['computed_totals'][sub]
        out[sub] = out[list(sp)].to_numpy(float) @ np.array(list(sp.values()), float)
    out['other_agi_remainder'] = remainder
    out['other_agi'] = out['other_agi'] + out[list(FOLD)].to_numpy(float) @ np.array(list(FOLD.values()), float)
    assert not out[list(FOLD) + KG_PIECES].isna().any().any(), 'a part is missing on a record'
    cats = list(META['cex_amounts'])
    if all(c in out for c in cats):
        out['total_consumption'] = out[cats].to_numpy(float).sum(1)
    for slot, (item, base) in RATIOS.items():      # after the totals above, so a share multiplies the final total
        if f'{slot}_band' in df:
            out[item] = out[slot] * out[base]
    for item, (total, other) in REMAINDERS.items():
        if other in out:
            out[item] = out[total] - out[other]
            assert np.allclose(out[item] + out[other], out[total], rtol=1e-12, atol=1e-6), item
    for a in AGE_SLOTS:                            # in years, -1 none
        if a in out:
            out[a] = out[a] - 1
    if all(f'{c}_band' in df for c in META['scf_amounts']):
        assert not out[list(NET_WORTH_PARTS)].isna().any().any(), 'a balance-sheet item is missing on a record'
        out['net_worth'] = out[list(NET_WORTH_PARTS)].to_numpy(float) @ np.array(list(NET_WORTH_PARTS.values()), float)
    return out

