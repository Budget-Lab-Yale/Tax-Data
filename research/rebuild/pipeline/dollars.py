"""Dollars for generated records.

The model works in bands. A record's dollars in a band are drawn from that
band's real records ("the map", built by build_table.py): the PUF's dollar
values for PUF variables, the SCF's for net worth, each record's chance of
being picked proportional to its weight. The 2019 map is the 2015 map grown
by the IRS growth per return holding each item (build_table.py); negative
capital gains are not grown (the $3,000 cap is nominal).

After the draws, the pieces are put on the definitions the IRS targets use:
  capital_gains   the Schedule D net, kg_st + kg_lt + kg_other (the line-13
                  total, with kg_dist, is kept as capital_gains_line13)
  business_income the exact signed sum of its eighteen parts
  other_agi       AGI minus wages, interest, dividends and capital gains:
                  the drawn remainder plus kg_dist plus the AGI items folded
                  back in (FOLD); the remainder alone is other_agi_remainder
"""
import json

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
KG_PIECES = ['kg_st', 'kg_lt', 'kg_dist', 'kg_other']
FOLD = {'business_income': 1, 'other_gains': 1, 'ira_taxable': 1, 'pensions_taxable': 1, 'ss_taxable': 1,
        'unemployment': 1, 'alimony': 1, 'state_refunds': 1, 'adjustments': -1}


def business_parts():
    return json.load(open(common.EDGES))['meta']['computed_totals']['business_income']


def load_maps(year):
    """{variable: map rows} for a year: the PUF map, plus the SCF's net worth map."""
    split = lambda m: {v: g for v, g in m.groupby('variable')}
    maps = split(pd.read_parquet(f'{common.MAPS}/map_puf_{year}.parquet'))
    maps['net_worth'] = split(pd.read_parquet(f'{common.MAPS}/map_scf_2015.parquet'))['net_worth']
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
        if v in maps and f'{v}_band' in df:
            out[v] = draw_one(df[f'{v}_band'].to_numpy(), maps[v], np.random.default_rng([seed, i]))
    remainder = out['other_agi'].to_numpy(float).copy()
    out['capital_gains_line13'] = out[KG_PIECES].sum(axis=1)
    out['capital_gains'] = out['kg_st'] + out['kg_lt'] + out['kg_other']
    out['other_agi'] = out['other_agi'] + out['kg_dist']
    parts = business_parts()
    out['business_income_drawn'] = out['business_income']
    out['business_income'] = out[list(parts)].to_numpy(float) @ np.array(list(parts.values()), float)
    out['other_agi_remainder'] = remainder
    out['other_agi'] = out['other_agi'] + out[list(FOLD)].to_numpy(float) @ np.array(list(FOLD.values()), float)
    assert not out[list(FOLD) + KG_PIECES].isna().any().any(), 'a part is missing on a record'
    return out


def grow_values(variable, x, factor):
    """2019 dollars from 2015: every nonzero value times the factor, except
    negative capital gains (the $3,000 loss cap is nominal in both years)."""
    x = np.array(x, dtype=float)
    x[(x > 0) if variable == 'capital_gains' else (x != 0)] *= factor
    return x
