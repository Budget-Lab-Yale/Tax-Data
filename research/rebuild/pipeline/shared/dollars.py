"""Dollars for generated records.

The model works in bands. A record's dollars in a band are drawn from that
band's real records ("the map", built by build_table.py): the PUF's dollar
values for PUF variables, the SCF's for the balance sheet, the CEX's for
consumption, each record's chance of being picked proportional to its weight.
The target year's maps are the PUF year's grown to the target year
(maps.build_maps); negative capital gains are not grown (the $3,000 limit
is nominal).

The balance sheet has one map per SCF wave (map_scf_wave<wave>_<year>): each
wave's own records, grown to the target year per tax unit, item by item
(maps.build_maps). A record's wave is its column scf_wave (generate.py
draws each record's balance sheet from one wave, by the year's mix), and its
balance-sheet bands, which are on that wave's band edges, become dollars from
that wave's map.

Stored picks. Generation stores, for each record and each amount, one uniform
number in [0, 1) (column u_<variable>). It selects the row within the band's
list of real records (rows in the map's order, each with a chance proportional
to its weight) in place of a fresh random number. Every year's map of a variable
has the same rows in the same order with the same weights, only the values
grown, so a record keeps the same real record's amount in every year and in
every step that turns its bands into dollars. A frame without the column (the
PUF's own records, say) draws a fresh number, as before.

The CEX's consumption, the CPS's items and the SIPP's tips have one map per survey year (map_<survey>_from<year>_to
<target year>): that year's own records, grown to the target year (maps.build_maps). A record's
year of each survey is its column cex_year, cps_year or sipp_year (generate.py draws them with the shares of the year
file's prior); its bands become dollars from that year's map. A frame without the column draws from the single-year
map (map_<survey>_<target year>), as before.

After the draws, the pieces are put on the definitions the IRS targets use:
  capital_gains   the Schedule D net as it enters AGI: kg_st + kg_lt, the
                  gains and losses before the $3,000 limit, a net loss held
                  to the limit ($1,500 on a separate return;
                  common.allowed_capital_gain); with kg_dist it is Form 1040
                  line 13, capital_gains_line13
  business_income the exact signed sum of its eighteen parts
  dividends       qualified plus other dividends (div_pref + div_ord), exactly
  adjustments     the exact sum of its eleven parts
  other_agi       AGI minus wages, interest, dividends and capital gains:
                  the drawn remainder plus kg_dist plus the AGI items folded
                  back in (FOLD) plus taxable Social Security; the remainder
                  alone is other_agi_remainder
  gross_pens_dist gross pensions, taxable pensions plus the non-taxable
                  rest (nontxbl_pens_dist), exactly (both drawn)
  ss_taxable      taxable Social Security, never drawn: the
                  section 86 worksheet on the record's own amounts, as the
                  tax calculator computes it (retirement_items)
  net_worth       the 14 assets less the 6 debts; the home mortgage is its
                  drawn share of the home's value times the home's dollars
  wages2, se2     the spouse's wages and self-employment income: the drawn
                  share times the couple's total (wages; Schedules C and F as
                  computed above); wages1 and se1, the primary filer's, are the
                  total less the spouse's, so the two add up to the total
  tips1, tips2    each adult's tips: the drawn tips in dollars (tips_primary,
                  tips_spouse, from the SIPP's map), at most that adult's
                  wages (wages1, wages2); tips = tips1 + tips2 exactly;
                  tips_lh1, tips_lh2 the leisure and hospitality flags, 0
                  where that adult has no tips
  ot1, ot2        each adult's overtime pay, the whole pay for the hours over
                  40 (Tax-Simulator's names): the drawn overtime in dollars
                  (ot_primary, ot_spouse, from the CPS's map), at most that
                  adult's wages less that adult's tips (wages1 - tips1, never
                  below zero: the tips are made first and the overtime gives
                  way); ot = ot1 + ot2 exactly
  auto_int_exp    the auto loan interest drawn (an SCF amount, from the
                  record's own wave's map)
  elective_deferrals  the unit's elective deferrals to 401(k)-type plans,
                  traditional plus Roth, as drawn (an SCF amount, from the
                  record's own wave's map, grown to the target year's wave),
                  split between the two people by the drawn
                  spouse_deferral_share (elective2; elective1 the rest),
                  then, with the file's year given (draw_dollars' year), each
                  person's held to that year's legal
                  limit for their age and to their own wages (wages1, wages2:
                  none without wages, as for tips); elective1 and elective2 are
                  the held amounts. trad_contr_er1, trad_contr_er2 (Tax-
                  Simulator's names): each person's traditional part, elective
                  x (1 - the year's Roth share for joint or non-joint returns,
                  settings deferrals); trad_deferrals their sum; medicare_wages
                  wages (box 1, which holds the Roth part) plus trad_deferrals
  CPS ages        age1, age2, dep_age1-3 in years, -1 for none
  ssn1, ssn2, dep_ssn1-3  whether each person has a Social Security number, 1 or 0,
                  from the drawn CPS status (ssn_status_primary, _spouse,
                  _dep1-3: 1 citizen, 2 noncitizen with a sign of lawful
                  status, 3 noncitizen without one): 0 for status 3, else 1.
                  dep_ssn1-3 follow the dependents' ages (youngest first) and
                  are Tax-Simulator's names, which it reads for three
                  dependents; missing (NA) where there is no such dependent,
                  as Tax-Simulator expects. ssn2 is missing without a spouse.
                  ssn1 is missing on the dependent filers' records, whose
                  survey items are all none (generate.structural); the non-
                  microdata imputations give them one.
"""
import glob
import os

import numpy as np
import pandas as pd

from shared import common
from shared import translate as TR

# The order fixes each amount's random stream; it must not change. An amount that is no longer drawn keeps its place (kg_other,
# retirement_gross, ss_taxable: a frame without its band skips it), and an amount added later goes at the very end, so no
# earlier stream moves.
META = common.meta()
OVERTIME = list(META['cps_structure'].get('overtime', []))
# the amounts added after the first lists, in the order they were added
TAIL = ['elective_deferrals', 'spouse_deferral_share', *OVERTIME, 'w_dc_stock_share', 'biz_workers_cf', 'biz_workers_ps',
        'nontxbl_pens_dist', 'inheritance']
# the CPS's amounts less the overtime, the shared PUF shares right after the spouse's wage share
_CPS_ORDER = [v for v in META['cps_amounts'] if v not in OVERTIME]
for _v in reversed(META.get('puf_shares', [])):
    _CPS_ORDER.insert(_CPS_ORDER.index('spouse_wage_share') + 1, _v)
ORDER = ['wages', 'txbl_int', 'dividends', 'capital_gains', 'other_agi', 'net_worth',
         'kg_st', 'kg_lt', 'kg_dist', 'kg_other',
         'business_income', 'retirement_gross', 'exempt_int', 'other_gains', 'txbl_ira_dist',
         'txbl_pens_dist', 'ss_taxable', 'ui', 'alimony', 'state_ref', 'adjustments',
         'sole_prop', 'part_passive', 'part_active', 'part_passive_loss', 'part_active_loss', 'part_179',
         'scorp_passive', 'scorp_active', 'scorp_passive_loss', 'scorp_active_loss', 'scorp_179',
         'rent', 'rent_loss', 'estate', 'estate_loss', 'farm_rent', 'sche_other', 'farm',
         'first_mort_int', 'salt_prop'] \
    + [v for v in META['scf_amounts'] if v not in TAIL and v != 'auto_int_exp'] + list(META['cex_amounts']) + ['gross_ss'] \
    + _CPS_ORDER + ['auto_int_exp'] + list(META.get('sipp_amounts', []))
ORDER += [v for v in META['items'] if v not in ORDER and v not in META['computed_totals'] and v not in TAIL]
ORDER += [v for v in TAIL if v in set(META['scf_amounts']) | set(OVERTIME) | set(META['items'])]
NET_WORTH_PARTS = META['net_worth_parts']
RATIOS = META['ratios']                           # slot: [dollar item, its base]
REMAINDERS = META['remainders']                   # item: [total, the other part]: the total less the other part
PERSON_CAPS = META.get('person_caps', {})         # slot: [item, its cap(, less)]: tips1 at most wages1; ot1 at most
#                                                   wages1 - tips1 (the third entry, made by an earlier entry)
SUMS = META.get('sums', {})                       # item: its parts (tips = tips1 + tips2)
FLAGS_WITH = META.get('flags_with', {})           # item: [yes/no slot, the amount it holds with] (0 without it)
AGE_SLOTS = META['cps_structure']['ages']         # the CPS's exact ages: value 0 none, else the age + 1
KG_PIECES = META['kg_pieces']
FOLD = META['agi_signs']                          # the AGI items folded into other_agi, with their signs
AGI_PARTS = ['wages', 'txbl_int', 'dividends', 'capital_gains', 'other_agi']
# What taxable Social Security reads besides AGI without it and the filing status (common.taxable_social_security)
SS_INPUTS = ['gross_ss', 'exempt_int', 'sl_int_ded']


def agi(df):
    """AGI of records whose dollars draw_dollars has put on the IRS targets' definitions."""
    return df[AGI_PARTS].to_numpy(float).sum(1)


def retirement_items(out, in_agi=False):
    """In place: the two computed retirement items.
      gross_pens_dist  gross pensions (Form 1040 line 5a) = taxable pensions + the non-taxable rest
                       (common.PENSION_PARTS), exactly; both parts at least zero. Made where the frame has both parts.
      ss_taxable       taxable Social Security by the section 86 worksheet (common.taxable_social_security) on the
                       record's own amounts: modified AGI is AGI without taxable benefits (wages, interest, dividends,
                       the Schedule D net, other AGI), plus tax-exempt interest, plus the student loan interest
                       deduction; the filing status sets the base amounts. Added to other_agi, so it is in the file's
                       AGI (dollars.agi). Every record, filers and non-filers, as the calculator computes it.
    in_agi: other_agi already holds the record's ss_taxable (a recomputation after an item of AGI changed, the refund
    rule of taxes.py): it is taken out before the worksheet runs again. Returns out."""
    if all(p in out for p in common.PENSION_PARTS):
        parts = out[common.PENSION_PARTS].to_numpy(float)
        assert np.isfinite(parts).all(), 'a pension part missing on a record'
        assert (parts >= 0).all(), 'a pension part below zero on a record'
        out[common.GROSS_PENSIONS] = parts.sum(1)
    need = AGI_PARTS + SS_INPUTS + ['filing_status'] + (['ss_taxable'] if in_agi else [])
    missing = [c for c in need if c not in out]
    assert not missing, f'taxable Social Security needs {missing}'
    other = out['other_agi'].to_numpy(float) - (out['ss_taxable'].to_numpy(float) if in_agi else 0.0)
    without = out[AGI_PARTS[:-1]].to_numpy(float).sum(1) + other
    magi = without + out['exempt_int'].to_numpy(float) + out['sl_int_ded'].to_numpy(float)
    taxable = common.taxable_social_security(out['gross_ss'].to_numpy(float), magi, out['filing_status'].to_numpy())
    out['ss_taxable'] = taxable
    out['other_agi'] = other + taxable
    return out


def signed_sum(df, parts):
    """The total made from its parts: parts is {column: sign} (business income's eighteen parts, net worth's assets
    and debts, the AGI items folded into other_agi), summed with their signs."""
    return df[list(parts)].to_numpy(float) @ np.array(list(parts.values()), float)


SCF_SET = set(META['scf_amounts'])
WAVE_MAPS = 'scf waves'                           # load_maps' key for {wave: {variable: map rows}}
YEAR_MAPS = 'survey years'                        # load_maps' key for {survey: {tax year: {variable: map rows}}}
YEAR_SURVEYS = {'cex': set(META['cex_amounts']), 'cps': set(META['cps_amounts']), 'sipp': set(META.get('sipp_amounts', []))}
SURVEY_OF = {v: src for src, vs in YEAR_SURVEYS.items() for v in vs}   # an amount: the survey whose year picks its map


def load_maps(year):
    """{variable: map rows} for a year: the PUF's, the CEX's consumption and the CPS's items; and under WAVE_MAPS
    each SCF wave's balance-sheet maps, {wave survey year: {variable: map rows}}."""
    split = lambda m: {v: g for v, g in m.groupby('variable')}
    maps = {}
    for source in ['puf', 'cex', 'cps', 'sipp']:
        maps.update(split(pd.read_parquet(f'{common.MAPS}/map_{source}_{year}.parquet')))
    files = sorted(glob.glob(f'{common.MAPS}/map_scf_wave*_{year}.parquet'))
    assert files, f'no SCF wave maps for {year} in {common.MAPS}'
    maps[WAVE_MAPS] = {int(os.path.basename(f).split('_')[2][len('wave'):]): split(pd.read_parquet(f)) for f in files}
    years = {}
    for src in YEAR_SURVEYS:
        for f in sorted(glob.glob(f'{common.MAPS}/map_{src}_from*_to{year}.parquet')):
            y = int(os.path.basename(f).split('_')[2][len('from'):])
            years.setdefault(src, {})[y] = split(pd.read_parquet(f))
    if years:
        maps[YEAR_MAPS] = years
    return maps


def by_band(vmap):
    """{band: (the rows' cumulative weights, their values)} of the map rows vmap, kept on the frame after the first call."""
    by = vmap.attrs.get('by_band')
    if by is None:
        by = {band: (np.cumsum(g['weight'].to_numpy()), g['value'].to_numpy()) for band, g in vmap.groupby('band', sort=True)}
        vmap.attrs['by_band'] = by
    return by


def draw_one(bands, vmap, rng, u=None):
    """Dollars for bands from the map rows vmap. u: each record's stored pick (see the top); None draws from rng."""
    u = rng.random(len(bands)) if u is None else np.asarray(u, float)
    values = np.full(len(bands), np.nan)
    by = by_band(vmap)
    for band in np.unique(bands):
        if band in by:
            cum, vals = by[band]
            idx = np.flatnonzero(bands == band)
            pick = np.minimum(np.searchsorted(cum, u[idx] * cum[-1], side='right'), len(cum) - 1)
            values[idx] = vals[pick]
    assert not (np.isnan(values) & (bands >= 0)).any(), 'records in bands the map lacks'
    return values


def _draw_by_group(df, v, column, maps_by_group, rng, u, what):
    """One amount, each record from the map of its own group (an SCF wave or a survey year, in column), maps_by_group
    {group: {variable: map rows}}; what names the group in the messages."""
    bands, group = df[f'{v}_band'].to_numpy(), df[column].to_numpy()
    assert not pd.isna(group).any(), f'{int(pd.isna(group).sum())} records with no {what}'
    u = rng.random(len(bands)) if u is None else np.asarray(u, float)
    values = np.full(len(bands), np.nan)
    for g in np.unique(group):
        assert int(g) in maps_by_group and v in maps_by_group[int(g)], f'no dollar map for {v} in {what} {int(g)}'
        idx = np.flatnonzero(group == g)
        values[idx] = draw_one(bands[idx], maps_by_group[int(g)][v], None, u[idx])
    return values


def draw_by_wave(df, v, maps, rng, u):
    """One balance-sheet amount, each record from its own SCF wave's map (column scf_wave)."""
    assert WAVE_MAPS in maps, 'no SCF wave maps loaded'
    assert 'scf_wave' in df, f'{v}: the frame says nothing of which SCF wave each record was drawn from (scf_wave)'
    return _draw_by_group(df, v, 'scf_wave', maps[WAVE_MAPS], rng, u, 'SCF wave')


def draw_by_year(df, v, maps, rng, u):
    """One CEX, CPS or SIPP amount, each record from the map of its own year of that survey (column <survey>_year)."""
    src = SURVEY_OF[v]
    assert YEAR_MAPS in maps and src in maps[YEAR_MAPS], f'no {src} maps by year loaded'
    return _draw_by_group(df, v, f'{src}_year', maps[YEAR_MAPS][src], rng, u, f'{src} year')


def person_cap(x, cap, less=None):
    """A person's drawn amount x (at least 0) held to at most cap less `less` (never below zero), and what each part
    of the rule takes off. Returns (held, by_cap, by_less): held = min(x, max(cap - less, 0)); by_cap, what the cap
    alone takes off, x - min(x, max(cap, 0)); by_less, what reducing the cap by `less` takes off on top of that (0
    where less is None). by_cap + by_less = x - held. Example: wages 50,000, tips 20,000, drawn overtime 40,000:
    held 30,000, by_cap 0, by_less 10,000."""
    x, c = np.asarray(x, float), np.maximum(np.asarray(cap, float), 0.0)
    assert (x >= 0).all(), 'a drawn amount below zero'
    by_wages = np.minimum(x, c)
    if less is None:
        return by_wages, x - by_wages, np.zeros(len(x))
    less = np.asarray(less, float)
    assert (less >= 0).all() and np.isfinite(less).all(), 'the amount the cap is reduced by is below zero or missing'
    held = np.minimum(x, np.maximum(np.asarray(cap, float) - less, 0.0))
    return held, x - by_wages, by_wages - held


def person_items(out, df):
    """In place, after the remainders: a person's amount in dollars held to at most a person's other amount (tips1 =
    the drawn tips_primary, at most wages1: a person's tips are part of that person's wages, as on Form W-2), or to
    that amount less one made before it (ot1 = the drawn ot_primary, at most wages1 - tips1: tips and overtime pay
    are both part of the person's wages; the tips are made first and the overtime gives way), then their sums (tips =
    tips1 + tips2, ot = ot1 + ot2) and the yes/no items that hold only with the item (tips_lh1: 0 where tips1 is 0).
    Prints how many people, and how many dollars, the cap takes off: records, and units by the frame's weight column
    when it has one; for a cap less another amount, what the wages alone take off and what the other amount takes off
    on top, separately. Each item is made only where the frame has its drawn amount's band column (a frame from before
    overtime has the tips and not the overtime), and a sum or flag only where its parts were made."""
    have = {slot: f'{slot}_band' in df for slot in PERSON_CAPS}
    if not any(have.values()):
        return out
    w = out['weight'].to_numpy(float) if 'weight' in out else None
    made = set()
    for slot, (item, cap, *less) in PERSON_CAPS.items():
        if not have[slot]:
            continue
        assert len(less) <= 1, f'person_caps {slot}: at most one amount to subtract'
        less = less[0] if less else None
        assert less is None or less in made, f'{item}: its cap is {cap} less {less}, which must be made first'
        made.add(item)
        x = out[slot].to_numpy(float)
        held, by_cap, by_less = person_cap(x, out[cap].to_numpy(float),
                                           None if less is None else out[less].to_numpy(float))
        out[item] = held
        c = np.maximum(out[cap].to_numpy(float), 0.0)
        bound = by_cap > 0
        line = (f'{item} at most {cap}{"" if less is None else " less " + less}: {int((x > 0).sum()):,} records with '
                f'{slot} above zero; {cap} binds on {int(bound.sum()):,} of them ({int((bound & (c == 0)).sum()):,} with '
                f'no {cap}), taking off ${by_cap.sum() / 1e6:,.1f} million of ${x.sum() / 1e6:,.1f} million')
        if less is not None:
            line += (f'; subtracting {less} takes off ${by_less.sum() / 1e6:,.1f} million more, on '
                     f'{int((by_less > 0).sum()):,} records ({int(((by_less > 0) & ~bound).sum()):,} of them not held '
                     f'by {cap} alone)')
        line += ' (records unweighted)'
        if w is not None:
            line += (f'; weighted: {cap} binds on {w[bound].sum() / 1e6:,.3f} million of {w[x > 0].sum() / 1e6:,.3f} '
                     f'million people, ${(w * by_cap).sum() / 1e9:,.2f} billion of ${(w * x).sum() / 1e9:,.2f} billion')
            if less is not None:
                line += (f'; subtracting {less} takes off ${(w * by_less).sum() / 1e9:,.2f} billion more, on '
                         f'{w[by_less > 0].sum() / 1e6:,.3f} million people')
        common.say(line)
    for item, parts in SUMS.items():
        if all(p in made for p in parts):
            out[item] = out[parts].to_numpy(float).sum(1)
    for item, (flag, amount) in FLAGS_WITH.items():
        if amount not in made:
            continue
        out[item] = np.where(out[amount].to_numpy(float) != 0, out[flag].to_numpy(), 0).astype(np.int64)
    return out


def deferral_items(out, year):
    """In place, after the ages are in years: each person's
    elective deferrals, the drawn unit total split by the drawn spouse's share (elective2 = share x total, elective1 the
    rest; all to the primary filer when the frame has no share), each held to the year's legal limit for that
    person's age (settings deferrals.limits; the catch-up limit from catch_up_age, age1 and age2) and to
    that person's own wages (wages1, wages2: none without wages, as for tips); elective1 and elective2 are the held
    amounts. Then each person's traditional part (Tax-Simulator's trad_contr_er1, trad_contr_er2: elective x (1 - the
    year's Roth share for joint or non-joint returns), translate.traditional_deferrals), their sum trad_deferrals, and
    Medicare wages, wages (box 1, which holds the Roth part) plus trad_deferrals: the payroll tax base for wages. The
    drawn total stays in elective_deferrals. Prints what each hold takes off. A frame without the drawn deferrals, or
    with no year, gets none of these."""
    if 'elective_deferrals' not in out or year is None:
        return out
    D = common.SETTINGS['deferrals']
    assert str(year) in D['limits'], f'no legal limit for {year} in settings deferrals.limits'
    lim = D['limits'][str(year)]
    e = out['elective_deferrals'].to_numpy(float)
    assert not np.isnan(e).any() and (e >= 0).all(), 'elective deferrals missing or below zero on a record'
    married = out['married'].to_numpy()
    if 'elective2' in out:
        drawn = {1: out['elective1'].to_numpy(float), 2: out['elective2'].to_numpy(float)}
    else:
        drawn = {1: e, 2: np.zeros(len(e))}
    assert (drawn[2][married != 1] == 0).all(), 'a spouse\'s deferrals off a couple'
    w = out['weight'].to_numpy(float) if 'weight' in out else np.ones(len(out))
    lines = []
    trad = np.zeros(len(e))
    for k, age_col in [(1, 'age1'), (2, 'age2')]:
        assert age_col in out and f'wages{k}' in out, f'deferral_items needs {age_col} and wages{k}'
        age = out[age_col].to_numpy(float)
        limit = np.where(age >= D['catch_up_age'], lim[1], lim[0])
        x = drawn[k]
        held = np.minimum(x, limit)
        own = np.minimum(held, np.maximum(out[f'wages{k}'].to_numpy(float), 0.0))
        out[f'elective{k}'] = own
        out[f'trad_contr_er{k}'] = TR.traditional_deferrals(own, year, married)
        trad = trad + out[f'trad_contr_er{k}'].to_numpy(float)
        lines.append(f'person {k}: drawn ${(w * x).sum() / 1e9:,.2f} billion ({w[x > 0].sum() / 1e6:,.2f} million); the '
                     f'{year} limit takes ${(w * (x - held)).sum() / 1e9:,.2f} billion off {w[x > held].sum() / 1e6:,.3f} '
                     f'million; own wages take ${(w * (held - own)).sum() / 1e9:,.2f} billion off '
                     f'{w[held > own].sum() / 1e6:,.3f} million ({w[(held > 0) & (own == 0)].sum() / 1e6:,.3f} million '
                     f'with no wages); left ${(w * own).sum() / 1e9:,.2f} billion ({w[own > 0].sum() / 1e6:,.2f} million)')
    out['trad_deferrals'] = trad
    out['medicare_wages'] = out['wages'].to_numpy(float) + trad
    common.say(f'elective deferrals ({year}; weights: the frame\'s weight column, else 1 per record):\n  '
               + '\n  '.join(lines) + f'\n  traditional, both people: ${(w * trad).sum() / 1e9:,.2f} billion')
    return out


SSN_COLUMNS = {'ssn_status_primary': 'ssn1', 'ssn_status_spouse': 'ssn2', 'ssn_status_dep1': 'dep_ssn1',
               'ssn_status_dep2': 'dep_ssn2', 'ssn_status_dep3': 'dep_ssn3'}


def ssn_items(out):
    """In place: ssn1, ssn2, dep_ssn1-3 from the drawn statuses (see the top): 1 has a number (status 1 or 2), 0 has
    none (status 3), missing where there is no such person (status 0). A frame without the statuses is left alone.
    Prints the weighted counts (weight column, else 1 per record)."""
    if not all(v in out for v in SSN_COLUMNS):
        return out
    w = out['weight'].to_numpy(float) if 'weight' in out else np.ones(len(out))
    lines = []
    for slot, col in SSN_COLUMNS.items():
        st = out[slot].to_numpy()
        assert np.isin(st, [0, 1, 2, 3]).all(), f'{slot}: a status outside 0-3'
        out[col] = np.where(st == 0, np.nan, (st != 3).astype(float))
        lines.append(f'{col}: {w[st == 0].sum() / 1e6:,.2f} million none, {w[st != 0].sum() / 1e6:,.2f} million '
                     f'people, {w[st == 3].sum() / 1e6:,.3f} million of them without a number')
    common.say('Social Security numbers (weighted):\n  ' + '\n  '.join(lines))
    return out


OWN = 'own_'   # a real PUF record's own PUF-year amount of an item (filers real_puf, generate.real_puf_part)


def own_values(out, df, year):
    """In place (settings generation.filers real_puf): on a real PUF record (a frame column own_<item> that is not
    missing), each PUF amount is the record's own PUF-year amount grown to year exactly as the PUF's dollar map is
    grown (maps.puf_target_map: positive values times the item's factor for positive values, negative values
    times its factor for negative values, zero left at zero; growth.factors), in place of the draw from its band.
    For an ordinary PUF record this is the value its own row of the map holds; the PUF's four aggregate records are
    not in the map, so this is the only way they get their amounts. Records with no own amounts (generated records)
    are left as drawn. The own_ columns are not carried into out."""
    cols = [c for c in df.columns if c.startswith(OWN)]
    if not cols:
        return out
    from shared import growth as GR
    assert year is not None, 'a frame with real PUF amounts (own_ columns) needs the year of the maps\' dollars'
    for c in cols:
        v = c[len(OWN):]
        assert v in out, f'{c}: the frame has no column {v} to put the record\'s own amount in'
        x = df[c].to_numpy(float)
        real = ~np.isnan(x)
        if not real.any():
            continue
        fp, fn = (1.0, 1.0) if int(year) == common.PUF_YEAR else GR.factors(v, int(year))
        g = x.copy()
        g[x > 0] *= fp
        g[x < 0] *= fn
        y = out[v].to_numpy(float).copy()
        y[real] = g[real]
        out[v] = y
    out.drop(columns=[c for c in cols if c in out], inplace=True)
    return out


def couple_se_earnings(out):
    """Each person's Schedule SE net earnings (earn_split.py): the couple's Schedules C and F plus part_se, split by
    the drawn self-employment share (the calculator splits each item by it), 92.35% of each person's part where that is
    $400 or more. Before the cap under the wage base (wage_share_bounds). Returns (primary's, spouse's)."""
    E = META['earn_split']
    x = out['sch_cf_income'].to_numpy(float) + (out['part_se'].to_numpy(float) if 'part_se' in out else 0.0)
    s = out['spouse_se_share'].to_numpy(float) if 'spouse_se_share' in out else np.zeros(len(out))
    f, floor = float(E['se_factor']), float(E['se_floor'])
    e = lambda v: np.where(f * v >= floor, f * v, 0.0)
    return e((1 - s) * x), e(s * x)


def wage_base(year):
    """The wage base in the dollars of draw_dollars' year: the PUF year's, grown as wages are, so a drawn record's wages
    stand against the cap as they did in the PUF year, which is what its earnings split code describes."""
    base = float(META['earn_split']['se_wage_base'])
    if year is None or int(year) == common.PUF_YEAR:
        return base
    from shared import growth as GR
    return base * GR.factors('wages', int(year))[0]


def wage_share_bounds(wages, e1, e2, code, cap):
    """The spouse's share of the couple's wages that puts the primary's share of earnings in the code's range: (low,
    high, unreachable), low = high = NaN where there is no bound (code 0, or no wages); where no share reaches the
    range, the share that comes nearest (0 or 1), flagged. EARNSPLIT is built on E30400, so each person's earnings count
    only up to the room under the wage base: min(e, max(cap - own wages, 0)). A person's wages plus counted earnings
    fall as the person's wages fall, so the primary's share falls as the spouse's wage share rises, and each end of the
    range is found by halving. With an infinite cap this gives earn_split.wage_share_bounds."""
    rng = META['earn_split']['ranges']
    low = np.array([np.nan] + [rng[str(k)][0] for k in (1, 2, 3)])
    high = np.array([np.nan] + [rng[str(k)][1] for k in (1, 2, 3)])
    w = np.maximum(np.asarray(wages, float), 0.0)
    on = (code >= 1) & (code <= 3) & (w > 0)
    c = np.where(on, code, 1).astype(int)
    a, b = low[c], high[c]

    def primary_share(s):
        w1, w2 = w * (1 - s), w * s
        p1 = w1 + np.minimum(e1, np.maximum(cap - w1, 0.0))
        p2 = w2 + np.minimum(e2, np.maximum(cap - w2, 0.0))
        return p1 / np.where(p1 + p2 > 0, p1 + p2, 1.0)

    def first_at_most(limit):
        """The smallest s in 0-1 with primary_share(s) <= limit (1 where there is none)."""
        s0, s1 = np.zeros(len(w)), np.ones(len(w))
        for _ in range(50):
            mid = (s0 + s1) / 2
            ok = primary_share(mid) <= limit
            s1, s0 = np.where(ok, mid, s1), np.where(ok, s0, mid)
        return np.where(primary_share(np.zeros(len(w))) <= limit, 0.0, s1)

    def last_at_least(limit):
        """The largest s in 0-1 with primary_share(s) >= limit (0 where there is none)."""
        s0, s1 = np.zeros(len(w)), np.ones(len(w))
        for _ in range(50):
            mid = (s0 + s1) / 2
            ok = primary_share(mid) >= limit
            s0, s1 = np.where(ok, mid, s0), np.where(ok, s1, mid)
        return np.where(primary_share(np.ones(len(w))) >= limit, 1.0, s0)

    lo, hi = first_at_most(b), last_at_least(a)
    unreachable = on & ((primary_share(np.ones(len(w))) > b) | (primary_share(np.zeros(len(w))) < a))
    return np.where(on, lo, np.nan), np.where(on, hi, np.nan), unreachable


def wage_share_in_range(out, df, maps, year=None):
    """In place (earn_split.py): the spouse's share of the couple's wages held inside the range its
    earnings split code allows given the couple's wages and each person's Schedule SE earnings up to the wage base
    (wage_share_bounds).
    A share already inside keeps its value. One outside is picked again inside the range, among the real shares of its
    own band's map (its CPS year's, else the single-year map) that lie in the range, with its stored pick rescaled to
    them, so that within each band the shares follow the band's own distribution cut to the range; where none of the
    band's real shares lies in the range (generation chose the band at median dollars), the nearest end of the range.
    Where no share reaches the code's range, the nearest share (0 or 1). Prints the counts. A frame without the code,
    or without the wage share, is left alone."""
    if 'earn_split' not in df or 'spouse_wage_share_band' not in df or 'earn_split' not in META:
        return out
    code = df['earn_split'].to_numpy()
    e1, e2 = couple_se_earnings(out)
    lo, hi, unreachable = wage_share_bounds(out['wages'].to_numpy(float), e1, e2, code, wage_base(year))
    married = out['married'].to_numpy() == 1
    s = out['spouse_wage_share'].to_numpy(float).copy()
    on = married & ~np.isnan(lo)
    outside = on & ((s < lo - 1e-12) | (s > hi + 1e-12))
    bands = df['spouse_wage_share_band'].to_numpy()
    u = df['u_spouse_wage_share'].to_numpy(float) if 'u_spouse_wage_share' in df else np.full(len(df), 0.5)
    cps_year = df['cps_year'].to_numpy() if 'cps_year' in df and YEAR_MAPS in maps else None
    clipped = np.zeros(len(df), bool)
    idx_all = np.flatnonzero(outside)
    groups = pd.DataFrame({'y': cps_year[idx_all] if cps_year is not None else 0, 'b': bands[idx_all], 'i': idx_all})
    for (yy, bb), ii in groups.groupby(['y', 'b'])['i']:
        idx = ii.to_numpy()
        vmap = maps[YEAR_MAPS]['cps'][int(yy)]['spouse_wage_share'] if cps_year is not None else maps['spouse_wage_share']
        g = vmap[vmap['band'] == bb]
        x, wt = g['value'].to_numpy(float), g['weight'].to_numpy(float)
        o = np.argsort(x, kind='stable')
        x, F = x[o], np.concatenate([[0.0], np.cumsum(wt[o])])
        a = np.searchsorted(x, lo[idx] - 1e-12, side='left')      # the band's real shares inside [lo, hi]
        b = np.searchsorted(x, hi[idx] + 1e-12, side='right')
        has = b > a
        pos = F[a] + u[idx] * (F[b] - F[a])
        k = np.clip(np.searchsorted(F, pos, side='right') - 1, a, np.maximum(b - 1, a))
        pick = np.where(has, x[np.minimum(k, len(x) - 1)] if len(x) else 0.0, np.clip(s[idx], lo[idx], hi[idx]))
        s[idx] = np.clip(pick, lo[idx], hi[idx])
        clipped[idx] = ~has
    assert not (on & ((s < lo - 1e-9) | (s > hi + 1e-9))).any(), 'a wage share outside its range'
    out['spouse_wage_share'] = s
    w = out['weight'].to_numpy(float) if 'weight' in out else np.ones(len(out))
    common.say(f'wage share in the earnings split range: couples with a range {w[on].sum() / 1e6:,.2f} million '
               f'({int(on.sum()):,} records); drawn outside it {w[outside].sum() / 1e6:,.3f} million ({int(outside.sum()):,}), '
               f'of which picked again within the band {w[outside & ~clipped].sum() / 1e6:,.3f} million and set to the '
               f'nearest end of the range {w[clipped].sum() / 1e6:,.3f} million; no share reaches the range '
               f'{w[unreachable & married].sum() / 1e6:,.3f} million (weights: the frame\'s, else 1 per record)')
    return out


def draw_dollars(df, maps, year=None):
    """Dollars for every banded amount, then the pieces combined (see the top). A column u_<variable> is the record's
    stored pick; a balance-sheet amount comes from the record's own wave's map. year: the tax year of the maps'
    dollars, for the traditional deferrals and Medicare wages (deferral_items; None: not made)."""
    seed = common.SETTINGS['dollars']['seed']
    out = df.copy()
    for i, v in enumerate(ORDER):
        if f'{v}_band' in df:
            u = df[f'u_{v}'].to_numpy(float) if f'u_{v}' in df else None
            rng = np.random.default_rng([seed, i])
            if v in SCF_SET:
                out[v] = draw_by_wave(df, v, maps, rng, u)
            elif v in SURVEY_OF and f'{SURVEY_OF[v]}_year' in df:
                out[v] = draw_by_year(df, v, maps, rng, u)
            else:
                assert v in maps, f'no dollar map for {v}'
                out[v] = draw_one(df[f'{v}_band'].to_numpy(), maps[v], rng, u)
    own_values(out, df, year)
    remainder = out['other_agi'].to_numpy(float).copy()
    # the Schedule D gains and losses are drawn before the loss limit; AGI's capital gain applies it
    out['capital_gains'] = common.allowed_capital_gain(out['kg_st'] + out['kg_lt'],
                                                       out['filing_status'] if 'filing_status' in out else None)
    out['capital_gains_line13'] = out['capital_gains'] + out['kg_dist']
    out['other_agi'] = out['other_agi'] + out['kg_dist']
    parts = META['computed_totals']['business_income']
    out['business_income_drawn'] = out['business_income']
    out['business_income'] = signed_sum(out, parts)
    for sub in [k for k in META['computed_totals'] if k != 'business_income']:   # the schedules, dividends, adjustments
        sp = META['computed_totals'][sub]
        if sub in ('dividends', 'adjustments') and not all(f'{p}_band' in df for p in sp):
            # a frame with the total's band and not its parts' (qgroups.band_agi, whose AGI group label reads the
            # two totals' bands) keeps the total's own dollars
            assert f'{sub}_band' in df, f'{sub}: neither its band nor its parts\' bands'
            continue
        out[sub] = signed_sum(out, sp)
    out['other_agi_remainder'] = remainder
    out['other_agi'] = out['other_agi'] + signed_sum(out, FOLD)
    assert not out[list(FOLD) + KG_PIECES].isna().any().any(), 'a part is missing on a record'
    retirement_items(out)                          # gross pensions; taxable Social Security into other_agi
    cats = list(META['cex_amounts'])
    if all(c in out for c in cats):
        out['total_consumption'] = out[cats].to_numpy(float).sum(1)
    wage_share_in_range(out, df, maps, year)       # before the shares multiply their totals
    for slot, (item, base) in RATIOS.items():      # after the totals above, so a share multiplies the final total
        if f'{slot}_band' in df:
            out[item] = out[slot] * out[base]
    for item, (total, other) in REMAINDERS.items():
        if other in out:
            out[item] = out[total] - out[other]
            assert np.allclose(out[item] + out[other], out[total], rtol=1e-12, atol=1e-6), item
    person_items(out, df)
    for a in AGE_SLOTS:                            # in years, -1 none
        if a in out:
            out[a] = out[a] - 1
    ssn_items(out)
    deferral_items(out, year)                      # after the ages: the legal limit depends on each person's
    if all(f'{c}_band' in df for c in META['scf_amounts']):
        assert not out[list(NET_WORTH_PARTS)].isna().any().any(), 'a balance-sheet item is missing on a record'
        out['net_worth'] = signed_sum(out, NET_WORTH_PARTS)
    return out

