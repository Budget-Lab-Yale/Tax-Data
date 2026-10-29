"""Dollars for generated records.

The model works in bands. A record's dollars in a band are drawn from that
band's real records ("the map", built by build_table.py): the PUF's dollar
values for PUF variables, the SCF's for the balance sheet, the CEX's for
consumption, each record's chance of being picked proportional to its weight.
The target year's maps are the PUF year's grown to the target year
(build_table.build_maps); negative capital gains are not grown (the $3,000 cap
is nominal).

The balance sheet has one map per SCF wave (map_scf_wave<wave>_<year>): each
wave's own records, grown to the target year per tax unit, item by item
(build_table.build_maps). A record's wave is its column scf_wave (generate.py
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
<target year>, since 28 October): that year's own records, grown to the target year (build_table.build_maps). A record's
year of each survey is its column cex_year, cps_year or sipp_year (generate.py draws them with the shares of the year
file's prior); its bands become dollars from that year's map. A frame without the column draws from the single-year
map (map_<survey>_<target year>), as before.

After the draws, the pieces are put on the definitions the IRS targets use:
  capital_gains   the Schedule D net, kg_st + kg_lt + kg_other (the line-13
                  total, with kg_dist, is kept as capital_gains_line13)
  business_income the exact signed sum of its eighteen parts
  dividends       qualified plus other dividends (div_pref + div_ord), exactly
  adjustments     the exact sum of its eleven parts (since 27 October)
  other_agi       AGI minus wages, interest, dividends and capital gains:
                  the drawn remainder plus kg_dist plus the AGI items folded
                  back in (FOLD); the remainder alone is other_agi_remainder
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
                  40 (since 28 October; Tax-Simulator's names): the drawn
                  overtime in dollars (ot_primary, ot_spouse, from the CPS's
                  map), at most that adult's wages less that adult's tips
                  (wages1 - tips1, never below zero; decided 27 October: the
                  tips are made first and the overtime gives way);
                  ot = ot1 + ot2 exactly
  auto_int_exp    the auto loan interest drawn (an SCF amount, from the
                  record's own wave's map)
  elective_deferrals  the unit's elective deferrals to 401(k)-type plans,
                  traditional plus Roth, as drawn (an SCF amount, from the
                  record's own wave's map, grown to the target year's wave;
                  since 28 October), split between the two people by the
                  drawn spouse_deferral_share (elective2; elective1 the rest),
                  then, with the file's year given (draw_dollars' year;
                  decided 27 October), each person's held to that year's legal
                  limit for their age and to their own wages (wages1, wages2:
                  none without wages, as for tips); elective1 and elective2 are
                  the held amounts. trad_contr_er1, trad_contr_er2 (Tax-
                  Simulator's names): each person's traditional part, elective
                  x (1 - the year's Roth share for joint or non-joint returns,
                  settings deferrals); trad_deferrals their sum; medicare_wages
                  wages (box 1, which holds the Roth part) plus trad_deferrals
  CPS ages        age_primary, age_spouse, age_dep1-3 in years, -1 for none
  ssn1, ssn2, dep_ssn1-3  (since 28 October; John, NEXT_STEPS.md item 1, Q4)
                  whether each person has a Social Security number, 1 or 0,
                  from the drawn CPS status (ssn_status_primary, _spouse,
                  _dep1-3: 1 citizen, 2 noncitizen with a sign of lawful
                  status, 3 noncitizen without one): 0 for status 3, else 1.
                  dep_ssn1-3 follow the dependents' ages (youngest first) and
                  are Tax-Simulator's names, which it reads for three
                  dependents; missing (NA) where there is no such dependent,
                  as Tax-Simulator expects. ssn2 is missing without a spouse.
                  ssn1 is missing on the dependent filers' records, whose
                  survey items are all none (generate.structural); the non-
                  microdata imputations give them one. Tax-Simulator has no
                  column for ssn1 and ssn2 yet (NEXT_STEPS.md step 2).
"""
import glob
import os

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
# Amounts added since 26 October go at the very end, so no earlier amount's stream moves: auto loan interest (an SCF
# amount, but not in the SCF's place in the list) and the SIPP's tips.
LATER = ['auto_int_exp'] + list(META.get('sipp_amounts', []))
# Elective deferrals (28 October), an SCF amount, after stage 3's items at the very end, so no earlier stream moves
NEWEST = ['elective_deferrals', 'spouse_deferral_share']   # the spouse's share since 28 October, after it
# Overtime (28 October), the CPS's dollar slots for each adult, at the very end after the elective deferrals
OVERTIME = list(META['cps_structure'].get('overtime', []))
# The SCF amounts of the retrain of 29 October (the retirement accounts' stock share; the QBI items' outside workers),
# at the very end after the overtime, so no earlier stream moves
RETRAIN_1029 = ['w_dc_stock_share', 'biz_workers_schc', 'biz_workers_part', 'biz_workers_scorp']
ORDER = ORDER + ['mortgage_interest', 'real_estate_tax'] + [v for v in META['scf_amounts'] if v not in LATER + NEWEST + RETRAIN_1029] \
    + list(META['cex_amounts']) + ['ss_gross'] + [v for v in META['cps_amounts'] if v not in OVERTIME] + LATER
# Stage 3 (27 October): the PUF items added then, at the very end, in the table's item order
ORDER = ORDER + [v for v in META['items'] if v not in ORDER and v not in META['computed_totals']]
ORDER = ORDER + [v for v in NEWEST if v in META['scf_amounts']] + OVERTIME
ORDER = ORDER + [v for v in RETRAIN_1029 if v in META['scf_amounts']]
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
AGI_PARTS = ['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']


def agi(df):
    """AGI of records whose dollars draw_dollars has put on the IRS targets' definitions."""
    return df[AGI_PARTS].to_numpy(float).sum(1)


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


def draw_one(bands, vmap, rng, u=None):
    """Dollars for bands from the map rows vmap. u: each record's stored pick (see the top); None draws from rng."""
    u = rng.random(len(bands)) if u is None else np.asarray(u, float)
    values = np.full(len(bands), np.nan)
    for band, g in vmap.groupby('band', sort=True):
        idx = np.flatnonzero(bands == band)
        if len(idx):
            cum = np.cumsum(g['weight'].to_numpy())
            pick = np.minimum(np.searchsorted(cum, u[idx] * cum[-1], side='right'), len(cum) - 1)
            values[idx] = g['value'].to_numpy()[pick]
    assert not (np.isnan(values) & (bands >= 0)).any(), 'records in bands the map lacks'
    return values


def draw_by_wave(df, v, maps, rng, u):
    """One balance-sheet amount, each record from its own SCF wave's map (column scf_wave)."""
    assert WAVE_MAPS in maps, 'no SCF wave maps loaded'
    assert 'scf_wave' in df, f'{v}: the frame says nothing of which SCF wave each record was drawn from (scf_wave)'
    bands, wave = df[f'{v}_band'].to_numpy(), df['scf_wave'].to_numpy()
    assert not pd.isna(wave).any(), f'{int(pd.isna(wave).sum())} records with no SCF wave'
    u = rng.random(len(bands)) if u is None else np.asarray(u, float)
    values = np.full(len(bands), np.nan)
    for w in np.unique(wave):
        assert int(w) in maps[WAVE_MAPS] and v in maps[WAVE_MAPS][int(w)], f'no dollar map for {v} in the {int(w)} wave'
        idx = np.flatnonzero(wave == w)
        values[idx] = draw_one(bands[idx], maps[WAVE_MAPS][int(w)][v], None, u[idx])
    return values


def draw_by_year(df, v, maps, rng, u):
    """One CEX, CPS or SIPP amount, each record from the map of its own year of that survey (column <survey>_year)."""
    src = SURVEY_OF[v]
    assert YEAR_MAPS in maps and src in maps[YEAR_MAPS], f'no {src} maps by year loaded'
    bands, year = df[f'{v}_band'].to_numpy(), df[f'{src}_year'].to_numpy()
    assert not pd.isna(year).any(), f'{int(pd.isna(year).sum())} records with no {src} year'
    u = rng.random(len(bands)) if u is None else np.asarray(u, float)
    values = np.full(len(bands), np.nan)
    for y in np.unique(year):
        assert int(y) in maps[YEAR_MAPS][src] and v in maps[YEAR_MAPS][src][int(y)], f'no dollar map for {v} in {src} {int(y)}'
        idx = np.flatnonzero(year == y)
        values[idx] = draw_one(bands[idx], maps[YEAR_MAPS][src][int(y)][v], None, u[idx])
    return values


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
    """In place (since 28 October; the person rules decided 27 October), after the ages are in years: each person's
    elective deferrals, the drawn unit total split by the drawn spouse's share (elective2 = share x total, elective1 the
    rest; all to the primary filer when the frame has no share), each held to the year's legal limit for that
    person's age (settings deferrals.limits; the catch-up limit from catch_up_age, age_primary and age_spouse) and to
    that person's own wages (wages1, wages2: none without wages, as for tips); elective1 and elective2 are the held
    amounts. Then each person's traditional part (Tax-Simulator's trad_contr_er1, trad_contr_er2: elective x (1 - the
    year's Roth share for joint or non-joint returns), translate.traditional_deferrals), their sum trad_deferrals, and
    Medicare wages, wages (box 1, which holds the Roth part) plus trad_deferrals: the payroll tax base for wages. The
    drawn total stays in elective_deferrals. Prints what each hold takes off. A frame without the drawn deferrals, or
    with no year, gets none of these."""
    if 'elective_deferrals' not in out or year is None:
        return out
    import translate as TR
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
    for k, age_col in [(1, 'age_primary'), (2, 'age_spouse')]:
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
    remainder = out['other_agi'].to_numpy(float).copy()
    out['capital_gains_line13'] = out[KG_PIECES].sum(axis=1)
    out['capital_gains'] = out['kg_st'] + out['kg_lt'] + out['kg_other']
    out['other_agi'] = out['other_agi'] + out['kg_dist']
    parts = META['computed_totals']['business_income']
    out['business_income_drawn'] = out['business_income']
    out['business_income'] = out[list(parts)].to_numpy(float) @ np.array(list(parts.values()), float)
    for sub in [k for k in META['computed_totals'] if k != 'business_income']:   # the schedules, dividends, adjustments
        sp = META['computed_totals'][sub]
        if sub in ('dividends', 'adjustments') and not all(f'{p}_band' in df for p in sp):
            # a frame with the total's band and not its parts' (qgroups.band_agi, whose AGI group label reads the
            # two totals' bands, as before stage 3) keeps the total's own dollars
            assert f'{sub}_band' in df, f'{sub}: neither its band nor its parts\' bands'
            continue
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
    person_items(out, df)
    for a in AGE_SLOTS:                            # in years, -1 none
        if a in out:
            out[a] = out[a] - 1
    ssn_items(out)
    deferral_items(out, year)                      # after the ages: the legal limit depends on each person's
    if all(f'{c}_band' in df for c in META['scf_amounts']):
        assert not out[list(NET_WORTH_PARTS)].isna().any().any(), 'a balance-sheet item is missing on a record'
        out['net_worth'] = out[list(NET_WORTH_PARTS)].to_numpy(float) @ np.array(list(NET_WORTH_PARTS.values()), float)
    return out

