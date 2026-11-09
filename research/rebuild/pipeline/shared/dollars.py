"""Dollars for generated records.

The networks draw each amount as a band. draw_dollars turns a band into dollars by picking one of the band's real
records in the source's dollar map, with chance proportional to its weight, then builds totals and each person's items
from the parts. A record's stored uniform number (column u_<variable>) fixes the pick, so the record keeps the same real
record's amount in every year. Balance-sheet amounts come from the map of the record's SCF wave, and CEX, CPS and SIPP
amounts from the map of the record's survey year. The rules are in FEATURES.md, "Dollars in the target year".
"""
import glob
import os

import numpy as np
import pandas as pd

from shared import common
from shared import translate as TR

# The order fixes each amount's random stream and must not change. Amounts that are not drawn (kg_other,
# retirement_gross, ss_taxable) keep their places, and a new amount goes at the end, so no other amount's stream moves.
META = common.meta()
OVERTIME = list(META['cps_structure'].get('overtime', []))
# Amounts placed after all others, in this order
TAIL = ['elective_deferrals', 'spouse_deferral_share', *OVERTIME, 'w_dc_stock_share', 'biz_workers_cf', 'biz_workers_ps',
        'nontxbl_pens_dist', 'inheritance']
# The CPS's amounts other than overtime, with the PUF's shares right after the spouse's wage share
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
PERSON_CAPS = META.get('person_caps', {})         # slot: [item, its cap, an amount taken off the cap (optional)]
SUMS = META.get('sums', {})                       # item: its parts (tips = tips1 + tips2)
FLAGS_WITH = META.get('flags_with', {})           # item: [yes/no slot, the amount it holds with] (0 without it)
AGE_SLOTS = META['cps_structure']['ages']         # the CPS's exact ages: value 0 none, else the age + 1
KG_PIECES = META['kg_pieces']
FOLD = META['agi_signs']                          # the AGI items folded into other_agi, with their signs
AGI_PARTS = ['wages', 'txbl_int', 'dividends', 'capital_gains', 'other_agi']
# What taxable Social Security reads besides AGI and the filing status
SS_INPUTS = ['gross_ss', 'exempt_int', 'sl_int_ded']


def agi(df):
    """AGI of records that have been through draw_dollars."""
    return df[AGI_PARTS].to_numpy(float).sum(1)


def retirement_items(out, in_agi=False):
    """In place: gross pensions, the sum of its two parts, where the frame has both; and taxable Social Security by the
    section 86 worksheet, added to other_agi so that it is in AGI. With in_agi, other_agi already holds the record's
    ss_taxable, which is taken out before the worksheet runs again. Returns out."""
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
    """The sum of columns with their signs; parts is {column: sign}."""
    return df[list(parts)].to_numpy(float) @ np.array(list(parts.values()), float)


SCF_SET = set(META['scf_amounts'])
WAVE_MAPS = 'scf waves'                           # load_maps' key for {wave: {variable: map rows}}
YEAR_MAPS = 'survey years'                        # load_maps' key for {survey: {tax year: {variable: map rows}}}
YEAR_SURVEYS = {'cex': set(META['cex_amounts']), 'cps': set(META['cps_amounts']), 'sipp': set(META.get('sipp_amounts', []))}
SURVEY_OF = {v: src for src, vs in YEAR_SURVEYS.items() for v in vs}   # amount: the survey whose year picks its map


def load_maps(year):
    """A year's dollar maps, {variable: map rows}, with each SCF wave's maps under WAVE_MAPS and each survey year's
    under YEAR_MAPS."""
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
    """{band: (cumulative weights, values)} of the map rows vmap, kept on the frame after the first call."""
    by = vmap.attrs.get('by_band')
    if by is None:
        by = {band: (np.cumsum(g['weight'].to_numpy()), g['value'].to_numpy()) for band, g in vmap.groupby('band', sort=True)}
        vmap.attrs['by_band'] = by
    return by


def draw_one(bands, vmap, rng, u=None):
    """Dollars for bands from the map rows vmap. u holds each record's stored uniform number; None draws from rng."""
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
    """One amount, each record from the map of its own group (an SCF wave or a survey year, held in column)."""
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
    """A person's amount x held to at most cap less `less`, never below zero. Returns (held, by_cap, by_less), where
    by_cap is what the cap alone takes off and by_less what subtracting `less` takes off on top. Example: wages 50,000,
    tips 20,000, drawn overtime 40,000: held 30,000, by_cap 0, by_less 10,000."""
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
    """In place: each person's tips held to that person's wages, and overtime to wages less tips, since both are part
    of W-2 wages; then the sums (tips = tips1 + tips2) and the flags that hold only with an amount (tips_lh1 is 0 where
    tips1 is 0). Prints what the caps take off. An item is made only where the frame has its band."""
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
    """In place, after the ages are in years: each person's elective deferrals (the drawn unit total split by the
    drawn spouse share), held to the year's legal limit for that person's age and to that person's wages. Then each
    person's traditional part, their sum trad_deferrals, and Medicare wages (box 1 wages, which hold the Roth part, plus
    traditional deferrals). Nothing is made without a year."""
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
    """In place: ssn1, ssn2 and dep_ssn1-3 from the drawn CPS statuses. 1 for a citizen or a noncitizen with a sign of
    lawful status (status 1 or 2), 0 for other noncitizens (status 3), missing where there is no such person."""
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


OWN = 'own_'   # prefix of a real PUF record's own amounts, when the filers are the real PUF records


def own_values(out, df, year):
    """In place, when the filers are the real PUF records: each real record's PUF amounts are its own PUF-year amounts
    grown to year as the PUF's dollar map is grown, in place of the draw from its band. The PUF's four aggregate
    records are not in the map, so this is how they get their amounts. The own_ columns are dropped."""
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
    """Each person's Schedule SE net earnings: the couple's Schedules C and F plus part_se, split by the drawn
    self-employment share, times 92.35% where that is $400 or more. Returns (primary's, spouse's)."""
    E = META['earn_split']
    x = out['sch_cf_income'].to_numpy(float) + (out['part_se'].to_numpy(float) if 'part_se' in out else 0.0)
    s = out['spouse_se_share'].to_numpy(float) if 'spouse_se_share' in out else np.zeros(len(out))
    f, floor = float(E['se_factor']), float(E['se_floor'])
    e = lambda v: np.where(f * v >= floor, f * v, 0.0)
    return e((1 - s) * x), e(s * x)


def wage_base(year):
    """The Social Security wage base in year's dollars: the PUF year's, grown as wages are, so a record's wages stand
    against it as in the PUF year, which is the year its earnings split code describes."""
    base = float(META['earn_split']['se_wage_base'])
    if year is None or int(year) == common.PUF_YEAR:
        return base
    from shared import growth as GR
    return base * GR.factors('wages', int(year))[0]


def wage_share_bounds(wages, e1, e2, code, cap):
    """The range (low, high, unreachable) of the spouse's share of the couple's wages that puts the primary's share of
    earnings in the range of the earnings split code. low and high are NaN where there is no bound (code 0, or no
    wages); where no share reaches the range they are the nearest share (0 or 1), flagged. The PUF's code is built on
    E30400, so each person's self-employment earnings count only up to the room left under the wage base. Each end is
    found by bisection, since the primary's share falls as the spouse's wage share rises."""
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
    """In place: the spouse's wage share held inside the range its earnings split code allows (wage_share_bounds). A
    share outside the range is picked again, with its stored uniform number, among the real shares of its band's map
    that lie in the range; where the band has none, it is set to the nearest end of the range."""
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
    """Dollars for every banded amount, then the totals and each person's items built from them. year is the tax year
    of the maps' dollars; without it the deferral items are not made."""
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
    # Gains and losses are drawn before the loss limit; AGI's capital gain applies it.
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
            # A frame with the total's band but not its parts' bands keeps the total's own dollars.
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
    for a in AGE_SLOTS:                            # ages in years, -1 for none
        if a in out:
            out[a] = out[a] - 1
    ssn_items(out)
    deferral_items(out, year)                      # after the ages, since the legal limit depends on age
    if all(f'{c}_band' in df for c in META['scf_amounts']):
        assert not out[list(NET_WORTH_PARTS)].isna().any().any(), 'a balance-sheet item is missing on a record'
        out['net_worth'] = signed_sum(out, NET_WORTH_PARTS)
    return out

