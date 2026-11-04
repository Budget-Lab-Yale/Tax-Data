"""prepare.py: the check tool's tables of records for one year: the posttrained file, and the sources it is checked
against, each on the pipeline's own definitions. Run once per file (and again after a change to settings.json's
year, runs or puf_source), in a Slurm job with the pipeline's modules (prepare.sbatch). The server (server.py) only
reads what this writes.

The year, the run folder and the source definition are settings.json's. Everything is computed by the pipeline's own
code, imported from research/rebuild/pipeline/ and run against that year's run folder (REBUILD_OUT, its settings):

  file  the posttrained file, <run>/tilt/<tilt_run>/file_<year>.parquet, every record on its own weight, with AGI
        computed as the pipeline computes it (dollars.agi).

  puf   the source for every PUF item, by settings.json's puf_source:
          puf_and_tax_data_nonfilers  every PUF return (dependent filers included) on its own weight and in its own
                                      dollars, from the training table's PUF rows (data/table.parquet), plus the
                                      non-filer units of the pipeline's tax data for the year (build_table.tax_data,
                                      as the table stage wrote it to data/tax_data_<year>.parquet). Only at the PUF's
                                      year (2015): it holds the PUF's own weights and dollars.
          tax_data                    the pipeline's tax data for the year exactly (data/tax_data_<year>.parquet):
                                      returns other than dependent filers, weights scaled by SOI Table 1.6, dollars
                                      grown per return, plus the non-filers; the PUF items it does not carry come from
                                      the same returns, grown by the same function (build_table.grown).
        The PUF's amounts are put on the file's definitions (capital gains with the loss limit, other income with the
        items folded in, AGI) by the same steps dollars.py applies to the file (on_file_definitions below). A return
        with no age range on the PUF is split over the ranges by step 0's probabilities, as the tax data does: one row
        per range, its weight times the probability; it counts as one sample record.
        The non-filers carry zero for the PUF items the CPS does not ask (the list is in the catalog's notes), as the
        file's non-filers do.

  scf   the SCF over the year's mix of waves, exactly the frame the SCF targets are computed on
        (scf_targets.frame: the balance sheet grown per tax unit to the year's wave, weights scaled to the year's
        units), each wave's weights times its share. Its income items (in PUF terms after the translation's function
        1, as on the training table's rows, before any fill-in) are grown from the wave's income year to the year per
        return by SOI's growth (build_table.growth, the rule build_table.grown applies).
  cex, cps, sipp
        the survey over the year's mix of survey years, exactly the frame the consumption targets and the CPS and SIPP
        reports use (survey_mix.mixed_units: every unit, its own items in the year's dollars by the dollar maps'
        factors), with its income items in PUF terms from the training table's rows (after function 1, before the
        fill-in), grown per return to the year as for the SCF.

Writes <cache>/<year>_<puf_source>/: file.parquet, puf.parquet, scf.parquet, cex.parquet, cps.parquet,
sipp.parquet, catalog.json (the variables, their plain names and kinds, which source each item's table uses, the
variables each source may be cut and filtered by, and the notes the page shows), and checks.md (the checks below).
The folder is readable by the owner and the group only (licensed PUF microdata).
"""
import json
import os
import sys
import time

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
S = json.load(open(os.path.join(HERE, 'settings.json')))
YEAR = int(S['year'])
RUN = S['runs'][str(YEAR)]
os.environ['REBUILD_OUT'] = RUN['out']
os.environ['REBUILD_SETTINGS'] = os.path.join(RUN['out'], 'settings.json')
os.environ['REBUILD_RUN'] = RUN['tilt_run']
sys.path.insert(0, os.path.abspath(os.path.join(HERE, '..', '..', 'pipeline')))

import common                       # noqa: E402  (the pipeline's modules, against the year's run folder)
import build_table as BT            # noqa: E402
import dollars as D                 # noqa: E402
import survey_mix                   # noqa: E402
import scf_targets                  # noqa: E402
import report_tilt                  # noqa: E402  (its plain names of the variables)

assert common.TARGET_YEAR == YEAR, f'the run folder {RUN["out"]} is for {common.TARGET_YEAR}, not {YEAR}'
PUF_YEAR = common.PUF_YEAR
META = common.meta()
PRESENT = META['present_on']
SOURCE_DEF = S['puf_source']
OUT = os.path.join(S['cache'], f'{YEAR}_{SOURCE_DEF}')
CHECKS = []


def say(*a):
    msg = ' '.join(str(x) for x in a)
    print(msg, flush=True)
    CHECKS.append(msg)


# ---- the variables ----------------------------------------------------------------------------------------------
AGE_PLAIN = ['primary filer under 26', 'primary filer 26-34', 'primary filer 35-44', 'primary filer 45-54',
             'primary filer 55-64', 'primary filer 65 and over', 'dependent filer under 18',
             'dependent filer 18-25', 'dependent filer 26 and over']
YES_NO = {0: 'no', 1: 'yes'}
COUNTS = {0: 'none', 1: 'one', 2: 'two', 3: 'three or more'}
CATEGORIES = {
    'filing_status': {1: 'single', 2: 'married filing jointly', 3: 'married filing separately', 4: 'head of household'},
    'married': {0: 'not married', 1: 'married'},
    'n_dep': COUNTS, 'n_dep_ctc': COUNTS, 'n_dep_eitc': COUNTS,
    'age_range': dict(enumerate(AGE_PLAIN)),
    'filer': {0: 'does not file', 1: 'files a return'},
    'dep_filer': {0: 'not a dependent filer', 1: 'dependent filer'},
    'k1_owner': YES_NO, 'owns_business': YES_NO, 'tips_lh_primary': YES_NO, 'tips_lh_spouse': YES_NO,
    'sex_primary': {1: 'man', 2: 'woman'}, 'sex_spouse': {1: 'man', 2: 'woman'},
    **{f'biz_industry_{k}': {c: f'industry code {c}' for c in range(9)} for k in ('schc', 'part', 'scorp')},
}
FLAGS = ['k1_owner', 'owns_business', 'tips_lh_primary', 'tips_lh_spouse']     # yes/no items offered as Y
AGES = ['age1', 'age2', 'dep_age1', 'dep_age2', 'dep_age3']                    # exact ages, in years
LABELS = dict(report_tilt.LABELS)
LABELS.update({
    'agi': 'adjusted gross income (wages, interest, dividends, capital gain, other income in AGI)',
    'capital_gains': 'capital gain or loss in AGI (Schedule D net, a net loss held to $3,000)',
    'capital_gains_all': 'capital gains before the loss limit, with distributions (the SCF\'s concept)',
    'other_agi': 'all other income in AGI (every AGI item but wages, interest, dividends and the capital gain)',
    'other_agi_remainder': 'other income: AGI less every item the file names',
    'div_pref': 'qualified dividends', 'div_ord': 'ordinary dividends other than qualified',
    'sche_rent': 'rents and royalties net (Schedule E)', 'sche_rest': 'Schedule E other than rents and royalties',
    'trad_contr_ira': 'IRA deduction', 'sl_int_ded': 'student loan interest deduction', 'ed_exp': 'educator expenses',
    'tuition_ded': 'tuition and fees deduction', 'dpad': 'domestic production activities deduction',
    'se_tax_ded': 'deductible part of self-employment tax', 'se_health': 'self-employed health insurance deduction',
    'hsa_contr': 'health savings account deduction', 'keogh_contr': 'self-employed retirement plans (SEP, SIMPLE)',
    'early_penalty': 'penalty on early withdrawal of savings', 'alimony_exp': 'alimony paid',
    'kg_1250': 'unrecaptured section 1250 gain', 'kg_collect': '28% rate (collectibles) gain',
    'part_se': 'partnership income subject to self-employment tax', 'med_exp': 'medical and dental expenses',
    'salt_inc_sales': 'state and local income or sales tax', 'char_cash': 'charitable gifts in cash',
    'char_noncash': 'charitable gifts other than cash', 'casualty_exp': 'casualty and theft losses',
    'job_exp': 'unreimbursed job expenses', 'tax_prep_exp': 'tax preparation fees',
    'other_misc_exp': 'other miscellaneous deductions', 'other_item_exp': 'other itemized deductions (residual)',
    'amt_nols': 'AMT net operating loss deduction', 'amt_ftc': 'AMT foreign tax credit', 'ftc': 'foreign tax credit',
    'aoc_exp': 'expenses for the American opportunity credit', 'llc_exp': 'expenses for the lifetime learning credit',
    'old_cred': 'credit for the elderly or disabled', 'savers_cred': 'retirement savings contributions credit',
    'res_energy_cred': 'residential energy credits', 'gbc': 'general business credit',
    'prior_yr_cred': 'prior year minimum tax credit', 'other_nonref': 'other nonrefundable credits',
    'net_ptc': 'net premium tax credit', 'excess_ptc': 'excess advance premium tax credit repayment',
    'recapture_tax': 'recapture taxes', 'ira_penalty': 'additional tax on IRAs and other plans',
    'auto_int_exp': 'auto loan interest (a year of interest on vehicle loans)',
    'w_primary_ltv': 'home mortgage as a share of the home\'s value',
    'married': 'married filing jointly or not', 'n_dep': 'dependents (0 to 3 or more)',
    'filing_status': 'filing status', 'n_dep_ctc': 'children for the child tax credit',
    'n_dep_eitc': 'children for the earned income credit', 'age_range': 'primary filer\'s age range',
    'filer': 'files a return', 'dep_filer': 'dependent filer (claimed on another return)',
    'k1_owner': 'holds a partnership, S corporation or LLC interest', 'owns_business': 'owns a private business',
    'age1': 'primary filer\'s age (80 means 80 and over)', 'age2': 'spouse\'s age (80 means 80 and over)',
    'dep_age1': 'youngest dependent\'s age', 'dep_age2': 'second dependent\'s age', 'dep_age3': 'third dependent\'s age',
    'sex_primary': 'primary filer\'s sex', 'sex_spouse': 'spouse\'s sex',
    'spouse_wage_share': 'spouse\'s share of the couple\'s wages', 'spouse_se_share': 'spouse\'s share of the couple\'s Schedule C and F income',
    'weeks_primary': 'primary filer\'s weeks worked', 'weeks_spouse': 'spouse\'s weeks worked',
    'hours_primary': 'primary filer\'s usual weekly hours', 'hours_spouse': 'spouse\'s usual weekly hours',
    'ot_primary': 'primary filer\'s overtime pay (as drawn, before the cap at wages less tips)',
    'ot_spouse': 'spouse\'s overtime pay (as drawn, before the cap)',
    'ot_drawn': 'overtime pay, both adults (as drawn, before the cap)',
    'tips_primary': 'primary filer\'s tips (as drawn, before the cap at wages)',
    'tips_spouse': 'spouse\'s tips (as drawn, before the cap)', 'tips_drawn': 'tips, both adults (as drawn, before the cap)',
    'tips_lh_primary': 'primary filer\'s tips mostly from leisure and hospitality',
    'tips_lh_spouse': 'spouse\'s tips mostly from leisure and hospitality',
    'value.primary_mortgage': 'mortgage on the primary home',
    **{f'biz_industry_{k}': f'industry of the {w}' for k, w in
       [('schc', 'Schedule C business'), ('part', 'partnership'), ('scorp', 'S corporation')]},
})

# the amounts each survey's rows carry in PUF terms (the translation's shared items): present on that survey and on
# the PUF (an exact age or a category is not an amount)
AMOUNTS = set(BT.AMOUNTS) | {'net_worth', 'value.primary_mortgage'}
SHARED = {s: sorted(v for v, on in PRESENT.items() if s in on and 'puf' in on and v in AMOUNTS)
          for s in ['scf', 'cex', 'cps', 'sipp']}
OWN = {s: sorted(v for v, on in PRESENT.items() if on == [s] and v in AMOUNTS) for s in ['scf', 'cex', 'cps', 'sipp']}


def survey_growth(df, cols, base):
    """The amounts cols of df, in year-base dollars, grown to the year per return: positive values by the item's
    positive factor, negative values by its negative one (build_table.growth; the rule build_table.grown applies,
    here also to the computed totals, which a survey carries without their parts)."""
    if base == YEAR:
        return df
    # a computed total whose parts the frame carries is made from its grown parts afterwards, as build_table.grown does
    made = [v for v in cols if v in BT.COMPUTED and all(p in df for p in BT.COMPUTED[v])]
    for v in [c for c in cols if c not in made]:
        x = df[v].to_numpy(float)
        out = x.copy()
        for k, m in enumerate([x > 0, x < 0]):
            if m.any():
                f = BT.growth(v, YEAR)[k] / BT.growth(v, base)[k]
                assert np.isfinite(f) and f > 0, f'{v}: no growth factor {base} to {YEAR}'
                out[m] = x[m] * f
        df[v] = out
    for v in made:
        df[v] = sum(s * df[p] for p, s in BT.COMPUTED[v].items())
    return df


def on_file_definitions(df):
    """The training table's PUF concepts put on the file's definitions, by the steps dollars.py (draw_dollars) applies
    after the draws: the capital gain in AGI is the Schedule D net with a net loss held to the limit
    (common.allowed_capital_gain), line 13 adds the distributions, other income in AGI is the remainder plus the
    distributions plus the AGI items folded in with their signs (dollars.FOLD), and AGI is dollars.agi. The
    table's own capital gain (all three pieces before the limit) is kept as capital_gains_all."""
    for v, parts in META['computed_totals'].items():           # totals from their parts, as dollars.py makes them
        if v not in df and all(p in df for p in parts):
            df[v] = D.signed_sum(df, parts)
    kg_st, kg_lt, kg_dist = (df[c].to_numpy(float) for c in D.KG_PIECES)
    rem = df['other_agi'].to_numpy(float).copy()
    df['capital_gains_all'] = kg_st + kg_lt + kg_dist
    df['capital_gains'] = common.allowed_capital_gain(kg_st + kg_lt, df['filing_status'].to_numpy())
    df['capital_gains_line13'] = df['capital_gains'] + kg_dist
    df['other_agi_remainder'] = rem
    df['other_agi'] = rem + kg_dist + D.signed_sum(df, D.FOLD)
    df['agi'] = D.agi(df)
    return df


def split_age_ranges(df, P):
    """One row per record and age range it may hold: a record with a known range keeps it; a record without one (the
    PUF's step 0 records) is split over the ranges by its probabilities P (records x 9), weight times probability.
    The column rec keeps the record, so it counts once."""
    known = df['age_range'].to_numpy() >= 0
    assert np.allclose(P[~known].sum(1), 1), 'a record with no age range whose probabilities do not add to one'
    parts = [df[known]]
    for k in range(P.shape[1]):
        m = ~known & (P[:, k] > 0)
        if m.any():
            parts.append(df[m].assign(age_range=k, w=df.loc[m, 'w'].to_numpy() * P[m, k]))
    out = pd.concat(parts, ignore_index=True)
    assert np.isclose(out['w'].sum(), df['w'].sum())
    return out


# ---- the sources --------------------------------------------------------------------------------------------------
def puf_source(t):
    puf = t[t.source == 'puf'].reset_index(drop=True)
    P = puf[[f'age_p{k}' for k in range(common.N_RANGES)]].to_numpy(float)
    known = puf['age_range'].to_numpy()
    onehot = np.zeros_like(P)
    onehot[known >= 0, known[known >= 0]] = 1
    Pall = np.where((known >= 0)[:, None], onehot, P)
    dep = Pall[:, common.ADULT_RANGES:].sum(1) > 0
    assert np.allclose(Pall[dep, common.ADULT_RANGES:].sum(1), 1), 'a record split between filer and dependent ranges'
    T = pd.read_parquet(f'{common.DATA}/tax_data_{YEAR}.parquet')
    tf, tn = T[T.filer == 1].reset_index(drop=True), T[T.filer == 0].reset_index(drop=True)
    nodep = puf[~dep].reset_index(drop=True)
    say(f'PUF rows {len(puf):,} ({puf.weight.sum() / 1e6:.3f} million returns); dependent filers {dep.sum():,} '
        f'({puf.weight[dep].sum() / 1e6:.3f} million); tax data {YEAR}: filers {len(tf):,} ({tf.weight.sum() / 1e6:.3f} '
        f'million), non-filers {len(tn):,} ({tn.weight.sum() / 1e6:.3f} million)')
    assert len(nodep) == len(tf), 'the tax data\'s filers are not the PUF\'s returns other than dependent filers'
    # the tax data's filers are those returns in the same order: checked on the dollars (grown, so equal at the PUF
    # year, proportional per item otherwise) and, at the PUF year, on the weights
    if YEAR == PUF_YEAR:
        for c in ['wages', 'dividends', 'kg_lt', 'gross_ss', 'sole_prop']:
            assert np.array_equal(nodep[c].to_numpy(float), tf[c].to_numpy(float)), f'tax data filers: {c} differs'
        assert np.allclose(nodep['weight'].to_numpy(float), tf['weight'].to_numpy(float), rtol=1e-12)
        say('check: the tax data\'s filers equal the PUF\'s returns other than dependent filers, row for row '
            '(wages, dividends, long-term gains, Social Security, Schedule C and the weights)')
    puf_items = sorted(v for v, on in PRESENT.items() if 'puf' in on and v in AMOUNTS)
    cats = ['married', 'n_dep', 'filing_status', 'n_dep_ctc', 'n_dep_eitc', 'k1_owner']
    if SOURCE_DEF == 'puf_and_tax_data_nonfilers':
        assert YEAR == PUF_YEAR, (f'puf_source "puf_and_tax_data_nonfilers" holds the PUF\'s own weights and dollars, so '
                                  f'it exists only for {PUF_YEAR}; use "tax_data" for {YEAR}')
        filers = puf[puf_items + cats + ['age_range']].assign(w=puf['weight'].to_numpy(float), rec=np.arange(len(puf)),
                                                             filer=1, dep_filer=dep.astype(int))
        filers = split_age_ranges(filers, Pall)
    elif SOURCE_DEF == 'tax_data':
        extra = [v for v in puf_items if v not in tf]
        g = BT.grown(nodep, YEAR, columns=extra)
        for c in ['wages', 'kg_lt', 'gross_ss']:     # the same returns in the same order, grown the same way
            assert np.allclose(BT.grown(nodep, YEAR, columns=[c])[c].to_numpy(float), tf[c].to_numpy(float)), \
                f'tax data filers: grown {c} differs'
        filers = tf[[c for c in puf_items + cats if c in tf]].copy()
        for v in extra:
            filers[v] = g[v].to_numpy(float)
        filers = filers.assign(w=tf['weight'].to_numpy(float), rec=np.arange(len(tf)), filer=1, dep_filer=0,
                               age_range=-1)
        Pt = np.zeros((len(tf), common.N_RANGES))
        Pt[:, :common.ADULT_RANGES] = tf[[f'age_p{k}' for k in range(common.ADULT_RANGES)]].to_numpy(float)
        filers = split_age_ranges(filers, Pt)
        say(f'tax_data: {len(extra)} PUF items the tax data does not carry taken from the same returns, grown with '
            f'build_table.grown')
    else:
        raise SystemExit(f'settings.json puf_source: unknown definition {SOURCE_DEF!r}')
    # the non-filers: the tax data's, every PUF item it does not carry zero (the CPS does not ask them)
    A = tn[[f'age_p{k}' for k in range(common.ADULT_RANGES)]].to_numpy(float)
    assert np.allclose(A.max(1), 1) and np.allclose(A.sum(1), 1), 'a non-filer without one age range'
    nf = tn[[c for c in cats + puf_items if c in tn]].copy()
    zero = [v for v in puf_items if v not in tn and v not in META['computed_totals']]
    for v in zero:
        nf[v] = 0.0
    nf = nf.assign(w=tn['weight'].to_numpy(float), rec=10_000_000 + np.arange(len(tn)), filer=0, dep_filer=0,
                   age_range=A.argmax(1))
    say(f'non-filers: zero for the {len(zero)} PUF items the tax data does not carry on them: {", ".join(zero)}')
    # the PUF's computed totals against their parts (dollars.py makes the file's from the parts)
    for v, parts in META['computed_totals'].items():
        if v in filers:
            d = filers[v].to_numpy(float) - D.signed_sum(filers, parts)
            say(f'check: PUF {v} less the sum of its parts: largest difference ${np.abs(d).max():,.2f}')
    out = pd.concat([on_file_definitions(filers), on_file_definitions(nf)], ignore_index=True)
    assert not out[puf_items + ['agi']].isna().any().any(), 'a PUF item missing in the PUF source'
    # AGI against the PUF's own (E00100), at the PUF year
    if SOURCE_DEF == 'puf_and_tax_data_nonfilers':
        raw = BT.read_puf()[['RECID', 'E00100']]
        a = out[out.filer == 1].drop_duplicates('rec')
        ids = puf['record_id'].to_numpy()[a['rec'].to_numpy()].astype(np.int64)
        e = raw.set_index('RECID').loc[ids, 'E00100'].to_numpy(float)
        d = a['agi'].to_numpy(float) - e
        w = puf['weight'].to_numpy(float)[a['rec'].to_numpy()]
        say(f'check: AGI from the items against the PUF\'s E00100: {int((np.abs(d) > 0.5).sum()):,} of {len(d):,} '
            f'returns differ; largest difference ${np.abs(d).max():,.0f}; total ${(w * d).sum() / 1e9:,.3f} billion '
            f'on ${(w * e).sum() / 1e9:,.1f} billion')
    CHECKS.append('')
    return out, zero


def scf_source(t):
    frames = []
    shared = [v for v in SHARED['scf']]
    for wave, share in scf_targets.mix(YEAR):
        u = scf_targets.frame(wave, YEAR)[0].copy()
        tab = t[(t.source == 'scf') & (t.year == wave - 1)]
        add = [c for c in shared + ['married', 'n_dep', 'k1_owner', 'owns_business', 'age2']
               + [f'biz_industry_{k}' for k in ('schc', 'part', 'scorp')] if c not in u]
        u = u.merge(tab[['record_id'] + add], on='record_id', how='left', validate='one_to_one')
        assert not u[add].isna().any().any(), f'SCF {wave}: a unit without its training table row'
        u = survey_growth(u, shared, wave - 1)
        u['w'] = u['weight'].to_numpy(float) * share
        u['scf_wave'] = wave
        say(f'SCF wave {wave} (income year {wave - 1}), share {share:.4f}: {len(u):,} unit rows (five implicates), '
            f'{u.household_id.nunique():,} households, {u.w.sum() / 1e6:.3f} million units after the share')
        frames.append(u)
    u = pd.concat(frames, ignore_index=True)
    a2 = u['age2'].to_numpy(float) - 1                 # the table's coding: 0 none, else the age plus 1
    u['age2'] = np.where(a2 < 0, np.nan, a2)
    u['age1'] = u['age1'].to_numpy(float)              # scf_targets.frame has already made it the age
    u['capital_gains_all'] = u.pop('capital_gains')    # the SCF's capital gain: the table's concept
    u['rec'] = u['household_id'].astype(np.int64)
    return u


def table_rows(t, src, ids, cols):
    """The training table's rows of a survey for the units ids (the table's record ids), the columns cols."""
    tab = t[t.source == src].set_index('record_id')
    assert not tab.index.duplicated().any()
    miss = ~pd.Index(ids).isin(tab.index)
    assert not miss.any(), f'{src}: {miss.sum()} units without a training table row'
    return tab.loc[ids, cols].reset_index(drop=True)


def survey_source(t, src):
    m = survey_mix.mixed_units(src, YEAR).reset_index(drop=True)
    yr = m['year'].astype(np.int64).to_numpy()
    if src == 'cex':
        ids = m['record_id'].to_numpy(float) + (yr - 2000) * BT.CEX_YEAR_ID_OFFSET
    else:          # the CPS's and the SIPP's unit files already carry the training table's ids
        ids = m['record_id'].to_numpy(float)
    shared = SHARED[src]
    cats = [c for c, on in PRESENT.items() if src in on and c in CATEGORIES and c != 'age_range'] + \
        [a for a in AGES if src in PRESENT.get(a, [])] + ['age_range']
    tab = table_rows(t, src, ids, shared + cats)
    out = m.drop(columns=[c for c in shared + cats if c in m]).copy()
    for c in shared + cats:
        out[c] = tab[c].to_numpy()
    for y in sorted(set(yr)):
        sel = yr == y
        part = survey_growth(out.loc[sel, shared].copy(), shared, int(y))
        out.loc[sel, shared] = part.to_numpy()
    for a in AGES:                                     # the table's coding: 0 none, else the age plus 1
        if a in out:
            x = out[a].to_numpy(float) - 1
            out[a] = np.where(x < 0, np.nan, x)
    for s_ in ['sex_primary', 'sex_spouse']:
        if s_ in out:
            out[s_] = np.where(out[s_].to_numpy(float) == 0, np.nan, out[s_].to_numpy(float))
    out['w'] = out['weight'].to_numpy(float)
    if src == 'cex':
        out['total_consumption'] = out[META['cex_amounts']].to_numpy(float).sum(1)
        out['rec'] = pd.factorize(pd.Series(yr).astype(str) + '_' + m['household_id'].astype(str))[0]
    elif src == 'cps':
        out['ot_drawn'] = out['ot_primary'].to_numpy(float) + out['ot_spouse'].to_numpy(float)
        out['rec'] = np.arange(len(out))
    else:
        out['tips_drawn'] = out['tips_primary'].to_numpy(float) + out['tips_spouse'].to_numpy(float)
        out['rec'] = np.arange(len(out))
    say(f'{src.upper()} year mix ' + ', '.join(f'{y} {s:.4f}' for y, s in survey_mix.mix(src, YEAR)) +
        f': {len(out):,} rows, {out.w.sum() / 1e6:.3f} million units')
    return out


def file_frame(columns):
    path = f'{common.TILT}/file_{YEAR}.parquet'
    import pyarrow.parquet as pq
    have = set(pq.read_schema(path).names)
    need = sorted((set(columns) | {'weight', 'filer', 'age_range', 'billionaire', 'filing_status', 'ot_primary',
                                   'ot_spouse', 'tips_primary', 'tips_spouse'} | set(D.AGI_PARTS) |
                   set(D.KG_PIECES)) & have)
    f = pd.read_parquet(path, columns=need)
    f['w'] = f['weight'].to_numpy(float)
    f['rec'] = np.arange(len(f))
    f['agi'] = D.agi(f)
    f['capital_gains_all'] = f[D.KG_PIECES].to_numpy(float).sum(1)
    f['dep_filer'] = (f['age_range'] >= common.ADULT_RANGES).astype(int)
    f['ot_drawn'] = f['ot_primary'].to_numpy(float) + f['ot_spouse'].to_numpy(float)
    f['tips_drawn'] = f['tips_primary'].to_numpy(float) + f['tips_spouse'].to_numpy(float)
    f['forbes'] = f['billionaire'].astype(int)
    for a in AGES:                                     # the file's coding: -1 none, else the age
        if a in f:
            x = f[a].to_numpy(float)
            assert x.min() >= -1 and x.max() <= 85, f'file {a}: values {x.min()} to {x.max()}'
            f[a] = np.where(x < 0, np.nan, x)
    for s_ in ['sex_primary', 'sex_spouse']:
        if s_ in f:
            f[s_] = np.where(f[s_].to_numpy(float) == 0, np.nan, f[s_].to_numpy(float))
    say(f'file: {path}: {len(f):,} records, {f.w.sum() / 1e6:.3f} million units, of them {f.filer.sum():,} filer '
        f'records ({f.w[f.filer == 1].sum() / 1e6:.3f} million) and {int(f.forbes.sum())} Forbes-list records')
    return f, path


# ---- the catalog --------------------------------------------------------------------------------------------------
SOURCE_TEXT = {
    'puf': {'puf_and_tax_data_nonfilers':
            f'The {PUF_YEAR} PUF: every return, dependent filers included, on its own weight and in its own dollars; '
            f'plus the non-filer units of the pipeline\'s tax data for {YEAR} (every CPS unit of the year weighted by '
            'its chance of not filing, its amounts in PUF terms; build_table.tax_data). A return with no age range '
            'on the PUF is split over the ranges by step 0\'s probabilities.',
            'tax_data': f'The pipeline\'s tax data for {YEAR} (build_table.tax_data): the {PUF_YEAR} PUF\'s returns '
            f'other than dependent filers, weights scaled by SOI Table 1.6, dollars grown per return to {YEAR}, plus '
            'the non-filer units (CPS units weighted by their chance of not filing). Dependent filers are not in it.'
            }[SOURCE_DEF],
    'scf': 'The SCF over the year\'s mix of waves ({mix}), as the SCF targets use it (scf_targets.frame): the balance '
           'sheet grown per tax unit to the year\'s wave, weights scaled to the year\'s units, all five implicates. '
           'Income items in PUF terms after the translation (function 1, before the fill-in of missed holders), '
           'grown per return from the wave\'s income year. The SCF folds dependents into the unit that claims them '
           'and leaves out the Forbes 400.',
    'cex': 'The CEX over the year\'s mix of survey years ({mix}), as the consumption targets use it '
           '(survey_mix.mixed_units): every tax unit, a household\'s spending divided among its tax units by head '
           'count, in the year\'s dollars by NIPA consumption per return. Income items in PUF terms after the '
           'translation (function 1, before the fill-in), grown per return to the year.',
    'cps': 'The CPS ASEC over the year\'s mix of tax years ({mix}), as the CPS report uses it (survey_mix.mixed_units): '
           'every tax unit, filers and non-filers. Overtime is asked only of the earnings-question group, so it is '
           'missing on the other units. Income items in PUF terms after the translation (function 1, before the '
           'fill-in), grown per return to the year.',
    'sipp': 'The SIPP over the year\'s mix of reference years ({mix}), as the SIPP report uses it '
            '(survey_mix.mixed_units): every tax unit. Income items in PUF terms after the translation (function 1, '
            'before the fill-in), grown per return to the year.',
}
COUNT_TEXT = {'file': 'records of the file', 'puf': 'PUF returns and CPS non-filer units',
              'scf': 'SCF households (five implicates count once)', 'cex': 'CEX consumer units in a survey year',
              'cps': 'CPS tax units', 'sipp': 'SIPP tax units'}
NAMES = {'file': f'posttrained file {YEAR}', 'puf': f'PUF {PUF_YEAR} and non-filers' if SOURCE_DEF != 'tax_data'
         else f'tax data {YEAR}', 'scf': 'SCF', 'cex': 'CEX', 'cps': 'CPS', 'sipp': 'SIPP'}


def unit(v):
    """What an amount's values are: dollars unless said here."""
    if v in AGES:
        return 'years'
    if v.startswith('weeks_'):
        return 'weeks'
    if v.startswith('hours_'):
        return 'hours'
    if v.endswith('_share') or v == 'w_primary_ltv':
        return 'share'
    if v.startswith('biz_workers'):
        return 'workers'
    return 'dollars'


def kind(v):
    if v in FLAGS:
        return 'flag'
    if v in CATEGORIES:
        return 'category'
    return 'amount'


def main():
    t0 = time.time()
    os.umask(0o027)
    os.makedirs(OUT, mode=0o750, exist_ok=True)
    say(f'# Check tool tables for {YEAR} (puf_source {SOURCE_DEF})', '')
    t = pd.read_parquet(common.TABLE)
    frames = {}
    frames['puf'], zero = puf_source(t)
    frames['scf'] = scf_source(t)
    for s in ['cex', 'cps', 'sipp']:
        frames[s] = survey_source(t, s)
    del t
    # each source's variables: those present on its label, as the network sees that label (model.py present_on,
    # in the band edges' meta), plus the totals computed from them
    xvars = {'puf': sorted(v for v, on in PRESENT.items() if 'puf' in on and v != 'agi_group')
             + ['agi', 'capital_gains_line13', 'other_agi_remainder', 'capital_gains_all', 'dep_filer']}
    for s in ['scf', 'cex', 'cps', 'sipp']:
        xvars[s] = sorted(v for v, on in PRESENT.items() if s in on and v not in ('filer_p',))
    xvars['scf'] = [v if v != 'capital_gains' else 'capital_gains_all' for v in xvars['scf']] + ['net_worth',
                                                                                            'value.primary_mortgage']
    xvars['cex'] += ['total_consumption']
    xvars['cps'] += ['ot_drawn']
    xvars['sipp'] += ['tips_drawn']
    known = set(LABELS) | set(CATEGORIES) | set(AGES)
    xvars = {s: [v for v in dict.fromkeys(vs) if v in frames[s] and v in known] for s, vs in xvars.items()}
    allvars = sorted(set().union(*xvars.values()))
    f, path = file_frame(allvars)
    frames['file'] = f
    missing_in_file = [v for v in allvars if v not in f]
    assert not missing_in_file, f'variables of a source not in the file: {missing_in_file}'
    # the source of each Y: a PUF item's is the PUF; a survey's own item's is that survey (the first, for an item two
    # surveys share without the PUF)
    ysrc = {}
    for s in ['puf', 'scf', 'cex', 'cps', 'sipp']:
        for v in xvars[s]:
            if v not in ysrc and (kind(v) in ('amount', 'flag')) and v not in AGES:
                if s == 'puf' or 'puf' not in PRESENT.get(v, []):
                    ysrc[v] = s
    for v in ['filer', 'dep_filer', 'married']:
        ysrc.pop(v, None)
    keep = {s: sorted(set(xvars[s]) | {'w', 'rec', 'filer', 'dep_filer', 'forbes', 'scf_wave'} & set(frames[s]))
            for s in frames if s != 'file'}
    keep['file'] = sorted(set(allvars) | {'w', 'rec', 'filer', 'dep_filer', 'forbes'})
    variables, missing = {}, {}
    for s, d in frames.items():
        d = d[[c for c in keep[s] if c in d]]
        for c in d.columns:
            if d[c].dtype == bool:
                d[c] = d[c].astype(np.int64)
            assert pd.api.types.is_numeric_dtype(d[c]), f'{s} {c}: not numeric ({d[c].dtype})'
        d = d.astype({c: float for c in d.columns if c not in ('rec',)})
        d.to_parquet(os.path.join(OUT, f'{s}.parquet'), index=False)
        na = d.drop(columns=['rec', 'w']).isna().sum()
        missing[s] = {c: int(n) for c, n in na.items() if n > 0}
        assert not d['w'].isna().any() and (d['w'] >= 0).all(), f'{s}: a missing or negative weight'
        frames[s] = d
    for v in allvars:
        variables[v] = {'label': LABELS.get(v, v.replace('_', ' ')), 'kind': kind(v) if v not in AGES else 'amount',
                        'unit': unit(v) if kind(v) == 'amount' else None,
                        'categories': {str(k): lab for k, lab in CATEGORIES.get(v, {}).items()},
                        'y_source': ysrc.get(v)}
    mixes = {'scf': ', '.join(f'{w} wave {s:.2%}' for w, s in scf_targets.mix(YEAR)),
             **{s: ', '.join(f'{y} {sh:.2%}' for y, sh in survey_mix.mix(s, YEAR)) for s in ['cex', 'cps', 'sipp']}}
    sources = {}
    for s in ['puf', 'scf', 'cex', 'cps', 'sipp', 'file']:
        d = frames[s]
        sources[s] = {'name': NAMES[s], 'what': SOURCE_TEXT.get(s, f'The posttrained file: {path}').format(mix=mixes.get(s, '')),
                      'records': int(d['rec'].nunique()), 'rows': len(d), 'units': float(d['w'].sum()),
                      'count': COUNT_TEXT[s], 'has_filer': bool('filer' in d and s in ('puf', 'cps', 'file')),
                      'missing': missing[s], 'x': xvars.get(s, [])}
    notes = [f'Non-filers in the PUF source carry zero for {len(zero)} PUF items the CPS does not ask: '
             + ', '.join(zero) + '.']
    catalog = {'year': YEAR, 'puf_year': PUF_YEAR, 'puf_source': SOURCE_DEF, 'file': path, 'run': RUN['out'],
               'built': time.strftime('%Y-%m-%d %H:%M'), 'variables': variables, 'sources': sources, 'notes': notes,
               'thin_cell_records': S['thin_cell_records'], 'default_percentiles': S['default_percentiles'],
               'default_dollar_cutoffs': S['default_dollar_cutoffs']}
    json.dump(catalog, open(os.path.join(OUT, 'catalog.json'), 'w'), indent=1)
    for s in sources:
        say(f'{s}: {sources[s]["records"]:,} sample records, {sources[s]["units"] / 1e6:.3f} million units; '
            f'{len(sources[s]["x"])} variables; missing values: {missing[s]}')
    say(f'Y items by source: ' + ', '.join(f'{s} {sum(1 for v in ysrc.values() if v == s)}' for s in sources))
    say(f'written to {OUT} in {time.time() - t0:.0f} seconds')
    open(os.path.join(OUT, 'checks.md'), 'w').write('\n'.join(CHECKS) + '\n')


if __name__ == '__main__':
    main()
