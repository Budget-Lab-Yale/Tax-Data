"""The published tables the non-microdata imputations read (non_micro_imputations.py), built once from their sources
into research/rebuild/resources/non_micro/, which is committed. Run again only when a source changes:

    sbatch research/rebuild/pipeline/run.sbatch non_micro_tables      (no GPU needed)

Each table is a CSV with a year column where the published numbers change by year; sources.csv in the same folder
says, for every table, where its numbers come from and how they were turned into the table. The sources:
  - production's resources/ (the SSA life tables, Chetty et al.'s mortality by income, Johnson et al.'s marital
    factors, SOI's Sales of Capital Assets tables, the S&P 500 index, Chetty et al.'s mobility matrix);
  - the Federal Reserve's Financial Accounts of the United States (Z.1), the CSV files of the release of 10 September
    2026 (settings paths.raw.z1);
  - the Census Bureau's population estimates by single year of age and sex (settings paths.raw.census_pep);
  - SOI's Publication 4801 line items (settings paths.raw.soi_mirror, line_items.csv);
  - the CPS ASEC (settings paths.raw.cps_earnings, the common extract): average pay of private employees by SCF
    industry group and tax year, for the QBI wage bills (since 29 October);
  - SOI's Publication 4801 blind boxes, typed in from the pages (since 29 October);
  - the 2015 PUF: the share of primary filers who are blind, by age range, where the return shows it (since the second
    retrain of 29 October; the blindness rule in non_micro_imputations reads it);
  - the law: the dependent filing threshold (Form 1040 instructions, Chart B, as production's
    src/nonfilers/asec_tax_units.R has it) and the child tax credit's age limit (IRC section 24(c)).
"""
import io
import os
import shutil
import zipfile

import numpy as np
import pandas as pd

import common

REPO = os.path.abspath(os.path.join(common.HERE, '..', '..', '..'))
PROD = os.path.join(REPO, 'resources')
OUTDIR = os.path.abspath(os.path.join(common.HERE, '..', 'resources', 'non_micro'))
RAW = common.RAW
SOURCES = []


def note(table, source, how):
    SOURCES.append({'table': table, 'source': source, 'how': how})


def write(df, name):
    df.to_csv(os.path.join(OUTDIR, name), index=False)
    common.say(f'{name}: {len(df)} rows')


# ---- the law ---------------------------------------------------------------------------------------------------
def dependent_threshold():
    """Chart B's earned income threshold for a dependent (the single standard deduction), as production's
    filing_requirement_params() in src/nonfilers/asec_tax_units.R has it, 2014-2023."""
    v = {2014: 6200, 2015: 6300, 2016: 6300, 2017: 6350, 2018: 12000, 2019: 12200, 2020: 12400, 2021: 12550,
         2022: 12950, 2023: 13850}
    write(pd.DataFrame({'year': list(v), 'earned_income_threshold': list(v.values())}),
          'dependent_filing_threshold.csv')
    note('dependent_filing_threshold.csv', 'Form 1040 instructions, Chart B (a dependent must file with earned income '
         'above the single standard deduction); production src/nonfilers/asec_tax_units.R, filing_requirement_params',
         'copied, 2014-2023')


def ctc_age_limit():
    """The child tax credit's age limit: a qualifying child is under this age at the end of the year."""
    rows = [{'year': y, 'under_age': 18 if y == 2021 else 17} for y in range(2015, 2026)]
    write(pd.DataFrame(rows), 'ctc_age_limit.csv')
    note('ctc_age_limit.csv', 'IRC section 24(c)(1) (under 17); American Rescue Plan Act of 2021, section 9611 '
         '(under 18 for 2021 only)', 'typed in, 2015-2025')


# ---- death probabilities ----------------------------------------------------------------------------------------
def life_table():
    """SSA's period life tables of the 2024 Trustees Report: q(x) and L(x) by year, sex and single age 0-119, the
    historical tables to 2021 and the intermediate (alternative 2) projections from 2022, years 2010-2100."""
    parts = []
    for sex, s in [('M', 1), ('F', 2)]:
        for kind in ['Hist', 'Alt2']:
            raw = pd.read_csv(os.path.join(PROD, f'PerLifeTables_{sex}_{kind}_TR2024.csv'), skiprows=4)
            parts.append(pd.DataFrame({'year': raw['Year'].astype(int), 'sex': s, 'age': raw['x'].astype(int),
                                       'q': raw['q(x)'].astype(float), 'L': raw['L(x)'].astype(float)}))
    t = pd.concat(parts).drop_duplicates(['year', 'sex', 'age'], keep='first')
    t = t[(t.year >= 2010) & (t.year <= 2100)].sort_values(['year', 'sex', 'age'])
    assert t[['q', 'L']].notna().all().all()
    write(t, 'ssa_period_life_table.csv')
    note('ssa_period_life_table.csv', 'SSA, 2024 Trustees Report, period life tables (historical to 2021, intermediate '
         'alternative from 2022); production resources/PerLifeTables_{M,F}_{Hist,Alt2}_TR2024.csv',
         'q(x) and L(x) kept, years 2010-2100; sex 1 men, 2 women')


def chetty_ratio():
    """Chetty et al. (2016) Table 15: mortality by sex, age at death and household income percentile, 2001-2014.
    As production's build_chetty_R(): ages put in four bins (under 50, 50-59, 60-69, 70 and over), the rate averaged
    over ages and years in each bin, then divided by its mean over the 100 percentiles, so it averages 1."""
    raw = pd.read_csv(os.path.join(PROD, 'chetty_table_15.csv'))
    assert raw['mortrate'].notna().all()
    a = raw['age_at_d']
    raw['age_bin'] = np.select([a < 50, a < 60, a < 70], ['<50', '50-59', '60-69'], '70+')
    raw['sex'] = np.where(raw['gnd'] == 'M', 1, 2)
    m = raw.groupby(['sex', 'age_bin', 'pctile'], as_index=False)['mortrate'].mean()
    m['ratio'] = m['mortrate'] / m.groupby(['sex', 'age_bin'])['mortrate'].transform('mean')
    write(m[['sex', 'age_bin', 'pctile', 'ratio']], 'chetty_mortality_ratio.csv')
    note('chetty_mortality_ratio.csv', 'Chetty et al. (2016), JAMA 315(16), online Table 15 (healthinequality.org), '
         'years of death 2001-2014; production resources/chetty_table_15.csv',
         'production build_chetty_R(): mean rate by sex, age bin and percentile over 2001-2014, over its mean across '
         'percentiles. No year column: one table for every year, as in production')


def johnson():
    shutil.copy(os.path.join(PROD, 'johnson_2000_marital_hr.csv'), os.path.join(OUTDIR, 'johnson_marital_factor.csv'))
    note('johnson_marital_factor.csv', 'Johnson, Backlund, Sorlie and Loveless (2000), Annals of Epidemiology 10(4), '
         'as in Ricco (2020) Table 5; production resources/johnson_2000_marital_hr.csv',
         'copied; men only (production applies no marital factor to women). No year column')


def census_population():
    """National resident population by sex and single year of age (85 = 85 and over), July 1, 2015-2024."""
    base = RAW['census_pep']
    parts = []
    for f, years in [('sc-est2020int-alldata6.csv', range(2015, 2020)), ('sc-est2024-alldata6.csv', range(2020, 2025))]:
        c = pd.read_csv(os.path.join(base, f), encoding='latin-1')
        c = c[(c.ORIGIN == 0) & c.SEX.isin([1, 2])]
        for y in years:
            g = c.groupby(['SEX', 'AGE'], as_index=False)[f'POPESTIMATE{y}'].sum()
            parts.append(pd.DataFrame({'year': y, 'sex': g.SEX, 'age': g.AGE, 'population': g[f'POPESTIMATE{y}']}))
    t = pd.concat(parts, ignore_index=True)
    write(t, 'census_population.csv')
    note('census_population.csv', 'Census Bureau population estimates by state, sex, race, Hispanic origin and single '
         'year of age (sc-est2020int-alldata6 for 2015-2019, sc-est2024-alldata6 for 2020-2024); settings '
         'paths.raw.census_pep', 'summed over states and races, both origins; July 1 estimates; used only by the '
         'death probabilities\' check')


# ---- yearly unrealized gains ------------------------------------------------------------------------------------
Z1_SERIES = {'equities': ('153064105', 'corporate equities, directly held'),
             'mutual_funds': ('153064205', 'mutual fund shares (shown in the check only)'),
             'pass_throughs': ('152090205', "proprietors' equity in noncorporate business"),
             'real_estate': ('155035005', 'real estate at market value'),
             'bonds': ('154022005', 'debt securities')}


def z1_rates():
    """Each year's revaluation rate by asset, households and nonprofit organizations: the year's revaluation (FR
    series, Z.1 table S1M.i.a, annual) over the amount held at the end of the year before (LM series, same table).
    The same series production's long-run rates were computed from (Tax-Data docs/wealth_accruals_design.md, then
    table R.101; the Federal Reserve renamed the tables on 11 June 2026)."""
    with zipfile.ZipFile(RAW['z1']) as z:
        a = pd.read_csv(io.BytesIO(z.read('csv/S1M_i_a.csv')))
    a['year'] = a['date'].astype(str).str[:4].astype(int)
    a = a.set_index('year')
    rows = []
    for asset, (code, what) in Z1_SERIES.items():
        fr = pd.to_numeric(a[f'FR{code}.A'], errors='coerce')
        lm = pd.to_numeric(a[f'LM{code}.A'], errors='coerce')
        for y in range(1990, int(a.index.max()) + 1):
            if not (np.isfinite(fr.get(y, np.nan)) and np.isfinite(lm.get(y - 1, np.nan)) and lm[y - 1] > 0):
                continue
            rows.append({'year': y, 'asset': asset, 'rate': fr[y] / lm[y - 1], 'revaluation_millions': fr[y],
                         'held_end_of_year_before_millions': lm[y - 1], 'held_end_of_year_millions': lm.get(y, np.nan),
                         'series': f'FR{code}.A / LM{code}.A (year before)', 'what': what})
    t = pd.DataFrame(rows)
    write(t, 'z1_revaluation_rates.csv')
    lr = t[(t.year >= 1990) & (t.year <= 2024)].groupby('asset')['rate'].mean()
    common.say('Z.1 mean rates 1990-2024 (production\'s long-run rates are 11.8%, 5.2%, 4.5%, bonds -0.08% rounded '
               'to 0): ' + ', '.join(f'{k} {100 * v:.2f}%' for k, v in lr.items()))
    note('z1_revaluation_rates.csv', 'Federal Reserve, Financial Accounts of the United States (Z.1), release of 10 '
         'September 2026 (data to 2026Q2), table S1M.i.a (households and nonprofit organizations, annual), '
         'https://www.federalreserve.gov/releases/z1/current/z1_csv_files.zip; settings paths.raw.z1',
         'rate = the year\'s revaluation (FR series) / the amount held at the end of the year before (LM series), '
         '1990-2025; series codes in the table')
    lr_rows = [{'asset': 'equities', 'rate': 0.118}, {'asset': 'pass_throughs', 'rate': 0.052},
               {'asset': 'real_estate', 'rate': 0.045}, {'asset': 'bonds', 'rate': 0.0}]
    write(pd.DataFrame(lr_rows), 'accrual_long_run_rates.csv')
    note('accrual_long_run_rates.csv', 'production src/imputations/accruals.R, Z1_RATES: the Z.1 mean revaluation '
         'rates 1990-2024 (bonds -0.08%, set to 0 by production)', 'copied; used in years with no full year of Z.1 '
         'data (projections). No year column: one row per asset')


# ---- holding period and basis, parents' rank --------------------------------------------------------------------
def copies():
    for src, dst, source, how in [
            ('soca_basis_sales.csv', 'soca_basis_sales.csv',
             'SOI Sales of Capital Assets panel, basis-to-sales ratio of long-term gains by holding period, 1985 and '
             '1997-2015; production resources/soca_basis_sales.csv', 'copied'),
            ('soca_hp_ingredients.csv', 'soca_gain_holding_shares.csv',
             'SOI Sales of Capital Assets: share of long-term gain dollars by holding period (pi_g); production '
             'resources/soca_hp_ingredients.csv', 'copied; no year column (one pooled table, as in production)'),
            ('soca_loss_hp.csv', 'soca_loss_holding_shares.csv',
             'SOI Sales of Capital Assets: share of long-term loss dollars by holding period (pi_l) and the loss '
             'basis-to-sales ratio; production resources/soca_loss_hp.csv', 'copied; no year column'),
            ('sp500_total_return_index.csv', 'sp500_total_return_index.csv',
             'S&P 500 total return index, annual, 1926-2025; production resources/sp500_total_return_index.csv',
             'copied'),
            ('mobility_matrix.csv', 'mobility_matrix.csv',
             'Chetty, Hendren, Kline and Saez (2014), the national distribution of child income rank by parent income '
             'rank (children born 1980-82); production resources/mobility_matrix.csv',
             'copied; columns are parent ranks, each column sums to 1 over child ranks. No year column')]:
        shutil.copy(os.path.join(PROD, src), os.path.join(OUTDIR, dst))
        note(dst, source, how)


# ---- checks -----------------------------------------------------------------------------------------------------
def soi_children():
    """Children on returns claiming the additional child tax credit: Schedule 8812 line 4 (the number of qualifying
    children under 17 with a Social Security number times $1,400) divided by $1,400, Publication 4801, 2018-2020."""
    li = pd.read_csv(os.path.join(RAW['soi_mirror'], 'line_items.csv'))
    m = (li.form == 'Schedule 8812') & (li.line.astype(str) == '4') & (li.universe == 'all returns')
    rows = []
    for y in [2018, 2019, 2020]:
        r = li[m & (li.tax_year == y)]
        if r.empty:
            continue
        amount = float(r.loc[r.measure == 'amount', 'value'].iloc[0]) * 1000
        returns = float(r.loc[r.measure == 'returns', 'value'].iloc[0])
        rows.append({'year': y, 'actc_returns': returns, 'actc_line4_dollars': amount, 'per_child': 1400,
                     'actc_children': amount / 1400})
    write(pd.DataFrame(rows), 'soi_actc_children.csv')
    note('soi_actc_children.csv', 'SOI Publication 4801 (line item estimates), Schedule 8812 line 4, all returns; '
         'settings paths.raw.soi_mirror line_items.csv', 'children = line 4 amount / $1,400, 2018-2020; only returns '
         'claiming the additional child tax credit, so a lower bound on all the credit\'s children')


# ---- the CPS: average pay of private employees by SCF industry group (29 October, the QBI wage bills) ----------
# The SCF public file's 7 industry groups (X3107, X3207) as sets of Census industry codes (the SCF codebook's collapse;
# the same ranges as explore/2026-10-28/qbi_plan/scripts/cps_bridge.py), lower and upper code inclusive. The CPS uses
# the Census 2012 industry codes to the March 2019 survey and the 2017 codes from March 2020; the ranges hold both (the
# 2017 codes that split performing arts, 8561-8564, are in group 3 with 8560). A code in no range is counted and left
# out of the averages.
SCF_INDUSTRY_RANGES = [(170, 290, 1), (7480, 7480, 1), (7770, 7770, 1), (370, 490, 2), (770, 770, 2), (1070, 3990, 3),
                       (6470, 6480, 3), (8560, 8564, 3), (4070, 5790, 4), (8680, 8690, 4), (6490, 6490, 5),
                       (6695, 6695, 5), (6870, 7080, 5), (7190, 7190, 5), (7580, 7590, 5), (7680, 7680, 5),
                       (8770, 8890, 5), (570, 690, 6), (6070, 6390, 6), (6570, 6692, 6), (6770, 6780, 6), (7170, 7180, 6),
                       (7270, 7470, 6), (7490, 7570, 6), (7670, 7670, 6), (7690, 7690, 6), (7780, 8470, 6),
                       (8570, 8670, 6), (8970, 9290, 6), (9370, 9890, 7)]
PRIVATE_CLASSES = [21, 22, 23]      # IPUMS CLASSWKR: wage and salary, private (for profit, nonprofit)


def scf_industry_group(ind):
    ind = np.asarray(ind, int)
    g = np.zeros(len(ind), int)
    for lo, hi, k in SCF_INDUSTRY_RANGES:
        g[(ind >= lo) & (ind <= hi)] = k
    return g


def cps_private_pay():
    """Average yearly wages of private employees by SCF industry group and tax year (survey year less one): people 18
    and over whose class of worker is private wage and salary (CLASSWKR 21-23) with wages above zero (INCWAGE, the
    previous calendar year's wages from all jobs; the industry is the current or most recent job's, IND), weighted
    by ASECWT. Group 8 ("no industry code", the SCF's businesses worth $100 million or more) is all private
    employees together. Nominal dollars of the tax year."""
    folder = RAW['cps_earnings']
    V = ['ASECWT', 'AGE', 'IND', 'CLASSWKR', 'INCWAGE']
    rows = []
    years = sorted(int(f[3:7]) for f in os.listdir(folder) if f.startswith('cps') and f.endswith('_03s'))
    for sy in years:
        sub = os.path.join(folder, f'cps{sy}_03s')
        dat = os.path.join(sub, f'cps_{sy}_03s.dat.gz')
        if not os.path.exists(dat):
            continue
        vv = pd.read_csv(os.path.join(sub, 'variables.csv')).set_index('var_name')
        spec = [(int(vv.loc[v, 'start']) - 1, int(vv.loc[v, 'end'])) for v in V]
        d = pd.read_fwf(dat, colspecs=spec, names=V, compression='gzip', dtype=float)
        d['ASECWT'] = d['ASECWT'] / 10 ** int(vv.loc['ASECWT', 'imp_decim'])
        # the universe: private employees of 18 and over with wages; INCWAGE 99999998-99999999 are missing or not in
        # universe (none expected in this universe, counted)
        d = d[(d['AGE'] >= 18) & d['CLASSWKR'].isin(PRIVATE_CLASSES) & (d['ASECWT'] > 0)]
        bad = int((d['INCWAGE'] >= 99999998).sum())
        d = d[(d['INCWAGE'] > 0) & (d['INCWAGE'] < 99999998)]
        g = scf_industry_group(d['IND'].to_numpy())
        w, x = d['ASECWT'].to_numpy(float), d['INCWAGE'].to_numpy(float)
        unmatched = w[g == 0].sum() / w.sum()
        common.say(f'CPS {sy} (tax year {sy - 1}): private employees with wages {w.sum() / 1e6:.1f} million; industry '
                   f'code in no SCF group {100 * unmatched:.2f}% of them (left out); wage codes missing {bad}')
        every = g > 0
        mean_all = float((w[every] * x[every]).sum() / w[every].sum())
        for k in range(1, 9):
            m = (g == k) if k < 8 else every
            # group 7, public administration, has no private employees: it takes all private employees' average
            own = bool(m.any())
            rows.append({'year': sy - 1, 'scf_industry_group': k, 'employees_millions': w[m].sum() / 1e6,
                         'sample_people': int(m.sum()),
                         'average_pay': float((w[m] * x[m]).sum() / w[m].sum()) if own else mean_all,
                         'pay_of': 'the group' if own else 'all private employees (no private employee in the group)',
                         'share_left_out_no_group': unmatched})
    t = pd.DataFrame(rows)
    write(t, 'cps_private_pay_by_scf_group.csv')
    common.say('average pay of private employees by SCF industry group:\n'
               + t.pivot(index='scf_industry_group', columns='year', values='average_pay').round(0).to_string())
    note('cps_private_pay_by_scf_group.csv', 'CPS ASEC, the common IPUMS extract (settings paths.raw.cps_earnings): '
         'ASECWT, AGE, IND, CLASSWKR, INCWAGE; the SCF codebook\'s collapse of Census industry codes to its 7 public '
         'groups (X3107)', 'weighted mean of INCWAGE over private wage and salary workers (CLASSWKR 21-23) 18 and '
         'over with wages, by SCF industry group of the current or most recent job; group 8 is all of them, and so is group 7 (public '
         'administration), which has no private employees; tax year = survey year - 1; nominal dollars')


def pub4801_blind():
    """The age and blindness boxes checked on all individual returns, and the returns taking the standard deduction,
    SOI Publication 4801 (line item estimates), read off the publication's pages (29 October; scratch
    explore/2026-10-29/retrain/p4801/): 2015 and 2017 the all-forms total of Form 1040 line 39a and Form 1040A line 23a
    (1040EZ has no boxes); 2018 and 2019 Form 1040 page 1 (the age and blindness boxes) and the summary table's basic
    standard deduction."""
    rows = [(2015, 24362239, 256557, 10153231, 81906, 103844288), (2017, 25924675, 260535, 10959898, 83983, 104013115),
            (2018, 26686849, 269239, 11260158, 75785, 134270136), (2019, 27477794, 260291, 11609322, 80564, 138307604)]
    write(pd.DataFrame(rows, columns=['year', 'primary_65_or_older', 'primary_blind', 'spouse_65_or_older', 'spouse_blind',
                                      'standard_deduction_returns']), 'pub4801_blind_boxes.csv')
    note('pub4801_blind_boxes.csv', 'SOI Publication 4801, line item estimates, all returns: 2015 and 2017 Form 1040 '
         'line 39a plus Form 1040A line 23a (the printed all-forms total); 2018 and 2019 Form 1040 page 1; the basic '
         'standard deduction returns from the all-forms page (2015, 2017) or the summary table (2018, 2019)',
         'typed in from the pages (scratch explore/2026-10-29/retrain/p4801/), 2015 and 2017-2019')


def puf_blindness(puf, age_range):
    """The primary filer's blindness on each PUF record, where the return shows it: 1 blind, 0 not blind, -1 not
    known (moved here from build_table.py, where it made the training table's blindness slot of 29 October). Shown
    only on unmarried returns taking the standard deduction (filing status single or head of household, FDED 2), not
    dependent filers (DSI 1, whose deduction is limited), with an age range: there the deduction above the basic amount,
    in units of the additional amount (settings blindness: $1,550 in 2015), is the number of extra boxes; the boxes are
    a whole number on 97% of these returns. Blind = the boxes less one at 65 and over (age range 65+). Not known: joint
    returns (in the 2015 PUF they show only 0, 2 or 4 boxes, the spouses evened out), separate returns, itemizers,
    dependent filers, records with no age range, boxes that are not a whole number, and boxes that do not fit the age
    (none at 65 and over, or two under 65). Prints the counts."""
    B = common.SETTINGS['blindness']
    mars, fded, dsi = (puf[c].to_numpy() for c in ('MARS', 'FDED', 'DSI'))
    ded = puf['P04470'].to_numpy(float)
    w = puf['S006'].to_numpy(float) / 100
    basic = np.vectorize(lambda m: float(B['puf_basic_standard_deduction'].get(str(int(m)), np.nan)))(mars)
    unmarried_std = np.isin(mars, [1, 4]) & (fded == 2) & (dsi == 0)
    boxes = np.where(unmarried_std, (ded - np.nan_to_num(basic)) / float(B['puf_additional_amount_unmarried']), np.nan)
    whole = unmarried_std & (np.abs(boxes - np.round(np.nan_to_num(boxes))) <= float(B['whole_tolerance']))
    has_range = (age_range >= 0) & (age_range < common.ADULT_RANGES)
    old = age_range == common.ADULT_RANGES - 1
    blind = np.round(np.nan_to_num(boxes)).astype(np.int64) - old.astype(np.int64)
    fits = whole & has_range & np.isin(blind, [0, 1])
    out = np.where(fits, blind, -1).astype(np.int64)
    n = lambda m: f'{w[m].sum() / 1e6:.2f}m'
    common.say(f'PUF blindness (weighted returns): unmarried standard-deduction returns other than dependent filers '
               f'{n(unmarried_std)}; boxes a whole number {n(whole)}; of those with no age range {n(whole & ~has_range)}; '
               f'boxes that do not fit the age {n(whole & has_range & ~np.isin(blind, [0, 1]))}; shown {n(fits)}, '
               f'blind {n(out == 1)}')
    return out


def puf_blind_rates():
    """The PUF's blind rate by the primary filer's age range, on the returns where blindness is shown (puf_blindness),
    weighted (S006). The blindness rule (non_micro_imputations.blindness) applies these rates, times one factor per
    person type, to every primary filer and spouse in the file. Age ranges as the training table's (ages.py, observed
    range only; records with no range are left out of the rates)."""
    import ages
    import build_table
    puf = build_table.read_puf()
    rng, _ = ages.puf_age_ranges(puf)
    assert np.array_equal(rng.RECID.to_numpy(), puf.RECID.to_numpy())
    age_range = rng.age_range.to_numpy()
    b = puf_blindness(puf, age_range)
    w = puf['S006'].to_numpy(float) / 100
    lo = [0, 26, 35, 45, 55, 65]
    hi = lo[1:] + [None]
    rows = []
    for k in range(common.ADULT_RANGES):
        known = (age_range == k) & (b >= 0)
        assert known.any(), f'no PUF return shows blindness in age range {common.AGE_LABELS[k]}'
        rows.append({'puf_year': common.PUF_YEAR, 'age_range': common.AGE_LABELS[k], 'age_from': lo[k],
                     'age_to': hi[k], 'returns_shown': w[known].sum(), 'blind_returns': w[known & (b == 1)].sum(),
                     'records_shown': int(known.sum()), 'blind_records': int((known & (b == 1)).sum()),
                     'blind_rate': w[known & (b == 1)].sum() / w[known].sum()})
    t = pd.DataFrame(rows)
    common.say('PUF blind rates by age range:\n' + t.to_string(index=False))
    write(t, 'puf_blind_rates.csv')
    note('puf_blind_rates.csv', f'the {common.PUF_YEAR} PUF (settings paths.raw.puf): single and head-of-household '
         'returns taking the standard deduction, not dependent filers, with an observed age range',
         'blind = the standard deduction above the basic amount, in units of the additional amount, less one at 65 and '
         'over (settings blindness); weighted share blind among the returns where it is shown, by the primary '
         'filer\'s age range (age_to empty: no upper limit)')


TABLES = [dependent_threshold, ctc_age_limit, life_table, chetty_ratio, johnson, census_population, z1_rates, copies,
          soi_children, cps_private_pay, pub4801_blind, puf_blind_rates]


def main(names=None):
    """Every table; or, with names (python build_non_micro_tables.py cps_private_pay ...), only those, their rows of
    sources.csv replaced and the other rows kept."""
    os.makedirs(OUTDIR, exist_ok=True)
    run = [f for f in TABLES if not names or f.__name__ in names]
    assert not names or len(run) == len(names), f'unknown tables in {names}'
    for f in run:
        f()
    src = pd.DataFrame(SOURCES)
    path = os.path.join(OUTDIR, 'sources.csv')
    if names and os.path.exists(path):
        old = pd.read_csv(path)
        src = pd.concat([old[~old['table'].isin(src['table'])], src], ignore_index=True)
    src.to_csv(path, index=False)
    common.say(f'wrote {len(SOURCES)} tables and sources.csv to {OUTDIR}')


if __name__ == '__main__':
    import sys
    main(sys.argv[1:] or None)
