"""errors.py: each target's standard error (TARGETS.md section 9).

Errors matter only when the targets cannot all be hit: they decide which target gives way. So they need to be roughly
right, and only three kinds are computed; everything else is a judgment number with a written reason.

1. SOI tables that publish coefficients of variation (Tables 1.4, 2.1 and 3.3; error_kind 'soi_cv'). SOI does not
   link the CV workbooks from any page; they sit at https://www.irs.gov/pub/irs-soi/{yy}in14cv.xls, {yy}in21cv.xls
   and {yy}in33cv.xls (found on 22 October 2026, explore/2026-10-22/targets/irs_data/REPORT.md). The workbooks are
   kept in <out>/targets/soi_cv/raw/ with a manifest of where each came from, and prep/soi_cv.py parses them
   into <out>/targets/soi_cv/soi_cv.csv: one row per published cell, its value from the shared SOI mirror (the same cells
   targets.py reads) and its CV. A CV workbook is laid out like its table (same AGI rows, same column numbers), so a
   CV is joined to its cell by tax year, panel, AGI class and column number.
   - A target that adds up published cells (an AGI bracket merged from several of SOI's, a business group of several
     classes, the capital gains net of gains and losses) gets the standard error of the sum, the cells taken as
     independent: the square root of the sum of the cells' squared standard errors (value x CV / 100).
   - A cell SOI blanked and folded into its neighbour for disclosure ('combined') has no value and no CV of its own;
     its dollars and returns are in the receiving cell, whose CV is taken to be the CV of the combined estimate.
   - A CV of exactly 0 is kept as 0 where SOI samples every return (the $10,000,000-or-more class): there is no
     sampling error there. A target resting only on such cells (the $10M-and-over bracket, kept separate from 25
     October) gets a judgment error instead, 1% of its value (FULL_SAMPLE_SE), since a target
     with no error could never give way.
2. Other SOI tables from the same sample (Table 1.6, Table 1.2's joint returns, any 1.4-like table without CVs;
   'soi_borrowed'): each cell borrows the CV of the Table 1.4 cells of the same year closest to it. For a count, the
   Table 1.4 count cells (number of returns with an item, any AGI class or the total row) closest in number of
   returns; for an amount, the amount cells closest in amount and in their number of returns (distance in logs). The
   CV used is the median of the closest five (BORROW_K), not the single closest, so one odd cell
   does not decide it. Cells sampled in full (CV 0) and cells without a CV are not lent.
3. Survey targets ('survey_replicate'): the sampling error from the survey's own replicate weights, carried to the
   tax units by the same rule as the main weight (a tax unit's replicate weight is its household's or interview's,
   as its weight is).
   - SCF (the wealth and wealth grid targets): computed with their values by scf_targets.py, over the year's mix of
     SCF waves (TARGETS.md S9). Within each wave, the Federal Reserve's method (codebook, "Replicate weights and
     multiplicity factors"): the replicate weights are WT1Bi x MMi (missing values are 0, as in the Fed's own
     program; scf_replicates below), for the first implicate only; sampling variance = sum over the 999
     replicates of (estimate - mean of the replicate estimates)^2 / 998, on the first implicate; imputation
     variance = the variance of the five implicates' estimates (divisor 4); total variance = sampling + (1 + 1/5)
     x imputation. The percentile lines the targets are defined by are held at their full-sample values, because
     the file is asked for the share above those dollar lines. The waves are independent samples, so the pooled
     variance is the sum over waves of share^2 x the wave's variance.
   - CEX (the consumption averages): the BLS's balanced repeated replication with 44 replicate weights
     (WTREP01-WTREP44 in the FMLI files, standard method up to the 2024 data): variance = sum over the 44 replicates
     of (replicate estimate - full-sample estimate)^2 / 44.
   - CPS (the overtime averages, since 28 October): Census's successive difference replication with the ASEC's 160
     person replicate weights (REPWTP1-REPWTP160, the unit head's, from settings paths.raw.cps_repwt): variance =
     4 / 160 x the sum over the replicates of (replicate estimate - full-sample estimate)^2; over the year's mix of
     CPS years, as the CEX's.
4. Everything else ('judgment'): the people in non-filer units, which come from the filing stage's calibration
   (settings, with its one-line reason); the non-filer count's error is set in the year's targets.yaml.
Identities (a row with identity true) have error 0 ('identity').

Interface: standard_errors(rows, year) -> one row per target, keyed by (kind, group, target) as in the run's
fit.csv, with se in the units of the published statistic (on the average itself for an average, on the share for a
share, in dollars or returns for a total), error_kind and se_reason.

Data, downloaded once (public; to scratch, never to the shared raw-data folder; the login node has the network):
  <out>/targets/soi_cv/raw/       the CV workbooks, with manifest.csv (file, URL, sha256):
                          curl -sSLO https://www.irs.gov/pub/irs-soi/19in14cv.xls   (and so on for each year and table)
  <out>/data/scf_rw/<wave>/ each SCF wave's replicate weights, one folder per wave in the year's mix: p16_rw1.dta, p19_rw1.dta, p22_rw1.dta for the
                          2016, 2019 and 2022 waves:
                          curl -sSLO https://www.federalreserve.gov/econres/files/scf2019rw1s.zip; unzip scf2019rw1s.zip
                          (and scf2016rw1s.zip, scf2022rw1s.zip). Check that unzip succeeds: one download of the
                          2022 file was cut short (26 October 2026). The 2016 and 2022 files were fetched on 26
                          October 2026 from the Federal Reserve; sha256 of the .dta files:
                          p16_rw1.dta 936201623d499a61d39461c5383f1b62fee9411f0d208c719298952e3b56ab0e,
                          p19_rw1.dta 0cfe01b806045c0fc3abe8b4902804ee05dda47da490c05397b1101f6e221739,
                          p22_rw1.dta 1f0222848253ec9d42ad614f1a28bb215d3ae8de3b17a66c53308d81066e91c2.
  The CEX's replicate weights are read from the FMLI files in the shared raw-data folder (settings paths.raw.cex).
"""
import glob
import os
import re

import numpy as np
import pandas as pd

from shared import common
from prep import soi as TG

Y = common.TARGET_YEAR
CV_DIR = f'{common.TARGETS}/soi_cv'
BORROW_K = 5   # the CV borrowed is the median of this many closest Table 1.4 cells
SCF_RW_DIR = f'{common.OUT}/data/scf_rw/{{wave}}'
# the people in non-filer units: a share of the population they are counted in (nonfiler_errors)
NONFILER_PEOPLE_SE = 0.005
# A Table 1.4 target resting only on cells SOI samples in full (the $10,000,000-or-more bracket, CV 0): a judgment
# share of the value, since a target with no error cannot give way (TARGETS.md section 9, "everything else").
FULL_SAMPLE_SE = 0.01
# (1 November, decision 55) A published CV below this many percent is treated like the printed 0.00: SOI samples every
# return in the cell, so there is no sampling error to give a standard error from. SOI printed 0.01 for the
# $10,000,000-or-more cells of 2022 and 0.00 in the other years; taken literally, 0.01% of the value was about 3 returns
# on 30,108, and no weights could meet it. 0 or less turns the rule off.
FULL_SAMPLE_CV_BELOW = 0.1
AGES = common.AGE_LABELS[:common.ADULT_RANGES]
INF = np.inf

# ---- 1. SOI's published coefficients of variation ------------------------------------------------
_CV = None


def soi_cells(year, table='1.4'):
    """Table 1.4's (or the table named: '3.3' since 30 October) all-returns cells of one year with their CVs, in
    targets.py's terms: lo, hi, variable, statistic (for the items targets.py names, else the mirror's item), value
    (dollars for amounts), cv_pct, se. A year with no CV workbook (a projection year) takes the CVs of the latest year
    that has one, said in the reason."""
    global _CV
    if _CV is None:
        _CV = pd.read_csv(f'{CV_DIR}/soi_cv.csv', keep_default_na=False, na_values=[''], low_memory=False)
    c = _CV[(_CV.table.astype(str) == table) & (_CV.panel == 'All returns')]
    have = sorted(c.tax_year.unique())
    use = year if year in have else max(y for y in have if y <= year) if any(y <= year for y in have) else min(have)
    c = c[c.tax_year == use].copy()
    known = c['item'].map(TG.TABLE_ITEMS[table])   # targets.T14 names a renamed line under both its names
    c['variable'] = [k[0] if isinstance(k, tuple) else i for k, i in zip(known, c['item'])]
    c['value'] = c['value'] * np.where(c['statistic'] == 'amount', 1000.0, 1.0)   # SOI publishes thousands
    # a published CV below the line is the same as 0.00 (FULL_SAMPLE_CV_BELOW); the published figure is kept in cv_pct_published
    c['cv_pct_published'] = c['cv_pct']
    c['cv_pct'] = c['cv_pct'].mask((c['cv_pct'] > 0) & (c['cv_pct'] < FULL_SAMPLE_CV_BELOW), 0.0)
    c['se'] = c['value'] * c['cv_pct'] / 100
    c['total_row'] = ~np.isfinite(c['lo']) & ~np.isfinite(c['hi'])
    return c, use


def sum_error(cells, parts, lo, hi):
    """The standard error of a sum of published cells (independent): the cells of each (variable, statistic, sign)
    with lo >= lo and hi <= hi, AGI classes only. Returns (se, sum of the cells' values, cells used, note)."""
    t = cells[~cells.total_row] if np.isfinite(lo) or np.isfinite(hi) else cells[cells.total_row]
    var, total, n, notes = 0.0, 0.0, 0, []
    for v, st, sign in parts:
        s = t[(t.variable == v) & (t.statistic == st) & (t.lo >= lo) & (t.hi <= hi) & t.value.notna()]
        assert len(s), f'no published cell for {v} {st} in [{lo}, {hi})'
        miss = s[s.cv_pct.isna() & (s.value != 0)]
        if len(miss):   # a published value without a CV: it borrows one (none in 2019's targets)
            for r in miss.itertuples():
                cv, _ = borrow_cv(cells, r.value if st == 'count' else None, r.value if st == 'amount' else None)
                var += (r.value * cv / 100) ** 2
            notes.append(f'{len(miss)} cell(s) without a published CV borrowed one')
        var += float((s.se.fillna(0) ** 2).sum())
        total += sign * float(s.value.sum())
        n += len(s)
        if (s.cv_pct == 0).any() and (s.value != 0).any():
            notes.append('includes cells SOI samples in full (CV 0)')
        if ((s.cv_pct_published > 0) & (s.cv_pct == 0) & (s.value != 0)).any():
            notes.append(f'includes cells with a published CV below {FULL_SAMPLE_CV_BELOW:g}%, treated as 0')
    return np.sqrt(var), total, n, '; '.join(sorted(set(notes)))


def borrow_cv(cells, count=None, amount=None, returns=None):
    """The CV a cell of another SOI table borrows from Table 1.4 (same year): the median CV of the BORROW_K
    closest count cells in log number of returns (a count), or the closest amount cells in log amount and log number
    of returns of the same item (an amount; without returns, in amount alone). Cells with a CV of 0 or none are not
    lent. Returns (cv_pct, description of the donors)."""
    # lending uses the published CVs, as before decision 55: the rule that a CV below the line counts as 0 is for the
    # standard error of a target resting on such cells, not for which cells are lent (soi_cells keeps both)
    if 'cv_pct_published' in cells:
        cells = cells.assign(cv_pct=cells['cv_pct_published'])
    d = cells[(cells.cv_pct > 0) & (cells.value > 0)]
    if count is not None:
        d = d[d.statistic == 'count']
        dist = np.abs(np.log(d.value) - np.log(count))
    else:
        d = d[d.statistic == 'amount'].copy()
        n = cells[(cells.statistic == 'count')].set_index(['lo', 'hi', 'item'])['value']
        d['returns'] = [n.get((r.lo, r.hi, r.item.replace(' > amount', ' > number of returns')), np.nan)
                        for r in d.itertuples()]
        dist = (np.log(d.value) - np.log(amount)) ** 2
        if returns is not None:
            d = d[d.returns > 0]
            dist = dist[d.index] + (np.log(d.returns) - np.log(returns)) ** 2
        dist = np.sqrt(dist)
    near = d.loc[dist.sort_values(kind='stable').index[:BORROW_K]]
    cv = float(near.cv_pct.median())
    what = '; '.join(f'{r.item} in {r.row_label} ({r.value:,.0f}, CV {r.cv_pct:.2f}%)' for r in near.itertuples())
    return cv, what


def dependent_filer_item_returns(year, variable, lo, hi):
    """Table 1.7's number of returns with an item (the variable's 'soi 1.7' count) in the AGI range [lo, hi), the
    all-AGI row when lo and hi are NaN: the count an amount's borrowed CV is matched on."""
    from reweight import target_list as TLS
    t = TLS.table_17(year)
    t = t[t['item'] == TLS.table_17_item(variable, 'count')]
    x = t[t['lo'].isna()] if np.isnan(lo) else t[t['lo'].notna() & (t['lo'] >= lo) & (t['hi'] <= hi)]
    assert len(x) and (x['value'] != '').all(), f'Table 1.7 {year}: no count of returns for {variable} in [{lo}, {hi})'
    return float(x['value'].astype(float).sum())


def soi_errors(rows, year):
    """Table 1.4 targets (published CVs) and the counts of other SOI tables (borrowed CVs)."""
    from reweight import target_list as TLS
    cells, cv_year = soi_cells(year)
    yr = '' if cv_year == year else f' (the {cv_year} CVs: no CV workbook for {year})'
    out = []
    for r in rows.itertuples():
        if r.kind == 'dependent filers' and r.statistic == 'amount':
            # (2 November 2026) a Table 1.7 amount (dependent filers' net capital gain): the median CV of the Table
            # 1.4 amount cells closest in amount and in their number of returns, the cell's own count of returns
            # with the item (Table 1.7 publishes it beside the amount)
            n = dependent_filer_item_returns(year, r.variable, r.agi_lo, r.agi_hi)
            cv, what = borrow_cv(cells, amount=r.value, returns=n)
            out.append((r.Index, r.value * cv / 100, 'soi_borrowed',
                        f'Table 1.7 amount, CV {cv:.2f}%: median of the {BORROW_K} Table 1.4 amount cells closest in '
                        f'amount and in returns with the item ({n:,.0f}){yr}: {what}'))
            continue
        if r.kind in ('age by AGI', 'status by AGI'):   # (round 8) Table 1.2 by AGI borrows the same way
            # (3 November 2026, John) Table 1.6 by AGI. In an AGI group where SOI samples every return (Table 1.4's
            # number of returns there has no sampling error: the $10,000,000-or-more class), the judgment number Table
            # 1.4's and Table 4.3's fully sampled cells get. Elsewhere the CV is borrowed by count as for the totals by
            # age, but only from Table 1.4's count cells in the same AGI group: SOI's sampling rate depends on income,
            # so a cell of 3,000 returns at $2-5 million is measured far better than one of 3,000 returns at $20,000,
            # and borrowing from the whole table gave errors of 0.9% to 96% of the value with no pattern
            assert not np.isnan(r.agi_lo), f'{r.group}: an age-by-AGI target with no AGI group'
            lo, hi = float(r.agi_lo), float(r.agi_hi)
            se14, _, _, _ = sum_error(cells, [('returns', 'count', 1)], lo, hi)
            if se14 == 0:
                out.append((r.Index, FULL_SAMPLE_SE * abs(r.value), 'judgment',
                            f'{100 * FULL_SAMPLE_SE:.1f}% of the value (settings errors.full_sample_se): SOI samples '
                            f'every return in this AGI group (Table 1.4\'s number of returns there has CV 0{yr}), so '
                            'there is no sampling error; the number stands for definition gaps'))
                continue
            same = cells[~cells.total_row & (cells.lo >= lo) & (cells.hi <= hi)]
            cv, what = borrow_cv(same, count=r.value)
            assert np.isfinite(cv), f'{r.group}: no Table 1.4 count cell with a CV in its AGI group'
            out.append((r.Index, r.value * cv / 100, 'soi_borrowed',
                        f'Table 1.6 count, CV {cv:.2f}%: median of the {BORROW_K} Table 1.4 count cells in the same AGI '
                        f'group closest in returns{yr}: {what}'))
            continue
        if r.kind == 'IRS' and r.target == 'joint returns' or r.kind in ('age', 'age by status', 'dependent filers'):
            cv, what = borrow_cv(cells, count=r.value)
            table = {'IRS': '1.2', 'age': '1.6', 'age by status': '1.6', 'dependent filers': '1.7'}[r.kind]
            out.append((r.Index, r.value * cv / 100, 'soi_borrowed',
                        f'Table {table} count, CV {cv:.2f}%: median of the {BORROW_K} Table 1.4 count cells closest in '
                        f'returns{yr}: {what}'))
            continue
        # the published lines behind the target, in Table 1.4 or (a calculator column, since 30 October) Table 3.3:
        # the catalog's own lines, as target_list.expand summed them for the value
        table, parts = TLS.soi_table_lines(r.variable, r.statistic)
        use = cells if table == '1.4' else soi_cells(year, table)[0]
        if np.isnan(r.agi_lo):   # the cell 'all': the table's total row
            assert r.group == 'all', f'{r.kind} {r.group} {r.target}: no AGI range'
            lo, hi = -INF, INF
        else:
            lo, hi = float(r.agi_lo), float(r.agi_hi)
        se, total, n, note = sum_error(use, parts, lo, hi)
        if abs(total - r.value) > 1e-6 * max(abs(r.value), 1):
            note = (note + '; ' if note else '') + f'cells add to {total:,.0f}, not the target {r.value:,.0f}'
        how = 'one published cell' if n == 1 else f'sum of {n} published cells, taken as independent'
        if se == 0 and r.value != 0:
            # only cells SOI samples in full ($10,000,000 or more): no sampling error, so a judgment number
            out.append((r.Index, FULL_SAMPLE_SE * abs(r.value), 'judgment',
                        f'{100 * FULL_SAMPLE_SE:.1f}% of the value (settings errors.full_sample_se): SOI samples every '
                        f'return here (published CV 0 or below {FULL_SAMPLE_CV_BELOW:g}%, {how}{yr}), so there is no sampling error; the number stands '
                        'for the gap between the file\'s definitions and SOI\'s (the next bracket down, $5M-10M, has '
                        'CVs of 0.6-2.9% on these items in 2019)'))
            continue
        out.append((r.Index, se, 'soi_cv', f'Table {table} CV, {how}{yr}' + (f'; {note}' if note else '')))
    return out


# ---- 3. Survey targets: replicate weights ------------------------------------------------------------
def scf_replicates(wave, units):
    """Each SCF tax unit's 999 replicate weights (first implicate only): its household's WT1Bi x MMi, missing values
    0 as in the Fed's program, the same rule as the main weight (every unit of a household carries the household's
    weight). Returns (units of the first implicate, replicate weights: units x 999)."""
    path = f"{SCF_RW_DIR.format(wave=wave)}/p{wave % 100:02d}_rw1.dta"
    rw = pd.read_stata(path)
    rw.columns = [c.lower() for c in rw.columns]
    imp = rw['y1'].astype(np.int64) - 10 * rw['yy1'].astype(np.int64)
    if not (imp == 1).all():
        common.say(f'  replicate file: Y1 - 10 x YY1 takes values {imp.value_counts().to_dict()}; joined on Y1')
    R = 999
    wt = rw[[f'wt1b{i}' for i in range(1, R + 1)]].to_numpy(float)
    mm = rw[[f'mm{i}' for i in range(1, R + 1)]].to_numpy(float)
    na = np.isnan(wt) | np.isnan(mm)
    common.say(f'SCF {wave} replicate weights: {len(rw)} households; {na.mean():.3f} of household x replicate cells '
               'missing (not drawn in that replicate), set to 0 as the Fed does')
    rep = np.maximum(0, np.nan_to_num(wt)) * np.maximum(0, np.nan_to_num(mm))
    u1 = units[units['implicate'] == 1].reset_index(drop=True)
    # the units' household is the SCF case YY1 (build_scf.R), plus build_table.py's id offset for the wave, a
    # multiple of 1e8; the replicate file's Y1 is YY1 x 10 + 1
    yy1 = np.round(u1['household_id'].to_numpy(float) % 1e8).astype(np.int64)
    idx = pd.Index(rw['y1'].astype(np.int64)).get_indexer(yy1 * 10 + 1)
    assert (idx >= 0).all(), f'{(idx < 0).sum()} SCF units of the first implicate with no replicate weights'
    one = u1.groupby('household_id')['weight'].agg(lambda x: x.max() - x.min() <= 1e-9 * x.max())
    assert one.all(), 'SCF units of one household with different weights'
    common.say(f'  units of the first implicate {len(u1)}; main weight (x 5) {5 * u1.weight.sum() / 1e6:.1f} m, '
               f'replicate weights {rep[idx].sum(0).mean() / 1e6:.1f} m on average '
               f'(range {rep[idx].sum(0).min() / 1e6:.1f}-{rep[idx].sum(0).max() / 1e6:.1f} m)')
    return u1, rep[idx]


def scf_errors(rows, year):
    """The SCF targets (kinds 'wealth', 'wealth grid' and 'auto loans'): the standard errors scf_targets.py computed with their
    values, over the year's mix of SCF waves. They are recorded when target_list.expand builds the year's targets."""
    from reweight import scf_targets
    keys = list(zip(rows['kind'], rows['group'], rows['target']))
    out = []
    for i, k in zip(rows.index, keys):
        assert k in scf_targets.RESULTS, f'SCF target {k}: not among the {year} targets'
        d = scf_targets.RESULTS[k]
        note = '' if abs(d['value'] - rows.loc[i, 'value']) <= 1e-9 * max(abs(d['value']), 1) else \
            f'; computed value {d["value"]:,.6g}, not {rows.loc[i, "value"]:,.6g}'
        out.append((i, d['se'], 'survey_replicate', d['reason'] + note))
    return out


def cex_replicates(year, units):
    """Each CEX tax unit's 44 replicate weights: its interview's WTREPnn / 4 x MO_SCOPE / 3, the same rule as the main
    weight (build_cex.R's FINLWT21 / 4 x MO_SCOPE / 3), so weight x WTREPnn / FINLWT21."""
    from prep import roster_cex
    reps = [f'WTREP{i:02d}' for i in range(1, 45)]
    f = roster_cex._read('fmli', year, ['NEWID', 'FINLWT21'] + reps)
    assert not f.NEWID.duplicated().any()
    f = f.set_index('NEWID')
    check = roster_cex.interview_weights(year).set_index('NEWID')['weight']
    nid = units['newid'].to_numpy()
    assert np.isin(nid, f.index).all(), 'CEX units with no FMLI record'
    assert np.allclose(check.loc[nid].to_numpy(), units['weight'].to_numpy(), rtol=1e-9), \
        "the units' weights are not FINLWT21 / 4 x MO_SCOPE / 3"
    W = f.loc[nid, reps].to_numpy(float)
    na = np.isnan(W)
    # Balanced repeated replication: each replicate is a half-sample, so about half the interviews are missing from
    # each replicate (a blank weight, which is weight 0) and the other half carry about twice their weight. Checked
    # here: the share blank per replicate is about one half, and each replicate's weights add up to about the
    # population, as the main weight does.
    rep = np.nan_to_num(units['weight'].to_numpy(float)[:, None] * W / f.loc[nid, 'FINLWT21'].to_numpy(float)[:, None])
    share = na.mean(0)
    tot = rep.sum(0) / units['weight'].sum()
    common.say(f'CEX {year} replicate weights: {len(units)} unit rows, {len(np.unique(nid))} interviews; blank '
               f'(not in the half-sample) in each replicate: {share.min():.3f}-{share.max():.3f} of unit rows; each '
               f"replicate's total over the main weight's: {tot.min():.3f}-{tot.max():.3f}")
    assert (np.abs(share - 0.5) < 0.05).all() and (np.abs(tot - 1) < 0.1).all(), 'CEX replicate weights: not half-samples'
    return rep


def consumption_errors(rows, year):
    """The consumption targets (target_list's consumption entries): each category's average per tax unit over all units and in
    each cell of the primary filer's adult age range by married, over the year's mix of CEX years (survey_mix.py):
    each year's standard error from its own 44 replicate weights, on its spending in the target year's dollars (the
    maps' factors, so a replicate's average is grown as the full-sample one is); the years are independent samples,
    so the pooled variance is the sum over years of share^2 x the year's variance."""
    from shared import survey_mix
    parts = []
    for y, share in survey_mix.mix('cex', year):
        u = survey_mix.grown_units('cex', y)
        parts.append((y, share, u, u['weight'].to_numpy(float), cex_replicates(y, u)))
    out = []
    for r in rows.itertuples():
        g = r.group.split(', ', 1)[1]
        var, value, notes = 0.0, 0.0, []
        for y, share, u, w, rep in parts:
            if g == 'all':
                mu = np.ones(len(u), bool)
            else:
                m = re.fullmatch(r'age (.+), (married|not married)', g)
                assert m, f'consumption target {r.group}: unknown cell'
                mu = ((u['age_range'] == AGES.index(m.group(1))) & (u['married'] == int(m.group(2) == 'married'))).to_numpy()
            assert mu.any(), f'consumption target {r.group}: no CEX {y} units in the cell'
            v = u[r.target.split(',')[0]].to_numpy(float)
            full = (w * v)[mu].sum() / w[mu].sum()
            reps = (rep[mu] * v[mu, None]).sum(0) / rep[mu].sum(0)
            var += share ** 2 * ((reps - full) ** 2).sum() / 44
            value += share * full
            notes.append(f'{y} {100 * share:.2f}%: {int(mu.sum())} unit rows')
        note = '' if abs(value - r.value) <= 1e-9 * abs(r.value) else f'; pooled full-sample value {value:,.6g}, not {r.value:,.6g}'
        out.append((r.Index, float(np.sqrt(var)), 'survey_replicate',
                    'CEX year mix, replicate weights (44, BLS formula) year by year, variance = sum of share^2 x the '
                    f'year\'s; {"; ".join(notes)}{note}'))
    return out


def _ipums_fixed_width(folder, fields):
    """Columns of an IPUMS extract in fixed-width form (the .dat.gz and its DDI .xml in folder), read without ipumsr:
    each field's start, width and implied decimals from the DDI. Returns {field: float array}. Every character of
    every field read must be a digit (checked), so a blank or a sign cannot be misread."""
    import gzip
    import xml.etree.ElementTree as ET
    xml, = glob.glob(f'{folder}/*.xml')
    dat, = glob.glob(f'{folder}/*.dat.gz')
    root = ET.parse(xml).getroot()
    spec = {}
    for v in root.iter():
        if v.tag.split('}')[-1] == 'var' and v.get('ID') in fields:
            loc = next(c for c in v if c.tag.split('}')[-1] == 'location')
            spec[v.get('ID')] = (int(loc.get('StartPos')) - 1, int(loc.get('width')), int(v.get('dcml') or 0))
    missing = set(fields) - set(spec)
    assert not missing, f'{xml}: no {sorted(missing)}'
    with gzip.open(dat, 'rb') as fh:
        raw = fh.read()
    n_line = raw.index(b'\n') + 1
    assert len(raw) % n_line == 0, f'{dat}: lines of unequal length'
    a = np.frombuffer(raw, dtype=np.uint8).reshape(-1, n_line)
    out = {}
    for name, (start, width, dcml) in spec.items():
        d = a[:, start:start + width].astype(np.int64) - 48
        ok = ((d >= 0) & (d <= 9)).all(1)
        val = (np.where(ok[:, None], d, 0) @ (10 ** np.arange(width - 1, -1, -1, dtype=np.int64))).astype(float)
        if not ok.all():
            # (30 October) a field that is not all digits must still be a whole number written out, a sign and leading
            # blanks allowed (the March 2016 ASEC's replicate weights have negative values); anything else stops
            for r in np.where(~ok)[0]:
                txt = bytes(a[r, start:start + width]).decode('ascii')
                assert re.fullmatch(r' *-?[0-9]+', txt), f'{dat}: {name} holds {txt!r}, not a number'
                val[r] = float(int(txt))
            common.say(f'{dat}: {name}: {int((~ok).sum())} values with a sign or blanks read as numbers')
        out[name] = val / 10 ** dcml
    return out


CPS_REPLICATES = 160   # the ASEC's person replicate weights, successive difference replication


def cps_replicates(year, units):
    """Each CPS tax unit's 160 replicate weights. A unit's weight is its head's ASECWT (build_cps.R; the head's PERNUM
    is the last two digits of build_cps.R's record_id), so its replicate weights are the head's REPWTP1-REPWTP160 times
    the unit's weight over the head's ASECWT (1 where they agree; checked and printed). Read from the shared replicate
    weights layer (settings paths.raw.cps_repwt, sample cps<year + 1>_03s: the ASEC of March of the next year)."""
    from prep import build_table as BT
    folder = f"{common.RAW['cps_repwt']}/cps{year + 1}_03s"
    reps = [f'REPWTP{i}' for i in range(1, CPS_REPLICATES + 1)]
    f = _ipums_fixed_width(folder, ['SERIAL', 'PERNUM', 'ASECWT'] + reps)
    key = f['SERIAL'].astype(np.int64) * 100 + f['PERNUM'].astype(np.int64)
    assert len(np.unique(key)) == len(key), f'{folder}: SERIAL x 100 + PERNUM is not unique'
    order = np.argsort(key)
    sk = key[order]
    rid = np.rint(units['record_id'].to_numpy(float) - BT.CPS_ID_OFFSET - (year - 2000) * 1e9).astype(np.int64)
    pos = np.minimum(np.searchsorted(sk, rid), len(sk) - 1)
    assert (sk[pos] == rid).all(), f'CPS {year}: {int((sk[pos] != rid).sum())} units whose head is not in {folder}'
    idx = order[pos]
    asecwt = f['ASECWT'][idx]
    assert (asecwt > 0).all(), f'CPS {year}: a unit head with no ASEC weight'
    ratio = units['weight'].to_numpy(float) / asecwt
    off = np.abs(ratio - 1) > 1e-6
    W = np.column_stack([f[r][idx] for r in reps]) * ratio[:, None]
    tot = W.sum(0) / units['weight'].sum()
    common.say(f'CPS {year} replicate weights ({folder}): {len(units):,} units; the unit\'s weight equals its head\'s '
               f'ASECWT on all but {int(off.sum()):,}'
               + (f' (ratio {ratio[off].min():.4f}-{ratio[off].max():.4f} there)' if off.any() else '')
               + f'; each replicate\'s total over the main weight\'s: {tot.min():.3f}-{tot.max():.3f}')
    # (30 October) the March 2016 ASEC (tax year 2015) has a few negative person replicate weights, which successive
    # difference replication allows; they are kept and counted, and only the replicates' totals are checked
    if (W < 0).any():
        common.say(f'CPS {year}: {int((W < 0).any(1).sum())} units with a negative replicate weight in some replicate '
                   f'({int((W < 0).sum())} unit-replicate cells), kept')
    assert (np.abs(tot - 1) < 0.1).all(), f'CPS {year}: replicate weights out of line'
    return W


def overtime_errors(rows, year):
    """The CPS overtime targets (kind 'overtime'): the share of units with overtime and overtime per unit, over all
    units and by the unit's wages, over the year's mix of CPS years (target_list.cps_mix_rows), on the units asked
    the earnings questions. Each year's standard error from the ASEC's 160 person replicate weights (the head's,
    cps_replicates), by Census's successive difference replication formula: variance = 4 / 160 x the sum over the
    replicates of (replicate estimate - full-sample estimate)^2; each replicate's average recomputed in full (its
    weights in numerator and denominator). The years are independent samples, so the pooled variance is the sum over
    years of share^2 x the year's variance. The cell is read from the row (range_variable, range_lo, range_hi)."""
    from shared import survey_mix
    from reweight import target_list as TLS
    parts = []
    for y, share in survey_mix.mix('cps', year):
        u = TLS.cps_overtime_units(y)
        parts.append((y, share, u, u['weight'].to_numpy(float), cps_replicates(y, u)))
    out = []
    for r in rows.itertuples():
        rng = (r.range_variable, r.range_lo, r.range_hi) if r.range_variable else None
        var, value, notes = 0.0, 0.0, []
        for y, share, u, w, rep in parts:
            if rng:
                xg = TLS.record_value(u, rng[0])
                mu = (xg >= rng[1]) & (xg < rng[2])
            else:
                assert r.group.endswith(', all'), f'overtime target {r.group}: a cell other than all with no range'
                mu = np.ones(len(u), bool)
            assert mu.any(), f'overtime target {r.group}: no CPS {y} units in the cell'
            v = TLS.record_value(u, r.variable)
            full = (w * v)[mu].sum() / w[mu].sum()
            reps = (rep[mu] * v[mu, None]).sum(0) / rep[mu].sum(0)
            var += share ** 2 * 4 / CPS_REPLICATES * ((reps - full) ** 2).sum()
            value += share * full
            notes.append(f'{y} {100 * share:.2f}%: {int(mu.sum())} asked units')
        note = '' if abs(value - r.value) <= 1e-9 * abs(r.value) else f'; pooled full-sample value {value:,.6g}, not {r.value:,.6g}'
        out.append((r.Index, float(np.sqrt(var)), 'survey_replicate',
                    'CPS year mix, the ASEC\'s 160 person replicate weights (the unit head\'s), successive difference '
                    'replication (variance = 4/160 x sum of squared deviations) year by year, variance = sum of '
                    f'share^2 x the year\'s; {"; ".join(notes)}{note}'))
    return out


def cps_age_errors(rows, year):
    """(2 November 2026) The counts of units by the primary's age from the CPS (kind 'older ages', target_list.cps_age_rows):
    each CPS year's standard error from the ASEC's 160 person replicate weights (the unit head's, cps_replicates), by
    successive difference replication, variance = 4 / 160 x the sum over the replicates of (replicate count - full-sample
    count)^2; the years are independent samples, so the pooled variance is the sum over years of share^2 x the year's."""
    from shared import survey_mix
    from reweight import target_list as TLS
    parts = []
    for y, share in survey_mix.mix('cps', year):
        u = TLS.cps_age_units(y)
        parts.append((y, share, u, u['weight'].to_numpy(float), cps_replicates(y, u)))
    out = []
    for r in rows.itertuples():
        assert r.range_variable, f'older ages target {r.group}: no age range'
        rng = (r.range_variable, r.range_lo, r.range_hi)
        var, value, notes = 0.0, 0.0, []
        for y, share, u, w, rep in parts:
            xg = TLS.record_value(u, rng[0])
            mu = (xg >= rng[1]) & (xg < rng[2])
            full = w[mu].sum()
            reps = rep[mu].sum(0)
            var += share ** 2 * 4 / CPS_REPLICATES * ((reps - full) ** 2).sum()
            value += share * full
            notes.append(f'{y} {100 * share:.2f}%: {int(mu.sum())} sample units, se {np.sqrt(4 / CPS_REPLICATES * ((reps - full) ** 2).sum()):,.0f}')
        note = '' if abs(value - r.value) <= 1e-9 * abs(r.value) else f'; pooled full-sample value {value:,.6g}, not {r.value:,.6g}'
        out.append((r.Index, float(np.sqrt(var)), 'survey_replicate',
                    'CPS year mix, the ASEC\'s 160 person replicate weights (the unit head\'s), successive difference '
                    'replication (variance = 4/160 x sum of squared deviations) year by year, variance = sum of '
                    f'share^2 x the year\'s; {"; ".join(notes)}{note}'))
    return out


# ---- 4. Judgment -----------------------------------------------------------------------------------
def nonfiler_errors(rows, year):
    """The non-filer count and the people in non-filer units (the filing stage's calibration targets)."""
    def census(y):
        """The filing stage's population by band in year y; a year with no CPS year of its own (30 October): the
        geometric mean of the CPS years either side, or past the last CPS year the last one grown with the
        population."""
        p = f'{common.DATA}/filing/checks/{y}_nonfilers_vs_targets.csv'   # the filing stage's checks
        if os.path.exists(p):
            return pd.read_csv(p).set_index('band')['census_adults']
        from shared import growth as GR
        from reweight import target_list as TLS
        have = TLS.cps_years()
        if y > max(have):
            return census(max(have)) * GR.population(y) / GR.population(max(have))
        lo, hi = TLS.cps_neighbours(y)
        return np.sqrt(census(lo) * census(hi))
    need = rows.kind.eq('non-filer people').any()
    t = census(year).to_frame('census_adults') if need else None
    out = []
    for r in rows.itertuples():
        if r.kind == 'non-filers':
            # the year's configured error (identity or judgment), which expand_entry has already applied, with its reason
            assert pd.notna(r.se), f'{year} non-filer count: no error in the year\'s configuration'
            out.append((r.Index, r.se, 'judgment', f'Year configuration: {r.error_reason}'))
            continue
        band = 'children under 17' if r.group == 'non-filers, children under 17' else \
            r.group.replace('non-filers, heads and spouses aged ', '')
        pop = 1e6 * t.loc[band, 'census_adults']
        s = NONFILER_PEOPLE_SE
        who = 'CPS children under 17' if band == 'children under 17' else "the band's CPS adults"
        out.append((r.Index, s * pop, 'judgment',
                    f'{100 * s:.1f}% of {who} ({pop / 1e6:.1f} m; settings errors.nonfiler_people_population_se): '
                    'the target is that population less the people on returns, each known to about half a percent, '
                    'so its error scales with the population, not with the residual'))
    return out


# ---- the interface -----------------------------------------------------------------------------------
def published_value(rows):
    """The published statistic of each row, as a number: the value column, which for an average or a share holds the
    average or share itself (target_list.expand's rows; tilt.py's precheck reads it here)."""
    return rows['value'].astype(float)


def standard_errors(rows, year=Y):
    """One row per target of rows (target_list.expand's): kind, group, target, se, error_kind, se_reason. se is in the
    published statistic's units (the average itself for an average, the share for a share; the value column holds
    the published statistic). A kind this module does not know gets no error (error_kind 'none')."""
    rows = rows.reset_index(drop=True).assign(value=lambda d: published_value(d))
    res = {}
    table21 = rows['error_kind'].eq('table21') if 'error_kind' in rows else pd.Series(False, index=rows.index)
    # (autoresearch, 4 November) a calculator target on Table 4.3 (the top groups' deductions) has its error from the
    # tidy grid already (target_list.published_grid), as the Table 2.1 targets do
    grid = rows['source'].astype(str).str.match(r'soi (4\.3|2\.5) ') if 'source' in rows else pd.Series(False, index=rows.index)
    grid_calc = grid & (rows.kind == 'calculator')
    table21 = table21 | grid_calc
    by = {'soi': rows.kind.isin(['IRS', 'age', 'age by AGI', 'age by status', 'status by AGI', 'business', 'dependent filers', 'calculator']) & ~table21,
          'scf': rows.kind.isin(['wealth', 'wealth grid', 'auto loans']), 'consumption': rows.kind == 'consumption',
          'nonfiler': rows.kind.isin(['non-filers', 'non-filer people']), 'overtime': rows.kind == 'overtime',
          'older ages': rows.kind == 'older ages'}
    for name, f in [('soi', soi_errors), ('scf', scf_errors), ('consumption', consumption_errors),
                    ('nonfiler', nonfiler_errors), ('overtime', overtime_errors), ('older ages', cps_age_errors)]:
        if by[name].any():
            for i, se, kind, why in f(rows[by[name]], year):
                res[i] = (se, kind, why)
    out = rows[['kind', 'group', 'target']].copy()
    out['se'] = [res.get(i, (np.nan,))[0] for i in out.index]
    out['error_kind'] = [res.get(i, (None, 'none'))[1] for i in out.index]
    out['se_reason'] = [res.get(i, (None, None, 'no error source'))[2] for i in out.index]
    out.loc[table21, 'se'] = rows.loc[table21, 'se']
    out.loc[table21, 'error_kind'] = 'table21'
    out.loc[table21, 'se_reason'] = rows.loc[table21, 'error_reason']
    out.loc[grid_calc, 'error_kind'] = rows.loc[grid_calc, 'error_kind']
    if 'se_reason' in rows:
        out.loc[grid_calc, 'se_reason'] = rows.loc[grid_calc, 'se_reason']
    if 'identity' in rows:
        m = rows['identity'].astype(bool)
        out.loc[m, ['se', 'error_kind', 'se_reason']] = [0.0, 'identity', 'an identity: hit exactly']
    return out

