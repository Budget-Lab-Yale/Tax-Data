"""Each target's standard error. The reweighting measures each target's miss in standard errors, so a target with a
smaller standard error is held closer.

Standard errors come from four sources:

1. SOI's published coefficients of variation (CVs) for Tables 1.4, 2.1 and 3.3. A CV is the standard error as a
   percentage of the value.
2. For the other SOI tables, which publish no CVs, a CV borrowed from the Table 1.4 cells of the same year that are
   closest in size.
3. For targets from surveys (the SCF, CEX and CPS), the survey's replicate weights: alternative sets of weights that
   the survey publishes so that users can measure sampling error. The estimate is computed once with each set, and
   the spread of those estimates gives the variance.
4. For everything else, a judgment number with a written reason.

Identities have a standard error of zero. standard_errors(rows, year) returns each target's standard error in the units
of the published statistic, with its kind and reason. TARGETS.md, "Standard errors", describes the rules.
"""
import glob
import gzip
import os
import re
import xml.etree.ElementTree as ET
from functools import lru_cache

import numpy as np
import pandas as pd

from shared import common
from shared import growth as GR
from shared import survey_mix
from prep import soi as TG
from reweight import target_list as TLS

# Public inputs downloaded once by hand to the output folder:
#   <out>/targets/soi_cv/raw/: SOI's CV workbooks, https://www.irs.gov/pub/irs-soi/{yy}in14cv.xls, {yy}in21cv.xls and
#     {yy}in33cv.xls, with manifest.csv (file, URL, sha256). prep/soi_cv.py reads them into soi_cv.csv.
#   <out>/data/scf_rw/<wave>/: each SCF wave's replicate weights, unzipped from
#     https://www.federalreserve.gov/econres/files/scf<year>rw1s.zip. The sha256 checksums of the .dta files:
#     p16_rw1.dta 936201623d499a61d39461c5383f1b62fee9411f0d208c719298952e3b56ab0e
#     p19_rw1.dta 0cfe01b806045c0fc3abe8b4902804ee05dda47da490c05397b1101f6e221739
#     p22_rw1.dta 1f0222848253ec9d42ad614f1a28bb215d3ae8de3b17a66c53308d81066e91c2
# The CEX and CPS replicate weights are read from the shared raw-data folder.
CV_DIR = f'{common.TARGETS}/soi_cv'
BORROW_K = 5   # a borrowed CV is the median CV of this many closest Table 1.4 cells
SCF_RW_DIR = f'{common.OUT}/data/scf_rw/{{wave}}'
# The standard error of the number of people in non-filer units, as a share of the population they belong to
# (nonfiler_errors).
NONFILER_PEOPLE_SE = 0.005
# The standard error, as a share of the value, of a Table 1.4 target that rests only on cells SOI samples in full (the
# $10,000,000-or-more bracket, CV 0). It is a judgment number. A target with no error at all would have to be met
# exactly, whatever it cost the other targets.
FULL_SAMPLE_SE = 0.01
# A published CV below this many percent is treated as 0.00, the CV SOI prints where it samples every return in the
# cell. SOI printed 0.01 for the $10,000,000-or-more cells of 2022. Taken literally, that is a standard error of about
# 3 returns out of 30,108, which no weights could meet. A value of 0 or less turns the rule off.
FULL_SAMPLE_CV_BELOW = 0.1
AGES = common.AGE_LABELS[:common.ADULT_RANGES]
CPS_REPLICATES = 160   # the number of the ASEC's person replicate weights (successive difference replication)


# ---- SOI targets: published and borrowed coefficients of variation ----

@lru_cache(maxsize=None)
def _cv_table():
    """The table of SOI's published CVs (soi_cv.csv), read once."""
    return pd.read_csv(f'{CV_DIR}/soi_cv.csv', keep_default_na=False, na_values=[''], low_memory=False)


def soi_cells(year, table='1.4'):
    """The all-returns cells of Table 1.4 (or of the table named, '3.3') in one year, with their CVs, in prep/soi.py's
    terms: lo, hi, variable, statistic (the name soi.py gives the item, or else the mirror's item), value (in dollars
    for amounts), cv_pct and se. A year with no CV workbook (a projection year) takes the CVs of the latest year that
    has one. Returns the cells and the year whose CVs were used."""
    cv = _cv_table()
    c = cv[(cv.table.astype(str) == table) & (cv.panel == 'All returns')]
    have = sorted(c.tax_year.unique())
    use = year if year in have else max(y for y in have if y <= year) if any(y <= year for y in have) else min(have)
    c = c[c.tax_year == use].copy()
    known = c['item'].map(TG.TABLE_ITEMS[table])   # soi.py lists a renamed line under both its names
    c['variable'] = [k[0] if isinstance(k, tuple) else i for k, i in zip(known, c['item'])]
    c['value'] = c['value'] * np.where(c['statistic'] == 'amount', 1000.0, 1.0)   # SOI publishes amounts in thousands
    # A published CV below FULL_SAMPLE_CV_BELOW counts as 0. The published figure is kept in cv_pct_published.
    c['cv_pct_published'] = c['cv_pct']
    c['cv_pct'] = c['cv_pct'].mask((c['cv_pct'] > 0) & (c['cv_pct'] < FULL_SAMPLE_CV_BELOW), 0.0)
    c['se'] = c['value'] * c['cv_pct'] / 100
    c['total_row'] = ~np.isfinite(c['lo']) & ~np.isfinite(c['hi'])
    return c, use


def sum_error(cells, parts, lo, hi):
    """The standard error of a sum of published cells, treating the cells as independent. For each (variable,
    statistic, sign) in parts, the sum takes the cells of AGI classes that lie within [lo, hi). With lo and hi both
    infinite it takes the total row instead. Returns (standard error, the sum of the cells' values, the number of cells
    used, a note)."""
    t = cells[~cells.total_row] if np.isfinite(lo) or np.isfinite(hi) else cells[cells.total_row]
    var, total, n, notes = 0.0, 0.0, 0, []
    for v, st, sign in parts:
        s = t[(t.variable == v) & (t.statistic == st) & (t.lo >= lo) & (t.hi <= hi) & t.value.notna()]
        assert len(s), f'no published cell for {v} {st} in [{lo}, {hi})'
        miss = s[s.cv_pct.isna() & (s.value != 0)]
        if len(miss):   # a published value without a CV borrows one
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
    """The CV that a cell of another SOI table borrows from Table 1.4 of the same year: the median CV of the BORROW_K
    closest Table 1.4 cells. For a count, the closest count cells in log number of returns. For an amount, the closest
    amount cells in log amount and in log number of returns with the item (in log amount alone when returns is not
    given). Cells with a CV of 0 or no CV are not lent. Returns (CV in percent, a description of the cells used)."""
    # Lending uses the published CVs. The rule that a CV below FULL_SAMPLE_CV_BELOW counts as 0 applies to the standard
    # error of a target that rests on such cells, not to the choice of cells to lend (soi_cells keeps both).
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
    """Table 1.7's number of returns with an item (the variable's 'soi 1.7' count) in the AGI range [lo, hi), or in
    the all-AGI row when lo and hi are NaN. A Table 1.7 amount borrows its CV from cells matched on this count."""
    t = TLS.table_17(year)
    t = t[t['item'] == TLS.table_17_item(variable, 'count')]
    x = t[t['lo'].isna()] if np.isnan(lo) else t[t['lo'].notna() & (t['lo'] >= lo) & (t['hi'] <= hi)]
    assert len(x) and (x['value'] != '').all(), f'Table 1.7 {year}: no count of returns for {variable} in [{lo}, {hi})'
    return float(x['value'].astype(float).sum())


def soi_errors(rows, year):
    """The standard errors of the SOI targets: published CVs for Tables 1.4 and 3.3, and borrowed CVs for the counts
    and amounts of the other SOI tables. Returns a list of (row index, standard error, error kind, reason)."""
    cells, cv_year = soi_cells(year)
    yr = '' if cv_year == year else f' (the {cv_year} CVs: no CV workbook for {year})'
    out = []
    for r in rows.itertuples():
        if r.kind == 'dependent filers' and r.statistic == 'amount':
            # A Table 1.7 amount borrows from the Table 1.4 amount cells closest in amount and in the number of returns
            # with the item, which Table 1.7 publishes beside the amount.
            n = dependent_filer_item_returns(year, r.variable, r.agi_lo, r.agi_hi)
            cv, what = borrow_cv(cells, amount=r.value, returns=n)
            out.append((r.Index, r.value * cv / 100, 'soi_borrowed',
                        f'Table 1.7 amount, CV {cv:.2f}%: median of the {BORROW_K} Table 1.4 amount cells closest in '
                        f'amount and in returns with the item ({n:,.0f}){yr}: {what}'))
        elif r.kind in ('age by AGI', 'status by AGI'):   # Tables 1.6 and 1.2 by AGI
            # Where SOI samples every return in the AGI group, the error is the judgment number FULL_SAMPLE_SE.
            # Elsewhere the CV is borrowed only from Table 1.4's count cells in the same AGI group. SOI's sampling rate
            # rises with income, so 3,000 returns at $2-5 million are measured far better than 3,000 returns at
            # $20,000.
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
        elif r.kind == 'IRS' and r.target == 'joint returns' or r.kind in ('age', 'age by status', 'dependent filers'):
            cv, what = borrow_cv(cells, count=r.value)
            table = {'IRS': '1.2', 'age': '1.6', 'age by status': '1.6', 'dependent filers': '1.7'}[r.kind]
            out.append((r.Index, r.value * cv / 100, 'soi_borrowed',
                        f'Table {table} count, CV {cv:.2f}%: median of the {BORROW_K} Table 1.4 count cells closest in '
                        f'returns{yr}: {what}'))
        else:
            # The published lines behind the target, in Table 1.4 or, for a calculator column, Table 3.3. They are the
            # catalog's lines, which target_list.expand summed for the value.
            table, parts = TLS.soi_table_lines(r.variable, r.statistic)
            use = cells if table == '1.4' else soi_cells(year, table)[0]
            if np.isnan(r.agi_lo):   # the cell 'all': the table's total row
                assert r.group == 'all', f'{r.kind} {r.group} {r.target}: no AGI range'
                lo, hi = -np.inf, np.inf
            else:
                lo, hi = float(r.agi_lo), float(r.agi_hi)
            se, total, n, note = sum_error(use, parts, lo, hi)
            if abs(total - r.value) > 1e-6 * max(abs(r.value), 1):
                note = (note + '; ' if note else '') + f'cells add to {total:,.0f}, not the target {r.value:,.0f}'
            how = 'one published cell' if n == 1 else f'sum of {n} published cells, taken as independent'
            if se == 0 and r.value != 0:
                # Only cells SOI samples in full ($10,000,000 or more) have no sampling error. They get the judgment
                # number.
                out.append((r.Index, FULL_SAMPLE_SE * abs(r.value), 'judgment',
                            f'{100 * FULL_SAMPLE_SE:.1f}% of the value (settings errors.full_sample_se): SOI samples every '
                            f'return here (published CV 0 or below {FULL_SAMPLE_CV_BELOW:g}%, {how}{yr}), so there is no sampling error; the number stands '
                            'for the gap between the file\'s definitions and SOI\'s (the next bracket down, $5M-10M, has '
                            'CVs of 0.6-2.9% on these items in 2019)'))
            else:
                out.append((r.Index, se, 'soi_cv', f'Table {table} CV, {how}{yr}' + (f'; {note}' if note else '')))
    return out


# ---- Survey targets: replicate weights ----
# Each survey's replicate weights are computed for its tax units by the same rule as the unit's main weight.
def scf_replicates(wave, units):
    """Each SCF tax unit's 999 replicate weights, for the units of the first implicate only. Replicate weight i of a
    unit is its household's WT1Bi times MMi, with missing values set to 0 as in the Federal Reserve's own program.
    Every unit of a household carries the household's weight, as for the main weight. Returns the units of the first
    implicate and their replicate weights (units by 999)."""
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
    # A unit's household id is the SCF case number YY1 (build_scf.R) plus build_table.py's id offset for the wave, a
    # multiple of 1e8. The replicate file's Y1 is YY1 x 10 + 1.
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
    """The standard errors of the SCF targets (kinds 'wealth', 'wealth grid' and 'auto loans'). scf_targets.py
    computes them, over the year's mix of SCF waves, when target_list.expand builds the year's targets, and this
    function reads them back."""
    from reweight import scf_targets
    out = []
    for i, k in zip(rows.index, zip(rows['kind'], rows['group'], rows['target'])):
        assert k in scf_targets.RESULTS, f'SCF target {k}: not among the {year} targets'
        d = scf_targets.RESULTS[k]
        note = '' if abs(d['value'] - rows.loc[i, 'value']) <= 1e-9 * max(abs(d['value']), 1) else \
            f'; computed value {d["value"]:,.6g}, not {rows.loc[i, "value"]:,.6g}'
        out.append((i, d['se'], 'survey_replicate', d['reason'] + note))
    return out


def cex_replicates(year, units):
    """Each CEX tax unit's 44 replicate weights: its interview's WTREPnn / 4 x MO_SCOPE / 3, the same rule as the main
    weight (build_cex.R's FINLWT21 / 4 x MO_SCOPE / 3). Each is therefore the unit's weight times WTREPnn / FINLWT21."""
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
    # The CEX uses balanced repeated replication. Each replicate is a half-sample: about half the interviews are
    # missing from it (a blank weight, which counts as 0), and the other half carry about twice their weight. The check
    # below confirms that about half the weights of each replicate are blank, and that each replicate's weights add up
    # to about the population, as the main weight does.
    rep = np.nan_to_num(units['weight'].to_numpy(float)[:, None] * W / f.loc[nid, 'FINLWT21'].to_numpy(float)[:, None])
    share = na.mean(0)
    tot = rep.sum(0) / units['weight'].sum()
    common.say(f'CEX {year} replicate weights: {len(units)} unit rows, {len(np.unique(nid))} interviews; blank '
               f'(not in the half-sample) in each replicate: {share.min():.3f}-{share.max():.3f} of unit rows; each '
               f"replicate's total over the main weight's: {tot.min():.3f}-{tot.max():.3f}")
    assert (np.abs(share - 0.5) < 0.05).all() and (np.abs(tot - 1) < 0.1).all(), 'CEX replicate weights: not half-samples'
    return rep


@lru_cache(maxsize=None)
def _ipums_fixed_width(folder, fields):
    """Columns of an IPUMS extract in fixed-width form (the .dat.gz file and its DDI .xml file in folder), read
    without the ipumsr package. The DDI gives each field's start, width and number of implied decimals. Returns a dict
    of field: float array. A field that is not all digits must be a whole number with an optional sign and leading
    blanks, or the run stops. Each folder and list of fields is read once (the overtime and age errors ask for the
    same columns of the same year)."""
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
            # A field that is not all digits must still be a whole number, with a sign and leading blanks allowed
            # (the March 2016 ASEC's replicate weights have negative values). Anything else stops the run.
            for r in np.where(~ok)[0]:
                txt = bytes(a[r, start:start + width]).decode('ascii')
                assert re.fullmatch(r' *-?[0-9]+', txt), f'{dat}: {name} holds {txt!r}, not a number'
                val[r] = float(int(txt))
            common.say(f'{dat}: {name}: {int((~ok).sum())} values with a sign or blanks read as numbers')
        out[name] = val / 10 ** dcml
    return out


def cps_replicates(year, units):
    """Each CPS tax unit's 160 replicate weights: its head's REPWTP1-REPWTP160 times the ratio of the unit's weight to
    the head's ASECWT (a ratio of 1 where they agree). The head's PERNUM is the last two digits of the unit's record
    id. The weights are read from the shared replicate-weights extract of the ASEC of March of the following year
    (settings paths.raw.cps_repwt)."""
    from prep import build_table as BT
    folder = f"{common.RAW['cps_repwt']}/cps{year + 1}_03s"
    reps = [f'REPWTP{i}' for i in range(1, CPS_REPLICATES + 1)]
    f = _ipums_fixed_width(folder, ('SERIAL', 'PERNUM', 'ASECWT', *reps))
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
    # The March 2016 ASEC (tax year 2015) has a few negative person replicate weights, which successive difference
    # replication allows. They are kept and counted, and only the replicates' totals are checked.
    if (W < 0).any():
        common.say(f'CPS {year}: {int((W < 0).any(1).sum())} units with a negative replicate weight in some replicate '
                   f'({int((W < 0).sum())} unit-replicate cells), kept')
    assert (np.abs(tot - 1) < 0.1).all(), f'CPS {year}: replicate weights out of line'
    return W


def year_mix_estimates(rows, year, source, units_of, replicates_of, cell_of):
    """For each target row and each year of the survey's mix: the year, its share, the mask of the cell's units, the
    estimate with the main weights, and the estimates with each replicate weight. cell_of(row, units, year) returns
    the mask of the cell's units and each unit's value, or None for the value when the target is a count of units
    (otherwise the estimate is the weighted mean of the value). Returns a list of (row, [(year, share, mask, full,
    replicates)])."""
    parts = []
    for y, share in survey_mix.mix(source, year):
        u = units_of(y)
        parts.append((y, share, u, u['weight'].to_numpy(float), replicates_of(y, u)))
    out = []
    for r in rows.itertuples():
        per = []
        for y, share, u, w, rep in parts:
            mu, v = cell_of(r, u, y)
            if v is None:
                full, reps = w[mu].sum(), rep[mu].sum(0)
            else:
                full, reps = (w * v)[mu].sum() / w[mu].sum(), (rep[mu] * v[mu, None]).sum(0) / rep[mu].sum(0)
            per.append((y, share, mu, full, reps))
        out.append((r, per))
    return out


def pooled_note(r, per):
    """A note when the pooled estimate with the main weights differs from the target's value; empty otherwise."""
    value = sum(share * full for _, share, _, full, _ in per)
    return '' if abs(value - r.value) <= 1e-9 * abs(r.value) else f'; pooled full-sample value {value:,.6g}, not {r.value:,.6g}'


def _consumption_cell(r, u, y):
    """The CEX units in a consumption target's cell (all units, or one age range and marital status), and each unit's
    spending on the target's category."""
    g = r.group.split(', ', 1)[1]
    if g == 'all':
        mu = np.ones(len(u), bool)
    else:
        m = re.fullmatch(r'age (.+), (married|not married)', g)
        assert m, f'consumption target {r.group}: unknown cell'
        mu = ((u['age_range'] == AGES.index(m.group(1))) & (u['married'] == int(m.group(2) == 'married'))).to_numpy()
    assert mu.any(), f'consumption target {r.group}: no CEX {y} units in the cell'
    return mu, u[r.target.split(',')[0]].to_numpy(float)


def consumption_errors(rows, year):
    """The standard errors of the consumption targets: each category's average per tax unit, over all units and by
    age range and marital status, over the year's mix of CEX years. Each year's standard error comes from its 44
    replicate weights, on spending in target-year dollars. The years are independent samples, so the pooled variance
    is the sum over years of share squared times the year's variance."""
    out = []
    for r, per in year_mix_estimates(rows, year, 'cex', lambda y: survey_mix.grown_units('cex', y), cex_replicates,
                                     _consumption_cell):
        var = sum(share ** 2 * ((reps - full) ** 2).sum() / 44 for _, share, _, full, reps in per)
        notes = [f'{y} {100 * share:.2f}%: {int(mu.sum())} unit rows' for y, share, mu, _, _ in per]
        note = pooled_note(r, per)
        out.append((r.Index, float(np.sqrt(var)), 'survey_replicate',
                    'CEX year mix, replicate weights (44, BLS formula) year by year, variance = sum of share^2 x the '
                    f'year\'s; {"; ".join(notes)}{note}'))
    return out


def _overtime_cell(r, u, y):
    """The CPS units in an overtime target's cell (all units, or a range of wages), and each unit's value of the
    target's variable."""
    if r.range_variable:
        xg = TLS.record_value(u, r.range_variable)
        mu = (xg >= r.range_lo) & (xg < r.range_hi)
    else:
        assert r.group.endswith(', all'), f'overtime target {r.group}: a cell other than all with no range'
        mu = np.ones(len(u), bool)
    assert mu.any(), f'overtime target {r.group}: no CPS {y} units in the cell'
    return mu, TLS.record_value(u, r.variable)


def overtime_errors(rows, year):
    """The standard errors of the CPS overtime targets: the share of units with overtime and overtime per unit, over
    all units and by the unit's wages, among the units asked the earnings questions, over the year's mix of CPS years.
    Each year's variance follows Census's formula for successive difference replication: 4 / 160 times the sum over
    the replicates of (replicate estimate - main estimate)^2. The years are pooled as independent samples."""
    out = []
    for r, per in year_mix_estimates(rows, year, 'cps', TLS.cps_overtime_units, cps_replicates, _overtime_cell):
        var = sum(share ** 2 * 4 / CPS_REPLICATES * ((reps - full) ** 2).sum() for _, share, _, full, reps in per)
        notes = [f'{y} {100 * share:.2f}%: {int(mu.sum())} asked units' for y, share, mu, _, _ in per]
        note = pooled_note(r, per)
        out.append((r.Index, float(np.sqrt(var)), 'survey_replicate',
                    'CPS year mix, the ASEC\'s 160 person replicate weights (the unit head\'s), successive difference '
                    'replication (variance = 4/160 x sum of squared deviations) year by year, variance = sum of '
                    f'share^2 x the year\'s; {"; ".join(notes)}{note}'))
    return out


def _older_age_cell(r, u, y):
    """The CPS units in an older-ages target's age range. The target is a count of units, so there is no value."""
    assert r.range_variable, f'older ages target {r.group}: no age range'
    xg = TLS.record_value(u, r.range_variable)
    return (xg >= r.range_lo) & (xg < r.range_hi), None


def cps_age_errors(rows, year):
    """The standard errors of the CPS counts of units by the primary filer's age (kind 'older ages'), computed as in
    overtime_errors."""
    out = []
    for r, per in year_mix_estimates(rows, year, 'cps', TLS.cps_age_units, cps_replicates, _older_age_cell):
        var = sum(share ** 2 * 4 / CPS_REPLICATES * ((reps - full) ** 2).sum() for _, share, _, full, reps in per)
        notes = [f'{y} {100 * share:.2f}%: {int(mu.sum())} sample units, se {np.sqrt(4 / CPS_REPLICATES * ((reps - full) ** 2).sum()):,.0f}'
                 for y, share, mu, full, reps in per]
        note = pooled_note(r, per)
        out.append((r.Index, float(np.sqrt(var)), 'survey_replicate',
                    'CPS year mix, the ASEC\'s 160 person replicate weights (the unit head\'s), successive difference '
                    'replication (variance = 4/160 x sum of squared deviations) year by year, variance = sum of '
                    f'share^2 x the year\'s; {"; ".join(notes)}{note}'))
    return out


# ---- Judgment ----

def _census_adults(y):
    """The filing stage's population by band in year y, in millions, read from its checks. A year with no CPS year of
    its own takes the geometric mean of the CPS years on either side. A year after the last CPS year takes the last
    one, grown with the population."""
    p = f'{common.DATA}/filing/checks/{y}_nonfilers_vs_targets.csv'
    if os.path.exists(p):
        return pd.read_csv(p).set_index('band')['census_adults']
    have = TLS.cps_years()
    if y > max(have):
        return _census_adults(max(have)) * GR.population(y) / GR.population(max(have))
    lo, hi = TLS.cps_neighbours(y)
    return np.sqrt(_census_adults(lo) * _census_adults(hi))


def nonfiler_errors(rows, year):
    """The standard errors of the non-filer count and of the people in non-filer units (the targets the filing stage
    calibrates to)."""
    t = _census_adults(year).to_frame('census_adults') if rows.kind.eq('non-filer people').any() else None
    out = []
    for r in rows.itertuples():
        if r.kind == 'non-filers':
            # The error set in the year's configuration (identity or judgment), which expand_entry has already
            # applied, with its reason.
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


# ---- All targets ----

# Each function above, with the kinds of target it gives errors for.
SOURCES = [(soi_errors, ['IRS', 'age', 'age by AGI', 'age by status', 'status by AGI', 'business', 'dependent filers',
                         'calculator']),
           (scf_errors, ['wealth', 'wealth grid', 'auto loans']), (consumption_errors, ['consumption']),
           (nonfiler_errors, ['non-filers', 'non-filer people']), (overtime_errors, ['overtime']),
           (cps_age_errors, ['older ages'])]


def standard_errors(rows, year):
    """One row per target of rows (from target_list.expand): kind, group, target, se, error_kind and se_reason. se is
    in the units of the published statistic: for an average, of the average itself; for a share, of the share. A kind
    this module does not know gets no error (error_kind 'none')."""
    rows = rows.reset_index(drop=True).assign(value=lambda d: d['value'].astype(float))
    # A Table 2.1 target, or a calculator target on Table 4.3 or 2.5, already has its error from its table
    # (target_list.published_21, published_grid, published_25).
    grid_calc = rows['source'].astype(str).str.match(r'soi (4\.3|2\.5) ') & (rows.kind == 'calculator')
    table21 = rows['error_kind'].eq('table21') | grid_calc
    res = {}
    for f, kinds in SOURCES:
        m = rows.kind.isin(kinds) & ~table21
        if m.any():
            for i, se, kind, why in f(rows[m], year):
                res[i] = (se, kind, why)
    out = rows[['kind', 'group', 'target']].copy()
    out['se'] = [res.get(i, (np.nan,))[0] for i in out.index]
    out['error_kind'] = [res.get(i, (None, 'none'))[1] for i in out.index]
    out['se_reason'] = [res.get(i, (None, None, 'no error source'))[2] for i in out.index]
    out.loc[table21, 'se'] = rows.loc[table21, 'se']
    out.loc[table21, 'error_kind'] = 'table21'
    out.loc[table21, 'se_reason'] = rows.loc[table21, 'error_reason']
    out.loc[grid_calc, 'error_kind'] = rows.loc[grid_calc, 'error_kind']
    m = rows['identity'].astype(bool)
    out.loc[m, ['se', 'error_kind', 'se_reason']] = [0.0, 'identity', 'an identity: hit exactly']
    return out
