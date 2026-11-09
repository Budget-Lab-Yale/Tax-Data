"""Paths, settings and small helpers shared by every stage.

Every choice of a run is in one settings file: REBUILD_SETTINGS if set (run.sbatch sets it; every stage, R and
Python, reads it), else the pipeline's own settings.json. A variant run is a copy of that file. Outputs go under OUT
(scratch), one folder per stage:
  data/     the training table, its band edges, the dollar maps   (build_table.py)
  targets/  the published SOI numbers and SOI's CVs                 (targets.py, errors.py)
  model/    the trained network                                    (train.py)
  gen/      the generated records                                  (generate.py)
  tilt/     the year's targets, weights, final file and run report (tilt.py, report_tilt.py)
Environment: REBUILD_OUT overrides OUT; REBUILD_SEED the training seed; REBUILD_RUN names the model/gen/tilt
subfolder (default seed<n>); REBUILD_PASSES the passes (for a quick test).
"""
import json
import os

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
SETTINGS = json.load(open(os.environ.get('REBUILD_SETTINGS', os.path.join(HERE, 'settings.json'))))
OUT = os.environ.get('REBUILD_OUT', SETTINGS['paths']['out'])
RAW = SETTINGS['paths']['raw']
SEED = int(os.environ.get('REBUILD_SEED', SETTINGS['training']['seed']))
PASSES = int(os.environ.get('REBUILD_PASSES', SETTINGS['training']['passes']))   # fewer for a quick test
PUF_YEAR = int(SETTINGS['years']['puf'])          # the PUF's tax year: its records, cut points and dollar maps
TARGET_YEAR = int(SETTINGS['years']['target'])    # the year of the file the tilt makes, and of its targets

# Where the filer records come from (settings generation.filers, 4 November 2026): 'synthetic', the filer batch drawn
# from the networks (the default, and every run before the setting); 'real_puf', every 2015 PUF record with its own
# PUF items, the networks drawing the rest (generate.real_puf_part). REAL_PUF is read by every stage that differs.
FILERS = SETTINGS['generation'].get('filers', 'synthetic')
assert FILERS in ('synthetic', 'real_puf'), f"settings generation.filers is 'synthetic' or 'real_puf', not {FILERS!r}"
REAL_PUF = FILERS == 'real_puf'

DATA = f'{OUT}/data'
TABLE = f'{DATA}/table.parquet'
EDGES = f'{DATA}/band_edges.json'
MAPS = f'{DATA}/maps'
TARGETS = f'{OUT}/targets'
RUN = os.environ.get('REBUILD_RUN', f'seed{SEED}')
MODEL = f'{OUT}/model/{RUN}'
GEN = f'{OUT}/gen/{RUN}'
TILT = f'{OUT}/tilt/{RUN}'

# The primary filer's age range (ages.py): six adult ranges, then three for dependent filers.
AGE_LABELS = ['under 26', '26-34', '35-44', '45-54', '55-64', '65+', 'dep <18', 'dep 18-25', 'dep 26+']
ADULT_RANGES = 6
N_RANGES = len(AGE_LABELS)
DEPENDENT_RANGES = list(range(ADULT_RANGES, N_RANGES))

# Wage groups for reports, lower edge inclusive: $25,000 is in $25k-50k.
WAGE_GROUPS = [(-np.inf, 1, 'none'), (1, 25e3, '$1-25k'), (25e3, 50e3, '$25k-50k'), (50e3, 100e3, '$50k-100k'),
               (100e3, 200e3, '$100k-200k'), (200e3, np.inf, '$200k+')]


# The capital loss limit (section 1211(b)): a net capital loss counts in AGI up to $3,000, $1,500 on a separate return
# (filing status 3); nominal, not indexed. Since 29-30 October the file draws the Schedule D gains and losses before
# the limit (kg_st, kg_lt) and the limit is applied here, where AGI is made, and by the calculator (John, 29 October).
CAPITAL_LOSS_LIMIT, CAPITAL_LOSS_LIMIT_SEPARATE = 3000.0, 1500.0


def allowed_capital_gain(sched_d, filing_status=None):
    """The Schedule D net gain as it enters AGI: the net (short term plus long term, before the limit), a loss held to
    the limit. filing_status None: every return held to $3,000."""
    sched_d = np.asarray(sched_d, float)
    limit = CAPITAL_LOSS_LIMIT if filing_status is None else \
        np.where(np.asarray(filing_status) == 3, CAPITAL_LOSS_LIMIT_SEPARATE, CAPITAL_LOSS_LIMIT)
    return np.maximum(sched_d, -limit)


# Social Security and pensions (3 November, John). The network draws three parts: gross Social Security (gross_ss),
# taxable pensions (txbl_pens_dist) and the non-taxable rest of pensions (nontxbl_pens_dist: on the PUF E01500 less
# E01700, never below zero). Two items are computed, never drawn: gross pensions (gross_pens_dist, Form 1040 line 5a)
# = taxable pensions + the rest; and taxable Social Security (ss_taxable) by the section 86 worksheet below, which is
# part of the file's AGI and is never targeted. Until 3 November the network drew gross Social Security plus total
# pensions (retirement_gross, E02400 + E01500) and taxable Social Security (E02500) as slots of their own.
PENSION_PARTS = ['txbl_pens_dist', 'nontxbl_pens_dist']
GROSS_PENSIONS = 'gross_pens_dist'
# Section 86, as tax-simulator-gpu has it (taxes/calc/agi.py, calc_ss; config/scenarios/tax_law/default/ss.yaml):
# half of the benefits plus modified AGI is the provisional income; the base and adjusted amounts are $25,000 and
# $34,000, $32,000 and $44,000 on a joint return (filing status 2). A separate return takes the single amounts, as the
# calculator's filing-status map does. Not indexed.
SS_BRACKETS = {'single': (25_000.0, 34_000.0), 'joint': (32_000.0, 44_000.0)}
SS_RATES = (0.5, 0.85)
SS_MAGI_RATE = 0.5


def taxable_social_security(gross_ss, magi, filing_status):
    """Taxable Social Security by the section 86 worksheet, the calculator's formula (calc_ss): provisional income
    p = max(0.5 x benefits + modified AGI, 0); 50% of the part of p between the base and the adjusted amount and 85% of
    the part above the adjusted amount, each part at most the benefits; the sum at most 85% of the benefits.
    magi: AGI without taxable benefits, plus tax-exempt interest, plus the student loan interest deduction (the
    calculator's magi_ss: income other than benefits, plus tax-exempt interest, less the adjustments other than the
    student loan interest deduction). Worked example, a single return with $20,000 of benefits and $30,000 of other
    income: p = $40,000; 50% of $9,000 (the part from $25,000 to $34,000) is $4,500; 85% of $6,000 (the part above
    $34,000) is $5,100; taxable $9,600, below 85% of the benefits ($17,000)."""
    ss = np.asarray(gross_ss, float)
    magi = np.asarray(magi, float)
    fs = np.asarray(filing_status)
    assert np.isfinite(ss).all() and np.isfinite(magi).all(), 'taxable Social Security: a missing benefit or income'
    assert (ss >= 0).all(), 'taxable Social Security: gross benefits below zero'
    assert np.isin(fs, [1, 2, 3, 4]).all(), 'taxable Social Security: a filing status outside 1-4'
    joint = fs == 2
    base = np.where(joint, SS_BRACKETS['joint'][0], SS_BRACKETS['single'][0])
    adjusted = np.where(joint, SS_BRACKETS['joint'][1], SS_BRACKETS['single'][1])
    p = np.maximum(SS_MAGI_RATE * ss + magi, 0.0)
    tier1 = np.minimum(np.clip(p, base, adjusted) - base, ss) * SS_RATES[0]
    tier2 = np.minimum(np.maximum(p - adjusted, 0.0), ss) * SS_RATES[1]
    return np.minimum(tier1 + tier2, SS_RATES[1] * ss)


def groups_by(x, lines):
    """Labels of dollar ranges: lines = [(lo, hi, label)], lo inclusive."""
    out = np.full(len(x), '', dtype=object)
    for lo, hi, lab in lines:
        out[(x >= lo) & (x < hi)] = lab
    return out


def wage_group(wages):
    return groups_by(wages, WAGE_GROUPS)


_EDGES = None


def edges():
    """The band edges written by build_table.py (read once)."""
    global _EDGES
    if _EDGES is None:
        _EDGES = json.load(open(EDGES))
    return _EDGES


def meta():
    """The band edges' meta: items, computed totals, the surveys' slots, AGI signs (build_table.py)."""
    return edges()['meta']


def wquantile(x, w, probs):
    """Smallest x whose cumulative weight share reaches each probability."""
    o = np.argsort(x, kind='stable')
    xs, F = x[o], np.cumsum(w[o]) / w.sum()
    return xs[np.minimum(np.searchsorted(F, np.asarray(probs) - 1e-12, side='left'), len(xs) - 1)]


def say(*parts):
    print(*parts, flush=True)
