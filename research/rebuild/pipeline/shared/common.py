"""Paths, settings and small helpers that every stage uses.

Every stage, in R or Python, reads one settings file: the file named by the environment variable REBUILD_SETTINGS if
it is set, else the pipeline's own settings.json. Outputs go in the folder OUT (the environment variable REBUILD_OUT
overrides the settings), in its subfolders data/, targets/, model/, gen/ and tilt/. REBUILD_RUN names the run's
subfolder of model/, gen/ and tilt/ (by default seed<n>). The helpers apply two pieces of tax law that several stages
need (the capital loss limit and the taxable share of Social Security) and compute weighted quantiles.
"""
import json
import os

import numpy as np

PIPELINE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))    # research/rebuild/pipeline
RESOURCES = os.path.join(os.path.dirname(PIPELINE), 'resources')           # research/rebuild/resources
REPO = os.path.dirname(os.path.dirname(os.path.dirname(PIPELINE)))         # the repository root
SETTINGS = json.load(open(os.environ.get('REBUILD_SETTINGS', os.path.join(PIPELINE, 'settings.json'))))
OUT = os.environ.get('REBUILD_OUT', SETTINGS['paths']['out'])
RAW = SETTINGS['paths']['raw']
SEED = int(os.environ.get('REBUILD_SEED', SETTINGS['training']['seed']))
# The number of passes over the training data (fewer for a quick test).
PASSES = int(os.environ.get('REBUILD_PASSES', SETTINGS['training']['passes']))
PUF_YEAR = int(SETTINGS['years']['puf'])          # the PUF's tax year: the year of its records, cut points and maps
TARGET_YEAR = int(SETTINGS['years']['target'])    # the year of the reweighted file and its targets

# Where the filer records come from: 'synthetic', drawn from the networks; or 'real_puf', every 2015 PUF record with
# its own PUF items, and the networks drawing the rest.
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
TOP_CACHE = f'{OUT}/top_cache.pkl'   # the records at the top of the file (generate/top.py), read by the reweighting

# The primary filer's age range: six ranges for filers who are not dependents, then three for dependent filers.
AGE_LABELS = ['under 26', '26-34', '35-44', '45-54', '55-64', '65+', 'dep <18', 'dep 18-25', 'dep 26+']
ADULT_RANGES = 6
N_RANGES = len(AGE_LABELS)
DEPENDENT_RANGES = list(range(ADULT_RANGES, N_RANGES))

# Wage groups for reports. The lower edge is inside the group: $25,000 is in $25k-50k.
WAGE_GROUPS = [(-np.inf, 1, 'none'), (1, 25e3, '$1-25k'), (25e3, 50e3, '$25k-50k'), (50e3, 100e3, '$50k-100k'),
               (100e3, 200e3, '$100k-200k'), (200e3, np.inf, '$200k+')]


# The capital loss limit (section 1211(b)): a net capital loss counts in AGI up to $3,000, or $1,500 on a separate
# return (filing status 3). The limit is not indexed. The file holds gains and losses before the limit.
CAPITAL_LOSS_LIMIT, CAPITAL_LOSS_LIMIT_SEPARATE = 3000.0, 1500.0


def allowed_capital_gain(sched_d, filing_status=None):
    """The Schedule D net gain as it enters AGI, with a net loss held to the limit. With no filing status, every
    return is held to $3,000."""
    sched_d = np.asarray(sched_d, float)
    limit = CAPITAL_LOSS_LIMIT if filing_status is None else \
        np.where(np.asarray(filing_status) == 3, CAPITAL_LOSS_LIMIT_SEPARATE, CAPITAL_LOSS_LIMIT)
    return np.maximum(sched_d, -limit)


# The networks draw gross Social Security, taxable pensions and the non-taxable rest of pensions (on the PUF, E01500
# less E01700, never below zero). Gross pensions (Form 1040 line 5a) and taxable Social Security are computed from
# them.
PENSION_PARTS = ['txbl_pens_dist', 'nontxbl_pens_dist']
GROSS_PENSIONS = 'gross_pens_dist'
# Section 86: the base and adjusted base amounts are $25,000 and $34,000, or $32,000 and $44,000 on a joint return
# (filing status 2). A separate return takes the single amounts, as in tax-simulator-gpu. The amounts are not indexed.
SS_BRACKETS = {'single': (25_000.0, 34_000.0), 'joint': (32_000.0, 44_000.0)}
SS_RATES = (0.5, 0.85)
SS_MAGI_RATE = 0.5


def taxable_social_security(gross_ss, magi, filing_status):
    """Taxable Social Security by the section 86 worksheet, as tax-simulator-gpu computes it. magi (modified AGI) is
    AGI without taxable benefits, plus tax-exempt interest, plus the student loan interest deduction. Provisional
    income is half the benefits plus magi. Example: a single return with $20,000 of benefits and $30,000 of other
    income has provisional income of $40,000 and $9,600 of taxable benefits."""
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
    """The label of each x's dollar range, from lines = [(lo, hi, label)]. lo is inside the range, hi is not."""
    out = np.full(len(x), '', dtype=object)
    for lo, hi, lab in lines:
        out[(x >= lo) & (x < hi)] = lab
    return out


def wage_group(wages):
    """The label of each unit's wage group (WAGE_GROUPS)."""
    return groups_by(wages, WAGE_GROUPS)


_EDGES = None   # the band edges, read once


def edges():
    """The band edges written by build_table.py, read once."""
    global _EDGES
    if _EDGES is None:
        _EDGES = json.load(open(EDGES))
    return _EDGES


def meta():
    """The table's description stored with the band edges: the items, the computed totals, the surveys' slots and
    their rules."""
    return edges()['meta']


def wquantile(x, w, probs):
    """Weighted quantiles: for each probability, the smallest x at which the cumulative share of the weight w reaches
    it."""
    o = np.argsort(x, kind='stable')
    xs, F = x[o], np.cumsum(w[o]) / w.sum()
    return xs[np.minimum(np.searchsorted(F, np.asarray(probs) - 1e-12, side='left'), len(xs) - 1)]


def say(*parts):
    """Prints to the stage's log at once."""
    print(*parts, flush=True)
