"""
02_model.py  (minimal example, step 2, part 1 of 2: the data, the network, training)

What it does
  Trains one small transformer on the stacked 2015 PUF + 2016 SCF table from
  step 1. A record is a short list of slots: source and year (context, always
  given, never predicted), then married, n_dep and the six amount bands
  (predicted). Two slots belong to one source only: net_worth exists only on
  SCF records and other_agi (AGI minus the four named incomes) only on PUF
  records. On the other source the slot is absent, not masked: the network
  never sees it and is never asked to predict it.

  Training is "masked any-subset": for each record, hide a random subset of
  its predicted slots, replace them with a mask token, and score the network's
  predictions for the hidden slots only. Over many passes one network learns
  every conditional distribution (dividends given wages, married given the
  incomes, ...), including the all-hidden case, which is the joint
  distribution that generation in step 3 starts from.

  Weights follow the plain rule in 02_MODEL_SHAPE.md: survey weights as they
  are, rescaled within each source (and split) to sum to the source's record
  count. Within a source records count by weight; across sources each source
  counts by its number of records.

Which table and which folders (environment variables; the experiment grid
is research/rebuild/minimal/09_BRIDGE_EXPERIMENTS.md):
  MINIMAL_TABLE=<tag>  reads data/stacked_<tag>.parquet and
         data/band_edges_<tag>.json. Unset: stacked.parquet and
         band_edges.json (the v2 model). If the table has the columns
         interest_small_band, dividends_small_band, capital_gains_small_band
         (the part of each amount below a dollar line), those become
         PUF-owned slots, absent on SCF rows, like other_agi.
  MINIMAL_RUN=<T>      model folder model/<T>/, generation folder gen/<T>/.
         Unset: model/ and gen/. MINIMAL_MODEL_DIR and MINIMAL_GEN_DIR,
         when set, override either.
  MINIMAL_EXPERIMENT   older name, kept as an alias: A or B means
         MINIMAL_TABLE=A or B with folders model/expA/, gen/expA/ (or B).
  MINIMAL_COARSEN=1    turns on the coarsened-zero loss of the v3 model
         (off by default; it failed).
  MINIMAL_ALL_RECORDS=1  (2026-10-15, the v4 baseline, 15_BASELINE_V4.md;
         John's decision of 14 October) Train on every record except a 2%
         held-back slice. The slice is drawn by household with a fixed seed
         (HOLDBACK_SEED): on the PUF the household is the record, on the SCF
         all units and implicates of a household go together, as in step 1's
         split. read_table() then relabels the split column: held-back rows
         'test', all others 'train' (step 1's validation and test rows
         included). Training runs all MINIMAL_MAX_EPOCHS epochs and keeps the
         last one: the held-back loss is printed each epoch but chooses
         nothing, so it stays a clean held-out score. train_meta.json gets the
         held-back loss per variable and source. 02_evaluate.py then scores
         the held-back slice wherever it scored the test split. Off by
         default: the 80/10/10 split as before.

Model-side switches for the grid, each off by default (09_BRIDGE_EXPERIMENTS.md):
  MINIMAL_NO_LABEL_SHARED=1  (E7) The shared variables (married, n_dep,
         wages, interest, dividends, capital_gains) are predicted without
         the source: each prediction is made in a second pass of the same
         network in which the source slot and the source-owned slots
         (other_agi, net_worth, any _small parts) are left out, since their
         presence would reveal the source. Owned slots are predicted as
         before, from a normal pass that sees everything. See predict().
  MINIMAL_SCF_ZERO_UNKNOWN=1 (E10) On SCF rows an exact zero for interest,
         dividends or capital_gains is treated as unknown: the network sees
         the mask token there and the slot is never scored.
  MINIMAL_REPORT_CURVE=1     (E9) On SCF rows interest, dividends and
         capital_gains are scored through a reporting curve g(b), the
         chance an SCF unit whose true amount is in band b reports it: a
         zero on p(0) + sum_b p(b)(1 - g(b)), a reported band b on
         p(b) g(b). g is estimated from the table (it needs shared bands on
         one dollar scale, table E4); see report_curve(). PUF rows as before.

What it writes (in the model folder above)
  model.pt          the trained weights and the settings to rebuild the network
  train_log.csv     one row per epoch: train loss, validation loss, learning rate
  train_meta.json   parameter count, epochs run, best epoch, record counts

The evaluation is in 02_evaluate.py; generation is in 03_generate.py. Both
load this file for the vocabulary, the network and the data encoding.
Run through Slurm: sbatch research/rebuild/minimal/02_train.sbatch
"""
import json
import math
import os
import time

import numpy as np
import pandas as pd
import pyarrow.parquet as pq
import torch
import torch.nn as nn

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
EXPERIMENT = os.environ.get('MINIMAL_EXPERIMENT', '')        # older alias: '', 'A' or 'B'
assert EXPERIMENT in ('', 'A', 'B'), EXPERIMENT
TABLE = os.environ.get('MINIMAL_TABLE', '')                   # '' = the v2 table
assert not (TABLE and EXPERIMENT and TABLE != EXPERIMENT), (TABLE, EXPERIMENT)
TABLE = TABLE or EXPERIMENT
RUN = os.environ.get('MINIMAL_RUN', '')
_SUFFIX = f'_{TABLE}' if TABLE else ''
DATA = f'{ROOT}/data/stacked{_SUFFIX}.parquet'
BAND_EDGES = f'{ROOT}/data/band_edges{_SUFFIX}.json'
_FOLDER = RUN or (f'exp{EXPERIMENT}' if EXPERIMENT else '')
MODEL_DIR = os.environ.get('MINIMAL_MODEL_DIR', f'{ROOT}/model/{_FOLDER}'.rstrip('/'))
GEN_DIR = os.environ.get('MINIMAL_GEN_DIR', f'{ROOT}/gen/{_FOLDER}'.rstrip('/'))
SEED = 20261016
ALL_RECORDS = os.environ.get('MINIMAL_ALL_RECORDS') == '1'
HOLDBACK_SEED, HOLDBACK_SHARE = 20261015, 0.02


def flags():
    """Every MINIMAL_* environment setting of this run, for the meta files."""
    return {k: v for k, v in sorted(os.environ.items()) if k.startswith('MINIMAL_')}

# ---- The slots and the vocabulary ------------------------------------------
SOURCES = ['puf', 'scf']
# The tax years are read from the table (2026-10-15, set B of
# 10_TRANSLATION_EXPERIMENTS.md): a one-year table gives [2015] as before;
# the several-years tables (2015, 2018, 2021) get one year token each.
YEARS = sorted({int(y) for y in pq.read_table(DATA, columns=['year']).column('year').to_pylist()})
CONTEXT = ['source', 'year']
# Tables with a split at a dollar line (table A and its kind): the part of
# each investment income below the line, a PUF-only slot. Included when and
# only when the table has the column.
_COLUMNS = set(pq.read_schema(DATA).names)
SMALL_PARTS = [v for v in ['interest_small', 'dividends_small', 'capital_gains_small']
               if v + '_band' in _COLUMNS]
PREDICTED = ['married', 'n_dep', 'wages', 'interest', 'dividends',
             'capital_gains'] + SMALL_PARTS + ['other_agi', 'net_worth']
AMOUNTS = PREDICTED[2:]
SLOTS = CONTEXT + PREDICTED  # the order of slots inside the network
OWNED_BY = {'net_worth': 'scf', 'other_agi': 'puf',          # slots one source has
            **{v: 'puf' for v in SMALL_PARTS}}
# Coarsened zeros (research/rebuild/minimal/08_STATE_2026-10-14.md, "The fix
# chosen"): an SCF zero for these variables means "zero or small". Step 1
# writes, per row, <var>_coarse_hi: -1 when the band is exact, otherwise the
# highest band that still counts as small. Such a row's target is the set
# {band 0} plus the positive bands up to coarse_hi. For capital gains, step 1
# also writes capital_gains_coarse_neg: the lowest negative band that still
# counts as a small loss (the set then also holds the negative bands from it
# up to the last negative band, the smallest losses), or -1 for none.
# Off by default: with it on (model v3) the SCF label lost its exact zeros.
COARSEN = os.environ.get('MINIMAL_COARSEN') == '1'
COARSENED = ['interest', 'dividends', 'capital_gains'] if COARSEN else []
COARSE_NEG = ['capital_gains'] if COARSEN else []
NO_LABEL_SHARED = os.environ.get('MINIMAL_NO_LABEL_SHARED') == '1'
SCF_ZERO_UNKNOWN = os.environ.get('MINIMAL_SCF_ZERO_UNKNOWN') == '1'
REPORT_CURVE = os.environ.get('MINIMAL_REPORT_CURVE') == '1'
assert not (REPORT_CURVE and (COARSEN or SCF_ZERO_UNKNOWN)), 'one treatment of SCF zeros at a time'
INVESTMENT = ['interest', 'dividends', 'capital_gains']   # the items E9 and E10 treat


def read_band_layout(path=BAND_EDGES):
    """The band layout of each amount, read from step 1's band_edges.json so
    that a re-banding needs no code change. Step 1's layout: band 0 is exactly
    zero, bands 1 to first_positive - 1 are negatives (1 = most negative), the
    rest are positives in dollar order. positive_top[i] is the cumulative share
    of the positive weight at the top of positive band i, in the source that
    owns the variable: the PUF for the shared variables (since the v3 table
    the SCF is banded on the PUF's dollar cut points for those, so its own
    shares differ), the SCF for net worth. The number of bands must be the
    same in every source (checked).

    A layout with no zero band (2026-10-15, option B, 13_option_b.py): an
    edge record with layout 'full_distribution' cuts the whole weighted
    distribution, zeros and losses included, at full.probs. Then
    first_positive is 0, positive_top holds the cumulative share of all the
    weight at the top of each band, and first_band_above_zero gives, per
    source, the first band lying wholly above that source's zero mass, and
    zero_bands the first and last band holding zeros. Every source must use
    the same kind of layout for a variable (checked). Step 1's layout is read
    exactly as before."""
    with open(path) as f:
        edges = json.load(f)
    layout = {}
    for a in AMOUNTS:
        recs = {s: edges[f'{s}/{y}/{a}'] for s in SOURCES for y in YEARS
                if 'positive' in edges.get(f'{s}/{y}/{a}', {}) or 'full' in edges.get(f'{s}/{y}/{a}', {})}
        n_bands = {r['n_bands'] for r in recs.values()}
        assert len(n_bands) == 1, f'{a}: band counts differ by source'
        owner = recs[OWNED_BY.get(a, 'puf')]
        if 'full' in owner:   # no zero band: see the docstring
            assert all(r.get('layout') == 'full_distribution' for r in recs.values()), f'{a}: mixed layouts'
            assert len(YEARS) == 1, 'full-distribution layout: one tax year only'
            n, top = n_bands.pop(), list(owner['full']['probs']) + [1.0]
            assert len(top) == n, a
            layout[a] = {'n_bands': n, 'first_positive': 0, 'positive_top': top,
                         'first_band_above_zero': {s: r['zero_mass']['first_band_above'] for s, r in recs.items()},
                         'zero_bands': {s: [r['zero_mass']['first_band'], r['zero_mass']['last_band']]
                                        for s, r in recs.items()}}
            continue
        n, top = n_bands.pop(), list(owner['positive']['probs']) + [1.0]
        layout[a] = {'n_bands': n, 'first_positive': n - len(top), 'positive_top': top}
    return layout


BAND_LAYOUT = read_band_layout()
N_CHOICES = {'married': 2, 'n_dep': 4, **{a: BAND_LAYOUT[a]['n_bands'] for a in AMOUNTS}}
MAX_CHOICES = max(N_CHOICES.values())


def positive_bands(var, low, high):
    """The positive bands of var lying between two cumulative shares of the
    positive weight, e.g. (0.99, 1.0) for the top 1% of positives."""
    lay = BAND_LAYOUT[var]
    lower = np.array([0.0] + lay['positive_top'][:-1])
    inside = (lower >= low - 1e-9) & (lower < high - 1e-9)
    return lay['first_positive'] + np.flatnonzero(inside)


# One token table for all slots. Index 0 is the mask token. The six amounts
# share one block of band tokens, lined up by meaning rather than by band
# number: zero shares one token, the k-th most negative band shares one, and
# the k-th positive band (the same share of the positive weight in every
# variable) shares one. So positive band 40 of wages and of dividends start
# from the same vector, and the slot embedding tells the network which
# variable it is looking at. Each predicted variable has its own output head.
MASK = 0
MAX_NEGATIVE = max(BAND_LAYOUT[a]['first_positive'] - 1 for a in AMOUNTS)
MAX_POSITIVE = max(BAND_LAYOUT[a]['n_bands'] - BAND_LAYOUT[a]['first_positive'] for a in AMOUNTS)
BLOCK_SIZES = {'source': len(SOURCES), 'year': len(YEARS), 'married': 2,
               'n_dep': 4, 'band': 1 + MAX_NEGATIVE + MAX_POSITIVE}
BLOCK_START, _next = {}, 1
for _name, _size in BLOCK_SIZES.items():
    BLOCK_START[_name] = _next
    _next += _size
VOCAB = _next


def token_table():
    """TOKEN_OF[slot, value] = token index of that value in that slot."""
    table = torch.zeros(len(SLOTS), MAX_CHOICES, dtype=torch.long)
    for j, slot in enumerate(SLOTS):
        if slot not in AMOUNTS:
            table[j, :BLOCK_SIZES[slot]] = BLOCK_START[slot] + torch.arange(BLOCK_SIZES[slot])
            continue
        first_pos, n = BAND_LAYOUT[slot]['first_positive'], BAND_LAYOUT[slot]['n_bands']
        bands = torch.arange(n)
        shared = torch.where(bands < first_pos, bands, 1 + MAX_NEGATIVE + bands - first_pos)
        table[j, :n] = BLOCK_START['band'] + shared
    return table


TOKEN_OF = token_table()


# ---- The data ----------------------------------------------------------------
def read_table(path=DATA):
    """Read the stacked table and check every value the model will see.

    Stops on anything unexpected rather than dropping or filling it. The
    allowed gaps are band = -1 (slot absent) for net_worth and other_agi:
    always on the source that does not own the variable, and on SCF rows whose
    wealth was not asked, if any remain.
    """
    table = pd.read_parquet(path)
    if 'other_agi_band' not in table and os.environ.get('MINIMAL_SMOKE') == '1':
        # Smoke test only: the table built before other_agi existed. The slot
        # is made absent everywhere so that the code path can be exercised.
        print('SMOKE TEST: no other_agi_band column; treating it as absent everywhere')
        table['other_agi_band'] = -1
    used = ['source', 'year', 'split', 'weight', 'married', 'n_dep'] + \
           [a + '_band' for a in AMOUNTS]
    n_missing = table[used].isna().sum()
    if n_missing.any():
        raise ValueError(f'missing values in the stacked table:\n{n_missing[n_missing > 0]}')
    assert table['source'].isin(SOURCES).all()
    assert table['year'].isin(YEARS).all()
    assert table['married'].isin([0, 1]).all() and table['n_dep'].isin(range(4)).all()
    assert (table['weight'] > 0).all()
    for a in AMOUNTS:
        lowest = -1 if a in OWNED_BY else 0
        assert table[a + '_band'].between(lowest, N_CHOICES[a] - 1).all(), a
    for a, owner in OWNED_BY.items():
        assert (table.loc[table.source != owner, a + '_band'] == -1).all(), a
    for a in COARSENED:
        hi, band = table[a + '_coarse_hi'], table[a + '_band']
        assert not hi.isna().any(), f'{a}_coarse_hi has missing values'
        coarse = hi >= 0
        assert (hi[~coarse] == -1).all(), a
        assert (band[coarse] == 0).all(), f'{a}: a coarsened row must report zero'
        assert hi[coarse].between(BAND_LAYOUT[a]['first_positive'], N_CHOICES[a] - 1).all(), a
    for a in COARSE_NEG:
        neg, coarse = table[a + '_coarse_neg'], table[a + '_coarse_hi'] >= 0
        assert not neg.isna().any() and (neg[~coarse] == -1).all(), a
        used = neg >= 0
        assert neg[used].between(1, BAND_LAYOUT[a]['first_positive'] - 1).all(), a
    if ALL_RECORDS:
        table['split'] = np.where(held_back(table), 'test', 'train')
    return table


def held_back(table):
    """MINIMAL_ALL_RECORDS: True for the rows of the 2% held-back slice. One
    uniform draw per sorted unique household id within each source (PUF:
    household_id = record_id), seed HOLDBACK_SEED (+1 for the SCF), so every
    implicate and unit of an SCF household falls on the same side."""
    out = np.zeros(len(table), dtype=bool)
    for k, s in enumerate(SOURCES):
        sel = (table['source'] == s).to_numpy()
        hh = table.loc[sel, 'household_id']
        assert not hh.isna().any(), f'{s}: missing household_id'
        ids = np.sort(hh.unique())
        u = pd.Series(np.random.default_rng(HOLDBACK_SEED + k).random(len(ids)), index=ids)
        out[sel] = hh.map(u).to_numpy() < HOLDBACK_SHARE
    return out


def training_weights(table):
    """The plain rule: weights rescaled to sum to the record count within each
    source and split."""
    w = table['weight'].astype(float)
    total = w.groupby([table['source'], table['split']]).transform('sum')
    count = w.groupby([table['source'], table['split']]).transform('size')
    return (w * count / total).to_numpy()


def encode(table, device='cpu'):
    """Turn rows into tensors. values[:, j] is the within-variable index of
    slot j (source 0/1, year index, married, n_dep, band), -1 where absent."""
    cols = [table['source'].map(SOURCES.index), table['year'].map(YEARS.index),
            table['married'], table['n_dep']] + [table[a + '_band'] for a in AMOUNTS]
    values = np.column_stack([np.asarray(c, dtype=np.int64) for c in cols])
    # coarse[:, j, 0]: lowest negative band in the set (-1: none);
    # coarse[:, j, 1]: highest positive band in the set (-1: the band is exact).
    coarse = np.full((len(table), len(PREDICTED), 2), -1, dtype=np.int64)
    for a in COARSENED:
        coarse[:, PREDICTED.index(a), 1] = table[a + '_coarse_hi'].to_numpy()
    for a in COARSE_NEG:
        coarse[:, PREDICTED.index(a), 0] = table[a + '_coarse_neg'].to_numpy()
    unknown = None
    if SCF_ZERO_UNKNOWN:      # SCF zeros of the investment items: unknown, not zero
        unknown = np.zeros((len(table), len(PREDICTED)), dtype=bool)
        scf = (table['source'] == 'scf').to_numpy()
        for a in INVESTMENT:
            unknown[:, PREDICTED.index(a)] = scf & (table[a + '_band'].to_numpy() == 0)
        unknown = torch.tensor(unknown, device=device)
    return {'values': torch.tensor(values, device=device), 'unknown': unknown,
            'coarse': torch.tensor(coarse, device=device) if COARSEN else None,
            'weight': torch.tensor(training_weights(table), dtype=torch.float32,
                                   device=device)}


def token_ids(values, hidden):
    """Token index per slot. Hidden predicted slots get the mask token. Absent
    slots also get it, but the attention mask makes the network ignore them.
    A coarsened SCF zero that is visible as input keeps the token of band 0,
    as reported. (A special "zero or small" input token is a possible later
    refinement.)"""
    slot = torch.arange(len(SLOTS), device=values.device)
    ids = TOKEN_OF.to(values.device)[slot, values.clamp(min=0)]
    full_hidden = torch.cat([torch.zeros_like(hidden[:, :len(CONTEXT)]), hidden], 1)
    return torch.where(full_hidden | (values < 0), MASK, ids)


# ---- The network ---------------------------------------------------------------
class SlotTransformer(nn.Module):
    """Slot vector = token embedding + slot embedding; four encoder layers;
    one output head per predicted variable."""

    def __init__(self, width=128, layers=4, heads=4, ffn=256, dropout=0.1):
        super().__init__()
        self.settings = dict(width=width, layers=layers, heads=heads, ffn=ffn,
                             dropout=dropout)
        self.token_embedding = nn.Embedding(VOCAB, width)
        self.slot_embedding = nn.Parameter(torch.randn(len(SLOTS), width) * 0.02)
        layer = nn.TransformerEncoderLayer(width, heads, ffn, dropout,
                                           batch_first=True, norm_first=True)
        self.encoder = nn.TransformerEncoder(layer, layers, enable_nested_tensor=False)
        self.final_norm = nn.LayerNorm(width)
        self.heads = nn.ModuleList(nn.Linear(width, N_CHOICES[v]) for v in PREDICTED)

    def forward(self, values, hidden):
        """Returns log-probabilities, shape [records, 8 predicted slots, MAX_CHOICES].
        Choices a variable does not have (married beyond 2, ...) get -inf."""
        x = self.token_embedding(token_ids(values, hidden)) + self.slot_embedding
        h = self.final_norm(self.encoder(x, src_key_padding_mask=values < 0))
        out = h.new_full((len(values), len(PREDICTED), MAX_CHOICES), -math.inf)
        for j, head in enumerate(self.heads):
            out[:, j, :head.out_features] = head(h[:, len(CONTEXT) + j])
        return out.log_softmax(-1)


OWNED = torch.tensor([v in OWNED_BY for v in PREDICTED])
OWNED_IDX = [j for j, v in enumerate(PREDICTED) if v in OWNED_BY]


def predict(model, values, hidden):
    """The model's log-probabilities for every predicted slot. Without
    MINIMAL_NO_LABEL_SHARED this is just model(values, hidden). With it, the
    shared slots come from a second pass in which the source slot and the
    source-owned slots are absent (the network never sees them), so the
    prediction for a shared variable is one rule for both sources; the owned
    slots come from the normal pass."""
    labelled = model(values, hidden)
    if not NO_LABEL_SHARED:
        return labelled
    neutral_values, neutral_hidden = values.clone(), hidden.clone()
    neutral_values[:, 0] = -1                       # no source
    neutral_values[:, [len(CONTEXT) + j for j in OWNED_IDX]] = -1
    neutral_hidden[:, OWNED_IDX] = False
    neutral = model(neutral_values, neutral_hidden)
    return torch.where(OWNED.to(values.device)[None, :, None], labelled, neutral)


def count_parameters(model):
    return sum(p.numel() for p in model.parameters())


def load_model(device, model_dir=MODEL_DIR):
    saved = torch.load(os.path.join(model_dir, 'model.pt'), map_location=device)
    assert saved['band_layout'] == BAND_LAYOUT, 'band_edges.json changed since training'
    model = SlotTransformer(**saved['settings']).to(device)
    model.load_state_dict(saved['state'])
    return model.eval()


# ---- Training ----------------------------------------------------------------------
def draw_hidden(values, generator, unknown=None):
    """Masked any-subset: per record a hide share u uniform on (0, 1], then
    ceil(u * n) of its n predicted slots hidden, chosen at random. So 1 to n
    slots are hidden, each count equally likely, never zero. Unknown slots
    (MINIMAL_SCF_ZERO_UNKNOWN) are not among the n: they are masked always
    and never scored (see masked_loss)."""
    present = values[:, len(CONTEXT):] >= 0
    if unknown is not None:
        present = present & ~unknown
    n_present = present.sum(1)
    share = 1 - torch.rand(len(values), generator=generator, device=values.device)
    n_hide = torch.ceil(share * n_present).clamp(min=1)
    scores = torch.rand(present.shape, generator=generator, device=values.device)
    scores[~present] = 2.0                       # absent slots sort last
    rank = scores.argsort(1).argsort(1)
    return (rank < n_hide[:, None]) & present


FIRST_POSITIVE = torch.tensor([BAND_LAYOUT[v]['first_positive'] if v in AMOUNTS else 0
                               for v in PREDICTED])


# The reporting curve (MINIMAL_REPORT_CURVE): set by set_report_curve().
REPORT_LOG_G, REPORT_LOG_1MG = {}, {}


def report_curve(table):
    """g(b) for each investment item: the chance that an SCF unit with a
    true amount in band b reports it. Estimated on the training split from
    the table itself, which must band both sources on the same dollar cut
    points (table E4): g(b) = SCF weighted share of records in band b over
    the PUF weighted share in band b. The bands are put in order of the size
    of the amount (|amount|, so a capital loss counts by its size: the
    median |amount| of the band's records, both sources), and g is made
    non-decreasing in that order by pooling adjacent bands (pool adjacent
    violators: a pooled g is the pooled SCF share over the pooled PUF
    share), then capped at 1. g(0) = 0 (band 0 is the zero itself).
    Bands empty in both sources get g = 1 (they never enter)."""
    train = table[table.split == 'train']
    curves = {}
    for a in INVESTMENT:
        assert a in train, f'{a}: the report curve needs the dollar column'
        n = N_CHOICES[a]
        share = {}
        for s in SOURCES:
            g = train[train.source == s]
            share[s] = np.bincount(g[a + '_band'], weights=g['weight'], minlength=n) / g['weight'].sum()
        size = train[a].abs().groupby(train[a + '_band']).median()
        bands = sorted((b for b in range(1, n) if share['puf'][b] > 0 or share['scf'][b] > 0),
                       key=lambda b: (size[b], b))
        blocks = []                           # [scf share, puf share, bands]
        for b in bands:
            blocks.append([share['scf'][b], share['puf'][b], [b]])
            # pool while the block before has the larger g (cross-multiplied,
            # so a PUF share of zero counts as an infinite g)
            while len(blocks) > 1 and blocks[-2][0] * blocks[-1][1] > blocks[-1][0] * blocks[-2][1]:
                last = blocks.pop()
                blocks[-1] = [blocks[-1][0] + last[0], blocks[-1][1] + last[1], blocks[-1][2] + last[2]]
        g = np.ones(n)
        g[0] = 0.0
        for scf, puf, members in blocks:
            g[members] = 1.0 if puf == 0 else min(1.0, scf / puf)
        holders = {s: float(share[s][1:].sum()) for s in SOURCES}
        curves[a] = {'g': g.tolist(), 'order_by_size': bands,
                     'pools': [members for _, _, members in blocks],
                     'puf_share': share['puf'].tolist(), 'scf_share': share['scf'].tolist(),
                     'holder_share': holders,
                     'scf_holder_share_implied': float((share['puf'] * g).sum())}
    return curves


def set_report_curve(table):
    """Compute the reporting curve and keep it for slot_nll. Returns it."""
    curves = report_curve(table)
    for a, c in curves.items():
        g = torch.tensor(c['g'], dtype=torch.float32)
        REPORT_LOG_G[a] = torch.log(g.clamp(min=1e-4))
        REPORT_LOG_1MG[a] = torch.log1p(-g)      # -inf where g = 1
    return curves


def slot_nll(log_probs, values, coarse=None, source=None):
    """Negative log-likelihood of the true token for each predicted slot.
    Where a slot is coarsened (an SCF zero that means "zero or small", see
    encode), the target is a set and the loss is -log of the model's total
    probability over it: band 0, the positive bands up to the line, and for
    capital gains the smallest-loss negative bands. The split between zero
    and small is then left to what the model learns elsewhere, mainly from
    the PUF."""
    target = values[:, len(CONTEXT):].clamp(min=0)
    nll = -log_probs.gather(2, target[..., None]).squeeze(2)
    if REPORT_CURVE:
        # SCF rows: the reporting curve (see report_curve). `source` is the
        # true source of each record when values carries another label
        # (the label swap in 02_evaluate.py); by default values' own.
        assert REPORT_LOG_G, 'set_report_curve() was not called'
        scf = (values[:, 0] if source is None else source) == SOURCES.index('scf')
        cols = list(nll.unbind(1))
        for a in INVESTMENT:
            j, dev = PREDICTED.index(a), log_probs.device
            lp, t = log_probs[:, j, :N_CHOICES[a]], target[:, j]
            zero_ll = torch.logsumexp(lp + REPORT_LOG_1MG[a].to(dev), 1)
            reported_ll = lp.gather(1, t[:, None]).squeeze(1) + REPORT_LOG_G[a].to(dev)[t]
            cols[j] = torch.where(scf, -torch.where(t == 0, zero_ll, reported_ll), cols[j])
        nll = torch.stack(cols, 1)
    if coarse is None or not (coarse[..., 1] >= 0).any():
        return nll
    lo, hi = coarse[..., 0:1], coarse[..., 1:2]
    band = torch.arange(log_probs.shape[2], device=log_probs.device)
    first = FIRST_POSITIVE.to(log_probs.device)[None, :, None]
    in_set = ((band == 0) | ((band >= first) & (band <= hi))
              | ((lo >= 1) & (band >= lo) & (band < first)))
    set_nll = -torch.logsumexp(log_probs.masked_fill(~in_set, -math.inf), dim=2)
    return torch.where(coarse[..., 1] >= 0, set_nll, nll)


def masked_loss(model, values, weight, hidden, coarse=None, unknown=None):
    """Weighted mean cross-entropy (nats) over hidden slots only. Unknown
    slots are masked in the input and never scored."""
    if unknown is not None:
        hidden = hidden & ~unknown
    nll = slot_nll(predict(model, values, hidden if unknown is None else hidden | unknown),
                   values, coarse)
    w = weight[:, None] * hidden
    return (nll * w).sum() / w.sum()


def train(train_data, val_data, max_epochs, batch_size=4096, peak_lr=1e-3,
          warmup_epochs=3, patience=12, keep_last=False):
    """keep_last (MINIMAL_ALL_RECORDS): no early stopping and no choice of
    epoch by the validation loss; the last epoch's weights are kept."""
    device = train_data['values'].device
    gen = torch.Generator(device=device).manual_seed(SEED)
    torch.manual_seed(SEED)
    model = SlotTransformer().to(device)
    optimizer = torch.optim.AdamW(model.parameters(), lr=peak_lr, weight_decay=0.01)
    n = len(train_data['values'])
    steps_per_epoch = math.ceil(n / batch_size)
    total_steps, warmup = max_epochs * steps_per_epoch, warmup_epochs * steps_per_epoch

    def lr_factor(step):   # linear warm-up, then cosine decay to 5% of the peak
        # at max_epochs, so the rate has decayed by the time training ends
        if step < warmup:
            return (step + 1) / warmup
        progress = (step - warmup) / max(1, total_steps - warmup)
        return 0.05 + 0.95 * 0.5 * (1 + math.cos(math.pi * progress))
    scheduler = torch.optim.lr_scheduler.LambdaLR(optimizer, lr_factor)

    # The validation loss uses four fixed hide draws per record, so that its
    # changes from epoch to epoch come from the model, not from the draws.
    val_hidden = [draw_hidden(val_data['values'], gen, val_data['unknown']) for _ in range(4)]
    log, best, best_state, since_best = [], math.inf, None, 0
    for epoch in range(1, max_epochs + 1):
        model.train()
        epoch_began = time.time()
        order = torch.randperm(n, generator=gen, device=device)
        sum_loss = 0.0
        for start in range(0, n, batch_size):
            idx = order[start:start + batch_size]
            values, weight = train_data['values'][idx], train_data['weight'][idx]
            coarse = None if train_data['coarse'] is None else train_data['coarse'][idx]
            unknown = None if train_data['unknown'] is None else train_data['unknown'][idx]
            loss = masked_loss(model, values, weight, draw_hidden(values, gen, unknown),
                               coarse, unknown)
            optimizer.zero_grad()
            loss.backward()
            optimizer.step()
            scheduler.step()
            sum_loss += loss.item() * len(idx)
        model.eval()
        with torch.no_grad():
            val_loss = np.mean([masked_loss(model, val_data['values'], val_data['weight'],
                                            h, val_data['coarse'], val_data['unknown']).item()
                                for h in val_hidden])
        log.append(dict(epoch=epoch, train_loss=sum_loss / n, val_loss=val_loss,
                        lr=scheduler.get_last_lr()[0], seconds=time.time() - epoch_began))
        print(f'epoch {epoch:3d}  train {sum_loss / n:.4f}  val {val_loss:.4f}  '
              f'{time.time() - epoch_began:.1f}s', flush=True)
        if keep_last:
            continue
        if val_loss < best - 1e-4:
            best, since_best = val_loss, 0
            best_state = {k: v.detach().clone() for k, v in model.state_dict().items()}
        else:
            since_best += 1
            if since_best >= patience:   # early stopping
                break
    if not keep_last:
        model.load_state_dict(best_state)
    return model, pd.DataFrame(log)


@torch.no_grad()
def loss_per_variable(model, table, device, draws=4, seed=SEED + 1):
    """Weighted mean cross-entropy (nats) of each predicted slot, per source,
    over records of `table` under masked any-subset hiding (the training
    loss, split by variable), averaged over a few fixed hide draws."""
    gen = torch.Generator(device=device).manual_seed(seed)
    out = {}
    for s in SOURCES:
        part = table[table.source == s]
        if part.empty:
            continue
        d = encode(part, device)
        num = torch.zeros(len(PREDICTED), device=device)
        den = torch.zeros(len(PREDICTED), device=device)
        for _ in range(draws):
            for i in range(0, len(d['values']), 50_000):      # chunks, for memory
                c = {k: None if v is None else v[i:i + 50_000] for k, v in d.items()}
                hidden = draw_hidden(c['values'], gen, c['unknown'])
                if c['unknown'] is not None:
                    hidden = hidden & ~c['unknown']
                nll = slot_nll(predict(model, c['values'], hidden if c['unknown'] is None
                                       else hidden | c['unknown']), c['values'], c['coarse'])
                w = c['weight'][:, None] * hidden
                num += (nll * w).sum(0)
                den += w.sum(0)
        out[s] = {v: float(num[j] / den[j]) for j, v in enumerate(PREDICTED) if den[j] > 0}
    return out


def main():
    # 80 epochs: the first run (cosine set for 200) stopped early at epoch 89,
    # best at 77, with the rate still at two-thirds of its peak. Setting the
    # horizon at 80 makes the decay finish where training was ending anyway.
    max_epochs = int(os.environ.get('MINIMAL_MAX_EPOCHS', '80'))
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    os.makedirs(MODEL_DIR, exist_ok=True)
    table = read_table()
    if REPORT_CURVE:
        with open(os.path.join(MODEL_DIR, 'report_curve.json'), 'w') as f:
            json.dump(set_report_curve(table), f, indent=1)
    absent_nw = int(((table.source == 'scf') & (table.net_worth_band == -1)).sum())
    absent_oa = int(((table.source == 'puf') & (table.other_agi_band == -1)).sum())
    print(f'rows: {len(table)}; SCF rows with no net worth slot: {absent_nw}; '
          f'PUF rows with no other_agi slot: {absent_oa}')
    # MINIMAL_ALL_RECORDS: the held-back slice ('test' after read_table) is
    # the validation data; it is watched, never used to choose.
    val_split = 'test' if ALL_RECORDS else 'validation'
    data = {s: encode(table[table.split == s], device) for s in ['train', val_split]}
    began = time.time()
    model, log = train(data['train'], data[val_split], max_epochs, keep_last=ALL_RECORDS)
    log.to_csv(os.path.join(MODEL_DIR, 'train_log.csv'), index=False)
    torch.save({'state': model.state_dict(), 'settings': model.settings,
                'band_layout': BAND_LAYOUT},
               os.path.join(MODEL_DIR, 'model.pt'))
    best = log.loc[log.val_loss.idxmin()]
    meta = dict(table=TABLE or 'v2', flags=flags(), data=DATA, band_edges=BAND_EDGES,
                predicted=PREDICTED,
                parameters=count_parameters(model), settings=model.settings,
                vocabulary=VOCAB, band_layout={a: {k: v for k, v in BAND_LAYOUT[a].items()
                                                   if k != 'positive_top'} for a in AMOUNTS},
                epochs_run=len(log), best_epoch=int(best.epoch),
                best_val_loss=float(best.val_loss), minutes=(time.time() - began) / 60,
                seed=SEED, max_epochs=max_epochs, device=device,
                rows=table.groupby(['source', 'split']).size().to_dict().__repr__(),
                scf_rows_without_net_worth=absent_nw,
                puf_rows_without_other_agi=absent_oa)
    if ALL_RECORDS:
        meta.update(all_records=True, holdback_seed=HOLDBACK_SEED, holdback_share=HOLDBACK_SHARE,
                    kept_epoch=int(log.epoch.iloc[-1]),
                    heldback_loss_last_epoch=float(log.val_loss.iloc[-1]),
                    heldback_loss_per_variable=loss_per_variable(model, table[table.split == 'test'], device),
                    train_loss_per_variable=loss_per_variable(model, table[table.split == 'train'], device))
    with open(os.path.join(MODEL_DIR, 'train_meta.json'), 'w') as f:
        json.dump(meta, f, indent=2)
    print(json.dumps(meta, indent=2))


if __name__ == '__main__':
    main()
