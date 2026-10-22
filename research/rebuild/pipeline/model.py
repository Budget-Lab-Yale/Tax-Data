"""The network and how a record is encoded for it.

A record is a list of slots: two context slots (source, year), always shown
and never predicted, then the predicted slots in a fixed order (PREDICTED).
Each amount is a band index (band 0 = exactly zero, then the negative bands
from most negative, then the positive bands in dollar order); married, number
of dependents, the primary filer's age range (ages.py), the filing flag and
the yes/no business items are categories.
A slot one source never has (the balance sheet on the PUF, the PUF-only items
on the SCF) is absent: band -1. The
network never sees an absent slot and is never asked to predict it.

Training hides a random subset of each record's slots and scores the
network's prediction of the hidden ones ("masked any-subset"), so one network
answers every question of the form "this variable given those".
"""
import json
import math

import numpy as np
import pandas as pd
import pyarrow.parquet as pq
import torch
import torch.nn as nn

import common

SOURCES = ['puf', 'scf']
CONTEXT = ['source', 'year']
YEARS = sorted({int(y) for y in pq.read_table(common.TABLE, columns=['year']).column('year').to_pylist()})
_EDGES = json.load(open(common.EDGES))
FILER = ['filer']
AGE_PROBS = [f'age_p{k}' for k in range(9)]           # step 0's probabilities on PUF rows with no range
KG_PIECES = ['kg_st', 'kg_lt', 'kg_dist', 'kg_other']
# The table's other items, in table order, each owned by the PUF or on both sources.
ITEMS = _EDGES['meta']['items']
CATEGORIES = _EDGES['meta']['categories']          # yes/no items: k1_owner (shared), owns_business (SCF)
AGE_RANGES = 9                                     # six ranges, three for dependent filers (ages.py)
CATEGORICAL = ['married', 'n_dep', 'age_range'] + FILER + list(CATEGORIES)
# The SCF-only slots: the balance sheet (the home mortgage as its share of the home's value), the
# unrealized gains, and owns_business. Net worth is computed from the balance sheet in dollars.
SCF_SLOTS = list(_EDGES['meta']['scf_amounts']) + [c for c, o in CATEGORIES.items() if o == 'scf']
NET_WORTH_PARTS = dict(_EDGES['meta']['net_worth_parts'])
RATIOS = {k: tuple(v) for k, v in _EDGES['meta']['ratios'].items()}   # slot: (dollar item, its base)
# Two slots per translated item: a second, SCF-only slot holds what the
# household reported; the original slot is the PUF concept, filled in on the SCF side during training (train.py).
REPORTED = {f'{v}_reported': v for v in common.SETTINGS['translation']['mapped']}
PREDICTED = (['married', 'n_dep', 'age_range', 'wages', 'interest', 'dividends', 'capital_gains'] + KG_PIECES
             + list(ITEMS) + ['other_agi'] + [c for c, o in CATEGORIES.items() if o == 'shared']
             + SCF_SLOTS + FILER + list(REPORTED))
AMOUNTS = [v for v in PREDICTED if v not in CATEGORICAL]
SLOTS = CONTEXT + PREDICTED
OWNED_BY = {**{v: 'scf' for v in SCF_SLOTS}, **{v: 'scf' for v in REPORTED}, 'other_agi': 'puf', **{v: 'puf' for v in KG_PIECES + FILER},
            **{v: 'puf' for v, o in ITEMS.items() if o == 'puf'}}
# The computed totals: never drawn, computed from their parts after the parts are drawn. The same for
# the computed flags on the PUF side (k1_owner: any of its parts nonzero).
COMPUTED = {'capital_gains': {p: 1 for p in KG_PIECES}, **_EDGES['meta']['computed_totals']}
COMPUTED_FLAGS = _EDGES['meta']['computed_flags']


def column(v):
    """The table column of a predicted slot: a band column for amounts."""
    return v if v in CATEGORICAL else v + '_band'


def band_layout():
    """Per amount: the number of bands, the first positive band, and the
    cumulative share of the owner's positive weight at the top of each
    positive band."""
    layout = {}
    for a in AMOUNTS:
        if a in REPORTED:
            continue
        recs = [_EDGES[f'{s}/{y}/{a}'] for s in SOURCES for y in YEARS if 'positive' in _EDGES.get(f'{s}/{y}/{a}', {})]
        n = {r['n_bands'] for r in recs}
        assert len(n) == 1, f'{a}: band counts differ by source'
        n = n.pop()
        owner = OWNED_BY.get(a, 'puf')
        # the owner's latest year that has the variable (net worth: the last SCF wave)
        top = list(next(_EDGES[f'{owner}/{y}/{a}'] for y in reversed(YEARS)
                        if 'positive' in _EDGES.get(f'{owner}/{y}/{a}', {}))['positive']['probs']) + [1.0]
        layout[a] = {'n_bands': n, 'first_positive': n - len(top), 'positive_top': top}
    for r, v in REPORTED.items():
        layout[r] = layout[v]
    return layout


BAND_LAYOUT = band_layout()
N_CHOICES = {'married': 2, 'n_dep': 4, 'age_range': AGE_RANGES, 'filer': 2, **{c: 2 for c in CATEGORIES},
             **{a: BAND_LAYOUT[a]['n_bands'] for a in AMOUNTS}}
MAX_CHOICES = max(N_CHOICES.values())
ZERO_BAND = 0


def positive_bands(var, low, high):
    """The positive bands of var between two cumulative shares of the positive weight."""
    lay = BAND_LAYOUT[var]
    lower = np.array([0.0] + lay['positive_top'][:-1])
    inside = (lower >= low - 1e-9) & (lower < high - 1e-9)
    return lay['first_positive'] + np.flatnonzero(inside)


# One token table for all slots; index 0 is the mask token. The amounts share
# one block of band tokens lined up by meaning: zero shares a token, the k-th
# most negative band shares one, the k-th positive band shares one.
MASK = 0
MAX_NEGATIVE = max(BAND_LAYOUT[a]['first_positive'] - 1 for a in AMOUNTS)
MAX_POSITIVE = max(BAND_LAYOUT[a]['n_bands'] - BAND_LAYOUT[a]['first_positive'] for a in AMOUNTS)
BLOCK_SIZES = {'source': len(SOURCES), 'year': len(YEARS), 'married': 2, 'n_dep': 4, 'age_range': AGE_RANGES,
               'band': 1 + MAX_NEGATIVE + MAX_POSITIVE, 'filer': 2, **{c: 2 for c in CATEGORIES}}
BLOCK_START, _next = {}, 1
for _name, _size in BLOCK_SIZES.items():
    BLOCK_START[_name] = _next
    _next += _size
VOCAB = _next


def _token_table():
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


TOKEN_OF = _token_table()


# ---- data ---------------------------------------------------------------------
def read_table():
    """The training table, checked; split relabelled: the held-back 2% 'test', the rest 'train'."""
    table = pd.read_parquet(common.TABLE)
    for r, v in REPORTED.items():
        table[r + '_band'] = np.where(table['source'] == 'scf', table[v + '_band'], -1)
    used = ['source', 'year', 'weight', 'married', 'n_dep', 'household_id'] + [column(v) for v in PREDICTED] + AGE_PROBS
    n_missing = table[used].isna().sum()
    if n_missing.any():
        raise ValueError(f'missing values in the training table:\n{n_missing[n_missing > 0]}')
    assert table['source'].isin(SOURCES).all() and (table['weight'] > 0).all()
    assert table['married'].isin([0, 1]).all() and table['n_dep'].isin(range(4)).all()
    # age_range: -1 only on PUF records with no range, which carry step 0's probabilities instead
    no_range = table['age_range'] == -1
    assert table['age_range'].between(-1, AGE_RANGES - 1).all() and (table.loc[no_range, 'source'] == 'puf').all()
    assert np.allclose(table.loc[no_range, AGE_PROBS].sum(1), 1) and (table.loc[~no_range, AGE_PROBS] == 0).all().all()
    for a in AMOUNTS:
        assert table[a + '_band'].between(-1 if a in OWNED_BY else 0, N_CHOICES[a] - 1).all(), a
    assert table.loc[table.source == 'puf', 'filer'].isin([0, 1]).all()
    for c in CATEGORIES:
        assert table[c].between(-1 if c in OWNED_BY else 0, 1).all(), c
    for a, owner in OWNED_BY.items():
        assert (table.loc[table.source != owner, column(a)] == -1).all(), a
    table['split'] = np.where(held_back(table), 'test', 'train')
    return table


def age_draws(table, device):
    """The rows with no age range and their step 0 probabilities, to draw a range from (train.py)."""
    rows = np.flatnonzero(table['age_range'].to_numpy() == -1)
    return (torch.tensor(rows, device=device),
            torch.tensor(table[AGE_PROBS].to_numpy()[rows], dtype=torch.float32, device=device))


def held_back(table):
    """The 2% held-back slice: one uniform draw per household within each
    source, so all units and implicates of an SCF household fall together."""
    s = common.SETTINGS['training']
    out = np.zeros(len(table), dtype=bool)
    for k, src in enumerate(SOURCES):
        sel = (table['source'] == src).to_numpy()
        hh = table.loc[sel, 'household_id']
        ids = np.sort(hh.unique())
        u = pd.Series(np.random.default_rng(s['holdback_seed'] + k).random(len(ids)), index=ids)
        out[sel] = hh.map(u).to_numpy() < s['holdback_share']
    return out


def training_weights(table):
    """Survey weights rescaled to sum to the record count within each source and split."""
    w = table['weight'].astype(float)
    total = w.groupby([table['source'], table['split']]).transform('sum')
    count = w.groupby([table['source'], table['split']]).transform('size')
    return (w * count / total).to_numpy()


def encode(table, device='cpu'):
    """values[:, j]: the within-variable index of slot j (-1 where absent); and training weights."""
    cols = [table['source'].map(SOURCES.index), table['year'].map(YEARS.index)] + \
           [table[column(v)] for v in PREDICTED]
    values = np.column_stack([np.asarray(c, dtype=np.int64) for c in cols])
    return {'values': torch.tensor(values, device=device),
            'weight': torch.tensor(training_weights(table), dtype=torch.float32, device=device)}


def token_ids(values, hidden):
    """Token per slot; hidden and absent slots get the mask token (absent ones are also masked out of attention)."""
    slot = torch.arange(len(SLOTS), device=values.device)
    ids = TOKEN_OF.to(values.device)[slot, values.clamp(min=0)]
    full_hidden = torch.cat([torch.zeros_like(hidden[:, :len(CONTEXT)]), hidden], 1)
    return torch.where(full_hidden | (values < 0), MASK, ids)


# ---- network ------------------------------------------------------------------
class SlotTransformer(nn.Module):
    """Slot vector = token embedding + slot embedding; encoder layers; one output head per predicted variable."""

    def __init__(self, width, layers, heads, ffn, dropout):
        super().__init__()
        self.settings = dict(width=width, layers=layers, heads=heads, ffn=ffn, dropout=dropout)
        self.token_embedding = nn.Embedding(VOCAB, width)
        self.slot_embedding = nn.Parameter(torch.randn(len(SLOTS), width) * 0.02)
        layer = nn.TransformerEncoderLayer(width, heads, ffn, dropout, batch_first=True, norm_first=True)
        self.encoder = nn.TransformerEncoder(layer, layers, enable_nested_tensor=False)
        self.final_norm = nn.LayerNorm(width)
        self.heads = nn.ModuleList(nn.Linear(width, N_CHOICES[v]) for v in PREDICTED)

    def forward(self, values, hidden):
        """Log-probabilities, [records, predicted slots, MAX_CHOICES]; choices a variable lacks get -inf."""
        x = self.token_embedding(token_ids(values, hidden)) + self.slot_embedding
        h = self.final_norm(self.encoder(x, src_key_padding_mask=values < 0))
        out = h.new_full((len(values), len(PREDICTED), MAX_CHOICES), -math.inf)
        for j, head in enumerate(self.heads):
            out[:, j, :head.out_features] = head(h[:, len(CONTEXT) + j])
        return out.log_softmax(-1)


def new_model(device):
    return SlotTransformer(**common.SETTINGS['network']).to(device)


def save_model(model, path):
    torch.save({'state': model.state_dict(), 'settings': model.settings, 'band_layout': BAND_LAYOUT}, path)


def load_model(device, path):
    saved = torch.load(path, map_location=device)
    assert saved['band_layout'] == BAND_LAYOUT, 'the band edges changed since training'
    model = SlotTransformer(**saved['settings']).to(device)
    model.load_state_dict(saved['state'])
    return model.eval()


# ---- training pieces -----------------------------------------------------------
def draw_hidden(values, generator):
    """Per record a hide share u uniform on (0, 1], then ceil(u * n) of its n present slots hidden at random."""
    present = values[:, len(CONTEXT):] >= 0
    n_present = present.sum(1)
    share = 1 - torch.rand(len(values), generator=generator, device=values.device)
    n_hide = torch.ceil(share * n_present).clamp(min=1)
    scores = torch.rand(present.shape, generator=generator, device=values.device)
    scores[~present] = 2.0
    rank = scores.argsort(1).argsort(1)
    return (rank < n_hide[:, None]) & present


def slot_nll(log_probs, values):
    """Negative log-likelihood of the true value of each predicted slot."""
    target = values[:, len(CONTEXT):].clamp(min=0)
    return -log_probs.gather(2, target[..., None]).squeeze(2)


def masked_loss(model, values, weight, hidden):
    """Weighted mean cross-entropy (nats) over the hidden slots."""
    nll = slot_nll(model(values, hidden), values)
    w = weight[:, None] * hidden
    return (nll * w).sum() / w.sum()
