"""The network and how a record is encoded for it.

A record is a list of slots: two context slots (source, year), always shown
and never predicted, then the predicted slots in a fixed order (PREDICTED).
Each amount is a band index (band 0 = exactly zero, then the negative bands
from most negative, then the positive bands in dollar order); married, number
of dependents, the primary filer's age range (ages.py), the filing flag, full
filing status, the child credit and EITC children (TAX_STATUS), the yes/no business items, the exact ages (primary filer and spouse: SCF, CEX and
CPS; dependents: CPS), the CPS's sex and the AGI group label (qgroups.py: on the PUF, the record's band-AGI group;
LABELS) are categories.
A slot one source never has (the balance sheet and the exact ages on the PUF,
the PUF-only items on the SCF, everything but the shared items and the
consumption categories on the CEX, everything but the shared items and the
person items on the CPS) is absent: band -1. Which sources have each slot is in the band
edges' meta ("present_on"). The network never sees an absent slot and is never
asked to predict it.

Training hides a random subset of each record's slots and scores the
network's prediction of the hidden ones ("masked any-subset"), so one network
answers every question of the form "this variable given those".
"""
import math

import numpy as np
import pandas as pd
import pyarrow.parquet as pq
import torch
import torch.nn as nn

from shared import common
from pretrain import qgroups
from shared import translate as TR

SOURCES = ['puf', 'scf', 'cex', 'cps', 'sipp']   # appended in this order, so each source's held-back slice stays
CONTEXT = ['source', 'year']
YEARS = sorted({int(y) for y in pq.read_table(common.TABLE, columns=['year']).column('year').to_pylist()})
_EDGES = common.edges()
META = common.meta()
FILER = ['filer']
# Full filing status (mars: 1 single, 2 joint, 3 separate, 4 head of household; value 0 never used), child credit
# children and EITC children (0 to 3, 3 = 3 or more): on the PUF and the CPS, absent on the SCF and CEX.
TAX_STATUS = dict(META['tax_categories'])
AGE_RANGES = common.N_RANGES                       # six adult ranges, three for dependent filers (ages.py)
AGE_PROBS = [f'age_p{k}' for k in range(AGE_RANGES)]   # step 0's probabilities on PUF rows with no range
KG_PIECES = META['kg_pieces']
# The table's other items, in table order, each owned by the PUF or on both sources.
ITEMS = META['items']
CATEGORIES = META['categories']                    # yes/no items: k1_owner (shared), owns_business (SCF)
CPS_CATEGORIES = META['cps_categories']            # CPS-only categories (dependents' ages, sex): {slot: number of values}
# The primary filer's and the spouse's exact ages: on the SCF, the CEX and the CPS (absent on the PUF), drawn at
# generation right after the PUF items (generate.py). PERSON_CATEGORIES: these and the CPS-only categories.
EXACT_AGES = list(META['exact_ages'])
PERSON_CATEGORIES = META['person_categories']
# The AGI group label (qgroups.py; build_table.agi_group_label): on PUF rows the record's band-AGI group, one of 12, a
# category learned like any other and always shown in training (train.py); absent on the surveys. Generation presets it
# on every filer record (generate.py). {label: number of values}.
LABELS = {v: int(d['values']) for v, d in META['labels'].items()}
PUF_LABELS = [v for v in LABELS if META['labels'][v]['source'] == 'puf']   # agi_group
assert PUF_LABELS == list(LABELS), 'labels on the PUF only'
# The SIPP-only categories (whether each adult's tips are mostly from leisure and hospitality): {slot: values}
SIPP_CATEGORIES = META['sipp_categories']
# The SCF-only categories: the industry group of the household's largest business of each kind
# (Schedule C, partnership, S corporation; 0 none, 1-7 the SCF's public groups, 8 no industry code; build_scf.R).
# {slot: values}
SCF_CATEGORIES = META['scf_categories']
# The categories the PUF and the CPS share beyond the type (the earnings split code,
# earn_split: 0 none, 1-3 the primary's share of the couple's earnings in EARNSPLIT's ranges), {slot: values}; and the
# PUF slots shared with the CPS that are shares (the spouse's share of the couple's self-employment income). Both are
# drawn at generation after the PUF items and their totals (generate.py).
SHARED_CATEGORIES = META['shared_categories']
PUF_SHARES = list(META['puf_shares'])
ES_PROBS = [f'es_p{k}' for k in range(SHARED_CATEGORIES['earn_split'])]   # step 0's probabilities on PUF rows with no code
CATEGORICAL = ['married', 'n_dep', 'age_range'] + FILER + list(TAX_STATUS) + list(CATEGORIES) + list(PERSON_CATEGORIES) \
    + list(LABELS) + list(SIPP_CATEGORIES) + list(SCF_CATEGORIES) + list(SHARED_CATEGORIES)
# The SCF-only slots: the balance sheet (the home mortgage as its share of the home's value), the
# unrealized gains, and owns_business. Net worth is computed from the balance sheet in dollars.
SCF_SLOTS = list(META['scf_amounts']) + [c for c, o in CATEGORIES.items() if o == 'scf'] + list(SCF_CATEGORIES)
NET_WORTH_PARTS = dict(META['net_worth_parts'])
RATIOS = {k: tuple(v) for k, v in META['ratios'].items()}   # slot: (dollar item, its base)
# Two slots per translated item with the fill-in: a second slot, on each survey that fills the item in,
# holds what the unit reported; the original slot is the PUF concept, filled in on
# the survey side during training (fillin.py, train.py).
FILLED_ON = {}
for _r in TR.filled():
    FILLED_ON.setdefault(_r['variable'], set()).add(_r['survey'])
REPORTED = {f'{v}_reported': v for v in FILLED_ON}
CEX_SLOTS = list(META['cex_amounts'])     # the consumption categories, CEX only
CPS_SLOTS = list(META['cps_amounts']) + list(CPS_CATEGORIES)   # the CPS's person items and childcare (the spouse's
# share of the couple's wages is also on the SIPP)
SIPP_SLOTS = list(META['sipp_amounts']) + list(SIPP_CATEGORIES)   # each adult's tips and leisure flag, SIPP only
# The order: the type (married, dependents, age range), the filing flag, full filing status and
# the credit children, then the amounts. Generation draws a batch's own items in this order (generate.py).
TYPE_SLOTS = ['married', 'n_dep', 'age_range']
PREDICTED = (TYPE_SLOTS + FILER + list(TAX_STATUS) + PUF_LABELS + ['wages', 'txbl_int', 'dividends', 'capital_gains'] + KG_PIECES
             + list(ITEMS) + ['other_agi'] + [c for c, o in CATEGORIES.items() if o == 'shared']
             + PUF_SHARES + list(SHARED_CATEGORIES) + SCF_SLOTS + list(REPORTED) + CEX_SLOTS + EXACT_AGES + CPS_SLOTS + SIPP_SLOTS)
AMOUNTS = [v for v in PREDICTED if v not in CATEGORICAL]
SLOTS = CONTEXT + PREDICTED
# The sources that have each slot; a slot on one source only is owned by it.
PRESENT_ON = {**{v: set(s) for v, s in META['present_on'].items()}, **{r: set(FILLED_ON[v]) for r, v in REPORTED.items()}}
assert set(PREDICTED) <= set(PRESENT_ON), set(PREDICTED) - set(PRESENT_ON)
OWNED_BY = {v: next(iter(PRESENT_ON[v])) for v in PREDICTED if len(PRESENT_ON[v]) == 1}
# The computed totals: never drawn, computed from their parts after the parts are drawn. The same for
# the computed flags on the PUF side (k1_owner: any of its parts nonzero).
COMPUTED = {'capital_gains': {p: 1 for p in KG_PIECES}, **META['computed_totals']}
COMPUTED_FLAGS = META['computed_flags']


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
        # a slot on several surveys and not on the PUF (the spouse's wage share, CPS and SIPP): the first in SOURCES
        owner = OWNED_BY.get(a, 'puf' if 'puf' in PRESENT_ON[a] else next(s for s in SOURCES if s in PRESENT_ON[a]))
        # the owner's latest year that has the variable (net worth: the last SCF wave)
        top = list(next(_EDGES[f'{owner}/{y}/{a}'] for y in reversed(YEARS)
                        if 'positive' in _EDGES.get(f'{owner}/{y}/{a}', {}))['positive']['probs']) + [1.0]
        layout[a] = {'n_bands': n, 'first_positive': n - len(top), 'positive_top': top}
    for r, v in REPORTED.items():
        layout[r] = layout[v]
    return layout


BAND_LAYOUT = band_layout()
N_CHOICES = {'married': 2, 'n_dep': 4, 'age_range': AGE_RANGES, 'filer': 2, **TAX_STATUS, **{c: 2 for c in CATEGORIES},
             **PERSON_CATEGORIES, **LABELS, **SIPP_CATEGORIES, **SCF_CATEGORIES, **SHARED_CATEGORIES,
             **{a: BAND_LAYOUT[a]['n_bands'] for a in AMOUNTS}}
MAX_CHOICES = max(N_CHOICES.values())


# One token table for all slots; index 0 is the mask token. The amounts share
# one block of band tokens lined up by meaning: zero shares a token, the k-th
# most negative band shares one, the k-th positive band shares one.
MASK = 0
MAX_NEGATIVE = max(BAND_LAYOUT[a]['first_positive'] - 1 for a in AMOUNTS)
MAX_POSITIVE = max(BAND_LAYOUT[a]['n_bands'] - BAND_LAYOUT[a]['first_positive'] for a in AMOUNTS)
BLOCK_SIZES = {'source': len(SOURCES), 'year': len(YEARS), 'married': 2, 'n_dep': 4, 'age_range': AGE_RANGES,
               'band': 1 + MAX_NEGATIVE + MAX_POSITIVE, 'filer': 2, **TAX_STATUS, **{c: 2 for c in CATEGORIES},
               **PERSON_CATEGORIES, **LABELS, **SIPP_CATEGORIES, **SCF_CATEGORIES, **SHARED_CATEGORIES}
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
        table[r + '_band'] = np.where(table['source'].isin(FILLED_ON[v]), table[v + '_band'], -1)
    used = ['source', 'year', 'weight', 'married', 'n_dep', 'household_id', 'filer_p'] + [column(v) for v in PREDICTED] \
        + AGE_PROBS + ES_PROBS
    n_missing = table[used].isna().sum()
    if n_missing.any():
        raise ValueError(f'missing values in the training table:\n{n_missing[n_missing > 0]}')
    assert table['source'].isin(SOURCES).all() and (table['weight'] > 0).all()
    assert table['married'].isin([0, 1]).all() and table['n_dep'].isin(range(4)).all()
    # age_range: -1 only on PUF records with no range, which carry step 0's probabilities instead
    no_range = table['age_range'] == -1
    assert table['age_range'].between(-1, AGE_RANGES - 1).all() and (table.loc[no_range, 'source'] == 'puf').all()
    assert np.allclose(table.loc[no_range, AGE_PROBS].sum(1), 1) and (table.loc[~no_range, AGE_PROBS] == 0).all().all()
    everywhere = lambda v: PRESENT_ON[v] == set(SOURCES)
    for a in AMOUNTS:
        assert table[a + '_band'].between(0 if everywhere(a) else -1, N_CHOICES[a] - 1).all(), a
    # the filing flag: 1 on every PUF row (filers only); on CPS rows drawn from filer_p each pass (train.py)
    puf, cps = table.source == 'puf', table.source == 'cps'
    assert (table.loc[puf, 'filer'] == 1).all() and (table.loc[puf, 'filer_p'] == 1).all()
    assert table.loc[cps, 'filer_p'].between(0, 1).all()
    for c, n in TAX_STATUS.items():
        assert table[c].between(-1, n - 1).all(), c
    assert table.loc[table.filing_status >= 0, 'filing_status'].isin([1, 2, 3, 4]).all()
    have = table.filing_status >= 0
    assert ((table.loc[have, 'filing_status'] == 2) == (table.loc[have, 'married'] == 1)).all(), \
        'mars 2 (joint) exactly when married; separate returns are not married'
    for c in CATEGORIES:
        assert table[c].between(0 if everywhere(c) else -1, 1).all(), c
    for c, n in {**PERSON_CATEGORIES, **LABELS, **SIPP_CATEGORIES, **SCF_CATEGORIES, **SHARED_CATEGORIES}.items():
        assert table[c].between(-1, n - 1).all(), c
    # earn_split: -1 only on PUF records with no code, which carry step 0's probabilities (earn_split.py)
    none = (table['earn_split'] == -1) & (table['source'] == 'puf')   # (-1 elsewhere: absent on the surveys)
    assert table.loc[table.source == 'cps', 'earn_split'].ge(0).all()
    assert np.allclose(table.loc[none, ES_PROBS].sum(1), 1) and (table.loc[~none, ES_PROBS] == 0).all().all()
    for a in PREDICTED:
        assert (table.loc[~table.source.isin(PRESENT_ON[a]), column(a)] == -1).all(), a
    table['split'] = np.where(held_back(table), 'test', 'train')
    table['q'] = qgroups.q_of(table)      # grouped weighting: q per row
    assert (table['q'] > 0).all() and np.isfinite(table['q']).all()
    return table


def filer_draws(table, device):
    """The CPS rows and their filing chances, to draw the filing flag from (train.py)."""
    rows = np.flatnonzero((table['source'] == 'cps').to_numpy())
    return (torch.tensor(rows, device=device),
            torch.tensor(table['filer_p'].to_numpy(float)[rows], dtype=torch.float32, device=device))


def age_draws(table, device):
    """The rows with no age range and their step 0 probabilities, to draw a range from (train.py)."""
    rows = np.flatnonzero(table['age_range'].to_numpy() == -1)
    return (torch.tensor(rows, device=device),
            torch.tensor(table[AGE_PROBS].to_numpy()[rows], dtype=torch.float32, device=device))


def earn_draws(table, device):
    """The PUF rows with no earnings split code and their step 0 probabilities, to draw a code from (train.py)."""
    rows = np.flatnonzero((table['earn_split'].to_numpy() == -1) & (table['source'] == 'puf').to_numpy())
    return (torch.tensor(rows, device=device),
            torch.tensor(table[ES_PROBS].to_numpy()[rows], dtype=torch.float32, device=device))


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


def training_weights(table, use_q=True):
    """Survey weights times q (grouped weighting, qgroups.py; column q from read_table) when use_q, then rescaled to
    sum to the record count within each source and split. use_q False gives the population weights: survey weights
    alone, rescaled the same way."""
    w = table['weight'].astype(float)
    if use_q:
        w = w * table['q'].astype(float)
    total = w.groupby([table['source'], table['split']]).transform('sum')
    count = w.groupby([table['source'], table['split']]).transform('size')
    return (w * count / total).to_numpy()


def encode(table, device='cpu'):
    """values[:, j]: the within-variable index of slot j (-1 where absent); the training weights (with q); and the
    population weights (without q), for averages that must describe the population (fillin.py, the logs)."""
    cols = [table['source'].map(SOURCES.index), table['year'].map(YEARS.index)] + \
           [table[column(v)] for v in PREDICTED]
    values = np.column_stack([np.asarray(c, dtype=np.int64) for c in cols])
    return {'values': torch.tensor(values, device=device),
            'weight': torch.tensor(training_weights(table), dtype=torch.float32, device=device),
            'pop_weight': torch.tensor(training_weights(table, use_q=False), dtype=torch.float32, device=device)}


def seen_as(values, hidden, label, year=None):
    """The record as source label would see it: that label (and year, if given), the slots it lacks absent. year: one
    year for every record, or a tensor of one index into YEARS per record (the SCF phase of generation, each record
    at its own wave's year)."""
    view, view_hidden = values.clone(), hidden.clone()
    view[:, 0] = SOURCES.index(label)
    if isinstance(year, torch.Tensor):
        view[:, 1] = year
    elif year is not None:
        view[:, 1] = YEARS.index(year)
    for j, v in enumerate(PREDICTED):
        if label not in PRESENT_ON[v]:
            view[:, len(CONTEXT) + j], view_hidden[:, j] = -1, False
    return view, view_hidden


# ---- packing: a batch of one source carries only that source's slots ----------
# SOURCE_SLOTS[src]: the positions in SLOTS a record of src carries (the two context slots, then its
# predicted slots in PREDICTED order). A packed record is values[:, SOURCE_SLOTS[src]].
SOURCE_SLOTS = {src: tuple([0, 1] + [len(CONTEXT) + j for j, v in enumerate(PREDICTED) if src in PRESENT_ON[v]])
                for src in SOURCES}
_SLOT_TENSORS = {}


def slot_tensor(slots, device):
    key = (slots, str(device))
    if key not in _SLOT_TENSORS:
        _SLOT_TENSORS[key] = torch.tensor(slots, dtype=torch.long, device=device)
    return _SLOT_TENSORS[key]


def predicted_of(slots):
    """The PREDICTED indices of a slot subset (its slots after the two context slots)."""
    return [s - len(CONTEXT) for s in slots[len(CONTEXT):]]


def pack(values, hidden, slots):
    """Full-width values [N, SLOTS] and hidden [N, PREDICTED] cut down to a slot subset."""
    st = slot_tensor(slots, values.device)
    return values[:, st], hidden[:, st[len(CONTEXT):] - len(CONTEXT)]


def token_ids(values, hidden, slots):
    """Token per slot; hidden and absent slots get the mask token (absent ones are also masked out of attention).
    slots: the SLOTS positions the columns of values stand for."""
    slot = slot_tensor(slots, values.device)
    ids = TOKEN_OF.to(values.device)[slot, values.clamp(min=0)]
    full_hidden = torch.cat([torch.zeros_like(hidden[:, :len(CONTEXT)]), hidden], 1)
    return torch.where(full_hidden | (values < 0), MASK, ids)


# ---- the CPS's shares: shown only with what decides whether they are asked ---------
# The spouse's share of the couple's wages (of its self-employment income) is present on a CPS row exactly when the
# unit is married with some wages (some self-employment income). A present share therefore tells the network the
# unit is married with some of the item. Generation draws a non-filer's married flag and amounts under the CPS label
# before it knows either (generate.py), so in training, and in every call of the network, a share is left absent
# (out of the record) whenever married or its base is hidden; it is present only once both are shown. Its own
# question is then only ever asked given married and its base, as generation asks it.
# The same holds for the spouse's share of the couple's self-employment income, a
# PUF slot shared with the CPS, whose base is Schedules C and F or part_se (Schedule SE earnings other than C and F)
# (any of several bases; a base a source lacks, as the CPS lacks the partnership item, counts as zero, and is not one
# that can be hidden). The table's meta "share_gates" names each share's bases.
# {share's PREDICTED index: (married's, [its bases'])}; a base band above 0 is a nonzero amount
SHARE_GATES = {PREDICTED.index(s): (PREDICTED.index('married'), [PREDICTED.index(b) for b in bases])
               for s, bases in META['share_gates'].items()}


def gate_shares(values, hidden, slots):
    """Packed values [N, len(slots)] and hidden [N, predicted positions]: each share made absent where married or any of
    its bases the source has is hidden (see above). Returns new tensors."""
    pred = predicted_of(slots)
    at = {j: k for k, j in enumerate(pred)}
    gated = [(at[s], at[m], [at[b] for b in bases if b in at]) for s, (m, bases) in SHARE_GATES.items() if s in at]
    if not gated:
        return values, hidden
    values, hidden = values.clone(), hidden.clone()
    for ks, km, kbs in gated:
        off = hidden[:, km] | (values[:, len(CONTEXT) + km] < 0)
        for kb in kbs:
            off = off | hidden[:, kb] | (values[:, len(CONTEXT) + kb] < 0)
        values[off, len(CONTEXT) + ks] = -1
        hidden[off, ks] = False
    return values, hidden


def share_presence(values, hidden):
    """Full-width values [N, SLOTS] and hidden [N, PREDICTED] in one source's view: each share present (and hidden)
    exactly when married and every base the view has (not absent) are shown, married is 1 and some base nonzero; absent
    otherwise."""
    values, hidden = values.clone(), hidden.clone()
    for s, (m, bases) in SHARE_GATES.items():
        vm = values[:, len(CONTEXT) + m]
        asked = ~hidden[:, m] & (vm == 1)
        some = torch.zeros_like(asked)
        for b in bases:
            vb = values[:, len(CONTEXT) + b]
            asked = asked & (~hidden[:, b] | (vb < 0))
            some = some | (vb > 0)
        asked = asked & some
        values[:, len(CONTEXT) + s] = torch.where(asked, 0, -1)
        hidden[:, s] = asked
    return values, hidden


# ---- network ------------------------------------------------------------------
def safe_chunk(n_slots, limit=20_000):
    """Records per forward pass without gradients, so that one chunk's attention scores (records x heads x
    slots x slots) stay under 1 billion entries, half the size at which PyTorch's masked attention failed."""
    heads = common.SETTINGS['network']['heads']
    return max(1, min(limit, int(1e9 // (heads * n_slots * n_slots))))


class SlotTransformer(nn.Module):
    """Slot vector = token embedding + slot embedding; encoder layers; one output head per predicted variable.
    Every call runs on one source's slots (SOURCE_SLOTS, "packing"): values has one column per position in slots
    (the two context slots first), hidden one per predicted position."""

    def __init__(self, width, layers, heads, ffn, dropout):
        super().__init__()
        self.settings = dict(width=width, layers=layers, heads=heads, ffn=ffn, dropout=dropout)
        self.token_embedding = nn.Embedding(VOCAB, width)
        self.slot_embedding = nn.Parameter(torch.randn(len(SLOTS), width) * 0.02)
        layer = nn.TransformerEncoderLayer(width, heads, ffn, dropout, batch_first=True, norm_first=True)
        self.encoder = nn.TransformerEncoder(layer, layers, enable_nested_tensor=False)
        self.final_norm = nn.LayerNorm(width)
        self.heads = nn.ModuleList(nn.Linear(width, N_CHOICES[v]) for v in PREDICTED)

    def last_layer(self, values, hidden, slots):
        """The network's last layer at each position: [records, len(slots), width]."""
        x = self.token_embedding(token_ids(values, hidden, slots)) + self.slot_embedding[slot_tensor(slots, values.device)]
        return self.final_norm(self.encoder(x, src_key_padding_mask=values < 0))

    def forward(self, values, hidden, slots):
        """Log-probabilities, [records, predicted positions of slots, MAX_CHOICES]; choices a variable lacks get -inf.
        All output layers in one batched multiplication: each padded to MAX_CHOICES choices, the padded choices given
        a bias of -inf, so their probability is exactly zero. Only the batch's own output layers are stacked, so the
        others get no gradient at all (not a zero gradient): AdamW then leaves them untouched."""
        h = self.last_layer(values, hidden, slots)
        W, b = self.stacked_heads([self.heads[j] for j in predicted_of(slots)])
        logits = torch.einsum('bjw,jcw->bjc', h[:, len(CONTEXT):], W) + b
        return logits.float().log_softmax(-1)

    def slot_probs(self, h, j, groups=None):
        """Per record i, the distribution of predicted slot j[i] from its last-layer vector h[i]: [records,
        MAX_CHOICES]. Only the output layers needed (generation). groups: [(slot, its rows)] when the caller knows
        the distinct slots in j and the rows at each (the redraw, top_billionaires), so j is not read back."""
        out = h.new_full((len(j), MAX_CHOICES), -math.inf)
        if groups is None:
            groups = [(jj, (j == jj).nonzero().squeeze(1)) for jj in torch.unique(j).tolist()]
        for jj, sel in groups:
            head = self.heads[jj]
            out[sel, :head.out_features] = head(h[sel])
        return out.log_softmax(-1).exp()

    @staticmethod
    def stacked_heads(heads):
        pad = lambda hd: MAX_CHOICES - hd.out_features
        W = torch.stack([nn.functional.pad(hd.weight, (0, 0, 0, pad(hd))) for hd in heads])
        b = torch.stack([nn.functional.pad(hd.bias, (0, pad(hd)), value=-math.inf) for hd in heads])
        return W, b


def log_probs_as(model, values, hidden, src):
    """The network on full-width records seen as source src (values [N, SLOTS] and hidden [N, PREDICTED] already
    in that source's view); returns a function from a PREDICTED index to its log-probabilities [N, MAX_CHOICES]."""
    slots = SOURCE_SLOTS[src]
    out = model(*pack(values, hidden, slots), slots)
    index = {j: k for k, j in enumerate(predicted_of(slots))}
    return lambda j: out[:, index[j]]


def new_model(device):
    # keys starting with '_' are notes in settings.json, not network settings
    network = {k: v for k, v in common.SETTINGS['network'].items() if not k.startswith('_')}
    return SlotTransformer(**network).to(device)


def save_model(model, path):
    torch.save({'state': model.state_dict(), 'settings': model.settings, 'band_layout': BAND_LAYOUT,
                'slots': SLOTS}, path)


def load_model(device, path):
    """A trained network; its slots and band layout (names and order) must equal today's."""
    saved = torch.load(path, map_location=device)
    assert saved['slots'] == SLOTS, 'the slots changed since training'
    assert saved['band_layout'] == BAND_LAYOUT, 'the band edges changed since training'
    assert list(saved['band_layout']) == list(BAND_LAYOUT), 'the amounts\' order changed since training'
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


def masked_loss(model, values, weight, hidden, slots, denominator):
    """Weighted cross-entropy (nats) over the hidden slots, divided by denominator (train.py: the records in the
    batch times the average weight x expected hidden slots per record)."""
    nll = slot_nll(model(values, hidden, slots), values)
    w = weight[:, None] * hidden
    return (nll * w).sum() / denominator


def masked_sums(model, values, weight, hidden, slots):
    """The two sums behind masked_loss: weighted cross-entropy and weight, over the hidden slots."""
    nll = slot_nll(model(values, hidden, slots), values)
    w = weight[:, None] * hidden
    return (nll * w).sum(), w.sum()
