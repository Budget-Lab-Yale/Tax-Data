"""Masked transformer for records from several sources (test 2, piece B).

Copied from test 1's puf_generator/model.py and extended:

  - Variables a source does not observe are coded -1 in the token files.
    They are always hidden from the model and never scored.
  - A learned embedding for the record's source (PUF, SCF, SCF_H1, ...) is
    added to every variable slot.
  - The record's year enters through a small learned function of
    (year - 2015), also added to every slot, so that 2015 can lie between
    the 2013 and 2016 SCF waves.
  - Context variables (meta.json group "context": source, year) are not
    slots. Source and year are read from the records_*.parquet files.

Each record is a set of variable slots. A slot's input vector is the sum of
  - a learned embedding for the variable's identity,
  - a learned embedding for its token, or a learned MASK vector if hidden
    (the same vector whether hidden for training or unobserved),
  - for ordered variables (amounts and the weight), a learned function of
    the token's position in the variable's value order,
  - the source embedding and the year function.
The output for each slot is a probability distribution over that
variable's own tokens.
"""

import json
import math
from pathlib import Path

import numpy as np
import pandas as pd
import torch
import torch.nn as nn
import torch.nn.functional as F

ORDERED_KINDS = ("amount", "weight")
YEAR_CENTRE = 2015.0
YEAR_SCALE = 5.0          # (year - 2015) / 5 is fed to the year function


# ---------------------------------------------------------------------------
# Reading meta.json and the data files
# ---------------------------------------------------------------------------

def load_meta(path):
    with open(path) as f:
        return json.load(f)


def all_variables(meta):
    return meta["variables"]


def slot_columns(meta):
    """Column indices (in the token arrays) of the variables that are model
    slots: everything except group 'context'."""
    return [j for j, v in enumerate(all_variables(meta))
            if v.get("group") != "context"]


def slot_variables(meta):
    vs = all_variables(meta)
    return [vs[j] for j in slot_columns(meta)]


def source_names(meta, records=None):
    """The list of source names; the index into it is the source code."""
    s = meta.get("sources")
    if s:
        out = []
        for x in s:
            out.append(x["name"] if isinstance(x, dict) else str(x))
        return out
    if records is None:
        raise ValueError("meta.json has no 'sources' and no records were given")
    return sorted(pd.unique(records["source"].astype(str)))


def fill_order(meta, slot_vars=None):
    """Order (indices into slot variables) in which unobserved variables are
    filled during completion: unobserved shared variables first (meta.json
    variable order), then meta['order'] (names or slot indices), then any
    variable not yet listed."""
    sv = slot_vars if slot_vars is not None else slot_variables(meta)
    names = [v["name"] for v in sv]
    listed = (meta.get("order") or meta.get("completion_order")
              or meta.get("generation_order") or [])
    listed_idx = []
    for x in listed:
        if isinstance(x, str):
            if x in names:
                listed_idx.append(names.index(x))
        else:
            listed_idx.append(int(x))
    shared_first = [j for j, v in enumerate(sv)
                    if v.get("group") == "shared" and j not in listed_idx]
    rest = [j for j in range(len(sv)) if j not in listed_idx and j not in shared_first]
    order = shared_first + listed_idx + rest
    assert sorted(order) == list(range(len(sv))), "fill order is not a permutation"
    return order


def load_split(data_dir, split, meta, sources, keep_sources=None):
    """Read tokens_<split>.npy (slot columns only) and the source and year of
    each record. Returns dict(tok, src, year, records, rows) where rows are
    the row numbers kept (all rows unless keep_sources filters)."""
    data_dir = Path(data_dir)
    tok_all = np.load(data_dir / f"tokens_{split}.npy")
    rec = pd.read_parquet(data_dir / f"records_{split}.parquet")
    if len(rec) != tok_all.shape[0]:
        raise ValueError(f"{split}: {len(rec)} records but {tok_all.shape[0]} token rows")
    cols = slot_columns(meta)
    tok = tok_all[:, cols].astype(np.int64)
    src_str = rec["source"].astype(str).to_numpy()
    unknown = sorted(set(src_str) - set(sources))
    if unknown:
        raise ValueError(f"{split}: sources {unknown} not in {sources}")
    src = np.array([sources.index(s) for s in src_str], dtype=np.int64)
    year = rec["year"].to_numpy(dtype=np.float64)
    rows = np.arange(len(rec))
    if keep_sources is not None:
        keep = np.isin(src_str, keep_sources)
        rows = rows[keep]
        tok, src, year = tok[keep], src[keep], year[keep]
    return dict(tok=tok, src=src, year=year, records=rec, rows=rows,
                tok_all=tok_all)


def check_tokens(name, arr, vocab):
    if arr.ndim != 2 or arr.shape[1] != len(vocab):
        raise ValueError(f"{name}: shape {arr.shape}, expected [*, {len(vocab)}]")
    bad_lo = (arr < -1).sum(axis=0)
    bad_hi = (arr >= np.asarray(vocab)[None, :]).sum(axis=0)
    if bad_lo.any() or bad_hi.any():
        raise ValueError(f"{name}: tokens out of range: below -1 {bad_lo.tolist()}, "
                         f"above vocab {bad_hi.tolist()}")


def _token_list(v):
    for key in ("tokens", "bins", "values"):
        if key in v and isinstance(v[key], list):
            return v[key]
    return None


def token_positions(v):
    """Normalised position (0..1) of each token in the variable's value order.

    Zero and negative tokens are placed by their sign: negatives (by their
    own lo) below zero, zero next, then positive rank bins by lo. If no
    usable information is present the token index is the order."""
    n = int(v["vocab_size"])
    toks = _token_list(v)
    key = None
    if toks is not None and len(toks) == n and all(isinstance(t, dict) for t in toks):
        def k(t, i):
            neg = bool(t.get("negative", False)) or t.get("sign") == -1
            zero = bool(t.get("zero", False)) or t.get("sign") == 0
            lo = t.get("lo", t.get("value", i))
            try:
                lo = float(lo)
            except (TypeError, ValueError):
                lo = float(i)
            if lo != lo:          # NaN
                lo = float(i)
            band = 0 if neg else (1 if zero else 2)
            return (band, lo, i)
        key = [k(t, i) for i, t in enumerate(toks)]
    if key is None:
        pos = np.arange(n, dtype=float)
    else:
        rank = sorted(range(n), key=lambda i: key[i])
        pos = np.empty(n)
        pos[rank] = np.arange(n)
    return pos / max(n - 1, 1)


def build_vocab_tables(variables):
    vocab = np.array([int(v["vocab_size"]) for v in variables])
    offsets = np.concatenate([[0], np.cumsum(vocab)[:-1]])
    total = int(vocab.sum())
    pos = np.zeros(total, dtype=np.float32)
    ordered = np.zeros(total, dtype=np.float32)
    for j, v in enumerate(variables):
        s = slice(offsets[j], offsets[j] + vocab[j])
        if v.get("kind") in ORDERED_KINDS:
            pos[s] = token_positions(v)
            ordered[s] = 1.0
    return dict(vocab=vocab, offsets=offsets, total=total, pos=pos, ordered=ordered)


# ---------------------------------------------------------------------------
# Model
# ---------------------------------------------------------------------------

class OrderedPositionMLP(nn.Module):
    """Learned function of a bin's normalised position p in [0, 1]."""

    def __init__(self, d_model, n_freq=16, hidden=128):
        super().__init__()
        self.register_buffer("freqs", torch.arange(1, n_freq + 1).float() * math.pi,
                             persistent=False)
        self.net = nn.Sequential(nn.Linear(1 + 2 * n_freq, hidden), nn.GELU(),
                                 nn.Linear(hidden, d_model))

    def forward(self, p):
        a = p[:, None] * self.freqs[None, :]
        feats = torch.cat([p[:, None], torch.sin(a), torch.cos(a)], dim=1)
        return self.net(feats)


class YearMLP(nn.Module):
    """Learned smooth function of the year: (year - 2015) / 5 through a small
    two-layer network. The last layer starts at zero, so the year has no
    effect until training gives it one."""

    def __init__(self, d_model, hidden=64):
        super().__init__()
        self.net = nn.Sequential(nn.Linear(1, hidden), nn.GELU(),
                                 nn.Linear(hidden, d_model))
        nn.init.zeros_(self.net[2].weight)
        nn.init.zeros_(self.net[2].bias)

    def forward(self, year):
        x = ((year - YEAR_CENTRE) / YEAR_SCALE).float()[:, None]
        return self.net(x)


class RecordTransformer(nn.Module):
    def __init__(self, variables, n_sources, d_model=256, n_layers=6, n_heads=8,
                 ff_mult=4, dropout=0.1, use_source=True):
        super().__init__()
        t = build_vocab_tables(variables)
        self.n_vars = len(t["vocab"])
        self.n_sources = n_sources
        self.config = dict(d_model=d_model, n_layers=n_layers, n_heads=n_heads,
                           ff_mult=ff_mult, dropout=dropout, use_source=use_source)
        self.use_source = use_source
        vocab = torch.tensor(t["vocab"], dtype=torch.long)
        offsets = torch.tensor(t["offsets"], dtype=torch.long)
        self.register_buffer("vocab", vocab, persistent=False)
        self.register_buffer("offsets", offsets, persistent=False)
        self.register_buffer("tok_pos", torch.tensor(t["pos"]), persistent=False)
        self.register_buffer("tok_ordered", torch.tensor(t["ordered"]), persistent=False)
        self.total = t["total"]
        max_v = int(vocab.max())
        self.max_v = max_v
        k = torch.arange(max_v)[None, :]
        valid = k < vocab[:, None]
        gidx = torch.where(valid, offsets[:, None] + k, torch.zeros_like(k))
        self.register_buffer("out_gidx", gidx, persistent=False)
        self.register_buffer("out_valid", valid, persistent=False)

        self.var_emb = nn.Embedding(self.n_vars, d_model)
        self.tok_emb = nn.Embedding(self.total, d_model)
        self.mask_emb = nn.Parameter(torch.zeros(d_model))
        self.in_pos = OrderedPositionMLP(d_model)
        self.src_emb = nn.Embedding(n_sources, d_model)
        self.year_fn = YearMLP(d_model)

        layer = nn.TransformerEncoderLayer(
            d_model, n_heads, dim_feedforward=ff_mult * d_model,
            dropout=dropout, activation="gelu", batch_first=True,
            norm_first=True)
        self.encoder = nn.TransformerEncoder(layer, n_layers,
                                             enable_nested_tensor=False)
        self.final_norm = nn.LayerNorm(d_model)

        self.out_proj = nn.Linear(d_model, d_model)
        self.out_tok = nn.Embedding(self.total, d_model)
        self.out_pos = OrderedPositionMLP(d_model)
        self.out_bias = nn.Parameter(torch.zeros(self.total))

        nn.init.normal_(self.var_emb.weight, std=0.02)
        nn.init.normal_(self.tok_emb.weight, std=0.02)
        nn.init.normal_(self.out_tok.weight, std=0.02)
        nn.init.normal_(self.mask_emb, std=0.02)
        nn.init.normal_(self.src_emb.weight, std=0.02)

    def _token_table(self, emb, pos_mlp):
        return emb.weight + self.tok_ordered[:, None] * pos_mlp(self.tok_pos)

    def forward(self, tokens, hidden, src, year):
        """tokens: [B, V] local token ids (-1 or anything at hidden slots).
        hidden: [B, V] bool, True where hidden (for training or unobserved).
        src: [B] source codes. year: [B] years (float).
        Returns logits [B, V, max_v] (float32), -inf for padding."""
        g = tokens.clamp(min=0) + self.offsets[None, :]
        g = torch.where(hidden, torch.zeros_like(g), g)
        in_table = self._token_table(self.tok_emb, self.in_pos)
        x = in_table[g]
        x = torch.where(hidden[..., None], self.mask_emb.to(x.dtype), x)
        # Keep the embedding allocated so the ablation preserves seeded
        # initialization of every other parameter and old checkpoints load.
        ctx = self.year_fn(year)                                # [B, d]
        if self.use_source:
            ctx = self.src_emb(src) + ctx
        x = x + self.var_emb.weight[None, :, :] + ctx[:, None, :]
        h = self.encoder(x)
        h = self.out_proj(self.final_norm(h))
        out_table = self._token_table(self.out_tok, self.out_pos)
        w = out_table[self.out_gidx]
        b = self.out_bias[self.out_gidx]
        logits = torch.einsum("bvd,vkd->bvk", h, w.to(h.dtype)).float() + b
        logits = logits.masked_fill(~self.out_valid[None], float("-inf"))
        return logits


def masked_ce(logits, tokens, scored):
    """Mean cross-entropy (nats) over scored slots; also the sum and count."""
    lp = F.log_softmax(logits, dim=-1)
    nll = -lp.gather(-1, tokens.clamp(min=0)[..., None]).squeeze(-1)
    nll = nll * scored
    s = nll.sum()
    n = scored.sum()
    return s / n.clamp(min=1), s.detach(), n.detach()


def draw_hidden_observed(observed, generator):
    """Any-order hiding restricted to observed variables.

    observed: [B, V] bool. Per record a rate r ~ U(0, 1); each observed
    variable is hidden with probability r; at least one observed variable is
    hidden (chosen uniformly among the observed ones if none was).
    Returns (hidden_input, scored): hidden_input also hides every unobserved
    variable; scored is the randomly hidden observed variables."""
    B, V = observed.shape
    device = observed.device
    r = torch.rand(B, 1, generator=generator, device=device)
    u = torch.rand(B, V, generator=generator, device=device)
    scored = (u < r) & observed
    none = ~scored.any(dim=1)
    if none.any():
        w = observed[none].float()
        pick = torch.multinomial(w, 1, generator=generator).squeeze(1)
        rows = torch.nonzero(none).squeeze(1)
        scored[rows, pick] = True
    hidden_input = scored | ~observed
    return hidden_input, scored


def count_parameters(model):
    return sum(p.numel() for p in model.parameters() if p.requires_grad)


def save_checkpoint(path, model, meta_path, sources, extra):
    torch.save(dict(state_dict=model.state_dict(), config=model.config,
                    meta_path=str(meta_path), sources=list(sources), **extra), path)


def load_checkpoint(path, meta, device):
    ck = torch.load(path, map_location=device, weights_only=False)
    model = RecordTransformer(slot_variables(meta), len(ck["sources"]),
                              **ck["config"]).to(device)
    model.load_state_dict(ck["state_dict"])
    model.eval()
    return model, ck


# ---------------------------------------------------------------------------
# Completion and scoring
# ---------------------------------------------------------------------------

@torch.no_grad()
def complete_batch(model, tok, src, year, order, gen, temperature=1.0, amp=True,
                   allowed=None):
    """Fill every unobserved (-1) slot of tok [B, V] in `order`, each drawn
    from the model's distribution given everything observed and filled so
    far. `allowed` [B, V, max_v] bool, if given, restricts the draw to the
    allowed codes (codes the decoder can turn into dollars for that
    record). Returns the completed tokens [B, V]."""
    tok = tok.clone()
    hid = tok < 0
    dev = tok.device
    for j in order:
        need = hid[:, j]
        if not need.any():
            continue
        with torch.autocast("cuda", dtype=torch.bfloat16,
                            enabled=amp and dev.type == "cuda"):
            logits = model(tok, hid, src, year)
        lj = logits[:, j, :].float()
        if allowed is not None:
            lj = lj.masked_fill(~allowed[:, j, :lj.shape[1]], float("-inf"))
        probs = torch.softmax(lj / temperature, dim=-1)
        draw = torch.multinomial(probs, 1, generator=gen).squeeze(1)
        tok[:, j] = torch.where(need, draw, tok[:, j])
        hid[:, j] = False
    return tok


@torch.no_grad()
def completion_nll(model, tok_in, tok_truth, src, year, order, batch_size, device,
                   amp=True):
    """Log-likelihood of the true values of the unobserved slots under the
    completion procedure: for each record, the sum over the fill order of
    -log p(true x_j | observed and earlier true values). Only slots that are
    -1 in tok_in and known (>= 0) in tok_truth are scored; slots unobserved
    in both stay hidden throughout. Returns per-record, per-variable NLL
    [N, V] (0 where not scored) and the scored mask."""
    N, V = tok_in.shape
    nll_out = np.zeros((N, V), dtype=np.float64)
    scored_out = (tok_in < 0) & (tok_truth >= 0)
    for s0 in range(0, N, batch_size):
        ti = torch.as_tensor(tok_in[s0:s0 + batch_size], device=device)
        tt = torch.as_tensor(tok_truth[s0:s0 + batch_size], device=device)
        sr = torch.as_tensor(src[s0:s0 + batch_size], device=device)
        yr = torch.as_tensor(year[s0:s0 + batch_size], device=device)
        sc = torch.as_tensor(scored_out[s0:s0 + batch_size], device=device)
        hid = ti < 0
        cur = torch.where(sc, tt, ti)
        res = torch.zeros(ti.shape, device=device, dtype=torch.float64)
        for j in order:
            if not sc[:, j].any():
                continue
            with torch.autocast("cuda", dtype=torch.bfloat16,
                                enabled=amp and device.type == "cuda"):
                logits = model(cur, hid, sr, yr)
            lp = F.log_softmax(logits[:, j, :].float(), dim=-1)
            v = -lp.gather(1, cur[:, j:j + 1].clamp(min=0)).squeeze(1)
            res[:, j] = torch.where(sc[:, j], v.double(), res[:, j])
            hid[:, j] = hid[:, j] & ~sc[:, j]
        nll_out[s0:s0 + batch_size] = res.cpu().numpy()
    return nll_out, scored_out
