"""Masked transformer model for tax records (part B of SPEC.md).

Each record is a set of variables. Each variable is one input slot; its
input vector is the sum of
  - a learned embedding for the variable's identity,
  - a learned embedding for its token, or a learned MASK vector if hidden,
  - for ordered variables (amounts and the weight), a learned function of
    the token's position in the variable's value order, so that the model
    starts out knowing that neighbouring bins are close.
There is no position encoding: the slots form an unordered set, and the
variable-identity embedding is the only thing that tells slots apart.

The output for each slot is a probability distribution over that
variable's own tokens. All variables share one global token index
(variable j's tokens occupy offsets[j] .. offsets[j] + vocab[j] - 1); the
output layer scores a slot's hidden vector against an output embedding for
each of the variable's tokens, and tokens belonging to other variables are
excluded by padding with -inf.
"""

import json
import math

import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F

ORDERED_KINDS = ("amount", "weight")


# ---------------------------------------------------------------------------
# Reading meta.json
# ---------------------------------------------------------------------------

def load_meta(path):
    with open(path) as f:
        return json.load(f)


def meta_variables(meta):
    """The variable list, in column order of the token arrays."""
    return meta["variables"]


def generation_order(meta):
    """Indices (into the column order) of the variables in generation order.

    meta["order"] may list names or integer indices.
    """
    names = [v["name"] for v in meta_variables(meta)]
    order = meta.get("order", names)
    if len(order) > 0 and isinstance(order[0], str):
        idx = [names.index(n) for n in order]
    else:
        idx = [int(i) for i in order]
    if sorted(idx) != list(range(len(names))):
        raise ValueError("meta['order'] is not a permutation of the variables")
    return idx


def _token_list(v):
    """The per-token description list of a variable, whatever A named it."""
    for key in ("tokens", "bins", "values"):
        if key in v and isinstance(v[key], list):
            return v[key]
    return None


def token_positions(v):
    """Normalised position (0..1) of each token in the variable's value order.

    Tokens are sorted by (lo, hi) as given in meta.json. If the variable has
    no lo/hi information the token index itself is taken as the order.
    """
    n = int(v["vocab_size"])
    toks = _token_list(v)
    if toks is not None and len(toks) == n and isinstance(toks[0], dict) \
            and "lo" in toks[0] and "hi" in toks[0]:
        lo = np.array([float(t["lo"]) for t in toks])
        hi = np.array([float(t["hi"]) for t in toks])
        rank = np.lexsort((hi, lo))          # sort by lo, then hi
        pos = np.empty(n)
        pos[rank] = np.arange(n)
    else:
        pos = np.arange(n, dtype=float)
    return pos / max(n - 1, 1)


def build_vocab_tables(meta):
    """Arrays describing the global token index."""
    vars_ = meta_variables(meta)
    vocab = np.array([int(v["vocab_size"]) for v in vars_])
    offsets = np.concatenate([[0], np.cumsum(vocab)[:-1]])
    total = int(vocab.sum())
    pos = np.zeros(total, dtype=np.float32)
    ordered = np.zeros(total, dtype=np.float32)
    for j, v in enumerate(vars_):
        s = slice(offsets[j], offsets[j] + vocab[j])
        if v["kind"] in ORDERED_KINDS:
            pos[s] = token_positions(v)
            ordered[s] = 1.0
    return dict(vocab=vocab, offsets=offsets, total=total,
                pos=pos, ordered=ordered)


# ---------------------------------------------------------------------------
# Model
# ---------------------------------------------------------------------------

class OrderedPositionMLP(nn.Module):
    """Learned function of a bin's normalised position p in [0, 1].

    p is expanded into sine and cosine features at several frequencies
    (so the function can be smooth or sharp) and passed through a small
    two-layer network.
    """

    def __init__(self, d_model, n_freq=16, hidden=128):
        super().__init__()
        self.register_buffer("freqs", torch.arange(1, n_freq + 1).float() * math.pi,
                             persistent=False)
        self.net = nn.Sequential(nn.Linear(1 + 2 * n_freq, hidden), nn.GELU(),
                                 nn.Linear(hidden, d_model))

    def forward(self, p):                      # p: [N]
        a = p[:, None] * self.freqs[None, :]
        feats = torch.cat([p[:, None], torch.sin(a), torch.cos(a)], dim=1)
        return self.net(feats)


class RecordTransformer(nn.Module):
    def __init__(self, meta, d_model=256, n_layers=6, n_heads=8,
                 ff_mult=4, dropout=0.1):
        super().__init__()
        t = build_vocab_tables(meta)
        self.n_vars = len(t["vocab"])
        self.config = dict(d_model=d_model, n_layers=n_layers, n_heads=n_heads,
                           ff_mult=ff_mult, dropout=dropout)
        vocab = torch.tensor(t["vocab"], dtype=torch.long)
        offsets = torch.tensor(t["offsets"], dtype=torch.long)
        self.register_buffer("vocab", vocab, persistent=False)
        self.register_buffer("offsets", offsets, persistent=False)
        self.register_buffer("tok_pos", torch.tensor(t["pos"]), persistent=False)
        self.register_buffer("tok_ordered", torch.tensor(t["ordered"]), persistent=False)
        self.total = t["total"]
        max_v = int(vocab.max())
        self.max_v = max_v
        # Padded [n_vars, max_v] map from (variable, local token) to global
        # token, and a mask that is True where the local token exists.
        k = torch.arange(max_v)[None, :]
        valid = k < vocab[:, None]
        gidx = torch.where(valid, offsets[:, None] + k, torch.zeros_like(k))
        self.register_buffer("out_gidx", gidx, persistent=False)
        self.register_buffer("out_valid", valid, persistent=False)

        self.var_emb = nn.Embedding(self.n_vars, d_model)
        self.tok_emb = nn.Embedding(self.total, d_model)
        self.mask_emb = nn.Parameter(torch.zeros(d_model))
        self.in_pos = OrderedPositionMLP(d_model)

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

    def _token_table(self, emb, pos_mlp):
        """Token vectors for every global token, with the ordered-position
        term added for amount and weight tokens."""
        return emb.weight + self.tok_ordered[:, None] * pos_mlp(self.tok_pos)

    def forward(self, tokens, hidden):
        """tokens: [B, V] local token ids (values at hidden slots ignored).
        hidden: [B, V] bool, True where the variable is hidden.
        Returns logits [B, V, max_v] (float32), -inf for padding."""
        B, V = tokens.shape
        g = (tokens.clamp(min=0) + self.offsets[None, :])
        g = torch.where(hidden, torch.zeros_like(g), g)
        in_table = self._token_table(self.tok_emb, self.in_pos)
        x = in_table[g]                                   # [B, V, d]
        x = torch.where(hidden[..., None], self.mask_emb.to(x.dtype), x)
        x = x + self.var_emb.weight[None, :, :]
        h = self.encoder(x)
        h = self.out_proj(self.final_norm(h))             # [B, V, d]
        out_table = self._token_table(self.out_tok, self.out_pos)
        w = out_table[self.out_gidx]                      # [V, max_v, d]
        b = self.out_bias[self.out_gidx]                  # [V, max_v]
        logits = torch.einsum("bvd,vkd->bvk", h, w.to(h.dtype)).float() + b
        logits = logits.masked_fill(~self.out_valid[None], float("-inf"))
        return logits


def masked_ce(logits, tokens, hidden):
    """Mean cross-entropy (nats) over hidden slots; also returns the sum and
    count so callers can average over a whole epoch exactly."""
    B, V, K = logits.shape
    lp = F.log_softmax(logits, dim=-1)
    nll = -lp.gather(-1, tokens.clamp(min=0)[..., None]).squeeze(-1)   # [B, V]
    nll = nll * hidden
    s = nll.sum()
    n = hidden.sum()
    return s / n.clamp(min=1), s.detach(), n.detach()


def draw_hidden(B, V, generator, device):
    """Any-order hiding: per record r ~ U(0,1), hide each variable with
    probability r, and force at least one hidden variable."""
    r = torch.rand(B, 1, generator=generator, device=device)
    u = torch.rand(B, V, generator=generator, device=device)
    hidden = u < r
    none = ~hidden.any(dim=1)
    if none.any():
        pick = torch.randint(0, V, (B,), generator=generator, device=device)
        hidden[none, pick[none]] = True
    return hidden


def draw_hidden_order(B, V, order_rank, generator, device):
    """Masks consistent with the generation order: the first k variables in
    the order are visible and the rest hidden, k uniform on 0..V-1."""
    k = torch.randint(0, V, (B, 1), generator=generator, device=device)
    return order_rank[None, :] >= k


def count_parameters(model):
    return sum(p.numel() for p in model.parameters() if p.requires_grad)


def save_checkpoint(path, model, meta_path, extra):
    torch.save(dict(state_dict=model.state_dict(), config=model.config,
                    meta_path=str(meta_path), **extra), path)


def load_checkpoint(path, meta, device):
    ck = torch.load(path, map_location=device, weights_only=False)
    model = RecordTransformer(meta, **ck["config"]).to(device)
    model.load_state_dict(ck["state_dict"])
    model.eval()
    return model, ck


@torch.no_grad()
def sample_records(model, n, order, batch_size, seed, device,
                   temperature=1.0, amp=True):
    """Generate n records: all variables hidden, then fill one at a time in
    `order`, each drawn from the model's distribution given what has been
    filled so far. Returns an int64 numpy array [n, V]."""
    V = model.n_vars
    gen = torch.Generator(device=device)
    gen.manual_seed(seed)
    out = np.empty((n, V), dtype=np.int64)
    for start in range(0, n, batch_size):
        b = min(batch_size, n - start)
        tok = torch.zeros(b, V, dtype=torch.long, device=device)
        hid = torch.ones(b, V, dtype=torch.bool, device=device)
        for j in order:
            with torch.autocast("cuda", dtype=torch.bfloat16,
                                enabled=amp and device.type == "cuda"):
                logits = model(tok, hid)
            lj = logits[:, j, :].float() / temperature
            probs = torch.softmax(lj, dim=-1)
            tok[:, j] = torch.multinomial(probs, 1, generator=gen).squeeze(1)
            hid[:, j] = False
        out[start:start + b] = tok.cpu().numpy()
    return out


@torch.no_grad()
def gibbs_refine(model, tokens, sweeps, batch_size, seed, device,
                 variables=None, temperature=1.0, amp=True):
    """Refine generated records by redrawing variables one at a time, each
    given all the others (the best-trained conditionals), for `sweeps`
    passes over `variables` (default: all). Returns a new int64 array."""
    V = model.n_vars
    variables = list(range(V)) if variables is None else list(variables)
    gen = torch.Generator(device=device)
    gen.manual_seed(seed + 7919)
    out = np.empty_like(tokens, dtype=np.int64)
    for start in range(0, tokens.shape[0], batch_size):
        tok = torch.as_tensor(tokens[start:start + batch_size], device=device).long()
        b = tok.shape[0]
        for _ in range(sweeps):
            for j in variables:
                hid = torch.zeros(b, V, dtype=torch.bool, device=device)
                hid[:, j] = True
                with torch.autocast("cuda", dtype=torch.bfloat16,
                                    enabled=amp and device.type == "cuda"):
                    logits = model(tok, hid)
                probs = torch.softmax(logits[:, j, :].float() / temperature, dim=-1)
                tok[:, j] = torch.multinomial(probs, 1, generator=gen).squeeze(1)
        out[start:start + b] = tok.cpu().numpy()
    return out


@torch.no_grad()
def sequential_nll(model, tokens, order, batch_size, device, amp=True):
    """Exact negative log-likelihood (nats per record) of records under the
    generation procedure: the sum over the generation order of
    -log p(x_j | variables earlier in the order). Returns per-record values."""
    V = model.n_vars
    res = []
    for start in range(0, tokens.shape[0], batch_size):
        tok = torch.as_tensor(tokens[start:start + batch_size], device=device).long()
        b = tok.shape[0]
        hid = torch.ones(b, V, dtype=torch.bool, device=device)
        tot = torch.zeros(b, device=device)
        for j in order:
            with torch.autocast("cuda", dtype=torch.bfloat16,
                                enabled=amp and device.type == "cuda"):
                logits = model(tok, hid)
            lp = F.log_softmax(logits[:, j, :].float(), dim=-1)
            tot -= lp.gather(1, tok[:, j:j + 1]).squeeze(1)
            hid[:, j] = False
        res.append(tot.cpu().numpy())
    return np.concatenate(res)
