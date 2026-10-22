"""generate.generate with performance options (experiment, 22 October 2026).

With every option off it makes the same draws as generate.generate; it also records seconds and peak
GPU memory by phase. Options (opts):
  heads        'all'    every output head at every step, as generate.py does ([records, slots, choices])
               'needed' only the head of the slot each record is drawing, plus in the SCF and CEX phases
                        the ruled holdings' heads (their chance of being nonzero, for the rules)
  pack         run the network only on the positions present in the current phase (the PUF phase has no
               SCF or CEX slots, the SCF view no PUF-only or CEX slots, the CEX view only its own); absent
               positions are invisible to the others, so this changes no result beyond rounding
  bf16         the encoder layers in bfloat16; the embeddings, final norm, heads and probabilities in 32-bit
  chunk        records per chunk, or 'auto': chosen from the GPU's free memory (mem_frac of it)
  empty_cache  return the GPU memory cache to the device after each chunk
  nomask       when no position in the network's input is absent (always so with pack), call the attention
               without a mask, which lets PyTorch use its memory-saving attention routine
"""
import copy
import math
import time

import torch

import model as M
import generate as G0
from generate import (NC, P, COMPUTED, FLAGS, PUF_SLOTS, SCF_SLOTS, CEX_SLOTS, CEX_YEAR, REP_SLOTS, RULES,
                      RULED, STATS, G, computed_band, scf_order, structural, apply_rules, to_table)

DEFAULT = dict(heads='all', pack=False, bf16=False, chunk=50_000, empty_cache=False, mem_frac=0.6, nomask=False)


class Timer:
    """Seconds and peak GPU memory (allocated by PyTorch) per phase, summed or maxed over chunks."""

    def __init__(self, device):
        self.device, self.sec, self.peak = device, {}, {}
        self.t = None

    def start(self):
        torch.cuda.synchronize()
        torch.cuda.reset_peak_memory_stats()
        self.t = time.perf_counter()

    def mark(self, phase):
        torch.cuda.synchronize()
        now = time.perf_counter()
        self.sec[phase] = self.sec.get(phase, 0.0) + now - self.t
        self.peak[phase] = max(self.peak.get(phase, 0), torch.cuda.max_memory_allocated())
        torch.cuda.reset_peak_memory_stats()
        self.t = time.perf_counter()


class Net:
    """The trained network's pieces, run on chosen positions and with chosen output heads."""

    def __init__(self, model, bf16, nomask=False):
        self.m, self.bf16, self.nomask = model, bf16, nomask
        self.enc = copy.deepcopy(model.encoder).to(torch.bfloat16) if bf16 else model.encoder

    def hidden(self, values, hidden, cols):
        """The final-norm output at the positions cols (all when None): [records, positions, width]."""
        ids, mask, emb = M.token_ids(values, hidden), values < 0, self.m.slot_embedding
        if cols is not None:
            ids, mask, emb = ids[:, cols], mask[:, cols], emb[cols]
        x = self.m.token_embedding(ids) + emb
        if self.bf16:
            x = x.to(torch.bfloat16)
        if self.nomask and not mask.any():                  # nothing absent: the attention needs no mask
            mask = None
        return self.m.final_norm(self.enc(x, src_key_padding_mask=mask).float())

    def all_probs(self, h, inv):
        """Every head present in h, as generate.py computes them: [records, slots, MAX_CHOICES]."""
        out = h.new_full((len(h), len(P), M.MAX_CHOICES), -math.inf)
        for j, head in enumerate(self.m.heads):
            pos = int(inv[NC + j])
            if pos >= 0:
                out[:, j, :head.out_features] = head(h[:, pos])
        return out.log_softmax(-1).exp()

    def slot_probs(self, h_at, j):
        """Per record, the distribution of predicted slot j[i] from its hidden vector h_at[i]: [records, MAX_CHOICES]."""
        out = h_at.new_full((len(j), M.MAX_CHOICES), -math.inf)
        for jj in torch.unique(j).tolist():
            sel = (j == jj).nonzero().squeeze(1)
            head = self.m.heads[jj]
            out[sel, :head.out_features] = head(h_at[sel])
        return out.log_softmax(-1).exp()


def view_columns(label):
    """The positions present in a phase's view (context included)."""
    keep = [0, 1] + [NC + j for j, v in enumerate(P) if label in M.PRESENT_ON[v]]
    return keep


def auto_chunk(net, o, n, device):
    """Records per chunk from free GPU memory: one probe step at 2,048 records in the current mode, on the
    widest view the mode uses, measured; plus 4 KB a record for the bookkeeping tensors."""
    probe = 2048
    width = max(len(view_columns(s)) for s in M.SOURCES) if o['pack'] else len(M.SLOTS)
    values = torch.full((probe, len(M.SLOTS)), -1, dtype=torch.long, device=device)
    values[:, :width] = 0
    values[:, 0], values[:, 1] = M.SOURCES.index('puf'), M.YEARS.index(G['year'])
    hidden = values[:, NC:] >= 0
    torch.cuda.synchronize()
    base = torch.cuda.memory_allocated()
    torch.cuda.reset_peak_memory_stats()
    cols = torch.arange(width, device=device) if o['pack'] else None
    h = net.hidden(values, hidden, cols)
    if o['heads'] == 'all':
        inv = torch.full((len(M.SLOTS),), -1, dtype=torch.long, device=device)
        inv[:width] = torch.arange(width, device=device)
        net.all_probs(h, inv).sum()
    else:
        j = torch.randint(0, width - NC, (probe,), device=device)
        net.slot_probs(h[torch.arange(probe, device=device), NC + j], j).sum()
        for v in RULED:
            net.slot_probs(h[:, 0], torch.full((probe,), P.index(v), device=device))
    torch.cuda.synchronize()
    per = (torch.cuda.max_memory_allocated() - base) / probe + 4096
    del h
    free, _ = torch.cuda.mem_get_info()
    free += torch.cuda.memory_reserved() - torch.cuda.memory_allocated()
    if o.get('gpu_bytes'):                                    # a smaller GPU, simulated
        free = min(free, o['gpu_bytes'] - torch.cuda.memory_allocated())
    chunk = int(o['mem_frac'] * free / per) // 1024 * 1024
    # PyTorch's attention with a padding mask fails (illegal memory access) once records x heads x positions^2
    # passes 2^31: keep below it
    heads = net.m.encoder.layers[0].self_attn.num_heads
    chunk = min(chunk, (2 ** 31 - 1) // (heads * width * width) // 1024 * 1024)
    return max(1024, min(n, chunk)), per


@torch.no_grad()
def generate(model, n, seed, preset=None, boost=None, factors=None, opts=None, timer=None, info=None):
    """As generate.generate (same arguments and table), with the options above."""
    o = {**DEFAULT, **(opts or {})}
    preset, factors = preset or {}, factors or {}
    device = next(model.parameters()).device
    net = Net(model, o['bf16'], o['nomask'])
    chunk = o['chunk']
    if chunk == 'auto':
        chunk, per = auto_chunk(net, o, n, device)
        if info is not None:
            info.update(chunk=chunk, bytes_per_record=per)
    timer = timer or Timer(device)
    cpu_gen = torch.Generator().manual_seed(seed)
    gpu_gen = torch.Generator(device=device).manual_seed(seed)
    own = torch.tensor([P.index(v) for v in PUF_SLOTS if v not in preset])
    scf = torch.tensor([P.index(v) for v in SCF_SLOTS])
    cex = torch.tensor([P.index(v) for v in CEX_SLOTS])
    ruled = torch.tensor([P.index(v) for v in RULED], device=device)
    n_own, n_scf = len(own), len(scf)
    identity = torch.arange(len(M.SLOTS), device=device)
    parts = []
    for start in range(0, n, chunk):
        timer.start()
        size = min(chunk, n - start)
        order = torch.cat([own[torch.rand(size, len(own), generator=cpu_gen).argsort(1)],
                           scf_order(size, cpu_gen),
                           cex[torch.rand(size, len(cex), generator=cpu_gen).argsort(1)]], 1).to(device)
        values = torch.full((size, len(M.SLOTS)), -1, dtype=torch.long, device=device)
        values[:, 0] = M.SOURCES.index('puf')
        values[:, 1] = M.YEARS.index(G['year'])
        rows = torch.arange(size, device=device)
        values[rows[:, None], NC + order] = 0
        values[:, NC + scf.to(device)] = -1
        values[:, NC + cex.to(device)] = -1
        for tot in list(COMPUTED) + list(FLAGS):
            values[:, NC + P.index(tot)] = 0
        filled = torch.zeros(size, len(P), dtype=torch.bool, device=device)
        for var, bands in preset.items():
            values[:, NC + P.index(var)] = bands[start:start + size].to(device)
            filled[:, P.index(var)] = True
        triggered = torch.zeros(size, len(RULES), dtype=torch.bool, device=device)
        resolved, must = triggered.clone(), torch.zeros(size, len(P), dtype=torch.bool, device=device)
        own_chance = torch.zeros(size, len(RULED), dtype=torch.float64, device=device)
        forced = torch.zeros(size, len(RULED), dtype=torch.bool, device=device)
        p_drawn = torch.full((size,), float('nan'), dtype=torch.float64, device=device)
        q_drawn = p_drawn.clone()
        cols_of = {}                                           # per phase: present positions and their inverse

        def columns(view, label):
            if not o['pack']:
                return None, identity
            if label not in cols_of:
                cols = torch.nonzero((view >= 0).any(0)).squeeze(1)
                inv = torch.full((len(M.SLOTS),), -1, dtype=torch.long, device=device)
                inv[cols] = torch.arange(len(cols), device=device)
                cols_of[label] = (cols, inv)
            return cols_of[label]

        timer.mark('setup')
        for step in range(order.shape[1]):
            slot = order[:, step]
            if step == n_own:
                timer.mark('puf')
                for tot in COMPUTED:
                    values[:, NC + P.index(tot)] = computed_band(values, gpu_gen, tot)
                    filled[:, P.index(tot)] = True
                for flag, flag_parts in FLAGS.items():
                    values[:, NC + P.index(flag)] = torch.stack(
                        [values[:, NC + P.index(v)] != 0 for v in flag_parts], 1).any(1).long()
                    filled[:, P.index(flag)] = True
                for r, rule in enumerate(RULES):
                    triggered[:, r] = torch.stack([values[:, NC + P.index(v)] != 0
                                                   for v in rule['when_any_nonzero']], 1).any(1)
                    STATS['rule'][rule['name']] += int(triggered[:, r].sum())
                values[:, NC + scf.to(device)] = 0
                values[:, NC + cex.to(device)] = 0
                values[:, NC + REP_SLOTS.to(device)] = 0
                timer.mark('totals')
            if step == n_own + n_scf:
                timer.mark('scf')
            hidden = (values[:, NC:] >= 0) & ~filled
            if step >= n_own:
                label, year = ('scf', None) if step < n_own + n_scf else ('cex', CEX_YEAR)
                view, view_hidden = M.seen_as(values, hidden, label, year)
                cols, inv = columns(view, label)
                h = net.hidden(view, view_hidden, cols)
                if o['heads'] == 'all':
                    all_probs = net.all_probs(h, inv)
                    slot_p = all_probs[rows, slot]
                    own_nonzero = 1 - all_probs[:, :, 0]
                else:
                    slot_p = net.slot_probs(h[rows, inv[NC + slot]], slot)
                    own_nonzero = torch.zeros(size, len(P), device=device)
                    for v in (RULED if label == 'scf' else []):   # no rule applies in the CEX phase
                        j = P.index(v)
                        own_nonzero[:, j] = 1 - net.slot_probs(h[:, inv[NC + j]], torch.full_like(slot, j))[:, 0]
                del h
                own_probs = structural(values, slot, slot_p.clone())
                scaled = slot_p.clone()
                for v, f in factors.items():                  # generate.scale_odds, on each record's own slot
                    if f != 1.0:
                        s = scaled.clone()
                        s[:, 1:] *= f
                        s /= s.sum(-1, keepdim=True)
                        scaled = torch.where((slot == P.index(v))[:, None], s, scaled)
                probs, ok = apply_rules(slot, filled, own_nonzero, structural(values, slot, scaled),
                                        triggered, resolved, must, gpu_gen)
                k = (slot[:, None] == ruled[None, :])
                hit = k.any(1)
                col = k.int().argmax(1)
                own_chance[rows[hit], col[hit]] = 1 - own_probs[hit, 0].double()
                forced[rows[hit], col[hit]] = ok[hit]
            else:
                cols, inv = columns(values, 'puf')
                h = net.hidden(values, hidden, cols)
                if o['heads'] == 'all':
                    probs = net.all_probs(h, inv)[rows, slot]
                else:
                    probs = net.slot_probs(h[rows, inv[NC + slot]], slot)
                del h
            draw_from, boosted = probs, None
            if boost is not None:
                var, bands, factor = boost
                boosted = slot == P.index(var)
                q = probs.clone()
                q[:, torch.as_tensor(bands, device=device)] *= factor
                draw_from = torch.where(boosted[:, None], q / q.sum(1, keepdim=True), probs)
            draw = torch.multinomial(draw_from, 1, generator=gpu_gen).squeeze(1)
            if boosted is not None and boosted.any():
                p_drawn[boosted] = probs[rows, draw][boosted].double()
                q_drawn[boosted] = draw_from[rows, draw][boosted].double()
            values[rows, NC + slot] = draw
            filled[rows, slot] = True
        timer.mark('cex')
        parts.append((values.cpu(), p_drawn.cpu(), q_drawn.cpu(), own_chance.cpu(), forced.cpu()))
        del values, filled, must, triggered, resolved, order
        if o['empty_cache']:
            torch.cuda.empty_cache()
        timer.mark('copy out')
    values = torch.cat([p[0] for p in parts])
    table = to_table(values)
    if boost is not None:
        table['p_band'] = torch.cat([p[1] for p in parts]).numpy()
        table['q_band'] = torch.cat([p[2] for p in parts]).numpy()
    oc, fc = torch.cat([p[3] for p in parts]).numpy(), torch.cat([p[4] for p in parts]).numpy()
    for i, v in enumerate(RULED):
        table[f'_own_{v}'], table[f'_forced_{v}'] = oc[:, i], fc[:, i]
    if info is not None:
        info['chunk'] = chunk
    return table
