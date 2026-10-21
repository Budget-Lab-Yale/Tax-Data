"""Generation: PUF-style records drawn from the trained model.

One record is drawn slot by slot:
  1. every PUF variable (married, dependents, the filing flag and every
     amount the PUF has) in a random order per record, each from the model
     given the slots drawn so far; the SCF-only slots are absent meanwhile,
     as on a real PUF record;
  2. the computed totals: line-13 capital gains from its four pieces, and
     business income and its two subtotals (Schedules C and F, Schedule E)
     from its eighteen parts. Each part gets a dollar value inside its band
     (one of 32 equally spaced weighted quantile points of the PUF records in
     that band), the signed sum is placed in the total's band on the PUF's
     cut points. Then the computed flag k1_owner: any partnership or
     S corporation part nonzero;
  3. the SCF-only slots (the balance sheet, the unrealized gains and
     owns_business) in a random order per record, except that the home
     mortgage, carried as a share of the home's value, comes after the home.
     Each is drawn under the SCF label with the PUF-only slots left out, as
     the SCF would see the household.

Consistency rules (RULES). When a PUF item on the record implies a holding,
the holding cannot be drawn as zero. For a rule met by any one of several
holdings ("at least one of"), the one that must be nonzero is chosen when the
first of them comes up, in proportion to the model's own chance that each is
nonzero; the others are drawn freely. Ruling out zero means setting the
model's chance of zero to nothing and rescaling the other bands; the model
still decides the size. Also: with no home, the mortgage share is zero.

The file is a stack of four parts of 100,000 records. Each part oversamples
the top 1% of one variable: that variable is drawn first, from the model
with nothing else known, with the bands covering the top 1% of its positive
weight drawn ten times as often (for an SCF-only slot: drawn in its usual
place, from the boosted conditional); each record's weight p / q undoes the
boost exactly. Each part's weights are scaled to sum to 100,000.

Writes common.GEN/stack.parquet: one row per record, the band of every slot,
weight, stratifier (which part), p_band and q_band.
"""
import json
import os

import numpy as np
import pandas as pd
import torch

import common
import model as M

G = common.SETTINGS['generation']
NC = len(M.CONTEXT)
P = M.PREDICTED
COMPUTED = M.COMPUTED                     # {total: {part: sign}}
FLAGS = M.COMPUTED_FLAGS                  # {flag: parts}
PUF_SLOTS = [v for v in P if M.OWNED_BY.get(v, 'puf') == 'puf' and v not in COMPUTED and v not in FLAGS]
SCF_SLOTS = M.SCF_SLOTS
K1_PARTS = FLAGS['k1_owner']
# rule: (what triggers it, a test on the record's PUF bands, the holdings of which at least one must be nonzero)
RULES = [
    ('rental income or loss', lambda b: (b('rent_inc') != 0) | (b('rent_loss') != 0), ['w_other_home', 'w_re_fund']),
    ('a partnership or S corporation item', lambda b: torch.stack([b(v) != 0 for v in K1_PARTS], 1).any(1),
     ['owns_business', 'w_other_home', 'w_re_fund']),
    ('mortgage interest paid: a home', lambda b: b('mortgage_interest') != 0, ['w_primary_home']),
    ('mortgage interest paid: a mortgage', lambda b: b('mortgage_interest') != 0, ['w_primary_ltv']),
    ('real estate tax paid', lambda b: b('real_estate_tax') != 0, ['w_primary_home', 'w_other_home'])]
RULE_COUNTS = {name: 0 for name, _, _ in RULES}           # records each rule applies to
FORCED = {}                                               # per holding: draws with zero ruled out
POINTS = 32


# ---- computed totals ---------------------------------------------------------
_points = {}


def part_points(total, device):
    """[bands, 32] dollar points per band of each part of a total, and the total's cut points."""
    if total in _points:
        return _points[total]
    parts = list(COMPUTED[total])
    t = pd.read_parquet(common.TABLE, columns=['source', 'weight'] + parts + [v + '_band' for v in parts])
    t = t[t.source == 'puf']
    w = t['weight'].to_numpy(float)
    out = {}
    for v in parts:
        x, b = t[v].to_numpy(float), t[v + '_band'].to_numpy()
        pts = np.zeros((M.N_CHOICES[v], POINTS))
        for k in range(M.N_CHOICES[v]):
            sel = b == k
            if sel.any():
                o = np.argsort(x[sel], kind='stable')
                xs, F = x[sel][o], np.cumsum(w[sel][o]) / w[sel].sum()
                pts[k] = xs[np.minimum(np.searchsorted(F, (np.arange(POINTS) + 0.5) / POINTS), len(xs) - 1)]
        out[v] = torch.tensor(pts, dtype=torch.float64, device=device)
    e = json.load(open(common.EDGES))[f'puf/2015/{total}']
    out['neg'] = torch.tensor(e['negative']['cuts'], dtype=torch.float64, device=device)
    out['pos'] = torch.tensor(e['positive']['cuts'], dtype=torch.float64, device=device)
    _points[total] = out
    return out


def computed_band(values, gen, total):
    """The band of a computed total from its parts' bands."""
    pts = part_points(total, values.device)
    s = torch.zeros(len(values), dtype=torch.float64, device=values.device)
    for v, sign in COMPUTED[total].items():
        b = values[:, NC + P.index(v)]
        assert (b >= 0).all(), f'{v} not drawn before {total}'
        k = torch.randint(0, POINTS, (len(values),), generator=gen, device=values.device)
        s += float(sign) * pts[v][b, k]
    band = torch.zeros(len(values), dtype=torch.long, device=values.device)
    neg, pos = s < 0, s > 0
    band[neg] = 1 + torch.searchsorted(pts['neg'], s[neg].contiguous(), right=False)
    band[pos] = M.BAND_LAYOUT[total]['first_positive'] + torch.searchsorted(pts['pos'], s[pos].contiguous(),
                                                                             right=False)
    return band


# ---- one draw ------------------------------------------------------------------
def scf_probs(model, values, hidden):
    """Every slot's distribution as the SCF would see the record: SCF label, PUF-only slots absent."""
    view, view_hidden = values.clone(), hidden.clone()
    view[:, 0] = M.SOURCES.index('scf')
    for other, owner in M.OWNED_BY.items():
        if owner != 'scf':
            j = P.index(other)
            view[:, NC + j], view_hidden[:, j] = -1, False
    return model(view, view_hidden).exp()


def scf_order(size, gen):
    """A random order of the SCF-only slots per record, each ratio slot moved after its base."""
    scf = torch.tensor([P.index(v) for v in SCF_SLOTS])
    perm = scf[torch.rand(size, len(scf), generator=gen).argsort(1)]
    rows = torch.arange(size)
    for slot, (_, base) in M.RATIOS.items():
        i, ib = P.index(slot), P.index(base)
        pos, pos_b = (perm == i).int().argmax(1), (perm == ib).int().argmax(1)
        swap = pos < pos_b
        perm[rows[swap], pos[swap]], perm[rows[swap], pos_b[swap]] = ib, i
    return perm


def apply_rules(values, filled, slot, all_probs, triggered, resolved, must, gen):
    """The current slot's distribution under the consistency rules (see the top)."""
    rows = torch.arange(len(values), device=values.device)
    nonzero = 1 - all_probs[:, :, 0]
    for r, (name, _, group) in enumerate(RULES):
        g = torch.tensor([P.index(v) for v in group], device=values.device)
        first = triggered[:, r] & ~resolved[:, r] & torch.isin(slot, g)
        if not first.any():
            continue
        resolved[:, r] |= first
        weight = (nonzero[:, g] * ~filled[:, g]).double().clamp_min(0) + 1e-12 * (~filled[:, g])
        pick = torch.multinomial(weight[first], 1, generator=gen).squeeze(1)
        must[rows[first], g[pick]] = True
    probs = all_probs[rows, slot].clone()
    for slot_name, (_, base) in M.RATIOS.items():           # no home: no mortgage share
        no_base = (slot == P.index(slot_name)) & (values[:, NC + P.index(base)] == 0)
        probs[no_base] = 0.0
        probs[no_base, 0] = 1.0
    force = must[rows, slot]
    q = probs.clone()
    q[:, 0] = 0
    ok = force & (q.sum(1) > 1e-12)
    for j in slot[ok].unique().tolist():                     # per holding: draws forced, and how many the
        m = ok & (slot == j)                                 # model alone would have drawn as zero
        c = FORCED.setdefault(P[j], {'draws with zero ruled out': 0, 'of which zero without the rule': 0.0})
        c['draws with zero ruled out'] += int(m.sum())
        c['of which zero without the rule'] += float(probs[m, 0].sum())
    return torch.where(ok[:, None], q / q.sum(1, keepdim=True).clamp_min(1e-30), probs)


@torch.no_grad()
def generate(model, n, seed, preset=None, boost=None, chunk=50_000):
    """n PUF-style records. preset: {variable: n bands} set before anything is drawn.
    boost: (variable, bands, factor), drawn from q (p with those bands multiplied) at its
    turn; that draw's p and q are returned as p_band, q_band."""
    preset = preset or {}
    device = next(model.parameters()).device
    cpu_gen = torch.Generator().manual_seed(seed)                  # the orders
    gpu_gen = torch.Generator(device=device).manual_seed(seed)     # the draws
    own = torch.tensor([P.index(v) for v in PUF_SLOTS if v not in preset])
    scf = torch.tensor([P.index(v) for v in SCF_SLOTS])
    n_own = len(own)
    parts = []
    for start in range(0, n, chunk):
        size = min(chunk, n - start)
        order = torch.cat([own[torch.rand(size, len(own), generator=cpu_gen).argsort(1)],
                           scf_order(size, cpu_gen)], 1).to(device)
        values = torch.full((size, len(M.SLOTS)), -1, dtype=torch.long, device=device)
        values[:, 0] = M.SOURCES.index('puf')
        values[:, 1] = M.YEARS.index(G['year'])
        rows = torch.arange(size, device=device)
        values[rows[:, None], NC + order] = 0                  # present, not yet drawn
        values[:, NC + scf.to(device)] = -1                     # absent until their turn
        for tot in list(COMPUTED) + list(FLAGS):
            values[:, NC + P.index(tot)] = 0                    # present and hidden until computed
        filled = torch.zeros(size, len(P), dtype=torch.bool, device=device)
        for var, bands in preset.items():
            values[:, NC + P.index(var)] = bands[start:start + size].to(device)
            filled[:, P.index(var)] = True
        triggered = torch.zeros(size, len(RULES), dtype=torch.bool, device=device)
        resolved, must = triggered.clone(), torch.zeros(size, len(P), dtype=torch.bool, device=device)
        p_drawn = torch.full((size,), float('nan'), dtype=torch.float64, device=device)
        q_drawn = p_drawn.clone()
        for step in range(order.shape[1]):
            slot = order[:, step]
            if step == n_own:                                  # the SCF slots' turn: compute the totals first
                for tot in COMPUTED:
                    values[:, NC + P.index(tot)] = computed_band(values, gpu_gen, tot)
                    filled[:, P.index(tot)] = True
                for flag, flag_parts in FLAGS.items():
                    values[:, NC + P.index(flag)] = torch.stack(
                        [values[:, NC + P.index(v)] != 0 for v in flag_parts], 1).any(1).long()
                    filled[:, P.index(flag)] = True
                band = lambda v: values[:, NC + P.index(v)]
                for r, (name, test, _) in enumerate(RULES):
                    triggered[:, r] = test(band)
                    RULE_COUNTS[name] += int(triggered[:, r].sum())
                values[:, NC + scf.to(device)] = 0
            hidden = (values[:, NC:] >= 0) & ~filled
            if step >= n_own:
                probs = apply_rules(values, filled, slot, scf_probs(model, values, hidden),
                                    triggered, resolved, must, gpu_gen)
            else:
                probs = model(values, hidden)[rows, slot].exp()
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
        parts.append((values.cpu(), p_drawn.cpu(), q_drawn.cpu()))
    table = to_table(torch.cat([v for v, _, _ in parts]))
    if boost is not None:
        table['p_band'] = torch.cat([p for _, p, _ in parts]).numpy()
        table['q_band'] = torch.cat([q for _, _, q in parts]).numpy()
    return table


def to_table(values):
    v = values.numpy()
    out = pd.DataFrame({'source': [M.SOURCES[i] for i in v[:, 0]], 'year': [M.YEARS[i] for i in v[:, 1]]})
    for j, var in enumerate(P):
        out[M.column(var)] = v[:, NC + j]
    out['weight'] = 1.0
    return out


# ---- the stack -----------------------------------------------------------------
@torch.no_grad()
def nothing_known(model, var):
    """var's distribution for a PUF record with every PUF slot hidden and no SCF-only slot."""
    device = next(model.parameters()).device
    values = torch.full((1, len(M.SLOTS)), -1, dtype=torch.long, device=device)
    values[0, 0], values[0, 1] = M.SOURCES.index('puf'), M.YEARS.index(G['year'])
    for j, v in enumerate(P):
        if M.OWNED_BY.get(v, 'puf') == 'puf':
            values[0, NC + j] = 0
    return model(values, values[:, NC:] >= 0)[0, P.index(var)].exp().double().cpu().numpy()[:M.N_CHOICES[var]]


def build_stack(model):
    n = G['records_per_file']
    parts = []
    for var, seed in G['files'].items():
        top = M.positive_bands(var, 1 - G['top_share'], 1.0)
        if M.OWNED_BY.get(var) == 'scf':
            part = generate(model, n, seed, boost=(var, top, G['boost']))
            part['weight'] = part['p_band'] / part['q_band']
        else:
            p = nothing_known(model, var)
            q = p.copy()
            q[top] *= G['boost']
            q /= q.sum()
            bands = torch.multinomial(torch.tensor(q), n, replacement=True,
                                      generator=torch.Generator().manual_seed(seed))
            part = generate(model, n, seed, preset={var: bands})
            b = bands.numpy()
            part['weight'], part['p_band'], part['q_band'] = p[b] / q[b], p[b], q[b]
        part['weight'] *= n / part['weight'].sum()
        part['stratifier'] = var
        parts.append(part)
        common.say(var, 'done')
    stack = pd.concat(parts, ignore_index=True)
    stack['record_id'] = np.arange(len(stack))
    common.say('consistency rules: records each applies to, over the whole stack:')
    common.say(pd.Series(RULE_COUNTS).to_string())
    common.say('draws with zero ruled out, by holding:')
    common.say(pd.DataFrame(FORCED).T.to_string())
    return stack


def main():
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(common.SEED)
    model = M.load_model(device, os.path.join(common.MODEL, 'model.pt'))
    os.makedirs(common.GEN, exist_ok=True)
    build_stack(model).to_parquet(os.path.join(common.GEN, 'stack.parquet'), index=False)


if __name__ == '__main__':
    main()
