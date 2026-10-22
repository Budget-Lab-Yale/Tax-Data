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
  (On a dependent filer, every SCF-only and CEX-only slot is zero: see structural.)
  3. the SCF-only slots (the balance sheet, the unrealized gains and
     owns_business) in a random order per record, except that the home
     mortgage, carried as a share of the home's value, comes after the home.
     Each is drawn under the SCF label with the PUF-only slots left out, as
     the SCF would see the household;
  4. the CEX-only slots (the 8 consumption categories) in a random order per
     record, each under the CEX label and year (settings "cex",
     generation_year) with every slot the CEX lacks left out: given married,
     dependents, the age range, wages, gross Social Security, self-employment
     income and the categories drawn so far.
Each step runs the network only on the slots present in its phase and
computes only the output layers it needs.

Consistency rules (settings.json, "consistency"). Each rule: when any of
its items is nonzero on the record, at least one of its holdings must be
nonzero. The one that must be nonzero is chosen when the first of them comes
up, in proportion to the model's own chance that each is nonzero (before the
offset's factors, below, so the choice does not move with them); the others are drawn
freely. Ruling out zero sets the chance of zero to nothing and rescales the
other bands; the model still decides the size. Also: with no home, the
mortgage share is zero. No rule applies to a dependent filer: it holds no
survey item at all.

The offset. The rules add holders and take none away, so each ruled holding
ends with more holders than the model alone would draw. Generation therefore
runs more than once ("offset_generations"). After each run, for each ruled
holding, one factor on the odds of holding it is found such that

  forced holders + expected holders among the rest = the model's own expected holders

(all weighted; a record's own expected count is the model's chance that the
holding is nonzero at its turn, before rules and factors). The next run
multiplies every record's odds of holding by that factor before the rules
apply; on a forced record the rule overrides it, so in effect only the
untouched records are scaled. The last run is the one kept. Items drawn
later condition on earlier ones, so a run's own figures are reported and
written to consistency.csv: what is left of the excess after the last run.

The file is a stack of four parts of 100,000 records. Each part oversamples
the top 1% of one variable: that variable is drawn first, from the model
with nothing else known, with the bands covering the top 1% of its positive
weight drawn ten times as often (for an SCF-only slot: drawn in its usual
place, from the boosted conditional); each record's weight p / q undoes the
boost exactly. Each part's weights are scaled to sum to 100,000.

Writes common.GEN/stack.parquet (one row per record: the band of every slot,
weight, stratifier, p_band, q_band) and common.GEN/consistency.csv (per run
and ruled holding: the model's own expected holders, the holders with the
rules, the factor used and the next one).
"""
import os

import numpy as np
import pandas as pd
import torch

import common
import model as M

G = common.SETTINGS['generation']
C = common.SETTINGS['consistency']
NC = len(M.CONTEXT)
P = M.PREDICTED
COMPUTED = M.COMPUTED                     # {total: {part: sign}}
FLAGS = M.COMPUTED_FLAGS                  # {flag: parts}
PUF_SLOTS = [v for v in P if 'puf' in M.PRESENT_ON[v] and v not in COMPUTED and v not in FLAGS]
SCF_SLOTS = M.SCF_SLOTS
CEX_SLOTS = M.CEX_SLOTS
CEX_YEAR = common.SETTINGS['cex']['generation_year']
REP_SLOTS = torch.tensor([P.index(v) for v in M.REPORTED])
RULES = C['rules']
for _r in RULES:
    assert all(M.OWNED_BY.get(v, 'puf') != 'scf' for v in _r['when_any_nonzero']), _r['name']
    assert all(v in SCF_SLOTS for v in _r['then_at_least_one_nonzero']), _r['name']
RULE_ITEMS = [torch.tensor([P.index(v) for v in r['when_any_nonzero']]) for r in RULES]
RULE_HOLDINGS = [torch.tensor([P.index(v) for v in r['then_at_least_one_nonzero']]) for r in RULES]
RULED = sorted({v for r in RULES for v in r['then_at_least_one_nonzero']}, key=P.index)   # the offset's holdings
DEPENDENT_RANGES = torch.tensor(common.DEPENDENT_RANGES)
SURVEY_ONLY = torch.tensor([P.index(v) for v in SCF_SLOTS + CEX_SLOTS])
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
    e = common.edges()[f'puf/{common.PUF_YEAR}/{total}']
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


# ---- rules on a draw -------------------------------------------------------------
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


def dependent(values):
    """Records whose primary filer is in a dependent-filer age range."""
    return torch.isin(values[:, NC + P.index('age_range')], DEPENDENT_RANGES.to(values.device))


def structural(values, slot, probs):
    """A ratio slot whose base is zero is zero (no home: no mortgage share). A dependent filer holds no survey-only
    item (balance sheet, unrealized gains, business ownership, consumption): the surveys count its wealth and
    spending in the household that claims it, so on its own return every one is zero."""
    for slot_name, (_, base) in M.RATIOS.items():
        no_base = (slot == P.index(slot_name)) & (values[:, NC + P.index(base)] == 0)
        probs[no_base] = 0.0
        probs[no_base, 0] = 1.0
    dep = dependent(values) & torch.isin(slot, SURVEY_ONLY.to(values.device))
    probs[dep] = 0.0
    probs[dep, 0] = 1.0
    return probs


def scale_odds(probs, slot, factors):
    """On the records drawing a ruled holding, its odds of being nonzero multiplied by its factor."""
    for v, f in factors.items():
        if f != 1.0:
            s = probs.clone()
            s[:, 1:] *= f
            s /= s.sum(-1, keepdim=True)
            probs = torch.where((slot == P.index(v))[:, None], s, probs)
    return probs


def apply_rules(slot, filled, nonzero, probs, triggered, resolved, must, gen):
    """The current slot's distribution under the consistency rules (see the top), and which rows were forced.
    nonzero: every slot's chance of being nonzero, the model's own (before the offset's factors), so which
    holding a rule forces does not depend on the factors."""
    rows = torch.arange(len(slot), device=slot.device)
    for r in range(len(RULES)):
        g = RULE_HOLDINGS[r].to(slot.device)
        first = triggered[:, r] & ~resolved[:, r] & torch.isin(slot, g)
        if not first.any():
            continue
        resolved[:, r] |= first
        weight = (nonzero[:, g] * ~filled[:, g]).double().clamp_min(0) + 1e-12 * (~filled[:, g])
        pick = torch.multinomial(weight[first], 1, generator=gen).squeeze(1)
        must[rows[first], g[pick]] = True
    q = probs.clone()
    q[:, 0] = 0
    ok = must[rows, slot] & (q.sum(1) > 1e-12)
    return torch.where(ok[:, None], q / q.sum(1, keepdim=True).clamp_min(1e-30), probs), ok


# ---- one chunk of records ----------------------------------------------------------
class Chunk:
    """size records being drawn: every slot's band (-1 absent), which slots are drawn, and the rules' and the
    boost's bookkeeping. Each step runs the network only on the slots present in its phase (model.SOURCE_SLOTS:
    the PUF's, then the SCF's view, then the CEX's view) and computes only the output layers it needs."""

    def __init__(self, model, size, order, preset, start, factors, boost, gen):
        device = order.device
        self.model, self.order, self.factors, self.boost, self.gen = model, order, factors, boost, gen
        self.rows = torch.arange(size, device=device)
        self.values = torch.full((size, len(M.SLOTS)), -1, dtype=torch.long, device=device)
        self.values[:, 0] = M.SOURCES.index('puf')
        self.values[:, 1] = M.YEARS.index(common.PUF_YEAR)
        self.values[self.rows[:, None], NC + order] = 0              # present, not yet drawn
        for v in SCF_SLOTS + CEX_SLOTS:
            self.values[:, NC + P.index(v)] = -1                      # absent until their turn
        for tot in list(COMPUTED) + list(FLAGS):
            self.values[:, NC + P.index(tot)] = 0                    # present and hidden until computed
        self.filled = torch.zeros(size, len(P), dtype=torch.bool, device=device)
        for var, bands in preset.items():
            self.values[:, NC + P.index(var)] = bands[start:start + size].to(device)
            self.filled[:, P.index(var)] = True
        self.triggered = torch.zeros(size, len(RULES), dtype=torch.bool, device=device)
        self.resolved = self.triggered.clone()
        self.must = torch.zeros(size, len(P), dtype=torch.bool, device=device)
        self.own_chance = torch.zeros(size, len(RULED), dtype=torch.float64, device=device)
        self.forced = torch.zeros(size, len(RULED), dtype=torch.bool, device=device)
        self.p_drawn = torch.full((size,), float('nan'), dtype=torch.float64, device=device)
        self.q_drawn = self.p_drawn.clone()
        self.ruled = torch.tensor([P.index(v) for v in RULED], device=device)
        self.column = {}                                             # per source: each of SLOTS' column, -1 if absent
        for src, slots in M.SOURCE_SLOTS.items():
            self.column[src] = torch.full((len(M.SLOTS),), -1, dtype=torch.long, device=device)
            self.column[src][list(slots)] = torch.arange(len(slots), device=device)

    def last_layer(self, values, hidden, src):
        """The network on the source's slots, all of which must be present."""
        slots = M.SOURCE_SLOTS[src]
        assert (values[:, M.slot_tensor(slots, values.device)] >= 0).all(), f'a {src} slot is absent'
        return self.model.last_layer(*M.pack(values, hidden, slots), slots)

    def take(self, probs, slot):
        """Draw slot from probs (boosted at the boost's variable), and record it."""
        draw_from, boosted = probs, None
        if self.boost is not None:
            var, bands, factor = self.boost
            boosted = slot == P.index(var)
            q = probs.clone()
            q[:, torch.as_tensor(bands, device=probs.device)] *= factor
            draw_from = torch.where(boosted[:, None], q / q.sum(1, keepdim=True), probs)
        draw = torch.multinomial(draw_from, 1, generator=self.gen).squeeze(1)
        if boosted is not None and boosted.any():
            self.p_drawn[boosted] = probs[self.rows, draw][boosted].double()
            self.q_drawn[boosted] = draw_from[self.rows, draw][boosted].double()
        self.values[self.rows, NC + slot] = draw
        self.filled[self.rows, slot] = True

    def puf_step(self, slot):
        """One PUF variable per record, given the PUF slots drawn so far."""
        hidden = (self.values[:, NC:] >= 0) & ~self.filled
        h = self.last_layer(self.values, hidden, 'puf')
        self.take(self.model.slot_probs(h[self.rows, self.column['puf'][NC + slot]], slot), slot)

    def compute_totals(self):
        """The computed totals and flags from their parts; which rules each record triggers (none for a
        dependent filer, which holds no survey item); the survey slots made present. Returns the rule counts."""
        v = self.values
        for tot in COMPUTED:
            v[:, NC + P.index(tot)] = computed_band(v, self.gen, tot)
            self.filled[:, P.index(tot)] = True
        for flag, flag_parts in FLAGS.items():
            v[:, NC + P.index(flag)] = torch.stack([v[:, NC + P.index(p)] != 0 for p in flag_parts], 1).any(1).long()
            self.filled[:, P.index(flag)] = True
        dep = dependent(v)
        for r in range(len(RULES)):
            self.triggered[:, r] = (v[:, NC + RULE_ITEMS[r].to(v.device)] != 0).any(1) & ~dep
        for s in SCF_SLOTS + CEX_SLOTS:
            v[:, NC + P.index(s)] = 0                                # each survey's view leaves out the other's
        v[:, NC + REP_SLOTS.to(v.device)] = 0                        # reported slots: present, never drawn (hidden)
        return {r['name']: int(self.triggered[:, k].sum()) for k, r in enumerate(RULES)}

    def survey_step(self, slot, label, year=None):
        """One survey-only slot per record, as the survey label would see the record, under the rules."""
        hidden = (self.values[:, NC:] >= 0) & ~self.filled
        view, view_hidden = M.seen_as(self.values, hidden, label, year)
        h, col = self.last_layer(view, view_hidden, label), self.column[label]
        slot_p = self.model.slot_probs(h[self.rows, col[NC + slot]], slot)
        # each ruled holding's chance of being nonzero, the model's own (no rule applies in the CEX phase)
        own_nonzero = torch.zeros(len(slot), len(P), device=slot.device)
        for v in (RULED if label == 'scf' else []):
            j = P.index(v)
            own_nonzero[:, j] = 1 - self.model.slot_probs(h[:, col[NC + j]], torch.full_like(slot, j))[:, 0]
        del h
        own_probs = structural(self.values, slot, slot_p.clone())   # the model's own, before rules
        probs, ok = apply_rules(slot, self.filled, own_nonzero,
                                structural(self.values, slot, scale_odds(slot_p.clone(), slot, self.factors)),
                                self.triggered, self.resolved, self.must, self.gen)
        k = (slot[:, None] == self.ruled[None, :])                  # rows whose slot is a ruled holding
        hit = k.any(1)
        col = k.int().argmax(1)
        self.own_chance[self.rows[hit], col[hit]] = 1 - own_probs[hit, 0].double()
        self.forced[self.rows[hit], col[hit]] = ok[hit]
        self.take(probs, slot)


@torch.no_grad()
def generate(model, n, seed, preset=None, boost=None, factors=None, chunk=50_000):
    """n PUF-style records, and how many records each consistency rule applies to. preset: {variable: n bands} set
    before anything is drawn. boost: (variable, bands, factor), drawn from q (p with those bands multiplied) at
    its turn; that draw's p and q are returned as p_band, q_band. factors: {ruled holding: factor on its odds of
    being nonzero}. For each ruled holding the table also carries the model's own chance it is nonzero
    (_own_<v>) and whether a rule forced it (_forced_<v>)."""
    preset, factors = preset or {}, factors or {}
    device = next(model.parameters()).device
    cpu_gen = torch.Generator().manual_seed(seed)                  # the orders
    gpu_gen = torch.Generator(device=device).manual_seed(seed)     # the draws
    own = torch.tensor([P.index(v) for v in PUF_SLOTS if v not in preset])
    cex = torch.tensor([P.index(v) for v in CEX_SLOTS])
    n_own, n_scf = len(own), len(SCF_SLOTS)
    rule_counts = {r['name']: 0 for r in RULES}
    parts = []
    for start in range(0, n, chunk):
        size = min(chunk, n - start)
        order = torch.cat([own[torch.rand(size, len(own), generator=cpu_gen).argsort(1)],
                           scf_order(size, cpu_gen),
                           cex[torch.rand(size, len(cex), generator=cpu_gen).argsort(1)]], 1).to(device)
        d = Chunk(model, size, order, preset, start, factors, boost, gpu_gen)
        for step in range(n_own):
            d.puf_step(order[:, step])
        for name, count in d.compute_totals().items():
            rule_counts[name] += count
        for step in range(n_own, n_own + n_scf):
            d.survey_step(order[:, step], 'scf')
        for step in range(n_own + n_scf, order.shape[1]):
            d.survey_step(order[:, step], 'cex', CEX_YEAR)
        parts.append((d.values.cpu(), d.p_drawn.cpu(), d.q_drawn.cpu(), d.own_chance.cpu(), d.forced.cpu()))
    table = to_table(torch.cat([p[0] for p in parts]))
    if boost is not None:
        table['p_band'] = torch.cat([p[1] for p in parts]).numpy()
        table['q_band'] = torch.cat([p[2] for p in parts]).numpy()
    oc, fc = torch.cat([p[3] for p in parts]).numpy(), torch.cat([p[4] for p in parts]).numpy()
    for i, v in enumerate(RULED):
        table[f'_own_{v}'], table[f'_forced_{v}'] = oc[:, i], fc[:, i]
    return table, rule_counts


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
    values[0, 0], values[0, 1] = M.SOURCES.index('puf'), M.YEARS.index(common.PUF_YEAR)
    for j, v in enumerate(P):
        if 'puf' in M.PRESENT_ON[v]:
            values[0, NC + j] = 0
    lp = M.log_probs_as(model, values, values[:, NC:] >= 0, 'puf')
    return lp(P.index(var))[0].exp().double().cpu().numpy()[:M.N_CHOICES[var]]


def build_stack(model, factors):
    """The four strata (settings "generation"), stacked; and how many records each rule applied to."""
    n = G['records_per_file']
    parts, rule_counts = [], {r['name']: 0 for r in RULES}
    for var, stratum in G['strata'].items():
        seed = stratum['seed']
        top = M.positive_bands(var, 1 - G['top_share'], 1.0)
        if M.OWNED_BY.get(var) == 'scf':
            part, counts = generate(model, n, seed, boost=(var, top, G['boost']), factors=factors)
            part['weight'] = part['p_band'] / part['q_band']
        else:
            p = nothing_known(model, var)
            q = p.copy()
            q[top] *= G['boost']
            q /= q.sum()
            bands = torch.multinomial(torch.tensor(q), n, replacement=True,
                                      generator=torch.Generator().manual_seed(seed))
            part, counts = generate(model, n, seed, preset={var: bands}, factors=factors)
            b = bands.numpy()
            part['weight'], part['p_band'], part['q_band'] = p[b] / q[b], p[b], q[b]
        part['weight'] *= n / part['weight'].sum()
        part['stratifier'] = var
        parts.append(part)
        rule_counts = {k: rule_counts[k] + counts[k] for k in rule_counts}
        common.say(var, 'done')
    stack = pd.concat(parts, ignore_index=True)
    stack['record_id'] = np.arange(len(stack))
    common.say('consistency rules: records each applies to, over the whole stack:')
    common.say(pd.Series(rule_counts).to_string())
    return stack


# ---- the offset -----------------------------------------------------------------
def held(m, f):
    """The chance of holding, the model's chance m with its odds multiplied by f."""
    return f * m / (f * m + (1 - m))


def offset(stack, factors, run):
    """Per ruled holding: the model's own expected holders, the holders with the rules, and the factor
    on the untouched records' odds that brings the second back to the first."""
    w = stack['weight'].to_numpy(float)
    rows, new = [], {}
    for v in RULED:
        m, forced = stack[f'_own_{v}'].to_numpy(float), stack[f'_forced_{v}'].to_numpy(bool)
        f0 = factors.get(v, 1.0)
        target = (w * m).sum()
        expected = (w * np.where(forced, 1.0, held(m, f0))).sum()
        fixed, rest = w[forced].sum(), ~forced
        lo, hi = -30.0, 30.0                                # bisection on the log of the factor
        for _ in range(100):
            mid = (lo + hi) / 2
            if fixed + (w[rest] * held(m[rest], np.exp(mid))).sum() > target:
                hi = mid
            else:
                lo = mid
        new[v] = float(np.exp((lo + hi) / 2))
        rows.append({'run': run, 'holding': v, "model's own expected holders": target,
                     'expected holders with the rules': expected, 'excess (%)': 100 * (expected / target - 1),
                     'forced draws (weighted)': fixed,
                     'forced draws the model would have made zero (weighted)': (w[forced] * (1 - m[forced])).sum(),
                     'factor used': f0, 'next factor': new[v]})
    return pd.DataFrame(rows), new


def main():
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(common.SEED)
    model = M.load_model(device, os.path.join(common.MODEL, 'model.pt'))
    os.makedirs(common.GEN, exist_ok=True)
    factors, report = {}, []
    runs = int(C['offset_generations'])
    for run in range(1, runs + 1):
        stack = build_stack(model, factors)
        table, nxt = offset(stack, factors, run)
        report.append(table)
        common.say(f'run {run} of {runs}:\n' + table.drop(columns='run').to_string(index=False))
        if run < runs:
            factors = nxt
    pd.concat(report).to_csv(os.path.join(common.GEN, 'consistency.csv'), index=False)
    stack.drop(columns=[c for c in stack if c.startswith(('_own_', '_forced_'))]) \
        .to_parquet(os.path.join(common.GEN, 'stack.parquet'), index=False)


if __name__ == '__main__':
    main()
