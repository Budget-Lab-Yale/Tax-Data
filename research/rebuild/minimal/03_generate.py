"""
03_generate.py  (minimal example, step 3, part 1 of 2: generation)

What it does
  Draws synthetic records from the step 2 model. A record starts with its
  source and year set and every predicted slot masked. Slots are then filled
  one at a time: the model gives a probability over the next slot's tokens
  given everything filled so far, and one token is drawn from it
  (temperature 1, nothing else).

  Two orders, set by MINIMAL_ORDER (added 2026-10-16):

  'block' (the default, so that every run before v4c reproduces exactly):
  the block order that research/rebuild/design/plan.md, decision 9, called
  for until 2026-10-16:
    block 1  the income variables, in a random order for each record
             (PUF: wages, interest, dividends, capital_gains, other_agi;
              SCF: the first four, since the SCF has no other_agi)
    block 2  net_worth, when the record is to have it
    block 3  married, then n_dep
  The reasoning then (research/rebuild/design/idea_space.md, "Block order"):
  incomes first, because the file is a tax file; wealth next, given the
  incomes; demographics last, because in the earlier tests drawing a
  demographic early taught the model wrong things about the top. Inside
  block 1 the order is random so that no income variable is privileged; the
  fixed-order file checks whether that choice matters.

  'own_first': the order decision 9 calls for since John changed it on
  2026-10-16, used from v4c onward. The record's own source's variables
  first, borrowed variables last:
    block 1  every variable the record's own survey has, demographics
             included, in a random order for each record (PUF: married,
             n_dep, wages, interest, dividends, capital_gains, other_agi;
             SCF: married, n_dep, the four shared incomes, net_worth)
    block 2  each borrowed variable (net_worth on a PUF record), drawn as
             the `borrowed` setting says (decision 16: under the SCF label
             with other_agi left out)
  While block 1 is drawn, a borrowed slot is absent, not masked, so a PUF
  record looks exactly like a PUF training record (which has no net worth
  slot); the slot appears, masked, only when its turn comes. Drawing married
  and dependents after net worth under the PUF label asked the model about a
  combination no PUF record has; drawn first, non-joint returns with two or
  more dependents came out at 11.4% against the PUF's 11.6%, where the block
  order gave 8.7% (20_SEPARATE_MODELS.md, arm C). Arm C drew married and
  n_dep after the incomes; here they are in the random order with them, as
  decision 9 states. fixed_order=True puts block 1 in the order of PREDICTED.

What it writes (in the generation folder of 02_model.py: gen/, or gen/<T>/
with MINIMAL_RUN=<T>, or MINIMAL_GEN_DIR)
  gen_puf.parquet                 (i)   200,000 PUF-labeled records, no net worth
  gen_scf.parquet                 (ii)   40,000 SCF-labeled records, all seven
  gen_puf_with_net_worth.parquet  (iii) 200,000 PUF-labeled records with net
                                        worth drawn in block 2 under the PUF
                                        label (never trained; as in v2)
  gen_puf_with_net_worth_ownerlabel.parquet
                                  (iii) again, seed 3 as well, with net worth
                                        drawn under the SCF label (the owner)
  gen_puf_fixed_order.parquet     (i) again, block 1 in the fixed order wages,
                                        interest, dividends, capital_gains, other_agi
  gen_puf_second_seed.parquet     (i) again, random order, another seed: the
                                        sampling-noise reference for the above
  gen_meta.json                   what each file is
Columns as in the stacked table: source, year, married, n_dep, the six band
columns (-1 where the slot is absent), weight = 1. No dollar columns.
"""
import importlib.util
import json
import os

import pandas as pd
import torch


def load_module(filename):
    """The step files start with a digit, so a plain `import` cannot load them."""
    path = os.path.join(os.path.dirname(os.path.abspath(__file__)), filename)
    spec = importlib.util.spec_from_file_location(filename[:-3], path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


m = load_module('02_model.py')
NC = len(m.CONTEXT)
GEN_DIR = m.GEN_DIR
# Block 1: every income a source has (all amounts but net worth). For the
# v2 table, PUF: wages, interest, dividends, capital_gains, other_agi; SCF:
# the first four. Experiment A adds the three small parts on the PUF side.
INCOMES = {s: [a for a in m.AMOUNTS if a != 'net_worth' and m.OWNED_BY.get(a, s) == s]
           for s in m.SOURCES}
# The draw order (see the docstring): 'block' (default) or 'own_first'.
ORDER = os.environ.get('MINIMAL_ORDER', 'block')
assert ORDER in ('block', 'own_first'), ORDER
# Under own_first: every variable a source has, and the ones it borrows.
OWN = {s: [v for v in m.PREDICTED if m.OWNED_BY.get(v, s) == s] for s in m.SOURCES}
BORROWED_VARS = {s: [v for v in m.PREDICTED if m.OWNED_BY.get(v, s) != s] for s in m.SOURCES}


def generation_order(n, source, with_net_worth, fixed_order, gen, already_set=()):
    """[n, steps] tensor of predicted-slot indices, one row per record, in
    the order set by ORDER. Slots in already_set were set before generation
    and are skipped."""
    if ORDER == 'own_first':
        own = torch.tensor([m.PREDICTED.index(v) for v in OWN[source]
                            if v not in already_set and (v != 'net_worth' or with_net_worth)])
        if fixed_order:
            block1 = own.repeat(n, 1)
        else:
            block1 = own[torch.rand(n, len(own), generator=gen).argsort(1)]
        # The one borrowed variable drawn in this code is net worth on a PUF
        # record (an SCF record never gets other_agi, in either order).
        late = [v for v in BORROWED_VARS[source]
                if v == 'net_worth' and with_net_worth and v not in already_set]
        if not late:
            return block1
        block2 = torch.tensor([m.PREDICTED.index(v) for v in late]).repeat(n, 1)
        return torch.cat([block1, block2], 1)
    incomes = torch.tensor([m.PREDICTED.index(v) for v in INCOMES[source]
                            if v not in already_set])
    if fixed_order:
        block1 = incomes.repeat(n, 1)
    else:
        block1 = incomes[torch.rand(n, len(incomes), generator=gen).argsort(1)]
    later = [v for v in (['net_worth'] if with_net_worth else []) + ['married', 'n_dep']
             if v not in already_set]
    block23 = torch.tensor([m.PREDICTED.index(v) for v in later]).repeat(n, 1)
    return torch.cat([block1, block23], 1)


@torch.no_grad()
def generate(model, n, source, with_net_worth, fixed_order, seed, chunk=50_000,
             preset=None, boost=None, borrowed=None, year=2015):
    """year: the year token every record carries (default 2015; must be in
    the table's years, m.YEARS).
    preset: optional {variable: tensor of n bands} set before the other
    slots are drawn (used by 03_oversample.py and 03_stack.py).
    boost: optional (variable, bands, factor). When that variable's turn
    comes, it is drawn not from the model's probabilities p for this record
    but from q: p with the listed bands multiplied by factor, rescaled to sum
    to one. The record's p and q of the drawn band are returned as columns
    p_band and q_band, so that p/q is its exact importance weight.
    borrowed: how to draw a borrowed variable, one the record's own survey
    does not have (net worth on a PUF record). None draws it like any other
    slot. Otherwise a dict, applied to that one draw only:
      label    'owner': the source token is set to the survey that owns the
               variable (a borrowed variable is drawn under the label of the
               survey that owns it: the model learned net worth only from
               SCF records). 'own': the record's own label is kept.
      foreign  what to do with the record's slots the owner never has
               (other_agi, when the owner is the SCF): None leaves them
               visible, 'mask' hides them behind the mask token, 'absent'
               leaves them out, so the model sees exactly the owner's slots.
      year     (optional; added 2026-10-16, step 21 second round, plan
               decision 3) the year token for this draw only, a year of the
               owner's data. The record keeps its own year token for its own
               variables; the table gets a column net_worth_year.
    The record itself keeps all its values; only that draw's view changes."""
    preset = preset or {}
    device = next(model.parameters()).device
    cpu_gen = torch.Generator().manual_seed(seed)                 # for the orders
    gpu_gen = torch.Generator(device=device).manual_seed(seed)    # for the draws
    parts = []
    for start in range(0, n, chunk):
        size = min(chunk, n - start)
        order = generation_order(size, source, with_net_worth, fixed_order, cpu_gen,
                                 preset).to(device)
        values = torch.full((size, len(m.SLOTS)), -1, dtype=torch.long, device=device)
        values[:, 0] = m.SOURCES.index(source)
        values[:, 1] = m.YEARS.index(year)
        rows = torch.arange(size, device=device)
        values[rows[:, None], NC + order] = 0    # present, not yet drawn (masked below)
        if ORDER == 'own_first':
            # A borrowed slot is absent until its turn (a PUF training record
            # has no net worth slot); it is put back, masked, at its step.
            for v in BORROWED_VARS[source]:
                values[:, NC + m.PREDICTED.index(v)] = -1
        filled = torch.zeros(size, len(m.PREDICTED), dtype=torch.bool, device=device)
        for var, bands in preset.items():
            j = m.PREDICTED.index(var)
            values[:, NC + j] = bands[start:start + size].to(device)
            filled[:, j] = True
        p_drawn = torch.full((size,), float('nan'), dtype=torch.float64, device=device)
        q_drawn = p_drawn.clone()
        for step in range(order.shape[1]):
            slot = order[:, step]
            if ORDER == 'own_first':
                values[rows, NC + slot] = 0       # the slot drawn now: present, masked
            hidden = (values[:, NC:] >= 0) & ~filled
            probs = m.predict(model, values, hidden)[rows, slot].exp()
            if borrowed is not None:
                probs = borrowed_draw(model, values, hidden, slot, probs, source, **borrowed)
            draw_from, boosted = probs, None
            if boost is not None:
                var, bands, factor = boost
                boosted = slot == m.PREDICTED.index(var)
                q = probs.clone()
                q[:, torch.as_tensor(bands, device=device)] *= factor
                q = q / q.sum(1, keepdim=True)
                draw_from = torch.where(boosted[:, None], q, probs)
            draw = torch.multinomial(draw_from, 1, generator=gpu_gen).squeeze(1)
            if boosted is not None and boosted.any():
                p_drawn[boosted] = probs[rows, draw][boosted].double()
                q_drawn[boosted] = draw_from[rows, draw][boosted].double()
            values[rows, NC + slot] = draw
            filled[rows, slot] = True
        parts.append((values.cpu(), p_drawn.cpu(), q_drawn.cpu()))
    table = to_table(torch.cat([v for v, _, _ in parts]))
    if borrowed is not None and borrowed.get('year') is not None:
        table['net_worth_year'] = int(borrowed['year'])
    if boost is not None:
        table['p_band'] = torch.cat([p for _, p, _ in parts]).numpy()
        table['q_band'] = torch.cat([q for _, _, q in parts]).numpy()
    return table


def borrowed_draw(model, values, hidden, slot, probs, source, label, foreign, year=None):
    """Replace the probabilities of records whose slot this step is a
    borrowed variable by the model's probabilities under the view set by
    label and foreign (see generate)."""
    rows = torch.arange(len(values), device=values.device)
    for var, owner in m.OWNED_BY.items():
        borrowed = slot == m.PREDICTED.index(var)
        if owner == source or not borrowed.any():
            continue
        view, view_hidden = values.clone(), hidden.clone()
        if label == 'owner':
            view[:, 0] = m.SOURCES.index(owner)
        if year is not None:
            view[:, 1] = m.YEARS.index(year)
        for other, other_owner in m.OWNED_BY.items():
            if other_owner == owner:
                continue                            # a slot the owner has
            j = m.PREDICTED.index(other)
            if foreign == 'mask':
                view_hidden[:, j] = view[:, NC + j] >= 0
            elif foreign == 'absent':
                view[:, NC + j], view_hidden[:, j] = -1, False
        view_probs = m.predict(model, view, view_hidden)[rows, slot].exp()
        probs = torch.where(borrowed[:, None], view_probs, probs)
    return probs


def to_table(values):
    """Same columns and types as the stacked table, weight 1, no dollars."""
    v = values.numpy()
    out = pd.DataFrame({'source': [m.SOURCES[i] for i in v[:, 0]],
                        'year': [m.YEARS[i] for i in v[:, 1]]})
    for j, var in enumerate(m.PREDICTED):
        out[var + '_band' if var in m.AMOUNTS else var] = v[:, NC + j]
    out['weight'] = 1.0
    stacked = pd.read_parquet(m.DATA, columns=list(out.columns))
    return out.astype(stacked.dtypes.to_dict())


# Net worth on a PUF record drawn under the SCF label, the rest of the record
# visible (variant A of 03_owner_label.py). Used for
# gen_puf_with_net_worth_ownerlabel, and by 03_stack.py.
OWNER_LABEL = {'label': 'owner', 'foreign': None}

RUNS = {
    'gen_puf': dict(n=200_000, source='puf', with_net_worth=False, fixed_order=False,
                    seed=1, what='(i) PUF-labeled records: the six PUF variables '
                                 '(other_agi included), no net worth (-1)'),
    'gen_scf': dict(n=40_000, source='scf', with_net_worth=True, fixed_order=False,
                    seed=2, what='(ii) SCF-labeled records: the SCF variables including '
                                 'net worth; no other_agi (-1)'),
    'gen_puf_with_net_worth': dict(
        n=200_000, source='puf', with_net_worth=True, fixed_order=False, seed=3,
        what='(iii) PUF-labeled records with net worth drawn in block 2 under the '
             'PUF label (as in v2): the model never saw a PUF record with net worth'),
    'gen_puf_with_net_worth_ownerlabel': dict(
        n=200_000, source='puf', with_net_worth=True, fixed_order=False, seed=3,
        borrowed=OWNER_LABEL,
        what='(iii) again (same seed, so the same draws up to net worth), with net '
             'worth drawn under the SCF label (owner label): as the SCF, the survey '
             'that owns net worth, would answer'),
    'gen_puf_fixed_order': dict(
        n=200_000, source='puf', with_net_worth=False, fixed_order=True, seed=4,
        what='(i) again with block 1 in the fixed order wages, interest, dividends, '
             'capital_gains, other_agi; for the order-sensitivity check only'),
    'gen_puf_second_seed': dict(
        n=200_000, source='puf', with_net_worth=False, fixed_order=False, seed=5,
        what='(i) again with another seed; the sampling-noise reference for the '
             'order-sensitivity check only'),
}

# MINIMAL_GEN_YEARS=2015,2018,2021 (2026-10-15, set B of
# 10_TRANSLATION_EXPERIMENTS.md): instead of RUNS, per year: 200,000
# PUF-labeled records with net worth under the PUF label and under the SCF
# label, and 40,000 SCF-labeled records, each with the year token set.
# Files gen_puf_with_net_worth_<year>, gen_puf_with_net_worth_ownerlabel_<year>,
# gen_scf_<year>. The same seeds in every year, so years differ only by the
# year token. Unset: RUNS, as before.
GEN_YEARS = [int(y) for y in os.environ.get('MINIMAL_GEN_YEARS', '').split(',') if y.strip()]


def year_runs(years):
    out = {}
    for y in years:
        assert y in m.YEARS, f'year {y} is not in the table (years {m.YEARS})'
        out[f'gen_puf_with_net_worth_{y}'] = dict(
            n=200_000, source='puf', with_net_worth=True, fixed_order=False, seed=3, year=y,
            what=f'PUF-labeled records, year token {y}, net worth in block 2 under the PUF label')
        out[f'gen_puf_with_net_worth_ownerlabel_{y}'] = dict(
            n=200_000, source='puf', with_net_worth=True, fixed_order=False, seed=3, year=y,
            borrowed=OWNER_LABEL,
            what=f'the same, year token {y}, net worth drawn under the SCF label')
        out[f'gen_scf_{y}'] = dict(
            n=40_000, source='scf', with_net_worth=True, fixed_order=False, seed=2, year=y,
            what=f'SCF-labeled records, year token {y}, all SCF variables')
    return out


ORDER_TEXT = {
    'block': 'block 1 incomes (random order per record unless fixed_order), block 2 net_worth '
             'if with_net_worth, block 3 married then n_dep; temperature 1',
    'own_first': 'own_first (decision 9 from 2026-10-16): every variable of the record\'s own '
                 'survey, demographics included, in a random order per record (unless '
                 'fixed_order), the borrowed slot absent meanwhile; then each borrowed variable '
                 '(net worth on a PUF record); temperature 1'}


def main():
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(m.SEED)
    model = m.load_model(device)
    os.makedirs(GEN_DIR, exist_ok=True)
    meta = {'model': os.path.join(m.MODEL_DIR, 'model.pt'),
            'table': m.TABLE or 'v2', 'flags': m.flags(),
            'columns': 'source, year, married, n_dep, six *_band columns (-1 = slot '
                       'absent, not missing), weight (= 1 for every record)',
            'order': ORDER_TEXT[ORDER], 'files': {}}
    for name, run in (year_runs(GEN_YEARS) if GEN_YEARS else RUNS).items():
        settings = {k: v for k, v in run.items() if k != 'what'}
        table = generate(model, **settings)
        table.to_parquet(os.path.join(GEN_DIR, name + '.parquet'), index=False)
        meta['files'][name + '.parquet'] = {'what': run['what'], **settings}
        print(name, len(table), flush=True)
    with open(os.path.join(GEN_DIR, 'gen_meta.json'), 'w') as f:
        json.dump(meta, f, indent=2)


if __name__ == '__main__':
    main()
