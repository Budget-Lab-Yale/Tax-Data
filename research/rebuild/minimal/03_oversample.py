"""
03_oversample.py  (minimal example, step 3, extra: oversampling the top, with exact weights)

What it does
  Generates PUF-labeled records in which the top of one amount is drawn ten
  times as often as the model would draw it, and gives each record the exact
  weight that undoes this. This is the IRS's own design, a sample that is
  dense at the top with small weights there, applied to the generator.

  For a stratifying variable (capital_gains, then wages):
    1. p(band) = the model's distribution of that variable for a PUF record
       with every other slot hidden (what generation draws when it is first).
    2. q(band) = p(band), except that the bands covering the top 1% of the
       positive weight (the 0.1% and 0.01% bands) get ten times their
       probability; q is then rescaled to sum to one.
    3. Each record's band is drawn from q. The other slots are then drawn
       given that band in the usual block order: the other incomes in a
       random order, then married, then n_dep.
    4. weight = p(band) / q(band). Weighted, the file estimates the same
       distribution as an unweighted plain file, but with about ten times as
       many records in the top bands.

What it writes (in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/gen/)
  gen_puf_oversampled_kg.parquet, gen_puf_oversampled_kg_seed2.parquet
  gen_puf_oversampled_wages.parquet, gen_puf_oversampled_wages_seed2.parquet
    200,000 records each; columns as gen_puf.parquet, but weight is the
    importance weight, plus p_band and q_band (the two probabilities of the
    record's stratifier band) so that the weight can be checked.
  gen_meta_oversample.json, 03_oversample_tables.md (quoted in 03_REPORT.md)
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd
import torch


def load_module(filename):
    """The step files start with a digit, so a plain `import` cannot load them."""
    path = os.path.join(os.path.dirname(os.path.abspath(__file__)), filename)
    spec = importlib.util.spec_from_file_location(filename[:-3], path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


g = load_module('03_generate.py')
cmp = load_module('03_compare.py')
m, ev = g.m, cmp.ev
N, BOOST = 200_000, 10.0
RUNS = {'kg': ('capital_gains', [11, 12]), 'wages': ('wages', [21, 22])}


@torch.no_grad()
def everything_hidden(model, var, source='puf', with_net_worth=False):
    """The model's distribution of var for a record of this source with
    every predicted slot hidden. with_net_worth adds a net worth slot to a
    PUF record, as in gen_puf_with_net_worth."""
    device = next(model.parameters()).device
    values = torch.full((1, len(m.SLOTS)), -1, dtype=torch.long, device=device)
    values[0, 0], values[0, 1] = m.SOURCES.index(source), m.YEARS.index(2015)
    for j, v in enumerate(m.PREDICTED):
        if m.OWNED_BY.get(v, source) == source or (v == 'net_worth' and with_net_worth):
            values[0, g.NC + j] = 0
    log_probs = model(values, values[:, g.NC:] >= 0)[0, m.PREDICTED.index(var)]
    return log_probs.exp().double().cpu().numpy()[:m.N_CHOICES[var]]


def sampling_distribution(p, var):
    q = p.copy()
    q[m.positive_bands(var, 0.99, 1.0)] *= BOOST
    return q / q.sum()


def oversampled_file(model, var, seed, n=N, with_net_worth=False, borrowed=None):
    # p must be the distribution generate() draws the stratifier from when it
    # goes first. Under 03_generate's own_first order (v4c on) the net worth
    # slot of a PUF record is absent until net worth's turn, so p is taken
    # without it; under the block order (default) with it, masked.
    p = everything_hidden(model, var, with_net_worth=with_net_worth and g.ORDER == 'block')
    q = sampling_distribution(p, var)
    bands = torch.multinomial(torch.tensor(q), n, replacement=True,
                              generator=torch.Generator().manual_seed(seed))
    table = g.generate(model, n, 'puf', with_net_worth=with_net_worth, fixed_order=False,
                       seed=seed, preset={var: bands}, borrowed=borrowed)
    b = bands.numpy()
    table['weight'], table['p_band'], table['q_band'] = p[b] / q[b], p[b], q[b]
    return table


# ---- Comparison ----------------------------------------------------------------------
def top_share(frame, var, low):
    """Weighted share of records in the positive bands above `low` (a share
    of the positive weight), and its standard error from this one file
    (the usual ratio-estimator formula, so it covers weighted files too)."""
    w = frame['weight'].to_numpy()
    y = np.isin(cmp.column(frame, var).to_numpy(), m.positive_bands(var, low, 1.0))
    share = (w * y).sum() / w.sum()
    se = np.sqrt(((w * (y - share)) ** 2).sum()) / w.sum()
    return share, se


def top_band_counts(files, var):
    bands = m.positive_bands(var, 0.99, 1.0)
    lower = [0.0] + m.BAND_LAYOUT[var]['positive_top']
    rows = []
    for b in bands:
        i = b - m.BAND_LAYOUT[var]['first_positive']
        row = {'band': int(b), 'share of positives': f'{100 * lower[i]:.2f}-{100 * lower[i + 1]:.2f}%'}
        row.update({label: int((cmp.column(f, var) == b).sum()) for label, f in files.items()})
        rows.append(row)
    rows.append({'band': 'all top 1%', 'share of positives': '99-100%',
                 **{label: int(np.isin(cmp.column(f, var), bands).sum()) for label, f in files.items()}})
    return pd.DataFrame(rows)


def main():
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(m.SEED)
    model = m.load_model(device)
    meta, files = {'model': os.path.join(m.MODEL_DIR, 'model.pt'), 'boost': BOOST,
                   'files': {}}, {}
    for short, (var, seeds) in RUNS.items():
        p = everything_hidden(model, var)
        meta[f'p_top1pct_{var}'] = float(p[m.positive_bands(var, 0.99, 1.0)].sum())
        for k, seed in enumerate(seeds):
            name = f'gen_puf_oversampled_{short}' + ('_seed2' if k else '')
            files[name] = oversampled_file(model, var, seed)
            files[name].to_parquet(os.path.join(g.GEN_DIR, name + '.parquet'), index=False)
            meta['files'][name + '.parquet'] = dict(
                stratifier=var + '_band', seed=seed, n=N,
                what=f'PUF-labeled records, {var} band drawn first from q = model p '
                     f'with the top-1% positive bands x{BOOST:g}; weight = p/q')
            print(name, 'weight sum', files[name]['weight'].sum(), flush=True)
    with open(os.path.join(g.GEN_DIR, 'gen_meta_oversample.json'), 'w') as f:
        json.dump(meta, f, indent=2)

    plain = {k: pd.read_parquet(os.path.join(g.GEN_DIR, k + '.parquet'))
             for k in ['gen_puf', 'gen_puf_second_seed']}
    md = []
    for short, (var, _) in RUNS.items():
        strat = files[f'gen_puf_oversampled_{short}']
        rows = []
        for v in m.PREDICTED:
            if v == 'net_worth':
                continue
            row = {'variable': v}
            for label, f in [('plain', plain['gen_puf']), ('stratified', strat)]:
                row.update({f'{label}: {k}': x for k, x in cmp.summary_row(f, v).items()})
            row['distance plain vs stratified'] = cmp.distance(cmp.band_shares(plain['gen_puf'], v),
                                                              cmp.band_shares(strat, v))
            row['distance plain vs plain, new seed'] = cmp.distance(
                cmp.band_shares(plain['gen_puf'], v), cmp.band_shares(plain['gen_puf_second_seed'], v))
            rows.append(row)
        md.append(f'### Weighted band summary, plain vs {var}-stratified\n\n'
                  + ev.md_table(pd.DataFrame(rows).fillna(''), 4))
        md.append(f'### Records in each top band of {var}\n\n' + ev.md_table(top_band_counts(
            {'plain': plain['gen_puf'], 'plain, new seed': plain['gen_puf_second_seed'],
             'stratified': strat, 'stratified, seed 2': files[f'gen_puf_oversampled_{short}_seed2']},
            var)))
    rows = []
    for var in ['capital_gains', 'wages']:
        for low, label in [(0.99, 'top 1%'), (0.999, 'top 0.1%')]:
            for name, pair in [('plain', ['gen_puf', 'gen_puf_second_seed']),
                               ('kg-stratified', ['gen_puf_oversampled_kg', 'gen_puf_oversampled_kg_seed2']),
                               ('wages-stratified', ['gen_puf_oversampled_wages', 'gen_puf_oversampled_wages_seed2'])]:
                (s1, se1), (s2, _) = [top_share(plain.get(f, files.get(f)), var, low) for f in pair]
                rows.append({'variable': var, 'measure': label, 'file': name, 'seed 1': s1,
                             'seed 2': s2, 'difference': s2 - s1, 'standard error': se1})
    md.append('### Seed-to-seed variation of top shares (share of all records, weighted)\n\n'
              + ev.md_table(pd.DataFrame(rows), 6))
    with open(os.path.join(g.GEN_DIR, '03_oversample_tables.md'), 'w') as f:
        f.write('\n\n'.join(md) + '\n')
    print('\n\n'.join(md))


if __name__ == '__main__':
    main()
