"""
03_compare.py  (minimal example, step 3, part 2 of 2: generated against held-out)

What it does
  Compares the generated files from 03_generate.py with the test split of the
  stacked table, in band space:
  1. Per variable and source: share zero, share negative, shares in the
     90-99th, 99-99.9th and top 0.1% bands of the positives, and the total
     variation distance between the two band distributions (half the sum of
     absolute differences in band shares; 0 = identical, 1 = no overlap).
     As a yardstick, the same distance between the test and training splits
     of the real data: two real samples are this far apart by chance.
  2. The joint tables of step 2, now data against generated.
  3. Order sensitivity: (i) with random order, with another seed, and with
     the fixed order, side by side.
  4. Net worth of the PUF-labeled records of (iii) next to the SCF's.
  Held-out records count by their survey weight; generated records count
  equally (weight 1).

What it writes
  /nfs/roberts/scratch/pi_nrs36/jar335/minimal/gen/03_compare.json and
  03_tables.md (quoted in 03_REPORT.md).
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd


def load_module(filename):
    """The step files start with a digit, so a plain `import` cannot load them."""
    path = os.path.join(os.path.dirname(os.path.abspath(__file__)), filename)
    spec = importlib.util.spec_from_file_location(filename[:-3], path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


ev = load_module('02_evaluate.py')
m = ev.m
GEN_DIR = os.environ.get('MINIMAL_GEN_DIR', '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/gen')
# Ranges of the positive bands, as shares of the positive weight.
TOP_RANGES = {'P90-99': (0.9, 0.99), 'P99-99.9': (0.99, 0.999), 'top 0.1%': (0.999, 1.0)}


def column(frame, var):
    return frame[var + '_band'] if var in m.AMOUNTS else frame[var]


def band_shares(frame, var):
    """Weighted share of each choice, over records that have the slot."""
    x = column(frame, var).to_numpy()
    has = x >= 0
    w = frame['weight'].to_numpy()[has]
    return np.bincount(x[has], weights=w, minlength=m.N_CHOICES[var]) / w.sum()


def summary_row(frame, var):
    """Shares of all records with the slot: zero, negative, and the top ranges
    of the positive bands. For married and n_dep: the share in each choice."""
    s = band_shares(frame, var)
    if var == 'married':
        return {'married': s[1]}
    if var == 'n_dep':
        return {f'{k} deps': s[k] for k in range(4)}
    first_positive = m.BAND_LAYOUT[var]['first_positive']
    return {'zero': s[0], 'negative': s[1:first_positive].sum(),
            **{k: s[m.positive_bands(var, lo, hi)].sum() for k, (lo, hi) in TOP_RANGES.items()}}


def distance(a, b):
    return 0.5 * np.abs(a - b).sum()


def band_summary(test, train, generated, variables):
    rows = []
    for var in variables:
        if (column(test, var) < 0).all():
            continue                                  # the slot this source lacks
        for label, frame in [('held-out', test), ('generated', generated)]:
            rows.append({'variable': var, 'which': label, **summary_row(frame, var)})
        rows[-1]['distance to held-out'] = distance(band_shares(test, var), band_shares(generated, var))
        rows[-1]['train vs held-out'] = distance(band_shares(test, var), band_shares(train, var))
    return pd.DataFrame(rows).fillna('')


def group_of(frame, var):
    return pd.Categorical.from_codes(ev.band_group(column(frame, var), var), ev.GROUP_LABELS)


def share_by(frames, var, by, with_married):
    """Weighted share with any amount of var, by group of `by` (and married)."""
    keys = [by + ' group'] + (['married'] if with_married else [])
    parts = []
    for label, frame in frames.items():
        w = frame['weight'].to_numpy()
        g = pd.DataFrame({by + ' group': group_of(frame, by), 'married': frame['married'].to_numpy(),
                          'w': w, 'wy': w * (column(frame, var).to_numpy() != 0)})
        s = g.groupby(keys, observed=True).agg(records=('w', 'size'), w=('w', 'sum'),
                                               wy=('wy', 'sum'))
        parts.append(pd.DataFrame({f'{label} records': s['records'], label: s['wy'] / s['w']}))
    return pd.concat(parts, axis=1).reset_index()


def n_dep_by_married(frames):
    rows = []
    for married in [0, 1]:
        row = {'married': married}
        for label, frame in frames.items():
            sel = frame[frame['married'] == married]
            s = np.bincount(sel['n_dep'], weights=sel['weight'], minlength=4) / sel['weight'].sum()
            row.update({f'{label}: {k}': s[k] for k in range(4)})
        rows.append(row)
    return pd.DataFrame(rows)


NW_LABELS = ['zero or neg.'] + ev.GROUP_LABELS[1:]


def net_worth_shares(frame, wage_group=None):
    sel = frame['net_worth_band'] >= 0
    if wage_group is not None:
        sel &= ev.band_group(frame['wages_band'], 'wages') == wage_group
    f = frame[sel]
    return np.bincount(ev.band_group(f['net_worth_band'], 'net_worth'), weights=f['weight'],
                       minlength=len(NW_LABELS)) / f['weight'].sum()


def net_worth_groups(frames):
    """Net worth group shares (rows) for each frame (columns)."""
    out = pd.DataFrame({label: net_worth_shares(f) for label, f in frames.items()},
                       index=NW_LABELS)
    return out.reset_index().rename(columns={'index': 'net worth group'})


def net_worth_by_wages(test, generated):
    """Cells: held-out share / generated share of each net worth group."""
    rows = []
    for g, label in enumerate(ev.GROUP_LABELS):
        d, p = net_worth_shares(test, g), net_worth_shares(generated, g)
        rows.append({'wage group': label,
                     **{nw: f'{d[k]:.2f} / {p[k]:.2f}' for k, nw in enumerate(NW_LABELS)}})
    return pd.DataFrame(rows)


def top_net_worth_by_wages(frames):
    """Share in the top net worth groups (P90 and above of positives), by wage group."""
    rows = []
    for g, label in enumerate(ev.GROUP_LABELS):
        row = {'wage group': label}
        for name, frame in frames.items():
            f = frame[(frame['net_worth_band'] >= 0) & (ev.band_group(frame['wages_band'], 'wages') == g)]
            top = ev.band_group(f['net_worth_band'], 'net_worth') >= 6
            row[name] = float((f['weight'] * top).sum() / f['weight'].sum())
        rows.append(row)
    return pd.DataFrame(rows)


def order_sensitivity(runs):
    rows = []
    for var in ['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']:
        for measure in ['any amount', 'top 1%', 'top 0.1%']:
            row = {'variable': var, 'measure': measure}
            for label, frame in runs.items():
                s = band_shares(frame, var)
                row[label] = {'any amount': 1 - s[0],
                              'top 1%': s[m.positive_bands(var, 0.99, 1.0)].sum(),
                              'top 0.1%': s[m.positive_bands(var, 0.999, 1.0)].sum()}[measure]
            rows.append(row)
    return pd.DataFrame(rows)


def main():
    table = m.read_table()
    gen = {name: pd.read_parquet(os.path.join(GEN_DIR, name + '.parquet'))
           for name in ['gen_puf', 'gen_scf', 'gen_puf_with_net_worth',
                        'gen_puf_fixed_order', 'gen_puf_second_seed']}
    data = {(s, split): table[(table.source == s) & (table.split == split)]
            for s in m.SOURCES for split in ['train', 'test']}
    puf_test, scf_test = data[('puf', 'test')], data[('scf', 'test')]
    puf_frames = {'held-out': puf_test, 'generated': gen['gen_puf']}
    scf_frames = {'held-out': scf_test, 'generated': gen['gen_scf']}
    tables = {
        '1. PUF amounts (held-out vs generated (i))':
            band_summary(puf_test, data[('puf', 'train')], gen['gen_puf'], m.AMOUNTS),
        '1. PUF married and dependents':
            band_summary(puf_test, data[('puf', 'train')], gen['gen_puf'], ['married', 'n_dep']),
        '1. SCF amounts (held-out vs generated (ii))':
            band_summary(scf_test, data[('scf', 'train')], gen['gen_scf'], m.AMOUNTS),
        '1. SCF married and dependents':
            band_summary(scf_test, data[('scf', 'train')], gen['gen_scf'], ['married', 'n_dep']),
        '2a. PUF: share with any dividends by wage group and married':
            share_by(puf_frames, 'dividends', 'wages', True),
        '2a. SCF: share with any dividends by wage group and married':
            share_by(scf_frames, 'dividends', 'wages', True),
        '2b. PUF: share with any capital gain or loss by dividend group':
            share_by(puf_frames, 'capital_gains', 'dividends', False),
        '2c. PUF: dependents by married': n_dep_by_married(puf_frames),
        '2c. SCF: dependents by married': n_dep_by_married(scf_frames),
        '3. Order sensitivity, PUF (i): random order, second seed, fixed order': order_sensitivity(
            {'random': gen['gen_puf'], 'random, seed 2': gen['gen_puf_second_seed'],
             'fixed': gen['gen_puf_fixed_order'], 'held-out': puf_test}),
        '4a. Net worth groups: SCF held-out, SCF generated (ii), PUF-labeled generated (iii)':
            net_worth_groups({'SCF held-out': scf_test, 'SCF generated': gen['gen_scf'],
                              'PUF-labeled generated': gen['gen_puf_with_net_worth']}),
        '4b. Share in the top net worth groups (P90+ of positives) by wage group':
            top_net_worth_by_wages({'SCF held-out': scf_test, 'SCF generated': gen['gen_scf'],
                                    'PUF-labeled generated': gen['gen_puf_with_net_worth']}),
    }
    tables['2d. SCF: net worth group shares by wage group (held-out / generated)'] = \
        net_worth_by_wages(scf_test, gen['gen_scf'])
    md = [f'### {title}\n\n' + ev.md_table(frame, 4) for title, frame in tables.items()]
    with open(os.path.join(GEN_DIR, '03_tables.md'), 'w') as f:
        f.write('\n\n'.join(md) + '\n')
    with open(os.path.join(GEN_DIR, '03_compare.json'), 'w') as f:
        json.dump({t: fr.astype(str).to_dict(orient='records') for t, fr in tables.items()},
                  f, indent=2)
    print('\n\n'.join(md))


if __name__ == '__main__':
    main()
