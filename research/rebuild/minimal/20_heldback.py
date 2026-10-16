"""
20_heldback.py  (minimal example, step 20: held-back loss, joint model against separate models)

Scores the joint model V4 and the one-source models (B1 = S20_PUF, trained on
the PUF records of table E4 only; B2 = S20_SCF, the SCF records only) on the
same held-back records: the 2% slice of 02_model.held_back, 4,124 PUF and
710 SCF records, which none of these models trained on. A PUF record is
scored by V4 and B1, an SCF record by V4 and B2, each under its own label
and with exactly the slots it has.

Three scores per variable, cross-entropy in nats, weighted by the training
weights of 02_model.training_weights (lower is better):
  one    that variable hidden, every other slot of the record visible
  all    every slot hidden (the model's distribution of that variable alone)
  chain  the record's joint log-probability split into its steps, in the
         order of generation: the incomes in a random order, then married,
         then n_dep, then (SCF records) net worth; four random orders per
         record, averaged. The sum over variables is the joint
         cross-entropy of the whole record.
For each model pair the paired difference (separate minus joint) gets a
standard error that treats households as the sampling unit (all units and
implicates of an SCF household together).

Usage: python 20_heldback.py <out csv> name=dir [name=dir ...]
Each dir holds a model.pt trained on table E4, E4puf or E4scf (all share the
band layout of E4). The name says which sources the model is scored on:
names starting 'V4' get both, 'B1' the PUF, 'B2' the SCF. Each B model is
compared with V4_s2 if its name ends in _s2, otherwise with V4.
"""
import importlib.util
import os
import sys

import numpy as np
import pandas as pd
import torch

HERE = os.path.dirname(os.path.abspath(__file__))
os.environ.setdefault('MINIMAL_TABLE', 'E4')
os.environ.setdefault('MINIMAL_RUN', 'V4')
os.environ['MINIMAL_ALL_RECORDS'] = '1'


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


g = load('g', '03_generate.py')
m, NC = g.m, g.NC
SEED = 20261020
N_ORDERS = 4


@torch.no_grad()
def per_record_scores(model, values, seed):
    """Per record and variable: nll with only it hidden ('one'), with all
    hidden ('all'), and its step in the generation-order chain ('chain',
    averaged over N_ORDERS orders). Arrays [records, predicted slots], NaN
    where the slot is absent."""
    present = values[:, NC:] >= 0
    n, k = present.shape
    out = {}
    nan = torch.full((n, k), float('nan'), dtype=torch.float64, device=values.device)
    one = nan.clone()
    for j in range(k):
        has = present[:, j]
        if not has.any():
            continue
        hidden = torch.zeros_like(present)
        hidden[:, j] = has
        nll = m.slot_nll(m.predict(model, values, hidden), values)
        one[has, j] = nll[has, j].double()
    out['one'] = one
    nll_all = m.slot_nll(m.predict(model, values, present), values).double()
    out['all'] = torch.where(present, nll_all, nan)
    source = m.SOURCES[int(values[0, 0])]
    assert (values[:, 0] == values[0, 0]).all()
    incomes = torch.tensor([m.PREDICTED.index(v) for v in g.INCOMES[source]], device=values.device)
    later = [m.PREDICTED.index(v) for v in ['married', 'n_dep']]
    if source == 'scf':
        later.append(m.PREDICTED.index('net_worth'))
    later = torch.tensor(later, device=values.device)
    gen = torch.Generator(device=values.device).manual_seed(seed)
    rows = torch.arange(n, device=values.device)
    chain = torch.zeros((n, k), dtype=torch.float64, device=values.device)
    for _ in range(N_ORDERS):
        perm = torch.rand(n, len(incomes), generator=gen, device=values.device).argsort(1)
        order = torch.cat([incomes[perm], later.repeat(n, 1)], 1)
        hidden = present.clone()
        for step in range(order.shape[1]):
            slot = order[:, step]
            assert present[rows, slot].all()
            nll = m.slot_nll(m.predict(model, values, hidden), values).double()
            chain[rows, slot] += nll[rows, slot]
            hidden[rows, slot] = False
        assert not hidden.any()
    out['chain'] = torch.where(present, chain / N_ORDERS, nan)
    return {k_: v.cpu().numpy() for k_, v in out.items()}


def clustered_se(d, w, cluster):
    """Standard error of the weighted mean of d, households as clusters."""
    mean = (d * w).sum() / w.sum()
    s = pd.Series(w * (d - mean)).groupby(cluster).sum().to_numpy()
    n = len(s)
    return np.sqrt(n / (n - 1) * (s ** 2).sum()) / w.sum()


def main():
    out_csv = sys.argv[1]
    models = dict(a.split('=', 1) for a in sys.argv[2:])
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    table = m.read_table()
    table['w'] = m.training_weights(table)
    held = table[table['split'] == 'test']
    scores = {}
    for name, d in models.items():
        model = m.load_model(device, d)
        for s in m.SOURCES:
            if not (name.startswith('V4') or (name.startswith('B1') and s == 'puf')
                    or (name.startswith('B2') and s == 'scf')):
                continue
            part = held[held['source'] == s].reset_index(drop=True)
            values = m.encode(part, device)['values']
            scores[(name, s)] = per_record_scores(model, values, SEED + m.SOURCES.index(s))
            print(name, s, 'scored', len(part), flush=True)
    rows = []
    for (name, s), sc in scores.items():
        part = held[held['source'] == s].reset_index(drop=True)
        w, cl = part['w'].to_numpy(float), part['household_id'].to_numpy()
        for kind, arr in sc.items():
            for j, v in enumerate(m.PREDICTED):
                if np.isnan(arr[:, j]).all():
                    continue
                assert not np.isnan(arr[:, j]).any()
                rows.append({'model': name, 'source': s, 'score': kind, 'variable': v,
                             'nats': (arr[:, j] * w).sum() / w.sum(),
                             'se': clustered_se(arr[:, j], w, cl), 'records': len(part)})
            if kind == 'chain':
                tot = np.nansum(arr, 1)
                rows.append({'model': name, 'source': s, 'score': kind, 'variable': 'whole record',
                             'nats': (tot * w).sum() / w.sum(), 'se': clustered_se(tot, w, cl),
                             'records': len(part)})
    res = pd.DataFrame(rows)
    # paired differences: each other model against the V4 model of the same seed tag
    diffs = []
    for (name, s), sc in scores.items():
        if name.startswith('V4'):
            continue
        ref = 'V4_s2' if name.endswith('_s2') else 'V4'
        if (ref, s) not in scores:
            continue
        part = held[held['source'] == s].reset_index(drop=True)
        w, cl = part['w'].to_numpy(float), part['household_id'].to_numpy()
        for kind in sc:
            a, b = sc[kind], scores[(ref, s)][kind]
            cols = [(v, a[:, j] - b[:, j]) for j, v in enumerate(m.PREDICTED) if not np.isnan(a[:, j]).all()]
            if kind == 'chain':
                cols.append(('whole record', np.nansum(a, 1) - np.nansum(b, 1)))
            for v, d in cols:
                diffs.append({'model': name, 'against': ref, 'source': s, 'score': kind, 'variable': v,
                              'difference (nats)': (d * w).sum() / w.sum(),
                              'se of difference': clustered_se(d, w, cl)})
    res.to_csv(out_csv, index=False)
    pd.DataFrame(diffs).to_csv(out_csv.replace('.csv', '_differences.csv'), index=False)
    print(res.to_string())
    print(pd.DataFrame(diffs).to_string())


if __name__ == '__main__':
    main()
