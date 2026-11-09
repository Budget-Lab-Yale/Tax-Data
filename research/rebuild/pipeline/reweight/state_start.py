"""The starting shares of the state weights: for each record of a finished year's file, the chance of each of the 51
states, read from the networks' state slot. The only GPU step of the state weights.

The network sees each record as a CPS row: the items the CPS has are shown, the state and the CPS's reported slots are
hidden, and PUF-only items are shown as well. A record's chance of a state is averaged over the networks and over the
target year's CPS years with the year mix's shares. Writes start_<year>.npz. See FEATURES.md, "State weights".
"""
import argparse
import os
import time

import numpy as np
import pandas as pd
import pyarrow.parquet as pq
import torch

from shared import common
from pretrain import model as M
from shared import survey_mix

C = len(M.CONTEXT)
P = M.PREDICTED
J_STATE = P.index('state')
N_STATES = M.N_CHOICES['state'] - 1
CPS_SLOTS_PACKED = M.SOURCE_SLOTS['cps']
CPS_ITEMS = [v for v in P if 'cps' in M.PRESENT_ON[v] and v not in M.REPORTED and v != 'state']   # the CPS slots shown
AGE_COLUMNS = {'age1': 80, 'age2': 80, 'dep_age1': 85, 'dep_age2': 85, 'dep_age3': 85}   # each age's top code, in years
PERSON = [v for v in M.EXACT_AGES + M.CPS_SLOTS if v != 'state']
# Slots hidden on filers: their holders are far more common on the file's filers than on CPS units, so a holder means
# something else to the network. In 2019, 58.7% of filers hold other income against 2.7% of CPS units (44% in Alaska,
# from the Permanent Fund dividend), and 3.0% hold a rental loss against 0.3%.
HIDE_ON_FILERS = ['other_agi', 'rent_loss']


def load_models(device):
    runs = [m['run'] for m in common.SETTINGS['generation']['models']]
    return [M.load_model(device, f'{common.OUT}/model/{r}/model.pt') for r in runs], runs


def present_by_year():
    """For each CPS year, the slots some row of that year holds; and the slots that are zero on every CPS row, which the
    CPS does not ask and which are therefore hidden."""
    t = pq.read_table(common.TABLE, columns=['year'] + [M.column(v) for v in CPS_ITEMS], filters=[('source', '=', 'cps')]).to_pandas()
    have = {int(y): {v for v in CPS_ITEMS if (g[M.column(v)] >= 0).any()} for y, g in t.groupby('year')}
    zero = [v for v in CPS_ITEMS if (t[M.column(v)] <= 0).all()]
    return have, zero


def file_values(path):
    """The file's slots as values [n, SLOTS], absent where the CPS does not hold the slot; the record ids; which records
    are dependent filers; and the file's PUF-only slots, as a frame. Ages become categories: 0 none, else the age plus 1."""
    names = set(pq.ParquetFile(path).schema.names)
    cols = [M.column(v) for v in CPS_ITEMS]
    missing = [c for c in cols if c not in names]
    assert not missing, f'{path}: no columns {missing}'
    extra = [v for v in P if M.OWNED_BY.get(v) == 'puf' and M.column(v) in names and 'cps' not in M.PRESENT_ON[v]]
    d = pd.read_parquet(path, columns=['record_id'] + cols + [M.column(v) for v in extra])
    na = d[cols].isna().sum()
    if na.any():
        print(f'{path}: bands or categories missing, shown as absent: {na[na > 0].to_dict()}', flush=True)
    values = torch.full((len(d), len(M.SLOTS)), -1, dtype=torch.long)
    for v, c in zip(CPS_ITEMS, cols):
        x = d[c].to_numpy(float)
        if c in AGE_COLUMNS:
            assert (np.nan_to_num(x, nan=-1) >= -1).all()
            x = np.where(x >= 0, np.minimum(x, AGE_COLUMNS[c]) + 1, 0)
        x = np.nan_to_num(x, nan=-1).astype(np.int64)
        over = x >= M.N_CHOICES[v]
        assert not over.any(), f'{c}: {int(over.sum())} values above the slot\'s {M.N_CHOICES[v]} choices'
        values[:, C + P.index(v)] = torch.as_tensor(x)
    dep = d['age_range'].to_numpy() >= common.ADULT_RANGES
    return values, d['record_id'].to_numpy(), dep, d[[M.column(v) for v in extra]]


def gate_shares_in_view(values):
    """Show the spouse's shares only where training has them on a CPS row: married, with some of a base the CPS has."""
    for s, (m, bases) in M.SHARE_GATES.items():
        if 'cps' not in M.PRESENT_ON[P[s]]:
            continue
        asked = values[:, C + m] == 1
        some = torch.zeros_like(asked)
        for b in bases:
            if 'cps' in M.PRESENT_ON[P[b]]:
                some |= values[:, C + b] > 0
        asked &= some
        values[:, C + s] = torch.where(asked, values[:, C + s].clamp(min=0), torch.full_like(values[:, C + s], -1))
    return values


@torch.no_grad()
def read_state(model, values, hidden, slots, chunk):
    """The chance of each of the 51 states, [n, 51], given the values and hidden flags."""
    pos = list(slots).index(C + J_STATE)
    st = M.slot_tensor(slots, values.device)
    keep = st[C:] - C
    out = []
    head = model.heads[J_STATE]
    for i in range(0, len(values), chunk):
        h = model.last_layer(values[i:i + chunk][:, st], hidden[i:i + chunk][:, keep], slots)[:, pos]
        out.append(head(h)[:, 1:].float().softmax(-1).cpu())   # the value none left out, the rest rescaled
    return torch.cat(out).numpy().astype(np.float64)


def cmd_start(a):
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    t0 = time.time()
    models, runs = load_models(device)
    mix = survey_mix.mix('cps', a.year)
    have, zero = present_by_year()
    print(f'zero on every CPS row, hidden: {zero}', flush=True)
    assert all(y in have for y, _ in mix), f'CPS years {mix} against the table\'s {sorted(have)}'
    values0, ids, dep, ex = file_values(a.file)
    values0[:, C + J_STATE] = 0                             # present and hidden
    for r in M.REPORTED:
        if 'cps' in M.PRESENT_ON[r]:
            values0[:, C + P.index(r)] = 0                  # present and hidden, as at generation
    hidden0 = torch.zeros(len(values0), len(P), dtype=torch.bool)
    hidden0[:, J_STATE] = True
    for r in M.REPORTED:
        if 'cps' in M.PRESENT_ON[r]:
            hidden0[:, P.index(r)] = True
    for v in zero:
        hidden0[:, P.index(v)] = True
        values0[:, C + P.index(v)] = 0
    on_filers = values0[:, C + P.index('filer')] == 1
    for v in HIDE_ON_FILERS:
        j = P.index(v)
        hidden0[on_filers, j] = True
        values0[on_filers, C + j] = values0[on_filers, C + j].clamp(min=0)
    # A dependent filer has no person items on its return and the CPS has no dependent filers, so its age range and
    # person items are hidden rather than shown as none.
    d = torch.as_tensor(dep)
    for v in PERSON + ['age_range']:
        j = P.index(v)
        hidden0[d, j] = True
        values0[d, C + j] = values0[d, C + j].clamp(min=0)
    values0 = gate_shares_in_view(values0)
    values0[:, 0] = M.SOURCES.index('cps')
    extra = [v for v in P if M.column(v) in ex]
    for v in extra:
        values0[:, C + P.index(v)] = torch.as_tensor(ex[M.column(v)].to_numpy(float)).long()
    slots = tuple(sorted(set(CPS_SLOTS_PACKED) | {C + P.index(v) for v in extra}))
    print(f'shown as well: {len(extra)} PUF-only slots', flush=True)
    chunk = M.safe_chunk(len(slots))
    probs = np.zeros((len(ids), N_STATES))
    for k, (m, run) in enumerate(zip(models, runs)):
        for y, share in mix:
            v = values0.clone()
            v[:, 1] = M.YEARS.index(y)
            for s in CPS_ITEMS:          # a slot this CPS year never holds is absent
                if s not in have[y]:
                    v[:, C + P.index(s)] = -1
            p = read_state(m, v.to(device), hidden0.to(device), slots, chunk)
            probs += share * p / len(models)
            print(f'{run}, CPS year {y} (share {share:.4f}): read {len(ids):,} records, {time.time() - t0:.0f} s', flush=True)
    assert np.isfinite(probs).all() and np.allclose(probs.sum(1), 1, atol=1e-5)
    os.makedirs(os.path.dirname(a.out), exist_ok=True)
    assert not os.path.exists(a.out), f'{a.out} is already there'
    np.savez(a.out, record_id=ids, p=probs.astype(np.float32), mix=np.array(mix))
    print(f'{a.out}: {len(ids):,} records; average chance by state, largest five: '
          + str(dict(sorted(zip(np.arange(N_STATES), probs.mean(0).round(4)), key=lambda z: -z[1])[:5])), flush=True)


if __name__ == '__main__':
    ap = argparse.ArgumentParser()
    ap.add_argument('--file', required=True)
    ap.add_argument('--year', type=int, required=True)
    ap.add_argument('--out', required=True)
    cmd_start(ap.parse_args())
