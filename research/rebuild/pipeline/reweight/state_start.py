"""The starting shares of the state weights (FEATURES.md, "State weights"): for each record of a finished year's file, the chance of
each of the 51 states, read from the networks trained with the state slot (build_table.py's CPS-only category 'state').
A GPU stage, and the only one of the state weights.

  python -m reweight.state_start --file <file_<year>.parquet> --year <year> --out <start_<year>.npz>

How a record is read. The network sees the record as a CPS row would look: the source is the CPS, the year is one of the target
year's CPS years (survey_mix.py), and each slot the CPS has is either shown or hidden:
  - shown: every item the record has that the CPS has (married, dependents, age range, filing flag and status, wages, dividends,
    pensions, Social Security, the person items ...). Bands and categories are the file's own, except the five ages, which the file
    holds in years and the network as categories (0 none, else the age plus 1; the primary filer's and spouse's held at 80).
  - hidden: state, the one slot read; the CPS's reported slots (what a survey unit reported before the fill-in), as at generation:
    a file record is on the PUF's concepts and has no report; and a slot that is zero on every CPS row (the CPS does not ask it:
    mortgage interest, property tax, partnership income and others; translation table): the network has only ever seen zero there,
    so a record's real amount is not shown to it, and neither is a false zero.
  - absent: PUF-only items; a slot the CPS year never holds (capital gains before tax year 2018: a slot absent on every CPS row of
    the year is never shown there); the spouse's shares where training would leave them out (not married, or none of the base).
On filers, the slots HIDE_ON_FILERS are hidden as well: slots with no fill-in in training whose holders are far more common on the file's
filers than on CPS rows, so that a holder means something else. Other income is the case that matters: 2.7% of CPS tax units hold it in 2019,
and 44% in Alaska (the Permanent Fund dividend), while 58.7% of the file's filers hold it (the remainder of AGI on the PUF). Shown, it put
1.46 million returns in Alaska in the 2019 start against the IRS's 0.36 million. Non-filers are drawn under the CPS label and keep these slots.
A dependent filer has no person items on its return (the surveys count it in the household that claims it), and the CPS has no
dependent filers: its age range and every person item are hidden rather than shown as none.
The chances over the 51 are the network's over the slot's 52 values, the value none left out and the rest rescaled. The record's
chance of a state is the average over the three networks and over the target year's CPS years with the year mix's shares. Worked
example: a 2018 file's CPS mix is 2017 and 2019; a record read as 0.11 for California at 2017 and 0.13 at 2019 with shares 0.5 and
0.5 under each network has 0.12.

The record's PUF-only items and AGI group label are shown as well, a combination the network never saw with state in training.
"""
import argparse
import os
import time

import numpy as np
import pandas as pd
import pyarrow.parquet as pq
import torch

import common
import model as M
import survey_mix

C = len(M.CONTEXT)
P = M.PREDICTED
J_STATE = P.index('state')
N_STATES = M.N_CHOICES['state'] - 1
CPS_SLOTS_PACKED = M.SOURCE_SLOTS['cps']
AGE_COLUMNS = {'age1': 80, 'age2': 80, 'dep_age1': 85, 'dep_age2': 85, 'dep_age3': 85}   # in years in the file; the top code
PERSON = [v for v in M.EXACT_AGES + M.CPS_SLOTS if v != 'state']
HIDE_ON_FILERS = ['other_agi', 'rent_loss']   # share holding it, the file's filers against CPS tax units, 2019: 58.7% / 2.7%, 3.0% / 0.3%


def load_models(device):
    runs = [m['run'] for m in common.SETTINGS['generation']['models']]
    return [M.load_model(device, f'{common.OUT}/model/{r}/model.pt') for r in runs], runs


def present_by_year():
    """Per CPS year the slots some row of that year holds (present in training), and the slots zero on every CPS row, from the
    training table."""
    slots = [v for v in P if 'cps' in M.PRESENT_ON[v] and v not in M.REPORTED]
    t = pq.read_table(common.TABLE, columns=['source', 'year'] + [M.column(v) for v in slots], filters=[('source', '=', 'cps')]).to_pandas()
    have = {int(y): {v for v in slots if (g[M.column(v)] >= 0).any()} for y, g in t.groupby('year')}
    zero = [v for v in slots if (t[M.column(v)] <= 0).all() and v != 'state']
    return have, zero


def file_values(path):
    """The file's slots as full-width values [n, SLOTS], absent where the CPS does not hold the slot, and the record ids, and which
    are dependent filers."""
    names = set(pq.ParquetFile(path).schema.names)
    slots = [v for v in P if 'cps' in M.PRESENT_ON[v] and v not in M.REPORTED and v != 'state']
    cols = [M.column(v) for v in slots]
    missing = [c for c in cols if c not in names]
    assert not missing, f'{path}: no columns {missing}'
    d = pd.read_parquet(path, columns=['record_id'] + cols)
    na = d[cols].isna().sum()
    if na.any():
        print(f'{path}: bands or categories missing, shown as absent: {na[na > 0].to_dict()}', flush=True)
    values = torch.full((len(d), len(M.SLOTS)), -1, dtype=torch.long)
    for v, c in zip(slots, cols):
        x = d[c].to_numpy(float)
        if c in AGE_COLUMNS:
            assert (np.nan_to_num(x, nan=-1) >= -1).all()
            x = np.where(x >= 0, np.minimum(x, AGE_COLUMNS[c]) + 1, 0)
        x = np.nan_to_num(x, nan=-1).astype(np.int64)
        over = x >= M.N_CHOICES[v]
        assert not over.any(), f'{c}: {int(over.sum())} values above the slot\'s {M.N_CHOICES[v]} choices'
        values[:, C + P.index(v)] = torch.as_tensor(x)
    dep = d['age_range'].to_numpy() >= common.ADULT_RANGES
    return values, d['record_id'].to_numpy(), dep


def gate_shares_in_view(values):
    """The spouse's shares present exactly where training has them on a CPS row: married, and some of a base the CPS has. (The file
    holds a share of zero elsewhere.)"""
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
    """The chance of each of the 51 states, [n, 51], on the given full-width values and hidden flags."""
    pos = list(slots).index(C + J_STATE)
    st = M.slot_tensor(slots, values.device)
    keep = st[C:] - C
    out = []
    head = model.heads[J_STATE]
    for i in range(0, len(values), chunk):
        h = model.last_layer(values[i:i + chunk][:, st], hidden[i:i + chunk][:, keep], slots)[:, pos]
        out.append(head(h)[:, 1:].float().softmax(-1).cpu())   # the value none left out: the rest rescaled
    return torch.cat(out).numpy().astype(np.float64)


def cmd_start(a):
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    t0 = time.time()
    models, runs = load_models(device)
    mix = survey_mix.mix('cps', a.year)
    have, zero = present_by_year()
    print(f'zero on every CPS row, hidden: {zero}', flush=True)
    assert all(y in have for y, _ in mix), f'CPS years {mix} against the table\'s {sorted(have)}'
    values0, ids, dep = file_values(a.file)
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
    d = torch.as_tensor(dep)
    for v in PERSON + ['age_range']:
        j = P.index(v)
        hidden0[d, j] = True
        values0[d, C + j] = values0[d, C + j].clamp(min=0)
    values0 = gate_shares_in_view(values0)
    values0[:, 0] = M.SOURCES.index('cps')
    fv = {c for c in pq.ParquetFile(a.file).schema.names}
    extra = [v for v in P if v in M.OWNED_BY and M.OWNED_BY[v] == 'puf' and M.column(v) in fv and 'cps' not in M.PRESENT_ON[v]]
    ex = pd.read_parquet(a.file, columns=[M.column(v) for v in extra])
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
            for s in P:          # a slot this CPS year never holds is absent
                if 'cps' in M.PRESENT_ON[s] and s not in M.REPORTED and s != 'state' and s not in have[y]:
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
