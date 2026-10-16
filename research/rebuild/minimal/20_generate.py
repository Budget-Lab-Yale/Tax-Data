"""
20_generate.py  (minimal example, step 20: the oversampled stacks for arms A, B and C)

20_SEPARATE_MODELS.md asks whether the joint model adds anything over
separate models plus calibration. This file builds the four-file stack of
v4b (03_stack.py) for three ways of drawing a record:

  A  v4b as it is (03_stack.build_stack with net worth under the SCF label,
     other_agi left out). Used here only for the second training seed;
     the seed-1 stack is gen/gen_puf_stack_v4b.parquet.
  B  separate models. The seven PUF variables come from a model trained on
     the PUF records only (B1, run S20_PUF): incomes in a random order, then
     married, then n_dep, exactly as a PUF record is generated. Net worth
     then comes from a model trained on the SCF records only (B2, run
     S20_SCF), as an SCF record: SCF label, married, n_dep, wages, interest,
     dividends and capital gains visible, other_agi absent.
  C  the joint model V4 drawn in the order of B: every PUF variable first
     under the PUF label (no net worth slot at all, as on a PUF training
     record), then net worth last as an SCF record would see it. Same code
     as B with the V4 model in both places.

Stack design, the same as v4b: four files of 100,000, seeds 31 to 34. In the
wages, capital_gains and other_agi files the variable's band is drawn first
from the PUF model's everything-hidden distribution with the bands of the
top 1% of positive weight boosted tenfold (03_oversample.py), weight p/q.
In the net worth file the PUF variables are drawn plainly, then net worth
from the boosted conditional of the net worth model, weight p/q with p and
q that record's own conditional values. Each file's weights are scaled to
sum to 100,000. One difference from v4b in the stratified files: v4b's
everything-hidden distribution was taken on a PUF record carrying a masked
net worth slot (03_stack: with_net_worth=True); here, as on a PUF training
record, there is no net worth slot.

Usage (GPU, through 20_run_gpu.sbatch):
  python 20_generate.py two <puf model dir> <net worth model dir> <out dir> <tag>
  python 20_generate.py A   <model dir> <out dir> <tag>
Writes in <out dir>: gen_puf_stack_<tag>.parquet, gen_meta_<tag>.json,
gen_plain_<tag>.parquet (200,000 records, seed 3, no oversampling, weight 1)
and gen_plain_seed35_<tag>.parquet (the same with seed 35).
"""
import importlib.util
import json
import os
import sys
import time

import numpy as np
import pandas as pd
import torch

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
HERE = os.path.dirname(os.path.abspath(__file__))
os.environ.setdefault('MINIMAL_TABLE', 'E4')
os.environ.setdefault('MINIMAL_RUN', 'V4')
os.environ.setdefault('MINIMAL_ALL_RECORDS', '1')
# For arm A, 03_stack's v4b rule: net worth under the SCF label, other_agi left out.
os.environ['MINIMAL_STACK_LABEL'] = 'scf'
os.environ['MINIMAL_STACK_FOREIGN'] = 'absent'
os.environ['MINIMAL_STACK_DIR'] = '/nonexistent'      # 03_stack's main is not run here

spec = importlib.util.spec_from_file_location('st', os.path.join(HERE, '03_stack.py'))
st = importlib.util.module_from_spec(spec)
spec.loader.exec_module(st)
os_, g, m = st.os_, st.g, st.m
NC = g.NC
PER_FILE, BOOST = st.PER_FILE, os_.BOOST
STRATIFIERS = st.STRATIFIERS                      # wages 31, capital_gains 32, other_agi 33, net_worth 34
NW_SEED_OFFSET = 1000                             # the net worth draw's own random stream
GIVEN = ['married', 'n_dep', 'wages', 'interest', 'dividends', 'capital_gains']   # what an SCF record has


def col(v):
    return v + '_band' if v in m.AMOUNTS else v


@torch.no_grad()
def draw_net_worth(model, recs, seed, boost=False, chunk=50_000):
    """Net worth for each PUF-style record in recs, drawn from model as an
    SCF record would be: SCF label, the six shared variables visible,
    other_agi absent, net worth the one hidden slot. boost: draw from q =
    the model's p with the top-1% net worth bands times BOOST, and return
    p and q of the drawn band."""
    device = next(model.parameters()).device
    gen = torch.Generator(device=device).manual_seed(seed)
    j = m.PREDICTED.index('net_worth')
    top = torch.as_tensor(m.positive_bands('net_worth', 0.99, 1.0), device=device)
    bands, ps, qs = [], [], []
    for start in range(0, len(recs), chunk):
        part = recs.iloc[start:start + chunk]
        n = len(part)
        values = torch.full((n, len(m.SLOTS)), -1, dtype=torch.long, device=device)
        values[:, 0] = m.SOURCES.index('scf')
        values[:, 1] = m.YEARS.index(2015)
        for v in GIVEN:
            x = torch.as_tensor(part[col(v)].to_numpy(np.int64), device=device)
            assert (x >= 0).all(), v
            values[:, NC + m.PREDICTED.index(v)] = x
        values[:, NC + j] = 0                                  # present, hidden
        hidden = torch.zeros(n, len(m.PREDICTED), dtype=torch.bool, device=device)
        hidden[:, j] = True
        p = m.predict(model, values, hidden)[:, j].exp()
        q = p
        if boost:
            q = p.clone()
            q[:, top] *= BOOST
            q = q / q.sum(1, keepdim=True)
        draw = torch.multinomial(q, 1, generator=gen).squeeze(1)
        rows = torch.arange(n, device=device)
        bands.append(draw.cpu())
        ps.append(p[rows, draw].double().cpu())
        qs.append(q[rows, draw].double().cpu())
    out = recs.copy()
    out['net_worth_band'] = torch.cat(bands).numpy().astype(out['net_worth_band'].dtype)
    return out, torch.cat(ps).numpy(), torch.cat(qs).numpy()


def puf_records(model, n, seed, preset=None):
    """The seven PUF variables as a PUF record is generated: no net worth slot."""
    t = g.generate(model, n, 'puf', with_net_worth=False, fixed_order=False, seed=seed, preset=preset)
    assert (t['net_worth_band'] == -1).all()
    return t


def build_two_stage(puf_model, nw_model):
    parts = []
    for var, seed in STRATIFIERS.items():
        if var == 'net_worth':
            part = puf_records(puf_model, PER_FILE, seed)
            part, p, q = draw_net_worth(nw_model, part, seed + NW_SEED_OFFSET, boost=True)
            part['weight'], part['p_band'], part['q_band'] = p / q, p, q
        else:
            p = os_.everything_hidden(puf_model, var, with_net_worth=False)
            q = os_.sampling_distribution(p, var)
            b = torch.multinomial(torch.tensor(q), PER_FILE, replacement=True,
                                  generator=torch.Generator().manual_seed(seed))
            part = puf_records(puf_model, PER_FILE, seed, preset={var: b})
            bn = b.numpy()
            part['weight'], part['p_band'], part['q_band'] = p[bn] / q[bn], p[bn], q[bn]
            part, _, _ = draw_net_worth(nw_model, part, seed + NW_SEED_OFFSET)
        part['weight'] *= PER_FILE / part['weight'].sum()
        part['stratifier'] = var
        parts.append(part)
        print(var, 'done', flush=True)
    return pd.concat(parts, ignore_index=True)


def plain_two_stage(puf_model, nw_model, seed, n=200_000):
    t = puf_records(puf_model, n, seed)
    t, _, _ = draw_net_worth(nw_model, t, seed + NW_SEED_OFFSET)
    return t


def main():
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(m.SEED)
    mode = sys.argv[1]
    began = time.time()
    if mode == 'two':
        puf_dir, nw_dir, out_dir, tag = sys.argv[2:6]
        puf_model = m.load_model(device, puf_dir)
        nw_model = puf_model if os.path.abspath(nw_dir) == os.path.abspath(puf_dir) else m.load_model(device, nw_dir)
        stack = build_two_stage(puf_model, nw_model)
        plains = {s: plain_two_stage(puf_model, nw_model, s) for s in [3, 35]}
        what = ('PUF variables from the PUF model as a PUF record (no net worth slot; incomes in '
                'random order, then married, then n_dep); then net worth from the net worth model '
                'as an SCF record (SCF label, married, n_dep and the four shared incomes visible, '
                'other_agi absent)')
        models = {'puf variables': os.path.join(puf_dir, 'model.pt'),
                  'net worth': os.path.join(nw_dir, 'model.pt')}
    elif mode == 'A':
        model_dir, out_dir, tag = sys.argv[2:5]
        model = m.load_model(device, model_dir)
        stack = st.build_stack(model)
        plains = {s: g.generate(model, 200_000, 'puf', with_net_worth=True, fixed_order=False,
                                seed=s, borrowed=st.BORROWED) for s in [3, 35]}
        what = 'v4b: 03_stack.build_stack, net worth under the SCF label with other_agi left out'
        models = {'all': os.path.join(model_dir, 'model.pt')}
    else:
        raise SystemExit(f'unknown mode {mode}')
    os.makedirs(out_dir, exist_ok=True)
    assert (stack['source'] == 'puf').all() and (stack['net_worth_band'] >= 0).all()
    assert (stack['other_agi_band'] >= 0).all()
    stack.to_parquet(os.path.join(out_dir, f'gen_puf_stack_{tag}.parquet'), index=False)
    plains[3].to_parquet(os.path.join(out_dir, f'gen_plain_{tag}.parquet'), index=False)
    plains[35].to_parquet(os.path.join(out_dir, f'gen_plain_seed35_{tag}.parquet'), index=False)
    sums = stack.groupby('stratifier')['weight'].sum().to_dict()
    meta = {'file': f'gen_puf_stack_{tag}.parquet', 'mode': mode, 'what': what, 'models': models,
            'records': len(stack), 'weight_sum': float(stack['weight'].sum()), 'weight_sums': sums,
            'boost': BOOST, 'seeds': STRATIFIERS, 'net_worth_seed_offset': NW_SEED_OFFSET,
            'plain_files': {'seed 3': f'gen_plain_{tag}.parquet', 'seed 35': f'gen_plain_seed35_{tag}.parquet'},
            'gpu': torch.cuda.get_device_name(0) if device == 'cuda' else 'cpu',
            'minutes': (time.time() - began) / 60, 'flags': m.flags()}
    with open(os.path.join(out_dir, f'gen_meta_{tag}.json'), 'w') as f:
        json.dump(meta, f, indent=2, default=str)
    print(json.dumps(meta, indent=2, default=str))


if __name__ == '__main__':
    main()
