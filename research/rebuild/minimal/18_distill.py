"""
18_distill.py  (minimal example, experiment 18: tilt-then-distill, the GPU steps)

What it does
  The v4b post-training is a reweighting: 400,000 records drawn from the
  pretrained model V4 (the v4b stack) get weights exp(lambda . f(record))
  that hit the 136 IRS targets exactly and eight wealth targets softly
  (tilt/v4b_allunits/weights_v4b.parquet, column weight_after). This file
  fine-tunes a copy of V4 so that the network itself, sampled with equal
  weights by the same procedure as v4b, generates that tilted distribution.
  The report is 18_DISTILL.md.

  The sampler being fine-tuned is the v4b one (03_generate.generate with
  borrowed={'label': 'owner', 'foreign': 'absent'}): the five incomes under
  the PUF label in a random order, then net worth under the SCF label with
  other_agi left out, then married and n_dep under the PUF label with net
  worth visible. The fine-tune trains each of those questions on the tilted
  records, and no others:

    PUF view   each tilted record as a PUF record: the five incomes, married
               and n_dep, scored under masked any-subset hiding, the same
               objective as pretraining. The net worth slot is present as an
               input but never scored under the PUF label: it is visible when
               all five incomes are visible and masked otherwise, which is
               exactly what the sampler shows the network (net worth is drawn
               after the incomes and before married and n_dep).
    SCF view   each tilted record relabelled SCF, other_agi left out, the
               four shared incomes visible, married and n_dep masked, net
               worth hidden and scored. This is exactly the question the
               sampler asks when it draws net worth (decision 16 of
               design/plan.md), so net worth is learned the same way as in
               pretraining: from records the network sees as SCF records.

  Training weight of both views: the tilted weight (weight_after), which is
  the tilt factor times the importance weight of the oversampled stack.
  5% of the stack records are held out from the fine-tune and used only to
  score it. Optional replay: the original pretraining records (2015 PUF and
  2016 SCF, plain-rule weights, masked any-subset) mixed in with a set share
  of the total weight.

Usage (through Slurm, 18_distill.sbatch), from the repo root:
  python 18_distill.py train <variant>      fine-tune, write model/D18/<variant>/
  python 18_distill.py generate <variant>   plain file and oversampled stack via
                                            03_stack.py pointed at that model
  python 18_distill.py logprob <variant>    record log-probabilities under V4 and
                                            the fine-tuned model; the move in nats
Variants: see VARIANTS below.

Writes under /nfs/roberts/scratch/pi_nrs36/jar335/minimal/:
  model/D18/<v>/model.pt, train_log.csv, train_meta.json
  gen/D18/<v>/gen_puf_plain_d18<v>.parquet (200,000 records, seed 3, no
      oversampling), gen_puf_stack_d18<v>.parquet (400,000, oversampled as
      v4b), gen_plain_seed35_d18<v>.parquet, gen_meta_d18<v>.json,
      03_stack_tables_d18<v>.md
  checks/D18/<v>/logprob_summary.csv, logprob_checks.csv
"""
import json
import math
import os
import subprocess
import sys
import time
import importlib.util

R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
os.environ.setdefault('MINIMAL_TABLE', 'E4')
os.environ.setdefault('MINIMAL_RUN', 'V4')
os.environ.setdefault('MINIMAL_ALL_RECORDS', '1')

import numpy as np
import pandas as pd
import torch

HERE = os.path.dirname(os.path.abspath(__file__))


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


m = load('m18', '02_model.py')
assert m.TABLE == 'E4' and m.ALL_RECORDS, 'the V4 settings are needed'

V4_DIR = f'{R}/model/V4'
STACK = f'{R}/gen/gen_puf_stack_v4b.parquet'
TILT_WEIGHTS = f'{R}/tilt/v4b_allunits/weights_v4b.parquet'
V4B_PLAIN = f'{R}/gen/V4b/gen_puf_with_net_worth_ownerlabel_absent.parquet'
HOLDOUT_SEED, HOLDOUT_SHARE, SEED = 20261018, 0.05, 20261019

# peak_lr: the top of the learning-rate schedule (pretraining peaked at 1e-3
# and ended at 5e-5). replay: share of the total training weight given to the
# original pretraining records (0 = none).
VARIANTS = {
    'A':   dict(epochs=8, peak_lr=2e-4, replay=0.0),
    'A20': dict(epochs=20, peak_lr=2e-4, replay=0.0),
    'B':   dict(epochs=8, peak_lr=2e-4, replay=1 / 3),
    # C: as A, but each pass uses a different file of 400,000 fresh records
    # from V4 with the tilt formula applied (18_fresh.py): no record seen twice.
    'C':   dict(epochs=8, peak_lr=2e-4, replay=0.0, fresh=True),
}
FRESH = f'{R}/gen/D18/fresh'

NC = len(m.CONTEXT)
J = {v: m.PREDICTED.index(v) for v in m.PREDICTED}
INCOMES = ['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']
INC = [J[v] for v in INCOMES]
NW, OA, MAR, DEP = J['net_worth'], J['other_agi'], J['married'], J['n_dep']
PUF, SCF = m.SOURCES.index('puf'), m.SOURCES.index('scf')
PUF_SLOTS = [J[v] for v in ['married', 'n_dep'] + INCOMES]


def model_dir(v):
    return f'{R}/model/D18/{v}'


def gen_dir(v):
    return f'{R}/gen/D18/{v}'


# ---------------------------------------------------------------- data

def tilted_stack():
    """The v4b stack with its tilted weights and the hold-out flag."""
    stack = pd.read_parquet(STACK)
    w = pd.read_parquet(TILT_WEIGHTS)
    assert len(w) == len(stack) and (w['record_id'].to_numpy() == np.arange(len(stack))).all()
    assert (w['stratifier'].to_numpy() == stack['stratifier'].to_numpy()).all()
    assert (stack['source'] == 'puf').all() and (stack['net_worth_band'] >= 0).all()
    stack['w_pre'] = w['weight_before'].to_numpy(float)
    stack['w_tilt'] = w['weight_after'].to_numpy(float)
    assert np.isfinite(stack['w_tilt']).all() and (stack['w_tilt'] >= 0).all()
    stack['holdout'] = np.random.default_rng(HOLDOUT_SEED).random(len(stack)) < HOLDOUT_SHARE
    return stack


def values_of(df, device):
    """[n, slots] band/value indices as 02_model.encode, -1 where absent."""
    cols = [df['source'].map(m.SOURCES.index), df['year'].map(m.YEARS.index),
            df['married'], df['n_dep']] + [df[a + '_band'] for a in m.AMOUNTS]
    v = np.column_stack([np.asarray(c, dtype=np.int64) for c in cols])
    return torch.tensor(v, device=device)


def puf_view(values, gen):
    """PUF view: any-subset hiding over the seven PUF slots; net worth input
    visible only when every income is visible; net worth never scored."""
    v = values.clone()
    v[:, 0] = PUF
    tmp = v.clone()
    tmp[:, NC + NW] = -1                       # not a candidate for hiding
    scored = m.draw_hidden(tmp, gen)
    inp = scored.clone()
    inp[:, NW] = scored[:, INC].any(1)
    return v, inp, scored


def scf_view(values):
    """SCF view: the sampler's net worth question."""
    v = values.clone()
    v[:, 0] = SCF
    v[:, NC + OA] = -1
    inp = torch.zeros(len(v), len(m.PREDICTED), dtype=torch.bool, device=v.device)
    inp[:, [NW, MAR, DEP]] = True
    scored = torch.zeros_like(inp)
    scored[:, NW] = True
    return v, inp, scored


def view_loss(model, v, inp, scored, w):
    nll = m.slot_nll(m.predict(model, v, inp), v)
    ww = w[:, None] * scored
    return (nll * ww).sum(), ww.sum()


@torch.no_grad()
def eval_losses(model, sets, gen_seed=SEED + 7, draws=4):
    """Weighted cross-entropy per scored slot (nats) on fixed hide draws."""
    model.eval()
    out = {}
    for name, (values, w, kind) in sets.items():
        g = torch.Generator(device=values.device).manual_seed(gen_seed)
        num = den = 0.0
        for _ in range(draws if kind != 'scf' else 1):
            for i in range(0, len(values), 50_000):
                vv, ww = values[i:i + 50_000], w[i:i + 50_000]
                if kind == 'puf':
                    a, b, c = puf_view(vv, g)
                elif kind == 'scf':
                    a, b, c = scf_view(vv)
                else:                              # original records, any-subset
                    a, b = vv, m.draw_hidden(vv, g)
                    c = b
                s, d = view_loss(model, a, b, c, ww)
                num, den = num + s.item(), den + d.item()
        out[name] = num / den
    return out


# ---------------------------------------------------------------- train

def train(variant):
    cfg = VARIANTS[variant]
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(SEED)
    gen = torch.Generator(device=device).manual_seed(SEED)
    os.makedirs(model_dir(variant), exist_ok=True)
    model = m.load_model(device, V4_DIR)

    stack = tilted_stack()
    tr, ho = stack[~stack.holdout], stack[stack.holdout]
    vt, vh = values_of(tr, device), values_of(ho, device)
    wt_ = tr['w_tilt'].to_numpy(float)
    wt_ = torch.tensor(wt_ / wt_.mean(), dtype=torch.float32, device=device)
    wh = ho['w_tilt'].to_numpy(float)
    wh = torch.tensor(wh / wh.mean(), dtype=torch.float32, device=device)

    table = m.read_table()                     # ALL_RECORDS: split train / test (held back)
    orig = {s: m.encode(table[table.split == s], device) for s in ['train', 'test']}
    held = table[table.split == 'test']
    held_sets = {}
    for src in m.SOURCES:
        e = m.encode(held[held.source == src], device)
        held_sets[f'held-back 2015 {src.upper()}'] = (e['values'], e['weight'], 'orig')
    eval_sets = {'held-out tilted, PUF view': (vh, wh, 'puf'),
                 'held-out tilted, SCF view (net worth)': (vh, wh, 'scf'), **held_sets}

    def fresh_file(k):
        f = pd.read_parquet(f'{FRESH}/fresh_{k}.parquet')
        w = pd.read_parquet(f'{FRESH}/fresh_{k}_w.parquet')['w_tilt'].to_numpy(float)
        assert len(w) == len(f) and np.isfinite(w).all()
        return values_of(f, device), torch.tensor(w / w.mean(), dtype=torch.float32, device=device)
    if cfg.get('fresh'):
        vt, wt_ = fresh_file(0)

    replay = cfg['replay']
    if replay > 0:
        ro_v, ro_w = orig['train']['values'], orig['train']['weight']
        # total replay weight = replay / (1 - replay) x total tilted record weight
        ro_w = ro_w * (replay / (1 - replay)) * wt_.sum() / ro_w.sum()

    n_rows = 2 * len(vt) + (len(ro_v) if replay > 0 else 0)
    batch = 4096
    steps_per_epoch = math.ceil(n_rows / batch)
    total = cfg['epochs'] * steps_per_epoch
    warm = max(1, int(0.05 * total))
    opt = torch.optim.AdamW(model.parameters(), lr=cfg['peak_lr'], weight_decay=0.01)

    def lr_factor(step):                       # warm-up, then cosine to 10% of the peak
        if step < warm:
            return (step + 1) / warm
        prog = (step - warm) / max(1, total - warm)
        return 0.10 + 0.90 * 0.5 * (1 + math.cos(math.pi * prog))
    sched = torch.optim.lr_scheduler.LambdaLR(opt, lr_factor)

    log = [{'epoch': 0, 'train_loss': np.nan, 'seconds': 0.0, **eval_losses(model, eval_sets)}]
    print(log[-1], flush=True)
    began = time.time()
    for epoch in range(1, cfg['epochs'] + 1):
        t0 = time.time()
        model.train()
        if cfg.get('fresh') and epoch > 1:
            vt, wt_ = fresh_file(epoch - 1)
        a1, b1, c1 = puf_view(vt, gen)
        a2, b2, c2 = scf_view(vt)
        V, INP, SC, W = [a1, a2], [b1, b2], [c1, c2], [wt_, wt_]
        if replay > 0:
            V.append(ro_v)
            h = m.draw_hidden(ro_v, gen)
            INP.append(h)
            SC.append(h)
            W.append(ro_w)
        V, INP, SC, W = (torch.cat(x) for x in (V, INP, SC, W))
        order = torch.randperm(len(V), generator=gen, device=device)
        tot = 0.0
        for s in range(0, len(V), batch):
            idx = order[s:s + batch]
            num, den = view_loss(model, V[idx], INP[idx], SC[idx], W[idx])
            loss = num / den
            opt.zero_grad()
            loss.backward()
            opt.step()
            sched.step()
            tot += loss.item() * len(idx)
        row = {'epoch': epoch, 'train_loss': tot / len(V), 'seconds': time.time() - t0,
               'lr': sched.get_last_lr()[0], **eval_losses(model, eval_sets)}
        log.append(row)
        print(row, flush=True)
    minutes = (time.time() - began) / 60
    model.eval()
    torch.save({'state': model.state_dict(), 'settings': model.settings,
                'band_layout': m.BAND_LAYOUT}, f'{model_dir(variant)}/model.pt')
    pd.DataFrame(log).to_csv(f'{model_dir(variant)}/train_log.csv', index=False)
    meta = dict(variant=variant, **cfg, start_model=f'{V4_DIR}/model.pt', stack=STACK,
                tilt_weights=TILT_WEIGHTS, holdout_share=HOLDOUT_SHARE,
                tilted_records_train=int(len(vt)) * (cfg['epochs'] if cfg.get('fresh') else 1), tilted_records_holdout=int(len(vh)),
                replay_records=int(len(ro_v)) if replay > 0 else 0, batch=batch,
                steps=total, warmup_steps=warm, minutes_training=minutes,
                device=torch.cuda.get_device_name() if device == 'cuda' else 'cpu',
                flags=m.flags())
    json.dump(meta, open(f'{model_dir(variant)}/train_meta.json', 'w'), indent=2)
    print(json.dumps(meta, indent=2), flush=True)


# ---------------------------------------------------------------- generate

def generate(variant):
    """03_stack.py with the model folder pointed at the fine-tuned model:
    the 200,000-record plain file (seed 3) and the four-part stack."""
    gd = gen_dir(variant)
    os.makedirs(gd, exist_ok=True)
    env = dict(os.environ, MINIMAL_MODEL_DIR=model_dir(variant), MINIMAL_GEN_DIR=gd,
               MINIMAL_STACK_DIR=gd, STACK_VERSION=f'd18{variant}', MINIMAL_STACK_LABEL='scf',
               MINIMAL_STACK_FOREIGN='absent',
               MINIMAL_STACK_PLAIN=f'{gd}/gen_puf_plain_d18{variant}.parquet')
    t0 = time.time()
    subprocess.run([sys.executable, os.path.join(HERE, '03_stack.py')], env=env, check=True)
    json.dump({'minutes': (time.time() - t0) / 60}, open(f'{gd}/generate_time.json', 'w'))


# ---------------------------------------------------------------- log-probabilities

@torch.no_grad()
def log_q(model, values, chunk=50_000):
    """log-probability of each PUF record (with net worth) under the v4b
    sampler. The five incomes are drawn in a uniformly random order, so the
    sampler's probability is the average over the 120 orders of the product
    of conditionals; it is summed over orders exactly, from the 32 forward
    passes that show each subset of incomes. Then net worth (SCF view,
    other_agi left out), married, n_dep. Returns a dict of [n] arrays:
    incomes, net_worth, married, n_dep, total."""
    model.eval()
    parts = {k: [] for k in ['incomes', 'net_worth', 'married', 'n_dep']}
    for i in range(0, len(values), chunk):
        v = values[i:i + chunk].clone()
        v[:, 0] = PUF
        n = len(v)
        tgt = v[:, NC:].clamp(min=0)
        cond = {}
        for S in range(32):
            hid = torch.zeros(n, len(m.PREDICTED), dtype=torch.bool, device=v.device)
            for k, j in enumerate(INC):
                if not (S >> k) & 1:
                    hid[:, j] = True
            hid[:, [NW, MAR, DEP]] = True
            lp = m.predict(model, v, hid)
            cond[S] = torch.stack([lp[:, j].gather(1, tgt[:, j:j + 1]).squeeze(1) for j in INC], 1).double()
        logA = {0: torch.zeros(n, dtype=torch.float64, device=v.device)}
        for S in sorted(range(1, 32), key=lambda s: bin(s).count('1')):
            terms = [logA[S ^ (1 << k)] + cond[S ^ (1 << k)][:, k] for k in range(5) if (S >> k) & 1]
            logA[S] = torch.logsumexp(torch.stack(terms), 0)
        parts['incomes'].append(logA[31] - math.log(120))
        vs, hid = v.clone(), torch.zeros(n, len(m.PREDICTED), dtype=torch.bool, device=v.device)
        vs[:, 0], vs[:, NC + OA] = SCF, -1
        hid[:, [NW, MAR, DEP]] = True
        parts['net_worth'].append(m.predict(model, vs, hid)[:, NW].gather(1, tgt[:, NW:NW + 1]).squeeze(1).double())
        hid = torch.zeros_like(hid)
        hid[:, [MAR, DEP]] = True
        parts['married'].append(m.predict(model, v, hid)[:, MAR].gather(1, tgt[:, MAR:MAR + 1]).squeeze(1).double())
        hid = torch.zeros_like(hid)
        hid[:, DEP] = True
        parts['n_dep'].append(m.predict(model, v, hid)[:, DEP].gather(1, tgt[:, DEP:DEP + 1]).squeeze(1).double())
        # the sampler's first wages draw, for the check against p_band
        parts.setdefault('wages_first', []).append(cond[0][:, 0])
    out = {k: torch.cat(x).cpu().numpy() for k, x in parts.items()}
    out['total'] = out['incomes'] + out['net_worth'] + out['married'] + out['n_dep']
    return out


def logprob(variant):
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    out_dir = f'{R}/checks/D18/{variant}'
    os.makedirs(out_dir, exist_ok=True)
    base = m.load_model(device, V4_DIR)
    ft = m.load_model(device, model_dir(variant))
    stack = tilted_stack()
    files = {
        'fine-tuned plain file (draws from the fine-tuned model)':
            (pd.read_parquet(f'{gen_dir(variant)}/gen_puf_plain_d18{variant}.parquet'), None),
        'v4b plain file (draws from V4)': (pd.read_parquet(V4B_PLAIN), None),
        'tilted stack, held-out records (draws from the tilted distribution)':
            (stack[stack.holdout], 'w_tilt'),
        'tilted stack, records used in the fine-tune': (stack[~stack.holdout], 'w_tilt'),
        'v4b stack before the tilt, all records (draws from V4)': (stack, 'w_pre'),
    }
    rows, checks = [], []
    for name, (df, wcol) in files.items():
        df = df[df.source == 'puf'].reset_index(drop=True)
        vals = values_of(df, device)
        assert (vals[:, NC:] >= 0).all(), 'every PUF slot and net worth must be present'
        lb, lf = log_q(base, vals), log_q(ft, vals)
        w = np.ones(len(df)) if wcol is None else df[wcol].to_numpy(float)
        p = w / w.sum()
        ess = 1 / (p ** 2).sum()
        r = {'file': name, 'records': len(df), 'effective records': ess}
        for k in ['total', 'incomes', 'net_worth', 'married', 'n_dep']:
            d = lf[k] - lb[k]
            r[f'mean log ratio fine-tuned/V4: {k}'] = float(p @ d)
            if k == 'total':
                r['standard error of the total'] = float(np.sqrt((p ** 2 * (d - p @ d) ** 2).sum()))
        r['mean log q under V4'] = float(p @ lb['total'])
        r['mean log q under the fine-tuned model'] = float(p @ lf['total'])
        rows.append(r)
        if name.startswith('v4b stack before'):
            # Check log_q against the probabilities recorded at generation.
            for strat, comp in [('net_worth', 'net_worth'), ('wages', 'wages_first')]:
                sel = (df['stratifier'] == strat).to_numpy()
                diff = np.abs(np.exp(lb[comp][sel]) - df.loc[sel, 'p_band'].to_numpy(float))
                checks.append({'check': f'V4 {comp} probability vs p_band of the {strat} file',
                               'records': int(sel.sum()), 'max abs difference': float(diff.max()),
                               'mean p_band': float(df.loc[sel, 'p_band'].mean())})
            # KL of the tilt measured on the same stack: E_tilt[log w_tilt/w_pre]
            pt = df['w_tilt'].to_numpy(float)
            pt, p0 = pt / pt.sum(), p
            pos = pt > 0
            rows[-1]['tilt move on this stack (nats), for reference'] = float(
                (pt[pos] * np.log(pt[pos] / p0[pos])).sum())
        print(r, flush=True)
    pd.DataFrame(rows).to_csv(f'{out_dir}/logprob_summary.csv', index=False)
    pd.DataFrame(checks).to_csv(f'{out_dir}/logprob_checks.csv', index=False)
    print(pd.DataFrame(checks).to_string(), flush=True)


if __name__ == '__main__':
    what, variant = sys.argv[1], sys.argv[2]
    assert variant in VARIANTS, variant
    {'train': train, 'generate': generate, 'logprob': logprob}[what](variant)
