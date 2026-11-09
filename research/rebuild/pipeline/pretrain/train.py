"""Pretraining: trains one network on the training table and saves it, with its logs, in the model folder.

Each row of the training table is a tax unit from one source (PUF, SCF, CEX, CPS or SIPP). Its variables are stored
as dollar bands or categories, one per "slot". In each batch, some of a record's known values are hidden and the
network predicts them from the values that are shown. The loss measures how far the predictions are from the hidden
values, and each batch adjusts the network to reduce it. Training goes over every record a fixed number of times
("passes"). A random 2% of the records form a test set that is not used for training; its loss is reported after
each pass. FEATURES.md, "2. Pretraining", describes the method.
"""
import json
import math
import os
import time

import numpy as np
import pandas as pd
import torch

from shared import common
from pretrain import fillin
from pretrain import model as M
from pretrain import qgroups

S = common.SETTINGS['translation']
T = dict(common.SETTINGS['training'], passes=common.PASSES)
C, P = len(M.CONTEXT), M.PREDICTED
J_LABEL = [P.index(v) for v in M.PUF_LABELS]       # the AGI group label's position among the predicted slots
J_AGE, J_ES, J_FILER = (C + P.index(v) for v in ('age_range', 'earn_split', 'filer'))


# ---- Reading the training table ----

def show_label(hidden, slots=None):
    """The mask of hidden values, with the AGI group label always shown. Generation always sets the label in advance,
    so training never hides it or scores it."""
    cols = list(range(len(P))) if slots is None else M.predicted_of(slots)
    for j in J_LABEL:
        if j in cols:
            hidden[:, cols.index(j)] = False
    return hidden


def encode(part, device):
    """The records of one split (training or test) as tensors: their values and weights, the probabilities used to
    draw missing age ranges, earnings split codes and filing flags, an indicator for each source, and the reported
    band of each item that has a fill-in."""
    d = M.encode(part, device)
    age_rows, age_probs = M.age_draws(part, device)
    es_rows, es_probs = M.earn_draws(part, device)
    filer_rows, filer_p = M.filer_draws(part, device)
    return dict(values=d['values'], weight=d['weight'], pop_weight=d['pop_weight'], age_rows=age_rows, age_probs=age_probs,
                es_rows=es_rows, es_probs=es_probs, filer_rows=filer_rows, filer_p=filer_p,
                **{src: torch.tensor((part['source'] == src).to_numpy(), device=device) for src in M.SOURCES},
                rep_band={v: d['values'][:, C + P.index(v)].clone() for v in fillin.MAPPED})


def read_splits(device):
    """Reads the training table. Returns the table, the training records as a data frame, and the training and test
    records as tensors."""
    table = M.read_table()
    train_rows = table[table.split == 'train']
    tr = encode(train_rows, device)
    te = encode(table[table.split == 'test'], device)
    # When a survey's test record reports zero for an item that the fill-in covers, its amount on a tax-data basis is
    # unknown. That value is marked missing and is not scored.
    te_year = torch.tensor(table.loc[table.split == 'test', 'year'].to_numpy(), device=device)
    for s, v in fillin.KEYS:
        act = torch.tensor([fillin.TR.fills_in(fillin.ROW[(s, v)], int(y)) for y in M.YEARS], device=device)
        yi = torch.searchsorted(torch.tensor(M.YEARS, device=device), te_year)
        te['values'][te[s] & act[yi] & (te['rep_band'][v] == 0), C + P.index(v)] = -1
    return table, train_rows, tr, te


# ---- Drawing missing values ----
# Some records have no age range (PUF returns without demographic information), no earnings split code (joint PUF
# returns without demographic information) or no filing flag (every CPS record; the CPS does not report filing).
# These values are drawn from their probabilities. The training records get a new draw at the start of every pass.

def draw_ages(d, gen):
    if len(d['age_rows']):
        d['values'][d['age_rows'], J_AGE] = torch.multinomial(d['age_probs'], 1, generator=gen).squeeze(1)


def draw_splits(d, gen):
    if len(d['es_rows']):
        d['values'][d['es_rows'], J_ES] = torch.multinomial(d['es_probs'], 1, generator=gen).squeeze(1)


def draw_filers(d, gen):
    if len(d['filer_rows']):
        d['values'][d['filer_rows'], J_FILER] = torch.bernoulli(d['filer_p'], generator=gen).long()


def draw_unknowns(d, gen):
    draw_ages(d, gen)
    draw_splits(d, gen)
    draw_filers(d, gen)


# ---- One pass of training ----

def batches(src_rows, bs, gen, device):
    """The batches for one pass, as (slots, record indices). Each source's records are shuffled and cut into batches of
    bs records, and then all the batches are shuffled together. A batch holds records from one source only, with only
    the slots that source has."""
    out = []
    for src, rows in src_rows.items():
        rows = rows[torch.randperm(len(rows), generator=gen, device=device)]
        out += [(M.SOURCE_SLOTS[src], rows[a:a + bs]) for a in range(0, len(rows), bs)]
    return [out[k] for k in torch.randperm(len(out), generator=gen, device=device).tolist()]


def weight_per_record(tr):
    """The average, over the training records, of weight times the expected number of hidden slots. Each batch's loss
    is divided by the batch's number of records times this average, which gives a record the same influence it would
    have in a batch that mixed all sources. With n slots present, the expected number hidden is (n + 1) / 2."""
    n_present = (tr['values'][:, C:] >= 0).sum(1).double()
    return float((tr['weight'].double() * (n_present + 1) / 2).mean())


def learning_rate(opt, batches_per_pass):
    """The learning rate rises linearly during the warm-up passes, then declines along a half cosine curve to its
    final share of the peak rate."""
    total, warm = T['passes'] * batches_per_pass, T['warmup_passes'] * batches_per_pass
    floor = T['final_learning_rate_share']
    return torch.optim.lr_scheduler.LambdaLR(
        opt, lambda s: (s + 1) / warm if s < warm else
        floor + (1 - floor) * 0.5 * (1 + math.cos(math.pi * (s - warm) / max(1, total - warm))))


def train_batches(model, opt, sched, fill, tr, plan, gen, per_record, device):
    """One pass over the training records. For each batch: hide some known values, have the network predict them,
    compute the loss, and update the network. fill is the fill-in, or None in the passes before it starts. Returns
    the average loss."""
    tot, seen = 0.0, 0
    for slots, idx in plan:
        if fill is not None:
            fill.draw(idx)
        values, weight = tr['values'][idx][:, M.slot_tensor(slots, device)], tr['weight'][idx]
        values, hidden = M.gate_shares(values, show_label(M.draw_hidden(values, gen), slots), slots)
        with torch.autocast('cuda', dtype=torch.bfloat16):
            loss = M.masked_loss(model, values, weight, hidden, slots, len(idx) * per_record)
        opt.zero_grad()
        loss.backward()
        opt.step()
        sched.step()
        tot += loss.item() * len(idx)
        seen += len(idx)
    return tot / seen


# ---- Test loss and logs ----

def held_back_loss(model, te, hidden_draws):
    """The test records' loss, overall and by source, averaged over the given choices of which values are hidden."""
    overall, by_src = [], {s: [] for s in M.SOURCES}
    for h in hidden_draws:
        num, den = {}, {}
        for src in M.SOURCES:
            r = te[src].nonzero().squeeze(1)
            sl = M.SOURCE_SLOTS[src]
            vals, hid = M.gate_shares(*M.pack(te['values'][r], h[r], sl), sl)
            ch = M.safe_chunk(vals.shape[1])
            sums = [M.masked_sums(model, vals[i:i + ch], te['weight'][r][i:i + ch], hid[i:i + ch], sl)
                    for i in range(0, len(vals), ch)]
            num[src], den[src] = sum(float(a) for a, _ in sums), sum(float(b) for _, b in sums)
        overall.append(sum(num.values()) / sum(den.values()))
        for src in M.SOURCES:
            by_src[src].append(num[src] / den[src])
    return float(np.mean(overall)), {s: float(np.mean(x)) for s, x in by_src.items()}


@torch.no_grad()
def log_variables(model, te, logged, var_log, epoch, device):
    """For each source's test records and each of its items: the weighted share of records with a nonzero amount,
    actual and predicted. The prediction is made twice, once with only that item hidden and once with every item
    hidden. Writes var_log.csv."""
    model.eval()
    for src, names in logged.items():
        hb = te[src].nonzero().squeeze(1)
        slots = M.SOURCE_SLOTS[src]
        hb_vals, hb_w = te['values'][hb][:, M.slot_tensor(slots, device)], te['pop_weight'][hb].double()
        present = hb_vals[:, C:] >= 0
        ch = M.safe_chunk(len(slots))
        for v in names:
            j = M.predicted_of(slots).index(P.index(v))
            ok = present[:, j]
            only = torch.zeros_like(present)
            only[:, j] = True
            row = {'pass': epoch, 'variable': v,
                   'holders, actual (%)': 100 * float((hb_w * (ok & (hb_vals[:, C + j] != 0))).sum() / (hb_w * ok).sum())}
            for lab, hid in [('only this hidden', only & present), ('all hidden', present)]:
                p0 = torch.cat([model(hb_vals[i:i + ch], hid[i:i + ch], slots)[:, j, 0].exp()
                                for i in range(0, len(hb_vals), ch)])
                row[f'holders, {lab} (%)'] = 100 * float((hb_w * ok * (1 - p0.double())).sum() / (hb_w * ok).sum())
            var_log.append(row)
    pd.DataFrame(var_log).to_csv(os.path.join(common.MODEL, 'var_log.csv'), index=False)
    model.train()


# ---- The whole run ----

def main():
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(common.SEED)
    gen = torch.Generator(device=device).manual_seed(common.SEED)
    os.makedirs(common.MODEL, exist_ok=True)

    # 1. Read the records and split them into training and test sets. Draw the test records' missing values (once;
    #    they are not redrawn). The training records' filing flags are also drawn here, before the fill-in is set up.
    table, train_rows, tr, te = read_splits(device)
    draw_unknowns(te, gen)
    draw_filers(tr, gen)

    # 2. Set up the network, the fill-in, the optimizer and the learning rate.
    model = M.new_model(device)
    fill = fillin.FillIn(model, tr, train_rows, gen)
    opt = torch.optim.AdamW(model.parameters(), lr=T['peak_learning_rate'], weight_decay=T['weight_decay'])
    bs = T['batch']
    src_rows = {src: tr[src].nonzero().squeeze(1) for src in M.SOURCES}
    batches_per_pass = sum(math.ceil(len(r) / bs) for r in src_rows.values())
    per_record = weight_per_record(tr)
    gpu = torch.cuda.get_device_name() if device == "cuda" else "cpu"
    common.say(f'device {gpu}; precision bf16; fill-in chunk {fill.chunk} records')
    common.say(f'batches per pass {batches_per_pass}; weight x expected hidden slots per record {per_record:.3f}')
    common.say("grouped weighting: {'puf': 'agi12', 'scf': 'income5'}")
    common.say(qgroups.describe(table).to_string(index=False, float_format=lambda x: f'{x:.6g}'))
    common.say('training share by source (unchanged by q): '
               + str(train_rows.groupby('source').size().div(len(train_rows)).round(4).to_dict()))
    sched = learning_rate(opt, batches_per_pass)

    # 3. Choose which values of each test record are hidden when the test loss is computed. Four such choices are made
    #    here and kept for every pass, and the test loss is the average over the four. Then list the items that
    #    var_log.csv reports (each source's own items).
    val_hidden = [show_label(M.draw_hidden(te['values'], gen)) for _ in range(4)]
    logged = {'puf': [v for v in M.AMOUNTS if f'puf/{common.PUF_YEAR}/{v}' in common.edges()], 'cex': M.CEX_SLOTS,
              'cps': M.CPS_SLOTS, 'sipp': M.SIPP_SLOTS}
    var_log, log, began = [], [], time.time()
    cuda = device == 'cuda'
    sync = (lambda: torch.cuda.synchronize()) if cuda else (lambda: None)

    # 4. The passes. In each pass: shuffle the batches, redraw the training records' missing values, refit the fill-in
    #    (once it has started), train on every batch, compute the test loss, and write the logs.
    for epoch in range(1, T['passes'] + 1):
        model.train()
        if cuda:
            torch.cuda.reset_peak_memory_stats()
        sync()
        t0 = time.time()
        plan = batches(src_rows, bs, gen, device)
        draw_unknowns(tr, gen)
        filling = epoch >= S['fill_from_pass']
        if filling:
            fill.refit(epoch)
        sync()
        t_refit = time.time() - t0
        train_loss = train_batches(model, opt, sched, fill if filling else None, tr, plan, gen, per_record, device)
        sync()
        t_batches = time.time() - t0 - t_refit
        model.eval()
        with torch.no_grad():
            val, val_src = held_back_loss(model, te, val_hidden)
        sync()
        row = dict(epoch=epoch, train_loss=train_loss, held_back_loss=val, seconds=time.time() - t0,
                   seconds_refit=t_refit, seconds_batches=t_batches,
                   peak_gpu_gb=torch.cuda.max_memory_allocated() / 1e9 if cuda else float('nan'),
                   **{f'held_back_{s}': x for s, x in val_src.items()})
        log.append(row)
        pd.DataFrame(log).to_csv(os.path.join(common.MODEL, 'train_log.csv'), index=False)
        common.say(f'pass {epoch:3d}  train {train_loss:.4f}  held back {val:.4f} '
                   + ' '.join(f'{s} {x:.4f}' for s, x in val_src.items())
                   + f'  {row["seconds"]:.1f}s (fill-in {t_refit:.1f}s, batches {t_batches:.1f}s)  peak {row["peak_gpu_gb"]:.2f} GB')
        if epoch == 1 or epoch % T['log_every_passes'] == 0 or epoch == T['passes']:
            log_variables(model, te, logged, var_log, epoch, device)

    # 5. Save the network and a record of the run (settings, seed, timing).
    pd.DataFrame(log).to_csv(os.path.join(common.MODEL, 'train_log.csv'), index=False)
    M.save_model(model, os.path.join(common.MODEL, 'model.pt'))
    meta = dict(seed=common.SEED, settings=common.SETTINGS, precision='bf16', shares_gated=True, gpu=gpu,
                node=os.environ.get('SLURMD_NODENAME'), job=os.environ.get('SLURM_JOB_ID'),
                minutes=(time.time() - began) / 60, last_held_back_loss=log[-1]['held_back_loss'],
                rows=table.groupby(['source', 'year', 'split']).size().to_dict().__repr__())
    json.dump(meta, open(os.path.join(common.MODEL, 'train_meta.json'), 'w'), indent=1, default=float)


if __name__ == '__main__':
    main()
