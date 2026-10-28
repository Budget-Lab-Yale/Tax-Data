"""Pretraining: the transformer on the PUF, SCF, CEX, CPS and SIPP table.

Masked any-subset training (model.py) on every record but a 2% held-back
slice, for a fixed number of passes, keeping the last.

Batches. Each batch holds records of one source and carries only that
source's slots (model.SOURCE_SLOTS). Each pass splits every source's training
rows into batches of the usual size and shuffles the batches of all sources
together, so each source gets updates in proportion to its rows. A batch's
weighted cross-entropy is divided by (records in the batch) x (the average
over all training records of weight x expected hidden slots), the denominator
a batch mixing all sources would have on average, so a source's records count
as much in the objective as they would there.

The fill-in (fillin.py, function 2): from the pass fill_from_pass on, the
PUF-concept amount of every zero-reporter of every row of the translation table
with fill_in (since 28 October on all four surveys, wages included) is redrawn
in each batch. In the held-back slice no fill-in is drawn, so there a
zero-reporter's PUF-concept slot is absent (unknown: not shown, not scored).

Age range (ages.py). PUF records with no range carry step 0's probabilities;
a range is drawn from them for every training record at the start of each
pass, and once for the held-back ones.

The filing flag (step 2). PUF rows are filers (filer = 1). On CPS rows the
flag is drawn from the unit's filing chance (filer_p) for every training
record at the start of each pass, and once for the held-back ones, the same
pattern as the age range.

The CPS's shares (model.gate_shares; settings "training",
shares_shown_with_their_base). The spouse's share of the couple's wages (of
its self-employment income) is left out of a record whenever married or the
share's base is hidden, so its presence never tells the network what those
hidden slots hold. Generation needs this to draw non-filers' married flag and
amounts under the CPS label.

Grouped weighting (qgroups.py; settings "training", q_groups). Each record counts by its survey weight times q, q
constant within a group (12 band-AGI groups on the PUF, 5 band-income classes per SCF wave, 1 on the CEX and CPS), then
rescaled within each source, so each source's share of training is unchanged. The training log prints the groups and
their q. Averages meant to describe the population use the survey weights without q: the fill-in's averages over a
type (fillin.py) and the per-variable log.

The AGI group label (settings "training", label_always_shown). Generation always presets a filer's AGI group, so on PUF
rows the label is never hidden in training and never scored: the hidden set is drawn over all of a record's present
slots, the label included, then the label alone is set back to shown. Which other slots are hidden, and how many, is
therefore as if the label were an ordinary slot. A record whose only hidden slot was the label has nothing hidden
(about 0.04% of PUF rows a pass) and adds nothing to the loss. The loss denominator still counts the label among the
expected hidden slots. The held-back loss in train_log.csv is measured with the label shown too.

Precision of the training steps (settings "training", precision): fp32,
standard 32-bit; tf32, large matrix multiplications at TF32's reduced internal
precision, storage 32-bit; bf16, the forward pass under autocast in bfloat16,
the log-probabilities, loss, gradients and weight updates in 32-bit. The
fill-in, the held-back loss and the per-variable log always run in 32-bit.

Writes, in common.MODEL: model.pt, train_log.csv (loss per pass),
var_log.csv (held-back PUF, CEX, CPS and SIPP holder shares per variable, every few
passes; for a CPS category, the share not in value 0), fill_log.csv (fillin.py), train_meta.json (settings).
"""
import json
import math
import os
import time

import numpy as np
import pandas as pd
import torch

import common
import fillin
import model as M
import qgroups

S = common.SETTINGS['translation']
T = dict(common.SETTINGS['training'], passes=common.PASSES)
PRECISION = T.get('precision', 'fp32')
GATE_SHARES = bool(T.get('shares_shown_with_their_base', True))
assert PRECISION in ('fp32', 'tf32', 'bf16'), PRECISION
C, P = len(M.CONTEXT), M.PREDICTED
LABEL_SHOWN = bool(T.get('label_always_shown', False))
J_LABEL = [P.index(v) for v in M.PUF_LABELS]       # the AGI group label's position among the predicted slots


def show_label(hidden, slots=None):
    """hidden [N, predicted slots of `slots`] (all of PREDICTED when slots is None), with the label set to shown."""
    if not LABEL_SHOWN:
        return hidden
    cols = list(range(len(P))) if slots is None else M.predicted_of(slots)
    for j in J_LABEL:
        if j in cols:
            hidden[:, cols.index(j)] = False
    return hidden


def training_precision(on):
    """Switch the training steps' precision on (on=True) or back to standard 32-bit."""
    torch.set_float32_matmul_precision('high' if on and PRECISION == 'tf32' else 'highest')


def autocast():
    return torch.autocast('cuda', dtype=torch.bfloat16, enabled=PRECISION == 'bf16')


def encode(part, device):
    """A split's records on the device: values, weights, step 0's age draws, the source masks, and the reported
    band of each mapped item (on SCF rows the PUF-concept slot starts as a copy of it)."""
    d = M.encode(part, device)
    age_rows, age_probs = M.age_draws(part, device)
    filer_rows, filer_p = M.filer_draws(part, device)
    return dict(values=d['values'], weight=d['weight'], pop_weight=d['pop_weight'], age_rows=age_rows, age_probs=age_probs,
                filer_rows=filer_rows, filer_p=filer_p,
                **{src: torch.tensor((part['source'] == src).to_numpy(), device=device) for src in M.SOURCES},
                rep_band={v: d['values'][:, C + P.index(v)].clone() for v in fillin.MAPPED})


def main():
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(common.SEED)
    gen = torch.Generator(device=device).manual_seed(common.SEED)
    os.makedirs(common.MODEL, exist_ok=True)
    table = M.read_table()
    train_rows = table[table.split == 'train']
    tr = encode(train_rows, device)
    te = encode(table[table.split == 'test'], device)
    te_year = torch.tensor(table.loc[table.split == 'test', 'year'].to_numpy(), device=device)
    for s, v in fillin.KEYS:   # held-back zero-reporters: the PUF-concept amount is unknown, so absent
        act = torch.tensor([fillin.TR.fills_in(fillin.ROW[(s, v)], int(y)) for y in M.YEARS], device=device)
        yi = torch.searchsorted(torch.tensor(M.YEARS, device=device), te_year)
        te['values'][te[s] & act[yi] & (te['rep_band'][v] == 0), C + P.index(v)] = -1
    j_age = C + P.index('age_range')

    def draw_ages(d):
        """A range for every record with none, from step 0's probabilities."""
        if len(d['age_rows']):
            d['values'][d['age_rows'], j_age] = torch.multinomial(d['age_probs'], 1, generator=gen).squeeze(1)
    draw_ages(te)
    j_filer = C + P.index('filer')

    def draw_filers(d):
        """The filing flag of every CPS record, from its filing chance."""
        if len(d['filer_rows']):
            d['values'][d['filer_rows'], j_filer] = torch.bernoulli(d['filer_p'], generator=gen).long()
    draw_filers(te)
    draw_filers(tr)

    def gate(values, hidden, slots):
        return M.gate_shares(values, hidden, slots) if GATE_SHARES else (values, hidden)

    model = M.new_model(device)
    fill = fillin.FillIn(model, tr, train_rows, gen)
    opt = torch.optim.AdamW(model.parameters(), lr=T['peak_learning_rate'], weight_decay=T['weight_decay'])
    n, bs = len(tr['values']), T['batch']
    src_rows = {src: tr[src].nonzero().squeeze(1) for src in M.SOURCES}
    spe = sum(math.ceil(len(r) / bs) for r in src_rows.values())
    # the average over training records of weight x expected hidden slots (ceil(u n) for u uniform on (0, 1] has
    # mean (n + 1) / 2): the denominator per record of a batch mixing all sources, on average
    n_present = (tr['values'][:, C:] >= 0).sum(1).double()
    per_record = float((tr['weight'].double() * (n_present + 1) / 2).mean())
    gpu = torch.cuda.get_device_name() if device == "cuda" else "cpu"
    common.say(f'device {gpu}; precision {PRECISION}; fill-in chunk {fill.chunk} records')
    common.say(f'batches per pass {spe}; weight x expected hidden slots per record {per_record:.3f}')
    common.say(f'grouped weighting: q_groups {qgroups.Q_GROUPS or None}; fill-in PUF side '
               f'{T.get("fillin_puf_side") or "model"}; AGI group label always shown {LABEL_SHOWN}')
    if qgroups.Q_GROUPS:
        common.say(qgroups.describe(table).to_string(index=False, float_format=lambda x: f'{x:.6g}'))
        common.say('training share by source (unchanged by q): '
                   + str(train_rows.groupby('source').size().div(len(train_rows)).round(4).to_dict()))

    def batches(order_gen):
        """(slots, row indices) per batch for one pass."""
        out = []
        for src, rows in src_rows.items():
            rows = rows[torch.randperm(len(rows), generator=order_gen, device=device)]
            out += [(M.SOURCE_SLOTS[src], rows[a:a + bs]) for a in range(0, len(rows), bs)]
        return [out[k] for k in torch.randperm(len(out), generator=order_gen, device=device).tolist()]

    def held_back_loss(hidden_draws):
        """Overall held-back loss (mean of the draws) and by source."""
        overall, by_src = [], {s: [] for s in M.SOURCES}
        for h in hidden_draws:
            num, den = {}, {}
            for src in M.SOURCES:
                r = te[src].nonzero().squeeze(1)
                sl = M.SOURCE_SLOTS[src]
                vals, hid = gate(*M.pack(te['values'][r], h[r], sl), sl)
                ch = M.safe_chunk(vals.shape[1])
                sums = [M.masked_sums(model, vals[i:i + ch], te['weight'][r][i:i + ch], hid[i:i + ch], sl)
                        for i in range(0, len(vals), ch)]
                num[src], den[src] = sum(float(a) for a, _ in sums), sum(float(b) for _, b in sums)
            overall.append(sum(num.values()) / sum(den.values()))
            for src in M.SOURCES:
                by_src[src].append(num[src] / den[src])
        return float(np.mean(overall)), {s: float(np.mean(x)) for s, x in by_src.items()}
    total, warm, floor = T['passes'] * spe, T['warmup_passes'] * spe, T['final_learning_rate_share']
    sched = torch.optim.lr_scheduler.LambdaLR(
        opt, lambda s: (s + 1) / warm if s < warm else
        floor + (1 - floor) * 0.5 * (1 + math.cos(math.pi * (s - warm) / max(1, total - warm))))
    val_hidden = [show_label(M.draw_hidden(te['values'], gen)) for _ in range(4)]

    # ---- the per-variable log: held-back PUF records for the PUF items, held-back CEX records for consumption
    logged = {'puf': [v for v in M.AMOUNTS if f'puf/{common.PUF_YEAR}/{v}' in common.edges()], 'cex': M.CEX_SLOTS,
              'cps': M.CPS_SLOTS, 'sipp': M.SIPP_SLOTS}
    var_log = []

    @torch.no_grad()
    def log_variables(epoch):
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

    log, began = [], time.time()
    cuda = device == 'cuda'
    sync = (lambda: torch.cuda.synchronize()) if cuda else (lambda: None)
    for epoch in range(1, T['passes'] + 1):
        model.train()
        if cuda:
            torch.cuda.reset_peak_memory_stats()
        sync()
        t0 = time.time()
        plan = batches(gen)
        tot = 0.0
        draw_ages(tr)
        draw_filers(tr)
        filling = epoch >= S['fill_from_pass']
        if filling:
            fill.refit(epoch)
        sync()
        t_refit = time.time() - t0
        training_precision(True)
        for slots, idx in plan:
            if filling:
                fill.draw(idx)
            values, weight = tr['values'][idx][:, M.slot_tensor(slots, device)], tr['weight'][idx]
            values, hidden = gate(values, show_label(M.draw_hidden(values, gen), slots), slots)
            with autocast():
                loss = M.masked_loss(model, values, weight, hidden, slots, denominator=len(idx) * per_record)
            opt.zero_grad()
            loss.backward()
            opt.step()
            sched.step()
            tot += loss.item() * len(idx)
        training_precision(False)
        sync()
        t_batches = time.time() - t0 - t_refit
        model.eval()
        with torch.no_grad():
            val, val_src = held_back_loss(val_hidden)
        sync()
        row = dict(epoch=epoch, train_loss=tot / n, held_back_loss=val, seconds=time.time() - t0,
                   seconds_refit=t_refit, seconds_batches=t_batches,
                   peak_gpu_gb=torch.cuda.max_memory_allocated() / 1e9 if cuda else float('nan'),
                   **{f'held_back_{s}': x for s, x in val_src.items()})
        log.append(row)
        pd.DataFrame(log).to_csv(os.path.join(common.MODEL, 'train_log.csv'), index=False)
        common.say(f'pass {epoch:3d}  train {tot / n:.4f}  held back {val:.4f} '
                   + ' '.join(f'{s} {x:.4f}' for s, x in val_src.items())
                   + f'  {row["seconds"]:.1f}s (fill-in {t_refit:.1f}s, batches {t_batches:.1f}s)  peak {row["peak_gpu_gb"]:.2f} GB')
        if epoch == 1 or epoch % T['log_every_passes'] == 0 or epoch == T['passes']:
            log_variables(epoch)
    pd.DataFrame(log).to_csv(os.path.join(common.MODEL, 'train_log.csv'), index=False)
    M.save_model(model, os.path.join(common.MODEL, 'model.pt'))
    meta = dict(seed=common.SEED, settings=common.SETTINGS, precision=PRECISION, shares_gated=GATE_SHARES, gpu=gpu,
                node=os.environ.get('SLURMD_NODENAME'), job=os.environ.get('SLURM_JOB_ID'),
                minutes=(time.time() - began) / 60, last_held_back_loss=log[-1]['held_back_loss'],
                rows=table.groupby(['source', 'year', 'split']).size().to_dict().__repr__())
    json.dump(meta, open(os.path.join(common.MODEL, 'train_meta.json'), 'w'), indent=1, default=float)


if __name__ == '__main__':
    main()
