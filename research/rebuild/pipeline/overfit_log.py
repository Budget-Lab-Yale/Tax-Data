"""Overfitting diagnostic for train.py (branch rebuild-overfit-diag; logging only, training unchanged).

Each pass, the loss per slot on two sets of records, each source alone:
  held_back     the held-back slice, with train.py's own four hidden-slot draws (val_hidden), exactly as the held-back
                loss in train_log.csv is measured;
  train_sample  a fixed sample of each source's training records (up to SAMPLE records per source), its values frozen
                at the end of pass 1 (age ranges and filing flags as drawn for pass 1; no fill-in has run yet), with
                every survey zero-reporter's PUF-concept slot set absent, as on the held-back slice, so the two sets
                are measured the same way. Four hidden-slot draws of its own.
The sample and its hidden draws use their own random generators (numpy, and a torch generator seeded apart from
training's), so training's random draws, and therefore the network, are unchanged.

Writes <model folder>/slot_log.csv: pass, split, source, variable, nll (weighted cross-entropy summed over the four
draws and the records where the slot is hidden), weight (the weight summed the same way). A group's loss is the sum of
its nll over the sum of its weight. train.py's held_back_<source> is the mean over draws of each draw's ratio; the
ratio of sums here differs from it only in the fourth decimal place or beyond.
"""
import os

import numpy as np
import pandas as pd
import torch

import fillin
import model as M

SAMPLE = 20_000
C, P = len(M.CONTEXT), M.PREDICTED


def absent_zero_reporters(values, rep_band, src_mask, years):
    """values [N, SLOTS]: each survey zero-reporter's PUF-concept slot set absent, in the years its row fills in (the
    same rule train.py applies to the held-back slice)."""
    yi = torch.searchsorted(torch.tensor(M.YEARS, device=values.device), years)
    for s, v in fillin.KEYS:
        act = torch.tensor([fillin.TR.fills_in(fillin.ROW[(s, v)], int(y)) for y in M.YEARS], device=values.device)
        values[src_mask[s] & act[yi] & (rep_band[v] == 0), C + P.index(v)] = -1
    return values


class OverfitLog:
    def __init__(self, model, tr, te, val_hidden, gate, show_label, seed, folder):
        self.model, self.tr, self.te, self.val_hidden = model, tr, te, val_hidden
        self.gate, self.show_label = gate, show_label
        self.device = tr['values'].device
        self.path = os.path.join(folder, 'slot_log.csv')
        if os.path.exists(self.path):
            os.remove(self.path)
        rng = np.random.default_rng(seed + 9_001)
        self.rows = {}
        for src in M.SOURCES:
            r = tr[src].nonzero().squeeze(1).cpu().numpy()
            pick = np.sort(rng.choice(r, size=min(SAMPLE, len(r)), replace=False))
            self.rows[src] = torch.tensor(pick, device=self.device)
        self.hidden_gen = torch.Generator(device=self.device).manual_seed(seed + 9_002)
        self.sample = None

    def freeze_sample(self):
        """The training sample's values and hidden draws, taken once (end of pass 1)."""
        idx = torch.cat([self.rows[s] for s in M.SOURCES])
        tr = self.tr
        vals = tr['values'][idx].clone()
        vals = absent_zero_reporters(vals, {v: tr['rep_band'][v][idx] for v in fillin.MAPPED},
                                     {s: tr[s][idx] for s in M.SOURCES},
                                     torch.tensor(M.YEARS, device=self.device)[vals[:, 1]])
        hidden = [self.show_label(M.draw_hidden(vals, self.hidden_gen)) for _ in range(4)]
        self.sample = dict(values=vals, weight=tr['weight'][idx], hidden=hidden,
                           **{s: tr[s][idx] for s in M.SOURCES})

    @torch.no_grad()
    def sums(self, d, hidden_draws):
        """Per source and slot: weighted cross-entropy and weight over the hidden slots, summed over the draws."""
        out = []
        for src in M.SOURCES:
            r = d[src].nonzero().squeeze(1)
            sl = M.SOURCE_SLOTS[src]
            names = [P[j] for j in M.predicted_of(sl)]
            num = torch.zeros(len(names), dtype=torch.float64, device=self.device)
            den = torch.zeros_like(num)
            for h in hidden_draws:
                vals, hid = self.gate(*M.pack(d['values'][r], h[r], sl), sl)
                ch = M.safe_chunk(vals.shape[1])
                w_all = d['weight'][r]
                for i in range(0, len(vals), ch):
                    v, hd, w = vals[i:i + ch], hid[i:i + ch], w_all[i:i + ch]
                    nll = M.slot_nll(self.model(v, hd, sl), v)
                    ww = w[:, None] * hd
                    num += (nll * ww).sum(0).double()
                    den += ww.sum(0).double()
            out.append(pd.DataFrame({'source': src, 'variable': names, 'nll': num.cpu().numpy(),
                                     'weight': den.cpu().numpy()}))
        return pd.concat(out)

    @torch.no_grad()
    def log(self, epoch):
        was_training = self.model.training
        self.model.eval()
        if self.sample is None:
            self.freeze_sample()
        parts = [self.sums(self.te, self.val_hidden).assign(split='held_back'),
                 self.sums(self.sample, self.sample['hidden']).assign(split='train_sample')]
        df = pd.concat(parts).assign(**{'pass': epoch})[['pass', 'split', 'source', 'variable', 'nll', 'weight']]
        df.to_csv(self.path, mode='a', header=not os.path.exists(self.path), index=False)
        filled = set(fillin.MAPPED)
        msg = []
        for src in ['puf', 'scf']:
            for split in ['train_sample', 'held_back']:
                x = df[(df.source == src) & (df.split == split)]
                f = x.variable.isin(filled)
                loss = lambda m: x.nll[m].sum() / x.weight[m].sum() if x.weight[m].sum() > 0 else float('nan')
                msg.append(f'{src} {split} {loss(x.weight >= 0):.4f} (filled {loss(f):.4f}, other {loss(~f):.4f})')
        print(f'  overfit log pass {epoch}: ' + '; '.join(msg), flush=True)
        if was_training:
            self.model.train()
