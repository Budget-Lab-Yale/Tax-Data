"""Pretraining: the transformer on the PUF and SCF table, with two slots per translated item.

Masked any-subset training (model.py) on every record but a 2% held-back
slice, for a fixed number of passes, keeping the last.

Translation (settings.json, "translation"). Some SCF households that report
zero interest, dividends or capital gains hold some. Each of these items has
two slots on SCF rows: the reported amount (model.REPORTED, never changed)
and the PUF-concept amount (the shared slot), which reporters copy and
zero-reporters get filled in. From the third pass on, at the start of each
pass, the reporting-rate formula gives every SCF zero-reporter its chance of
a hidden holding in each size range:
  C_k  its chance of reporting range k: SCF label, the reported slot,
       everything else shown except every PUF-concept slot (so no fill-in
       ever feeds the formula);
  B_k  C_k averaged over SCF units of the same year, married, dependents and
       wage band (the type);
  A_k  the PUF concept given the type only (PUF label, the fitting year);
  chance of a hidden holding in k = C_k * max(A_k / B_k - 1, 0) / C_0,
  capped at 100% in total; within a range, bands follow A.
Each batch redraws the PUF-concept slot of its zero-reporters from those
chances. It assumes a holder's chance of reporting depends only on its type
and true amount, not on anything else on the record.

Writes, in common.MODEL: model.pt, train_log.csv (loss per pass),
var_log.csv (held-back PUF holder shares and loss per variable, every few
passes), fill_log.csv (share with none after the fill-in against the PUF, per
pass), train_meta.json (settings).
"""
import json
import math
import os
import time

import numpy as np
import pandas as pd
import torch

import common
import model as M

S = common.SETTINGS['translation']
T = dict(common.SETTINGS['training'], passes=common.PASSES)
MAPPED = list(S['mapped'])
FIT_YEAR = int(S['fitting_tax_year'])
INF = math.inf
RANGES = {v: [(-INF if lo is None else lo, INF if hi is None else hi) for lo, hi in S['size_ranges'][v]]
          for v in MAPPED}


def range_label(lo, hi):
    f = lambda x: f'${x:,.0f}'.replace('$-', '-$')
    if lo == -INF:
        return f'below {f(hi)}'
    if hi == INF:
        return f'{f(lo)} and over'
    return f'{f(lo)} to {f(hi)}'


def in_range(x, lo, hi):
    """Small positive ranges are (0, hi); every other range is [lo, hi)."""
    return 0 < x < hi if lo == 0 else lo <= x < hi


def range_of_band(edges, v):
    """Size range of every band of v by its 2015 PUF median: 0 = zero, 1..K = the ranges, K + 1 = a band with no PUF records."""
    out = []
    for bd in edges[f'puf/2015/{v}']['bands']:
        if bd['band'] == 0:
            out.append(0)
        elif bd['median'] is None:
            out.append(len(RANGES[v]) + 1)
        else:
            hit = [i for i, (lo, hi) in enumerate(RANGES[v]) if in_range(bd['median'], lo, hi)]
            assert len(hit) == 1, (v, bd)
            out.append(hit[0] + 1)
    return np.array(out, dtype=np.int64)


def main():
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(common.SEED)
    gen = torch.Generator(device=device).manual_seed(common.SEED)
    os.makedirs(common.MODEL, exist_ok=True)
    table = M.read_table()
    edges = json.load(open(common.EDGES))
    C, P = len(M.CONTEXT), M.PREDICTED
    NB = {v: M.N_CHOICES[v] for v in MAPPED}
    jl = {v: P.index(v) for v in MAPPED}
    RO_np = {v: range_of_band(edges, v) for v in MAPPED}
    RO = {v: torch.tensor(RO_np[v], device=device) for v in MAPPED}
    NR = {v: len(RANGES[v]) + 2 for v in MAPPED}

    def enc(part):
        d = M.encode(part, device)
        return dict(values=d['values'], weight=d['weight'], years=part['year'].to_numpy(),
                    srv=torch.tensor((part['source'] == 'scf').to_numpy(), device=device),
                    rep_band={v: d['values'][:, C + jl[v]].clone() for v in MAPPED})
    tr = enc(table[table.split == 'train'])
    te = enc(table[table.split == 'test'])
    puf_rows = (table.source == 'puf').to_numpy()

    # ---- the fill-in: the reporting-rate formula reads the reported slots, fills the PUF-concept slots
    jr = {v: P.index(v + '_reported') for v in MAPPED}
    srv_idx = tr['srv'].nonzero().squeeze(1)
    pos = torch.full((len(tr['values']),), -1, dtype=torch.long, device=device)
    pos[srv_idx] = torch.arange(len(srv_idx), device=device)
    trs = table[table.split == 'train']
    tkey = torch.tensor(trs.groupby(['year', 'married', 'n_dep', 'wages_band'], sort=False).ngroup().to_numpy(),
                        device=device)[srv_idx]
    NT = int(tkey.max()) + 1
    w_s = tr['weight'][srv_idx].double()
    fitw_s = torch.tensor(trs['year'].to_numpy() == FIT_YEAR, device=device)[srv_idx]
    TYPE = ['married', 'n_dep', 'wages']
    FILL, fill_log = {}, []

    model = M.new_model(device)
    opt = torch.optim.AdamW(model.parameters(), lr=T['peak_learning_rate'], weight_decay=T['weight_decay'])
    n, bs = len(tr['values']), T['batch']
    spe = math.ceil(n / bs)
    total, warm, floor = T['passes'] * spe, T['warmup_passes'] * spe, T['final_learning_rate_share']
    sched = torch.optim.lr_scheduler.LambdaLR(
        opt, lambda s: (s + 1) / warm if s < warm else
        floor + (1 - floor) * 0.5 * (1 + math.cos(math.pi * (s - warm) / max(1, total - warm))))
    val_hidden = [M.draw_hidden(te['values'], gen) for _ in range(4)]

    def type_only(v, label):
        v = v.clone()
        v[:, 0] = M.SOURCES.index(label)
        if label == 'puf':
            v[:, 1] = M.YEARS.index(FIT_YEAR)
        hid = torch.zeros(len(v), len(P), dtype=torch.bool, device=device)
        for j, sl in enumerate(P):
            owner = M.OWNED_BY.get(sl)
            if owner is not None and owner != label:
                v[:, C + j] = -1
            else:
                v[:, C + j] = v[:, C + j].clamp(min=0)
                hid[:, j] = sl not in TYPE
        return v, hid

    @torch.no_grad()
    def refit(epoch):
        """Each SCF record's fill-in distribution over the PUF-concept bands of each mapped item, from the
        formula: a zero-reporter's chance of a hidden holding in range k is C_k * e_k / C_0, with C its
        chance of reporting each range (SCF label, reported slot, every filled slot hidden), e_k = A_k / B_k - 1
        where positive, A the PUF concept given type only, B the average of C over its type (year, married,
        dependents, wage band); capped at 100%. Within a range, bands follow A."""
        model.eval()
        for v in MAPPED:
            K = NR[v]
            Ab, Cr = [], []
            for a in range(0, len(srv_idx), 20_000):
                vals = tr['values'][srv_idx[a:a + 20_000]]
                va, ha = type_only(vals, 'puf')
                Ab.append(model(va, ha)[:, jl[v], :NB[v]].exp().double())
                hc = torch.zeros(len(vals), len(P), dtype=torch.bool, device=device)
                hc[:, jr[v]] = True
                for u in MAPPED:
                    hc[:, jl[u]] = True
                pc = model(vals, hc)[:, jr[v], :NB[v]].exp().double()
                Cr.append(torch.zeros(len(pc), K, dtype=torch.float64, device=device).index_add_(1, RO[v], pc))
            Ab, Cr = torch.cat(Ab), torch.cat(Cr)
            Ar = torch.zeros(len(Ab), K, dtype=torch.float64, device=device).index_add_(1, RO[v], Ab)
            num = torch.zeros(NT, K, dtype=torch.float64, device=device).index_add_(0, tkey, w_s[:, None] * Cr)
            den = torch.zeros(NT, dtype=torch.float64, device=device).index_add_(0, tkey, w_s)
            B = num[tkey] / den[tkey][:, None]
            e = (Ar / B.clamp_min(1e-12) - 1).clamp(min=0)
            e[:, 0], e[:, K - 1] = 0, 0
            h = Cr * e / Cr[:, :1].clamp_min(1e-12)
            H = h.sum(1)
            h = h / H.clamp(min=1)[:, None]
            dist = Ab * (h / Ar.clamp_min(1e-15))[:, RO[v]]
            dist[:, 0] = 1 - h.sum(1)
            FILL[v] = dist.clamp(min=0).float()
            zero = tr['rep_band'][v][srv_idx] == 0
            m = fitw_s
            wm = lambda x, mm: float((w_s[mm] * x[mm]).sum() / w_s[mm].sum())
            row = {'pass': epoch, 'item': v,
                   'none, reported (%)': 100 * wm(zero.double(), m),
                   'none, after fill (%)': 100 * wm(torch.where(zero, 1 - h.sum(1), torch.zeros_like(H)), m),
                   'none, PUF at SCF types (%)': 100 * wm(Ar[:, 0], m),
                   'zero-reporters over 100% (%)': 100 * wm((H > 1).double(), m & zero)}
            fill_log.append(row)
            common.say('  fill ' + '  '.join(f'{k} {x:.2f}' if isinstance(x, float) else f'{k} {x}' for k, x in row.items()))
        pd.DataFrame(fill_log).to_csv(os.path.join(common.MODEL, 'fill_log.csv'), index=False)
        model.train()

    @torch.no_grad()
    def draw_fills(idx):
        """PUF-concept slot of each SCF zero-reporter in the batch drawn from its fill-in distribution;
        reporters keep the reported amount (dollar rule: unchanged)."""
        p = pos[idx]
        sv = p >= 0
        for v in MAPPED:
            z = sv & (tr['rep_band'][v][idx] == 0)
            if not z.any():
                continue
            band = torch.multinomial(FILL[v][p[z]], 1, generator=gen).squeeze(1)
            tr['values'][idx[z], C + jl[v]] = band

    # ---- the per-variable log: held-back PUF records
    zero_band = {v: edges[f'puf/2015/{v}']['zero_band'] for v in M.AMOUNTS if f'puf/2015/{v}' in edges}
    logged = [v for v in zero_band if v != 'net_worth']
    hb = (~te['srv']).nonzero().squeeze(1)
    hb_vals, hb_w = te['values'][hb], te['weight'][hb].double()
    var_log = []

    @torch.no_grad()
    def log_variables(epoch):
        model.eval()
        present = hb_vals[:, C:] >= 0
        for v in logged:
            j = P.index(v)
            ok = present[:, j]
            only = torch.zeros_like(present)
            only[:, j] = True
            row = {'pass': epoch, 'variable': v}
            actual = (hb_w * (ok & (hb_vals[:, C + j] != zero_band[v]))).sum() / (hb_w * ok).sum()
            row['holders, actual (%)'] = 100 * float(actual)
            for lab, hid in [('only this hidden', only & present), ('all hidden', present)]:
                p0 = torch.cat([model(hb_vals[i:i + 8192], hid[i:i + 8192])[:, j, :NB.get(v, M.N_CHOICES[v])]
                                .log_softmax(1)[:, zero_band[v]].exp() for i in range(0, len(hb_vals), 8192)])
                row[f'holders, {lab} (%)'] = 100 * float((hb_w * ok * (1 - p0.double())).sum() / (hb_w * ok).sum())
            var_log.append(row)
        pd.DataFrame(var_log).to_csv(os.path.join(common.MODEL, 'var_log.csv'), index=False)
        model.train()

    log, began = [], time.time()
    for epoch in range(1, T['passes'] + 1):
        model.train()
        t0 = time.time()
        order = torch.randperm(n, generator=gen, device=device)
        tot = 0.0
        fill = epoch >= S['fill_from_pass']
        if fill:
            refit(epoch)
        for start in range(0, n, bs):
            idx = order[start:start + bs]
            if fill:
                draw_fills(idx)
            values, weight = tr['values'][idx], tr['weight'][idx]
            loss = M.masked_loss(model, values, weight, M.draw_hidden(values, gen))
            opt.zero_grad()
            loss.backward()
            opt.step()
            sched.step()
            tot += loss.item() * len(idx)
        model.eval()
        with torch.no_grad():
            val = float(np.mean([M.masked_loss(model, te['values'], te['weight'], h).item() for h in val_hidden]))
        log.append(dict(epoch=epoch, train_loss=tot / n, held_back_loss=val, seconds=time.time() - t0))
        common.say(f'pass {epoch:3d}  train {tot / n:.4f}  held back {val:.4f}  {time.time() - t0:.0f}s')
        if epoch == 1 or epoch % T['log_every_passes'] == 0 or epoch == T['passes']:
            log_variables(epoch)
    pd.DataFrame(log).to_csv(os.path.join(common.MODEL, 'train_log.csv'), index=False)
    M.save_model(model, os.path.join(common.MODEL, 'model.pt'))
    meta = dict(seed=common.SEED, settings=common.SETTINGS,
                minutes=(time.time() - began) / 60, last_held_back_loss=log[-1]['held_back_loss'],
                rows=table.groupby(['source', 'year', 'split']).size().to_dict().__repr__())
    json.dump(meta, open(os.path.join(common.MODEL, 'train_meta.json'), 'w'), indent=1, default=float)


if __name__ == '__main__':
    main()
