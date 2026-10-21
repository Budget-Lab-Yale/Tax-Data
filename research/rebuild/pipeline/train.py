"""Pretraining: the transformer on the PUF and SCF table, with the cell method.

Masked any-subset training (model.py) on every record but a 2% held-back
slice, for a fixed number of passes, keeping the last.

The cell method (settings.json, "translation"). Some SCF households that
report zero interest, dividends or capital gains hold some. From the third
pass on, before each batch, the SCF zero-reporters of each mapped item get a
fill-in drawn from the model's own belief (under the SCF label, the item
hidden, everything else shown), with the odds of each size range multiplied
so that, in each cell (wage group x married) and SCF wave, the expected
number filled in at each size equals a target:
  - the target share of the cell's zero-reporters for each size range is the
    PUF's share of units in that range minus the SCF reporters' share, where
    positive, in ranges wholly below the item's top line; nothing is filled
    above it;
  - shares come from the fitting wave (2016 SCF, tax year 2015) and the
    2015 PUF, and are applied to each wave's own zero-reporters.
The multipliers are refit every few batches. A fill-in stays on the record
(and is seen by the other items) until it is redrawn.

Writes, in common.MODEL: model.pt, train_log.csv (loss per pass),
var_log.csv (held-back PUF holder shares and loss per variable, every few
passes), train_meta.json (settings, cells, the cell table).
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
WAGE_GROUPS = [(n, lo, INF if hi is None else hi) for n, lo, hi in S['cells']['wage_groups']]
RANGES = {v: [(-INF if lo is None else lo, INF if hi is None else hi) for lo, hi in S['size_ranges'][v]]
          for v in MAPPED}
TOP = {v: float(S['top_line'][v]) for v in MAPPED}
FLOOR = {v: float(S['fill_losses_down_to'].get(v, 0.0)) for v in MAPPED}


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


def fill_allowed(v, lo, hi):
    """Hidden holders are filled in only in ranges wholly below the top line and above the loss floor."""
    return hi <= TOP[v] and lo >= FLOOR[v] and lo > -INF


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


def wage_group_of_band(edges):
    """Wage group (0..4) of each wages band, by its 2015 PUF median."""
    e = edges['puf/2015/wages']
    out = np.zeros(e['n_bands'], dtype=np.int64)
    for bd in e['bands']:
        b, med = bd['band'], bd['median']
        if b == 0:
            continue
        out[b] = 1 if med is None else 1 + sum(med >= WAGE_GROUPS[g][1] for g in (2, 3, 4))
    return out


def cell_map(table, edges):
    """Cell of every row: wage group x married, small cells (under min_households
    fitting-wave SCF households) merged with a neighbour, smallest first."""
    wg = wage_group_of_band(edges)[table['wages_band'].to_numpy()]
    mar = table['married'].to_numpy().astype(np.int64)
    sel = ((table['source'] == 'scf') & (table['year'] == FIT_YEAR)).to_numpy()
    hh = table['household_id'].to_numpy()
    groups = {m: [[g] for g in range(5)] for m in (0, 1)}

    def count(m, gs):
        return len(np.unique(hh[sel & (mar == m) & np.isin(wg, gs)]))
    for m in (0, 1):
        while len(groups[m]) > 1:
            cnt = [count(m, gs) for gs in groups[m]]
            small = [i for i, c in enumerate(cnt) if c < S['cells']['min_households']]
            if not small:
                break
            i = min(small, key=lambda q: cnt[q])
            j = i - 1 if i > 0 else i + 1
            lo, hi = min(i, j), max(i, j)
            groups[m][lo] = sorted(groups[m][lo] + groups[m][hi])
            del groups[m][hi]
    cell = np.full(len(table), -1, dtype=np.int64)
    info = []
    for m in (0, 1):
        for gs in groups[m]:
            cell[(mar == m) & np.isin(wg, gs)] = len(info)
            names = [WAGE_GROUPS[g][0] for g in gs]
            info.append({'cell': len(info), 'married': m, 'wage groups': gs,
                         'label': f"{'married' if m else 'not married'}, wages "
                                  + (names[0] if len(names) == 1 else f'{names[0]} to {names[-1]}'),
                         'fitting-wave SCF households': count(m, gs)})
    assert (cell >= 0).all()
    return cell, info


def main():
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(common.SEED)
    gen = torch.Generator(device=device).manual_seed(common.SEED)
    os.makedirs(common.MODEL, exist_ok=True)
    table = M.read_table()
    edges = json.load(open(common.EDGES))
    table['cell'], cell_info = cell_map(table, edges)
    NCELL = len(cell_info)
    WAVES = sorted(int(y) for y in table.loc[table.source == 'scf', 'year'].unique())
    NG = len(WAVES) * NCELL
    wi = table['year'].map({y: i for i, y in enumerate(WAVES)}).fillna(WAVES.index(FIT_YEAR)).astype(int)
    table['grp'] = wi.to_numpy() * NCELL + table['cell'].to_numpy()     # wave x cell (PUF rows: unused)
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
                    rep_band={v: d['values'][:, C + jl[v]].clone() for v in MAPPED},
                    cell=torch.tensor(part['cell'].to_numpy(), device=device),
                    grp=torch.tensor(part['grp'].to_numpy(), device=device))
    tr = enc(table[table.split == 'train'])
    te = enc(table[table.split == 'test'])
    puf_rows = (table.source == 'puf').to_numpy()

    # ---- the cell table: target shares of each cell's zero-reporters to fill, by size range
    TGT, LOGH, cell_table = {}, {}, []
    fitw = tr['srv'] & torch.tensor(tr['years'] == FIT_YEAR, device=device)
    for v in MAPPED:
        pr = RO_np[v][table.loc[puf_rows, f'{v}_band'].to_numpy()]
        pw = table.loc[puf_rows, 'weight'].to_numpy(float)
        pc = table.loc[puf_rows, 'cell'].to_numpy()
        frac = np.zeros((NCELL, NR[v]))
        for c in range(NCELL):
            Pc = np.bincount(pr[pc == c], weights=pw[pc == c], minlength=NR[v]) / pw[pc == c].sum()
            fr = fitw & (tr['cell'] == c)
            rw = tr['weight'][fr].double().cpu().numpy()
            Sc = np.bincount(RO[v][tr['rep_band'][v][fr]].cpu().numpy(), weights=rw, minlength=NR[v]) / rw.sum()
            Wz = Sc[0]
            hid = np.zeros(NR[v])
            for i, (lo, hi) in enumerate(RANGES[v], start=1):
                ok = fill_allowed(v, lo, hi)
                hid[i] = max(Pc[i] - Sc[i], 0.0) if ok else 0.0
                cell_table.append({'item': v, 'cell': cell_info[c]['label'], 'range': range_label(lo, hi),
                                   'below the top line': ok, 'PUF (% of units)': 100 * Pc[i],
                                   'SCF reported (% of units)': 100 * Sc[i],
                                   'filled in (% of units)': 100 * hid[i]})
            if hid.sum() > Wz:                       # never more than the zeros
                hid *= Wz / hid.sum()
            frac[c] = hid / Wz if Wz > 0 else 0.0
            frac[c, 0] = 1.0 - frac[c].sum()
        tg = torch.zeros(NG, NR[v], dtype=torch.float64)
        for i, y in enumerate(WAVES):
            for c in range(NCELL):
                zeros = tr['srv'] & torch.tensor(tr['years'] == y, device=device) & (tr['cell'] == c) & \
                    (tr['rep_band'][v] == 0)
                tg[i * NCELL + c] = torch.tensor(frac[c] * float(tr['weight'][zeros].double().sum()))
        TGT[v] = tg.to(device=device, dtype=torch.float32)
        LOGH[v] = torch.zeros(NG, NR[v], device=device)

    model = M.new_model(device)
    opt = torch.optim.AdamW(model.parameters(), lr=T['peak_learning_rate'], weight_decay=T['weight_decay'])
    n, bs = len(tr['values']), T['batch']
    spe = math.ceil(n / bs)
    total, warm, floor = T['passes'] * spe, T['warmup_passes'] * spe, T['final_learning_rate_share']
    sched = torch.optim.lr_scheduler.LambdaLR(
        opt, lambda s: (s + 1) / warm if s < warm else
        floor + (1 - floor) * 0.5 * (1 + math.cos(math.pi * (s - warm) / max(1, total - warm))))
    val_hidden = [M.draw_hidden(te['values'], gen) for _ in range(4)]

    @torch.no_grad()
    def draw_fills(vals, srv_rows, rep_band, grps):
        """Each mapped item in random order: each zero-reporter's band drawn from
        the SCF-labelled belief times its size range's multiplier."""
        model.eval()
        sub, sg = vals[srv_rows], grps[srv_rows]
        for k in torch.randperm(len(MAPPED), generator=gen, device=device).tolist():
            v = MAPPED[k]
            zero = rep_band[v][srv_rows] == 0
            if not zero.any():
                continue
            hid = torch.zeros(len(sub), len(P), dtype=torch.bool, device=device)
            hid[:, jl[v]] = True
            logits = (model(sub, hid)[:, jl[v], :NB[v]] + LOGH[v][sg][:, RO[v]])[zero]
            u = torch.rand(logits.shape, generator=gen, device=device).clamp_min(1e-20)
            col = sub[:, C + jl[v]].clone()
            col[zero] = (logits - torch.log(-torch.log(u))).argmax(1)
            sub[:, C + jl[v]] = col
        vals[srv_rows] = sub
        model.train()

    @torch.no_grad()
    def fit_multipliers(iters):
        """One multiplier per wave, cell and range on the odds, adjusted until each
        range's expected count over the zero-reporters equals its target."""
        model.eval()
        idx = tr['srv'].nonzero().squeeze(1)
        for v in MAPPED:
            z = idx[tr['rep_band'][v][idx] == 0]
            lps = []
            for i in range(0, len(z), 20_000):
                zz = z[i:i + 20_000]
                hid = torch.zeros(len(zz), len(P), dtype=torch.bool, device=device)
                hid[:, jl[v]] = True
                lps.append(model(tr['values'][zz], hid)[:, jl[v], :NB[v]])
            lp, g, w = torch.cat(lps), tr['grp'][z], tr['weight'][z]
            logh = LOGH[v].clone()
            for _ in range(iters):
                p = (lp + logh[g][:, RO[v]]).softmax(1)
                a = torch.zeros(NG, NB[v], device=device).index_add_(0, g, w[:, None] * p)
                agg = torch.zeros(NG, NR[v], device=device).index_add_(1, RO[v], a)
                logh = logh + torch.log(TGT[v] + 1e-9) - torch.log(agg + 1e-9)
                logh = (logh - logh[:, :1]).clamp(-40, 40)
            LOGH[v] = logh
        model.train()

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
        for start in range(0, n, bs):
            idx = order[start:start + bs]
            if fill:
                vals = tr['values'][idx]
                draw_fills(vals, tr['srv'][idx], {v: tr['rep_band'][v][idx] for v in MAPPED}, tr['grp'][idx])
                tr['values'][idx] = vals
            values, weight = tr['values'][idx], tr['weight'][idx]
            loss = M.masked_loss(model, values, weight, M.draw_hidden(values, gen))
            opt.zero_grad()
            loss.backward()
            opt.step()
            sched.step()
            tot += loss.item() * len(idx)
            if fill and (start // bs) % T['refit_every_batches'] == T['refit_every_batches'] - 1:
                fit_multipliers(150)
        if epoch >= S['fill_from_pass'] - 1:
            fit_multipliers(400)
        model.eval()
        with torch.no_grad():
            val = float(np.mean([M.masked_loss(model, te['values'], te['weight'], h).item() for h in val_hidden]))
        log.append(dict(epoch=epoch, train_loss=tot / n, held_back_loss=val, seconds=time.time() - t0))
        common.say(f'pass {epoch:3d}  train {tot / n:.4f}  held back {val:.4f}  {time.time() - t0:.0f}s')
        if epoch == 1 or epoch % T['log_every_passes'] == 0 or epoch == T['passes']:
            log_variables(epoch)
    pd.DataFrame(log).to_csv(os.path.join(common.MODEL, 'train_log.csv'), index=False)
    M.save_model(model, os.path.join(common.MODEL, 'model.pt'))
    meta = dict(seed=common.SEED, settings=common.SETTINGS, cells=cell_info, cell_table=cell_table,
                minutes=(time.time() - began) / 60, last_held_back_loss=log[-1]['held_back_loss'],
                rows=table.groupby(['source', 'year', 'split']).size().to_dict().__repr__())
    json.dump(meta, open(os.path.join(common.MODEL, 'train_meta.json'), 'w'), indent=1, default=float)


if __name__ == '__main__':
    main()
