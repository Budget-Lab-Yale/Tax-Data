"""
23_train_latent.py  (minimal example, step 23, part 2: plan decision 18 on the
real SCF; 2026-10-16. Write-up: 23_NONFILERS.md. The method was tested on a
fake survey in step 22: 22_fake_survey.py, 22_FAKE_SURVEY.md.)

What it does
  Trains the model of table E4N (the PUF with crude non-filers, and the 2016
  SCF wave) with the SCF's interest and dividends read as reports of true
  amounts, as decision 18 says:

  - No source label anywhere (MINIMAL_NO_SOURCE=1). Each source's own slots
    on the other source's rows are present but always hidden and never
    scored (net worth on PUF rows; other income and the filing flag on SCF
    rows), so a record whose own slots are hidden looks the same from either
    file. The two files are taken to cover the same population: that is what
    the non-filer rows are for.
  - The SCF's interest and dividends: an SCF unit reporting zero may be a
    holder who did not report. Each pass, every SCF zero gets a true amount
    drawn from the model's belief given everything else on the record (net
    worth included) times the chance of reporting zero at that amount; the
    model then trains on the drawn amount. An SCF unit reporting an amount
    has true dollars equal to its reported dollars over the dollar factor.
  - The reporting settings, learned: the chance of reporting rises with the
    amount, 1 / (1 + exp(-(alpha + beta (ln x - ln 150)))), fit by gradient
    steps on whether each SCF unit reports anything; the dollar factor, once
    per pass, by a grid search on the likelihood of the reporters' slices.
    Both are fit against the model's belief given only what the SCF reports
    truly here: marriage, dependents and wages (decision 18's rule; capital
    gains, net worth and the other item hidden).
  - The anchor (PUF = actual): SCF rows train their interest and dividend
    slots only when their net worth is visible, so the belief about true
    amounts given the shared variables alone comes from the PUF side.
  - Capital gains is left as today (reported amounts on the PUF's scale).
  Everything else as the v4 model: 80 passes, all records but a 2% held-back
  slice, batch 4,096, the same network and seed.

Writes model/<run>/: model.pt (loadable by 02_model.load_model), train_log.csv,
train_meta.json (learned settings per pass, hidden-holder shares).
Usage (Slurm, 23_run.sbatch): MINIMAL_TABLE=E4N MINIMAL_RUN=N2 MINIMAL_NO_SOURCE=1
  python research/rebuild/minimal/23_train_latent.py
"""
import importlib.util
import json
import math
import os
import time

import numpy as np
import pandas as pd
import torch

HERE = 'research/rebuild/minimal'
LATENT = ['interest', 'dividends']
MID = 150.0
FILL_FROM_EPOCH = 3
FACTOR_GRID = np.exp(np.linspace(math.log(0.5), math.log(2.0), 81))   # 1.0 is the middle point
# Alternatives of step 25 (25_REPORTING_SHAPE.md), each unset by default:
#   MINIMAL_LATENT_BETA_FIXED=<b>   the curve's steepness fixed at b, only its level learned:
#                                   0 = the chance of reporting does not depend on the amount;
#                                   8 = close to a sharp cutoff below a learned amount
#   MINIMAL_LATENT_FACTOR_FIXED=<f> the dollar factor fixed at f (no grid search)
BETA_FIXED = os.environ.get('MINIMAL_LATENT_BETA_FIXED')
FACTOR_FIXED = os.environ.get('MINIMAL_LATENT_FACTOR_FIXED')


def load_model_module():
    assert os.environ.get('MINIMAL_NO_SOURCE') == '1', 'set MINIMAL_NO_SOURCE=1'
    os.environ['MINIMAL_ALL_RECORDS'] = '1'
    spec = importlib.util.spec_from_file_location('model02', f'{HERE}/02_model.py')
    M = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(M)
    assert M.NO_SOURCE and M.ALL_RECORDS and not (M.NO_LABEL_SHARED or M.REPORT_CURVE or M.COARSEN)
    return M


def main():
    M = load_model_module()
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    seed, max_epochs = M.SEED, int(os.environ.get('MINIMAL_MAX_EPOCHS', '80'))
    torch.manual_seed(seed)
    gen = torch.Generator(device=device).manual_seed(seed)
    os.makedirs(M.MODEL_DIR, exist_ok=True)
    table = M.read_table()
    edges = json.load(open(M.BAND_EDGES))
    C, NB, POS0 = len(M.CONTEXT), M.N_CHOICES['interest'], M.BAND_LAYOUT['interest']['first_positive']
    P = M.PREDICTED
    jl = {v: P.index(v) for v in LATENT}
    j_nw = P.index('net_worth')
    LATENT_COL = torch.tensor([v in LATENT for v in P], device=device)
    # belief for the settings: what the SCF reports truly here (married, n_dep, wages, year)
    ANCHOR_HIDE = torch.tensor([v not in ('married', 'n_dep', 'wages') for v in P], device=device)

    def enc(part):
        d = M.encode(part, device)
        vals = d['values'].clone()
        vals[:, 0] = -1
        src = part['source'].to_numpy()
        unknown = torch.zeros(len(part), len(P), dtype=torch.bool, device=device)
        for v, owner in M.OWNED_BY.items():
            rows = torch.tensor(src != owner, device=device)
            unknown[rows, P.index(v)] = True
            vals[rows, C + P.index(v)] = 0
        srv = torch.tensor(src == 'scf', device=device)
        rep_band = {v: vals[:, C + jl[v]].clone() for v in LATENT}
        dollars = {v: torch.tensor(part[v].to_numpy(dtype=np.float64), device=device) for v in LATENT}
        return dict(values=vals, weight=d['weight'], unknown=unknown, srv=srv, rep_band=rep_band,
                    dollars=dollars)
    tr = enc(table[table.split == 'train'])
    te = enc(table[table.split == 'test'])
    puf_rows = (table.source == 'puf').to_numpy()

    # per slice of each item: PUF-side mean log dollars, and 32 points of its dollars
    cuts, lnx, points = {}, {}, {}
    for v in LATENT:
        c = edges[f'puf/2015/{v}']['positive']['cuts']
        cuts[v] = torch.tensor(c, dtype=torch.float64, device=device)
        x, b, w = (table.loc[puf_rows, v].to_numpy(float), table.loc[puf_rows, f'{v}_band'].to_numpy(),
                   table.loc[puf_rows, 'weight'].to_numpy(float))
        l, pts = np.zeros(NB), np.zeros((NB, 32))
        for k in range(POS0, NB):
            m = b == k
            kk = k - POS0
            if m.any():
                l[k] = np.average(np.log(x[m]), weights=w[m])
                pts[k] = np.quantile(x[m], (np.arange(32) + 0.5) / 32)
            else:
                l[k] = math.log(c[min(kk, len(c) - 1)])
                pts[k] = c[min(kk, len(c) - 1)]
        lnx[v] = torch.tensor(l, dtype=torch.float32, device=device)
        points[v] = pts
    pos = torch.arange(NB, device=device) >= POS0
    is_zero = torch.arange(NB, device=device) == 0
    LN_CENTRE = math.log(MID)
    alpha = torch.nn.Parameter(torch.zeros(len(LATENT), device=device))
    beta = torch.nn.Parameter(torch.zeros(len(LATENT), device=device))
    if BETA_FIXED is not None:
        beta = torch.full((len(LATENT),), float(BETA_FIXED), device=device)   # not learned
    factor = {v: (float(FACTOR_FIXED) if FACTOR_FIXED is not None else 1.0) for v in LATENT}

    def log_g(k):
        z = alpha[k] + beta[k] * (lnx[LATENT[k]] - LN_CENTRE)
        lg, l1mg = torch.nn.functional.logsigmoid(z), torch.nn.functional.logsigmoid(-z)
        lg = torch.where(pos, lg, torch.zeros_like(lg))
        l1mg = torch.where(pos, l1mg, torch.full_like(l1mg, -math.inf))
        lg = torch.where(is_zero, torch.full_like(lg, -math.inf), lg)
        l1mg = torch.where(is_zero, torch.zeros_like(l1mg), l1mg)
        return lg, l1mg

    def reporter_bands(d):
        for v in LATENT:
            x = d['dollars'][v]
            rep = d['srv'] & (x > 0)
            b = POS0 + torch.searchsorted(cuts[v], x[rep] / factor[v], right=False)
            d['values'][rep, C + jl[v]] = b.clamp(max=NB - 1).to(d['values'].dtype)

    def transfer(v, f):
        cut = np.array(edges[f'puf/2015/{v}']['positive']['cuts'])
        r = np.minimum(POS0 + np.searchsorted(cut, points[v][POS0:] * f, side='left'), NB - 1)
        T = np.zeros((NB, NB))
        for b in range(POS0, NB):
            np.add.at(T[b], r[b - POS0], 1.0 / r.shape[1])
        return torch.tensor(T, dtype=torch.float32, device=device)

    model = M.SlotTransformer().to(device)
    opt = torch.optim.AdamW([{'params': model.parameters(), 'lr': 1e-3, 'weight_decay': 0.01},
                             {'params': [alpha] + ([beta] if BETA_FIXED is None else []),
                              'lr': 5e-2, 'weight_decay': 0.0}])
    n, bs = len(tr['values']), 4096
    spe = math.ceil(n / bs)
    total, warm = max_epochs * spe, 3 * spe
    sched = torch.optim.lr_scheduler.LambdaLR(
        opt, lambda s: (s + 1) / warm if s < warm else
        0.05 + 0.95 * 0.5 * (1 + math.cos(math.pi * (s - warm) / max(1, total - warm))))
    val_hidden = [M.draw_hidden(te['values'], gen, te['unknown']) for _ in range(4)]

    @torch.no_grad()
    def draw_fills(vals, srv_rows, rep_band):
        model.eval()
        order = torch.randperm(len(LATENT), generator=gen, device=device).tolist()
        sub = vals[srv_rows]
        beliefs = {}
        for k in order:
            v = LATENT[k]
            zero = rep_band[v][srv_rows] == 0
            hid = torch.zeros(len(sub), len(P), dtype=torch.bool, device=device)
            hid[:, jl[v]] = True
            lp = model(sub, hid)[:, jl[v], :NB]
            beliefs[v] = model(sub, hid | ANCHOR_HIDE[None, :])[:, jl[v], :NB]
            if zero.any():
                _, l1mg = log_g(k)
                logits = (lp + l1mg)[zero]
                u = torch.rand(logits.shape, generator=gen, device=device).clamp_min(1e-20)
                col = sub[:, C + jl[v]].clone()
                col[zero] = (logits - torch.log(-torch.log(u))).argmax(1)
                sub[:, C + jl[v]] = col
        vals[srv_rows] = sub
        model.train()
        return beliefs

    @torch.no_grad()
    def update_factor():
        model.eval()
        for k, v in enumerate(LATENT):
            rep = (tr['srv'] & (tr['rep_band'][v] > 0)).nonzero().squeeze(1)
            lg, _ = log_g(k)
            qs = []
            for i in range(0, len(rep), 20_000):
                idx = rep[i:i + 20_000]
                hid = torch.zeros(len(idx), len(P), dtype=torch.bool, device=device)
                hid[:, jl[v]] = True
                lp = model(tr['values'][idx], hid | ANCHOR_HIDE[None, :])[:, jl[v], :NB] + lg
                qs.append(torch.where(pos, lp, torch.full_like(lp, -math.inf)).softmax(1))
            q, r_obs, w = torch.cat(qs), tr['rep_band'][v][rep], tr['weight'][rep]
            ll = [float((w * torch.log((q @ transfer(v, f)).gather(1, r_obs[:, None]).squeeze(1)
                                       .clamp_min(1e-12))).sum()) for f in FACTOR_GRID]
            factor[v] = float(FACTOR_GRID[int(np.argmax(ll))])
        model.train()
        reporter_bands(tr)
        reporter_bands(te)

    log, began = [], time.time()
    for epoch in range(1, max_epochs + 1):
        model.train()
        t0 = time.time()
        order = torch.randperm(n, generator=gen, device=device)
        tot = 0.0
        for start in range(0, n, bs):
            idx = order[start:start + bs]
            beliefs = None
            if epoch >= FILL_FROM_EPOCH:
                vals = tr['values'][idx]
                beliefs = draw_fills(vals, tr['srv'][idx], {v: tr['rep_band'][v][idx] for v in LATENT})
                tr['values'][idx] = vals
            values, weight, unknown = tr['values'][idx], tr['weight'][idx], tr['unknown'][idx]
            hidden = M.draw_hidden(values, gen, unknown)
            nll = M.slot_nll(model(values, hidden | unknown), values)
            scored = hidden & ~unknown
            drop = tr['srv'][idx][:, None] & LATENT_COL[None, :] & hidden[:, j_nw][:, None]
            w = weight[:, None] * (scored & ~drop)
            loss = (nll * w).sum() / w.sum()
            if beliefs is not None:
                srv = tr['srv'][idx]
                ws = weight[srv]
                for k, v in enumerate(LATENT):
                    lg, l1mg = log_g(k)
                    reported = tr['rep_band'][v][idx][srv] > 0
                    ll = torch.where(reported, torch.logsumexp(beliefs[v] + lg, 1),
                                     torch.logsumexp(beliefs[v] + l1mg, 1))
                    loss = loss - (ws * ll).sum() / ws.sum()
            opt.zero_grad()
            loss.backward()
            opt.step()
            sched.step()
            tot += loss.item() * len(idx)
        if epoch >= FILL_FROM_EPOCH and FACTOR_FIXED is None:
            update_factor()
        elif epoch == FILL_FROM_EPOCH:
            reporter_bands(tr)
            reporter_bands(te)
        model.eval()
        with torch.no_grad():
            val = float(np.mean([M.masked_loss(model, te['values'], te['weight'], h, None,
                                               te['unknown']).item() for h in val_hidden]))
        row = dict(epoch=epoch, train_loss=tot / n, val_loss=val, seconds=time.time() - t0)
        for k, v in enumerate(LATENT):
            a, b_ = float(alpha[k].detach()), float(beta[k].detach())
            for x in (50, 500, 5000):
                row[f'{v}_report_at_{x}'] = 1 / (1 + math.exp(-(a + b_ * (math.log(x) - LN_CENTRE))))
            row[f'{v}_factor'] = factor[v]
        log.append(row)
        print(' '.join(f'{k} {v:.4f}' if isinstance(v, float) else f'{k} {v}' for k, v in row.items()),
              flush=True)
    log = pd.DataFrame(log)
    log.to_csv(os.path.join(M.MODEL_DIR, 'train_log.csv'), index=False)
    torch.save({'state': model.state_dict(), 'settings': model.settings, 'band_layout': M.BAND_LAYOUT},
               os.path.join(M.MODEL_DIR, 'model.pt'))

    # the model's share of hidden holders among the SCF's reported zeros
    model.eval()
    shares = {}
    with torch.no_grad():
        for k, v in enumerate(LATENT):
            zero = (tr['srv'] & (tr['rep_band'][v] == 0)).nonzero().squeeze(1)
            p_hold, w = [], tr['weight'][zero]
            for i in range(0, len(zero), 20_000):
                idx = zero[i:i + 20_000]
                hid = torch.zeros(len(idx), len(P), dtype=torch.bool, device=device)
                hid[:, jl[v]] = True
                _, l1mg = log_g(k)
                q = (model(tr['values'][idx], hid)[:, jl[v], :NB] + l1mg).softmax(1)
                p_hold.append(1 - q[:, 0])
            p_hold = torch.cat(p_hold)
            shares[v] = {'hidden holders among SCF zeros (%)': float(100 * (w * p_hold).sum() / w.sum()),
                         'SCF units reporting any (%)': float(100 * (tr['weight'][tr['srv'] & (tr['rep_band'][v] > 0)].sum()
                                                                     / tr['weight'][tr['srv']].sum())),
                         'final dollar factor': factor[v],
                         'chance of reporting at $50, $500, $5,000 (%)': [
                             100 * log[f'{v}_report_at_{x}'].iloc[-1] for x in (50, 500, 5000)]}
    # How well the reporting layer explains what SCF units report (step 25): for each
    # SCF unit and item, the chance of its reported slice given only what the SCF
    # reports truly (the anchored belief): reported $0 -> sum_b q(b) (1 - g(b));
    # reported slice r -> sum_b q(b) g(b) T_f(b, r). Mean log per unit, weighted,
    # for the training units and the held-back 2% separately.
    fit = {}
    with torch.no_grad():
        for split, d in [('train', tr), ('held back', te)]:
            for k, v in enumerate(LATENT):
                rows = d['srv'].nonzero().squeeze(1)
                lg, l1mg = log_g(k)
                Tm = transfer(v, factor[v])
                tot_ll, tot_w, zero_ll, zero_w = 0.0, 0.0, 0.0, 0.0
                for i in range(0, len(rows), 20_000):
                    idx = rows[i:i + 20_000]
                    hid = torch.zeros(len(idx), len(P), dtype=torch.bool, device=device)
                    hid[:, jl[v]] = True
                    q = model(d['values'][idx], hid | ANCHOR_HIDE[None, :])[:, jl[v], :NB].softmax(1)
                    r_obs = d['rep_band'][v][idx]
                    p_zero = (q * l1mg.exp()).sum(1)
                    p_amt = ((q * lg.exp()) @ Tm).gather(1, r_obs[:, None]).squeeze(1)
                    p = torch.where(r_obs == 0, p_zero, p_amt).clamp_min(1e-12)
                    w = d['weight'][idx]
                    tot_ll += float((w * p.log()).sum()); tot_w += float(w.sum())
                    z = r_obs == 0
                    zero_ll += float((w * torch.where(z, p_zero, 1 - p_zero).clamp_min(1e-12).log()).sum())
                fit[f'{v}, {split}'] = {'mean log chance of what was reported (nats)': tot_ll / tot_w,
                                        'of which: zero or not (nats)': zero_ll / tot_w}
    meta = dict(table=M.TABLE, flags=M.flags(), predicted=P, seed=seed, max_epochs=max_epochs,
                beta_fixed=BETA_FIXED, factor_fixed=FACTOR_FIXED, fit=fit,
                minutes=(time.time() - began) / 60, latent=LATENT, centre_dollars=MID,
                last_val_loss=float(log['val_loss'].iloc[-1]), settings=shares,
                rows=table.groupby(['source', 'split']).size().to_dict().__repr__())
    json.dump(meta, open(os.path.join(M.MODEL_DIR, 'train_meta.json'), 'w'), indent=2)
    print(json.dumps(meta, indent=2))


if __name__ == '__main__':
    main()
