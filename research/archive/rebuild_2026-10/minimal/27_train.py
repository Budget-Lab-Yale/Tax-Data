"""
27_train.py  (minimal example, step 27: the mapping learned in pretraining
with the source label on; 27_MAPPING_SPEC.md, John's sign-off of 17 October
2026. Results: 27_RESULTS.md.)

What it does
  Trains the model of table E27 (2015 PUF with capital gains pieces and the
  crude non-filers; the SCF waves) with the source label ON (plan decision 14
  as changed on 17 October) and the SCF's interest, dividends and capital
  gains read through a reporting rule (decision 18), as step 26 did on the
  fake survey (22_fake_survey.py, label-on arms), here on the real SCF:

  - Every row is encoded as today's model sees it: SCF rows carry the SCF
    label and their wave's year token, PUF rows the PUF label; each source's
    own slots are absent on the other's rows.
  - Family A of the spec, for each mapped item (interest, dividends, the
    line-13 capital gains total): a holder reports anything with chance
    g = 1 / (1 + exp(-(alpha + beta (ln|x| - ln 150)))), and a reporter gives
    a fixed share f of the amount (the dollar factor). For capital gains |x|
    is the size of the gain or loss.
  - The settings are fit on the 2016 wave (tax year 2015) only, on 80% of its
    households (all five implicates of a household together); the other 20%
    are held back for the scores (27_score.py). They are fit against the
    PUF-labelled belief about the true amount given only married, dependents
    and wages: the row asked under the PUF label and the 2015 year token, its
    SCF-owned slot (net worth) absent, the PUF-owned slots present and
    hidden, every other slot hidden. Only PUF rows teach that belief. The
    S-curve by gradient steps on whether each unit reports anything; the
    dollar factor once per pass by a grid search on the reporters' slices.
    The same settings are used for every wave: a wave's dollars are read on
    its own ruler (the PUF's 2015 cut points grown by the IRS growth in the
    item), so the chance of reporting at a position of the PUF's scale is
    the same in every wave.
  - Filling in, every pass: each SCF zero gets a true slice drawn from the
    belief under the SCF label (its own year) given everything else on the
    record, times the chance of reporting zero at that slice. A reporter's
    true amount is its reported dollars over the dollar factor; for capital
    gains a reported loss beyond $3,000 is read as the after-limit amount,
    -$3,000 (the after-limit basis of the spec, section 2), which on the
    E27 slices is the same slice. The SCF-labelled model trains on these.
  - Family B (rank-preserving) is not used in training; its score against
    family A's is computed after training by 27_score.py on the held-back
    households, with the final belief. See 27_RESULTS.md.

  MINIMAL_SCF_YEARS=2015 keeps only that wave's SCF rows (run R1).
  MINIMAL_LATENT=interest,dividends,capital_gains (default) picks the items.
  Everything else as step 23: 80 passes, batch 4,096, the v4 network, all
  records but the 2% held-back slice (02_model.py MINIMAL_ALL_RECORDS).

Writes model/<run>/: model.pt, train_log.csv, train_meta.json (settings per
pass, the held-back household ids, hidden-holder shares), fills.parquet
(five draws of the filled-in true slice per SCF row and item, from the final
model and settings).
Usage (Slurm, 27_run.sbatch): MINIMAL_TABLE=E27 MINIMAL_RUN=step27/R2s1
  python research/rebuild/minimal/27_train.py
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
LATENT = [v for v in os.environ.get('MINIMAL_LATENT', 'interest,dividends,capital_gains').split(',') if v]
MID = 150.0
FILL_FROM_EPOCH = 3
# MINIMAL_FACTOR_GRID=lo,hi (step 27 sensitivity run R2W; default 0.5,2.0 as step 23):
# the range of the dollar factor's grid search, 81 points evenly spaced in logs.
_lo, _hi = (float(x) for x in os.environ.get('MINIMAL_FACTOR_GRID', '0.5,2.0').split(','))
FACTOR_GRID = np.exp(np.linspace(math.log(_lo), math.log(_hi), 81))
FIT_YEAR = int(os.environ.get('MINIMAL_FIT_YEAR', '2015'))
HOLD_SHARE, HOLD_SEED = 0.20, 20261017
SCF_YEARS = [int(y) for y in os.environ.get('MINIMAL_SCF_YEARS', '').split(',') if y.strip()]
FACTOR_FIXED = os.environ.get('MINIMAL_LATENT_FACTOR_FIXED')
LOSS_LIMIT = -3000.0
# MINIMAL_FAMILY_B=1 (step 27, spec sections 3 and 4): both families in
# training. Family B (rank-preserving, zeros included): the survey household
# at the p-th percentile of the 2016 wave's fit households is the PUF-side
# household at the p-th percentile; in slices, B gives a reported slice r
# from a true slice b the share of b's percentile interval lying in r's.
# Each pass, for each item, one family is drawn for the whole survey with the
# current probability (one half each to start); under B every SCF row's true
# slice (reporters too) is drawn from the SCF-labelled belief times that
# share. After each pass the probability is set from the two families' log
# scores on the held-back households (equal prior odds). Unset: family A only.
FAMILY_B = os.environ.get('MINIMAL_FAMILY_B') == '1'
# MINIMAL_FILL_FROM=puf (step 27, diagnostic run R2P; unset = 'scf', the spec):
# the fill-ins drawn from the PUF-labelled belief (2015 token, net worth
# absent, PUF-owned slots present and hidden) given the record's married,
# dependents, wages and the other mapped items, instead of the SCF-labelled
# belief given everything on the record. Added after the first real runs
# showed the SCF-labelled fill-ins for capital gains drifting far above the
# IRS count of holders (27_RESULTS.md).
# MINIMAL_SETTINGS_FIT=joint (step 27, runs with J in the name; unset = 'split',
# as steps 22, 23 and 26): the S-curve's gradient uses the chance of exactly
# what each unit reported (zero: sum_b q(b) (1 - g(b)); a slice r:
# sum_b q(b) g(b) T_f(b, r)), the score of spec section 3, instead of the
# chance of reporting anything. The dollar factor's grid search maximises the
# same quantity, so the two steps climb one likelihood. Added after the first
# runs: with only married, dependents and wages in the anchored belief, the
# split fit ran the dollar factor to the edge of its grid on the fake survey
# as well as on the real SCF (27_RESULTS.md).
SETTINGS_FIT = os.environ.get('MINIMAL_SETTINGS_FIT', 'split')
assert SETTINGS_FIT in ('split', 'joint'), SETTINGS_FIT
FILL_FROM = os.environ.get('MINIMAL_FILL_FROM', 'scf')
assert FILL_FROM in ('scf', 'puf'), FILL_FROM
FILL_SHARE = {}


def rank_map(pf, ps, NB, POS0):
    """T[b, r]: share of PUF slice b's percentile interval inside survey slice
    r's, slices in dollar order (negatives, zero, positives)."""
    order = list(range(1, POS0)) + [0] + list(range(POS0, NB))
    cf = np.cumsum(pf[order]); lo_f = np.r_[0, cf[:-1]]
    cs = np.cumsum(ps[order]); lo_s = np.r_[0, cs[:-1]]
    T = np.zeros((NB, NB))
    for i, b in enumerate(order):
        width = cf[i] - lo_f[i]
        if width <= 0:
            T[b, b] = 1.0
            continue
        for j, r in enumerate(order):
            ov = min(cf[i], cs[j]) - max(lo_f[i], lo_s[j])
            if ov > 0:
                T[b, r] = ov / width
    return T


def load_model_module():
    assert os.environ.get('MINIMAL_NO_SOURCE') != '1', 'label on: MINIMAL_NO_SOURCE must be unset'
    os.environ['MINIMAL_ALL_RECORDS'] = '1'
    spec = importlib.util.spec_from_file_location('model02', f'{HERE}/02_model.py')
    M = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(M)
    assert M.ALL_RECORDS and not (M.NO_SOURCE or M.NO_LABEL_SHARED or M.REPORT_CURVE or M.COARSEN)
    return M


def fit_households(table):
    """The 2016-wave households held back from fitting the mapping: 20%, by
    household, all implicates together (seed HOLD_SEED)."""
    sel = (table.source == 'scf') & (table.year == FIT_YEAR)
    ids = np.sort(table.loc[sel, 'household_id'].unique())
    u = np.random.default_rng(HOLD_SEED).random(len(ids))
    return set(ids[u < HOLD_SHARE].tolist()), len(ids)


def band_signed(x, edge, NB, POS0):
    """Slices of signed dollars on an edge record (01_band.py's rule)."""
    x = np.asarray(x, float)
    b = np.zeros(len(x), dtype=np.int64)
    neg, pos = x < 0, x > 0
    nc = np.array(edge['negative']['cuts']) if edge.get('negative', {}).get('cuts') else np.array([])
    if neg.any():
        assert len(nc), 'negatives with no negative cuts'
        b[neg] = 1 + np.searchsorted(nc, x[neg], side='left')
    if pos.any():
        b[pos] = POS0 + np.searchsorted(np.array(edge['positive']['cuts']), x[pos], side='left')
    return np.minimum(b, NB - 1)


def main():
    M = load_model_module()
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    seed, max_epochs = M.SEED, int(os.environ.get('MINIMAL_MAX_EPOCHS', '80'))
    torch.manual_seed(seed)
    gen = torch.Generator(device=device).manual_seed(seed)
    os.makedirs(M.MODEL_DIR, exist_ok=True)
    table = M.read_table()
    if SCF_YEARS:
        keep = (table.source != 'scf') | table.year.isin(SCF_YEARS)
        print(f'MINIMAL_SCF_YEARS={SCF_YEARS}: dropping {int((~keep).sum())} SCF rows of other waves', flush=True)
        table = table[keep].reset_index(drop=True)
    edges = json.load(open(M.BAND_EDGES))
    held_hh, n_hh = fit_households(table)
    table['fit_role'] = np.where((table.source == 'scf') & (table.year == FIT_YEAR),
                                 np.where(table.household_id.isin(held_hh), 'held', 'fit'), '')
    C, P = len(M.CONTEXT), M.PREDICTED
    NB = {v: M.N_CHOICES[v] for v in LATENT}
    POS0 = {v: M.BAND_LAYOUT[v]['first_positive'] for v in LATENT}
    jl = {v: P.index(v) for v in LATENT}
    KEEP = torch.tensor([v in ('married', 'n_dep', 'wages') for v in P], device=device)
    SCF_OWNED = [P.index(v) for v, o in M.OWNED_BY.items() if o == 'scf']
    PUF_OWNED = [P.index(v) for v, o in M.OWNED_BY.items() if o == 'puf']
    y2015 = M.YEARS.index(2015)

    def enc(part):
        d = M.encode(part, device)
        srv = torch.tensor((part['source'] == 'scf').to_numpy(), device=device)
        fit = torch.tensor((part['fit_role'] == 'fit').to_numpy(), device=device)
        held = torch.tensor((part['fit_role'] == 'held').to_numpy(), device=device)
        rep_band = {v: d['values'][:, C + jl[v]].clone() for v in LATENT}
        dollars = {v: part[v].to_numpy(dtype=np.float64) for v in LATENT}
        years = part['year'].to_numpy()
        unknown = torch.zeros(len(part), len(P), dtype=torch.bool, device=device)
        return dict(values=d['values'], weight=d['weight'], unknown=unknown, srv=srv, fit=fit, held=held,
                    rep_band=rep_band, dollars=dollars, years=years,
                    scf_np=(part['source'] == 'scf').to_numpy())
    tr = enc(table[table.split == 'train'])
    te = enc(table[table.split == 'test'])
    puf_rows = (table.source == 'puf').to_numpy()

    # per slice of each item: PUF-side mean log |dollars| and 32 points of its dollars
    lnx, points = {}, {}
    for v in LATENT:
        x, b = table.loc[puf_rows, v].to_numpy(float), table.loc[puf_rows, f'{v}_band'].to_numpy()
        w = table.loc[puf_rows, 'weight'].to_numpy(float)
        l, pts = np.full(NB[v], math.log(MID)), np.zeros((NB[v], 32))
        for k in range(1, NB[v]):
            m_ = b == k
            if m_.any():
                l[k] = np.average(np.log(np.abs(x[m_])), weights=w[m_])
                o = np.argsort(x[m_], kind='stable')
                xs, F = x[m_][o], np.cumsum(w[m_][o]) / w[m_].sum()
                pts[k] = xs[np.minimum(np.searchsorted(F, (np.arange(32) + 0.5) / 32), len(xs) - 1)]
            else:
                pts[k] = np.nan
        lnx[v] = torch.tensor(l, dtype=torch.float32, device=device)
        points[v] = pts
    holder = {v: torch.arange(NB[v], device=device) != 0 for v in LATENT}
    LN_CENTRE = math.log(MID)
    alpha = torch.nn.Parameter(torch.zeros(len(LATENT), device=device))
    beta = torch.nn.Parameter(torch.zeros(len(LATENT), device=device))
    factor = {v: (float(FACTOR_FIXED) if FACTOR_FIXED is not None else 1.0) for v in LATENT}

    def log_g(k):
        v = LATENT[k]
        z = alpha[k] + beta[k] * (lnx[v] - LN_CENTRE)
        lg, l1mg = torch.nn.functional.logsigmoid(z), torch.nn.functional.logsigmoid(-z)
        lg = torch.where(holder[v], lg, torch.full_like(lg, -math.inf))
        l1mg = torch.where(holder[v], l1mg, torch.zeros_like(l1mg))
        return lg, l1mg

    def true_dollars(v, x, f):
        t = x / f
        if v == 'capital_gains':
            t = np.where(t < LOSS_LIMIT, LOSS_LIMIT, t)
        return t

    def reporter_bands(d, items=None):
        """Each SCF reporter's true slice: reported dollars over the factor, on
        its own wave's ruler."""
        for v in (items or LATENT):
            col = d['values'][:, C + jl[v]].clone()
            for y in np.unique(d['years'][d['scf_np']]):
                rows = d['scf_np'] & (d['years'] == y) & (d['dollars'][v] != 0)
                if rows.any():
                    b = band_signed(true_dollars(v, d['dollars'][v][rows], factor[v]), edges[f'scf/{y}/{v}'],
                                    NB[v], POS0[v])
                    col[torch.tensor(rows, device=device)] = torch.tensor(b, device=device)
            d['values'][:, C + jl[v]] = col

    # check: at factor 1 the reporters' slices are the table's own; rep_band is
    # then the reported slice (capital gains: losses beyond $3,000 at -$3,000)
    kept_factor = dict(factor)
    factor.update({v: 1.0 for v in LATENT})
    for d in (tr, te):
        before = {v: d['values'][:, C + jl[v]].clone() for v in LATENT}
        reporter_bands(d)
        for v in LATENT:
            same = (before[v] == d['values'][:, C + jl[v]])
            n_diff = int((~same).sum())
            print(f'{v}: reporters re-sliced at factor 1 differ from the table on {n_diff} rows '
                  f'(capital gains: losses beyond $3,000 read as -$3,000)', flush=True)
            if v != 'capital_gains':
                assert n_diff == 0, v
        d['rep_band'] = {v: d['values'][:, C + jl[v]].clone() for v in LATENT}
    factor.update(kept_factor)
    reporter_bands(tr)
    reporter_bands(te)

    # family B's slice map, from the PUF side and the 2016 wave's fit households
    fam = {v: 'A' for v in LATENT}
    pA = {v: (0.5 if FAMILY_B else 1.0) for v in LATENT}
    logTB = {}
    fitrows = (table.fit_role == 'fit').to_numpy()
    for v in LATENT:
        pf = np.bincount(table.loc[puf_rows, f'{v}_band'], weights=table.loc[puf_rows, 'weight'], minlength=NB[v])
        ps = np.bincount(table.loc[fitrows, f'{v}_band'], weights=table.loc[fitrows, 'weight'], minlength=NB[v])
        TB = rank_map(pf / pf.sum(), ps / ps.sum(), NB[v], POS0[v])
        logTB[v] = torch.tensor(np.log(np.clip(TB, 1e-300, None)), dtype=torch.float32, device=device)
    hd = enc(table[table.fit_role == 'held']) if FAMILY_B else None

    def transfer(v, f):
        cut = edges[f'puf/2015/{v}']
        T = np.zeros((NB[v], NB[v]))
        for b in range(1, NB[v]):
            if np.isnan(points[v][b]).any():
                T[b, b] = 1.0
                continue
            r = band_signed(true_dollars(v, points[v][b] * f, 1.0), cut, NB[v], POS0[v])
            np.add.at(T[b], r, 1.0 / len(r))
        return torch.tensor(T, dtype=torch.float32, device=device)

    def anchor_view(vals):
        """The row under the PUF label and the 2015 token, SCF-owned slots
        absent, PUF-owned slots present and hidden; hide everything but
        married, n_dep, wages."""
        view = vals.clone()
        view[:, 0] = M.SOURCES.index('puf')
        view[:, 1] = y2015
        for j in SCF_OWNED:
            view[:, C + j] = -1
        for j in PUF_OWNED:
            view[:, C + j] = 0
        hid = ~KEEP[None, :].expand(len(vals), -1) & (view[:, C:] >= 0)
        return view, hid

    model = M.SlotTransformer().to(device)
    opt = torch.optim.AdamW([{'params': model.parameters(), 'lr': 1e-3, 'weight_decay': 0.01},
                             {'params': [alpha, beta], 'lr': 5e-2, 'weight_decay': 0.0}])
    n, bs = len(tr['values']), 4096
    spe = math.ceil(n / bs)
    total, warm = max_epochs * spe, 3 * spe
    sched = torch.optim.lr_scheduler.LambdaLR(
        opt, lambda s: (s + 1) / warm if s < warm else
        0.05 + 0.95 * 0.5 * (1 + math.cos(math.pi * (s - warm) / max(1, total - warm))))
    val_hidden = [M.draw_hidden(te['values'], gen, te['unknown']) for _ in range(4)]

    @torch.no_grad()
    def anchored_beliefs(vals):
        view, hid = anchor_view(vals)
        lp = model(view, hid)
        return {v: lp[:, jl[v], :NB[v]] for v in LATENT}

    @torch.no_grad()
    def draw_fills(vals, srv_rows, rep_band):
        model.eval()
        order = torch.randperm(len(LATENT), generator=gen, device=device).tolist()
        sub = vals[srv_rows]
        for k in order:
            v = LATENT[k]
            if fam[v] == 'B':          # every row: belief times B's share for the reported slice
                zero = torch.ones(len(sub), dtype=torch.bool, device=device)
            else:
                zero = rep_band[v][srv_rows] == 0
            if not zero.any():
                continue
            hid = torch.zeros(len(sub), len(P), dtype=torch.bool, device=device)
            hid[:, jl[v]] = True
            if FILL_FROM == 'puf':
                view = sub.clone()
                view[:, 0] = M.SOURCES.index('puf')
                view[:, 1] = y2015
                for j in SCF_OWNED:
                    view[:, C + j] = -1
                for j in PUF_OWNED:
                    view[:, C + j] = 0
                    hid[:, j] = True
                lp = model(view, hid)[:, jl[v], :NB[v]]
            else:
                lp = model(sub, hid)[:, jl[v], :NB[v]]
            if fam[v] == 'B':
                logits = lp + logTB[v][:, rep_band[v][srv_rows]].T
            else:
                _, l1mg = log_g(k)
                logits = (lp + l1mg)[zero]
            u = torch.rand(logits.shape, generator=gen, device=device).clamp_min(1e-20)
            col = sub[:, C + jl[v]].clone()
            col[zero] = (logits - torch.log(-torch.log(u))).argmax(1)
            sub[:, C + jl[v]] = col
            z0 = rep_band[v][srv_rows] == 0
            a, b_ = FILL_SHARE.get(v, (0, 0))
            FILL_SHARE[v] = (a + int((col[z0] != 0).sum()), b_ + int(z0.sum()))
        vals[srv_rows] = sub
        model.train()

    @torch.no_grad()
    def update_factor():
        """Grid search for the dollar factor: weighted log likelihood of the
        fit reporters' slices, q(b) g(b) T_f(b, r) over the holder slices."""
        model.eval()
        for k, v in enumerate(LATENT):
            rep = (tr['fit'] & (torch.tensor(tr['dollars'][v] != 0, device=device))).nonzero().squeeze(1)
            lg, _ = log_g(k)
            qs = []
            for i in range(0, len(rep), 20_000):
                lp = anchored_beliefs(tr['values'][rep[i:i + 20_000]])[v] + lg
                qs.append(torch.where(holder[v], lp, torch.full_like(lp, -math.inf)).softmax(1))
            q, r_obs, w = torch.cat(qs), tr['rep_band'][v][rep], tr['weight'][rep]
            ll = [float((w * torch.log((q @ transfer(v, f)).gather(1, r_obs[:, None]).squeeze(1)
                                       .clamp_min(1e-12))).sum()) for f in FACTOR_GRID]
            factor[v] = float(FACTOR_GRID[int(np.argmax(ll))])
        model.train()
        # the reporters' true slices change with the factor; rep_band (the reported
        # slice, on the wave's ruler) does not
        reporter_bands(tr)
        reporter_bands(te)

    @torch.no_grad()
    def score_families():
        """Log scores of A and B on the held-back households (implicates
        averaged: sum over unit rows / 5); sets pA."""
        model.eval()
        out = {}
        for k, v in enumerate(LATENT):
            lg, l1mg = log_g(k)
            Tm = transfer(v, factor[v])
            TB = logTB[v].exp()
            la = lb = 0.0
            for i in range(0, len(hd['values']), 20_000):
                q = anchored_beliefs(hd['values'][i:i + 20_000])[v].softmax(1)
                r = hd['rep_band'][v][i:i + 20_000]
                p_zero = (q * l1mg.exp()).sum(1)
                p_amt = ((q * lg.exp()) @ Tm).gather(1, r[:, None]).squeeze(1)
                la += float(torch.where(r == 0, p_zero, p_amt).clamp_min(1e-300).log().sum())
                lb += float((q @ TB).gather(1, r[:, None]).squeeze(1).clamp_min(1e-300).log().sum())
            la, lb = la / 5, lb / 5
            pA[v] = 1 / (1 + math.exp(max(-700, min(700, lb - la))))
            out[v] = (la, lb)
        model.train()
        return out

    def draw_families():
        for v in LATENT:
            new = 'A' if float(torch.rand(1, generator=gen, device=device)) < pA[v] else 'B'
            if new == 'A' and fam[v] == 'B':
                reporter_bands(tr, [v])
                reporter_bands(te, [v])
            fam[v] = new

    logT = {}

    def set_logT():
        for v in LATENT:
            logT[v] = torch.log(transfer(v, factor[v]).clamp_min(1e-12))
    set_logT()

    log, began = [], time.time()
    fam_scores = {v: (float('nan'), float('nan')) for v in LATENT}
    for epoch in range(1, max_epochs + 1):
        model.train()
        t0 = time.time()
        if FAMILY_B and epoch >= FILL_FROM_EPOCH:
            draw_families()
        order = torch.randperm(n, generator=gen, device=device)
        tot = 0.0
        for start in range(0, n, bs):
            idx = order[start:start + bs]
            fill = epoch >= FILL_FROM_EPOCH
            if fill:
                vals = tr['values'][idx]
                draw_fills(vals, tr['srv'][idx], {v: tr['rep_band'][v][idx] for v in LATENT})
                tr['values'][idx] = vals
            values, weight, unknown = tr['values'][idx], tr['weight'][idx], tr['unknown'][idx]
            hidden = M.draw_hidden(values, gen, unknown)
            nll = M.slot_nll(model(values, hidden), values)
            w = weight[:, None] * hidden
            loss = (nll * w).sum() / w.sum()
            fit = tr['fit'][idx]
            if fill and fit.any():
                beliefs = anchored_beliefs(values[fit])
                ws = weight[fit]
                for k, v in enumerate(LATENT):
                    lg, l1mg = log_g(k)
                    r_fit = tr['rep_band'][v][idx][fit]
                    reported = r_fit != 0
                    if SETTINGS_FIT == 'joint':
                        amt = torch.logsumexp(beliefs[v] + lg + logT[v][:, r_fit].T, 1)
                    else:
                        amt = torch.logsumexp(beliefs[v] + lg, 1)
                    ll = torch.where(reported, amt, torch.logsumexp(beliefs[v] + l1mg, 1))
                    loss = loss - (ws * ll).sum() / ws.sum()
            opt.zero_grad()
            loss.backward()
            opt.step()
            sched.step()
            tot += loss.item() * len(idx)
        if epoch >= FILL_FROM_EPOCH and FACTOR_FIXED is None:
            update_factor()
            set_logT()
        if FAMILY_B and epoch >= FILL_FROM_EPOCH - 1:
            fam_scores = score_families()
        model.eval()
        with torch.no_grad():
            val = float(np.mean([M.masked_loss(model, te['values'], te['weight'], h, None,
                                               te['unknown']).item() for h in val_hidden]))
        row = dict(epoch=epoch, train_loss=tot / n, val_loss=val, seconds=time.time() - t0)
        for k, v in enumerate(LATENT):
            a, b_ = float(alpha[k].detach()), float(beta[k].detach())
            row[f'{v}_alpha'], row[f'{v}_beta'] = a, b_
            for x in (50, 500, 5000):
                row[f'{v}_report_at_{x}'] = 1 / (1 + math.exp(-(a + b_ * (math.log(x) - LN_CENTRE))))
            row[f'{v}_factor'] = factor[v]
            a, b_ = FILL_SHARE.pop(v, (0, 0))
            row[f'{v}_zeros_filled_as_holders_this_pass'] = a / b_ if b_ else float('nan')
            row[f'{v}_family_this_pass'] = fam[v]
            row[f'{v}_prob_A'] = pA[v]
            row[f'{v}_logscore_A'], row[f'{v}_logscore_B'] = fam_scores[v]
        log.append(row)
        print(' '.join(f'{k} {v:.4f}' if isinstance(v, float) else f'{k} {v}' for k, v in row.items()),
              flush=True)
    log = pd.DataFrame(log)
    log.to_csv(os.path.join(M.MODEL_DIR, 'train_log.csv'), index=False)
    torch.save({'state': model.state_dict(), 'settings': model.settings, 'band_layout': M.BAND_LAYOUT},
               os.path.join(M.MODEL_DIR, 'model.pt'))

    # the filled-in true slices: five draws per SCF row and item, final model and settings
    model.eval()
    allt = table.reset_index(drop=True)
    d_all = enc(allt)
    reporter_bands(d_all)            # reporters: true slice = reported dollars over the final factor
    scf_idx = torch.tensor(np.flatnonzero(d_all['scf_np']), dtype=torch.long, device=device)
    fills = {'record_id': allt.loc[d_all['scf_np'], 'record_id'].to_numpy(),
             'household_id': allt.loc[d_all['scf_np'], 'household_id'].to_numpy(),
             'year': allt.loc[d_all['scf_np'], 'year'].to_numpy(),
             'weight': allt.loc[d_all['scf_np'], 'weight'].to_numpy(),
             'fit_role': allt.loc[d_all['scf_np'], 'fit_role'].to_numpy()}
    for v in LATENT:
        fills[f'{v}_reported_band'] = d_all['rep_band'][v][scf_idx].cpu().numpy()
        fills[f'{v}_reported'] = d_all['dollars'][v][d_all['scf_np']]
        fills[f'{v}_reporter_true_band'] = d_all['values'][scf_idx, C + jl[v]].cpu().numpy()
    for draw in range(5):
        if FAMILY_B:
            for v in LATENT:
                fam[v] = 'A' if float(torch.rand(1, generator=gen, device=device)) < pA[v] else 'B'
                fills[f'{v}_family{draw + 1}'] = fam[v]
        base = d_all['values'].clone()
        for i in range(0, len(scf_idx), 20_000):
            idx = scf_idx[i:i + 20_000]
            vals = base[idx]
            srv = torch.ones(len(idx), dtype=torch.bool, device=device)
            for _ in range(2):          # two rounds, so each item sees the other items' fills
                draw_fills(vals, srv, {v: d_all['rep_band'][v][idx] for v in LATENT})
            base[idx] = vals
        model.eval()
        for v in LATENT:
            fills[f'{v}_fill{draw + 1}'] = base[scf_idx, C + jl[v]].cpu().numpy()
    pd.DataFrame(fills).to_parquet(os.path.join(M.MODEL_DIR, 'fills.parquet'), index=False)

    # the model's share of hidden holders among the SCF's reported zeros, per wave
    shares = {}
    with torch.no_grad():
        for k, v in enumerate(LATENT):
            _, l1mg = log_g(k)
            for y in sorted(np.unique(d_all['years'][d_all['scf_np']])):
                zero = torch.tensor(np.flatnonzero(d_all['scf_np'] & (d_all['years'] == y) & (d_all['dollars'][v] == 0)),
                                    dtype=torch.long, device=device)
                p_hold = []
                for i in range(0, len(zero), 20_000):
                    idx = zero[i:i + 20_000]
                    hid = torch.zeros(len(idx), len(P), dtype=torch.bool, device=device)
                    hid[:, jl[v]] = True
                    q = (model(d_all['values'][idx], hid)[:, jl[v], :NB[v]] + l1mg).softmax(1)
                    p_hold.append(1 - q[:, 0])
                p_hold = torch.cat(p_hold) if p_hold else torch.zeros(0, device=device)
                w = torch.tensor(allt['weight'].to_numpy()[zero.cpu().numpy()], dtype=torch.float32, device=device)
                shares[f'{v}, tax year {y}'] = {
                    'hidden holders among SCF zeros (%)': float(100 * (w * p_hold).sum() / w.sum()) if len(w) else None}
            shares[v] = {'final dollar factor': factor[v], 'alpha': float(alpha[k]), 'beta': float(beta[k]),
                         'chance of reporting at $50, $500, $5,000 (%)': [
                             100 * log[f'{v}_report_at_{x}'].iloc[-1] for x in (50, 500, 5000)]}
    meta = dict(table=M.TABLE, flags=M.flags(), predicted=P, seed=seed, max_epochs=max_epochs, latent=LATENT,
                fit_year=FIT_YEAR, scf_years=SCF_YEARS or 'all', hold_share=HOLD_SHARE, hold_seed=HOLD_SEED,
                held_households=sorted(float(h) for h in held_hh), fit_wave_households=n_hh,
                factor_fixed=FACTOR_FIXED, family_b=FAMILY_B, settings_fit=SETTINGS_FIT, fill_from=FILL_FROM,
                factor_grid=[float(FACTOR_GRID[0]), float(FACTOR_GRID[-1])],
                family_probability_A={v: pA[v] for v in LATENT},
                family_log_scores_last_pass={v: list(fam_scores[v]) for v in LATENT}, minutes=(time.time() - began) / 60, centre_dollars=MID,
                last_val_loss=float(log['val_loss'].iloc[-1]), settings=shares,
                rows=table.groupby(['source', 'year', 'split']).size().to_dict().__repr__())
    json.dump(meta, open(os.path.join(M.MODEL_DIR, 'train_meta.json'), 'w'), indent=2)
    print(json.dumps({k: v for k, v in meta.items() if k != 'held_households'}, indent=2))


if __name__ == '__main__':
    main()
