"""
29_train.py  (minimal example, step 29: three rules for how many SCF
zero-reporters are hidden holders, and at what sizes. Status: 29_STATUS.md;
results: 29_RESULTS.md.)

Built from 27_train.py (run R2J: label on, family A only, the S-curve fit on
the chance of exactly what each unit reported, the rule fit on 80% of the 2016
wave's households and used for every wave). Changes, all for step 29:

  - The dollar factor is fixed at 1 for every item (John, 17 October evening:
    reporters' dollars are taken as right). MINIMAL_LATENT_FACTOR_FIXED must
    be 1.
  - Family B, the PUF-labelled fill-in diagnostic and the "split" settings
    fit are removed. Every rule here keeps the transformer deciding WHICH
    zero-reporters are the hidden holders: each pass, each SCF zero gets a
    slice drawn from the SCF-labelled belief given the whole record, times a
    rule-specific factor per slice.
  - Cells: wage group x married. Wage groups by the wages slice, using the
    2015 PUF median dollars of each slice: none; under $25,000; $25,000 to
    $75,000; $75,000 to $200,000; over $200,000 (2015 dollars on the PUF's
    scale; for other waves the same slice, which is on that wave's grown
    ruler). A cell with fewer than MIN_HH distinct 2016-wave SCF households
    is merged with the next lower wage group of the same marital status (the
    next higher for the lowest group), smallest cell first, repeated until
    none is below.

MINIMAL_RULE picks the rule:
  curve      (rule 1) one S-curve per item, the same for every household. The
             factor per slice is the chance of reporting zero at that slice,
             1 - g(b). Fit as R2J (joint fit, anchored PUF-labelled belief
             given married, dependents, wages).
  cellcurve  (rule 2) the S-curve's middle varies by cell; the steepness is
             shared. Fit the same way, alpha per cell.
  cellcount  (rule 3; Larrimore, Mortenson and Splinter's method for
             unemployment insurance in the CPS) no curve. In each cell of the
             2016 wave's fit households: reporters stay at their reported
             amounts; the number of hidden holders is the PUF side's share of
             holders in the cell times the SCF cell's weight, minus the SCF's
             reporters (floored at zero); their sizes, slice by slice, are
             the PUF holders' distribution in the cell (scaled to the SCF
             cell's weight) minus the SCF reporters in that slice, floored at
             zero, then scaled so the slices add up to the number of hidden
             holders. The factor per slice and cell, h(c, b), is refit every
             H_EVERY (10) batches and after every pass by iterative
             proportional fitting so that the
             SCF-labelled belief times h, summed over the cell's fit zeros,
             gives exactly those counts per slice. The same h is used for
             every wave (slices on each wave's ruler).
  cellcount_wave  (rule 3, applied wave by wave; added after the first
             rule-3 runs, whose fill-ins rose pass by pass in the waves that
             only borrowed h) the same 2016-wave cell targets, read as each
             cell's share of zeros that are hidden holders, slice by slice;
             in every wave the counts are that share times the wave's own
             zeros in the cell, and h is fit per wave and cell (2016 wave: on
             its fit households, as in cellcount; other waves: on all their
             training rows).

Writes model/<run>/: model.pt, train_log.csv, train_meta.json (settings,
cells, rule-3 targets), fills.parquet (five draws of the filled-in slice per
SCF row and item, with the row's cell).
Usage (Slurm, 29_run.sbatch): MINIMAL_TABLE=E27 MINIMAL_RUN=step29/C1s1
  MINIMAL_RULE=curve MINIMAL_LATENT_FACTOR_FIXED=1 python 29_train.py
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
FIT_YEAR = int(os.environ.get('MINIMAL_FIT_YEAR', '2015'))
HOLD_SHARE, HOLD_SEED = 0.20, 20261017
SCF_YEARS = [int(y) for y in os.environ.get('MINIMAL_SCF_YEARS', '').split(',') if y.strip()]
FACTOR_FIXED = os.environ.get('MINIMAL_LATENT_FACTOR_FIXED')
LOSS_LIMIT = -3000.0
RULE = os.environ.get('MINIMAL_RULE', 'curve')
assert RULE in ('curve', 'cellcurve', 'cellcount', 'cellcount_wave'), RULE
COUNT_RULE = RULE.startswith('cellcount')
MIN_HH = int(os.environ.get('MINIMAL_CELL_MIN_HH', '300'))
H_EVERY = int(os.environ.get('MINIMAL_H_EVERY', '10'))
# rule 3, how hidden holders are spread over slices (step 29 runs C5, F29C5:
# 'smallest_first'; default 'cumulative' as runs C3, C4). smallest_first: slices
# taken in order of the size of the amount (gains and losses together), each
# given the PUF side's holders minus the SCF's reporters (floored at zero),
# until the cell's count of hidden holders is reached; what is left in larger
# slices is dropped. Added after the fake survey showed the cumulative version
# placing hidden holders in the top slices, where nearly every holder reports
# and the difference is sampling noise (hidden dollars 3 to 10 times the truth).
ALLOC = os.environ.get('MINIMAL_RULE3_ALLOC', 'cumulative')
assert ALLOC in ('cumulative', 'smallest_first'), ALLOC     # rule 3: batches between refits of h
WAGE_GROUPS = [('none', 0, 0), ('under $25k', 0, 25e3), ('$25k-75k', 25e3, 75e3),
               ('$75k-200k', 75e3, 200e3), ('over $200k', 200e3, np.inf)]
FILL_SHARE = {}


def wage_group_of_band(edges):
    """Wage group (0..4) of each wages slice, by the slice's 2015 PUF median."""
    e = edges['puf/2015/wages']
    out = np.zeros(e['n_bands'], dtype=np.int64)
    for bd in e['bands']:
        b, med = bd['band'], bd['median']
        if b == 0:
            out[b] = 0
            continue
        if med is None:                      # unused slice (wages have no negatives)
            out[b] = 1
            continue
        out[b] = 1 + sum(med >= t for t in (25e3, 75e3, 200e3))
    return out


def cell_map(table, edges, fit_year=FIT_YEAR, min_hh=MIN_HH):
    """Cell of every row (wage group x married, small cells merged) and a
    description: label, groups merged, 2016-wave households, rows, weight."""
    wg_of = wage_group_of_band(edges)
    wg = wg_of[table['wages_band'].to_numpy()]
    mar = table['married'].to_numpy().astype(np.int64)
    sel = ((table['source'] == 'scf') & (table['year'] == fit_year)).to_numpy()
    groups = {m: [[g] for g in range(5)] for m in (0, 1)}
    hh = table['household_id'].to_numpy()

    def count(m, gs):
        r = sel & (mar == m) & np.isin(wg, gs)
        return len(np.unique(hh[r]))
    for m in (0, 1):
        while True:
            cnt = [count(m, gs) for gs in groups[m]]
            small = [i for i, c in enumerate(cnt) if c < min_hh]
            if not small or len(groups[m]) == 1:
                break
            i = min(small, key=lambda q: cnt[q])          # the smallest cell first
            j = i - 1 if i > 0 else i + 1
            lo, hi = min(i, j), max(i, j)
            groups[m][lo] = sorted(groups[m][lo] + groups[m][hi])
            del groups[m][hi]
    cell = np.full(len(table), -1, dtype=np.int64)
    info, k = [], 0
    w = table['weight'].to_numpy(float)
    for m in (0, 1):
        for gs in groups[m]:
            r = (mar == m) & np.isin(wg, gs)
            cell[r] = k
            names = [WAGE_GROUPS[g][0] for g in gs]
            info.append({'cell': k, 'label': f"{'married' if m else 'not married'}, wages "
                         + (names[0] if len(names) == 1 else f'{names[0]} to {names[-1]}'),
                         'married': m, 'wage groups': gs,
                         '2016-wave SCF households': count(m, gs),
                         '2016-wave SCF unit rows': int((r & sel).sum()),
                         '2016-wave SCF weight (millions)': float(w[r & sel].sum() / 1e6),
                         'PUF-side records': int((r & (table['source'] == 'puf').to_numpy()).sum()),
                         'PUF-side weight (millions)': float(w[r & (table['source'] == 'puf').to_numpy()].sum() / 1e6)})
            k += 1
    assert (cell >= 0).all(), 'a row with no cell'
    return cell, info


def load_model_module():
    assert os.environ.get('MINIMAL_NO_SOURCE') != '1', 'label on: MINIMAL_NO_SOURCE must be unset'
    os.environ['MINIMAL_ALL_RECORDS'] = '1'
    spec = importlib.util.spec_from_file_location('model02', f'{HERE}/02_model.py')
    M = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(M)
    assert M.ALL_RECORDS and not (M.NO_SOURCE or M.NO_LABEL_SHARED or M.REPORT_CURVE or M.COARSEN)
    return M


def fit_households(table):
    sel = (table.source == 'scf') & (table.year == FIT_YEAR)
    ids = np.sort(table.loc[sel, 'household_id'].unique())
    u = np.random.default_rng(HOLD_SEED).random(len(ids))
    return set(ids[u < HOLD_SHARE].tolist()), len(ids)


def band_signed(x, edge, NB, POS0):
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
    assert FACTOR_FIXED is not None and float(FACTOR_FIXED) == 1.0, 'step 29: MINIMAL_LATENT_FACTOR_FIXED=1'
    M = load_model_module()
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    seed, max_epochs = M.SEED, int(os.environ.get('MINIMAL_MAX_EPOCHS', '80'))
    torch.manual_seed(seed)
    gen = torch.Generator(device=device).manual_seed(seed)
    os.makedirs(M.MODEL_DIR, exist_ok=True)
    table = M.read_table()
    if SCF_YEARS:
        keep = (table.source != 'scf') | table.year.isin(SCF_YEARS)
        table = table[keep].reset_index(drop=True)
    edges = json.load(open(M.BAND_EDGES))
    held_hh, n_hh = fit_households(table)
    table['fit_role'] = np.where((table.source == 'scf') & (table.year == FIT_YEAR),
                                 np.where(table.household_id.isin(held_hh), 'held', 'fit'), '')
    table['cell'], cell_info = cell_map(table, edges)
    NCELL = len(cell_info)
    WAVES = sorted(int(y) for y in table.loc[table.source == 'scf', 'year'].unique())
    NG = len(WAVES) * NCELL if RULE == 'cellcount_wave' else NCELL
    # group of a row for the rule's factor: the cell, or (cellcount_wave) wave x cell
    if RULE == 'cellcount_wave':
        wi = table['year'].map({y: i for i, y in enumerate(WAVES)}).fillna(WAVES.index(FIT_YEAR)).astype(int)
        table['grp'] = wi.to_numpy() * NCELL + table['cell'].to_numpy()
    else:
        table['grp'] = table['cell']
    print('cells:', json.dumps(cell_info, indent=1), flush=True)
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
        rep_band = {v: d['values'][:, C + jl[v]].clone() for v in LATENT}
        dollars = {v: part[v].to_numpy(dtype=np.float64) for v in LATENT}
        unknown = torch.zeros(len(part), len(P), dtype=torch.bool, device=device)
        return dict(values=d['values'], weight=d['weight'], unknown=unknown, srv=srv, fit=fit,
                    rep_band=rep_band, dollars=dollars, years=part['year'].to_numpy(),
                    cell=torch.tensor(part['cell'].to_numpy(), device=device),
                    grp=torch.tensor(part['grp'].to_numpy(), device=device),
                    scf_np=(part['source'] == 'scf').to_numpy())
    tr = enc(table[table.split == 'train'])
    te = enc(table[table.split == 'test'])
    puf_rows = (table.source == 'puf').to_numpy()

    lnx, points, meanx = {}, {}, {}
    for v in LATENT:
        x, b = table.loc[puf_rows, v].to_numpy(float), table.loc[puf_rows, f'{v}_band'].to_numpy()
        w = table.loc[puf_rows, 'weight'].to_numpy(float)
        l, pts, mx = np.full(NB[v], math.log(MID)), np.zeros((NB[v], 32)), np.zeros(NB[v])
        for k in range(1, NB[v]):
            m_ = b == k
            if m_.any():
                l[k] = np.average(np.log(np.abs(x[m_])), weights=w[m_])
                mx[k] = np.average(x[m_], weights=w[m_])
                o = np.argsort(x[m_], kind='stable')
                xs, F = x[m_][o], np.cumsum(w[m_][o]) / w[m_].sum()
                pts[k] = xs[np.minimum(np.searchsorted(F, (np.arange(32) + 0.5) / 32), len(xs) - 1)]
            else:
                pts[k] = np.nan
        lnx[v] = torch.tensor(l, dtype=torch.float32, device=device)
        points[v], meanx[v] = pts, mx
    holder = {v: torch.arange(NB[v], device=device) != 0 for v in LATENT}
    LN_CENTRE = math.log(MID)
    NA = NCELL if RULE == 'cellcurve' else 1
    alpha = torch.nn.Parameter(torch.zeros(len(LATENT), NA, device=device))
    beta = torch.nn.Parameter(torch.zeros(len(LATENT), device=device))

    def middle(a, b_):
        """Dollars at which the curve gives one half (nan if the curve is flat or falling)."""
        return math.exp(min(60.0, max(-60.0, LN_CENTRE - a / b_))) if b_ > 1e-6 else float('nan')

    def cidx(cells):
        return cells if RULE == 'cellcurve' else torch.zeros_like(cells)

    def log_g(k, cells):
        """Per row: log chance of reporting anything, and of reporting zero, at each slice."""
        v = LATENT[k]
        z = alpha[k][cidx(cells)][:, None] + beta[k] * (lnx[v] - LN_CENTRE)[None, :]
        lg, l1mg = torch.nn.functional.logsigmoid(z), torch.nn.functional.logsigmoid(-z)
        lg = torch.where(holder[v][None, :], lg, torch.full_like(lg, -math.inf))
        l1mg = torch.where(holder[v][None, :], l1mg, torch.zeros_like(l1mg))
        return lg, l1mg

    def true_dollars(v, x):
        return np.where(x < LOSS_LIMIT, LOSS_LIMIT, x) if v == 'capital_gains' else x

    # reporters' true slice = reported slice (factor 1; capital gains: a loss
    # beyond $3,000 read as -$3,000, the same slice on E27)
    for d in (tr, te):
        for v in LATENT:
            col = d['values'][:, C + jl[v]].clone()
            for y in np.unique(d['years'][d['scf_np']]):
                rows = d['scf_np'] & (d['years'] == y) & (d['dollars'][v] != 0)
                if rows.any():
                    b = band_signed(true_dollars(v, d['dollars'][v][rows]), edges[f'scf/{y}/{v}'], NB[v], POS0[v])
                    col[torch.tensor(rows, device=device)] = torch.tensor(b, device=device)
            n_diff = int((col != d['values'][:, C + jl[v]]).sum())
            print(f'{v}: reporters re-sliced differ from the table on {n_diff} rows', flush=True)
            if v != 'capital_gains':
                assert n_diff == 0, v
            d['values'][:, C + jl[v]] = col
            d['rep_band'][v] = col.clone()

    def transfer(v):
        cut = edges[f'puf/2015/{v}']
        T = np.zeros((NB[v], NB[v]))
        for b in range(1, NB[v]):
            if np.isnan(points[v][b]).any():
                T[b, b] = 1.0
                continue
            r = band_signed(true_dollars(v, points[v][b]), cut, NB[v], POS0[v])
            np.add.at(T[b], r, 1.0 / len(r))
        return torch.tensor(T, dtype=torch.float32, device=device)
    logT = {v: torch.log(transfer(v).clamp_min(1e-12)) for v in LATENT}

    # rule 3: per-cell target counts by slice for the fit households' zeros
    TGT, LOGH, cell_targets = {}, {}, []
    fit_t = tr['fit']
    for v in LATENT:
        pb = table.loc[puf_rows, f'{v}_band'].to_numpy()
        pw = table.loc[puf_rows, 'weight'].to_numpy(float)
        pc = table.loc[puf_rows, 'cell'].to_numpy()
        tgt = torch.zeros(NCELL, NB[v], dtype=torch.float64)
        for c in range(NCELL):
            m_ = pc == c
            P_cb = np.bincount(pb[m_], weights=pw[m_], minlength=NB[v]) / pw[m_].sum()
            fr = fit_t & (tr['cell'] == c)
            W = float(tr['weight'][fr].double().sum())
            rb = tr['rep_band'][v][fr].cpu().numpy()
            rw = tr['weight'][fr].double().cpu().numpy()
            rep = np.bincount(rb, weights=rw, minlength=NB[v])
            Wz = rep[0]
            rep[0] = 0.0
            n_hid = max(0.0, W * (1 - P_cb[0]) - rep.sum())
            diff = W * P_cb - rep
            diff[0] = 0.0
            # slice by slice, floored at zero, scaled to the count (diagnostic only)
            d_floor = np.maximum(0.0, diff)
            if d_floor.sum() > 0:
                d_floor *= n_hid / d_floor.sum()
            # used: the difference in counts ABOVE each amount (losses: below
            # each loss), cumulated from the largest amounts toward zero and
            # never allowed to fall as the amount falls, so a slice where the
            # SCF has more reporters than the PUF side has holders offsets the
            # slices beyond it instead of being floored on its own (sampling
            # noise in the top slices would otherwise add spurious large hidden
            # holdings); then scaled so the total is the number of hidden holders.
            d_ = np.zeros(NB[v])
            for sl in (list(range(NB[v] - 1, POS0[v] - 1, -1)), list(range(1, POS0[v]))):
                S, H = 0.0, 0.0                    # S: raw cumulative difference; H: its running maximum
                for b in sl:
                    S += diff[b]
                    Hb = max(H, S)                  # hidden holders beyond this amount never fall
                    d_[b] = Hb - H
                    H = Hb
            if ALLOC == 'smallest_first':
                d_ = np.zeros(NB[v])
                left = n_hid
                for b in sorted(range(1, NB[v]), key=lambda q: abs(meanx[v][q]) if meanx[v][q] != 0 else np.inf):
                    take = min(max(0.0, diff[b]), left)
                    d_[b], left = take, left - take
                    if left <= 0:
                        break
            if d_.sum() > 0:
                d_ *= n_hid / d_.sum()
            tgt[c] = torch.tensor(d_)
            tgt[c, 0] = Wz - n_hid
            # dollars on the PUF's 2015 scale (slice mean dollars of the PUF side)
            cell_targets.append({
                'item': v, 'cell': c, 'label': cell_info[c]['label'],
                'SCF fit weight (training units)': W,
                'PUF side: share holding (%)': 100 * (1 - P_cb[0]),
                'SCF reporters: share (%)': 100 * rep.sum() / W,
                'hidden holders: share of all units (%)': 100 * n_hid / W,
                'hidden holders: share of zeros (%)': 100 * n_hid / Wz if Wz > 0 else float('nan'),
                'PUF side: mean dollars per unit': float(P_cb @ meanx[v]),
                'SCF reporters: mean dollars per unit': float(rep @ meanx[v] / W),
                'hidden holders: mean dollars per unit (target, used)': float(d_ @ meanx[v] / W),
                'hidden holders: mean dollars per unit (slice by slice, floored; not used)': float(d_floor @ meanx[v] / W),
                'dollar gap per unit, PUF side minus SCF reporters (what hidden holders would need)':
                    float((W * P_cb - np.r_[0.0, rep[1:]]) @ meanx[v] / W),
                'slices where the PUF side is below the SCF reporters (count; floored)': int(((W * P_cb - rep) < 0)[1:].sum()),
                'weight of reporters above the PUF side in those slices, share of reporters (%)':
                    float(100 * np.maximum(0, rep - W * P_cb)[1:].sum() / max(rep.sum(), 1e-12)),
            })
        if RULE == 'cellcount_wave':
            frac = tgt / tgt.sum(1, keepdim=True).clamp_min(1e-12)     # per cell: share of zeros by slice
            tg = torch.zeros(NG, NB[v], dtype=torch.float64)
            for i, y in enumerate(WAVES):
                for c in range(NCELL):
                    if y == FIT_YEAR:
                        tg[i * NCELL + c] = tgt[c]
                    else:
                        r_ = tr['srv'] & torch.tensor(tr['years'] == y, device=device) & (tr['cell'] == c) & \
                            (tr['rep_band'][v] == 0)
                        tg[i * NCELL + c] = frac[c] * float(tr['weight'][r_].double().sum())
            tgt = tg
        TGT[v] = tgt.to(device=device, dtype=torch.float32)
        LOGH[v] = torch.zeros(NG, NB[v], device=device)
    if COUNT_RULE:
        print('rule 3 cell targets:', json.dumps(cell_targets, indent=1), flush=True)

    def anchor_view(vals):
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

    def zero_adjust(k, cells):
        """log of the rule's factor per slice for a zero-reporter: 1 - g (rules 1, 2) or h (rule 3)."""
        if COUNT_RULE:
            return LOGH[LATENT[k]][cells]
        return log_g(k, cells)[1]

    @torch.no_grad()
    def draw_fills(vals, srv_rows, rep_band, cells):
        model.eval()
        order = torch.randperm(len(LATENT), generator=gen, device=device).tolist()
        sub, sc = vals[srv_rows], cells[srv_rows]
        for k in order:
            v = LATENT[k]
            zero = rep_band[v][srv_rows] == 0
            if not zero.any():
                continue
            hid = torch.zeros(len(sub), len(P), dtype=torch.bool, device=device)
            hid[:, jl[v]] = True
            lp = model(sub, hid)[:, jl[v], :NB[v]]
            logits = (lp + zero_adjust(k, sc))[zero]
            u = torch.rand(logits.shape, generator=gen, device=device).clamp_min(1e-20)
            col = sub[:, C + jl[v]].clone()
            col[zero] = (logits - torch.log(-torch.log(u))).argmax(1)
            sub[:, C + jl[v]] = col
            a, b_ = FILL_SHARE.get(v, (0, 0))
            FILL_SHARE[v] = (a + int((col[zero] != 0).sum()), b_ + int(zero.sum()))
        vals[srv_rows] = sub
        model.train()

    ipf_log = {}

    @torch.no_grad()
    def fit_h(iters=400):
        """Rule 3: h(c, b) so that the fit zeros' SCF-labelled belief times h
        adds up, per cell and slice, to the target counts."""
        model.eval()
        use = tr['fit']
        if RULE == 'cellcount_wave':
            use = use | (tr['srv'] & torch.tensor(tr['years'] != FIT_YEAR, device=device))
        idx_fit = use.nonzero().squeeze(1)
        for k, v in enumerate(LATENT):
            z = idx_fit[tr['rep_band'][v][idx_fit] == 0]
            lps = []
            for i in range(0, len(z), 20_000):
                zz = z[i:i + 20_000]
                hid = torch.zeros(len(zz), len(P), dtype=torch.bool, device=device)
                hid[:, jl[v]] = True
                lps.append(model(tr['values'][zz], hid)[:, jl[v], :NB[v]])
            lp, c, w = torch.cat(lps), tr['grp'][z], tr['weight'][z]
            logh, tgt = LOGH[v].clone(), TGT[v]
            for _ in range(iters):
                p = (lp + logh[c]).softmax(1)
                agg = torch.zeros(NG, NB[v], device=device).index_add_(0, c, w[:, None] * p)
                logh = logh + torch.log(tgt + 1e-9) - torch.log(agg + 1e-9)
                logh = (logh - logh[:, :1]).clamp(-40, 40)
            p = (lp + logh[c]).softmax(1)
            agg = torch.zeros(NG, NB[v], device=device).index_add_(0, c, w[:, None] * p)
            hid_agg, hid_tgt = agg[:, 1:].sum(1), tgt[:, 1:].sum(1)
            ipf_log[v] = float(((hid_agg - hid_tgt).abs() / tgt.sum(1).clamp_min(1e-9)).max())
            LOGH[v] = logh
        model.train()

    log, began = [], time.time()
    for epoch in range(1, max_epochs + 1):
        model.train()
        t0 = time.time()
        order = torch.randperm(n, generator=gen, device=device)
        tot = 0.0
        for start in range(0, n, bs):
            idx = order[start:start + bs]
            fill = epoch >= FILL_FROM_EPOCH
            if fill:
                vals = tr['values'][idx]
                draw_fills(vals, tr['srv'][idx], {v: tr['rep_band'][v][idx] for v in LATENT}, tr['grp'][idx])
                tr['values'][idx] = vals
            values, weight, unknown = tr['values'][idx], tr['weight'][idx], tr['unknown'][idx]
            hidden = M.draw_hidden(values, gen, unknown)
            nll = M.slot_nll(model(values, hidden), values)
            w = weight[:, None] * hidden
            loss = (nll * w).sum() / w.sum()
            fit = tr['fit'][idx]
            if not COUNT_RULE and fill and fit.any():
                beliefs = anchored_beliefs(values[fit])
                ws, cf = weight[fit], tr['cell'][idx][fit]
                for k, v in enumerate(LATENT):
                    lg, l1mg = log_g(k, cf)
                    r_fit = tr['rep_band'][v][idx][fit]
                    amt = torch.logsumexp(beliefs[v] + lg + logT[v][:, r_fit].T, 1)
                    ll = torch.where(r_fit != 0, amt, torch.logsumexp(beliefs[v] + l1mg, 1))
                    loss = loss - (ws * ll).sum() / ws.sum()
            opt.zero_grad()
            loss.backward()
            opt.step()
            sched.step()
            tot += loss.item() * len(idx)
            # rule 3: h refit every H_EVERY batches while filling in. Refit once
            # per pass only, the smoke test oscillated (the model moves toward
            # the fill-ins within the pass while h stays fixed).
            if COUNT_RULE and fill and (start // bs) % H_EVERY == H_EVERY - 1:
                fit_h(150)
        if COUNT_RULE and epoch >= FILL_FROM_EPOCH - 1:
            fit_h()
        model.eval()
        with torch.no_grad():
            val = float(np.mean([M.masked_loss(model, te['values'], te['weight'], h, None,
                                               te['unknown']).item() for h in val_hidden]))
        row = dict(epoch=epoch, train_loss=tot / n, val_loss=val, seconds=time.time() - t0)
        for k, v in enumerate(LATENT):
            b_ = float(beta[k].detach())
            row[f'{v}_beta'] = b_
            for c in range(NA):
                a = float(alpha[k, c].detach())
                row[f'{v}_alpha_c{c}'] = a
                row[f'{v}_middle_dollars_c{c}'] = middle(a, b_)
            a0 = float(alpha[k, 0].detach())
            for x in (50, 500, 5000):
                row[f'{v}_report_at_{x}_c0'] = 1 / (1 + math.exp(-(a0 + b_ * (math.log(x) - LN_CENTRE))))
            a, b2 = FILL_SHARE.pop(v, (0, 0))
            row[f'{v}_zeros_filled_as_holders_this_pass'] = a / b2 if b2 else float('nan')
            if COUNT_RULE:
                row[f'{v}_ipf_max_miss'] = ipf_log.get(v, float('nan'))
        log.append(row)
        print(' '.join(f'{k} {v:.4f}' if isinstance(v, float) else f'{k} {v}' for k, v in row.items()),
              flush=True)
    log = pd.DataFrame(log)
    log.to_csv(os.path.join(M.MODEL_DIR, 'train_log.csv'), index=False)
    torch.save({'state': model.state_dict(), 'settings': model.settings, 'band_layout': M.BAND_LAYOUT},
               os.path.join(M.MODEL_DIR, 'model.pt'))

    # the filled-in slices: five draws per SCF row and item, final model and rule
    model.eval()
    allt = table.reset_index(drop=True)
    d_all = enc(allt)
    for v in LATENT:                      # reporters at their reported slice (as in training)
        col = d_all['values'][:, C + jl[v]].clone()
        for y in np.unique(d_all['years'][d_all['scf_np']]):
            rows = d_all['scf_np'] & (d_all['years'] == y) & (d_all['dollars'][v] != 0)
            if rows.any():
                col[torch.tensor(rows, device=device)] = torch.tensor(
                    band_signed(true_dollars(v, d_all['dollars'][v][rows]), edges[f'scf/{y}/{v}'], NB[v], POS0[v]),
                    device=device)
        d_all['values'][:, C + jl[v]] = col
        d_all['rep_band'][v] = col.clone()
    scf_idx = torch.tensor(np.flatnonzero(d_all['scf_np']), dtype=torch.long, device=device)
    sm = d_all['scf_np']
    fills = {'record_id': allt.loc[sm, 'record_id'].to_numpy(),
             'household_id': allt.loc[sm, 'household_id'].to_numpy(),
             'year': allt.loc[sm, 'year'].to_numpy(), 'weight': allt.loc[sm, 'weight'].to_numpy(),
             'fit_role': allt.loc[sm, 'fit_role'].to_numpy(), 'cell': allt.loc[sm, 'cell'].to_numpy(),
             'married': allt.loc[sm, 'married'].to_numpy(), 'wages_band': allt.loc[sm, 'wages_band'].to_numpy()}
    for v in LATENT:
        fills[f'{v}_reported_band'] = d_all['rep_band'][v][scf_idx].cpu().numpy()
        fills[f'{v}_reported'] = d_all['dollars'][v][sm]
    for draw in range(5):
        base = d_all['values'].clone()
        for i in range(0, len(scf_idx), 20_000):
            idx = scf_idx[i:i + 20_000]
            vals = base[idx]
            srv = torch.ones(len(idx), dtype=torch.bool, device=device)
            for _ in range(2):
                draw_fills(vals, srv, {v: d_all['rep_band'][v][idx] for v in LATENT}, d_all['grp'][idx])
            base[idx] = vals
        model.eval()
        for v in LATENT:
            fills[f'{v}_fill{draw + 1}'] = base[scf_idx, C + jl[v]].cpu().numpy()
    pd.DataFrame(fills).to_parquet(os.path.join(M.MODEL_DIR, 'fills.parquet'), index=False)

    settings = {}
    for k, v in enumerate(LATENT):
        b_ = float(beta[k].detach())
        settings[v] = {'beta (shared)': b_, 'dollar factor': 1.0,
                       'alpha by cell' if RULE == 'cellcurve' else 'alpha': [float(a) for a in alpha[k].detach()],
                       'curve middle (dollars, 2015 PUF scale) by cell' if RULE == 'cellcurve' else 'curve middle (dollars)':
                           [middle(float(a), b_) for a in alpha[k].detach()],
                       'chance of reporting at $50, $500, $5,000 (%), by cell' if RULE == 'cellcurve' else
                       'chance of reporting at $50, $500, $5,000 (%)':
                           [[100 / (1 + math.exp(-(float(a) + b_ * (math.log(x) - LN_CENTRE)))) for x in (50, 500, 5000)]
                            for a in alpha[k].detach()]}
        if COUNT_RULE:
            settings[v] = {'rule': 'per-cell counts and slices (no curve)',
                           'IPF largest miss of hidden holders, share of cell weight': ipf_log.get(v),
                           'log h by cell and slice': LOGH[v].cpu().numpy().round(3).tolist()}
    meta = dict(table=M.TABLE, flags=M.flags(), predicted=P, seed=seed, max_epochs=max_epochs, latent=LATENT,
                rule=RULE, fit_year=FIT_YEAR, scf_years=SCF_YEARS or 'all', hold_share=HOLD_SHARE,
                hold_seed=HOLD_SEED, held_households=sorted(float(h) for h in held_hh), fit_wave_households=n_hh,
                factor_fixed=FACTOR_FIXED, settings_fit='joint', rule3_alloc=ALLOC, fill_from='scf', centre_dollars=MID,
                cells=cell_info, min_households_per_cell=MIN_HH, cell_targets=cell_targets,
                settings=settings, minutes=(time.time() - began) / 60,
                last_val_loss=float(log['val_loss'].iloc[-1]),
                rows=table.groupby(['source', 'year', 'split']).size().to_dict().__repr__())
    json.dump(meta, open(os.path.join(M.MODEL_DIR, 'train_meta.json'), 'w'), indent=2)
    print(json.dumps({k: v for k, v in meta.items() if k not in ('held_households', 'cell_targets')}, indent=2)[:20000])


if __name__ == '__main__':
    main()
