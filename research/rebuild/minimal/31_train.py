"""
31_train.py  (minimal example, step 31: the cell method, design/cell_method.md,
first full run. Status: 31_STATUS.md.)

Built from 29_train.py (rule 3 wave by wave, run C4). What changes, all from
the note:

  - Every setting is read from a settings sheet (31_settings_scf.json, or
    MINIMAL_SETTINGS) and copied into train_meta.json.
  - The cell table is built by broad comparison ranges, not slice by slice
    (note, steps 2 to 4). In each cell and range below the top line, the
    hidden holders to fill in are the PUF side's share minus the SCF
    reporters' share where that is positive; where the SCF's is larger the
    target is zero and the excess is recorded; above the top line nothing is
    filled in and the gap is recorded. Targets are then read as shares of the
    cell's zero-reporters and applied to each wave's own zeros (step 5).
  - The cell table uses every 2016-wave SCF training row (no held-back fit
    households; step 29 held 20% back to score the S-curves).
  - The scaling (step 7b) has one multiplier per cell, wave and range, not
    per slice, so the model's belief decides the slice inside a range.
  - The S-curve parameters and their loss are removed.

A slice belongs to the range that holds its 2015 PUF median dollars. The same
slice-to-range map is used for the PUF side, the SCF reporters and the
fill-ins, so the table and the scaling count the same thing. Four or five
slices per item straddle a range edge; they count wholly in the range of
their median (step 30 split them by PUF composition instead). Unused slices
(no PUF records) can never be filled in.

Writes model/<run>/: model.pt, train_log.csv, train_meta.json (settings
sheet, cells, cell table, wave targets), fills.parquet (five draws of the
filled-in slice per SCF row and item).
Usage (Slurm, 31_run.sbatch): MINIMAL_TABLE=E27 MINIMAL_RUN=step31/Ks1 python 31_train.py
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
SETTINGS_PATH = os.environ.get('MINIMAL_SETTINGS', f'{HERE}/31_settings_scf.json')
S = json.load(open(SETTINGS_PATH))
LATENT = list(S['mapped'])
FILL_FROM_EPOCH = int(S['fill_from_pass'])
FIT_YEAR = int(S['fitting_tax_year'])
MIN_HH = int(S['cells']['min_households'])
H_EVERY = int(os.environ.get('MINIMAL_H_EVERY', '10'))     # batches between refits of the multipliers
# MINIMAL_HIDE_WEIGHT=1 (2026-10-19, 34_hidecount.py): each hidden slot's loss
# is multiplied by the record's number of slots over its number hidden, so
# that, for any one variable, every count of other slots hidden gets the same
# share of training, as in generation's random order (Uria, Murray and
# Larochelle 2014). The multiplier depends only on which slots are hidden, not
# on their values, so no conditional share the model aims at changes. Off: the
# rule of every run before 19 October.
HIDE_WEIGHT = os.environ.get('MINIMAL_HIDE_WEIGHT') == '1'
# Per-variable log (2026-10-19): every VAR_LOG_EVERY passes and at the last,
# on the held-back PUF records and on a fixed sample of PUF training records,
# each amount's actual share of holders, the model's share with only it
# hidden and with every slot hidden, and the loss on holders and non-holders
# under one fixed random hiding. Written to var_log.csv. 0 turns it off.
VAR_LOG_EVERY = int(os.environ.get('MINIMAL_VAR_LOG_EVERY', '5'))
# MINIMAL_DROPOUT_OFF_LAST=N (2026-10-19, 34_dropcheck.py): dropout set to zero
# for the last N passes, so the output intercepts settle without the noise
# that is absent when the model is used. 0 (default): dropout throughout.
DROPOUT_OFF_LAST = int(os.environ.get('MINIMAL_DROPOUT_OFF_LAST', '0'))
# MINIMAL_OVERSAMPLE=N (2026-10-19, John): every PUF training record holding
# any of OVERSAMPLE_PARTS appears N times, each copy with 1/N of its weight.
# The weighted data, and so every share the model aims at, are unchanged; each
# such record is seen N times a pass, each time with its own hidden slots, and
# its pull is spread over N batches. 1 (default): no copies.
OVERSAMPLE = int(os.environ.get('MINIMAL_OVERSAMPLE', '1'))
OVERSAMPLE_PARTS = ['part_pass_inc', 'part_nonpass_inc', 'part_pass_loss', 'part_nonpass_loss', 'part_179',
                    'scorp_pass_inc', 'scorp_nonpass_inc', 'scorp_pass_loss', 'scorp_nonpass_loss', 'scorp_179',
                    'estate_inc', 'estate_loss', 'farm']


def dropout_off(model):
    for mod in model.modules():
        if isinstance(mod, torch.nn.Dropout):
            mod.p = 0.0
        if isinstance(mod, torch.nn.MultiheadAttention):
            mod.dropout = 0.0
VAR_LOG_SAMPLE = 50_000
MID = 150.0
INF = math.inf
WAGE_GROUPS = [(n, lo, INF if hi is None else hi) for n, lo, hi in S['cells']['wage_groups']]
assert S['cells']['by_married'] is True
assert [g[0] for g in WAGE_GROUPS] == ['none', 'under $25k', '$25k-75k', '$75k-200k', 'over $200k'], \
    'wage_group_of_band below is written for these five groups'
assert all(S['dollar_rule'][v] == 'unchanged' for v in LATENT), 'only the "unchanged" dollar rule is coded'
assert set(S['loss_cap']) <= {'capital_gains'} and S['loss_cap'].get('capital_gains') == -3000
LOSS_LIMIT = float(S['loss_cap']['capital_gains'])
RANGES = {v: [(-INF if lo is None else lo, INF if hi is None else hi) for lo, hi in S['comparison_ranges'][v]]
          for v in LATENT}
TOP = {v: float(S['top_line'][v]) for v in LATENT}
FLOOR = {v: float(S.get('fill_losses_down_to', {}).get(v, 0.0)) for v in LATENT}
FILL_SHARE = {}


def range_label(lo, hi):
    if lo == hi == 0:
        return 'zero'
    f = lambda x: f'${x:,.0f}'.replace('$-', '-$')
    if lo == -INF:
        return f'below {f(hi)}'
    if hi == INF:
        return f'{f(lo)} and over'
    return f'{f(lo)} to {f(hi)}'


def in_range(x, lo, hi):
    """As 30_compare.py: negative ranges [lo, 0) or [lo, hi); small positive (0, hi); else [lo, hi)."""
    if lo == 0:
        return 0 < x < hi
    return lo <= x < hi


def fill_allowed(v, lo, hi):
    """Hidden holders may be filled in only in ranges wholly below the top line
    (and, for losses, not beyond the loss floor)."""
    return hi <= TOP[v] and lo >= FLOOR[v] and lo > -INF


def slice_ranges(edges, v):
    """Range index of every slice: 0 = zero, 1..K = the comparison ranges,
    K + 1 = an unused slice (no PUF records; never filled in)."""
    rng = RANGES[v]
    out = []
    for bd in edges[f'puf/2015/{v}']['bands']:
        b, med = bd['band'], bd['median']
        if b == 0:
            out.append(0)
            continue
        if med is None:
            out.append(len(rng) + 1)
            continue
        hit = [i for i, (lo, hi) in enumerate(rng) if in_range(med, lo, hi)]
        assert len(hit) == 1, (v, b, med, hit)
        out.append(hit[0] + 1)
    return np.array(out, dtype=np.int64)


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
        out[b] = 1 + sum(med >= t for t in (WAGE_GROUPS[2][1], WAGE_GROUPS[3][1], WAGE_GROUPS[4][1]))
    return out


def cell_map(table, edges, fit_year=FIT_YEAR, min_hh=MIN_HH):
    """Cell of every row (wage group x married, small cells merged), as 29_train.py."""
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
    M = load_model_module()
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    seed, max_epochs = M.SEED, int(os.environ.get('MINIMAL_MAX_EPOCHS', '80'))
    torch.manual_seed(seed)
    gen = torch.Generator(device=device).manual_seed(seed)
    os.makedirs(M.MODEL_DIR, exist_ok=True)
    table = M.read_table()
    if OVERSAMPLE > 1:
        sel = ((table.source == 'puf') & (table.split == 'train') &
               (table[OVERSAMPLE_PARTS] != 0).any(axis=1)).to_numpy()
        table.loc[sel, 'weight'] = table.loc[sel, 'weight'] / OVERSAMPLE
        table = pd.concat([table] + [table[sel]] * (OVERSAMPLE - 1), ignore_index=True)
        print(f'oversampled: {int(sel.sum())} PUF training records x {OVERSAMPLE}; {len(table)} rows', flush=True)
    edges = json.load(open(M.BAND_EDGES))
    table['fit_role'] = np.where((table.source == 'scf') & (table.year == FIT_YEAR), 'fit', '')
    table['cell'], cell_info = cell_map(table, edges)
    NCELL = len(cell_info)
    WAVES = sorted(int(y) for y in table.loc[table.source == 'scf', 'year'].unique())
    assert FIT_YEAR in WAVES
    NG = len(WAVES) * NCELL
    # group of a row for the multipliers: wave x cell (PUF rows: the fitting wave's index, unused)
    wi = table['year'].map({y: i for i, y in enumerate(WAVES)}).fillna(WAVES.index(FIT_YEAR)).astype(int)
    table['grp'] = wi.to_numpy() * NCELL + table['cell'].to_numpy()
    print('settings sheet:', json.dumps(S, indent=1), flush=True)
    print('cells:', json.dumps(cell_info, indent=1), flush=True)
    C, P = len(M.CONTEXT), M.PREDICTED
    NB = {v: M.N_CHOICES[v] for v in LATENT}
    POS0 = {v: M.BAND_LAYOUT[v]['first_positive'] for v in LATENT}
    jl = {v: P.index(v) for v in LATENT}
    RO_np = {v: slice_ranges(edges, v) for v in LATENT}
    RO = {v: torch.tensor(RO_np[v], device=device) for v in LATENT}
    NR = {v: len(RANGES[v]) + 2 for v in LATENT}           # zero, the ranges, unused slices
    for v in LATENT:
        assert len(RO_np[v]) == NB[v], v
        print(f'{v}: slices per range (0 = zero, last = unused):',
              np.bincount(RO_np[v], minlength=NR[v]).tolist(), flush=True)

    def enc(part):
        d = M.encode(part, device)
        srv = torch.tensor((part['source'] == 'scf').to_numpy(), device=device)
        rep_band = {v: d['values'][:, C + jl[v]].clone() for v in LATENT}
        dollars = {v: part[v].to_numpy(dtype=np.float64) for v in LATENT}
        unknown = torch.zeros(len(part), len(P), dtype=torch.bool, device=device)
        return dict(values=d['values'], weight=d['weight'], unknown=unknown, srv=srv,
                    rep_band=rep_band, dollars=dollars, years=part['year'].to_numpy(),
                    cell=torch.tensor(part['cell'].to_numpy(), device=device),
                    grp=torch.tensor(part['grp'].to_numpy(), device=device),
                    scf_np=(part['source'] == 'scf').to_numpy())
    tr = enc(table[table.split == 'train'])
    te = enc(table[table.split == 'test'])
    puf_rows = (table.source == 'puf').to_numpy()

    def true_dollars(v, x):
        return np.where(x < LOSS_LIMIT, LOSS_LIMIT, x) if v == 'capital_gains' else x

    # reporters' slice = reported slice under the dollar rule "unchanged" (capital
    # gains: a loss beyond the cap read as the cap first; the concept conversion)
    def reslice(d):
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
    reslice(tr)
    reslice(te)

    # ---- the cell table (note, steps 2-4): fitting wave, every training row ----
    TGT, LOGH, cell_table, wave_targets = {}, {}, [], []
    fitw = tr['srv'] & torch.tensor(tr['years'] == FIT_YEAR, device=device)
    for v in LATENT:
        K = len(RANGES[v])
        pr = RO_np[v][table.loc[puf_rows, f'{v}_band'].to_numpy()]
        pw = table.loc[puf_rows, 'weight'].to_numpy(float)
        pc = table.loc[puf_rows, 'cell'].to_numpy()
        assert (pr != NR[v] - 1).all(), f'{v}: PUF rows in an unused slice'
        frac = np.zeros((NCELL, NR[v]))            # share of the cell's zeros to fill, by range
        for c in range(NCELL):
            m_ = pc == c
            Pc = np.bincount(pr[m_], weights=pw[m_], minlength=NR[v]) / pw[m_].sum()
            fr = fitw & (tr['cell'] == c)
            rr = RO[v][tr['rep_band'][v][fr]].cpu().numpy()
            rw = tr['weight'][fr].double().cpu().numpy()
            W = rw.sum()
            Sc = np.bincount(rr, weights=rw, minlength=NR[v]) / W
            if Sc[NR[v] - 1] > 0:                   # reporters in a slice with no PUF records: kept, counted
                print(f'{v}, cell {c}: SCF reporters in unused slices, {100 * Sc[NR[v] - 1]:.3f}% of units',
                      flush=True)
            Wz = Sc[0]
            hid = np.zeros(NR[v])
            for i, (lo, hi) in enumerate(RANGES[v], start=1):
                gap = Pc[i] - Sc[i]
                ok = fill_allowed(v, lo, hi)
                hid[i] = max(gap, 0.0) if ok else 0.0
                cell_table.append({
                    'item': v, 'cell': c, 'cell label': cell_info[c]['label'], 'range': range_label(lo, hi),
                    'below the top line': ok,
                    'PUF side (% of units)': 100 * Pc[i], 'SCF reported (% of units)': 100 * Sc[i],
                    'gap, PUF minus SCF (points)': 100 * gap,
                    'filled in (% of units)': 100 * hid[i],
                    'filled in (% of zeros)': 100 * hid[i] / Wz if Wz > 0 else float('nan'),
                    'recorded, SCF excess (points)': 100 * max(-gap, 0.0),
                    'recorded, gap above the top line (points)': 100 * max(gap, 0.0) if not ok else 0.0})
            scale = 1.0
            if hid.sum() > Wz:                       # cannot fill more than the zeros; never seen, recorded
                scale = Wz / hid.sum()
                hid *= scale
            frac[c] = hid / Wz if Wz > 0 else 0.0
            frac[c, 0] = 1.0 - frac[c].sum()
            cell_table.append({'item': v, 'cell': c, 'cell label': cell_info[c]['label'], 'range': 'zero',
                               'below the top line': None,
                               'PUF side (% of units)': 100 * Pc[0], 'SCF reported (% of units)': 100 * Wz,
                               'gap, PUF minus SCF (points)': 100 * (Pc[0] - Wz),
                               'filled in (% of units)': 100 * (hid.sum()),
                               'filled in (% of zeros)': 100 * hid.sum() / Wz if Wz > 0 else float('nan'),
                               'recorded, SCF excess (points)': float('nan'),
                               'recorded, gap above the top line (points)': float('nan'),
                               'scaled down to fit the zeros (factor)': scale})
        # targets per wave and cell: the shares of zeros times the wave's own zeros
        tg = torch.zeros(NG, NR[v], dtype=torch.float64)
        for i, y in enumerate(WAVES):
            for c in range(NCELL):
                r_ = tr['srv'] & torch.tensor(tr['years'] == y, device=device) & (tr['cell'] == c) & \
                    (tr['rep_band'][v] == 0)
                Wz_y = float(tr['weight'][r_].double().sum())
                tg[i * NCELL + c] = torch.tensor(frac[c] * Wz_y)
                wave_targets.append({'item': v, 'tax year': y, 'cell': c, 'zeros (millions)': Wz_y / 1e6,
                                     'hidden holders to fill (millions)': float(tg[i * NCELL + c, 1:].sum()) / 1e6})
        TGT[v] = tg.to(device=device, dtype=torch.float32)
        LOGH[v] = torch.zeros(NG, NR[v], device=device)
    print('cell table:', pd.DataFrame(cell_table).to_string(), flush=True)

    model = M.SlotTransformer().to(device)
    opt = torch.optim.AdamW(model.parameters(), lr=1e-3, weight_decay=0.01)
    n, bs = len(tr['values']), 4096
    spe = math.ceil(n / bs)
    total, warm = max_epochs * spe, 3 * spe
    sched = torch.optim.lr_scheduler.LambdaLR(
        opt, lambda s: (s + 1) / warm if s < warm else
        0.05 + 0.95 * 0.5 * (1 + math.cos(math.pi * (s - warm) / max(1, total - warm))))
    val_hidden = [M.draw_hidden(te['values'], gen, te['unknown']) for _ in range(4)]

    # ---- the per-variable log (VAR_LOG_EVERY) ----
    LOGGED = [v for v in M.AMOUNTS if 'zero_band' in edges.get(f'puf/2015/{v}', {})]
    ZB = {v: edges[f'puf/2015/{v}']['zero_band'] for v in LOGGED}
    log_gen = torch.Generator(device=device).manual_seed(seed + 1)
    puf_tr = torch.tensor(np.flatnonzero(~tr['scf_np']), device=device)
    puf_tr = puf_tr[torch.randperm(len(puf_tr), generator=log_gen, device=device)[:VAR_LOG_SAMPLE]]
    log_sets = {}
    for lab, d_, rows in [('held-back', te, torch.tensor(np.flatnonzero(~te['scf_np']), device=device)),
                          ('training sample', tr, puf_tr)]:
        vals = d_['values'][rows].clone()         # PUF rows: never filled in, so fixed
        log_sets[lab] = dict(values=vals, weight=d_['weight'][rows].double(),
                             hidden=M.draw_hidden(vals, log_gen, None))
    var_log = []

    @torch.no_grad()
    def log_variables(epoch):
        model.eval()
        for lab, d_ in log_sets.items():
            vals, w, rand_hid = d_['values'], d_['weight'], d_['hidden']
            present = vals[:, C:] >= 0

            def per_variable(hid_of, vs):
                """Chance of nonzero and loss of the true band, per variable, in chunks."""
                p_nz = {v: [] for v in vs}
                nll = {v: [] for v in vs}
                for i in range(0, len(vals), 8192):
                    vv = vals[i:i + 8192]
                    for v, hid in hid_of(vv, present[i:i + 8192]):
                        out = model(vv, hid)
                        for u in (vs if v is None else [v]):
                            lp = out[:, P.index(u), :M.N_CHOICES[u]].log_softmax(1)
                            p_nz[u].append(1 - lp[:, ZB[u]].exp().double())
                            nll[u].append(-lp.gather(1, vv[:, C + P.index(u)].clamp(min=0)[:, None])
                                          .squeeze(1).double())
                return {v: torch.cat(p_nz[v]) for v in vs}, {v: torch.cat(nll[v]) for v in vs}

            def only_each(vv, pr):
                for v in LOGGED:
                    h = torch.zeros_like(pr)
                    h[:, P.index(v)] = True
                    yield v, h & pr
            p_all, _ = per_variable(lambda vv, pr: [(None, pr)], LOGGED)
            # random hiding: the fixed draw, chunked alongside the values
            nll_r = {v: [] for v in LOGGED}
            for i in range(0, len(vals), 8192):
                vv, hh = vals[i:i + 8192], rand_hid[i:i + 8192]
                out = model(vv, hh)
                for u in LOGGED:
                    lp = out[:, P.index(u), :M.N_CHOICES[u]].log_softmax(1)
                    nll_r[u].append(-lp.gather(1, vv[:, C + P.index(u)].clamp(min=0)[:, None])
                                    .squeeze(1).double())
            nll_r = {v: torch.cat(nll_r[v]) for v in LOGGED}
            p_only, _ = per_variable(only_each, LOGGED)
            for v in LOGGED:
                j = P.index(v)
                band = vals[:, C + j]
                ok = band >= 0
                if not ok.any():
                    continue
                holder = ok & (band != ZB[v])
                scored = ok & rand_hid[:, j]

                def wmean(x, m):
                    return float((w[m] * x[m]).sum() / w[m].sum()) if m.any() else float('nan')
                var_log.append({'epoch': epoch, 'records': lab, 'variable': v,
                                'holders, actual (%)': 100 * wmean(holder.double(), ok),
                                'holders, only this hidden (%)': 100 * wmean(p_only[v], ok),
                                'holders, all hidden (%)': 100 * wmean(p_all[v], ok),
                                'loss on holders': wmean(nll_r[v], scored & holder),
                                'loss on non-holders': wmean(nll_r[v], scored & ~holder)})
        pd.DataFrame(var_log).to_csv(os.path.join(M.MODEL_DIR, 'var_log.csv'), index=False)
        model.train()

    def zero_adjust(v, grps):
        """log multiplier per slice for a zero-reporter: its range's multiplier in its wave and cell."""
        return LOGH[v][grps][:, RO[v]]

    @torch.no_grad()
    def draw_fills(vals, srv_rows, rep_band, grps):
        """Note, step 7: the items in a random order, each seeing the others' current values;
        each zero-reporter's slice drawn from the SCF-labelled belief times its range's multiplier."""
        model.eval()
        order = torch.randperm(len(LATENT), generator=gen, device=device).tolist()
        sub, sg = vals[srv_rows], grps[srv_rows]
        for k in order:
            v = LATENT[k]
            zero = rep_band[v][srv_rows] == 0
            if not zero.any():
                continue
            hid = torch.zeros(len(sub), len(P), dtype=torch.bool, device=device)
            hid[:, jl[v]] = True
            lp = model(sub, hid)[:, jl[v], :NB[v]]
            logits = (lp + zero_adjust(v, sg))[zero]
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
        """Note, step 7b: one multiplier per wave, cell and range, applied to the odds,
        adjusted back and forth until each range's weighted sum of chances over the
        zero-reporters equals its target."""
        model.eval()
        idx = tr['srv'].nonzero().squeeze(1)
        for v in LATENT:
            z = idx[tr['rep_band'][v][idx] == 0]
            lps = []
            for i in range(0, len(z), 20_000):
                zz = z[i:i + 20_000]
                hid = torch.zeros(len(zz), len(P), dtype=torch.bool, device=device)
                hid[:, jl[v]] = True
                lps.append(model(tr['values'][zz], hid)[:, jl[v], :NB[v]])
            lp, g, w = torch.cat(lps), tr['grp'][z], tr['weight'][z]
            logh, tgt = LOGH[v].clone(), TGT[v]

            def agg_of(lh):
                p = (lp + lh[g][:, RO[v]]).softmax(1)
                a = torch.zeros(NG, NB[v], device=device).index_add_(0, g, w[:, None] * p)
                return torch.zeros(NG, NR[v], device=device).index_add_(1, RO[v], a)
            for _ in range(iters):
                agg = agg_of(logh)
                logh = logh + torch.log(tgt + 1e-9) - torch.log(agg + 1e-9)
                logh = (logh - logh[:, :1]).clamp(-40, 40)
            agg = agg_of(logh)
            ipf_log[v] = float(((agg[:, 1:] - tgt[:, 1:]).abs().sum(1) / tgt.sum(1).clamp_min(1e-9)).max())
            LOGH[v] = logh
        model.train()

    log, began = [], time.time()
    for epoch in range(1, max_epochs + 1):
        if DROPOUT_OFF_LAST and epoch == max_epochs - DROPOUT_OFF_LAST + 1:
            dropout_off(model)
            print(f'dropout off from pass {epoch}', flush=True)
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
            if HIDE_WEIGHT:
                n_slots = ((values[:, C:] >= 0) & ~unknown).sum(1)
                w = w * (n_slots / hidden.sum(1).clamp(min=1))[:, None]
            loss = (nll * w).sum() / w.sum()
            opt.zero_grad()
            loss.backward()
            opt.step()
            sched.step()
            tot += loss.item() * len(idx)
            # refit every H_EVERY batches while filling in (step 29: refitting once per
            # pass only oscillated, the model moving toward the fill-ins within the pass)
            if fill and (start // bs) % H_EVERY == H_EVERY - 1:
                fit_h(150)
        if epoch >= FILL_FROM_EPOCH - 1:
            fit_h()
        model.eval()
        with torch.no_grad():
            val = float(np.mean([M.masked_loss(model, te['values'], te['weight'], h, None,
                                               te['unknown']).item() for h in val_hidden]))
        row = dict(epoch=epoch, train_loss=tot / n, val_loss=val, seconds=time.time() - t0)
        for v in LATENT:
            a, b2 = FILL_SHARE.pop(v, (0, 0))
            row[f'{v}_zeros_filled_as_holders_this_pass'] = a / b2 if b2 else float('nan')
            row[f'{v}_ipf_max_miss'] = ipf_log.get(v, float('nan'))
        log.append(row)
        print(' '.join(f'{k} {v:.4f}' if isinstance(v, float) else f'{k} {v}' for k, v in row.items()),
              flush=True)
        if VAR_LOG_EVERY and (epoch % VAR_LOG_EVERY == 0 or epoch == max_epochs or epoch == 1):
            log_variables(epoch)
    log = pd.DataFrame(log)
    log.to_csv(os.path.join(M.MODEL_DIR, 'train_log.csv'), index=False)
    torch.save({'state': model.state_dict(), 'settings': model.settings, 'band_layout': M.BAND_LAYOUT},
               os.path.join(M.MODEL_DIR, 'model.pt'))

    # the filled-in survey: five draws per SCF row and item, final model and multipliers
    model.eval()
    allt = table.reset_index(drop=True)
    d_all = enc(allt)
    reslice(d_all)
    scf_idx = torch.tensor(np.flatnonzero(d_all['scf_np']), dtype=torch.long, device=device)
    sm = d_all['scf_np']
    fills = {'record_id': allt.loc[sm, 'record_id'].to_numpy(),
             'household_id': allt.loc[sm, 'household_id'].to_numpy(),
             'year': allt.loc[sm, 'year'].to_numpy(), 'weight': allt.loc[sm, 'weight'].to_numpy(),
             'split': allt.loc[sm, 'split'].to_numpy(), 'fit_role': allt.loc[sm, 'fit_role'].to_numpy(),
             'cell': allt.loc[sm, 'cell'].to_numpy(),
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

    meta = dict(table=M.TABLE, flags=M.flags(), predicted=P, seed=seed, max_epochs=max_epochs, latent=LATENT,
                hide_weight=HIDE_WEIGHT, dropout_off_last=DROPOUT_OFF_LAST, oversample=OVERSAMPLE,
                rule='cell method (design/cell_method.md)', settings_sheet=S, settings_path=SETTINGS_PATH,
                fit_year=FIT_YEAR, waves=WAVES, cells=cell_info, cell_table=cell_table,
                wave_targets=wave_targets,
                slice_to_range={v: RO_np[v].tolist() for v in LATENT},
                ipf_largest_miss=ipf_log, log_multipliers={v: LOGH[v].cpu().numpy().round(3).tolist() for v in LATENT},
                minutes=(time.time() - began) / 60, last_val_loss=float(log['val_loss'].iloc[-1]),
                rows=table.groupby(['source', 'year', 'split']).size().to_dict().__repr__())
    json.dump(meta, open(os.path.join(M.MODEL_DIR, 'train_meta.json'), 'w'), indent=2, default=float)
    print(json.dumps({k: v for k, v in meta.items() if k not in ('cell_table', 'wave_targets', 'log_multipliers',
                                                                  'slice_to_range')}, indent=2, default=float)[:20000])


if __name__ == '__main__':
    main()
