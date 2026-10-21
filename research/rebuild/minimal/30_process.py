"""
30_process.py  (minimal example, step 30, part 2; 2026-10-18)

What it does
  Runs once, outside training, the processing step that turns the 2016 SCF
  wave's reported interest, dividends and capital gains into PUF-concept
  values, and writes tables of what the filled-in SCF looks like. A look at
  the method, not a training run. Dollar factor g = 1 for all three items:
  reporters keep the amounts they reported; only zero-reporters change.

  Data: table E27, tax year 2015 (the 2016 SCF wave against the 2015 PUF with
  the crude non-filers of step 23). Cells, size ranges, weights and NA checks
  are those of 30_compare.py (imported from it).

  A. Cell table. For each item, cell c and nonzero range r: the PUF side's
     weighted share of units in r (p) and the SCF's (s). Where p > s the
     target share of hidden holders in (c, r) is p - s; otherwise 0 and the
     excess s - p is recorded (no reporter is changed).
  B. Which zero-reporters are filled in. The step-29 model C4 seed 1 (label
     on, table E27) gives, for every SCF row that reported zero on the item,
     its belief over the item's 84 slices under the SCF label, with that slot
     hidden and everything else on the record visible (net worth included; the
     other two items at their reported values). The belief is collapsed to
     the broad ranges plus zero. Per cell, one multiplier per range (and one
     for zero) is found by iterative proportional scaling so that the
     weighted sum over the cell's zero-reporters of each range's probability
     equals the cell table's target. Each row's probability of range r is its
     belief times the multiplier, normalized. One range (or zero) is drawn per
     row; five independent draws.
  C. The amount. A slice within the drawn range from the belief restricted to
     that range, then a dollar amount from the 2015 PUF side's holders in that
     slice and range (all cells pooled), drawn with the PUF weights.

  Slices and ranges: a slice can straddle a range edge, so the collapse uses
  the PUF side's composition: the share of slice b's PUF-side weight lying in
  range r. A slice's belief mass goes to ranges in those shares, the slice
  drawn within range r has probability proportional to belief(b) times that
  share, and the amount comes from slice b's PUF-side rows in range r. Slices
  with no PUF-side rows carry no range; the belief mass on them is reported
  and removed before normalizing.

  Capital gains: the SCF's one number against PUF line 13 (capital_gains,
  after the loss limit), the main version, and against the amount before the
  limit (kg_st + kg_lt + kg_dist), a second version that uses the same
  line-13 belief collapsed through the before-limit composition.
  The model sees a reporter's capital gains as training did: a loss beyond
  $3,000 is read as -$3,000 (the same slice on E27).

  The three items are processed one at a time (interest, dividends, capital
  gains), each conditioning on the others' REPORTED values, not their
  fill-ins. Training draws them in turn; this is a simplification.

Writes (in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/checks/step30/)
  process.md          the tables
  process.json        the cell targets, multipliers and convergence
  fills_2016.parquet  per SCF row and item: record_id, household_id,
                      implicate, weight, cell, variable, reported, fill1-5
Run: sbatch research/rebuild/minimal/30_run.sbatch process
"""
import importlib.util
import json
import os

os.environ.setdefault('MINIMAL_TABLE', 'E27')
os.environ['MINIMAL_ALL_RECORDS'] = '1'          # as 29_train.py's load_model_module
assert os.environ['MINIMAL_TABLE'] == 'E27', os.environ['MINIMAL_TABLE']

import numpy as np
import pandas as pd
import torch

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{R}/checks/step30'
MODEL_DIR = os.environ.get('PROCESS_MODEL_DIR', f'{R}/model/step29/C4s1')
YEAR = 2015
N_DRAWS = 5
SEED = 20261018
LOSS_LIMIT = -3000.0
SLOTS3 = ['interest', 'dividends', 'capital_gains']
# (key, title, PUF column, SCF column, model slot)
CONCEPTS = [('interest', 'interest', 'interest', 'interest', 'interest'),
            ('dividends', 'dividends', 'dividends', 'dividends', 'dividends'),
            ('capital_gains', 'capital gains, PUF line 13 (after the loss limit)',
             'capital_gains', 'capital_gains', 'capital_gains'),
            ('capital_gains_before_limit', 'capital gains, PUF before the loss limit',
             'cg_before', 'capital_gains', 'capital_gains')]
WHO_FOR = ['interest', 'dividends', 'capital_gains']    # section 4 for these


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, f'{HERE}/{file}')
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def wmean(x, w):
    return float((x * w).sum() / w.sum()) if w.sum() > 0 else float('nan')


def main():
    os.makedirs(OUT, exist_ok=True)
    M = load('model02', '02_model.py')
    assert M.ALL_RECORDS and not (M.NO_SOURCE or M.NO_LABEL_SHARED or M.REPORT_CURVE or M.COARSEN)
    CMP = load('compare30', '30_compare.py')
    T29 = load('train29', '29_train.py')                 # band_signed only
    edges = json.load(open(M.BAND_EDGES))
    device = 'cpu'
    model = M.load_model(device, MODEL_DIR)
    meta29 = json.load(open(f'{MODEL_DIR}/train_meta.json'))
    assert meta29['table'] == 'E27' and meta29['rule'] == 'cellcount_wave', (meta29['table'], meta29['rule'])

    table = M.read_table()                               # stops on any NA in what the model sees
    t = table[table.year == YEAR]
    puf = t[t.source == 'puf'].reset_index(drop=True).copy()
    scf = t[t.source == 'scf'].reset_index(drop=True).copy()

    # the same checks and derived columns as 30_compare.py
    nf = puf.filer == 0
    assert (puf.loc[nf, 'capital_gains'] == 0).all()
    for c in ['kg_st', 'kg_lt', 'kg_dist']:
        n_na = int(puf.loc[~nf, c].isna().sum())
        assert n_na == 0, f'{c}: {n_na} NA on PUF filers'
    puf['cg_before'] = np.where(nf, 0.0, puf[['kg_st', 'kg_lt', 'kg_dist']].fillna(0).sum(axis=1))
    for c in ['wages', 'interest', 'dividends', 'capital_gains']:
        for name, f in [('PUF side', puf), ('SCF', scf)]:
            n_na = int(f[c].isna().sum())
            assert n_na == 0, f'{name} {c}: {n_na} NA'
    for c in ['net_worth', 'implicate', 'household_id', 'record_id']:
        n_na = int(scf[c].isna().sum())
        assert n_na == 0, f'SCF {c}: {n_na} NA'
    for f in (puf, scf):
        g = CMP.wage_group(f.wages.to_numpy(float))
        assert (g >= 0).all()
        f['cell'] = -1
        for k, (m, gs) in enumerate(CMP.CELLS):
            f.loc[(f.married.to_numpy() == m) & np.isin(g, gs), 'cell'] = k
        assert (f.cell >= 0).all()
    NCELL = len(CMP.CELLS)
    cell_label = [f"{'married' if m else 'not married'}, wages "
                  + (CMP.WAGE_GROUPS[gs[0]][0] if len(gs) == 1
                     else f'{CMP.WAGE_GROUPS[gs[0]][0]} to {CMP.WAGE_GROUPS[gs[-1]][0]}')
                  for m, gs in CMP.CELLS]

    # ---- the model's beliefs for every SCF row, one item at a time ----------
    C, P = len(M.CONTEXT), M.PREDICTED
    d = M.encode(scf, device)
    values = d['values']
    NB = {v: M.N_CHOICES[v] for v in SLOTS3}
    POS0 = {v: M.BAND_LAYOUT[v]['first_positive'] for v in SLOTS3}
    notes = []
    for v in SLOTS3:                     # reporters' slices exactly as in training (29_train.py)
        j = C + P.index(v)
        x = scf[v].to_numpy(float)
        rows = x != 0
        xx = np.where(x < LOSS_LIMIT, LOSS_LIMIT, x) if v == 'capital_gains' else x
        b = T29.band_signed(xx[rows], edges[f'scf/{YEAR}/{v}'], NB[v], POS0[v])
        col = values[:, j].clone()
        col[torch.tensor(rows)] = torch.tensor(b)
        n_diff = int((col != values[:, j]).sum())
        notes.append(f'{v}: reporters re-sliced as in training differ from the table on {n_diff} rows')
        if v != 'capital_gains':
            assert n_diff == 0, v
        values[:, j] = col
    belief = {}
    with torch.no_grad():
        for v in SLOTS3:
            j = P.index(v)
            out = []
            for i in range(0, len(values), 5000):
                vals = values[i:i + 5000]
                hid = torch.zeros(len(vals), len(P), dtype=torch.bool)
                hid[:, j] = True
                out.append(model(vals, hid)[:, j, :NB[v]].double().exp())
            bl = torch.cat(out).numpy()
            assert np.isfinite(bl).all()
            belief[v] = bl / bl.sum(1, keepdims=True)

    sw = scf.weight.to_numpy(float)
    pw = puf.weight.to_numpy(float)
    scell, pcell = scf.cell.to_numpy(), puf.cell.to_numpy()
    nw = scf.net_worth.to_numpy(float)
    wages = scf.wages.to_numpy(float)
    rep = {v: scf[v].to_numpy(float) for v in SLOTS3}
    md, js, fills_long = [], {'model_dir': MODEL_DIR, 'notes': notes, 'items': {}}, []
    md += ['# Step 30: the processing step on the 2016 SCF wave (tables)', '',
           f'Model: `{MODEL_DIR}` (step 29, rule 3 applied wave by wave, label on, table E27). '
           f'SCF 2016 wave: {len(scf):,} unit rows, {scf.household_id.nunique():,} households, '
           f'{sw.sum() / 1e6:.1f} million units (five implicates, weights summing to the population). '
           f'PUF side: {pw.sum() / 1e6:.1f} million units (returns plus crude non-filers). '
           f'Draws: {N_DRAWS}; shares and means are weighted and averaged over the draws.', '']
    md += [f'- {n}' for n in notes] + ['']
    head, md = md, []
    summary_rows = []

    for ci, (key, title, pv, sv, slot) in enumerate(CONCEPTS):
        rng_def = CMP.RANGES[sv]
        nR = len(rng_def)
        zi = rng_def.index((0, 0))
        labels = [CMP.label(lo, hi) for lo, hi in rng_def]
        px, sx = puf[pv].to_numpy(float), scf[sv].to_numpy(float)
        pin = np.column_stack([CMP.in_range(px, lo, hi) for lo, hi in rng_def])
        sin = np.column_stack([CMP.in_range(sx, lo, hi) for lo, hi in rng_def])
        assert (pin.sum(1) == 1).all() and (sin.sum(1) == 1).all(), f'{key}: a value in no range or two'
        pr, sr = pin.argmax(1), sin.argmax(1)

        # composition of each slice (PUF side) over the ranges
        pb = puf[f'{slot}_band'].to_numpy()
        S = np.zeros((NB[slot], nR))
        for r in range(nR):
            S[:, r] = np.bincount(pb[pin[:, r]], weights=pw[pin[:, r]], minlength=NB[slot])
        mass = S.sum(1)
        empty = mass == 0
        S[~empty] /= mass[~empty, None]
        straddle = int(((S > 0).sum(1) > 1).sum())
        # amount pools: PUF-side rows of slice b in range r
        pools = {}
        for b in range(NB[slot]):
            for r in range(nR):
                if r == zi or S[b, r] == 0:
                    continue
                m = (pb == b) & pin[:, r]
                o = np.argsort(px[m], kind='stable')
                pools[(b, r)] = (px[m][o], np.cumsum(pw[m][o]) / pw[m].sum())

        zero = sx == 0
        q = belief[slot] @ S
        lost = 1 - q.sum(1)
        lost_mean = wmean(lost[zero], sw[zero])
        q = q / q.sum(1, keepdims=True)

        # A. the cell table
        cell_rows, range_rows, prob = [], [], np.zeros((len(scf), nR))
        ipf = []
        for c in range(NCELL):
            pc, sc = pcell == c, scell == c
            p_r = np.array([pw[pc & (pr == r)].sum() for r in range(nR)]) / pw[pc].sum()
            s_r = np.array([sw[sc & (sr == r)].sum() for r in range(nR)]) / sw[sc].sum()
            tgt = np.where(np.arange(nR) == zi, 0.0, np.maximum(p_r - s_r, 0.0))
            exc = np.where(np.arange(nR) == zi, 0.0, np.maximum(s_r - p_r, 0.0))
            tgt[zi] = s_r[zi] - tgt.sum()
            assert tgt[zi] >= 0, f'{key} cell {c}: hidden holders exceed the zeros'
            W = sw[sc].sum()
            # B. multipliers by iterative proportional scaling on the cell's zeros
            zc = np.flatnonzero(sc & zero)
            qc, wc = q[zc], sw[zc]
            T = W * tgt
            mult = np.where(T > 0, 1.0, 0.0)
            unreachable = [labels[r] for r in range(nR) if T[r] > 0 and (wc @ qc[:, r]) == 0]
            for it in range(5000):
                pp = qc * mult
                pp /= pp.sum(1, keepdims=True)
                agg = wc @ pp
                if np.abs(agg - T).max() / T.sum() < 1e-10:
                    break
                with np.errstate(divide='ignore', invalid='ignore'):
                    mult = np.where(T > 0, mult * np.where(agg > 0, T / agg, 1.0), 0.0)
                mult /= mult.max()
            pp = qc * mult
            no_mass = int((pp.sum(1) == 0).sum())
            assert no_mass == 0, f'{key} cell {c}: {no_mass} zero rows with no probability left'
            pp /= pp.sum(1, keepdims=True)
            prob[zc] = pp
            agg = wc @ pp
            ipf.append({'cell': c, 'iterations': it + 1, 'largest miss, share of cell zeros':
                        float(np.abs(agg - T).max() / T.sum()), 'multipliers': mult.tolist(),
                        'ranges with a target but no belief': unreachable})
            cell_rows.append({'c': c, 'W': W, 'p': p_r, 's': s_r, 'tgt': tgt, 'exc': exc})

        # C. five draws: range, slice within the range, PUF-side amount
        fills = np.repeat(sx[:, None], N_DRAWS, 1)
        zrows = np.flatnonzero(zero)
        for k in range(N_DRAWS):
            rng = np.random.default_rng([SEED, ci, k])
            cp = prob[zrows].cumsum(1)
            u = rng.random(len(zrows))[:, None] * cp[:, -1:]
            rr = np.minimum((u >= cp).sum(1), nR - 1)
            amt = np.zeros(len(zrows))
            for r in range(nR):
                if r == zi:
                    continue
                sel = np.flatnonzero(rr == r)
                if not len(sel):
                    continue
                wb = belief[slot][zrows[sel]] * S[:, r][None, :]
                cb = wb.cumsum(1)
                assert (cb[:, -1] > 0).all(), f'{key}: a drawn range with no belief'
                ub = rng.random(len(sel))[:, None] * cb[:, -1:]
                bb = np.minimum((ub >= cb).sum(1), NB[slot] - 1)
                for b in np.unique(bb):
                    s2 = sel[bb == b]
                    xs, F = pools[(b, r)]
                    amt[s2] = xs[np.minimum(np.searchsorted(F, rng.random(len(s2))), len(xs) - 1)]
            fills[zrows, k] = amt
        fill_r = [np.column_stack([CMP.in_range(fills[:, k], lo, hi) for lo, hi in rng_def]).argmax(1)
                  for k in range(N_DRAWS)]
        hid_mask = [(zero & (fills[:, k] != 0)) for k in range(N_DRAWS)]

        fills_long.append(pd.DataFrame({
            'record_id': scf.record_id.to_numpy(), 'household_id': scf.household_id.to_numpy(),
            'implicate': scf.implicate.to_numpy(), 'weight': sw, 'cell': scell, 'variable': key,
            'reported': sx, **{f'fill{k + 1}': fills[:, k] for k in range(N_DRAWS)}}))

        # ---- tables ----
        md += [f'## {title}', '',
               f'Belief mass on slices with no PUF-side rows (removed before normalizing), weighted mean '
               f'over zero-reporters: {100 * lost_mean:.4f}%. Slices straddling a range edge: {straddle}. '
               f'Iterative scaling: largest miss over cells {max(i["largest miss, share of cell zeros"] for i in ipf):.2e} '
               f'of the cell\'s zeros.', '']
        if any(i['ranges with a target but no belief'] for i in ipf):
            md += ['Ranges with a positive target but no belief mass: ' +
                   '; '.join(f'cell {i["cell"]}: {i["ranges with a target but no belief"]}'
                             for i in ipf if i['ranges with a target but no belief']), '']
        # 1a. cell summary
        rows = []
        for cr in cell_rows + [None]:
            if cr is None:
                sc, label_ = np.ones(len(scf), bool), 'all cells'
                pc = np.ones(len(puf), bool)
            else:
                c = cr['c']
                sc, label_, pc = scell == c, cell_label[c], pcell == c
            W = sw[sc].sum()
            Wz = sw[sc & zero].sum()
            if cr is None:
                tg = sum(x['W'] * x['tgt'] for x in cell_rows)
                ex = sum(x['W'] * x['exc'] for x in cell_rows)
            else:
                tg, ex = cr['W'] * cr['tgt'], cr['W'] * cr['exc']
            hid_tgt = tg.sum() - tg[zi]
            hid_draw = np.mean([sw[sc & h].sum() for h in hid_mask])
            rows.append({'cell': label_, 'SCF households': scf.household_id[sc].nunique(),
                         'holders, PUF (%)': 100 * pw[pc & (px != 0)].sum() / pw[pc].sum(),
                         'holders, SCF reported (%)': 100 * sw[sc & ~zero].sum() / W,
                         'hidden holders, target (% of units)': 100 * hid_tgt / W,
                         'hidden holders, target (% of zeros)': 100 * hid_tgt / Wz,
                         'hidden holders, five draws (% of units)': 100 * hid_draw / W,
                         'holders, SCF filled in (%)': 100 * (sw[sc & ~zero].sum() + hid_draw) / W,
                         'excess SCF reporters (points)': 100 * ex.sum() / W})
        md += ['### 1. Hidden holders by cell', '',
               'Target: the sum over size ranges of PUF share minus SCF reported share, where positive. '
               'Excess: the sum of SCF reported share minus PUF share where that is positive (left as reported).', '',
               pd.DataFrame(rows).to_markdown(index=False, floatfmt='.2f'), '']
        summary_rows.append({'item': title, **{k_: v_ for k_, v_ in rows[-1].items() if k_ not in ('cell', 'SCF households')}})
        summary_rows[-1]['holders, PUF at the SCF cell mix (%)'] = 100 * sum(
            sw[scell == c].sum() / sw.sum() * pw[(pcell == c) & (px != 0)].sum() / pw[pcell == c].sum()
            for c in range(NCELL))
        # 1b + 2. by cell and range
        rows = []
        for cr in cell_rows + [None]:
            if cr is None:
                sc, label_, W = np.ones(len(scf), bool), 'all cells', sw.sum()
                pc = np.ones(len(puf), bool)
                p_r = np.array([pw[pr == r].sum() for r in range(nR)]) / pw.sum()
                s_r = np.array([sw[sr == r].sum() for r in range(nR)]) / W
                tgt = sum(x['W'] * x['tgt'] for x in cell_rows) / W
                exc = sum(x['W'] * x['exc'] for x in cell_rows) / W
            else:
                c = cr['c']
                sc, label_, W, pc = scell == c, cell_label[c], cr['W'], pcell == c
                p_r, s_r, tgt, exc = cr['p'], cr['s'], cr['tgt'], cr['exc']
            Wz = sw[sc & zero].sum()
            for r in range(nR):
                rep_r = sw[sc & ~zero & (sr == r)].sum()
                exp_r = (rep_r + sw[sc & zero] @ prob[sc & zero, r]) / W if r != zi else \
                    (sw[sc & zero] @ prob[sc & zero, r]) / W
                draw_r = np.mean([sw[sc & (fr == r)].sum() for fr in fill_r]) / W
                hm = [sc & h & (fr == r) for h, fr in zip(hid_mask, fill_r)]
                hid_mean = np.nanmean([wmean(fills[m_, k], sw[m_]) if m_.any() else np.nan
                                       for k, m_ in enumerate(hm)]) if r != zi and any(m_.any() for m_ in hm) else np.nan
                pm_ = pc & (pr == r)
                rows.append({'cell': label_, 'range': labels[r],
                             'PUF (%)': 100 * p_r[r], 'SCF reported (%)': 100 * s_r[r],
                             'target, hidden (% of units)': 100 * tgt[r] if r != zi else np.nan,
                             'target, hidden (% of zeros)': 100 * tgt[r] * W / Wz if r != zi else np.nan,
                             'excess (points)': 100 * exc[r] if r != zi else np.nan,
                             'SCF filled in, expected (%)': 100 * exp_r,
                             'SCF filled in, five draws (%)': 100 * draw_r,
                             'filled in minus PUF (points)': 100 * (draw_r - p_r[r]),
                             'mean, hidden holders ($)': hid_mean,
                             'mean, PUF ($)': wmean(px[pm_], pw[pm_]) if r != zi and pm_.any() else np.nan})
        md += ['### 2. By cell and size range: targets, excesses and the filled-in shares', '',
               'Shares are of all units in the cell. "Expected" uses each zero-reporter\'s final probabilities; '
               '"five draws" the drawn ranges. Where PUF exceeds SCF reported, the filled-in share should equal the PUF share.', '',
               pd.DataFrame(rows).to_markdown(index=False, floatfmt='.2f'), '']
        # 3. overall
        tot = {'reported': (sw * sx).sum() / 1e9,
               'filled': np.mean([(sw * fills[:, k]).sum() for k in range(N_DRAWS)]) / 1e9,
               'puf': (pw * px).sum() / 1e9}
        o = {'holders, SCF reported (%)': 100 * sw[~zero].sum() / sw.sum(),
             'holders, SCF filled in (%)': 100 * np.mean([sw[fills[:, k] != 0].sum() for k in range(N_DRAWS)]) / sw.sum(),
             'holders, PUF side (%)': 100 * pw[px != 0].sum() / pw.sum(),
             # the PUF side's cell rates at the SCF's mix of units across cells
             'holders, PUF side at the SCF cell mix (%)': 100 * sum(
                 sw[scell == c].sum() / sw.sum() * pw[(pcell == c) & (px != 0)].sum() / pw[pcell == c].sum()
                 for c in range(NCELL)),
             'total, SCF reported ($bn)': tot['reported'], 'total, SCF filled in ($bn)': tot['filled'],
             'total, PUF side ($bn)': tot['puf'],
             'per unit, SCF reported ($)': 1e9 * tot['reported'] / sw.sum(),
             'per unit, SCF filled in ($)': 1e9 * tot['filled'] / sw.sum(),
             'per unit, PUF side ($)': 1e9 * tot['puf'] / pw.sum(),
             'per unit, PUF side at the SCF cell mix ($)': sum(
                 sw[scell == c].sum() / sw.sum() * wmean(px[pcell == c], pw[pcell == c]) for c in range(NCELL)),
             'hidden holders\' dollars, share of filled-in total (%)':
                 100 * (tot['filled'] - tot['reported']) / tot['filled'] if tot['filled'] else np.nan}
        if sv == 'capital_gains':
            o['losses, SCF reported ($bn)'] = (sw * np.minimum(sx, 0)).sum() / 1e9
            o['losses, SCF filled in ($bn)'] = np.mean([(sw * np.minimum(fills[:, k], 0)).sum() for k in range(N_DRAWS)]) / 1e9
            o['losses, PUF side ($bn)'] = (pw * np.minimum(px, 0)).sum() / 1e9
        md += ['### 3. Overall', '', pd.DataFrame([o]).T.rename(columns={0: 'value'}).to_markdown(floatfmt=',.2f'), '']
        # 4. who the hidden holders are
        if key in WHO_FOR:
            others = [v for v in SLOTS3 if v != slot]
            rows = []
            for c in list(range(NCELL)) + ['all']:
                sc = np.ones(len(scf), bool) if c == 'all' else scell == c
                for grp in ['reporters', 'hidden holders', 'remaining zeros']:
                    per = []
                    for k in range(N_DRAWS):
                        m_ = sc & (~zero if grp == 'reporters' else (hid_mask[k] if grp == 'hidden holders'
                                                                     else zero & ~hid_mask[k]))
                        if not m_.any():
                            continue
                        w_ = sw[m_]
                        r_ = {'share of cell units (%)': 100 * w_.sum() / sw[sc].sum(),
                              'unit rows (per draw)': int(m_.sum()),
                              'net worth, mean ($)': wmean(nw[m_], w_),
                              'net worth, median ($)': CMP.wpct(nw[m_], w_, 50),
                              'wages, mean ($)': wmean(wages[m_], w_),
                              **{f'reports {o_.replace("_", " ")} (%)': 100 * w_[rep[o_][m_] != 0].sum() / w_.sum()
                                 for o_ in others},
                              f'{key.replace("_", " ")}, mean ($)': wmean(fills[m_, k], w_)}
                        per.append(r_)
                    if not per:
                        continue
                    avg = {k_: float(np.mean([p_[k_] for p_ in per])) for k_ in per[0]}
                    rows.append({'cell': 'all cells' if c == 'all' else cell_label[c], 'group': grp, **avg})
            md += ['### 4. Who the hidden holders are', '',
                   'Reporters: reported a nonzero amount. Hidden holders: reported zero, filled in nonzero. '
                   'Remaining zeros: reported zero, filled in zero. The other items are as reported. '
                   'Each figure is computed per draw and averaged over the five draws.', '',
                   pd.DataFrame(rows).to_markdown(index=False, floatfmt=',.1f'), '']
        js['items'][key] = {'title': title, 'belief mass on empty slices (weighted mean over zeros)': lost_mean,
                            'slices straddling a range edge': straddle, 'ranges': labels,
                            'cells': [{'cell': cell_label[x['c']], 'PUF share': x['p'].tolist(),
                                       'SCF share': x['s'].tolist(), 'target share of units': x['tgt'].tolist(),
                                       'excess': x['exc'].tolist()} for x in cell_rows],
                            'scaling': ipf, 'overall': o}

    md = head + ['## Summary over all cells', '',
                 pd.DataFrame(summary_rows).to_markdown(index=False, floatfmt='.2f'), ''] + md
    pd.concat(fills_long, ignore_index=True).to_parquet(f'{OUT}/fills_2016.parquet', index=False)
    open(f'{OUT}/process.md', 'w').write('\n'.join(md))
    json.dump(js, open(f'{OUT}/process.json', 'w'), indent=1, default=float)
    print('\n'.join(md))


if __name__ == '__main__':
    main()
