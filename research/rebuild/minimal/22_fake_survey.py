"""
22_fake_survey.py  (minimal example, step 22: the fake-survey test of plan
decision 18; 2026-10-16. Design: design/translation_by_latent_amounts.md,
section 8, test 1. Write-up: 22_FAKE_SURVEY.md.)

What it does
  Makes a survey out of the PUF, so the right answer is known, and asks how
  well each way of translating it lets the model hand the survey's own
  variable to PUF returns.

  The 2015 PUF records of stacked_E4.parquet are split at random into two
  piles of different returns:
    - half A plays the PUF: married, dependents, wages, interest, dividends
      and capital gains as they are; other income (other_agi) removed.
    - half B, 30,000 records drawn from the other half, plays the survey: it
      keeps other income, the variable the survey owns (stored in the
      net_worth slot, which the model treats as owned by the survey), and
      its interest and dividends are broken on purpose (the distortions).
  Half A's true other income is never shown to the model, so the model's
  other income for half A can be scored against it.

  Distortions of the survey's interest and dividends (wages, capital gains,
  married, dependents and other income are left true):
    D0  none (the best this sample size can do).
    D1  small holders report zero: a holder of x dollars reports with chance
        S(x) = 1 / (1 + exp(-SLOPE (ln x - ln MID))), MID = $150, SLOPE =
        1.05 (13% report at $50, 50% at $150, 95% at $2,500). Each item drawn
        on its own. Reporters' dollars are right.
    D2  D1, and reporters report 0.8 of their true dollars.
    D3  D1, but households with other income below the survey's weighted
        median report less (logit lowered by 0.75) and those above more
        (raised by 0.75): the hidden holders are poorer than the reporters,
        which no method can see in the data.
    D4  D1, but one draw per household decides both items, so a household
        that omits interest tends to omit dividends too.

  Methods ("arms"):
    oracle        D0 only; today's model on the undistorted survey.
    rule          today's rule (decision 15): the survey's reported dollars
                  placed on the PUF's dollar scale; today's model (source
                  label on every slot).
    rank          each file on its own percentiles (the old rule).
    prefill       fill in the survey's true interest and dividends once,
                  before training, from a separate simple model (PUF shares
                  by wage group and marriage times the planted chance of
                  reporting zero; reporters' dollars divided by the planted
                  factor), then today's model on the filled-in table.
    nofill        the model with no source label (shared slots predicted the
                  same way for both files), survey zeros taken as zeros.
    latent_fixed  decision 18 with the reporting settings fixed at the
                  planted ones: no source label; each pass, each survey zero
                  of interest and dividends gets a true amount drawn from the
                  model's belief given everything else on the record times
                  the chance of reporting zero; everything trains on the
                  drawn amount.
    latent_learn  the same, with the settings learned in training: the
                  S-curve's two settings by gradient steps on the chance of
                  reporting anything given the rest of the record (the
                  model's belief about the true amount summed against the
                  curve), the dollar factor once per pass by a grid search
                  on the likelihood of the reporters' reported slices.

  The score: for every half-A return, the model's distribution of other
  income given the return's true married, dependents and four incomes (the
  survey's variable drawn under the survey's label, decision 16; no label
  for nofill and the latent arms). Reported: the log score of the true
  slice (lower is better), and the expected other income by group, in 2015
  dollars (the target year of this test), against the truth.

Usage (Slurm, 22_run.sbatch):
  python research/rebuild/minimal/22_fake_survey.py build
  python research/rebuild/minimal/22_fake_survey.py run <arm> <D> <seed>
  python research/rebuild/minimal/22_fake_survey.py report
Writes, in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/:
  data/stacked_F22_<D>_<dollar|rank>.parquet, data/band_edges_F22_<D>_<...>.json
  fake22/aux_<D>.json        planted settings, band summaries, survey notes
  fake22/<arm>_<D>_s<seed>/  model.pt, train_log.csv, score.json
  fake22/report.md           (report)
"""
import ast
import importlib.util
import json
import math
import os
import sys
import time

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
DATA = f'{R}/data'
OUT = os.environ.get('MINIMAL_F22_OUT', f'{R}/fake22')
SPLIT_SEED, SURVEY_N = 20261016, 30_000
MID, SLOPE = 150.0, 1.05          # planted S-curve
SCALE_D2, SHIFT_D3 = 0.8, 0.75
LATENT = ['interest', 'dividends']
SHARED = ['wages', 'interest', 'dividends', 'capital_gains']
AMOUNTS = SHARED + ['other_agi', 'net_worth']
DISTORTIONS = ['D0', 'D1', 'D2', 'D3', 'D4']
ARMS = ['oracle', 'rule', 'rank', 'prefill', 'nofill', 'latent_fixed', 'latent_learn']
LABEL_FREE = ['nofill', 'latent_fixed', 'latent_learn']
FILL_FROM_EPOCH = int(os.environ.get('MINIMAL_F22_FILL_FROM', '3'))   # fill-ins start after the warm-up
DISTORTION_TEXT = {
    'D0': 'none',
    'D1': 'small holders report zero (S-curve, 50% at $150); dollars right',
    'D2': 'D1, and reporters report 0.8 of their true dollars',
    'D3': 'D1, but households with other income below the median report less (hidden holders poorer)',
    'D4': 'D1, but one draw decides both items (omit interest, omit dividends too)',
}

# 01_band.py's constants and functions only (as 10_distort.py loads them).
_band_file = f'{HERE}/01_band.py'
_tree = ast.parse(open(_band_file).read())
_stop = next(i for i, n in enumerate(_tree.body) if 'read_csv' in ast.unparse(n))
_keep = [n for n in _tree.body[:_stop] if isinstance(n, (ast.Import, ast.ImportFrom, ast.FunctionDef))
         or (isinstance(n, (ast.Assign, ast.Assert)) and 'sys.argv' not in ast.unparse(n)
             and 'SUF' not in ast.unparse(n))]
B = {}
exec(compile(ast.Module(body=_keep, type_ignores=[]), _band_file, 'exec'), B)
band_variable, wquantile, N_BANDS, POS0 = (B[k] for k in ['band_variable', 'wquantile', 'N_BANDS', 'POS0'])
assert N_BANDS == 84


def p_report(x, shift=0.0):
    """Planted chance that a holder of x dollars reports a nonzero amount."""
    x = np.asarray(x, dtype=float)
    z = SLOPE * (np.log(np.maximum(x, 1e-9)) - math.log(MID)) + shift
    return np.where(x > 0, 1.0 / (1.0 + np.exp(-z)), 0.0)


def wmean(x, w):
    return float(np.sum(x * w) / np.sum(w))


# ---- build ---------------------------------------------------------------------------
def distort(Bdf, D, other_median):
    """Reported interest and dividends of the survey pile under distortion D."""
    rng = np.random.default_rng(SPLIT_SEED + DISTORTIONS.index(D))
    rep = {v: Bdf[v].to_numpy(dtype=float).copy() for v in LATENT}
    if D == 'D0':
        return rep
    shift = 0.0
    if D == 'D3':
        shift = np.where(Bdf['other_true'].to_numpy() >= other_median, SHIFT_D3, -SHIFT_D3)
    shared_u = rng.random(len(Bdf))
    for v in LATENT:
        u = shared_u if D == 'D4' else rng.random(len(Bdf))
        reports = u < p_report(rep[v], shift)
        rep[v] = np.where(reports, rep[v], 0.0)
        if D == 'D2':
            rep[v] = rep[v] * SCALE_D2
    return rep


def band_table(df, survey_on_puf_cuts):
    """Band every amount. PUF rows on their own cuts; survey shared amounts on
    the PUF's cuts (dollar) or their own (rank); other income (net_worth slot)
    on the survey's cuts; other_agi absent everywhere (its PUF layout is kept
    for the model's vocabulary only)."""
    edges = {}
    puf, srv = (df.source == 'puf').to_numpy(), (df.source == 'scf').to_numpy()
    for v in AMOUNTS:
        df[f'{v}_band'] = np.int16(-1)
    for v in AMOUNTS:
        for s, m in [('puf', puf), ('scf', srv)]:
            x, w = df.loc[m, v].to_numpy(dtype=float), df.loc[m, 'weight'].to_numpy()
            if v == 'other_agi' and s == 'puf':      # layout only: from half A's truth
                _, rec = band_variable(df.loc[m, 'other_true'].to_numpy(dtype=float), w)
                edges[f'{s}/2015/{v}'] = rec
                continue
            fixed = edges[f'puf/2015/{v}'] if (s == 'scf' and v in SHARED and survey_on_puf_cuts) else None
            band, rec = band_variable(x, w, fixed)
            df.loc[m, f'{v}_band'] = band
            edges[f'{s}/2015/{v}'] = rec
    for v in AMOUNTS:
        df[f'{v}_band'] = df[f'{v}_band'].astype('int16')
    # truth bands for scoring: true interest and dividends on the PUF's cuts,
    # true other income on the survey's cuts
    for v in LATENT:
        df[f'{v}_true_band'] = band_variable(df[f'{v}_true'].to_numpy(dtype=float), df['weight'].to_numpy(),
                                             edges[f'puf/2015/{v}'])[0].astype('int16')
    df['other_true_band'] = band_variable(df['other_true'].to_numpy(dtype=float), df['weight'].to_numpy(),
                                          edges['scf/2015/net_worth'])[0].astype('int16')
    return edges


def build():
    os.makedirs(OUT, exist_ok=True)
    src = pd.read_parquet(f'{DATA}/stacked_E4.parquet')
    puf = src[src.source == 'puf'].reset_index(drop=True)
    base = ['source', 'year', 'record_id', 'household_id', 'implicate', 'split', 'weight',
            'married', 'n_dep'] + SHARED
    puf = puf[base + ['other_agi']].rename(columns={'other_agi': 'other_true'})
    assert not puf[base + ['other_true']].isna().any().any()
    for v in LATENT:
        puf[f'{v}_true'] = puf[v]
        assert (puf[v] >= 0).all(), v
    rng = np.random.default_rng(SPLIT_SEED)
    in_a = rng.random(len(puf)) < 0.5
    A = puf[in_a].copy()
    rest = puf[~in_a]
    Bdf = rest.iloc[np.sort(rng.choice(len(rest), SURVEY_N, replace=False))].copy()
    A['other_agi'], A['net_worth'] = np.nan, np.nan
    Bdf['source'] = 'scf'
    Bdf['other_agi'], Bdf['net_worth'] = np.nan, Bdf['other_true']
    other_median = float(wquantile(Bdf['other_true'].to_numpy(), Bdf['weight'].to_numpy(), [0.5])[0])
    A_shift_group = np.where(A['other_true'].to_numpy() >= other_median, SHIFT_D3, -SHIFT_D3)
    for D in DISTORTIONS:
        Bd = Bdf.copy()
        rep = distort(Bd, D, other_median)
        for v in LATENT:
            Bd[v] = rep[v]
        df = pd.concat([A, Bd], ignore_index=True)
        df['split'] = 'train'                    # 02_model's MINIMAL_ALL_RECORDS relabels
        rules = ['dollar'] if D == 'D0' else ['dollar', 'rank']
        for rule in rules:
            t = df.copy()
            edges = band_table(t, rule == 'dollar')
            check(t, D, rule)
            tag = f'F22_{D}_{rule}'
            t.to_parquet(f'{DATA}/stacked_{tag}.parquet', index=False)
            meta = {'set': 'F22', 'distortion': D, 'text': DISTORTION_TEXT[D], 'rule': rule}
            json.dump({'meta': meta, **edges}, open(f'{DATA}/band_edges_{tag}.json', 'w'), indent=1)
            if rule == 'dollar':
                aux = aux_record(t, edges, D, A_shift_group, other_median)
                json.dump(aux, open(f'{OUT}/aux_{D}.json', 'w'), indent=1)
                print(json.dumps(aux['note'], indent=1), flush=True)
    print('build done', flush=True)


def check(t, D, rule):
    puf, srv = t.source == 'puf', t.source == 'scf'
    assert puf.sum() > 100_000 and srv.sum() == SURVEY_N
    assert (t.loc[puf, 'net_worth_band'] == -1).all() and (t.loc[srv, 'net_worth_band'] >= 0).all()
    assert (t['other_agi_band'] == -1).all()
    for v in SHARED:
        b = t[f'{v}_band']
        assert ((b == 0) == (t[v] == 0)).all() and b.between(0, N_BANDS - 1).all(), (D, rule, v)
    for v in LATENT:   # the survey reports zero or a band at or below... (D2: scaled)
        assert (t.loc[puf, f'{v}_band'] == t.loc[puf, f'{v}_true_band']).all(), (D, v)
        rz = t.loc[srv, v] == 0
        tz = t.loc[srv, f'{v}_true'] == 0
        assert (rz | ~tz).all(), f'{D}: a true zero reported as nonzero'
    print(f'{D} {rule}: checks passed', flush=True)


def aux_record(t, edges, D, A_shift_group, other_median):
    """Planted settings and band summaries used by run()."""
    puf, srv = (t.source == 'puf').to_numpy(), (t.source == 'scf').to_numpy()
    w = t['weight'].to_numpy()
    out = {'D': D, 'planted': {'mid': MID, 'slope': SLOPE, 'alpha': -SLOPE * math.log(MID),
                               'factor': SCALE_D2 if D == 'D2' else 1.0,
                               'shift_d3': SHIFT_D3 if D == 'D3' else 0.0},
           'lnx': {}, 'g_planted': {}, 'cuts': {}, 'note': {'D': D, 'text': DISTORTION_TEXT[D]}}
    for v in LATENT:
        x, b = t.loc[puf, v].to_numpy(dtype=float), t.loc[puf, f'{v}_band'].to_numpy()
        wa = w[puf]
        cuts = edges[f'puf/2015/{v}']['positive']['cuts']
        out['cuts'][v] = cuts
        lnx, g = np.full(N_BANDS, np.nan), np.ones(N_BANDS)
        g[0] = 0.0
        shift = A_shift_group if D == 'D3' else 0.0
        pr = p_report(x, shift) if D != 'D0' else np.where(x > 0, 1.0, 0.0)
        for k in range(POS0, N_BANDS):
            m = b == k
            if m.any():
                lnx[k] = wmean(np.log(x[m]), wa[m])
                g[k] = wmean(pr[m], wa[m])
        # bands no half-A record falls in (duplicate cut points): log of the band's upper cut
        upper = np.log(np.array(cuts + [cuts[-1] * 2.0]))
        for k in range(POS0, N_BANDS):
            if np.isnan(lnx[k]):
                lnx[k] = upper[k - POS0]
        out['lnx'][v] = lnx.tolist()
        out['g_planted'][v] = g.tolist()
        rep, tru = t.loc[srv, v].to_numpy(), t.loc[srv, f'{v}_true'].to_numpy()
        ws = w[srv]
        out['note'][v] = {
            'survey holders, true (%)': round(100 * wmean(tru > 0, ws), 2),
            'survey holders, reported (%)': round(100 * wmean(rep > 0, ws), 2),
            'hidden holders among reported zeros (%)': round(100 * float(ws[(rep == 0) & (tru > 0)].sum()
                                                                          / ws[rep == 0].sum()), 2),
            'reported total over true total': round(float((rep * ws).sum() / (tru * ws).sum()), 4),
            'PUF (half A) holders (%)': round(100 * wmean(t.loc[puf, v].to_numpy() > 0, w[puf]), 2)}
    # dollars of each other-income slice: weighted mean of the survey's other income
    nb = t.loc[srv, 'net_worth_band'].to_numpy()
    oi = t.loc[srv, 'net_worth'].to_numpy()
    m_b = np.zeros(N_BANDS)
    for k in range(N_BANDS):
        m = nb == k
        m_b[k] = wmean(oi[m], w[srv][m]) if m.any() else 0.0
    out['other_band_dollars'] = m_b.tolist()
    out['other_median'] = other_median
    return out


# ---- run -------------------------------------------------------------------------------
def load_model_module(tag, model_dir):
    os.environ.update(MINIMAL_TABLE=tag, MINIMAL_ALL_RECORDS='1', MINIMAL_MODEL_DIR=model_dir,
                      MINIMAL_GEN_DIR=model_dir)
    spec = importlib.util.spec_from_file_location('model02', f'{HERE}/02_model.py')
    M = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(M)
    assert not (M.NO_LABEL_SHARED or M.REPORT_CURVE or M.COARSEN or M.SCF_ZERO_UNKNOWN)
    assert M.PREDICTED == ['married', 'n_dep', 'wages', 'interest', 'dividends', 'capital_gains',
                           'other_agi', 'net_worth'], M.PREDICTED
    return M


def prefill(table, aux, seed):
    """Fill in the survey's true interest and dividends once, before training,
    from PUF shares by wage group and marriage times the planted chance of
    reporting zero; reporters' dollars divided by the planted factor. Each
    item on its own."""
    rng = np.random.default_rng(seed + 7)
    puf, srv = (table.source == 'puf').to_numpy(), (table.source == 'scf').to_numpy()
    wb = table['wages_band'].to_numpy()
    wgroup = np.where(wb == 0, 0, 1 + np.clip((wb - POS0) * 10 // (N_BANDS - POS0), 0, 9))
    cell = wgroup * 2 + table['married'].to_numpy()
    w = table['weight'].to_numpy()
    f = aux['planted']['factor']
    out = table.copy()
    for v in LATENT:
        b = table[f'{v}_band'].to_numpy()
        counts = np.zeros((22, N_BANDS))
        np.add.at(counts, (cell[puf], b[puf]), w[puf])
        marg = counts.sum(0) / counts.sum()
        p = counts / counts.sum(1, keepdims=True).clip(min=1e-12)
        p = 0.999 * p + 0.001 * marg
        one_minus_g = 1.0 - np.array(aux['g_planted'][v])
        new = b.copy()
        dollars = table[v].to_numpy(dtype=float)
        cuts = np.array(aux['cuts'][v])
        rep = srv & (dollars > 0)
        new[rep] = POS0 + np.searchsorted(cuts, dollars[rep] / f, side='left')
        for i in np.flatnonzero(srv & (dollars == 0)):
            q = p[cell[i]] * one_minus_g
            new[i] = rng.choice(N_BANDS, p=q / q.sum())
        out[f'{v}_band'] = new.astype('int16')
    return out


def lr_factor_fn(max_epochs, steps_per_epoch, warmup_epochs=3):
    total, warmup = max_epochs * steps_per_epoch, warmup_epochs * steps_per_epoch

    def f(step):
        if step < warmup:
            return (step + 1) / warmup
        progress = (step - warmup) / max(1, total - warmup)
        return 0.05 + 0.95 * 0.5 * (1 + math.cos(math.pi * progress))
    return f


def train_latent(M, table, aux, arm, seed, max_epochs, device):
    """The label-free model, with (latent_*) or without (nofill) the fill-ins."""
    import torch
    torch.manual_seed(seed)
    gen = torch.Generator(device=device).manual_seed(seed)
    C = len(M.CONTEXT)
    j_nw = M.PREDICTED.index('net_worth')
    jl = {v: M.PREDICTED.index(v) for v in LATENT}
    LATENT_COL = torch.tensor([v in LATENT for v in M.PREDICTED], device=device)

    def enc(part):
        d = M.encode(part, device)
        vals = d['values'].clone()
        vals[:, 0] = -1                                  # no source label
        puf = torch.tensor((part.source == 'puf').to_numpy(), device=device)
        unknown = torch.zeros(len(part), len(M.PREDICTED), dtype=torch.bool, device=device)
        unknown[puf, j_nw] = True                        # the survey's slot on PUF rows: always
        vals[puf, C + j_nw] = 0                          # hidden, never scored (as a hidden one)
        srv = ~puf
        rep_band = {v: vals[:, C + jl[v]].clone() for v in LATENT}
        dollars = {v: torch.tensor(part[v].to_numpy(dtype=np.float64), device=device) for v in LATENT}
        return dict(values=vals, weight=d['weight'], unknown=unknown, srv=srv, rep_band=rep_band,
                    dollars=dollars)
    tr = enc(table[table.split == 'train'])
    te = enc(table[table.split == 'test'])
    fill = arm != 'nofill'
    learn = arm == 'latent_learn'
    cuts = {v: torch.tensor(aux['cuts'][v], dtype=torch.float64, device=device) for v in LATENT}
    # mean log dollars per slice; the loss slices (unused for these items) are
    # undefined and set to 0, since a NaN there poisons the gradient even where masked
    lnx = {v: torch.nan_to_num(torch.tensor(aux['lnx'][v], dtype=torch.float32, device=device), nan=0.0)
           for v in LATENT}
    pos = torch.arange(M.N_CHOICES['interest'], device=device) >= POS0
    is_zero = torch.arange(M.N_CHOICES['interest'], device=device) == 0
    g_fixed = {v: torch.tensor(aux['g_planted'][v], dtype=torch.float32, device=device) for v in LATENT}
    factor = {v: (aux['planted']['factor'] if arm == 'latent_fixed' else 1.0) for v in LATENT}
    LN_CENTRE = math.log(MID)        # $150; a reporting setting, not the answer: alpha starts at 0 (50%)
    alpha = torch.nn.Parameter(torch.zeros(len(LATENT), device=device))
    beta = torch.nn.Parameter(torch.zeros(len(LATENT), device=device))

    def log_g(k):
        """log g and log(1 - g) over the bands of LATENT[k]; band 0 never
        reports, other non-positive bands (unused) always."""
        v = LATENT[k]
        if learn:   # log dollars measured from LN_CENTRE, so the two settings are not tied together
            z = alpha[k] + beta[k] * (lnx[v] - LN_CENTRE)
            lg, l1mg = torch.nn.functional.logsigmoid(z), torch.nn.functional.logsigmoid(-z)
        else:
            g = g_fixed[v].clamp(1e-6, 1 - 1e-6)
            lg, l1mg = torch.log(g), torch.log1p(-g)
        lg = torch.where(pos, lg, torch.zeros_like(lg))
        l1mg = torch.where(pos, l1mg, torch.full_like(l1mg, -math.inf))
        lg = torch.where(is_zero, torch.full_like(lg, -math.inf), lg)
        l1mg = torch.where(is_zero, torch.zeros_like(l1mg), l1mg)
        return lg, l1mg

    def reporter_bands(d):
        """True band of each survey reporter: its dollars over the factor, on the PUF's cuts."""
        for v in LATENT:
            x = d['dollars'][v]
            rep = d['srv'] & (x > 0)
            b = POS0 + torch.searchsorted(cuts[v], x[rep] / factor[v], right=False)
            d['values'][rep, C + jl[v]] = b.to(d['values'].dtype)
    if fill:
        reporter_bands(tr)
        reporter_bands(te)

    model = M.SlotTransformer().to(device)
    params = [{'params': model.parameters(), 'lr': 1e-3, 'weight_decay': 0.01}]
    if learn:
        params.append({'params': [alpha, beta], 'lr': 5e-2, 'weight_decay': 0.0})
    opt = torch.optim.AdamW(params)
    n, bs = len(tr['values']), 4096
    spe = math.ceil(n / bs)
    sched = torch.optim.lr_scheduler.LambdaLR(opt, lr_factor_fn(max_epochs, spe))
    val_hidden = [M.draw_hidden(te['values'], gen, te['unknown']) for _ in range(4)]

    @torch.no_grad()
    def draw_fills(vals, srv_rows, rep_band):
        """New true amounts for the survey zeros among vals[srv_rows], each
        item given everything else on the record (the other item at its
        current fill), in a random order. Returns the belief (log
        probabilities over the item's slices, the item hidden) for every
        survey row, for the reporting-curve loss."""
        was = model.training
        model.eval()
        order = torch.randperm(len(LATENT), generator=gen, device=device).tolist()
        sub = vals[srv_rows]
        beliefs = {}
        for k in order:
            v = LATENT[k]
            zero = rep_band[v][srv_rows] == 0
            hid = torch.zeros(len(sub), len(M.PREDICTED), dtype=torch.bool, device=device)
            hid[:, jl[v]] = True
            lp = model(sub, hid)[:, jl[v], :M.N_CHOICES[v]]
            if learn:
                # for the settings: the belief with the survey's own slot hidden
                # too, which the PUF pins (the anchor); the fill-in below uses
                # the full belief
                hid_a = hid.clone()
                hid_a[:, j_nw] = True
                hid_a |= LATENT_COL[None, :]  # and both items: the settings must rest
                # only on what the survey reports truly, or a wrong setting for one
                # item inflates the other item's belief (step 22, test 5)
                beliefs[v] = model(sub, hid_a)[:, jl[v], :M.N_CHOICES[v]]
            if not zero.any():
                continue
            _, l1mg = log_g(k)
            logits = (lp + l1mg)[zero]
            u = torch.rand(logits.shape, generator=gen, device=device).clamp_min(1e-20)
            draw = (logits - torch.log(-torch.log(u))).argmax(1)       # Gumbel-max draw
            col = sub[:, C + jl[v]].clone()
            col[zero] = draw
            sub[:, C + jl[v]] = col
        vals[srv_rows] = sub
        if was:
            model.train()
        return beliefs

    FACTOR_GRID = np.exp(np.linspace(math.log(0.5), math.log(2.0), 81))   # 1.0 is the middle point

    def band_points(v, n=32):
        """Dollar points for each positive slice of v: n quantiles of the PUF
        pile's own dollars inside the slice, so rounding lumps ($10, $20)
        sit where they are. A slice with no PUF record gets its cut."""
        c = aux['cuts'][v]
        puf_rows = (table.source == 'puf').to_numpy()
        x = table.loc[puf_rows, v].to_numpy(dtype=float)
        b_of = table.loc[puf_rows, f'{v}_band'].to_numpy()
        pts = np.zeros((N_BANDS, n))
        for b in range(POS0, N_BANDS):
            inside = x[b_of == b]
            k = b - POS0
            pts[b] = (np.quantile(inside, (np.arange(n) + 0.5) / n) if len(inside)
                      else c[min(k, len(c) - 1)])
        return pts
    POINTS = {v: band_points(v) for v in LATENT}

    def transfer(v, f):
        """T[b, r]: share of true slice b that lands in reported slice r when
        dollars are multiplied by f (positive slices only)."""
        cut = np.array(aux['cuts'][v])
        r = POS0 + np.searchsorted(cut, POINTS[v][POS0:] * f, side='left')
        T = np.zeros((N_BANDS, N_BANDS))
        for b in range(POS0, N_BANDS):
            np.add.at(T[b], r[b - POS0], 1.0 / r.shape[1])
        return torch.tensor(T, dtype=torch.float32, device=device)

    @torch.no_grad()
    def update_factor():
        """Dollar factor per item: the factor on FACTOR_GRID under which the
        survey reporters' reported slices are most likely, given the model's
        belief about each reporter's true slice (from the rest of the record)
        times the chance of reporting."""
        model.eval()
        for k, v in enumerate(LATENT):
            rep = (tr['srv'] & (tr['rep_band'][v] > 0)).nonzero().squeeze(1)
            lg, _ = log_g(k)
            qs = []
            for i in range(0, len(rep), 20_000):
                idx = rep[i:i + 20_000]
                hid = torch.zeros(len(idx), len(M.PREDICTED), dtype=torch.bool, device=device)
                hid[:, jl[v]] = True
                hid[:, j_nw] = True          # the anchored belief, as in the curve's loss
                hid |= LATENT_COL[None, :]
                lp = model(tr['values'][idx], hid)[:, jl[v], :M.N_CHOICES[v]] + lg
                lp = torch.where(pos, lp, torch.full_like(lp, -math.inf))
                qs.append(lp.softmax(1))
            q = torch.cat(qs)
            r_obs = tr['rep_band'][v][rep]
            w = tr['weight'][rep]
            ll = [float((w * torch.log((q @ transfer(v, f)).gather(1, r_obs[:, None]).squeeze(1)
                                       .clamp_min(1e-12))).sum()) for f in FACTOR_GRID]
            factor[v] = float(FACTOR_GRID[int(np.argmax(ll))])
        model.train()
        reporter_bands(tr)
        reporter_bands(te)

    log = []
    for epoch in range(1, max_epochs + 1):
        model.train()
        t0 = time.time()
        order = torch.randperm(n, generator=gen, device=device)
        tot = 0.0
        for start in range(0, n, bs):
            idx = order[start:start + bs]
            beliefs = None
            if fill and epoch >= FILL_FROM_EPOCH:
                vals = tr['values'][idx]
                srv_rows = tr['srv'][idx]
                beliefs = draw_fills(vals, srv_rows, {v: tr['rep_band'][v][idx] for v in LATENT})
                tr['values'][idx] = vals
            values, weight, unknown = tr['values'][idx], tr['weight'][idx], tr['unknown'][idx]
            hidden = M.draw_hidden(values, gen, unknown)
            if learn:
                # The anchor (PUF = actual): the belief about a true amount given
                # the shared variables alone is learned from PUF rows only. A
                # survey row trains its interest and dividend slots only when its
                # own slot (other income) is visible, which is where it teaches
                # how they go with other income. Without this, the fill-ins feed
                # back into the belief the settings are fit against, and the
                # settings drift (step 22, tests 3 and 4).
                nll = M.slot_nll(model(values, hidden | unknown), values)
                scored = hidden & ~unknown
                drop = tr['srv'][idx][:, None] & LATENT_COL[None, :] & hidden[:, j_nw][:, None]
                w = weight[:, None] * (scored & ~drop)
                loss = (nll * w).sum() / w.sum()
            else:
                loss = M.masked_loss(model, values, weight, hidden, None, unknown)
            if learn and beliefs is not None:
                # reporting curve: the chance that a survey record reports
                # any amount, given the rest of the record, is the model's
                # belief about its true slice summed against g. The belief
                # is held fixed here (it is fit by the main loss, anchored by
                # the PUF); only the curve's two settings move. The factor
                # does not enter.
                srv = tr['srv'][idx]
                ws = weight[srv]
                for k, v in enumerate(LATENT):
                    lg, l1mg = log_g(k)
                    lp = beliefs[v]
                    reported = tr['rep_band'][v][idx][srv] > 0
                    ll = torch.where(reported, torch.logsumexp(lp + lg, 1), torch.logsumexp(lp + l1mg, 1))
                    loss = loss - (ws * ll).sum() / ws.sum()
            opt.zero_grad()
            loss.backward()
            opt.step()
            sched.step()
            tot += loss.item() * len(idx)
        if learn and fill and epoch >= FILL_FROM_EPOCH:
            update_factor()
        model.eval()
        with torch.no_grad():
            val = float(np.mean([M.masked_loss(model, te['values'], te['weight'], h, None,
                                               te['unknown']).item() for h in val_hidden]))
        row = dict(epoch=epoch, train_loss=tot / n, val_loss=val, seconds=time.time() - t0)
        for k, v in enumerate(LATENT):
            row[f'alpha_{v}'], row[f'beta_{v}'] = float(alpha[k] - beta[k] * LN_CENTRE), float(beta[k])
            row[f'factor_{v}'] = factor[v]
        log.append(row)
        print(' '.join(f'{k} {v:.4f}' if isinstance(v, float) else f'{k} {v}' for k, v in row.items()),
              flush=True)
    settings = {v: {'alpha': float(alpha[k] - beta[k] * LN_CENTRE), 'beta': float(beta[k]), 'factor': factor[v]}
                for k, v in enumerate(LATENT)}
    diag = fill_diagnostics(M, model, tr, table[table.split == 'train'], log_g, jl, device) if fill else None
    return model, pd.DataFrame(log), settings, diag


def fill_diagnostics(M, model, tr, part, log_g, jl, device):
    """Among the survey's reported zeros: the model's chance that each is a
    hidden holder, against the truth (share and ranking quality)."""
    import torch
    out = {}
    C = len(M.CONTEXT)
    srv = tr['srv']
    w = tr['weight']
    model.eval()
    for k, v in enumerate(LATENT):
        zero = srv & (tr['rep_band'][v] == 0)
        idx = zero.nonzero().squeeze(1)
        truth = torch.tensor(part[f'{v}_true'].to_numpy() > 0, device=device)[idx]
        probs = []
        with torch.no_grad():
            for i in range(0, len(idx), 20_000):
                ii = idx[i:i + 20_000]
                hid = torch.zeros(len(ii), len(M.PREDICTED), dtype=torch.bool, device=device)
                hid[:, jl[v]] = True
                lp = model(tr['values'][ii], hid)[:, jl[v], :M.N_CHOICES[v]]
                _, l1mg = log_g(k)
                q = (lp + l1mg).softmax(1)
                probs.append(1 - q[:, 0])
        p = torch.cat(probs).cpu().numpy()
        t = truth.cpu().numpy()
        ww = w[idx].cpu().numpy()
        out[v] = {'hidden holder share, model (%)': round(100 * wmean(p, ww), 2),
                  'hidden holder share, truth (%)': round(100 * wmean(t, ww), 2),
                  'ranking quality (AUC)': round(auc(p, t, ww), 4)}
    return out


def auc(score, truth, w):
    order = np.argsort(score, kind='mergesort')
    s, t, w = score[order], truth[order], w[order]
    pos_w, neg_w = w * t, w * (1 - t)
    # weighted Mann-Whitney with ties split
    df = pd.DataFrame({'s': s, 'p': pos_w, 'n': neg_w}).groupby('s').sum()
    neg_below = df['n'].cumsum() - df['n']
    return float(((df['p'] * (neg_below + 0.5 * df['n'])).sum()) / (df['p'].sum() * df['n'].sum()))


def score(M, model, table, aux, labelled, device):
    """Other income for every half-A return given its true married, dependents
    and four incomes."""
    import torch
    A = table[table.source == 'puf'].reset_index(drop=True)
    d = M.encode(A, device)
    vals = d['values'].clone()
    C = len(M.CONTEXT)
    j = M.PREDICTED.index('net_worth')
    vals[:, 0] = M.SOURCES.index('scf') if labelled else -1
    vals[:, C + j] = 0
    hid = torch.zeros(len(A), len(M.PREDICTED), dtype=torch.bool, device=device)
    hid[:, j] = True
    model.eval()
    lps = []
    with torch.no_grad():
        for i in range(0, len(A), 50_000):
            lps.append(model(vals[i:i + 50_000], hid[i:i + 50_000])[:, j, :M.N_CHOICES['net_worth']])
    lp = torch.cat(lps).cpu().numpy().astype(np.float64)
    truth_band = A['other_true_band'].to_numpy()
    w = A['weight'].to_numpy()
    nll = -lp[np.arange(len(A)), truth_band]
    p = np.exp(lp)
    expected = p @ np.array(aux['other_band_dollars'])
    any_other = 1 - p[:, 0]
    truth = A['other_true'].to_numpy()
    # the same with every amount held between the survey's weighted 1st and
    # 99th percentiles, so a few huge amounts do not swing the comparison
    srv = table[table.source == 'scf']
    clip_lo, clip_hi = wquantile(srv['net_worth'].to_numpy(), srv['weight'].to_numpy(), [0.01, 0.99])
    clipped = srv['net_worth'].clip(clip_lo, clip_hi).to_numpy()
    nb = srv['net_worth_band'].to_numpy()
    m_clip = np.array([wmean(clipped[nb == k], srv['weight'].to_numpy()[nb == k]) if (nb == k).any() else 0.0
                       for k in range(lp.shape[1])])
    expected_clip = p @ m_clip
    truth_clip = np.clip(truth, clip_lo, clip_hi)
    groups = {}

    def add(name, m):
        groups[name] = {'returns (weighted, millions)': float(w[m].sum() / 1e6),
                        'truth': wmean(truth[m], w[m]), 'model': wmean(expected[m], w[m]),
                        'truth, held 1st-99th pct': wmean(truth_clip[m], w[m]),
                        'model, held 1st-99th pct': wmean(expected_clip[m], w[m]),
                        'share with any, truth (%)': 100 * wmean(truth[m] != 0, w[m]),
                        'share with any, model (%)': 100 * wmean(any_other[m], w[m]),
                        'log score': wmean(nll[m], w[m])}
    add('all', np.ones(len(A), bool))
    for v in LATENT:
        x = A[f'{v}_true'].to_numpy()
        add(f'{v}: none', x == 0)
        add(f'{v}: $1-499', (x > 0) & (x < 500))
        add(f'{v}: $500-4,999', (x >= 500) & (x < 5000))
        add(f'{v}: $5,000+', x >= 5000)
    both0 = (A['interest_true'].to_numpy() == 0) & (A['dividends_true'].to_numpy() == 0)
    add('interest and dividends both none', both0)
    wg = A['wages'].to_numpy()
    for name, lo, hi in [('wages 0', 0, 1), ('wages 1-25k', 1, 25e3), ('wages 25-50k', 25e3, 50e3),
                         ('wages 50-100k', 50e3, 100e3), ('wages 100-200k', 100e3, 200e3),
                         ('wages 200k+', 200e3, np.inf)]:
        add(name, (wg >= lo) & (wg < hi))
    return {'log_score': wmean(nll, w), 'groups': groups, 'clip': [float(clip_lo), float(clip_hi)]}


def run(arm, D, seed):
    import torch
    assert arm in ARMS and D in DISTORTIONS
    assert (arm == 'oracle') == (D == 'D0'), 'oracle runs on D0, every other arm on D1-D4'
    seed = int(seed)
    rule = 'rank' if arm == 'rank' else 'dollar'
    tag = f'F22_{D}_{rule}'
    folder = f'{OUT}/{arm}_{D}_s{seed}'
    if os.path.exists(f'{folder}/score.json'):
        print(f'{folder}: done already', flush=True)
        return
    os.makedirs(folder, exist_ok=True)
    M = load_model_module(tag, folder)
    aux = json.load(open(f'{OUT}/aux_{D}.json'))
    max_epochs = int(os.environ.get('MINIMAL_MAX_EPOCHS', '80'))
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    table = M.read_table()
    t0 = time.time()
    settings = diag = None
    if arm in LABEL_FREE:
        model, log, settings, diag = train_latent(M, table, aux, arm, seed, max_epochs, device)
    else:
        if arm == 'prefill':
            table = prefill(table, aux, seed)
        M.SEED = seed
        data = {s: M.encode(table[table.split == s], device) for s in ['train', 'test']}
        model, log = M.train(data['train'], data['test'], max_epochs, keep_last=True)
    log.to_csv(f'{folder}/train_log.csv', index=False)
    torch.save({'state': model.state_dict(), 'settings': model.settings, 'band_layout': M.BAND_LAYOUT},
               f'{folder}/model.pt')
    sc = score(M, model, table, aux, arm not in LABEL_FREE, device)
    sc.update(arm=arm, D=D, seed=seed, minutes=(time.time() - t0) / 60, epochs=max_epochs,
              learned_settings=settings, planted=aux['planted'], fill_diagnostics=diag,
              last_val_loss=float(log['val_loss'].iloc[-1]))
    json.dump(sc, open(f'{folder}/score.json', 'w'), indent=1)
    print(json.dumps({k: sc[k] for k in ['arm', 'D', 'seed', 'log_score', 'learned_settings',
                                          'fill_diagnostics', 'minutes']}, indent=1), flush=True)


def rescore():
    """Score every saved model again with the current score() (no training);
    the rest of score.json is kept."""
    import glob
    import torch
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    for f in sorted(glob.glob(f'{OUT}/*/score.json')):
        old = json.load(open(f))
        arm, D = old['arm'], old['D']
        tag = f"F22_{D}_{'rank' if arm == 'rank' else 'dollar'}"
        folder = os.path.dirname(f)
        M = load_model_module(tag, folder)
        aux = json.load(open(f'{OUT}/aux_{D}.json'))
        table = M.read_table()
        model = M.load_model(device, folder)
        sc = score(M, model, table, aux, arm not in LABEL_FREE, device)
        old.update(sc)
        json.dump(old, open(f, 'w'), indent=1)
        print(f'rescored {folder}', flush=True)


if __name__ == '__main__':
    what = sys.argv[1]
    if what == 'build':
        build()
    elif what == 'run':
        run(*sys.argv[2:5])
    elif what == 'rescore':
        rescore()
    elif what == 'report':
        spec = importlib.util.spec_from_file_location('rep22', f'{HERE}/22_report.py')
        rep = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(rep)
        rep.main()
    else:
        raise SystemExit(f'unknown step {what}')
