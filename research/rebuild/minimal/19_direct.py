"""
19_direct.py  (minimal example, step 19: post-training by changing the network itself)

What it does
  The v4b baseline post-trains by reweighting: 400,000 records drawn from the
  pretrained model V4 get weights exp(lambda . f(record)) that hit the 136
  IRS targets for 2019 exactly and the eight wealth targets softly, with the
  smallest relative entropy (KL) from the starting weights. This file instead
  changes the parameters of a copy of V4, so that the copy's own draws hit
  the same targets, while staying as close as possible to V4:

          minimize over the parameters theta
          KL(q_theta || p)  +  a penalty on each target's miss,

  where p is V4 and q_theta the adjusted copy. This is the "direct gradient" and "policy
  gradient" rung of the fallback ladder (design/plan.md, decision 8).

How a record's probability is computed
  Both p and q_theta are evaluated in one fixed fill order: wages, interest,
  dividends, capital gains, other_agi (block 1, in this fixed order), then
  net worth (block 2), then married, then dependents (block 3). The record's
  probability is the product of the eight conditionals in that order. Net
  worth is drawn and scored as decision 16 says: under the SCF label with
  other_agi left out; the other seven slots under the PUF label with
  everything already drawn visible. This is exactly what 03_generate.generate
  does with fixed_order=True and borrowed={'label':'owner','foreign':'absent'}.

How the targets are estimated and differentiated
  Each target is an expectation over the model's draws: target k is
  R * E_q[F_k(record)], R the 2019 return count, F_k the record's
  contribution (1/0 for a count, dollars for an amount; dollars drawn from
  the rank-to-dollar map, as in the tilt). We keep a sample of records drawn
  from a proposal s (the current model, with the top 1% bands of wages,
  capital gains, other_agi and net worth drawn five times as often), and
  estimate every expectation under q_theta by self-normalized importance
  weighting: weight_i proportional to q_theta(x_i) / s(x_i). Because
  q_theta(x_i) is a product of the network's conditionals, it is
  differentiable in theta. The derivative of the objective with respect to
  log q_theta(x_i) is

      c_i = weight_i * (r_i - average r),  r_i = log q_theta(x_i)/p(x_i) + pi . X_i,

  X_i the record's contributions in units of each target, pi_k the
  derivative of the penalty with respect to target k's miss (plus its
  multiplier, in the versions that have one). Following c_i through the network is the policy gradient
  (score-function) estimator with a baseline, evaluated on a sample that is
  refreshed from the current model at the start of each round.

  Three versions of the objective were run (D19_METHOD; 19_DIRECT.md):
    warm     multipliers only, started at the exact tilt's multipliers on the
             first sample and corrected by one Newton step of the tilt's dual
             problem per round. Failed: the tilt sets 0.6% of V4's
             probability to essentially zero (log factor about -429), and with
             those multipliers fixed the network chases that far past
             anything needed, moving 0.19 nats and missing the wealth targets
             by up to 58%.
    penalty  every multiplier starts at zero; a quadratic penalty on each
             relative miss (1% on the exact targets, 2% on the soft ones),
             and for the exact targets the augmented Lagrangian update
             mu += miss / 1%^2 after each round. Failed: the small and
             zero-AGI amount cells have sampling errors of tens of percent
             even in 400,000 records, so the multipliers chased noise and
             grew without bound.
    z        (default for the reported runs) penalty only: half the squared
             miss of each exact target in units of its standard error in an
             equal-weight file of 200,000 records (D19_SE_FILE), the yardstick
             of v4b's redraw; the soft targets keep 2%. No multipliers.
  Every update is from the full sample, and kept only if it lowers the
  objective on that sample (otherwise undone, learning rate halved).

Usage (through Slurm, 19_direct.sbatch and 19_cpu.sbatch), from the repo root:
  python 19_direct.py test             sanity checks and timing
  python 19_direct.py train <tag>      adjust the network; draw and save the files
  python 19_direct.py pretrain         V4's own fixed-order draws and the order check
  (scoring against v4b is 19_score.py, on the CPU)
Settings for train (environment):
  D19_PARAMS   all (default) or last: train every parameter, or only the last
               encoder layer, the final normalization and the output heads
  D19_LR       starting Adam learning rate (default 2e-5); D19_LR_MAX its cap
               (default 1e-4). Adam is the usual adaptive gradient method; the
               learning rate is the size of each parameter change.
  D19_ROUNDS   rounds (default 5), each with a fresh sample from the current
               model; D19_STEPS parameter updates per round (default 15). An
               update is kept only if it lowers the objective on the round's
               sample; otherwise it is undone and the learning rate halved.
  D19_N        proposal sample size (default 400,000)
  D19_ETA      damping of the multiplier step (default 1.0)

Outputs under /nfs/roberts/scratch/pi_nrs36/jar335/minimal/:
  model/G19/<tag>/model.pt      the adjusted network (same format as model/V4)
  tilt/G19/<tag>/train_log.csv  one row per update; rounds.csv per round
  gen/G19/<tag>/final_fixed.parquet   fresh equal-weight draws, fixed order
  gen/G19/<tag>/final_random.parquet  fresh draws, random order inside block 1
  gen/G19/<tag>/logratio_final.parquet  per record log q, log p, by step
  gen/G19/pretrain_fixed.parquet      V4's own draws, fixed order (reference)
  checks/G19/<tag>/scores.md and CSVs
"""
import copy
import importlib.util
import json
import math
import os
import sys
import time

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
os.environ.setdefault('MINIMAL_TABLE', 'E4')
os.environ.setdefault('MINIMAL_RUN', 'V4')
os.environ.setdefault('MINIMAL_ALL_RECORDS', '1')
os.environ.setdefault('MINIMAL_MAP_DIR', f'{ROOT}/map/V4')

import numpy as np
import pandas as pd
import torch

HERE = os.path.dirname(os.path.abspath(__file__))


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


g = load('gen03', '03_generate.py')
m = g.m
base = load('base07', '07_baseline.py')
wt, dollars, tiltmod = base.wt, base.dollars, base.tiltmod

NC = len(m.CONTEXT)
P = len(m.PREDICTED)
PUF, SCF = m.SOURCES.index('puf'), m.SOURCES.index('scf')
YEAR = m.YEARS.index(2015)
ORDER = ['wages', 'interest', 'dividends', 'capital_gains', 'other_agi', 'net_worth', 'married', 'n_dep']
ORDER_REV = ['other_agi', 'capital_gains', 'dividends', 'interest', 'wages', 'net_worth', 'married', 'n_dep']
BORROWED = {'label': 'owner', 'foreign': 'absent'}     # decision 16
BOOST_VARS, BOOST = ['wages', 'capital_gains', 'other_agi', 'net_worth'], 5.0
SIGMA = wt.SIGMA                                       # 2%, as the tilt
OUT_MODEL, OUT_TILT = f'{ROOT}/model/G19', f'{ROOT}/tilt/G19'
OUT_GEN, OUT_CHECK = f'{ROOT}/gen/G19', f'{ROOT}/checks/G19'
DEVICE = 'cuda' if torch.cuda.is_available() else 'cpu'


def idx(order):
    return [m.PREDICTED.index(v) for v in order]


# ---------------------------------------------------------------- probabilities

def step_views(values, order):
    """The eight views of a complete record, one per step of the fill order:
    the slots at or after the step hidden; at the net worth step the SCF label
    and other_agi absent (decision 16). Returns stacked values and hidden."""
    n, oi = len(values), idx(order)
    pos = torch.empty(P, dtype=torch.long)
    pos[oi] = torch.arange(P)
    oa = m.PREDICTED.index('other_agi')
    views, hiddens = [], []
    for k, j in enumerate(oi):
        v = values.clone()
        h = (pos >= k).to(values.device)[None].expand(n, P).clone()
        if m.PREDICTED[j] == 'net_worth':
            v[:, 0] = SCF
            v[:, NC + oa] = -1
            h[:, oa] = False
        views.append(v)
        hiddens.append(h)
    return torch.cat(views), torch.cat(hiddens)


def log_prob_steps(model, values, order=ORDER):
    """[n, 8] log-probability of each slot given the earlier ones, in `order`
    (columns in that order). Differentiable when called with grad enabled."""
    n, oi = len(values), idx(order)
    V, H = step_views(values, order)
    lp = m.predict(model, V, H)
    rows = torch.arange(len(V), device=values.device)
    slots = torch.tensor(oi, device=values.device).repeat_interleave(n)
    tgt = values[:, NC:].repeat(len(oi), 1)[rows, slots]
    return lp[rows, slots, tgt].view(len(oi), n).T


@torch.no_grad()
def log_prob(model, values, order=ORDER, chunk=40_000, steps=False):
    """Whole-record log-probability (float64, on the CPU), in chunks."""
    out = []
    for s in range(0, len(values), chunk):
        v = values[s:s + chunk].to(DEVICE)
        lp = log_prob_steps(model, v, order).double()
        out.append((lp if steps else lp.sum(1)).cpu())
    return torch.cat(out)


@torch.no_grad()
def sample(model, n, seed, boost=True, chunk=50_000):
    """Fixed-order draws of complete PUF records with net worth (decision 16).
    boost: the top 1% bands of BOOST_VARS drawn BOOST times as often. Returns
    values [n, slots] (CPU), log q (the model's own probability) and log s
    (the sampling probability)."""
    gen = torch.Generator(device=DEVICE).manual_seed(seed)
    oi = idx(ORDER)
    oa, nw = m.PREDICTED.index('other_agi'), m.PREDICTED.index('net_worth')
    top = {m.PREDICTED.index(v): torch.as_tensor(m.positive_bands(v, 0.99, 1.0), device=DEVICE)
           for v in BOOST_VARS} if boost else {}
    parts = []
    for start in range(0, n, chunk):
        size = min(chunk, n - start)
        rows = torch.arange(size, device=DEVICE)
        values = torch.zeros(size, len(m.SLOTS), dtype=torch.long, device=DEVICE)
        values[:, 0], values[:, 1] = PUF, YEAR
        filled = torch.zeros(size, P, dtype=torch.bool, device=DEVICE)
        lq = torch.zeros(size, dtype=torch.float64, device=DEVICE)
        ls = torch.zeros_like(lq)
        for j in oi:
            view, hidden = values, ~filled
            if j == nw:
                view = values.clone()
                view[:, 0], view[:, NC + oa] = SCF, -1
                hidden = hidden.clone()
                hidden[:, oa] = False
            probs = m.predict(model, view, hidden)[:, j].exp()
            s = probs
            if j in top:
                s = probs.clone()
                s[:, top[j]] *= BOOST
                s = s / s.sum(1, keepdim=True)
            draw = torch.multinomial(s, 1, generator=gen).squeeze(1)
            lq += probs[rows, draw].double().log()
            ls += s[rows, draw].double().log()
            values[:, NC + j] = draw
            filled[:, j] = True
        parts.append((values.cpu(), lq.cpu(), ls.cpu()))
    return (torch.cat([p[0] for p in parts]), torch.cat([p[1] for p in parts]),
            torch.cat([p[2] for p in parts]))


def table_to_values(df):
    """Band table (03_generate.to_table columns) to the value tensor."""
    cols = [df['source'].map(m.SOURCES.index), df['year'].map(m.YEARS.index),
            df['married'], df['n_dep']] + [df[a + '_band'] for a in m.AMOUNTS]
    return torch.tensor(np.column_stack([np.asarray(c, dtype=np.int64) for c in cols]))


# ---------------------------------------------------------------- targets

class Targets:
    """The 136 IRS cells and the eight wealth targets (threshold form, all
    2019 SCF units), as in tilt/v4b_allunits."""

    def __init__(self):
        self.brackets, self.cells = tiltmod.target_set('brackets')
        self.R = float(tiltmod.target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0])
        self.wealth, self.threshold = wt.wealth_targets('all')
        self.maps19, self.maps15 = dollars.puf_maps(2019), dollars.puf_maps(2015)
        dummy = np.zeros(10)
        _, Tq = base.threshold_form(dummy, self.R, self.wealth, self.threshold, 'all')
        self.T = np.r_[self.cells['value'].to_numpy(float), Tq]
        assert (self.T != 0).all()
        self.n_exact = len(self.cells)
        self.soft = np.r_[np.zeros(self.n_exact), np.ones(len(Tq))].astype(bool)
        self.names = list(self.cells['bracket'] + ' / ' + self.cells['cell']) + [
            'units above SCF p99', 'net worth above SCF p99', 'units above SCF p90',
            'net worth above SCF p90', 'units above SCF p50', 'net worth above SCF p50',
            'total net worth', 'units above $1m']

    def dollars(self, values, seed=dollars.SEED):
        """2019-dollar and 2015-dollar tables of a value tensor (same draw)."""
        df = g.to_table(values)
        df['record_id'] = np.arange(len(df))
        g19, _ = dollars.draw_dollars(df, self.maps19, seed=seed)
        g15, _ = dollars.draw_dollars(df, self.maps15, seed=seed)
        return g19, g15

    def F(self, g19):
        """Records-by-144 contributions (dollars or 0/1)."""
        x = g19['net_worth'].to_numpy(float)
        Fq, _ = base.threshold_form(x, self.R, self.wealth, self.threshold, 'all')
        return np.column_stack([tiltmod.contributions(g19, self.brackets), Fq])

    def X(self, F):
        """Contributions in units of each target: E_q[X] = 1 on target."""
        return self.R * F / self.T


def tilt_lambda(w, F, T, rel_tol, max_iter=200):
    """05_tilt.tilt, copied to return the multipliers as well (05 returns
    only the weights). Same algorithm and stopping rule."""
    W = w.sum()
    p = w / W
    X = F * (W / np.abs(T))
    t = np.sign(T)
    s2 = np.asarray(rel_tol, float) ** 2
    lam = np.zeros(len(T))

    def dual(lam):
        e = p * np.exp(X @ lam)
        return e.sum() - lam @ t + 0.5 * (s2 * lam ** 2).sum(), e
    D, e = dual(lam)
    it = 0
    for it in range(max_iter):
        gr = X.T @ e - t + s2 * lam
        if np.abs(gr).max() < 1e-11:
            break
        H = X.T @ (X * e[:, None]) + np.diag(s2)
        step = np.linalg.solve(H + 1e-12 * np.eye(len(T)), -gr)
        a = 1.0
        while True:
            D_new, e_new = dual(lam + a * step)
            if np.isfinite(D_new) and D_new <= D + 1e-4 * a * (gr @ step):
                break
            a /= 2
            if a < 1e-12:
                return W * e, lam, False
        lam, D, e = lam + a * step, D_new, e_new
    return W * e, lam, bool(np.abs(X.T @ e - t + s2 * lam).max() < 1e-7)


# ---------------------------------------------------------------- the objective

def estimates(lq, ls, lp, X, mu, s2, inv_tau2=None):
    """Self-normalized importance estimates under q_theta and the policy
    gradient coefficients c_i (torch float64 on DEVICE).
    Objective J = KL + sum_k [mu_k miss_k + inv_tau2_k miss_k^2 / 2]; its
    derivative in miss_k is pi_k = mu_k + inv_tau2_k miss_k, and
    c_i = w_i (r_i - average r), r_i = log q/p + pi . X_i. inv_tau2 None:
    no quadratic part (the first design, multipliers only)."""
    a = lq - ls
    w = torch.softmax(a, 0)
    d = lq - lp
    EX = w @ X
    miss = EX - 1
    pi = mu if inv_tau2 is None else mu + inv_tau2 * miss
    r = d + X @ pi
    c = w * (r - w @ r)
    kl = float(w @ d)
    ess = float(1 / (w ** 2).sum())
    soft = s2 > 0
    primal = kl + float(((miss[soft] ** 2) / (2 * s2[soft])).sum())
    J = None if inv_tau2 is None else kl + float(mu @ miss) + 0.5 * float((inv_tau2 * miss ** 2).sum())
    return dict(w=w, c=c, miss=miss, kl=kl, ess=ess, primal=primal, J=J)


def newton_mu(w, X, miss, mu, s2, eta):
    """One damped Newton step of the dual: mu += eta (Cov + S)^-1 (miss - S mu)."""
    Xc = X - w @ X
    C = Xc.T @ (Xc * w[:, None])
    rhs = miss - s2 * mu
    A = C + torch.diag(s2) + 1e-10 * torch.eye(len(mu), device=X.device, dtype=X.dtype)
    step = torch.linalg.lstsq(A, rhs[:, None]).solution.squeeze(1)
    return mu + eta * step


def trainable(model, which):
    for p_ in model.parameters():
        p_.requires_grad = which == 'all'
    if which == 'last':
        for mod in [model.encoder.layers[-1], model.final_norm, model.heads]:
            for p_ in mod.parameters():
                p_.requires_grad = True
    return [p_ for p_ in model.parameters() if p_.requires_grad]


def proposal(model, ref, tg, n, seed):
    """A fresh boosted sample from `model`: values, log s, log p (V4), X."""
    t0 = time.time()
    values, lq_s, ls = sample(model, n, seed, boost=True)
    lp = log_prob(ref, values)
    g19, _ = tg.dollars(values)
    F = tg.F(g19)
    X = torch.tensor(tg.X(F), dtype=torch.float64, device=DEVICE)
    print(f'  proposal of {n:,} drawn in {time.time() - t0:.0f}s', flush=True)
    return values, lq_s, ls, lp, F, X


def snapshot(model, opt):
    return copy.deepcopy(model.state_dict()), copy.deepcopy(opt.state_dict())


def restore(model, opt, snap):
    model.load_state_dict(snap[0])
    opt.load_state_dict(snap[1])


def lagrangian(e, mu, s2):
    """The objective theta minimizes at fixed multipliers:
    KL + mu . miss - (1/2) sum_soft sigma^2 mu^2 (the last term does not
    depend on theta; kept so the value equals the primal at the optimum)."""
    return e['kl'] + float(mu @ e['miss']) - 0.5 * float((s2 * mu ** 2).sum())


def gradient_step(model, opt, values, c, chunk):
    """One update from the full sample: gradient of sum_i c_i log q(x_i)."""
    opt.zero_grad()
    for s in range(0, len(values), chunk):
        v = values[s:s + chunk].to(DEVICE)
        (c[s:s + chunk] * log_prob_steps(model, v).sum(1)).sum().backward()
    opt.step()


def train(tag):
    """Rounds: a fresh proposal from the current model; a multiplier step
    (round 1: the exact tilt on that sample); then `steps` parameter updates,
    each from the full sample with fresh coefficients. An update is kept only
    if it lowers the Lagrangian at the round's multipliers (estimated on the
    round's sample); otherwise it is undone and the learning rate halved. A
    kept update raises the learning rate by a quarter, up to D19_LR_MAX."""
    which = os.environ.get('D19_PARAMS', 'all')
    lr = float(os.environ.get('D19_LR', '2e-5'))
    lr_max = float(os.environ.get('D19_LR_MAX', '1e-4'))
    rounds = int(os.environ.get('D19_ROUNDS', '5'))
    steps = int(os.environ.get('D19_STEPS', '15'))
    n = int(os.environ.get('D19_N', '400000'))
    eta = float(os.environ.get('D19_ETA', '1.0'))
    chunk = int(os.environ.get('D19_CHUNK', '12500'))
    method = os.environ.get('D19_METHOD', 'z')
    assert method in ('penalty', 'warm', 'z'), method
    tau = float(os.environ.get('D19_TAU', '0.01'))
    settings = dict(tag=tag, params=which, method=method, tau_exact=tau, lr_start=lr, lr_max=lr_max, rounds=rounds, steps=steps,
                    n=n, eta=eta, boost=BOOST, boost_vars=BOOST_VARS, order=ORDER,
                    borrowed=BORROWED, sigma_soft=SIGMA, device=DEVICE,
                    gpu=torch.cuda.get_device_name() if DEVICE == 'cuda' else None)
    print(json.dumps(settings), flush=True)
    for d in [f'{OUT_MODEL}/{tag}', f'{OUT_TILT}/{tag}', f'{OUT_GEN}/{tag}']:
        os.makedirs(d, exist_ok=True)
    torch.manual_seed(m.SEED)
    ref = m.load_model(DEVICE)
    for p_ in ref.parameters():
        p_.requires_grad = False
    model = copy.deepcopy(ref).eval()
    params = trainable(model, which)
    print(f'trainable parameters: {sum(p_.numel() for p_ in params):,} of '
          f'{sum(p_.numel() for p_ in model.parameters()):,}', flush=True)
    opt = torch.optim.Adam(params, lr=lr)
    tg = Targets()
    s2 = torch.tensor(np.where(tg.soft, SIGMA ** 2, 0.0), device=DEVICE)
    # penalty: the quadratic part, 1/sigma^2 on the soft targets, 1/tau^2 on the exact
    inv_tau2 = None if method == 'warm' else torch.tensor(
        np.where(tg.soft, 1 / SIGMA ** 2, 1 / tau ** 2), device=DEVICE)
    soft_d = torch.tensor(tg.soft, device=DEVICE)
    se_file = int(os.environ.get('D19_SE_FILE', '200000'))
    began = time.time()
    log, rlog = [], []
    mu = None
    soft_keys = ['u99', 'nw99', 'u90', 'nw90', 'u50', 'nw50', 'total', 'u1m']
    for rnd in range(1, rounds + 1):
        values, lq_s, ls, lp, F, X = proposal(model, ref, tg, n, seed=1000 + rnd)
        ls_d, lp_d = ls.to(DEVICE), lp.to(DEVICE)
        if rnd == 1:
            # warm start: the exact tilt on this sample, from V4's weights p/s
            w0 = np.exp((lp - ls).numpy())
            w0 *= tg.R / w0.sum()
            t0 = time.time()
            w_star, lam, ok = tilt_lambda(w0, F, tg.T, np.where(tg.soft, SIGMA, 0.0))
            mv = tiltmod.move(w0, w_star)
            gap = 100 * (F.T @ w_star / tg.T - 1)
            worst = np.argsort(-np.abs(gap[:tg.n_exact]))[:5]
            print(f'  exact tilt on the first sample: converged {ok}, KL {mv["kl_nats"]:.5f}, '
                  f'ESS kept {mv["ess_ratio"]:.4f}, {time.time() - t0:.0f}s; largest exact gaps: ' +
                  '; '.join(f'{tg.names[k]} {gap[k]:+.4f}%' for k in worst), flush=True)
            mu = torch.tensor(-np.sign(tg.T) * lam, device=DEVICE) if method == 'warm' else \
                torch.zeros(len(tg.T), dtype=torch.float64, device=DEVICE)
            np.save(f'{OUT_TILT}/{tag}/round1_tilt_weights.npy', np.c_[w0, w_star])
            torch.save({'values': values, 'log_s': ls, 'log_p': lp}, f'{OUT_TILT}/{tag}/round1_sample.pt')
            np.save(f'{OUT_TILT}/{tag}/round1_F.npy', F)
            if method == 'z':
                # each exact target's tolerance: its relative standard error in an
                # equal-weight file of se_file records drawn from V4 (estimated on
                # this sample with V4's weights p/s); the soft ones keep sigma
                wv = torch.softmax(lp_d - ls_d, 0)
                var = wv @ X ** 2 - (wv @ X) ** 2
                tau_k = torch.sqrt(var / se_file).clamp(min=float(os.environ.get('D19_TAU_FLOOR', '0.001')))
                inv_tau2 = torch.where(soft_d, torch.full_like(tau_k, 1 / SIGMA ** 2), 1 / tau_k ** 2)
                pd.DataFrame({'target': tg.names, 'tau_relative': tau_k.cpu().numpy(), 'soft': tg.soft}).to_csv(
                    f'{OUT_TILT}/{tag}/tolerances.csv', index=False)
                print('  tolerances (exact targets, relative): median', float(tau_k[~soft_d].median()),
                      'min', float(tau_k[~soft_d].min()), 'max', float(tau_k[~soft_d].max()), flush=True)
            rlog_extra = dict(tilt_kl=mv['kl_nats'], tilt_ess_ratio=mv['ess_ratio'], tilt_converged=ok,
                              tilt_max_abs_gap_exact_pct=float(np.abs(gap[:tg.n_exact]).max()))
            lq = lp.to(DEVICE)
        else:
            lq = log_prob(model, values).to(DEVICE)
            e = estimates(lq, ls_d, lp_d, X, mu, s2, inv_tau2)
            miss = e['miss'].cpu().numpy()
            worst = np.argsort(-np.abs(miss[:tg.n_exact]))[:5]
            print('  round start, largest exact misses: ' +
                  '; '.join(f'{tg.names[k]} {100 * miss[k]:+.2f}%' for k in worst), flush=True)
            mu_old = mu
            if method == 'warm':
                mu = newton_mu(e['w'], X, e['miss'], mu, s2, eta)
            elif method == 'z':
                pass    # penalty only: no multipliers
            else:   # augmented Lagrangian: exact targets mu += miss / tau^2; soft stay 0
                mu = torch.where(soft_d, mu, mu + eta * inv_tau2 * e['miss'])
            rlog_extra = dict(mu_change=float((mu - mu_old).abs().max()),
                              start_max_abs_miss_exact_pct=float(100 * np.abs(miss[:tg.n_exact]).max()),
                              start_kl_is=e['kl'])
        e = estimates(lq, ls_d, lp_d, X, mu, s2, inv_tau2)
        L = lagrangian(e, mu, s2) if method == 'warm' else e['J']
        snap = snapshot(model, opt)
        for st in range(1, steps + 1):
            t0 = time.time()
            for grp in opt.param_groups:
                grp['lr'] = lr
            gradient_step(model, opt, values, e['c'].float(), chunk)
            lq_new = log_prob(model, values).to(DEVICE)
            e_new = estimates(lq_new, ls_d, lp_d, X, mu, s2, inv_tau2)
            L_new = lagrangian(e_new, mu, s2) if method == 'warm' else e_new['J']
            kept = L_new <= L
            if kept:
                e, L, snap = e_new, L_new, snapshot(model, opt)
                lr = min(lr * 1.25, lr_max)
            else:
                restore(model, opt, snap)
                lr *= 0.5
            miss = e['miss'].cpu().numpy()
            row = dict(round=rnd, step=st, kept=kept, lr_next=lr, lagrangian=L, kl_is=e['kl'],
                       primal=e['primal'], ess_share=e['ess'] / n,
                       max_abs_miss_exact_pct=100 * np.abs(miss[:tg.n_exact]).max(),
                       rms_miss_exact_pct=100 * np.sqrt((miss[:tg.n_exact] ** 2).mean()),
                       **{f'soft miss % {k}': 100 * miss[tg.n_exact + i] for i, k in enumerate(soft_keys)},
                       seconds=time.time() - t0, minutes=(time.time() - began) / 60)
            log.append(row)
            print({k: (round(v, 6) if isinstance(v, float) else v) for k, v in row.items()}, flush=True)
        miss = e['miss'].cpu().numpy()
        rlog.append(dict(round=rnd, kl_is=e['kl'], lagrangian=L, primal=e['primal'], ess_share=e['ess'] / n,
                         max_abs_miss_exact_pct=100 * np.abs(miss[:tg.n_exact]).max(),
                         rms_miss_exact_pct=100 * np.sqrt((miss[:tg.n_exact] ** 2).mean()),
                         max_abs_miss_soft_pct=100 * np.abs(miss[tg.n_exact:]).max(),
                         mu_abs_max=float(mu.abs().max()), minutes=(time.time() - began) / 60,
                         **rlog_extra))
        print('ROUND', rlog[-1], flush=True)
        pd.DataFrame(log).to_csv(f'{OUT_TILT}/{tag}/train_log.csv', index=False)
        pd.DataFrame(rlog).to_csv(f'{OUT_TILT}/{tag}/rounds.csv', index=False)
    train_minutes = (time.time() - began) / 60
    torch.save({'state': model.state_dict(), 'settings': model.settings, 'band_layout': m.BAND_LAYOUT},
               f'{OUT_MODEL}/{tag}/model.pt')
    pd.DataFrame({'target': tg.names, 'T': tg.T, 'mu': mu.cpu().numpy(), 'soft': tg.soft}).to_csv(
        f'{OUT_TILT}/{tag}/multipliers.csv', index=False)

    # ---- the round-1 sample under the adjusted model (for the comparison with the tilt)
    r1 = torch.load(f'{OUT_TILT}/{tag}/round1_sample.pt')
    lq_r1 = log_prob(model, r1['values'], steps=True)
    np.save(f'{OUT_TILT}/{tag}/round1_logq_steps.npy', lq_r1.numpy())

    # ---- fresh equal-weight draws, by the official generator
    t0 = time.time()
    final = g.generate(model, int(os.environ.get('D19_FINAL_N', '400000')), 'puf', with_net_worth=True,
                       fixed_order=True, seed=71, borrowed=BORROWED)
    final.to_parquet(f'{OUT_GEN}/{tag}/final_fixed.parquet', index=False)
    gen_seconds = time.time() - t0
    rand = g.generate(model, 200_000, 'puf', with_net_worth=True, fixed_order=False, seed=72,
                      borrowed=BORROWED)
    rand.to_parquet(f'{OUT_GEN}/{tag}/final_random.parquet', index=False)
    v = table_to_values(final)
    lq_steps = log_prob(model, v, steps=True).numpy()
    lp_steps = log_prob(ref, v, steps=True).numpy()
    lq_rev = log_prob(model, v, ORDER_REV).numpy()
    lr_ = pd.DataFrame({f'logq_{k}': lq_steps[:, i] for i, k in enumerate(ORDER)})
    for i, k in enumerate(ORDER):
        lr_[f'logp_{k}'] = lp_steps[:, i]
    lr_['logq_reversed_order'] = lq_rev
    lr_.to_parquet(f'{OUT_GEN}/{tag}/logratio_final.parquet', index=False)
    d = lq_steps.sum(1) - lp_steps.sum(1)
    meta = dict(settings, train_minutes=train_minutes, final_generation_seconds=gen_seconds,
                kl_from_draws=float(d.mean()), kl_se=float(d.std() / np.sqrt(len(d))),
                kl_by_step={k: float((lq_steps[:, i] - lp_steps[:, i]).mean()) for i, k in enumerate(ORDER)},
                order_kl_adjusted=float((lq_steps.sum(1) - lq_rev).mean()),
                total_minutes=(time.time() - began) / 60)
    json.dump(meta, open(f'{OUT_TILT}/{tag}/meta.json', 'w'), indent=2)
    print(json.dumps(meta, indent=2), flush=True)


@torch.no_grad()
def pretrain_reference():
    """V4's own fixed-order draws (400,000, seed 71, as the adjusted files),
    random-order draws, and the order check: KL between V4's fixed order and
    the reversed block-1 order, from V4's draws."""
    os.makedirs(OUT_GEN, exist_ok=True)
    ref = m.load_model(DEVICE)
    out = f'{OUT_GEN}/pretrain_fixed.parquet'
    t0 = time.time()
    final = g.generate(ref, 400_000, 'puf', with_net_worth=True, fixed_order=True, seed=71,
                       borrowed=BORROWED)
    final.to_parquet(out, index=False)
    secs = time.time() - t0
    g.generate(ref, 200_000, 'puf', with_net_worth=True, fixed_order=False, seed=72,
               borrowed=BORROWED).to_parquet(f'{OUT_GEN}/pretrain_random.parquet', index=False)
    v = table_to_values(final)
    lp = log_prob(ref, v).numpy()
    lp_rev = log_prob(ref, v, ORDER_REV).numpy()
    d = lp - lp_rev
    meta = dict(generation_seconds_400k=secs, order_kl_v4=float(d.mean()),
                order_kl_v4_se=float(d.std() / np.sqrt(len(d))),
                mean_log_prob=float(lp.mean()))
    json.dump(meta, open(f'{OUT_GEN}/pretrain_meta.json', 'w'), indent=2)
    print(meta, flush=True)


def test():
    """Sanity: the sampler's log-probabilities equal the teacher-forced ones;
    without the boost, its draws match 03_generate's fixed-order draws in
    distribution; timing of one gradient pass."""
    ref = m.load_model(DEVICE)
    t0 = time.time()
    v, lq, ls = sample(ref, 50_000, seed=5, boost=True)
    t_sample = time.time() - t0
    lq2 = log_prob(ref, v)
    print('sampler vs teacher-forced log q: max abs diff', float((lq - lq2).abs().max()),
          'mean log q', float(lq.mean()), 'mean log s', float(ls.mean()), f'{t_sample:.1f}s', flush=True)
    v_nb, lq_nb, ls_nb = sample(ref, 100_000, seed=6, boost=False)
    assert torch.allclose(lq_nb, ls_nb)
    official = table_to_values(g.generate(ref, 100_000, 'puf', with_net_worth=True, fixed_order=True,
                                          seed=7, borrowed=BORROWED))
    for j, var in enumerate(m.PREDICTED):
        a = np.bincount(v_nb[:, NC + j].numpy(), minlength=m.N_CHOICES[var]) / len(v_nb)
        b = np.bincount(official[:, NC + j].numpy(), minlength=m.N_CHOICES[var]) / len(official)
        print(f'{var}: total variation between this sampler and 03_generate: {0.5 * np.abs(a - b).sum():.4f}')
    lq_off = log_prob(ref, official)
    print('mean log p, this sampler', float(lq_nb.mean()), '03_generate', float(lq_off.mean()), flush=True)
    model = copy.deepcopy(ref).eval()
    params = trainable(model, 'all')
    t0 = time.time()
    for s in range(0, 100_000, 12_500):
        x = v_nb[s:s + 12_500].to(DEVICE)
        log_prob_steps(model, x).sum().backward()
    torch.cuda.synchronize() if DEVICE == 'cuda' else None
    print(f'gradient pass over 100,000 records: {time.time() - t0:.1f}s', flush=True)
    t0 = time.time()
    log_prob(model, v_nb)
    torch.cuda.synchronize() if DEVICE == 'cuda' else None
    print(f'no-gradient pass over 100,000 records: {time.time() - t0:.1f}s', flush=True)
    tg = Targets()
    t0 = time.time()
    g19, _ = tg.dollars(v_nb)
    F = tg.F(g19)
    print(f'dollars and contributions for 100,000: {time.time() - t0:.1f}s; F {F.shape}', flush=True)
    X = tg.X(F)
    print('estimates of E[X] under V4 (plain sample): min', X.mean(0).min(), 'max', X.mean(0).max())


if __name__ == '__main__':
    what = sys.argv[1]
    if what == 'test':
        test()
    elif what == 'pretrain':
        pretrain_reference()
    elif what == 'train':
        train(sys.argv[2])
    else:
        raise SystemExit(f'unknown step {what}')
