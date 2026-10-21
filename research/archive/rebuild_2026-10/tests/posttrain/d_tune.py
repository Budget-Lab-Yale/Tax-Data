"""Test 3, piece D: method 3, tune the network's weights (SPEC.md section D).

Two variants share this file.

  distill  Weighted distillation. Fine-tune the reference network on the
           reference draws reweighted by method 1 (piece B's calibrated
           weights). Each training example is one draw of one record: the
           24 wealth tokens in the fixed generation order, given the
           record's observed incomes and demographics. A training step
           picks draws with probability proportional to B's weight, picks
           one position k in the generation order at random, shows the
           draw's first k wealth tokens, and scores the model's prediction
           of the token at position k (restricted to the allowed codes).
           Averaged over k this is the fixed-order completion likelihood of
           the whole draw divided by 24, so the model is trained on exactly
           the conditionals that define its distribution of wealth.
           Drawing examples in proportion to B's weight is, in expectation,
           the same as weighting each example's loss by it.
           Optionally a share of each step's loss is the original training
           task (random hiding of observed variables on all expB training
           records, PUF and SCF, records drawn in proportion to their
           population weight), to keep the rest of the model intact.

  direct   Direct target loss with a KL penalty (d_direct.py).

Both variants stop on the target fit of fresh draws from the tuned model,
not on training loss: every --eval-min minutes the current model (the moving
average of its weights) completes every record --eval-k times in float32,
and the fit and the KL from the reference are logged. The checkpoint kept is
the one with the best fit (if --kl-cap is given, among those with KL below
it).

Measures logged at each evaluation:
  misplaced share  sum over all target cells of |expected cell weight -
                   target| divided by twice the sum of the targets. Each
                   (wave, family, group, variable) table of coarse bins sums
                   to the group's weight in truth and in the draws, so this
                   is the share of population weight that sits in the wrong
                   coarse bin, averaged over the 2 x 264 tables (weighted by
                   their totals). Expected cell weights use each step's bin
                   probabilities in place of the drawn bin (less noise); the
                   realized version uses the drawn bins.
  KL               KL(tuned || reference) in nats per unit of population
                   weight, by the chain rule: the sum over the 24 steps of
                   the exact KL between the two models' step distributions
                   given the drawn prefix, averaged over draws.

Subcommands:
  train   fine-tune; writes checkpoint.pt, curve.csv, train.json
  draws   K fresh draws from a checkpoint (float32 log-probabilities under
          the tuned and the reference model); writes draws.npz in A's
          layout plus logp_new
  report  (d_report.py) residual calibration summary, report.json, curves

Outputs in $P/d/<variant>/ (SPEC.md).
"""

import argparse
import csv
import json
import os
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd
import torch

HERE = Path(__file__).resolve().parent
T2CODE = HERE.parent / "scf_joint"
for p_ in (str(T2CODE), str(HERE)):
    if p_ not in sys.path:
        sys.path.insert(0, p_)


def _env_from_argv():
    """Set the POSTTRAIN_* environment variables (the convention piece C
    uses) from the command line before piece A's module is imported, in
    case A reads them at import."""
    keys = {"--dataset": "POSTTRAIN_DATASET", "--ref-checkpoint": "POSTTRAIN_CHECKPOINT",
            "--a-arm": "POSTTRAIN_ARM", "--a-dir": "POSTTRAIN_ADIR"}
    argv = sys.argv
    for i, x in enumerate(argv[:-1]):
        if x in keys:
            os.environ[keys[x]] = argv[i + 1]


_env_from_argv()
import a_common as A                                                    # noqa: E402
from b_model import (draw_hidden_observed, load_checkpoint, load_meta,  # noqa: E402
                     masked_ce, save_checkpoint, slot_columns)

P = A.P
NV = A.NV
T0 = time.time()


def log(msg):
    print(f"[{time.time() - T0:7.1f}s] {msg}", flush=True)


# ---------------------------------------------------------------------------
# Setup: the reference model and its record inputs (piece A), the target
# cells and the cell lookup on the GPU.
# ---------------------------------------------------------------------------

def load_context(device, recs, arm):
    """The reference network and every record's inputs, from piece A."""
    if arm == "scf":
        return A.load_reference(device, recs=recs)
    # Arm P: PUF records presented with the SCF source tag; codes allowed by
    # the 2015 quantile functions (as test 2's joint_w1_puf_asSCF). Any
    # other unobserved slot (e.g. a missing age range) stays hidden and is
    # never drawn.
    names = {v["name"] for v in A.meta()["variables"]}
    fill = [x for x in A.WEALTH if x in names]        # as a_full.Spec.wealth
    return A.load_reference(device, recs=recs, fill=fill, as_source="SCF", decode_year=2015)


def coarse_t2b(name):
    """Token -> coarse bin of a target variable (piece A's mapping)."""
    vm = A.vmeta(name)
    if vm.get("kind") == "categorical":
        return np.arange(int(vm["vocab_size"]))
    return A._coarse(vm, A.WEALTH_POS_EDGES, A.WEALTH_POS_LABELS)[1]


class Setup:
    """Records, reference network, targets and the cell lookup on the GPU.

    The lookup is base[i, t, k] + t2b[k, token]: for record i, table t (a
    family of groups) and drawn variable k, base is the id of the cell of
    bin 0 of variable k in the record's group (-1 if the record has no group
    in that table, e.g. unknown age, or the table does not cover k), and
    t2b maps a token to its coarse bin. Two layouts are read:
      - the SCF arm (a_common): targets with `block` and `var_i`, records with
        `blk_<family>` columns; cell = block * VB + vb_start[var] + bin;
      - the full model (a_full): targets with `table`, records with
        `g_<set>_<family>` columns; cell = offset[table] + group * VB[table]
        + vb_start[table][var] + bin.
    Cells whose target is missing (NaN: no SCF group to carry a share from)
    are not targeted: they are left out of every measure and of the
    residual calibration, and their number is logged."""

    def __init__(self, device, records_path=None, max_records=None, targets_path=None,
                 arm="scf", target_set=None):
        self.device = device
        if target_set:
            import a_full as AF
            recs = pd.read_parquet(records_path) if records_path else AF.load_puf_records()
            targ = pd.read_parquet(targets_path) if targets_path else AF.load_set(target_set)
        else:
            recs = pd.read_parquet(records_path) if records_path else A.load_records()
            targ = pd.read_parquet(targets_path) if targets_path else A.load_targets()
        if max_records:
            # Dev runs only (SCF arm): a subset of records, targets recomputed on it.
            recs = recs.iloc[:max_records].reset_index(drop=True)
            targ = A.targets(recs)
        self.records = recs
        self.N = len(recs)
        self.arm = arm
        self.ref, self.ctx = load_context(device, recs, arm)
        self.meta = load_meta(A.DATA / "meta.json")
        self.sources = torch.load(A.CKPT, map_location="cpu", weights_only=False)["sources"]
        self.W = torch.as_tensor(recs["weight"].to_numpy(np.float64), device=device)
        if not torch.isfinite(self.W).all() or (self.W <= 0).any():
            raise ValueError("record weights not all positive and finite")
        self.Wsum = float(self.W.sum())
        self.fill = list(getattr(self.ctx, "fill", A.WEALTH))
        self.nv = len(self.fill)
        self.vocab = [int(A.vmeta(v)["vocab_size"]) for v in self.fill]
        maxv = max(self.vocab)
        t2b = np.full((self.nv, maxv), -1, dtype=np.int64)
        for k, v in enumerate(self.fill):
            m_ = coarse_t2b(v)
            t2b[k, :len(m_)] = m_
        self.t2b = torch.as_tensor(t2b, device=device)
        self.n_cells = len(targ)
        if (targ["cell"].to_numpy() != np.arange(self.n_cells)).any():
            raise ValueError("targets table is not in cell order")
        if "block" in targ.columns:
            L = A.layout()
            fams = [f for f in A.FAMILIES if f in set(targ["family"].astype(str))]
            vbs = np.array([L.vb_start[A.WEALTH.index(v)] for v in self.fill])
            blk = recs[["blk_" + f for f in fams]].to_numpy(np.int64)          # [N, nt]
            base = np.where(blk[:, :, None] >= 0, blk[:, :, None] * L.VB + vbs[None, None, :], -1)
            tab_key = targ["block"].to_numpy() * 1000 + targ["var_i"].to_numpy()
        else:
            import a_full as AF
            pilot = "pilot" in str(A.PA)
            ts = AF.rebuild_set(target_set, pilot)
            if ts.n_cells != self.n_cells:
                raise ValueError(f"target set {target_set}: {ts.n_cells} cells, table has {self.n_cells}")
            fams = [fam.name for fam, _ in ts.tables]
            base = np.full((self.N, len(ts.tables), self.nv), -1, dtype=np.int64)
            for ti, (fam, vs) in enumerate(ts.tables):
                g = recs[f"g_{target_set}_{fam.name}"].to_numpy(np.int64)
                for k, v in enumerate(self.fill):
                    if v in vs:
                        c = ts.offsets[ti] + g * ts.VB[ti] + ts.vb_start[ti][vs.index(v)]
                        base[:, ti, k] = np.where(g >= 0, c, -1)
            tab_key = pd.factorize(targ["table"].astype(str) + "|" + targ["group"].astype(str)
                                   + "|" + targ["variable"].astype(str))[0]
        self.families = fams
        self.nt = len(fams)
        self.base = torch.as_tensor(base, device=device)                       # [N, nt, nv]
        self.cover = float((self.W[:, None, None] * (self.base >= 0)).sum())   # draw weight entering cells
        t = targ["target"].to_numpy(np.float64)
        self.tmask_np = np.isfinite(t)
        self.n_untargeted = int((~self.tmask_np).sum())
        if self.n_untargeted:
            log(f"{self.n_untargeted} cells have no target (NaN); left out of every measure")
        t = np.where(self.tmask_np, t, 0.0)
        self.targets_df = targ
        self.t = torch.as_tensor(t, device=device)
        self.tmask = torch.as_tensor(self.tmask_np, device=device)
        _, tab_id = np.unique(tab_key, return_inverse=True)
        self.tab_id = torch.as_tensor(tab_id, device=device)
        self.tab_total = torch.as_tensor(np.bincount(tab_id, weights=t), device=device)
        self.cell_tab_total = self.tab_total[self.tab_id]
        self.zero = (self.t == 0) & self.tmask
        self.fam_of_cell = torch.as_tensor(
            targ["family"].astype(str).map({f: i for i, f in enumerate(fams)}).to_numpy(),
            device=device)
        self.order = self.ctx.order            # slot of each step
        self.step_var = self.ctx.step_var      # index into fill of each step
        self.wslot_t = torch.as_tensor(self.ctx.wslot, device=device)
        self.step_var_t = torch.as_tensor(self.step_var, device=device)
        log(f"{self.N} records, {self.n_cells} cells, tables {fams}; generation order "
            f"{self.ctx.order_names}; unobserved and not drawn: "
            f"{getattr(self.ctx, 'unfilled_hidden', {})}")

    def load_model(self, path):
        m, _ = load_checkpoint(path, self.meta, self.device)
        return m

    # -- cells -------------------------------------------------------------
    def add_expected(self, ec, ri, k, pn, w):
        """ec += expected cell weights of variable fill[k] for draws of
        records ri with step probabilities pn [B, max_v] and weights w [B]."""
        nv = self.vocab[k]
        b = self.base[ri, :, k]                                           # [B, nt]
        ok = (b >= 0)[:, :, None]
        cid = b.clamp(min=0)[:, :, None] + self.t2b[k, :nv][None, None, :]  # [B, nt, nv]
        val = (w[:, None] * pn[:, :nv].double())[:, None, :] * ok
        ec.index_add_(0, cid.reshape(-1), val.reshape(-1))

    def token_cost(self, g, ri, k, max_v):
        """Sum over the tables of g at the cell each token of variable fill[k]
        would fall in, for records ri. [B, max_v] (0 beyond the vocabulary
        and for tables where the record has no group)."""
        nv = self.vocab[k]
        b = self.base[ri, :, k]
        cid = b.clamp(min=0)[:, :, None] + self.t2b[k, :nv][None, None, :]
        c = (g[cid] * (b >= 0)[:, :, None]).sum(1)
        out = torch.zeros(len(ri), max_v, dtype=c.dtype, device=self.device)
        out[:, :nv] = c
        return out

    def realized(self, tokens, ri, w):
        """Cell totals of drawn tokens [B, nv] (fill order) of records ri with
        weights w [B]."""
        tb = self.t2b[torch.arange(self.nv, device=self.device)[None, :], tokens.long()]  # [B, nv]
        b = self.base[ri]                                                          # [B, nt, nv]
        idx = b.clamp(min=0) + tb[:, None, :]
        val = w.double()[:, None, None] * (b >= 0)
        out = torch.zeros(self.n_cells, dtype=torch.float64, device=self.device)
        out.index_add_(0, idx.reshape(-1), val.reshape(-1))
        return out

    def cell_index_np(self, tokens):
        """Cell ids each draw falls in, for B's calibrate_matrix: tokens
        [N, K, nv] -> int32 [N*K, nt*nv], rows record-major, -1 where the
        record has no group in a table or the cell has no target."""
        t = np.asarray(tokens, dtype=np.int64)
        N, K, V = t.shape
        t2b = self.t2b.cpu().numpy()
        tb = t2b[np.arange(V)[None, None, :], t]                          # [N, K, nv]
        if (tb < 0).any():
            raise ValueError("token outside a variable's vocabulary")
        base = self.base.cpu().numpy()                                    # [N, nt, nv]
        out = np.empty((N, K, base.shape[1], V), dtype=np.int64)
        for s0 in range(0, N, 20000):
            b = base[s0:s0 + 20000][:, None, :, :]
            out[s0:s0 + 20000] = np.where(b >= 0, b + tb[s0:s0 + 20000][:, :, None, :], -1)
        out = out.reshape(N * K, -1)
        if self.n_untargeted:
            bad = ~self.tmask_np
            out = np.where((out >= 0) & bad[np.maximum(out, 0)], -1, out)
        return out.astype(np.int32)

    def fit(self, cells):
        m = self.tmask
        ae = torch.where(m, (cells - self.t).abs(), torch.zeros_like(cells))
        tot = self.t.sum()
        has = (self.cell_tab_total > 0) & m
        share = torch.where(has, ae / self.cell_tab_total.clamp(min=1e-300), torch.zeros_like(ae))
        byf = {}
        for i, f in enumerate(self.families):
            mf = self.fam_of_cell == i
            byf[f] = float(ae[mf].sum() / (2 * self.t[mf].sum()))
        return dict(misplaced=float(ae.sum() / (2 * tot)),
                    mean_err_share=float(share[has].mean()),
                    max_err_share=float(share[has].max()),
                    zero_cell_weight=float(cells[self.zero].sum() / self.cover),
                    by_family=byf)

    def sq_fit_and_grad(self, cells):
        """Mean over tables (with a positive total) of the sum of squared
        errors as a share of the table total; and its gradient per cell."""
        has = (self.cell_tab_total > 0) & self.tmask
        n_tab = int((self.tab_total > 0).sum())
        tt = self.cell_tab_total.clamp(min=1e-300)
        e = torch.where(has, (cells - self.t) / tt, torch.zeros_like(cells))
        fit = float((e ** 2).sum() / n_tab)
        grad = torch.where(has, 2 * e / tt / n_tab, torch.zeros_like(cells))
        return fit, grad


# ---------------------------------------------------------------------------
# Sampling with both models
# ---------------------------------------------------------------------------

def step_logp(S, model, cur, hid, ri, j, k, amp=False):
    """Masked float32 log-probabilities of slot j (WEALTH[k]) for records ri,
    computed as in a_common (allowed codes). With amp the forward pass runs
    under bf16 autocast (sampling only, never for a reported KL)."""
    with torch.autocast("cuda", dtype=torch.bfloat16, enabled=amp):
        return A._step_logits(model, S.ctx, cur, hid, ri, j, k, True, None)


@torch.no_grad()
def sample(S, model, ri, gen, ref=True, want_cells=True, w=None, amp=False):
    """One completion of each record in ri (LongTensor; repeats allowed).
    Returns tokens [B, 24] (WEALTH order), lp_new, lp_ref [B, 24] float32,
    kl_step [B, 24] (exact KL(new || ref) of each step given the drawn
    prefix), and, if want_cells, the expected cell weights (draw weights w,
    default the record's weight)."""
    model.eval()
    B = ri.shape[0]
    cur = S.ctx.tok_in[ri].clone()
    hid = cur < 0
    tk = torch.empty(B, S.nv, dtype=torch.long, device=S.device)
    lpn_o = torch.empty(B, S.nv, dtype=torch.float32, device=S.device)
    lpr_o = torch.zeros(B, S.nv, dtype=torch.float32, device=S.device)
    kl = torch.zeros(B, S.nv, dtype=torch.float32, device=S.device)
    ec = torch.zeros(S.n_cells, dtype=torch.float64, device=S.device) if want_cells else None
    if w is None:
        w = S.W[ri]
    for j, k in zip(S.order, S.step_var):
        lpn = step_logp(S, model, cur, hid, ri, j, k, amp)
        pn = lpn.exp()
        d = torch.multinomial(pn, 1, generator=gen).squeeze(1)
        tk[:, k] = d
        lpn_o[:, k] = lpn.gather(1, d[:, None]).squeeze(1)
        if ref:
            lpr = step_logp(S, S.ref, cur, hid, ri, j, k, amp)
            lpr_o[:, k] = lpr.gather(1, d[:, None]).squeeze(1)
            kl[:, k] = torch.where(pn > 0, pn * (lpn - lpr), torch.zeros_like(pn)).sum(1)
        if want_cells:
            S.add_expected(ec, ri, k, pn, w)
        cur[:, j] = d
        hid[:, j] = False
    return dict(tokens=tk, lp_new=lpn_o, lp_ref=lpr_o, kl_step=kl, cells=ec)


def evaluate(S, model, k, seed, batch):
    """Complete every record k times in float32; target fit of the expected
    and of the realized cells, and KL from the reference."""
    gen = torch.Generator(device=S.device)
    gen.manual_seed(seed)
    ec = torch.zeros(S.n_cells, dtype=torch.float64, device=S.device)
    rc = torch.zeros_like(ec)
    kl_chain = 0.0
    kl_samp = 0.0
    allidx = torch.arange(S.N, device=S.device).repeat_interleave(k)
    for s0 in range(0, allidx.shape[0], batch):
        ri = allidx[s0:s0 + batch]
        w = S.W[ri] / k
        r = sample(S, model, ri, gen, ref=True, want_cells=True, w=w)
        ec += r["cells"]
        rc += S.realized(r["tokens"], ri, w)
        kl_chain += float((w * r["kl_step"].double().sum(1)).sum())
        kl_samp += float((w * (r["lp_new"] - r["lp_ref"]).double().sum(1)).sum())
    fe = S.fit(ec)
    fr = S.fit(rc)
    return dict(misplaced_expected=fe["misplaced"], misplaced_realized=fr["misplaced"],
                mean_err_share_expected=fe["mean_err_share"],
                max_err_share_expected=fe["max_err_share"],
                zero_cell_weight_expected=fe["zero_cell_weight"],
                by_family_expected=fe["by_family"],
                kl_chain=kl_chain / S.Wsum, kl_sampled=kl_samp / S.Wsum)


# ---------------------------------------------------------------------------
# Training data
# ---------------------------------------------------------------------------

class DistillData:
    """B's calibrated draws as training examples."""

    def __init__(self, S, draws_path, weights_path, max_records=None):
        z = np.load(draws_path)
        tokens = z["tokens"]
        w = np.load(weights_path)["w"].astype(np.float64)
        if max_records:
            tokens, w = tokens[:max_records], w[:max_records]
        if tokens.shape[0] != S.N or w.shape != tokens.shape[:2]:
            raise ValueError(f"draws {tokens.shape}, weights {w.shape}, records {S.N}")
        if not np.all(np.isfinite(w)) or (w < 0).any():
            raise ValueError(f"weights: {int((~np.isfinite(w)).sum())} non-finite, "
                             f"{int((w < 0).sum())} negative")
        rs = w.sum(1)
        if (rs <= 0).any():
            raise ValueError(f"{int((rs <= 0).sum())} records whose draws all have weight zero")
        self.K = tokens.shape[1]
        self.tokens = torch.as_tensor(tokens.astype(np.int64), device=S.device)  # [N,K,24]
        flat = torch.as_tensor(w.reshape(-1), device=S.device)
        self.p = (flat / flat.sum()).float()
        q = w / rs[:, None]
        self.ess = float(np.average(1.0 / (q ** 2).sum(axis=1), weights=rs))
        log(f"distillation draws: K={self.K}, {int((flat > 0).sum())} of {flat.numel()} with "
            f"positive weight; weighted mean effective draws per record {self.ess:.2f}")

    def batch(self, S, B, gen):
        flat = torch.multinomial(self.p, B, replacement=True, generator=gen)
        i = flat // self.K
        j = flat % self.K
        dt = self.tokens[i, j]                                   # [B, 24] WEALTH order
        k = torch.randint(0, S.nv, (B,), generator=gen, device=S.device)   # generation position
        # draw tokens shown: those whose generation position is below k
        pos_of_var = torch.empty(S.nv, dtype=torch.long, device=S.device)
        pos_of_var[S.step_var_t] = torch.arange(S.nv, device=S.device)
        show = pos_of_var[None, :] < k[:, None]                  # [B, 24] WEALTH order
        cur = S.ctx.tok_in[i].clone()
        cur[:, S.wslot_t] = torch.where(show, dt, torch.full_like(dt, -1))
        hid = cur < 0
        var = S.step_var_t[k]                                    # WEALTH index scored
        slot = S.wslot_t[var]
        target = dt.gather(1, var[:, None]).squeeze(1)
        return i, cur, hid, var, slot, target


def position_loss(S, model, i, cur, hid, var, slot, target, amp):
    with torch.autocast("cuda", dtype=torch.bfloat16, enabled=amp):
        lg = model(cur, hid, S.ctx.src[i], S.ctx.year[i])
    ar = torch.arange(cur.shape[0], device=S.device)
    lg = lg[ar, slot].float()
    al = S.ctx.allowed[S.ctx.wave_i[i], var][:, :lg.shape[1]]
    lp = torch.log_softmax(lg.masked_fill(~al, float("-inf")), dim=-1)
    nll = -lp.gather(1, target[:, None]).squeeze(1)
    if not torch.isfinite(nll).all():
        raise RuntimeError("a training target is a disallowed code")
    return nll.mean()


class OriginalData:
    """The original training task (b_train.py with loss weight 'survey' and
    the SCF weight dropped): all expB training records, random hiding of
    observed variables, records drawn in proportion to population weight."""

    def __init__(self, S):
        meta = S.meta
        tok = np.load(A.DATA / "tokens_train.npy")[:, slot_columns(meta)].astype(np.int64)
        rec = pd.read_parquet(A.DATA / "records_train.parquet", columns=["source", "year", "weight"])
        for x in S.ctx.dropped:
            tok[:, S.ctx.snames.index(x)] = -1
        w = rec["weight"].to_numpy(float)
        if not np.all(np.isfinite(w)) or (w <= 0).any():
            raise ValueError("expB training weights not all positive and finite")
        sources = S.sources
        self.tok = torch.as_tensor(tok, device=S.device)
        self.src = torch.as_tensor(np.array([sources.index(s) for s in rec["source"].astype(str)]),
                                   device=S.device)
        self.year = torch.as_tensor(rec["year"].to_numpy(np.float32), device=S.device)
        self.p = torch.as_tensor(w / w.sum(), dtype=torch.float32, device=S.device)
        log(f"original task: {len(rec)} expB training records")

    def loss(self, model, B, gen, amp):
        i = torch.multinomial(self.p, B, replacement=True, generator=gen)
        t = self.tok[i]
        h, sc = draw_hidden_observed(t >= 0, gen)
        with torch.autocast("cuda", dtype=torch.bfloat16, enabled=amp):
            lg = model(t, h, self.src[i], self.year[i])
        loss, _, _ = masked_ce(lg.float(), t, sc)
        return loss


# ---------------------------------------------------------------------------
# train
# ---------------------------------------------------------------------------

def argd(a):
    """The command-line arguments as a plain dict (no function objects, so
    checkpoints load in any script)."""
    return {k: v for k, v in vars(a).items() if not callable(v)}


def setup_torch():
    torch.backends.cuda.matmul.allow_tf32 = False
    torch.backends.cudnn.allow_tf32 = False
    torch.set_float32_matmul_precision("highest")


def cmd_train(a):
    out = Path(a.out_dir)
    out.mkdir(parents=True, exist_ok=True)
    device = torch.device("cuda")
    setup_torch()
    torch.manual_seed(a.seed)
    S = Setup(device, records_path=a.records, max_records=a.max_records,
              targets_path=a.targets, arm=a.arm, target_set=a.target_set)
    model = S.load_model(A.CKPT)
    from torch.optim.swa_utils import AveragedModel, get_ema_multi_avg_fn
    ema = AveragedModel(model, multi_avg_fn=get_ema_multi_avg_fn(a.ema)) if a.ema > 0 else None

    if a.variant == "distill":
        D = DistillData(S, a.draws, a.weights, a.max_records)
    else:
        import d_direct
        D = d_direct.DirectObjective(S, a)
    O = OriginalData(S) if a.mix > 0 else None
    opt = torch.optim.AdamW(model.parameters(), lr=a.lr, betas=(0.9, 0.98), weight_decay=0.0)
    gen = torch.Generator(device=device)
    gen.manual_seed(a.seed)
    amp = not a.no_amp

    cols = ["step", "minutes", "train_loss", "orig_loss", "misplaced_expected",
            "misplaced_realized", "mean_err_share_expected", "max_err_share_expected",
            "zero_cell_weight_expected", "kl_chain", "kl_sampled", "best"]
    fcur = open(out / "curve.csv", "w", newline="")
    cw = csv.writer(fcur)
    cw.writerow(cols)
    t0 = time.time()

    def do_eval(step, tl, ol):
        m = ema.module if ema is not None else model
        te = time.time()
        r = evaluate(S, m, a.eval_k, a.eval_seed, a.sample_batch)
        r.update(step=step, minutes=(time.time() - t0) / 60, train_loss=tl, orig_loss=ol,
                 eval_seconds=time.time() - te)
        return r

    history = []
    r0 = do_eval(0, float("nan"), float("nan"))
    r0["best"] = 1
    history.append(r0)
    cw.writerow([r0.get(c, "") for c in cols])
    fcur.flush()
    log(f"reference: misplaced share {r0['misplaced_expected']:.5f} (expected cells), "
        f"{r0['misplaced_realized']:.5f} (realized); KL {r0['kl_chain']:.2e}; "
        f"eval {r0['eval_seconds']:.0f}s")
    best, best_step, bad = r0["misplaced_expected"], 0, 0
    ck_path = out / "checkpoint.pt"
    save_checkpoint(ck_path, model, A.DATA / "meta.json", S.sources,
                    dict(step=0, eval=r0, args=argd(a), reference=str(A.CKPT)))
    step = 0
    acc_l, acc_o, acc_n = 0.0, 0.0, 0
    next_eval = time.time() + a.eval_min * 60
    stop_reason = "time limit"
    while True:
        for g in opt.param_groups:
            g["lr"] = a.lr * min(1.0, (step + 1) / a.warmup)
        opt.zero_grad(set_to_none=True)
        if a.variant == "distill":
            model.train()
            i, cur, hid, var, slot, target = D.batch(S, a.batch, gen)
            loss = position_loss(S, model, i, cur, hid, var, slot, target, amp)
            ((1 - a.mix) * loss).backward()
            lv = float(loss.detach())
        else:
            lv = D.loss_backward(model, gen, scale=1 - a.mix)
        ol = None
        if O is not None:
            ol = O.loss(model, a.mix_batch, gen, amp)
            (a.mix * ol).backward()
        torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        opt.step()
        if ema is not None:
            ema.update_parameters(model)
        step += 1
        acc_l += lv
        acc_o += float(ol.detach()) if ol is not None else 0.0
        acc_n += 1
        if a.variant == "direct":
            D.log_step(step)
        now = time.time()
        timeup = (now - t0) > a.minutes * 60
        if now >= next_eval or timeup or step >= a.max_steps:
            r = do_eval(step, acc_l / acc_n, (acc_o / acc_n) if O is not None else float("nan"))
            acc_l, acc_o, acc_n = 0.0, 0.0, 0
            ok_kl = a.kl_cap is None or r["kl_chain"] <= a.kl_cap
            better = ok_kl and r["misplaced_expected"] < best * (1 - a.min_improve)
            r["best"] = int(better)
            if better:
                best, best_step, bad = r["misplaced_expected"], step, 0
                m = ema.module if ema is not None else model
                save_checkpoint(ck_path, m, A.DATA / "meta.json", S.sources,
                                dict(step=step, eval=r, args=argd(a), reference=str(A.CKPT)))
            else:
                bad += 1
            history.append(r)
            cw.writerow([r.get(c, "") for c in cols])
            fcur.flush()
            orig_txt = "" if O is None else f" (original task {r['orig_loss']:.4f})"
            log(f"step {step}: train loss {r['train_loss']:.4f}{orig_txt}"
                f", misplaced share {r['misplaced_expected']:.5f} (realized "
                f"{r['misplaced_realized']:.5f}), KL {r['kl_chain']:.4f} (sampled "
                f"{r['kl_sampled']:.4f}) {'*' if better else ''} eval {r['eval_seconds']:.0f}s")
            next_eval = time.time() + a.eval_min * 60
            if bad >= a.patience:
                stop_reason = f"no improvement in {a.patience} evaluations"
                break
            if timeup:
                break
            if step >= a.max_steps:
                stop_reason = "step limit"
                break
    fcur.close()
    summ = dict(variant=a.variant, args=argd(a), best_step=best_step,
                best_misplaced_expected=best, reference_eval=r0, stop_reason=stop_reason,
                steps=step, minutes=(time.time() - t0) / 60, history=history,
                n_records=S.N, slurm_job_id=os.environ.get("SLURM_JOB_ID"),
                gpu=torch.cuda.get_device_name(0))
    with open(out / "train.json", "w") as f:
        json.dump(summ, f, indent=2, default=float)
    log(f"done: {stop_reason}; best step {best_step}, misplaced share {best:.5f}")


# ---------------------------------------------------------------------------
# draws
# ---------------------------------------------------------------------------

def cmd_draws(a):
    out = Path(a.out_dir)
    out.mkdir(parents=True, exist_ok=True)
    device = torch.device("cuda")
    setup_torch()
    S = Setup(device, records_path=a.records, max_records=a.max_records,
              targets_path=a.targets, arm=a.arm, target_set=a.target_set)
    ck = Path(a.checkpoint) if a.checkpoint else out / "checkpoint.pt"
    model = S.load_model(ck)
    for p_ in model.parameters():
        p_.requires_grad_(False)
    K = a.k
    if a.sample_batch % K:
        raise ValueError("--sample-batch must be a multiple of --k")
    gen = torch.Generator(device=device)
    gen.manual_seed(a.seed)
    tokens = np.empty((S.N, K, S.nv), dtype=np.int16)
    lpn = np.empty((S.N, K, S.nv), dtype=np.float32)
    lpr = np.empty((S.N, K, S.nv), dtype=np.float32)
    klc = np.empty((S.N, K), dtype=np.float32)
    ec = torch.zeros(S.n_cells, dtype=torch.float64, device=device)
    rc = torch.zeros_like(ec)
    allidx = torch.arange(S.N, device=device).repeat_interleave(K)
    t0 = time.time()
    for s0 in range(0, allidx.shape[0], a.sample_batch):
        ri = allidx[s0:s0 + a.sample_batch]
        r = sample(S, model, ri, gen, ref=True, want_cells=True, w=S.W[ri] / K)
        ec += r["cells"]
        rc += S.realized(r["tokens"], ri, S.W[ri] / K)
        n0, n1 = s0 // K, (s0 + ri.shape[0]) // K
        tokens[n0:n1] = r["tokens"].view(-1, K, S.nv).cpu().numpy()
        lpn[n0:n1] = r["lp_new"].view(-1, K, S.nv).cpu().numpy()
        lpr[n0:n1] = r["lp_ref"].view(-1, K, S.nv).cpu().numpy()
        klc[n0:n1] = r["kl_step"].sum(1).view(-1, K).cpu().numpy()
    log(f"{S.N} x {K} draws in {time.time() - t0:.0f}s")
    # Check the in-loop reference log-probabilities against piece A's
    # ref_logprob on a subset of draws.
    rng = np.random.default_rng(0)
    pick = rng.choice(S.N * K, size=min(a.check_n, S.N * K), replace=False)
    lp_a = A.ref_logprob(S.ref, S.ctx, pick // K, tokens.reshape(-1, S.nv)[pick].astype(np.int64))
    dif = np.abs(lp_a - lpr.reshape(-1, S.nv)[pick])
    chk = dict(n=int(len(pick)), max_abs_diff=float(dif.max()), mean_abs_diff=float(dif.mean()))
    W = S.records["weight"].to_numpy(np.float64)
    kl_samp = float((W[:, None] * (lpn.astype(np.float64).sum(-1) - lpr.astype(np.float64).sum(-1))
                     ).mean(1).sum() / W.sum())
    kl_chain = float((W[:, None] * klc.astype(np.float64)).mean(1).sum() / W.sum())
    np.savez(out / "draws.npz", tokens=tokens,
             logp_new=lpn.astype(np.float64).sum(-1).astype(np.float32),
             logp_ref=lpr.astype(np.float64).sum(-1).astype(np.float32),
             logp_new_var=lpn, logp_ref_var=lpr, kl_chain=klc,
             seed=np.int64(a.seed), K=np.int64(K), rec=np.arange(S.N),
             wealth=np.array(S.fill))
    np.save(out / "expected_cells.npy", ec.cpu().numpy())
    fe = S.fit(ec)
    fr = S.fit(rc)
    info = dict(checkpoint=str(ck), k=K, seed=a.seed, n_records=S.N,
                seconds=time.time() - t0, ref_logprob_check=chk,
                kl_chain=kl_chain, kl_sampled=kl_samp,
                fit_expected=fe, fit_realized=fr,
                slurm_job_id=os.environ.get("SLURM_JOB_ID"))
    with open(out / "draws_info.json", "w") as f:
        json.dump(info, f, indent=2)
    log(f"wrote {out / 'draws.npz'}; KL {kl_chain:.4f} (chain rule), {kl_samp:.4f} (sampled); "
        f"misplaced share {fe['misplaced']:.5f} expected, {fr['misplaced']:.5f} realized; "
        f"check against A.ref_logprob: {chk}")


def cmd_resid(a):
    """Residual calibration of the tuned network's fresh draws with piece
    B's calibrate_matrix; writes weights_resid.npz (B's layout) and
    report_resid.json."""
    from b_calibrate import calibrate_matrix
    out = Path(a.out_dir)
    device = torch.device("cuda")
    S = Setup(device, records_path=a.records, max_records=a.max_records,
              targets_path=a.targets, arm=a.arm, target_set=a.target_set)
    z = np.load(out / "draws.npz")
    tokens = z["tokens"]
    K = tokens.shape[1]
    idx = S.cell_index_np(tokens)
    W = S.records["weight"].to_numpy(np.float64)
    t = S.targets_df["target"].to_numpy(np.float64)
    w, lam, rep = calibrate_matrix(idx, W, t, K, cell_meta=S.targets_df)
    cid = S.targets_df["cell"].to_numpy()
    np.savez(out / "weights_resid.npz", w=w, **{"lambda": lam}, cell_id=cid,
             cell_status=np.array(rep["cell_status"]))
    rep["draws_file"] = str(out / "draws.npz")
    rep["tag"] = "resid"
    with open(out / "report_resid.json", "w") as f:
        json.dump(rep, f, indent=1, default=float)
    log(f"residual calibration: KL {rep['kl_nats_per_unit_weight']:.4f}, effective draws "
        f"{rep['effective_draws_weighted_mean']:.2f}, statuses {rep['status_counts']}")


def parse_args():
    p = argparse.ArgumentParser()
    sub = p.add_subparsers(dest="cmd", required=True)

    def common(q):
        q.add_argument("--out-dir", required=True)
        q.add_argument("--records", default=None, help="records.parquet (default A's)")
        q.add_argument("--targets", default=None, help="targets.parquet (default A's)")
        q.add_argument("--arm", choices=["scf", "puf"], default="scf",
                       help="which records: SCF training records (arm S) or PUF records (arm P)")
        q.add_argument("--dataset", choices=["expB", "full", "full_v2"], default="expB",
                       help="expB: the pilot model of test 2; full, full_v2: the PUF + SCF wealth models")
        q.add_argument("--ref-checkpoint", default=None,
                       help="reference network (default: piece A's for the dataset)")
        q.add_argument("--a-dir", default=None, help="piece A's output folder for this dataset")
        q.add_argument("--target-set", default=None,
                       help="full model: piece A's target set (wcore, wfull); records and targets from a_full")
        q.add_argument("--a-arm", default=None,
                       help="piece A's arm / target-set name (e.g. P_core), passed to A.configure if it takes one")
        q.add_argument("--max-records", type=int, default=None,
                       help="dev only: first N records, targets recomputed on them")
        q.add_argument("--sample-batch", type=int, default=16384)

    t = sub.add_parser("train")
    common(t)
    t.add_argument("--variant", choices=["distill", "direct"], default="distill")
    t.add_argument("--draws", default=str(P / "a/ref_draws.npz"))
    t.add_argument("--weights", default=str(P / "b/weights_ref.npz"))
    t.add_argument("--lr", type=float, default=1e-4)
    t.add_argument("--warmup", type=int, default=200)
    t.add_argument("--batch", type=int, default=2048)
    t.add_argument("--mix", type=float, default=0.2,
                   help="share of the loss from the original training task (0 = none)")
    t.add_argument("--mix-batch", type=int, default=512)
    t.add_argument("--ema", type=float, default=0.998)
    t.add_argument("--minutes", type=float, default=75)
    t.add_argument("--max-steps", type=int, default=10 ** 9)
    t.add_argument("--eval-min", type=float, default=6)
    t.add_argument("--eval-k", type=int, default=4)
    t.add_argument("--eval-seed", type=int, default=4242)
    t.add_argument("--patience", type=int, default=3)
    t.add_argument("--min-improve", type=float, default=0.005)
    t.add_argument("--kl-cap", type=float, default=None)
    t.add_argument("--seed", type=int, default=1)
    t.add_argument("--no-amp", action="store_true")
    t.add_argument("--beta", type=float, default=1.0, help="direct: KL penalty")
    t.add_argument("--rl-batch", type=int, default=1024, help="direct: draws per step")
    t.add_argument("--refresh-k", type=int, default=1,
                   help="direct: draws per record in each refresh")
    t.add_argument("--refresh-steps", type=int, default=5,
                   help="direct: steps between refreshes of the expected cells")
    t.set_defaults(func=cmd_train)

    d = sub.add_parser("draws")
    common(d)
    d.add_argument("--checkpoint", default=None)
    d.add_argument("--k", type=int, default=32)
    d.add_argument("--seed", type=int, default=31337)
    d.add_argument("--check-n", type=int, default=4096)
    d.set_defaults(func=cmd_draws)

    r = sub.add_parser("resid")
    common(r)
    r.set_defaults(func=cmd_resid)
    return p.parse_args()


def configure(a):
    """Point piece A's module at the dataset and reference checkpoint."""
    if a.dataset in ("full", "full_v2"):
        A.configure_full(a.dataset, a.ref_checkpoint)
    elif a.ref_checkpoint:
        A.configure(ckpt=a.ref_checkpoint)
    if a.a_dir:
        A.configure(out=a.a_dir)
    if a.a_arm:
        import inspect
        if "arm" in inspect.signature(A.configure).parameters:
            A.configure(arm=a.a_arm)
        else:
            log(f"A.configure takes no arm; --a-arm {a.a_arm} only set POSTTRAIN_ARM")
    log(f"dataset {A.DATA}; reference {A.CKPT}; piece A folder {A.PA}; arm {a.arm}")


if __name__ == "__main__":
    a = parse_args()
    configure(a)
    a.func(a)
