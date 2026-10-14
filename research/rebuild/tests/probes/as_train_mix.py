"""Probe AS: unchanged copy of s_train.py. Probe S (test 4): copy of t_train.py with one addition.

  - --mix-style {ordered, random_next, random_all}: what the share
    --mix-ordered of records gets each step.
      ordered      T's task: a random prefix of the record's observed
                   variables in one fixed generation order is visible and
                   the next observed variable is scored (weight n observed).
      random_next  each record gets its own random order of its observed
                   variables, drawn afresh each step; a random prefix
                   (length uniform on 0..n-1) is visible and the next
                   variable in that order is scored (weight n observed).
      random_all   the same random order and prefix, but every remaining
                   variable is scored, each with weight n / (n - prefix).
    random_next and random_all have the same expected loss; random_all has
    less noise. T's `--mask-style orderless` is random_all applied to every
    record with the validation loss also drawn that way; here the
    validation loss is always the plain any-order loss, so it compares
    across runs.

Original notes of t_train.py:

Probe T (test 4): copy of ../posttrain/h_train.py with four additions.

  - --stop-on puf: early stopping on the PUF part of the validation loss
    (population-weighted) instead of the loss over all records.
  - The checkpoint with the lowest population-weighted PUF validation loss
    is always saved as well, as checkpoint_<tag>_pufbest.pt, and the wealth
    completion score is computed for it too (run json key
    val_completion_pufbest).
  - --top-tilt "0.9:3,0.99:10,0.999:30": PUF records whose AGI (E00100)
    token lies at or above a weighted rank among positive AGI values get
    their loss weight multiplied by the factor (the largest edge reached),
    then all PUF factors are divided by their population-weighted mean so
    the PUF records' total share of the training signal is unchanged. The
    model then learns the PUF population tilted by the factor; generated
    records are weighted back by 1 / factor of their drawn AGI token.
  - --lr-epochs N: the cosine learning-rate decay runs over N epochs
    (default: --max-epochs, which at 300 means the rate never decays in
    practice).
  - --mix-ordered P: a share P of records each step get the "ordered" task
    instead of any-order hiding: a random prefix of the record's observed
    variables in the generation order (demographics moved after the PUF
    amounts, wealth last) is visible and the next observed variable is
    scored, with weight (number observed) x record weight; this is exactly
    the question generation asks at each step.
  The validation loss by source is always also logged with plain population
  weights (columns val_pop_<source>), so runs with a tilt can be compared.

Original docstring of h_train.py:

Train the masked record transformer on the `full` dataset (test 3, piece H).

Copy of ../scf_joint/b_train.py (test 2), changed for 63 variables
(test 1's 39 PUF variables and the 24 SCF wealth variables):

  - Imports the model unchanged from ../scf_joint/b_model.py.
  - --drop-weight-vars treats every variable of kind "weight" as
    unobserved for every record (the record weight is never a model
    variable; with --loss-weight survey it weights the loss instead).
  - The validation loss is also logged separately for each source
    (columns val_loss_<source> in the training log), weighted the same way.
  - The "wealth" set for the completion score after training is found
    from the data: variables observed on some SCF training records and on
    no PUF training record (test 2 used the meta.json group "scf_only").

Test 2's notes on b_train.py follow.

Copied from test 1's puf_generator/train.py and changed for SPEC.md
section B:

  - Unobserved variables (-1 in tokens_*.npy) are always hidden and never
    scored. Any-order hiding is applied to the observed variables only, with
    at least one observed variable hidden per record per step.
  - Each record's source and year (from records_*.parquet) enter the model.
  - --sources restricts training and validation to some sources (the
    benchmark trained on half 2 alone is --sources SCF).
  - Records are unweighted; the SCF weight is itself a variable.

Validation uses fixed masks (drawn once from --val-mask-seed, several per
record), drawn the same way as training masks. Training stops when
validation loss has not improved for --patience epochs; the checkpoint kept
is the one with the lowest validation loss (of the moving average of the
weights, as in test 1).

After training, a second measure on the best checkpoint: the completion
log-likelihood of the validation records' SCF-only variables. For every
validation record that observes all SCF-only variables, they are all hidden
and filled in the completion order; the score is the sum over that order of
-log p(true value | shared variables and earlier SCF-only values). This is
the model as it is used for completion.

Outputs in --out-dir, suffixed with --tag:
  checkpoint_<tag>.pt, train_log_<tag>.csv, run_<tag>.json
"""

import argparse
import csv
import json
import math
import os
import time
from pathlib import Path

import sys

import numpy as np
import torch

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "scf_joint"))
from b_model import (RecordTransformer, check_tokens, completion_nll,
                     count_parameters, draw_hidden_observed, fill_order,
                     load_meta, load_split, masked_ce, save_checkpoint,
                     slot_variables, source_names)


def parse_args():
    p = argparse.ArgumentParser()
    p.add_argument("--data-dir", required=True)
    p.add_argument("--out-dir", required=True)
    p.add_argument("--tag", default="joint_seed01")
    p.add_argument("--sources", default=None,
                   help="comma-separated sources to train on (default: all)")
    p.add_argument("--no-source", action="store_true",
                   help="disable source embedding in training and saved model; "
                        "retain source metadata for filtering and evaluation")
    p.add_argument("--d-model", type=int, default=256)
    p.add_argument("--layers", type=int, default=6)
    p.add_argument("--heads", type=int, default=8)
    p.add_argument("--ff-mult", type=int, default=4)
    p.add_argument("--dropout", type=float, default=0.1)
    p.add_argument("--lr", type=float, default=5e-4)
    p.add_argument("--weight-decay", type=float, default=0.01)
    p.add_argument("--batch-size", type=int, default=512)
    p.add_argument("--max-epochs", type=int, default=300)
    p.add_argument("--warmup-steps", type=int, default=1000)
    p.add_argument("--patience", type=int, default=15)
    p.add_argument("--keep-last", action="store_true",
                   help="probe S: no early stopping; the kept checkpoint is the last epoch "
                        "(for models trained mostly on generation questions, whose any-order "
                        "validation loss is not what they are trained on)")
    p.add_argument("--seed", type=int, default=1)
    p.add_argument("--val-mask-seed", type=int, default=777)
    p.add_argument("--val-masks", type=int, default=4)
    p.add_argument("--max-hours", type=float, default=6.0)
    p.add_argument("--no-amp", action="store_true")
    p.add_argument("--ema", type=float, default=0.999)
    p.add_argument("--drop-weight-vars", action="store_true",
                   help="treat every variable of kind 'weight' as unobserved everywhere")
    p.add_argument("--drop-vars", default="",
                   help="comma-separated variables treated as unobserved for every "
                        "record (always hidden, never scored), e.g. scf_weight")
    p.add_argument("--loss-weight", default="none", choices=["none", "survey", "sqrt"],
                   help="weight each record's loss by its population weight "
                        "(records file column `weight`), or its square root; "
                        "normalised to mean 1 over the training records. The "
                        "validation loss used for stopping is weighted the same way.")
    p.add_argument("--task-frac", type=float, default=0.0,
                   help="share of records (among those observing any SCF-only "
                        "variable) that get the completion task each step: every "
                        "observed SCF-only variable hidden and scored, the rest "
                        "visible. The other records get any-order hiding. 0 keeps "
                        "the old behaviour. Validation masks are drawn the same way.")
    p.add_argument("--stop-on", choices=["all", "puf"], default="all")
    p.add_argument("--top-tilt", default="")
    p.add_argument("--lr-epochs", type=int, default=None)
    p.add_argument("--mix-ordered", type=float, default=0.0)
    p.add_argument("--ordered-first", default="",
                   help="comma-separated variables moved to the front of the ordered-task order")
    p.add_argument("--mix-style", choices=["ordered", "ordered2", "random_next", "random_all"],
                   default="ordered")
    p.add_argument("--mask-style", choices=["random", "orderless", "ordered"],
                   default="random", help="random preserves existing masking/loss; "
                   "orderless samples a uniform nonempty hidden count and weights "
                   "loss by observed/hidden count; ordered scores one random next "
                   "variable in the fixed completion order, weighted by observed count")
    return p.parse_args()


def draw_hidden_sequence(observed, order, style, generator):
    """Monte Carlo autoregressive objectives on each row's observed variables.

    With gaps, these are objectives on observed subsequences, not exact
    marginal likelihoods of a common full-data fixed-order joint.
    Caller scales each record's loss by n_observed / n_scored.
    """
    n = observed.sum(dim=1)
    if (n == 0).any():
        raise ValueError("sequence masking requires an observed variable per row")
    u = torch.rand(len(n), device=observed.device, generator=generator)
    cut = (u * n).long()  # uniform visible-prefix length 0,...,n-1
    if style == "ordered":
        obs = observed[:, order]
        rank = obs.long().cumsum(dim=1) - 1
        visible = obs & (rank < cut[:, None])
        scored = obs & (rank == cut[:, None])
        inverse = torch.argsort(order)
        return ~visible[:, inverse], scored[:, inverse]
    if style == "orderless":
        keys = torch.rand(observed.shape, device=observed.device, generator=generator)
        keys = keys.masked_fill(~observed, 2.0)
        rank = keys.argsort(dim=1).argsort(dim=1)
        visible = observed & (rank < cut[:, None])
        return ~visible, observed & ~visible
    raise ValueError(style)


def draw_hidden_random_order(observed, generator, score_all):
    """Probe S: per record a fresh random order of its observed variables and
    a visible prefix of uniform length 0..n-1. Returns (hidden_input, scored,
    per-record loss factor): next variable only with factor n, or every
    remaining variable with factor n / (n - prefix)."""
    n = observed.sum(dim=1)
    if (n == 0).any():
        raise ValueError("random-order task needs an observed variable per row")
    u = torch.rand(len(n), device=observed.device, generator=generator)
    cut = (u * n).long().clamp(max=n - 1)
    keys = torch.rand(observed.shape, device=observed.device, generator=generator)
    keys = keys.masked_fill(~observed, 2.0)
    rank = keys.argsort(dim=1).argsort(dim=1)
    visible = observed & (rank < cut[:, None])
    if score_all:
        scored = observed & ~visible
        factor = n.float() / (n - cut).float()
    else:
        scored = observed & (rank == cut[:, None])
        factor = n.float()
    return ~visible, scored, factor


def sequence_weights(tokens, scored, rw=None):
    """Inverse target-sampling correction; denominator remains total weighted
    observed slots, independent of the randomly chosen mask size."""
    factor = (tokens >= 0).sum(dim=1) / scored.sum(dim=1).clamp(min=1)
    return factor if rw is None else factor * rw


def draw_hidden_mixed(observed, wcols, frac, generator):
    """Any-order hiding, except that a share `frac` of the records observing
    any SCF-only variable get the completion task instead (main session,
    9 October: train on the task that is graded)."""
    hidden, scored = draw_hidden_observed(observed, generator)
    if frac <= 0:
        return hidden, scored
    has_w = (observed & wcols).any(dim=1)
    pick = (torch.rand(observed.shape[0], generator=generator,
                       device=observed.device) < frac) & has_w
    if pick.any():
        sc_task = observed[pick] & wcols
        scored[pick] = sc_task
        hidden[pick] = sc_task | ~observed[pick]
    return hidden, scored


@torch.no_grad()
def evaluate(model, tok, hid, scored, src, year, batch_size, amp, rw=None):
    """Mean cross-entropy over scored slots; with rw [N] record weights, each
    scored slot of record i counts with weight rw[i]."""
    model.eval()
    s_tot, n_tot = 0.0, 0.0
    for i in range(0, tok.shape[0], batch_size):
        sl = slice(i, i + batch_size)
        with torch.autocast("cuda", dtype=torch.bfloat16, enabled=amp):
            logits = model(tok[sl], hid[sl], src[sl], year[sl])
        if rw is None:
            _, s, n = masked_ce(logits, tok[sl], scored[sl])
        else:
            s, n = weighted_ce(logits, tok[sl], scored[sl], rw[sl])
        s_tot += float(s)
        n_tot += float(n)
    model.train()
    return s_tot / max(n_tot, 1e-12)


def weighted_ce(logits, tokens, scored, rw):
    """Sum of record-weighted cross-entropy over scored slots, and the sum of
    the weights of the scored slots."""
    lp = torch.log_softmax(logits.float(), dim=-1)
    nll = -lp.gather(-1, tokens.clamp(min=0)[..., None]).squeeze(-1)
    wh = scored * rw[:, None]
    return (nll * wh).sum(), wh.sum()


def record_weights(d, mode):
    """Loss weight of each record: none, population weight, or its square root."""
    if mode == "none":
        return None
    w = d["records"]["weight"].to_numpy(dtype=float)[d["rows"]]
    if not np.all(np.isfinite(w)) or (w <= 0).any():
        raise ValueError(f"record weights: {int((~np.isfinite(w)).sum())} non-finite, "
                         f"{int((w <= 0).sum())} non-positive")
    return np.sqrt(w) if mode == "sqrt" else w


def wealth_columns(d, sources, svars, dropped):
    """Variables observed on some SCF training records and on no PUF
    training record (the 24 wealth variables in `full`)."""
    obs = d["tok"] >= 0
    is_puf = d["src"] == sources.index("PUF") if "PUF" in sources else np.zeros(len(obs), bool)
    is_scf = np.isin(np.array(sources)[d["src"]], [s for s in sources if s.startswith("SCF")])
    out = obs[is_scf].any(axis=0) & ~obs[is_puf].any(axis=0)
    out &= np.array([v["name"] not in dropped for v in svars])
    return out


def main():
    a = parse_args()
    if a.mask_style != "random" and a.task_frac != 0:
        raise ValueError("--task-frac is only supported with --mask-style random")
    out = Path(a.out_dir)
    out.mkdir(parents=True, exist_ok=True)
    data = Path(a.data_dir)
    meta = load_meta(data / "meta.json")
    svars = slot_variables(meta)
    vocab = [int(v["vocab_size"]) for v in svars]
    names = [v["name"] for v in svars]
    V = len(vocab)

    import pandas as pd
    rec_tr = pd.read_parquet(data / "records_train.parquet", columns=["source"])
    sources = source_names(meta, rec_tr)
    keep = a.sources.split(",") if a.sources else None
    if keep is not None and not set(keep) <= set(sources):
        raise ValueError(f"--sources {keep} not all in {sources}")

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    amp = (device.type == "cuda") and not a.no_amp
    gpu_name = torch.cuda.get_device_name(0) if device.type == "cuda" else "cpu"
    torch.manual_seed(a.seed)
    np.random.seed(a.seed)

    tr = load_split(data, "train", meta, sources, keep)
    va = load_split(data, "val", meta, sources, keep)
    check_tokens("train", tr["tok"], vocab)
    check_tokens("val", va["tok"], vocab)
    dropped = [x for x in a.drop_vars.split(",") if x]
    if a.drop_weight_vars:
        dropped += [v["name"] for v in svars if v.get("kind") == "weight"
                    and v["name"] not in dropped]
    for x in dropped:
        if x not in names:
            raise ValueError(f"--drop-vars: {x} is not a model variable")
        for d in (tr, va):
            d["tok"][:, names.index(x)] = -1
    if dropped:
        print(f"variables treated as unobserved everywhere: {dropped}", flush=True)
    tr_w = record_weights(tr, a.loss_weight)
    va_w = record_weights(va, a.loss_weight)
    va_pop = record_weights(va, "survey")
    va_pop = va_pop / record_weights(tr, "survey").mean()
    tilt_info = None
    if a.top_tilt:
        if a.loss_weight != "survey":
            raise ValueError("--top-tilt needs --loss-weight survey")
        edges = sorted((float(x.split(":")[0]), float(x.split(":")[1]))
                       for x in a.top_tilt.split(","))
        jA = names.index("E00100")
        vA = svars[jA]
        toks = vA["tokens"]
        def tilt_factor(tokcol):
            f = np.ones(len(tokcol))
            for t in toks:
                if t.get("kind") != "pos":
                    continue
                fac = 1.0
                for e, c in edges:
                    if float(t["lo"]) >= e - 1e-9:
                        fac = c
                f[tokcol == int(t["token"])] = fac
            return f
        ipuf = sources.index("PUF")
        f_tr = np.where(tr["src"] == ipuf, tilt_factor(tr["tok"][:, jA]), 1.0)
        f_va = np.where(va["src"] == ipuf, tilt_factor(va["tok"][:, jA]), 1.0)
        mp = tr["src"] == ipuf
        norm = (tr_w[mp] * f_tr[mp]).sum() / tr_w[mp].sum()
        f_tr = np.where(mp, f_tr / norm, 1.0)
        f_va = np.where(va["src"] == ipuf, f_va / norm, 1.0)
        tr_w, va_w = tr_w * f_tr, va_w * f_va
        tilt_info = dict(edges=edges, norm=float(norm))
        print(f"top tilt {edges}; PUF factors divided by {norm:.4f}; share of PUF "
              f"training signal on records at or above each edge: " + ", ".join(
                  f"{e}: {(tr_w[mp] * (f_tr[mp] >= c / norm - 1e-12)).sum() / tr_w[mp].sum():.4f}"
                  for e, c in edges), flush=True)
    if tr_w is not None:
        scale = tr_w.mean()
        tr_w, va_w = tr_w / scale, va_w / scale
        ess = tr_w.sum() ** 2 / (tr_w ** 2).sum()
        print(f"loss weights ({a.loss_weight}), mean 1 on training records: min "
              f"{tr_w.min():.4f}, median {np.median(tr_w):.4f}, max {tr_w.max():.4f}; "
              f"effective number of training records {ess:.0f} of {tr_w.size}", flush=True)
    for nm, d in (("train", tr), ("val", va)):
        n_obs = (d["tok"] >= 0).sum(axis=1)
        if (n_obs == 0).any():
            raise ValueError(f"{nm}: {int((n_obs == 0).sum())} records observe no variable")
    counts = {nm: {s: int((d["src"] == i).sum()) for i, s in enumerate(sources)}
              for nm, d in (("train", tr), ("val", va))}
    years = {nm: sorted(set(d["year"].tolist())) for nm, d in (("train", tr), ("val", va))}
    print(f"sources {sources}; kept {keep or 'all'}; records by source {counts}; "
          f"years {years}; variables {V}; total tokens {sum(vocab)}; GPU {gpu_name}",
          flush=True)
    obs_share = {names[j]: float((tr["tok"][:, j] >= 0).mean()) for j in range(V)}
    print("share of training records observing each variable: " +
          ", ".join(f"{k} {v:.2f}" for k, v in obs_share.items()), flush=True)

    def to_dev(d):
        return (torch.as_tensor(d["tok"], device=device),
                torch.as_tensor(d["src"], device=device),
                torch.as_tensor(d["year"], device=device, dtype=torch.float32))

    tr_t, tr_s, tr_y = to_dev(tr)
    va_t, va_s, va_y = to_dev(va)

    g_val = torch.Generator(device=device)
    g_val.manual_seed(a.val_mask_seed)
    rep = a.val_masks
    val_tok, val_src, val_year = va_t.repeat(rep, 1), va_s.repeat(rep), va_y.repeat(rep)
    wealth_np = wealth_columns(tr, sources, svars, dropped)
    print(f"wealth variables (observed on SCF records, never on PUF records): "
          f"{[names[j] for j in np.flatnonzero(wealth_np)]}", flush=True)
    wcols = torch.as_tensor(wealth_np, device=device)
    if a.task_frac > 0:
        print(f"completion task for a share {a.task_frac} of records observing SCF-only "
              f"variables ({int(wcols.sum())} variables)", flush=True)
    seq_order = torch.as_tensor(fill_order(meta, svars), device=device)
    if a.mix_ordered > 0:
        fo = [names[j] for j in fill_order(meta, svars)]
        demo = ["AGERANGE", "GENDER", "EARNSPLIT"]
        wl = [names[j] for j in np.flatnonzero(wealth_np)]
        pre = [n for n in fo if n not in demo and n not in wl]
        fo = pre + demo + [n for n in fo if n in wl]
        seq_order_b = torch.as_tensor([names.index(n) for n in fo], device=device)
        first = [x for x in a.ordered_first.split(",") if x]
        fo = first + [n for n in fo if n not in first]
        print(f"ordered task order: {fo}", flush=True)
        seq_order = torch.as_tensor([names.index(n) for n in fo], device=device)
        if a.mix_style == "ordered2":
            print("ordered2: each record of the ordered share uses the order above or, with "
                  "probability one half, the same order without the variables moved first",
                  flush=True)
    def draw_masks(tok, generator):
        if a.mix_ordered > 0:
            obs = tok >= 0
            h1, s1 = draw_hidden_mixed(obs, wcols, a.task_frac, generator)
            if a.mix_style == "ordered":
                h2, s2 = draw_hidden_sequence(obs, seq_order, "ordered", generator)
                f2 = obs.sum(dim=1).float()
            elif a.mix_style == "ordered2":
                ha, sa = draw_hidden_sequence(obs, seq_order, "ordered", generator)
                hb, sb = draw_hidden_sequence(obs, seq_order_b, "ordered", generator)
                coin = torch.rand(obs.shape[0], generator=generator, device=obs.device) < 0.5
                h2 = torch.where(coin[:, None], hb, ha)
                s2 = torch.where(coin[:, None], sb, sa)
                f2 = obs.sum(dim=1).float()
            else:
                h2, s2, f2 = draw_hidden_random_order(obs, generator,
                                                      a.mix_style == "random_all")
            pick = torch.rand(obs.shape[0], generator=generator, device=obs.device) < a.mix_ordered
            return (torch.where(pick[:, None], h2, h1), torch.where(pick[:, None], s2, s1),
                    torch.where(pick, f2, torch.ones_like(pick, dtype=torch.float32)))
        if a.mask_style == "random":
            return draw_hidden_mixed(tok >= 0, wcols, a.task_frac, generator) + (None,)
        return draw_hidden_sequence(tok >= 0, seq_order, a.mask_style, generator) + (None,)
    print(f"mask style: {a.mask_style}", flush=True)
    if a.mix_ordered > 0:
        # validation keeps plain any-order hiding, so its loss compares with other runs
        val_hid, val_sc = draw_hidden_mixed(val_tok >= 0, wcols, a.task_frac, g_val)
        val_mix = None
    else:
        val_hid, val_sc, val_mix = draw_masks(val_tok, g_val)
    tr_w_t = torch.as_tensor(tr_w, dtype=torch.float32, device=device) if tr_w is not None else None
    val_w_t = (torch.as_tensor(np.tile(va_w, rep), dtype=torch.float32, device=device)
               if va_w is not None else None)
    val_pop_t = torch.as_tensor(np.tile(va_pop, rep), dtype=torch.float32, device=device)
    if a.mask_style != "random":
        val_w_t = sequence_weights(val_tok, val_sc, val_w_t)
    if val_mix is not None:
        val_w_t = val_w_t * val_mix
        val_pop_t = val_pop_t * val_mix

    model = RecordTransformer(svars, len(sources), d_model=a.d_model, n_layers=a.layers,
                              n_heads=a.heads, ff_mult=a.ff_mult,
                              dropout=a.dropout, use_source=not a.no_source).to(device)
    print(f"source embedding enabled: {model.use_source}", flush=True)
    n_params = count_parameters(model)
    print(f"parameters: {n_params:,}", flush=True)
    ema_model = None
    if a.ema > 0:
        from torch.optim.swa_utils import AveragedModel, get_ema_multi_avg_fn
        ema_model = AveragedModel(model, multi_avg_fn=get_ema_multi_avg_fn(a.ema))

    decay, no_decay = [], []
    for n_, p in model.named_parameters():
        (no_decay if p.ndim < 2 or "emb" in n_ or "bias" in n_ else decay).append(p)
    opt = torch.optim.AdamW([{"params": decay, "weight_decay": a.weight_decay},
                             {"params": no_decay, "weight_decay": 0.0}],
                            lr=a.lr, betas=(0.9, 0.98))
    steps_per_epoch = math.ceil(tr_t.shape[0] / a.batch_size)
    total_steps = steps_per_epoch * (a.lr_epochs or a.max_epochs)

    def lr_at(step):
        if step < a.warmup_steps:
            return a.lr * (step + 1) / a.warmup_steps
        frac = (step - a.warmup_steps) / max(1, total_steps - a.warmup_steps)
        return a.lr * (0.1 + 0.9 * 0.5 * (1 + math.cos(math.pi * min(frac, 1.0))))

    g_train = torch.Generator(device=device)
    g_train.manual_seed(a.seed)
    log_path = out / f"train_log_{a.tag}.csv"
    ck_path = out / f"checkpoint_{a.tag}.pt"
    ck_puf = out / f"checkpoint_{a.tag}_pufbest.pt"
    log_f = open(log_path, "w", newline="")
    log = csv.writer(log_f)
    log.writerow(["epoch", "train_loss", "val_loss"] +
                 [f"val_loss_{s}" for s in sources] +
                 [f"val_pop_{s}" for s in sources] +
                 ["lr", "epoch_seconds", "elapsed_seconds", "best"])

    best, best_epoch, bad = float("inf"), -1, 0
    best_puf, best_puf_epoch, best_all, best_all_epoch = float("inf"), -1, float("inf"), -1
    step = 0
    t0 = time.time()
    model.train()
    for epoch in range(1, a.max_epochs + 1):
        te = time.time()
        perm = torch.randperm(tr_t.shape[0], generator=g_train, device=device)
        s_tot = torch.zeros((), device=device)
        n_tot = torch.zeros((), device=device)
        for i in range(0, tr_t.shape[0], a.batch_size):
            idx = perm[i:i + a.batch_size]
            t = tr_t[idx]
            h, sc, mixf = draw_masks(t, g_train)
            for gp in opt.param_groups:
                gp["lr"] = lr_at(step)
            with torch.autocast("cuda", dtype=torch.bfloat16, enabled=amp):
                logits = model(t, h, tr_s[idx], tr_y[idx])
            rw = tr_w_t[idx] if tr_w_t is not None else None
            if a.mask_style != "random":
                rw = sequence_weights(t, sc, rw)
            if mixf is not None:
                rw = rw * mixf
            if rw is None:
                loss, s, n = masked_ce(logits, t, sc)
            else:
                s, n = weighted_ce(logits, t, sc, rw)
                loss = s / n.clamp(min=1e-8)
                s, n = s.detach(), n.detach()
            opt.zero_grad(set_to_none=True)
            loss.backward()
            torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
            opt.step()
            if ema_model is not None:
                ema_model.update_parameters(model)
            s_tot += s
            n_tot += n
            step += 1
        train_loss = (s_tot / n_tot).item()
        if not math.isfinite(train_loss):
            raise RuntimeError(f"non-finite training loss at epoch {epoch}")
        eval_model = ema_model.module if ema_model is not None else model
        val_loss = evaluate(eval_model, val_tok, val_hid, val_sc, val_src, val_year,
                            4096, amp, rw=val_w_t)
        val_by_src = []
        for si in range(len(sources)):
            m = val_src == si
            if not bool(m.any()):
                val_by_src.append(float("nan"))
                continue
            val_by_src.append(evaluate(eval_model, val_tok[m], val_hid[m], val_sc[m],
                                       val_src[m], val_year[m], 4096, amp,
                                       rw=None if val_w_t is None else val_w_t[m]))
        val_pop = []
        for si in range(len(sources)):
            m = val_src == si
            val_pop.append(evaluate(eval_model, val_tok[m], val_hid[m], val_sc[m],
                                    val_src[m], val_year[m], 4096, amp, rw=val_pop_t[m])
                           if bool(m.any()) else float("nan"))
        puf_pop = val_pop[sources.index("PUF")]
        if val_loss < best_all or a.keep_last:
            best_all, best_all_epoch = val_loss, epoch
            save_checkpoint(ck_path, eval_model, data / "meta.json", sources,
                            dict(epoch=epoch, val_loss=val_loss, args=vars(a),
                                 trained_sources=keep or sources, kept_by="all"))
        if puf_pop < best_puf:
            best_puf, best_puf_epoch = puf_pop, epoch
            save_checkpoint(ck_puf, eval_model, data / "meta.json", sources,
                            dict(epoch=epoch, val_loss=val_loss, val_pop_puf=puf_pop,
                                 args=vars(a), trained_sources=keep or sources,
                                 kept_by="puf"))
        crit = puf_pop if a.stop_on == "puf" else val_loss
        is_best = crit < best
        if is_best:
            best, best_epoch, bad = crit, epoch, 0
        else:
            bad += 1
        elapsed = time.time() - t0
        log.writerow([epoch, f"{train_loss:.6f}", f"{val_loss:.6f}"] +
                     [f"{x:.6f}" for x in val_by_src] +
                     [f"{x:.6f}" for x in val_pop] +
                     [f"{lr_at(step):.3e}", f"{time.time() - te:.1f}",
                      f"{elapsed:.1f}", int(is_best)])
        log_f.flush()
        print(f"epoch {epoch:4d}  train {train_loss:.4f}  val {val_loss:.4f} ("
              + ", ".join(f"{s} {x:.4f}" for s, x in zip(sources, val_by_src)) + ")"
              f"  {'*' if is_best else ' '}  {time.time() - te:.1f}s", flush=True)
        if bad >= a.patience and not a.keep_last:
            print(f"early stop: no improvement for {a.patience} epochs", flush=True)
            break
        if elapsed > a.max_hours * 3600:
            print("stopping on wall-time limit", flush=True)
            break
    log_f.close()
    train_seconds = time.time() - t0

    # Completion log-likelihood of SCF-only variables on validation records
    # that observe all of them.
    comps = {}
    for which, cp in (("all", ck_path), ("pufbest", ck_puf)):
        comps[which] = completion_score(cp, model, device, meta, svars, wealth_np, va,
                                        names, V, amp)
    comp = comps["all"]
    summary_extra = dict(val_completion_pufbest=comps["pufbest"], best_all_epoch=best_all_epoch,
                         best_all_val_loss=best_all, best_puf_epoch=best_puf_epoch,
                         best_puf_val_pop=best_puf, stop_on=a.stop_on, tilt=tilt_info)
    _finish(a, out, sources, keep, n_params, gpu_name, counts, years, V, vocab,
            steps_per_epoch, epoch, best_epoch, best, train_seconds, comp, dropped,
            summary_extra)


def completion_score(cp, model, device, meta, svars, wealth_np, va, names, V, amp):
    ck = torch.load(cp, map_location=device, weights_only=False)
    model.load_state_dict(ck["state_dict"])
    model.eval()
    order = fill_order(meta, svars)
    scf_only = wealth_np.copy()
    wealth = wealth_np & np.array([v.get("kind") == "amount" for v in svars])
    comp = {}
    if scf_only.any():
        vt = va["tok"]
        full = (vt[:, scf_only] >= 0).all(axis=1)
        if full.any():
            tin = vt[full].copy()
            tin[:, scf_only] = -1
            nll, scm = completion_nll(model, tin, vt[full], va["src"][full],
                                      va["year"][full].astype(np.float32), order,
                                      4096, device, amp=amp)
            per_rec = nll.sum(axis=1)
            per_rec_w = nll[:, wealth].sum(axis=1)
            pop_w = va["records"]["weight"].to_numpy(float)[va["rows"]][full]
            per_var = {names[j]: float(nll[:, j].mean()) for j in range(V) if scf_only[j]}
            comp = dict(n_records=int(full.sum()),
                        nll_per_record=float(per_rec.mean()),
                        nll_per_variable=float(per_rec.mean() / scf_only.sum()),
                        # The 24 wealth amounts only (comparable whether or not
                        # the weight is a variable), unweighted and weighted by
                        # the records' population weights.
                        wealth_nll_per_record=float(per_rec_w.mean()),
                        wealth_nll_per_record_popweighted=float(np.average(per_rec_w,
                                                                           weights=pop_w)),
                        by_variable=per_var)
            print(f"validation completion NLL of SCF-only variables: "
                  f"{comp['nll_per_record']:.4f} per record over {comp['n_records']} records "
                  f"({cp.name}, epoch {ck.get('epoch')})", flush=True)
            comp["epoch"] = ck.get("epoch")
    return comp


def _finish(a, out, sources, keep, n_params, gpu_name, counts, years, V, vocab,
            steps_per_epoch, epoch, best_epoch, best, train_seconds, comp, dropped,
            summary_extra):

    summary = dict(tag=a.tag, args=vars(a), sources=sources, trained_sources=keep or sources,
                   n_params=n_params, gpu=gpu_name, counts=counts, years=years,
                   n_variables=V, total_tokens=int(sum(vocab)),
                   steps_per_epoch=steps_per_epoch, epochs_run=epoch,
                   best_epoch=best_epoch, best_val_loss=best,
                   train_seconds=train_seconds, val_completion=comp,
                   dropped_variables=dropped, loss_weight=a.loss_weight,
                   slurm_job_id=os.environ.get("SLURM_JOB_ID"), **summary_extra)
    with open(out / f"run_{a.tag}.json", "w") as f:
        json.dump(summary, f, indent=2)
    print(json.dumps({k: v for k, v in summary.items() if k != "args"}, indent=2), flush=True)


if __name__ == "__main__":
    main()
