"""Train the masked record transformer (part B of SPEC.md).

Any-order masked training: for each record at each step, draw a hiding
rate r ~ U(0, 1), hide each variable with probability r (at least one),
and minimise the cross-entropy of the hidden variables given the visible
ones. Records are unweighted; the weight is itself a variable.

Validation uses a fixed set of masks (drawn once from --val-mask-seed,
several per record) so that the validation loss is comparable across
epochs and across model sizes. Training stops when validation loss has
not improved for --patience epochs; the checkpoint kept is the one with
the lowest validation loss.

Outputs in --out-dir, all suffixed with --tag:
  checkpoint_<tag>.pt   best-validation model
  train_log_<tag>.csv   per-epoch train and validation loss
  run_<tag>.json        settings, parameter count, timings, GPU, final losses
"""

import argparse
import csv
import json
import math
import os
import time
from pathlib import Path

import numpy as np
import torch

from model import (RecordTransformer, count_parameters, draw_hidden,
                   draw_hidden_order,
                   generation_order, load_meta, masked_ce, save_checkpoint,
                   sequential_nll, meta_variables)


def parse_args():
    p = argparse.ArgumentParser()
    p.add_argument("--data-dir", required=True)
    p.add_argument("--out-dir", required=True)
    p.add_argument("--tag", default="base")
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
    p.add_argument("--seed", type=int, default=20261008)
    p.add_argument("--val-mask-seed", type=int, default=777)
    p.add_argument("--val-masks", type=int, default=4,
                   help="fixed masks drawn per validation record")
    p.add_argument("--max-hours", type=float, default=10.0,
                   help="stop after this much wall time")
    p.add_argument("--no-amp", action="store_true")
    p.add_argument("--ema", type=float, default=0.999,
                   help="if > 0, keep an exponential moving average of the "
                        "weights with this decay and evaluate/save it")
    p.add_argument("--order-frac", type=float, default=0.0,
                   help="share of training records whose mask follows the "
                        "generation order (visible = a prefix of the order); "
                        "0 = pure any-order training as in SPEC.md")
    # Options added for the night-of-2026-10-08 experiments (EXPERIMENTS.md).
    # Defaults reproduce the original behaviour exactly.
    p.add_argument("--train-order", default=None,
                   help="comma-separated variable names: the order used by "
                        "--order-frac masks and by the sequential log-likelihood "
                        "(default: meta.json's order)")
    p.add_argument("--drop-token", default=None,
                   help="VAR=TOKEN: drop training and validation records whose "
                        "VAR has this token code (e.g. AGERANGE=0 keeps only "
                        "records with demographics); default: keep all")
    p.add_argument("--loss-weight-power", type=float, default=0.0,
                   help="if > 0, weight each training record's loss by "
                        "(record weight)^power, normalised to mean 1 over the "
                        "training records; validation loss stays unweighted. "
                        "0 = unweighted (original)")
    p.add_argument("--val-mask-mode", default="random", choices=["random", "train"],
                   help="random (original): any-order validation masks; train: "
                        "validation masks drawn like the training masks, i.e. a "
                        "share --order-frac of them follow --train-order, so early "
                        "stopping tracks the objective actually trained")
    return p.parse_args()


def check_tokens(name, arr, vocab):
    if arr.ndim != 2 or arr.shape[1] != len(vocab):
        raise ValueError(f"{name}: shape {arr.shape}, expected [*, {len(vocab)}]")
    bad_lo = (arr < 0).sum(axis=0)
    bad_hi = (arr >= np.asarray(vocab)[None, :]).sum(axis=0)
    if bad_lo.any() or bad_hi.any():
        raise ValueError(f"{name}: tokens out of range: below 0 {bad_lo.tolist()}, "
                         f"above vocab {bad_hi.tolist()}")


@torch.no_grad()
def evaluate(model, val_tok, val_hid, batch_size, amp):
    model.eval()
    s_tot, n_tot = 0.0, 0
    for i in range(0, val_tok.shape[0], batch_size):
        t = val_tok[i:i + batch_size]
        h = val_hid[i:i + batch_size]
        with torch.autocast("cuda", dtype=torch.bfloat16, enabled=amp):
            logits = model(t, h)
        _, s, n = masked_ce(logits, t, h)
        s_tot += s.item()
        n_tot += n.item()
    model.train()
    return s_tot / n_tot


def main():
    a = parse_args()
    out = Path(a.out_dir)
    out.mkdir(parents=True, exist_ok=True)
    data = Path(a.data_dir)
    meta = load_meta(data / "meta.json")
    vocab = [int(v["vocab_size"]) for v in meta_variables(meta)]
    V = len(vocab)

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    amp = (device.type == "cuda") and not a.no_amp
    gpu_name = torch.cuda.get_device_name(0) if device.type == "cuda" else "cpu"
    torch.manual_seed(a.seed)
    np.random.seed(a.seed)

    tr = np.load(data / "tokens_train.npy")
    va = np.load(data / "tokens_val.npy")
    check_tokens("train", tr, vocab)
    check_tokens("val", va, vocab)
    names = [v["name"] for v in meta_variables(meta)]

    rec_w = None
    if a.loss_weight_power > 0:
        # Record weights from values_train.parquet (same row order as
        # tokens_train.npy); checked against the decoded weight token.
        import pandas as pd
        wv = pd.read_parquet(data / "values_train.parquet", columns=["weight"])["weight"].to_numpy(float)
        if wv.shape[0] != tr.shape[0] or not np.all(np.isfinite(wv)) or (wv <= 0).any():
            raise ValueError(f"record weights: {wv.shape[0]} rows for {tr.shape[0]} records, "
                             f"non-finite {int((~np.isfinite(wv)).sum())}, non-positive {int((wv <= 0).sum())}")
        rec_w = wv

    if a.drop_token:
        var, tok = a.drop_token.split("=")
        j = names.index(var)
        keep_tr = tr[:, j] != int(tok)
        keep_va = va[:, j] != int(tok)
        print(f"--drop-token {a.drop_token}: keeping {int(keep_tr.sum())} of {tr.shape[0]} "
              f"training and {int(keep_va.sum())} of {va.shape[0]} validation records", flush=True)
        tr, va = tr[keep_tr], va[keep_va]
        if rec_w is not None:
            rec_w = rec_w[keep_tr]

    if rec_w is not None:
        rec_w = rec_w ** a.loss_weight_power
        rec_w = rec_w / rec_w.mean()
        print(f"loss weights (weight^{a.loss_weight_power}, mean 1): min {rec_w.min():.4f} "
              f"median {np.median(rec_w):.4f} max {rec_w.max():.4f}; effective share of "
              f"records {rec_w.sum() ** 2 / (rec_w ** 2).sum() / rec_w.size:.3f}", flush=True)
    print(f"train {tr.shape}, val {va.shape}, variables {V}, "
          f"total tokens {sum(vocab)}, GPU {gpu_name}", flush=True)

    tr_t = torch.as_tensor(tr, dtype=torch.long, device=device)
    va_t = torch.as_tensor(va, dtype=torch.long, device=device)

    # Fixed validation masks: --val-masks per record, same every epoch.
    g_val = torch.Generator(device=device)
    g_val.manual_seed(a.val_mask_seed)
    val_tok = va_t.repeat(a.val_masks, 1)
    val_hid = draw_hidden(val_tok.shape[0], V, g_val, device)

    model = RecordTransformer(meta, d_model=a.d_model, n_layers=a.layers,
                              n_heads=a.heads, ff_mult=a.ff_mult,
                              dropout=a.dropout).to(device)
    n_params = count_parameters(model)
    ema_model = None
    if a.ema > 0:
        from torch.optim.swa_utils import AveragedModel, get_ema_multi_avg_fn
        ema_model = AveragedModel(model, multi_avg_fn=get_ema_multi_avg_fn(a.ema))
    train_order = generation_order(meta)
    if a.train_order:
        train_order = [names.index(x) for x in a.train_order.split(",")]
        if sorted(train_order) != list(range(V)):
            raise ValueError("--train-order must list every variable exactly once")
    order_rank = torch.empty(V, dtype=torch.long, device=device)
    order_rank[torch.tensor(train_order, device=device)] = torch.arange(V, device=device)
    rec_w_t = (torch.as_tensor(rec_w, dtype=torch.float32, device=device)
               if rec_w is not None else None)
    if a.val_mask_mode == "train" and a.order_frac > 0:
        use_v = torch.rand(val_tok.shape[0], 1, generator=g_val, device=device) < a.order_frac
        val_hid = torch.where(use_v, draw_hidden_order(val_tok.shape[0], V, order_rank,
                                                       g_val, device), val_hid)
    print(f"parameters: {n_params:,}", flush=True)

    decay, no_decay = [], []
    for n_, p in model.named_parameters():
        (no_decay if p.ndim < 2 or "emb" in n_ or "bias" in n_ else decay).append(p)
    opt = torch.optim.AdamW([{"params": decay, "weight_decay": a.weight_decay},
                             {"params": no_decay, "weight_decay": 0.0}],
                            lr=a.lr, betas=(0.9, 0.98))
    steps_per_epoch = math.ceil(tr.shape[0] / a.batch_size)
    total_steps = steps_per_epoch * a.max_epochs

    def lr_at(step):
        if step < a.warmup_steps:
            return a.lr * (step + 1) / a.warmup_steps
        frac = (step - a.warmup_steps) / max(1, total_steps - a.warmup_steps)
        return a.lr * (0.1 + 0.9 * 0.5 * (1 + math.cos(math.pi * min(frac, 1.0))))

    g_train = torch.Generator(device=device)
    g_train.manual_seed(a.seed)
    log_path = out / f"train_log_{a.tag}.csv"
    ck_path = out / f"checkpoint_{a.tag}.pt"
    log_f = open(log_path, "w", newline="")
    log = csv.writer(log_f)
    log.writerow(["epoch", "train_loss", "val_loss", "lr", "epoch_seconds",
                  "elapsed_seconds", "best"])

    best, best_epoch, bad = float("inf"), -1, 0
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
            h = draw_hidden(t.shape[0], V, g_train, device)
            if a.order_frac > 0:
                use = torch.rand(t.shape[0], 1, generator=g_train, device=device) < a.order_frac
                h = torch.where(use, draw_hidden_order(t.shape[0], V, order_rank,
                                                       g_train, device), h)
            for gp in opt.param_groups:
                gp["lr"] = lr_at(step)
            with torch.autocast("cuda", dtype=torch.bfloat16, enabled=amp):
                logits = model(t, h)
            if rec_w_t is None:
                loss, s, n = masked_ce(logits, t, h)
            else:
                # Weighted mean over hidden slots: each hidden slot of record
                # i counts with weight w_i (mean-1 normalised).
                lp = torch.log_softmax(logits, dim=-1)
                nll = -lp.gather(-1, t.clamp(min=0)[..., None]).squeeze(-1)
                wh = h * rec_w_t[idx][:, None]
                s = (nll * wh).sum()
                n = wh.sum()
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
        val_loss = evaluate(eval_model, val_tok, val_hid, 4096, amp)
        is_best = val_loss < best
        if is_best:
            best, best_epoch, bad = val_loss, epoch, 0
            save_checkpoint(ck_path, eval_model, data / "meta.json",
                            dict(epoch=epoch, val_loss=val_loss, args=vars(a)))
        else:
            bad += 1
        elapsed = time.time() - t0
        log.writerow([epoch, f"{train_loss:.6f}", f"{val_loss:.6f}",
                      f"{lr_at(step):.3e}", f"{time.time() - te:.1f}",
                      f"{elapsed:.1f}", int(is_best)])
        log_f.flush()
        print(f"epoch {epoch:4d}  train {train_loss:.4f}  val {val_loss:.4f}"
              f"  {'*' if is_best else ' '}  {time.time() - te:.1f}s", flush=True)
        if bad >= a.patience:
            print(f"early stop: no improvement for {a.patience} epochs", flush=True)
            break
        if elapsed > a.max_hours * 3600:
            print("stopping on wall-time limit", flush=True)
            break
    log_f.close()
    train_seconds = time.time() - t0

    # Exact log-likelihood of the validation records under the generation
    # order, for the best checkpoint. Comparable across model sizes.
    ck = torch.load(ck_path, map_location=device, weights_only=False)
    model.load_state_dict(ck["state_dict"])
    model.eval()
    order = train_order
    ts = time.time()
    seq = sequential_nll(model, va, order, 4096, device, amp=amp)
    seq_seconds = time.time() - ts
    if not np.all(np.isfinite(seq)):
        print(f"warning: {int((~np.isfinite(seq)).sum())} validation records have "
              "non-finite sequential NLL", flush=True)

    summary = dict(tag=a.tag, args=vars(a), n_params=n_params, gpu=gpu_name,
                   n_train=int(tr.shape[0]), n_val=int(va.shape[0]),
                   n_variables=V, total_tokens=int(sum(vocab)),
                   steps_per_epoch=steps_per_epoch, epochs_run=epoch,
                   best_epoch=best_epoch, best_val_loss=best,
                   train_seconds=train_seconds,
                   val_seq_nll_per_record=float(np.mean(seq)),
                   val_seq_nll_per_variable=float(np.mean(seq) / V),
                   seq_nll_seconds=seq_seconds,
                   slurm_job_id=os.environ.get("SLURM_JOB_ID"))
    with open(out / f"run_{a.tag}.json", "w") as f:
        json.dump(summary, f, indent=2)
    print(json.dumps(summary, indent=2), flush=True)


if __name__ == "__main__":
    main()
