"""
02_size_sweep.py  (minimal example, step 2: how held-out loss changes with network size)

What it does
  Trains the step 2 model at five sizes on the same data, split, seed and
  training recipe as the v2 run (02_model.py: masked any-subset, plain
  weights, AdamW at a peak rate of 0.001, three warm-up epochs, cosine decay
  to 5% of the peak over 80 epochs, early stopping on validation loss with
  patience 12). Only the network's width and depth change. The attention
  heads are kept at 32 numbers each (heads = width / 32) and the
  feed-forward layer at twice the width, as in the v2 network (width 128,
  four heads, feed-forward 256).

      name      layers  width  parameters
      L3_W64       3      64     140,350
      L3_W96       3      96     283,998
      L4_W128      4     128     609,278   (the v2 network)
      L5_W160      5     160   1,131,870
      L6_W224      6     224   2,561,726

  For each size it records the parameter count, training minutes, best
  epoch, the training and validation loss at the best epoch, and the test
  cross-entropy per variable and source (one hidden, half hidden, all
  hidden, and the plain-distribution baseline), scored with the same
  functions and seed as 02_evaluate.py.

  Two training losses are kept. The one in train_log.csv is the running
  average during each epoch, with dropout on and fresh hide draws, which is
  what 02_REPORT.md quotes. The overfitting gap uses a second one: the best
  weights scored on the training split exactly as validation is scored
  (dropout off, four fixed hide draws per record), so the two numbers differ
  only in which records they cover. Four draws turned out too noisy (the
  same validation records moved 0.010 between two sets of draws), so
  `rescore` scores the gap again with 32 draws and with one slot hidden at a
  time; those are the gap numbers in 02_SIZE_SWEEP.md.

What it writes (one folder per size, never in model/ itself)
  /nfs/roberts/scratch/pi_nrs36/jar335/minimal/model/sweep/<name>/
      model.pt, train_log.csv, result.json
  and, from `summarize`, sweep/summary.json and sweep/summary.md.

Run through Slurm: sbatch research/rebuild/minimal/02_size_sweep.sbatch
(a five-task array; then `python 02_size_sweep.py summarize`, which only
reads the result files and needs no GPU).
"""
import json
import os
import sys
import time

SWEEP_DIR = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/model/sweep'
SIZES = [  # (name, layers, width)
    ('L3_W64', 3, 64),
    ('L3_W96', 3, 96),
    ('L4_W128', 4, 128),
    ('L5_W160', 5, 160),
    ('L6_W224', 6, 224),
]
GAP_SEED = 20261018   # fixed hide draws for scoring the training split


def settings_of(layers, width):
    return dict(width=width, layers=layers, heads=width // 32, ffn=2 * width, dropout=0.1)


# ---- Training and scoring one size (GPU job) ----------------------------------
def run_one(index):
    import functools
    import importlib.util
    import math

    import numpy as np
    import pandas as pd
    import torch

    here = os.path.dirname(os.path.abspath(__file__))
    spec = importlib.util.spec_from_file_location('evaluate02', os.path.join(here, '02_evaluate.py'))
    ev = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(ev)
    m = ev.m   # the 02_model.py module that 02_evaluate.py loaded

    name, layers, width = SIZES[index]
    settings = settings_of(layers, width)
    out_dir = os.path.join(SWEEP_DIR, name)
    os.makedirs(out_dir, exist_ok=True)
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    max_epochs = int(os.environ.get('MINIMAL_MAX_EPOCHS', '80'))

    table = m.read_table()
    data = {s: m.encode(table[table.split == s], device) for s in ['train', 'validation']}

    # m.train() builds the network as SlotTransformer() with the v2 defaults.
    # Swap in a constructor with this size's settings for the call, so the
    # training loop itself is the unchanged v2 code.
    original = m.SlotTransformer
    m.SlotTransformer = functools.partial(original, **settings)
    try:
        began = time.time()
        model, log = m.train(data['train'], data['validation'], max_epochs)
        minutes = (time.time() - began) / 60
    finally:
        m.SlotTransformer = original
    assert model.settings == settings
    log.to_csv(os.path.join(out_dir, 'train_log.csv'), index=False)
    torch.save({'state': model.state_dict(), 'settings': model.settings,
                'band_layout': m.BAND_LAYOUT}, os.path.join(out_dir, 'model.pt'))
    best = log.loc[log.val_loss.idxmin()]
    model.eval()

    @torch.no_grad()
    def fixed_draw_loss(split_data, seed, draws=4, chunk=32768):
        """Weighted mean cross-entropy over hidden slots, dropout off, with
        `draws` fixed hide draws per record: how validation is scored in
        m.train(). Done in chunks so the training split fits in memory."""
        gen = torch.Generator(device=device).manual_seed(seed)
        values, weight = split_data['values'], split_data['weight']
        hidden = [m.draw_hidden(values, gen) for _ in range(draws)]
        losses = []
        for h in hidden:
            total, wsum = 0.0, 0.0
            for start in range(0, len(values), chunk):
                v, w, hh = values[start:start + chunk], weight[start:start + chunk], h[start:start + chunk]
                nll = m.slot_nll(model(v, hh), v)
                ww = w[:, None] * hh
                total += float((nll * ww).sum())
                wsum += float(ww.sum())
            losses.append(total / wsum)
        return float(np.mean(losses))

    train_fixed = fixed_draw_loss(data['train'], GAP_SEED)
    val_fixed = fixed_draw_loss(data['validation'], GAP_SEED)

    # Test split: the same scoring as 02_evaluate.py (same functions, same seed).
    table['w'] = m.training_weights(table)
    test_ce = {}
    for source in m.SOURCES:
        sel = table.source == source
        train_rows = table[sel & (table.split == 'train')]
        test_rows = table[sel & (table.split == 'test')].reset_index(drop=True)
        values = m.encode(test_rows, device)['values']
        weight = torch.tensor(test_rows['w'].to_numpy(), dtype=torch.float32, device=device)
        ce, _ = ev.score_records(model, values, weight, ev.EVAL_SEED)
        for var in ce:
            ce[var]['marginal'] = ev.marginal_baseline(
                pd.DataFrame({var: ev.column_of(train_rows, var), 'w': train_rows['w']}),
                pd.DataFrame({var: ev.column_of(test_rows, var), 'w': test_rows['w']}), var)
        test_ce[source] = ce

    result = dict(
        name=name, layers=layers, width=width, settings=settings,
        parameters=m.count_parameters(model), minutes=minutes, device=device,
        gpu=torch.cuda.get_device_name(0) if device == 'cuda' else None,
        epochs_run=len(log), max_epochs=max_epochs, best_epoch=int(best.epoch),
        early_stopped=bool(len(log) < max_epochs),
        train_loss_logged=float(best.train_loss),     # running, dropout on
        val_loss=float(best.val_loss),                # as logged by m.train()
        train_loss_fixed=train_fixed,                 # dropout off, fixed draws
        val_loss_fixed=val_fixed,                     # same, other draws seed
        gap_train_minus_val=train_fixed - val_fixed,
        lr_at_best=float(best.lr), seed=m.SEED, eval_seed=ev.EVAL_SEED,
        test_cross_entropy=test_ce)
    with open(os.path.join(out_dir, 'result.json'), 'w') as f:
        json.dump(result, f, indent=2)
    print(json.dumps({k: v for k, v in result.items() if k != 'test_cross_entropy'}, indent=2))


# ---- Rescoring the gap (GPU job, after training) ---------------------------------
def rescore():
    """Scores each saved network on the training and validation splits again,
    two ways, and adds the results to its result.json.

    Why: in the first finished run the same validation records, scored with
    two different sets of four hide draws, gave losses 0.010 apart. Survey
    weights are uneven, so a few heavy records decide much of the loss and
    the random hiding moves it. That is as large as the gap being measured.
      (a) the training-recipe loss with 32 fixed hide draws per record
          instead of 4;
      (b) one slot hidden at a time, for every slot a record has: no random
          hiding at all. This is the pure conditional fit, where memorizing
          the training records would show first.
    Both are scored with dropout off on the best weights."""
    import importlib.util

    import numpy as np
    import torch

    here = os.path.dirname(os.path.abspath(__file__))
    spec = importlib.util.spec_from_file_location('model02', os.path.join(here, '02_model.py'))
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    table = m.read_table()
    data = {s: m.encode(table[table.split == s], device) for s in ['train', 'validation']}
    nc, chunk = len(m.CONTEXT), 32768

    @torch.no_grad()
    def scores(model, split_data, draws=32):
        values, weight = split_data['values'], split_data['weight']
        gen = torch.Generator(device=device).manual_seed(GAP_SEED)
        recipe = []
        for _ in range(draws):
            h = m.draw_hidden(values, gen)
            total, wsum = 0.0, 0.0
            for a in range(0, len(values), chunk):
                v, w, hh = values[a:a + chunk], weight[a:a + chunk], h[a:a + chunk]
                ww = w[:, None] * hh
                total += float((m.slot_nll(model(v, hh), v) * ww).sum())
                wsum += float(ww.sum())
            recipe.append(total / wsum)
        present = values[:, nc:] >= 0
        total, wsum = 0.0, 0.0
        for j in range(len(m.PREDICTED)):
            for a in range(0, len(values), chunk):
                v, w, pr = values[a:a + chunk], weight[a:a + chunk], present[a:a + chunk]
                hh = torch.zeros_like(pr)
                hh[:, j] = pr[:, j]
                ww = w[:, None] * hh
                total += float((m.slot_nll(model(v, hh), v) * ww).sum())
                wsum += float(ww.sum())
        return float(np.mean(recipe)), float(np.std(recipe) / np.sqrt(draws)), total / wsum

    for name, _, _ in SIZES:
        path = os.path.join(SWEEP_DIR, name, 'result.json')
        if not os.path.exists(path):
            print(f'missing: {path}')
            continue
        saved = torch.load(os.path.join(SWEEP_DIR, name, 'model.pt'), map_location=device)
        model = m.SlotTransformer(**saved['settings']).to(device)
        model.load_state_dict(saved['state'])
        model.eval()
        tr, tr_se, tr_one = scores(model, data['train'])
        va, va_se, va_one = scores(model, data['validation'])
        with open(path) as f:
            result = json.load(f)
        result.update(train_loss_32=tr, train_loss_32_draw_se=tr_se,
                      val_loss_32=va, val_loss_32_draw_se=va_se,
                      gap_32=tr - va, train_one_hidden=tr_one, val_one_hidden=va_one,
                      gap_one_hidden=tr_one - va_one)
        with open(path, 'w') as f:
            json.dump(result, f, indent=2)
        print(name, json.dumps({k: result[k] for k in ['train_loss_32', 'val_loss_32', 'gap_32',
                                                       'train_one_hidden', 'val_one_hidden',
                                                       'gap_one_hidden']}))


# ---- Summary (no GPU, reads the result files) -----------------------------------
def summarize():
    results = []
    for name, _, _ in SIZES:
        path = os.path.join(SWEEP_DIR, name, 'result.json')
        if not os.path.exists(path):
            print(f'missing: {path}')
            continue
        with open(path) as f:
            results.append(json.load(f))
    if not results:
        return
    lines = ['### Size against loss (nats per hidden slot)', '',
             'Logged: the losses m.train() printed at the best epoch (training loss is the '
             'running average with dropout on). Scored: best weights, dropout off, 32 fixed '
             'hide draws per record (recipe) or one slot hidden at a time (one hidden).', '',
             '| layers | width | parameters | minutes | best epoch / run | train (logged) '
             '| validation (logged) | train (recipe) | validation (recipe) | gap (recipe) '
             '| train (one hidden) | validation (one hidden) | gap (one hidden) |',
             '|' + '---|' * 13]
    for r in results:
        g = lambda k: f"{r[k]:.4f}" if k in r else 'n/a'
        gp = lambda k: f"{r[k]:+.4f}" if k in r else 'n/a'
        lines.append(f"| {r['layers']} | {r['width']} | {r['parameters']:,} | {r['minutes']:.1f} "
                     f"| {r['best_epoch']} / {r['epochs_run']} | {r['train_loss_logged']:.4f} "
                     f"| {r['val_loss']:.4f} | {g('train_loss_32')} | {g('val_loss_32')} "
                     f"| {gp('gap_32')} | {g('train_one_hidden')} | {g('val_one_hidden')} "
                     f"| {gp('gap_one_hidden')} |")
    for source in ['puf', 'scf']:
        for case, label in [('one', 'one hidden'), ('all', 'all hidden'), ('half', 'half hidden')]:
            variables = list(results[0]['test_cross_entropy'][source])
            head = ' | '.join(f"{r['parameters'] / 1e6:.2f}M" for r in results)
            lines += ['', f'### {source.upper()} test records, {label} (nats)', '',
                      f'| variable | {head} | baseline |',
                      '|---|' + '---|' * (len(results) + 1)]
            for var in variables:
                cells = ' | '.join(f"{r['test_cross_entropy'][source][var][case]:.3f}"
                                   for r in results)
                base = results[0]['test_cross_entropy'][source][var]['marginal']
                lines.append(f'| {var} | {cells} | {base:.3f} |')
    text = '\n'.join(lines) + '\n'
    with open(os.path.join(SWEEP_DIR, 'summary.md'), 'w') as f:
        f.write(text)
    with open(os.path.join(SWEEP_DIR, 'summary.json'), 'w') as f:
        json.dump(results, f, indent=2)
    print(text)


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == 'summarize':
        summarize()
    elif len(sys.argv) > 1 and sys.argv[1] == 'rescore':
        rescore()
    else:
        run_one(int(sys.argv[1]) if len(sys.argv) > 1 else int(os.environ['SLURM_ARRAY_TASK_ID']))
