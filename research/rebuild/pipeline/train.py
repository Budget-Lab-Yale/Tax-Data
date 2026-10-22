"""Pretraining: the transformer on the PUF, SCF and CEX table, with two slots per translated item.

Masked any-subset training (model.py) on every record but a 2% held-back
slice, for a fixed number of passes, keeping the last.

Translation (settings.json, "translation"). Some SCF households that report
zero interest, dividends or capital gains hold some. Each of these items has
two slots on SCF rows: the reported amount (model.REPORTED, never changed)
and the PUF-concept amount (the shared slot), which reporters copy and
zero-reporters get filled in. From the third pass on, at the start of each
pass, the reporting-rate formula gives every SCF zero-reporter its chance of
a hidden holding in each size range:
  C_k  its chance of reporting range k: SCF label, the reported slot,
       everything else shown except every PUF-concept slot (so no fill-in
       ever feeds the formula);
  B_k  C_k averaged over SCF units of the same year, married, dependents and
       wage band (the type);
  A_k  the PUF concept given the type only (PUF label, the fitting year), over
       the PUF's units other than dependent filers: the item given the type
       and each adult age range, mixed by the model's chance of that range;
  chance of a hidden holding in k = C_k * max(A_k / B_k - 1, 0) / C_0,
  capped at 100% in total; within a range, bands follow A.
Each batch redraws the PUF-concept slot of its zero-reporters from those
chances. In the held-back slice no fill-in is drawn, so a zero-reporter's
PUF-concept slot is absent there (unknown: not shown, not scored). The
formula assumes a holder's chance of reporting depends only on its type and
true amount, not on anything else on the record.

Age range (ages.py). PUF records with no range carry step 0's probabilities;
a range is drawn from them for every training record at the start of each
pass, and once for the held-back ones.

Writes, in common.MODEL: model.pt, train_log.csv (loss per pass),
var_log.csv (held-back PUF holder shares and loss per variable, every few
passes), fill_log.csv (share with none after the fill-in against the PUF, per
pass), train_meta.json (settings).

Packing (settings "training": packing; REBUILD_PACK=0 or 1 overrides): each batch holds records of one source and carries only that source's
slots (model.SOURCE_SLOTS). Each pass splits every source's training rows into batches of the usual size
and shuffles the batches of all sources together, so each source gets updates in proportion to its rows.
Each packed batch's weighted cross-entropy is divided by (records in the batch) x (the average over all
training records of weight x expected hidden slots), the denominator a mixed batch has on average, so a
source's records count as much in the objective as in mixed batches. The held-back loss, the fill-in and
the per-variable log use the same packed forward pass; the held-back loss is the same quantity in both modes.
"""
import json
import math
import os
import time

import numpy as np
import pandas as pd
import torch

import common
import model as M

S = common.SETTINGS['translation']
T = dict(common.SETTINGS['training'], passes=common.PASSES)
PACK = os.environ.get('REBUILD_PACK', '1' if T.get('packing') else '0') == '1'   # settings "training", packing
PUF_SLOTS, SCF_SLOTS_ = M.SOURCE_SLOTS['puf'], M.SOURCE_SLOTS['scf']
# Precision of the training steps (settings "training": precision): fp32, standard 32-bit; tf32, large matrix
# multiplications at TF32's reduced internal precision, storage 32-bit; bf16, the forward pass under autocast
# in bfloat16, the log-probabilities, loss, gradients and weight updates in 32-bit. Only the training steps change:
# the fill-in refit, the held-back loss and the per-variable log always run in standard 32-bit.
PRECISION = T.get('precision', 'fp32')
assert PRECISION in ('fp32', 'tf32', 'bf16'), PRECISION


def training_precision(on):
    """Switch the training steps' precision on (on=True) or back to standard 32-bit."""
    torch.set_float32_matmul_precision('high' if on and PRECISION == 'tf32' else 'highest')


def autocast():
    return torch.autocast('cuda', dtype=torch.bfloat16, enabled=PRECISION == 'bf16')
MAPPED = list(S['mapped'])
FIT_YEAR = int(S['fitting_tax_year'])
INF = math.inf
RANGES = {v: [(-INF if lo is None else lo, INF if hi is None else hi) for lo, hi in S['size_ranges'][v]]
          for v in MAPPED}


def range_label(lo, hi):
    f = lambda x: f'${x:,.0f}'.replace('$-', '-$')
    if lo == -INF:
        return f'below {f(hi)}'
    if hi == INF:
        return f'{f(lo)} and over'
    return f'{f(lo)} to {f(hi)}'


def in_range(x, lo, hi):
    """Small positive ranges are (0, hi); every other range is [lo, hi)."""
    return 0 < x < hi if lo == 0 else lo <= x < hi


def range_of_band(edges, v):
    """Size range of every band of v by its 2015 PUF median: 0 = zero, 1..K = the ranges, K + 1 = a band with no PUF records."""
    out = []
    for bd in edges[f'puf/2015/{v}']['bands']:
        if bd['band'] == 0:
            out.append(0)
        elif bd['median'] is None:
            out.append(len(RANGES[v]) + 1)
        else:
            hit = [i for i, (lo, hi) in enumerate(RANGES[v]) if in_range(bd['median'], lo, hi)]
            assert len(hit) == 1, (v, bd)
            out.append(hit[0] + 1)
    return np.array(out, dtype=np.int64)


def main():
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(common.SEED)
    gen = torch.Generator(device=device).manual_seed(common.SEED)
    os.makedirs(common.MODEL, exist_ok=True)
    table = M.read_table()
    edges = json.load(open(common.EDGES))
    C, P = len(M.CONTEXT), M.PREDICTED
    NB = {v: M.N_CHOICES[v] for v in MAPPED}
    jl = {v: P.index(v) for v in MAPPED}
    RO_np = {v: range_of_band(edges, v) for v in MAPPED}
    RO = {v: torch.tensor(RO_np[v], device=device) for v in MAPPED}
    NR = {v: len(RANGES[v]) + 2 for v in MAPPED}

    def enc(part):
        d = M.encode(part, device)
        age_rows, age_probs = M.age_draws(part, device)
        return dict(values=d['values'], weight=d['weight'], years=part['year'].to_numpy(),
                    age_rows=age_rows, age_probs=age_probs,
                    srv=torch.tensor((part['source'] == 'scf').to_numpy(), device=device),
                    scf=torch.tensor((part['source'] == 'scf').to_numpy(), device=device),
                    puf=torch.tensor((part['source'] == 'puf').to_numpy(), device=device),
                    cex=torch.tensor((part['source'] == 'cex').to_numpy(), device=device),
                    rep_band={v: d['values'][:, C + jl[v]].clone() for v in MAPPED})
    tr = enc(table[table.split == 'train'])
    te = enc(table[table.split == 'test'])
    # A held-back SCF zero-reporter's PUF-concept amount is unknown (training fills it in; the held-back slice is
    # never filled), so it is absent there: not shown, not scored.
    for v in MAPPED:
        te['values'][te['scf'] & (te['rep_band'][v] == 0), C + jl[v]] = -1
    j_age = C + P.index('age_range')

    def draw_ages(d):
        """A range for every record with none, from step 0's probabilities."""
        if len(d['age_rows']):
            d['values'][d['age_rows'], j_age] = torch.multinomial(d['age_probs'], 1, generator=gen).squeeze(1)
    draw_ages(te)
    puf_rows = (table.source == 'puf').to_numpy()

    # ---- the fill-in: the reporting-rate formula reads the reported slots, fills the PUF-concept slots
    jr = {v: P.index(v + '_reported') for v in MAPPED}
    srv_idx = tr['srv'].nonzero().squeeze(1)
    pos = torch.full((len(tr['values']),), -1, dtype=torch.long, device=device)
    pos[srv_idx] = torch.arange(len(srv_idx), device=device)
    trs = table[table.split == 'train']
    tkey = torch.tensor(trs.groupby(['year', 'married', 'n_dep', 'wages_band'], sort=False).ngroup().to_numpy(),
                        device=device)[srv_idx]
    NT = int(tkey.max()) + 1
    w_s = tr['weight'][srv_idx].double()
    fitw_s = torch.tensor(trs['year'].to_numpy() == FIT_YEAR, device=device)[srv_idx]
    TYPE = ['married', 'n_dep', 'wages']
    FILL, fill_log = {}, []

    model = M.new_model(device)
    opt = torch.optim.AdamW(model.parameters(), lr=T['peak_learning_rate'], weight_decay=T['weight_decay'])
    n, bs = len(tr['values']), T['batch']
    src_rows = {src: tr[src].nonzero().squeeze(1) for src in M.SOURCES}
    spe = sum(math.ceil(len(r) / bs) for r in src_rows.values()) if PACK else math.ceil(n / bs)
    # packing: the average over training records of weight x expected hidden slots (ceil(u n) for u
    # uniform on (0, 1] has mean (n + 1) / 2), the denominator per record of a mixed batch on average
    n_present = (tr['values'][:, C:] >= 0).sum(1).double()
    per_record = float((tr['weight'].double() * (n_present + 1) / 2).mean())
    REFIT_CHUNK = M.safe_chunk(len(M.SLOTS))   # the full width: safe for the packed widths too
    gpu = torch.cuda.get_device_name() if device == "cuda" else "cpu"
    common.say(f'device {gpu}; precision {PRECISION}; refit chunk {REFIT_CHUNK} records')
    common.say(f'packing {PACK}; batches per pass {spe}; weight x expected hidden slots per record {per_record:.3f}')

    def batches(order_gen):
        """(slots, row indices) per batch for one pass."""
        if not PACK:
            order = torch.randperm(n, generator=order_gen, device=device)
            return [(None, order[a:a + bs]) for a in range(0, n, bs)]
        out = []
        for src, rows in src_rows.items():
            rows = rows[torch.randperm(len(rows), generator=order_gen, device=device)]
            out += [(M.SOURCE_SLOTS[src], rows[a:a + bs]) for a in range(0, len(rows), bs)]
        return [out[k] for k in torch.randperm(len(out), generator=order_gen, device=device).tolist()]

    def held_back_loss(hidden_draws):
        """Overall held-back loss (mean of the draws) and by source; the same quantity packed or not."""
        overall, by_src = [], {s: [] for s in M.SOURCES}
        for h in hidden_draws:
            num, den = {}, {}
            for src in M.SOURCES:
                r = te[src].nonzero().squeeze(1)
                vals, hid = te['values'][r], h[r]
                sl = M.SOURCE_SLOTS[src] if PACK else None
                if PACK:
                    vals, hid = M.pack(vals, hid, sl)
                ch = M.safe_chunk(vals.shape[1])
                sums = [M.masked_sums(model, vals[i:i + ch], te['weight'][r][i:i + ch], hid[i:i + ch], sl)
                        for i in range(0, len(vals), ch)]
                num[src], den[src] = sum(float(a) for a, _ in sums), sum(float(b) for _, b in sums)
            overall.append(sum(num.values()) / sum(den.values()))
            for src in M.SOURCES:
                by_src[src].append(num[src] / den[src])
        return float(np.mean(overall)), {s: float(np.mean(x)) for s, x in by_src.items()}
    total, warm, floor = T['passes'] * spe, T['warmup_passes'] * spe, T['final_learning_rate_share']
    sched = torch.optim.lr_scheduler.LambdaLR(
        opt, lambda s: (s + 1) / warm if s < warm else
        floor + (1 - floor) * 0.5 * (1 + math.cos(math.pi * (s - warm) / max(1, total - warm))))
    val_hidden = [M.draw_hidden(te['values'], gen) for _ in range(4)]

    def type_only(v, label):
        v = v.clone()
        v[:, 0] = M.SOURCES.index(label)
        if label == 'puf':
            v[:, 1] = M.YEARS.index(FIT_YEAR)
        hid = torch.zeros(len(v), len(P), dtype=torch.bool, device=device)
        for j, sl in enumerate(P):
            if label not in M.PRESENT_ON[sl]:
                v[:, C + j] = -1
            else:
                v[:, C + j] = v[:, C + j].clamp(min=0)
                hid[:, j] = sl not in TYPE
        return v, hid

    def puf_log_probs(va, ha):
        """The network on a PUF-label view; returns a function from a PREDICTED index to its log-probabilities."""
        if PACK:
            vp, hp = M.pack(va, ha, PUF_SLOTS)
            out, idx = model(vp, hp, PUF_SLOTS), M.predicted_of(PUF_SLOTS)
            return lambda j: out[:, idx.index(j)]
        out = model(va, ha)
        return lambda j: out[:, j]

    j_age_p = P.index('age_range')

    def puf_at_type(vals):
        """A for every mapped item: the PUF concept given the type only (PUF label, the fitting year), over the
        PUF's units other than dependent filers, which are outside any comparison of tax units with a survey.
        The model's chance of each of the six adult age ranges given the type, rescaled to sum to one, times
        the item's distribution given the type and that range."""
        va, ha = type_only(vals, 'puf')
        age = puf_log_probs(va, ha)(j_age_p)[:, :6].exp().double()
        age = age / age.sum(1, keepdim=True)
        A = {v: 0.0 for v in MAPPED}
        for k in range(6):
            vk, hk = va.clone(), ha.clone()
            vk[:, C + j_age_p], hk[:, j_age_p] = k, False
            lp = puf_log_probs(vk, hk)
            for v in MAPPED:
                A[v] = A[v] + age[:, k:k + 1] * lp(jl[v])[:, :NB[v]].exp().double()
        return A

    @torch.no_grad()
    def refit(epoch):
        """Each SCF record's fill-in distribution over the PUF-concept bands of each mapped item, from the
        formula: a zero-reporter's chance of a hidden holding in range k is C_k * e_k / C_0, with C its
        chance of reporting each range (SCF label, reported slot, every filled slot hidden), e_k = A_k / B_k - 1
        where positive, A the PUF concept given type only (no dependent filers: puf_at_type), B the average of
        C over its type (year, married, dependents, wage band); capped at 100%. Within a range, bands follow A."""
        model.eval()
        A_all, C_all = {v: [] for v in MAPPED}, {v: [] for v in MAPPED}
        for a in range(0, len(srv_idx), REFIT_CHUNK):
            vals = tr['values'][srv_idx[a:a + REFIT_CHUNK]]
            A = puf_at_type(vals)
            for v in MAPPED:
                A_all[v].append(A[v])
                hc = torch.zeros(len(vals), len(P), dtype=torch.bool, device=device)
                hc[:, jr[v]] = True
                for u in MAPPED:
                    hc[:, jl[u]] = True
                if PACK:
                    vs, hs = M.pack(vals, hc, SCF_SLOTS_)
                    pc = model(vs, hs, SCF_SLOTS_)[:, M.predicted_of(SCF_SLOTS_).index(jr[v]), :NB[v]].exp().double()
                else:
                    pc = model(vals, hc)[:, jr[v], :NB[v]].exp().double()
                C_all[v].append(torch.zeros(len(pc), NR[v], dtype=torch.float64, device=device).index_add_(1, RO[v], pc))
        for v in MAPPED:
            K = NR[v]
            Ab, Cr = torch.cat(A_all[v]), torch.cat(C_all[v])
            Ar = torch.zeros(len(Ab), K, dtype=torch.float64, device=device).index_add_(1, RO[v], Ab)
            num = torch.zeros(NT, K, dtype=torch.float64, device=device).index_add_(0, tkey, w_s[:, None] * Cr)
            den = torch.zeros(NT, dtype=torch.float64, device=device).index_add_(0, tkey, w_s)
            B = num[tkey] / den[tkey][:, None]
            e = (Ar / B.clamp_min(1e-12) - 1).clamp(min=0)
            e[:, 0], e[:, K - 1] = 0, 0
            h = Cr * e / Cr[:, :1].clamp_min(1e-12)
            H = h.sum(1)
            h = h / H.clamp(min=1)[:, None]
            dist = Ab * (h / Ar.clamp_min(1e-15))[:, RO[v]]
            dist[:, 0] = 1 - h.sum(1)
            FILL[v] = dist.clamp(min=0).float()
            zero = tr['rep_band'][v][srv_idx] == 0
            m = fitw_s
            wm = lambda x, mm: float((w_s[mm] * x[mm]).sum() / w_s[mm].sum())
            row = {'pass': epoch, 'item': v,
                   'none, reported (%)': 100 * wm(zero.double(), m),
                   'none, after fill (%)': 100 * wm(torch.where(zero, 1 - h.sum(1), torch.zeros_like(H)), m),
                   'none, PUF at SCF types (%)': 100 * wm(Ar[:, 0], m),
                   'zero-reporters over 100% (%)': 100 * wm((H > 1).double(), m & zero)}
            fill_log.append(row)
            common.say('  fill ' + '  '.join(f'{k} {x:.2f}' if isinstance(x, float) else f'{k} {x}' for k, x in row.items()))
        pd.DataFrame(fill_log).to_csv(os.path.join(common.MODEL, 'fill_log.csv'), index=False)
        model.train()

    @torch.no_grad()
    def draw_fills(idx):
        """PUF-concept slot of each SCF zero-reporter in the batch drawn from its fill-in distribution;
        reporters keep the reported amount (dollar rule: unchanged)."""
        p = pos[idx]
        sv = p >= 0
        for v in MAPPED:
            z = sv & (tr['rep_band'][v][idx] == 0)
            if not z.any():
                continue
            band = torch.multinomial(FILL[v][p[z]], 1, generator=gen).squeeze(1)
            tr['values'][idx[z], C + jl[v]] = band

    # ---- the per-variable log: held-back PUF records
    zero_band = {v: edges[f'puf/2015/{v}']['zero_band'] for v in M.AMOUNTS if f'puf/2015/{v}' in edges}
    logged = [v for v in zero_band if v != 'net_worth']
    var_log = []

    @torch.no_grad()
    def log_variables(epoch):
        """Held-back PUF records for the PUF items; held-back CEX records for the consumption categories."""
        model.eval()
        for src, names in [('puf', logged), ('cex', M.CEX_SLOTS)]:
            hb = te[src].nonzero().squeeze(1)
            log_source(epoch, names, te['values'][hb], te['weight'][hb].double(), M.SOURCE_SLOTS[src] if PACK else None)
        pd.DataFrame(var_log).to_csv(os.path.join(common.MODEL, 'var_log.csv'), index=False)
        model.train()

    def log_source(epoch, names, hb_vals, hb_w, slots=None):
        if slots is not None:
            hb_vals = hb_vals[:, M.slot_tensor(slots, device)]
        present = hb_vals[:, C:] >= 0
        for v in names:
            zb = zero_band.get(v, 0)
            j = P.index(v) if slots is None else M.predicted_of(slots).index(P.index(v))
            ok = present[:, j]
            only = torch.zeros_like(present)
            only[:, j] = True
            row = {'pass': epoch, 'variable': v}
            actual = (hb_w * (ok & (hb_vals[:, C + j] != zb))).sum() / (hb_w * ok).sum()
            row['holders, actual (%)'] = 100 * float(actual)
            for lab, hid in [('only this hidden', only & present), ('all hidden', present)]:
                p0 = torch.cat([model(hb_vals[i:i + 8192], hid[i:i + 8192], slots)[:, j, :NB.get(v, M.N_CHOICES[v])]
                                .log_softmax(1)[:, zb].exp() for i in range(0, len(hb_vals), 8192)])
                row[f'holders, {lab} (%)'] = 100 * float((hb_w * ok * (1 - p0.double())).sum() / (hb_w * ok).sum())
            var_log.append(row)

    log, began = [], time.time()
    cuda = device == 'cuda'
    sync = (lambda: torch.cuda.synchronize()) if cuda else (lambda: None)
    for epoch in range(1, T['passes'] + 1):
        model.train()
        if cuda:
            torch.cuda.reset_peak_memory_stats()
        sync()
        t0 = time.time()
        plan = batches(gen)
        tot = 0.0
        draw_ages(tr)
        fill = epoch >= S['fill_from_pass']
        if fill:
            refit(epoch)
        sync()
        t_refit = time.time() - t0
        training_precision(True)
        for slots, idx in plan:
            if fill:
                draw_fills(idx)
            values, weight = tr['values'][idx], tr['weight'][idx]
            with autocast():
                if slots is None:
                    loss = M.masked_loss(model, values, weight, M.draw_hidden(values, gen))
                else:
                    values = values[:, M.slot_tensor(slots, device)]
                    loss = M.masked_loss(model, values, weight, M.draw_hidden(values, gen), slots,
                                         denominator=len(idx) * per_record)
            opt.zero_grad()
            loss.backward()
            opt.step()
            sched.step()
            tot += loss.item() * len(idx)
        training_precision(False)
        sync()
        t_batches = time.time() - t0 - t_refit
        model.eval()
        with torch.no_grad():
            val, val_src = held_back_loss(val_hidden)
        sync()
        row = dict(epoch=epoch, train_loss=tot / n, held_back_loss=val, seconds=time.time() - t0,
                   seconds_refit=t_refit, seconds_batches=t_batches,
                   peak_gpu_gb=torch.cuda.max_memory_allocated() / 1e9 if cuda else float('nan'),
                   **{f'held_back_{s}': x for s, x in val_src.items()})
        log.append(row)
        pd.DataFrame(log).to_csv(os.path.join(common.MODEL, 'train_log.csv'), index=False)
        common.say(f'pass {epoch:3d}  train {tot / n:.4f}  held back {val:.4f} '
                   + ' '.join(f'{s} {x:.4f}' for s, x in val_src.items())
                   + f'  {row["seconds"]:.1f}s (fill-in {t_refit:.1f}s, batches {t_batches:.1f}s)  peak {row["peak_gpu_gb"]:.2f} GB')
        if epoch == 1 or epoch % T['log_every_passes'] == 0 or epoch == T['passes']:
            log_variables(epoch)
    pd.DataFrame(log).to_csv(os.path.join(common.MODEL, 'train_log.csv'), index=False)
    M.save_model(model, os.path.join(common.MODEL, 'model.pt'))
    meta = dict(seed=common.SEED, settings=common.SETTINGS, packing=PACK, precision=PRECISION, gpu=gpu,
                node=os.environ.get('SLURMD_NODENAME'), job=os.environ.get('SLURM_JOB_ID'),
                minutes=(time.time() - began) / 60, last_held_back_loss=log[-1]['held_back_loss'],
                rows=table.groupby(['source', 'year', 'split']).size().to_dict().__repr__())
    json.dump(meta, open(os.path.join(common.MODEL, 'train_meta.json'), 'w'), indent=1, default=float)


if __name__ == '__main__':
    main()
