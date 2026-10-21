"""Business income split into parts: a resampling pass after generation.

John, 19 October (34_gengap.py). The model's answers for a variable given only
some of the record do not fit one joint distribution exactly, and a file
drawn once in a random order holds 3-5% fewer rare-part holders than the
same model, reading the same records in another order, expects. The
resampling pass uses only one kind of answer: each PUF variable redrawn
given every other variable on the record (the computed totals hidden), the
variables in a random order, the whole record swept several times; then the
computed totals (business income from its parts, capital gains from its
pieces) are recomputed. (Gibbs sampling, with the network's answers as the
conditionals; not guaranteed to settle on one distribution, so tested here.)

For each run (RESAMPLE_RUNS): the run's plain generated file (200,000
records, equal weights, 03_stack's check file) after 0 to SWEEPS sweeps.
Reported against the PUF side (all records, PUF weights): holders of each
rare part and of common items, joint patterns, married, mean business income
(2015 dollars, 04_dollars). Also the calibration of the question the pass
uses, on the real PUF records: the variable given everything except the
computed totals.

Output: $R/step34/resample.md, and the resampled files in
$R/gen/step34/<run>/resampled_<k>.parquet. GPU job, as 34_gengap.py.
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd
import torch

HERE = os.path.dirname(os.path.abspath(__file__))
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
RUNS = os.environ.get('RESAMPLE_RUNS', 'D0,D0s2,D0w,Ns1').split(',')
SWEEPS = int(os.environ.get('RESAMPLE_SWEEPS', '3'))
PLAIN = 'gen_puf_with_net_worth_ownerlabel_absent_ownfirst.parquet'
RARE = ['part_nonpass_inc', 'part_pass_inc', 'part_nonpass_loss', 'scorp_nonpass_inc', 'scorp_nonpass_loss',
        'rent_inc', 'rent_loss', 'estate_inc', 'farm', 'sch_c']
COMMON = ['wages', 'interest', 'dividends', 'capital_gains', 'unemployment', 'ss_taxable']


def load(name, f):
    spec = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def main():
    torch.set_grad_enabled(False)
    os.environ['MINIMAL_ALL_RECORDS'] = '1'
    G = load('gen03', '03_generate.py')
    M = G.m
    rep = load('rep34', '34_report.py')
    dollars = load('dollars', '04_dollars.py')
    device = 'cuda'
    edges = json.load(open(M.BAND_EDGES))
    C, P = len(M.CONTEXT), M.PREDICTED
    zb = lambda v: edges[f'puf/2015/{v}']['zero_band']
    totals = [P.index(t) for t in G.COMPUTED_EXTRA] + ([P.index('capital_gains')] if G.COMPUTED_TOTAL else [])
    own = [P.index(v) for v in G.OWN['puf'] if v != 'net_worth']

    real, wr = rep.real_puf_side()
    table = M.read_table()
    puf = table[table.source == 'puf'].reset_index(drop=True)
    real_vals = M.encode(puf, device)['values']
    real_w = puf['weight'].to_numpy(float)

    def stats(t, w):
        """Shares (%) and means on a table with bands, and dollars for business income."""
        out = {}
        for v in RARE + COMMON:
            out[f'holders: {v} (%)'] = 100 * w[t[v + '_band'].to_numpy() != zb(v)].sum() / w.sum()
        anyp = np.column_stack([t[v + '_band'].to_numpy() != zb(v) for v in rep.PARTS if v.startswith('part_')]).any(1)
        anys = np.column_stack([t[v + '_band'].to_numpy() != zb(v) for v in rep.PARTS if v.startswith('scorp_')]).any(1)
        out['both partnership and S corp items (%)'] = 100 * w[anyp & anys].sum() / w.sum()
        out['married (%)'] = 100 * w[t['married'].to_numpy() == 1].sum() / w.sum()
        out['filer (%)'] = 100 * w[t['filer'].to_numpy() == 1].sum() / w.sum()
        return out

    def bus_mean(t, w):
        d, _ = dollars.draw_dollars(t, dollars.puf_maps(2015))
        return float((w * d['business_income'].to_numpy(float)).sum() / w.sum())

    real_stats = stats(puf, real_w)
    real_bus = float((wr * real['business_income'].to_numpy(float)).sum() / wr.sum())

    def predict_probs(model, vals, hid, j):
        out = []
        for i in range(0, len(vals), 8192):
            lp = M.predict(model, vals[i:i + 8192], hid[i:i + 8192])[:, j, :M.N_CHOICES[P[j]]]
            out.append(lp.log_softmax(1).exp())
        return torch.cat(out)

    rows, calib = [], []
    for r in RUNS:
        model = M.load_model(device, f'{R}/model/step34/{r}')
        # calibration of the pass's question on real records: v given all but the totals
        present = real_vals[:, C:] >= 0
        for v in ['part_nonpass_inc', 'scorp_nonpass_inc', 'farm', 'rent_inc', 'sch_c', 'wages']:
            j = P.index(v)
            hid = torch.zeros_like(present)
            hid[:, j] = True
            hid[:, totals] = True
            p = predict_probs(model, real_vals, hid & present, j)
            h = (1 - p[:, zb(v)]).double().cpu().numpy()
            actual = real_w[puf[v + '_band'].to_numpy() != zb(v)].sum()
            calib.append({'run': r, 'variable': v,
                          'model, given all but the totals, ratio to PUF': (real_w * h).sum() / actual})
        plain = pd.read_parquet(f'{R}/gen/step34/{r}/{PLAIN}')
        plain = plain[plain.source == 'puf'].reset_index(drop=True)
        nw = plain['net_worth_band'].to_numpy()
        vals = M.encode(plain.assign(split='train', weight=1.0, net_worth_band=-1), device)['values'].clone()
        w = np.ones(len(plain))
        gen = torch.Generator(device=device).manual_seed(2026)
        cpu_gen = torch.Generator().manual_seed(2026)
        for sweep in range(SWEEPS + 1):
            if sweep > 0:
                order = torch.tensor(own)[torch.randperm(len(own), generator=cpu_gen)].tolist()
                present = vals[:, C:] >= 0
                for j in order:
                    hid = torch.zeros_like(present)
                    hid[:, j] = True
                    hid[:, totals] = True
                    p = predict_probs(model, vals, hid & present, j)
                    draw = torch.multinomial(p, 1, generator=gen).squeeze(1)
                    ok = present[:, j]
                    vals[ok, C + j] = draw[ok]
                if G.COMPUTED_TOTAL:
                    vals[:, C + P.index('capital_gains')] = G.computed_total_band(vals, gen)
                for tot in G.COMPUTED_EXTRA:
                    vals[:, C + P.index(tot)] = G.computed_extra_band(vals, gen, tot)
            t = G.to_table(vals.cpu()).assign(net_worth_band=nw, weight=1.0)
            if sweep > 0:
                t.to_parquet(f'{R}/gen/step34/{r}/resampled_{sweep}.parquet', index=False)
            s = stats(t, w)
            rows.append({'run': r, 'sweeps': sweep, **s, 'business income mean, ratio to PUF': bus_mean(t, w) / real_bus})
            print(r, sweep, {k: round(v, 3) for k, v in rows[-1].items() if k not in ('run',)}, flush=True)
        del model
        torch.cuda.empty_cache()
    df = pd.DataFrame(rows)
    ratio = df.copy()
    for k, v in real_stats.items():
        ratio[k] = df[k] / v
    ratio.columns = [c.replace(' (%)', ', ratio to PUF') if c in real_stats else c for c in ratio.columns]
    lines = ['# Resampling after generation', '',
             'Written by `34_resample.py`. Each run\'s plain generated file (200,000 records, equal weights), then '
             f'up to {SWEEPS} sweeps: every PUF variable redrawn, in a random order, given every other variable on '
             'the record with the computed totals hidden; the totals then recomputed. Ratios to the PUF side (all '
             'records, PUF weights). Business income: mean per unit, 2015 dollars. Standard error of a plain '
             'file\'s mean: about 0.03-0.05.', '',
             '## Calibration of the question the pass uses (real PUF records; 1.00 = right)', '',
             pd.DataFrame(calib).pivot(index='variable', columns='run',
                                       values='model, given all but the totals, ratio to PUF')
               .reset_index().to_markdown(index=False, floatfmt='.3f'), '',
             '## Ratios to the PUF by number of sweeps', '', ratio.to_markdown(index=False, floatfmt='.3f'), '',
             '## Shares (%)', '', pd.concat([pd.DataFrame([{'run': 'PUF', 'sweeps': '', **real_stats,
                                                           'business income mean, ratio to PUF': 1.0}]), df])
               .to_markdown(index=False, floatfmt='.3f'), '']
    open(f'{R}/step34/resample.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
