"""
22_report.py  (step 22, the fake-survey test; called by
`22_fake_survey.py report`). Reads every fake22/<arm>_<D>_s<seed>/score.json
and writes fake22/report.md: per distortion, other income of half-A returns
by group (truth against each method, 2015 dollars, the year of the test,
mean over seeds), the log score, the learned reporting settings against the
planted ones, and how well the fill-ins pick out the hidden holders.
"""
import glob
import json
import math
import os

import numpy as np

R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = os.environ.get('MINIMAL_F22_OUT', f'{R}/fake22')
ARMS = ['oracle', 'rule', 'rank', 'prefill', 'nofill', 'latent_fixed', 'latent_learn']
NAMES = {'oracle': 'undistorted survey', 'rule': "today's rule", 'rank': 'ranks',
         'prefill': 'fill in once before training', 'nofill': 'no label, no fill-in',
         'latent_fixed': 'latent, settings fixed', 'latent_learn': 'latent, settings learned'}
GROUPS = ['all', 'interest: none', 'interest: $1-499', 'interest: $500-4,999', 'interest: $5,000+',
          'dividends: none', 'dividends: $1-499', 'dividends: $500-4,999', 'dividends: $5,000+',
          'interest and dividends both none', 'wages 0', 'wages 1-25k', 'wages 25-50k',
          'wages 50-100k', 'wages 100-200k', 'wages 200k+']
TEXT = {'D1': 'small holders report zero; dollars right',
        'D2': 'small holders report zero; reporters give 0.8 of true dollars',
        'D3': 'small holders report zero, more so if other income is below the median (hidden holders poorer)',
        'D4': 'small holders report zero; one draw decides both interest and dividends'}


def sig(z):
    return 1 / (1 + math.exp(-z))


def money(x):
    return f'{x:,.0f}'


def main():
    scores = {}
    for f in sorted(glob.glob(f'{OUT}/*/score.json')):
        s = json.load(open(f))
        scores.setdefault((s['D'], s['arm']), []).append(s)
    lines = ['# Step 22: fake survey, results tables', '',
             'Other income of the half-A returns (the PUF pile), which the model never saw, against '
             "each method's expected other income given the return's true married, dependents and "
             'four incomes. Dollars are 2015 dollars (the year of this test). Each method cell is the '
             'mean over seeds; the number of seeds is in the log-score table.', '']
    oracle = scores.get(('D0', 'oracle'), [])
    for D in ['D1', 'D2', 'D3', 'D4']:
        arms = [a for a in ARMS[1:] if (D, a) in scores]
        if not arms:
            continue
        cols = (['oracle'] if oracle else []) + arms
        get = lambda a: oracle if a == 'oracle' else scores[(D, a)]
        lines += [f'## {D}: {TEXT[D]}', '']
        ref = get(cols[0])[0]['groups']
        tables = [('Mean other income by group, each amount held between the 1st and 99th percentiles ($)',
                   'truth, held 1st-99th pct', 'model, held 1st-99th pct', 'money'),
                  ('Share with any other income (%)', 'share with any, truth (%)', 'share with any, model (%)', 'pct'),
                  ('Mean other income by group, plain ($; swung by a few huge amounts)', 'truth', 'model', 'money')]
        for title, tkey, mkey, kind in tables:
            lines += [f'### {title}', '',
                      '| Group | Returns (m) | Truth | ' + ' | '.join(NAMES[a] for a in cols) + ' |',
                      '|---|---:|---:|' + '---:|' * len(cols)]
            for g in (GROUPS if kind != 'money' or 'held' in title else GROUPS[:1] + GROUPS[1:2] + GROUPS[9:10]):
                t = ref[g][tkey]
                row = [g, f"{ref[g]['returns (weighted, millions)']:.1f}", money(t) if kind == 'money' else f'{t:.1f}']
                for a in cols:
                    m = np.mean([s['groups'][g][mkey] for s in get(a)])
                    row.append(f'{money(m)} ({100 * (m / t - 1):+.0f}%)' if kind == 'money'
                               else f'{m:.1f} ({m - t:+.1f})')
                lines.append('| ' + ' | '.join(row) + ' |')
            lines.append('')
        lines += ['### Log score of the true other-income slice (nats; lower is better)', '',
                  '| | ' + ' | '.join(NAMES[a] for a in cols) + ' |', '|---|' + '---:|' * len(cols)]
        for g in ['all', 'interest: none', 'interest and dividends both none']:
            row = [g]
            for a in cols:
                v = [s['groups'][g]['log score'] for s in get(a)]
                row.append(f'{np.mean(v):.4f}' + (f' ({min(v):.4f}-{max(v):.4f}, {len(v)} seeds)' if len(v) > 1 else ''))
            lines.append('| ' + ' | '.join(row) + ' |')
        learned = scores.get((D, 'latent_learn'), [])
        if learned:
            pl = learned[0]['planted']
            lines += ['', '### Reporting settings: planted against learned (latent, settings learned)', '',
                      '| Item | Setting | Planted | ' + ' | '.join(f"seed {s['seed']}" for s in learned) + ' |',
                      '|---|---|---:|' + '---:|' * len(learned)]
            for v in ['interest', 'dividends']:
                for x in [50, 500, 5000]:
                    planted = sig(pl['alpha'] + pl['slope'] * math.log(x))
                    got = [sig(s['learned_settings'][v]['alpha'] + s['learned_settings'][v]['beta'] * math.log(x))
                           for s in learned]
                    lines.append(f'| {v} | chance of reporting at ${x:,} | {100 * planted:.0f}% | '
                                 + ' | '.join(f'{100 * q:.0f}%' for q in got) + ' |')
                lines.append(f"| {v} | dollar factor | {pl['factor']:.2f} | "
                             + ' | '.join(f"{s['learned_settings'][v]['factor']:.3f}" for s in learned) + ' |')
            if D == 'D3':
                lines.append('')
                lines.append(f"D3's planted chance is shifted by ±{pl['shift_d3']} on the log-odds scale by "
                             'other income; the row shows the unshifted curve.')
        fills = [(a, s) for a in ['latent_fixed', 'latent_learn'] for s in scores.get((D, a), [])
                 if s.get('fill_diagnostics')]
        if fills:
            lines += ['', "### Survey zeros: the model's share of hidden holders and how well it ranks them", '',
                      '| Method | Seed | Item | Hidden share, model | Hidden share, truth | Ranking (AUC) |',
                      '|---|---|---|---:|---:|---:|']
            for a, s in fills:
                for v, d in s['fill_diagnostics'].items():
                    lines.append(f"| {NAMES[a]} | {s['seed']} | {v} | {d['hidden holder share, model (%)']:.1f}% | "
                                 f"{d['hidden holder share, truth (%)']:.1f}% | {d['ranking quality (AUC)']:.3f} |")
        lines.append('')
    open(f'{OUT}/report.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
