"""The translation report of a run, all four stages side by side (translate.py).

For every row of the translation table and every survey year: the survey as
reported, after function 1 (both from the table stage,
data/translation_report.csv), after function 2, and the tax data.

After function 2: reporters keep their amounts after function 1; the
zero-reporters' weight is spread over the bands by the run's fill-in, from
the trained model (fillin.FillIn.refit once, on the training rows, as at the
start of a pass), each band at its PUF median grown to the survey's year. So
the statistics of this stage are approximate within a band.

Writes common.MODEL/translation_report.csv (long: survey, variable, year,
stage, statistic, value) and translation_report.md (one block per row and
year). With --table-only (no trained model needed): the table stage's three
stages only, to common.DATA/translation_report.md.
"""
import os
import sys

import numpy as np
import pandas as pd

import common
import translate as TR

STAGES = ['as reported', 'after function 1', 'after function 2', 'tax data', 'survey', 'CPS (nearest year)']


def after_fill_in():
    """The stage after function 2, as long report rows."""
    import torch
    import fillin
    import model as M
    import train
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    table = M.read_table()
    train_rows = table[table.split == 'train']
    tr = train.encode(train_rows, device)
    model = M.load_model(device, os.path.join(common.MODEL, 'model.pt'))
    fill = fillin.FillIn(model, tr, train_rows, torch.Generator(device=device).manual_seed(common.SEED))
    fill.refit(0, write_log=False)
    scf = fill.scf_rows.cpu().numpy()
    year = train_rows['year'].to_numpy()[scf]
    w = train_rows['weight'].to_numpy(float)[scf]
    edges = common.edges()
    growth = edges['meta']['growth_factors']
    rows = []
    for r in TR.filled():
        v = r['variable']
        zero = (tr['rep_band'][v][fill.scf_rows] == 0).cpu().numpy()
        dist = fill.dist[v].double().cpu().numpy()
        med = np.array([np.nan if b['median'] is None else b['median'] for b in edges[f'puf/{common.PUF_YEAR}/{v}']['bands']])
        for y in sorted(set(year)):
            # the shares of the zero-reporters' weight in each band, over the training rows
            m = (year == y) & zero
            share = (w[m, None] * dist[m]).sum(0) / w[m].sum()
            fp, fn = growth[str(y)][v]
            value = np.where(med > 0, med * fp, med * fn)
            value[0] = 0.0
            used = share > 0
            assert not np.isnan(value[used]).any(), f'{v} {y}: fill-in in a band with no PUF record'
            d = table[(table.source == r['survey']) & (table.year == y)]
            x, wt = d[v].to_numpy(float), d['weight'].to_numpy(float)
            xx = np.concatenate([x[x != 0], value[used]])
            ww = np.concatenate([wt[x != 0], wt[x == 0].sum() * share[used]])
            rows += TR.report_rows(r, int(y), {'after function 2': TR.describe(xx, ww)})
    return rows


def main():
    table_only = '--table-only' in sys.argv[1:]
    rep = pd.read_csv(f'{common.DATA}/translation_report.csv')
    out_dir = common.DATA if table_only else common.MODEL
    if not table_only:
        rep = pd.concat([rep, pd.DataFrame(after_fill_in())], ignore_index=True)
        rep.to_csv(os.path.join(common.MODEL, 'translation_report.csv'), index=False)
    text = ['# Translation report', '',
            'Each row of the translation table and survey year, against the tax data for that year. After function 2: '
            'filled-in amounts at their band\'s PUF median (approximate within a band). Units in millions.'
            + (' Table stage only: no stage after function 2.' if table_only else ''), '']
    for (s, v, y), g in rep.groupby(['survey', 'variable', 'year'], sort=False):
        wide = g.pivot_table(index='statistic', columns='stage', values='value', sort=False)
        wide = wide[[c for c in STAGES if c in wide]]
        text += [f'## {s.upper()} {y}: {v}', '', '```', wide.round(2).to_string(), '```', '']
    open(os.path.join(out_dir, 'translation_report.md'), 'w').write('\n'.join(text))
    common.say(f'translation report: {len(rep)} rows, {os.path.join(out_dir, "translation_report.md")}')


if __name__ == '__main__':
    main()
