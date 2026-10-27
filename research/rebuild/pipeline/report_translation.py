"""The translation report of a run, every stage side by side (translate.py).

For every row of the translation table and every survey year: the survey as
reported, the link (after the definition fixes), after function 1 (all from
the table stage, data/translation_report.csv), after function 2, and the tax
data. First, since 28 October, a wages section: each survey's wages raw, link,
mapped and tax data, and how far the mapping moved the link wages against the
size NEXT_STEPS.md expects (about 3% where deferrals come off, about 8% where
they do not).

After function 2: reporters keep their amounts after function 1; the
zero-reporters' weight is spread over the bands by the run's fill-in, from
the trained model (fillin.FillIn.refit once, on the training rows, as at the
start of a pass), each band at its PUF median grown to the survey's year. So
the statistics of this stage are approximate within a band.

Writes common.MODEL/translation_report.csv (long: survey, variable, year,
stage, statistic, value) and translation_report.md (one block per row and
year). With --table-only (no trained model needed): the table stage's
stages only, to common.DATA/translation_report.md.
"""
import os
import sys

import numpy as np
import pandas as pd

import common
import translate as TR

STAGES = ['as reported', 'link', 'after function 1', 'after function 2', 'tax data', 'survey', 'CPS (nearest year)',
          'CPS (same year)']

# The wages section (since 28 October)
WAGE_STAGES = {'as reported': 'raw', 'link': 'link', 'after function 1': 'mapped', 'tax data': 'tax data'}
WAGE_ROWS = ['units (millions)', 'with a positive amount (%)'] + [f'holders p{p} ($)' for p in TR.PERCENTILES] + \
    ['above $100,000 (millions)', 'above $1,000,000 (millions)', 'mean per unit ($)']
DEFERRAL_ROWS = ['traditional deferrals subtracted ($ billion)', 'units with deferrals subtracted (millions)',
                 'units not asked about deferrals (millions)', 'wages of units not asked ($ billion)']
MOVE_ROWS = ['move: total, mapped over link (%)', 'move: median unit, mapped over link (%)',
             'move: units down by more than 10% (%)', 'move: units up by more than 10% (%)']
EXPECTED = {True: -3.0, False: -8.0}   # NEXT_STEPS.md: what the link wage leaves in, as a move in percent


def wages_section(rep):
    """Every survey year's wages: raw (as reported), link (gross less the traditional deferrals the survey reports),
    mapped (after function 1) and the tax data; and how far the mapping moved the link wages, against the size
    NEXT_STEPS.md expects."""
    w = rep[rep.variable == 'wages']
    if w.empty:
        return []
    blocks, summary = [], []
    for (s, y), g in w.groupby(['survey', 'year'], sort=False):
        wide = g.pivot_table(index='statistic', columns='stage', values='value', sort=False)
        tab = wide.loc[[r for r in WAGE_ROWS if r in wide.index], [c for c in WAGE_STAGES if c in wide]]
        ded = wide.loc[[r for r in DEFERRAL_ROWS if r in wide.index], 'link'] if 'link' in wide else pd.Series(dtype=float)
        off = bool(ded.get('traditional deferrals subtracted ($ billion)', 0) > 0)
        mv = wide.loc[[r for r in MOVE_ROWS if r in wide.index], 'after function 1']
        blocks += [f'### {s.upper()} {y}', '', tab.rename(columns=WAGE_STAGES).to_markdown(floatfmt=',.2f'), '']
        if len(ded):
            blocks += ['Deferrals: ' + '; '.join(f'{k} {v:,.2f}' for k, v in ded.items()) + '.', '']
        blocks += ['The move: ' + '; '.join(f'{k[len("move: "):]} {v:,.1f}' for k, v in mv.items())
                   + f'. Expected about {EXPECTED[off]:.0f}% ('
                   + ('deferrals come off' if off else 'no deferrals come off') + ').', '']
        summary.append({'survey': s.upper(), 'year': y, 'deferrals come off': 'yes' if off else 'no',
                        'expected move (%)': EXPECTED[off],
                        'move, total (%)': mv.get('move: total, mapped over link (%)', np.nan),
                        'move, median unit (%)': mv.get('move: median unit, mapped over link (%)', np.nan)})
    return ['## Wages: raw, link, mapped and tax data', '',
            'Raw is the survey\'s wages as reported (gross). Link is raw less the traditional (not Roth) elective '
            'deferrals the survey reports: the SCF (respondent and spouse or partner, each held to the legal limit; '
            'not asked outside the primary economic unit) and the SIPP in 2021 and 2023. The CEX, the CPS and the '
            'SIPP before 2021 report none, so link equals raw there. Mapped is the link wage after function 1: the '
            'survey\'s share of units with wages kept, each earner given the tax data\'s wages (box 1) at the same '
            'percentile among earners. Holders\' percentiles are among units with wages. Nominal dollars of the '
            'income year; units in millions.', '',
            'The expected move. Box 1 wages are wages after every pre-tax payroll deduction. The link wage still holds '
            'the health premiums and other cafeteria-plan items, and, where the survey reports no deferrals, the '
            'deferrals too. So the mapping should move the link wages down by about 3% where deferrals come off and '
            'about 8% where they do not. A move far from that means the survey\'s wages differ from the tax data\'s '
            'for other reasons as well (fewer small earners, the very top, the year).', '',
            pd.DataFrame(summary).to_markdown(index=False, floatfmt=',.1f'), ''] + blocks


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
            + (' Table stage only: no stage after function 2.' if table_only else ''), ''] + wages_section(rep) + \
        ['## Every row', '']
    for (s, v, y), g in rep.groupby(['survey', 'variable', 'year'], sort=False):
        wide = g.pivot_table(index='statistic', columns='stage', values='value', sort=False)
        wide = wide[[c for c in STAGES if c in wide]]
        text += [f'## {s.upper()} {y}: {v}', '', '```', wide.round(2).to_string(), '```', '']
    open(os.path.join(out_dir, 'translation_report.md'), 'w').write('\n'.join(text))
    common.say(f'translation report: {len(rep)} rows, {os.path.join(out_dir, "translation_report.md")}')


if __name__ == '__main__':
    main()
