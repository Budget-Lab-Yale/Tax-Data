"""The translation report of a trained run. Translation puts each survey's items on the tax data's terms (FEATURES.md,
"Translation: putting the surveys on the PUF's terms"). For every row of the translation table and every survey year,
the report shows the item side by side at each stage: as reported; after the definition fixes ("link"); after
function 1, which gives each holder the tax data's amount at the same rank; after function 2, the fill-in, which
gives some survey zeros an amount; and in the tax data. A section on wages comes first. Writes translation_report.csv
and translation_report.md to the run's model folder or, run on a year folder, to the year's tilt folder beside the
other reports.
"""
import os

import numpy as np
import pandas as pd

from shared import common
from shared import translate as TR

STAGES = ['as reported', 'link', 'after function 1', 'after function 2', 'tax data', 'survey', 'CPS (nearest year)',
          'CPS (same year)']

# ---- The wages section ----
WAGE_STAGES = {'as reported': 'raw', 'link': 'link', 'after function 1': 'mapped', 'tax data': 'tax data'}
WAGE_ROWS = ['units (millions)', 'with a positive amount (%)'] + [f'holders p{p} ($)' for p in TR.PERCENTILES] + \
    ['above $100,000 (millions)', 'above $1,000,000 (millions)', 'mean per unit ($)']
DEFERRAL_ROWS = ['traditional deferrals subtracted ($ billion)', 'units with deferrals subtracted (millions)',
                 'units not asked about deferrals (millions)', 'wages of units not asked ($ billion)']
MOVE_ROWS = ['move: total, mapped over link (%)', 'move: median unit, mapped over link (%)',
             'move: units down by more than 10% (%)', 'move: units up by more than 10% (%)']
# The expected move from the link wage to box 1 wages, in percent: about -3% where the survey's deferrals come off
# (health premiums and other cafeteria-plan items remain) and about -8% where they do not.
EXPECTED = {True: -3.0, False: -8.0}


def wages_section(rep):
    """Every survey year's wages as reported, less reported traditional deferrals (the link wage), after function 1,
    and in the tax data; and how far function 1 moved the link wages against the expected move."""
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


# ---- The stage after function 2 ----

def after_fill_in(model_dir):
    """The stage after function 2, as report rows. The weight of the survey records that report zero is spread over
    the dollar bands by the trained network's fill-in, and each band is valued at its PUF median grown to the survey
    year. This stage is therefore approximate within a band."""
    import torch
    from pretrain import fillin
    from pretrain import model as M
    from pretrain import train
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    table = M.read_table()
    train_rows = table[table.split == 'train']
    tr = train.encode(train_rows, device)
    model = M.load_model(device, os.path.join(model_dir, 'model.pt'))
    fill = fillin.FillIn(model, tr, train_rows, torch.Generator(device=device).manual_seed(common.SEED))
    fill.refit(0, write_log=False)
    edges = common.edges()
    growth = edges['meta']['growth_factors']
    rows = []
    for (s, v), r in fillin.ROW.items():
        recs = fill.recs[s].cpu().numpy()
        year = train_rows['year'].to_numpy()[recs]
        w = train_rows['weight'].to_numpy(float)[recs]
        zero = fill.zero[(s, v)].cpu().numpy()                      # the records reporting zero, in the years filled in
        dist = np.zeros((len(recs), fill.n_bands[v]))
        dist[zero] = fill.dist[(s, v)].double().cpu().numpy()
        med = np.array([np.nan if b['median'] is None else b['median'] for b in edges[f'puf/{common.PUF_YEAR}/{v}']['bands']])
        for y in sorted(set(year)):
            if not TR.fills_in(r, int(y)):
                continue
            # The share of the zero-reporters' weight in each band, over the training rows.
            m = (year == y) & zero
            share = (w[m, None] * dist[m]).sum(0) / w[m].sum()
            fp, fn = growth[str(y)][v]
            value = np.where(med > 0, med * fp, med * fn)
            value[0] = 0.0
            used = share > 0
            assert not np.isnan(value[used]).any(), f'{v} {y}: fill-in in a band with no PUF record'
            d = table[(table.source == s) & (table.year == y)]
            x, wt = d[v].to_numpy(float), d['weight'].to_numpy(float)
            xx = np.concatenate([x[x != 0], value[used]])
            ww = np.concatenate([wt[x != 0], wt[x == 0].sum() * share[used]])
            rows += TR.report_rows(r, int(y), {'after function 2': TR.describe(xx, ww)})
    return rows


# ---- The report ----

def folders():
    """The network's folder and the report's folder. In a training folder, both are the run's own model folder. In a
    year folder, whose model/ links to the training folder's, the network is the first of settings generation.models
    and the report goes to the year's tilt folder, beside the other reports."""
    if os.path.exists(os.path.join(common.MODEL, 'model.pt')):
        return common.MODEL, common.MODEL
    return f"{common.OUT}/model/{common.SETTINGS['generation']['models'][0]['run']}", common.TILT


def main():
    # 1. Read the report rows that data preparation wrote, add the stage after function 2, and write them all.
    model_dir, out = folders()
    rep = pd.read_csv(f'{common.DATA}/translation_report.csv')
    rep = pd.concat([rep, pd.DataFrame(after_fill_in(model_dir))], ignore_index=True)
    rep.to_csv(os.path.join(out, 'translation_report.csv'), index=False)
    # 2. The page: the wages section, then one table per survey, item and year.
    text = ['# Translation report', '',
            'Each row of the translation table and survey year, against the tax data for that year. After function 2: '
            'filled-in amounts at their band\'s PUF median (approximate within a band). Units in millions.', ''] + \
        wages_section(rep) + ['## Every row', '']
    for (s, v, y), g in rep.groupby(['survey', 'variable', 'year'], sort=False):
        wide = g.pivot_table(index='statistic', columns='stage', values='value', sort=False)
        wide = wide[[c for c in STAGES if c in wide]]
        text += [f'## {s.upper()} {y}: {v}', '', '```', wide.round(2).to_string(), '```', '']
    open(os.path.join(out, 'translation_report.md'), 'w').write('\n'.join(text))
    common.say(f'translation report: {len(rep)} rows, {os.path.join(out, "translation_report.md")}')


if __name__ == '__main__':
    main()
