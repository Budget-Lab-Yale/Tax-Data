"""Consumption in the target year's file (2019 below) against that year's CEX.

The CEX side is the target year's tax units (build_table.cex_target_units): all of them, a shared household's
spending divided among its tax units by head count, as the tilt's targets and the training rows are; and, for
reference, only those whose household is one tax unit, whose spending needs no dividing. Both are in nominal target-year dollars, as is
the file, so no conversion stands between them.

Targeted (the tilt): each category's average per tax unit, overall and by the primary filer's adult age range.
Dependent filers have no consumption of their own: the CEX folds them into the unit that claims them, and the file
gives them zero.
Not targeted: everything else below. Income groups: wages and gross Social Security mean the same on both
sides; AGI and CEX income do not (the CEX's income before taxes, summed from its items, is a survey concept;
production found it 22-48% below PUF income at the same rank), so income is compared by each source's own
deciles, not in dollars.

Writes common.TILT/cex_report.md.
"""
import numpy as np
import pandas as pd

from shared import common
from shared import survey_mix
from shared import dollars as D

CATS = common.meta()['cex_amounts']
NAMES = dict(zip(CATS, ['clothing', 'motor vehicles', 'durables', 'other nondurables', 'food at home', 'gasoline',
                        'housing and utilities', 'other services and health']))
AGES = {k: lab if k < common.ADULT_RANGES else 'dependent filer' for k, lab in enumerate(common.AGE_LABELS)}
CEX_INCOME = ['wages', 'se_income', 'social_security', 'ssi', 'int_div', 'net_rent', 'pensions', 'other_regular', 'roy_est']
Y = common.TARGET_YEAR
groups_by = common.groups_by
WAGES = common.WAGE_GROUPS
SS = [(-np.inf, 1, 'none'), (1, 15e3, '$1-15k'), (15e3, 30e3, '$15k-30k'), (30e3, 50e3, '$30k-50k'),
      (50e3, np.inf, '$50k+')]


def deciles(x, w):
    """Weighted decile (1-10) of x within its own source."""
    o = np.argsort(x, kind='stable')
    cum = np.empty(len(x))
    cum[o] = (np.cumsum(w[o]) - w[o] / 2) / w.sum()
    return np.minimum((cum * 10).astype(int) + 1, 10)


def compare(sources, key, order):
    """Units (millions) and mean total consumption ($ thousands) by group, per source."""
    rows = []
    for g in order:
        r = {'group': g}
        for lab, d in sources.items():
            m = d[key] == g
            r[f'{lab}: units (m)'] = d.w[m].sum() / 1e6
            r[f'{lab}: mean ($k)'] = np.average(d.total[m], weights=d.w[m]) / 1e3 if m.any() else np.nan
        rows.append(r)
    return pd.DataFrame(rows).set_index('group').to_markdown(floatfmt=',.1f')


def main():
    f = pd.read_parquet(f'{common.TILT}/file_{Y}.parquet')
    c = survey_mix.mixed_units('cex')
    f['w'], f['total'] = f['weight'], f['total_consumption']
    c['w'], c['total'] = c['weight'], c[CATS].sum(1)
    c['gross_ss'], c['income'] = c['social_security'], c[CEX_INCOME].sum(1)
    f['agi'] = D.agi(f)
    one = c[c['one_tu'] == 1]
    src = {'file': f, 'CEX mix': c, 'CEX mix, one-unit': one}
    for d in src.values():
        d['age'] = d['age_range'].map(AGES)
        d['hh'] = np.char.add(np.where(d['married'] == 1, 'married', 'not married'),
                              np.where(d['n_dep'] > 0, ', with dependents', ', no dependents'))
        d['wg'] = groups_by(d['wages'].to_numpy(float), WAGES)
        d['ssg'] = groups_by(d['gross_ss'].to_numpy(float), SS)
    adult = lambda d: d[d['age'] != 'dependent filer']
    f_ad, c_ad, o_ad = adult(f), adult(c), adult(one)
    f_ad = f_ad.assign(dec=deciles(f_ad['agi'].to_numpy(float), f_ad.w.to_numpy(float)))
    c_ad = c_ad.assign(dec=deciles(c_ad['income'].to_numpy(float), c_ad.w.to_numpy(float)))
    o_ad = o_ad.assign(dec=deciles(o_ad['income'].to_numpy(float), o_ad.w.to_numpy(float)))

    out = [f'# Consumption in the {Y} file against the CEX year mix', '',
           f'CEX: the year mix of the {Y} file (survey_mix.py), each year in {Y} dollars by NIPA PCE per return (income and Social Security are not grown), all tax units (a shared household\'s spending divided by head count), and the one-unit '
           f'households. Nominal {Y} dollars. The model trains on every CEX tax unit. The tilt targets each category\'s '
           'average per unit, overall and by the adult age ranges by married, within a 5% band; nothing else here is targeted.', '']

    tot = pd.DataFrame({'file ($bn)': [(f[v] * f.w).sum() / 1e9 for v in CATS],
                        'CEX mix ($bn)': [(c[v] * c.w).sum() / 1e9 for v in CATS]}, index=[NAMES[v] for v in CATS])
    tot.loc['total'] = tot.sum()
    tot['file / CEX'] = tot.iloc[:, 0] / tot.iloc[:, 1]
    out += ['## Totals (not targeted: the targets are averages, and the file counts more units)', '',
            f'Units: file {f.w.sum() / 1e6:.1f} million (returns and non-filers); CEX {c.w.sum() / 1e6:.1f} million tax units.', '',
            tot.to_markdown(floatfmt=',.2f'), '',
            '## By the primary filer\'s age (targeted: each category\'s average per unit)', '',
            compare(src, 'age', list(dict.fromkeys(AGES.values()))), '']

    out += ['# Not targeted', '',
            '## By wages', '', compare(src, 'wg', [l for _, _, l in WAGES]), '',
            '## By income decile, each source\'s own (file: AGI; CEX: income before taxes summed from its items); '
            'adults, dependent filers left out', '',
            compare({'file': f_ad, 'CEX mix': c_ad, 'CEX mix, one-unit': o_ad}, 'dec', list(range(1, 11))), '',
            '## By married and dependents', '', compare(src, 'hh', sorted(set(c['hh']))), '',
            '## 65+, by gross Social Security', '',
            compare({k: d[d['age'] == '65+'] for k, d in src.items()}, 'ssg', [l for _, _, l in SS]), '']

    rows = []
    for lab, d in [('file', f), ('CEX mix', c)]:
        for g in [l for _, _, l in WAGES]:
            m = d[d['wg'] == g]
            tt = (m.total * m.w).sum()
            rows.append({'wages': g, 'source': lab, **{NAMES[v]: 100 * (m[v] * m.w).sum() / tt for v in CATS}})
    out += ['## Category shares of consumption by wages (%)', '',
            pd.DataFrame(rows).set_index(['wages', 'source']).to_markdown(floatfmt='.1f'), '']
    z = pd.DataFrame({'file (%)': [100 * f.w[f[v] == 0].sum() / f.w.sum() for v in CATS],
                      'CEX mix (%)': [100 * c.w[c[v] == 0].sum() / c.w.sum() for v in CATS],
                      'CEX mix, one-unit (%)': [100 * one.w[one[v] == 0].sum() / one.w.sum() for v in CATS]},
                     index=[NAMES[v] for v in CATS])
    out += ['## Units with none of the category', '',
            'Head-count splitting gives every tax unit in a spending household some of each category, so the all-units CEX '
            'column understates zeros; the one-unit column is the cleaner yardstick.', '', z.to_markdown(floatfmt='.1f'), '']
    open(f'{common.TILT}/cex_report.md', 'w').write('\n'.join(out))
    common.say('\n'.join(out))


if __name__ == '__main__':
    main()
