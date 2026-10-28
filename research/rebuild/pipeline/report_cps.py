"""The CPS-only items in the target year's file (2019 below) against that year's CPS ASEC.

The CPS side is the tax units of the year mix of the target year's file (survey_mix.py; dollars grown to the target year by SOI wages per return) (build_table.cps_person_slots): every tax unit, filers and non-filers,
the person items in primary-filer order by the rule the age range uses, dependent filers folded into the unit that
claims them. The file's dependent filers hold no CPS item (generate.py), so the file side below leaves them out.
Both sides are weighted: the file by its tilted weights, the CPS by its own. Wage groups: the file's wages (nominal
2019 dollars) and the CPS's wages after function 1 (the translation table puts them on the tax data's scale by percentile
among earners since 28 October), so a group holds the same ranks on both sides; the spouse's share uses the CPS's own amounts.

None of this is targeted. Sections: the primary filer's and the spouse's age within each age range; dependents'
ages; the primary filer's sex; for married couples, whether the spouse has wages and the spouse's share of the
couple's wages, by the couple's wages, and the same for self-employment income; childcare by the number of
dependents under 13 and by wages; weeks and hours worked; overtime pay (since 28 October; overtime_section).

Writes common.TILT/cps_report.md.
"""
import numpy as np
import pandas as pd

import common
import survey_mix

Y = common.TARGET_YEAR
ADULT = common.AGE_LABELS[:common.ADULT_RANGES]
WAGES = common.WAGE_GROUPS
groups_by = common.groups_by
DEP_AGE_GROUPS = [(0, 6, '0-5'), (6, 13, '6-12'), (13, 17, '13-16'), (17, 19, '17-18'), (19, 24, '19-23'),
                  (24, 200, '24 and over')]
WEEKS = [(-1, 1, 'none'), (1, 27, '1-26'), (27, 50, '27-49'), (50, 53, '50-52')]
HOURS = [(-1, 1, 'none'), (1, 35, '1-34'), (35, 45, '35-44'), (45, 200, '45 and over')]
SHARE = [(-1, 1e-9, 'none'), (1e-9, 0.25, 'under 25%'), (0.25, 0.5, '25-50%'), (0.5, 0.75, '50-75%'),
         (0.75, 1 - 1e-9, '75% to under 100%'), (1 - 1e-9, 2, 'all')]


def wavg(x, w):
    return float(np.average(x, weights=w)) if w.sum() > 0 else np.nan


def wq(x, w, p):
    return float(common.wquantile(np.asarray(x, float), np.asarray(w, float), [p])[0]) if len(x) else np.nan


def side_by_side(sources, rows, stat):
    """One row per (label, selector); for each source, stat(frame) -> dict of columns."""
    out = []
    for lab, sel in rows:
        r = {'group': lab}
        for name, d in sources.items():
            m = d[sel(d)]
            for k, v in stat(m).items():
                r[f'{name}: {k}'] = v
        out.append(r)
    return pd.DataFrame(out).set_index('group').to_markdown(floatfmt=',.1f')


def shares(d, col, bands):
    """Percent of weight in each band of col."""
    g = groups_by(d[col].to_numpy(float), bands)
    return {lab: 100 * d.w[g == lab].sum() / d.w.sum() if d.w.sum() > 0 else np.nan for _, _, lab in bands}


def overtime_section(src):
    """Overtime pay (since 28 October) by the unit's wages: units, the share with overtime, the mean among those with
    any, and the total. The file: overtime1 + overtime2 (each held to the person's wages). The CPS: only the units
    asked the earnings questions have overtime (the rest are missing); within each survey year their weights are
    scaled up to the year's whole weight, so the totals are for every unit (the asked units are a random part of the
    sample: months 4 and 8 in the survey)."""
    def ot(m):
        x, w = m['overtime'].to_numpy(float), m.w.to_numpy(float)
        return {'units (m)': w.sum() / 1e6, 'with overtime (%)': 100 * w[x > 0].sum() / w.sum() if w.sum() > 0 else np.nan,
                'mean, those with any ($)': wavg(x[x > 0], w[x > 0]), 'total ($bn)': (w * x).sum() / 1e9}
    frames = {}
    for name, d in src.items():
        if 'overtime1' in d:                                        # the file
            d = d.copy()
            d['overtime'] = d['overtime1'] + d['overtime2']
            frames[name] = d
        elif 'overtime_primary' in d:                               # the CPS side: asked units, scaled up
            a = d[d['overtime_primary'].notna()].copy()
            if 'survey_year' in d:
                scale = d.groupby('survey_year')['w'].sum() / a.groupby('survey_year')['w'].sum()
                a['w'] = a['w'] * a['survey_year'].map(scale)
            else:
                a['w'] = a['w'] * d.w.sum() / a.w.sum()
            a['overtime'] = a['overtime_primary'] + a['overtime_spouse']
            frames[name] = a
    if len(frames) < 2:
        return []
    return ['## Overtime pay, by the unit\'s wages', '',
            'Each adult\'s overtime by Tedeschi\'s rules (FLSA coverage, exemptions, hours over 40 at time and a half, '
            'times weeks worked), at most the person\'s wages. CPS: the units asked the earnings questions, weighted up to '
            'every unit of their year. Nominal dollars of the file\'s year.', '',
            side_by_side(frames, [(lab, (lambda lab: lambda d: d['wg'] == lab)(lab)) for _, _, lab in WAGES]
                         + [('all', lambda d: d['married'] >= 0)], ot), '']


def main():
    f = pd.read_parquet(f'{common.TILT}/file_{Y}.parquet')
    c = survey_mix.mixed_units('cps')
    f = f[f['age_range'] < common.ADULT_RANGES].copy()        # dependent filers hold no CPS item
    f['w'] = f['weight']
    c['w'] = c['weight']
    # the CPS side on the file's terms: ages in years (-1 none), the spouse's amounts, wages as reported
    for a in ['age_primary', 'age_spouse', 'age_dep1', 'age_dep2', 'age_dep3']:
        c[a] = c[a] - 1
    c['wages_reported'] = c['wages_primary_reported'] + c['wages_spouse_reported']
    c['wages'] = c['wages_translated']             # the tax data's wages at the same percentile among earners
    c['wages2'] = c['wages_spouse_reported']        # the spouse's share uses the CPS's own amounts
    c['sch_cf_income'] = c['se_primary_reported'] + c['se_spouse_reported']
    c['se2'] = c['se_spouse_reported']
    src = {'file': f, f'CPS mix': c}
    for d in src.values():
        d['wg'] = groups_by(d['wages'].to_numpy(float), WAGES)
        d['n_young'] = sum(((d[f'age_dep{k}'] >= 0) & (d[f'age_dep{k}'] < 13)).astype(int) for k in (1, 2, 3))
        d['woman'] = (d['sex_primary'] == 2).astype(float)
    out = [f'# The CPS-only items in the {Y} file against the CPS year mix', '',
           f'CPS: tax year {Y}, every tax unit (filers and non-filers), weighted. File: after the tilt, dependent filers '
           'left out (they hold no CPS item). Nothing here is targeted. Units in millions; ages in years.', '',
           f'Units: file {f.w.sum() / 1e6:.1f} million; CPS {c.w.sum() / 1e6:.1f} million.', '']

    age_stat = lambda col: (lambda m: {'units (m)': m.w.sum() / 1e6, 'mean': wavg(m[col], m.w),
                                       'p10': wq(m[col], m.w, 0.1), 'median': wq(m[col], m.w, 0.5),
                                       'p90': wq(m[col], m.w, 0.9)})
    by_range = [(lab, (lambda k: lambda d: d['age_range'] == k)(k)) for k, lab in enumerate(ADULT)]
    out += ['## The primary filer\'s age, within each age range', '',
            side_by_side(src, by_range, age_stat('age_primary')), '',
            '## The spouse\'s age (married couples), by the primary filer\'s age range', '',
            side_by_side(src, [(lab, (lambda k: lambda d: (d['age_range'] == k) & (d['married'] == 1))(k))
                               for k, lab in enumerate(ADULT)], age_stat('age_spouse')), '',
            '## The spouse\'s age less the primary filer\'s (married couples)', '',
            side_by_side(src, [('all couples', lambda d: d['married'] == 1)],
                         lambda m: {'mean': wavg(m.age_spouse - m.age_primary, m.w),
                                    'p10': wq(m.age_spouse - m.age_primary, m.w, 0.1),
                                    'median': wq(m.age_spouse - m.age_primary, m.w, 0.5),
                                    'p90': wq(m.age_spouse - m.age_primary, m.w, 0.9)}), '']

    def dep_ages(m):
        x = np.concatenate([m[f'age_dep{k}'].to_numpy(float) for k in (1, 2, 3)])
        w = np.concatenate([m.w.to_numpy(float)] * 3)
        x, w = x[x >= 0], w[x >= 0]
        g = groups_by(x, DEP_AGE_GROUPS)
        return {'dependents (m)': w.sum() / 1e6,
                **{f'{lab} (%)': 100 * w[g == lab].sum() / w.sum() for _, _, lab in DEP_AGE_GROUPS}}
    out += ['## Dependents\' ages (the first three of each unit), by the number of dependents', '',
            side_by_side(src, [(f'{k}{"+" if k == 3 else ""} dependents', (lambda k: lambda d: d['n_dep'] == k)(k))
                               for k in (1, 2, 3)] + [('all', lambda d: d['n_dep'] > 0)], dep_ages), '']

    sex = lambda m: {'units (m)': m.w.sum() / 1e6, 'women (%)': 100 * wavg(m.woman, m.w)}
    out += ['## The primary filer\'s sex: share who are women', '',
            side_by_side(src, [('married', lambda d: d['married'] == 1), ('not married', lambda d: d['married'] == 0)]
                         + [(f'not married, {lab}', (lambda k: lambda d: (d['married'] == 0) & (d['age_range'] == k))(k))
                            for k, lab in enumerate(ADULT)], sex), '']

    def split(total, part):
        def stat(m):
            s = np.where(m[total] != 0, m[part] / np.where(m[total] != 0, m[total], 1), 0)
            g = groups_by(s, SHARE)
            return {'couples (m)': m.w.sum() / 1e6, **{f'{lab} (%)': 100 * m.w[g == lab].sum() / m.w.sum()
                                                       for _, _, lab in SHARE}}
        return stat
    couples = lambda extra: (lambda d: (d['married'] == 1) & extra(d))
    out += ['## Married couples: the spouse\'s share of the couple\'s wages, by the couple\'s wages', '',
            'Couples with wages. "none": the spouse has no wages; "all": the spouse earns all of them.', '',
            side_by_side(src, [(lab, couples((lambda lab: lambda d: d['wg'] == lab)(lab))) for _, _, lab in WAGES[1:]]
                         + [('all with wages', couples(lambda d: d['wages'] > 0))], split('wages', 'wages2')), '',
            '## Married couples: the spouse\'s share of the couple\'s self-employment income', '',
            side_by_side(src, [('a net profit', couples(lambda d: d['sch_cf_income'] > 0)),
                               ('a net loss', couples(lambda d: d['sch_cf_income'] < 0))],
                         split('sch_cf_income', 'se2')), '']

    care = lambda m: {'units (m)': m.w.sum() / 1e6, 'with any (%)': 100 * m.w[m.childcare > 0].sum() / m.w.sum(),
                      'mean, all ($)': wavg(m.childcare, m.w),
                      'mean, those with any ($)': wavg(m.childcare[m.childcare > 0], m.w[m.childcare > 0])}
    out += ['## Childcare, by the number of dependents under 13', '',
            side_by_side(src, [(str(k), (lambda k: lambda d: d['n_young'] == k)(k)) for k in (0, 1, 2, 3)], care), '',
            '## Childcare, units with a dependent under 13, by wages', '',
            side_by_side(src, [(lab, (lambda lab: lambda d: (d['n_young'] > 0) & (d['wg'] == lab))(lab))
                               for _, _, lab in WAGES], care), '']

    for who, cond in [('primary', lambda d: d['married'] >= 0), ('spouse', lambda d: d['married'] == 1)]:
        for item, bands in [('weeks', WEEKS), ('hours', HOURS)]:
            col = f'{item}_{who}'
            stat = lambda m, col=col, bands=bands: {'units (m)': m.w.sum() / 1e6,
                                                     **{f'{k} (%)': v for k, v in shares(m, col, bands).items()}}
            out += [f'## {"Weeks worked" if item == "weeks" else "Usual weekly hours"}, the {who}'
                    f'{" (married couples)" if who == "spouse" else ""}, by the unit\'s wages', '',
                    side_by_side(src, [(lab, (lambda lab, cond=cond: lambda d: cond(d) & (d['wg'] == lab))(lab))
                                       for _, _, lab in WAGES] + [('all', cond)], stat), '']
    out += overtime_section(src)
    open(f'{common.TILT}/cps_report.md', 'w').write('\n'.join(out))
    common.say('\n'.join(out))


if __name__ == '__main__':
    main()
