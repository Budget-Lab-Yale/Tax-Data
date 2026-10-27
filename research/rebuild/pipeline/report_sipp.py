"""Tips in the target year's file (2019 below) against that year's SIPP.

The SIPP side is the target year's tax units (build_table.sipp_person_slots): every tax unit, the person items in
primary-filer order by the rule the age range uses, dependent filers folded into the unit that claims them (their own
tips go nowhere). The file's dependent filers hold no SIPP item (generate.py), so the file side leaves them out. Both
sides are weighted: the file by its tilted weights, the SIPP by its own. Wage groups: the unit's wages, the file's in
nominal 2019 dollars and the SIPP's as the translation table gives them (its wages row; dollars throughout by
default), so the groups mean the same on both sides. Tips: the file's are each adult's drawn tips held to at most
that adult's wages (dollars.person_items), the SIPP's as reported.

None of this is targeted. Writes common.TILT/sipp_report.md.
"""
import numpy as np
import pandas as pd

import common
import survey_mix

Y = common.TARGET_YEAR
WAGES = common.WAGE_GROUPS
ADULT = common.AGE_LABELS[:common.ADULT_RANGES]


def stat(m):
    w, t = m.w.to_numpy(float), m.tips.to_numpy(float)
    has = t > 0
    p = ((m.tips1 > 0).astype(float) + (m.tips2 > 0)).to_numpy()
    return {'units (m)': w.sum() / 1e6, 'tips ($bn)': (w * t).sum() / 1e9, 'units with tips (m)': w[has].sum() / 1e6,
            'people with tips (m)': (w * p).sum() / 1e6,
            'mean per unit with tips ($)': (w * t).sum() / w[has].sum() if has.any() else np.nan,
            'leisure and hospitality share of tips (%)':
                100 * (w * (m.tips1 * m.tips_lh1 + m.tips2 * m.tips_lh2)).sum() / (w * t).sum() if has.any() else np.nan}


def table(src, rows):
    out = []
    for lab, sel in rows:
        r = {'group': lab}
        for name, d in src.items():
            for k, v in stat(d[sel(d)]).items():
                r[f'{name}: {k}'] = v
        out.append(r)
    return pd.DataFrame(out).set_index('group').to_markdown(floatfmt=',.2f')


def main():
    f = pd.read_parquet(f'{common.TILT}/file_{Y}.parquet')
    f = f[f['age_range'] < common.ADULT_RANGES].copy()
    f['w'] = f['weight']
    s = survey_mix.mixed_units('sipp')
    s['w'] = s['weight']
    s['tips1'], s['tips2'] = s['tips_primary_reported'], s['tips_spouse_reported']
    s['tips'] = s['tips1'] + s['tips2']
    s['tips_lh1'], s['tips_lh2'] = s['tips_lh_primary'], s['tips_lh_spouse']
    s['wages'] = s['wages_translated']
    src = {'file': f, f'SIPP mix': s}
    for d in src.values():
        d['wg'] = common.groups_by(d['wages'].to_numpy(float), WAGES)
    out = [f'# Tips in the {Y} file against the SIPP year mix', '',
           f'SIPP: reference year {Y}, every tax unit, weighted. File: after the tilt, dependent filers left out (they '
           'hold no SIPP item). Nothing here is targeted. Nominal dollars of the year.', '',
           '## All units', '', table(src, [('all', lambda d: d.w > 0)]), '',
           '## By the unit\'s wages', '', table(src, [(lab, (lambda lab: lambda d: d.wg == lab)(lab)) for _, _, lab in WAGES]), '',
           '## By the primary filer\'s age range', '',
           table(src, [(lab, (lambda k: lambda d: d.age_range == k)(k)) for k, lab in enumerate(ADULT)]), '',
           '## By marital status', '', table(src, [('married', lambda d: d.married == 1), ('not married', lambda d: d.married == 0)]),
           '']
    rows = []
    for name, d in src.items():
        w = d.w.to_numpy(float)
        for k, who in [(1, 'primary filer'), (2, 'spouse')]:
            t, lh = d[f'tips{k}'].to_numpy(float), d[f'tips_lh{k}'].to_numpy()
            rows.append({'source': name, 'person': who, 'people with tips (m)': w[t > 0].sum() / 1e6,
                         'tips ($bn)': (w * t).sum() / 1e9,
                         'mean among those with tips ($)': (w * t).sum() / w[t > 0].sum(),
                         'with tips mostly from leisure and hospitality (m)': w[(t > 0) & (lh == 1)].sum() / 1e6,
                         'their tips ($bn)': (w * t * lh).sum() / 1e9})
    out += ['## By person', '', pd.DataFrame(rows).to_markdown(index=False, floatfmt=',.2f'), '']
    open(f'{common.TILT}/sipp_report.md', 'w').write('\n'.join(out))
    common.say('\n'.join(out))


if __name__ == '__main__':
    main()
