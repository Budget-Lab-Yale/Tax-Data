"""Tips in the target year's file against that year's SIPP tax units, both weighted: by wages, age range, marital
status and person. The file's dependent filers hold no SIPP item and are left out. Nothing here is targeted. Writes
<tilt>/sipp_report.md.
"""
import numpy as np
import pandas as pd

from shared import common
from shared import survey_mix
from output.report_cps import side_by_side

Y = common.TARGET_YEAR
WAGES = common.WAGE_GROUPS
ADULT = common.AGE_LABELS[:common.ADULT_RANGES]


def stat(m):
    """Units, total tips, units and people with tips, mean tips per unit with tips, and the share of tips from leisure
    and hospitality jobs."""
    w, t = m.w.to_numpy(float), m.tips.to_numpy(float)
    has = t > 0
    p = ((m.tips1 > 0).astype(float) + (m.tips2 > 0)).to_numpy()
    return {'units (m)': w.sum() / 1e6, 'tips ($bn)': (w * t).sum() / 1e9, 'units with tips (m)': w[has].sum() / 1e6,
            'people with tips (m)': (w * p).sum() / 1e6,
            'mean per unit with tips ($)': (w * t).sum() / w[has].sum() if has.any() else np.nan,
            'leisure and hospitality share of tips (%)':
                100 * (w * (m.tips1 * m.tips_lh1 + m.tips2 * m.tips_lh2)).sum() / (w * t).sum() if has.any() else np.nan}


def table(src, rows):
    """A markdown table of stat for each row and source."""
    return side_by_side(src, rows, stat, ',.2f')


def main():
    # 1. Read the file (adults only) and the SIPP tax units of the year mix, with the SIPP's tips and wages under the
    #    file's names. Give both sides their wage groups.
    f = pd.read_parquet(f'{common.TILT}/file_{Y}.parquet')
    f = f[f['age_range'] < common.ADULT_RANGES].copy()
    f['w'] = f['weight']
    s = survey_mix.mixed_units('sipp')
    s['w'] = s['weight']
    s['tips1'], s['tips2'] = s['tips_primary_reported'], s['tips_spouse_reported']
    s['tips'] = s['tips1'] + s['tips2']
    s['tips_lh1'], s['tips_lh2'] = s['tips_lh_primary'], s['tips_lh_spouse']
    s['wages'] = s['wages_translated']
    src = {'file': f, 'SIPP mix': s}
    for d in src.values():
        d['wg'] = common.groups_by(d['wages'].to_numpy(float), WAGES)
    # 2. The tables by group, then by person.
    out = [f'# Tips in the {Y} file against the SIPP year mix', '',
           f'SIPP: the year mix of the {Y} file (survey_mix.py), each year in {Y} dollars by SOI wages per return, every tax unit, weighted. File: after the tilt, dependent filers left out (they '
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
    # 3. Write the report.
    open(f'{common.TILT}/sipp_report.md', 'w').write('\n'.join(out))
    common.say('\n'.join(out))


if __name__ == '__main__':
    main()
