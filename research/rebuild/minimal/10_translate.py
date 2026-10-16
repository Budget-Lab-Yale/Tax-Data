"""
10_translate.py  (minimal example, translation experiments, Set A; 2026-10-15)

What it does. For a table whose SCF dollars may be distorted (Set A of
10_TRANSLATION_EXPERIMENTS.md), decides for each variable the PUF and the SCF
share (wages, interest, dividends, capital gains) whether the SCF's amounts
are in the PUF's dollars, in scaled dollars, or in something else, by a
stated rule, and writes the decision. It also writes the detection table of
01_detection_table.py for that table. The rule reads only the table; it is
never told which distortion was applied.

The quantities (computed here from the table, the same way as
01_detection_table.py; that file runs as a script, so its logic is copied):
  s      ratio of totals: SCF weighted total over PUF weighted total of the
         signed amount (capital gains net of losses);
  c(e)   the cumulative-share ratio at each bracket lower edge e: the SCF's
         weighted share of units with an absolute amount at or above e, over
         the PUF's (shares within source).

The rule (a first, deliberately simple one):
  1. If |s - 1| <= 0.15: case "same dollars", factor 1.
  2. Else, if c(e) for every edge e from $500 up ($500, $2,000, $10,000,
     $50,000, $200,000) is on the same side of 1 as s (all below 1 when
     s < 1, all above 1 when s > 1): case "scale", factor s. The SCF's amounts
     are then divided by s before banding on the PUF's cut points (rule R2).
  3. Otherwise: case "definition or mixed", factor 1, flagged for a person to
     look at.

Usage:
  python 10_translate.py <D>    reads data/stacked_A_<D>_R0.parquet (its
                                dollar columns are the distorted dollars) and
                                writes data/translation_<D>.json and
                                score/detection_<D>.md
  Imported by 10_distort.py, which calls translate() and detection_md() on
  the distorted table directly.

Writes (in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/):
  data/translation_<D>.json   per variable: s, the cumulative ratios, case,
                              factor, flag; and the rule text
  score/detection_<D>.md      the detection table of the distorted table,
                              with the rule's decision at the top
"""
import json
import os
import sys
import numpy as np
import pandas as pd

R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
VARS = ['wages', 'interest', 'dividends', 'capital_gains']
LOWER = [100, 500, 2_000, 10_000, 50_000, 200_000]          # as 01_detection_table.py
LABELS = ['0', '1-100', '100-500', '500-2,000', '2,000-10,000',
          '10,000-50,000', '50,000-200,000', '200,000+']
RULE_EDGES = [500, 2_000, 10_000, 50_000, 200_000]           # the edges the rule reads
TOL = 0.15
RULE_TEXT = ('If |s - 1| <= 0.15: "same dollars", factor 1. Else if the cumulative-share ratio '
             'SCF/PUF at every bracket lower edge from $500 up ($500, $2,000, $10,000, $50,000, '
             '$200,000) is on the same side of 1 as s: "scale", factor s (SCF amounts divided by s). '
             'Else: "definition or mixed", factor 1, flagged. s = SCF weighted total / PUF weighted '
             'total of the signed amount; shares within source, on the absolute amount.')


def bracket(a):
    """Bracket index 0-7 of an absolute amount (01_detection_table.py)."""
    return np.where(a == 0, 0, 1 + np.searchsorted(LOWER, a, side='right'))


def stats(df, v):
    """Per source: bracket shares, record counts, within-bracket means,
    cumulative shares, share negative, weighted total."""
    out = {}
    for s in ['puf', 'scf']:
        x = df[df['source'] == s]
        signed = x[v].to_numpy(dtype=float)
        assert not np.isnan(signed).any(), (s, v)
        a, w = np.abs(signed), x['weight'].to_numpy()
        b = bracket(a)
        W = w.sum()
        share = np.array([w[b == k].sum() / W for k in range(8)])
        out[s] = dict(share=share, n=np.array([(b == k).sum() for k in range(8)]),
                      mean=np.array([np.average(a[b == k], weights=w[b == k]) if (b == k).any() else np.nan
                                     for k in range(8)]),
                      cum=np.array([share[k:].sum() for k in range(8)]),
                      neg=w[signed < 0].sum() / W, total=float((w * signed).sum()))
    return out


def translate(df):
    """The rule, per shared variable. Returns {variable: decision}."""
    res = {}
    for v in VARS:
        st = stats(df, v)
        s = st['scf']['total'] / st['puf']['total']
        cr = st['scf']['cum'] / st['puf']['cum']            # index k: at or above bracket k's lower edge
        # bracket k (k >= 1) has lower edge 0+ for k = 1, else LOWER[k - 2]
        edge_of = {1: 0, **{k: LOWER[k - 2] for k in range(2, 8)}}
        ratios = {edge_of[k]: float(cr[k]) for k in range(1, 8)}
        rule_r = np.array([ratios[e] for e in RULE_EDGES])
        if abs(s - 1) <= TOL:
            case, factor, flag = 'same dollars', 1.0, False
        elif (s < 1 and (rule_r < 1).all()) or (s > 1 and (rule_r > 1).all()):
            case, factor, flag = 'scale', float(s), False
        else:
            case, factor, flag = 'definition or mixed', 1.0, True
        res[v] = {'s': float(s), 'scf_total_bn': st['scf']['total'] / 1e9, 'puf_total_bn': st['puf']['total'] / 1e9,
                  'cumulative_ratio_at_edge': {str(e): r for e, r in ratios.items()},
                  'case': case, 'factor': factor, 'flag': flag}
    return res


def decision_table(res):
    """Markdown table of what the rule saw and decided."""
    L = ['| Variable | s (SCF total / PUF total) | c($1) | c($100) | c($500) | c($2,000) | c($10,000) '
         '| c($50,000) | c($200,000) | Case | Factor |',
         '|---|---:|---:|---:|---:|---:|---:|---:|---:|---|---:|']
    for v, r in res.items():
        c = r['cumulative_ratio_at_edge']
        L.append(f"| {v.replace('_', ' ')} | {r['s']:.3f} | " +
                 ' | '.join(f"{c[str(e)]:.3f}" for e in [0] + LOWER) +
                 f" | {r['case']}{' (flag)' if r['flag'] else ''} | {r['factor']:.3f} |")
    return '\n'.join(L)


def fmt_pct(x):
    return f'{100 * x:.2f}%'


def fmt_ratio(x):
    return '' if not np.isfinite(x) else f'{x:.3f}'


def detection_md(df, title, source_note, res=None):
    """The three tables of 01_detection_table.py per variable, for this table."""
    lines = [f'# {title}', '', source_note, '']
    if res is not None:
        lines += ['## What 10_translate.py decided', '', 'The rule: ' + RULE_TEXT, '',
                  'c($e) is the SCF share of units at or above $e over the PUF share ($1 means above zero).', '',
                  decision_table(res), '']
    lines += ['The tables below are those of `01_detection_table.py`: shares by dollar bracket within '
              'source (capital gains on the absolute amount), the SCF/PUF ratio by bracket and cumulatively '
              '(a star marks the first bracket above zero where the ratio is 0.9 or more), and the mean '
              'amount within each bracket.', '']
    for v in VARS:
        st = stats(df, v)
        p, q = st['puf'], st['scf']
        with np.errstate(divide='ignore', invalid='ignore'):
            ratio, cratio = q['share'] / p['share'], q['cum'] / p['cum']

        def first90(r):
            for k in range(1, 8):
                if np.isfinite(r[k]) and r[k] >= 0.9:
                    return k
            return None
        f1, f2 = first90(ratio), first90(cratio)
        name = v.replace('_', ' ')
        ab = ', absolute amount' if v == 'capital_gains' else ''
        lines += [f'## {name.capitalize()}', '',
                  f'Weighted totals: PUF ${p["total"] / 1e9:,.1f} billion, SCF ${q["total"] / 1e9:,.1f} billion, '
                  f'ratio {q["total"] / p["total"]:.3f}.', '',
                  f'### Share of units by bracket ({name}{ab})', '',
                  '| Bracket ($) | PUF share | SCF share | SCF / PUF | PUF records | SCF records |',
                  '|---|---:|---:|---:|---:|---:|']
        for k in range(8):
            lines.append(f'| {LABELS[k]} | {fmt_pct(p["share"][k])} | {fmt_pct(q["share"][k])} | '
                         f'{fmt_ratio(ratio[k])} | {p["n"][k]:,} | {q["n"][k]:,} |')
        if v == 'capital_gains':
            lines += ['', f'Share with a negative amount (a net loss): PUF {fmt_pct(p["neg"])}, '
                      f'SCF {fmt_pct(q["neg"])}, SCF / PUF {fmt_ratio(q["neg"] / p["neg"])}.']
        lines += ['', '### Reporting rate taking the PUF as the truth', '',
                  '| Bracket ($) | By bracket: SCF / PUF | Cumulative: PUF share at or above | '
                  'Cumulative: SCF share at or above | Cumulative: SCF / PUF |',
                  '|---|---:|---:|---:|---:|']
        for k in range(8):
            r1 = fmt_ratio(ratio[k]) + (' *' if k == f1 else '')
            r2 = fmt_ratio(cratio[k]) + (' *' if k == f2 else '')
            lines.append(f'| {LABELS[k]} | {r1} | {fmt_pct(p["cum"][k])} | {fmt_pct(q["cum"][k])} | {r2} |')
        lines += ['', f'### Weighted mean amount within each bracket ($' + ab + ')', '',
                  '| Bracket ($) | PUF mean | SCF mean | SCF / PUF |', '|---|---:|---:|---:|']
        for k in range(8):
            pm, qm = p['mean'][k], q['mean'][k]
            r = qm / pm if k > 0 and np.isfinite(pm) and np.isfinite(qm) else np.nan
            pm_s = '' if not np.isfinite(pm) else f'{pm:,.0f}'
            qm_s = '' if not np.isfinite(qm) else f'{qm:,.0f}'
            lines.append(f'| {LABELS[k]} | {pm_s} | {qm_s} | {fmt_ratio(r)} |')
        lines.append('')
    return '\n'.join(lines)


def write(D, df, distortion_text):
    """Decide, print, and write translation_<D>.json and detection_<D>.md."""
    res = translate(df)
    print(f'10_translate, {D}: {distortion_text}')
    print(decision_table(res), flush=True)
    json.dump({'distortion': D, 'rule': RULE_TEXT, 'variables': res},
              open(f'{R}/data/translation_{D}.json', 'w'), indent=1)
    os.makedirs(f'{R}/score', exist_ok=True)
    open(f'{R}/score/detection_{D}.md', 'w').write(detection_md(
        df, f'Detection table, Set A distortion {D}',
        f'Distortion: {distortion_text} PUF 2015 against the distorted SCF 2016 wave (tax year 2015). '
        f'Written by `research/rebuild/minimal/10_translate.py`.', res))
    return res


if __name__ == '__main__':
    D = sys.argv[1]
    df = pd.read_parquet(f'{R}/data/stacked_A_{D}_R0.parquet', columns=['source', 'weight'] + VARS)
    write(D, df, f'see data/band_edges_A_{D}_R0.json (meta).')
