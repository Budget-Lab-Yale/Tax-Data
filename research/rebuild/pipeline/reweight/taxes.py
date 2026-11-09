"""Taxes computed inside the model: each record's federal income and payroll taxes under the target year's law, from
tax-simulator-gpu, on the year's start, before the reweighting (piece 3 of the overnight plan of 29-30 October; the
design is scratch explore/2026-10-22/targets/calculator_api/REPORT.md).

The tilt moves weights only, so a record's computed taxes are fixed once computed and the computed items can be
targeted like any drawn item (kind "calculator" in targets_config/; target_list.expand). tilt.py calls compute() on
the start after the top of the file and the non-microdata imputations.

1. The batch command (run_batch). The file becomes the calculator's input frame (calculator.convert: the contract,
   one row per column the calculator reads, with Tax-Simulator's names), written as a parquet file keyed by record
   id. tax-simulator-gpu's batch command (`python -m taxmodel.run.batch`, a local branch taxdata-batch-1103 of that
   repository, not pushed) runs it under the year's law in the calculator's own Python environment, from a copy of
   the repository at the commit in settings taxes.commit (pin: written once with git archive under
   <out>/lib/tax-simulator-gpu_<first 12 characters>). Its output, one row per record keyed by the same id, is read
   back, and the columns in OUTPUTS are joined to the file by record id. Every record must come back, with no
   missing value.
2. Marginal rates on wages: with settings wage_increment, the batch command reruns the chain with the primary
   earner's wages raised by that many dollars; calc_mtr_wages_iit and calc_mtr_wages_payroll are the change in
   income tax after every credit and in payroll tax, per dollar.

Writes in <tilt>/taxes/: the calculator's input and output files of the run with their manifests, the conversion
notes and taxes_summary.md. (Until 6 November a search for two QBI dials ran here, against Treasury's share and SOI's
deduction, with a second calculator run; the QBI columns are now set before the calculator, non_micro_imputations.qbi.)
"""
import json
import os
import subprocess

import numpy as np
import pandas as pd

import common
import dollars as D

Y = common.TARGET_YEAR
S = common.SETTINGS.get('taxes', {})
HERE = os.path.dirname(os.path.abspath(__file__))

# The columns joined to the file: file column -> (the calculator's column, or a formula on its columns, and meaning).
# Each is the calculator's own name with calc_ in front, so a computed item is never mistaken for a drawn one.
OUTPUTS = {
    'calc_claimed_medical': ('claimed_medical', 'medical deductions on itemizers'),
    'calc_claimed_taxes': ('claimed_taxes', 'taxes paid deductions on itemizers'),
    'calc_claimed_interest': ('claimed_interest', 'interest deductions on itemizers'),
    'calc_claimed_charity': ('claimed_charity', 'charitable deductions on itemizers'),
    'calc_itemized_total_minus_four': ('itemized_total_minus_four', 'itemized deductions less the four named categories'),
    'calc_agi': ('agi', 'adjusted gross income as the calculator computes it'),
    'calc_itemizing': ('itemizing', '1 if the return itemizes (itemized deductions above the standard deduction)'),
    'calc_item_ded': ('item_ded', 'itemized deductions after the law\'s limits, whether or not the return itemizes'),
    'calc_item_ded_claimed': ('item_ded x itemizing', 'itemized deductions claimed: on itemizing returns only'),
    'calc_std_ded': ('std_ded', 'the standard deduction the return is entitled to'),
    'calc_qbi_ded': ('qbi_ded', 'the qualified business income deduction'),
    'calc_txbl_inc': ('txbl_inc', 'taxable income'),
    'calc_liab_bc': ('liab_bc', 'income tax before credits: regular tax, AMT and excess advance premium tax credit '
                                'repaid (SOI Table 1.4\'s line)'),
    'calc_liab_amt': ('liab_amt', 'alternative minimum tax'),
    'calc_ctc_nonref': ('ctc_nonref', 'child tax credit and credit for other dependents, the nonrefundable part'),
    'calc_ctc_ref': ('ctc_ref', 'additional child tax credit (the refundable part)'),
    # (autoresearch, 4 November) the education credits, for Table 3.3's targets
    'calc_ed_nonref': ('ed_nonref', 'education credits used against tax (lifetime learning and the nonrefundable part of '
                                    'the American opportunity credit)'),
    'calc_ed_ref': ('ed_ref', 'the refundable part of the American opportunity credit'),
    'calc_cdctc_nonref': ('cdctc_nonref', 'credit for child and dependent care, used against tax'),
    'calc_savers_nonref': ('savers_nonref', 'retirement savings contributions credit, used against tax'),
    # (autoresearch, 4 November) itemizers' cash gifts and gifts other than cash as reported on Schedule A (before the
    # deduction's limits), for Table 2.1's targets; set in compute from the calculator's input and its itemizing flag
    'calc_char_cash_itemizers': ('input char_cash x itemizing', 'cash gifts to charity on itemizing returns, as reported'),
    'calc_char_noncash_itemizers': ('input char_noncash x itemizing', 'gifts to charity other than cash on itemizing returns, as reported'),
    'calc_eitc': ('eitc', 'earned income tax credit, all of it (refundable and used against tax)'),
    'calc_nonref': ('nonref', 'nonrefundable credits used'),
    'calc_ref': ('ref', 'refundable credits'),
    'calc_liab_after_credits': ('liab_bc - nonref - ref_iit', 'income tax after credits (SOI Table 3.3\'s line: '
                                'before credits less nonrefundable credits and the refundable credits used against '
                                'tax)'),
    'calc_liab_niit': ('liab_niit', 'net investment income tax'),
    'calc_liab_iit': ('liab_iit', 'total income tax: after credits plus the net investment income tax (SOI Table '
                                  '3.3\'s "total income tax")'),
    'calc_liab_iit_net': ('liab_iit_net', 'income tax less every refundable credit (negative for a net refund)'),
    'calc_liab_seca': ('liab_seca', 'self-employment tax'),
    'calc_liab_pr': ('liab_pr', 'payroll tax, Social Security and Medicare, both halves, employees and the '
                                'self-employed'),
    'calc_mtr_wages_iit': ('mtr_wages_iit', 'marginal rate of income tax after every credit on the primary earner\'s '
                                            'wages (settings taxes.wage_increment dollars more)'),
    'calc_mtr_wages_payroll': ('mtr_wages_payroll', 'marginal rate of payroll tax on the primary earner\'s wages'),
}


# ---- 1. the batch command ------------------------------------------------------------------------------------
def lib():
    return f'{common.OUT}/lib/tax-simulator-gpu_{S["commit"][:12]}'


def pin():
    """The pinned copy of tax-simulator-gpu: written once from the repository with git archive; checked every run."""
    path = lib()
    stamp = f'{path}/PINNED_COMMIT'
    if not os.path.exists(stamp):
        os.makedirs(path, exist_ok=True)
        archive = subprocess.run(['git', '-C', S['repo'], 'archive', S['commit']], check=True, capture_output=True)
        subprocess.run(['tar', '-x', '-C', path], input=archive.stdout, check=True)
        with open(stamp, 'w') as fh:
            fh.write(S['commit'] + '\n')
    got = open(stamp).read().strip()
    assert got == S['commit'], f'{path} holds commit {got}, settings taxes.commit says {S["commit"]}'
    return path


def run_batch(frame, year, folder, tag, wage_increment=0.0, law_sections=()):
    """The calculator's input frame (calculator.convert's) through tax-simulator-gpu's batch command under year's law.
    Writes <folder>/taxes_input_<tag>.parquet and taxes_output_<tag>.parquet (with its manifest). Returns the output
    in the input's record order and the manifest."""
    path = pin()
    os.makedirs(folder, exist_ok=True)
    src, dst = f'{folder}/taxes_input_{tag}.parquet', f'{folder}/taxes_output_{tag}.parquet'
    frame.to_parquet(src, index=False)
    env = {k: v for k, v in os.environ.items() if k not in ('PYTHONPATH', 'LD_LIBRARY_PATH', 'PYTHONHOME')}
    env.update(PYTHONPATH=f'{path}/src', PYTHONUNBUFFERED='1')
    env['JAX_PLATFORMS'] = 'cpu'   # the calculator runs on the CPU, as the whole tilt does
    cmd = [S['python'], '-m', 'taxmodel.run.batch', '--records', src, '--year', str(int(year)), '--out', dst,
           '--seed', str(S['draw_seed']), '--commit', S['commit'], '--wage-increment', str(float(wage_increment))]
    if law_sections:
        cmd += ['--law-sections', *law_sections]
    common.say('taxes: ' + ' '.join(cmd))
    subprocess.run(cmd, cwd=path, env=env, check=True)
    out = pd.read_parquet(dst)
    man = json.load(open(f'{dst}.manifest.json'))
    assert len(out) == len(frame) and out['id'].is_unique, f'{dst}: {len(out)} rows for {len(frame)} records'
    out = frame[['id']].merge(out, on='id', how='left', validate='one_to_one', indicator=True)
    gone = int((out.pop('_merge') != 'both').sum())
    assert not gone, f'{dst}: {gone} records did not come back'
    assert not man['missing_values_in_outputs'], f'{dst}: missing values {man["missing_values_in_outputs"]}'
    return out, man


def joined_columns(out):
    """OUTPUTS from a batch output, in the output's record order."""
    x = lambda c: out[c].to_numpy(float)
    cols = {}
    for name, (src, _) in OUTPUTS.items():
        if src == 'item_ded x itemizing':
            cols[name] = x('item_ded') * (x('itemizing') > 0)
        elif src == 'liab_bc - nonref - ref_iit':
            cols[name] = x('liab_bc') - x('nonref') - x('ref_iit')
        elif src in out:
            cols[name] = x(src)
        elif src.startswith('mtr_'):
            continue   # no wage increment in this run
        elif src.startswith('input '):
            continue   # set in compute, from the calculator's input
        else:
            raise KeyError(f'the calculator wrote no column {src}')
    return pd.DataFrame(cols)


# ---- the refund rule and the greedy Schedule A step (30 October, decisions 47 and 49) ----------------------------
SCHEDULE_A = {   # Table 2.1's category: (the file's input columns, the calculator's claimed column)
    'medical': (['med_exp'], 'claimed_medical'),
    'taxes': (['salt_inc_sales', 'salt_prop'], 'claimed_taxes'),
    'interest': (['first_mort_int'], 'claimed_interest'),
    'charity': (['char_cash', 'char_noncash'], 'claimed_charity'),
}


def apply_repealed_adjustments(df, year, w, ctx):
    """(Autoresearch, 4 November) Adjustments the law has repealed by the year, set to zero (settings
    taxes.repealed_adjustments: item -> first tax year without it): the domestic production activities deduction from
    2018 (the 2017 law), the tuition and fees deduction from 2021 (the 2020 law). The draw carries 2015's claims into
    every year; the calculator already ignores them under the year's law, so the file's adjustments and AGI disagreed
    with its own: run3's 2019 file subtracted $3.8 billion of the production deduction from AGI, and its 2023 file $3.9
    billion of it and $3.9 billion of tuition and fees (1.6 million returns), part of why it had 41.6 million returns
    with adjustments against SOI's 35.8 million. The adjustments total and other_agi (adjustments sign -1 in
    dollars.FOLD) move by the removed dollars, then taxable Social Security is computed again on the new AGI."""
    rep = {k: int(v) for k, v in S.get('repealed_adjustments', {}).items() if not k.startswith('_')}
    gone = [k for k, y0 in rep.items() if year >= y0 and k in df]
    if not gone:
        return df
    df = df.copy()
    filer = df['filer'].to_numpy() == 1
    assert D.FOLD['adjustments'] == -1, 'other_agi holds the adjustments with sign -1 (dollars.FOLD)'
    lines = []
    for k in gone:
        x = df[k].to_numpy(float)
        assert np.isfinite(x).all(), f'{k}: a missing value'
        lines.append(f'{k} ${(w * x)[filer].sum() / 1e9:.2f} billion on {w[filer & (x != 0)].sum() / 1e6:.2f} million filers')
        df['adjustments'] = df['adjustments'].to_numpy(float) - x
        df['other_agi'] = df['other_agi'].to_numpy(float) + x
        df[k] = 0.0
    D.retirement_items(df, in_agi=True)
    ctx['notes'].append(f'repealed adjustments set to zero under {year} law (start weights): ' + '; '.join(lines))
    return df


def apply_refund_rule(df, year, w, folder, ctx, C):
    """Decision 47: taxable state refunds set to zero on records the calculator says do not itemize under the year's
    law, and the total built from them (other_agi) lowered by the same dollars; then taxable Social Security, which
    depends on AGI, computed again (3 November). One calculator run on the grown start gives the flag. Returns the
    frame; the counts go to ctx['refund']."""
    inp, _ = C.convert(df)
    out, man = run_batch(inp, year, folder, 'refund', wage_increment=0.0)
    assert (inp['id'].to_numpy() == df['record_id'].to_numpy()).all(), 'record order'
    item = out['itemizing'].to_numpy(float) > 0
    ref = df['state_ref'].to_numpy(float)
    assert np.isfinite(ref).all() and (ref >= 0).all(), 'state_ref: a missing or negative value'
    filer = df['filer'].to_numpy() == 1
    zero = ~item & (ref > 0)
    row = {'returns with a refund before (millions)': w[filer & (ref > 0)].sum() / 1e6,
           'dollars before ($ billion)': (w * ref)[filer].sum() / 1e9,
           'records zeroed (unweighted)': int(zero.sum()),
           'itemizing filers (millions)': w[filer & item].sum() / 1e6}
    df = df.copy()
    kept = np.where(item, ref, 0.0)
    # (round 10) a refund received in year t repays year t-1's taxes, so the cap on the deduction bears on it only from
    # 2019 (refunds of 2017 taxes, deducted with no cap, filled 2018: SOI has 7.3 million returns with a refund at AGI
    # $100,000-200,000 that year, and the rule left 1.8 million)
    if int(year) >= int(S['refund_tax_benefit_from']):
        # (autoresearch, 4 November) The tax benefit rule (IRS Publication 525, worksheet 2): a refund of state income
        # tax is taxable only to the extent that deducting the tax lowered tax. Two limits on an itemizer: (1) the
        # taxes deduction the return claimed less the one it would have claimed had it paid the refund less tax;
        # under a cap on the deduction (2018 on) a return whose taxes paid exceed the cap by more than the refund got
        # no benefit, so its refund is not taxable; (2) the itemized deductions less the standard deduction. The
        # current year's taxes paid and deductions stand in for last year's, as the itemizing flag does above. The
        # cap is read from the calculator's own result (claimed taxes below taxes paid), so the rule needs no
        # parameter of the year's law. In run3's 2019 the draw kept 2015's refunds at every income: SOI has 6,818
        # returns with a refund at AGI $2-5 million against the file's 61,269.
        paid = df['salt_inc_sales'].to_numpy(float) + df['salt_prop'].to_numpy(float)
        claimed = out['claimed_taxes'].to_numpy(float)
        excess = np.maximum(out['item_ded'].to_numpy(float) - out['std_ded'].to_numpy(float), 0.0)
        assert np.isfinite(paid).all() and np.isfinite(claimed).all() and np.isfinite(excess).all(), \
            'refund rule: a missing taxes paid, claimed taxes or deduction'
        benefit = np.maximum(claimed - np.maximum(paid - ref, 0.0), 0.0)
        kept = np.where(item, np.minimum(ref, np.minimum(benefit, excess)), 0.0)
        # (round 3) Returns still report part of the refunds the rule takes off: SOI has taxable refunds on about 5%
        # of returns at AGI $2-5 million in 2019 and 2023, where taxes paid are far above the cap. Without this share
        # the rule left 800 such returns in 2019 against SOI's 6,818, and its $1 million and over cell could not be
        # met. A record keeps its whole refund with this chance, drawn from its record id (the same draw every run).
        share = float(S.get('refund_reported_anyway', 0.0))
        if share > 0:
            rid = df['record_id'].to_numpy().astype(np.uint64)
            u = ((rid * np.uint64(2654435761) + np.uint64(97)) % np.uint64(2 ** 32)).astype(float) / 2 ** 32
            kept = np.where(item & (u < share), ref, kept)
        row.update({'itemizer refunds cut by the tax benefit rule (unweighted)': int((item & (kept < ref - 1e-6)).sum()),
                    'itemizer refund dollars removed by the tax benefit rule, filers ($ billion)':
                        (w * (np.where(item, ref, 0.0) - kept))[filer].sum() / 1e9})
    df['state_ref'] = kept
    ref2 = df['state_ref'].to_numpy(float)
    # (1 November, decision 75) every total built from the refund is recomputed with it: other_agi holds the refund
    # (dollars.FOLD, sign +1), so it falls by what the rule removed. Without this the file's other_agi (and AGI, which
    # adds it) stayed above its parts' sum on every record the rule zeroed.
    assert (ref2 <= ref + 1e-9).all(), 'the refund rule only removes refunds'
    assert D.FOLD['state_ref'] == 1, 'other_agi is built from state_ref with sign +1 (dollars.FOLD)'
    df['other_agi'] = df['other_agi'].to_numpy(float) + D.FOLD['state_ref'] * (ref2 - ref)
    # (3 November) taxable Social Security, computed by the section 86 worksheet on AGI, is computed again on the lower
    # AGI (dollars.retirement_items: the old amount is taken out of other_agi first)
    ss_before = df['ss_taxable'].to_numpy(float).copy()
    D.retirement_items(df, in_agi=True)
    ss_after = df['ss_taxable'].to_numpy(float)
    row.update({'records whose taxable Social Security fell with the refund (unweighted)': int((ss_after < ss_before - 1e-6).sum()),
                'taxable Social Security removed with the refund, filers ($ billion)': (w * (ss_before - ss_after))[filer].sum() / 1e9})
    row.update({'returns with a refund after (millions)': w[filer & (ref2 > 0)].sum() / 1e6,
                'dollars after ($ billion)': (w * ref2)[filer].sum() / 1e9,
                'non-filer dollars before ($ billion)': (w * ref)[~filer].sum() / 1e9})
    ctx['refund'] = row
    ctx['notes'].append(
        f"refund rule (decision 47): taxable state refunds zeroed on records that do not itemize: filer returns with "
        f"a refund {row['returns with a refund before (millions)']:.2f} million -> "
        f"{row['returns with a refund after (millions)']:.2f} million, dollars ${row['dollars before ($ billion)']:.1f} "
        f"billion -> ${row['dollars after ($ billion)']:.1f} billion (start weights)")
    return df


def table21_averages(year):
    """Table 2.1's dollars per claiming return, all AGI groups, by category: sum of amounts over sum of counts."""
    path = os.path.join(HERE, S['greedy_schedule_a']['table21'])
    t = pd.read_csv(path)
    # (1 November, decision 54) taxes paid is Table 2.1's state and local line (category taxes_state_local), as the targets use
    t = t.assign(category=t.category.replace({'taxes_state_local': 'taxes'}))[t.category != 'taxes']
    t = t[(t.year == year) & t.category.isin(list(SCHEDULE_A))]   # the other residual has an amount only
    if not len(t):
        return None
    assert t[['count', 'amount']].notna().all().all(), f'Table 2.1 {year}: a missing count or amount'
    return {c: (float(g['amount'].sum()), float(g['count'].sum())) for c, g in t.groupby('category') if c in SCHEDULE_A}


def prior_factor(col, year):
    """The declared growth of a PUF amount from the PUF's year to the target year (growth.yaml), for the report; NaN with a
    reason where the series cannot be read."""
    import growth as GR
    try:
        spec = GR.line(col, year)['source']
        t, s0 = year, common.PUF_YEAR
        f = GR._factor(spec, t) / GR._factor(spec, s0) if spec['series'] == 'soi 1.4' else GR.level(spec, t) / GR.level(spec, s0)
        return float(f), GR.describe(col, year)
    except Exception as e:   # reported in the table, not hidden
        return float('nan'), f'not read: {type(e).__name__}: {str(e)[:80]}'


def greedy_schedule_a(df, year, w, folder, ctx, C):
    """Decision 49, the greedy version of ALGO_GROWTH_SEARCH.md: each category's input factor s is scaled by the ratio
    of Table 2.1's dollars per claiming return to the start's, passes times through the calculator, s kept within one
    plus or minus max_move of the prior (s = 1). Returns the frame with the inputs multiplied, or the frame unchanged
    where the year has no Table 2.1."""
    G = S['greedy_schedule_a']
    target = table21_averages(year)
    if target is None:
        ctx['notes'].append(f'greedy Schedule A step (decision 49): no Table 2.1 for {year}; the declared priors stand')
        return df
    assert set(target) == set(SCHEDULE_A), f'Table 2.1 {year} categories {sorted(target)}'
    filer = df['filer'].to_numpy() == 1
    base = df.copy()
    s = {c: 1.0 for c in SCHEDULE_A}
    lo, hi = 1.0 - G['max_move'], 1.0 + G['max_move']
    rows = []
    for p in range(int(G['passes'])):
        cur = base.copy()
        for c, (cols, _) in SCHEDULE_A.items():
            for col in cols:
                assert col in cur and np.isfinite(cur[col].to_numpy(float)).all(), f'{col}: missing from the file or missing values'
                cur[col] = base[col].to_numpy(float) * s[c]
        inp, _ = C.convert(cur)
        out, man = run_batch(inp, year, folder, f'greedy{p + 1}', wage_increment=0.0)
        assert (inp['id'].to_numpy() == cur['record_id'].to_numpy()).all(), 'record order'
        for c, (cols, claimed) in SCHEDULE_A.items():
            x = out[claimed].to_numpy(float)
            claim = filer & (x > 0)
            avg = float((w * x)[claim].sum() / w[claim].sum())
            soi_avg = target[c][0] / target[c][1]
            new = float(np.clip(s[c] * soi_avg / avg, lo, hi))
            rows.append({'pass': p + 1, 'category': c, 'factor used (s)': s[c], 'start $ per claiming return': avg,
                         'Table 2.1 $ per claiming return': soi_avg, 'claiming returns, start (millions)': w[claim].sum() / 1e6,
                         'Table 2.1 returns (millions)': target[c][1] / 1e6, 'next factor': new,
                         'held at the limit': bool(new in (lo, hi))})
            s[c] = new
    tab = pd.DataFrame(rows)
    pri = []
    for c, (cols, _) in SCHEDULE_A.items():
        f, src = prior_factor(cols[0], year)
        pri.append({'category': c, 'input columns': ', '.join(cols), 'declared growth from the PUF year (prior)': f,
                    'prior series': src, 'multiplier s on the prior': s[c],
                    'factor used, prior x s': f * s[c] if np.isfinite(f) else float('nan')})
    pri = pd.DataFrame(pri)
    tab.to_csv(f'{folder}/schedule_a_passes.csv', index=False)
    pri.to_csv(f'{folder}/schedule_a_factors.csv', index=False)
    ctx['greedy'] = pri
    ctx['greedy_passes'] = tab
    ctx['notes'].append('greedy Schedule A step (decision 49): multipliers on the priors ' +
                        ', '.join(f'{c} {v:.3f}' for c, v in s.items()))
    out_df = df.copy()
    for c, (cols, _) in SCHEDULE_A.items():
        for col in cols:
            out_df[col] = base[col].to_numpy(float) * s[c]
    return out_df


# ---- the stage inside the tilt ---------------------------------------------------------------------------------
def compute(df, year, w, folder):
    """Every record's computed taxes joined to the file (module docstring). df: the start, after the non-microdata
    imputations; w: the start's weights. Returns the frame with OUTPUTS and a context for the summary."""
    import calculator as C
    ctx = {'notes': [], 'runs': {}}
    # A start from an earlier year's tilted file (targets.yaml start: from_year) carries that year's computed columns,
    # computed under that year's law on that year's dollars. They are dropped and computed again here. Until the
    # review of 30 October an assertion stood here, which stopped every chained year (2024 on).
    stale = [c for c in OUTPUTS if c in df]
    if stale:
        df = df.drop(columns=stale)
        ctx['notes'].append(f'the start\'s computed columns (from its own year) dropped and computed again under '
                            f'{int(year)} law: {len(stale)} columns')
    df = apply_repealed_adjustments(df, int(year), w, ctx)
    df = apply_refund_rule(df, year, w, folder, ctx, C)
    df = greedy_schedule_a(df, year, w, folder, ctx, C)
    inp, notes = C.convert(df)
    ctx['conversion'] = notes

    out, man = run_batch(inp, year, folder, 'final', wage_increment=float(S.get('wage_increment', 0)))
    ctx['runs']['final'] = man
    cols = joined_columns(out)
    assert (inp['id'].to_numpy() == df['record_id'].to_numpy()).all(), 'record order'
    # (autoresearch, 4 November) the deductible half of self-employment tax is the calculator's (liab_seca_er,
    # computed from each person's self-employment income and wages), not the drawn one: the calculator never reads
    # the drawn deduction, so the file's adjustments and AGI disagreed with its own (run3's 2019 file: the drawn
    # deduction $34.6 billion, the calculator's $29.9 billion, on 21 million returns). The adjustments total and
    # other_agi (adjustments sign -1 in dollars.FOLD) move with it, then taxable Social Security is computed again,
    # so the file's AGI is the calculator's.
    new_se = out['liab_seca_er'].to_numpy(float)
    assert np.isfinite(new_se).all() and (new_se >= 0).all(), 'liab_seca_er: a missing or negative value'
    old_se = df['se_tax_ded'].to_numpy(float)
    filer_ = df['filer'].to_numpy() == 1
    ctx['notes'].append(f'deductible half of self-employment tax from the calculator (start weights): drawn '
                        f'${(w * old_se)[filer_].sum() / 1e9:.2f} billion, calculator\'s ${(w * new_se)[filer_].sum() / 1e9:.2f} '
                        f'billion on filers; {int((np.abs(new_se - old_se) > 1).sum()):,} records changed')
    df = df.copy()
    df['se_tax_ded'] = new_se
    df['adjustments'] = df['adjustments'].to_numpy(float) + (new_se - old_se)
    df['other_agi'] = df['other_agi'].to_numpy(float) - (new_se - old_se)
    D.retirement_items(df, in_agi=True)
    item = out['itemizing'].to_numpy(float) > 0
    for name, (src, _) in OUTPUTS.items():
        if src.startswith('input '):
            col = src.split(' ')[1]
            cols[name] = inp[col].to_numpy(float) * item
    cols.index = df.index
    df = pd.concat([df, cols], axis=1)
    bad = [c for c in cols if not np.isfinite(df[c].to_numpy(float)).all()]
    assert not bad, f'computed columns with missing or infinite values: {bad}'
    with open(f'{folder}/conversion_notes.txt', 'w') as fh:
        fh.write('\n'.join(notes) + '\n')
    ctx['notes'].append(f'taxes computed under {int(year)} law by tax-simulator-gpu {S["commit"][:12]} '
                        f'({", ".join(man["devices"])}, {man["seconds"]["frame steps and taxes"]} s); columns joined: '
                        + ', '.join(cols.columns))
    return df, ctx


def summary_lines(ctx, df, w0, w1):
    """The taxes section of the tilt's summary.md, also written to <tilt>/taxes/taxes_summary.md."""
    filer = df['filer'].to_numpy() == 1
    lines = ['## Taxes computed inside the model (taxes.py)', ''] + [f'- {n}' for n in ctx['notes']] + ['']
    tab = []
    for c in ['calc_agi', 'calc_item_ded_claimed', 'calc_qbi_ded', 'calc_txbl_inc', 'calc_liab_bc', 'calc_liab_amt',
              'calc_ctc_nonref', 'calc_ctc_ref', 'calc_eitc', 'calc_liab_after_credits', 'calc_liab_iit',
              'calc_liab_seca', 'calc_liab_pr']:
        x = df[c].to_numpy(float)
        tab.append({'column': c, 'returns with any, before (millions)': w0[filer & (x != 0)].sum() / 1e6,
                    'after (millions)': w1[filer & (x != 0)].sum() / 1e6,
                    'filers, before ($ billion)': (w0 * x)[filer].sum() / 1e9,
                    'after ($ billion)': (w1 * x)[filer].sum() / 1e9,
                    'non-filers, after ($ billion)': (w1 * x)[~filer].sum() / 1e9})
    lines += [pd.DataFrame(tab).to_markdown(index=False, floatfmt=',.2f'), '']
    if 'refund' in ctx:
        lines += ['Taxable state refunds after the rule (decision 47), start weights:', '',
                  pd.Series(ctx['refund']).to_frame('').to_markdown(floatfmt=',.3f'), '']
    if 'greedy' in ctx:
        lines += ['Schedule A factors (decision 49), each against its declared prior:', '',
                  ctx['greedy'].to_markdown(index=False, floatfmt=',.4f'), '',
                  'The passes (dollars per claiming return, start against Table 2.1):', '',
                  ctx['greedy_passes'].to_markdown(index=False, floatfmt=',.4f'), '']
    return lines


# ---- the report on a finished file ------------------------------------------------------------------------------
def evaluate(df, rows, w0, w1):
    """The calculator targets' rows evaluated on the file at weights w0 and w1 (solver.fit_table's columns)."""
    import solver as SV
    import target_list as TLS
    if not len(rows):   # (review, 30 October) a year with no calculator entries (every year but 2019 for now)
        return pd.DataFrame(columns=['group', 'target', 'value', 'kind', 'before', 'after', 'se', 'miss_in_se'])
    F, T, averages = TLS.conditions(df, rows, w0)
    se, identity = rows['se'].to_numpy(float), rows['identity'].to_numpy(bool)
    return SV.fit_table(w0, w1, F, T, se, averages, rows, identity)
