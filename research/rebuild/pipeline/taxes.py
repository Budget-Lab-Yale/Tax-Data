"""Taxes computed inside the model: each record's federal income and payroll taxes under the target year's law, from
tax-simulator-gpu, on the year's start, before the reweighting (piece 3 of the overnight plan of 29-30 October; the
design is scratch explore/2026-10-22/targets/calculator_api/REPORT.md).

The tilt moves weights only, so a record's computed taxes are fixed once computed and the computed items can be
targeted like any drawn item (kind "calculator" in targets_config/; target_list.expand). tilt.py calls compute() on
the start after the top of the file and the non-microdata imputations.

1. The batch command (run_batch). The file becomes the calculator's input frame (calculator.convert: the contract,
   one row per column the calculator reads, with Tax-Simulator's names), written as a parquet file keyed by record
   id. tax-simulator-gpu's batch command (`python -m taxmodel.run.batch`, a local branch taxdata-batch of that
   repository, not pushed) runs it under the year's law in the calculator's own Python environment, from a copy of
   the repository at the commit in settings taxes.commit (pin: written once with git archive under
   <out>/lib/tax-simulator-gpu_<first 12 characters>). Its output, one row per record keyed by the same id, is read
   back, and the columns in OUTPUTS are joined to the file by record id. Every record must come back, with no
   missing value.
2. The QBI dials (scratch explore/2026-10-28/qbi_plan/SIMPLE_PLAN.md, step 4; solve_qbi_dials). The non-microdata
   imputations draw each business's specified-service status with chance b x c(industry group) and its wage bill as
   a x outside workers x average pay. The first calculator run gives each record's taxable income before the QBI
   deduction, which a and b do not move; the deduction is then the calculator's own formula (qbi_formula, checked
   against the calculator's result on every record), so each dial is found by bisection:
     a: among owners of exactly one business (Schedule C, partnership or S corporation; no farm income) that is not a
        specified service, with taxable income above the top of the phase-in range, the deduction as a share of 20% of
        their business income equals Treasury's measured share (Goodman, Lim, Sacerdote and Whitten, NBER Working
        Paper 28680, Table A1, column 1: the published industries matched to SCF industry groups, each group's
        average weighted by the file's business income of those owners in the group);
     b: with a fixed, the deduction on returns with AGI of $500,000 or more (the file's AGI, as every IRS target)
        equals SOI Table 1.4's.
   Which owners are specified services moves with b, so the two are solved in turn (settings rounds). Then the QBI
   columns are imputed again with the solved values and the calculator runs a second time; that run's columns are
   the ones joined.
3. Marginal rates on wages: with settings wage_increment, the batch command reruns the chain with the primary
   earner's wages raised by that many dollars; calc_mtr_wages_iit and calc_mtr_wages_payroll are the change in
   income tax after every credit and in payroll tax, per dollar.

Writes in <tilt>/taxes/: the calculator's input and output files of each run with their manifests, the conversion
notes, qbi_dials.csv and taxes_summary.md.
The stage `taxes` (python taxes.py report <file> <folder>) computes the taxes on a finished file with its own columns,
with no dials solved, and evaluates the year's calculator targets on its weights before and after the tilt
(report()); it is how a file made before this stage is compared.
"""
import json
import os
import subprocess
import sys

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
QBI_LAW = 'qbi_ded'   # the section of the year's law the batch command writes per record for the QBI formula


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


def law_year(year):
    return int(S.get('law_year') or year)


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
    if S.get('device', 'cpu') == 'cpu':
        env['JAX_PLATFORMS'] = 'cpu'
    else:
        env['XLA_PYTHON_CLIENT_PREALLOCATE'] = 'false'
    cmd = [S['python'], '-m', 'taxmodel.run.batch', '--records', src, '--year', str(law_year(year)), '--out', dst,
           '--seed', str(S['draw_seed']), '--commit', S['commit'], '--wage-increment', str(float(wage_increment))]
    if S.get('law_alternative'):
        cmd += ['--alternative', S['law_alternative']]
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
        else:
            raise KeyError(f'the calculator wrote no column {src}')
    return pd.DataFrame(cols)


# ---- 2. the QBI dials ------------------------------------------------------------------------------------------
KINDS = ['sole_prop', 'part', 'scorp', 'farm']   # the calculator's four kinds of business


def qbi_parts(inp, out):
    """What the QBI formula reads that the dials do not move: the provisional taxable income (AGI less the larger of
    the standard and itemized deductions and the personal exemptions, at least 0, as the calculator's QBI step), each
    kind's income as the calculator counts it, the cap's base, and the year's QBI law for each record."""
    x = lambda d, c: d[c].to_numpy(float)
    txbl = np.maximum(x(out, 'agi') - np.maximum(x(out, 'item_ded'), x(out, 'std_ded')) - x(out, 'pe_ded'), 0.0)
    inc = np.column_stack([x(inp, 'sole_prop1') + x(inp, 'sole_prop2'), x(out, 'part'), x(out, 'scorp'),
                           x(inp, 'farm1') + x(inp, 'farm2')])
    law = {c[len(f'law.{QBI_LAW}.'):]: x(out, c) for c in out.columns if c.startswith(f'law.{QBI_LAW}.')}
    assert law, 'the batch output has no QBI law columns (--law-sections qbi_ded)'
    cap_base = np.maximum(txbl - x(inp, 'div_pref') - x(out, 'kg_pref'), 0.0)
    return {'txbl': txbl, 'inc': inc, 'law': law, 'cap_base': cap_base}


def qbi_formula(p, sstb, wagebill):
    """The QBI deduction of every record, the calculator's formula (tax-simulator-gpu deductions.calc_qbi_ded):
    each kind's 20% of its income, less, above the threshold, the phase-in share of the excess over the wage limit
    (50% of the wage bill; for a specified service the whole of it), at least 0, summed over the kinds; under OBBBA
    law (po_type 1) the better of the wage-limited amount and the income-based amount after its reduction; held to
    20% of taxable income less qualified dividends and capital gains taxed at the preferential rates. sstb and
    wagebill: records x the four kinds (KINDS)."""
    L, inc, txbl = p['law'], p['inc'], p['txbl']
    col = lambda k: L[k][:, None]
    sstb = np.nan_to_num(sstb.astype(float))
    wages = np.nan_to_num(wagebill.astype(float))
    thr = np.where(sstb == 1, col('po_thresh_sstb'), col('po_thresh_non_sstb'))
    width = np.where(sstb == 1, col('po_range_sstb'), col('po_range_non_sstb'))
    share = np.minimum(np.maximum(txbl[:, None] - thr, 0.0) / width, 1.0)
    exception = np.where(sstb == 1, col('wage_exception_sstb'), col('wage_exception_non_sstb'))
    tentative = inc * col('rate')
    tcja = np.maximum(tentative - share * np.maximum(tentative - wages * col('wage_limit') * exception, 0.0), 0.0).sum(1)
    obbb1 = np.minimum(inc * col('rate') * (sstb == 0), wages * col('wage_limit')).sum(1)
    total = inc.sum(1)
    obbb2 = np.maximum(total * L['rate'] - 0.75 * np.maximum(txbl - L['po_thresh_sstb'], 0.0), 0.0)
    q = np.where(L['po_type'] == 0, tcja, np.maximum(obbb1, obbb2))
    q = np.maximum(q, L['min_value'] * (total > 1000.0))
    return np.minimum(q, p['cap_base'] * L['txbl_inc_limit'])


def qbi_columns(df):
    """sstb and wagebill for the four kinds, records x KINDS, from the file's columns (non_micro_imputations.qbi)."""
    return (np.column_stack([df[f'sstb_{k}'].to_numpy(float) for k in KINDS]),
            np.column_stack([df[f'wagebill_{k}'].to_numpy(float) for k in KINDS]))


def check_formula(p, df, out, tag):
    """The formula's deduction against the calculator's on every record; returns the largest gap in dollars."""
    q = qbi_formula(p, *qbi_columns(df))
    gap = float(np.abs(q - out['qbi_ded'].to_numpy(float)).max())
    assert gap < 0.01, f'QBI formula and calculator differ by up to ${gap:,.2f} ({tag})'
    return gap


def bisect(f, lo, hi, rising, iterations=50):
    """x in [lo, hi] with f(x) = 0 for f monotone (rising or falling); an end when the root lies outside."""
    if lo == hi:   # a range of one value (settings taxes.qbi_dials.a_range [1, 1] since 30 October): held, not solved
        return lo, 'held at this value (its range in the settings is this one value)'
    flo, fhi = f(lo), f(hi)
    if (flo >= 0) == rising:
        return lo, 'at the lower end of its range: the target lies beyond that end'
    if (fhi <= 0) == rising:
        return hi, 'at the upper end of its range: the target lies beyond that end'
    for _ in range(iterations):
        mid = 0.5 * (lo + hi)
        if (f(mid) < 0) == rising:
            lo = mid
        else:
            hi = mid
    return 0.5 * (lo + hi), 'solved'


def soi_qbi_cells(year):
    """SOI Table 1.4's QBI deduction amounts by AGI for the year (targets/soi.csv); empty for a year SOI has not
    published (2024 on)."""
    import targets as TG
    soi = TG.load()
    return soi[(soi.year == year) & (soi.table.astype(str) == '1.4') & (soi.variable == 'qbi_deduction')
               & (soi.statistic == 'amount') & (soi.label != 'all')]


def carried_dials(year):
    """(a, b, where they come from) for a year with no SOI QBI line: the last round of the dials of the file the start
    comes from (<out>/tilt/<run>/taxes/qbi_dials.csv, the path tilt.start_from_year reads the file from), else the
    settings' values."""
    import non_micro_imputations as NMI
    import target_list as TLS
    spec = TLS.start_spec(year)
    if spec['mode'] == 'from_year':
        path = f"{common.OUT}/tilt/{spec['run']}/taxes/qbi_dials.csv"
        if os.path.exists(path):
            r = pd.read_csv(path).iloc[-1]
            return float(r['a']), float(r['b']), f"carried from the {spec['year']} file ({path})"
    return (float(NMI.ITEMS['qbi']['a']), float(NMI.ITEMS['qbi']['b']),
            'the settings\' values (non_micro_imputations.items.qbi): no dials of an earlier year to carry')


def solve_qbi_dials(df, inp, out, w, year, nmi_ctx):
    """The two QBI dials (module docstring, step 2). Returns (a, b, the table of rounds, the check by SCF group, notes)."""
    import non_micro_imputations as NMI
    Q = S['qbi_dials']
    p = qbi_parts(inp, out)
    filer = df['filer'].to_numpy() == 1
    agi = D.agi(df)
    s = soi_qbi_cells(year)
    floor = float(Q['soi_agi_floor'])
    assert len(s), f'SOI Table 1.4 {year}: no QBI deduction line in targets/soi.csv'
    # a cell SOI combined with the one below the floor would carry dollars across it
    assert not ((s.lo < floor) & (s.hi > floor)).any() and not s.loc[s.lo >= floor, 'value'].isna().any(), \
        f'SOI Table 1.4 {year}: the QBI deduction cells do not split at ${floor:,.0f}'
    soi_top = float(s.loc[s.lo >= floor, 'value'].sum())
    top = filer & (agi >= floor)
    L = p['law']
    end = L['po_thresh_non_sstb'] + L['po_range_non_sstb']   # the top of the phase-in range
    inc = p['inc']
    pos = inc[:, :3] > 0
    one = filer & (pos.sum(1) == 1) & (inc[:, 3] <= 0) & (p['txbl'] > end)
    kind = np.argmax(pos, axis=1)                       # the one kind, where one
    inc_one = inc[np.arange(len(df)), kind]
    groups = np.column_stack([df[NMI.QBI_KINDS[k][0]].to_numpy().astype(int) for k in KINDS[:3]])
    group = groups[np.arange(len(df)), kind]
    treasury = {int(g): float(np.mean(v)) for g, v in Q['treasury_share_by_scf_group'].items()}
    ctx = {'notes': []}

    def impute(a, b):
        cols = NMI.qbi(df, year, w, ctx, a=a, b=b)
        sstb = np.column_stack([cols[f'sstb_{k}'] for k in KINDS]).astype(float)
        bill = np.column_stack([cols[f'wagebill_{k}'] for k in KINDS]).astype(float)
        return sstb, bill

    def share_a(a, b):
        sstb, bill = impute(a, b)
        q = qbi_formula(p, sstb, bill)
        pop = one & (sstb[np.arange(len(df)), kind] == 0)
        num = (w * q)[pop].sum()
        den = (w * 0.2 * inc_one)[pop].sum()
        inc_g = {g: (w * inc_one)[pop & (group == g)].sum() for g in treasury}
        target = sum(inc_g[g] * treasury[g] for g in treasury) / sum(inc_g.values())
        by_group = [{'SCF industry group': g, 'owners (millions)': w[pop & (group == g)].sum() / 1e6,
                     'business income ($ billion)': (w * inc_one)[pop & (group == g)].sum() / 1e9,
                     'deduction as a share of 20% of income, file': (w * q)[pop & (group == g)].sum()
                     / max((w * 0.2 * inc_one)[pop & (group == g)].sum(), 1.0),
                     'Treasury (Table A1, column 1; industries in the group averaged)': treasury.get(g, np.nan)}
                    for g in range(1, 9)]
        return num / den, target, by_group

    def top_b(a, b):
        sstb, bill = impute(a, b)
        return (w * qbi_formula(p, sstb, bill))[top].sum()

    a, b = float(NMI.ITEMS['qbi']['a']), float(NMI.ITEMS['qbi']['b'])
    rounds = []
    for r in range(int(Q['rounds'])):
        target = share_a(a, b)[1]
        a_new, a_how = bisect(lambda v: share_a(v, b)[0] - target, *Q['a_range'], rising=True)
        b_new, b_how = bisect(lambda v: top_b(a_new, v) - soi_top, *Q['b_range'], rising=False)
        sh, target, _ = share_a(a_new, b_new)
        rounds.append({'round': r + 1, 'a': a_new, 'a solved': a_how, 'b': b_new, 'b solved': b_how,
                       'share of 20% kept, file': sh, 'Treasury target': target,
                       'deduction at AGI $500,000 and over, file ($ billion)': top_b(a_new, b_new) / 1e9,
                       'SOI ($ billion)': soi_top / 1e9})
        common.say(f'QBI dials, round {r + 1}: a = {a_new:.4f} ({a_how}), b = {b_new:.4f} ({b_how}); share {sh:.4f} '
                   f'against {target:.4f}')
        done = abs(a_new - a) < 1e-4 and abs(b_new - b) < 1e-4
        a, b = a_new, b_new
        if done:
            break
    _, _, by_group = share_a(a, b)
    return a, b, pd.DataFrame(rounds), pd.DataFrame(by_group)


# ---- the stage inside the tilt ---------------------------------------------------------------------------------
def compute(df, year, w, nmi_ctx, folder):
    """Every record's computed taxes joined to the file (module docstring). df: the start, after the non-microdata
    imputations; w: the start's weights (for the dials). Returns the frame with OUTPUTS and a context for the summary."""
    import calculator as C
    import non_micro_imputations as NMI
    ctx = {'notes': [], 'runs': {}}
    # A start from an earlier year's tilted file (targets.yaml start: from_year) carries that year's computed columns,
    # computed under that year's law on that year's dollars. They are dropped and computed again here. Until the
    # review of 30 October an assertion stood here, which stopped every chained year (2024 on).
    stale = [c for c in OUTPUTS if c in df]
    if stale:
        df = df.drop(columns=stale)
        ctx['notes'].append(f'the start\'s computed columns (from its own year) dropped and computed again under '
                            f'{law_year(year)} law: {len(stale)} columns')
    inp, notes = C.convert(df)
    ctx['conversion'] = notes
    solve = bool(S.get('qbi_dials', {}).get('solve')) and nmi_ctx is not None and nmi_ctx.get('qbi') is not None
    out, man = run_batch(inp, year, folder, 'first', law_sections=[QBI_LAW] if solve else [],
                         wage_increment=0.0 if solve else float(S.get('wage_increment', 0)))
    ctx['runs']['first'] = man
    if solve:
        p = qbi_parts(inp, out)
        if not (p['law']['rate'] > 0).any():
            ctx['notes'].append(f'QBI dials not solved: no QBI deduction in {law_year(year)} law')
            solve = False
        else:
            gap = check_formula(p, df, out, 'first run')
            ctx['notes'].append(f'QBI formula against the calculator, first run: largest gap ${gap:.6f}')
    if solve and not len(soi_qbi_cells(year)):
        # no SOI Table 1.4 for the year (2024 on): nothing to solve against, so the dials of the year the start comes
        # from are carried, and with them the specified-service flags (the draw is keyed by record id), as a chained
        # year holds the distribution; settings' values when there are none to carry (review of 30 October: the solve
        # stopped on its assertion; decision in DECISIONS_FOR_JOHN.md)
        a, b, how = carried_dials(year)
        cols = NMI.qbi(df, year, w, nmi_ctx, a=a, b=b)
        df = df.copy()
        for c, v in cols.items():
            df[c] = v
        inp, notes = C.convert(df)
        out, man = run_batch(inp, year, folder, 'final', law_sections=[QBI_LAW],
                             wage_increment=float(S.get('wage_increment', 0)))
        ctx['runs']['final'] = man
        gap = check_formula(qbi_parts(inp, out), df, out, 'final run')
        pd.DataFrame([{'round': 0, 'a': a, 'a solved': how, 'b': b, 'b solved': how}]).to_csv(
            f'{folder}/qbi_dials.csv', index=False)
        ctx.update(a=a, b=b)
        ctx['notes'].append(f'QBI dials not solved (no SOI Table 1.4 QBI deduction for {year}): a = {a:.4f}, '
                            f'b = {b:.4f}, {how}; QBI formula against the calculator, final run: largest gap '
                            f'${gap:.6f}')
        solve = False
    if solve:
        a, b, rounds, by_group = solve_qbi_dials(df, inp, out, w, year, nmi_ctx)
        cols = NMI.qbi(df, year, w, nmi_ctx, a=a, b=b)   # nmi_ctx['qbi'] now describes the solved columns
        df = df.copy()
        for c, v in cols.items():
            df[c] = v
        inp, notes = C.convert(df)
        out, man = run_batch(inp, year, folder, 'final', law_sections=[QBI_LAW],
                             wage_increment=float(S.get('wage_increment', 0)))
        ctx['runs']['final'] = man
        gap = check_formula(qbi_parts(inp, out), df, out, 'final run')
        ctx.update(a=a, b=b, rounds=rounds, by_group=by_group)
        rounds.to_csv(f'{folder}/qbi_dials.csv', index=False)
        by_group.to_csv(f'{folder}/qbi_share_by_scf_group.csv', index=False)
        ctx['notes'].append(f'QBI dials solved: a = {a:.4f}, b = {b:.4f} (settings a = {NMI.ITEMS["qbi"]["a"]}, '
                            f'b = {NMI.ITEMS["qbi"]["b"]}); QBI formula against the calculator, final run: largest gap '
                            f'${gap:.6f}')
    elif 'mtr_wages_iit' not in out and float(S.get('wage_increment', 0)) > 0:
        # the first run left out the marginal rates because the dials were to be solved, and they were not (a year
        # whose law has no QBI deduction, 2015-2017): one more run for them, or those years' files would lack the
        # calc_mtr_* columns (review of rebuild-taxes-in-model, 30 October)
        out, man = run_batch(inp, year, folder, 'final', wage_increment=float(S['wage_increment']))
        ctx['runs']['final'] = man
    cols = joined_columns(out)
    assert (inp['id'].to_numpy() == df['record_id'].to_numpy()).all(), 'record order'
    cols.index = df.index
    df = pd.concat([df, cols], axis=1)
    bad = [c for c in cols if not np.isfinite(df[c].to_numpy(float)).all()]
    assert not bad, f'computed columns with missing or infinite values: {bad}'
    with open(f'{folder}/conversion_notes.txt', 'w') as fh:
        fh.write('\n'.join(notes) + '\n')
    ctx['notes'].append(f'taxes computed under {law_year(year)} law by tax-simulator-gpu {S["commit"][:12]} '
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
    if 'rounds' in ctx:
        lines += ['QBI dials, round by round:', '', ctx['rounds'].to_markdown(index=False, floatfmt=',.4f'), '',
                  'The share of 20% of business income kept by owners of one business that is not a specified service, '
                  'above the phase-in range, by SCF industry group (start weights):', '',
                  ctx['by_group'].to_markdown(index=False, floatfmt=',.3f'), '']
    return lines


# ---- the report on a finished file ------------------------------------------------------------------------------
def evaluate(df, rows, w0, w1):
    """The calculator targets' rows evaluated on the file at weights w0 and w1 (solver.fit_table's columns)."""
    import solver as SV
    import target_list as TLS
    if not len(rows):   # (review, 30 October) a year with no calculator entries (every year but 2019 for now)
        return pd.DataFrame(columns=['group', 'target', 'value', 'kind', 'before', 'after', 'se', 'miss_in_se'])
    F, T, averages = TLS.conditions(df, rows, w0, average_form=common.SETTINGS['tilt'].get('average_form', 'zero'))
    se, identity = rows['se'].to_numpy(float), rows['identity'].to_numpy(bool)
    return SV.fit_table(w0, w1, F, T, se, averages, rows, common.SETTINGS['tilt'], identity)


def report(path, folder):
    """Taxes on a finished file (its own columns, no dials solved) and the year's calculator targets evaluated on
    its weights before and after the tilt: <folder>/taxes_fit.csv."""
    import target_list as TLS
    os.makedirs(folder, exist_ok=True)
    df = pd.read_parquet(path)
    if 'calc_txbl_inc' not in df:
        import calculator as C
        inp, notes = C.convert(df)
        out, man = run_batch(inp, Y, folder, 'report')
        cols = joined_columns(out)
        cols.index = df.index
        df = pd.concat([df, cols], axis=1)
    rows = TLS.expand(Y, start=df, which='calculator')
    fit = evaluate(df, rows, df['weight_start'].to_numpy(float), df['weight'].to_numpy(float))
    fit.to_csv(f'{folder}/taxes_fit.csv', index=False)
    common.say(fit[['group', 'target', 'value', 'before', 'after', 'se', 'miss_in_se']].to_string())


if __name__ == '__main__':
    assert sys.argv[1] == 'report', 'usage: python taxes.py report <file> <folder>'
    report(sys.argv[2], sys.argv[3])
