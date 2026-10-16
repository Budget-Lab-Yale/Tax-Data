"""
03_stack.py  (minimal example, step 3: the oversampled stack, v4)

What it does
  Builds the baseline generated file for the later steps: a stack of four
  files of 100,000 PUF-labeled records each, every record with all six PUF
  variables plus net worth (drawn as in gen_puf_with_net_worth). Each file
  oversamples the top of one variable. In the bands covering the top 1% of
  positive weight (the 0.1% and 0.01% bands) the sampling probability q is
  ten times the model's probability p, and each record's weight is
  p(band) / q(band), which undoes the boost exactly.

    files 1-3  stratified on wages, capital_gains, other_agi, with the
               mechanism of 03_oversample.py: the variable's band is drawn
               first from the boosted nothing-known distribution; then the
               other incomes in random order, net worth, married, n_dep.
    file 4     stratified on net_worth, keeping the normal block order:
               incomes first (random order), then net worth drawn from a
               boosted version of the model's CONDITIONAL distribution for
               that record, p_i(band | its incomes), then married, n_dep.
               p and q are that record's own conditional values.

  Why file 4 differs. A first version drew net worth first, then the
  incomes. Net worth is an SCF variable that the model never saw on a PUF
  record, and drawing it first pulled the whole record toward the SCF: more
  married couples and much more investment income than the plain file. Block
  order puts a record's own survey's variables first for exactly this
  reason, so wealth is boosted where it is normally drawn, after the incomes.

  Each file's weights are then scaled to sum to 100,000, so the stack of
  400,000 records has weights summing to 400,000 and each file contributes a
  quarter. Each file on its own estimates the same distribution, so the
  stack does too, with the top of all four variables dense in records.

Settings (environment; added 2026-10-15 for the v4 baseline, 15_BASELINE_V4.md):
  STACK_VERSION        the version in the file names (default v4).
  MINIMAL_STACK_DIR    where the stack and its meta go (default: the
                       generation folder of 02_model.py, gen/ or gen/<T>/).
                       The model is read from 02_model.py's model folder
                       (MINIMAL_RUN), the plain file from its generation folder.
  MINIMAL_STACK_LABEL  'puf' (default): net worth drawn under the PUF label,
                       as in gen_puf_with_net_worth and run E12 of
                       09_RESULTS.md. 'scf': under the SCF label (owner label),
                       as in gen_puf_with_net_worth_ownerlabel. The plain file
                       for the checks is the matching one of the two.
  MINIMAL_STACK_FOREIGN  (added 2026-10-15 for v4b) what the net worth draw
                       does with the record's slots the owning survey never
                       has (other_agi): unset (default) leaves them visible,
                       as in v4; 'absent' leaves them out, 'mask' hides them
                       (03_generate.generate, borrowed['foreign']). With
                       MINIMAL_STACK_LABEL=scf and 'absent' the net worth draw
                       sees exactly what an SCF record looks like (v4b).
  MINIMAL_ORDER        (added 2026-10-16 for v4c) the draw order, read by
                       03_generate.py: 'block' (default, v4 and v4b) or
                       'own_first' (decision 9 since 2026-10-16, v4c on):
                       every PUF variable first, married and n_dep included,
                       in a random order, net worth last. Under own_first the
                       three income-stratified files draw their stratifier
                       first from the distribution of a PUF record with no
                       net worth slot, the other PUF variables in a random
                       order, then net worth; the net worth file draws the
                       PUF variables in a random order, then net worth from
                       the boosted conditional. The plain file's default
                       name gets the suffix _ownfirst.
  MINIMAL_STACK_PLAIN  the plain (not oversampled) file the checks compare
                       with. Default: the generation folder's file named
                       above. If the file does not exist it is generated
                       here under the same rule (200,000 records, seed 3, as
                       gen_puf_with_net_worth in 03_generate.py) and written.

What it writes (in MINIMAL_STACK_DIR, default the generation folder)
  gen_puf_stack_v4.parquet   columns as gen_puf_with_net_worth.parquet, with
                             weight = the scaled importance weight, plus
                             p_band and q_band (the stratifier band's two
                             probabilities) and stratifier (which file)
  gen_meta_v4.json           what the file is
  03_stack_tables_<v>.md     the checks quoted in 03_REPORT.md (v4: 15_BASELINE_V4.md)
  gen_plain_seed35_<v>.parquet  a second plain file (seed 35), the noise yardstick
"""
import importlib.util
import json
import os
import sys

import numpy as np
import pandas as pd
import torch


def load_module(filename):
    """The step files start with a digit, so a plain `import` cannot load them."""
    path = os.path.join(os.path.dirname(os.path.abspath(__file__)), filename)
    spec = importlib.util.spec_from_file_location(filename[:-3], path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


os_ = load_module('03_oversample.py')
g, cmp, m, ev = os_.g, os_.cmp, os_.m, os_.ev
VERSION = os.environ.get('STACK_VERSION', 'v4')
LABEL = os.environ.get('MINIMAL_STACK_LABEL', 'puf')
assert LABEL in ('puf', 'scf'), LABEL
FOREIGN = os.environ.get('MINIMAL_STACK_FOREIGN') or None
assert FOREIGN in (None, 'mask', 'absent'), FOREIGN
if LABEL == 'scf':
    BORROWED = dict(g.OWNER_LABEL, foreign=FOREIGN)      # FOREIGN unset: g.OWNER_LABEL itself
elif FOREIGN is not None:
    BORROWED = {'label': 'own', 'foreign': FOREIGN}
else:
    BORROWED = None
PLAIN = ('gen_puf_with_net_worth' + ('_ownerlabel' if LABEL == 'scf' else '')
         + (f'_{FOREIGN}' if FOREIGN else '') + ('_ownfirst' if g.ORDER == 'own_first' else ''))
PLAIN_PATH = os.environ.get('MINIMAL_STACK_PLAIN', os.path.join(g.GEN_DIR, PLAIN + '.parquet'))
STACK_DIR = os.environ.get('MINIMAL_STACK_DIR', g.GEN_DIR)
FOREIGN_TEXT = {None: 'other_agi visible', 'mask': 'other_agi hidden behind the mask token',
                'absent': 'other_agi left out'}[FOREIGN]
CHANGES = (f'{VERSION}: the model of run {m.RUN or "(root)"} (table {m.TABLE or "v2"}: shared '
           'variables on the PUF dollar scale, trained on all records but a 2% held-back slice), '
           f'net worth drawn under the {LABEL.upper()} label with {FOREIGN_TEXT} in all four files; '
           f'draw order {g.ORDER}; otherwise built as v3')
ORDER_TEXT = {
    'block': 'wages/capital_gains/other_agi parts: stratifier band first from the boosted '
             'nothing-known distribution, then the other incomes in random order, net worth, '
             'married, n_dep. net_worth part: normal block order, net worth drawn from the '
             'boosted conditional given the incomes; p_band and q_band are that record\'s '
             'conditional values',
    'own_first': 'wages/capital_gains/other_agi parts: stratifier band first from the boosted '
                 'nothing-known distribution of a PUF record with no net worth slot, then the '
                 'other PUF variables (married and n_dep included) in random order, then net '
                 'worth. net_worth part: the PUF variables in random order, then net worth from '
                 'the boosted conditional given all of them; p_band and q_band are that '
                 'record\'s conditional values'}
PER_FILE = 100_000
STRATIFIERS = {'wages': 31, 'capital_gains': 32, 'other_agi': 33, 'net_worth': 34}  # seeds


def net_worth_file(model, seed):
    """The record's other variables first (block order: the incomes;
    own_first: every PUF variable), then net worth from the boosted
    conditional."""
    top = m.positive_bands('net_worth', 0.99, 1.0)
    part = g.generate(model, PER_FILE, 'puf', with_net_worth=True, fixed_order=False,
                      seed=seed, boost=('net_worth', top, os_.BOOST), borrowed=BORROWED)
    assert part['p_band'].notna().all() and part['q_band'].notna().all()
    part['weight'] = part['p_band'] / part['q_band']
    return part


def build_stack(model, reuse=None):
    """reuse: an earlier stack whose wages, capital_gains and other_agi parts
    are kept as they are (only the net worth part is rebuilt)."""
    parts = []
    for var, seed in STRATIFIERS.items():
        if reuse is not None and var != 'net_worth':
            part = reuse[reuse['stratifier'] == var].copy()
            assert len(part) == PER_FILE
        elif var == 'net_worth':
            part = net_worth_file(model, seed)
        else:
            part = os_.oversampled_file(model, var, seed, n=PER_FILE, with_net_worth=True,
                                        borrowed=BORROWED)
        part['weight'] *= PER_FILE / part['weight'].sum()
        part['stratifier'] = var
        parts.append(part)
        print(var, 'done', flush=True)
    return pd.concat(parts, ignore_index=True)


def top_counts(stack, plain):
    rows = []
    for var in STRATIFIERS:
        for low, label in [(0.99, 'top 1%'), (0.999, 'top 0.1%')]:
            bands = m.positive_bands(var, low, 1.0)
            row = {'variable': var, 'bands': label}
            for name, f in [('plain', plain), ('stack', stack)]:
                inside = np.isin(cmp.column(f, var).to_numpy(), bands)
                row[f'{name} records'] = int(inside.sum())
                row[f'{name} weighted share'] = float((f['weight'] * inside).sum() / f['weight'].sum())
            row['stack records from its own file'] = int(
                (np.isin(cmp.column(stack, var).to_numpy(), bands) & (stack['stratifier'] == var)).sum())
            rows.append(row)
    return pd.DataFrame(rows)


def distribution_check(stack, plain, plain2):
    rows = []
    for var in m.PREDICTED:
        row = {'variable': var}
        row.update({f'plain: {k}': x for k, x in cmp.summary_row(plain, var).items()})
        row.update({f'stack: {k}': x for k, x in cmp.summary_row(stack, var).items()})
        row['distance plain vs stack'] = cmp.distance(cmp.band_shares(plain, var),
                                                      cmp.band_shares(stack, var))
        row['distance plain vs plain, new seed'] = cmp.distance(cmp.band_shares(plain, var),
                                                                cmp.band_shares(plain2, var))
        rows.append(row)
    return pd.DataFrame(rows).fillna('')


def main():
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(m.SEED)
    model = m.load_model(device)
    os.makedirs(STACK_DIR, exist_ok=True)
    path = os.path.join(STACK_DIR, f'gen_puf_stack_{VERSION}.parquet')
    # "net_worth_only": rebuild the net worth part and keep the other three.
    reuse = pd.read_parquet(path) if 'net_worth_only' in sys.argv else None
    stack = build_stack(model, reuse)
    stack.to_parquet(path, index=False)
    meta = {'file': f'gen_puf_stack_{VERSION}.parquet', 'changes_since_v3': CHANGES,
            'model': os.path.join(m.MODEL_DIR, 'model.pt'), 'net_worth_label': LABEL,
            'net_worth_draw': BORROWED, 'plain_file': PLAIN_PATH,
            'records': len(stack), 'weight_sum': float(stack['weight'].sum()),
            'boost': os_.BOOST, 'boosted_bands': 'the positive bands covering the top 1% of '
            'positive weight (0.1% and 0.01% bands)',
            'parts': {v: {'records': PER_FILE, 'seed': s, 'weight_sum': PER_FILE}
                      for v, s in STRATIFIERS.items()},
            'columns': 'as gen_puf_with_net_worth.parquet; weight = p_band / q_band scaled '
                       'so each stratifier part sums to 100,000; stratifier names the part',
            'draw_order': g.ORDER, 'order': ORDER_TEXT[g.ORDER]}
    with open(os.path.join(STACK_DIR, f'gen_meta_{VERSION}.json'), 'w') as f:
        json.dump(meta, f, indent=2)

    # Checks against the plain file, with a second plain draw as the noise yardstick.
    if not os.path.exists(PLAIN_PATH):
        # the plain file under this stack's rule, as gen_puf_with_net_worth (seed 3)
        os.makedirs(os.path.dirname(PLAIN_PATH), exist_ok=True)
        g.generate(model, 200_000, 'puf', with_net_worth=True, fixed_order=False, seed=3,
                   borrowed=BORROWED).to_parquet(PLAIN_PATH, index=False)
        print('plain file written:', PLAIN_PATH, flush=True)
    plain = pd.read_parquet(PLAIN_PATH)
    plain2 = g.generate(model, len(plain), 'puf', with_net_worth=True, fixed_order=False, seed=35,
                        borrowed=BORROWED)
    # kept for the dollar-level checks of the stack (noise yardstick)
    plain2.to_parquet(os.path.join(STACK_DIR, f'gen_plain_seed35_{VERSION}.parquet'), index=False)
    md = ['### Weighted band summary: plain gen_puf_with_net_worth (200,000 records) vs '
          'the stack (400,000)\n\n' + ev.md_table(distribution_check(stack, plain, plain2), 4),
          '### Records in the top bands, plain vs stack\n\n' + ev.md_table(top_counts(stack, plain), 5)]
    with open(os.path.join(STACK_DIR, f'03_stack_tables_{VERSION}.md'), 'w') as f:
        f.write('\n\n'.join(md) + '\n')
    print('\n\n'.join(md))


if __name__ == '__main__':
    main()
