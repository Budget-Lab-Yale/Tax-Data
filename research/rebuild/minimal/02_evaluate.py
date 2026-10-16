"""
02_evaluate.py  (minimal example, step 2, part 2 of 2: scoring the trained model)

What it does
  Scores the model from 02_model.py on the test split, once, three ways:
  1. Cross-entropy in nats per predicted variable and source, at three hide
     shares: only that variable hidden; about half the slots hidden (that
     variable plus random others); everything hidden. Next to each, the
     cross-entropy of the training data's weighted marginal distribution of
     that variable (a model that ignores every other slot).
  2. Conditional tables, model against held-out data. The model column is the
     model's probability for the variable given all the record's other slots,
     averaged with the survey weights over the records in a cell; the data
     column is the weighted share in the same records.
  3. Label swap: the same records and hidden slots, with the source token
     flipped. Slots the other source never has are removed from both sides of
     the comparison (net_worth from SCF records; other_agi, and the _small
     parts when the table has them, from PUF records), so the change comes
     from the label alone.

The table and the model folder are those of 02_model.py (MINIMAL_TABLE,
MINIMAL_RUN, MINIMAL_MODEL_DIR), and so are its switches: predictions come
from m.predict (MINIMAL_NO_LABEL_SHARED), unknown SCF zeros are masked and
not scored (MINIMAL_SCF_ZERO_UNKNOWN; the SCF cross-entropy of those three
items is then over nonzero records only), SCF rows are scored through the
reporting curve (MINIMAL_REPORT_CURVE; the marginal column is then not
comparable for those three SCF items). In the label swap a record is always
scored by its true source's rule.

What it writes (in the model folder)
  02_eval.json     every number below
  02_tables.md     the same as Markdown tables, quoted in 02_REPORT.md
"""
import importlib.util
import json
import math
import os

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


m = load_module('02_model.py')
NC = len(m.CONTEXT)
EVAL_SEED = 20261017

# ---- Band groups used in all tables -------------------------------------------
# Positive bands are grouped by their share of the positive weight (from the
# band layout in band_edges.json). Group 0 is zero, together with the
# negatives when a variable has any.
GROUP_LABELS = ['zero', 'P0-20', 'P20-40', 'P40-60', 'P60-80', 'P80-90',
                'P90-99', 'P99-100']
GROUP_LOWER_SHARE = [0.0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.99]


def band_group(bands, var):
    """Group index (0-7) of each band of var."""
    lay = m.BAND_LAYOUT[var]
    lower = np.array([0.0] + lay['positive_top'][:-1])   # lower share of each positive band
    group_of_positive = np.searchsorted(GROUP_LOWER_SHARE, lower + 1e-9, side='right')
    bands = np.asarray(bands)
    positive = bands >= lay['first_positive']
    out = np.zeros(bands.shape, dtype=int)
    out[positive] = group_of_positive[bands[positive] - lay['first_positive']]
    return out


def group_matrix(var):
    """[bands, 8] matrix that sums band probabilities into groups."""
    return np.eye(len(GROUP_LABELS))[band_group(np.arange(m.N_CHOICES[var]), var)]


# ---- Hide patterns ---------------------------------------------------------------
def present_of(values):
    return values[:, NC:] >= 0


def one_hidden(values, j):
    hidden = torch.zeros_like(present_of(values))
    hidden[:, j] = present_of(values)[:, j]
    return hidden


def half_hidden(values, j, gen, present=None):
    """Slot j plus random others, ceil(n/2) hidden in all."""
    present = present_of(values) if present is None else present
    n_hide = torch.ceil(present.sum(1) / 2)
    scores = torch.rand(present.shape, generator=gen, device=values.device)
    scores[~present] = 2.0
    scores[:, j] = -1.0                      # slot j is always among the hidden
    rank = scores.argsort(1).argsort(1)
    return (rank < n_hide[:, None]) & present


def weighted_mean(x, w):
    return float((x * w).sum() / w.sum())


# ---- 1. Cross-entropy -------------------------------------------------------------
@torch.no_grad()
def score_records(model, values, weight, seed, coarse=None, n_half_draws=3,
                  unknown=None, source=None):
    """Per variable: weighted mean cross-entropy at the three hide shares, and
    the one-hidden probability vectors (kept for the conditional tables).
    The same seed gives the same hide patterns for the same records.
    Coarsened SCF zeros (coarse >= 0) are scored on the set {zero or small},
    as in training. Unknown slots are masked always and not scored; source,
    if given, is the true source used by the scoring rule."""
    gen = torch.Generator(device=values.device).manual_seed(seed)
    present = present_of(values)
    unk = torch.zeros_like(present) if unknown is None else unknown & present
    known = present & ~unk

    def run(hidden):
        return m.predict(model, values, hidden | unk)

    def nll(logp):
        return m.slot_nll(logp, values, coarse, source)
    all_nll = nll(run(known))
    result, probs = {}, {}
    for j, var in enumerate(m.PREDICTED):
        has = known[:, j]
        if not has.any():
            continue
        w = weight[has]
        logp = run(one_hidden(values, j) & known)
        probs[var] = logp[:, j].exp().cpu().numpy()
        one = nll(logp)[has, j]
        half = [nll(run(half_hidden(values, j, gen, known)))[has, j]
                for _ in range(n_half_draws)]
        result[var] = {'one': weighted_mean(one, w),
                       'half': float(np.mean([weighted_mean(h, w) for h in half])),
                       'all': weighted_mean(all_nll[has, j], w)}
    return result, probs


def marginal_baseline(train, test, var, test_coarse=None, test_coarse_neg=None):
    """Cross-entropy of the test records under the training data's weighted
    marginal for var. Each possible choice gets an extra 0.5 records' weight so
    that a band empty in training does not give an infinite score. A
    coarsened test zero is scored on the marginal's total over its set."""
    choices = m.N_CHOICES[var]
    share = np.bincount(train[var], weights=train['w'], minlength=choices) + 0.5
    share = share / share.sum()
    p = share[test[var].to_numpy()]
    if test_coarse is not None:
        cum = np.cumsum(share)
        first = m.BAND_LAYOUT[var]['first_positive']
        hi = test_coarse.to_numpy()
        set_p = share[0] + cum[np.maximum(hi, first)] - cum[first - 1]
        if test_coarse_neg is not None:          # the smallest-loss bands, lo .. first - 1
            lo = test_coarse_neg.to_numpy()
            set_p = set_p + np.where(lo >= 1, cum[first - 1] - cum[np.maximum(lo, 1) - 1], 0.0)
        p = np.where(hi >= 0, set_p, p)
    return weighted_mean(-np.log(p), test['w'].to_numpy())


def column_of(table, var):
    return table[var + '_band'] if var in m.AMOUNTS else table[var]


# ---- 2. Conditional tables --------------------------------------------------------
def cell_table(keys, data_value, model_value, weight, extra=None):
    """Weighted means of data_value and model_value within cells of keys."""
    frame = keys.copy()
    frame['records'] = 1
    frame['wd'], frame['wm'], frame['w'] = data_value * weight, model_value * weight, weight
    if extra is not None:
        frame['we'] = extra * weight
    g = frame.groupby(list(keys.columns), observed=True).sum(numeric_only=True)
    out = pd.DataFrame({'records': g['records'], 'data': g['wd'] / g['w'],
                        'model': g['wm'] / g['w']})
    if extra is not None:
        out['model, other label'] = g['we'] / g['w']
    return out.reset_index()


def md_table(frame, digits=3):
    def fmt(x):
        return f'{x:.{digits}f}' if isinstance(x, float) else str(x)
    lines = ['| ' + ' | '.join(map(str, frame.columns)) + ' |',
             '|' + '---|' * len(frame.columns)]
    lines += ['| ' + ' | '.join(fmt(x) for x in row) + ' |'
              for row in frame.itertuples(index=False)]
    return '\n'.join(lines)


def nonzero_share(bands, p, var, source):
    """Data indicator and model probability of a nonzero amount of var.
    Step 1's layout: band 0 is zero. A layout with no zero band (2026-10-15,
    02_model.read_band_layout): the bands lying wholly outside the source's
    zero mass (below its first zero band or above its last), so a band that
    straddles zero and small amounts counts as zero in data and model alike."""
    lay = m.BAND_LAYOUT[var]
    if 'zero_bands' not in lay:
        return (bands != 0).astype(float), 1 - p[:, 0]
    lo, hi = lay['zero_bands'][source]
    n = m.N_CHOICES[var]
    return ((bands < lo) | (bands > hi)).astype(float), p[:, :lo].sum(1) + p[:, hi + 1:n].sum(1)


def any_amount_by(test, probs, var, by, swapped_probs=None):
    """Share with a nonzero amount of var, by group of `by` and married."""
    group = pd.Categorical.from_codes(band_group(test[by + '_band'], by), GROUP_LABELS)
    keys = pd.DataFrame({by + ' group': group})
    if by == 'wages':
        keys['married'] = test['married'].to_numpy()
    source = test['source'].iloc[0]
    assert (test['source'] == source).all()
    bands = test[var + '_band'].to_numpy()
    data, model = nonzero_share(bands, probs[var], var, source)
    extra = None if swapped_probs is None else nonzero_share(bands, swapped_probs[var], var, source)[1]
    return cell_table(keys, data, model, test['w'].to_numpy(), extra)


def n_dep_by_married(test, probs):
    rows = []
    for married in [0, 1]:
        sel = test['married'].to_numpy() == married
        w = test['w'].to_numpy()[sel]
        data = np.bincount(test['n_dep'].to_numpy()[sel], weights=w, minlength=4) / w.sum()
        model = (probs['n_dep'][sel] * w[:, None]).sum(0) / w.sum()
        rows.append({'married': married, 'records': int(sel.sum()),
                     **{f'{k} deps': f'{data[k]:.3f} / {model[k]:.3f}' for k in range(4)}})
    return pd.DataFrame(rows)


def net_worth_by_wages(test, probs):
    """SCF records with a net worth slot: net worth group shares by wage group."""
    sel = test['net_worth_band'].to_numpy() >= 0
    w = test['w'].to_numpy()[sel]
    wage_group = band_group(test['wages_band'].to_numpy()[sel], 'wages')
    nw_data = np.eye(len(GROUP_LABELS))[band_group(test['net_worth_band'].to_numpy()[sel], 'net_worth')]
    nw_model = probs['net_worth'][sel][:, :m.N_CHOICES['net_worth']] @ group_matrix('net_worth')
    labels = ['zero or neg.'] + GROUP_LABELS[1:]
    rows = []
    for g, label in enumerate(GROUP_LABELS):
        cell = wage_group == g
        if not cell.any():
            continue
        d = (nw_data[cell] * w[cell, None]).sum(0) / w[cell].sum()
        p = (nw_model[cell] * w[cell, None]).sum(0) / w[cell].sum()
        rows.append({'wage group': label, 'records': int(cell.sum()),
                     **{nw: f'{d[k]:.2f} / {p[k]:.2f}' for k, nw in enumerate(labels)}})
    return pd.DataFrame(rows)


# ---- 3. Label swap ------------------------------------------------------------------
def swap_inputs(values, source):
    """Two copies of the records without the slots the other source lacks: one
    under the true label, one under the other label."""
    same = values.clone()
    for var, owner in m.OWNED_BY.items():
        if owner == source:
            same[:, NC + m.PREDICTED.index(var)] = -1
    swapped = same.clone()
    swapped[:, 0] = 1 - swapped[:, 0]
    return same, swapped


def main():
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    model = m.load_model(device)
    table = m.read_table()
    if m.REPORT_CURVE:
        m.set_report_curve(table)
    table['w'] = m.training_weights(table)
    out, md = {'cross_entropy': {}, 'label_swap': {}}, []
    probs, swap_probs, tests = {}, {}, {}
    for source in m.SOURCES:
        sel = table.source == source
        train, test = table[sel & (table.split == 'train')], table[sel & (table.split == 'test')].reset_index(drop=True)
        tests[source] = test
        encoded = m.encode(test, device)
        values, coarse, unknown = encoded['values'], encoded['coarse'], encoded['unknown']
        true_source = values[:, 0].clone()
        weight = torch.tensor(test['w'].to_numpy(), dtype=torch.float32, device=device)
        ce, probs[source] = score_records(model, values, weight, EVAL_SEED, coarse,
                                          unknown=unknown, source=true_source)
        for var in ce:
            # unknown SCF zeros (MINIMAL_SCF_ZERO_UNKNOWN) are left out here too
            k_tr = k_te = slice(None)
            if unknown is not None and source == 'scf' and var in m.INVESTMENT:
                k_tr, k_te = column_of(train, var) != 0, column_of(test, var) != 0
            tr, te = train[k_tr], test[k_te]
            ce[var]['marginal'] = marginal_baseline(
                pd.DataFrame({var: column_of(tr, var), 'w': tr['w']}),
                pd.DataFrame({var: column_of(te, var), 'w': te['w']}), var,
                te[var + '_coarse_hi'] if var in m.COARSENED else None,
                te[var + '_coarse_neg'] if var in m.COARSE_NEG else None)
        out['cross_entropy'][source] = ce
        md.append(f'### Cross-entropy, {source.upper()} test records (nats)\n\n' + md_table(
            pd.DataFrame(ce).T.reset_index().rename(columns={'index': 'variable'})))
        same, swapped = swap_inputs(values, source)
        ce_same, _ = score_records(model, same, weight, EVAL_SEED, coarse,
                                   unknown=unknown, source=true_source)
        ce_swap, swap_probs[source] = score_records(model, swapped, weight, EVAL_SEED, coarse,
                                                    unknown=unknown, source=true_source)
        change = {v: {k: ce_swap[v][k] - ce_same[v][k] for k in ce_same[v]} for v in ce_same}
        out['label_swap'][source] = {'true_label': ce_same, 'other_label': ce_swap,
                                     'change': change}
        md.append(f'### Label swap, {source.upper()} records scored under the other label: '
                  'change in cross-entropy (nats, positive = worse)\n\n' + md_table(
                      pd.DataFrame(change).T.reset_index().rename(columns={'index': 'variable'}), 4))

    puf, scf = tests['puf'], tests['scf']
    tables = {
        '(a) PUF: share with any dividends, by wage group and married':
            any_amount_by(puf, probs['puf'], 'dividends', 'wages', swap_probs['puf']),
        '(a) SCF: share with any dividends, by wage group and married':
            any_amount_by(scf, probs['scf'], 'dividends', 'wages', swap_probs['scf']),
        '(b) PUF: share with any capital gain or loss, by dividend group':
            any_amount_by(puf, probs['puf'], 'capital_gains', 'dividends'),
        '(c) PUF: dependents by married (data / model)': n_dep_by_married(puf, probs['puf']),
        '(c) SCF: dependents by married (data / model)': n_dep_by_married(scf, probs['scf']),
        '(d) SCF: net worth group shares by wage group (data / model)':
            net_worth_by_wages(scf, probs['scf']),
    }
    for title, frame in tables.items():
        md.append(f'### {title}\n\n' + md_table(frame))
        out[title] = frame.to_dict(orient='records')
    with open(os.path.join(m.MODEL_DIR, '02_eval.json'), 'w') as f:
        json.dump(out, f, indent=2, default=float)
    with open(os.path.join(m.MODEL_DIR, '02_tables.md'), 'w') as f:
        f.write('\n\n'.join(md) + '\n')
    print('\n\n'.join(md))


if __name__ == '__main__':
    main()
