"""Diagnostic: decode true held-out wealth tokens, bypassing the model.

This is an oracle representation check, not a predictive benchmark.
It measures wealth tokenization/decoding loss with income held exact;
it does not measure information lost from the model's income inputs.
"""
import argparse
import json
from pathlib import Path

import numpy as np
import pandas as pd

from a_tokenizer import decode
from c_common import (Dataset, WEALTH, add_nw, truth_frame, frame_stats,
                      compare_stats, classifier_test, jsonable)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--dataset', required=True)
    ap.add_argument('--out-root', required=True)
    a = ap.parse_args()
    ds = Dataset(a.dataset)
    T = truth_frame(ds, (ds.records['source'] != 'PUF').to_numpy())
    rows = T['row'].to_numpy()
    q = pd.read_parquet(Path(ds.dir) / 'quantiles.parquet')
    age = sorted(T['t_age_range'].unique())
    frames = []
    for draw in range(5):
        decoded = decode(ds.truth[rows], ds.meta, ds.records.iloc[rows].reset_index(drop=True),
                         q, np.random.default_rng(20261010 + draw))
        F = T.copy()
        F['draw'] = draw
        F['ndraw'] = 5
        F['w'] = F['w'] / 5
        for v in WEALTH:
            F['d_' + v] = decoded[v].to_numpy()
        frames.append(add_nw(F))
    C = pd.concat(frames, ignore_index=True)
    h, detail = compare_stats(frame_stats(T, ds.tinfo, age), frame_stats(C, ds.tinfo, age))
    clf, _ = classifier_test(T, frames[0].assign(w=lambda f: f['w'] * 5), 20261309)
    h['classifier_auc'] = clf['auc']
    out = Path(a.out_root) / a.dataset / 'oracle_true_tokens'
    out.mkdir(parents=True, exist_ok=True)
    result = dict(dataset=a.dataset, label='oracle_true_tokens', kind='oracle',
                  n_records=len(T), n_records_missing=0, n_draws=5, headline=h,
                  interpretation='True wealth tokens decoded without prediction; incomes held exact.')
    (out / 'summary.json').write_text(json.dumps(jsonable(result), indent=2))
    for name, frame in detail.items():
        frame.to_csv(out / f'detail_{name}.csv', index=False)
    print(json.dumps(jsonable(result), indent=2))


if __name__ == '__main__':
    main()
