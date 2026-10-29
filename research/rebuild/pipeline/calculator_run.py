"""The GPU tax calculator on every record of the converted year frame.

Run by calculator.py under the calculator's own Python environment, with the pinned copy of tax-simulator-gpu first
on the path and as the working folder (its law and settings files are read from there). Steps, all the calculator's
own code:
  1. the columns the pinned calculator reads are checked against the frame (none may be missing);
  2. the wage split and the random draws keyed by record id (frame/prepare.py, as its bridge does);
  3. the year's current law, compiled from its default law folder with its default price indexes;
  4. its frame steps under that law (the law join, the state tax workaround, the capital gains price ratio, net
     worth), then the whole tax chain (statutory_order.run_taxes; the estate tax left out).
Writes <out>/calculator_<year>.parquet (record id and every column the chain computes, plus the two state tax
workaround amounts the frame steps make) and <out>/calculator_manifest.json.
"""
import argparse
import json
import os
import time

import numpy as np
import pandas as pd


def main():
    p = argparse.ArgumentParser()
    for a in ('--input', '--out', '--lib', '--commit'):
        p.add_argument(a, required=True)
    p.add_argument('--year', type=int, required=True)
    p.add_argument('--seed', type=int, required=True)
    a = p.parse_args()
    t0 = time.time()
    import jax
    import jax.numpy as jnp
    import taxmodel
    from taxmodel.config import check_toolchain
    from taxmodel.frame import build_frame, prepare
    from taxmodel.scenario import law_by_year, provenance
    from taxmodel.taxes import statutory_order
    from taxmodel.years import one_year

    where = os.path.realpath(taxmodel.__file__)
    assert where.startswith(os.path.realpath(a.lib) + os.sep), f'taxmodel imported from {where}, not the pinned copy'
    check_toolchain()
    print(f'taxmodel from {where}; jax {jax.__version__} on {jax.devices()}', flush=True)

    frame = pd.read_parquet(a.input)
    n = len(frame)
    needed = set(statutory_order.RECORD_COLUMNS) | set(statutory_order.ESTATE_RECORD_COLUMNS) \
        | set(build_frame.FRAME_STEP_RECORD_COLUMNS)
    made_by_the_calculator = {'kg_lt_cpi_ratio', 'salt_workaround_part', 'salt_workaround_scorp', 'net_worth',
                              'wages_base1', 'wages_base2', *prepare.DRAW_COLUMNS}
    supplied = set(frame.columns)
    missing = sorted(needed - made_by_the_calculator - supplied)
    assert not missing, f'the frame lacks columns the pinned calculator reads: {missing}'
    unread = sorted(supplied - needed - set(prepare.OVERLAPPING_COLUMNS))
    print(f'{n:,} records; columns supplied {len(supplied)}, read by the tax chain or frame steps '
          f'{len(needed - made_by_the_calculator)}; supplied and not read by them: {unread}', flush=True)

    frame = prepare.split_overlapping_wages(frame, a.year)
    draws = prepare.keyed_draws(frame['id'].to_numpy(), seed=a.seed)
    frame = frame.merge(draws, on='id', how='left', validate='one_to_one')
    ids = frame['id'].to_numpy(np.int64)
    raw = {c: jnp.asarray(frame[c].to_numpy(np.float64)) for c in frame.columns if c != 'id'}
    raw['id'] = jnp.asarray(ids)
    raw['valid'] = jnp.ones(n, dtype=bool)

    scenario = provenance.load_scenario([a.year])
    series = law_by_year.bind_annual_laws(scenario.values, scenario.years, scenario.law_table, [a.year])
    annual = series.by_year[0]
    double_run = statutory_order.charity_double_run_applies(annual.calculator)
    law = one_year.law_read_for_each_household(annual, raw['filing_status'])
    builder = build_frame.make_frame_builder(scenario.indexes, scenario.cluster_death_weight_cap)
    t1 = time.time()
    built = builder(raw, law, jnp.asarray(float(a.year)), apply_adjusters=False)
    taxes = statutory_order.run_taxes(built, law.calculator, None, double_run=double_run)
    out = {'id': ids}
    for k, v in taxes.items():
        out[k] = np.asarray(v, dtype=np.float64)
    for k in ('salt_workaround_part', 'salt_workaround_scorp', 'kg_lt_cpi_ratio', 'net_worth'):
        out[k] = np.asarray(built[k], dtype=np.float64)
    out = pd.DataFrame(out)
    t2 = time.time()
    nan = {k: int(np.isnan(out[k].to_numpy()).sum()) for k in out.columns if k != 'id'}
    nan = {k: v for k, v in nan.items() if v}
    out.to_parquet(f'{a.out}/calculator_{a.year}.parquet', index=False)
    law_values = {k: [float(v) for v in np.asarray(annual.frame_columns[k]).ravel()]
                  for k in ('std.value', 'item.salt_limit', 'ctc.need_ssn', 'ctc.value_young1', 'amt.exempt')
                  if k in annual.frame_columns}
    manifest = {'commit': a.commit, 'taxmodel_from': where, 'jax': jax.__version__,
                'devices': [str(d) for d in jax.devices()], 'law_year': a.year, 'draw_seed': a.seed,
                'records': n, 'charity_double_run': bool(double_run),
                'index_table': provenance.index_series()[1],
                'law_values_by_filing_status': law_values,
                'columns_supplied_not_read': unread, 'missing_values_in_outputs': nan,
                'seconds': {'setup': round(t1 - t0, 1), 'frame steps and taxes': round(t2 - t1, 1)}}
    with open(f'{a.out}/calculator_manifest.json', 'w') as fh:
        json.dump(manifest, fh, indent=1)
    print(json.dumps(manifest, indent=1), flush=True)


if __name__ == '__main__':
    main()
