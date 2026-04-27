# Tilt Wealth Imputation — v4 Report

v4 changes vs v3: `lambda_max=5` per-component cap on the tilt + Step
B `fallback_uniform` branch disabled (it inflated counts when tilt
collapsed in infeasible cells; now Step B falls back to `skip` if PUF
total is near zero, but in v4 no cells hit that — every (cell × cat)
has positive PUF mass after the constrained tilt). Plus the upstream
age fix that tightened cell-pop ratios.

See `slurm_tilt_v4.out` for full per-bucket diagnostics.

## Method

For each PUF tax unit, we ask the per-(income-cell) DRF for forest-
averaged donor probabilities `p_ij` over the cell's SCF bootstrap, then
tilt them by `q_ij ∝ p_ij · exp(Z_jᵀ λ)` where `Z_j` are donor-side
wealth target features (8 categories × {amount, count}) and `λ` is
optimized per (income × age) bucket against SCF cell aggregates via
BFGS. Solver runs on a normalized basis (`Z_n = Z / max|Z|` per column)
so condition number stays in check across count vs amount targets.
One donor per record is then sampled from `q_ij` with top-K=300
sparsification.

A residual rescale (Step B) closes any per-(cell × cat) dollar gap
that the tilt couldn't reach. Three modes:

- `multiplicative` — `factor = SCF_total / PUF_total` when both alive
- `fallback_uniform` — when tilt collapsed donors to zero in a
  category (PUF total ≈ 0 but SCF target > 0), distribute SCF
  aggregate uniformly across PUF records in the cell
- `skip` — when SCF target ≈ 0 (don't fabricate amounts)

DRF `min.node.size = 50`. Per-cell forests cached at
`resources/cache/qrf/wealth_percell_<ci>_mns50.rds`.

---

## Table 1 — Net worth ($T) by income percentile

Income percentile is computed within-frame (each source ranks its own
records). NW = sum(13 assets) − sum(6 debts). Excludes `kg_*`
unrealized capital gains (consistent with `cat_nw` definition in
`stage3_target_qc.R`).

|bin      |      SCF|  raw_DRF| post_tilt| post_rescale| rescale_factor|
|:--------|--------:|--------:|---------:|------------:|--------------:|
|p0-20    |   $2.35T|   $3.08T|    $2.07T|       $2.20T|          1.059|
|p20-40   |   $3.63T|   $4.42T|    $3.47T|       $3.83T|          1.105|
|p40-60   |   $8.26T|   $8.56T|    $8.17T|       $8.40T|          1.028|
|p60-80   |  $14.58T|  $17.88T|   $14.27T|      $14.60T|          1.023|
|p80-90   |  $15.78T|  $19.34T|   $15.11T|      $16.11T|          1.066|
|p90-99   |  $41.89T|  $51.17T|   $44.58T|      $44.67T|          1.002|
|p99-99.9 |  $12.83T|  $11.06T|   $10.02T|       $9.93T|          0.991|
|top0.1%  |  $39.80T|  $59.49T|   $44.43T|      $39.37T|          0.886|
|TOTAL    | $139.12T| $175.00T|  $142.12T|     $139.12T|          0.979|

## Table 2 — Net worth ($T) by age group

Age group from `age_older` = max(age1, age2), capped at 80. SCF and PUF
use the same definition.

|bin   |      SCF|  raw_DRF| post_tilt| post_rescale| rescale_factor|
|:-----|--------:|--------:|---------:|------------:|--------------:|
|18-24 |   $0.75T|   $0.83T|    $0.59T|       $0.57T|          0.969|
|25-34 |   $4.26T|   $4.74T|    $3.71T|       $3.70T|          0.996|
|35-44 |  $11.67T|  $16.57T|   $13.24T|      $12.82T|          0.968|
|45-54 |  $19.84T|  $29.94T|   $23.19T|      $22.38T|          0.965|
|55-64 |  $36.21T|  $45.82T|   $34.20T|      $33.26T|          0.972|
|65-74 |  $39.84T|  $43.65T|   $39.42T|      $38.95T|          0.988|
|75+   |  $26.55T|  $33.46T|   $27.77T|      $27.44T|          0.988|
|TOTAL | $139.12T| $175.00T|  $142.12T|     $139.12T|          0.979|

## Table 3 — Net worth shares + thresholds by wealth percentile

Wealth percentile is computed within-frame on net worth. Share is
fraction of total NW. Threshold is the lower-edge wealth value at the
bin boundary (e.g., `p50-90` lower threshold = the median NW; `top0.1%`
lower threshold = 99.9th percentile NW).

|bin      |   SCF| raw_DRF| post_tilt| post_rescale| SCF thr| raw_DRF thr| post_tilt thr| post_rescale thr|
|:--------|-----:|-------:|---------:|------------:|-------:|-----------:|-------------:|----------------:|
|bot50    | 0.008|   0.004|     0.001|        0.001| $-0.56M|     $-0.54M|       $-0.42M|          $-0.48M|
|p50-90   | 0.219|   0.191|     0.212|        0.222|  $0.10M|      $0.08M|        $0.06M|           $0.07M|
|p90-99   | 0.389|   0.367|     0.393|        0.406|  $1.39M|      $1.51M|        $1.28M|           $1.31M|
|p99-99.9 | 0.222|   0.235|     0.241|        0.231| $11.49M|     $12.45M|       $10.73M|          $10.79M|
|top0.1%  | 0.162|   0.203|     0.153|        0.139| $54.45M|     $69.36M|       $55.42M|          $49.55M|

## Table 4 — Counts and amounts by wealth category

Eight categories: total NW, plus the seven dollar-target categories
(equities, bonds, homes [primary residence], retirement [IRA + 401k +
pensions], business [pass-throughs], other [residual assets], debt).
Count = weighted M-households with `cat > 0`; amount = weighted $T sum.
NW count is `cat_nw > 0` (positive net worth).

|category   | SCF cnt| raw_DRF cnt| post_tilt cnt| post_rescale cnt| rescale_factor cnt|  SCF amt| raw_DRF amt| post_tilt amt| post_rescale amt| rescale_factor amt|
|:----------|-------:|-----------:|-------------:|----------------:|------------------:|--------:|-----------:|-------------:|----------------:|------------------:|
|nw         |  134.4M|      146.0M|        132.8M|           132.1M|              0.995| $139.12T|    $175.00T|      $142.12T|         $139.12T|              0.979|
|equities   |   38.8M|       41.3M|         36.8M|            36.8M|              1.000|  $21.06T|     $27.45T|       $22.24T|          $21.06T|              0.947|
|bonds      |   13.1M|       13.4M|         12.9M|            12.9M|              1.000|   $4.84T|      $5.57T|        $5.21T|           $4.84T|              0.929|
|homes      |   95.5M|       97.9M|         92.3M|            92.3M|              1.000|  $40.85T|     $45.27T|       $39.55T|          $40.85T|              1.033|
|retirement |   78.7M|       85.0M|         75.0M|            75.0M|              1.000|  $23.84T|     $26.69T|       $23.31T|          $23.84T|              1.023|
|business   |   18.5M|       17.5M|         18.3M|            18.3M|              1.000|  $30.77T|     $45.65T|       $32.34T|          $30.77T|              0.952|
|other      |  143.1M|      155.6M|        139.7M|           139.7M|              1.000|  $34.41T|     $42.77T|       $35.21T|          $34.41T|              0.977|
|debt       |  113.9M|      123.0M|        104.8M|           104.8M|              1.000|  $16.66T|     $18.40T|       $15.75T|          $16.66T|              1.058|

---

## Microdata for re-cuts

All record-level microdata is saved — you can build new tables without
re-running the harness:

  - 3-stage record-level Y matrices: /nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026042712/baseline/wealth_harness_tilt_diag.rds
     (y_pre_tilt = raw DRF leaf draw; y_post_tilt_pre_rescale =
      after tilt, before Step B; y_post_rescale = final.
     Each is a tibble with id + 23 wealth Y vars.)
  - PUF X-side covariates: /nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026042712/baseline/tax_units_2022.csv
  - SCF target side: resources/cache/scf_tax_units.rds

Re-run this report after editing `src/eda/report_v3.R`:

```
sbatch slurm_render_status.sh   # re-renders HTML from AM_STATUS.md
```

(or call `Rscript src/eda/report_v3.R <output_dir>` via sbatch to
rebuild AM_STATUS.md from microdata first, then re-render HTML.)
