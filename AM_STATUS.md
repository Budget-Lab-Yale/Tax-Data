# Tilt Wealth Imputation — v3 Report

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
|p0-20    |   $2.35T|   $2.92T|    $0.19T|       $2.19T|         11.716|
|p20-40   |   $3.63T|   $4.15T|    $3.54T|       $3.80T|          1.074|
|p40-60   |   $8.26T|   $8.74T|    $8.09T|       $8.23T|          1.017|
|p60-80   |  $14.58T|  $19.23T|   $14.14T|      $14.78T|          1.045|
|p80-90   |  $15.78T|  $19.19T|   $15.06T|      $16.10T|          1.069|
|p90-99   |  $41.89T|  $52.18T|   $46.52T|      $44.80T|          0.963|
|p99-99.9 |  $12.83T|  $10.91T|   $10.41T|       $9.94T|          0.955|
|top0.1%  |  $39.80T|  $60.45T|   $42.79T|      $39.28T|          0.918|
|TOTAL    | $139.12T| $177.79T|  $140.73T|     $139.12T|          0.989|

## Table 2 — Net worth ($T) by age group

Age group from `age_older` = max(age1, age2), capped at 80. SCF and PUF
use the same definition.

|bin   |      SCF|  raw_DRF| post_tilt| post_rescale| rescale_factor|
|:-----|--------:|--------:|---------:|------------:|--------------:|
|18-24 |   $0.75T|   $0.72T|    $0.52T|       $0.94T|          1.796|
|25-34 |   $4.26T|   $4.37T|    $3.89T|       $4.04T|          1.040|
|35-44 |  $11.67T|  $15.96T|   $13.32T|      $12.93T|          0.971|
|45-54 |  $19.84T|  $29.31T|   $22.61T|      $21.83T|          0.966|
|55-64 |  $36.21T|  $44.58T|   $34.11T|      $32.98T|          0.967|
|65-74 |  $39.84T|  $46.70T|   $37.67T|      $37.82T|          1.004|
|75+   |  $26.55T|  $36.14T|   $28.61T|      $28.58T|          0.999|
|TOTAL | $139.12T| $177.79T|  $140.73T|     $139.12T|          0.989|

## Table 3 — Net worth shares + thresholds by wealth percentile

Wealth percentile is computed within-frame on net worth. Share is
fraction of total NW. Threshold is the lower-edge wealth value at the
bin boundary (e.g., `p50-90` lower threshold = the median NW; `top0.1%`
lower threshold = 99.9th percentile NW).

|bin      |   SCF| raw_DRF| post_tilt| post_rescale| SCF thr| raw_DRF thr| post_tilt thr| post_rescale thr|
|:--------|-----:|-------:|---------:|------------:|-------:|-----------:|-------------:|----------------:|
|bot50    | 0.008|   0.004|    -0.001|        0.011| $-0.56M|     $-0.54M|       $-0.43M|          $-0.47M|
|p50-90   | 0.219|   0.193|     0.206|        0.216|  $0.10M|      $0.08M|        $0.05M|           $0.07M|
|p90-99   | 0.389|   0.369|     0.402|        0.403|  $1.39M|      $1.48M|        $1.25M|           $1.28M|
|p99-99.9 | 0.222|   0.235|     0.251|        0.237| $11.49M|     $12.81M|       $11.27M|          $10.69M|
|top0.1%  | 0.162|   0.198|     0.142|        0.133| $54.45M|     $66.86M|       $54.00M|          $49.16M|

## Table 4 — Counts and amounts by wealth category

Eight categories: total NW, plus the seven dollar-target categories
(equities, bonds, homes [primary residence], retirement [IRA + 401k +
pensions], business [pass-throughs], other [residual assets], debt).
Count = weighted M-households with `cat > 0`; amount = weighted $T sum.
NW count is `cat_nw > 0` (positive net worth).

|category   | SCF cnt| raw_DRF cnt| post_tilt cnt| post_rescale cnt| rescale_factor cnt|  SCF amt| raw_DRF amt| post_tilt amt| post_rescale amt| rescale_factor amt|
|:----------|-------:|-----------:|-------------:|----------------:|------------------:|--------:|-----------:|-------------:|----------------:|------------------:|
|nw         |  134.4M|      146.0M|        120.2M|           157.3M|              1.308| $139.12T|    $177.79T|      $140.73T|         $139.12T|              0.989|
|equities   |   38.8M|       41.5M|         35.6M|            73.3M|              2.057|  $21.06T|     $28.19T|       $22.52T|          $21.06T|              0.935|
|bonds      |   13.1M|       13.2M|         12.7M|            50.3M|              3.969|   $4.84T|      $5.66T|        $5.25T|           $4.84T|              0.922|
|homes      |   95.5M|       99.9M|         85.3M|           122.9M|              1.442|  $40.85T|     $45.85T|       $38.39T|          $40.85T|              1.064|
|retirement |   78.7M|       85.0M|         72.3M|           110.0M|              1.521|  $23.84T|     $26.80T|       $22.74T|          $23.84T|              1.048|
|business   |   18.5M|       18.0M|         17.3M|            48.3M|              2.783|  $30.77T|     $46.93T|       $32.78T|          $30.77T|              0.939|
|other      |  143.1M|      154.9M|        131.6M|           162.5M|              1.235|  $34.41T|     $43.06T|       $34.48T|          $34.41T|              0.998|
|debt       |  113.9M|      122.0M|        103.3M|           134.2M|              1.299|  $16.66T|     $18.70T|       $15.43T|          $16.66T|              1.079|

---

## Microdata for re-cuts

All record-level microdata is saved — you can build new tables without
re-running the harness:

  - 3-stage record-level Y matrices: /nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline/wealth_harness_tilt_diag.rds
     (y_pre_tilt = raw DRF leaf draw; y_post_tilt_pre_rescale =
      after tilt, before Step B; y_post_rescale = final.
     Each is a tibble with id + 23 wealth Y vars.)
  - PUF X-side covariates: /nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline/tax_units_2022.csv
  - SCF target side: resources/cache/scf_tax_units.rds

Re-run this report after editing `src/eda/report_v3.R`:

```
sbatch slurm_render_status.sh   # re-renders HTML from AM_STATUS.md
```

(or call `Rscript src/eda/report_v3.R <output_dir>` via sbatch to
rebuild AM_STATUS.md from microdata first, then re-render HTML.)
