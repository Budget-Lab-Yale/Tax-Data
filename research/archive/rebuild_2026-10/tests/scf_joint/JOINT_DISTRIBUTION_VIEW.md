# Where the wealth lands: observed SCF, raw DRF, transformer

Same 12,021 held-out SCF tax-unit records (2,207 households) from 2013 and 2016. All levels are population-weighted; methods are means of three fits with five draws each. Income groups are ranks of total income within source and year. Rows without observed SCF wealth are excluded from all three columns.

Raw DRF means the refitted production donor-draw stage **before** its later calibration. The transformer is the baseline joint PUF+SCF model, completed for held-out SCF units as SCF. The experimental survey-weighted DRF is excluded.

## Share of total net worth by income group

| Income group | Observed SCF | Raw DRF | Transformer |
|---|---:|---:|---:|
| Bottom 50% | 8.9% | 7.1% | 10.9% |
| 50-90% | 27.1% | 22.7% | 28.9% |
| 90-99% | 33.4% | 32.0% | 35.1% |
| Top 1% | 30.6% | 38.2% | 25.1% |

The top 1% of **income** holds 30.6% of observed wealth. The transformer puts 25.1% there; raw DRF puts 38.2% there. This measures the wealth–income link, distinct from the share held by the top 1% of **wealth**.

## Mean net worth within each income group

| Income group | Observed SCF | Raw DRF | Transformer |
|---|---:|---:|---:|
| Bottom 50% | 132k | 118k | 181k |
| 50-90% | 407k | 385k | 489k |
| 90-99% | 2,173k | 2,351k | 2,569k |
| Top 1% | 17,691k | 24,989k | 16,380k |

## Share directly holding stocks

| Income group | Observed SCF | Raw DRF | Transformer |
|---|---:|---:|---:|
| Bottom 50% | 8.5% | 8.8% | 10.4% |
| 50-90% | 23.1% | 21.3% | 24.1% |
| 90-99% | 50.9% | 51.6% | 54.4% |
| Top 1% | 79.1% | 78.9% | 84.6% |

## Share owning a primary home

| Income group | Observed SCF | Raw DRF | Transformer |
|---|---:|---:|---:|
| Bottom 50% | 48.4% | 47.1% | 47.9% |
| 50-90% | 78.4% | 75.5% | 75.2% |
| 90-99% | 93.0% | 92.6% | 93.0% |
| Top 1% | 99.3% | 97.3% | 97.6% |

## Share owning a pass-through business

| Income group | Observed SCF | Raw DRF | Transformer |
|---|---:|---:|---:|
| Bottom 50% | 5.1% | 5.5% | 4.9% |
| 50-90% | 12.9% | 12.4% | 11.8% |
| 90-99% | 31.9% | 28.4% | 29.9% |
| Top 1% | 63.5% | 66.6% | 67.4% |

## Cross-variable ownership

| Relationship | Observed SCF | Raw DRF | Transformer |
|---|---:|---:|---:|
| Mortgage among homeowners | 70.9% | 67.3% | 69.0% |
| Business assets among units with business income | 58.2% | 38.0% | 46.9% |
| Stocks among units with interest or dividends | 52.1% | 48.7% | 58.7% |

The accompanying [interactive charts](JOINT_DISTRIBUTION_VIEW.html) and [stacked wealth chart](JOINT_DISTRIBUTION_VIEW.svg) show these same evaluated values. These SCF holdout comparisons test relationships that are observed; PUF wealth is not observed and cannot be validated record by record.

## Architectural note: PUF as the starting population

The PUF-first version can keep the actual PUF rows, tax fields, and weights as the starting population. One model trained across SCF, SIPP, and other surveys can learn the missing survey fields given their common fields; it then fills those fields on PUF rows. The resulting completed records still define one joint distribution over the tax and survey fields. Reweighting can move that joint distribution to aggregate targets. It leaves PUF-only tax–wealth dependence beyond the shared fields as a modeling assumption until bridging evidence is added.

Source: `$S/logs/levels_28877312.out` and expB `detail_cond.csv` for seeds 1–3. Generate with `python3 make_joint_distribution_view.py`.
