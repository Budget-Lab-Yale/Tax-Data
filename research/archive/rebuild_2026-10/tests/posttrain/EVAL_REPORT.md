---
title: "Test 3 — evaluation of the post-training methods"
role: results
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/posttrain/SPEC.md
supersedes: []
superseded_by: null
---

# Test 3: evaluation

Piece E of `SPEC.md`. The main line (contract entries of 20:45 and 20:55)
is the full model: one transformer over test 1's PUF variables and the 24
SCF wealth components, in two data versions (version 2 is the main one,
version 1 the fallback). This report measures, first, how close the full
model's records are to ground truth before any post-training, and second,
what each post-training method does once its files land. The earlier work
on test 2's model is a pilot, kept in the appendix.

## Summary

Written 9 October 22:15 to 23:10, from version 2 seeds 1 to 3 and
version 1 seeds 1 and 3 of the full model, and the first post-training
files. The tables below are regenerated as more files land; this text is
not.

**Whole PUF records generated from nothing.** The middle of the income
distribution comes out close to the PUF training records, and the top does
not. In version 2, mean AGI in the bottom half is $12,000 to $13,000
against $14,000, and in the 50th to 90th percentiles $63,000 to $69,000
against $71,000. In the top 0.1% of AGI it is $1.3 to $1.7 million against
$6.1 million, and total AGI is $6.2 to $6.6 trillion against $8.1
trillion. The shortfall is in the model's draws, not in turning tokens
into dollars: 0.24% to 0.29% of generated records fall in the top 1% rank
band of AGI, against 0.99% of the training records, and 0.009% to 0.013%
fall in the top 0.1% band, against 0.099%. Version 1 shows the same
pattern. On test 1's distance measures the generated files sit about
twice the sampling floor (median single-variable distance 0.010 to 0.014
against a floor of 0.005; median two-way table distance 0.022 to 0.030
against 0.012). Test 1's classifier reports an AUC of 1.0 only because
every generated record carries the same weight and that classifier uses
the weight; the report adds a classifier that leaves the weight out.

**SCF records' wealth redrawn from their observed variables.** Against the
same records' true wealth (training records, version 2, seeds 1 and 2),
mean net worth is too high in the lower income groups and too low at the
very top: $143,000 to $151,000 against $112,000 in the bottom half of
income, and $40 to $43 million against $52 million in the top 0.1% of
income. Version 1 had $31 million there, as did test 2's model. Overall top
shares are close (top 1% of net worth: 38% to 39% against 38%). The
untargeted measures (holding by income and age together, joint holding,
rank correlations) are off by two to five times the draw-to-draw noise.
Held-out SCF test records give a similar picture.

**PUF records' wealth, against the SCF interpolated to 2015.** Mean net
worth comes out above the interpolated SCF in every income group below the
top 0.1% (all records: $713,000 and $807,000 for seeds 2 and 1, against
$628,000), and the median below it ($55,000 to $57,000 against $91,000),
so the completed PUF wealth is more spread out than the SCF's. Seeds
differ by more than draw noise at the top (mean net worth in the top 0.1%
income group: $50 million and $74 million against $55 million). The PUF
and SCF differ in unit and income measurement, so these gaps are not all
model error.

**Post-training (first result, version 1, arm P).** Before any
post-training, the reference model's draws for PUF records miss the core
wealth targets (wealth bins by income group, age, marital status and
dependents, carried from the SCF) by 0.85% of a group's weight on average,
worst cell 24%, and 8 positive cells are reached by no draw. Method 2 (the
probability tilt, fitted to the core set) brings the average miss to
0.08% (worst cell 2.0%) before any residual calibration. The move costs a
KL divergence of 0.67 nats per unit of weight, which is as if each
record's weight were spread evenly over half (51%) of its draws; piece C's
own estimate agrees (0.673). The tilt brings overall net worth into line
with the interpolated SCF (mean $630,000 against $628,000; median $83,000
against $91,000; top 1% share 38.2% against 37.8%), and it lowers mean net
worth in the top 0.1% income group from $54 million to $40 million against
the SCF's $55 million. The income groups in these levels tables are test
2's seven-income total, not the AGI-type measure that defines the target
groups, so this table is not one of the targets.

Method 1 (reweighting the reference draws, core set, version 1) moved
further and fit worse. Its KL is 1.21 nats per unit of weight (as if each
record's weight sat on 30% of its draws), each record keeps 9.6 effective
draws of 32 on average, and 29% of the weight sits on records with fewer
than 4. Piece B's solver did not reach an exact fit within its time
budget: of the core set's cells it lists 3,854 as missed and 1,211 as
unable to be hit together, so the average miss is still 0.23% of a
group's weight (worst cell 38%). Mean net worth comes out at $662,000
(SCF $628,000) and the top 0.1% income group at $42 million.

On the pilot (test 2's model, SCF records, truth known; appendix) the same
ordering holds: reweighting needed a KL of 1.93 and left 5.2 effective
draws of 32, while the tilt needed 1.31 and, after residual calibration,
hit every target with 0.009 nats more.

Method 3 and the residual calibrations on the full model were still
running at 23:10; the watcher adds them to the sections below.

## What ran

- `e_full.py` reads piece H's files for each data version and seed: whole
  PUF records generated from nothing; PUF training records completed with
  wealth; SCF training and test records with their wealth set aside and
  redrawn. Generated PUF records are compared with the PUF training
  records (levels by AGI group, and test 1's measures from
  `../puf_generator/evaluate.py` against both training and held-out
  records). SCF records' drawn wealth is compared with the same records'
  true wealth. PUF records' drawn wealth is compared with the SCF 2013 and
  2016 training records interpolated to 2015 (as test 2's
  `c_evaluate_puf.py`). It also evaluates the post-training files for arm P
  as they land under `$P/<version>/`.
- `e_evaluate.py` and `e_puf.py` do the same for the pilot on test 2's
  model.
- `e_compare.py` sets everything side by side below. Every number is
  weighted by population weight; a record with several draws counts each
  draw at its share of the record's weight (or at the method's draw
  weight). Draw noise columns compare a file's own draws with each other.
- All of it runs in Slurm; `e_watch.sh` repeats it every ten minutes
  until 06:00 on 10 October (see `STATUS.md`).

<!-- BEGIN GENERATED (e_compare.py) -->
## Pretraining: the full model, version 2 (main: Schedule E total added, more SCF-observed PUF items), against ground truth

The full model (piece H) is one transformer over test 1's PUF variables and the 24 SCF wealth components, trained on PUF 2015 and SCF 2013 and 2016 records together. These sections measure how close its records are to the data before any post-training.

### Whole PUF records generated from nothing

The full model generates as many PUF records as there are PUF training records, from nothing but the PUF tag and the year 2015, each at weight (total PUF weight) / (number generated). Each file is ranked on its own AGI into the five groups. Levels first, then test 1's distance measures.

**Mean AGI, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $14k | $13k | $12k | $12k |
| 50-90% | $71k | $69k | $64k | $63k |
| 90-99% | $207k | $174k | $169k | $168k |
| 99-99.9% | $804k | $404k | $400k | $380k |
| Top 0.1% | $6.05m | $1.64m | $1.32m | $1.70m |

**Mean wages, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $13k | $14k | $13k | $12k |
| 50-90% | $55k | $57k | $53k | $52k |
| 90-99% | $146k | $149k | $140k | $141k |
| 99-99.9% | $419k | $306k | $299k | $293k |
| Top 0.1% | $1.75m | $744k | $715k | $675k |

**Mean Schedule C, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $1k | $1k | $1k | $1k |
| 50-90% | $2k | $1k | $1k | $2k |
| 90-99% | $8k | $4k | $3k | $4k |
| 99-99.9% | $30k | $8k | $7k | $7k |
| Top 0.1% | $82k | $19k | $16k | $20k |

**Mean partnership / S corp, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $-1k | $1k | $0k | $0k |
| 50-90% | $1k | $2k | $2k | $2k |
| 90-99% | $12k | $11k | $9k | $11k |
| 99-99.9% | $172k | $45k | $63k | $45k |
| Top 0.1% | $1.45m | $178k | $263k | $122k |

**Mean long-term gains, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $0k | $1k | $2k | $1k |
| 50-90% | $1k | $2k | $2k | $1k |
| 90-99% | $9k | $7k | $6k | $9k |
| 99-99.9% | $103k | $31k | $44k | $38k |
| Top 0.1% | $2.05m | $181k | $314k | $115k |

**Mean dividends, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $0k | $1k | $1k | $1k |
| 50-90% | $1k | $1k | $1k | $1k |
| 90-99% | $5k | $3k | $3k | $3k |
| 99-99.9% | $36k | $9k | $11k | $12k |
| Top 0.1% | $370k | $57k | $76k | $38k |

**Mean Social Security, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $3k | $4k | $4k | $4k |
| 50-90% | $4k | $4k | $4k | $5k |
| 90-99% | $6k | $3k | $4k | $4k |
| 99-99.9% | $6k | $3k | $4k | $4k |
| Top 0.1% | $7k | $4k | $6k | $2k |

**Share of all AGI (%), by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | 10.3 | 11.6 | 11.7 | 11.6 |
| 50-90% | 42.2 | 50.2 | 49.5 | 49.1 |
| 90-99% | 27.7 | 28.6 | 29.3 | 29.3 |
| 99-99.9% | 10.8 | 6.6 | 6.9 | 6.7 |
| Top 0.1% | 9.0 | 3.0 | 2.5 | 3.3 |

**Share with wages (%), by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | 78.3 | 79.5 | 80.2 | 75.3 |
| 50-90% | 87.1 | 88.4 | 88.6 | 86.9 |
| 90-99% | 88.5 | 92.8 | 92.9 | 91.9 |
| 99-99.9% | 85.9 | 93.8 | 92.4 | 95.4 |
| Top 0.1% | 82.7 | 88.6 | 81.9 | 91.0 |

**Share with long-term gains or losses (%), by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | 4.7 | 4.8 | 4.9 | 5.3 |
| 50-90% | 11.8 | 10.2 | 9.1 | 10.0 |
| 90-99% | 34.4 | 24.7 | 23.7 | 25.4 |
| 99-99.9% | 71.8 | 43.0 | 46.5 | 49.8 |
| Top 0.1% | 90.5 | 66.3 | 69.3 | 69.9 |

**Share with Schedule C (%), by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | 16.2 | 14.6 | 12.7 | 15.2 |
| 50-90% | 15.3 | 13.8 | 12.7 | 14.4 |
| 90-99% | 21.7 | 17.7 | 16.8 | 20.2 |
| 99-99.9% | 25.7 | 20.7 | 18.6 | 20.5 |
| Top 0.1% | 25.6 | 27.7 | 19.3 | 18.1 |

| Total | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Total weight (millions) | 120.7 | 120.7 | 120.7 | 120.7 |
| Total AGI ($ billions) | 8,094 | 6,616 | 6,264 | 6,209 |
| Median AGI | $36k | $35k | $34k | $32k |

**Test 1's measures** (TVD: total variation distance between weighted binned distributions; the floor is the training records themselves against the held-out test records, i.e. sampling noise only). Test 1's classifier uses the record weight as a variable; generated records all carry the same weight, so its AUC of 1 says only that; the last row leaves the weight out.

| Test 1 measure | Seed 1 vs training records | Seed 1 vs held-out test records | Floor: training vs test records | Test records' own value | Seed 2 vs training records | Seed 2 vs held-out test records | Floor: training vs test records | Test records' own value | Seed 3 vs training records | Seed 3 vs held-out test records | Floor: training vs test records | Test records' own value |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Single variables: median TVD across 40 | 0.010 | 0.011 | 0.005 |  | 0.015 | 0.014 | 0.005 |  | 0.014 | 0.013 | 0.005 |  |
| Two-way tables: median TVD across 780 pairs | 0.022 | 0.026 | 0.012 |  | 0.028 | 0.030 | 0.012 |  | 0.025 | 0.027 | 0.012 |  |
| Two-way tables: 90th percentile TVD | 0.049 | 0.051 | 0.022 |  | 0.055 | 0.057 | 0.022 |  | 0.053 | 0.059 | 0.022 |  |
| Three-way tables: mean TVD | 0.062 | 0.078 | 0.038 |  | 0.067 | 0.078 | 0.038 |  | 0.071 | 0.085 | 0.038 |  |
| Nonzero patterns of income items: TVD | 0.086 | 0.108 | 0.080 |  | 0.094 | 0.119 | 0.080 |  | 0.086 | 0.113 | 0.080 |  |
| Top 1% share of AGI | 0.093 | 0.093 | 0.194 | 0.195 | 0.092 | 0.092 | 0.194 | 0.195 | 0.097 | 0.097 | 0.194 | 0.195 |
| Top 0.1% share of AGI | 0.029 | 0.029 | 0.088 | 0.089 | 0.025 | 0.025 | 0.088 | 0.089 | 0.032 | 0.032 | 0.088 | 0.089 |
| Classifier AUC (0.5 = indistinguishable) | 1.000 | 1.000 | 0.505 |  | 1.000 | 1.000 | 0.505 |  | 1.000 | 1.000 | 0.505 |  |
| Classifier AUC with the weight left out (E; training records against generated) | 0.835 |  |  |  | 0.842 |  |  |  | 0.841 |  |  |  |

Test 1's full tables for seed 1 against the held-out test records: `e/puf_eval/full_v2_gen_seed01/report_tables.md` under the scratch folder.


### SCF training records: wealth drawn by the full model against the truth

Each SCF training record with wealth observed has its 24 wealth components set aside and redrawn 5 times from its observed variables (the crosswalked PUF items, demographics, year). Income groups: test 2's seven-income total, ranked within wave over all SCF units of the split. Draw noise: draws 0 and 1 against each other (one draw per record), and the first 2 draws against the next 2.



#### Net worth by income group, in levels

Income groups rank the seven-income total within each wave over all SCF training units. Net worth is in dollars of the wave (2013 and 2016 pooled at their population weights). The draw-noise column is the absolute difference between two halves of one set of reference draws, the size of draw-to-draw noise for a column built from many draws per record.

**Mean net worth**

| Income group | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | $112k | $151k | $143k | $149k | $4k |
| 50-90% | $366k | $469k | $415k | $450k | $4k |
| 90-99% | $2.09m | $2.46m | $2.23m | $2.63m | $5k |
| 99-99.9% | $11.90m | $12.49m | $12.32m | $13.02m | $716k |
| Top 0.1% | $52.40m | $43.35m | $39.52m | $41.70m | $1.52m |

**Median net worth**

| Income group | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | $17k | $15k | $16k | $16k | $0k |
| 50-90% | $144k | $138k | $131k | $139k | $1k |
| 90-99% | $1.03m | $981k | $954k | $963k | $35k |
| 99-99.9% | $7.42m | $7.00m | $6.89m | $7.62m | $11k |
| Top 0.1% | $27.81m | $25.17m | $21.37m | $21.63m | $1.84m |

**Share of all net worth (%)**

| Income group | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | 8.1 | 9.4 | 9.7 | 9.1 | 0.1 |
| 50-90% | 26.9 | 29.7 | 28.6 | 28.1 | 0.1 |
| 90-99% | 35.0 | 35.6 | 35.1 | 37.4 | 0.5 |
| 99-99.9% | 20.1 | 18.2 | 19.5 | 18.7 | 0.8 |
| Top 0.1% | 9.9 | 7.1 | 7.0 | 6.7 | 0.3 |

**All records** (top shares rank records by net worth)

| Statistic | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Mean net worth | $608k | $705k | $648k | $717k | $9k |
| Median net worth | $90k | $85k | $83k | $88k | $0k |
| 90th percentile | $1.06m | $1.18m | $1.10m | $1.12m | $35k |
| 99th percentile | $9.08m | $10.50m | $9.56m | $10.85m | $310k |
| Top 10% share (%) | 75.8 | 78.3 | 77.2 | 79.1 | 0.1 |
| Top 1% share (%) | 37.6 | 39.2 | 38.2 | 40.8 | 0.4 |
| Top 0.1% share (%) | 14.4 | 14.6 | 14.0 | 16.2 | 0.8 |
| Share with negative net worth (%) | 11.2 | 12.1 | 12.0 | 11.5 | 0.1 |

**Mean net worth by age of head**

| Age | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| under 26 | $32k | $21k | $24k | $26k | $3k |
| 26 to 34 | $79k | $119k | $129k | $119k | $33k |
| 35 to 44 | $321k | $405k | $421k | $447k | $12k |
| 45 to 54 | $625k | $671k | $649k | $708k | $21k |
| 55 to 64 | $1.03m | $1.16m | $1.07m | $1.19m | $2k |
| 65 and over | $1.12m | $1.33m | $1.12m | $1.29m | $27k |

#### Holding one component given another

Share holding a nonzero amount of the first, among those holding the second (or with that income nonzero).

| Rate (%) | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Mortgage, among homeowners | 68.7 | 69.4 | 68.5 | 68.6 | 0.3 |
| Mortgage on other property, among owners of other homes | 39.6 | 38.4 | 39.8 | 36.3 | 0.3 |
| Stocks, among owners of a business (pass-through) | 32.6 | 35.3 | 35.5 | 36.2 | 1.2 |
| Stocks, among units with business income | 26.5 | 26.1 | 27.0 | 25.9 | 0.0 |
| Business (pass-through) asset, among units with business income | 51.7 | 49.6 | 53.1 | 49.5 | 0.3 |
| Unrealized gain or loss on the home, among homeowners | 95.0 | 95.7 | 95.1 | 96.0 | 0.1 |
| Unrealized gain or loss on other real estate, among its owners | 86.4 | 91.0 | 88.7 | 88.5 | 0.4 |
| Unrealized gain or loss on the business, among business owners | 90.1 | 92.0 | 92.7 | 92.3 | 0.1 |
| Unrealized gain or loss on stocks and funds, among stockholders | 90.2 | 90.8 | 91.9 | 91.5 | 0.1 |
| Other homes, among units with rental income | – | – | – | – | – |
| Home, among stockholders | 86.0 | 88.3 | 86.0 | 86.1 | 0.2 |
| Credit-card debt, among homeowners | 44.4 | 43.7 | 43.4 | 43.6 | 0.0 |

#### Rank correlation of amounts with total income, among positive holders

Weighted correlation of the amount's rank band with the record's income rank.

| Component | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| cash | 0.549 | 0.493 | 0.496 | 0.488 | 0.003 |
| equities | 0.340 | 0.292 | 0.309 | 0.319 | 0.010 |
| dc | 0.519 | 0.470 | 0.468 | 0.461 | 0.005 |
| pass_throughs | 0.426 | 0.460 | 0.469 | 0.441 | 0.017 |
| primary_home | 0.546 | 0.515 | 0.509 | 0.495 | 0.009 |
| primary_mortgage | 0.464 | 0.455 | 0.459 | 0.436 | 0.004 |
| other_home | 0.388 | 0.330 | 0.342 | 0.353 | 0.016 |
| kg_other | 0.373 | 0.309 | 0.335 | 0.342 | 0.019 |
| bonds | 0.289 | 0.239 | 0.255 | 0.277 | 0.031 |
| credit_cards | 0.315 | 0.297 | 0.291 | 0.271 | 0.002 |

#### Summary of untargeted gaps

Each row is an average absolute gap against the truth over many cells (definitions as in test 2, with five income groups). The two noise columns compare reference draws with each other: one draw against another (the noise of a file with one draw per record) and two halves of one set of reference draws against each other (the noise of a many-draw column). A reweighted column with few effective draws per record sits between the two. Gaps no larger than the noise are not evidence of a difference.

| Gap against the truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Noise: one ref draw vs another | Noise: many draws vs many |
|---|---:|---:|---:|---:|---:|
| Share holding each component, by income group x age (30 cells x 24): average gap, points | 3.35 | 3.62 | 3.57 | 2.32 | 1.40 |
| Median amount rank among holders, by income group x age: average gap, percentiles | 4.58 | 4.74 | 4.74 | 3.06 | 2.36 |
| Pairs of components (276 two-by-two holding tables): average share misplaced, % | 1.15 | 1.57 | 1.44 | 0.35 | 0.29 |
| Same, worst pair, % | 3.20 | 4.70 | 3.80 | 0.85 | 0.84 |
| Pattern of holding across all 24 components: share of weight in the wrong pattern, % | 29.7 | 30.6 | 30.8 | 19.9 | 16.0 |
| Twelve conditional holding rates (listed below): average gap, points | 1.67 | 1.18 | 1.59 | 0.49 | 0.26 |
| Rank correlation of each amount with total income, among holders: average gap | 0.044 | 0.046 | 0.045 | 0.020 | 0.010 |
| Rank correlation between two amounts, among joint holders: average gap | 0.067 | 0.066 | 0.067 | 0.034 | 0.030 |
| Mean net worth by income group (5 groups): average gap, % | 20.5 | 15.3 | 22.4 | 8.6 | 2.6 |
| Median net worth by income group: average gap, % | 6.9 | 9.8 | 7.6 | 4.4 | 2.4 |
| Share of all net worth held by each income group: sum of absolute gaps, points | 9.5 | 6.9 | 9.3 | 6.4 | 1.9 |
| Top 1% share of net worth (by net worth): gap, points | 1.63 | 0.63 | 3.14 | 0.91 | 0.42 |
| Top 0.1% share of net worth: gap, points | 0.22 | 0.38 | 1.81 | 0.49 | 0.80 |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) | 0.541 | 0.562 | 0.557 | 0.503 | – |


### SCF held-out test records: wealth drawn by the full model against the truth

Each SCF held-out test record with wealth observed has its 24 wealth components set aside and redrawn 5 times from its observed variables (the crosswalked PUF items, demographics, year). Income groups: test 2's seven-income total, ranked within wave over all SCF units of the split. Draw noise: draws 0 and 1 against each other (one draw per record), and the first 2 draws against the next 2.



#### Net worth by income group, in levels

Income groups rank the seven-income total within each wave over all SCF training units. Net worth is in dollars of the wave (2013 and 2016 pooled at their population weights). The draw-noise column is the absolute difference between two halves of one set of reference draws, the size of draw-to-draw noise for a column built from many draws per record.

**Mean net worth**

| Income group | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | $132k | $169k | $167k | $158k | $20k |
| 50-90% | $407k | $536k | $451k | $509k | $2k |
| 90-99% | $2.17m | $2.79m | $2.52m | $2.81m | $231k |
| 99-99.9% | $14.61m | $15.98m | $14.67m | $17.95m | $14k |
| Top 0.1% | $45.09m | $50.23m | $45.21m | $45.46m | $3.95m |

**Median net worth**

| Income group | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | $19k | $15k | $16k | $16k | $0k |
| 50-90% | $149k | $144k | $138k | $146k | $1k |
| 90-99% | $915k | $1.02m | $1.00m | $1.03m | $15k |
| 99-99.9% | $9.02m | $9.26m | $7.97m | $8.98m | $179k |
| Top 0.1% | $14.79m | $29.04m | $27.82m | $28.84m | $3.99m |

**Share of all net worth (%)**

| Income group | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | 8.9 | 9.3 | 10.2 | 8.7 | 0.6 |
| 50-90% | 27.1 | 28.9 | 27.1 | 27.4 | 1.2 |
| 90-99% | 33.4 | 34.7 | 35.0 | 35.0 | 1.3 |
| 99-99.9% | 22.7 | 20.1 | 20.6 | 22.6 | 0.9 |
| Top 0.1% | 7.9 | 7.1 | 7.1 | 6.4 | 0.2 |

**All records** (top shares rank records by net worth)

| Statistic | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Mean net worth | $661k | $816k | $731k | $817k | $38k |
| Median net worth | $85k | $84k | $87k | $88k | $0k |
| 90th percentile | $1.09m | $1.29m | $1.17m | $1.22m | $32k |
| 99th percentile | $9.95m | $12.10m | $11.15m | $12.37m | $233k |
| Top 10% share (%) | 77.8 | 80.3 | 79.0 | 80.9 | 0.6 |
| Top 1% share (%) | 37.3 | 40.6 | 39.1 | 41.9 | 2.8 |
| Top 0.1% share (%) | 13.3 | 15.4 | 13.4 | 16.4 | 4.3 |
| Share with negative net worth (%) | 10.1 | 12.0 | 12.1 | 11.5 | 0.1 |

**Mean net worth by age of head**

| Age | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| under 26 | $27k | $20k | $23k | $32k | $7k |
| 26 to 34 | $77k | $89k | $96k | $104k | $17k |
| 35 to 44 | $253k | $457k | $459k | $534k | $61k |
| 45 to 54 | $598k | $730k | $649k | $826k | $42k |
| 55 to 64 | $1.20m | $1.43m | $1.36m | $1.45m | $41k |
| 65 and over | $1.36m | $1.60m | $1.31m | $1.40m | $118k |

#### Holding one component given another

Share holding a nonzero amount of the first, among those holding the second (or with that income nonzero).

| Rate (%) | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Mortgage, among homeowners | 70.9 | 70.5 | 68.7 | 69.0 | 0.1 |
| Mortgage on other property, among owners of other homes | 41.8 | 39.1 | 39.7 | 36.9 | 1.2 |
| Stocks, among owners of a business (pass-through) | 36.9 | 38.0 | 37.4 | 39.1 | 1.5 |
| Stocks, among units with business income | 30.1 | 28.2 | 29.1 | 27.7 | 0.5 |
| Business (pass-through) asset, among units with business income | 57.9 | 52.0 | 53.9 | 50.9 | 1.6 |
| Unrealized gain or loss on the home, among homeowners | 95.2 | 95.5 | 95.1 | 96.0 | 0.4 |
| Unrealized gain or loss on other real estate, among its owners | 87.4 | 91.7 | 88.3 | 88.8 | 0.5 |
| Unrealized gain or loss on the business, among business owners | 90.8 | 91.6 | 93.0 | 92.5 | 0.1 |
| Unrealized gain or loss on stocks and funds, among stockholders | 91.7 | 91.4 | 92.3 | 91.3 | 0.6 |
| Other homes, among units with rental income | – | – | – | – | – |
| Home, among stockholders | 83.5 | 88.7 | 86.6 | 86.7 | 0.3 |
| Credit-card debt, among homeowners | 47.1 | 43.4 | 43.9 | 42.6 | 0.7 |

#### Rank correlation of amounts with total income, among positive holders

Weighted correlation of the amount's rank band with the record's income rank.

| Component | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| cash | 0.576 | 0.498 | 0.498 | 0.492 | 0.005 |
| equities | 0.427 | 0.271 | 0.301 | 0.311 | 0.024 |
| dc | 0.519 | 0.480 | 0.487 | 0.482 | 0.005 |
| pass_throughs | 0.447 | 0.443 | 0.458 | 0.429 | 0.019 |
| primary_home | 0.579 | 0.520 | 0.519 | 0.508 | 0.000 |
| primary_mortgage | 0.511 | 0.445 | 0.464 | 0.440 | 0.003 |
| other_home | 0.300 | 0.357 | 0.379 | 0.376 | 0.014 |
| kg_other | 0.398 | 0.292 | 0.321 | 0.339 | 0.013 |
| bonds | 0.351 | 0.270 | 0.276 | 0.296 | 0.028 |
| credit_cards | 0.307 | 0.287 | 0.291 | 0.278 | 0.010 |

#### Summary of untargeted gaps

Each row is an average absolute gap against the truth over many cells (definitions as in test 2, with five income groups). The two noise columns compare reference draws with each other: one draw against another (the noise of a file with one draw per record) and two halves of one set of reference draws against each other (the noise of a many-draw column). A reweighted column with few effective draws per record sits between the two. Gaps no larger than the noise are not evidence of a difference.

| Gap against the truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Noise: one ref draw vs another | Noise: many draws vs many |
|---|---:|---:|---:|---:|---:|
| Share holding each component, by income group x age (30 cells x 24): average gap, points | 5.55 | 5.52 | 5.41 | 3.09 | 2.61 |
| Median amount rank among holders, by income group x age: average gap, percentiles | 6.92 | 6.92 | 6.79 | 4.01 | 3.74 |
| Pairs of components (276 two-by-two holding tables): average share misplaced, % | 1.65 | 2.31 | 2.11 | 0.51 | 0.46 |
| Same, worst pair, % | 4.36 | 5.96 | 5.20 | 1.35 | 1.43 |
| Pattern of holding across all 24 components: share of weight in the wrong pattern, % | 40.7 | 42.0 | 42.3 | 29.6 | 24.3 |
| Twelve conditional holding rates (listed below): average gap, points | 2.41 | 1.79 | 2.74 | 1.17 | 0.68 |
| Rank correlation of each amount with total income, among holders: average gap | 0.060 | 0.057 | 0.060 | 0.023 | 0.017 |
| Rank correlation between two amounts, among joint holders: average gap | 0.088 | 0.086 | 0.086 | 0.054 | 0.045 |
| Mean net worth by income group (5 groups): average gap, % | 21.7 | 10.7 | 19.6 | 4.7 | 5.9 |
| Median net worth by income group: average gap, % | 27.4 | 26.4 | 25.8 | 5.9 | 4.2 |
| Share of all net worth held by each income group: sum of absolute gaps, points | 6.7 | 5.7 | 3.7 | 1.7 | 4.3 |
| Top 1% share of net worth (by net worth): gap, points | 3.33 | 1.78 | 4.54 | 1.79 | 2.81 |
| Top 0.1% share of net worth: gap, points | 2.13 | 0.12 | 3.05 | 2.67 | 4.26 |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) | 0.565 | 0.594 | 0.580 | 0.482 | – |


### PUF training records completed with wealth, against the SCF

PUF training records (2015) are completed with the 24 wealth components, presented to the model as SCF records of 2015. The PUF has no wealth, so the comparison is with the SCF 2013 and 2016 training records and their interpolation to 2015 (one third 2013, two thirds 2016; dollar levels interpolated in logs). Income groups rank the seven-income total among PUF records on the PUF side and among SCF units of each wave on the SCF side. The SCF's own change from 2013 to 2016 (which includes its sampling noise) is a scale for how large a gap is. The PUF and SCF differ in unit, sampling and income measurement, so matching the SCF's aggregates does not validate the PUF's joint distribution.

Columns: **SCF 2013**: SCF 2013 training records with wealth observed (full dataset, true wealth); **SCF 2016**: SCF 2016 training records with wealth observed (full dataset, true wealth); **SCF interpolated 2015**: one third SCF 2013 plus two thirds SCF 2016 (dollar levels in logs); **Full model, seed 1**: PUF training records completed with wealth by the full model (seed01); **Full model, seed 2**: PUF training records completed with wealth by the full model (seed02); **Full model, seed 3**: PUF training records completed with wealth by the full model (seed03).

#### Levels

##### Net worth by income group, in levels

Dollars of each year (PUF and interpolated SCF at 2015). "PUF draw noise" is the absolute difference between two halves of the PUF completion's draws.

**Mean net worth**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Bottom 50% | $109k | $115k | $113k | $178k | $164k | $202k | $19k |
| 50-90% | $339k | $390k | $372k | $576k | $522k | $664k | $30k |
| 90-99% | $1.82m | $2.34m | $2.15m | $2.84m | $2.64m | $3.37m | $82k |
| 99-99.9% | $9.78m | $13.85m | $12.33m | $17.52m | $14.91m | $18.54m | $281k |
| Top 0.1% | $41.17m | $62.94m | $54.64m | $73.53m | $50.41m | $68.18m | $44k |

**Median net worth**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Bottom 50% | $16k | $17k | $17k | $7k | $9k | $10k | $0k |
| 50-90% | $138k | $153k | $148k | $172k | $165k | $215k | $1k |
| 90-99% | $898k | $1.22m | $1.10m | $1.23m | $1.13m | $1.48m | $2k |
| 99-99.9% | $5.87m | $9.42m | $8.05m | $9.35m | $8.39m | $10.65m | $74k |
| Top 0.1% | $23.05m | $39.02m | $32.74m | $49.24m | $30.11m | $38.48m | $714k |

**Share of all net worth (%)**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Bottom 50% | 9.2 | 7.3 | 7.9 | 11.1 | 11.5 | 11.2 | 0.8 |
| 50-90% | 28.2 | 25.9 | 26.7 | 28.6 | 29.3 | 29.4 | 0.6 |
| 90-99% | 35.1 | 34.9 | 35.0 | 31.7 | 33.3 | 33.5 | 0.1 |
| 99-99.9% | 18.7 | 21.2 | 20.3 | 19.5 | 18.8 | 18.4 | 0.9 |
| Top 0.1% | 8.9 | 10.7 | 10.1 | 9.1 | 7.1 | 7.5 | 0.3 |

**All records** (top shares rank records by net worth)

| Statistic | SCF 2013 | SCF 2016 | SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Mean net worth | $530k | $683k | $628k | $807k | $713k | $904k | $26k |
| Median net worth | $85k | $95k | $91k | $55k | $57k | $75k | $0k |
| 90th percentile | $942k | $1.17m | $1.09m | $1.30m | $1.18m | $1.51m | $10k |
| 99th percentile | $7.90m | $10.30m | $9.43m | $11.64m | $10.81m | $13.47m | $353k |
| Top 10% share (%) | 74.5 | 76.5 | 75.8 | 81.6 | 80.3 | 80.1 | 0.4 |
| Top 1% share (%) | 34.6 | 39.4 | 37.8 | 44.1 | 40.5 | 40.5 | 1.3 |
| Top 0.1% share (%) | 12.6 | 15.4 | 14.4 | 17.1 | 15.0 | 16.1 | 1.1 |
| Share with negative net worth (%) | 11.1 | 11.3 | 11.2 | 15.4 | 14.7 | 13.5 | 0.0 |

**Mean net worth by age of head** (about 42% of PUF records have no age; they are their own row)

| Age | SCF 2013 | SCF 2016 | SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| under 26 | $26k | $37k | $33k | $32k | $35k | $50k | $2k |
| 26 to 34 | $87k | $71k | $76k | $95k | $96k | $122k | $5k |
| 35 to 44 | $347k | $293k | $310k | $309k | $316k | $399k | $29k |
| 45 to 54 | $506k | $747k | $656k | $540k | $501k | $668k | $60k |
| 55 to 64 | $795k | $1.24m | $1.07m | $847k | $775k | $978k | $20k |
| 65 and over | $1.04m | $1.18m | $1.14m | $1.22m | $1.10m | $1.35m | $57k |
| age unobserved (PUF) | – | – | – | $13.41m | $10.98m | $14.03m | $37k |

#### Holding one component given another (%)

| Rate (%) | SCF 2013 | SCF 2016 | SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Mortgage, among homeowners | 69.3 | 68.0 | 68.5 | 71.3 | 68.3 | 68.7 | 0.1 |
| Mortgage on other property, among owners of other homes | 38.3 | 40.8 | 40.0 | 41.9 | 44.1 | 46.4 | 0.5 |
| Stocks, among owners of a business (pass-through) | 34.0 | 31.3 | 32.2 | 40.1 | 37.6 | 39.1 | 0.2 |
| Stocks, among units with business income | 28.9 | 24.7 | 26.1 | 27.1 | 26.9 | 26.8 | 0.1 |
| Business (pass-through) asset, among units with business income | 52.2 | 51.4 | 51.7 | 50.8 | 56.6 | 53.4 | 0.4 |
| Unrealized gain or loss on the home, among homeowners | 95.1 | 94.9 | 95.0 | 95.5 | 94.5 | 95.2 | 0.2 |
| Unrealized gain or loss on other real estate, among its owners | 86.3 | 86.4 | 86.4 | 89.0 | 86.2 | 95.2 | 0.1 |
| Unrealized gain or loss on the business, among business owners | 90.6 | 89.6 | 89.9 | 92.3 | 93.1 | 92.4 | 0.3 |
| Unrealized gain or loss on stocks and funds, among stockholders | 91.4 | 89.0 | 89.8 | 89.1 | 91.2 | 91.8 | 0.2 |
| Other homes, among units with rental income | – | – | – | 69.3 | 63.1 | 77.0 | 0.5 |
| Home, among stockholders | 86.1 | 85.9 | 86.0 | 85.4 | 85.8 | 85.7 | 0.1 |
| Credit-card debt, among homeowners | 42.4 | 46.4 | 45.1 | 43.6 | 40.7 | 43.1 | 0.1 |

#### Summary of untargeted gaps

Same measures as for the SCF arm, against the interpolated SCF. The SCF 2013-against-2016 column is the scale from test 2: a gap is small if it is small next to how much the SCF itself moves in three years.

| Gap against SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Scale: SCF 2013 vs 2016 | PUF draw noise |
|---|---:|---:|---:|---:|---:|
| Share holding each component, by income group x age (30 cells x 24): average gap, points | 5.28 | 5.28 | 6.67 | 5.86 | 0.56 |
| Median amount rank among holders, by income group x age: average gap, percentiles | 5.91 | 5.64 | 6.25 | 7.02 | 1.42 |
| Pairs of components (276 two-by-two holding tables): average share misplaced, % | 5.14 | 4.91 | 6.82 | 2.15 | 0.13 |
| Same, worst pair, % | 16.35 | 17.36 | 23.08 | 7.44 | 0.41 |
| Pattern of holding across all 24 components: share of weight in the wrong pattern, % | 40.2 | 40.0 | 43.5 | 42.7 | 12.4 |
| Twelve conditional holding rates (listed below): average gap, points | 2.07 | 2.26 | 2.89 | 1.75 | 0.22 |
| Rank correlation of each amount with total income, among holders: average gap | 0.055 | 0.054 | 0.046 | 0.035 | 0.005 |
| Rank correlation between two amounts, among joint holders: average gap | 0.069 | 0.066 | 0.066 | 0.086 | 0.015 |
| Mean net worth by income group (5 groups): average gap, % | 44.2 | 27.3 | 57.6 | 28.7 | 4.2 |
| Median net worth by income group: average gap, % | 30.7 | 14.5 | 34.2 | 37.1 | 0.9 |
| Share of all net worth held by each income group: sum of absolute gaps, points | 10.0 | 12.4 | 11.8 | 8.7 | 2.7 |
| Top 1% share of net worth (by net worth): gap, points | 6.30 | 2.67 | 2.64 | 4.76 | 1.26 |
| Top 0.1% share of net worth: gap, points | 2.65 | 0.62 | 1.72 | 2.80 | 1.10 |


## Post-training on the full model (full_v2): arm P (PUF records completed with wealth)

PUF training records (2015) are completed with the 24 wealth components, presented to the model as SCF records of 2015. The PUF has no wealth, so the comparison is with the SCF 2013 and 2016 training records and their interpolation to 2015 (one third 2013, two thirds 2016; dollar levels interpolated in logs). Income groups rank the seven-income total among PUF records on the PUF side and among SCF units of each wave on the SCF side. The SCF's own change from 2013 to 2016 (which includes its sampling noise) is a scale for how large a gap is. The PUF and SCF differ in unit, sampling and income measurement, so matching the SCF's aggregates does not validate the PUF's joint distribution.

Columns: **SCF 2013**: SCF 2013 training records with wealth observed (full dataset, true wealth); **SCF 2016**: SCF 2016 training records with wealth observed (full dataset, true wealth); **SCF interpolated 2015**: one third SCF 2013 plus two thirds SCF 2016 (dollar levels in logs); **PUF reference**: reference full model completing PUF records, unweighted.

### Q1. How far each method moved from the reference model

KL is the Kullback–Leibler divergence of the final distribution of wealth from the reference model's, per unit of population weight, in nats; zero means no move. For intuition, a KL of log(2) = 0.69 is what one gets by putting each record's weight evenly on half of its draws and none on the rest; the column "as if spread over" turns each KL into that share (e^−KL). Effective draws per record is 1 / Σ q², where q are the shares of a record's weight on its draws (K when all draws count equally). For methods 2 and 3 the KL has two parts: the changed model (estimated as the average over draws of the log-probability under the new model minus that under the reference) and the residual calibration's weights on the new model's draws.

| Method | K | KL, changed model | KL, weights | KL, total | As if spread over | Effective draws | Records with < 4 effective draws | Largest share on one draw |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| PUF reference | 32 | – | 0.000 | 0.000 | 100% | 32.0 | 0.0% | 3% |

Method 1 with different numbers of reference draws per record (piece B's reports; "infeasible" cells are those B's solver set aside because no weighting of the available draws can hit them, with their share of all target weight):

| Run | K | KL | As if spread over | Effective draws | Records with < 4 effective | Infeasible cells | Their target weight | Largest relative error, feasible cells |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| ../full_v2/b/P_core/report_P_K8.json | 8 | 1.293 | 27% | 2.2 | 88.5% | 5138 | 99.828% | 9.5e-08 |

_Cross-check: KL as computed here from the weights and log-probabilities in each file; the pieces' own reports should agree._

### Q2. Target fit

Error in each target cell's weight as a share of its group's PUF weight, averaged over all cells of the target set, worst cell in brackets; "unreached": cells with a positive target that no weighted draw falls in. Core sets (core, wcore): wealth by income group, age, married, dependents. Full sets (full, wfull): also wealth by each income item's band.

| Column | wcore: mean (max) | wcore unreached | wfull: mean (max) | wfull unreached |
|---|---:|---:|---:|---:|
| PUF reference | 0.790% (24.9%) | 8 | 1.228% (67.6%) | 31 |

### Q3. Untargeted relationships

#### Net worth by income group, in levels

Dollars of each year (PUF and interpolated SCF at 2015). "PUF draw noise" is the absolute difference between two halves of the PUF completion's draws.

**Mean net worth**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 | PUF reference | PUF draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | $109k | $115k | $113k | $178k | $6k |
| 50-90% | $339k | $390k | $372k | $571k | $3k |
| 90-99% | $1.82m | $2.34m | $2.15m | $2.85m | $34k |
| 99-99.9% | $9.78m | $13.85m | $12.33m | $17.75m | $25k |
| Top 0.1% | $41.17m | $62.94m | $54.64m | $74.03m | $77k |

**Median net worth**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 | PUF reference | PUF draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | $16k | $17k | $17k | $7k | $0k |
| 50-90% | $138k | $153k | $148k | $171k | $0k |
| 90-99% | $898k | $1.22m | $1.10m | $1.22m | $5k |
| 99-99.9% | $5.87m | $9.42m | $8.05m | $9.36m | $9k |
| Top 0.1% | $23.05m | $39.02m | $32.74m | $49.32m | $0k |

**Share of all net worth (%)**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 | PUF reference | PUF draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | 9.2 | 7.3 | 7.9 | 11.0 | 0.3 |
| 50-90% | 28.2 | 25.9 | 26.7 | 28.3 | 0.1 |
| 90-99% | 35.1 | 34.9 | 35.0 | 31.8 | 0.1 |
| 99-99.9% | 18.7 | 21.2 | 20.3 | 19.8 | 0.2 |
| Top 0.1% | 8.9 | 10.7 | 10.1 | 9.2 | 0.1 |

**All records** (top shares rank records by net worth)

| Statistic | SCF 2013 | SCF 2016 | SCF interpolated 2015 | PUF reference | PUF draw noise |
|---|---:|---:|---:|---:|---:|
| Mean net worth | $530k | $683k | $628k | $808k | $7k |
| Median net worth | $85k | $95k | $91k | $55k | $0k |
| 90th percentile | $942k | $1.17m | $1.09m | $1.30m | $6k |
| 99th percentile | $7.90m | $10.30m | $9.43m | $11.77m | $12k |
| Top 10% share (%) | 74.5 | 76.5 | 75.8 | 81.6 | 0.1 |
| Top 1% share (%) | 34.6 | 39.4 | 37.8 | 44.1 | 0.4 |
| Top 0.1% share (%) | 12.6 | 15.4 | 14.4 | 17.2 | 0.6 |
| Share with negative net worth (%) | 11.1 | 11.3 | 11.2 | 15.4 | 0.0 |

**Mean net worth by age of head** (about 42% of PUF records have no age; they are their own row)

| Age | SCF 2013 | SCF 2016 | SCF interpolated 2015 | PUF reference | PUF draw noise |
|---|---:|---:|---:|---:|---:|
| under 26 | $26k | $37k | $33k | $33k | $3k |
| 26 to 34 | $87k | $71k | $76k | $94k | $4k |
| 35 to 44 | $347k | $293k | $310k | $313k | $2k |
| 45 to 54 | $506k | $747k | $656k | $527k | $2k |
| 55 to 64 | $795k | $1.24m | $1.07m | $840k | $22k |
| 65 and over | $1.04m | $1.18m | $1.14m | $1.22m | $20k |
| age unobserved (PUF) | – | – | – | $13.56m | $13k |

#### Holding one component given another (%)

| Rate (%) | SCF 2013 | SCF 2016 | SCF interpolated 2015 | PUF reference | PUF draw noise |
|---|---:|---:|---:|---:|---:|
| Mortgage, among homeowners | 69.3 | 68.0 | 68.5 | 71.4 | 0.0 |
| Mortgage on other property, among owners of other homes | 38.3 | 40.8 | 40.0 | 41.9 | 0.0 |
| Stocks, among owners of a business (pass-through) | 34.0 | 31.3 | 32.2 | 40.0 | 0.1 |
| Stocks, among units with business income | 28.9 | 24.7 | 26.1 | 27.1 | 0.0 |
| Business (pass-through) asset, among units with business income | 52.2 | 51.4 | 51.7 | 50.7 | 0.0 |
| Unrealized gain or loss on the home, among homeowners | 95.1 | 94.9 | 95.0 | 95.5 | 0.0 |
| Unrealized gain or loss on other real estate, among its owners | 86.3 | 86.4 | 86.4 | 89.0 | 0.0 |
| Unrealized gain or loss on the business, among business owners | 90.6 | 89.6 | 89.9 | 92.3 | 0.1 |
| Unrealized gain or loss on stocks and funds, among stockholders | 91.4 | 89.0 | 89.8 | 89.2 | 0.1 |
| Other homes, among units with rental income | – | – | – | 68.9 | 0.2 |
| Home, among stockholders | 86.1 | 85.9 | 86.0 | 85.4 | 0.0 |
| Credit-card debt, among homeowners | 42.4 | 46.4 | 45.1 | 43.8 | 0.1 |

#### Summary of untargeted gaps

Same measures as for the SCF arm, against the interpolated SCF. The SCF 2013-against-2016 column is the scale from test 2: a gap is small if it is small next to how much the SCF itself moves in three years.

| Gap against SCF interpolated 2015 | PUF reference | Scale: SCF 2013 vs 2016 | PUF draw noise |
|---|---:|---:|---:|
| Share holding each component, by income group x age (30 cells x 24): average gap, points | 5.00 | 5.86 | 0.53 |
| Median amount rank among holders, by income group x age: average gap, percentiles | 5.22 | 7.02 | 0.77 |
| Pairs of components (276 two-by-two holding tables): average share misplaced, % | 5.12 | 2.15 | 0.05 |
| Same, worst pair, % | 16.35 | 7.44 | 0.13 |
| Pattern of holding across all 24 components: share of weight in the wrong pattern, % | 40.1 | 42.7 | 5.8 |
| Twelve conditional holding rates (listed below): average gap, points | 2.05 | 1.75 | 0.06 |
| Rank correlation of each amount with total income, among holders: average gap | 0.053 | 0.035 | 0.003 |
| Rank correlation between two amounts, among joint holders: average gap | 0.068 | 0.086 | 0.006 |
| Mean net worth by income group (5 groups): average gap, % | 44.5 | 28.7 | 1.1 |
| Median net worth by income group: average gap, % | 30.6 | 37.1 | 0.2 |
| Share of all net worth held by each income group: sum of absolute gaps, points | 9.4 | 8.7 | 0.8 |
| Top 1% share of net worth (by net worth): gap, points | 6.25 | 4.76 | 0.40 |
| Top 0.1% share of net worth: gap, points | 2.74 | 2.80 | 0.58 |



## Pretraining: the full model, version 1 (fallback), against ground truth

The full model (piece H) is one transformer over test 1's PUF variables and the 24 SCF wealth components, trained on PUF 2015 and SCF 2013 and 2016 records together. These sections measure how close its records are to the data before any post-training.

### Whole PUF records generated from nothing

The full model generates as many PUF records as there are PUF training records, from nothing but the PUF tag and the year 2015, each at weight (total PUF weight) / (number generated). Each file is ranked on its own AGI into the five groups. Levels first, then test 1's distance measures.

**Mean AGI, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $14k | $13k | $13k | $13k |
| 50-90% | $71k | $69k | $66k | $67k |
| 90-99% | $207k | $178k | $166k | $171k |
| 99-99.9% | $804k | $422k | $364k | $380k |
| Top 0.1% | $6.05m | $2.04m | $1.64m | $1.43m |

**Mean wages, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $13k | $14k | $13k | $14k |
| 50-90% | $55k | $59k | $56k | $58k |
| 90-99% | $146k | $151k | $134k | $147k |
| 99-99.9% | $419k | $321k | $259k | $280k |
| Top 0.1% | $1.75m | $669k | $611k | $457k |

**Mean Schedule C, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $1k | $1k | $1k | $1k |
| 50-90% | $2k | $1k | $2k | $2k |
| 90-99% | $8k | $4k | $4k | $4k |
| 99-99.9% | $30k | $8k | $7k | $9k |
| Top 0.1% | $82k | $10k | $15k | $10k |

**Mean partnership / S corp, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $-1k | $-0k | $-0k | $0k |
| 50-90% | $1k | $1k | $1k | $1k |
| 90-99% | $12k | $7k | $9k | $8k |
| 99-99.9% | $172k | $70k | $40k | $37k |
| Top 0.1% | $1.45m | $175k | $287k | $132k |

**Mean long-term gains, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $0k | $1k | $2k | $1k |
| 50-90% | $1k | $2k | $2k | $1k |
| 90-99% | $9k | $7k | $8k | $17k |
| 99-99.9% | $103k | $28k | $31k | $21k |
| Top 0.1% | $2.05m | $138k | $706k | $94k |

**Mean dividends, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $0k | $1k | $1k | $1k |
| 50-90% | $1k | $1k | $1k | $1k |
| 90-99% | $5k | $3k | $3k | $3k |
| 99-99.9% | $36k | $12k | $10k | $8k |
| Top 0.1% | $370k | $50k | $30k | $26k |

**Mean Social Security, by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | $3k | $4k | $4k | $3k |
| 50-90% | $4k | $4k | $4k | $4k |
| 90-99% | $6k | $4k | $4k | $4k |
| 99-99.9% | $6k | $5k | $4k | $3k |
| Top 0.1% | $7k | $4k | $5k | $4k |

**Share of all AGI (%), by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | 10.3 | 11.7 | 12.3 | 12.5 |
| 50-90% | 42.2 | 49.2 | 50.2 | 49.9 |
| 90-99% | 27.7 | 28.7 | 28.2 | 28.6 |
| 99-99.9% | 10.8 | 6.8 | 6.2 | 6.4 |
| Top 0.1% | 9.0 | 3.7 | 3.1 | 2.7 |

**Share with wages (%), by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | 78.3 | 79.4 | 80.2 | 82.0 |
| 50-90% | 87.1 | 90.2 | 90.7 | 91.1 |
| 90-99% | 88.5 | 93.2 | 92.3 | 95.1 |
| 99-99.9% | 85.9 | 93.8 | 91.1 | 93.5 |
| Top 0.1% | 82.7 | 88.0 | 83.1 | 84.9 |

**Share with long-term gains or losses (%), by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | 4.7 | 4.6 | 4.8 | 4.6 |
| 50-90% | 11.8 | 9.2 | 10.1 | 9.6 |
| 90-99% | 34.4 | 26.3 | 25.8 | 26.3 |
| 99-99.9% | 71.8 | 50.0 | 45.1 | 42.4 |
| Top 0.1% | 90.5 | 60.2 | 77.1 | 62.7 |

**Share with Schedule C (%), by AGI group**

| AGI group | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Bottom 50% | 16.2 | 14.7 | 14.2 | 14.2 |
| 50-90% | 15.3 | 14.0 | 14.4 | 14.8 |
| 90-99% | 21.7 | 20.1 | 19.1 | 20.2 |
| 99-99.9% | 25.7 | 22.0 | 21.7 | 21.9 |
| Top 0.1% | 25.6 | 23.5 | 27.7 | 23.5 |

| Total | PUF training records | Generated, seed 1 | Generated, seed 2 | Generated, seed 3 |
|---|---:|---:|---:|---:|
| Total weight (millions) | 120.7 | 120.7 | 120.7 | 120.7 |
| Total AGI ($ billions) | 8,094 | 6,732 | 6,375 | 6,495 |
| Median AGI | $36k | $35k | $33k | $35k |

**Test 1's measures** (TVD: total variation distance between weighted binned distributions; the floor is the training records themselves against the held-out test records, i.e. sampling noise only). Test 1's classifier uses the record weight as a variable; generated records all carry the same weight, so its AUC of 1 says only that; the last row leaves the weight out.

| Test 1 measure | Seed 1 vs training records | Seed 1 vs held-out test records | Floor: training vs test records | Test records' own value | Seed 2 vs training records | Seed 2 vs held-out test records | Floor: training vs test records | Test records' own value | Seed 3 vs training records | Seed 3 vs held-out test records | Floor: training vs test records | Test records' own value |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Single variables: median TVD across 40 | 0.010 | 0.012 | 0.005 |  | 0.010 | 0.009 | 0.005 |  | 0.014 | 0.014 | 0.005 |  |
| Two-way tables: median TVD across 780 pairs | 0.023 | 0.025 | 0.012 |  | 0.022 | 0.024 | 0.012 |  | 0.026 | 0.027 | 0.012 |  |
| Two-way tables: 90th percentile TVD | 0.054 | 0.058 | 0.022 |  | 0.053 | 0.054 | 0.022 |  | 0.055 | 0.055 | 0.022 |  |
| Three-way tables: mean TVD | 0.061 | 0.077 | 0.038 |  | 0.064 | 0.078 | 0.038 |  | 0.065 | 0.078 | 0.038 |  |
| Nonzero patterns of income items: TVD | 0.098 | 0.118 | 0.080 |  | 0.095 | 0.116 | 0.080 |  | 0.102 | 0.120 | 0.080 |  |
| Top 1% share of AGI | 0.102 | 0.102 | 0.194 | 0.195 | 0.091 | 0.091 | 0.194 | 0.195 | 0.088 | 0.088 | 0.194 | 0.195 |
| Top 0.1% share of AGI | 0.036 | 0.036 | 0.088 | 0.089 | 0.030 | 0.030 | 0.088 | 0.089 | 0.026 | 0.026 | 0.088 | 0.089 |
| Classifier AUC (0.5 = indistinguishable) | 1.000 | 1.000 | 0.505 |  | 1.000 | 1.000 | 0.505 |  | 1.000 | 1.000 | 0.505 |  |
| Classifier AUC with the weight left out (E; training records against generated) | 0.844 |  |  |  | 0.843 |  |  |  | 0.842 |  |  |  |

Test 1's full tables for seed 1 against the held-out test records: `e/puf_eval/full_gen_seed01/report_tables.md` under the scratch folder.


### SCF training records: wealth drawn by the full model against the truth

Each SCF training record with wealth observed has its 24 wealth components set aside and redrawn 5 times from its observed variables (the crosswalked PUF items, demographics, year). Income groups: test 2's seven-income total, ranked within wave over all SCF units of the split. Draw noise: draws 0 and 1 against each other (one draw per record), and the first 2 draws against the next 2.



#### Net worth by income group, in levels

Income groups rank the seven-income total within each wave over all SCF training units. Net worth is in dollars of the wave (2013 and 2016 pooled at their population weights). The draw-noise column is the absolute difference between two halves of one set of reference draws, the size of draw-to-draw noise for a column built from many draws per record.

**Mean net worth**

| Income group | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | $112k | $169k | $190k | $165k | $11k |
| 50-90% | $366k | $440k | $483k | $430k | $17k |
| 90-99% | $2.09m | $2.40m | $2.48m | $2.35m | $58k |
| 99-99.9% | $11.90m | $11.65m | $11.70m | $12.29m | $729k |
| Top 0.1% | $52.40m | $31.33m | $29.28m | $34.45m | $2.23m |

**Median net worth**

| Income group | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | $17k | $18k | $21k | $18k | $0k |
| 50-90% | $144k | $120k | $129k | $132k | $0k |
| 90-99% | $1.03m | $871k | $903k | $918k | $32k |
| 99-99.9% | $7.42m | $5.94m | $5.99m | $5.52m | $23k |
| Top 0.1% | $27.81m | $17.00m | $14.80m | $16.31m | $870k |

**Share of all net worth (%)**

| Income group | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | 8.1 | 11.1 | 11.8 | 10.8 | 0.7 |
| 50-90% | 26.9 | 29.3 | 30.5 | 28.7 | 1.1 |
| 90-99% | 35.0 | 36.4 | 35.8 | 35.7 | 1.0 |
| 99-99.9% | 20.1 | 17.8 | 17.0 | 18.9 | 1.1 |
| Top 0.1% | 9.9 | 5.4 | 4.8 | 5.9 | 0.4 |

**All records** (top shares rank records by net worth)

| Statistic | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Mean net worth | $608k | $671k | $707k | $671k | $2k |
| Median net worth | $90k | $80k | $89k | $88k | $2k |
| 90th percentile | $1.06m | $1.07m | $1.17m | $1.10m | $24k |
| 99th percentile | $9.08m | $10.70m | $10.79m | $10.27m | $139k |
| Top 10% share (%) | 75.8 | 79.3 | 78.5 | 78.0 | 0.1 |
| Top 1% share (%) | 37.6 | 39.6 | 38.6 | 39.9 | 0.4 |
| Top 0.1% share (%) | 14.4 | 13.5 | 13.7 | 14.6 | 0.0 |
| Share with negative net worth (%) | 11.2 | 11.0 | 11.0 | 11.7 | 0.3 |

**Mean net worth by age of head**

| Age | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| under 26 | $32k | $37k | $52k | $24k | $1k |
| 26 to 34 | $79k | $126k | $131k | $123k | $4k |
| 35 to 44 | $321k | $466k | $438k | $439k | $47k |
| 45 to 54 | $625k | $650k | $675k | $662k | $36k |
| 55 to 64 | $1.03m | $1.08m | $1.15m | $1.10m | $58k |
| 65 and over | $1.12m | $1.19m | $1.29m | $1.18m | $17k |

#### Holding one component given another

Share holding a nonzero amount of the first, among those holding the second (or with that income nonzero).

| Rate (%) | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Mortgage, among homeowners | 68.7 | 69.9 | 70.3 | 68.9 | 0.1 |
| Mortgage on other property, among owners of other homes | 39.6 | 38.2 | 37.9 | 38.8 | 0.2 |
| Stocks, among owners of a business (pass-through) | 32.6 | 35.6 | 36.9 | 35.9 | 0.1 |
| Stocks, among units with business income | 26.7 | 25.5 | 27.2 | 26.9 | 0.2 |
| Business (pass-through) asset, among units with business income | 52.5 | 51.3 | 48.1 | 50.1 | 0.2 |
| Unrealized gain or loss on the home, among homeowners | 95.0 | 95.4 | 95.7 | 95.7 | 0.1 |
| Unrealized gain or loss on other real estate, among its owners | 86.4 | 89.7 | 89.0 | 88.2 | 0.5 |
| Unrealized gain or loss on the business, among business owners | 90.1 | 93.2 | 92.7 | 93.6 | 0.0 |
| Unrealized gain or loss on stocks and funds, among stockholders | 90.2 | 91.8 | 92.3 | 91.9 | 0.3 |
| Other homes, among units with rental income | – | – | – | – | – |
| Home, among stockholders | 86.0 | 86.5 | 87.2 | 85.3 | 0.1 |
| Credit-card debt, among homeowners | 44.4 | 45.0 | 44.6 | 43.4 | 0.4 |

#### Rank correlation of amounts with total income, among positive holders

Weighted correlation of the amount's rank band with the record's income rank.

| Component | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| cash | 0.549 | 0.450 | 0.449 | 0.472 | 0.006 |
| equities | 0.341 | 0.298 | 0.285 | 0.284 | 0.022 |
| dc | 0.519 | 0.448 | 0.431 | 0.465 | 0.006 |
| pass_throughs | 0.426 | 0.422 | 0.370 | 0.390 | 0.002 |
| primary_home | 0.547 | 0.469 | 0.450 | 0.461 | 0.002 |
| primary_mortgage | 0.464 | 0.434 | 0.408 | 0.438 | 0.002 |
| other_home | 0.389 | 0.337 | 0.310 | 0.333 | 0.005 |
| kg_other | 0.373 | 0.309 | 0.305 | 0.307 | 0.023 |
| bonds | 0.289 | 0.255 | 0.244 | 0.237 | 0.007 |
| credit_cards | 0.315 | 0.291 | 0.263 | 0.270 | 0.002 |

#### Summary of untargeted gaps

Each row is an average absolute gap against the truth over many cells (definitions as in test 2, with five income groups). The two noise columns compare reference draws with each other: one draw against another (the noise of a file with one draw per record) and two halves of one set of reference draws against each other (the noise of a many-draw column). A reweighted column with few effective draws per record sits between the two. Gaps no larger than the noise are not evidence of a difference.

| Gap against the truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Noise: one ref draw vs another | Noise: many draws vs many |
|---|---:|---:|---:|---:|---:|
| Share holding each component, by income group x age (30 cells x 24): average gap, points | 3.87 | 3.81 | 3.92 | 2.32 | 1.39 |
| Median amount rank among holders, by income group x age: average gap, percentiles | 5.06 | 5.16 | 5.12 | 3.32 | 2.40 |
| Pairs of components (276 two-by-two holding tables): average share misplaced, % | 1.43 | 0.88 | 1.46 | 0.28 | 0.22 |
| Same, worst pair, % | 3.34 | 2.76 | 4.54 | 0.84 | 0.85 |
| Pattern of holding across all 24 components: share of weight in the wrong pattern, % | 30.5 | 30.7 | 30.3 | 20.1 | 15.7 |
| Twelve conditional holding rates (listed below): average gap, points | 1.60 | 2.00 | 1.50 | 0.53 | 0.21 |
| Rank correlation of each amount with total income, among holders: average gap | 0.058 | 0.070 | 0.062 | 0.018 | 0.014 |
| Rank correlation between two amounts, among joint holders: average gap | 0.063 | 0.071 | 0.067 | 0.035 | 0.027 |
| Mean net worth by income group (5 groups): average gap, % | 25.6 | 33.3 | 23.0 | 5.1 | 5.2 |
| Median net worth by income group: average gap, % | 20.1 | 22.7 | 19.1 | 5.2 | 2.3 |
| Share of all net worth held by each income group: sum of absolute gaps, points | 13.6 | 16.5 | 10.5 | 2.1 | 4.2 |
| Top 1% share of net worth (by net worth): gap, points | 2.03 | 0.95 | 2.28 | 0.17 | 0.44 |
| Top 0.1% share of net worth: gap, points | 0.89 | 0.73 | 0.21 | 0.09 | 0.00 |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) | 0.557 | 0.569 | 0.573 | 0.501 | – |


### SCF held-out test records: wealth drawn by the full model against the truth

Each SCF held-out test record with wealth observed has its 24 wealth components set aside and redrawn 5 times from its observed variables (the crosswalked PUF items, demographics, year). Income groups: test 2's seven-income total, ranked within wave over all SCF units of the split. Draw noise: draws 0 and 1 against each other (one draw per record), and the first 2 draws against the next 2.



#### Net worth by income group, in levels

Income groups rank the seven-income total within each wave over all SCF training units. Net worth is in dollars of the wave (2013 and 2016 pooled at their population weights). The draw-noise column is the absolute difference between two halves of one set of reference draws, the size of draw-to-draw noise for a column built from many draws per record.

**Mean net worth**

| Income group | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | $132k | $162k | $182k | $177k | $14k |
| 50-90% | $407k | $536k | $517k | $497k | $34k |
| 90-99% | $2.17m | $2.68m | $2.66m | $2.69m | $99k |
| 99-99.9% | $14.61m | $14.56m | $14.15m | $15.12m | $539k |
| Top 0.1% | $45.09m | $42.01m | $40.86m | $54.52m | $4.49m |

**Median net worth**

| Income group | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | $19k | $18k | $20k | $18k | $0k |
| 50-90% | $149k | $130k | $137k | $141k | $3k |
| 90-99% | $915k | $967k | $947k | $990k | $53k |
| 99-99.9% | $9.02m | $6.46m | $7.22m | $6.57m | $713k |
| Top 0.1% | $14.79m | $27.61m | $27.15m | $34.05m | $1.57m |

**Share of all net worth (%)**

| Income group | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Bottom 50% | 8.9 | 9.3 | 10.5 | 10.0 | 0.5 |
| 50-90% | 27.1 | 30.3 | 29.5 | 27.7 | 1.2 |
| 90-99% | 33.4 | 34.9 | 35.0 | 34.6 | 0.4 |
| 99-99.9% | 22.7 | 19.2 | 18.8 | 19.7 | 1.2 |
| Top 0.1% | 7.9 | 6.2 | 6.1 | 8.0 | 0.8 |

**All records** (top shares rank records by net worth)

| Statistic | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Mean net worth | $661k | $778k | $771k | $789k | $21k |
| Median net worth | $85k | $81k | $89k | $91k | $0k |
| 90th percentile | $1.09m | $1.18m | $1.23m | $1.17m | $19k |
| 99th percentile | $9.95m | $12.33m | $12.09m | $11.65m | $826k |
| Top 10% share (%) | 77.8 | 81.4 | 79.6 | 80.2 | 0.4 |
| Top 1% share (%) | 37.3 | 42.2 | 39.3 | 43.1 | 1.3 |
| Top 0.1% share (%) | 13.3 | 15.1 | 13.4 | 16.4 | 0.8 |
| Share with negative net worth (%) | 10.1 | 11.1 | 10.8 | 11.5 | 0.1 |

**Mean net worth by age of head**

| Age | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| under 26 | $27k | $39k | $39k | $33k | $4k |
| 26 to 34 | $77k | $113k | $104k | $107k | $6k |
| 35 to 44 | $253k | $470k | $462k | $597k | $31k |
| 45 to 54 | $598k | $706k | $686k | $764k | $11k |
| 55 to 64 | $1.20m | $1.34m | $1.41m | $1.39m | $113k |
| 65 and over | $1.36m | $1.47m | $1.42m | $1.32m | $37k |

#### Holding one component given another

Share holding a nonzero amount of the first, among those holding the second (or with that income nonzero).

| Rate (%) | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| Mortgage, among homeowners | 70.9 | 70.0 | 70.8 | 69.5 | 0.4 |
| Mortgage on other property, among owners of other homes | 41.8 | 38.9 | 38.0 | 39.1 | 2.6 |
| Stocks, among owners of a business (pass-through) | 36.9 | 38.0 | 38.1 | 38.5 | 0.3 |
| Stocks, among units with business income | 30.5 | 27.1 | 29.3 | 30.1 | 0.3 |
| Business (pass-through) asset, among units with business income | 58.2 | 52.5 | 50.4 | 52.6 | 0.6 |
| Unrealized gain or loss on the home, among homeowners | 95.2 | 95.4 | 95.8 | 95.3 | 0.0 |
| Unrealized gain or loss on other real estate, among its owners | 87.4 | 90.0 | 88.9 | 88.5 | 0.7 |
| Unrealized gain or loss on the business, among business owners | 90.8 | 92.7 | 92.7 | 93.3 | 0.5 |
| Unrealized gain or loss on stocks and funds, among stockholders | 91.7 | 91.9 | 92.1 | 92.3 | 0.9 |
| Other homes, among units with rental income | – | – | – | – | – |
| Home, among stockholders | 83.5 | 87.3 | 87.4 | 85.2 | 0.1 |
| Credit-card debt, among homeowners | 47.1 | 43.8 | 44.6 | 42.9 | 0.5 |

#### Rank correlation of amounts with total income, among positive holders

Weighted correlation of the amount's rank band with the record's income rank.

| Component | Truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Draw noise |
|---|---:|---:|---:|---:|---:|
| cash | 0.576 | 0.459 | 0.458 | 0.483 | 0.012 |
| equities | 0.428 | 0.293 | 0.284 | 0.293 | 0.034 |
| dc | 0.519 | 0.472 | 0.457 | 0.484 | 0.004 |
| pass_throughs | 0.446 | 0.401 | 0.350 | 0.389 | 0.023 |
| primary_home | 0.579 | 0.484 | 0.472 | 0.481 | 0.002 |
| primary_mortgage | 0.511 | 0.447 | 0.423 | 0.447 | 0.016 |
| other_home | 0.300 | 0.352 | 0.333 | 0.318 | 0.015 |
| kg_other | 0.398 | 0.315 | 0.292 | 0.310 | 0.042 |
| bonds | 0.351 | 0.268 | 0.264 | 0.264 | 0.026 |
| credit_cards | 0.307 | 0.297 | 0.260 | 0.283 | 0.021 |

#### Summary of untargeted gaps

Each row is an average absolute gap against the truth over many cells (definitions as in test 2, with five income groups). The two noise columns compare reference draws with each other: one draw against another (the noise of a file with one draw per record) and two halves of one set of reference draws against each other (the noise of a many-draw column). A reweighted column with few effective draws per record sits between the two. Gaps no larger than the noise are not evidence of a difference.

| Gap against the truth | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Noise: one ref draw vs another | Noise: many draws vs many |
|---|---:|---:|---:|---:|---:|
| Share holding each component, by income group x age (30 cells x 24): average gap, points | 5.60 | 5.59 | 5.78 | 3.15 | 2.34 |
| Median amount rank among holders, by income group x age: average gap, percentiles | 6.97 | 7.19 | 7.24 | 4.48 | 3.53 |
| Pairs of components (276 two-by-two holding tables): average share misplaced, % | 2.02 | 1.31 | 2.07 | 0.71 | 0.37 |
| Same, worst pair, % | 4.73 | 3.74 | 5.06 | 1.66 | 1.02 |
| Pattern of holding across all 24 components: share of weight in the wrong pattern, % | 41.4 | 41.8 | 41.4 | 28.6 | 24.0 |
| Twelve conditional holding rates (listed below): average gap, points | 2.36 | 2.25 | 1.97 | 0.85 | 0.62 |
| Rank correlation of each amount with total income, among holders: average gap | 0.063 | 0.073 | 0.068 | 0.034 | 0.021 |
| Rank correlation between two amounts, among joint holders: average gap | 0.087 | 0.089 | 0.087 | 0.050 | 0.045 |
| Mean net worth by income group (5 groups): average gap, % | 16.9 | 20.0 | 20.9 | 11.3 | 6.5 |
| Median net worth by income group: average gap, % | 28.6 | 23.7 | 35.2 | 12.0 | 5.2 |
| Share of all net worth held by each income group: sum of absolute gaps, points | 10.2 | 11.2 | 6.1 | 6.6 | 4.1 |
| Top 1% share of net worth (by net worth): gap, points | 4.92 | 2.01 | 5.76 | 2.04 | 1.29 |
| Top 0.1% share of net worth: gap, points | 1.82 | 0.06 | 3.14 | 1.99 | 0.77 |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) | 0.584 | 0.581 | 0.581 | 0.484 | – |


### PUF training records completed with wealth, against the SCF

PUF training records (2015) are completed with the 24 wealth components, presented to the model as SCF records of 2015. The PUF has no wealth, so the comparison is with the SCF 2013 and 2016 training records and their interpolation to 2015 (one third 2013, two thirds 2016; dollar levels interpolated in logs). Income groups rank the seven-income total among PUF records on the PUF side and among SCF units of each wave on the SCF side. The SCF's own change from 2013 to 2016 (which includes its sampling noise) is a scale for how large a gap is. The PUF and SCF differ in unit, sampling and income measurement, so matching the SCF's aggregates does not validate the PUF's joint distribution.

Columns: **SCF 2013**: SCF 2013 training records with wealth observed (full dataset, true wealth); **SCF 2016**: SCF 2016 training records with wealth observed (full dataset, true wealth); **SCF interpolated 2015**: one third SCF 2013 plus two thirds SCF 2016 (dollar levels in logs); **Full model, seed 1**: PUF training records completed with wealth by the full model (seed01); **Full model, seed 2**: PUF training records completed with wealth by the full model (seed02); **Full model, seed 3**: PUF training records completed with wealth by the full model (seed03).

#### Levels

##### Net worth by income group, in levels

Dollars of each year (PUF and interpolated SCF at 2015). "PUF draw noise" is the absolute difference between two halves of the PUF completion's draws.

**Mean net worth**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Bottom 50% | $109k | $115k | $113k | $143k | $181k | $176k | $4k |
| 50-90% | $339k | $390k | $372k | $555k | $652k | $633k | $18k |
| 90-99% | $1.82m | $2.34m | $2.15m | $2.97m | $3.40m | $3.30m | $25k |
| 99-99.9% | $9.78m | $13.85m | $12.33m | $16.06m | $16.55m | $18.46m | $199k |
| Top 0.1% | $41.17m | $62.94m | $54.64m | $54.30m | $51.86m | $71.54m | $53k |

**Median net worth**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Bottom 50% | $16k | $17k | $17k | $11k | $12k | $8k | $0k |
| 50-90% | $138k | $153k | $148k | $188k | $206k | $188k | $1k |
| 90-99% | $898k | $1.22m | $1.10m | $1.31m | $1.47m | $1.52m | $15k |
| 99-99.9% | $5.87m | $9.42m | $8.05m | $9.06m | $9.24m | $9.37m | $57k |
| Top 0.1% | $23.05m | $39.02m | $32.74m | $32.85m | $28.77m | $40.95m | $79k |

**Share of all net worth (%)**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Bottom 50% | 9.2 | 7.3 | 7.9 | 9.4 | 10.5 | 10.1 | 0.4 |
| 50-90% | 28.2 | 25.9 | 26.7 | 29.2 | 30.4 | 28.9 | 0.7 |
| 90-99% | 35.1 | 34.9 | 35.0 | 35.2 | 35.7 | 33.9 | 0.1 |
| 99-99.9% | 18.7 | 21.2 | 20.3 | 19.0 | 17.4 | 19.0 | 0.4 |
| Top 0.1% | 8.9 | 10.7 | 10.1 | 7.1 | 6.0 | 8.2 | 0.1 |

**All records** (top shares rank records by net worth)

| Statistic | SCF 2013 | SCF 2016 | SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Mean net worth | $530k | $683k | $628k | $760k | $858k | $876k | $5k |
| Median net worth | $85k | $95k | $91k | $71k | $81k | $64k | $0k |
| 90th percentile | $942k | $1.17m | $1.09m | $1.25m | $1.44m | $1.44m | $0k |
| 99th percentile | $7.90m | $10.30m | $9.43m | $11.88m | $13.02m | $12.86m | $157k |
| Top 10% share (%) | 74.5 | 76.5 | 75.8 | 80.0 | 79.8 | 81.6 | 0.0 |
| Top 1% share (%) | 34.6 | 39.4 | 37.8 | 39.8 | 39.2 | 43.2 | 0.3 |
| Top 0.1% share (%) | 12.6 | 15.4 | 14.4 | 13.9 | 14.5 | 17.3 | 0.2 |
| Share with negative net worth (%) | 11.1 | 11.3 | 11.2 | 12.3 | 13.2 | 14.6 | 0.0 |

**Mean net worth by age of head** (about 42% of PUF records have no age; they are their own row)

| Age | SCF 2013 | SCF 2016 | SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| under 26 | $26k | $37k | $33k | $43k | $51k | $45k | $5k |
| 26 to 34 | $87k | $71k | $76k | $121k | $128k | $110k | $5k |
| 35 to 44 | $347k | $293k | $310k | $351k | $422k | $413k | $6k |
| 45 to 54 | $506k | $747k | $656k | $569k | $645k | $641k | $2k |
| 55 to 64 | $795k | $1.24m | $1.07m | $822k | $987k | $949k | $39k |
| 65 and over | $1.04m | $1.18m | $1.14m | $1.12m | $1.35m | $1.25m | $12k |
| age unobserved (PUF) | – | – | – | $11.55m | $11.99m | $13.84m | $33k |

#### Holding one component given another (%)

| Rate (%) | SCF 2013 | SCF 2016 | SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Mortgage, among homeowners | 69.3 | 68.0 | 68.5 | 71.0 | 69.9 | 71.2 | 0.2 |
| Mortgage on other property, among owners of other homes | 38.3 | 40.8 | 40.0 | 45.2 | 43.2 | 45.1 | 0.5 |
| Stocks, among owners of a business (pass-through) | 34.0 | 31.3 | 32.2 | 34.7 | 38.9 | 45.8 | 0.2 |
| Stocks, among units with business income | 29.4 | 24.7 | 26.2 | 22.0 | 25.3 | 29.9 | 0.2 |
| Business (pass-through) asset, among units with business income | 52.7 | 52.3 | 52.4 | 56.5 | 51.8 | 52.2 | 0.4 |
| Unrealized gain or loss on the home, among homeowners | 95.1 | 94.9 | 95.0 | 95.2 | 94.9 | 94.5 | 0.0 |
| Unrealized gain or loss on other real estate, among its owners | 86.3 | 86.4 | 86.4 | 84.9 | 88.4 | 85.7 | 0.2 |
| Unrealized gain or loss on the business, among business owners | 90.6 | 89.6 | 89.9 | 94.8 | 93.5 | 93.6 | 0.1 |
| Unrealized gain or loss on stocks and funds, among stockholders | 91.4 | 89.0 | 89.8 | 91.2 | 92.3 | 90.9 | 0.0 |
| Other homes, among units with rental income | – | – | – | 47.2 | 55.1 | 49.1 | 0.5 |
| Home, among stockholders | 86.1 | 85.9 | 86.0 | 87.4 | 86.3 | 85.9 | 0.2 |
| Credit-card debt, among homeowners | 42.4 | 46.4 | 45.1 | 48.6 | 47.0 | 43.8 | 0.0 |

#### Summary of untargeted gaps

Same measures as for the SCF arm, against the interpolated SCF. The SCF 2013-against-2016 column is the scale from test 2: a gap is small if it is small next to how much the SCF itself moves in three years.

| Gap against SCF interpolated 2015 | Full model, seed 1 | Full model, seed 2 | Full model, seed 3 | Scale: SCF 2013 vs 2016 | PUF draw noise |
|---|---:|---:|---:|---:|---:|
| Share holding each component, by income group x age (30 cells x 24): average gap, points | 5.60 | 6.54 | 6.14 | 5.86 | 0.68 |
| Median amount rank among holders, by income group x age: average gap, percentiles | 5.84 | 6.53 | 6.11 | 6.97 | 1.56 |
| Pairs of components (276 two-by-two holding tables): average share misplaced, % | 4.81 | 4.70 | 6.03 | 2.15 | 0.11 |
| Same, worst pair, % | 16.21 | 16.28 | 17.79 | 7.44 | 0.33 |
| Pattern of holding across all 24 components: share of weight in the wrong pattern, % | 40.8 | 39.7 | 42.7 | 42.7 | 13.2 |
| Twelve conditional holding rates (listed below): average gap, points | 2.86 | 2.12 | 2.96 | 1.77 | 0.21 |
| Rank correlation of each amount with total income, among holders: average gap | 0.046 | 0.038 | 0.051 | 0.035 | 0.006 |
| Rank correlation between two amounts, among joint holders: average gap | 0.062 | 0.066 | 0.068 | 0.086 | 0.014 |
| Mean net worth by income group (5 groups): average gap, % | 28.9 | 46.4 | 52.0 | 28.7 | 1.7 |
| Median net worth by income group: average gap, % | 19.2 | 25.7 | 31.6 | 37.1 | 0.8 |
| Share of all net worth held by each income group: sum of absolute gaps, points | 8.5 | 14.0 | 8.8 | 8.7 | 1.6 |
| Top 1% share of net worth (by net worth): gap, points | 1.95 | 1.39 | 5.40 | 4.76 | 0.29 |
| Top 0.1% share of net worth: gap, points | 0.50 | 0.13 | 2.91 | 2.80 | 0.20 |


## Post-training on the full model (full): arm P (PUF records completed with wealth)

PUF training records (2015) are completed with the 24 wealth components, presented to the model as SCF records of 2015. The PUF has no wealth, so the comparison is with the SCF 2013 and 2016 training records and their interpolation to 2015 (one third 2013, two thirds 2016; dollar levels interpolated in logs). Income groups rank the seven-income total among PUF records on the PUF side and among SCF units of each wave on the SCF side. The SCF's own change from 2013 to 2016 (which includes its sampling noise) is a scale for how large a gap is. The PUF and SCF differ in unit, sampling and income measurement, so matching the SCF's aggregates does not validate the PUF's joint distribution.

Columns: **SCF 2013**: SCF 2013 training records with wealth observed (full dataset, true wealth); **SCF 2016**: SCF 2016 training records with wealth observed (full dataset, true wealth); **SCF interpolated 2015**: one third SCF 2013 plus two thirds SCF 2016 (dollar levels in logs); **PUF reference**: reference full model completing PUF records, unweighted; **M2 tilt P core, before**: method 2 (P_core/draws.npz), before residual calibration; **M2 tilt P full, before**: method 2 (P_full/draws.npz), before residual calibration.

### Q1. How far each method moved from the reference model

KL is the Kullback–Leibler divergence of the final distribution of wealth from the reference model's, per unit of population weight, in nats; zero means no move. For intuition, a KL of log(2) = 0.69 is what one gets by putting each record's weight evenly on half of its draws and none on the rest; the column "as if spread over" turns each KL into that share (e^−KL). Effective draws per record is 1 / Σ q², where q are the shares of a record's weight on its draws (K when all draws count equally). For methods 2 and 3 the KL has two parts: the changed model (estimated as the average over draws of the log-probability under the new model minus that under the reference) and the residual calibration's weights on the new model's draws.

| Method | K | KL, changed model | KL, weights | KL, total | As if spread over | Effective draws | Records with < 4 effective draws | Largest share on one draw |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| PUF reference | 32 | – | 0.000 | 0.000 | 100% | 32.0 | 0.0% | 3% |
| M2 tilt P core, before | 32 | 0.673 | 0.000 | 0.673 | 51% | 32.0 | 0.0% | 3% |
| M2 tilt P full, before | 32 | 1.563 | 0.000 | 1.563 | 21% | 32.0 | 0.0% | 3% |

Method 1 with different numbers of reference draws per record (piece B's reports; "infeasible" cells are those B's solver set aside because no weighting of the available draws can hit them, with their share of all target weight):

| Run | K | KL | As if spread over | Effective draws | Records with < 4 effective | Infeasible cells | Their target weight | Largest relative error, feasible cells |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| ../full/b/P_core/report_P.json | 32 | 1.213 | 30% | 9.6 | 28.7% | 5092 | 99.870% | 2.5e-07 |

_Cross-check: KL as computed here from the weights and log-probabilities in each file; the pieces' own reports should agree._

### Q2. Target fit

Error in each target cell's weight as a share of its group's PUF weight, averaged over all cells of the target set, worst cell in brackets; "unreached": cells with a positive target that no weighted draw falls in. Core sets (core, wcore): wealth by income group, age, married, dependents. Full sets (full, wfull): also wealth by each income item's band.

| Column | wcore: mean (max) | wcore unreached | wfull: mean (max) | wfull unreached |
|---|---:|---:|---:|---:|
| PUF reference | 0.853% (24.2%) | 8 | 0.997% (24.2%) | 23 |
| M2 tilt P core, before | 0.078% (2.0%) | 10 | 0.603% (27.8%) | 25 |
| M2 tilt P full, before | 0.105% (3.0%) | 12 | 0.134% (5.7%) | 29 |

### Q3. Untargeted relationships

#### Net worth by income group, in levels

Dollars of each year (PUF and interpolated SCF at 2015). "PUF draw noise" is the absolute difference between two halves of the PUF completion's draws.

**Mean net worth**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 | PUF reference | M2 tilt P core, before | M2 tilt P full, before | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Bottom 50% | $109k | $115k | $113k | $147k | $164k | $191k | $0k |
| 50-90% | $339k | $390k | $372k | $556k | $471k | $538k | $3k |
| 90-99% | $1.82m | $2.34m | $2.15m | $2.98m | $2.39m | $2.60m | $5k |
| 99-99.9% | $9.78m | $13.85m | $12.33m | $16.10m | $11.57m | $13.07m | $38k |
| Top 0.1% | $41.17m | $62.94m | $54.64m | $54.47m | $40.40m | $44.26m | $41k |

**Median net worth**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 | PUF reference | M2 tilt P core, before | M2 tilt P full, before | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Bottom 50% | $16k | $17k | $17k | $11k | $18k | $19k | $0k |
| 50-90% | $138k | $153k | $148k | $187k | $158k | $162k | $0k |
| 90-99% | $898k | $1.22m | $1.10m | $1.32m | $1.08m | $986k | $4k |
| 99-99.9% | $5.87m | $9.42m | $8.05m | $9.09m | $5.94m | $6.41m | $10k |
| Top 0.1% | $23.05m | $39.02m | $32.74m | $32.98m | $21.23m | $24.51m | $55k |

**Share of all net worth (%)**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 | PUF reference | M2 tilt P core, before | M2 tilt P full, before | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Bottom 50% | 9.2 | 7.3 | 7.9 | 9.6 | 13.0 | 13.5 | 0.0 |
| 50-90% | 28.2 | 25.9 | 26.7 | 29.1 | 29.9 | 30.4 | 0.1 |
| 90-99% | 35.1 | 34.9 | 35.0 | 35.1 | 34.1 | 33.1 | 0.1 |
| 99-99.9% | 18.7 | 21.2 | 20.3 | 19.0 | 16.5 | 16.6 | 0.1 |
| Top 0.1% | 8.9 | 10.7 | 10.1 | 7.1 | 6.4 | 6.3 | 0.0 |

**All records** (top shares rank records by net worth)

| Statistic | SCF 2013 | SCF 2016 | SCF interpolated 2015 | PUF reference | M2 tilt P core, before | M2 tilt P full, before | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Mean net worth | $530k | $683k | $628k | $763k | $630k | $707k | $1k |
| Median net worth | $85k | $95k | $91k | $72k | $83k | $89k | $1k |
| 90th percentile | $942k | $1.17m | $1.09m | $1.26m | $1.09m | $1.11m | $0k |
| 99th percentile | $7.90m | $10.30m | $9.43m | $11.81m | $9.33m | $11.37m | $46k |
| Top 10% share (%) | 74.5 | 76.5 | 75.8 | 80.0 | 77.2 | 79.1 | 0.0 |
| Top 1% share (%) | 34.6 | 39.4 | 37.8 | 40.0 | 38.2 | 40.3 | 0.1 |
| Top 0.1% share (%) | 12.6 | 15.4 | 14.4 | 14.4 | 14.0 | 14.3 | 0.1 |
| Share with negative net worth (%) | 11.1 | 11.3 | 11.2 | 12.3 | 11.8 | 11.9 | 0.0 |

**Mean net worth by age of head** (about 42% of PUF records have no age; they are their own row)

| Age | SCF 2013 | SCF 2016 | SCF interpolated 2015 | PUF reference | M2 tilt P core, before | M2 tilt P full, before | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| under 26 | $26k | $37k | $33k | $49k | $46k | $68k | $1k |
| 26 to 34 | $87k | $71k | $76k | $121k | $96k | $142k | $1k |
| 35 to 44 | $347k | $293k | $310k | $354k | $277k | $295k | $2k |
| 45 to 54 | $506k | $747k | $656k | $576k | $515k | $546k | $5k |
| 55 to 64 | $795k | $1.24m | $1.07m | $822k | $864k | $909k | $4k |
| 65 and over | $1.04m | $1.18m | $1.14m | $1.11m | $983k | $1.06m | $0k |
| age unobserved (PUF) | – | – | – | $11.61m | $7.89m | $9.43m | $5k |

#### Holding one component given another (%)

| Rate (%) | SCF 2013 | SCF 2016 | SCF interpolated 2015 | PUF reference | M2 tilt P core, before | M2 tilt P full, before | PUF draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Mortgage, among homeowners | 69.3 | 68.0 | 68.5 | 71.0 | 67.8 | 68.0 | 0.1 |
| Mortgage on other property, among owners of other homes | 38.3 | 40.8 | 40.0 | 45.4 | 35.5 | 35.4 | 0.2 |
| Stocks, among owners of a business (pass-through) | 34.0 | 31.3 | 32.2 | 34.8 | 33.7 | 40.9 | 0.0 |
| Stocks, among units with business income | 29.4 | 24.7 | 26.2 | 22.0 | 20.7 | 27.7 | 0.0 |
| Business (pass-through) asset, among units with business income | 52.7 | 52.3 | 52.4 | 56.5 | 44.0 | 52.6 | 0.1 |
| Unrealized gain or loss on the home, among homeowners | 95.1 | 94.9 | 95.0 | 95.2 | 95.0 | 95.0 | 0.0 |
| Unrealized gain or loss on other real estate, among its owners | 86.3 | 86.4 | 86.4 | 84.9 | 82.1 | 82.6 | 0.1 |
| Unrealized gain or loss on the business, among business owners | 90.6 | 89.6 | 89.9 | 94.8 | 90.8 | 91.1 | 0.0 |
| Unrealized gain or loss on stocks and funds, among stockholders | 91.4 | 89.0 | 89.8 | 91.1 | 89.5 | 90.1 | 0.1 |
| Other homes, among units with rental income | – | – | – | 47.1 | 33.8 | 36.3 | 0.2 |
| Home, among stockholders | 86.1 | 85.9 | 86.0 | 87.6 | 88.3 | 88.8 | 0.0 |
| Credit-card debt, among homeowners | 42.4 | 46.4 | 45.1 | 48.6 | 45.2 | 44.8 | 0.0 |

#### Summary of untargeted gaps

Same measures as for the SCF arm, against the interpolated SCF. The SCF 2013-against-2016 column is the scale from test 2: a gap is small if it is small next to how much the SCF itself moves in three years.

| Gap against SCF interpolated 2015 | PUF reference | M2 tilt P core, before | M2 tilt P full, before | Scale: SCF 2013 vs 2016 | PUF draw noise |
|---|---:|---:|---:|---:|---:|
| Share holding each component, by income group x age (30 cells x 24): average gap, points | 5.19 | 3.78 | 5.81 | 5.86 | 0.50 |
| Median amount rank among holders, by income group x age: average gap, percentiles | 5.49 | 4.44 | 6.56 | 6.97 | 0.81 |
| Pairs of components (276 two-by-two holding tables): average share misplaced, % | 4.80 | 0.86 | 0.62 | 2.15 | 0.05 |
| Same, worst pair, % | 16.18 | 2.70 | 2.36 | 7.44 | 0.14 |
| Pattern of holding across all 24 components: share of weight in the wrong pattern, % | 40.8 | 32.2 | 31.7 | 42.7 | 6.1 |
| Twelve conditional holding rates (listed below): average gap, points | 2.90 | 2.59 | 2.15 | 1.77 | 0.08 |
| Rank correlation of each amount with total income, among holders: average gap | 0.045 | 0.041 | 0.060 | 0.035 | 0.002 |
| Rank correlation between two amounts, among joint holders: average gap | 0.061 | 0.058 | 0.056 | 0.086 | 0.005 |
| Mean net worth by income group (5 groups): average gap, % | 29.7 | 22.9 | 31.9 | 28.7 | 0.3 |
| Median net worth by income group: average gap, % | 19.4 | 15.9 | 15.1 | 37.1 | 0.2 |
| Share of all net worth held by each income group: sum of absolute gaps, points | 8.6 | 16.6 | 18.8 | 8.7 | 0.3 |
| Top 1% share of net worth (by net worth): gap, points | 2.21 | 0.34 | 2.53 | 4.76 | 0.06 |
| Top 0.1% share of net worth: gap, points | 0.00 | 0.40 | 0.12 | 2.80 | 0.10 |



## Appendix: pilot on the current model

Everything below uses test 2's model (10 PUF-side variables), which the full model replaces. It checked the evaluation code and the post-training methods; it is not the main result.

_Generated by `e_compare.py` at 2026-10-09 22:49 from 7 evaluations: Truth, Truth, tokens decoded, Reference, M1 reweight, M2 tilt, before, M2 tilt, after._

### Arm P: completing PUF records (no true wealth; compared with the SCF)

PUF training records (2015) are completed with the 24 wealth components, presented to the model as SCF records of 2015. The PUF has no wealth, so the comparison is with the SCF 2013 and 2016 training records and their interpolation to 2015 (one third 2013, two thirds 2016; dollar levels interpolated in logs). Income groups rank the seven-income total among PUF records on the PUF side and among SCF units of each wave on the SCF side. The SCF's own change from 2013 to 2016 (which includes its sampling noise) is a scale for how large a gap is. The PUF and SCF differ in unit, sampling and income measurement, so matching the SCF's aggregates does not validate the PUF's joint distribution.

Columns: **SCF 2013**: SCF 2013 training records with wealth observed (true wealth); **SCF 2016**: SCF 2016 training records with wealth observed (true wealth); **SCF interpolated 2015**: one third SCF 2013 plus two thirds SCF 2016 (dollar levels in logs).

#### P3. Untargeted relationships

##### Net worth by income group, in levels

Dollars of each year (PUF and interpolated SCF at 2015). "PUF draw noise" is the absolute difference between two halves of the PUF completion's draws.

**Mean net worth**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 |
|---|---:|---:|---:|
| Bottom 50% | $109k | $115k | $113k |
| 50-90% | $339k | $390k | $372k |
| 90-99% | $1.82m | $2.34m | $2.15m |
| 99-99.9% | $9.78m | $13.85m | $12.33m |
| Top 0.1% | $41.17m | $62.94m | $54.64m |

**Median net worth**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 |
|---|---:|---:|---:|
| Bottom 50% | $16k | $17k | $17k |
| 50-90% | $138k | $153k | $148k |
| 90-99% | $898k | $1.22m | $1.10m |
| 99-99.9% | $5.87m | $9.42m | $8.05m |
| Top 0.1% | $23.05m | $39.02m | $32.74m |

**Share of all net worth (%)**

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 |
|---|---:|---:|---:|
| Bottom 50% | 9.2 | 7.3 | 7.9 |
| 50-90% | 28.2 | 25.9 | 26.7 |
| 90-99% | 35.1 | 34.9 | 35.0 |
| 99-99.9% | 18.7 | 21.2 | 20.3 |
| Top 0.1% | 8.9 | 10.7 | 10.1 |

**All records** (top shares rank records by net worth)

| Statistic | SCF 2013 | SCF 2016 | SCF interpolated 2015 |
|---|---:|---:|---:|
| Mean net worth | $530k | $683k | $628k |
| Median net worth | $85k | $95k | $91k |
| 90th percentile | $942k | $1.17m | $1.09m |
| 99th percentile | $7.90m | $10.30m | $9.43m |
| Top 10% share (%) | 74.5 | 76.5 | 75.8 |
| Top 1% share (%) | 34.6 | 39.4 | 37.8 |
| Top 0.1% share (%) | 12.6 | 15.4 | 14.4 |
| Share with negative net worth (%) | 11.1 | 11.3 | 11.2 |

**Mean net worth by age of head** (about 42% of PUF records have no age; they are their own row)

| Age | SCF 2013 | SCF 2016 | SCF interpolated 2015 |
|---|---:|---:|---:|
| under 26 | $26k | $37k | $33k |
| 26 to 34 | $87k | $71k | $76k |
| 35 to 44 | $347k | $293k | $310k |
| 45 to 54 | $506k | $747k | $656k |
| 55 to 64 | $795k | $1.24m | $1.07m |
| 65 and over | $1.04m | $1.18m | $1.14m |

##### Holding one component given another (%)

| Rate (%) | SCF 2013 | SCF 2016 | SCF interpolated 2015 |
|---|---:|---:|---:|
| Mortgage, among homeowners | 69.3 | 68.0 | 68.5 |
| Mortgage on other property, among owners of other homes | 38.3 | 40.8 | 40.0 |
| Stocks, among owners of a business (pass-through) | 34.0 | 31.3 | 32.2 |
| Stocks, among units with business income | 29.4 | 24.7 | 26.2 |
| Business (pass-through) asset, among units with business income | 52.7 | 52.3 | 52.4 |
| Unrealized gain or loss on the home, among homeowners | 95.1 | 94.9 | 95.0 |
| Unrealized gain or loss on other real estate, among its owners | 86.3 | 86.4 | 86.4 |
| Unrealized gain or loss on the business, among business owners | 90.6 | 89.6 | 89.9 |
| Unrealized gain or loss on stocks and funds, among stockholders | 91.4 | 89.0 | 89.8 |
| Other homes, among units with rental income | 52.3 | 48.0 | 49.4 |
| Home, among stockholders | 86.1 | 85.9 | 86.0 |
| Credit-card debt, among homeowners | 42.4 | 46.4 | 45.1 |

##### Summary of untargeted gaps

Same measures as for the SCF arm, against the interpolated SCF. The SCF 2013-against-2016 column is the scale from test 2: a gap is small if it is small next to how much the SCF itself moves in three years.

| Gap against SCF interpolated 2015 | Scale: SCF 2013 vs 2016 |
|---|---:|
| Share holding each component, by income group x age (30 cells x 24): average gap, points | 5.86 |
| Median amount rank among holders, by income group x age: average gap, percentiles | 7.01 |
| Pairs of components (276 two-by-two holding tables): average share misplaced, % | 2.15 |
| Same, worst pair, % | 7.44 |
| Pattern of holding across all 24 components: share of weight in the wrong pattern, % | 42.7 |
| Twelve conditional holding rates (listed below): average gap, points | 1.98 |
| Rank correlation of each amount with total income, among holders: average gap | 0.035 |
| Rank correlation between two amounts, among joint holders: average gap | 0.086 |
| Mean net worth by income group (5 groups): average gap, % | 28.7 |
| Median net worth by income group: average gap, % | 37.1 |
| Share of all net worth held by each income group: sum of absolute gaps, points | 8.7 |
| Top 1% share of net worth (by net worth): gap, points | 4.76 |
| Top 0.1% share of net worth: gap, points | 2.80 |


### Arm S: completing SCF records (true wealth known)

The SCF training records' own wealth is known, so here every statistic is compared with the truth for the same records.

Columns:

- **Truth**: truth.
- **Truth, tokens decoded**: truth_decoded.
- **Reference**: reference model, unweighted (`a/ref_draws.npz`).
- **M1 reweight**: method 1: reference draws reweighted (`a/ref_draws.npz`; weights `b/weights_ref.npz`).
- **M2 tilt, before**: method 2: tilted model, before residual calibration (`c/draws.npz`).
- **M2 tilt, after**: method 2 after residual calibration (`c/draws.npz`; weights `c/weights_resid.npz`).

#### 1. How far each method moved from the reference model

KL is the Kullback–Leibler divergence of the final distribution of wealth from the reference model's, per unit of population weight, in nats; zero means no move. For intuition, a KL of log(2) = 0.69 is what one gets by putting each record's weight evenly on half of its draws and none on the rest; the column "as if spread over" turns each KL into that share (e^−KL). Effective draws per record is 1 / Σ q², where q are the shares of a record's weight on its draws (K when all draws count equally). For methods 2 and 3 the KL has two parts: the changed model (estimated as the average over draws of the log-probability under the new model minus that under the reference) and the residual calibration's weights on the new model's draws.

| Method | K | KL, changed model | KL, weights | KL, total | As if spread over | Effective draws | Records with < 4 effective draws | Largest share on one draw |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Reference | 32 | – | 0.000 | 0.000 | 100% | 32.0 | 0.0% | 3% |
| M1 reweight | 32 | – | 1.927 | 1.927 | 15% | 5.2 | 57.8% | 100% |
| M2 tilt, before | 32 | 1.313 | 0.000 | 1.313 | 27% | 32.0 | 0.0% | 3% |
| M2 tilt, after | 32 | 1.312 | 0.009 | 1.320 | 27% | 31.5 | 0.0% | 76% |

Method 1 with different numbers of reference draws per record (piece B's reports; "infeasible" cells are those B's solver set aside because no weighting of the available draws can hit them, with their share of all target weight):

| Run | K | KL | As if spread over | Effective draws | Records with < 4 effective | Infeasible cells | Their target weight | Largest relative error, feasible cells |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| report_ref.json | 32 | 1.927 | 15% | 5.2 | 57.8% | 27584 | 99.931% | 9.0e-07 |
| report_ref_K16.json | 16 | 1.914 | 15% | 2.5 | 84.5% | 27609 | 99.924% | 9.4e-07 |
| report_ref_K64.json | 64 | 1.846 | 16% | 11.7 | 37.3% | 27554 | 99.369% | 9.8e-07 |
| report_ref_K8.json | 8 | 1.730 | 18% | 1.4 | 98.6% | 27687 | 99.943% | 1.9e-07 |

Infeasible cells in the calibrations evaluated here: M1 reweight: 27584 cells (99.931% of target weight, age 3337, x_rent 2910, x_business 2903, x_capital_gains 2832, inc 2784, x_wages 2463, dep 2436, x_ss_pens 2432, x_int_div 2326, x_ui_other 1898, mar 1263).

_Cross-check: KL as computed here from the weights and log-probabilities in each file; the pieces' own reports should agree._

#### 2. Target fit

Error in each target cell's weight as a share of its group's total weight (the group total is the weight of the records in that wave and group), averaged over the cells of each family, with the worst cell in brackets. A target cell is a (wave, family, group, wealth component, coarse bin) total; families are defined in `SPEC.md`. "Unreached" counts cells with a positive true total that no draw with positive weight falls in (infeasible for that set of draws).

| Family | Reference | M1 reweight | M2 tilt, before | M2 tilt, after | Draw noise |
|---|---:|---:|---:|---:|---:|
| inc | 1.014% (19.2%) | 0.429% (22.7%) | 0.069% (1.4%) | 0.000% (0.0%) | 0.106% (2.2%) |
| age | 0.475% (8.0%) | 0.014% (1.6%) | 0.033% (0.4%) | 0.000% (0.0%) | 0.048% (0.6%) |
| mar | 0.289% (4.5%) | 0.005% (0.1%) | 0.020% (0.2%) | 0.000% (0.0%) | 0.029% (0.4%) |
| dep | 0.471% (7.7%) | 0.014% (0.6%) | 0.034% (0.4%) | 0.000% (0.0%) | 0.050% (1.0%) |
| x_wages | 0.416% (8.4%) | 0.015% (0.6%) | 0.032% (0.5%) | 0.000% (0.0%) | 0.046% (0.5%) |
| x_business | 1.175% (19.2%) | 0.323% (17.0%) | 0.086% (1.3%) | 0.000% (0.1%) | 0.128% (1.6%) |
| x_int_div | 0.660% (11.2%) | 0.070% (4.8%) | 0.050% (0.6%) | 0.000% (0.0%) | 0.072% (0.8%) |
| x_capital_gains | 1.221% (19.6%) | 0.445% (24.3%) | 0.092% (1.5%) | 0.000% (0.1%) | 0.146% (1.9%) |
| x_rent | 1.261% (17.4%) | 0.388% (16.2%) | 0.086% (1.7%) | 0.000% (0.0%) | 0.125% (1.6%) |
| x_ss_pens | 0.646% (11.8%) | 0.045% (3.0%) | 0.047% (0.9%) | 0.000% (0.0%) | 0.069% (1.1%) |
| x_ui_other | 0.933% (26.1%) | 0.205% (12.5%) | 0.065% (1.0%) | 0.000% (0.0%) | 0.095% (2.8%) |
| all | 0.818% (26.1%) | 0.196% (24.3%) | 0.059% (1.7%) | 0.000% (0.1%) | 0.088% (2.8%) |
| Weight in cells whose true total is zero (% of all) | 0.044% | 0.000% | 0.000% | 0.000% | – |
| Unreached cells (positive truth, no weighted draw) | 10 | 65 | 62 | 62 | – |
| Share of all weight misplaced across cells, % | 2.25 | 0.13 | 0.17 | 0.00 | – |

Unreached cells by family: Reference: inc 2, x_business 1, x_capital_gains 2, x_int_div 1, x_rent 2, x_ss_pens 1, x_wages 1; M1 reweight: age 5, dep 2, inc 8, mar 2, x_business 10, x_capital_gains 8, x_int_div 5, x_rent 6, x_ss_pens 4, x_ui_other 12, x_wages 3; M2 tilt, before: age 2, dep 1, inc 13, x_business 11, x_capital_gains 17, x_int_div 5, x_rent 9, x_ss_pens 1, x_wages 3; M2 tilt, after: age 2, dep 1, inc 13, x_business 11, x_capital_gains 17, x_int_div 5, x_rent 9, x_ss_pens 1, x_wages 3.

#### 3. Untargeted relationships

##### Net worth by income group, in levels

Income groups rank the seven-income total within each wave over all SCF training units. Net worth is in dollars of the wave (2013 and 2016 pooled at their population weights). "Truth, tokens decoded" puts the true records' own wealth tokens through the same decoder as the draws: the difference from the truth is decoding noise, not model error. The draw-noise column is the absolute difference between two independent sets of 32 reference draws (section 4), the size of draw-to-draw noise for a column built from many draws per record.

**Mean net worth**

| Income group | Truth | Truth, tokens decoded | Reference | M1 reweight | M2 tilt, before | M2 tilt, after | Draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Bottom 50% | $112k | $112k | $163k | $115k | $115k | $115k | $5k |
| 50-90% | $366k | $366k | $446k | $388k | $379k | $379k | $2k |
| 90-99% | $2.09m | $2.09m | $2.29m | $2.22m | $2.17m | $2.15m | $10k |
| 99-99.9% | $11.90m | $12.03m | $11.23m | $11.95m | $12.15m | $12.10m | $218k |
| Top 0.1% | $52.40m | $51.59m | $30.28m | $32.54m | $46.97m | $46.99m | $854k |

**Median net worth**

| Income group | Truth | Truth, tokens decoded | Reference | M1 reweight | M2 tilt, before | M2 tilt, after | Draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Bottom 50% | $17k | $17k | $20k | $20k | $19k | $19k | $0k |
| 50-90% | $144k | $145k | $142k | $152k | $147k | $147k | $0k |
| 90-99% | $1.03m | $1.02m | $942k | $988k | $973k | $973k | $1k |
| 99-99.9% | $7.42m | $7.36m | $6.23m | $6.51m | $7.06m | $7.02m | $50k |
| Top 0.1% | $27.81m | $28.37m | $17.18m | $18.10m | $26.98m | $26.88m | $194k |

**Share of all net worth (%)**

| Income group | Truth | Truth, tokens decoded | Reference | M1 reweight | M2 tilt, before | M2 tilt, after | Draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Bottom 50% | 8.1 | 8.1 | 10.9 | 8.3 | 8.2 | 8.2 | 0.3 |
| 50-90% | 26.9 | 26.8 | 30.4 | 28.4 | 27.3 | 27.4 | 0.0 |
| 90-99% | 35.0 | 35.0 | 35.7 | 37.0 | 35.6 | 35.5 | 0.0 |
| 99-99.9% | 20.1 | 20.3 | 17.6 | 20.1 | 20.2 | 20.1 | 0.4 |
| Top 0.1% | 9.9 | 9.8 | 5.3 | 6.1 | 8.7 | 8.8 | 0.1 |

**All records** (top shares rank records by net worth)

| Statistic | Truth | Truth, tokens decoded | Reference | M1 reweight | M2 tilt, before | M2 tilt, after | Draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Mean net worth | $608k | $609k | $655k | $610k | $620k | $618k | $3k |
| Median net worth | $90k | $91k | $95k | $93k | $92k | $92k | $0k |
| 90th percentile | $1.06m | $1.06m | $1.13m | $1.05m | $1.05m | $1.05m | $3k |
| 99th percentile | $9.08m | $9.15m | $9.72m | $9.16m | $9.32m | $9.27m | $45k |
| Top 10% share (%) | 75.8 | 75.8 | 76.3 | 75.7 | 76.2 | 76.2 | 0.3 |
| Top 1% share (%) | 37.6 | 37.7 | 36.9 | 36.6 | 37.7 | 37.6 | 0.4 |
| Top 0.1% share (%) | 14.4 | 14.4 | 13.3 | 13.0 | 13.7 | 13.7 | 0.3 |
| Share with negative net worth (%) | 11.2 | 11.4 | 11.4 | 11.3 | 11.3 | 11.4 | 0.0 |

**Mean net worth by age of head**

| Age | Truth | Truth, tokens decoded | Reference | M1 reweight | M2 tilt, before | M2 tilt, after | Draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| under 26 | $32k | $32k | $43k | $27k | $36k | $35k | $2k |
| 26 to 34 | $79k | $77k | $140k | $85k | $81k | $79k | $6k |
| 35 to 44 | $321k | $321k | $435k | $364k | $322k | $322k | $9k |
| 45 to 54 | $625k | $626k | $667k | $615k | $635k | $635k | $1k |
| 55 to 64 | $1.03m | $1.03m | $1.07m | $1.04m | $1.05m | $1.05m | $8k |
| 65 and over | $1.12m | $1.12m | $1.12m | $1.09m | $1.14m | $1.14m | $18k |

##### Holding one component given another

Share holding a nonzero amount of the first, among those holding the second (or with that income nonzero).

| Rate (%) | Truth | Truth, tokens decoded | Reference | M1 reweight | M2 tilt, before | M2 tilt, after | Draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| Mortgage, among homeowners | 68.7 | 68.7 | 69.8 | 68.6 | 68.6 | 68.6 | 0.2 |
| Mortgage on other property, among owners of other homes | 39.6 | 39.6 | 38.3 | 38.8 | 39.1 | 39.0 | 0.1 |
| Stocks, among owners of a business (pass-through) | 32.6 | 32.6 | 36.7 | 31.3 | 33.1 | 33.1 | 0.2 |
| Stocks, among units with business income | 26.7 | 26.7 | 27.4 | 26.7 | 26.6 | 26.7 | 0.0 |
| Business (pass-through) asset, among units with business income | 52.5 | 52.5 | 45.1 | 52.4 | 52.3 | 52.5 | 0.2 |
| Unrealized gain or loss on the home, among homeowners | 95.0 | 95.0 | 95.4 | 95.0 | 95.0 | 94.9 | 0.0 |
| Unrealized gain or loss on other real estate, among its owners | 86.4 | 86.4 | 87.1 | 84.9 | 86.3 | 86.6 | 0.2 |
| Unrealized gain or loss on the business, among business owners | 90.1 | 90.1 | 90.6 | 89.4 | 89.8 | 89.7 | 0.1 |
| Unrealized gain or loss on stocks and funds, among stockholders | 90.2 | 90.2 | 90.6 | 89.7 | 89.9 | 89.7 | 0.0 |
| Other homes, among units with rental income | 50.0 | 50.0 | 43.5 | 49.9 | 50.2 | 50.0 | 0.1 |
| Home, among stockholders | 86.0 | 86.0 | 87.4 | 85.3 | 86.3 | 86.4 | 0.1 |
| Credit-card debt, among homeowners | 44.4 | 44.4 | 45.2 | 44.5 | 45.0 | 45.0 | 0.0 |

##### Rank correlation of amounts with total income, among positive holders

Weighted correlation of the amount's rank band with the record's income rank.

| Component | Truth | Truth, tokens decoded | Reference | M1 reweight | M2 tilt, before | M2 tilt, after | Draw noise |
|---|---:|---:|---:|---:|---:|---:|---:|
| cash | 0.549 | 0.549 | 0.457 | 0.522 | 0.526 | 0.526 | 0.002 |
| equities | 0.341 | 0.341 | 0.271 | 0.331 | 0.334 | 0.333 | 0.002 |
| dc | 0.518 | 0.518 | 0.458 | 0.503 | 0.504 | 0.506 | 0.002 |
| pass_throughs | 0.426 | 0.426 | 0.414 | 0.437 | 0.436 | 0.436 | 0.005 |
| primary_home | 0.547 | 0.547 | 0.489 | 0.543 | 0.543 | 0.543 | 0.001 |
| primary_mortgage | 0.465 | 0.465 | 0.443 | 0.455 | 0.455 | 0.454 | 0.001 |
| other_home | 0.388 | 0.388 | 0.325 | 0.392 | 0.381 | 0.383 | 0.002 |
| kg_other | 0.373 | 0.373 | 0.286 | 0.363 | 0.370 | 0.368 | 0.000 |
| bonds | 0.289 | 0.289 | 0.239 | 0.288 | 0.291 | 0.290 | 0.002 |
| credit_cards | 0.315 | 0.315 | 0.289 | 0.314 | 0.309 | 0.307 | 0.000 |

##### Summary of untargeted gaps

Each row is an average absolute gap against the truth over many cells (definitions as in test 2, with five income groups). The two noise columns compare reference draws with each other: one draw against another (the noise of a file with one draw per record) and two independent sets of 32 reference draws against each other (the noise of a many-draw column). A reweighted column with few effective draws per record sits between the two. Gaps no larger than the noise are not evidence of a difference.

| Gap against the truth | Truth, tokens decoded | Reference | M1 reweight | M2 tilt, before | M2 tilt, after | Noise: one ref draw vs another | Noise: many draws vs many |
|---|---:|---:|---:|---:|---:|---:|---:|
| Share holding each component, by income group x age (30 cells x 24): average gap, points | 0.00 | 3.70 | 2.92 | 2.64 | 2.65 | 2.07 | 0.50 |
| Median amount rank among holders, by income group x age: average gap, percentiles | 0.00 | 5.14 | 3.92 | 3.20 | 3.21 | 3.47 | 0.95 |
| Pairs of components (276 two-by-two holding tables): average share misplaced, % | 0.00 | 1.28 | 0.32 | 0.26 | 0.26 | 0.40 | 0.06 |
| Same, worst pair, % | 0.00 | 3.48 | 1.78 | 2.33 | 2.34 | 0.96 | 0.21 |
| Pattern of holding across all 24 components: share of weight in the wrong pattern, % | 0.0 | 30.6 | 31.7 | 30.6 | 30.6 | 20.7 | 6.1 |
| Twelve conditional holding rates (listed below): average gap, points | 0.00 | 2.12 | 0.48 | 0.24 | 0.27 | 0.54 | 0.11 |
| Rank correlation of each amount with total income, among holders: average gap | 0.000 | 0.056 | 0.012 | 0.011 | 0.011 | 0.018 | 0.002 |
| Rank correlation between two amounts, among joint holders: average gap | 0.000 | 0.078 | 0.061 | 0.058 | 0.057 | 0.035 | 0.007 |
| Mean net worth by income group (5 groups): average gap, % | 0.6 | 24.9 | 10.5 | 4.5 | 4.2 | 10.3 | 1.7 |
| Median net worth by income group: average gap, % | 1.2 | 17.0 | 15.3 | 6.0 | 6.0 | 2.8 | 0.5 |
| Share of all net worth held by each income group: sum of absolute gaps, points | 0.4 | 14.1 | 7.6 | 2.4 | 2.3 | 10.7 | 0.8 |
| Top 1% share of net worth (by net worth): gap, points | 0.11 | 0.73 | 1.06 | 0.09 | 0.02 | 1.40 | 0.43 |
| Top 0.1% share of net worth: gap, points | 0.02 | 1.08 | 1.36 | 0.70 | 0.68 | 1.91 | 0.25 |
| Classifier telling completed from true records: AUC (0.5 = cannot tell) | – | 0.596 | 0.534 | 0.545 | 0.537 | 0.500 | – |

#### 4. Monte Carlo yardstick

Reference draws: `a/ref_draws.npz` and `a/ref_draws_b.npz` under the scratch folder. The yardstick puts the reference model's own draws against each other, so it measures nothing but the randomness of drawing. Its values appear as the noise columns in sections 2 and 3. Two readings: a statistic of one completed file (one draw per record) moves by about the one-against-one figure from draw to draw; a statistic averaged over many draws per record, compared between two independent sets of 32 reference draws, moves by about the many-against-many figure. A reweighted column with few effective draws per record lies between the two. The truth here is fixed (the training records themselves), so there is no survey-sampling noise in these comparisons; draw noise is the only noise.

Variables the classifier leans on most (drop in AUC when shuffled):

- Reference: AUC 0.596; primary_home 0.052, primary_mortgage 0.038, kg_primary_home 0.037, cash 0.031
- M1 reweight: AUC 0.534; primary_home 0.026, primary_mortgage 0.019, kg_primary_home 0.016, cash 0.015
- M2 tilt, before: AUC 0.545; primary_home 0.032, cash 0.027, primary_mortgage 0.022, kg_primary_home 0.021
- M2 tilt, after: AUC 0.537; primary_home 0.027, primary_mortgage 0.024, cash 0.020, kg_primary_home 0.017
<!-- END GENERATED -->
