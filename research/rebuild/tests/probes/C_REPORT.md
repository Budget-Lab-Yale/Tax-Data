---
title: "Test 4, probe C — all the data fixes together"
role: report
workstream: rebuild
status: current
updated: 2026-10-11
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# All the data fixes together

## Summary

- **The question.** Each of test 4's data fixes was tested alone on test 3's
  version 2 data. This probe put them all into one dataset (every total
  replaced by its parts plus a remainder, as D25 requires; itemized
  deductions unknown rather than zero for non-itemizers; the bridge
  variables mortgage interest and real estate tax on SCF rows; the SCF's
  shared variables translated into PUF terms) and asked whether the
  combined model is better at generating whole PUF records and at
  completing PUF records' wealth.
- **The itemizer fix and whole-record generation do not go together.**
  With the deductions unknown for non-itemizers, the model generates 88%
  itemizers and $12.8tn of AGI when the deductions can be drawn before the
  form of deduction, and 6% itemizers when they are drawn after it (PUF:
  30% and $8.09tn). With the deductions coded zero, as in version 2,
  generation is fine, but completed wealth breaks instead: only 28% of PUF
  returns are given a home (SCF 67%), because the model learns from SCF
  rows that zero mortgage interest and zero real estate tax mean no home.
  So each coding fails one of the two jobs. Two models were trained, one
  with each coding; each is reported on the job it can do.
- **Generation (zero coding) is as good as probe AS's model, not clearly
  better.** Six files (three model seeds, two sampling seeds), a fresh
  random order per record: total AGI $7.85tn, top 1% share 19.0%, top 0.1%
  share 8.5% (PUF $8.09tn, 19.4%, 8.8%; AS $7.97tn, 19.2%, 8.3%).
  Bottom-half capital gains $1bn (PUF $13bn, AS about $0, version 2 $32bn).
  In the top 0.1% the six main items sum to $653bn (PUF $702bn, AS $620bn)
  and AGI less those items is $29bn (PUF $28bn, AS $60bn); partnership and
  S corporation income there is $182bn (PUF $175bn, AS $133bn). On probe
  V's scorecard, records breaking a rule that real records never break
  fall from 12.9% (AS) to 1.0%, because taxable pensions above total
  pensions and qualified dividends above ordinary dividends can no longer
  be drawn; the other checks are about equal to AS's. Worse than AS:
  bottom-half partnership losses (−$53bn against −$31bn in the PUF and
  −$15bn in AS), now that Schedule E is drawn as five pieces.
- **SCF wealth completion is better than any single fix.** On SCF
  validation records, 34.11–34.20 nats per record (zero coding) and
  34.13–34.17 (unknown coding), against 35.50–35.56 for version 2, 35.27–35.32
  for X's translated model and 34.21–34.31 for BR's bridge model. Almost all
  of the gain comes from the bridge variables: with them hidden, the
  combined model scores 34.56–34.66 on SCF test records against 34.69–34.71
  for version 2.
- **Completed PUF wealth (unknown coding) is too low at the top.** Mean net
  worth is above the SCF in the bottom half and the 50th to 90th
  percentiles ($153k against $113k; $423k against $370k; X's model was
  within 10 percent), and below it above the 99th percentile: top 0.1% $38m
  against the SCF's $55m and X's translated model's $53m. Median net worth
  of all returns is $56k (SCF $91k, or $56–57k counting split-off units at
  zero). Home ownership is 57% (SCF 67%). The cause is not known. A diagnostic model with
  the SCF's Schedule E left unobserved (instead of split by a PUF donor) is
  just as low (two model seeds), so the donor split is not it.
- **The weight as a model variable (probe UW's weighting) does not combine
  well**: generated AGI $9.43tn, top 1% share 33.9%, bottom-half AGI
  −$1.0tn, the same failure UW found on its own.

## What was run

All jobs ran in Slurm on 11 October from 00:15. Outputs are under
`$Q = /nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/`, logs under
`../logs/c3_*`, and the job list in `$Q/jobs.txt`.

| Step | Script (this folder) | Jobs |
|---|---|---|
| Check that the three source datasets have the same rows | `c3_inspect.py`, `c3_cpu.sbatch` | 29097839 |
| Dataset `full_v3c` | `c3_prepare.py`, `c3_identities.py` | 29098235 |
| Check: training records' tokens decoded back to dollars, totals from the decoded parts | `c3_compare.py` | 29098356 |
| Training, version 2 recipe, model seeds 1–3 (runs `../posttrain/h_train.py` unchanged with `h_train.sbatch`'s settings) | `c3_train.sbatch` | 29098346–48 |
| Test generations from early checkpoints (20,000 and 30,000 records) | `c3_gen.sbatch`, `c3_compare.py` | 29098853–54, 29099543–45 |
| Dataset `full_v3cz`: as `full_v3c`, but the six itemized deductions zero for PUF non-itemizers (version 2's coding) | `c3_prepare.py` with `C3ZERO=1` | 29099869 |
| Training on `full_v3cz`, seeds 1–3 | `c3_train.sbatch` | 29099935–37 |
| Dataset `full_v3czw` (`full_v3cz` plus the PUF sampling weight as a model variable; an earlier version built on `full_v3c` was not used) | `c3_uwprepare.py` (copy of `uw_prepare.py`) | 29099934 |
| Training with probe UW's weighting (PUF records unweighted, weight a model variable; `uw_train.py --loss-weight uw`), seeds 1–2 | `c3_uwtrain.sbatch` | 29099938–39 |
| Diagnostic dataset `full_v3czs` (as `full_v3cz`, the SCF's Schedule E unobserved) and its training, seeds 1–2 | `c3_prepare.py` with `C3HIDESCHE=1`, `c3_train.sbatch` | 29103634, 29103636–37 |
| One job per model after training: generation of 664,624 whole PUF records per file, a fresh random order per record (`c3_complete.py`, a copy of `as_complete.py` with one added order mode), totals computed from parts (`c3_totals.py`); wealth completion of PUF training records (five draws) and SCF test records with the bridge variables observed and hidden (`../posttrain/h_complete.py`, unchanged) | `c3_chain.sbatch` | 29099921–23 (`full_v3c`), 29099942, 29102589–90 (`full_v3cz`; the first two attempts, 29099940–41, ran out of GPU memory on a node shared with another job), 29099943–44 (`full_v3czw`), 29103638–39 (`full_v3czs`, completion only) |
| SCF test-record scores for probe X's two models (for comparison) | `c3_comp.sbatch` | 29098371–72 |
| Collector, first pass and final pass: every table below, then probe V's scorecard (`v_score.py`, unchanged) on sampling seed 1 of the combined models | `c3_collect.sbatch`, `c3_collect.sh`, `c3_compare.py` (copy of `as_compare.py`), `c3_table.py` (copy of `as_table.py`), `c3_wealth.py` (copy of `x_compare.py`), `c3_summary.py` | 29103416, 29103417 |

Training, generation and completion settings are version 2's throughout
(`h_train.py` unchanged, `--drop-weight-vars --loss-weight survey`, early
stopping as in test 3). The first chain submissions (gen and completion
jobs 29098782–29098802) were cancelled before they ran, when the coding
problem below appeared.

## The dataset

`full_v3c` has the same 275,260 rows, split, weights and wealth as test 3's
version 2 (`full_v2`). The three datasets it draws on (BR's `full_v3br`, X's
`full_v3x2`, AS's `full_v2s`) were each built from version 2 and have
identical rows (checked on source, year, household, implicate, unit and
weight; job 29097839). The changes:

1. **Totals replaced by their parts (D25).** Each total is removed from the
   model and its column holds the piece that completes the identity:

   | Total removed | Variable in its place | Identity used after generation |
   |---|---|---|
   | AGI (`E00100`) | AGI remainder (probe AS's, unchanged) | AGI = ten income items less three adjustments plus the remainder (`as_agi.py`) |
   | Total Schedule E (`E02000`) | Schedule E remainder | total = partnership and S corporation + rent and royalty income − rent and royalty loss + estate and trust income − estate and trust loss + remainder |
   | Ordinary dividends (`E00600`) | Non-qualified dividends | ordinary = qualified + non-qualified |
   | Total pensions (`E01500`) | Non-taxable pensions | total = taxable + non-taxable |

   No other total sits beside its parts among the 40 PUF variables: long-term
   gains, short-term gains and capital gain distributions are separate
   pieces of line 13 (the AGI identity combines them with the $3,000 loss
   limit); gross Social Security has no taxable part in the model; the
   itemized deductions are separate items. On the true dollars every
   identity reproduces the real total on every PUF record (training,
   validation and test; largest difference $0.00). Decoding the training
   records' own tokens and computing AGI from the decoded parts puts 24.2%
   of records within 1% of their true AGI, the same as test 3's data (24.2%)
   and AS's (24.1%) (job 29098356).

   The Schedule E remainder is not small in count: it is nonzero on 15% of
   PUF records (unweighted), with a net total of about $4bn, mostly positive.
   The likely content is rental losses disallowed under the passive-loss
   rules (the loss variable is before the limit; the total is after it),
   farm rental and REMIC income; this was not checked further. Non-qualified
   dividends and non-taxable pensions are never negative in the PUF, so
   they use the positive-only coding of the totals they replace; the
   Schedule E remainder can be negative and uses the coding of total
   Schedule E.

2. **Itemized deductions unknown for non-itemizers.** For PUF returns that
   take the standard deduction, all six itemized deductions in the model
   (medical, state and local income or sales tax, real estate tax, interest
   paid, cash and non-cash charity) are unknown (not zero) in training. BR
   did this for its two bridge variables only; here it covers all six. No
   non-itemizer had a nonzero amount in any of them. In generated records
   the model draws them for everyone; they are set to zero where the drawn
   form of deduction is not itemized (`c3_totals.py`).

   Because this coding fails in generation (see Results), a second dataset,
   `full_v3cz`, is identical except that the six deductions are zero for
   PUF non-itemizers, as in version 2 (the SCF rows keep BR's bridge
   values). "Unknown coding" and "zero coding" below refer to these two.

3. **Bridge variables** (probe BR): mortgage interest and real estate tax
   observed on every SCF row, built and ranked on the PUF itemizer
   distribution exactly as BR built them (tokens copied from `full_v3br`).

4. **Translated SCF** (probe X, `full_v3x2`, the final rule: switch on by
   likelihood, switch off at random within cells): the SCF rows' filing
   status, exemptions, children, age range, sex and eleven shared incomes
   are copied from X's dataset, in PUF 2015 dollars and tokens.

   **How the SCF's totals meet D25.** The SCF measures total dividends and
   total Schedule E only. Each SCF unit's translated total is split into
   the PUF's parts using a donor: a PUF training record drawn, with
   probability proportional to its weight, among PUF records whose total
   falls in the same weighted-rank band (the same token). Dividends: the
   qualified part is the SCF total times the donor's qualified share.
   Schedule E: each piece and the remainder are the donor's amounts scaled
   by the SCF total over the donor's total, so they add to the SCF total
   exactly and keep the donor's mix of partnership, rental and estate
   income. A zero total gives zero parts. Every SCF row with a nonzero total
   found a donor in its band. The split draws from the PUF's distribution
   given the total; it does not use the SCF household's own wealth, so on
   SCF rows the mix of partnership against rental income carries no
   information about business against real estate wealth. Taxable pensions
   stay as X translated them; non-taxable pensions are unobserved on SCF
   rows, as total pensions were.

**Left out:** nothing on the list. The weakest pieces are the donor split
of the SCF's Schedule E and dividends, described above, and the itemizer
coding, which works for one job and not the other.

## Results

Every number below is from the collector's first pass (job 29103416,
`$Q/C_TABLES.md`, written 01:37) unless marked. Values are means over files
with the range in brackets. The real PUF column is the PUF training
records with their weights; AGI groups use the real PUF's thresholds
(50th percentile $36,200, 90th $133,700, 99th $464,600, 99.9th $2.131m).

### Whole PUF records generated from scratch

Each file is 664,624 records drawn in a fresh random order per record and
weighted equally to the PUF total. Combined = zero coding (three model
seeds by two sampling seeds, six files). Version 2's files are probe O's
(three model seeds), AS's are probe AS's (four model seeds by two sampling
seeds).

| | PUF | Version 2 | AS (AGI remainder) | Combined, zero coding | Combined, weight as variable | Combined, unknown coding, plain random order | Combined, unknown coding, deductions after form of deduction |
|---|---|---|---|---|---|---|---|
| Files | | 3 | 8 | 6 | 4 | 3 | 3 |
| Total AGI ($tn) | 8.09 | 7.99 [7.84–8.11] | 7.97 [7.81–8.11] | 7.85 [7.80–7.92] | 9.43 [9.38–9.48] | 12.80 [12.62–13.12] | 7.83 [7.71–8.00] |
| Weight above the true 99th percentile (%) | 1.00 | 0.91 | 1.00 | 0.97 [0.94–1.00] | 2.27 | 1.82 | 0.77 |
| Weight above the true 99.9th percentile (%) | 0.100 | 0.093 | 0.100 | 0.101 [0.095–0.108] | 0.223 | 0.179 | 0.080 |
| Top 1% share of AGI (%) | 19.4 | 17.8 [17.3–18.4] | 19.2 [18.0–20.2] | 19.0 [18.1–19.6] | 33.9 | 21.6 | 15.3 |
| Top 0.1% share of AGI (%) | 8.8 | 8.0 [7.6–8.6] | 8.3 [7.5–9.5] | 8.5 [7.7–9.1] | 15.6 | 9.4 | 6.9 |
| Itemizers, bottom half (%) | 7.7 | 7.8 | 8.8 | 8.9 | 10.4 | 82.7 | 1.9 |
| Itemizers, 50th–90th percentile (%) | 42.8 | 42.3 | 41.3 | 41.0 | 43.5 | 88.4 | 6.6 |
| Capital gains, bottom half ($bn) | 13 | 32 | 0 [−4–2] | 1 [−1–2] | −123 | 1 | −1 |
| Partnership and S corporation, bottom half ($bn) | −31 | 16 | −15 [−29–29] | −53 [−78– −36] | −336 | −85 | −52 |
| Top 0.1%: six main items ($bn) | 702 | 341 | 620 [541–730] | 653 [585–708] | 1,544 | 1,156 | 533 |
| Top 0.1%: AGI less the six items ($bn) | 28 | 313 | 60 [46–82] | 29 [22–49] | 233 | 67 | 19 |

The six main items are wages, capital gains (short and long term),
partnership and S corporation income, ordinary dividends, Schedule C and
taxable interest, as in AS's report.

Components by AGI group, $bn (combined = zero coding, six files):

| AGI group | Item | PUF | AS | Combined |
|---|---|---:|---:|---:|
| Bottom half | AGI | 830 | 806 | 786 |
| | Wages | 810 | 821 | 820 |
| | Schedule C | 59 | 47 | 48 |
| | Schedule E total | −33 | −43 | −63 |
| | Dividends | 14 | 13 | 15 |
| 50th–90th | AGI | 3,415 | 3,357 | 3,354 |
| | Capital gains | 30 | 23 | 28 |
| | Partnership and S corp | 38 | 36 | 31 |
| 90th–99th | AGI | 2,245 | 2,247 | 2,176 |
| | Wages | 1,590 | 1,608 | 1,562 |
| | Capital gains | 84 | 73 | 72 |
| | Partnership and S corp | 129 | 126 | 117 |
| 99th–99.9th | AGI | 873 | 885 | 848 |
| | Wages | 455 | 433 | 418 |
| | Capital gains | 104 | 108 | 97 |
| | Partnership and S corp | 186 | 185 | 187 |
| Top 0.1% | AGI | 731 | 680 | 682 |
| | Wages | 211 | 180 | 176 |
| | Capital gains | 245 | 242 | 230 |
| | Partnership and S corp | 175 | 133 | 182 |
| | Dividends | 45 | 42 | 43 |
| | Schedule E total | 190 | 179 | 198 |
| | Interest | 18 | 12 | 12 |

The full table, with every item and the AGI remainder's distribution by
group, is in `$Q/C_TABLES.md`.

**Why the unknown coding fails in generation.** The model only ever sees
deduction amounts for itemizers, so a deduction drawn before the form of
deduction is a typical itemizer's amount, and the form of deduction then
follows it (88% itemizers, and income pulled up with it). Drawn after, the
form of deduction is chosen with all six deductions hidden. In training,
non-itemizers always have them hidden, while an itemizer has all six hidden
only when the random hiding happens to cover them all, so the hiding
itself reads as evidence of a non-itemizer: the itemizer share falls to 6%
(the odds of itemizing fall by a factor of about seven). The test
generations from early checkpoints showed the same
pattern (job 29099545).

### Probe V's scorecard

Probe V's `v_score.py`, unchanged, on sampling seed 1 of each zero-coded
combined model (three files) and of the weight-as-variable model (two
files), from the collector's first pass. AS's column is from probe V's own
table of AS's model, random order, all draws
(`/nfs/roberts/scratch/pi_nrs36/jar335/probes/v/V_TABLES_AS_ALL_DRAWS.md`).
Mean over files, range in brackets.

| Check | Real PUF | AS, random order | Combined, zero coding | Combined, weight as variable |
|---|---|---|---|---|
| Sum over 30 amounts and 5 AGI groups of the dollar miss ($bn) | 0 | 794 [687–873] | 877 [860–897] | 6,688 |
| Mean gap in share with a nonzero amount (points) | 0 | 2.48 | 2.31 [2.13–2.49] | 4.11 |
| Co-holding of 28 pairs of items, mean gap (points) | 0 | 2.45 | 2.49 [2.19–2.77] | 4.24 |
| Returns breaking a rule real records never break (%) | 0 | 12.91 | 1.00 [0.93–1.07] | 1.08 |
| of which taxable pensions above total pensions (%) | 0 | 7.77 | 0.00 | 0.00 |
| of which qualified dividends above ordinary dividends (%) | 0 | 4.52 | 0.00 | 0.00 |
| Any rule broken, top 0.1% group (%) | 0 | 32.14 | 1.83 | 1.38 |
| Net income tax, all returns ($bn; V's simplified 2015 tax) | 1,095 | 1,117 | 1,093 [1,077–1,109] | 1,887 |
| Net income tax, top 0.1% group ($bn) | 204 | 203 | 204 [189–216] | 516 |
| Top 1% share of net income tax (%) | 39.7 | 39.5 | 39.8 [38.4–40.8] | 55.7 |
| Largest gap in average tax rate by AGI group (points) | 0 | 1.91 | 2.69 [2.45–2.91] | 6.88 |
| Classifier telling generated from real, area under the curve (0.5 = cannot tell) | 0.5 | 0.814 | 0.814 [0.812–0.816] | 0.827 |
| The same within the top 0.1% group | 0.5 | 0.780 | 0.802 [0.797–0.808] | 0.905 |
| Generated records identical to a training record on all 37 variables (%) | | 25.51 | 25.65 | 22.14 |

D25 removes the impossible records that AS's model still made: drawing
non-taxable pensions and non-qualified dividends instead of the totals
makes "taxable above total" and "qualified above ordinary" impossible by
construction, and the share of records breaking any rule falls from 12.9%
to 1.0% (32% to 1.8% in the top 0.1% group). On the other checks the
combined model and AS's are about equal. The full scorecard, with every
variable by AGI group, is in `$Q/v_tables.md`.

### SCF wealth completion

Nats per record, lower is better; one value per training seed. Validation:
the score each training run reports on SCF validation records with every
other variable observed (test 3's 35.50–35.56). Test: the 24 wealth values
of SCF test records, summed, as in BR's table; "bridge hidden" hides
mortgage interest and real estate tax too, so it measures the model
without them.

| Model | Validation records | Test records | Test records, bridge hidden |
|---|---|---|---|
| Version 2 (test 3) | 35.50, 35.52, 35.56 | 34.69, 34.69, 34.71 | not run |
| AGI remainder (AS) | 35.52, 35.52, 35.50 | not run | not run |
| Translated SCF (X) | 35.32, 35.27 | 34.54, 34.50 | not run |
| Bridge variables (BR) | 34.31, 34.21 | 33.52, 33.45 | 34.72, 34.70 |
| Combined, zero coding | 34.20, 34.12, 34.11 | 33.46, 33.46, 33.41 | 34.66, 34.66, 34.61 |
| Combined, unknown coding | 34.13, 34.17, 34.15 | 33.37, 33.43, 33.42 | 34.56, 34.65, 34.62 |
| Combined, weight as variable | 34.24, 34.19 | 33.49, 33.54 | 34.69, 34.72 |
| Combined, SCF Schedule E unobserved (diagnostic) | 34.32, 34.24 | 33.61, 33.58 | 34.85, 34.81 |

X's test-record scores were run by this probe (jobs 29098371–72); the
others are from each probe's own files.

### Completed PUF wealth by AGI group

PUF training records, wealth drawn five times with the SCF tag at 2015;
mean over training seeds, range in brackets. PUF groups are by true AGI.
The SCF benchmark is the 2013 and 2016 waves interpolated to 2015, units
with wealth observed, grouped by the seven-income total within each wave,
as probe X did. Counting split-off units at zero wealth, the SCF's median
is $56k (2013) and $57k (2016) instead of $91k. Version 2's completions are
test 3's; X's and BR's are theirs. The unknown-coded combined model is
the one to read here; the zero-coded model's column shows its failure.

| | SCF 2015 | Version 2 | X (translated) | BR (bridge) | Combined, unknown coding | Combined, zero coding |
|---|---|---|---|---|---|---|
| Training seeds | | 3 | 2 | 2 | 3 | 3 |
| All returns, mean | $628k | $808k | $565k | $754k | $559k [$537k–$596k] | $508k |
| All returns, median | $91k | $62k | $41k | $65k | $56k [$56k–$57k] | $24k |
| Mean, bottom half | $113k | $207k | $113k | $190k | $153k | $105k |
| Mean, 50th–90th | $370k | $561k | $344k | $536k | $423k | $362k |
| Mean, 90th–99th | $2.13m | $2.97m | $2.18m | $2.74m | $2.03m | $1.98m |
| Mean, 99th–99.9th | $12.3m | $16.7m | $13.6m | $15.6m | $10.2m | $10.4m |
| Mean, top 0.1% | $54.7m | $63.0m | $53.0m | $58.0m | $37.8m [$34.0–44.5m] | $38.6m |
| Median, bottom half | $17k | $9k | $7k | $10k | $10k | $5k |
| Median, 50th–90th | $146k | $164k | $114k | $157k | $131k | $84k |
| Median, 90th–99th | $1.10m | $1.29m | $1.01m | $1.20m | $0.92m | $0.91m |
| Median, 99th–99.9th | $7.96m | $9.16m | $7.94m | $8.24m | $5.59m | $5.70m |
| Median, top 0.1% | $32.7m | $38.2m | $30.8m | $34.3m | $20.7m | $21.2m |
| Own a home, bottom half | 49% | 32% | 28% | 35% | 37% | 7% |
| Own a home, 50th–90th | 77% | 72% | 67% | 74% | 73% | 40% |
| Own a home, 90th–99th | 92% | 94% | 93% | 92% | 92% | 83% |
| Own a home, top 0.1% | 99% | 99% | 99% | 97% | 97% | 91% |
| Own a home, all returns | 67% | 54% | 50% | 56% | 57% | 28% |
| Hold stocks, 50th–90th | 21% | 30% | 22% | 27% | 21% | 21% |
| Hold stocks, 90th–99th | 53% | 67% | 54% | 61% | 51% | 52% |
| Hold stocks, top 0.1% | 94% | 93% | 91% | 91% | 89% | 90% |
| Hold a business, 90th–99th | 28% | 32% | 23% | 30% | 23% | 24% |
| Hold a business, 99th–99.9th | 64% | 68% | 52% | 65% | 49% | 51% |
| Hold a business, top 0.1% | 85% | 87% | 73% | 84% | 71% | 73% |

PUF returns that are not dependent filers (the SCF's population): median
net worth $68k and home ownership 60% for the unknown-coded model (version
2: $78k and 57%; X: $51k and 53%; BR: $79k and 59%).

The other two arms, two training seeds each (job 29107711, a rerun of
`c3_wealth.py` and `c3_summary.py` after their completions finished): the
weight-as-variable model gives a top 0.1% mean of $33.5m and 27% home
ownership (zero coding, so the home result is expected); the diagnostic
model with the SCF's Schedule E unobserved gives a top 0.1% mean of $34.0m
($30.9–37.0m) and median $19.8m, no higher than the zero-coded combined
model's $38.6m and $21.2m, and 99th–99.9th percentile mean $10.8m against
$10.4m.

## What it means

- **The fixes combine without undoing each other on the parts each was
  built for.** D25 keeps AS's gains in generation (top shares, no false
  bottom-half capital gains, AGI adding up in the top 0.1%), and the bridge
  variables and translation together give the best SCF completion score of
  any model so far. In the top 0.1%, drawing Schedule E and dividends as
  their parts brings partnership income up to the PUF's level ($182bn
  against $175bn), which neither AS's model nor version 2 managed.
- **But the combined model is not better than AS's on generation, and it
  is worse than X's and BR's on completed wealth at the top.** Drawing
  Schedule E as five pieces adds bottom-half partnership losses; and
  completed net worth above the 99th percentile of AGI is 17 to 37 percent
  below the SCF (means and medians), where X's model was within 10 percent.
  That second result is unexplained. The SCF's Schedule E split is ruled
  out (two seeds). The candidates left are the other changes made together
  here and how they interact: the bridge variables with the translated
  SCF, the remainders replacing AGI and the totals on the PUF side, and
  the split of the SCF's dividends.
- **The itemized deductions need a different design, not either coding.**
  The deduction a PUF return claims and the amount a household pays are
  different quantities. A design that would serve both jobs: keep the
  claimed deductions as PUF variables (zero for non-itemizers, as the
  return says), and add the economic amounts (mortgage interest paid, real
  estate tax paid) as separate shared variables, observed on SCF rows and
  on PUF itemizers, unknown for PUF non-itemizers and never drawn in
  whole-record generation of PUF records. This was not tried.
- **Weight as a model variable does not combine.** UW's weighting gave the
  same overshoot at the top and negative income at the bottom here as on
  its own.
- For the plan: D25 and the bridge variables should stay; the translated
  SCF should stay but its interaction with the other changes at the top of
  the wealth distribution needs a dedicated test; the itemizer coding
  needs the split design above before the next combined run.

## To finish

Job list: `/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/jobs.txt`.
Logs: `/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/c3_*`.

Every job has finished and every result is in this report; nothing is
pending and no probe C job is running.

| Job | State | Writes |
|---|---|---|
| 29103416 (collector, first pass) | Finished 01:45 | `$Q/C_TABLES.md`, `$Q/v/*.json`, `$Q/v_tables.md` |
| 29107711 (rerun of `c3_wealth.py`, `c3_summary.py`) | Finished 01:48 | log `../logs/c3_cpush_29107711.out` |
| 29103417 (collector, final pass) | Finished 01:47; its tables match this report | `$Q/C_TABLES.md` (final) |

If a scorecard is ever rerun, its log is `$Q/v/log_*.txt`; `v_score.py`
reads AGI from the file's `E00100`, which `c3_totals.py` computes from
the parts.

Not done, for a next run: the split design for itemized deductions
described above; a test that isolates which change lowers completed
wealth at the top (for example, the combined data without translation,
and without the bridge variables).
