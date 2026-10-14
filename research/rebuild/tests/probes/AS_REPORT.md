---
title: "Test 4, probe AS — AGI as the sum of its components plus a remainder"
role: report
workstream: rebuild
status: current
updated: 2026-10-11
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# AGI as the sum of its components plus a remainder

## Summary

- **John's question.** In test 3's model, AGI is drawn as a variable of its
  own and can disagree with its income items. This probe removes AGI from
  the model. In its place the model draws a remainder: AGI less the income
  items in the model, plus the adjustments in the model. Generated AGI is
  then computed as items less adjustments plus the remainder, so it adds up
  by construction.
- **It fixes the bottom.** With a fresh random order for each record, false
  bottom-half capital gains fall from $32bn (test 3's model, same order) and
  $124bn (test 3's model, AGI first) to about $0, against $13bn in the PUF.
  Bottom-half partnership and S corporation income is a net loss of $8bn,
  against a net loss of $31bn in the PUF. Test 3's model gave a net gain of
  $16bn to $40bn. Bottom-half dividends are $13bn (PUF $14bn).
- **It mostly fixes the top.** With a random order, the top 1% share of AGI
  is 19.4% (PUF 19.4%) and the top 0.1% share 8.6% (PUF 8.8%), means over
  four model seeds. In the top 0.1%, the six main components now sum to
  $642bn (range $541–730bn) against $702bn in the PUF. Test 3's model gave
  $341bn. AGI less those components is $61bn, against $28bn in the PUF and
  $313bn in test 3's model. Capital gains in the top 0.1% are $249bn
  (PUF $245bn), against $85bn before.
- **What is still off.** In the top 0.1%, wages ($180bn against $211bn) and
  partnership and S corporation income ($142bn against $175bn) are short.
  Bottom-half Schedule C is $47bn against $59bn. The remainder's spread at
  the very top is about half the PUF's. The fixed order with the largest
  amounts first overshoots the 99th to 99.9th percentile ($954bn of AGI
  against $873bn; top 1% share 20.4%).
- **The SCF side is unchanged.** Wealth completion on SCF validation records
  scores 35.52, 35.52 and 35.50 nats per record, against test 3's 35.50,
  35.52 and 35.56.
- **Probe S's training recipe did not help here.** Two models were trained
  with 25% of each batch on generation's questions, in the size-first order.
  Both overshoot the top 0.1% (share 9.9% against 8.8%; capital gains there
  $376bn against $245bn).

## What was run

All jobs ran in Slurm on 10 October between 23:00 and 23:55. Outputs are
under `/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/`, and logs are
under `../logs/as_*`.

| Step | Script | Jobs |
|---|---|---|
| Dataset `full_v2s` | `as_agi.py` (the identity), `as_prepare.py`, `as_cpu.sbatch` | 29087665 |
| Training, version 2 recipe, model seeds 1–4 | `as_train.sbatch` (runs `../posttrain/h_train.py` unchanged, with `h_train.sbatch`'s settings) | 29087760–62, 29087849 |
| Training, probe S's 25% recipe, seeds 1–2 | `as_mix.sbatch`, `as_train_mix.py` (an unchanged copy of `s_train.py`) | 29089331, 29089333 |
| Generation, 664,624 whole PUF records per file | `as_gen.sbatch`, `as_complete.py` (an unchanged copy of `o_complete.py`), `as_orders.py` | 29087860–70, 29090031–32, 29091103–16, 29089332, 29089334, 29094768–71 |
| Held-out log-probability by order | `as_score.sbatch` | 29091130–32 |
| Levels tables | `as_compare.py` (a copy of `o_compare.py` with AGI computed from its parts), `as_table.py`, `as_final.sh`, `as_cpush.sbatch` | 29087830 (check), 29095422 (final) |

Seed 4 was added in case scavenge jobs were preempted; none were, so the
version 2 arms have four model seeds. Every model was used to generate in
two orders with sampling seeds 1, 2 and 3. Results for the 25% recipe
are from both seeds and all three sampling seeds.

### The dataset

`full_v2s` is test 3's version 2 data (`scf_joint/data/full_v2`) with one
change. AGI (`E00100`) is removed and its column holds the remainder
(`AGI_REMAINDER`). The remainder is encoded with the same rank-band
tokenizer as AGI. Its 80 codes are 13 bands of weighted rank among negative
values, one code for exactly zero, and 66 bands among positive values. The
bands come from the PUF training records. Every other column is copied
unchanged. AGI was already unobserved for SCF rows, and so is the remainder,
so the SCF side of the data is identical.

The identity uses 2015 Form 1040 lines, as far as the 40 model variables
carry them (`as_agi.py`):

| Form 1040, 2015 | Variable | Enters as |
|---|---|---|
| Line 7, wages | E00200 | income item |
| Line 8a, taxable interest | E00300 | income item |
| Line 9a, ordinary dividends | E00600 | income item |
| Line 12, Schedule C | E00900 | income item |
| Line 13, capital gain or loss | E01100 + max(P22250 + P23250, −$3,000) | income item; the loss limit is −$1,500 for married filing separately |
| Line 15b, taxable IRA distributions | E01400 | income item |
| Line 16b, taxable pensions and annuities | E01700 | income item |
| Line 17, Schedule E total | E02000 | income item |
| Line 18, Schedule F | E02100 | income item |
| Line 19, unemployment compensation | E02300 | income item |
| Line 29, self-employed health insurance | E03270 | adjustment |
| Line 32, IRA deduction | E03150 | adjustment |
| Line 33, student loan interest | E03210 | adjustment |

Notes on the identity:

- **Capital losses.** Line 13 uses the Schedule D net gain or loss
  (long-term P23250 plus short-term P22250), with a net loss capped at
  $3,000. E01100 is capital gain distributions reported directly on line 13
  by filers without Schedule D. The gain variables in the model stay
  unlimited, and the cap is applied when AGI is computed.
- **Schedule E.** Line 17 enters as the total, E02000. Its pieces in the
  model are partnership and S corporation income (E26270), rent and royalty
  income and loss (E25850, E25860), and estate and trust income and loss
  (E26390, E26400). These are inside E02000 and are not added again.
- **Not in AGI.** Tax-exempt interest (E00400), qualified dividends (inside
  E00600), total pensions (E01500), gross Social Security (E02400) and the
  itemized deductions are left out.
- **What the remainder holds.** It is everything on the return that the
  model does not carry. Income items: taxable refunds (line 10), alimony
  received (11), Form 4797 gains (14), taxable Social Security (20b) and
  other income (21), including net operating loss carryovers, which are
  entered as negative amounts. Adjustments: half of self-employment tax, SEP
  and similar plans, HSA, moving expenses, educator expenses, early
  withdrawal penalty, alimony paid, tuition and the domestic production
  deduction.

In the PUF the remainder totals $17bn. By AGI group it is −$165bn in the
bottom half (not broken down further; negative other income such as
operating loss carryovers is a likely part), +$138bn
in the 50th to 90th percentiles (taxable Social Security among it), +$40bn
in the 90th to 99th, −$2bn in the 99th to 99.9th, and +$7bn in the top
0.1%. It is exactly zero on 58% of bottom-half returns.

### Check on the PUF training records (job 29087830)

- **Dollars.** On the true dollar values, items less adjustments plus the
  remainder equal AGI on every record (100% within $1).
- **Tokens decoded back to dollars.** Decoding the training records' own
  tokens, and computing AGI from the decoded parts, puts 24.1% of records
  within $1 or 1% of their true AGI. Decoding AGI directly in test 3's data
  gives 24.2%. Decoding draws each amount at random inside its rank band, so
  no single record is exact either way. The difference is that the error is
  now spread across several decoded amounts instead of one.
- **Totals by true AGI group** (true / decoded from parts / test 3's AGI
  decoded directly, $bn):

| Group | True | From parts | Test 3, AGI directly |
|---|---|---|---|
| Bottom 50 | 830 | 842 | 832 |
| 50–90 | 3,415 | 3,411 | 3,416 |
| 90–99 | 2,245 | 2,242 | 2,246 |
| 99–99.9 | 873 | 894 | 873 |
| Top 0.1 | 731 | 704 | 731 |

  Grouped by the decoded AGI, the decoded records give total AGI $8.09tn,
  1.00% of weight above the true 99th percentile, and top shares of 19.6%
  and 9.0% (PUF 19.4% and 8.8%). Encoding and decoding therefore lose almost
  nothing in aggregate. The $27bn shortfall in the top 0.1%, on the same
  records, is mostly the wage decoding loss T found ($186bn against $211bn).

### Orders and comparisons

- **Random:** a fresh random order of the 40 variables for each record.
  This was O's best compromise.
- **Size first:** a fixed order. It starts with every amount, largest first
  by weighted total of absolute dollars in the PUF: wages, total pensions,
  Schedule E, partnership, the remainder, long-term gains, taxable pensions,
  Social Security, Schedule C, and so on. The six categoricals follow,
  then age, gender and earnings split.
- **Comparison with test 3.** Test 3's model is compared through O's
  generated files (random order and AGI first, model seeds 1–3, sampling
  seed 1). They were re-tabulated here with the same code. The figures
  match `O_REPORT.md`, and the implied remainder (AGI less items plus
  adjustments) is added.
- **Thresholds.** Every file is compared with the PUF training records
  (166,156 records, 120.7 million returns), with records grouped by their
  own AGI. Thresholds are the PUF's AGI percentiles: 50th $36,200, 90th
  $133,700, 99th $464,600, 99.9th $2.131m. "Six components" means wages,
  capital gains (long plus short term, unlimited), partnership and S
  corporation income, ordinary dividends, taxable interest and Schedule C,
  as in O.

## Results

Means over model seeds at sampling seed 1, with the range over seeds in
brackets. Test 3 has 3 model seeds, the summed-AGI models 4, and the 25%
recipe 2 seeds × 3 sampling seeds. Amounts are weighted totals in $bn
unless marked otherwise.

| | PUF | Test 3, random | Test 3, AGI first | Summed, random | Summed, size first | Summed + 25% ordered training, size first |
|---|---|---|---|---|---|---|
| Total AGI ($tn) | 8.09 | 7.99 [7.84–8.11] | 7.84 [7.60–8.00] | 7.99 [7.89–8.11] | 8.00 [7.89–8.26] | 8.05 [7.89–8.22] |
| Weight above true 99th pct (%) | 1.00 | 0.91 | 0.95 | 1.00 [0.96–1.02] | 1.08 | 1.02 |
| Weight above true 99.9th pct (%) | 0.100 | 0.093 | 0.085 | 0.103 [0.097–0.108] | 0.106 | 0.111 |
| Top 1% share of AGI (%) | 19.4 | 17.8 [17.3–18.4] | 17.3 [16.4–17.7] | 19.4 [18.8–20.2] | 20.4 [19.2–21.7] | 20.6 [18.8–22.4] |
| Top 0.1% share of AGI (%) | 8.8 | 8.0 [7.6–8.6] | 6.9 [6.2–7.2] | 8.6 [7.7–9.5] | 8.8 [7.7–9.4] | 9.9 [8.7–10.9] |
| AGI, bottom 50 | 830 | 869 | 800 | 802 | 787 | 754 |
| AGI, 50–90 | 3,415 | 3,407 | 3,487 | 3,354 | 3,332 | 3,377 |
| AGI, 90–99 | 2,245 | 2,269 | 2,171 | 2,243 | 2,203 | 2,212 |
| AGI, 99–99.9 | 873 | 793 | 832 | 885 | 954 | 893 |
| AGI, top 0.1 | 731 | 654 | 554 | 703 [624–778] | 722 [626–781] | 817 [701–923] |
| Capital gains, bottom 50 | 13 | 32 | 124 | −1 [−4–2] | −1 | −4 |
| Capital gains, 50–90 | 30 | 61 | 94 | 24 | 26 | 13 |
| Capital gains, 99–99.9 | 104 | 144 | 178 | 110 | 130 | 129 |
| Capital gains, top 0.1 | 245 | 85 | 114 | 249 [196–300] | 313 [288–339] | 376 [325–437] |
| Partnership and S corp, bottom 50 | −31 | 16 | 40 | −8 [−28–29] | 1 | −10 |
| Partnership and S corp, 50–90 | 38 | 63 | 137 | 36 | 47 | 42 |
| Partnership and S corp, top 0.1 | 175 | 96 | 92 | 142 [125–158] | 128 | 122 |
| Dividends, bottom 50 | 14 | 21 | 48 | 13 | 14 | 13 |
| Dividends, top 0.1 | 45 | 22 | 27 | 47 | 47 | 50 |
| Schedule C, bottom 50 | 59 | 71 | 73 | 47 | 67 | 58 |
| Interest, bottom 50 | 9 | 11 | 22 | 7 | 8 | 8 |
| Wages, 90–99 | 1,590 | 1,701 | 1,766 | 1,604 | 1,515 | 1,576 |
| Wages, 99–99.9 | 455 | 478 | 597 | 431 | 458 | 429 |
| Wages, top 0.1 | 211 | 124 | 132 | 180 [156–220] | 164 | 164 |
| Schedule E total, top 0.1 | 190 | 102 | 95 | 188 | 134 | 172 |
| Six components, bottom 50 | 874 | 982 | 1,184 | 879 | 896 | 874 |
| Six components, 99–99.9 | 829 | 845 | 1,033 | 820 | 902 | 848 |
| Six components, top 0.1 | 702 | 341 [321–356] | 376 [323–404] | 642 [541–730] | 683 [599–723] | 751 [620–854] |
| AGI less six components, top 0.1 | 28 | 313 | 178 | 61 | 39 | 65 |
| Itemizers, top 0.1 (% of group) | 92.8 | 88.6 | 78.8 | 85.6 | 87.4 | 87.5 |
| Joint filers, top 0.1 (% of group) | 82.5 | 73.6 | 73.7 | 74.4 | 82.4 | 81.1 |
| Both wages and capital gains, top 0.1 (%) | 75.7 | 62.5 | 71.7 | 65.9 | 70.3 | 65.8 |

The remainder by AGI group, the PUF against the models. For test 3, the
remainder is implied: AGI less items plus adjustments.

| | PUF | Test 3, random | Test 3, AGI first | Summed, random | Summed, size first |
|---|---|---|---|---|---|
| Total, all returns ($bn) | 17 | −85 | −1,083 | 26 | −22 |
| Bottom 50 ($bn) | −165 | −266 | −565 | −144 | −190 |
| 50–90 ($bn) | 138 | 24 | −238 | 116 | 101 |
| 90–99 ($bn) | 40 | −50 | −241 | 40 | 38 |
| 99–99.9 ($bn) | −2 | −94 | −206 | 6 | 5 |
| Top 0.1 ($bn) | 7 | 302 | 168 | 8 | 25 |
| Exactly zero, bottom 50 (% of group) | 57.8 | 0.3 | 0.3 | 58.7 | 58.3 |
| Exactly zero, 50–90 (% of group) | 21.8 | 0.0 | 0.0 | 22.8 | 24.4 |
| Negative, top 0.1 (% of group) | 43.3 | 21.1 | 22.8 | 49.8 | 52.3 |
| Median, 99–99.9 ($) | 10 | 71,004 | 32,887 | 17 | 5 |
| Median, top 0.1 ($) | 930 | 2.16m | 1.97m | −7 | −220 |
| 5th percentile, top 0.1 ($) | −366,600 | −7.19m | −9.49m | −165,456 | −231,960 |
| 95th percentile, top 0.1 ($) | 540,800 | 14.75m | 10.85m | 249,242 | 420,287 |

Full tables, which cover every component in every group, the remainder's
5th, 50th and 95th percentiles in every group, and the itemizer,
joint-filer and wages-and-capital-gains shares, are in the log of job
29095422 and in `compare_final.json`.

**Sampling noise.** All 12 draws of each version 2 arm (4 model seeds × 3
sampling seeds) give the following.

- **Random order:** total AGI $7.97tn [7.81–8.11], top 1% share 19.1%
  [18.0–20.2], top 0.1% share 8.3% [7.5–9.5], top 0.1% capital gains
  $244bn [191–300].
- **Size first:** total AGI $8.02tn [7.89–8.37], top 1% share 20.6%
  [18.8–22.9], top 0.1% share 8.9% [7.3–10.8].

The top 0.1% is about 660 generated records per file, so a difference of
one or two points in its share is within the noise.

**Held-out log-probability by order** (jobs 29091130–32). Each PUF
validation record was scored by the chain rule in six orders: size first,
its reverse, categoricals first, remainder first, remainder last, and five
draws of a random order. Across these orders the mean score spans 0.15–0.20
nats per record (three model seeds). For a single record, the best and
worst order differ by 1.8 nats on average. The standard deviation across
orders is 0.55 nats for all records and 1.3 nats for top 1% records. For
test 3's model, O found a span of 0.44 nats, a gap of 2.3 nats and
standard deviations of 0.66 and 1.4, over ten orders. The order sets differ,
so this comparison is rough. It suggests that the model's answers to
different questions disagree somewhat less than before, though not by much.
The score levels cannot be compared with test 3's, because one of the 40
scored codes is now the remainder instead of AGI.

## Does making AGI a sum fix the top and the bottom?

1. **The bottom: yes.** Test 3's model drew low AGI and then drew income
   inconsistent with it. Its implied remainder in the bottom half is
   −$266bn (random order) and −$565bn (AGI first), against −$165bn in the
   PUF. It is negative on 53–55% of bottom-half records, against 25% in the
   PUF. Now a record's AGI is low only when its items are. The bottom half
   carries no false capital gains, partnership or dividend income.
   Bottom-half capital gains are now slightly too low ($0 against $13bn),
   and so is Schedule C in the random order ($47bn against $59bn).
2. **The top: mostly.** The random order now gets the top 1% and 0.1%
   shares right on average, and the top 0.1% is made of the right kinds of
   income. Capital gains and dividends match the PUF there. Schedule E is
   right in total ($188bn against $190bn), although partnership income
   within it is short. AGI no longer floats above its components: the gap
   in the top 0.1% is $61bn against $313bn in test 3. Wages in the top 0.1%
   remain short ($180bn against $211bn), and part of that, about $25bn, is
   the tokenizer's own decoding loss. Joint filers are under-represented in
   the top 0.1% (74% against 83%).
3. **The order still matters, but less.** Under test 3's model, orders
   moved the top 1% share from 4.7% to 24.5%. Here the two orders give
   19.4% and 20.4%. The size-first order puts too much income just below
   the top ($954bn of AGI in the 99th to 99.9th percentiles against $873bn).
   Its top 0.1% capital gains are too high ($313bn against $245bn), and its
   bottom-half Schedule C is about right. The random order is the better of
   the two.
4. **Probe S's 25% recipe does not carry over to this data.** Trained on
   the size-first questions and used in that order, the two models overshoot
   the top 0.1%. The share is 9.9% and capital gains there are $376bn, so
   the extra training did not fix the overshoot of the plain size-first
   order. Wealth completion moved by about 0.1 nats per record (35.61 and
   35.58). Only one order and one share of ordered questions were tried.
5. **For the plan.** Replacing AGI with its remainder costs nothing on the
   SCF side, and it removes the inconsistency between AGI and its parts
   that no generation order removed. Paired with a random order for each
   record, it is the best whole-record PUF generation of the probes so far,
   without post-training. The same approach could extend to other totals
   that the model draws beside their parts. Examples: total pensions with
   taxable pensions; ordinary dividends with qualified dividends; Schedule
   E with its pieces, where partnership income is short at the top.

## Unfinished and caveats

- **Seeds and draws.** The version 2 arms have 4 model seeds and 3 sampling
  seeds. The 25% recipe has 2 seeds; S used five for its recipe.
- **Not run.** Other fixed orders for the summed-AGI model; the 25% recipe
  with a random order, or with a smaller share; generation of more than
  664,624 records to steady the top 0.1%; tilt or reweighting
  post-training on this model; replacing other totals (E01500, E02000,
  E00600) by remainders.
- **Capital gains.** The capital gains in the tables are the unlimited
  Schedule D amounts. AGI uses them with the $3,000 loss cap. Records with
  large losses therefore fall in the AGI group their capped loss implies,
  as on a real return.
- **Process.** Every computation ran in Slurm. All figures in this report
  are weighted aggregates, and no record-level rows were written outside
  scratch. No probe AS jobs are running.
