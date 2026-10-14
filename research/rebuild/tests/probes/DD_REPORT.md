---
title: "Test 4, probe DD — itemized deductions as the amount claimed and the amount paid"
role: report
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# Itemized deductions as the amount claimed and the amount paid

## Summary

- **What was tried.** Each itemized deduction the SCF also measures is
  carried twice: the amount claimed on the return (a PUF variable, zero for
  non-itemizers) and the amount paid (a new variable, observed on SCF rows).
  Two versions: paid also observed for PUF itemizers (where it equals the
  claimed amount), and paid observed on SCF rows only. Three and two
  training seeds; one generated file of 664,624 whole PUF records per
  model; PUF wealth completed five times per model.
- **One model now serves both jobs, with two conditions.** The version
  with paid on SCF rows only generates PUF records as well as probe C's
  zero-coded model (29.4% itemizers against 29.8% in the PUF; total AGI
  $8.02tn against $8.09tn; each claimed deduction's total by AGI group
  about equal to C's zero-coded model's) and completes PUF wealth as
  well as C's unknown-coded model (55% of returns own a home against 57%
  for C's model and 67% in the SCF; mean net worth $559k against $559k;
  SCF test-record score 33.41–33.44 against C's 33.37–33.46). The two
  conditions: paid must not be observed on PUF rows in training, and when
  PUF records are completed, the form of deduction and the claimed amounts
  must be hidden, so that the model sees the record as the SCF rows it
  learned wealth from.
- **Paid observed for PUF itemizers breaks generation again, more mildly
  than in probe C.** Generated returns itemize 22.6% (three seeds, all
  22.5–22.7%) and total AGI falls to $7.34tn. Paid is hidden for every
  non-itemizer in training and for itemizers only when the random mask
  hides it, so in generation, where paid is hidden until the form of
  deduction is drawn, the hiding itself most likely reads as evidence of
  a non-itemizer, the mechanism probe C described. Its wealth completion
  (claimed hidden) is as good as the other version's.
- **Showing the claimed deductions in completion breaks wealth, in both
  versions** (39% and 41% of returns own a home): non-itemizers, whose
  claimed amounts are zero, get 18–19% home ownership, close to probe C's
  zero coding (1%) and far from its unknown coding (45%). Why, when the
  model never saw a claimed amount on an SCF row, was not investigated.
- **Not fixed by this:** completed net worth in the top 0.1% is still
  $37–39m against the SCF's $55m, as in probe C (probe WT's question),
  and home ownership of all returns is 55–56% against the SCF's 67%, as in
  probe C.

## The question

Probe C found that the six itemized deductions could not be coded one way
for both jobs the model has to do. Coded unknown for PUF returns that took
the standard deduction, whole-record generation broke (88% or 6% of
generated returns itemized, against 30% in the PUF). Coded zero, completed
wealth broke (28% of PUF returns were given a home, against 67% in the
SCF), because on SCF rows the same columns held mortgage interest and real
estate tax actually paid, so the model learned that zero means no home.
This probe separates the two quantities.

## The variable design

| Variable | PUF itemizers | PUF non-itemizers | SCF rows | Drawn in whole-record generation? |
|---|---|---|---|---|
| Form of deduction (`FDED`) | observed | observed | unknown | yes |
| Six claimed deductions (`E17500` medical, `E18400` state and local income or sales tax, `E18500` real estate tax, `E19200` interest paid, `E19800` cash charity, `E20100` non-cash charity) | observed, the amount on the return | observed, zero | unknown (new: in probe C the SCF's paid amounts sat in two of these columns) | yes |
| Mortgage interest paid (`MORTGAGE_INTEREST_PAID`, new) | observed, equal to claimed interest | unknown | observed (probe BR's bridge value) | no |
| Real estate tax paid (`REAL_ESTATE_TAX_PAID`, new) | observed, equal to claimed real estate tax | unknown | observed (probe BR's bridge value) | no |

The table is the main arm. In the second arm (below), the two paid
variables are unknown on every PUF row, itemizers included.

What is computed and what is drawn (D25):

- **Claimed is drawn, not computed, in generation.** The PUF observes the
  claimed amounts for every return, and the law does not make them a
  function of anything else the PUF observes for non-itemizers: they are
  zero because the return did not itemize. So the form of deduction and
  the claimed amounts are model variables, drawn like any other PUF
  variable.
- **Paid is computed from claimed for itemizers.** For a PUF itemizer,
  paid is set equal to the claimed amount. This is exact if the PUF records
  interest paid and real estate tax before any limit. D20 notes that which
  PUF fields are before or after floors and limits has not been checked,
  and this probe did not check it either. The copy itself was checked on
  every PUF itemizer, in dollars and in tokens (job 29534792). Paid is
  never drawn for a PUF record: for non-itemizers it is not known and the
  model does not supply it. (Drawing paid for non-itemizers is D20's
  imputation, a separate job; see the end of this report.)
- **Only two paid variables.** No source in the model observes the paid
  amount of medical expenses, state and local income tax or charity except
  where it equals the claimed amount, so a paid twin for those four would
  carry no information and was not added.

Two arms, each its own training data:

- **`full_v4dd` (the proposed design).** Paid observed on SCF rows and on
  PUF itemizers, unknown on PUF non-itemizers. Risk, from probe C: on PUF
  rows paid is hidden for every non-itemizer and for itemizers only when
  the random training mask happens to hide it, so in generation, where
  paid is hidden until the form of deduction is drawn, the hiding itself
  may read as evidence of a non-itemizer.
- **`full_v4ddp` (paid on SCF rows only).** Paid unknown on every PUF row,
  so its hiding carries no information on PUF rows. The model never sees
  claimed and paid on the same record; the link (paid = claimed for
  itemizers) is applied outside the model, when PUF records are completed.

Both are built from probe C's `full_v3cz` (all of test 4's fixes, the six
deductions zero for non-itemizers) by `dd_prepare.py`; every other variable,
row, split and weight is unchanged.

## What was run

All jobs ran in Slurm on 14 October from 10:19. Outputs under
`$Q = /nfs/roberts/scratch/pi_nrs36/jar335/probes/dd/`, logs under
`../logs/dd_*`, job list in `$Q/jobs.txt`.

| Step | Script (this folder) | Jobs |
|---|---|---|
| Datasets `full_v4dd`, `full_v4ddp` and the completion input `full_v4dd_hide`, with the identity checks | `dd_prepare.py`, `dd_cpug.sbatch` | 29534792 (29534589 cancelled: CPU partitions full) |
| Training, version 2 recipe (`../posttrain/h_train.py` unchanged, `--drop-weight-vars --loss-weight survey`), `full_v4dd` seeds 1–3, `full_v4ddp` seeds 1–2 | `dd_train.sbatch` | 29534590–92, 29534593–94 |
| Test of every chain step on first-epoch checkpoints (4,000 records) | `dd_test.sbatch` | 29535141 |
| One job per model after training: generation of 664,624 whole PUF records, a fresh random order per record (`dd_complete.py`, a copy of `c3_complete.py` with one added order mode), totals from parts (`c3_totals.py`); wealth completion of PUF training records presented as SCF records, five draws, with two inputs; SCF test records with paid observed and hidden (`../posttrain/h_complete.py` unchanged) | `dd_chain.sbatch` | 29534739–41 (`full_v4dd`), 29534742–43 (`full_v4ddp`) |
| Collector: every table below | `dd_collect.sbatch`, `dd_collect.sh`, `dd_compare.py` and `dd_table.py` (copies of C's with the deductions added), `dd_scores.py`, `dd_wealth.py`, `dd_rows.py` | 29535134 (final, after all five chains); interim runs 29537275 and 29539082; 29535103 was a test run on C's and AS's files |

Generation settings: sampling seed 1 per model. For `full_v4dd` the order
mode is `random_paid`: a fresh random order per record, and as soon as a
record has drawn "itemized" and a claimed interest or real estate tax, the
paid twin is set equal to it and becomes visible to the model, as it is for
itemizers in training; for non-itemizers paid stays hidden, as in training.
For model seed 1 a second file leaves paid hidden throughout (plain random
order). Because paid only becomes visible after the form of deduction is
drawn and the random stream is the same, the two files should draw the
same form of deduction for each record (their itemizer shares are equal);
they differ in what is drawn afterwards. `full_v4ddp`
uses plain random order (its paid is never seen on PUF rows).

Completion of PUF training records (wealth drawn five times, SCF tag, 2015)
uses two inputs:

- **claimed shown** (`full_v4dd`): form of deduction and the six claimed
  amounts observed, paid = claimed for itemizers, unknown for
  non-itemizers. This is how probes BR and C completed PUF records (every
  PUF variable visible under the SCF tag).
- **claimed hidden** (`full_v4dd_hide`): the same, but the form of
  deduction and the six claimed amounts hidden, since SCF rows never
  carry them; the model then sees the shared incomes, demographics and,
  for itemizers, paid.

Both arms are completed with both inputs. SCF test records are scored as in
probe C (24 wealth values summed), with paid observed and with paid hidden.

## Results

Numbers are from the final collector's tables (`$Q/DD_TABLES.md`, job
29535134, 11:38). Values are means over training seeds with the range in
brackets. Every job finished; nothing is pending. C's columns are probe C's files, sampling seed
1 only (so C's zero-coded generation column has three files, not C's six,
and differs slightly from C's report); AS's are probe AS's four models.
AGI groups use the real PUF's thresholds.

### Whole PUF records generated from scratch

| | PUF | AS | C, zero coding | C, unknown coding | DD, paid observed for itemizers | DD, paid on SCF rows only |
|---|---|---|---|---|---|---|
| Files | | 4 | 3 | 3 | 3 | 2 |
| Total AGI ($tn) | 8.09 | 7.99 [7.89–8.11] | 7.82 [7.80–7.85] | 12.80 [12.62–13.12] | 7.34 [7.24–7.49] | 8.02 [7.89–8.14] |
| Top 1% share of AGI (%) | 19.4 | 19.4 | 18.8 | 21.6 | 18.5 | 20.0 [19.1–20.9] |
| Top 0.1% share of AGI (%) | 8.8 | 8.6 | 8.3 | 9.4 | 8.5 | 9.1 [8.3–9.9] |
| Itemizers, all returns (%) | 29.8 | 29.1 | 28.8 | 87.9 | 22.6 [22.5–22.7] | 29.4 [29.2–29.6] |
| Itemizers, bottom half (%) | 7.7 | 8.8 | 8.8 | 82.7 | 6.3 | 9.3 |
| Itemizers, 50th–90th (%) | 42.8 | 41.3 | 41.0 | 88.4 | 32.8 | 41.1 |
| Itemizers, 90th–99th (%) | 87.7 | 82.7 | 82.6 | 92.8 | 76.4 | 82.4 |
| Itemizers, 99th–99.9th (%) | 92.6 | 88.0 | 87.9 | 94.5 | 82.7 | 87.4 |
| Itemizers, top 0.1% (%) | 92.8 | 85.6 | 86.2 | 93.2 | 80.1 | 83.4 |
| Top 0.1%: six main items ($bn) | 702 | 642 | 630 | 1,156 | 608 | 709 [624–793] |
| Top 0.1%: AGI less the six items ($bn) | 28 | 61 | 35 | 67 | 35 | 41 |

The C column with unknown coding is plain random order; C's other
unknown-coded file (deductions drawn after the form of deduction) gave
5.7% itemizers.

Claimed deductions by AGI group, $bn (claimed: zero for non-itemizers):

| Deduction | AGI group | PUF | C, zero coding | DD, paid observed for itemizers | DD, paid on SCF rows only |
|---|---|---:|---:|---:|---:|
| Medical | All | 107 | 96 | 76 | 98 |
| | Bottom half | 27 | 25 | 20 | 27 |
| | 50th–90th | 63 | 52 | 42 | 53 |
| | 90th–99th | 16 | 16 | 13 | 16 |
| State and local income or sales tax | All | 274 | 254 | 203 | 257 |
| | Bottom half | 5 | 16 | 12 | 16 |
| | 50th–90th | 71 | 77 | 60 | 79 |
| | 90th–99th | 98 | 93 | 76 | 99 |
| | 99th–99.9th | 50 | 45 | 37 | 46 |
| | Top 0.1% | 50 | 23 | 16 | 17 |
| Real estate tax | All | 152 | 146 | 119 | 148 |
| | Bottom half | 10 | 15 | 12 | 16 |
| | 50th–90th | 65 | 66 | 52 | 66 |
| | 90th–99th | 58 | 51 | 42 | 52 |
| | 99th–99.9th | 14 | 11 | 10 | 12 |
| | Top 0.1% | 4 | 2 | 2 | 3 |
| Interest paid | All | 244 | 234 | 192 | 241 |
| | Bottom half | 16 | 23 | 19 | 25 |
| | 50th–90th | 118 | 118 | 94 | 120 |
| | 90th–99th | 87 | 79 | 65 | 79 |
| | 99th–99.9th | 16 | 12 | 11 | 14 |
| | Top 0.1% | 6 | 2 | 2 | 3 |
| Cash charity | All | 126 | 116 | 97 | 116 |
| | 90th–99th | 39 | 34 | 29 | 35 |
| | 99th–99.9th | 17 | 15 | 13 | 15 |
| | Top 0.1% | 17 | 6 | 7 | 6 |
| Non-cash charity | All | 45 | 43 | 43 | 43 |
| | Top 0.1% | 17 | 3 | 6 | 8 |

With paid on SCF rows only, the model generates the form of deduction and
the claimed amounts as C's zero-coded model does, including its faults:
too much state and local tax, interest and charity claimed in the bottom
half, and too little state and local tax, interest and charity claimed in
the top 0.1% (the top 0.1% has few records, and every model so far misses
there). With paid observed for itemizers, every deduction is about 20%
low, because about a quarter fewer returns itemize, and AGI is also low
in the 50th–99.9th percentiles ($1,918bn against $2,245bn in the
90th–99th, where the other version gives $2,223bn).

Running the main arm's seed 1 model with paid left hidden throughout
(instead of set equal to claimed once a record itemizes) gives the same
22.6% itemizers by construction (the form of deduction is drawn before
paid can be set) and slightly lower income at the top (AGI $7.26tn).

### SCF wealth completion

Nats per record, lower is better; one value per training seed. Test: the
24 wealth values of SCF test records summed, with the other variables
observed; "paid hidden" also hides mortgage interest and real estate tax
paid.

| Model | Validation records | Test records | Test records, paid hidden |
|---|---|---|---|
| Bridge variables (BR) | 34.31, 34.21 | 33.52, 33.45 | 34.72, 34.70 |
| C, zero coding | 34.20, 34.12, 34.11 | 33.46, 33.46, 33.41 | 34.66, 34.66, 34.61 |
| C, unknown coding | 34.13, 34.17, 34.15 | 33.37, 33.43, 33.42 | 34.56, 34.65, 34.62 |
| DD, paid observed for itemizers | 34.10, 34.13, 34.14 | 33.40, 33.37, 33.42 | 34.60, 34.58, 34.60 |
| DD, paid on SCF rows only | not comparable | 33.41, 33.44 | 34.61, 34.67 |

The validation score of the paid-on-SCF-rows-only models is not
comparable: `h_train.py` treats any variable observed on SCF rows and on
no PUF row as wealth, so there it hides and scores paid too. All models
complete SCF records about equally well.

### Completed PUF wealth

PUF training records presented as SCF records, five draws each. "Claimed
hidden" hides the form of deduction and the six claimed amounts and shows
paid for itemizers (equal to claimed); "claimed shown" shows all of them.
SCF benchmark: 2013 and 2016 waves interpolated to 2015, units with wealth
observed, grouped by the seven-income total within each wave (as probes X
and C). PUF groups by true AGI.

| | SCF 2015 | C, zero coding | C, unknown coding | DD, paid for itemizers, claimed shown | DD, paid for itemizers, claimed hidden | DD, paid on SCF only, claimed shown | DD, paid on SCF only, claimed hidden |
|---|---|---|---|---|---|---|---|
| Training seeds | | 3 | 3 | 3 | 3 | 2 | 2 |
| Own a home, all returns | 67% | 28% [28%–28%] | 57% [56%–58%] | 39% [37%–41%] | 56% [55%–57%] | 41% [38%–44%] | 55% [55%–55%] |
| Own a home, bottom half | 49% | 7% [7%–8%] | 37% [35%–38%] | 19% [18%–21%] | 35% [34%–37%] | 20% [18%–23%] | 35% [34%–36%] |
| Own a home, 50th–90th | 77% | 40% [39%–40%] | 73% [72%–74%] | 51% [48%–56%] | 72% [72%–72%] | 55% [51%–58%] | 71% [71%–71%] |
| Own a home, 90th–99th | 92% | 83% [83%–84%] | 92% [92%–93%] | 87% [85%–89%] | 92% [92%–92%] | 90% [89%–92%] | 92% [92%–93%] |
| Own a home, 99th–99.9th | 95% | 90% [90%–90%] | 95% [95%–95%] | 92% [91%–93%] | 94% [94%–94%] | 96% [95%–96%] | 95% [95%–95%] |
| Own a home, top 0.1% | 99% | 91% [91%–92%] | 97% [96%–97%] | 94% [94%–95%] | 96% [95%–96%] | 97% [96%–98%] | 97% [96%–97%] |
| Mean home value, all returns | $184k | $112k [$109k–$115k] | $168k [$160k–$172k] | $132k [$125k–$137k] | $170k [$167k–$176k] | $134k [$129k–$139k] | $170k [$169k–$172k] |
| Mean home value, bottom half | $68k | $16k [$15k–$16k] | $62k [$59k–$64k] | $35k [$32k–$40k] | $62k [$60k–$65k] | $36k [$30k–$41k] | $64k [$63k–$65k] |
| Mean home value, 50th–90th | $178k | $115k [$111k–$120k] | $187k [$176k–$193k] | $139k [$130k–$147k] | $190k [$187k–$196k] | $143k [$138k–$149k] | $190k [$190k–$191k] |
| Mean home value, top 0.1% | $3.0m | $2.5m [$2.3m–$2.6m] | $2.7m [$2.6m–$2.8m] | $2.7m [$2.6m–$2.8m] | $2.7m [$2.6m–$2.8m] | $2.5m [$2.4m–$2.6m] | $2.7m [$2.6m–$2.8m] |
| Have a mortgage, all returns | 46% | 22% [21%–22%] | 42% [41%–44%] | 24% [24%–25%] | 42% [41%–43%] | 26% [23%–28%] | 41% [41%–41%] |
| Have a mortgage, bottom half | 26% | 4% [4%–4%] | 23% [21%–24%] | 6% [6%–7%] | 22% [21%–24%] | 8% [6%–9%] | 22% [22%–22%] |
| Have a mortgage, 50th–90th | 58% | 32% [31%–32%] | 58% [57%–60%] | 35% [34%–36%] | 58% [57%–59%] | 36% [34%–39%] | 57% [56%–57%] |
| Mean mortgage debt, all returns | $67k | $45k [$44k–$46k] | $69k [$66k–$72k] | $48k [$47k–$48k] | $71k [$68k–$75k] | $47k [$45k–$49k] | $69k [$69k–$70k] |
| Mean mortgage debt, bottom half | $20k | $5k [$5k–$5k] | $22k [$20k–$24k] | $7k [$7k–$7k] | $22k [$20k–$25k] | $7k [$6k–$9k] | $22k [$22k–$22k] |
| Mean mortgage debt, top 0.1% | $516k | $596k [$570k–$621k] | $639k [$617k–$661k] | $659k [$629k–$701k] | $676k [$645k–$722k] | $583k [$550k–$616k] | $653k [$624k–$682k] |
| Mean net worth, all returns | $628k | $508k [$495k–$518k] | $559k [$537k–$596k] | $541k [$529k–$551k] | $578k [$566k–$588k] | $525k [$511k–$539k] | $559k [$557k–$560k] |
| Mean net worth, bottom half | $113k | $105k [$105k–$105k] | $153k [$144k–$167k] | $131k [$116k–$146k] | $156k [$149k–$162k] | $140k [$124k–$156k] | $163k [$161k–$164k] |
| Mean net worth, 50th–90th | $370k | $362k [$358k–$370k] | $423k [$407k–$449k] | $386k [$360k–$403k] | $440k [$425k–$455k] | $389k [$378k–$401k] | $438k [$436k–$440k] |
| Mean net worth, 90th–99th | $2.1m | $2.0m [$1.9m–$2.0m] | $2.0m [$1.9m–$2.1m] | $2.0m [$2.0m–$2.1m] | $2.1m [$2.1m–$2.2m] | $1.9m [$1.9m–$1.9m] | $2.0m [$1.9m–$2.0m] |
| Mean net worth, 99th–99.9th | $12.3m | $10.4m [$9.9m–$11.0m] | $10.2m [$9.8m–$10.7m] | $10.7m [$10.3m–$11.2m] | $10.6m [$10.5m–$10.7m] | $9.8m [$9.6m–$9.9m] | $9.8m [$9.7m–$9.9m] |
| Mean net worth, top 0.1% | $54.7m | $38.6m [$37.9m–$39.7m] | $37.8m [$34.0m–$44.5m] | $40.6m [$36.9m–$44.0m] | $38.5m [$35.6m–$40.3m] | $37.2m [$36.9m–$37.6m] | $36.6m [$34.9m–$38.2m] |
| Median net worth, all returns | $91k | $24k [$22k–$25k] | $56k [$56k–$57k] | $31k [$27k–$34k] | $51k [$49k–$52k] | $32k [$27k–$38k] | $50k [$49k–$52k] |
| Median net worth, 50th–90th | $146k | $84k [$82k–$86k] | $131k [$125k–$137k] | $100k [$93k–$111k] | $124k [$122k–$125k] | $106k [$97k–$114k] | $125k [$121k–$129k] |
| Median net worth, top 0.1% | $32.7m | $21.2m [$20.2m–$22.2m] | $20.7m [$19.1m–$22.8m] | $21.8m [$20.2m–$23.1m] | $19.9m [$19.2m–$20.2m] | $20.7m [$20.5m–$21.0m] | $19.2m [$18.8m–$19.5m] |

**By mortgage-interest band.** PUF returns by the interest deduction they
claim (non-itemizers apart); SCF units by the mortgage interest they pay.
The two groupings are not the same population (a PUF band holds only
itemizers, an SCF band every unit paying that much), so net worth by band
is not comparable and is left out; the full band tables are in
`$Q/DD_TABLES.md`. Within the bands
where interest is positive, every model with the bridge variables (BR's,
C's and this probe's) gives at least 96% home ownership and at least 93%
with a mortgage in every band, as in the SCF (98–99% and 96–98%); probe
X's translated model, which has no bridge variables, gives as little as
86% and 62%. The models
differ on non-itemizers (70% of returns):

| PUF non-itemizers | C, zero coding | C, unknown coding | DD, paid for itemizers, claimed shown | DD, paid for itemizers, claimed hidden | DD, paid on SCF only, claimed shown | DD, paid on SCF only, claimed hidden |
|---|---|---|---|---|---|---|
| Own a home | 1% [1%–1%] | 45% [43%–47%] | 18% [16%–23%] | 43% [42%–45%] | 19% [15%–23%] | 42% [42%–43%] |
| Mean home value | $2k [$2k–$3k] | $88k [$83k–$91k] | $37k [$28k–$49k] | $89k [$86k–$92k] | $36k [$26k–$46k] | $91k [$90k–$92k] |
| Have a mortgage | 0% [0%–0%] | 30% [28%–32%] | 4% [3%–5%] | 29% [28%–31%] | 5% [3%–8%] | 28% [28%–28%] |
| Mean mortgage debt | $0k [$0k–$0k] | $34k [$30k–$38k] | $3k [$2k–$4k] | $35k [$32k–$39k] | $5k [$2k–$8k] | $35k [$33k–$36k] |
| Mean net worth | $138k [$132k–$149k] | $221k [$212k–$234k] | $178k [$160k–$188k] | $228k [$221k–$231k] | $181k [$158k–$204k] | $235k [$233k–$236k] |
| Median net worth | $8k [$8k–$9k] | $19k [$17k–$20k] | $10k [$8k–$10k] | $16k [$15k–$17k] | $10k [$8k–$11k] | $16k [$15k–$17k] |

For comparison, all SCF units: 67% own a home, 46% have a mortgage, mean
home value $184k, mean mortgage debt $67k. Non-itemizers should be below
these, since itemizers are disproportionately mortgage holders, but the
SCF has no form of deduction to say by how much; D20's imputation would
have to answer this.

## Whether one model now serves both jobs

Yes, the version with paid on SCF rows only: the same trained model
generates PUF records as well as probe C's zero-coded model and completes
PUF wealth as well as C's unknown-coded model, which were two different
models. It needs the two conditions above. In this version the model never
sees claimed and paid on the same record; the link between them (paid
equals claimed for an itemizer) is applied outside the model when a PUF
record is completed, which fits D25: the model draws what is not
determined, and the rest is computed by rule.

The version the brief proposed, with paid observed for PUF itemizers,
does not serve generation: 22.6% itemizers. The cause is how a missing
value enters the model: it cannot tell "not observed for this kind of
record" from "not yet drawn". Any variable that is unknown for one group
of PUF records and known for another will carry this signal in
generation. That is a general constraint on the data design, not specific
to deductions.

What remains wrong is not about the deductions: the top 0.1%'s net worth
($37–39m against $55m) and median net worth of all returns ($50–51k against
the SCF's $91k, or $56–57k counting split-off units at zero) are as in
probe C.

## What this does not cover

- **Paid for non-itemizers.** No PUF record's paid amount is drawn here.
  Tax-Simulator needs it for anyone who might switch to itemizing (D20).
  In this design that is one more completion: draw paid for PUF
  non-itemizers from the model, given their record, under the condition
  that it be consistent with taking the standard deduction. Not tried.
- **Floors and limits.** Paid = claimed for itemizers assumes the PUF
  amounts are before any limit; not checked (D20).
- **The other four deductions** have no paid variable, since no source in
  the model measures them.
- One sampling seed per model; probe V's scorecard was not run.


## To finish

Nothing. All five training runs, all five chains and the final collector
(job 29535134, finished 11:38) completed without errors; every table above
is final. Job list: `/nfs/roberts/scratch/pi_nrs36/jar335/probes/dd/jobs.txt`;
logs: `/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/dd_*`; full tables,
including every band and every claimed deduction by AGI group:
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/dd/DD_TABLES.md`.

Possible next steps, not run: draw paid for PUF non-itemizers (D20) with
the second-arm model; probe V's scorecard on the second arm's files; a
second sampling seed.
