---
title: "Minimal example: why the model-side fixes E7 to E9 failed, and what a model-side fix would have to be"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Why the model-side fixes E7 to E9 failed

John asked on 15 October why the three fixes inside the model failed,
especially E9, which on paper describes the omission mechanism exactly,
and whether a more basic fix exists inside the transformer. This note
reads the code, runs diagnostics on the models already trained (nothing
was retrained), and sets out the options. The data-side rule (decision 15,
`14_TRANSLATION_RULE.md`) is taken as given.

Code written for this note: `16_diag.py` with `16_diag.sbatch`, and
`16_zero_band.py` with `16_zero_band.sbatch`. Output:
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/diag16/<run>/diag.json` and
`diag16/zero_band.json`; logs `logs/16_diag_29690843.*` and
`logs/16_zero_band_29691102.*`. No existing file was changed.

Throughout, "net worth given X" means the model's distribution of net
worth for a record whose shared variables (married, dependents, wages,
interest, dividends, capital gains) are X. A "true amount" is what the
return would show; a "reported amount" is what the survey file shows.

## 1. What E7 to E9 did

All three keep the source label for the two variables one source owns
(net worth, owned by the SCF; other income, owned by the PUF) and remove it
for the six shared variables. Technically, each shared variable is
predicted in a second pass of the same network that is not shown the
source and not shown the owned slots, because their presence would reveal
the source (`02_model.py:387-402`, `predict`). So the model is forced to
learn one distribution of the shared variables for both files.

- **E7** does only that, on the v2 table, where each file's amounts are
  placed on its own percentile scale.
- **E8** adds the "zero or small" treatment of SCF zeros on the same table.
- **E9** uses the E4 table (both files on the PUF's dollar scale) and adds
  a reporting curve to the loss for SCF interest, dividends and capital
  gains. The curve g(b) is the chance that an SCF unit whose true amount is
  in dollar range b reports it. An SCF zero is scored on
  p(0) + Σ p(b)(1 − g(b)); a reported amount in range b on p(b)·g(b)
  (`02_model.py:509-522`). The curve is the SCF's share of units in each
  range over the PUF's, forced to rise with the amount and capped at one
  (`02_model.py:444-486`; values in `model/E9/report_curve.json`).

Generation then draws whole PUF-style records: incomes first, net worth
second (under the SCF label for the scoreboard's "SCF label" column, under
the PUF label for its "PUF label" column), married and dependents last
(`03_generate.py`).

## 2. What the code does, and the problems found

There is no coding error in the reporting-curve likelihood. It is a
proper likelihood: for each SCF record the probabilities of "reported
zero" and of each reported range add to one. It is applied only to SCF
records and only to the slot being scored, as intended. The fitted model
does what the loss asks (section 3, table 3): its single distribution of
interest matches the PUF's zero share (71.1% against 71.8%), and through
the curve it implies an SCF zero share of 86.3% against the actual 88.4%.

The problems are in what the design leaves out and in how wealth is drawn
at generation. In order of how much they matter:

1. **Net worth never sees the curve.** The curve changes only how the
   model predicts the investment amounts themselves. Net worth is
   predicted from the SCF's reported amounts, with the SCF label, exactly
   as in E4: a zero in the input is a reported zero
   (`02_model.py:343-352`, `token_ids`, feeds the reported value; the
   curve in `02_model.py:509-522` touches only the scored target). So the
   wealth of SCF units reporting zero interest is still a mixture of true
   non-holders and holders who did not report, and PUF records with zero
   interest still inherit that mixture. E9's net worth given X is E4's by
   construction. What the curve does buy, one shared distribution of true
   amounts, is something the PUF already provides directly.

2. **Net worth on PUF-style records is drawn from inputs it never saw in
   training.** The scoreboard's SCF-label file draws net worth with the
   SCF label while the record's other income (a PUF-only slot) stays
   visible (`03_generate.py:200`, `OWNER_LABEL = {'label': 'owner',
   'foreign': None}`). In training, net worth was only ever predicted on
   SCF records, which have no other-income slot. The PUF-label file is
   further out still: net worth was never predicted under the PUF label
   (`03_generate.py:209-212`). This convention applies to every run, but
   E7 to E9 react to it far more than E4 does (section 3, table 1). That
   is most of the gap between E9 and E4 on the scoreboard.

3. **Removing the label from the shared variables cuts links the PUF
   needs.** Because the second pass cannot see other income, wages are
   predicted without it. In generation, when other income is drawn before
   wages, wages are drawn as if other income were unknown. The generated
   files of E7 to E9 have 19 to 20% zero-wage records against the PUF's
   17.1%, and only about 30% of their zero-wage records have more than
   $25,000 of other income, against 43% in the PUF (section 3, table 4).
   The E7 notes recorded this cost (0.20 nats worse on wages); the tables
   here show it in the generated records.

4. **Removing the label also forces one population.** Without the label
   the model must give PUF and SCF records the same distribution of
   wages, marriage and dependents given the other shared variables. The
   SCF has 30% zero-wage units, the PUF 17%, mostly because the SCF
   includes units that would not file. That is a difference in who is in
   the file, not in how a variable is reported, and the label is what
   lets the model keep it. E9 removed the label to test a measurement
   model and in doing so also assumed the two files hold the same people.

5. **The curve mixes population into reporting, and ignores who
   reports.** It is the ratio of per-head shares of all units in each
   file (`02_model.py:462-464`). The SCF's extra non-filers have little
   interest, so part of what the curve reads as non-reporting is extra
   people. And the curve depends only on the amount. Given wages and
   demographics, E9 implies an SCF zero share for interest of 83.6%
   against the actual 88.4%; the gap is the part a single curve cannot
   fit (section 3, table 3).

Line numbers refer to `02_model.py` as it stood at 18:38 on 15 October.
Another agent is editing that file for the v4 baseline; the parts cited
are the same as the E9 run's notes describe (`model/E9/NOTES.md`).

## 3. Diagnostics and numbers

All diagnostics load the trained models of E0 (the v2 model), E4, E7, E8
and E9 and draw net worth five times per record. Net worth is in 2015
dollars from the SCF map, as in `09_score.py`. Ratios are to the actual
2016-wave SCF, all units: mean $614,191, median $74,705. Means of net
worth from 20,000 records are noisy because of the top; medians are
steadier.

### Table 1. Net worth given X, separated into "which X" and "what net worth sees"

Each cell: mean ratio / median ratio to the actual SCF. "Generated X" is
the run's own generated PUF-style file (the X of the scoreboard file);
"real PUF X" is the 20,758 PUF test records with their actual values and
PUF weights. "Other income visible" is the scoreboard's convention;
"absent" means net worth is drawn from exactly the inputs it was trained
on (the SCF's slots, SCF label). Married and dependents are hidden in all
cases, as in generation.

| Run | Generated X, SCF label, other income visible (scoreboard) | Generated X, SCF label, other income absent | Real PUF X, SCF label, other income visible | Real PUF X, SCF label, other income absent | Generated X, PUF label, other income visible (scoreboard) | Generated X, PUF label, other income absent |
|---|---:|---:|---:|---:|---:|---:|
| E0 | 1.39 / 1.75 | 1.39 / 1.30 | 1.48 / 1.87 | 1.48 / 1.34 | 1.76 / 1.41 | 1.83 / 1.55 |
| E4 | 0.88 / 1.07 | 0.90 / 0.98 | 0.95 / 1.18 | 0.98 / 1.04 | 0.96 / 0.93 | 1.00 / 1.05 |
| E7 | 1.78 / 2.71 | 1.50 / 1.24 | 1.81 / 2.61 | 1.54 / 1.30 | 1.77 / 3.27 | 1.48 / 2.00 |
| E8 | 1.81 / 2.77 | 1.54 / 1.31 | 1.80 / 2.61 | 1.55 / 1.31 | 1.76 / 3.42 | 1.47 / 2.19 |
| E9 | 1.24 / 2.15 | 1.08 / 1.12 | 1.18 / 1.87 | 1.03 / 1.06 | 1.38 / 3.06 | 1.19 / 2.10 |

The first and fifth columns reproduce the scoreboard to within the noise
of the dollar draw (E9 scoreboard: 1.29 / 2.12 and 1.40 / 3.04).

What the table shows:

- **E9 with net worth drawn from its training inputs is close to E4.** On
  real PUF records, E9 gives a mean of $632,000 and median of $79,000;
  E4 gives $602,000 and $77,000. On its own generated records E9 gives
  1.08 / 1.12, against E4's 0.90 / 0.98. Of E9's median excess on the
  scoreboard (2.15 under the SCF label), almost all goes away when other
  income is not shown to the net worth draw (1.12).
- **E7 and E8 stay high even then** (1.50 to 1.55 on the mean), about at
  E0's level on real PUF records (1.54 against 1.48). That is the v2
  table's problem: their net worth head still reads a PUF record's
  position on the PUF's percentile scale as if it were the same position
  on the SCF's scale. Removing the label from the shared variables cannot
  touch that, because net worth keeps the label and reads the same
  positions.
- **The PUF label is a poor way to draw net worth in E7 to E9** (E9: median
  2.10 even with other income absent), while in E4 it is harmless (1.05).
  In E7 to E9 the source label is seen only by the pass that predicts the
  two owned variables, so in training "PUF label" always came with "predict
  other income" and "SCF label" with "predict net worth".
- **Net worth given the SCF's own records is learned equally well in every
  run.** Drawn for the SCF test records under the SCF label, the model
  means are $580,000 to $598,000 and medians $78,000 to $85,000, against
  the actual $620,000 and $78,000 on those records. The held-out loss for
  net worth on SCF records is 3.78 nats (one variable hidden) in all five
  runs. So nothing was lost in what the model learned on the SCF; the
  failures are in what it is asked on PUF records.

### Table 2. E4 and E9 by wage group, net worth drawn from its training inputs

Mean / median net worth ($ thousand), SCF label, other income absent.

| Wages | Actual SCF | E4, real PUF X | E9, real PUF X | E4, generated X | E9, generated X |
|---|---:|---:|---:|---:|---:|
| 0 | 663 / 97 | 1,317 / 273 | 1,209 / 270 | 1,254 / 268 | 1,148 / 243 |
| $1-25k | 173 / 8 | 233 / 9 | 252 / 7 | 197 / 9 | 227 / 8 |
| $25-50k | 181 / 30 | 195 / 33 | 229 / 33 | 202 / 34 | 235 / 34 |
| $50-100k | 353 / 109 | 424 / 143 | 473 / 145 | 406 / 137 | 492 / 143 |
| $100-200k | 934 / 334 | 863 / 363 | 1,004 / 404 | 880 / 365 | 1,076 / 405 |
| $200k+ | 5,119 / 1,794 | 3,755 / 1,339 | 4,309 / 1,383 | 3,452 / 1,289 | 4,732 / 1,383 |
| Share of units with zero wages | 30.4% | 17.7% | 17.7% | 16.4% | 19.8% |

E9's net worth given X is within a few percent of E4's overall and 10 to
17% higher in the middle and upper wage groups. The README notes that
training noise alone moves the wealth ratio by about 0.1, so these
differences cannot be told from noise without repeat runs. Both runs put
twice the SCF's wealth on zero-wage records, which is mostly the
population difference (the SCF's zero-wage units include non-filers).
E9's generated files have more zero-wage records (19.8% against E4's
16.4%), and those records are the richest group, which accounts for much
of the remaining gap between E9 and E4 on generated records.

### Table 3. The model's chance of a zero, against the actual share

Weighted mean of the model's probability that the item is zero, with only
that item hidden (others visible). For E7 to E9 the PUF and SCF numbers
come from the same unlabelled prediction; the records differ.

| Run | Interest, PUF test (actual 71.8%) | Interest, SCF test (actual 88.4%) | Dividends, SCF test (actual 87.6%) |
|---|---:|---:|---:|
| E0 | 71.4% | 87.5% | 88.2% |
| E4 | 71.2% | 86.3% | 88.3% |
| E7 | 73.4% | 73.9% | 85.4% |
| E8 | 71.6% | 71.1% | 84.1% |
| E9, true amount | 71.1% | 68.1% | 84.2% |
| E9, the same records seen through the curve (implied reported zero) | 87.4% | 86.3% | 90.4% |

With only wages, married and dependents visible, E9 implies an SCF
interest zero share of 83.6% against 88.4%.

This answers the question whether removing the label just averages two
incompatible scales. For the shared variables themselves, yes: E7 gives
SCF records a 74% chance of zero interest where the SCF has 88%, and PUF
records 73% where the PUF has 72%. That is why E7's SCF-style file breaks
($131 trillion of net worth against $79 trillion for E0). But it is not
why PUF-style wealth stays high; table 1 shows that comes from the net
worth head reading the v2 table's positions, plus the drawing convention.

### Table 4. The generated shared variables against the real PUF

| | Real PUF | E0 | E4 | E7 | E8 | E9 |
|---|---:|---:|---:|---:|---:|---:|
| Zero-wage share | 17.1% | 16.8% | 16.4% | 18.9% | 19.0% | 19.8% |
| Zero-wage records with other income over $25k | 42.9% | 43.1% | 43.3% | 29.7% | 30.3% | 31.5% |
| Wages $1-25k with other income over $25k | 7.3% | 7.3% | 7.0% | 9.9% | 9.9% | 9.8% |
| Interest holders, wages $200k+ | 76.5% | 74.1% | 74.0% | 74.0% | 75.8% | 75.2% |

The labelled runs reproduce the PUF's link between wages and other
income; the unlabelled runs do not. Holder shares within wage groups are
fine in every run.

### Table 5. What sits inside the SCF's zero, if the curve is right

From the data alone (`16_zero_band.py`), using E9's curve and assuming
(a) whether a unit reports depends only on its true amount and (b) the
SCF and the PUF hold the same people per head. Mean net worth, all SCF
units, 2015 dollars.

| Item | SCF reported zero share | Implied true zero share (PUF: 71.7 / 81.6 / 86.7%) | Mean, all reported zeros | Mean, implied true non-holders | Mean, implied unreported holders | Mean, reporting holders |
|---|---:|---:|---:|---:|---:|---:|
| Interest | 87.4% | 71.2% | $292,000 | $124,000 | $1,030,000 | $2,840,000 |
| Dividends | 88.0% | 80.7% | $250,000 | $173,000 | $1,096,000 | $3,279,000 |
| Capital gains | 91.6% | 86.3% | $320,000 | $214,000 | $2,044,000 | $3,836,000 |

So under the curve's assumptions, about one SCF unit in six that reports
zero interest is a holder who did not report it, and those units are much
richer than true non-holders. The data-side rule gives every PUF record
with zero interest the wealth of the whole SCF zero group, mixture
included. Done wage group by wage group, giving PUF records with a zero
the wealth of true non-holders instead would lower PUF-style mean net
worth by about $83,000 for interest, $42,000 for dividends and $57,000
for capital gains, each 7 to 14% of the SCF mean. These overlap (often the
same households), so they do not add. One cell fails the assumptions
outright: for wages of $1 to $25,000 the implied true non-holders have a
mean of minus $3,000, which means the curve assigns more of that group's
zeros to non-reporting than the group's wealth allows. That is a sign the
curve is partly measuring non-filers rather than omission.

These numbers are the size of the stake for a model-side fix, not a
result: they rest entirely on assumption (a), which no data we have can
test (`12_CROSSWALK_THEORY.md`, section 4), and on a curve that mixes in
population.

## 4. Why they failed

**Established by the diagnostics above:**

1. E7 and E8 cannot fix the doubling by design. The doubling comes from
   the net worth head reading the SCF's percentile positions; both runs
   keep that head and those positions. On real PUF records, with net
   worth drawn from its training inputs, they give 1.54 and 1.55 times the
   SCF mean, about E0's 1.48.
2. E9's net worth given X is E4's, up to training noise (table 1, real PUF
   records: 1.03 / 1.06 against 0.98 / 1.04). The curve changes how the
   investment amounts are predicted, not how net worth reads them.
3. Most of E9's bad scoreboard row comes from drawing net worth with
   inputs the net worth head never saw in training: other income visible
   (median ratio 2.15 against 1.12 without it) and, for the PUF-label
   column, the PUF label (2.10 even without other income). E4 is barely
   affected by the same convention.
4. Removing the label from the shared variables broke the PUF's link
   between wages and other income in generated records and raised the
   zero-wage share by 2 to 3 points (table 4).
5. The curve does what it was built to do for the investment amounts
   themselves (table 3).

**Likely, not shown:**

- Why E7 to E9 react so much more than E4 to inputs outside training.
  The likely reason is that in E7 to E9 the pass that sees the label
  is trained only on the two owned variables, so its representation of
  "SCF label with other income present" or "PUF label, predict net worth"
  is never constrained. In E4 the labelled pass is trained on every
  variable of both files.
- That the remaining gap between E9 and E4 on generated records (1.08
  against 0.90 on the mean) comes from E9's extra zero-wage records. The
  arithmetic fits (3.4 points more records in a group holding about
  $1.2 million each adds about $40,000 to the mean), but it was not
  isolated.
- Whether E9's 10 to 17% higher wealth in the middle wage groups is real
  or training noise. Repeat runs would settle it.

**The short answer.** E9 put the reporting mechanism in the wrong place.
The wealth problem lives in the link from the shared variables to net
worth, and specifically in what a reported zero means as an input to net
worth. E9 modelled reporting only for the shared variables' own
distribution, which the PUF already supplies, and left net worth reading
reported values exactly as E4 does. Its worse scoreboard row is then the
cost of removing the label (a forced common population, broken links,
and a net worth head that extrapolates badly), not of the curve.

## 5. The foundational options

The setting, in the notation of `12_CROSSWALK_THEORY.md`: a unit has a
true amount X*, net worth Y and other items Z. The PUF records X*. The
SCF records X_S = R·X*, where R = 1 with probability r(X*) (whether a
holder reports). Population differences (non-filers) are separate from r.

What the data-side rule (E4) delivers: for a reported positive amount,
net worth given the reported amount equals net worth given the true
amount, provided reporting does not depend on wealth once the amount is
fixed (call this assumption A). For a reported zero it delivers the
mixture of true non-holders and unreported holders. Any model-side
mechanism is worth building only if it can do better than that at the
zero, or handle cases the data cannot be translated for.

| Option | What it is | What it can identify beyond E4 | Verdict |
|---|---|---|---|
| Source label on shared variables, PUF dollar scale (E4) | The model learns each file's own distribution of reported amounts and net worth given reported amounts | Nothing beyond itself; this is the baseline | Adopted |
| A source-specific output head for each shared variable | Separate prediction layers per file over a shared network | Nothing: the same as giving the label as an input | Equivalent to E4 |
| Reporting curve on the shared variables' own prediction, no label (E9) | One distribution of true amounts, observed through r in the SCF | Only the distribution of true amounts, which the PUF already gives; net worth unchanged | Not useful for this problem; also forces one population |
| Reporting curve learned jointly instead of fixed | r as trainable parameters, possibly depending on wages | Same as a fixed curve when r depends only on the amount. Can let r vary with wages, but r is only identified where the two files hold the same people given the conditioning variables, so a filing rule must come first | A refinement of the next row, not a separate fix |
| Latent true amount feeding everything downstream | X* is a variable of the model; the SCF's reported value is an observation of it through r; net worth and every other variable condition on X*, never on the reported value | Net worth of true non-holders, separated from unreported holders, under assumption A and a known r. This is the one thing the mapping cannot give | The only option that adds something; worth one experiment |
| Latent true amount with a sensitivity parameter | As above, but non-reporters' wealth is allowed to differ from reporters' at the same amount by a stated factor | Nothing more is identified; it shows how much the answer depends on assumption A | The honest way to present the previous row |

How the latent true amount would work inside the masked transformer. The
input for a shared investment amount is always the true amount on the
PUF's dollar scale. On PUF records it is observed. On an SCF record with a
reported positive amount it is observed (under the rule's assumption that
reported dollars are true dollars). On an SCF record with a reported zero
it is unknown, and the model fills it in. The practical method is data
augmentation, also called stochastic EM (Tanner and Wong 1987; Celeux and
Diebolt 1985): at each training pass, for each SCF zero, draw a true
amount from the model's current distribution of that amount given
everything else on the record, net worth included, multiplied by the
chance (1 − g) of not reporting at each amount; then train on the
completed record as if it were observed. Because the draw uses net worth,
a rich SCF household reporting zero interest is more likely to be filled
in as a holder, and the net worth given "true interest zero" that the
model learns is that of true non-holders. The source label stays on every
variable, so the model can still learn that the SCF holds different
people; the curve should be estimated after the filing rule (decision 4
in the README), or conditional on wages, so that it measures reporting
rather than population. An exact alternative sums over every possible
true amount for each SCF zero instead of drawing one; with three items
and 84 ranges each that is too many network evaluations, so drawing is
the workable form. The machine-learning version of the same idea is a
generative model with an explicit model of which values go missing
(Ipsen, Mattei and Frellsen 2021, "not-MIWAE").

Two limits. First, assumption A (non-reporters are as wealthy as
reporters with the same true amount) is untestable without linked
survey-IRS records; the latent model relies on it more heavily than E4
does, because it uses it to subtract the unreported holders out of the
zero. Table 5 shows the subtraction is large (true non-holders at
$124,000 against $292,000 for all reported zeros, for interest), so the
answer will be sensitive to it, and a sensitivity factor is needed to
show by how much (Rubin 1977; Little 1994). Second, the curve itself is
identified only by comparing the two files' distributions, which works
only once they describe the same people; the negative cell in table 5
shows the present curve fails that.

For "a survey zero that hides a real amount", John's example of what the
data cannot fix: the latent model is the version that handles it, and it
is the same mechanism as above. Removing the label (E7 to E9) is not part
of it; that is a separate and stronger assumption (the two files hold the
same people) which is false here.

## 6. Proposed experiments

None of these was run. Training costs are from the E-runs' logs (E4 and E9
about 5 minutes on one GPU, E7 about 8).

1. **Re-score with net worth drawn from its training inputs** (no
   training). Regenerate the PUF-style files of E4, E7, E8 and E9 with
   other income absent when net worth is drawn (03_generate already has
   the option, `foreign='absent'`), and run the full scoreboard,
   including the tilt meter. About 15 GPU minutes and 30 CPU minutes per
   run. This corrects the E7 to E9 rows of `09_RESULTS.md`, and it is a
   decision for John whether "absent" should be the convention from now
   on (for E4 it moves the SCF-label mean from 0.88 to 0.90 and the median
   from 1.07 to 0.98). It also bears on the v4 baseline.

2. **Repeat runs of E4 and E9** (two more seeds each, about 20 GPU minutes
   in all). Needed to say whether E9's net worth given X really differs
   from E4's (table 2) and to put an error band on every row of the grid.

3. **Latent true amount, E15** (new training code; about half a day to
   write and check, about 10 GPU minutes per run because the SCF zeros
   need one extra network evaluation per training pass). E4 table, label
   kept on every variable, start from E4's weights after half the epochs,
   then switch on the fill-in draw for SCF zeros of interest, dividends
   and capital gains, one item at a time. Three seeds, about 40 GPU
   minutes. What to look at: PUF-style net worth among records with zero
   interest, by wage group, against E4; the overall mean should fall by
   roughly the 5 to 14% of table 5 if the model is using the mechanism.
   The SCF-style file's implied true holder shares should approach the
   PUF's in wage groups where the two files hold similar people.

4. **Sensitivity of E15** (four runs, about 40 GPU minutes): the curve as
   in E9; the curve estimated after a filing rule on the SCF; the curve
   by wage group; and the fill-in draw with non-reporters' wealth tilted
   by a stated factor (for example, half and one and a half times
   reporters' at the same amount). This shows how much of any E15 change
   rests on assumption A and on the curve.

Not proposed: E9 with the label kept. With the label, the SCF's own
prediction of reported amounts is already free to fit the SCF, and the
curve would still not reach net worth.

## References

Celeux, G. and J. Diebolt (1985). The SEM algorithm: a probabilistic
teacher algorithm derived from the EM algorithm for the mixture problem.
Computational Statistics Quarterly 2, 73-82.

Ipsen, N., P.-A. Mattei and J. Frellsen (2021). not-MIWAE: deep generative
modelling with missing not at random data. International Conference on
Learning Representations.

Little, R. (1994). A class of pattern-mixture models for normal incomplete
data. Biometrika 81(3), 471-483.

Rubin, D. (1977). Formalizing subjective notions about the effect of
nonrespondents in sample surveys. Journal of the American Statistical
Association 72, 538-543.

Tanner, M. and W. Wong (1987). The calculation of posterior distributions
by data augmentation. Journal of the American Statistical Association 82,
528-540.

Cited from memory; page numbers and the not-MIWAE details were not
checked. The rest of the literature is in `12_CROSSWALK_THEORY.md`.

## Revision history

- 2026-10-15: written; diagnostics run as Slurm jobs 29690843 and 29691102.
