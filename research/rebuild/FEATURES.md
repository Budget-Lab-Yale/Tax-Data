# What the pipeline does now

The current run ("latest") produces a 2019 file of tax units: every PUF
variable, plus the household balance sheet from the SCF (20 items, with net
worth computed from them), weighted to 2019 targets. The code is in
`pipeline/`; every setting is in `pipeline/settings.json`.

## Data

**Sources:**

| Source | Rows | What it covers |
|---|---:|---|
| 2015 PUF | 207,692 | filers |
| 2017 CPS non-filer pool | 166,938 | crude non-filers on the PUF side, dollars put in 2015 terms |
| SCF waves 2013, 2016, 2019, 2022 | 33,058 / 34,377 / 31,830 / 25,600 | tax years 2012–2021, split into tax units by the production builder, five implicates each |

**Variables:**
- marital status (joint or not), dependents (0–3), and the filing flag;
- the primary filer's age range (below);
- wages, taxable interest, ordinary dividends, and capital gains in four
  pieces (short term, long term, distributions, other);
- more AGI items: other gains, taxable IRA distributions, taxable
  pensions, taxable Social Security, unemployment, alimony, state refunds,
  adjustments, and the rest of AGI ("other income");
- two items outside AGI: gross Social Security plus pensions, and
  tax-exempt interest;
- two itemized deductions (PUF only, itemizers only): mortgage interest
  paid and real estate tax. They are kept in 2015 dollars in the 2019 file
  (SOI Table 1.4 has no row to grow them by);
- business income in its 18 PUF parts: Schedule C; partnership and
  S corporation income, losses and section 179, passive and nonpassive;
  rents, royalties, estates and trusts; farm rental; farm. Business income
  is computed from the parts, and so are two subtotals of it: Schedules C
  and F, and Schedule E;
- whether the unit holds a partnership, S corporation or LLC interest
  (on the PUF: any partnership or S corporation part nonzero);
- the balance sheet (SCF only), the 20 items of production's wealth
  schema: 14 assets (deposits; stocks and stock funds; bonds and bond
  funds; IRAs and DC accounts; DB account balances; cash-value life
  insurance; annuities; trusts; other financial; business equity; the
  home; other residential real estate; nonresidential real estate;
  vehicles and other nonfinancial) and 6 debts (home mortgage; debt on
  other residential property; other lines of credit; credit cards;
  installment loans; other debt). Net worth is computed from them. The
  home mortgage is carried as its share of the home's value;
- the 4 unrealized gains (home, other real estate, business, stocks and
  funds) and whether the unit owns any private business (SCF only).

The sources share married, dependents, wages, interest, dividends, capital
gains, the two business subtotals, the partnership/S corporation/LLC flag,
gross retirement income and tax-exempt interest. The rest belong to one
source; on the other source the slot is absent.

**Business items on the SCF.** The SCF's two business income questions
follow the tax return: X5704 is Schedules C and F, X5714 is Schedule E
(where partnership and S corporation income goes). The ownership items come
from the SCF's business section: owns any private business (X3103 or
X3401); a partnership, S corporation or LLC interest (the legal form of
actively managed businesses 1 and 2, X3119 and X3219, or a non-actively
managed one, X3407, X3411, X3451, X3415). Both are household answers; each
goes to the unit that holds the household's business equity, or to the
respondent's unit when that equity is zero.

**Amounts as bands.** Each amount is coded as one of 84 bands:
- band 0 is exactly zero;
- 10 bands of losses;
- 73 bands of positives, finer at the top. The top bands each hold 0.01%
  of positive weight.

A band becomes dollars again by drawing a real record's value from that
band.

**The primary filer's age range** (`pipeline/ages.py`, settings "ages"),
shared by every source. Nine categories: the PUF demographic file's six
ranges (under 26, 26–34, 35–44, 45–54, 55–64, 65+) and its three for
dependent filers (under 18, 18–25, 26+), kept apart.
- **PUF:** the demographic file's range. The 88,269 records with none
  (3.68 million returns, mostly AGI above $400,000) carry probabilities
  instead ("step 0"), and training draws a range from them at the start
  of every pass:
  1. a logit of the range on what the return shows about age (filing
     status, AGI, wages, Social Security, pensions, IRA distributions, the
     early-withdrawal penalty, the standard deduction's 65+/blind
     additions, children at home, other dependents; no capital or business
     income), fitted on the known ranges;
  2. in each SOI Table 1.6 cell (filing status by AGI bracket) where at
     least 10% of the weight has no range, one factor per range scales
     every missing record's probabilities until their weight in each range
     is the table's count less the known-range weight (where the table
     merges under 26 into 26–34, the pair has one target). Below that
     share the residual is mostly sampling noise, and the logit stands.
     30 cells are calibrated, holding 2.68 million of the 3.68 million.
  Tested by hiding the known ranges at AGI $250,000–400,000: among Social
  Security recipients 82% come out 65+ against 86% true (cell shares
  alone: 18%); with long-term gains over $50,000, 46% against 48%
  (`explore/2026-10-21/step0/`).
- **SCF and non-filers:** the range of the primary filer's age. In a
  mixed-sex couple the man is the primary, except the woman with a chance
  that depends on the man's range (22% under 26 down to 4% at 65+), set so
  the SCF's female primaries by range equal the PUF's joint returns
  (27.5% under 26 down to 3.4% at 65+); keyed by household. Same-sex
  couples: the older partner. Ages as the surveys report them, with no
  adjustment for the interview date.

## Translation: putting the SCF on the PUF's terms

- **SCF dollars on the PUF's dollar scale.** Wages, interest, dividends,
  capital gains, retirement income and tax-exempt interest are banded on
  the PUF's cut points, grown by SOI to each wave's year. The SCF's own
  pattern of zeros stays visible, through a source label the model always
  sees.
- **The two business subtotals** are placed by their rank within each SCF
  wave.
- **Top wages.** The SCF reports more high earners than tax data at every
  line (2015: 0.47 million units with $1M+ of wages against the PUF's
  0.15 million), as the literature also finds (Gale, Joshi, Pulliam and
  Sabelhaus 2022: 2.2 times as many units with $1M+ of AGI). SCF wages
  above $100,000 (2015 dollars, grown to each wave) are placed on the PUF
  filers' distribution by count from the top: an SCF unit with N units
  earning more gets the PUF wage with N returns earning more, times one
  constant that keeps the map continuous at $100,000. Below $100,000 they
  are unchanged. Settings: "translation", "wage_top_scaling".
- **The balance sheet** is banded within each SCF wave.
- **Capital losses** below −$3,000 in the SCF are read as −$3,000, the
  deductible limit on the return.
- **Two slots, filled during training.** Many SCF households that report
  zero interest, dividends or capital gains do hold some. Each of these
  items has two slots on SCF rows: what the household reported (never
  changed) and the PUF-concept amount (reporters copy their answer;
  zero-reporters are filled in). The PUF has only the PUF-concept slot.
  - From pass 3, at the start of each pass, each zero-reporter gets its
    chance of a hidden holding in each size range from the reporting-rate
    formula: its own chance of reporting that range (from the rest of its
    record, every filled-in slot hidden), times (PUF share ÷ SCF share − 1)
    for its type (married, dependents, wage band), divided by its chance
    of reporting zero; capped at 100%. Each batch redraws the fill-ins from
    those chances.
  - The formula reads only the reported slots and the PUF, never a fill-in.
  - No cells, top lines or loss floors: a range where the SCF has as many
    holders as the PUF, or more, gets none.
  - It assumes a holder's chance of reporting depends only on its type and
    true amount, not on net worth or anything else on the record.
  - Net worth is drawn given the PUF-concept amounts; the reported slots
    stay hidden at generation.

## The model

- **Network.** A transformer: 4 layers, width 128, about 1 million
  parameters, no dropout.
- **What it learns.** It is trained to fill in any hidden subset of a
  record's variables from the rest. So one network answers every question
  of the form "this variable, given those".
- **Weights.** Survey weights are used as they are, rescaled within each
  source.
- **Training.** 80 passes, all records but a 2% held-back slice. Peak
  learning rate 0.003: in a sweep of 0.001 to 0.01 (two or three seeds
  each, 21 October), it brought the top income group's net worth before
  the tilt close to the SCF's, at a slightly worse fit on the average
  held-back record.
- **Why no dropout.** Dropout made rare items come out 10–20% too low
  (`HISTORY.md`).

## Generation

**One record is drawn slot by slot:**
1. the PUF variables, in a random order;
2. line-13 capital gains, business income and its two subtotals, computed
   from their parts; then the partnership/S corporation/LLC flag;
3. the SCF-only items last, in a random order (the mortgage share after the
   home), as the SCF would see the household (SCF label, PUF-only variables
   left out). Net worth is computed from the balance sheet in dollars.

**Consistency rules** (`settings.json`, "consistency"; each rule is a list
of items that trigger it and a list of holdings of which at least one must
be nonzero, so a new rule is a settings change). When a PUF item implies a
holding, that holding cannot be drawn as zero:

| On the return | Must hold at least one of |
|---|---|
| rental income or loss | other residential real estate, nonresidential real estate |
| any partnership or S corporation item | a private business (it may be valued at zero), other residential or nonresidential real estate |
| mortgage interest paid | a home, and a mortgage on it |
| real estate tax paid | a home, other residential real estate |

Where several holdings would do, the one that must be nonzero is picked
when the first of them comes up, in proportion to the model's own chance
that each is nonzero. Ruling out zero sets the model's chance of zero to
nothing and rescales the rest; the model still decides the size. With no
home, the mortgage share is zero.

**The offset.** The rules add holders and take none away, so without more
each ruled holding ends with more holders than the model alone would draw.
Generation therefore runs three times (`offset_generations`). After each
run but the last, for each ruled holding, one factor on every record's odds
of holding it is found so that forced holders plus the expected holders
among the rest equal the model's own expected holders (weighted). On a
forced record the rule overrides the factor, so in effect only the records
no rule touches are scaled down. The last run is kept; `gen/<run>/
consistency.csv` reports, per run and holding, the model's own expected
holders, the holders with the rules, and the factors.

**The stack.** 400,000 records in four parts. Each part oversamples the
top 1% of one variable (wages, long-term gains, S corporation nonpassive
income, business equity). Weights undo the oversampling exactly.

## Post-training: the 2019 file

**Dollars.** Each band's real 2015 values, grown to 2019 by SOI's growth
in dollars per return holding each item.

**Start.**
- Weights are scaled to SOI's 2019 return count and the non-filer count.
- Filers are then scaled within joint and non-joint returns to SOI's 2019
  counts.

**The tilt.** The smallest reweighting, by Kullback–Leibler divergence,
that meets:

| Targets | Number | How they are met |
|---|---:|---|
| IRS (SOI 2019 Table 1.4): returns, and the number with and amount of wages, interest, dividends and capital gains, in 15 AGI brackets; plus joint returns | 136 | exactly |
| Non-filers | 1 | exactly |
| IRS business: six families, net income and net loss, returns and dollars, in 6 AGI groups | about 140 | exactly, or within 2% if that fails |
| SCF 2019 wealth: units and net worth above the 99th, 90th and 50th percentiles, total net worth, units above $1 million; counts on the file's own units | 8 | within 2% |

## How the latest run comes out

**Since 21 October (night), "latest" also rescales the SCF's top wages**
(above); three seeds; the age-range runs moved to
`previous_2026-10-21_ages/`. Against them, after the tilt: mean net worth
at wages $200k+ $3.67–3.73M ($3.40–3.56M), median $1.53–1.56M
($1.37–1.44M); effective sample kept 88.5–89.0% (88.4–89.6%). Median net
worth at 65+ with $200k+ of wages barely moves: $2.9M against the SCF's
$6.5M (`ROADMAP.md`, known gaps). Full tables:
`explore/2026-10-21/wage_scale/compare.md`.

**Before that, "latest" added the primary filer's age range** to the
two-slot run, three seeds (20261016–18); the two-slot runs
moved to `previous_2026-10-21_two_slot/`. Against them, after the tilt:
median $90.9k for both; tilt move 0.05–0.06 for both, effective sample
kept 88.4–89.6% (89.1–89.8%); mean net worth at wages $200k+
$3.40–3.56M ($3.47–3.57M). Generated filers' ranges match the PUF's
(before the tilt, 11.2% under 26 and 16.4% 65+, against 11.1% and 16.2%).
Full tables: `explore/2026-10-21/ages_v1/compare.md`. Net worth by age
within wage band against the SCF is in the known gaps (`ROADMAP.md`).

**Until then, "latest" was the two-slot run**, three
seeds (20261016–18), in `model/`, `gen/` and `tilt/`; the cell-method runs
described below moved to `previous_2026-10-21_cell/`. Two-slot against cell
method, after the tilt, all three seeds: median $90.9k for both; mean net
worth at wages $200k+ $3.47–3.57M (cell $3.38–3.68M); wages $1–25k
$183–185k (cell $180–192k); tilt move 0.05–0.06 and about 90% of the
effective sample kept for both. Full tables:
`explore/2026-10-21/two_slot/compare.md`. The text below describes the
cell-method runs.

The balance-sheet run of 21 October, three seeds (20261016–18), outputs
under `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/`; full tables in
`explore/2026-10-21/compare_latest.md`. The previous latest (one net worth
item) is kept in `previous_2026-10-20/`. Nominal 2019 dollars; ranges are
over the three seeds.

**With the offset** (the current latest; the run without it is kept as
`gen/` and `tilt/seed<n>_no_offset/`). After the last of the three
generations every ruled holding is within 0.02% of the model's own expected
holders. After the tilt:

| | With offset | Without | SCF 2019 |
|---|---:|---:|---:|
| home mortgage holders | 38.6–40.1% | 45.1–46.8% | 41.6% |
| homeowners with a mortgage | 65.1–67.0% | 72.1–74.2% | 65.6% |
| home mortgage dollars | $11.1–11.6 trillion | $12.3–12.8 trillion | $9.8 trillion |
| other residential real estate holders | 11.4–12.9% | 15.7–17.7% | 12.7% |
| nonresidential real estate holders | 5.4–5.7% | 6.7–7.1% | 6.5% |
| homeowners | 59.4–60.0% | 62.5–63.1% | 63.4% |

Shares of units are not fully comparable: the file has 187 million units
(returns plus non-filers), the SCF 143 million, and the extra units are
mostly dependent filers and non-filers who rarely hold these assets. The
offset fixes counts, not sizes: mortgage dollars stay above the SCF's.
Every IRS and business target is met exactly; 89.5–90.1% of the effective
sample kept.

**Without the offset** (the figures below):

**The tilt:** every IRS target and all 140 business targets met exactly in
every seed; 89.8–90.8% of the effective sample kept.

**Net worth before the tilt:** $96.5–107.5 trillion (SCF $96.1 trillion).

**After the tilt:**

| | File | SCF 2019 |
|---|---:|---:|
| median net worth | $90,900–91,000 | $91,050 |
| mean net worth, income under $25k | $125,000–131,000 | $78,900 |
| mean net worth, income $1 million and over | $16.0–18.3 million | $26.2 million |
| homeowners | 62.5–63.1% | 63.4% |
| home mortgage holders | 45.1–46.8% | 41.6% |
| home mortgage dollars | $12.3–12.8 trillion | $9.8 trillion |
| mortgage above the home's value (% of mortgagors) | 2.9–3.3% | 1.5% |
| other residential real estate holders | 15.7–17.7% | 12.7% |
| business equity holders | 12.1–12.6% | 11.4% |
| stocks outside retirement, holders | 16.2–17.3% | 19.2% |
| dividends over $1,000 with no stocks (no rule) | 18.8–21.0% | 18.7% |

Every consistency rule holds on every record (0% breaks).

**What the rules add.** Of the draws each rule forced, the share the model
alone would have drawn as zero, as a share of all holders in the stack
(unweighted draws): home about 6%; home mortgage about 10%; other
residential real estate about 25%; nonresidential real estate about 24%;
owning a business about 0.1%. Nothing takes these holders back elsewhere, so
mortgages and other real estate come out above the SCF.

The previous latest (one net worth item, 20 October), after the tilt:
median $90,850, income $1 million and over $17.2 million, income under $25k
$121,400; 90% of the effective sample kept.

Known gaps are listed at the end of `ROADMAP.md`.
