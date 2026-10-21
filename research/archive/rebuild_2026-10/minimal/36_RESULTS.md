---
title: "Step 36: the 2019 tilt with business income targets"
role: evidence
workstream: rebuild
status: open (target rule not decided)
updated: 2026-10-19
---

# Step 36: the 2019 tilt with business income targets

John, 19 October. Can the tilt absorb the generated files' shortfall in
business income, at what cost, and how many business targets, built how?
The rule is not decided. John: "we are going to need to sort this out based
on vibes and judgment later". This note records what was run and what it
showed, so the decision can be made from it.

Scripts: `36_business_targets.py` (the SOI rows and the definition check),
`36_tilt.py` and `36_tilt.sbatch` (the tilt). Outputs in
`$R/step36/business_targets.md` and `$R/tilt/step36/s34_<run>/tables.md`.

## What SOI publishes

SOI Pub 1304 Table 1.4, by size of AGI, 2015 and 2019 (shared raw-data
mirror). For six families: returns and dollars with a net income, and with
a net loss. The families are Schedule C, partnership and S corporation
together, rent and royalty, estate and trust, farm, and farm rental. That
makes 24 numbers per AGI group.

SOI does not split partnerships from S corporations, passive from
nonpassive, or publish the section 179 deduction.

**Disclosure merges.** SOI blanks thin cells and merges them into a
neighbour, flagged "combined". The non-blank cell holds its own bracket and
the blank ones after it. For example, 2019 farm net income from $500k up is
one number, and 2019 estate and trust losses from $1 to $15k are one
number. AGI groups are merged where such a cell crosses a group edge.

## Definitions check (2015 PUF against SOI 2015)

A return counts as having net income (loss) in a family when the signed sum
of the family's fields is above (below) zero. That gives one net figure per
return. This "net" reading matches SOI 2015 counts within 0–5% for every
family. Counting income and loss fields separately overcounts partnership
and S corp losses by 90%.

The amounts also match within 0–10%, except estate and trust losses. Their
PUF amount is half of SOI's, on about 58,000 returns.

## The tilt with business targets added

The usual tilt is the 136 IRS targets and the non-filer count exact, with
the 8 wealth targets soft at 2%. The business targets are added exact,
falling back to soft at 2% if the solver fails. Both runs are step-34
stacks with dropout in training, chosen because they are furthest from the
PUF:
- **Ns1:** too few holders.
- **HL1:** business income at 78% of the PUF's.

| Business targets | Ns1: effective sample kept | HL1: effective sample kept |
|---|---:|---:|
| none (usual tilt) | 94.9% | 95.1% |
| 24 totals | 93.8%, exact | 94.6%, exact |
| 6 AGI groups (140) | 93.2%, exact | 93.9%; all hit except estate and trust losses (amount 10% short) |
| 15 brackets (344) | 92.6%; not converged, some weights pushed to zero | 93.4%, soft |

The 6 AGI groups are: no AGI, under $50k, $50k–100k, $100k–200k,
$200k–1m, and $1m and over.

- **The usual tilt alone leaves business income well off.** On Ns1, after
  it, Schedule C net income is 0.87 of SOI's amount and its losses 0.70;
  partnership + S corp income is 0.86; rent losses are 1.30; estate and
  trust loss returns are 2.45.
- **Totals alone leave errors by income.** On Ns1, Schedule C income at
  $1m and over is 1.61 of SOI's, and partnership + S corp income is 0.78
  under $50k. The 6 groups fix these for 0.6 points of effective sample.
- **The wealth targets stay met in every version.**

## What the business targets leave untargeted (Ns1, 6 groups, filers)

| | Tilted | PUF 2015 |
|---|---:|---:|
| Partnership nonpassive income holders | 1.59% | 1.60% |
| S corp nonpassive income holders | 2.00% | 2.14% |
| Partnership nonpassive loss holders | 0.97% | 1.14% |
| S corp nonpassive loss holders | 0.72% | 0.92% |
| Both partnership and S corp items | 1.05% | 0.78% |
| Passive share of partnership + S corp income | 14.5% | 12.8% |

The splits SOI does not publish are left to the model. The model has too
few nonpassive loss holders, too many returns holding both kinds, and a
somewhat high passive share. Rent and estate loss holders fall below the
2015 PUF's because SOI 2019 has fewer of them than 2015; that is a real
change, not an error.

## Constructed targets against the published 2019 numbers

These test how good a built target is, using items where the truth is
known. Totals are built from the 2015 PUF and compared with SOI 2019:
- **Naive:** counts times return growth; amounts times return growth and
  growth in AGI per return.
- **Tilted:** the 2015 PUF with dollars grown, reweighted to the 136 core
  2019 targets.

| Ratio to SOI 2019 | Naive | Tilted |
|---|---:|---:|
| Schedule C income: returns / amount | 0.98 / 1.03 | 0.95 / 0.98 |
| Partnership + S corp income: returns / amount | 1.04 / 1.01 | 1.03 / 1.04 |
| Rent income: returns | 1.08 | 1.08 |
| Schedule C loss returns | 0.86 | 0.86 |
| Partnership + S corp loss amount | 0.74 | 1.26 |
| Rent loss returns | 1.25 | 1.32 |
| Farm income returns | 1.29 | 1.27 |
| Estate loss returns | 1.42 | 1.53 |

Large income items come out within about 5%. Losses and small families are
off by 15–50%. Reweighting to the core targets does not beat naive aging.

## Open, for John's judgment

1. **Level of detail.** About 6 AGI groups looks like the sweet spot. Totals
   leave errors by income; 15 brackets do not converge on thin cells.
2. **Thin cells.** Estate and trust losses, and other thin cells, could be
   kept exact, made soft, or dropped.

   An example from 19 October, the no-dropout model D0s2 at 6 AGI groups:
   - SOI 2019 counts 5,935 returns with estate and trust losses at AGI of
     $1m and over, $2.03bn in all, about $342,000 each.
   - D0s2 generates about that many returns (0.92 of the count), but with
     tiny losses: 0.03 of the dollars.
   - The exact tilt fails. With the 2% tolerance it reaches 0.75 of the
     amount and pushes a few weights to zero.
   - D0 and D0s3 hit the cell exactly.

   Possible rules:
   - make cells soft below a size, for example under about 10,000 SOI
     returns, or under about 50 records in the file;
   - target thin families only in total;
   - keep the automatic fallback to soft.
3. **Building targets where SOI publishes nothing.** The evidence above says
   such targets are good to about 5% for large income items and poor for
   losses and small items. Options: soft targets with a tolerance near the
   construction error, or no target, with the model's split checked as an
   untargeted diagnostic.
4. **The untargeted splits** (nonpassive losses, co-holding, passive share)
   depend on the pretrained model. Whether they matter enough to target,
   given point 3, is open.
