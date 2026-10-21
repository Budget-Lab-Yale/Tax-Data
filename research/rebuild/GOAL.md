# The end goal

Tax-Data should be a general method for building a synthetic tax file for
any year, historical or projected: 2015 through 2060 and beyond. Every
year's file should have the same variables, mean the same things, and be
produced the same way.

Today's pipeline is different. It mixes variables taken from different
historical years. Each period is aged by its own method. Similar
imputation problems are modelled in different ways.

## How: pretraining and post-training

The design borrows from how large language models are built.

**Pretraining** learns a prior: one large conditional probability object
over tax units. It is trained on microdata from many sources and years:
the PUF, the SCF, and later the CPS, CEX and others. It learns how
variables go together within each source and year, including what the
PUF lacks, such as net worth, tips and consumption.

**Post-training** shapes that object for a target year t, in two steps:
1. **Choose the priors.** Say which mix of source-year relationships year
   t should draw on: for example, the 2015 PUF for tax items and the
   nearest SCF waves for wealth.
2. **Hit the aggregates.** Reweight records drawn from the model so they
   hit aggregate targets, at whatever detail exists for year t. These
   range from detailed IRS tabulations to a single CBO total for wages.
   Some targets are on computed tax values, such as itemizers after TCJA
   or revenue, so post-training needs a tax calculator in the loop.

The output for year t is a PUF-style file, augmented with the
non-tax variables, ready for Tax-Simulator.

## The central difficulty

Pretraining can learn how net worth goes with income only if the variables
the sources share mean the same thing in each source. That covers wages,
interest, dividends, business income, marital status and dependents. They
must match in three ways:
- **Population:** who is covered.
- **Definition:** what is counted.
- **Measurement:** how it is reported.

Surveys miss small holders, misreport losses, and cover non-filers the PUF
never sees. Lining the shared variables up, as an algorithm driven by data
and theory rather than by hand, is the main open problem
(`ROADMAP.md`, item 2).

## What "done" looks like

- **One pipeline.** Train once. Post-train per year from a short
  specification: the year, its prior mix, its targets.
- **Every variable Tax-Simulator needs,** with the same definitions in
  every year.
- **Checked.** Targets are met. Relationships that no target covers stay
  close to their sources. A standing set of benchmarks says, for each
  variable, whether the file is good enough.
- **It replaces `src/main.R`** as the source of Tax-Simulator's input.
