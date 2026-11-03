# The check tool: the posttrained file against its sources

This tool lets you check the posttrained file of one year (2015 for now) against the data it was built from. You pick
an item Y and a variable X, and the tool shows Y by X in two tables side by side: one from the item's source data and
one from the posttrained file. Each table shows, for each X group, the share of units with a nonzero Y, the weighted
total and the mean per unit of Y, and each group's share of the total. The page runs in your web browser; the
numbers are computed by a small server that runs in a Slurm job on Bouchet. The server sends tables only, never
records.

## Starting it, step by step

You need a Bouchet account, the PUF license, and read access to the group folder `pi_nrs36` (the tool's data sit in
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/check_tool_cache/`, readable by the group only). If a step below says
"permission denied", ask John to check that your account is in the group.

1. Log in to Bouchet: in Open OnDemand (https://ood-bouchet.ycrc.yale.edu), choose Clusters, then Bouchet Shell
   Access; or by SSH from your own computer, `ssh <your NetID>@bouchet.ycrc.yale.edu`.
2. Go to your home folder (`cd ~`) and submit the server's job:

       sbatch /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data-tabtool/research/rebuild/tools/check_tool/start.sbatch

   Slurm answers `Submitted batch job <job id>`.
3. Wait until the job runs: `squeue --me` shows `R` in the state column. This usually takes under a minute, but can
   take longer when the cluster is busy. The server then needs a few seconds to read its tables.
4. Read the job's output: `cat check_tool_<job id>.out`. It shows the node the server runs on, its port, the token
   (a random string made anew each time the server starts) and the three addresses below with these filled in. The
   file is readable by you alone; do not share the token.
5. Open the page by one of the three ways in the next section.
6. When you are done, stop the server: `scancel <job id>`. If you forget, it stops by itself after 8 hours.

The page must be opened with the address that ends in `?token=...`. Without the token the server refuses every
request, including the page itself.

## Opening the page

**1. Open OnDemand's Remote Desktop.** In Open OnDemand choose Interactive Apps, then Remote Desktop, and start a
session (one core and a few gigabytes of memory are enough). In the desktop, open the web browser and paste the first
address from the job's output, `http://<node>:<port>/?token=<token>`. This uses only YCRC's documented apps and keeps
the page inside the cluster.

**2. Open OnDemand's node proxy.** Open OnDemand can pass a web page from a compute node through the portal, at
`https://ood-bouchet.ycrc.yale.edu/rnode/<node>/<port>/?token=<token>` (the second address in the output). YCRC's
documentation does not say whether this is switched on for Bouchet's compute nodes; it was not tested, because it
needs a browser signed in to the portal. If the address gives an error page from the portal, use the first or the
third way. The tool works under the proxy's address if the proxy is on: the page uses only relative links.

**3. An SSH tunnel from your own computer.** This is YCRC's documented way to reach a server in a job (their guide
for Jupyter over SSH). On your own computer, in a terminal, run the command from the output:

    ssh -N -L <port>:<node>:<port> <your NetID>@bouchet.ycrc.yale.edu

It asks for your password and Duo and then shows nothing; leave it running. Then open
`http://localhost:<port>/?token=<token>` in your browser. Close the tunnel with Ctrl-C when you are done.

## What each control means

The controls are applied the same way to both tables: the source's and the file's.

**Y, the item tabulated.** Every item the tool can compare, grouped by its source. The box under the choice says
which source the table uses and how its sample records are counted.
- PUF items (wages, interest, capital gains, business income, deductions, credits and the rest) are compared with
  the PUF source (below, "The sources").
- Items that only a survey has are compared with that survey: the balance sheet, unrealized gains, auto loan interest
  and business ownership with the SCF; the eight consumption categories and their total with the CEX; overtime,
  childcare, weeks and hours worked and the spouses' shares with the CPS; tips with the SIPP. For these items, X and
  the filters offer only the variables the network was shown for that survey when it drew the item: those present on
  that survey's records in training. Net worth by AGI, for instance, cannot be asked, because the SCF has no AGI.
- A yes-or-no item (owns a business, say) is shown as the share answering yes and the number of units answering yes.
- Overtime and tips are compared as drawn, before the file caps each person's amount at that person's wages
  (overtime: wages less tips), because the surveys hold the uncapped amounts.

**Units.**
- *All units:* every record.
- *All units except dependent filers:* returns filed by someone claimed as a dependent are left out of the file
  (and of the PUF source). The surveys count a dependent inside the unit that claims them, so this is the choice to
  use with survey items, and the page makes it the default for them.
- *Filers only* and *non-filers only:* offered only where the source says who files: the PUF source (its PUF
  returns are the filers, its CPS units the non-filers) and the CPS (its filing flag). Use *filers only* to compare
  the PUF's own returns alone.

**Leave out the file's Forbes-list records.** The file holds the Forbes list's billionaires at weight 1; the SCF
leaves out the Forbes 400. Tick this to compare the file without them. The notes under the table say how many
Forbes-list records the file's table holds.

**Filing status.** Keeps one filing status (single, married filing jointly, married filing separately, head of
household). Offered where the source has filing status (the PUF source and the CPS).

**Add a filter.** Each filter keeps the units whose value of a variable is greater than a number, less than a number,
or between two numbers (both ends included), or, for a variable with categories, in one category. Several filters
apply together: a unit must pass all of them. A unit whose filter variable is missing (a spouse's age where there is
no spouse, say) fails the filter, and the notes under the table say how many records that left out in each file.
Age: on the PUF source only the age range exists (the PUF has no exact ages); on the surveys and the file the
primary filer's and the spouse's exact ages exist too, 80 meaning 80 and over.

**X, the groups.** None (one row for every unit shown), a variable with categories (one row per category), or a
number (dollars, years, weeks) cut into groups in one of two ways:
- *Percentiles:* type the percentiles that bound the groups, for example `50, 90, 99, 99.9` for the groups below
  the median, the median to the 90th percentile, and so on. Two definitions, chosen by a switch:
  - *(a) each file ranked within itself:* each file's units are ranked by X in that file, and a unit's place is the
    share of the weight below it plus half its own. The p90-p99 group is each file's own 90th to 99th percentiles,
    whatever their dollars. Units tied at one value (many units have zero wages) are split between groups by their
    order in the file. The column "X range in the group" shows each group's lowest and highest X in each file.
  - *(b) cutoffs taken from the source, applied to both files:* the source's percentiles of X are found in dollars
    (the smallest value at which the weight share below reaches the percentile), and both files are cut at those
    dollars. The notes list the cutoffs. A unit exactly at a cutoff goes to the group above it, so groups bounded by
    two equal cutoffs (both zero, say) are empty.
  
  And a second switch: percentiles of *the filtered units* (ranks or cutoffs computed among the units that pass the
  filters) or of *all units*, meaning all units of the Units choice above, before the filing status and other
  filters.
- *Cutoffs I type:* the group boundaries in X's own units, for example `0, 25000, 50000, 100000`. Groups are below
  the first cutoff, from each cutoff to below the next, and the last cutoff and over.

Units whose X is missing get a row of their own at the bottom, never dropped.

**Make the table.** Sends the choices to the server and shows the table. The status line says how long it took;
the server takes a tenth to a third of a second.

## Reading the table

Each row is an X group, then a row for all units shown. For each file:

| Column | What it is |
|---|---|
| sample records | how many of the file's own sample records the cell rests on (below, "What a record is") |
| units (millions) | the weighted number of units in the group, on that file's own weights |
| share of units | the group's units over all units shown |
| share with a nonzero value | weighted share of the group's units whose Y is not zero (for a yes-or-no item, the share yes) |
| total ($ billions) | the weighted sum of Y in the group |
| mean per unit | the total over the group's units (all of them, zeros included) |
| share of the total | the group's total over the total of all units shown |
| records missing Y | shown only when Y is missing on some records (CPS overtime, which only part of the sample is asked); those records are left out of the share, the total and the mean |
| X range in the group | the lowest and highest X in the group, for a number cut into groups |

**Thin cells.** A cell resting on fewer than 30 sample records is shaded, and a row is marked "(thin)" when either
file's cell is thin. The threshold is the setting `thin_cell_records`.

**What a record is.** Sample records are counted as each file's own sample units: for the file, its records; for the
PUF source, PUF returns and CPS non-filer units (a PUF return split over age ranges, below, counts once); for the SCF,
households (its five implicates and the several tax units of one household count once); for the CEX, consumer units
in a survey year (their quarterly interviews count once); for the CPS and the SIPP, tax units.

## The sources

The year is 2015, and the file is the posttrained 2015 file of the 2 November integration run:
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-11-02/integration/run/2015/tilt/y2015/file_2015.parquet`.

- **PUF items: the PUF and the non-filers.** Every 2015 PUF return, dependent filers included, on its own weight
  and in its own dollars, plus the non-filer units of the pipeline's "tax data for 2015" (every CPS 2015 unit,
  weighted by its chance of not filing, its amounts in PUF terms). This is the setting `puf_source =
  puf_and_tax_data_nonfilers`. The PUF's amounts are put on the file's definitions by the pipeline's own steps:
  capital gains in AGI hold a net loss to $3,000, "other income in AGI" folds in every AGI item but wages, interest,
  dividends and the capital gain, and AGI is their sum. AGI computed this way equals the PUF's own AGI (E00100) on
  every return. A return with no age range on the PUF (88,269 returns, mostly at high incomes) is split over the
  ranges by the pipeline's step 0 probabilities, as the tax data does. The non-filers carry zero for the 38 PUF
  items the CPS does not ask (deductions, credits, adjustments and the like), as the file's non-filers do.
- **SCF items: the SCF** over 2015's mix of waves (the 2016 wave 81.76%, the 2013 wave 18.24%), the same frame the
  SCF targets use: the balance sheet grown per tax unit to the year's wave, weights scaled to the year's units.
- **CEX items: the CEX** over 2015's mix of survey years (2015 73.11%, 2017 26.89%), the frame the consumption
  targets use: every tax unit, a household's spending divided among its tax units by head count, in 2015 dollars.
- **CPS items: the CPS** over the same mix, every tax unit, filers and non-filers.
- **SIPP items: the SIPP** over the same mix, every tax unit.

On the surveys, the income items (wages, dividends, Social Security and the others the survey shares with the PUF)
are in PUF terms after the translation's first step (each survey amount mapped to the PUF's, "function 1"), as the
training table holds them, and grown per return to 2015. They are before the fill-in of holders the survey misses,
which happens during training; so a survey's share of units with, say, dividends is lower than the file's, by design.
The SCF's capital gain is its own concept: gains before the $3,000 loss limit, distributions included; the file's
matching column is `capital_gains_all`.

## For John: settings and rebuilding

Settings are in `settings.json` next to this file:
- `year` and `runs`: the year checked and its run folder. A new year needs a run folder in `runs` and a rebuild.
- `puf_source`: the source definition for PUF items, one setting. `puf_and_tax_data_nonfilers` is the default
  above; `tax_data` is the pipeline's tax data for the year exactly (no dependent filers, weights scaled by SOI
  Table 1.6, dollars grown per return), the only one defined for a year other than 2015.
- `thin_cell_records` (30), `default_percentiles`, `default_dollar_cutoffs`, `port_range`, `cache`.

After a change to `year`, `runs` or `puf_source`, or a new file, rebuild the tool's tables:

    sbatch research/rebuild/tools/check_tool/prepare.sbatch

`prepare.py` runs the pipeline's own code (with the pipeline's modules, a few minutes) and writes the tables to
`<cache>/<year>_<puf_source>/`, with `checks.md` (the checks it ran: the tax data's filers against the PUF's returns
row for row, AGI against E00100, the weights) and `catalog.json` (the variables, their plain names, which source each
item uses and which variables each source may be cut by). The server (`server.py`) needs only the Python/3.12.3 and
Arrow/17.0.0 modules and reads those tables at start, in about a second.

A 2019 view against SOI's published tables is not built. It would be one more source in `prepare.py` (the published
cells as a table) and one more kind of item on the page.

## Security

The data are licensed PUF microdata, and other Bouchet users share the nodes and could reach the server's port.
So every request must carry the token, compared in constant time; a request without it, or with a wrong one, is
refused (tested). The token is new at every start, printed only in the job's output, which is made readable by you
alone, and never written to the server's log. The server answers with tables of weighted sums, means, shares and
record counts, never with records. The page loads nothing from the internet.
