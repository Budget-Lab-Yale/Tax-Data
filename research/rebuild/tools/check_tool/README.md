# The check tool: the posttrained file against its sources

This tool lets you check the posttrained file of one year (2015 for now) against the data it was built from. You pick
an item Y, a variable X and one statistic (the share of units with a nonzero Y, say, or the weighted total of Y), and
the tool shows that statistic for each X group in a short table, with three columns: the item's source data, the
posttrained file, and the difference between them. A plot of the same numbers is drawn under the table.

The tool is a Jupyter notebook, `check_tool.ipynb`, that you open in Open OnDemand's Jupyter app. The notebook runs in
your own Jupyter session on Bouchet and reads the tool's tables there; nothing else needs to be started, and no
address, port or password has to be typed.

## Starting it, step by step

You need a Bouchet account, the PUF license, and read access to the group folder `pi_nrs36` (the tool's data sit in
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/check_tool_cache/`, readable by the group only). If the notebook says
"permission denied", ask John to check that your account is in the group. From off campus, connect to the Yale VPN
first.

1. Open https://ood-bouchet.ycrc.yale.edu in your web browser and log in with your NetID and Duo.
2. In the menu at the top, choose **Interactive Apps**, then **Jupyter**. A form opens.
3. Fill in the form:
   - The environment drop-down (YCRC's documentation calls it the version or the Conda environment): leave it at
     `ycrc_default`. The notebook does not use this environment's packages, but the form needs one. If you have
     never used the Jupyter app before, the portal builds `ycrc_default` for you the first time, which can take a
     few minutes; this is the portal's own standard setup, not something you install.
   - **Additional modules** (the field for modules to load with Jupyter): type exactly

         Jupyter-bundle/20250530-GCCcore-13.3.0 Arrow/17.0.0-gfbf-2024a

     These are two of YCRC's own software modules: Jupyter with its interactive controls, and the library that reads
     the tool's tables. They are what the notebook runs on.
   - **Partition:** `devel`.
   - **Number of CPU cores:** 2.
   - **Memory:** 8 GiB. (The notebook uses about 3 GiB.)
   - **Number of hours:** 2, or up to 6, the most `devel` allows.
   - **Start JupyterLab:** either way works.
   - Leave every other field as it is.
4. Click **Launch**. The page shows the session as queued, then as running, usually within a minute. Then click
   **Connect to Jupyter**. Jupyter opens in a new tab, showing the files in your home folder.
5. Open the notebook: click your way through the folders `project_pi_nrs36`, `jar335`, `Repositories`,
   `Tax-Data-tabtool`, `research`, `rebuild`, `tools`, `check_tool`, and open `check_tool.ipynb`.
6. Run it: in the **Run** menu, choose **Run All Cells**. After about five seconds the controls appear under the code
   cell, with a first table and plot (the share of units with wages, by AGI) below them.
7. Change any control and the table and the plot are made again, in under a second. After typing in a box (percentiles, cutoffs,
   a filter's number), press Enter.
8. When you are done, close the tab, go back to Open OnDemand, choose **My Interactive Sessions** and click
   **Delete** on the Jupyter session. (Closing the tab alone leaves the session running until its hours run out.)

The notebook saves your last table into the file when Jupyter saves it. That does no harm; the next Run All starts
afresh.

### If the controls do not appear

- If the notebook prints "This notebook cannot start: Python cannot find ...", the Additional modules field was
  empty or mistyped. Delete the session (step 8) and start a new one with the field filled in exactly as in step 3.
- If the cell shows a line of text such as `VBox(children=...)` or "Loading widget..." instead of the controls, the
  Jupyter app started its Jupyter from the environment instead of from the modules, and that Jupyter cannot draw the
  controls. Use the Remote Desktop way below instead, and tell John, so he can ask YCRC about the Jupyter app.

**The Remote Desktop way.** In Open OnDemand choose Interactive Apps, then **Remote Desktop**, with the same partition,
cores, memory and hours as above, and click Launch, then Launch Remote Desktop. In the desktop, open a terminal (from the
desktop's menu) and type these two lines:

    module load Jupyter-bundle/20250530-GCCcore-13.3.0 Arrow/17.0.0-gfbf-2024a
    jupyter lab ~/project_pi_nrs36/jar335/Repositories/Tax-Data-tabtool/research/rebuild/tools/check_tool/check_tool.ipynb

JupyterLab opens in the desktop's web browser with the notebook. Continue from step 6. If no browser opens, the
terminal shows an address starting with `http://localhost:8888/lab?token=`; open the desktop's web browser from its
menu and paste that address. When you are done, close the browser, press Ctrl-C twice in the terminal, and delete the
desktop session in My Interactive Sessions.

## What each control means

The controls are applied the same way to both tables: the source's and the file's.

**Y, the item tabulated.** Every item the tool can compare. Each item's name starts with its source (for example
"SCF: net_worth: net worth"), and the list runs through the PUF items first, then the SCF, CEX, CPS and SIPP items.
The box under the choice says which source the table uses and how its sample records are counted.
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
  use with survey items, and the tool makes it the default for them.
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

**The statistic shown.** One statistic at a time, chosen from the drop-down:
- *share of units with a nonzero value:* the weighted share of the group's units whose Y is not zero;
- *number of units with a nonzero value,* in millions, on each file's own weights;
- *weighted total:* the weighted sum of Y in the group, in $ billions (for an item not in dollars, in millions of its
  unit: weeks, hours);
- *mean per unit (zeros included):* the total over all of the group's units;
- *mean among units with a nonzero value:* the total over the group's units whose Y is not zero;
- *each group's share of the total:* the group's total over the total of all units shown.

For a yes-or-no item the list is shorter: the share of units answering yes, the number of units answering yes, and
each group's share of all units answering yes. (Its total is the number answering yes, and its means are the share
yes, so those would repeat.) The choice is kept when Y changes, where the new item offers it.

**Also show the full tables.** Tick this to show, under the notes, the earlier view: both files' tables with every
statistic at once, the units in each group and, for a number cut into groups, the lowest and highest X in each group
(the column "X range in the group"). See "The full tables" below.

**The table and the plot are made again after every change.** The line above the table says how long it took: a
tenth to a third of a second. A change of the statistic alone only redraws them, at once. If a choice cannot be
tabulated (a percentile of 0 or 100, a filter with no number yet), that line says why in red and no table or plot is
shown until the choice is fixed.

## Reading the table

Above the table, its title names Y and X, and a box says what the source and the file are. Over the table, a line
names the statistic and its unit (percent, $ billions, millions of units, dollars per unit).

Each row is an X group, then a row for all units shown. The columns:

| Column | What it is |
|---|---|
| source | the statistic in the item's source data (its name is in the column's heading) |
| posttrained file | the statistic in the posttrained file |
| difference (percentage points, file minus source) | only for the two shares: the file's share minus the source's. A share of 12.0% in the source and 15.0% in the file is +3.0 points |
| percent difference | the file's value minus the source's, divided by the source's: 15.0% against 12.0% is +25.0%. A plus sign means the file is higher; where the source's value is negative it is divided by its size, so plus still means higher. Blank where the source's value is zero |
| sample records: source, and: file | how many of each file's own sample records the group rests on (below, "What a record is"), in small grey type |
| records missing Y: source, and: file | shown only when Y is missing on some records (CPS overtime, which only part of the sample is asked); those records are left out of every statistic |

For the two shares both differences are given: points say how far apart the shares are, the percent difference says
how far apart relative to the source's share, which matters most for small shares.

**The plot.** Under the table, the same statistic by X group, the source in blue and the file in orange. When the
groups are in an order (percentiles, or cutoffs you type) it draws two lines; when they are categories (filing status,
age range) or there is no X, it draws pairs of bars. The left axis gives the unit, the bottom axis names X and how it
was cut. A hollow point or a pale bar marks a group resting on fewer than 30 sample records in that file. Hovering
over a point or bar shows its value and its number of sample records. Units whose X is missing are left out of the
lines (they are in the table) but get their own pair of bars.

## The full tables

With "also show the full tables" ticked, the earlier view appears under the notes: one row per X group, and for each
file:

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

**Thin cells.** In the table and in the full tables, a cell resting on fewer than 30 sample records is shaded, and a
row is marked "(thin)" when either file's cell is thin. In the compact table the shaded cells are the file's value and
its sample records. The threshold is the setting `thin_cell_records`.

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
item uses and which variables each source may be cut by). The notebook and the web server read those tables at start, in about a
second, and need no pipeline modules.

The files:
- `check_tool.ipynb`: the notebook the RA opens. One code cell; it calls `check_notebook.show()`.
- `check_notebook.py`: the notebook's controls (ipywidgets) and the drawing of the table and the plot. It turns the
  controls into a request and the answer into a table and a plot; it does no tabulation of its own. The plot is drawn
  as SVG (a picture format browsers read) by its own short code, because the two modules hold no plotting package:
  matplotlib is not among them (checked 3 November), and the tool installs nothing.
- `check_tables.py`: the tabulation, used by both the notebook and the web server.
- `server.py`, `index.html` and `start.sbatch`: the web page, the earlier way of using the tool (below).
- `prepare.py`, `prepare.sbatch` and `settings.json`: the tables and the settings.

What was tested and what was not (3 November): in a Slurm job with only the two modules above, the notebook ran
headless (`jupyter nbconvert --execute`) and drew its controls and first table; the tabulation gave the same tables as
the server's test for its nine cases, cell for cell against a separate pandas tabulation and field for field against
the server; and the controls, set one at a time as the RA would, made the same requests and tables. Later on 3 November the
notebook got its compact view (one statistic, the difference, the plot); tested the same way in a Slurm job with only
the two modules: for eight cases (PUF, SCF, CEX, CPS and SIPP items, a yes-or-no item, filters, percentiles, cutoffs
and categories) and every statistic in the drop-down, every number of the compact table equals the full table's
within the rounding of the two, and the differences equal those recomputed from the tabulation; the plots were
rendered to pictures and looked at. The Jupyter app's
form was not tried: its field names above come from YCRC's documentation (the Jupyter page and the Ollama page's
"additional modules for Jupyter"), and whether the app runs Jupyter from the modules rather than from the
environment is not documented. The first time someone uses it, check that the controls appear.

The notebook finds `check_notebook.py` next to itself; a copy of the notebook opened elsewhere finds it at the
worktree path written in its code cell, which needs changing if the tool moves.

A 2019 view against SOI's published tables is not built. It would be one more source in `prepare.py` (the published
cells as a table) and one more kind of item in the tool.

## Security

The data are licensed PUF microdata. The notebook runs inside the RA's own Jupyter session, which Open OnDemand
starts as her own Slurm job and reaches only through the portal after her NetID and Duo login; the notebook starts no
server and opens no port of its own. It shows tables of weighted sums, means, shares and record counts, never
records. The tables it reads are readable by the `pi_nrs36` group only.

## The web page (the earlier way)

Before the notebook, the tool was a web page served by `server.py` from a Slurm job. It gives the same tables (the
two share `check_tables.py`; tested), but it has to be reached through the Remote Desktop's browser, Open OnDemand's
node proxy or an SSH tunnel, which did not work for everyone. The files are kept; these were its steps.

### Starting the web page

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

### Opening the page

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

Every request to the server must carry the token printed at start, compared in constant time; a request without it,
or with a wrong one, is refused (tested). The token is new at every start, printed only in the job's output, which is
made readable by you alone, and never written to the server's log. The page loads nothing from the internet.
