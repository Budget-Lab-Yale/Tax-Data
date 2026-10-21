# How we got here

The full record, with every script, report and decision, is in
`research/archive/rebuild_2026-10/`, also committed as `b3b3a2f`. Start
there at `README.md` and `design/plan.md`. This is the short version.

## The path

**6–7 October: background.** The problem was stated, and twelve
background reports were written on statistical matching, generative
models, imputation methods, and survey and tax data.

**8–14 October: first experiments.** A transformer that fills in hidden
variables of a record from the rest, tested on the PUF alone, then on the
PUF plus the SCF, then with reweighting to targets. The lessons were
mostly about data, not the model:
- compute totals from their parts, never predict them;
- train on the sample as drawn, so the top is not starved;
- translate surveys into tax concepts before training;
- score on held-out records.

**14–16 October: the minimal example.** Seven variables, the 2015 PUF
and the 2016 SCF, a small transformer, a rank-to-dollar map, and the
reweighting ("tilt") to 136 IRS targets for 2019 plus SCF wealth targets.
- **Income worked:** the tilt moved the model about as far as it moves
  the real PUF.
- **Wealth came out wrong by a factor of two.** The SCF and the PUF
  measure shared variables differently: the SCF misses small holders of
  interest, dividends and gains. That became the central problem.
- **The fix that worked:** put the SCF's dollars on the PUF's dollar
  scale, and keep a source label so the model learns the SCF's extra
  zeros. That brought wealth within a few percent.
- **Also settled:**
  - draw a record's own source's variables first and borrowed ones last;
  - draw net worth as an SCF household would be asked;
  - state the targets in dollars, not ranks.

**16–18 October: translation, round two.** A "fake survey" (PUF records
made to report the way SCF households do) was built to test translation
rules against a known truth. That led to the **cell method**:
- In cells of wage group by marital status, the PUF sets how many SCF
  zero-reporters of interest, dividends and capital gains are hidden
  holders, and at what sizes.
- The model decides which households they are, during training.
- On the fake survey it removed 80–90% of the error of taking zeros at
  face value.
- Crude non-filers were added to the PUF side, capital gains were split
  into four pieces, and three more SCF waves were added.

**18–19 October: more PUF items, and why rare items came out short.**
- More AGI items were added, and business income was split into its 18
  PUF parts, with the total computed from them.
- The generated files had 10–20% too few holders of rare parts. The main
  cause was dropout, the random switching-off of parts of the network
  during training: rare items were right on average with it on and too
  low with it off, as in generation. Training without it closed half the
  gap.
- The rest comes from the model's answers not fitting one joint
  distribution exactly.
- IRS business targets were added to the tilt, and they absorb what is
  left cheaply.
- Wealth count targets moved to the file's own units.
- The real 2015 PUF turned out to cost no more to reweight to 2019 than
  the synthetic file.

**20 October: this fresh start.** The old folder was archived and the
pipeline rewritten to reproduce the latest run (`FEATURES.md`). Checks:
- the new table builder gave the old training table exactly: every row,
  band, dollar value, band edge and dollar map;
- the new generation and tilt, run on the old no-dropout model, gave the
  latest run's results within noise: effective sample 91.0% against 90.9%,
  median net worth $90,852 against $90,847, every target met. The records
  themselves differ because a different GPU rounds differently;
- the training script ran a 4-pass test; it was not rerun to 80 passes.

## Paths tried and not taken

| What | Why not |
|---|---|
| Sequential trees (as in the R package synthpop) in place of the transformer | Tied or beat it on PUF-only generation and were about even on wealth; the transformer was kept for the minimal example, and the comparison was left open in the old plan |
| Ranking SCF values over the whole distribution, zeros included | Assumes the SCF covers the same population as the filers; it does not |
| SCF zeros treated as "zero or small" | Lost the SCF's exact zeros; replaced by the cell method |
| A fitted reporting curve (chance of reporting by size) | Worse than the cell method on the fake survey |
| S-curves inside the fill-in | Removed in the cell method; no gain |
| A translation layer learned inside the network | Direction agreed, not built; superseded for now by the cell method |
| Separate models per source, plus reweighting | The same for the PUF variables, but net worth before the tilt was 25–37% too high in total and too flat across wage groups; one model trained on both sources did better |
| Wealth targets stated by rank, solved by iteration | Never settled; replaced by fixed dollar lines |
| Wealth counts on the SCF's own 143 million units | Pushed the file's 44 million extra units below every line; the median fell to $39,000 against the SCF's $91,000 |
| Business income drawn as one item | Kept the total right but lost the parts, which tax law needs |
| A bigger network (4 times the parameters) | Within seed noise of the base network |
| Training longer, weighting the loss by how much of a record is hidden, oversampling rare holders in training | No measurable gain on rare items |
| Dropout left on in generation | Works about as well as training without dropout; dropped for simplicity |
| Resampling each variable given the rest after generation (Gibbs) | The file drifted with every pass instead of settling |
