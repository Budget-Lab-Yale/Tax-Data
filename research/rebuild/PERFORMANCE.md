# Performance considerations

Not started. To do before the training table grows: John plans to add many
more surveys, so the table will have at least ten times the rows and many
more columns. This note says what training time depends on today and what
could keep it manageable. Nothing here is measured beyond the timings in
the first table; the first step is a benchmark (the last section).

## What training costs today

Pretraining runs as a Slurm batch job (`pipeline/run.sbatch`): one GPU,
4 CPU cores, 96 GB of memory, on the H100, H200 or B200 partitions.

| Run (H100) | Seconds per pass |
|---|---:|
| one net worth item: 48 slots, 500,000 rows | 20 |
| the balance sheet (21 October): 73 slots, same rows | 28 |
| the same, three runs sharing one node | 36 |
| a network about 6 times larger, same table | 87 |
| the first two passes, before the cell method's fill-ins start | 8 (of the 20) |

80 passes take about 35–50 minutes.

**The network** (`pipeline/model.py`): a transformer with 4 layers, width
128, 4 attention heads and an inner layer of 256; about 1.3 million
parameters, more than half of them in the output layers (one per variable,
up to 84 choices each).

**What the time depends on:**
1. **Rows.** In proportion: every record is processed every pass.
2. **Columns.** Every record carries every slot (below), so the cost goes
   with rows times all columns, not with the data present. The attention
   step grows with the square of the number of slots: smaller than the
   rest of each layer at 73 slots, about equal near 300, larger beyond.
3. **Network size.** 6 times the parameters gave 3 times the time per pass
   and no better results (`HISTORY.md`).
4. **The cell method.** Its fill-ins on the SCF are a fixed cost per pass
   that grows with the SCF's rows; it is most of the gap between 8 and 20
   seconds. A translation step for each new survey could add the same kind
   of cost.

Done the way it is done today, ten times the rows and three times the
columns would make each pass roughly thirty times longer.

## 1. Slots: carry only what a record has

Each record is a row of slots, one per variable, always in the same order;
a variable its source lacks holds −1. Such a slot is hidden from the
others, but it is still computed through every layer and output layer and
then discarded.

| | Slots in the row | Slots a PUF record uses | Wasted |
|---|---:|---:|---:|
| today | 73 | about 48 | about a third |
| five more surveys with 30 variables of their own | about 223 | about 48 | about 80% |

**Packing:** each record carries only its own slots, each tagged with its
variable (the network already labels every position by variable, the slot
embedding). Records of different lengths are batched together, the shorter
padded to the longest in the batch, or batched by source. What the model
learns does not change, since empty slots are already invisible to it; the
wasted arithmetic goes. The cost then follows the data actually present.

## 2. Storing the table compactly

During training the table sits in GPU memory as 64-bit integers, 8 bytes a
value: about 290 MB today (on disk, compressed, 25 MB). At 50 million rows
and 300 slots that would be about 120 GB, more than an H100's 80 GB. Every
value is a band from −1 to 83, so one byte is enough: about 15 GB in the same
example, less with packing. Beyond that, keep the table in ordinary memory
and send it to the GPU a batch at a time. A memory ceiling, not a speed
problem.

## 3. Precision

Today everything is standard 32-bit floating point (about 7 significant
digits); `train.py` turns on no reduced precision.

| Option | What it is | Tradeoff |
|---|---|---|
| TF32 | one setting: the H100 does large matrix multiplications with less internal precision, storage stays 32-bit | cheapest; accuracy loss usually negligible |
| bfloat16 mixed precision | large multiplications in a 16-bit format (about 3 significant digits, the 32-bit range); sums, the probabilities, the loss and the weight updates stay 32-bit | several times faster matrix arithmetic on an H100 and half the memory; rounding error could matter for rare items with small probabilities |
| 8-bit | supported on the H100 | riskier; not recommended here |

Evidence so far: the PUF-only model of 8–9 October ran in bfloat16; one-step
probabilities in bfloat16 and 32-bit differed by at most 0.4%, and a file
generated in 32-bit fell inside the seed range on every measure checked
(`archive/tests/puf_generator/EXPERIMENTS.md`). A different model, and it
compared generation, not training; it would need repeating.

Faster arithmetic only helps if the GPU spends its time multiplying. At this
network size much of a pass may be the cell method's fill-ins and the
overhead around each step; unmeasured.

## 4. Fewer passes over a larger table

A pass shows the model every training record once: today about 120 weight
updates (batches of 4,096), so 80 passes are about 9,600 updates. What the
model gains depends on the number of updates and how much distinct data
they see, not on passes as such. With ten times the rows, 8–10 passes give
as many updates as today, on data seen less often, which usually
generalizes as well or better.

Evidence: 50 passes did as well as 80; with the balance sheet the model
begins to fit the SCF's training records better than its held-back ones, a
sign of seeing the same households too often.

What would need changing:
1. the learning-rate schedule (warm up over 3 passes, decay to 5% of the
   peak) is written in passes; state it in updates;
2. small sources: with the SCF's 125,000 rows among millions, draw each
   batch with fixed shares by source so every survey gets enough updates;
3. the cell method refits every 10 batches; may need retuning.

## The first step: a benchmark

About an hour of GPU time. Time a few passes while varying:
- rows (copies of the table);
- empty slots (dummy columns absent on every record);
- precision (32-bit, TF32, bfloat16);
- batch size;
and record how busy the GPU is, and how much of each pass the cell method
takes. That says which of the four changes above matter, and in what order.
