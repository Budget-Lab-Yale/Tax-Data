# Performance considerations

John plans to add many more surveys, so the table will have at least ten
times the rows and many more columns. This note says what training and
generation cost depend on and what keeps them manageable.

## Measured on 22 October, and what the pipeline now does

Four experiments, all on H200 GPUs, on the PUF + SCF + CEX table (545,000
to 675,000 rows, 86 slots); reports under
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-22/perf/`.

| Change | Seconds a training pass | Peak GPU memory | Same results? | In the pipeline |
|---|---:|---:|---|---|
| before (every record carries every slot) | 18.5 | 10.7 GB | | |
| packing: a batch holds one source and only its slots (`packing/`) | 8.5 | 5.7 GB | same answers to rounding; trained results within seed noise | yes |
| and the output layers in one batched multiplication (`precision/`) | 7.8 | 5.8 GB | same answers | yes |
| and TF32 for the training steps (`precision/`) | 6.5 | 5.8 GB | within seed noise | no (a setting until 3 November) |
| and bfloat16 instead (`precision/`) | 5.4 | 5.6 GB | within seed noise, on every measure checked | yes, since 26 October |
| table stored as 8-bit integers (`benchmark/`) | no change | table 364 MB → 45 MB | | no: matters only at tens of millions of rows |
| batch of 16,384 records (`benchmark/`) | −5% | 36 GB | | no |

**Generation** (`generation/`), seconds and peak memory per 50,000 records:
before, 20.4 s and 24.5 GB (it ran out of memory on a 32 GB card); running
the network only on each phase's slots and computing only the needed output
layers, 9.4 s and 10.8 GB, the same draws (in the pipeline: `generate.py`,
the only drawing loop since 22 October); with bfloat16 as well, 5.0 s and
5.5 GB, differences well inside seed noise (not switched on; the option was
removed when the two generators were merged, and adding it back is a few
lines).

**Two limits found:**
- Empty slots cost more than in proportion: 3.4 times the slots took 5.2
  times as long, although no record used the extra ones. Packing removes
  them.
- PyTorch's masked attention fails ("illegal memory access") once records x
  attention heads x slots x slots passes about 2.1 billion in one step. It
  hit the fill-in refit at about 160 slots and generation with the larger
  network at 38,000 records a chunk. The refit's chunk size is now set to
  stay under half that (`model.safe_chunk`).

**Open:** a stack generated on a B200 GPU had about 11% fewer rare business
holders than on an H200, in one run made with since-fixed code; untested
since. `run.sbatch` asks for H100 or H200 until it is.

**Still to do** (below): fewer passes over a larger table, with the
learning-rate schedule stated in updates and batches drawn with fixed
shares by source.

The rest of this note is the analysis written before the measurements.

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

Written before the measurements above; since 26 October training runs in
bfloat16 (a setting, training.precision, until 3 November). Before then everything was standard
32-bit floating point (about 7 significant
digits).

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

A pass shows the model every training record once. With batches of 4,096
(until 26 October) a pass was about 224 weight updates; with batches of
512 (since then) about 1,790, so 80 passes are about 143,000 updates.
The tests of 25–26 October (`HISTORY.md`) bear on this: on the real model,
320 passes at batch 4,096 helped the top of the file, 640 added nothing,
and batch 512 at 80 passes helped less than 320 passes did, so passes over
the data mattered more than the number of updates. What the
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
