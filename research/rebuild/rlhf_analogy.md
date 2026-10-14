---
title: "Tax-Data rebuild — calibration and language-model post-training"
role: notes
workstream: rebuild
status: current
updated: 2026-10-08
sot: research/rebuild/design_decisions.md
supersedes: []
superseded_by: null
---

# Calibration and language-model post-training

From the discussion of 8 October 2026. John asked whether reweighting a
microdata file to targets resembles how a pretrained language model is
shaped into an assistant (supervised fine-tuning, then reinforcement
learning from human feedback, RLHF). Part of it is the same mathematics,
and part is analogy.

## Terms

- **KL divergence (Kullback–Leibler divergence).** A measure of how far one
  probability distribution is from a reference distribution. For weights on
  records it is `Σᵢ pᵢ log(pᵢ / qᵢ)`, where `pᵢ` and `qᵢ` are the new and
  reference shares of record i. It is zero when the two agree. It grows as
  mass moves, and it grows fastest when mass is put where the reference has
  little. It is not symmetric. It is the same quantity as the relative
  entropy in D3 (D3's version adds `− wᵢ + bᵢ` so that weights need not sum
  to a fixed total).
- **Reference model.** In RLHF, the pretrained (or fine-tuned) model that
  the trained model is kept close to. Here, the aged file's weights, or
  more generally the reference distribution for year t.

## The shared mathematics

KL-regularised reinforcement learning chooses a model π to maximise the
expected reward minus β times the KL divergence from the reference model
π_ref. The solution has a closed form. Entropy calibration has the same
form:

```
RLHF:          π*(y) ∝ π_ref(y) · exp( r(y) / β )
Calibration:   wᵢ  =  bᵢ      · exp( Σₖ λₖ aᵢₖ )
```

In calibration, a record's "reward" is a weighted sum of its contributions
`aᵢₖ` to the targets. The multipliers λₖ are set so that each target is
hit, rather than chosen in advance. β corresponds to how strongly the
reference resists change. DPO (direct preference optimisation), a common
post-training method, is built on this closed form.

## What carries over

| Language-model post-training | Tax-Data calibration |
|---|---|
| Pretraining builds a broad distribution | The reference: PUF records, non-filers, imputations, aged to year t |
| Fine-tuning and RL select within it | Calibration reweights within it |
| KL penalty to the reference model | Entropy distance to the aged weights |
| Reward hacking: the reward rises while unmeasured behaviour gets worse | Targets hit by piling weight on a few records, distorting everything untargeted |
| Mode collapse: loss of output diversity | Falling effective sample size (D8) |
| Held-out evaluations | Held-out tables and untargeted relationships (D17) |
| Post-training mostly selects behaviours the base model already has | Reweighting cannot create a household the pool lacks |

**Reward hacking has a concrete example in this field.** PolicyEngine's
Enhanced CPS fitted weights with no distance-to-start term. In May–June
2026 its optimizer hit new non-filer targets by putting weight on a handful
of synthetic very-high-income records; total AGI reached $23.9 trillion
against about $15.5 trillion in SOI (`background/07_practice_tax_models.md`).
The KL term is the protection.

**The base sets the limits.** A claim from the language-model literature,
not checked here: a 2025 paper found that reinforcement learning with
verifiable rewards mostly sharpens what the base model could already do.
The calibration counterpart is certain: weights cannot create records. So
what goes into the reference (the pool and its starting weights) matters
more than the solver. The real-SCF test (`background/12_pooled_scf_test.md`)
showed the same thing: the starting weights across waves did most of the
work.

## Where the analogy breaks

- A language model is used by sampling from it. Here the weights
  themselves are the answer, read as population counts.
- RLHF pushes a reward as high as it will go. Calibration meets targets at
  a stated precision and stops.

## A consequence for generative models

If a neural network generates the reference distribution, idealized
minimum-KL adjustment to feasible targets has the exponential-tilt form
above, provided that the tilted distribution can be normalized. Calibrating
a finite sample approximates that population-level solution on the drawn
support; fine-tuning a finite network approximates it within the network's
model class. They need not give identical distributions in practice.

Weighted maximum-likelihood distillation of calibrated samples minimizes
KL from the calibrated sample distribution to the fitted model. This is a
different optimization from KL-regularized RL, whose penalty is from the
adjusted model to the reference. Both can reach the same desired
distribution under ideal conditions, but that is not a general convergence
guarantee. Resampled records must be checked against the targets again.
An iterative procedure intended to preserve the original minimum-KL
solution must retain that original reference, rather than silently change
the reference after every refit.
