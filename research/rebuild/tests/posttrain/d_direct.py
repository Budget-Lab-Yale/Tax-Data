"""Test 3, piece D, variant 2: direct target loss with a KL penalty.

Objective, per unit of population weight:

    L(theta) = fit(theta) / fit(reference) + beta * KL(p_theta || p_ref)

fit = the mean over target tables (one table = one wave, family, group and
wealth variable; its cells are the coarse bins) of the sum over the table's
cells of ((expected cell weight - target) / table total)^2. Dividing by the
reference model's fit makes the first term 1 at the start, so beta reads as
"nats of KL the method would pay to remove the whole of the reference's
squared error". KL is per unit of population weight, in nats.

Gradient. The expected cells are refreshed every --refresh-steps steps from
a full pass over every record (one draw each, each step's bin probabilities
used in place of the drawn bin). Between refreshes the fit term is
linearized: its gradient with respect to cell c, g_c, is held fixed, so the
fit term becomes an expected cost per record, the sum over the 24
generation steps of r_v(x_v) = the sum over the 11 families of g at the
cell the drawn bin falls in. The KL term is also a sum over steps, of
log p_theta(x_v | earlier draws) - log p_ref(x_v | earlier draws). For a
model that draws one variable at a time, the gradient of the expected total
cost is, in expectation,

    sum over steps v of
      gradient of [the exact expected cost of step v given the drawn prefix]
    + gradient of log p_theta(x_v | prefix) x (cost after step v - baseline)

The first part is computed exactly from the step's probabilities over all
tokens, including the step's exact KL. The second part (the effect of a
step's draw on later steps) uses, for each later step, its exact expected
cost given its prefix in place of the sampled cost, and as baseline the
mean of the same quantity over the other draws of the same record (G = 4
draws per record). Records are drawn in proportion to their population
weight.

Each training step: a rollout without gradients of B = records x G draws
(tuned and reference model; bf16 autocast for speed), then 24 forward and
backward passes, one per generation step, accumulating the gradient. The
model is kept in evaluation mode (no dropout), so the objective is a fixed
function of the weights. Reported KL and fit come from the float32
evaluation in d_tune.py, not from these passes.
"""

import torch

from d_tune import log, sample, step_logp


class DirectObjective:
    def __init__(self, S, a):
        self.S = S
        self.beta = a.beta
        self.G = 4
        self.B_rec = max(1, a.rl_batch // self.G)
        self.refresh_steps = a.refresh_steps
        self.refresh_k = a.refresh_k
        self.sample_batch = a.sample_batch
        self.amp = not a.no_amp
        self.pW = (S.W / S.W.sum()).float()
        self.g = None
        self.fit_ref = None
        self.last = {}
        self.step = 0
        self.gen_refresh = torch.Generator(device=S.device)
        self.gen_refresh.manual_seed(a.seed + 1000)

    @torch.no_grad()
    def refresh(self, model):
        """Expected cells under the current model (--refresh-k draws per
        record), then the linearized fit gradient g_c."""
        S = self.S
        kk = self.refresh_k
        ec = torch.zeros(S.n_cells, dtype=torch.float64, device=S.device)
        allidx = torch.arange(S.N, device=S.device).repeat_interleave(kk)
        for s0 in range(0, allidx.shape[0], self.sample_batch):
            ri = allidx[s0:s0 + self.sample_batch]
            r = sample(S, model, ri, self.gen_refresh, ref=False, want_cells=True,
                       w=S.W[ri] / kk, amp=self.amp)
            ec += r["cells"]
        fit, grad = S.sq_fit_and_grad(ec)
        if self.fit_ref is None:
            self.fit_ref = fit
            log(f"direct: reference fit (mean over tables of squared share errors) {fit:.3e}")
        self.g = grad / self.fit_ref                    # d(fit / fit_ref) / d(cell)
        self.last["fit_rel"] = fit / self.fit_ref

    def loss_backward(self, model, gen, scale=1.0):
        """Accumulates scale x the gradient (24 backward passes); returns the
        surrogate's value, for the log only."""
        S = self.S
        if self.g is None or self.step % self.refresh_steps == 0:
            self.refresh(model)
        self.step += 1
        model.eval()
        rec = torch.multinomial(self.pW, self.B_rec, replacement=True, generator=gen)
        ri = rec.repeat_interleave(self.G)
        B = ri.shape[0]
        steps = list(zip(S.order, S.step_var))
        # ---- rollout, no gradient ------------------------------------------
        with torch.no_grad():
            cur = S.ctx.tok_in[ri].clone()
            hid = cur < 0
            prefixes, lpr_all, cost_all, draws = [], [], [], []
            exp_cost = torch.zeros(B, S.nv, device=S.device)
            for p, (j, k) in enumerate(steps):
                prefixes.append((cur.clone(), hid.clone()))
                lpn = step_logp(S, model, cur, hid, ri, j, k, self.amp)
                lpr = step_logp(S, S.ref, cur, hid, ri, j, k, self.amp)
                pn = lpn.exp()
                ct = (S.Wsum * S.token_cost(self.g, ri, k, lpn.shape[1])).float()
                klt = torch.where(pn > 0, lpn - lpr, torch.zeros_like(pn))
                exp_cost[:, p] = (pn * (ct + self.beta * klt)).sum(1)
                d = torch.multinomial(pn, 1, generator=gen).squeeze(1)
                draws.append(d)
                lpr_all.append(lpr)
                cost_all.append(ct)
                cur[:, j] = d
                hid[:, j] = False
            ctg = exp_cost.flip(1).cumsum(1).flip(1)              # cost from step p on
            after = torch.cat([ctg[:, 1:], torch.zeros(B, 1, device=S.device)], 1)
            a3 = after.view(self.B_rec, self.G, S.nv)
            base = (a3.sum(1, keepdim=True) - a3) / (self.G - 1)  # leave-one-out mean
            adv = (a3 - base).view(B, S.nv)
        # ---- 24 forward and backward passes --------------------------------
        total = 0.0
        for p, (j, k) in enumerate(steps):
            cu, hd = prefixes[p]
            lpn = step_logp(S, model, cu, hd, ri, j, k, self.amp)
            pn = lpn.exp()
            klt = torch.where(pn > 0, lpn - lpr_all[p], torch.zeros_like(pn))
            step_cost = (pn * (cost_all[p] + self.beta * klt)).sum(1)
            lp_d = lpn.gather(1, draws[p][:, None]).squeeze(1)
            sur = (step_cost + lp_d * adv[:, p]).mean()
            (scale * sur).backward()
            total += float(sur.detach())
        self.last["cost_rollout"] = float(exp_cost.sum(1).mean())
        return total

    def log_step(self, step):
        if step % 50 == 0:
            log(f"direct step {step}: relative fit at last refresh "
                f"{self.last.get('fit_rel', float('nan')):.4f}, mean rollout cost "
                f"{self.last.get('cost_rollout', float('nan')):.4f}")
