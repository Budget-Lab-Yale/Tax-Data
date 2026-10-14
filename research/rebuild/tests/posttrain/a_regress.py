"""Regression check of a_common.py after the generalization (9 October
20:50): the SCF arm's K = 4 development draws are reproduced exactly with
the same seed, and decoded dollars of the true tokens match records.parquet."""
import sys
from pathlib import Path
import numpy as np
sys.path.insert(0, str(Path(__file__).resolve().parent))
import a_common as A

recs = A.load_records()
model, ctx = A.load_reference("cuda", recs)
print("order", ctx.order_names, "unfilled hidden", ctx.unfilled_hidden, flush=True)
d = A.draw_set(model, ctx, 4, 20261101)
old = A.load_draws("dev")
print("tokens identical:", bool(np.array_equal(d["tokens"], old["tokens"])),
      "logp max diff:", float(np.abs(d["logp_ref_var"] - old["logp_ref_var"]).max()), flush=True)
dec = A.decode_dollars(A.true_tokens(recs), recs, 20261110)
ref = np.stack([recs["dec_" + v].to_numpy() for v in A.WEALTH], axis=1)
print("decoded true dollars identical:", bool(np.array_equal(dec, ref)), flush=True)
# Draws depend on the GPU model (kernels differ), so the exact-equality test
# above holds only on the same GPU type as the original (B200). The
# teacher-forced log-probabilities of the stored draws do not depend on the
# sampler: compare them.
sub = np.arange(0, len(recs), 7)
lp = A.ref_logprob(model, ctx, sub, old["tokens"][sub, 0].astype(np.int64))
print("teacher-forced logp of stored dev draws, max abs diff:",
      float(np.abs(lp - old["logp_ref_var"][sub, 0]).max()), flush=True)
