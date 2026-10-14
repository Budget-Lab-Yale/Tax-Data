"""Piece E development only: fabricated draws in $P/dev/e/ (the true tokens
with a share of each variable's tokens swapped among records of the same
wave), used before piece A's draws exist. Not part of the evaluation."""
import os
import sys

import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
os.environ.setdefault("E_OUT", "/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/e/out")
from e_evaluate import Base, P  # noqa: E402

base = Base()
rng = np.random.default_rng(1)
K = 4
N = base.N
tok = np.repeat(base.ttok[:, None, :], K, axis=1).copy()
for k in range(K):
    for j in range(tok.shape[2]):
        sw = rng.random(N) < 0.3
        for y in np.unique(base.year):
            idx = np.flatnonzero(sw & (base.year == y))
            tok[idx, k, j] = base.ttok[rng.permutation(idx), j]
lp = rng.normal(-30, 5, (N, K))
d = os.path.join(P, "dev", "e")
os.makedirs(d, exist_ok=True)
np.savez(os.path.join(d, "fake_draws.npz"), tokens=tok.astype(np.int16), logp_ref=lp,
         logp_ref_var=np.zeros((N, K, 24), np.float32))
np.savez(os.path.join(d, "fake_draws_new.npz"), tokens=tok.astype(np.int16), logp_ref=lp,
         logp_new=lp + rng.gamma(1, 0.1, (N, K)))
q = rng.dirichlet(np.ones(K), N)
np.savez(os.path.join(d, "fake_weights.npz"), w=q * base.W[:, None])
print("fake files written", N, K)
