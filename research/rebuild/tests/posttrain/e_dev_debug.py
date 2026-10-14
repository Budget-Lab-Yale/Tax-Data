"""Piece E development: component means of fabricated draws against the truth (decoder check)."""
import os, sys
import numpy as np
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from e_evaluate import Base, decode, load_draws, P, WEALTH
base = Base()
D = load_draws(os.path.join(P, "dev/e/fake_draws.npz"), base)
tok = D["tokens"]
d1 = decode(base, tok)
W = base.W
print("same token share", (tok[:, 0, :] == base.ttok).mean())
for j, v in enumerate(WEALTH):
    t = (W * base.tdol[:, j]).sum() / W.sum()
    f = (W[:, None] * d1[:, :, j]).sum() / W.sum() / tok.shape[1]
    same = tok[:, 0, j] == base.ttok[:, j]
    fs = (W[same] * d1[same, 0, j]).sum() / W[same].sum()
    ts = (W[same] * base.tdol[same, j]).sum() / W[same].sum()
    print(f"{v:18s} truth {t:14,.0f} fake {f:14,.0f} | same-token rows: truth {ts:14,.0f} decoded {fs:14,.0f}")
