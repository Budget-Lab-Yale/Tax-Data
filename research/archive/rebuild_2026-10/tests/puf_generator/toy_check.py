"""Toy check of the model code on fabricated data with a known joint
distribution (no PUF data involved).

  python toy_check.py make    --dir D   # write meta.json, tokens, values, tokenizer.py
  python toy_check.py compare --dir D --synthetic D/model/synthetic_tokens.npy

The fabricated records have a weight (6 point values), two dependent
categoricals A -> B, an amount C whose zero share and level depend on B, an
amount D that follows C, and five independent noise categoricals. `compare`
measures the total variation distance (half the sum of absolute differences
in cell shares) between the generated records and a fresh draw from the
true distribution, for several two- and three-variable tables, and sets it
beside the same distance between two fresh true draws of the same size
(sampling noise only).
"""

import argparse
import json
from pathlib import Path

import numpy as np
import pandas as pd

P_A = np.array([0.5, 0.3, 0.2])
P_B_A = np.array([[0.70, 0.20, 0.05, 0.05],
                  [0.10, 0.60, 0.20, 0.10],
                  [0.05, 0.05, 0.30, 0.60]])
P_W_A = np.array([[0.40, 0.30, 0.15, 0.10, 0.04, 0.01],
                  [0.10, 0.20, 0.40, 0.20, 0.08, 0.02],
                  [0.02, 0.08, 0.20, 0.30, 0.25, 0.15]])
W_VALUES = [50.0, 120.0, 300.0, 800.0, 2000.0, 9000.0]
NC, ND, NE, KE = 41, 21, 5, 5   # C: zero + 40 bins; D: zero + 20 bins


def sample(n, rng):
    A = rng.choice(3, n, p=P_A)
    B = np.array([rng.choice(4, p=P_B_A[a]) for a in A])
    W = np.array([rng.choice(6, p=P_W_A[a]) for a in A])
    zero_c = rng.random(n) < np.array([0.7, 0.4, 0.2, 0.05])[B]
    lvl = np.clip(np.round(rng.normal(5 + 9 * B, 3)), 1, NC - 1).astype(int)
    C = np.where(zero_c, 0, lvl)
    zero_d = np.where(C == 0, rng.random(n) < 0.9, rng.random(n) < 0.3)
    dl = np.clip(np.round(C / 2 + rng.normal(0, 1.5, n)), 1, ND - 1).astype(int)
    D = np.where(zero_d, 0, dl)
    E = rng.integers(0, KE, size=(n, NE))
    return np.column_stack([W, A, B, C, D, E]).astype(np.int64)


def make_meta():
    def amount(name, nv):
        toks = [dict(lo=0.0, hi=0.0, point=True)]
        edges = np.geomspace(10, 1e6, nv)
        toks += [dict(lo=float(edges[i]), hi=float(edges[i + 1]), point=False)
                 for i in range(nv - 1)]
        return dict(name=name, kind="amount", vocab_size=nv, tokens=toks)
    vars_ = [dict(name="S006", kind="weight", vocab_size=6,
                  tokens=[dict(lo=w, hi=w, point=True) for w in W_VALUES]),
             dict(name="A", kind="categorical", vocab_size=3,
                  tokens=[dict(value=v) for v in range(3)]),
             dict(name="B", kind="categorical", vocab_size=4,
                  tokens=[dict(value=v) for v in range(4)]),
             amount("C", NC), amount("D", ND)]
    vars_ += [dict(name=f"E{i}", kind="categorical", vocab_size=KE,
                   tokens=[dict(value=v) for v in range(KE)]) for i in range(NE)]
    names = [v["name"] for v in vars_]
    return dict(variables=vars_, order=names, seeds=dict(toy=1))


TOY_TOKENIZER = '''
import numpy as np, pandas as pd
def decode(tokens, meta, rng):
    out = {}
    for j, v in enumerate(meta["variables"]):
        t = tokens[:, j]
        if v["kind"] == "categorical":
            out[v["name"]] = np.array([v["tokens"][k]["value"] for k in range(v["vocab_size"])])[t]
        else:
            lo = np.array([x["lo"] for x in v["tokens"]])[t]
            hi = np.array([x["hi"] for x in v["tokens"]])[t]
            val = lo + (hi - lo) * rng.random(len(t))
            out["weight" if v["kind"] == "weight" else v["name"]] = val
    return pd.DataFrame(out)
'''


def cmd_make(d, n_train=20000):
    d = Path(d)
    d.mkdir(parents=True, exist_ok=True)
    rng = np.random.default_rng(1)
    meta = make_meta()
    with open(d / "meta.json", "w") as f:
        json.dump(meta, f, indent=1)
    (d / "tokenizer.py").write_text(TOY_TOKENIZER)
    ns = dict(train=n_train, val=n_train // 8, test=n_train // 8)
    import importlib.util
    spec = importlib.util.spec_from_file_location("tokenizer", d / "tokenizer.py")
    tk = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(tk)
    for k, n in ns.items():
        x = sample(n, rng)
        np.save(d / f"tokens_{k}.npy", x)
        df = tk.decode(x, meta, rng)
        df.insert(0, "RECID", np.arange(n))
        df.to_parquet(d / f"values_{k}.parquet", index=False)
    print("toy data written to", d)


def tv(x, y, cols):
    def shares(z):
        keys = [tuple(r) for r in z[:, cols]]
        s = pd.Series(keys).value_counts(normalize=True)
        return s
    a, b = shares(x), shares(y)
    idx = a.index.union(b.index)
    return 0.5 * np.abs(a.reindex(idx, fill_value=0) - b.reindex(idx, fill_value=0)).sum()


def cmd_compare(d, synth):
    d = Path(d)
    s = np.load(synth)
    n = s.shape[0]
    rng = np.random.default_rng(99)
    r1, r2 = sample(n, rng), sample(n, rng)
    tr = np.load(d / "tokens_train.npy")
    # Coarsen C and D into 5 groups for the three-way table.
    def coarse(x):
        y = x.copy()
        y[:, 3] = np.digitize(x[:, 3], [1, 10, 20, 30])
        y[:, 4] = np.digitize(x[:, 4], [1, 5, 10, 15])
        return y
    tables = {"weight": [0], "A": [1], "B": [2], "C": [3], "D": [4],
              "weight x A": [0, 1], "A x B": [1, 2], "B x C": [2, 3],
              "C x D": [3, 4], "A x E0": [1, 5], "E0 x E1": [5, 6],
              "A x B x C(coarse)": [1, 2, 3], "B x C x D (coarse)": [2, 3, 4]}
    rows = []
    for name, cols in tables.items():
        f = coarse if "coarse" in name else (lambda z: z)
        rows.append(dict(table=name, model_vs_truth=tv(f(s), f(r1), cols),
                         truth_vs_truth=tv(f(r2), f(r1), cols),
                         train_vs_truth=tv(f(tr), f(r1), cols)))
    res = pd.DataFrame(rows)
    # Exact check of P(B | A): generated vs true conditional probabilities.
    cond = np.zeros((3, 4))
    for a_ in range(3):
        m = s[:, 1] == a_
        cond[a_] = np.bincount(s[m, 2], minlength=4) / max(m.sum(), 1)
    print(res.to_string(index=False, float_format=lambda v: f"{v:.4f}"))
    print("\nP(B | A) generated:\n", np.round(cond, 3))
    print("P(B | A) true:\n", P_B_A)
    print("P(A) generated:", np.round(np.bincount(s[:, 1], minlength=3) / n, 3),
          " true:", P_A)


if __name__ == "__main__":
    p = argparse.ArgumentParser()
    p.add_argument("cmd", choices=["make", "compare"])
    p.add_argument("--dir", required=True)
    p.add_argument("--synthetic")
    p.add_argument("--n-train", type=int, default=20000)
    a = p.parse_args()
    if a.cmd == "make":
        cmd_make(a.dir, a.n_train)
    else:
        cmd_compare(a.dir, a.synthetic)
