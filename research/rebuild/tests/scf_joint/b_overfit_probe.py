"""Diagnostic: can the existing transformer memorize 64 complete SCF wealth rows?

Select distinct shared contexts and hide every wealth field. This is an
in-sample capacity check, not a generalization result or a production model.
"""

import argparse
import json
from pathlib import Path

import numpy as np
import torch
import torch.nn.functional as F

from b_model import RecordTransformer, load_meta, load_split, slot_variables, source_names


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--data-dir", required=True)
    p.add_argument("--out", required=True)
    p.add_argument("--steps", type=int, default=500)
    p.add_argument("--n", type=int, default=64)
    a = p.parse_args()

    torch.manual_seed(20261009)
    torch.set_num_threads(4)
    data = Path(a.data_dir)
    meta = load_meta(data / "meta.json")
    vars_ = slot_variables(meta)
    sources = source_names(meta)
    d = load_split(data, "train", meta, sources)
    shared = [j for j, v in enumerate(vars_) if v["group"] == "shared"]
    wealth = [j for j, v in enumerate(vars_) if v["group"] == "scf_only" and v["kind"] == "amount"]
    eligible = np.flatnonzero((d["src"] == sources.index("SCF")) &
                          (d["tok"][:, wealth] >= 0).all(axis=1) &
                          (d["tok"][:, shared] >= 0).all(axis=1))
    chosen, keys = [], set()
    for i in eligible:
        key = (float(d["year"][i]), *d["tok"][i, shared].tolist())
        if key in keys:
            continue
        keys.add(key)
        chosen.append(i)
        if len(chosen) == a.n:
            break
    if len(chosen) != a.n:
        raise RuntimeError(f"only {len(chosen)} distinct complete SCF contexts")

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    true = torch.as_tensor(d["tok"][chosen], dtype=torch.long, device=device)
    hidden = torch.zeros_like(true, dtype=torch.bool)
    hidden[:, wealth] = True
    hidden |= true < 0
    src = torch.as_tensor(d["src"][chosen], dtype=torch.long, device=device)
    year = torch.as_tensor(d["year"][chosen], dtype=torch.float32, device=device)
    model = RecordTransformer(vars_, len(sources), d_model=256, n_layers=6,
                              n_heads=8, dropout=0.0).to(device)
    opt = torch.optim.AdamW(model.parameters(), lr=0.001, weight_decay=0.0)

    def score():
        logits = model(true, hidden, src, year)[:, wealth]
        target = true[:, wealth]
        loss = F.cross_entropy(logits.flatten(0, 1), target.flatten())
        pred = logits.argmax(dim=-1)
        return loss, (pred == target).float().mean(), (pred == target).all(dim=1).float().mean()

    history = []
    for step in range(a.steps + 1):
        if step == 0 or step % 50 == 0 or step == a.steps:
            model.eval()
            with torch.no_grad():
                loss, field_accuracy, full_record_accuracy = score()
            row = dict(step=step, loss=float(loss), field_accuracy=float(field_accuracy),
                       full_record_accuracy=float(full_record_accuracy))
            history.append(row)
            print(json.dumps(row), flush=True)
            if field_accuracy >= 0.999 and full_record_accuracy >= 0.999:
                break
        if step == a.steps:
            break
        model.train()
        loss, _, _ = score()
        opt.zero_grad(set_to_none=True)
        loss.backward()
        torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        opt.step()

    out = Path(a.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps(dict(n_rows=len(chosen), n_wealth_fields=len(wealth),
                                   n_params=sum(p.numel() for p in model.parameters()),
                                   device=str(device), history=history), indent=2) + "\n")


if __name__ == "__main__":
    main()
