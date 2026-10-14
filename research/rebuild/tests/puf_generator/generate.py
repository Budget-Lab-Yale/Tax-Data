"""Generate synthetic records with a trained model (part B of SPEC.md).

Every variable starts hidden; variables are filled one at a time in
meta.json's `order`, each drawn at temperature 1 from the model's
distribution given everything filled so far. Tokens are decoded to values
with part A's tokenizer.decode.

Writes, in --out-dir:
  synthetic.parquet          as many records as values_train.parquet (unless
                             --n is given), values_*.parquet format without RECID
  synthetic_tokens.npy       the generated tokens, columns in meta.json order
  synthetic_info.json        seed, checkpoint, counts, timings
"""

import argparse
import importlib.util
import json
import os
import time
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow.parquet as pq
import torch

from model import generation_order, gibbs_refine, load_checkpoint, load_meta, sample_records


def parse_args():
    p = argparse.ArgumentParser()
    p.add_argument("--data-dir", required=True)
    p.add_argument("--checkpoint", required=True)
    p.add_argument("--out-dir", required=True)
    p.add_argument("--out-name", default="synthetic")
    p.add_argument("--seed", type=int, default=20261009)
    p.add_argument("--n", type=int, default=None,
                   help="records to generate (default: rows of values_train.parquet)")
    p.add_argument("--batch-size", type=int, default=8192)
    p.add_argument("--temperature", type=float, default=1.0)
    p.add_argument("--tokenizer-dir", default=str(Path(__file__).resolve().parent))
    p.add_argument("--order", default=None,
                   help="comma-separated variable names giving the generation order "
                        "(default: meta.json's order)")
    p.add_argument("--gibbs-sweeps", type=int, default=0,
                   help="after generating, redraw each variable given all others, "
                        "this many passes (default 0: off)")
    return p.parse_args()


def import_tokenizer(d):
    spec = importlib.util.spec_from_file_location("tokenizer", Path(d) / "tokenizer.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def main():
    a = parse_args()
    data = Path(a.data_dir)
    out = Path(a.out_dir)
    out.mkdir(parents=True, exist_ok=True)
    meta = load_meta(data / "meta.json")
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    gpu_name = torch.cuda.get_device_name(0) if device.type == "cuda" else "cpu"

    train_cols = pq.read_schema(data / "values_train.parquet").names
    n_train = pq.read_metadata(data / "values_train.parquet").num_rows
    n = a.n if a.n is not None else n_train

    model, ck = load_checkpoint(a.checkpoint, meta, device)
    order = generation_order(meta)
    if a.order:
        names = [v["name"] for v in meta["variables"]]
        order = [names.index(x) for x in a.order.split(",")]
        if sorted(order) != list(range(len(names))):
            raise ValueError("--order must list every variable exactly once")
    print(f"generating {n} records, seed {a.seed}, checkpoint {a.checkpoint} "
          f"(epoch {ck.get('epoch')}, val loss {ck.get('val_loss')}), GPU {gpu_name}",
          flush=True)

    t0 = time.time()
    tokens = sample_records(model, n, order, a.batch_size, a.seed, device,
                            temperature=a.temperature)
    if a.gibbs_sweeps > 0:
        tokens = gibbs_refine(model, tokens, a.gibbs_sweeps, a.batch_size, a.seed, device,
                              temperature=a.temperature)
    gen_seconds = time.time() - t0
    np.save(out / f"{a.out_name}_tokens.npy", tokens)
    print(f"sampling took {gen_seconds:.1f}s", flush=True)

    tok_mod = import_tokenizer(a.tokenizer_dir)
    rng = np.random.default_rng(a.seed)
    t1 = time.time()
    df = tok_mod.decode(tokens, meta, rng)
    decode_seconds = time.time() - t1

    expected = [c for c in train_cols if c != "RECID"]
    if list(df.columns) != expected:
        missing = [c for c in expected if c not in df.columns]
        extra = [c for c in df.columns if c not in expected]
        if missing or extra:
            raise ValueError(f"decoded columns differ from values_train: "
                             f"missing {missing}, extra {extra}")
        df = df[expected]
    if len(df) != n:
        raise ValueError(f"decoded {len(df)} rows, expected {n}")
    n_na = int(df.isna().sum().sum())
    if n_na:
        print(f"warning: decoded file has {n_na} missing values: "
              f"{df.isna().sum()[df.isna().sum() > 0].to_dict()}", flush=True)

    info = dict(seed=a.seed, checkpoint=str(a.checkpoint),
                checkpoint_epoch=ck.get("epoch"),
                checkpoint_val_loss=ck.get("val_loss"),
                model_config=ck.get("config"), n_records=int(n),
                n_values_train=int(n_train), temperature=a.temperature,
                order=[meta["variables"][j]["name"] for j in order],
                sampling_seconds=gen_seconds, gibbs_sweeps=a.gibbs_sweeps, decode_seconds=decode_seconds,
                gpu=gpu_name, batch_size=a.batch_size,
                missing_values=n_na,
                slurm_job_id=os.environ.get("SLURM_JOB_ID"))
    table_meta = {"generator_info": json.dumps(info)}
    import pyarrow as pa
    tbl = pa.Table.from_pandas(df, preserve_index=False)
    tbl = tbl.replace_schema_metadata({**(tbl.schema.metadata or {}),
                                       **{k.encode(): v.encode() for k, v in table_meta.items()}})
    pq.write_table(tbl, out / f"{a.out_name}.parquet")
    with open(out / f"{a.out_name}_info.json", "w") as f:
        json.dump(info, f, indent=2)
    print(json.dumps({k: v for k, v in info.items() if k != "order"}, indent=2), flush=True)


if __name__ == "__main__":
    main()
