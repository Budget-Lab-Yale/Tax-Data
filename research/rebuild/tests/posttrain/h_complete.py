"""Completions and generations from the `full` model (test 3, piece H).

Built on ../scf_joint/b_complete.py (test 2), which fills every unobserved
variable of a record. This version fills only a chosen set of variables and
leaves every other unobserved variable hidden (the model is asked about it
by nothing; it is never filled), and it can also generate records from
nothing. Model, decoder tables and decoding are test 2's, imported
unchanged.

Modes
-----
--mode complete
    Target records: rows of --split, filtered by --target-sources and
    --target-years. The variables in --fill are hidden (their observed
    values set aside) and drawn one at a time in the fill order, each given
    the record's observed variables and the values drawn before it.
    Variables outside --fill that the record does not observe stay hidden.
    Records are presented with their own source tag, or --as-source;
    year is the record's own year.
--mode generate
    --n records (default: as many as the training records of
    --gen-source) start with every variable hidden, tagged --gen-source at
    year --gen-year, and the variables in --fill are drawn in order. Each
    generated record gets weight = (total weight of the training records
    of that source) / n, so the file has the training population total.

--fill: `wealth` (variables observed on some SCF training record and on no
PUF training record), `puf` (variables observed on some PUF training
record), or a comma-separated list of names. Weight-kind variables dropped
in training are never filled.

Order: by default the fill set in test 2's fill order (b_model.fill_order:
meta.json's generation_order, which in `full` lists the wealth variables).
For --fill puf the default is meta.json's `puf_generation_order` if present,
else meta.json's `generation_order` if it names every PUF variable,
else test 1's order (`order` in test 1's meta.json, S006 removed), with any
fill variable that order does not name appended in meta.json order. --order
overrides both. The order used is written to the info file.

--source-by-step `group:SOURCE`: draw the variables of that meta.json group
with another source tag (for later arms that switch the tag between steps;
not used by piece H's jobs).

Outputs, in --out-dir, with <base> = h_<tag>_seed<NN>:
  <base>.parquet: one row per (record, draw).
    complete mode: `row` (0-based row in records_<split>.parquet), the
      record's non-variable columns from that file (source, year, weight,
      ...), `draw`, and for every model variable: the dollar value (or
      category value) under the variable's name (true value where observed,
      decoded draw where filled, NaN where unobserved and not filled),
      `tok_<name>` (token, -1 where hidden), `obs_<name>` (True where the
      value is the record's own).
    generate mode: `gen_id`, `source`, `year`, `weight`, `draw` (0), and
      for each filled variable its value and `tok_<name>`.
  <base>_tokens.npz: `tokens` [draws, N, V] int16 over the model's slot
    variables (-1 hidden), `names` (slot variable names), `rows`
    (complete mode) and `fill` (names filled, in order).
  <base>_info.json: settings, fill order, timings, the completion
    log-likelihood of the true values where known (complete mode).
"""

import argparse
import importlib.util
import json
import os
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd
import torch

HERE = Path(__file__).resolve().parent
T2_CODE = HERE.parent / "scf_joint"
sys.path.insert(0, str(T2_CODE))
from b_model import (all_variables, completion_nll, fill_order, load_checkpoint,  # noqa: E402
                     load_meta, load_split, slot_columns, slot_variables)
from b_complete import add_dollars, allowed_code_tables  # noqa: E402

TEST1_META = Path("/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/data/meta.json")


def parse_args():
    p = argparse.ArgumentParser()
    p.add_argument("--data-dir", required=True)
    p.add_argument("--checkpoint", required=True)
    p.add_argument("--out-dir", required=True)
    p.add_argument("--tag", required=True)
    p.add_argument("--train-seed", type=int, required=True)
    p.add_argument("--mode", choices=["complete", "generate"], default="complete")
    p.add_argument("--fill", default="wealth")
    p.add_argument("--order", default=None, help="comma-separated names (overrides)")
    p.add_argument("--split", default="train")
    p.add_argument("--target-sources", default=None)
    p.add_argument("--target-years", default=None)
    p.add_argument("--as-source", default=None)
    p.add_argument("--source-by-step", default=None, help="group:SOURCE[,group:SOURCE]")
    p.add_argument("--gen-source", default="PUF")
    p.add_argument("--gen-year", type=float, default=2015.0)
    p.add_argument("--n", type=int, default=None)
    p.add_argument("--decode-year", type=float, default=None)
    p.add_argument("--draws", type=int, default=5)
    p.add_argument("--seed", type=int, default=20261012)
    p.add_argument("--batch-size", type=int, default=16384)
    p.add_argument("--temperature", type=float, default=1.0)
    p.add_argument("--max-records", type=int, default=None, help="testing only")
    p.add_argument("--tokenizer", default=None,
                   help="path of the tokenizer module with decode(); default: "
                        "g_tokenizer.py in this folder if present, else "
                        "../scf_joint/a_tokenizer.py")
    p.add_argument("--no-decode", action="store_true")
    p.add_argument("--no-allowed", action="store_true",
                   help="do not restrict draws to codes the decoder can turn into dollars")
    p.add_argument("--no-score", action="store_true")
    return p.parse_args()


def import_tokenizer(path):
    path = Path(path)
    if not path.exists():
        return None, f"{path} not found"
    try:
        spec = importlib.util.spec_from_file_location("a_tokenizer", path)
        mod = importlib.util.module_from_spec(spec)
        sys.modules["a_tokenizer"] = mod
        spec.loader.exec_module(mod)
        if not hasattr(mod, "decode"):
            return None, f"{path} has no decode"
        return mod, None
    except Exception as e:                       # report, do not hide
        return None, f"import of {path} failed: {e!r}"


def default_tokenizer():
    g = HERE / "g_tokenizer.py"
    return g if g.exists() else T2_CODE / "a_tokenizer.py"


def observed_sets(data, meta, sources, svars):
    """Boolean [V] arrays: observed on some PUF training record; observed on
    some SCF training record."""
    tr = load_split(data, "train", meta, sources)
    obs = tr["tok"] >= 0
    src = np.array(sources)[tr["src"]]
    puf = obs[src == "PUF"].any(axis=0) if (src == "PUF").any() else np.zeros(len(svars), bool)
    scf_rows = np.char.startswith(src.astype(str), "SCF")
    scf = obs[scf_rows].any(axis=0) if scf_rows.any() else np.zeros(len(svars), bool)
    return puf, scf, tr


def resolve_fill(a, meta, svars, puf_obs, scf_obs, dropped):
    names = [v["name"] for v in svars]
    if a.fill == "wealth":
        fill = scf_obs & ~puf_obs
    elif a.fill == "puf":
        fill = puf_obs.copy()
    else:
        want = [x for x in a.fill.split(",") if x]
        bad = [x for x in want if x not in names]
        if bad:
            raise ValueError(f"--fill names not model variables: {bad}")
        fill = np.isin(names, want)
    for x in dropped:
        fill[names.index(x)] = False
    if not fill.any():
        raise ValueError("nothing to fill")
    if a.order:
        order = [names.index(x) for x in a.order.split(",")]
        if sorted(order) != sorted(np.flatnonzero(fill).tolist()):
            raise ValueError("--order must list exactly the fill variables")
        return fill, order, "--order"
    if a.fill == "puf":
        listed = meta.get("puf_generation_order")
        how = "meta.json puf_generation_order"
        gen = meta.get("generation_order") or []
        if not listed and all(names[j] in gen for j in np.flatnonzero(fill)):
            listed = gen
            how = "meta.json generation_order restricted to the PUF variables"
        if not listed and TEST1_META.exists():
            with open(TEST1_META) as f:
                listed = [x for x in json.load(f)["order"] if x != "S006"]
            how = "test 1's meta.json order (S006 removed)"
        listed = listed or []
        lower = {n.lower(): j for j, n in enumerate(names)}
        order = []
        for x in listed:
            j = names.index(x) if x in names else lower.get(str(x).lower())
            if j is not None and fill[j] and j not in order:
                order.append(j)
        rest = [j for j in np.flatnonzero(fill) if j not in order]
        if rest:
            print(f"fill variables not named in {how}, appended in meta.json order: "
                  f"{[names[j] for j in rest]}", flush=True)
            how += f"; appended {[names[j] for j in rest]}"
        return fill, order + rest, how
    order = [j for j in fill_order(meta, svars) if fill[j]]
    return fill, order, "b_model.fill_order restricted to the fill set"


@torch.no_grad()
def fill_batch(model, tok, src, year, order, gen, src_step=None, temperature=1.0,
               amp=True, allowed=None):
    """Fill the slots in `order` that are hidden (-1) in tok [B, V]; every
    other hidden slot stays hidden. src_step: {j: [B] source codes} used at
    step j instead of src (and for later steps the tag returns to src)."""
    tok = tok.clone()
    hid = tok < 0
    dev = tok.device
    for j in order:
        need = hid[:, j]
        if not need.any():
            continue
        s = src_step.get(j, src) if src_step else src
        with torch.autocast("cuda", dtype=torch.bfloat16,
                            enabled=amp and dev.type == "cuda"):
            logits = model(tok, hid, s, year)
        lj = logits[:, j, :].float()
        if allowed is not None:
            lj = lj.masked_fill(~allowed[:, j, :lj.shape[1]], float("-inf"))
        probs = torch.softmax(lj / temperature, dim=-1)
        draw = torch.multinomial(probs, 1, generator=gen).squeeze(1)
        tok[:, j] = torch.where(need, draw, tok[:, j])
        hid[:, j] = hid[:, j] & ~need
    return tok


def main():
    a = parse_args()
    data = Path(a.data_dir)
    out = Path(a.out_dir)
    out.mkdir(parents=True, exist_ok=True)
    meta = load_meta(data / "meta.json")
    names_all = [v["name"] for v in all_variables(meta)]
    svars = slot_variables(meta)
    scols = slot_columns(meta)
    snames = [v["name"] for v in svars]
    V = len(svars)
    base = f"h_{a.tag}_seed{a.train_seed:02d}"

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    amp = device.type == "cuda"
    gpu_name = torch.cuda.get_device_name(0) if device.type == "cuda" else "cpu"
    model, ck = load_checkpoint(a.checkpoint, meta, device)
    sources = ck["sources"]
    dropped = [x for x in (ck.get("args") or {}).get("drop_vars", "").split(",") if x]
    if (ck.get("args") or {}).get("drop_weight_vars"):
        dropped += [v["name"] for v in svars if v.get("kind") == "weight" and v["name"] not in dropped]

    puf_obs, scf_obs, tr = observed_sets(data, meta, sources, svars)
    fill, order, order_how = resolve_fill(a, meta, svars, puf_obs, scf_obs, dropped)
    print(f"mode {a.mode}; fill {int(fill.sum())} variables in order ({order_how}): "
          f"{[snames[j] for j in order]}; dropped in training: {dropped}; GPU {gpu_name}",
          flush=True)

    tok_path = a.tokenizer or default_tokenizer()
    tokmod, dec_err = (None, "--no-decode") if a.no_decode else import_tokenizer(tok_path)
    if dec_err:
        print(f"decoding off: {dec_err}", flush=True)

    # Target records.
    if a.mode == "complete":
        sp = tr if a.split == "train" else load_split(data, a.split, meta, sources)
        rec = sp["records"]
        rows = np.arange(len(rec))
        if a.target_sources:
            rows = rows[rec["source"].astype(str).isin(a.target_sources.split(",")).to_numpy()]
        if a.target_years:
            ty = [int(x) for x in a.target_years.split(",")]
            rows = rows[rec["year"].astype(int).isin(ty).to_numpy()[rows]]
        if a.max_records:
            rows = rows[:a.max_records]
        N = len(rows)
        if N == 0:
            raise ValueError("no target records")
        tok_obs = sp["tok"][rows].copy()
        src = sp["src"][rows].copy()
        if a.as_source:
            src[:] = sources.index(a.as_source)
        year = sp["year"][rows].astype(np.float32)
        tok_in = tok_obs.copy()
        tok_in[:, fill] = -1
        rec_t = rec.iloc[rows].reset_index(drop=True)
        n_set_aside = int(((tok_obs >= 0) & fill[None, :]).sum())
        print(f"{N} records ({a.target_sources or 'all'} of {a.split}); as source "
              f"{a.as_source or 'own'}; observed values set aside to be redrawn: "
              f"{n_set_aside}; slots to fill: {int((tok_in[:, fill] < 0).sum())}; "
              f"other hidden slots left hidden: {int((tok_in[:, ~fill] < 0).sum())}",
              flush=True)
    else:
        if a.gen_source not in sources:
            raise ValueError(f"--gen-source {a.gen_source} not in {sources}")
        trw = tr["records"]["weight"].to_numpy(float)[tr["src"] == sources.index(a.gen_source)]
        if not np.all(np.isfinite(trw)):
            raise ValueError(f"{int((~np.isfinite(trw)).sum())} non-finite training weights")
        N = a.n or int(trw.size)
        if a.max_records:
            N = min(N, a.max_records)
        rows = np.arange(N)
        tok_in = np.full((N, V), -1, dtype=np.int64)
        src = np.full(N, sources.index(a.gen_source), dtype=np.int64)
        year = np.full(N, a.gen_year, dtype=np.float32)
        rec_t = pd.DataFrame({"gen_id": rows, "source": a.gen_source,
                              "year": int(a.gen_year) if float(a.gen_year).is_integer() else a.gen_year,
                              "weight": trw.sum() / N})
        n_set_aside = 0
        print(f"generating {N} records as {a.gen_source} at {a.gen_year}; total training "
              f"weight {trw.sum():,.0f} spread equally", flush=True)

    src_step = None
    if a.source_by_step:
        src_step_np = {}
        for item in a.source_by_step.split(","):
            g, s = item.split(":")
            for j, v in enumerate(svars):
                if v.get("group") == g:
                    src_step_np[j] = sources.index(s)
        print(f"source tag switched at {len(src_step_np)} steps: {a.source_by_step}", flush=True)

    # Codes the decoder can turn into dollars (test 2's rule).
    allowed_t, allowed_g, allowed_notes, allowed_err = None, None, [], None
    if tokmod is not None and not a.no_allowed:
        try:
            dec_recs = rec_t[["source", "year"]].copy()
            tab, gi, allowed_notes = allowed_code_tables(
                tokmod, meta, data, dec_recs, a.decode_year, svars, model.max_v)
            allowed_t = torch.as_tensor(tab, device=device)
            allowed_g = torch.as_tensor(gi)
            for n_ in allowed_notes:
                print("allowed codes:", n_, flush=True)
        except Exception as e:                  # report, draw unrestricted
            allowed_err = f"allowed-code table failed, draws unrestricted: {e!r}"
            print(allowed_err, flush=True)

    # Completion log-likelihood of the true values (complete mode).
    scores = None
    if a.mode == "complete" and not a.no_score:
        truth = tok_obs.copy()
        tf = data / f"truth_{a.split}.npy"
        if tf.exists():
            tt = np.load(tf)[:, scols][rows].astype(np.int64)
            truth = np.where(truth >= 0, truth, tt)
        truth[:, ~fill] = np.where(tok_in[:, ~fill] >= 0, truth[:, ~fill], -1)
        ts0 = time.time()
        nll, scm = completion_nll(model, tok_in, truth, src, year, order, 8192, device, amp)
        if scm.any():
            anyv = scm.any(axis=1)
            per_rec = nll.sum(axis=1)
            w = rec_t["weight"].to_numpy(float) if "weight" in rec_t else None
            scores = dict(
                n_records_scored=int(anyv.sum()),
                n_records_all_fill_scored=int(scm[:, fill].all(axis=1).sum()),
                nll_per_record=float(per_rec[anyv].mean()),
                nll_per_record_popweighted=(float(np.average(per_rec[anyv], weights=w[anyv]))
                                            if w is not None else None),
                by_variable={snames[j]: dict(n=int(scm[:, j].sum()),
                                             nll=float(nll[scm[:, j], j].mean()))
                             for j in range(V) if scm[:, j].any()},
                seconds=time.time() - ts0)
            print(f"completion NLL of true values: {scores['nll_per_record']:.4f} per record "
                  f"over {scores['n_records_scored']} records", flush=True)

    # Draws.
    gen = torch.Generator(device=device)
    gen.manual_seed(a.seed)
    toks = np.empty((a.draws, N, V), dtype=np.int16)
    t0 = time.time()
    tin_t = torch.as_tensor(tok_in)
    src_t = torch.as_tensor(src)
    yr_t = torch.as_tensor(year)
    for d in range(a.draws):
        for s0 in range(0, N, a.batch_size):
            sl = slice(s0, s0 + a.batch_size)
            allw = allowed_t[allowed_g[sl].to(device)] if allowed_t is not None else None
            sb = src_t[sl].to(device)
            ss = ({j: torch.full_like(sb, c) for j, c in src_step_np.items()}
                  if a.source_by_step else None)
            res = fill_batch(model, tin_t[sl].to(device), sb, yr_t[sl].to(device), order,
                             gen, ss, a.temperature, amp, allowed=allw)
            toks[d, sl] = res.cpu().numpy().astype(np.int16)
        print(f"draw {d + 1}/{a.draws} done, {time.time() - t0:.1f}s", flush=True)
    comp_seconds = time.time() - t0
    assert (toks[:, :, fill] >= 0).all(), "unfilled slots remain"
    keep = tok_in >= 0
    assert (toks[:, keep] == np.broadcast_to(tok_in, toks.shape)[:, keep]).all(), \
        "an observed value changed"
    left = ~keep & ~fill[None, :]
    if left.any():
        assert (toks[:, left] < 0).all(), "a variable outside the fill set was filled"
    np.savez_compressed(out / f"{base}_tokens.npz", tokens=toks, names=np.array(snames),
                        rows=rows, fill=np.array([snames[j] for j in order]))

    # Parquet with tokens and dollars.
    var_set = set(snames)
    rec_cols = [c for c in rec_t.columns if c not in var_set]
    frames = []
    for d in range(a.draws):
        full = np.full((N, len(names_all)), -1, dtype=np.int64)
        full[:, scols] = toks[d]
        cols = {"row": rows.astype(np.int64)} if a.mode == "complete" else {}
        for c in rec_cols:                      # gen_id comes from rec_t in generate mode
            cols[c] = rec_t[c].to_numpy()
        cols["draw"] = np.full(N, d, dtype=np.int64)
        obs_full = np.zeros((N, len(names_all)), dtype=bool)
        for jj, c in enumerate(scols):
            obs_full[:, c] = tok_in[:, jj] >= 0
        for jj, n in enumerate(names_all):
            if n not in var_set:
                continue
            j = snames.index(n)
            if a.mode == "generate" and not fill[j]:
                continue
            if a.mode == "complete" and n in rec_t.columns:
                cols[n] = np.where(obs_full[:, jj], rec_t[n].to_numpy(dtype=float), np.nan)
            else:
                cols[n] = np.full(N, np.nan)
            cols[f"tok_{n}"] = full[:, jj].astype(np.int32)
            if a.mode == "complete":
                cols[f"obs_{n}"] = obs_full[:, jj]
        df = pd.DataFrame(cols)
        frames.append((df, full, obs_full))
    dec_seconds = None
    if tokmod is not None:
        td = time.time()
        try:
            rng = np.random.default_rng(a.seed + 1)
            for df, full, obs_full in frames:
                add = {}
                for n in names_all:
                    if f"tok_{n}" not in df:
                        add[f"tok_{n}"] = np.full(len(df), -1, dtype=np.int32)
                    if n not in df and n in var_set:
                        add[n] = np.full(len(df), np.nan)
                tmp = pd.concat([df, pd.DataFrame(add, index=df.index)], axis=1) if add \
                    else df.copy()
                dec = add_dollars(tmp, meta, data, ["source", "year"], tokmod, rng,
                                  a.decode_year, obs_full)
                for n in snames:
                    if n in df.columns:
                        df[n] = dec[n].to_numpy()
            dec_seconds = time.time() - td
        except Exception as e:                  # keep the tokens; report
            dec_err = f"decode failed: {e!r}"
            print(dec_err, flush=True)
            tokmod = None
    df = pd.concat([f[0] for f in frames], ignore_index=True)
    fn = out / f"{base}.parquet"
    df.to_parquet(fn, index=False)

    info = dict(tag=a.tag, mode=a.mode, train_seed=a.train_seed, checkpoint=str(a.checkpoint),
                checkpoint_epoch=ck.get("epoch"), checkpoint_val_loss=ck.get("val_loss"),
                model_config=ck.get("config"), sources=sources, split=a.split,
                target_sources=a.target_sources, target_years=a.target_years,
                as_source=a.as_source, source_by_step=a.source_by_step,
                gen_source=a.gen_source if a.mode == "generate" else None,
                gen_year=a.gen_year if a.mode == "generate" else None,
                n_records=int(N), draws=a.draws, seed=a.seed, temperature=a.temperature,
                fill=[snames[j] for j in order], order_rule=order_how,
                observed_values_set_aside=n_set_aside, dropped_in_training=dropped,
                decode_year=a.decode_year, tokenizer=str(tok_path),
                decoded=tokmod is not None, decode_error=dec_err if tokmod is None else None,
                allowed_codes=(allowed_t is not None), allowed_error=allowed_err,
                codes_not_drawn=allowed_notes, record_columns=rec_cols,
                completion_seconds=comp_seconds, decode_seconds=dec_seconds,
                gpu=gpu_name, scores=scores,
                files=dict(parquet=str(fn), tokens=str(out / f"{base}_tokens.npz")),
                slurm_job_id=os.environ.get("SLURM_JOB_ID"))
    with open(out / f"{base}_info.json", "w") as f:
        json.dump(info, f, indent=2)
    print(json.dumps({k: v for k, v in info.items() if k not in ("fill", "scores")}, indent=2),
          flush=True)


if __name__ == "__main__":
    main()
