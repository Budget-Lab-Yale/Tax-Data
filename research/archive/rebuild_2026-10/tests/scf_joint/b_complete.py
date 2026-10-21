"""Complete records with a trained model (test 2, piece B).

For each target record, every observed value is kept and every unobserved
value is filled one variable at a time (unobserved shared variables first,
then meta.json's order: the SCF weight, then the wealth variables), each
drawn at temperature 1 given everything observed and drawn so far.

Two kinds of completion are written:

  plain  -- one completion per draw, as drawn.
  wc     -- weight-corrected: K completions per draw (K = 20 by default),
            of which one is kept with probability proportional to its drawn
            SCF weight (decoded to the weight's value). The model learns the
            SCF sample, which over-represents wealthy households; keeping
            draws in proportion to the weight turns the sample relationship
            between incomes and wealth into the population relationship.
            The plain draw is the first of the K completions, so the two
            files come from the same model draws.

--hide-group scf_only hides every SCF-only variable of the target records
before completing (needed when the targets are SCF units whose wealth is
observed, as in experiment B; a no-op for the pretend-PUF half and the PUF,
where they are already unobserved). The SCF weight is in that group, so the
targets are completed without their weight, as PUF records would be.

--complete-as-source (alias --as-source) completes the records as if they came from another source (the
benchmark trained on half 2 alone has never seen source SCF_H1).

Scoring: where the true values of the completed variables are known
(truth_<split>.npy, or the tokens themselves for hidden observed values),
the completion log-likelihood is reported: the sum over the fill order of
-log p(true value | observed and earlier true values), per record and per
variable, in the info file.

Output, in --out-dir:
  completed_<tag>_<plain|wc>_seed<NN>.parquet: `row` (0-based row of the
    record in records_<split>.parquet), the record's non-variable columns
    from that file (source, year, hh_id, implicate, unit, weight, half);
    one column per variable with the completed record's dollars (observed
    variables keep their true dollars; completed ones are decoded with
    a_tokenizer.decode); tok_<variable>, the completed tokens;
    obs_<variable>, True where the value was observed (kept) rather than
    completed; and `draw` (0 .. draws-1).
  completed_<tag>_seed<NN>_info.json: settings, timings, scores.
If a_tokenizer cannot be imported, completed dollars are left NaN and the
info file says so; rerun with --decode-only later.
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

from b_model import (all_variables, completion_nll, complete_batch, fill_order,
                     load_checkpoint, load_meta, load_split, slot_columns,
                     slot_variables)

CODE_DIR = Path(__file__).resolve().parent


def parse_args():
    p = argparse.ArgumentParser()
    p.add_argument("--data-dir", required=True)
    p.add_argument("--checkpoint", required=True)
    p.add_argument("--out-dir", required=True)
    p.add_argument("--tag", required=True, help="e.g. joint, half2, joint_puf")
    p.add_argument("--train-seed", type=int, required=True,
                   help="the training seed of the checkpoint (for the file name)")
    p.add_argument("--split", default="test")
    p.add_argument("--target-years", default=None,
                   help="comma-separated years of the records to complete (default: all)")
    p.add_argument("--target-sources", default=None,
                   help="comma-separated sources of the records to complete "
                        "(default: all records of the split)")
    p.add_argument("--complete-as-source", "--as-source", dest="as_source", default=None,
                   help="complete the targets as if from this source")
    p.add_argument("--decode-year", type=float, default=None,
                   help="year passed to a_tokenizer.decode (2015 for PUF wealth)")
    p.add_argument("--hide-group", default="scf_only",
                   help="meta.json group hidden in target records before "
                        "completion ('' for none)")
    p.add_argument("--draws", type=int, default=5)
    p.add_argument("--k", type=int, default=20)
    p.add_argument("--seed", type=int, default=20261011,
                   help="seed of the completion draws")
    p.add_argument("--batch-size", type=int, default=16384)
    p.add_argument("--temperature", type=float, default=1.0)
    p.add_argument("--reverse-wealth-order", action="store_true",
                   help="diagnostic: reverse SCF-only variable order, preserving shared-variable order")
    p.add_argument("--max-records", type=int, default=None,
                   help="complete only the first N target records (testing)")
    p.add_argument("--no-score", action="store_true")
    p.add_argument("--no-decode", action="store_true")
    p.add_argument("--decode-only", action="store_true",
                   help="add dollars to existing completed files and exit")
    return p.parse_args()


def import_tokenizer():
    path = CODE_DIR / "a_tokenizer.py"
    if not path.exists():
        return None, "a_tokenizer.py not found"
    try:
        spec = importlib.util.spec_from_file_location("a_tokenizer", path)
        mod = importlib.util.module_from_spec(spec)
        sys.modules["a_tokenizer"] = mod
        spec.loader.exec_module(mod)
        if not hasattr(mod, "decode"):
            return None, "a_tokenizer has no decode"
        return mod, None
    except Exception as e:                       # report, do not hide
        return None, f"import failed: {e!r}"


def weight_values_from_meta(v):
    """Weight value of each token from meta.json, if the tokens carry one
    ('value'); used for the fabricated data. Returns None otherwise (A's
    tokens carry rank bands, so their values come from the decoder)."""
    toks = v.get("tokens")
    n = int(v["vocab_size"])
    if not isinstance(toks, list) or len(toks) != n or \
            any(t.get("value") is None for t in toks):
        return None
    out = np.array([float(t["value"]) for t in toks])
    if not np.all(np.isfinite(out)) or (out <= 0).any():
        raise ValueError(f"weight token values not all positive and finite: {out}")
    return out


def weight_tables_from_decoder(tokmod, meta, data, recs, decode_year, seed, reps=400):
    """Mean decoded weight of each weight token, for each (source, year) of
    the target records: decode every token `reps` times (rank drawn inside
    the token's band) through a_tokenizer.decode and average. Returns
    (table [groups, vocab], group index per target record)."""
    vars_ = all_variables(meta)
    jw = [j for j, v in enumerate(vars_) if v.get("kind") == "weight"][0]
    nw = int(vars_[jw]["vocab_size"])
    quant = pd.read_parquet(data / "quantiles.parquet")
    keys = list(zip(recs["source"].astype(str), recs["year"].astype(int)))
    groups = sorted(set(keys))
    gidx = np.array([groups.index(k) for k in keys])
    rng = np.random.default_rng(seed)
    table = np.empty((len(groups), nw))
    for g, (s, y) in enumerate(groups):
        tok = np.full((nw * reps, len(vars_)), -1, dtype=np.int64)
        tok[:, jw] = np.repeat(np.arange(nw), reps)
        r = pd.DataFrame({"source": [s] * len(tok), "year": [y] * len(tok)})
        dec = decode_frame(tokmod, tok, meta, r, quant, rng, decode_year)
        table[g] = dec[vars_[jw]["name"]].to_numpy(float).reshape(nw, reps).mean(axis=1)
    if not np.all(np.isfinite(table)) or (table <= 0).any():
        raise ValueError(f"decoded weight table not all positive and finite: {table}")
    return table, gidx, groups


def allowed_code_tables(tokmod, meta, data, recs, decode_year, svars, max_v):
    """Which codes the decoder can turn into dollars for each (source, year)
    of the target records. An amount's negative (or positive) codes are
    allowed only if every quantile function a_tokenizer.decode would use for
    that record has a negative (positive) side; e.g. SCF 2016 has no
    negative home values, so a negative home-value code cannot be decoded
    for a 2016 record (or for a PUF record at 2015, which uses 2013 and
    2016). Mirrors the basis choice in a_tokenizer.decode. Returns
    (table [groups, V, max_v] bool, group index per record, notes)."""
    quant = pd.read_parquet(data / "quantiles.parquet")
    cache = tokmod._qf_cache(quant)
    keys = list(zip(recs["source"].astype(str), recs["year"].astype(int)))
    groups = sorted(set(keys))
    gidx = np.array([groups.index(k) for k in keys])
    V = len(svars)
    table = np.zeros((len(groups), V, max_v), dtype=bool)
    notes = []
    for g, (s, y) in enumerate(groups):
        for jj, v in enumerate(svars):
            n = int(v["vocab_size"])
            table[g, jj, :n] = True
            if v.get("kind") != "amount":
                continue
            own = tokmod._rank_basis(v, s, int(y), cache)
            basis = own if own[0][0] == s else tokmod._rank_basis(
                v, s, int(decode_year) if decode_year is not None else int(y), cache)
            kinds = np.array([t.get("kind") for t in v["tokens"]])
            for side in ("neg", "pos"):
                if not (kinds == side).any():
                    continue
                if any((bs, by, v["name"], side) not in cache for bs, by, _ in basis):
                    table[g, jj, :n][kinds == side] = False
                    notes.append(f"{s} {y} {v['name']}: no {side} values in "
                                 f"{[(bs, by) for bs, by, _ in basis]}; {side} codes not drawn")
    return table, gidx, notes


def decode_frame(tokmod, tok_full, meta, records, quantiles, rng, year):
    """Call a_tokenizer.decode; return a DataFrame of dollars, one row per
    record, one column per decoded variable."""
    res = tokmod.decode(tok_full, meta, records, quantiles, rng, year=year)
    if isinstance(res, pd.DataFrame):
        df = res.reset_index(drop=True)
    elif isinstance(res, dict):
        df = pd.DataFrame(res)
    else:
        arr = np.asarray(res)
        df = pd.DataFrame(arr, columns=[v["name"] for v in all_variables(meta)])
    if len(df) != len(records):
        raise ValueError(f"decode returned {len(df)} rows for {len(records)} records")
    return df


def add_dollars(df, meta, data, rec_cols, tokmod, rng, decode_year, observed_mask):
    """df: completed frame with tok_<name> columns, record columns, and a
    dollar column per variable holding the true dollars where observed and
    NaN where completed. Fills the completed ones with decoded dollars."""
    names = [v["name"] for v in all_variables(meta)]
    tok_full = np.stack([df[f"tok_{n}"].to_numpy() for n in names], axis=1)
    quant = pd.read_parquet(data / "quantiles.parquet")
    recs = df[rec_cols].reset_index(drop=True)
    dec = decode_frame(tokmod, tok_full, meta, recs, quant, rng, decode_year)
    for j, n in enumerate(names):
        if n not in dec.columns:
            continue
        obs = observed_mask[:, j]
        if n in df.columns:
            df[n] = np.where(obs, df[n].to_numpy(dtype=float),
                             dec[n].to_numpy(dtype=float))
        else:
            df[n] = dec[n].to_numpy(dtype=float)
    return df


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
    base = f"completed_{a.tag}"
    info_path = out / f"{base}_seed{a.train_seed:02d}_info.json"

    if a.decode_only:
        tokmod, err = import_tokenizer()
        if tokmod is None:
            raise RuntimeError(f"cannot decode: {err}")
        with open(info_path) as f:
            info = json.load(f)
        for mode in ("plain", "wc"):
            fn = out / f"{base}_{mode}_seed{a.train_seed:02d}.parquet"
            df = pd.read_parquet(fn)
            obs = np.stack([df[f"obs_{n}"].to_numpy() for n in names_all], axis=1)
            df = add_dollars(df, meta, data, info["record_columns"], tokmod,
                             np.random.default_rng(a.seed + (mode == "wc")),
                             info["decode_year"], obs)
            df.to_parquet(fn, index=False)
            print(f"decoded {fn}", flush=True)
        info["decoded"] = True
        info["decode_error"] = None
        with open(info_path, "w") as f:
            json.dump(info, f, indent=2)
        return

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    amp = device.type == "cuda"
    gpu_name = torch.cuda.get_device_name(0) if device.type == "cuda" else "cpu"
    model, ck = load_checkpoint(a.checkpoint, meta, device)
    sources = ck["sources"]

    sp = load_split(data, a.split, meta, sources)
    rec = sp["records"]
    rows = np.arange(len(rec))
    if a.target_sources:
        ts = a.target_sources.split(",")
        rows = rows[rec["source"].astype(str).isin(ts).to_numpy()[rows]]
    if a.target_years:
        ty = [int(x) for x in a.target_years.split(",")]
        rows = rows[rec["year"].astype(int).isin(ty).to_numpy()[rows]]
    if a.max_records:
        rows = rows[:a.max_records]
    N = len(rows)
    if N == 0:
        raise ValueError("no target records")
    tok_obs = sp["tok"][rows].copy()                      # slot columns, -1 unobserved
    src = sp["src"][rows].copy()
    if a.as_source:
        src[:] = sources.index(a.as_source)
    year = sp["year"][rows].astype(np.float32)

    hide = np.zeros(V, dtype=bool)
    if a.hide_group:
        hide = np.array([v.get("group") == a.hide_group for v in svars])
    tok_in = tok_obs.copy()
    tok_in[:, hide] = -1
    n_hidden_observed = int(((tok_obs >= 0) & hide[None, :]).sum())
    print(f"{N} target records ({a.target_sources or 'all'}) from {a.split}; "
          f"as source {a.as_source or 'own'}; hid {n_hidden_observed} observed values "
          f"of group '{a.hide_group}'; unobserved values to fill: "
          f"{int((tok_in < 0).sum())}; GPU {gpu_name}", flush=True)

    # Variables dropped in training (always hidden, never learned) are not
    # filled: they stay -1 (NaN in dollars). With the weight dropped there
    # is no weight correction.
    dropped = [x for x in (ck.get("args") or {}).get("drop_vars", "").split(",") if x]
    drop_j = [snames.index(x) for x in dropped]
    order = [j for j in fill_order(meta, svars) if j not in drop_j]
    if a.reverse_wealth_order:
        reversed_wealth = iter(reversed([j for j in order if svars[j].get("group") == "scf_only"]))
        order = [next(reversed_wealth) if svars[j].get("group") == "scf_only" else j
                 for j in order]
        print("diagnostic: reversed wealth completion order", flush=True)
    fillable = np.ones(V, dtype=bool)
    fillable[drop_j] = False
    if dropped:
        print(f"variables dropped in training, left unfilled: {dropped}", flush=True)
    wname = [v["name"] for v in svars if v.get("kind") == "weight" and v["name"] not in dropped]
    w_j = snames.index(wname[0]) if wname else None
    tokmod, dec_err = (None, "--no-decode") if a.no_decode else import_tokenizer()
    w_table = None                                   # [N, vocab of weight]
    w_source = None
    if w_j is not None:
        wv = weight_values_from_meta(svars[w_j])
        if wv is not None:
            w_table = np.broadcast_to(wv, (N, len(wv)))
            w_source = "meta.json token values"
        else:
            tm, err = import_tokenizer()
            if tm is None:
                raise RuntimeError(f"weight correction needs a_tokenizer: {err}")
            tab, gidx, groups = weight_tables_from_decoder(
                tm, meta, data, rec.iloc[rows].reset_index(drop=True),
                a.decode_year, a.seed + 99)
            w_table = tab[gidx]
            w_source = {f"{s} {y}": [round(float(x), 2) for x in tab[g]]
                        for g, (s, y) in enumerate(groups)}
            print(f"weight value of each token, by source and year (decoded): "
                  f"{json.dumps(w_source)}", flush=True)
    else:
        print("no weight variable: wc completion equals plain", flush=True)

    # Codes the decoder can turn into dollars, per target record.
    allowed_t, allowed_g, allowed_notes = None, None, []
    if tokmod is not None:
        tab, gi, allowed_notes = allowed_code_tables(
            tokmod, meta, data, rec.iloc[rows].reset_index(drop=True), a.decode_year,
            svars, model.max_v)
        allowed_t = torch.as_tensor(tab, device=device)
        allowed_g = torch.as_tensor(np.repeat(gi, a.k))
        for n_ in allowed_notes:
            print("allowed codes:", n_, flush=True)

    # Scoring against the truth.
    scores = None
    if not a.no_score:
        tf = data / f"truth_{a.split}.npy"
        truth = np.load(tf)[:, scols][rows].astype(np.int64) if tf.exists() else tok_obs
        truth = np.where(tok_obs >= 0, tok_obs, truth)
        truth[:, drop_j] = -1
        ts0 = time.time()
        nll, scm = completion_nll(model, tok_in, truth, src, year, order, 8192, device, amp)
        if scm.any():
            per_rec = nll.sum(axis=1)
            anyv = scm.any(axis=1)
            scores = dict(
                n_records_scored=int(anyv.sum()),
                nll_per_record=float(per_rec[anyv].mean()),
                by_variable={snames[j]: dict(n=int(scm[:, j].sum()),
                                             nll=float(nll[scm[:, j], j].mean()))
                             for j in range(V) if scm[:, j].any()},
                seconds=time.time() - ts0)
            print(f"completion NLL of true values: {scores['nll_per_record']:.4f} per "
                  f"record over {scores['n_records_scored']} records", flush=True)

    # Completion.
    gen = torch.Generator(device=device)
    gen.manual_seed(a.seed)
    rng_sel = np.random.default_rng(a.seed)
    K = a.k
    plain = np.empty((a.draws, N, V), dtype=np.int64)
    wc = np.empty((a.draws, N, V), dtype=np.int64)
    ess = []
    t0 = time.time()
    exp_tok = torch.as_tensor(np.repeat(tok_in, K, axis=0))        # [N*K, V]
    exp_src = torch.as_tensor(np.repeat(src, K))
    exp_year = torch.as_tensor(np.repeat(year, K))
    for d in range(a.draws):
        res = np.empty((N * K, V), dtype=np.int64)
        for s0 in range(0, N * K, a.batch_size):
            sl = slice(s0, s0 + a.batch_size)
            allw = (allowed_t[allowed_g[sl].to(device)] if allowed_t is not None else None)
            res[sl] = complete_batch(model, exp_tok[sl].to(device), exp_src[sl].to(device),
                                     exp_year[sl].to(device), order, gen,
                                     a.temperature, amp, allowed=allw).cpu().numpy()
        res = res.reshape(N, K, V)
        plain[d] = res[:, 0, :]
        if w_j is not None:
            w = np.take_along_axis(w_table, res[:, :, w_j], axis=1)   # [N, K]
            # Records whose weight was observed keep it in every draw; the
            # selection is then uniform, as it should be.
            p = w / w.sum(axis=1, keepdims=True)
            c = p.cumsum(axis=1)
            u = rng_sel.random((N, 1))
            pick = (u > c).sum(axis=1).clip(max=K - 1)
            wc[d] = res[np.arange(N), pick, :]
            ess.append(float(np.mean(w.sum(1) ** 2 / (w ** 2).sum(1))))
        else:
            wc[d] = res[:, 0, :]
        print(f"draw {d + 1}/{a.draws} done, {time.time() - t0:.1f}s", flush=True)
    comp_seconds = time.time() - t0
    assert (plain[:, :, fillable] >= 0).all() and (wc[:, :, fillable] >= 0).all(), \
        "unfilled slots remain"
    keep_obs = tok_in >= 0
    for arr in (plain, wc):
        assert (arr[:, keep_obs] == np.broadcast_to(tok_in, arr.shape)[:, keep_obs]).all(), \
            "an observed value changed during completion"

    # Assemble output frames.
    var_set = set(snames)          # context variables (source) stay record columns
    rec_cols = [c for c in rec.columns if c not in var_set]
    rec_t = rec.iloc[rows].reset_index(drop=True)
    tokall_rows = sp["tok_all"][rows]
    observed_full = np.ones((N, len(names_all)), dtype=bool)
    for jj, c in enumerate(scols):
        observed_full[:, c] = tok_in[:, jj] >= 0

    files = {}
    for mode, arr in (("plain", plain), ("wc", wc)):
        frames = []
        for d in range(a.draws):
            df = pd.DataFrame({"row": rows.astype(np.int64)})
            for c in rec_cols:
                df[c] = rec_t[c].to_numpy()
            full = tokall_rows.astype(np.int64).copy()
            full[:, scols] = arr[d]
            for jj, n in enumerate(names_all):
                if n in var_set and n in rec_t.columns:
                    # True dollars where observed; completed values are
                    # filled by the decoder (NaN until then).
                    df[n] = np.where(observed_full[:, jj],
                                     rec_t[n].to_numpy(dtype=float), np.nan)
                df[f"tok_{n}"] = full[:, jj].astype(np.int32)
                df[f"obs_{n}"] = observed_full[:, jj]
            df["draw"] = d
            frames.append(df)
        df = pd.concat(frames, ignore_index=True)
        obs_stack = np.tile(observed_full, (a.draws, 1))
        if tokmod is not None:
            try:
                df = add_dollars(df, meta, data, rec_cols, tokmod,
                                 np.random.default_rng(a.seed + (mode == "wc")),
                                 a.decode_year, obs_stack)
            except Exception as e:                 # keep tokens; report
                dec_err = f"decode failed: {e!r}"
                print(dec_err, flush=True)
                tokmod = None
        fn = out / f"{base}_{mode}_seed{a.train_seed:02d}.parquet"
        df.to_parquet(fn, index=False)
        files[mode] = str(fn)

    info = dict(tag=a.tag, train_seed=a.train_seed, checkpoint=str(a.checkpoint),
                checkpoint_epoch=ck.get("epoch"), checkpoint_val_loss=ck.get("val_loss"),
                model_config=ck.get("config"), trained_sources=ck.get("trained_sources"),
                split=a.split, target_sources=a.target_sources, target_years=a.target_years, as_source=a.as_source,
                n_records=int(N), draws=a.draws, k=K, completion_seed=a.seed,
                hide_group=a.hide_group, hidden_observed_values=n_hidden_observed,
                fill_order=[snames[j] for j in order], temperature=a.temperature,
                decode_year=a.decode_year, decoded=tokmod is not None,
                decode_error=dec_err if tokmod is None else None,
                record_columns=rec_cols,
                mean_effective_draws_of_K=(float(np.mean(ess)) if ess else None),
                weight_token_values=w_source,
                codes_not_drawn=allowed_notes,
                completion_seconds=comp_seconds, gpu=gpu_name, scores=scores,
                files=files, slurm_job_id=os.environ.get("SLURM_JOB_ID"))
    with open(info_path, "w") as f:
        json.dump(info, f, indent=2)
    print(json.dumps({k: v for k, v in info.items() if k != "fill_order"},
                     indent=2), flush=True)


if __name__ == "__main__":
    main()
