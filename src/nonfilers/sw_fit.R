# =============================================================================
# sw_fit.R  --  split out of state_weights.R (F4, 2026-09-23)
#
# The split-weight fit: PUF target series, HT2 stub assignment, target assembly
# # (build_weight_inputs), the two engines and build_split_weights.
#
# Code moved verbatim; see state_weights.R for the module map.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(stringr)
})

# =============================================================================
# PUF-SIDE TARGET ASSEMBLY (plan §2.1)
#
# Maps PUF records onto the ingested targets and builds the (w, P0, targets)
# inputs both engines consume, per partition:
#   filers     -> HT2 state x AGI-stub cells, one target per (state, stub,
#                 series) with shared row/x vectors per (stub, series)
#   non-filers -> one adult count per state from the netted residual anchor
#                 (S37), from an ACS state x age-band x income-tier prior
#
# NORMALIZATION (key design point): split weights control only the GEOGRAPHIC
# distribution -- national totals are fixed by construction. Raw HT2/ACS
# levels differ from PUF nationals (coverage, vintage), which would make
# level targets infeasible. So each target is the PUF's own national total
# for that (cell, series), distributed across states by the HT2/ACS SHARES:
#   target(st, cell, v) = PUF_total(cell, v) * share_HT2(st | cell, v).
# Diagnostics against raw HT2 levels remain a separate validation step.
#
# v1 documented approximations:
#   - capital gains in AGI proxied by kg_st + kg_lt with the $3,000 loss cap
#   - SALT income+sales targeted combined (PUF salt_inc_sales is the greater-
#     of election; HT2 splits A18425/A18450 -- elections are mutually
#     exclusive per return, so counts/amounts add)
#   - stub-1 (negative-AGI) amount targets are skipped by the calibration
#     engine's positivity guard; count targets still bind there
#   - non-filer gross income proxy mirrors the ACS INCTOT unit construction
# =============================================================================

# PUF x-vector for one HT2 target series (NULL if inputs absent)
puf_series_x <- function(tu, series) {
  has <- function(...) all(c(...) %in% names(tu))
  kg_agi <- function() {
    kg <- tu$kg_st + tu$kg_lt
    fifelse(kg < 0, pmax(kg, -3000), kg)
  }
  switch(series,
    n_returns  = rep(1, nrow(tu)),
    n_single   = as.numeric(tu$filing_status == 1),
    n_joint    = as.numeric(tu$filing_status == 2),
    n_hoh      = as.numeric(tu$filing_status == 4),
    # ADULTS on the return: the D5 concept. Distinct from n_indiv, which is
    # HT2's "individuals" and counts dependents too. The non-filer partition
    # targets adults because its anchors do (PEP adults - HT2 filing adults),
    # and a unit-denominated target compared against an adult-denominated
    # anchor is a units-vs-persons mismatch, not a fit.
    n_adults   = 1 + (tu$filing_status == 2),
    n_indiv    = 1 + (tu$filing_status == 2) + tu$n_dep,
    agi_amt    = if (has("agi")) tu$agi,
    n_wages    = if (has("wages")) as.numeric(tu$wages != 0),
    wages_amt  = if (has("wages")) tu$wages,
    n_int      = if (has("txbl_int")) as.numeric(tu$txbl_int != 0),
    int_amt    = if (has("txbl_int")) tu$txbl_int,
    n_div      = if (has("div_ord","div_pref")) as.numeric(tu$div_ord + tu$div_pref != 0),
    div_amt    = if (has("div_ord","div_pref")) tu$div_ord + tu$div_pref,
    n_kg       = if (has("kg_st","kg_lt")) as.numeric(kg_agi() != 0),
    kg_amt     = if (has("kg_st","kg_lt")) kg_agi(),
    n_salt     = if (has("salt_inc_sales")) as.numeric(tu$salt_inc_sales != 0),
    salt_amt   = if (has("salt_inc_sales")) tu$salt_inc_sales,
    n_re_tax   = if (has("salt_prop")) as.numeric(tu$salt_prop != 0),
    re_tax_amt = if (has("salt_prop")) tu$salt_prop,
    n_mort_int = if (has("first_mort_int","second_mort_int"))
                   as.numeric(tu$first_mort_int + tu$second_mort_int != 0),
    mort_int_amt = if (has("first_mort_int","second_mort_int"))
                   tu$first_mort_int + tu$second_mort_int,
    n_eitc     = if (has("eitc")) as.numeric(tu$eitc > 0),
    eitc_amt   = if (has("eitc")) tu$eitc,
    NULL)
}

# HT2 series consumed per PUF series (salt combines income + sales elections)
HT2_SERIES_FOR <- list(
  n_returns = "n_returns", n_single = "n_single", n_joint = "n_joint",
  n_hoh = "n_hoh", n_indiv = "n_indiv",
  agi_amt = "agi_amt", n_wages = "n_wages", wages_amt = "wages_amt",
  n_int = "n_int", int_amt = "int_amt", n_div = "n_div", div_amt = "div_amt",
  n_kg = "n_kg", kg_amt = "kg_amt",
  n_salt = c("n_salt_inc","n_salt_sales"), salt_amt = c("salt_inc_amt","salt_sales_amt"),
  n_re_tax = "n_re_tax", re_tax_amt = "re_tax_amt",
  n_mort_int = "n_mort_int", mort_int_amt = "mort_int_amt",
  n_eitc = "n_eitc", eitc_amt = "eitc_amt")

# Assign each record to its HT2 AGI stub (lower <= agi < upper)
assign_ht2_stub <- function(agi, year) {
  breaks <- ht2_stub_breaks(year)
  findInterval(agi, c(breaks$lower, Inf), rightmost.closed = FALSE)
}

# Gross-income proxy for non-filer cell assignment, mirroring the ACS INCTOT
# unit construction (sum of positive incomes)
puf_gross_income <- function(tu) {
  pmax(tu$wages, 0) + pmax(tu$txbl_int, 0) + pmax(tu$exempt_int, 0) +
    pmax(tu$div_ord + tu$div_pref, 0) + pmax(tu$kg_st + tu$kg_lt, 0) +
    pmax(tu$txbl_pens_dist, 0) + pmax(tu$txbl_ira_dist, 0) +
    pmax(tu$gross_ss, 0) + pmax(tu$sole_prop, 0)
}

# =============================================================================
# NON-FILER STATE ANCHOR (S37, the Phase 2 bake-off's arm B)
#
# The non-filer partition targets one adult count per state: the residual
# anchor (Census PEP adults - IRS filing adults, less ACS dorm students; built
# by nonfiler_residual/02) NET of the claimed adult dependents who do not file.
# The residual counts those dependents; the pool does not contain them -- they
# ride on filers' returns -- so a share of the gross residual would assume
# they are spread across states like non-filers are. They are not: the
# bake-off's held-out metrics prefer the netted anchor in both TY2022 and
# TY2023, in 200/200 household-bootstrap replicates of the ASEC netting.
#
# The netting is the pool's own (04_calibrate.R's join, calibrated filing
# probabilities), summed by the dependent's ASEC state. ASEC puts dorm students
# at their parents' address and PEP puts them at school; the anchor's dorm
# netting already removes them at school, so the ASEC's dorm share is added
# back at the parents' state -- the national ACS dorm count, allocated by the
# ASEC's non-filing full-time students of DORM_AGES claimed as dependents.
# =============================================================================

RESIDUAL_ANCHORS_PATH <- "research/state_weights/nonfiler_residual/results/residual_anchors_%d.csv"
NONFILER_POOL_RESULTS <- "research/state_weights/nonfiler_pool/results"
DORM_AGES <- 18:24

#' Claimed adult dependents in the non-filer pool's ASEC, one row per person,
#' with the weight that nets out of the residual (ASECWT x (1 - p_file)).
#' Reads the pool's gitignored intermediates, so it is a PRODUCER helper --
#' nonfiler_pool/17_netting_by_state.R and the bake-off call it; production
#' reads the committed CSV instead. Ties out to the pool's national netting.
asec_adult_dependents <- function(year) {
  st <- readRDS(file.path(NONFILER_POOL_RESULTS, sprintf("units_%d.rds", year)))
  cu <- readRDS(file.path(NONFILER_POOL_RESULTS, sprintf("calibrated_units_%d.rds", year)))
  deps <- st$persons[is_dependent == TRUE & AGE >= 18,
                     .(SERIAL, PERNUM, AGE, ASECWT, STATEFIP, is_full_time_student,
                       unit_id = as.numeric(SERIAL) * 100 + PERNUM + 1e9)]
  deps <- merge(deps, cu[unit_type == "dependent", .(unit_id, p_file_cal)],
                by = "unit_id", all.x = TRUE)
  stopifnot(!anyNA(deps$p_file_cal), !anyNA(deps$is_full_time_student))
  deps[, state := FIPS_TO_STATE[as.character(STATEFIP)]]
  stopifnot(!anyNA(deps$state))
  deps[, w_net := ASECWT * (1 - p_file_cal)]
  sens <- fread(file.path(NONFILER_POOL_RESULTS, sprintf("netting_sensitivity_%d.csv", year)))
  stopifnot(abs(deps[, sum(w_net)] / sens[scenario == "central", sum(dep_nonfiling)] - 1) < 1e-9)
  deps[]
}

#' Sum the netting by state (for the committed CSV, and the bake-off bootstrap)
netting_by_state <- function(deps) {
  deps[, .(asec_netting = sum(w_net), asec_netting_n = .N,
           asec_ft_student = sum(w_net * (is_full_time_student & AGE %in% DORM_AGES))),
       keyby = state]
}

#' Combine the residual anchors with the netting: data.table(state, anchor)
nonfiler_state_anchor <- function(anch, net) {
  a <- merge(anch[, .(state, anchor_net_dorm = residual_nonfiling_adults_net_dorm,
                      dorm_dependents)],
             net, by = "state")
  stopifnot(nrow(a) == 51, !anyNA(a))
  a[, dorm_at_parents := sum(dorm_dependents) * asec_ft_student / sum(asec_ft_student)]
  a[, anchor := anchor_net_dorm - asec_netting + dorm_at_parents]
  stopifnot(all(a$anchor > 0))
  a[, .(state, anchor, anchor_net_dorm, asec_netting, dorm_at_parents, dorm_dependents)]
}

read_nonfiler_state_anchor <- function(year) {
  nonfiler_state_anchor(
    fread(sprintf(RESIDUAL_ANCHORS_PATH, year)),
    fread(file.path(NONFILER_POOL_RESULTS, sprintf("netting_by_state_%d.csv", year))))
}

#' One adult-count target per anchor state: the partition's national adult
#' total x the state's anchor share. `anchor` is data.table(state, anchor).
nonfiler_state_targets <- function(w, x_adults, anchor, jurisdictions) {
  stopifnot(all(anchor$state %in% jurisdictions), all(anchor$anchor > 0))
  total <- sum(w * x_adults)
  rows <- seq_along(w)
  lapply(seq_len(nrow(anchor)), function(k) list(
    rows = rows, state = match(anchor$state[k], jurisdictions), x = x_adults,
    target = total * anchor$anchor[k] / sum(anchor$anchor), lambda = 1,
    series = "n_adults_state", state_code = anchor$state[k]))
}

#' Assemble engine inputs for one year: partitions, priors, and targets.
#' @param tax_units PUF records incl. baseline-calculated `agi` and `eitc`
#'        (join from baseline detail when running standalone)
#' @param ht2 long target table from read_ht2(); read from the store if NULL
#' @param acs_margins list from build_acs_margins(); built if NULL (slow)
#' @param nonfiler_anchor data.table(state, anchor); read_nonfiler_state_anchor()
#'        if NULL
#' @return list(jurisdictions, filers = list(idx, w, P0, targets),
#'              nonfilers = list(idx, w, P0, targets))
build_weight_inputs <- function(tax_units, year, ht2 = NULL, acs_margins = NULL,
                                nonfiler_anchor = NULL, verbose = TRUE) {

  tu <- as.data.table(tax_units)
  if (is.null(ht2)) ht2 <- read_ht2(ht2_path(year), year)
  if (is.null(acs_margins)) {
    acs_margins <- build_acs_margins(read_acs_extract(year), year)
  }

  # Jurisdiction order defines P0/engine state indices: HT2 areas as published
  # (51 + OA, + PR from 2018), sorted for determinism
  jurisdictions <- sort(unique(ht2$state))
  S <- length(jurisdictions)
  st_idx <- setNames(seq_len(S), jurisdictions)

  #---------------------------
  # Filer partition (HT2)
  #---------------------------

  idx_f <- which(tu$filer == 1)
  tu_f  <- tu[idx_f]
  stub  <- assign_ht2_stub(tu_f$agi, year)

  # `agi` is baseline-CALCULATED, not carried on the raw Tax-Data file, so a
  # standalone caller that forgets the detail join gets an empty stub vector,
  # zero filer targets, and an all-zero P0 -- which only surfaces later, as an
  # invariant assertion inside build_split_weights(). Say so here instead.
  # This is a warning and not a stop because the non-filer-only callers
  # (scripts/f1_adult_target_check.R) rely on the empty filer partition.
  if (nrow(tu_f) > 0 && (length(stub) != nrow(tu_f) || all(is.na(stub)))) {
    warning("build_weight_inputs(): no usable `agi` on the filer partition, so ",
            "it will emit ZERO targets and an all-zero prior. Join `agi`/`eitc` ",
            "from a baseline static detail file (see @param tax_units).",
            call. = FALSE, immediate. = TRUE)
  }

  # Prior: HT2 return shares within stub
  ht2_ret <- dcast(ht2[variable == "n_returns"], agi_stub ~ state, value.var = "value")
  P0_f <- matrix(0, nrow(tu_f), S)
  for (s10 in sort(unique(stub))) {
    shares <- as.numeric(ht2_ret[agi_stub == s10, ..jurisdictions])
    shares <- pmax(shares, 0); shares <- shares / sum(shares)
    P0_f[stub == s10, ] <- matrix(shares, sum(stub == s10), S, byrow = TRUE)
  }

  # Targets: PUF national totals per (stub, series), distributed by HT2 shares.
  # kg_amt is excluded: capital-gains x-vectors are sign-mixed within stubs
  # and several (stub, state) HT2 cells are net-negative -- negative targets
  # flip multiplicative-IPF cells negative and poison entire state columns
  # (root-caused 2026-07-13; n_kg still binds; the gradient engine can
  # reclaim kg_amt later since it has no positivity constraint)
  targets_f <- list()
  skipped <- character(0)
  n_dropped_nonpos <- 0
  for (series in setdiff(names(HT2_SERIES_FOR), "kg_amt")) {
    x_all <- puf_series_x(tu_f, series)
    if (is.null(x_all)) { skipped <- c(skipped, series); next }
    ht2_v <- ht2[variable %in% HT2_SERIES_FOR[[series]],
                 .(value = sum(value)), by = .(state, agi_stub)]
    for (s10 in sort(unique(stub))) {
      rows  <- which(stub == s10)
      x     <- x_all[rows]
      puf_total <- sum(tu_f$weight[rows] * x)
      cell  <- ht2_v[agi_stub == s10]
      denom <- sum(cell$value)
      if (denom <= 0 || puf_total == 0) next
      for (k in seq_len(nrow(cell))) {
        tgt <- puf_total * cell$value[k] / denom
        if (!is.finite(tgt)) next
        # Non-positive targets (negative HT2 cells, e.g. agi_amt in stub 1)
        # cannot be calibrated multiplicatively; drop and count
        if (tgt <= 0) { n_dropped_nonpos <- n_dropped_nonpos + 1; next }
        targets_f[[length(targets_f) + 1]] <- list(
          rows = rows, state = st_idx[[cell$state[k]]], x = x,
          target = tgt, lambda = 1,
          # metadata (ignored by the engines; used by diagnostics)
          series = series, stub = s10, state_code = cell$state[k])
      }
    }
  }
  if (verbose) {
    message(sprintf("  filer targets: %d (%d records; %d non-positive dropped%s)",
                    length(targets_f), nrow(tu_f), n_dropped_nonpos,
                    if (length(skipped)) paste0("; series without inputs: ",
                                                paste(skipped, collapse = " ")) else ""))
  }

  #---------------------------
  # Non-filer partition (ACS)
  #---------------------------

  idx_n <- which(tu$filer == 0)
  tu_n  <- tu[idx_n]
  nm    <- as.data.table(acs_margins$nonfiler_margins)
  cell_n <- paste(age_band(tu_n$age1), income_tier(puf_gross_income(tu_n)), sep = "|")
  nm[, cell := paste(age_band, income_tier, sep = "|")]

  # PRIOR is unit-denominated, TARGET is adult-denominated. Not an oversight:
  # a PUF non-filer record IS a tax unit, so the question "where does this
  # record live" is answered by where non-filer UNITS live; the constraint the
  # fit must satisfy is a count of ADULTS, because that is what the residual
  # anchors measure (PEP adults - HT2 filing adults). Mixing the two was the
  # latent problem D5 names. 17.2% of non-filer units are joint, so
  # adults/units = 1.17 nationally and varies by state with the married mix.
  overall <- nm[, .(n = sum(n_units)), by = state]
  overall_shares <- setNames(rep(0, S), jurisdictions)
  overall_shares[overall$state] <- overall$n / sum(overall$n)

  x_adults <- puf_series_x(tu_n, "n_adults")   # 1 + (filing_status == 2), D5

  # The ACS cells are the PRIOR only (F2): state x age band x income tier
  # shape where each record starts. Until S37 they were also the targets, so
  # the fit reproduced its prior exactly (2 IPF passes to 4.4e-16).
  P0_n <- matrix(0, nrow(tu_n), S)
  for (cl in sort(unique(cell_n))) {
    rows <- which(cell_n == cl)
    m_cl <- nm[cell == cl]
    if (nrow(m_cl) == 0) {
      P0_n[rows, ] <- matrix(overall_shares, length(rows), S, byrow = TRUE)
      next
    }
    shares <- setNames(rep(0, S), jurisdictions)
    shares[m_cl$state] <- m_cl$n_units / sum(m_cl$n_units)
    P0_n[rows, ] <- matrix(shares, length(rows), S, byrow = TRUE)
  }

  # The TARGETS (F1b, S37): one adult count per state, the partition's
  # national adult total split by the netted residual anchor. The ACS prior
  # puts no mass in PR or Other Areas, which is what lets 51 state targets
  # close the partition.
  if (is.null(nonfiler_anchor)) nonfiler_anchor <- read_nonfiler_state_anchor(year)
  stopifnot(all(P0_n[, !(jurisdictions %in% nonfiler_anchor$state)] == 0))
  targets_n <- nonfiler_state_targets(tu_n$weight, x_adults, nonfiler_anchor, jurisdictions)
  nf_adults <- sum(tu_n$weight * x_adults)
  if (verbose) {
    message(sprintf(paste("  non-filer targets: %d states (%d records, prior",
                          "from %d ACS cells; %.3fM adults from %.3fM units)"),
                    length(targets_n), nrow(tu_n), length(unique(cell_n)),
                    nf_adults / 1e6, sum(tu_n$weight) / 1e6))
  }

  list(jurisdictions = jurisdictions,
       filers    = list(idx = idx_f, w = tu_f$weight, P0 = P0_f, targets = targets_f),
       nonfilers = list(idx = idx_n, w = tu_n$weight, P0 = P0_n, targets = targets_n))
}

# Config 7, the Phase 1 bake-off winner (state_weights_phase1_summary.md §6):
# 95.3% of filer targets within 2%, MARD 0.43%, reproduced exactly 2026-09-23.
# It is a TWO-STAGE pipeline, not only a set of hyperparameters. A per-cell IPF
# multiplier cannot satisfy the ~21 series that constrain one stub x state cell
# (§3.2), so the return counts are fitted first by IPF, and the joint gradient
# fit starts from that prior rather than from the raw HT2 shares in P0.
CONFIG_7 <- list(prior_series = 'n_returns', prior_n_iter = 50L,
                 beta = 1e-4, lr = 0.1, n_steps = 3000L, lr_schedule = 'cosine')

#' Config 7's two stages on the filer partition: IPF on the return counts,
#' then the joint gradient fit started from that prior.
#' @param p the `filers` element of build_weight_inputs()
fit_filers_config7 <- function(p, verbose = FALSE) {
  counts <- Filter(function(t) t$series == CONFIG_7$prior_series, p$targets)
  prior  <- fit_calibration(p$w, p$P0, counts, n_iter = CONFIG_7$prior_n_iter)$P
  fit_gradient(p$w, prior, p$targets,
               beta = CONFIG_7$beta, lr = CONFIG_7$lr, n_steps = CONFIG_7$n_steps,
               lr_schedule = CONFIG_7$lr_schedule, verbose = verbose)
}

#' Fit both partitions and assemble long split weights.
#'
#' method = "gradient" is the production path: filers get config 7's two
#' stages; non-filers get exact IPF. method = "calibration" fits both
#' partitions by IPF alone.
#'
#' Non-filers are on IPF under either method because their targets are
#' single-membership count cells, which IPF fits exactly in one pass (§2), and
#' that is what the bake-off used for them (§5.4).
#'
#' There is no `...`: it used to forward hyperparameters to whichever engine
#' ran, nothing ever supplied them, and so config 7 existed in prose only while
#' the gradient engine ran its own defaults (plan section 5a, finding 1). Tune
#' through the sweep harness, which calls the engines directly.
#' @return data.table(id, state, weight) with Σ_state weight = national weight
build_split_weights <- function(tax_units, year,
                                method = c("calibration", "gradient"),
                                inputs = NULL, verbose = FALSE) {
  method <- match.arg(method)
  tu <- as.data.table(tax_units)
  if (is.null(inputs)) inputs <- build_weight_inputs(tu, year)

  out <- list()
  for (part in c("filers", "nonfilers")) {
    p <- inputs[[part]]
    if (length(p$idx) == 0) next

    fit <- if (method == "gradient" && part == "filers") {
      fit_filers_config7(p, verbose = verbose)
    } else {
      fit_calibration(p$w, p$P0, p$targets, verbose = verbose)
    }

    # The split constraint must hold EXACTLY here -- a violation is a defect
    # (the 2026-07-13 leak came from negative P cells being dropped by the
    # weight filter below; negative targets are now blocked at assembly)
    stopifnot(all(fit$P >= 0), max(abs(rowSums(fit$P) - 1)) < 1e-8)

    W <- p$w * fit$P
    dt <- data.table(id = rep(tu$id[p$idx], ncol(W)),
                     state = rep(inputs$jurisdictions, each = nrow(W)),
                     weight = as.vector(W))
    out[[part]] <- dt[weight > 0]
  }
  rbindlist(out)
}

# =============================================================================
# WEIGHTING ENGINES
#
# Both operate on a common problem: N records (a filer or non-filer partition),
# S states, national weights w (length N), a prior share matrix P0 (N×S, rows sum
# to 1), and a target list. They return a share matrix P (N×S) with W = w * P.
#
# A target is list(rows = integer idx into 1..N, state = s in 1..S, x = numeric
# length-|rows| record values, target = scalar, lambda = weight). The predicted
# total is  sum_{i in rows} w[i] * P[i, s] * x[i].  (x ≡ 1 for count targets,
# x = agi for amount targets.)  The shared invariant Σ_s P[i,s] = 1 holds by
# construction in both engines, so Σ_s W[i,s] = w[i] always.
# =============================================================================

# --- Approach B: differentiable reweighting (matrix analytic-gradient) --------
# torch is not installed on the cluster; this is a dependency-free implementation
# of the identical softmax objective (research/archive/state_weights_ml_alternative_2026-07-08_superseded.md §2). Logits
# theta (N×S); P = softmax(theta); loss = Σ_t lambda_t ((That_t-T_t)/T_t)^2 +
# beta * Σ_i KL(P[i,]||P0[i,]). Adam on theta.
#
# VECTORIZED (2026-07-13): targets sharing a rows set (all series within a
# stub, by assembly construction; identified via the stub/cell metadata) are
# stacked into one group -- X (n_g × K) holds the w·x columns, T/L (K × S)
# the targets/lambdas -- so each Adam step does ~10 GEMMs instead of ~10,700
# per-target scatter updates:
#   predicted totals (all series × states):  crossprod(X, P[rows, ])
#   gradient accumulation:                    G[rows, ] += X %*% C
# Targets without metadata become singleton groups (still exact; used by the
# self-test). Verified against finite differences in .state_weights_selftest().
fit_gradient <- function(w, P0, targets, beta = 1e-3,
                         lr = 0.1, n_steps = 500, verbose = FALSE,
                         lr_schedule = c("constant", "cosine"),
                         theta0 = NULL) {
  # lr_schedule: "cosine" anneals lr from lr to ~0 over n_steps (half-cosine);
  # theta0: optional warm-start logits (e.g. a prior run's theta). The KL
  # anchor stays P0 regardless -- warm-starting changes the path, not the
  # objective.
  lr_schedule <- match.arg(lr_schedule)
  N <- length(w); S <- ncol(P0)

  # ---- group targets by shared rows (stub / cell metadata; else singleton)
  key <- vapply(seq_along(targets), function(i) {
    t <- targets[[i]]
    if (!is.null(t$stub)) paste0("stub_", t$stub)
    else if (!is.null(t$cell)) paste0("cell_", t$cell)
    else paste0("tgt_", i)
  }, character(1))
  groups <- lapply(split(seq_along(targets), key), function(idx) {
    rows <- targets[[idx[1]]]$rows
    # one X column per distinct series in the group; map each target to its
    # column and its state
    ser <- vapply(idx, function(i) {
      t <- targets[[i]]
      if (is.null(t$series)) paste0("x", i) else t$series
    }, character(1))
    useries <- unique(ser)
    X <- matrix(0, length(rows), length(useries))
    Tm <- matrix(NA_real_, length(useries), S)
    Lm <- matrix(0, length(useries), S)
    for (j in seq_along(idx)) {
      t <- targets[[idx[j]]]
      stopifnot(identical(t$rows, rows))       # grouping precondition
      k <- match(ser[j], useries)
      X[, k] <- w[rows] * t$x
      Tm[k, t$state] <- t$target
      Lm[k, t$state] <- t$lambda
    }
    list(rows = rows, X = X, Tm = Tm, Lm = Lm, has = !is.na(Tm))
  })

  theta <- if (is.null(theta0)) log(pmax(P0, 1e-12)) else theta0  # warm start at the prior
  m <- matrix(0, N, S); v <- matrix(0, N, S)             # Adam moments
  b1 <- 0.9; b2 <- 0.999; eps <- 1e-8
  softmax_rows <- function(z) { z <- z - apply(z, 1, max); e <- exp(z); e / rowSums(e) }

  loss_hist <- numeric(n_steps)
  for (step in seq_len(n_steps)) {
    P <- softmax_rows(theta)
    G <- matrix(0, N, S)                                 # dLoss_fit / dP
    loss_fit <- 0
    for (g in groups) {
      That  <- crossprod(g$X, P[g$rows, , drop = FALSE]) # K × S predicted totals
      resid <- (That - g$Tm) / g$Tm                      # NA where untargeted
      loss_fit <- loss_fit + sum(g$Lm * resid^2, na.rm = TRUE)
      C <- ifelse(g$has, 2 * g$Lm * resid / g$Tm, 0)     # K × S coefficient
      G[g$rows, ] <- G[g$rows, ] + g$X %*% C
    }
    # KL(P||P0) gradient wrt P, plus softmax backprop:  gtheta = P*(g - rowSums(P*g))
    Greg <- beta * (log(pmax(P, 1e-12)) - log(pmax(P0, 1e-12)) + 1)
    g <- G + Greg
    gtheta <- P * (g - rowSums(P * g))
    # Adam
    m <- b1 * m + (1 - b1) * gtheta
    v <- b2 * v + (1 - b2) * gtheta^2
    mhat <- m / (1 - b1^step); vhat <- v / (1 - b2^step)
    lr_t <- if (lr_schedule == "cosine") lr * 0.5 * (1 + cos(pi * step / n_steps)) else lr
    theta <- theta - lr_t * mhat / (sqrt(vhat) + eps)
    loss_kl <- beta * sum(P * (log(pmax(P,1e-12)) - log(pmax(P0,1e-12))))
    loss_hist[step] <- loss_fit + loss_kl
    if (verbose && step %% max(1, n_steps %/% 10) == 0)
      message(sprintf("  step %d  loss_fit=%.4e  loss_kl=%.4e", step, loss_fit, loss_kl))
  }
  list(P = softmax_rows(theta), loss_hist = loss_hist, theta = theta)
}

# --- Approach A: classical raking / IPF ---------------------------------------
# Seeded at the prior P0, iteratively rescales each (target) so predicted totals
# approach targets, then renormalizes rows to restore Σ_s P[i,s] = 1. Count and
# amount targets are handled by the generic x-vector; targets and x must be
# non-negative (enforced at assembly). Hardened 2026-07-13 after root-causing
# the first full-scale fit:
#   - DAMPED: the per-pass factor is clamped to [1/f_max, f_max] so no single
#     extreme cell (OA/PR wages showed raw factors up to ~530) can distort the
#     shared rows for every other target in one pass; convergence is
#     geometric instead of cyclic
#   - denominator floor: targets whose predicted total is vanishingly small
#     relative to the target are skipped that pass and reported as unfittable
#     if still starved at exit (never silently no-opped)
fit_calibration <- function(w, P0, targets, n_iter = 200, tol = 1e-4,
                            f_max = 2, floor_rel = 1e-9, verbose = FALSE) {
  P <- P0
  for (iter in seq_len(n_iter)) {
    maxrel <- 0
    for (t in targets) {
      That <- sum(w[t$rows] * P[t$rows, t$state] * t$x)
      if (That > floor_rel * t$target) {
        f <- min(max(t$target / That, 1 / f_max), f_max)
        P[t$rows, t$state] <- P[t$rows, t$state] * f      # damped rescale
        maxrel <- max(maxrel, abs(t$target / That - 1))
      }
    }
    rs <- rowSums(P); P <- P / rs                          # restore row-sum = 1
    if (verbose && iter %% 10 == 0) message(sprintf("  iter %d  max|rel err|=%.4f", iter, maxrel))
    if (maxrel < tol) break
  }
  # Report targets still starved at exit
  unfittable <- which(vapply(targets, function(t) {
    sum(w[t$rows] * P[t$rows, t$state] * t$x) <= floor_rel * t$target
  }, logical(1)))
  if (verbose && length(unfittable) > 0) {
    message("  unfittable targets at exit: ", length(unfittable))
  }
  list(P = P, iters = iter, maxrel = maxrel, unfittable = unfittable)
}

# --- self-test: gradient correctness (finite differences) + both engines ------
# Runs a tiny synthetic problem; not part of the production path.
.state_weights_selftest <- function() {
  set.seed(1)
  N <- 6; S <- 3
  w  <- c(10, 20, 5, 8, 12, 15)
  P0 <- matrix(1/S, N, S)
  # two count targets: state 1 total = 25, state 2 total = 30 (of total 70)
  targets <- list(
    list(rows = 1:N, state = 1L, x = rep(1, N), target = 25, lambda = 1),
    list(rows = 1:N, state = 2L, x = rep(1, N), target = 30, lambda = 1))
  # finite-difference check of the analytic gradient at theta0
  loss_at <- function(theta) {
    z <- theta - apply(theta,1,max); e <- exp(z); P <- e/rowSums(e)
    lf <- 0
    for (t in targets) { That <- sum(w[t$rows]*P[t$rows,t$state]*t$x); lf <- lf + t$lambda*((That-t$target)/t$target)^2 }
    lf
  }
  theta0 <- matrix(0, N, S)
  P <- matrix(1/S, N, S)
  G <- matrix(0, N, S)
  for (t in targets) { That <- sum(w[t$rows]*P[t$rows,t$state]*t$x)
    G[t$rows,t$state] <- G[t$rows,t$state] + t$lambda*2*((That-t$target)/t$target)/t$target*w[t$rows]*t$x }
  gtheta <- P*(G - rowSums(P*G))
  num <- matrix(0, N, S); h <- 1e-6
  for (i in 1:N) for (s in 1:S) { d <- matrix(0,N,S); d[i,s] <- h
    num[i,s] <- (loss_at(theta0+d) - loss_at(theta0-d))/(2*h) }
  max_grad_err <- max(abs(gtheta - num))

  rg <- fit_gradient(w, P0, targets, beta = 0, lr = 0.2, n_steps = 2000)
  rc <- fit_calibration(w, P0, targets, n_iter = 200)
  totals <- function(P) c(sum(w*P[,1]), sum(w*P[,2]), sum(w*P[,3]))
  cat("gradient vs finite-diff max err:", format(max_grad_err, digits=3), "\n")
  cat("row-sum invariant (grad):", all(abs(rowSums(rg$P)-1) < 1e-8),
      " (calib):", all(abs(rowSums(rc$P)-1) < 1e-8), "\n")
  cat("target = [25, 30, 15]\n")
  cat("gradient totals:   ", round(totals(rg$P),2), "\n")
  cat("calibration totals:", round(totals(rc$P),2), "\n")
  invisible(list(max_grad_err = max_grad_err, grad = totals(rg$P), calib = totals(rc$P)))
}
