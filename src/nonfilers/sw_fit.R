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
source('src/agi_proxy.R', local = TRUE)   # txbl_kg(), the capital-loss limit

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
#   - capital gains in AGI proxied by kg_st + kg_lt with the loss limit
#     ($3,000, $1,500 MFS: txbl_kg() in src/agi_proxy.R)
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
  kg_agi <- function() txbl_kg(tu$kg_st, tu$kg_lt, tu$filing_status)
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

# The Tax-Data columns build_weight_inputs() reads (plus `agi` and `eitc`
# joined from the Tax-Simulator baseline). One list, used by the producer
# (src/main_state_weights.R) and the research prep script alike.
WEIGHT_INPUT_COLS <- c('id', 'weight', 'filer', 'filing_status', 'n_dep', 'age1',
                       'wages', 'txbl_int', 'exempt_int', 'div_ord', 'div_pref',
                       'kg_st', 'kg_lt', 'salt_inc_sales', 'salt_prop',
                       'first_mort_int', 'second_mort_int', 'txbl_pens_dist',
                       'txbl_ira_dist', 'gross_ss', 'sole_prop', 'dep_status')

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

# =============================================================================
# STATE-PERCENTILE TARGETS (the Phase 3 re-run's percentile arm; plan.md §5a,
# Phase 3 addendum of 2026-09-29)
#
# HT2's stubs are national AGI classes, so they leave free WHICH state gets the
# richest records inside a stub: the S39 fits put FL and TX top-1% AGI at 0.81
# and 0.82 of the IRS figure and NY and CA at 1.10 and 1.15
# (research/state_weights/scripts/results/phase3_percentile_heldout*.csv).
# SOI's AGI percentiles by state publish each state's own top-1% and top-5%
# cutoff, return count and AGI. A record counts toward (state s, group g) when
# it is in the source's universe -- a filer, not a dependent, AGI > 0 -- and
# its AGI is at or above s's cutoff, which is linear in the split, so each
# (s, g, series) is an ordinary engine target.
#
# Level. Tax-Data's national top tail runs below the IRS's (top-1% AGI 0.98 of
# IRS, 0.89 in TY2021) and no split moves a national total, so the IRS state
# figures set the ALLOCATION at the file's own national level -- as the HT2
# targets distribute PUF totals by HT2 shares. Counts scale by the file's
# universe count over the IRS's; AGI, per group, by the file's AGI above the
# NATIONAL cutoff over the IRS's national group AGI.
# =============================================================================
PCT_TARGET_GROUPS <- c(1, 5)   # top 1% and top 5%; items wait for the top-tail base rebuild

#' Engine targets for each state's top 1% and 5%: return count and AGI.
#' @param tu_f the filer partition in partition order, with weight, agi
#'        (baseline-calculated) and dep_status
#' @param irs read_state_percentile(keep_us = TRUE); read from the store if NULL
#' @return list of targets (one engine group, cell "state_percentile"; `measure`
#'         pct_returns / pct_agi), with attribute `scale` holding the level factors
state_percentile_targets <- function(tu_f, year, jurisdictions, irs = NULL) {
  if (is.null(irs)) irs <- read_state_percentile(state_percentile_path(year), year, keep_us = TRUE)
  stopifnot(all(c('weight', 'agi', 'dep_status') %in% names(tu_f)),
            !anyNA(tu_f$agi), !anyNA(tu_f$dep_status))
  w   <- tu_f$weight
  agi <- tu_f$agi
  in_universe <- tu_f$dep_status == 0 & agi > 0
  us  <- irs[state == 'US']
  st  <- irs[state != 'US' & top_pct %in% PCT_TARGET_GROUPS & series %in% c('returns', 'agi')]
  stopifnot(nrow(us) > 0, all(unique(st$state) %in% jurisdictions))

  agi_scale <- vapply(PCT_TARGET_GROUPS, function(g) {
    cut_us <- us[top_pct == g & series == 'agi', cutoff_agi]
    sum((w * agi)[in_universe & agi >= cut_us]) / us[top_pct == g & series == 'agi', value]
  }, numeric(1))
  scale <- data.table(top_pct = PCT_TARGET_GROUPS,
                      returns = sum(w[in_universe]) / us[top_pct == 100 & series == 'returns', value],
                      agi = agi_scale)

  # ONE engine group for all of them (gradient_groups() keys on `cell`): rows
  # are every universe record at or above the lowest cutoff, and each target's
  # x is zero below its own state's cutoff. A group per state x top group
  # would multiply the rows by ~100 and price every one against all 53 state
  # columns; this way a step costs two products of an n x 204 matrix
  rows <- which(in_universe & agi >= min(st$cutoff_agi))
  stopifnot(length(rows) > 0)
  targets <- list()
  for (g in PCT_TARGET_GROUPS) {
    for (s in sort(unique(st$state))) {
      cell <- st[state == s & top_pct == g]
      above <- as.numeric(agi[rows] >= cell$cutoff_agi[1])
      stopifnot(sum(above) > 0)
      for (ser in c('returns', 'agi')) {
        tgt <- cell[series == ser, value] * scale[top_pct == g][[ser]]
        stopifnot(length(tgt) == 1, is.finite(tgt), tgt > 0)
        targets[[length(targets) + 1]] <- list(
          rows = rows, state = match(s, jurisdictions),
          x = if (ser == 'returns') above else above * agi[rows],
          target = tgt, lambda = 1,
          # metadata: the engine gives each distinct `series` its own column
          series = sprintf('pct_%s_%s_%g', ser, s, g), cell = 'state_percentile',
          measure = paste0('pct_', ser), state_code = s, top_pct = g)
      }
    }
  }
  attr(targets, 'scale') <- scale
  targets
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
#' @param theta0 optional warm-start logits (Phase 3 arm B); the KL anchor is
#'        the counts prior regardless
fit_filers_config7 <- function(p, verbose = FALSE, theta0 = NULL) {
  prior <- config7_prior(p)
  fit_gradient(p$w, prior, p$targets,
               beta = CONFIG_7$beta, lr = CONFIG_7$lr, n_steps = CONFIG_7$n_steps,
               lr_schedule = CONFIG_7$lr_schedule, verbose = verbose, theta0 = theta0)
}

#' Config 7's first stage: IPF on the return counts from the HT2-share prior.
#' Both the gradient fit's starting point and its KL anchor.
config7_prior <- function(p) {
  counts <- Filter(function(t) t$series == CONFIG_7$prior_series, p$targets)
  fit_calibration(p$w, p$P0, counts, n_iter = CONFIG_7$prior_n_iter)$P
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

    out[[part]] <- emit_split(fit$P, p$w, tu$id[p$idx], inputs$jurisdictions)
  }
  rbindlist(out)
}

#' The split invariant on a share matrix: every cell non-negative, every row a
#' distribution. A violation is a defect (the 2026-07-13 leak came from
#' negative P cells being dropped by the weight filter; negative targets are
#' now blocked at assembly).
assert_split_shares <- function(P) {
  stopifnot(all(P >= 0), max(abs(rowSums(P) - 1)) < 1e-8)
  invisible(P)
}

#' Long split weights from a share matrix: data.table(id, state, weight) with
#' the zero cells dropped. The invariant is checked on the EMITTED weights --
#' Σ_state weight equals the record's national weight to 1e-8 relative -- not
#' only on P, so what a consumer reads is what was asserted (Phase 4).
emit_split <- function(P, w, ids, jurisdictions) {
  assert_split_shares(P)
  stopifnot(length(w) == nrow(P), length(ids) == nrow(P), !anyDuplicated(ids),
            length(jurisdictions) == ncol(P))
  W <- w * P
  stopifnot(max(abs(rowSums(W) - w) / pmax(abs(w), 1e-12)) < 1e-8)
  dt <- data.table(id = rep(ids, ncol(W)),
                   state = rep(jurisdictions, each = nrow(W)),
                   weight = as.vector(W))
  dt[weight > 0]
}

# =============================================================================
# THE PER-YEAR FILER CHAIN (Phase 3, S39; moved from
# research/state_weights/scripts/phase3_fit.R on 2026-09-29, Phase 4 P3)
#
# Filers persist across years under the same ids (plus Forbes records, new
# each year from 2022), so the per-year fits are chained. Each `p` below is
# the `filers` element of build_weight_inputs() with two fields attached by
# the caller: `ids` (the record id of each row, in partition order) and
# `jurisdictions` (the year's state columns, from HT2). A fit is
# list(P, theta, ids, jurisdictions).
# =============================================================================

#' Arm B's warm start: each carried record starts at last year's split, by
#' jurisdiction name. Jurisdictions can grow (PR enters HT2 in 2018): a column
#' new to the year gets the record's prior mass there, and the carried split
#' is scaled to make room. A record new to the year starts at its prior.
#' @return list(theta0, n_warm, n_cold)
warm_start_theta <- function(p, prev) {
  prior <- config7_prior(p)
  theta0 <- log(pmax(prior, 1e-12))
  rows <- match(p$ids, prev$ids)
  keep <- which(!is.na(rows))
  cols <- match(prev$jurisdictions, p$jurisdictions)
  stopifnot(!anyNA(cols))
  new_cols <- setdiff(seq_along(p$jurisdictions), cols)
  q <- if (length(new_cols)) rowSums(prior[keep, new_cols, drop = FALSE]) else 0
  theta0[keep, cols] <- log(pmax(prev$P[rows[keep], ] * (1 - q), 1e-12))
  list(theta0 = theta0, n_warm = length(keep), n_cold = length(p$ids) - length(keep))
}

#' Arm B: config 7 cold on the first year, then each year warm-started from
#' the previous year's fit (warm_start_theta()). The objective is unchanged, so
#' B differs from A only by how unconverged 3,000 steps leave either.
#' @param ps filer partitions in year order
#' @return list of fits, one per year
fit_filers_chain_B <- function(ps, verbose = FALSE) {
  fits <- vector('list', length(ps))
  f <- fit_filers_config7(ps[[1]], verbose = verbose)
  fits[[1]] <- list(P = f$P, theta = f$theta, ids = ps[[1]]$ids, jurisdictions = ps[[1]]$jurisdictions)
  for (k in seq_along(ps)[-1]) {
    p <- ps[[k]]
    ws <- warm_start_theta(p, fits[[k - 1]])
    if (verbose) message(sprintf('  %d records warm-started, %d cold (new ids)', ws$n_warm, ws$n_cold))
    f <- fit_filers_config7(p, verbose = verbose, theta0 = ws$theta0)
    fits[[k]] <- list(P = f$P, theta = f$theta, ids = p$ids, jurisdictions = p$jurisdictions)
  }
  for (x in fits) assert_split_shares(x$P)
  fits
}

#' Arm C: one joint fit over consecutive years with gamma * KL(P_y || P_{y-1})
#' on every persistent record (fit_gradient_panel()), gamma = gamma_mult x
#' config 7's beta. Every year must carry the same jurisdictions, which is why
#' the chain starts in 2018 (PR splits out of OA that year). `warm`, if given,
#' is arm B's fits for the same years: each year starts from B's logits
#' (C_{mult}_warmB, S39's pick).
#' @return list(fits = one per year, loss_hist)
fit_filers_chain_C <- function(ps, gamma_mult, warm = NULL, verbose = FALSE) {
  stopifnot(is.finite(gamma_mult), gamma_mult > 0)
  J <- ps[[1]]$jurisdictions
  stopifnot(all(vapply(ps, function(p) identical(p$jurisdictions, J), logical(1))))
  years <- lapply(seq_along(ps), function(k) {
    p <- ps[[k]]
    y <- list(w = p$w, P0 = config7_prior(p), targets = p$targets, ids = p$ids)
    if (!is.null(warm)) {
      b <- warm[[k]]
      stopifnot(identical(b$ids, p$ids), identical(b$jurisdictions, J))
      y$theta0 <- b$theta
    }
    y
  })
  gamma <- gamma_mult * CONFIG_7$beta
  if (verbose) message(sprintf('arm C: gamma = %g (%g x beta), %d years', gamma, gamma_mult, length(ps)))
  f <- fit_gradient_panel(years, gamma = gamma, beta = CONFIG_7$beta, lr = CONFIG_7$lr,
                          n_steps = CONFIG_7$n_steps, lr_schedule = CONFIG_7$lr_schedule,
                          verbose = verbose)
  fits <- lapply(seq_along(ps), function(k)
    list(P = f$P[[k]], theta = f$theta[[k]], ids = ps[[k]]$ids, jurisdictions = J))
  for (x in fits) assert_split_shares(x$P)
  list(fits = fits, loss_hist = f$loss_hist)
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

  groups <- gradient_groups(w, targets, S)

  theta <- if (is.null(theta0)) log(pmax(P0, 1e-12)) else theta0  # warm start at the prior
  m <- matrix(0, N, S); v <- matrix(0, N, S)             # Adam moments
  b1 <- 0.9; b2 <- 0.999; eps <- 1e-8
  softmax_rows <- function(z) { z <- z - apply(z, 1, max); e <- exp(z); e / rowSums(e) }

  loss_hist <- numeric(n_steps)
  for (step in seq_len(n_steps)) {
    P <- softmax_rows(theta)
    ft <- gradient_fit_term(groups, P, N, S)             # dLoss_fit / dP
    G <- ft$G; loss_fit <- ft$loss_fit
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

# --- Phase 3 arm C: the multi-year ("amortized") gradient fit ---------------
# One joint objective over consecutive years y = 1..Y, each with its own
# records, weights, targets and KL anchor, plus a temporal penalty on every
# record present in consecutive years:
#
#   Σ_y [ fit_y(P_y) + beta · KL(P_y || P0_y) ]
#     + gamma · Σ_{y>1} Σ_{i in y-1 and y} KL(P_y[i,] || P_{y-1}[i,])
#
# Records are matched across years by `ids`. gamma = 0 is Y independent
# fit_gradient() runs, step for step (asserted by the tests); gamma -> Inf
# holds every persistent record's state split fixed over the panel. Every term
# is convex in P, so the joint problem keeps a unique optimum.
#' @param years list of list(w, P0, targets, ids[, theta0]) in year order
fit_gradient_panel <- function(years, gamma, beta = 1e-3, lr = 0.1, n_steps = 500,
                               lr_schedule = c("constant", "cosine"), verbose = FALSE) {
  lr_schedule <- match.arg(lr_schedule)
  Y <- length(years)
  S <- ncol(years[[1]]$P0)
  stopifnot(all(vapply(years, function(y) ncol(y$P0) == S, logical(1))),
            all(vapply(years, function(y) !anyDuplicated(y$ids), logical(1))))
  N <- vapply(years, function(y) length(y$w), integer(1))
  groups <- lapply(years, function(y) gradient_groups(y$w, y$targets, S))
  # link[[y]]: rows of year y-1 (prev) and year y (cur) holding the same record
  link <- lapply(seq_len(Y), function(y) {
    if (y == 1) return(NULL)
    cur <- which(years[[y]]$ids %in% years[[y - 1]]$ids)
    list(cur = cur, prev = match(years[[y]]$ids[cur], years[[y - 1]]$ids))
  })

  theta <- lapply(years, function(y) if (is.null(y$theta0)) log(pmax(y$P0, 1e-12)) else y$theta0)
  m <- lapply(N, function(n) matrix(0, n, S)); v <- m
  b1 <- 0.9; b2 <- 0.999; eps <- 1e-8
  softmax_rows <- function(z) { z <- z - apply(z, 1, max); e <- exp(z); e / rowSums(e) }

  loss_hist <- matrix(0, n_steps, 3, dimnames = list(NULL, c("fit", "kl", "temporal")))
  for (step in seq_len(n_steps)) {
    P <- lapply(theta, softmax_rows)
    g <- vector("list", Y)
    loss <- c(fit = 0, kl = 0, temporal = 0)
    for (y in seq_len(Y)) {
      ft <- gradient_fit_term(groups[[y]], P[[y]], N[[y]], S)
      Greg <- beta * (log(pmax(P[[y]], 1e-12)) - log(pmax(years[[y]]$P0, 1e-12)) + 1)
      g[[y]] <- ft$G + Greg
      loss[["fit"]] <- loss[["fit"]] + ft$loss_fit
      loss[["kl"]] <- loss[["kl"]] +
        beta * sum(P[[y]] * (log(pmax(P[[y]], 1e-12)) - log(pmax(years[[y]]$P0, 1e-12))))
    }
    if (gamma > 0) for (y in seq_len(Y)[-1]) {
      cur <- link[[y]]$cur; prev <- link[[y]]$prev
      Pc <- P[[y]][cur, , drop = FALSE]; Pp <- pmax(P[[y - 1]][prev, , drop = FALSE], 1e-12)
      lr_ <- log(pmax(Pc, 1e-12)) - log(Pp)
      g[[y]][cur, ] <- g[[y]][cur, ] + gamma * (lr_ + 1)
      g[[y - 1]][prev, ] <- g[[y - 1]][prev, ] - gamma * Pc / Pp
      loss[["temporal"]] <- loss[["temporal"]] + gamma * sum(Pc * lr_)
    }
    lr_t <- if (lr_schedule == "cosine") lr * 0.5 * (1 + cos(pi * step / n_steps)) else lr
    for (y in seq_len(Y)) {
      gtheta <- P[[y]] * (g[[y]] - rowSums(P[[y]] * g[[y]]))
      m[[y]] <- b1 * m[[y]] + (1 - b1) * gtheta
      v[[y]] <- b2 * v[[y]] + (1 - b2) * gtheta^2
      mhat <- m[[y]] / (1 - b1^step); vhat <- v[[y]] / (1 - b2^step)
      theta[[y]] <- theta[[y]] - lr_t * mhat / (sqrt(vhat) + eps)
    }
    loss_hist[step, ] <- loss
    if (verbose && step %% max(1, n_steps %/% 10) == 0)
      message(sprintf("  step %d  fit=%.4e  kl=%.4e  temporal=%.4e",
                      step, loss[["fit"]], loss[["kl"]], loss[["temporal"]]))
  }
  list(P = lapply(theta, softmax_rows), theta = theta, loss_hist = loss_hist)
}

# The gradient engines' shared pieces. Targets sharing a rows set (every series
# within a stub, by assembly construction) are stacked into one group so each
# step is a few GEMMs, not one scatter per target.
gradient_groups <- function(w, targets, S) {
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
  groups
}

#' The fit loss and its gradient in P. na.rm is the universe, not a fallback:
#' resid is NA exactly in the (series, state) slots a group does not target.
gradient_fit_term <- function(groups, P, N, S) {
  G <- matrix(0, N, S)
  loss_fit <- 0
  for (g in groups) {
    That  <- crossprod(g$X, P[g$rows, , drop = FALSE]) # K × S predicted totals
    resid <- (That - g$Tm) / g$Tm                      # NA where untargeted
    loss_fit <- loss_fit + sum(g$Lm * resid^2, na.rm = TRUE)
    C <- ifelse(g$has, 2 * g$Lm * resid / g$Tm, 0)     # K × S coefficient
    G[g$rows, ] <- G[g$rows, ] + g$X %*% C
  }
  list(G = G, loss_fit = loss_fit)
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
