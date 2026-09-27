#------------------------------------------------------------------------------
# refit_common.R -- shared by 03_refit_fit.R and 04_refit_score.R
#
# The per-year national filer refit bake-off (S40;
# research/state_weights/notes/filer_refit_design.md): record loading with the
# AGI proxy, the target design matrix, and the priors.
#
# Inputs are a published Tax-Data vintage (VINTAGE below) and, for TY2017's
# prior, the 2017 LP's cached multipliers: resources/cache/lp/weight_deltas.rds
# holds one per PUF record in RECID order, and dividing them out of the 2017
# weights leaves S006/100 times one of 76 create_2017_puf.R cell factors
# (checked 2026-09-27), i.e. the weights before the LP.
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table); library(dplyr); library(Matrix); library(yaml) })
source('src/nonfilers/state_weights.R')
source('src/agi_proxy.R')

VINTAGE  <- '2026092220_s33_scratch'   # the vintage the AGI proxy was scored on
TARGETS  <- 'resources/filer_refit_targets.csv'
LP_CACHE <- 'resources/cache/lp/weight_deltas.rds'
FORBES_ID_MIN <- 9e6                   # S25: Forbes synthetic ids; fixed, never refit

# Fit settings shared by every arm
TOL        <- 0.005    # target band: +-0.5% ...
TOL_FLOOR  <- 500      # ... but never tighter than 500 returns
RATIO_MIN  <- 0.2      # bounds on a record's weight relative to its prior
RATIO_MAX  <- 5

vintage_dir <- function() file.path(model_data_root(), 'Tax-Data/v1', VINTAGE, 'baseline')
fit_dir <- function() file.path(scratch_root(), 'filer_refit')

PROXY_COLS <- c('id', 'weight', 'filer', 'filing_status', 'dep_status', 'age1',
                'wages', 'wages1', 'wages2', 'txbl_int', 'exempt_int', 'div_ord', 'div_pref',
                'state_ref', 'alimony', 'divorce_year', 'sole_prop', 'sole_prop1', 'sole_prop2',
                'farm', 'farm1', 'farm2', 'part_se1', 'part_se2', 'kg_st', 'kg_lt', 'other_gains',
                'txbl_ira_dist', 'gross_pens_dist', 'txbl_pens_dist', 'rent', 'rent_loss',
                'estate', 'estate_loss', 'part_active', 'part_active_loss', 'part_passive',
                'part_passive_loss', 'part_179', 'scorp_active', 'scorp_active_loss',
                'scorp_passive', 'scorp_passive_loss', 'scorp_179', 'ui', 'gross_ss', 'other_inc',
                'ed_exp', 'hsa_contr', 'trad_contr_ira', 'sl_int_ded', 'keogh_contr', 'se_health',
                'early_penalty', 'alimony_exp', 'tuition_ded', 'dpad', 'other_above_ded', 'char_cash')

# Filers of one year with positive weight, the AGI proxy, and the vintage's
# weight as `w_current`. Forbes records are kept (they count toward targets)
# and flagged `fixed`.
load_filers <- function(year) {
  d <- fread(file.path(vintage_dir(), sprintf('tax_units_%d.csv', year)), select = PROXY_COLS)
  d <- d[filer == 1 & weight > 0]
  d <- as.data.table(agi_proxy(as_tibble(d), year))
  d[, `:=`(w_current = weight, fixed = id >= FORBES_ID_MIN)]
  setorder(d, id)
  d[]
}

# The 2017 prior: the vintage's 2017 weights with the LP multipliers divided out
prior_2017 <- function(d17) {
  delta <- readRDS(LP_CACHE)
  w <- as.data.table(readRDS(file.path(vintage_dir(), 'weight_ledger.rds')))[year == 2017L & id < 1e6]
  setorder(w, id)
  stopifnot(length(delta) == nrow(w), identical(as.numeric(w$id), as.numeric(d17[fixed == FALSE, id])))
  w$weight / delta
}

# Each record's status x age factor from 2017 to `year`, as the vintage's
# weight ledger applies it (compute_weights_for_year() in src/project_puf.R)
status_age_factor <- function(ids, yr) {
  wl <- as.data.table(readRDS(file.path(vintage_dir(), 'weight_ledger.rds')))[year %in% c(2017L, yr)]
  f <- dcast(wl[id %in% ids], id ~ year, value.var = 'weight')
  setnames(f, c('id', 'w17', 'wy'))
  f[match(ids, f$id), wy / w17]
}

# Sparse records x targets indicator matrix for one year's targets. A target is
# the union of its member cells; NA in a member dimension means "all".
target_matrix <- function(d, tg) {
  cls_ok <- function(lo, hi) d$agi_proxy >= lo & d$agi_proxy < hi
  cols <- lapply(split(tg, tg$target_id), function(m) {
    hit <- rep(FALSE, nrow(d))
    for (k in seq_len(nrow(m))) {
      r <- m[k]
      h <- cls_ok(r$agi_lo, r$agi_hi)
      if (!is.na(r$filing_status)) h <- h & d$filing_status == r$filing_status
      if (!is.na(r$age_lo))        h <- h & d$age1 >= r$age_lo & d$age1 < r$age_hi
      if (!is.na(r$dependent))     h <- h & d$dep_status == r$dependent
      if (!is.na(r$wages))         h <- h & (d$wages > 0) == (r$wages == 1)
      hit <- hit | h
    }
    which(hit)
  })
  X <- sparseMatrix(i = unlist(cols), j = rep(seq_along(cols), lengths(cols)), x = 1,
                    dims = c(nrow(d), length(cols)))
  list(X = X, target_id = as.integer(names(cols)),
       target = tg[, .(t = target_value[1], source = source[1]), by = target_id][
         match(as.integer(names(cols)), target_id)])
}

# Target bands in returns
band <- function(t) pmax(TOL * t, TOL_FLOOR)
