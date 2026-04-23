#--------------------------------------
# wealth_decomp_grid.R
#
# Grid search over Oaxaca-decomposition
# cell definitions. For each non-empty
# subset of an 8-dim candidate pool (up
# to k=5), compute the decomp of the PUF
# − SCF net-worth gap and record:
#   - #cells defined, #used, #dropped
#   - fraction of total weight in used cells
#   - aggregate X-effect and Y|X-residual
#   - Σ|per-cell Y|X| (localization metric)
#   - unallocated gap (from dropped cells)
#
# Caveat on "Y|X residual": the DRF imputes
# at the level of its full X (>= 5 features).
# At the decomp cell c (coarser than X),
#   μ_PUF,c − μ_SCF,c = Σ_x [P_PUF(x|c) −
#                            P_SCF(x|c)] E[Y|x]_SCF
# is a within-cell X-composition shift, not
# a DRF bias. Adding dims to c shifts this
# residual into X-effect; the grid tells us
# which dim does that most.
#
# Dim pool (8):
#   pct_q        (income quintile, 5 levels)
#   mar_b        (single/married)
#   age_b        (<35 / 35-54 / 55-64 / 65+)
#   dep_b        (0 / 1 / 2 / 3+)
#   has_biz_b    (business > 0)
#   has_cg_b     (capital_gains > 0)
#   has_intdiv_b (int_div > 0)
#   has_sspens_b (ss_pens > 0)
#
# Total combos up to k=5: 218.
#
# Sparsity guard: drop cells with
# min(W_SCF, W_PUF) < 50k weighted units.
# Report the unallocated gap that results.
#
# Inputs (all cached, no pipeline rerun):
#   resources/cache/scf_tax_units.rds
#   resources/cache/wealth_analysis.rds
#   resources/cache/consumption_analysis.rds
#
# Outputs:
#   plots/wealth_decomp/grid_search_report.txt
#   plots/wealth_decomp/grid_search.csv
#   plots/wealth_decomp/grid_topcombo_<n>_cells.csv
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)

report_path = 'plots/wealth_decomp/grid_search_report.txt'
plot_dir    = 'plots/wealth_decomp'
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

MIN_W = 50e3       # weight floor per side for a cell to be "used"
MAX_K = 5L         # max dims per combo
COV_FLOOR = 0.80   # drill-down requires >= this fraction of PUF weight used

#---------------------------------------------------------------------------
# Load caches
#---------------------------------------------------------------------------

scf_path = 'resources/cache/scf_tax_units.rds'
puf_path = 'resources/cache/wealth_analysis.rds'
cons_path = 'resources/cache/consumption_analysis.rds'

for (p in c(scf_path, puf_path, cons_path))
  if (!file.exists(p)) stop('Cache missing: ', p)

scf      = read_rds(scf_path)
puf      = read_rds(puf_path)
puf_cons = read_rds(cons_path)

sink(report_path, split = TRUE)
cat('Oaxaca decomp — grid search over cell definitions\n')
cat('Generated: ', as.character(Sys.time()), '\n', sep = '')
cat(paste0(rep('=', 72), collapse = ''), '\n\n')

#---------------------------------------------------------------------------
# Harmonize both sides with income components + all binning dims.
# SCF has business_scf / int_div_scf / ... directly on scf_tax_units.
# PUF gets them via join from consumption_analysis.rds (filtered to
# income > 0 & C > 0); non-matching rows → NA → dropped in combos that
# condition on any has_* dim.
#---------------------------------------------------------------------------

asset_vars = c('cash', 'equities', 'bonds', 'retirement', 'life_ins',
               'annuities', 'trusts', 'other_fin', 'pass_throughs',
               'primary_home', 'other_home', 're_fund', 'other_nonfin')
debt_vars  = c('primary_mortgage', 'other_mortgage', 'credit_lines',
               'credit_cards', 'installment_debt', 'other_debt')
y_vars = c(asset_vars, debt_vars)

scf_h = scf %>%
  transmute(
    weight, age1, income, married = as.integer(married),
    n_dep = as.integer(n_dep),
    business_comp  = business_scf,
    int_div_comp   = int_div_scf,
    cap_gains_comp = capital_gains_scf,
    ss_pens_comp   = ss_pens_scf,
    !!!setNames(lapply(y_vars, function(v) rlang::sym(v)), y_vars),
    total_assets = !!rlang::parse_expr(paste(asset_vars, collapse = ' + ')),
    total_debts  = !!rlang::parse_expr(paste(debt_vars,  collapse = ' + ')),
    net_worth    = total_assets - total_debts
  )

puf_x = puf_cons %>%
  transmute(
    id,
    business_comp  = sole_prop + farm +
                     scorp_active  - scorp_active_loss  - scorp_179 +
                     scorp_passive - scorp_passive_loss +
                     part_active   - part_active_loss   - part_179 +
                     part_passive  - part_passive_loss,
    int_div_comp   = txbl_int + exempt_int + div_ord + div_pref,
    cap_gains_comp = kg_lt + kg_st,
    ss_pens_comp   = gross_ss + gross_pens_dist
  )

puf_h = puf %>%
  left_join(puf_x, by = 'id') %>%
  transmute(
    weight, age1, income, married = as.integer(married),
    n_dep = as.integer(n_dep),
    business_comp, int_div_comp, cap_gains_comp, ss_pens_comp,
    !!!setNames(lapply(y_vars, function(v) rlang::sym(v)), y_vars),
    total_assets, total_debts, net_worth
  )

#---------------------------------------------------------------------------
# Binning. pct_q uses SCF-pooled breaks so cell labels are comparable? No —
# using each dataset's own breaks preserves the "same-rank" interpretation
# the DRF uses. Non-positive income → pct_q = 0, a separate bin.
#---------------------------------------------------------------------------

bin_dims = function(d) {
  pos = d$income > 0
  brks = Hmisc::wtd.quantile(d$income[pos], d$weight[pos],
                             probs = seq(0, 1, 0.2))
  d$pct_q = findInterval(d$income, brks, all.inside = TRUE)
  d$pct_q[!pos] = 0L
  d$pct_q = as.character(d$pct_q)

  d$age_b = as.character(cut(d$age1, c(-Inf, 35, 55, 65, Inf),
                             labels = c('<35', '35-54', '55-64', '65+'),
                             right = FALSE))
  d$mar_b = ifelse(d$married == 1L, 'mar', 'sin')
  d$dep_b = as.character(factor(pmin(as.integer(d$n_dep), 3L),
                                levels = 0:3,
                                labels = c('d0', 'd1', 'd2', 'd3+')))
  d$has_biz_b    = ifelse(is.na(d$business_comp),  NA_character_,
                    ifelse(d$business_comp  > 0, 'biz+', 'biz0'))
  d$has_cg_b     = ifelse(is.na(d$cap_gains_comp), NA_character_,
                    ifelse(d$cap_gains_comp > 0, 'cg+', 'cg0'))
  d$has_intdiv_b = ifelse(is.na(d$int_div_comp),   NA_character_,
                    ifelse(d$int_div_comp   > 0, 'id+', 'id0'))
  d$has_sspens_b = ifelse(is.na(d$ss_pens_comp),   NA_character_,
                    ifelse(d$ss_pens_comp   > 0, 'ss+', 'ss0'))
  d
}

scf_h = bin_dims(scf_h)
puf_h = bin_dims(puf_h)

# Sanity: report how much of each side gets dropped by NA on the has_* dims.
puf_na_biz = sum(puf_h$weight[is.na(puf_h$has_biz_b)]) / sum(puf_h$weight)
cat(sprintf('PUF weight with NA has_biz_b (non-positive income / nonfiler): %.2f%%\n',
            100 * puf_na_biz))

#---------------------------------------------------------------------------
# Decomposition primitive.
#---------------------------------------------------------------------------

w_sum = function(x, w) sum(x * w, na.rm = FALSE)

oaxaca_decomp = function(scf_df, puf_df, cell_dims,
                         y_var = 'net_worth', min_w = MIN_W) {
  # Baseline aggregates — always computed on the full (unfiltered) data
  # so "observed_gap" means "gap across the full universe". Any units
  # filtered out by NA or sparsity get counted as unallocated.
  agg_scf = w_sum(scf_df[[y_var]], scf_df$weight)
  agg_puf = w_sum(puf_df[[y_var]], puf_df$weight)
  observed_gap_tn = (agg_puf - agg_scf) / 1e12

  # Drop units with NA in any cell dim
  scf_sub = scf_df %>% tidyr::drop_na(all_of(cell_dims))
  puf_sub = puf_df %>% tidyr::drop_na(all_of(cell_dims))

  scf_sub$cell = do.call(paste, c(scf_sub[cell_dims], sep = '|'))
  puf_sub$cell = do.call(paste, c(puf_sub[cell_dims], sep = '|'))

  sc_cell = scf_sub %>% group_by(cell) %>%
    summarise(W_scf = sum(weight),
              mu_scf = sum(.data[[y_var]] * weight) / sum(weight),
              .groups = 'drop')
  pc_cell = puf_sub %>% group_by(cell) %>%
    summarise(W_puf = sum(weight),
              mu_puf = sum(.data[[y_var]] * weight) / sum(weight),
              .groups = 'drop')

  dec = full_join(sc_cell, pc_cell, by = 'cell') %>%
    mutate(across(c(W_scf, W_puf, mu_scf, mu_puf),
                  ~ replace_na(., 0)))

  n_cells_def = nrow(dec)

  dec = dec %>% mutate(used = pmin(W_scf, W_puf) >= min_w)
  dec_used = dec %>% filter(used) %>%
    mutate(x_effect = (W_puf - W_scf) * mu_scf / 1e12,
           yx_resid = W_puf * (mu_puf - mu_scf) / 1e12,
           total    = x_effect + yx_resid)

  n_cells_used    = nrow(dec_used)
  n_cells_dropped = n_cells_def - n_cells_used

  weight_used_pct = sum(dec_used$W_puf) / sum(puf_df$weight)

  x_eff    = sum(dec_used$x_effect)
  yx_resid = sum(dec_used$yx_resid)
  gap_used = x_eff + yx_resid
  sum_abs_yx = sum(abs(dec_used$yx_resid))

  list(
    per_cell = dec_used %>% arrange(desc(abs(total))),
    summary = tibble(
      cell_def        = paste(cell_dims, collapse = '×'),
      k               = length(cell_dims),
      n_cells_def     = n_cells_def,
      n_cells_used    = n_cells_used,
      n_cells_dropped = n_cells_dropped,
      weight_used_pct = weight_used_pct,
      x_eff           = x_eff,
      yx_resid        = yx_resid,
      sum_abs_yx      = sum_abs_yx,
      gap_used        = gap_used,
      observed_gap    = observed_gap_tn,
      unallocated     = observed_gap_tn - gap_used
    )
  )
}

#---------------------------------------------------------------------------
# Build the combo list (k = 1..MAX_K) and run the grid.
#---------------------------------------------------------------------------

dim_pool = c('pct_q', 'mar_b', 'age_b', 'dep_b',
             'has_biz_b', 'has_cg_b', 'has_intdiv_b', 'has_sspens_b')

combos = unlist(
  lapply(seq(1, min(MAX_K, length(dim_pool))), function(k) {
    combn(dim_pool, k, simplify = FALSE)
  }),
  recursive = FALSE
)

cat(sprintf('\nGrid: %d combos over %d dims, k=1..%d\n',
            length(combos), length(dim_pool), MAX_K))
cat(sprintf('Sparsity floor: min(W_SCF, W_PUF) >= %s weighted units per cell\n\n',
            formatC(MIN_W, big.mark = ',', format = 'd')))

t0 = Sys.time()
grid_rows = lapply(combos, function(dims) {
  oaxaca_decomp(scf_h, puf_h, as.character(dims))$summary
})
grid = bind_rows(grid_rows) %>% arrange(sum_abs_yx)
elapsed_min = as.numeric(Sys.time() - t0, units = 'mins')
cat(sprintf('Grid compute time: %.1f min\n\n', elapsed_min))

write_csv(grid, file.path(plot_dir, 'grid_search.csv'))

#---------------------------------------------------------------------------
# Headline: top 25 combos (sorted by Σ|Y|X residual| ascending) +
# per-k best.
#---------------------------------------------------------------------------

cat('Top 25 combos — sorted by Σ|Y|X residual| ascending (lower = more localized)\n')
cat(paste0(rep('-', 120), collapse = ''), '\n')
cat(sprintf('  %-58s  %2s %6s %6s %7s %7s %7s %8s %8s\n',
            'cell_def', 'k', '#defd', '#used', '%wt',
            'X-eff', 'Y|X', 'Σ|Y|X|', 'unalloc'))
cat(paste0(rep('-', 120), collapse = ''), '\n')
for (i in seq_len(min(25, nrow(grid)))) {
  cat(sprintf('  %-58s  %2d %6d %6d  %5.1f%%  %+6.2f  %+6.2f  %8.2f  %+6.2f\n',
              grid$cell_def[i], grid$k[i],
              grid$n_cells_def[i], grid$n_cells_used[i],
              100 * grid$weight_used_pct[i],
              grid$x_eff[i], grid$yx_resid[i],
              grid$sum_abs_yx[i], grid$unallocated[i]))
}

cat('\n\nBest combo per k (by Σ|Y|X residual|):\n')
cat(paste0(rep('-', 120), collapse = ''), '\n')
best_per_k = grid %>% group_by(k) %>% slice_min(sum_abs_yx, n = 1) %>% ungroup() %>%
  arrange(k)
for (i in seq_len(nrow(best_per_k))) {
  cat(sprintf('  k=%d %-56s  %+6.2f X, %+6.2f Y|X, Σ|Y|X|=%6.2f, %d used, %5.1f%% wt\n',
              best_per_k$k[i], best_per_k$cell_def[i],
              best_per_k$x_eff[i], best_per_k$yx_resid[i],
              best_per_k$sum_abs_yx[i],
              best_per_k$n_cells_used[i],
              100 * best_per_k$weight_used_pct[i]))
}

#---------------------------------------------------------------------------
# Dim ranking: for each dim, what's the mean Σ|Y|X| across combos that
# include it vs combos that don't? Negative delta = including the dim
# reduces residual (useful).
#---------------------------------------------------------------------------

cat('\n\nDim ranking: average Σ|Y|X residual| when dim is IN vs OUT of combo\n')
cat(paste0(rep('-', 80), collapse = ''), '\n')
cat(sprintf('  %-18s  %10s  %10s  %10s  %s\n',
            'dim', 'mean IN', 'mean OUT', 'Δ', 'interpretation'))
cat(paste0(rep('-', 80), collapse = ''), '\n')

dim_ranks = map_dfr(dim_pool, function(d) {
  grep_pat = paste0('(^|×)', gsub('\\.', '\\\\.', d), '($|×)')
  in_d  = grid %>% filter(grepl(grep_pat, cell_def))
  out_d = grid %>% filter(!grepl(grep_pat, cell_def))
  tibble(dim = d,
         mean_in  = mean(in_d$sum_abs_yx),
         mean_out = mean(out_d$sum_abs_yx),
         delta    = mean(in_d$sum_abs_yx) - mean(out_d$sum_abs_yx))
}) %>% arrange(delta)

for (i in seq_len(nrow(dim_ranks))) {
  interp = if (dim_ranks$delta[i] < -0.5) 'strong absorber' else
           if (dim_ranks$delta[i] < -0.1) 'moderate absorber' else
           if (dim_ranks$delta[i] <  0.1) 'neutral' else
                                          'sparsity penalty'
  cat(sprintf('  %-18s  %10.2f  %10.2f  %+10.2f  %s\n',
              dim_ranks$dim[i], dim_ranks$mean_in[i],
              dim_ranks$mean_out[i], dim_ranks$delta[i], interp))
}

write_csv(dim_ranks, file.path(plot_dir, 'grid_dim_ranking.csv'))

#---------------------------------------------------------------------------
# Drill-down on top 3 combos that also meet the weight-coverage floor.
#---------------------------------------------------------------------------

cat(sprintf('\n\nDrill-down: top combos with >= %.0f%% weight coverage\n',
            100 * COV_FLOOR))
cat('(top 15 cells per combo by |total| contribution)\n')
cat(paste0(rep('=', 120), collapse = ''), '\n')

top_combos = grid %>% filter(weight_used_pct >= COV_FLOOR) %>%
  slice_head(n = 3)

for (i in seq_len(nrow(top_combos))) {
  cd = strsplit(top_combos$cell_def[i], '×')[[1]]
  r = oaxaca_decomp(scf_h, puf_h, cd)
  cat(sprintf('\n--- Drill-down #%d: %s ---\n',
              i, top_combos$cell_def[i]))
  cat(sprintf('  k=%d, %d/%d cells used, %.1f%% PUF weight, ',
              top_combos$k[i],
              top_combos$n_cells_used[i], top_combos$n_cells_def[i],
              100 * top_combos$weight_used_pct[i]))
  cat(sprintf('X=%+6.2fT Y|X=%+6.2fT Σ|Y|X|=%.2fT unalloc=%+.2fT\n\n',
              top_combos$x_eff[i], top_combos$yx_resid[i],
              top_combos$sum_abs_yx[i], top_combos$unallocated[i]))

  top_cells = r$per_cell %>% slice_head(n = 15)
  cat(sprintf('  %-48s | %8s %8s %10s %10s | %8s %8s %8s\n',
              'cell', 'W_SCF(M)', 'W_PUF(M)',
              'μ_SCF($K)', 'μ_PUF($K)',
              'X-eff$T', 'Y|X$T', 'tot$T'))
  cat('  ', strrep('-', 108), '\n', sep = '')
  for (j in seq_len(nrow(top_cells))) {
    cat(sprintf('  %-48s | %8.2f %8.2f %10.0f %10.0f | %+8.2f %+8.2f %+8.2f\n',
                top_cells$cell[j],
                top_cells$W_scf[j] / 1e6, top_cells$W_puf[j] / 1e6,
                top_cells$mu_scf[j] / 1e3, top_cells$mu_puf[j] / 1e3,
                top_cells$x_effect[j], top_cells$yx_resid[j],
                top_cells$total[j]))
  }

  write_csv(r$per_cell,
            file.path(plot_dir, sprintf('grid_topcombo_%d_cells.csv', i)))
}

#---------------------------------------------------------------------------
# Summary artifacts.
#---------------------------------------------------------------------------

cat('\n\nArtifacts saved to ', plot_dir, '/\n', sep = '')
cat('  grid_search.csv             — all ', length(combos),
    ' combos, summary row each\n', sep = '')
cat('  grid_dim_ranking.csv        — per-dim absorption ranking\n')
cat('  grid_topcombo_{1,2,3}_cells.csv — drill-down cells for top 3 combos\n')
cat('  grid_search_report.txt      — this report\n')

sink()
