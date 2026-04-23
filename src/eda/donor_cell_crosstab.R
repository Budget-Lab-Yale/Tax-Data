#--------------------------------------
# donor_cell_crosstab.R
#
# Test the "leaf leakage" hypothesis by
# identifying which SCF row each PUF row
# drew as its donor, then cross-tabbing
# (PUF decomp cell × SCF donor cell).
#
# If donors mostly come from the same
# demographic cell as the PUF unit → no
# leakage → need another explanation for
# the P99 overshoot.
#
# If donors commonly come from cells
# richer than the PUF unit's cell (e.g.,
# 5|mar|65+|biz+ donors landing on PUF
# units in 4|mar|65+|biz0) → leakage is
# real.
#
# Mechanism: the ablation cache saves
#   donor_y = Y_mat[donors, ]  (matrix
#     of 23 wealth values per PUF unit)
# and boot_idx (250k bootstrap indices
# into scf_tax_units). Since continuous
# wealth tuples are unique per SCF row,
# we can recover the donor SCF row by
# matching donor_y's 23-tuple to each
# SCF row's 23-tuple.
#
# Inputs:
#   resources/cache/scf_tax_units.rds
#   resources/cache/wealth_analysis.rds
#   resources/cache/wealth_ablation_large.rds
#   resources/cache/consumption_analysis.rds
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)

report_path = 'plots/wealth_decomp/donor_crosstab_report.txt'
plot_dir    = 'plots/wealth_decomp'

scf      = read_rds('resources/cache/scf_tax_units.rds')
puf_dem  = read_rds('resources/cache/wealth_analysis.rds')
abl      = read_rds('resources/cache/wealth_ablation_large.rds')
puf_cons = read_rds('resources/cache/consumption_analysis.rds')

sink(report_path, split = TRUE)
cat('Donor cell cross-tabulation — leaf leakage test\n')
cat('Generated: ', as.character(Sys.time()), '\n', sep = '')
cat(paste0(rep('=', 72), collapse = ''), '\n\n')

#---------------------------------------------------------------------------
# Harmonize SCF with wealth Y vars + demographics needed for cell def.
#---------------------------------------------------------------------------

asset_vars = c('cash', 'equities', 'bonds', 'retirement', 'life_ins',
               'annuities', 'trusts', 'other_fin', 'pass_throughs',
               'primary_home', 'other_home', 're_fund', 'other_nonfin')
debt_vars  = c('primary_mortgage', 'other_mortgage', 'credit_lines',
               'credit_cards', 'installment_debt', 'other_debt')
kg_vars    = c('kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other')
y_vars     = c(asset_vars, debt_vars, kg_vars)

# scf_tax_units already has the canonical wealth schema (see
# stage1_scf_tax_units.R:617-640). No re-computation needed.
scf_y = scf
stopifnot(all(y_vars %in% names(scf_y)))

#---------------------------------------------------------------------------
# Build matching keys from 23 wealth values.
# Use high-precision format so collisions are negligible for continuous wealth.
#---------------------------------------------------------------------------

make_key = function(df, cols) {
  do.call(paste, c(lapply(cols, function(v) sprintf('%.12g', df[[v]])),
                   sep = ':'))
}

scf_keys = make_key(scf_y, y_vars)
puf_donor_df = as_tibble(abl$donor_y)
puf_keys = make_key(puf_donor_df, y_vars)

# Check uniqueness of SCF keys
dup_rate_scf = mean(duplicated(scf_keys))
cat(sprintf('SCF key duplicate rate: %.4f%% (%d of %d rows)\n',
            100 * dup_rate_scf, sum(duplicated(scf_keys)), length(scf_keys)))

# Build SCF row index from key. On duplicates, keep the first.
scf_lookup = tibble(key = scf_keys, scf_row = seq_len(length(scf_keys))) %>%
  group_by(key) %>% slice(1) %>% ungroup()

# Match PUF donors to SCF rows
match_df = tibble(key = puf_keys, puf_id = abl$id) %>%
  left_join(scf_lookup, by = 'key')

n_unmatched = sum(is.na(match_df$scf_row))
cat(sprintf('PUF rows unmatched to SCF: %d of %d\n',
            n_unmatched, nrow(match_df)))
if (n_unmatched > 0)
  stop('Unmatched PUF donors — key precision too low or schema mismatch')

#---------------------------------------------------------------------------
# Build cell definitions on each side using the same binning.
# Use income quintile × married × age × has_biz (4 dims).
#---------------------------------------------------------------------------

bin_dims_scf = function(d) {
  pos = d$income > 0
  brks = Hmisc::wtd.quantile(d$income[pos], d$weight[pos], probs = seq(0, 1, 0.2))
  d$pct_q = findInterval(d$income, brks, all.inside = TRUE)
  d$pct_q[!pos] = 0L
  d$age_b = cut(d$age1, c(-Inf, 35, 55, 65, Inf),
                labels = c('<35', '35-54', '55-64', '65+'), right = FALSE)
  d$mar_b = ifelse(d$married == 1L, 'mar', 'sin')
  d$has_biz_b = ifelse(d$business_scf > 0, 'biz+', 'biz0')
  d$cell = paste(d$pct_q, d$mar_b, d$age_b, d$has_biz_b, sep = '|')
  d
}

scf_cells = bin_dims_scf(scf)

# PUF cell: from wealth_analysis.rds demographics + income components
puf_bix = puf_cons %>%
  transmute(
    id,
    business_p = sole_prop + farm +
                 scorp_active  - scorp_active_loss  - scorp_179 +
                 scorp_passive - scorp_passive_loss +
                 part_active   - part_active_loss   - part_179 +
                 part_passive  - part_passive_loss
  )

puf_cells = puf_dem %>%
  select(id, weight, age1, married, n_dep, income) %>%
  left_join(puf_bix, by = 'id')

pos_p = puf_cells$income > 0
brks_p = Hmisc::wtd.quantile(puf_cells$income[pos_p],
                             puf_cells$weight[pos_p], probs = seq(0, 1, 0.2))
puf_cells$pct_q = findInterval(puf_cells$income, brks_p, all.inside = TRUE)
puf_cells$pct_q[!pos_p] = 0L
puf_cells$age_b = cut(puf_cells$age1, c(-Inf, 35, 55, 65, Inf),
                      labels = c('<35', '35-54', '55-64', '65+'), right = FALSE)
puf_cells$mar_b = ifelse(puf_cells$married == 1L, 'mar', 'sin')
puf_cells$has_biz_b = ifelse(is.na(puf_cells$business_p), NA_character_,
                       ifelse(puf_cells$business_p > 0, 'biz+', 'biz0'))
puf_cells$puf_cell = paste(puf_cells$pct_q, puf_cells$mar_b, puf_cells$age_b,
                           puf_cells$has_biz_b, sep = '|')

# Join: puf_id → (puf_cell, weight) + donor SCF cell
joined = match_df %>%
  left_join(puf_cells %>% select(id, puf_cell, weight), by = c('puf_id' = 'id')) %>%
  mutate(scf_cell = scf_cells$cell[scf_row])

cat('\nJoin diagnostics:\n')
cat(sprintf('  rows: %d\n', nrow(joined)))
cat(sprintf('  missing puf_cell: %d\n', sum(is.na(joined$puf_cell))))
cat(sprintf('  missing scf_cell: %d\n', sum(is.na(joined$scf_cell))))

#---------------------------------------------------------------------------
# Cross-tab: diagonal share per PUF cell (weight of donors from same cell
# as PUF unit) and off-diagonal distribution for top overshot cells.
#---------------------------------------------------------------------------

j = joined %>% filter(!is.na(puf_cell), !is.na(scf_cell))

# Per PUF cell: total weight, weight with scf_cell == puf_cell, weight with
# scf_cell != puf_cell, and top-3 donor cells (by weight) when off-diagonal.
diag_tbl = j %>%
  group_by(puf_cell) %>%
  summarise(
    W_puf         = sum(weight),
    W_same_cell   = sum(weight[scf_cell == puf_cell]),
    W_other_cell  = sum(weight[scf_cell != puf_cell]),
    same_frac     = W_same_cell / W_puf,
    n_rows        = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(W_puf))

cat('\n', strrep('=', 80), '\n', sep = '')
cat('Diagonal fraction per PUF cell (top by weight)\n')
cat(strrep('=', 80), '\n', sep = '')
cat('  same_frac = weight of donors from same cell / total weight in puf cell\n')
cat('  Low same_frac = DRF donors coming from different cells than the PUF unit\n\n')

cat(sprintf('  %-30s %10s %10s %8s\n',
            'puf_cell', 'W_puf(M)', 'W_same(M)', 'same%'))
cat('  ', strrep('-', 64), '\n', sep = '')
for (i in seq_len(min(25, nrow(diag_tbl)))) {
  cat(sprintf('  %-30s %10.2f %10.2f %7.1f%%\n',
              diag_tbl$puf_cell[i],
              diag_tbl$W_puf[i] / 1e6,
              diag_tbl$W_same_cell[i] / 1e6,
              100 * diag_tbl$same_frac[i]))
}

write_csv(diag_tbl, file.path(plot_dir, 'donor_diagonal_frac.csv'))

#---------------------------------------------------------------------------
# For the top-overshot PUF cells, break down where donors come from.
#---------------------------------------------------------------------------

target_cells = c('4|mar|65+|biz0', '4|mar|65+|biz+',
                 '5|mar|35-54|biz0', '5|mar|65+|biz+',
                 '2|sin|65+|biz0')

for (tc in target_cells) {
  cat('\n', strrep('-', 80), '\n', sep = '')
  cat(sprintf('Donor breakdown for PUF cell %s\n', tc))
  cat(strrep('-', 80), '\n', sep = '')

  sub = j %>% filter(puf_cell == tc) %>%
    group_by(scf_cell) %>%
    summarise(W = sum(weight), n_rows = n(), .groups = 'drop') %>%
    arrange(desc(W)) %>%
    mutate(share = W / sum(W))

  W_total = sum(sub$W)
  cat(sprintf('  PUF total weight: %.2fM\n', W_total / 1e6))
  cat(sprintf('  Number of distinct donor cells: %d\n', nrow(sub)))
  cat(sprintf('  Donor cells receiving >= 1%% of weight:\n\n'))
  cat(sprintf('  %-30s %10s %8s\n', 'donor scf_cell', 'W (M)', 'share'))
  shown = sub %>% filter(share >= 0.01)
  for (k in seq_len(nrow(shown))) {
    marker = if (shown$scf_cell[k] == tc) ' ← self' else ''
    cat(sprintf('  %-30s %10.2f %7.1f%%%s\n',
                shown$scf_cell[k], shown$W[k] / 1e6,
                100 * shown$share[k], marker))
  }

  # How much of the PUF cell's imputed wealth comes from donors in wealthier
  # cells? Define "wealthier" as higher income quintile (pct_q).
  tc_q = as.integer(strsplit(tc, '\\|')[[1]][1])
  sub_q = j %>% filter(puf_cell == tc) %>%
    mutate(scf_q = as.integer(sapply(strsplit(scf_cell, '\\|'),
                                     function(x) x[1])))
  higher_q = sum(sub_q$weight[sub_q$scf_q > tc_q]) / W_total
  lower_q  = sum(sub_q$weight[sub_q$scf_q < tc_q]) / W_total
  same_q   = sum(sub_q$weight[sub_q$scf_q == tc_q]) / W_total
  cat(sprintf('\n  Donor income quintile (relative to PUF cell q=%d):\n', tc_q))
  cat(sprintf('    lower  q:  %6.1f%% of weight\n', 100 * lower_q))
  cat(sprintf('    same   q:  %6.1f%% of weight\n', 100 * same_q))
  cat(sprintf('    higher q:  %6.1f%% of weight\n', 100 * higher_q))
}

cat('\n\nArtifacts:\n')
cat('  donor_diagonal_frac.csv  — diagonal fraction per PUF cell\n')
cat('  donor_crosstab_report.txt — this report\n')

sink()
