#--------------------------------------
# wealth_gap_attribution_large.R
#
# Same gap attribution as
# wealth_gap_attribution.R, but against
# the LARGE-spec ablation output
# (resources/cache/wealth_ablation_large.rds)
# instead of production wealth_analysis.rds
# (small spec).
#
# The large spec uses 15 features including
# pctile_business, has_business_*,
# pctile_capital_gains, has_capital_gains_*,
# pctile_int_div, has_int_div,
# pctile_ss_pens, has_ss_pens, plus
# dual-binary income encoding. So the
# within-cell residual at biz/int_div
# dimensions should shrink relative to
# small spec.
#
# Inputs:
#   resources/cache/scf_tax_units.rds
#   resources/cache/wealth_analysis.rds       (demographics + weights)
#   resources/cache/wealth_ablation_large.rds (large-spec imputed Y)
#   resources/cache/consumption_analysis.rds  (raw income components)
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)

report_path = 'plots/wealth_decomp/gap_attribution_large_report.txt'
plot_dir    = 'plots/wealth_decomp'
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

scf      = read_rds('resources/cache/scf_tax_units.rds')
puf_dem  = read_rds('resources/cache/wealth_analysis.rds')  # demographics
abl      = read_rds('resources/cache/wealth_ablation_large.rds')
puf_cons = read_rds('resources/cache/consumption_analysis.rds')

sink(report_path, split = TRUE)
cat('Wealth gap attribution — LARGE-SPEC output\n')
cat('Generated: ', as.character(Sys.time()), '\n', sep = '')
cat(paste0(rep('=', 72), collapse = ''), '\n\n')

cat('Large spec features (', length(abl$feats), '):\n  ', sep = '')
cat(paste(abl$feats, collapse = ', '), '\n\n')

#---------------------------------------------------------------------------
# Build PUF frame with large-spec imputed wealth.
#   - demographics & weights: from wealth_analysis.rds (full universe)
#   - wealth columns: from ablation donor_y, matched by id
#   - X composition columns: from consumption_analysis.rds
#---------------------------------------------------------------------------

asset_vars = c('cash', 'equities', 'bonds', 'retirement', 'life_ins',
               'annuities', 'trusts', 'other_fin', 'pass_throughs',
               'primary_home', 'other_home', 're_fund', 'other_nonfin')
debt_vars  = c('primary_mortgage', 'other_mortgage', 'credit_lines',
               'credit_cards', 'installment_debt', 'other_debt')
y_vars = c(asset_vars, debt_vars)

# donor_y is a matrix (n_puf × 23) aligned to abl$id. Assemble into a tibble
# with id for joining.
stopifnot(length(abl$id) == nrow(abl$donor_y))
donor_tbl = as_tibble(abl$donor_y) %>% mutate(id = abl$id)

# Guard: the ablation used the same tax_units order as wealth_analysis.rds
# (both come from the same pipeline run). Confirm id coverage.
stopifnot(all(puf_dem$id %in% donor_tbl$id))

# Replace wealth columns on puf_dem with large-spec donor_y values,
# keeping demographics + weight + income.
puf_large = puf_dem %>%
  select(id, weight, age1, age2, married, n_dep, male1, income) %>%
  left_join(donor_tbl %>% select(id, all_of(y_vars)), by = 'id') %>%
  mutate(
    total_assets = rowSums(across(all_of(asset_vars))),
    total_debts  = rowSums(across(all_of(debt_vars))),
    net_worth    = total_assets - total_debts
  )

# X composition columns from consumption_analysis
puf_x = puf_cons %>%
  transmute(
    id,
    business_comp = sole_prop + farm +
                    scorp_active  - scorp_active_loss  - scorp_179 +
                    scorp_passive - scorp_passive_loss +
                    part_active   - part_active_loss   - part_179 +
                    part_passive  - part_passive_loss,
    int_div_comp  = txbl_int + exempt_int + div_ord + div_pref
  )

puf_h = puf_large %>%
  left_join(puf_x, by = 'id') %>%
  transmute(
    weight, age1, income, married = as.integer(married),
    business_comp, int_div_comp, net_worth
  )

scf_h = scf %>%
  transmute(
    weight, age1, income, married = as.integer(married),
    business_comp = business_scf,
    int_div_comp  = int_div_scf,
    net_worth = !!rlang::parse_expr(paste(asset_vars, collapse = ' + ')) -
                !!rlang::parse_expr(paste(debt_vars,  collapse = ' + '))
  )

cat(sprintf('Aggregate NW: SCF = $%.2fT,  PUF (large) = $%.2fT,  gap = %+.2fT\n',
            sum(scf_h$net_worth * scf_h$weight) / 1e12,
            sum(puf_h$net_worth * puf_h$weight) / 1e12,
            sum(puf_h$net_worth * puf_h$weight) / 1e12 -
              sum(scf_h$net_worth * scf_h$weight) / 1e12))

#---------------------------------------------------------------------------
# Binning + attribution — identical to wealth_gap_attribution.R
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
  d$has_biz_b = ifelse(is.na(d$business_comp), NA_character_,
                 ifelse(d$business_comp > 0, 'biz+', 'biz0'))
  d$has_id_b  = ifelse(is.na(d$int_div_comp), NA_character_,
                 ifelse(d$int_div_comp  > 0, 'id+', 'id0'))
  d
}
scf_h = bin_dims(scf_h)
puf_h = bin_dims(puf_h)

attribute_gap = function(scf_df, puf_df, cell_dims) {
  s = scf_df %>% tidyr::drop_na(all_of(cell_dims))
  p = puf_df %>% tidyr::drop_na(all_of(cell_dims))
  s$cell = do.call(paste, c(s[cell_dims], sep = '|'))
  p$cell = do.call(paste, c(p[cell_dims], sep = '|'))

  sc = s %>% group_by(cell) %>%
    summarise(W_scf = sum(weight),
              NW_scf_tn = sum(net_worth * weight) / 1e12,
              mu_scf = sum(net_worth * weight) / sum(weight),
              .groups = 'drop')
  pc = p %>% group_by(cell) %>%
    summarise(W_puf = sum(weight),
              NW_puf_tn = sum(net_worth * weight) / 1e12,
              mu_puf = sum(net_worth * weight) / sum(weight),
              .groups = 'drop')

  full_join(sc, pc, by = 'cell') %>%
    mutate(across(c(W_scf, W_puf, NW_scf_tn, NW_puf_tn, mu_scf, mu_puf),
                  ~ replace_na(., 0)),
           gap_tn = NW_puf_tn - NW_scf_tn) %>%
    arrange(desc(abs(gap_tn)))
}

pretty_print = function(df, title, n = 20) {
  cat('\n', strrep('=', 72), '\n', sep = '')
  cat(title, '\n')
  cat(strrep('=', 72), '\n', sep = '')
  cat(sprintf('  aggregate totals — SCF: $%.2fT, PUF: $%.2fT, gap: %+.2fT\n',
              sum(df$NW_scf_tn), sum(df$NW_puf_tn),
              sum(df$NW_puf_tn) - sum(df$NW_scf_tn)))
  cat(sprintf('  cells: %d populated; showing top %d by |gap|\n\n',
              nrow(df), min(n, nrow(df))))
  cat(sprintf('  %-30s | %8s %8s %8s | %10s %10s %8s\n',
              'cell', 'W_SCF(M)', 'W_PUF(M)', 'ΔW(M)',
              'NW_SCF$T', 'NW_PUF$T', 'gap$T'))
  cat('  ', strrep('-', 92), '\n', sep = '')
  for (i in seq_len(min(n, nrow(df)))) {
    cat(sprintf('  %-30s | %8.2f %8.2f %+8.2f | %10.2f %10.2f %+8.2f\n',
                df$cell[i],
                df$W_scf[i] / 1e6, df$W_puf[i] / 1e6,
                (df$W_puf[i] - df$W_scf[i]) / 1e6,
                df$NW_scf_tn[i], df$NW_puf_tn[i], df$gap_tn[i]))
  }
  cum_abs = cumsum(abs(df$gap_tn))
  total_abs = sum(abs(df$gap_tn))
  for (frac in c(0.5, 0.75, 0.9)) {
    n_cover = which(cum_abs / total_abs >= frac)[1]
    cat(sprintf('  top %d cells cover %.0f%% of Σ|gap| ($%.2fT of $%.2fT)\n',
                n_cover, 100 * frac, cum_abs[n_cover], total_abs))
  }
}

for (spec in list(
  list(title = 'By income × marital × age (40-cell baseline)',
       dims  = c('pct_q', 'mar_b', 'age_b')),
  list(title = 'By income × age × has_biz (40 cells)',
       dims  = c('pct_q', 'age_b', 'has_biz_b')),
  list(title = 'By income × age × has_biz × has_int_div (80 cells)',
       dims  = c('pct_q', 'age_b', 'has_biz_b', 'has_id_b')),
  list(title = 'By income × marital × age × has_biz (80 cells)',
       dims  = c('pct_q', 'mar_b', 'age_b', 'has_biz_b'))
)) {
  df = attribute_gap(scf_h, puf_h, spec$dims)
  pretty_print(df, spec$title, n = 20)
  tag = paste(spec$dims, collapse = '_')
  write_csv(df, file.path(plot_dir, sprintf('gap_attribution_LARGE_%s.csv', tag)))
}

cat('\n\nArtifacts:\n')
cat('  gap_attribution_LARGE_<cells>.csv — per-cell tables, large-spec PUF\n')
cat('  gap_attribution_large_report.txt  — this report\n')

sink()
