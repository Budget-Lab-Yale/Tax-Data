#--------------------------------------
# wealth_gap_attribution.R
#
# Simple question: for each
# (demographics × income × composition)
# cell, what's the raw NW gap between PUF
# and SCF? Each cell contributes
#   gap_c = W_PUF,c · μ_PUF,c
#         − W_SCF,c · μ_SCF,c
# and Σ_c gap_c = aggregate gap. No
# Oaxaca decomposition, no X-effect vs
# Y|X. Just "where is the money."
#
# Cell def: pct_q × age_b × has_biz_b
#         × has_intdiv_b (80 cells)
# Also reports baseline pct_q × mar_b
#         × age_b (40 cells) for contrast.
#
# Outputs top-20 cells sorted by |gap|
# so the reader can scan which joint
# buckets drive the aggregate.
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)

report_path = 'plots/wealth_decomp/gap_attribution_report.txt'
plot_dir    = 'plots/wealth_decomp'
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

scf      = read_rds('resources/cache/scf_tax_units.rds')
puf      = read_rds('resources/cache/wealth_analysis.rds')
puf_cons = read_rds('resources/cache/consumption_analysis.rds')

sink(report_path, split = TRUE)
cat('Wealth gap attribution — raw per-cell NW contribution\n')
cat('Generated: ', as.character(Sys.time()), '\n', sep = '')
cat(paste0(rep('=', 72), collapse = ''), '\n\n')

#---------------------------------------------------------------------------
# Harmonize. Use the asset/debt totals already on each side.
#---------------------------------------------------------------------------

asset_vars = c('cash', 'equities', 'bonds', 'retirement', 'life_ins',
               'annuities', 'trusts', 'other_fin', 'pass_throughs',
               'primary_home', 'other_home', 're_fund', 'other_nonfin')
debt_vars  = c('primary_mortgage', 'other_mortgage', 'credit_lines',
               'credit_cards', 'installment_debt', 'other_debt')

scf_h = scf %>%
  transmute(
    weight, age1, income, married = as.integer(married),
    business_comp = business_scf,
    int_div_comp  = int_div_scf,
    net_worth = !!rlang::parse_expr(paste(asset_vars, collapse = ' + ')) -
                !!rlang::parse_expr(paste(debt_vars,  collapse = ' + '))
  )

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

puf_h = puf %>%
  left_join(puf_x, by = 'id') %>%
  transmute(
    weight, age1, income, married = as.integer(married),
    business_comp, int_div_comp, net_worth
  )

#---------------------------------------------------------------------------
# Binning.
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

#---------------------------------------------------------------------------
# Attribution: for a given cell def, compute per-cell aggregate NW on
# each side and the raw gap. Sort by |gap|.
#---------------------------------------------------------------------------

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

  # Running sum: how many cells to cover X% of |gap|?
  cum_abs = cumsum(abs(df$gap_tn))
  total_abs = sum(abs(df$gap_tn))
  for (frac in c(0.5, 0.75, 0.9)) {
    n_cover = which(cum_abs / total_abs >= frac)[1]
    cat(sprintf('  top %d cells cover %.0f%% of Σ|gap| ($%.2fT of $%.2fT)\n',
                n_cover, 100 * frac, cum_abs[n_cover], total_abs))
  }
}

# Three views, simple to complex.
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
  write_csv(df, file.path(plot_dir, sprintf('gap_attribution_%s.csv', tag)))
}

cat('\n\nArtifacts:\n')
cat('  gap_attribution_<cells>.csv — full per-cell gap tables\n')
cat('  gap_attribution_report.txt  — this report\n')

sink()
