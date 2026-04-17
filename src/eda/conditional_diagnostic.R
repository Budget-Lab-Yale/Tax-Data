#--------------------------------------
# conditional_diagnostic.R
#
# Compares F(C | X) and F(share_k | X) between raw CEX (donor
# reference) and DRF-imputed PUF using cell-wise weighted quantile
# tables. Steps 1-3 of the conditional-diagnostic protocol:
#   1. Define covariate cells
#   2. Per-cell weighted quantile table at p = 0.01..0.99 for
#      total consumption, plus KS and |Δ median|
#   3. Repeat step 2 for each share_k = cat_k / total_C
# Step 4 (marginal comparison + X-reweighting) is deferred.
#--------------------------------------

lapply(c('tidyverse', 'magrittr', 'data.table', 'Hmisc', 'ggplot2'),
       library, character.only = TRUE)

pce_cats = c('c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables',
             'c_food_off_premises', 'c_gasoline', 'c_housing_utilities',
             'c_other_services_health')

plot_dir  = 'plots'
cache_dir = 'resources/cache'
dir.create(plot_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

probs = seq(0.01, 0.99, 0.01)

# cell_mode:
#   'pctile' — bin by own-dataset positive-income rank. Tests rank-match
#             assumption (what this pipeline is built around): $ values
#             may differ across datasets, but rank order is preserved.
#             Cells align with the QRF's `pctile_income` feature.
#   'dollar' — bin by raw $ income (with thresholds from CEX quintiles).
#             Tests $-level conditional match. Reveals income-measurement
#             mismatch between CEX and PUF but is NOT the rank-match test.
cell_mode = 'pctile'
suffix    = if (cell_mode == 'pctile') '_pctile' else '_dollar'
cat(sprintf('Cell mode: %s (suffix=%s)\n', cell_mode, suffix))

#---------------------------------------------------------------------------
# Load inputs (CEX training + PUF post-imputation), harmonize schemas
#---------------------------------------------------------------------------

cex_cache_file = file.path(cache_dir, 'cex_training_cached.rds')
if (!file.exists(cex_cache_file)) {
  cat('Building CEX training data (cache miss; ~2 min)...\n')
  source('src/cex.R')
  cex_raw = build_cex_training()
  write_rds(list(
    cex             = cex_raw,
    elasticity_beta = attr(cex_raw, 'elasticity_beta'),
    y_ref           = attr(cex_raw, 'y_ref')
  ), cex_cache_file)
  cex = cex_raw
} else {
  cat('Loading cached CEX training data...\n')
  cex = read_rds(cex_cache_file)$cex
}

puf_cache = file.path(cache_dir, 'consumption_analysis.rds')
if (!file.exists(puf_cache))
  stop('PUF cache missing — run test_consumption.R first')
puf = read_rds(puf_cache)

# Harmonize to a common schema:
#   w, income, age1, married (0/1), n_dep, total_consumption, <pce_cats>
cex_h = cex %>%
  filter(has_income == 1, income > 0, total_consumption > 0,
         pctile_income >= 1) %>%
  mutate(married = as.integer(married), w = WT_ANNUAL,
         pct     = pctile_income) %>%
  select(w, pct, income, age1, married, n_dep,
         total_consumption, all_of(pce_cats))

puf_h = puf %>%
  filter(income > 0, C > 0) %>%
  mutate(married = as.integer(!is.na(male2)),
         w       = weight,
         total_consumption = C,
         pct     = pctile) %>%
  select(w, pct, income, age1, married, n_dep,
         total_consumption, all_of(pce_cats))

#---------------------------------------------------------------------------
# Step 1: cell definition
#
# Income bands set on CEX-weighted quintile breakpoints, with an extra
# top band above CEX P98 flagged as "OOS" (PUF units up there extrapolate
# via the power-law — not validated by CEX conditional). Age and n_dep
# discretized coarsely so CEX has enough per cell.
#---------------------------------------------------------------------------

if (cell_mode == 'pctile') {
  inc_brk  = c(0, 20, 40, 60, 80, 98, 100)
  inc_labs = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5', 'OOS')
  cell_col = 'pct'
} else {
  inc_brk  = c(-Inf, wtd.quantile(cex_h$income, cex_h$w,
                                  probs = c(0.2, 0.4, 0.6, 0.8, 0.98)), Inf)
  inc_labs = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5', 'OOS')
  cell_col = 'income'
}
age_brk  = c(-Inf, 34, 54, 64, Inf)
age_labs = c('18-34', '35-54', '55-64', '65+')
dep_brk  = c(-Inf, 0, 1, 2, Inf)
dep_labs = c('0', '1', '2', '3+')

cat('\nCell scheme:\n')
cat(sprintf('  income bin breakpoints (%s): %s\n',
            cell_mode, paste(round(inc_brk[2:6], 2), collapse = ', ')))
cat(sprintf('  top band "OOS": %s > %s\n', cell_col, round(inc_brk[6], 2)))

bin_cells = function(df) {
  df %>% mutate(
    inc_bin = cut(.data[[cell_col]], inc_brk, labels = inc_labs,
                  right = TRUE, include.lowest = TRUE),
    age_bin = cut(age1,   age_brk, labels = age_labs, right = TRUE),
    dep_bin = cut(n_dep,  dep_brk, labels = dep_labs, right = TRUE),
    mar_bin = factor(married, levels = c(0, 1), labels = c('single', 'married')),
    cell    = paste(inc_bin, mar_bin, age_bin, dep_bin, sep = '|')
  )
}

cex_h = bin_cells(cex_h)
puf_h = bin_cells(puf_h)

cat(sprintf('\nCEX: %d obs, %d unique cells\n', nrow(cex_h), n_distinct(cex_h$cell)))
cat(sprintf('PUF: %d obs, %d unique cells\n', nrow(puf_h), n_distinct(puf_h$cell)))

# Prune to cells with >=50 obs on both sides and income != OOS
cell_n = cex_h %>% filter(inc_bin != 'OOS') %>%
  count(cell, name = 'n_cex') %>%
  inner_join(puf_h %>% filter(inc_bin != 'OOS') %>%
               count(cell, name = 'n_puf'), by = 'cell') %>%
  filter(n_cex >= 50, n_puf >= 50) %>%
  arrange(cell)

valid_cells = cell_n$cell
cat(sprintf('\nValid cells (both n>=50, not income-OOS): %d\n', length(valid_cells)))
cat(sprintf('  total CEX obs in valid cells: %d\n', sum(cell_n$n_cex)))
cat(sprintf('  total PUF obs in valid cells: %d\n', sum(cell_n$n_puf)))

# Record PUF units excluded as OOS or in cells below threshold
n_oos_income = sum(puf_h$inc_bin == 'OOS')
n_thin_cells = sum(!(puf_h$cell %in% valid_cells) & puf_h$inc_bin != 'OOS')
cat(sprintf('\nPUF coverage of valid cells: %.1f%%\n',
            100 * sum(cell_n$n_puf) / nrow(puf_h)))
cat(sprintf('  PUF units in top-tail OOS band:  %d (%.1f%%)\n',
            n_oos_income, 100 * n_oos_income / nrow(puf_h)))
cat(sprintf('  PUF units in thin-CEX cells:     %d (%.1f%%)\n',
            n_thin_cells, 100 * n_thin_cells / nrow(puf_h)))

#---------------------------------------------------------------------------
# Helpers: weighted quantile table (long), weighted 2-sample KS
#---------------------------------------------------------------------------

wquant_long = function(df, val_col, src_lbl) {
  df %>% filter(cell %in% valid_cells) %>%
    group_by(cell) %>%
    group_modify(~ {
      x = .x[[val_col]]; w = .x$w
      tibble(p = probs,
             q = as.numeric(Hmisc::wtd.quantile(x, w, probs = probs,
                                                na.rm = TRUE)))
    }) %>%
    ungroup() %>%
    mutate(source = src_lbl)
}

# Sort-merge weighted 2-sample KS: O((n1+n2) log(n1+n2)).
# Evaluates both step CDFs at the sorted union of the two samples.
w_ks = function(x1, w1, x2, w2) {
  df = data.frame(x = c(x1, x2),
                  w = c(w1, w2),
                  src = c(rep(1L, length(x1)), rep(2L, length(x2))))
  df = df[order(df$x), ]
  tw1 = sum(w1); tw2 = sum(w2)
  F1 = cumsum(ifelse(df$src == 1L, df$w, 0)) / tw1
  F2 = cumsum(ifelse(df$src == 2L, df$w, 0)) / tw2
  max(abs(F1 - F2))
}

per_cell_ks = function(df_cex, df_puf, val_col) {
  # Pre-split for speed
  cex_by = split(df_cex[, c(val_col, 'w')], df_cex$cell)
  puf_by = split(df_puf[, c(val_col, 'w')], df_puf$cell)
  map_dfr(valid_cells, ~ {
    c1 = cex_by[[.x]]; c2 = puf_by[[.x]]
    tibble(cell = .x,
           ks   = w_ks(c1[[val_col]], c1$w, c2[[val_col]], c2$w))
  })
}

summarize_one = function(quant_long, df_cex, df_puf, val_col, tag) {
  wide = quant_long %>%
    pivot_wider(names_from = source, values_from = q,
                names_prefix = 'q_')
  smr = wide %>% group_by(cell) %>%
    summarise(med_cex = q_CEX[p == 0.5],
              med_puf = q_PUF[p == 0.5],
              .groups = 'drop') %>%
    mutate(d_med     = med_puf - med_cex,
           rel_d_med = d_med / pmax(abs(med_cex), 1e-9))

  # Weighted means per cell (for reference)
  m_cex = df_cex %>% filter(cell %in% valid_cells) %>%
    group_by(cell) %>% summarise(mean_cex = weighted.mean(.data[[val_col]], w),
                                 .groups = 'drop')
  m_puf = df_puf %>% filter(cell %in% valid_cells) %>%
    group_by(cell) %>% summarise(mean_puf = weighted.mean(.data[[val_col]], w),
                                 .groups = 'drop')

  ks = per_cell_ks(df_cex, df_puf, val_col)

  cell_n %>%
    left_join(smr,   by = 'cell') %>%
    left_join(m_cex, by = 'cell') %>%
    left_join(m_puf, by = 'cell') %>%
    left_join(ks,    by = 'cell') %>%
    mutate(d_mean     = mean_puf - mean_cex,
           rel_d_mean = d_mean / pmax(abs(mean_cex), 1e-9),
           metric     = tag)
}

#---------------------------------------------------------------------------
# Step 2: Stage A — F(total_consumption | cell)
#---------------------------------------------------------------------------

cat('\n\n=== Stage A: F(C | cell) ===\n')

qA = bind_rows(
  wquant_long(cex_h, 'total_consumption', 'CEX'),
  wquant_long(puf_h, 'total_consumption', 'PUF')
)

summary_A = summarize_one(qA, cex_h, puf_h, 'total_consumption', 'C')

cat(sprintf('  cells:            %d\n', nrow(summary_A)))
cat(sprintf('  median KS:        %.3f\n', median(summary_A$ks)))
cat(sprintf('  90th-pct KS:      %.3f\n', quantile(summary_A$ks, 0.9)))
cat(sprintf('  max KS (cell):    %.3f   (%s)\n',
            max(summary_A$ks),
            summary_A$cell[which.max(summary_A$ks)]))
cat(sprintf('  median |rel Δ median|:  %.3f\n',
            median(abs(summary_A$rel_d_med))))
cat(sprintf('  median |rel Δ mean|:    %.3f\n',
            median(abs(summary_A$rel_d_mean))))

#---------------------------------------------------------------------------
# Step 3: Stage B — F(share_k | cell) for each category
#---------------------------------------------------------------------------

cat('\n\n=== Stage B: F(share_k | cell) ===\n')

# Construct share columns on both frames
for (cat in pce_cats) {
  sh = paste0(cat, '_sh')
  cex_h[[sh]] = cex_h[[cat]] / cex_h$total_consumption
  puf_h[[sh]] = puf_h[[cat]] / puf_h$total_consumption
  cex_h[[sh]][!is.finite(cex_h[[sh]])] = 0
  puf_h[[sh]][!is.finite(puf_h[[sh]])] = 0
}

qB_all = list()
summary_B_all = list()

for (cat in pce_cats) {
  sh = paste0(cat, '_sh')
  qB = bind_rows(
    wquant_long(cex_h, sh, 'CEX'),
    wquant_long(puf_h, sh, 'PUF')
  ) %>% mutate(category = cat)
  smr = summarize_one(qB %>% select(-category), cex_h, puf_h, sh, cat) %>%
    mutate(category = cat)
  qB_all[[cat]]        = qB
  summary_B_all[[cat]] = smr

  cat(sprintf('  %-24s  med KS %.3f   p90 KS %.3f   med |Δmed| %.4f\n',
              cat, median(smr$ks), quantile(smr$ks, 0.9),
              median(abs(smr$d_med))))
}

qB_long       = bind_rows(qB_all)
summary_B     = bind_rows(summary_B_all)

#---------------------------------------------------------------------------
# Persist full quantile long-tables + per-cell summaries
#---------------------------------------------------------------------------

write_rds(list(
  quantile_long = qA,
  summary       = summary_A,
  cell_n        = cell_n,
  inc_brk       = inc_brk,
  cell_mode     = cell_mode
), file.path(cache_dir, paste0('conditional_diag_stageA', suffix, '.rds')))

write_rds(list(
  quantile_long = qB_long,
  summary       = summary_B,
  cell_n        = cell_n,
  inc_brk       = inc_brk,
  cell_mode     = cell_mode
), file.path(cache_dir, paste0('conditional_diag_stageB', suffix, '.rds')))

cat(sprintf('\nSaved: resources/cache/conditional_diag_stageA%s.rds\n', suffix))
cat(sprintf('Saved: resources/cache/conditional_diag_stageB%s.rds\n', suffix))

#---------------------------------------------------------------------------
# Plot 09: Stage A heatmap of relative |Δ median C| across cells
# Grid:   x = age_bin, y = dep_bin
# Facet:  rows = marital, cols = income quintile
#---------------------------------------------------------------------------

hm_A = summary_A %>%
  separate(cell, c('inc_bin','mar_bin','age_bin','dep_bin'),
           sep = '\\|', remove = FALSE) %>%
  mutate(inc_bin = factor(inc_bin, levels = inc_labs[1:5]),
         mar_bin = factor(mar_bin, levels = c('single','married')),
         age_bin = factor(age_bin, levels = age_labs),
         dep_bin = factor(dep_bin, levels = dep_labs),
         rel_d_med_pct = 100 * rel_d_med)

pA = ggplot(hm_A, aes(age_bin, dep_bin, fill = rel_d_med_pct)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = sprintf('%+.0f%%', rel_d_med_pct)),
            size = 2.4, color = 'black') +
  scale_fill_gradient2(low = '#2166ac', mid = 'white', high = '#b2182b',
                       midpoint = 0, name = 'Δ med C\n(PUF - CEX)\n(%)',
                       limits = c(-50, 50), oob = scales::squish) +
  facet_grid(mar_bin ~ inc_bin) +
  labs(title    = sprintf('Stage A: relative Δ median C by cell  (PUF - CEX)  [%s]', cell_mode),
       subtitle = sprintf('Cells: income %s × marital × age × n_dep.  >=50 obs each side.',
                          if (cell_mode == 'pctile') 'rank-bin' else '$-bin'),
       x = 'Age band', y = 'n_dep band') +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = 'bold'))

ggsave(file.path(plot_dir, paste0('09_conditional_stageA_heatmap', suffix, '.png')),
       pA, width = 12, height = 5, dpi = 140)

#---------------------------------------------------------------------------
# Plot 10: Stage A heatmap of KS distance — same grid
#---------------------------------------------------------------------------

pA_ks = ggplot(hm_A, aes(age_bin, dep_bin, fill = ks)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = sprintf('%.2f', ks)), size = 2.4, color = 'black') +
  scale_fill_gradient(low = 'white', high = '#d73027',
                      name = 'KS (CEX vs PUF)', limits = c(0, NA)) +
  facet_grid(mar_bin ~ inc_bin) +
  labs(title    = sprintf('Stage A: weighted KS(F_CEX(C), F_PUF(C)) by cell  [%s]', cell_mode),
       x = 'Age band', y = 'n_dep band') +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = 'bold'))

ggsave(file.path(plot_dir, paste0('10_conditional_stageA_ks', suffix, '.png')),
       pA_ks, width = 12, height = 5, dpi = 140)

#---------------------------------------------------------------------------
# Plot 11: Stage B heatmap — per-category median |Δ median share| summary
# Collapsed over cells to a single matrix (income × category).
#---------------------------------------------------------------------------

hm_B = summary_B %>%
  separate(cell, c('inc_bin','mar_bin','age_bin','dep_bin'),
           sep = '\\|', remove = FALSE) %>%
  mutate(inc_bin = factor(inc_bin, levels = inc_labs[1:5])) %>%
  group_by(inc_bin, category) %>%
  summarise(median_abs_d_med = median(abs(d_med)),
            median_ks        = median(ks),
            .groups = 'drop') %>%
  mutate(category = factor(category, levels = pce_cats))

pB_med = ggplot(hm_B, aes(inc_bin, category, fill = median_abs_d_med)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = sprintf('%.3f', median_abs_d_med)),
            size = 2.8, color = 'black') +
  scale_fill_gradient(low = 'white', high = '#b2182b',
                      name = 'median |Δ med share|') +
  labs(title    = 'Stage B: per-category median |Δ median share| (across cells)',
       subtitle = 'One value per (income quintile, category).  Lower = better.',
       x = 'Income quintile', y = 'Category') +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank())

ggsave(file.path(plot_dir, paste0('11_conditional_stageB_med', suffix, '.png')),
       pB_med, width = 8, height = 5, dpi = 140)

pB_ks = ggplot(hm_B, aes(inc_bin, category, fill = median_ks)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = sprintf('%.2f', median_ks)),
            size = 2.8, color = 'black') +
  scale_fill_gradient(low = 'white', high = '#d73027',
                      name = 'median KS') +
  labs(title    = 'Stage B: per-category median KS across cells',
       x = 'Income quintile', y = 'Category') +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank())

ggsave(file.path(plot_dir, paste0('12_conditional_stageB_ks', suffix, '.png')),
       pB_ks, width = 8, height = 5, dpi = 140)

#---------------------------------------------------------------------------
# Plot 13: QQ panels for the Stage A cells with the 12 largest KS values
# (one mini-plot per cell, CEX quantile on x, PUF quantile on y)
#---------------------------------------------------------------------------

worst12 = summary_A %>% arrange(desc(ks)) %>% head(12) %>% pull(cell)

qq_df = qA %>% filter(cell %in% worst12) %>%
  pivot_wider(names_from = source, values_from = q, names_prefix = 'q_') %>%
  mutate(cell = factor(cell, levels = worst12))

pQQ = ggplot(qq_df, aes(q_CEX, q_PUF)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = 'grey60') +
  geom_point(size = 0.5, alpha = 0.7, color = 'steelblue') +
  facet_wrap(~ cell, scales = 'free', ncol = 4) +
  labs(title = 'Stage A: CEX vs PUF quantiles for the 12 worst-KS cells',
       subtitle = 'Each point is one percentile p ∈ {0.01, ..., 0.99}. 45° = perfect match.',
       x = 'CEX quantile of C', y = 'PUF quantile of C') +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(size = 7))

ggsave(file.path(plot_dir, paste0('13_conditional_stageA_qq_worst', suffix, '.png')),
       pQQ, width = 12, height = 8, dpi = 140)

cat('\nPlots written:\n')
cat(sprintf('  09_conditional_stageA_heatmap%s.png   — Δ median C by cell\n', suffix))
cat(sprintf('  10_conditional_stageA_ks%s.png        — KS(C) by cell\n', suffix))
cat(sprintf('  11_conditional_stageB_med%s.png       — |Δ median share| per (income × category)\n', suffix))
cat(sprintf('  12_conditional_stageB_ks%s.png        — median KS(share) per (income × category)\n', suffix))
cat(sprintf('  13_conditional_stageA_qq_worst%s.png  — QQ for 12 worst-KS cells\n', suffix))

cat('\nDone.\n')
