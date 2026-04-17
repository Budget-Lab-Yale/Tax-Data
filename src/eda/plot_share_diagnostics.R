#--------------------------------------
# plot_share_diagnostics.R
#
# Stage B companion diagnostics:
# (1) Per-cell quantile curves (CEX vs PUF) for housing_utilities and
#     other_services_health, on the same 10 sampled cells as plot 14.
# (2) Motor-vehicles zero-share test: weighted Pr(share_mv > 0) per
#     cell, CEX vs PUF. Plus the same scatter for all 8 categories.
#--------------------------------------

lapply(c('tidyverse', 'ggplot2', 'Hmisc', 'data.table'),
       library, character.only = TRUE)

pce_cats = c('c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables',
             'c_food_off_premises', 'c_gasoline', 'c_housing_utilities',
             'c_other_services_health')

plot_dir = 'plots'

cex = read_rds('resources/cache/cex_training_cached.rds')$cex
puf = read_rds('resources/cache/consumption_analysis.rds')

cex_h = cex %>%
  filter(has_income == 1, income > 0, total_consumption > 0,
         pctile_income >= 1) %>%
  mutate(married = as.integer(married), w = WT_ANNUAL,
         pct = pctile_income) %>%
  select(w, pct, age1, married, n_dep, total_consumption, all_of(pce_cats))

puf_h = puf %>%
  filter(income > 0, C > 0) %>%
  mutate(married = as.integer(!is.na(male2)),
         w = weight, total_consumption = C, pct = pctile) %>%
  select(w, pct, age1, married, n_dep, total_consumption, all_of(pce_cats))

# Cell binning (same as conditional_diagnostic.R in pctile mode)
inc_brk = c(0, 20, 40, 60, 80, 98, 100)
inc_labs = c('Q1','Q2','Q3','Q4','Q5','OOS')
age_brk = c(-Inf, 34, 54, 64, Inf); age_labs = c('18-34','35-54','55-64','65+')
dep_brk = c(-Inf, 0, 1, 2, Inf);    dep_labs = c('0','1','2','3+')

bin_cells = function(df) {
  df %>% mutate(
    inc_bin = cut(pct, inc_brk, labels = inc_labs, right = TRUE,
                  include.lowest = TRUE),
    age_bin = cut(age1, age_brk, labels = age_labs, right = TRUE),
    dep_bin = cut(n_dep, dep_brk, labels = dep_labs, right = TRUE),
    mar_bin = factor(married, levels = c(0,1),
                     labels = c('single','married')),
    cell = paste(inc_bin, mar_bin, age_bin, dep_bin, sep = '|')
  )
}

cex_h = bin_cells(cex_h)
puf_h = bin_cells(puf_h)

cell_n = cex_h %>% filter(inc_bin != 'OOS') %>%
  count(cell, name = 'n_cex') %>%
  inner_join(puf_h %>% filter(inc_bin != 'OOS') %>%
               count(cell, name = 'n_puf'), by = 'cell') %>%
  filter(n_cex >= 50, n_puf >= 50)

# Share columns (cat / total_C, with !is.finite → 0)
for (cat in pce_cats) {
  sh = paste0(cat, '_sh')
  cex_h[[sh]] = cex_h[[cat]] / cex_h$total_consumption
  puf_h[[sh]] = puf_h[[cat]] / puf_h$total_consumption
  cex_h[[sh]][!is.finite(cex_h[[sh]])] = 0
  puf_h[[sh]][!is.finite(puf_h[[sh]])] = 0
}

#---------------------------------------------------------------------------
# (1) Per-cell quantile curves — same 10 sampled cells as plot 14
#---------------------------------------------------------------------------

MIN_N = 250
eligible = cell_n %>% filter(n_cex >= MIN_N, n_puf >= MIN_N)
set.seed(42)
sampled = eligible %>% sample_n(10) %>% arrange(cell)
cat(sprintf('Sampled %d cells (same seed=42 as plot 14).\n', nrow(sampled)))

probs = seq(0.01, 0.99, 0.01)

quant_for = function(df, val_col) {
  df %>% filter(cell %in% sampled$cell) %>%
    group_by(cell) %>%
    group_modify(~ tibble(
      p = probs,
      q = as.numeric(Hmisc::wtd.quantile(.x[[val_col]], .x$w,
                                         probs = probs, na.rm = TRUE))
    )) %>%
    ungroup()
}

make_panel = function(cat, y_lbl, title_extra = '') {
  sh = paste0(cat, '_sh')
  ql = bind_rows(
    quant_for(cex_h, sh) %>% mutate(source = 'CEX'),
    quant_for(puf_h, sh) %>% mutate(source = 'PUF')
  ) %>%
    left_join(sampled %>% select(cell, n_cex, n_puf), by = 'cell') %>%
    mutate(cell_lbl = sprintf('%s  (n=%d/%d)', cell, n_cex, n_puf))

  ggplot(ql, aes(p, q, color = source)) +
    geom_line(linewidth = 0.9) +
    facet_wrap(~ cell_lbl, ncol = 2, scales = 'free_y') +
    scale_color_manual(values = c(CEX = '#2c7bb6', PUF = '#d7191c'),
                       name = NULL) +
    labs(title    = sprintf('F^-1(p) of %s share by cell%s', cat, title_extra),
         subtitle = 'Same 10 rank-binned cells as plot 14.  x=percentile, y=share of total C.',
         x = 'Percentile p', y = y_lbl) +
    theme_minimal(base_size = 10) +
    theme(legend.position = 'top',
          strip.text      = element_text(size = 8),
          panel.grid.minor = element_blank())
}

ggsave(file.path(plot_dir, '15_share_housing_sample.png'),
       make_panel('c_housing_utilities',     'Housing + utilities share'),
       width = 11, height = 14, dpi = 140)

ggsave(file.path(plot_dir, '16_share_svcshealth_sample.png'),
       make_panel('c_other_services_health', 'Other services + health share'),
       width = 11, height = 14, dpi = 140)

ggsave(file.path(plot_dir, '17_share_motorveh_sample.png'),
       make_panel('c_motor_vehicles',        'Motor vehicles share'),
       width = 11, height = 14, dpi = 140)

#---------------------------------------------------------------------------
# (2) Motor-vehicles zero-share test across all 81 valid cells
#---------------------------------------------------------------------------

zero_frac = function(df, val_col) {
  df %>% filter(cell %in% cell_n$cell) %>%
    group_by(cell) %>%
    summarise(pr_pos = sum(w * (.data[[val_col]] > 0)) / sum(w),
              .groups = 'drop')
}

zf_mv = zero_frac(cex_h, 'c_motor_vehicles_sh') %>% rename(cex_pr = pr_pos) %>%
  inner_join(zero_frac(puf_h, 'c_motor_vehicles_sh') %>%
               rename(puf_pr = pr_pos), by = 'cell') %>%
  inner_join(cell_n, by = 'cell') %>%
  mutate(d = puf_pr - cex_pr)

cat('\n========== Motor vehicles: Pr(share > 0) per cell ==========\n')
cat(sprintf('cells: %d\n', nrow(zf_mv)))
cat(sprintf('CEX weighted-mean Pr(pos): %.3f\n',
            weighted.mean(zf_mv$cex_pr, zf_mv$n_cex)))
cat(sprintf('PUF weighted-mean Pr(pos): %.3f\n',
            weighted.mean(zf_mv$puf_pr, zf_mv$n_puf)))
cat(sprintf('median (PUF - CEX) across cells: %+.3f\n', median(zf_mv$d)))
cat(sprintf('range (PUF - CEX): [%+.3f, %+.3f]\n',
            min(zf_mv$d), max(zf_mv$d)))

cat('\nAll 81 cells (Pr(mv > 0), CEX vs PUF, diff), sorted by |diff|:\n')
tbl = zf_mv %>%
  arrange(desc(abs(d))) %>%
  mutate(cex_pr = round(cex_pr, 3),
         puf_pr = round(puf_pr, 3),
         d      = round(d, 3)) %>%
  select(cell, cex_pr, puf_pr, d, n_cex, n_puf)
print(tbl, n = nrow(tbl))

p_mv = ggplot(zf_mv, aes(cex_pr, puf_pr)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = 'grey50') +
  geom_point(alpha = 0.7, size = 2.2, color = '#d73027') +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  coord_equal() +
  labs(title    = 'Motor vehicles: Pr(share > 0) per cell, CEX vs PUF',
       subtitle = 'Each dot = one of 81 valid cells. Dashed 45°: perfect match.',
       x = 'CEX weighted Pr(motor_veh share > 0)',
       y = 'PUF weighted Pr(motor_veh share > 0)') +
  theme_minimal(base_size = 11)

ggsave(file.path(plot_dir, '18_motor_vehicles_zero_test.png'),
       p_mv, width = 6, height = 6, dpi = 140)

#---------------------------------------------------------------------------
# (3) Same zero-fraction scatter for ALL 8 categories
#---------------------------------------------------------------------------

zfs = pce_cats %>% map(function(cat) {
  sh = paste0(cat, '_sh')
  zero_frac(cex_h, sh) %>% rename(cex_pr = pr_pos) %>%
    inner_join(zero_frac(puf_h, sh) %>% rename(puf_pr = pr_pos),
               by = 'cell') %>%
    mutate(category = cat)
}) %>% bind_rows() %>%
  mutate(category = factor(category, levels = pce_cats))

p_all = ggplot(zfs, aes(cex_pr, puf_pr)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = 'grey50') +
  geom_point(alpha = 0.5, size = 1.2, color = '#2c7bb6') +
  facet_wrap(~ category, ncol = 4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  coord_equal() +
  labs(title    = 'Pr(share > 0) per cell, CEX vs PUF — all 8 categories',
       subtitle = 'Each dot = one of 81 valid cells. 45° = perfect match.',
       x = 'CEX weighted Pr(share > 0)',
       y = 'PUF weighted Pr(share > 0)') +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = 'bold'))

ggsave(file.path(plot_dir, '19_all_categories_zero_test.png'),
       p_all, width = 12, height = 6, dpi = 140)

cat('\nSaved:\n')
cat('  15_share_housing_sample.png\n')
cat('  16_share_svcshealth_sample.png\n')
cat('  17_share_motorveh_sample.png\n')
cat('  18_motor_vehicles_zero_test.png\n')
cat('  19_all_categories_zero_test.png\n')
