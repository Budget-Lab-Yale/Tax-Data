#---------------------------------------------
# wealth_summary_diagnostics.R
#
# Post-run diagnostic harness comparing SCF
# (ground truth), PUF pre-tilt (Stage 2 only;
# uniform leaf draw), and PUF post-tilt
# (Stage 3 calibrated) wealth distributions.
#
# Produces tables + plots:
#   - Aggregate dollar totals per category
#   - Top wealth shares (1, 5, 10, 0.1 %) + Gini
#   - NW CDF overlay (three-way)
#   - NW composition by (cell_income × cell_age)
#   - Homeownership by cell
#   - Joint ownership patterns
#   - Per-cell intensive + extensive residuals
#     against SCF (the target-gap table)
#
# Inputs:
#   output_dir: directory with tax_units_2022.csv,
#               wealth_pre_tilt.rds, stage3_qc_report.rds
#               (produced by a completed pipeline run).
#
# Outputs:
#   Tables to <output_dir>/wealth_diagnostics/*.rds
#   Plots  to plots/wealth_diagnostics/<vintage>/*.pdf
#
# Run:
#   Rscript src/eda/wealth_summary_diagnostics.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble)
  library(ggplot2); library(Hmisc)
})

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')


#--- Args -------------------------------------------------------------------

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop('Usage: Rscript src/eda/wealth_summary_diagnostics.R <output_dir>')
}
output_dir = args[1]
stopifnot(dir.exists(output_dir))


#--- Load inputs ------------------------------------------------------------

puf_csv_path  = file.path(output_dir, 'tax_units_2022.csv')
pre_tilt_path = file.path(output_dir, 'wealth_pre_tilt.rds')
qc_path       = file.path(output_dir, 'stage3_qc_report.rds')
for (p in c(puf_csv_path, pre_tilt_path, qc_path)) {
  if (!file.exists(p)) stop('Missing: ', p)
}

cat('Loading inputs...\n')
puf_post = read_csv(puf_csv_path, show_col_types = FALSE)
puf_pre  = read_rds(pre_tilt_path)    # id + 23 y_vars; pre-tilt donors
qc       = read_rds(qc_path)
scf      = read_rds('resources/cache/scf_tax_units.rds')

# Ensure SCF carries 23 y_vars (collapsing raw SCFP fields if needed).
if (!all(wealth_asset_vars %in% names(scf))) {
  scf = scf %>% mutate(
    cash             = LIQ + CDS,
    equities         = STOCKS + STMUTF + COMUTF,
    bonds            = BOND + SAVBND + TFBMUTF + GBMUTF + OBMUTF,
    retirement       = IRAKH + THRIFT + FUTPEN + CURRPEN,
    life_ins         = CASHLI,
    annuities        = ANNUIT,
    trusts           = TRUSTS,
    other_fin        = OTHFIN + OMUTF,
    pass_throughs    = BUS,
    primary_home     = HOUSES,
    other_home       = ORESRE,
    re_fund          = NNRESRE,
    other_nonfin     = VEHIC + OTHNFIN,
    primary_mortgage = MRTHEL,
    other_mortgage   = RESDBT,
    credit_lines     = OTHLOC,
    credit_cards     = CCBAL,
    installment_debt = INSTALL,
    other_debt       = ODEBT
  )
}


#--- Compute cell assignment + category values for each frame --------------

# SCF.
scf_age2 = ifelse(is.na(scf$age2), 0L, scf$age2)
scf$age_older = pmax(pmin(80L, scf$age1), pmin(80L, scf_age2))
scf$income_recon = with(scf,
  wages_scf + business_scf + int_div_scf + capital_gains_scf +
  rent_scf + ss_pens_scf + ui_other_scf
)
scf = cbind(scf, compute_category_values(scf))
scf = assign_calibration_cells(scf, scf$income_recon,
                               scf$age_older, scf$weight)
scf$source_label = 'SCF'

# PUF post-tilt: reconstitute income + cell from csv cols.
puf_post_age2 = ifelse(is.na(puf_post$age2), 0L, puf_post$age2)
puf_post$age_older = pmax(pmin(80L, puf_post$age1), pmin(80L, puf_post_age2))
puf_post$income_recon = with(puf_post,
  wages + sole_prop + farm +
  scorp_active  - scorp_active_loss  - scorp_179 +
  scorp_passive - scorp_passive_loss +
  part_active   - part_active_loss   - part_179 +
  part_passive  - part_passive_loss +
  txbl_int + exempt_int + div_ord + div_pref +
  kg_lt + kg_st +
  gross_ss + gross_pens_dist + ui +
  rent - rent_loss + estate - estate_loss
)
puf_post = cbind(puf_post, compute_category_values(puf_post))
puf_post = assign_calibration_cells(puf_post, puf_post$income_recon,
                                    puf_post$age_older, puf_post$weight)
puf_post$source_label = 'PUF_post_tilt'

# PUF pre-tilt: the pre-tilt tibble is {id, 23 y_vars}. Attach its
# demographic context by id-joining to the post-tilt frame.
stopifnot(nrow(puf_pre) == nrow(puf_post))
puf_pre = puf_pre %>%
  inner_join(
    puf_post %>% select(id, weight, age_older, income_recon,
                        cell_income, cell_age),
    by = 'id'
  )
puf_pre = cbind(puf_pre, compute_category_values(puf_pre))
puf_pre$source_label = 'PUF_pre_tilt'


#--- Shared helpers ---------------------------------------------------------

# Weighted Gini via the "sum of |x_i - x_j|" identity.
weighted_gini = function(x, w) {
  o = order(x); x = x[o]; w = w[o]
  W = sum(w); mu = sum(w * x) / W
  cw = cumsum(w) - 0.5 * w          # mid-rank cumulative weight
  # 1 - 2 * integral of L(p), using the discrete analogue.
  num = sum(w * x * (2 * cw - W))
  num / (W^2 * mu)
}

weighted_quantile = function(x, w, probs) {
  o = order(x); x = x[o]; w = w[o]
  p = cumsum(w) / sum(w)
  approx(p, x, xout = probs, rule = 2)$y
}

# Top-share helpers: "top P% holds X% of aggregate Y".
top_share = function(x, w, top_frac) {
  cutoff = weighted_quantile(x, w, 1 - top_frac)
  above  = x >= cutoff
  sum(w[above] * x[above]) / sum(w * x)
}


#--- Table 1: aggregate totals per category ---------------------------------

cat('\n=== Table 1: aggregate dollar totals per category ===\n')

# Per-frame, per-category weighted sum. Explicit; avoids dplyr
# across()/all_of()/named-vector naming surprises.
sum_per_cat = function(df, label) {
  tibble(
    source_label  = label,
    category      = CALIB_CATEGORIES,
    total_dollars = vapply(unname(CAT_COL),
                           function(c) sum(df$weight * df[[c]]),
                           numeric(1))
  )
}

agg_totals = bind_rows(
  sum_per_cat(scf,      'SCF'),
  sum_per_cat(puf_pre,  'PUF_pre_tilt'),
  sum_per_cat(puf_post, 'PUF_post_tilt')
) %>%
  pivot_wider(names_from = source_label, values_from = total_dollars) %>%
  mutate(
    pre_vs_scf_pct  = 100 * (PUF_pre_tilt  - SCF) / SCF,
    post_vs_scf_pct = 100 * (PUF_post_tilt - SCF) / SCF
  )

print(agg_totals %>%
        mutate(across(c(SCF, PUF_pre_tilt, PUF_post_tilt),
                      ~ paste0('$', round(.x / 1e9, 1), 'B'))),
      n = Inf)


#--- Table 2: top wealth shares --------------------------------------------

cat('\n=== Table 2: top NW shares + Gini ===\n')

top_tab = bind_rows(lapply(list(SCF = scf, pre = puf_pre, post = puf_post),
                           function(df) {
  x = df$cat_nw; w = df$weight
  tibble(
    top_01 = top_share(x, w, 0.001),
    top_1  = top_share(x, w, 0.01),
    top_5  = top_share(x, w, 0.05),
    top_10 = top_share(x, w, 0.10),
    gini   = weighted_gini(x, w)
  )
}), .id = 'source_label')
print(top_tab %>% mutate(across(-source_label, ~ round(.x, 4))))


#--- Table 3: composition of NW by cell ------------------------------------

cat('\n=== Table 3: per-cell aggregates (SCF vs PUF_post) ===\n')

cell_agg = function(df) {
  out = df %>%
    group_by(cell_income, cell_age) %>%
    summarise(n_wt = sum(weight), .groups = 'drop')
  # Per-category aggregation done imperatively to avoid across()/all_of()
  # naming surprises.
  for (cat_name in CALIB_CATEGORIES) {
    c_col = CAT_COL[[cat_name]]  # e.g. "cat_nw"
    tot = df %>% group_by(cell_income, cell_age) %>%
      summarise(tot = sum(weight * .data[[c_col]]),
                pos = sum(weight * (.data[[c_col]] > 0)),
                .groups = 'drop')
    out[[paste0(c_col, '__total')]] = tot$tot[
      match(paste(out$cell_income, out$cell_age),
            paste(tot$cell_income, tot$cell_age))]
    out[[paste0(c_col, '__pos_n')]] = tot$pos[
      match(paste(out$cell_income, out$cell_age),
            paste(tot$cell_income, tot$cell_age))]
  }
  out
}
scf_c  = cell_agg(scf)
post_c = cell_agg(puf_post)
pre_c  = cell_agg(puf_pre)

make_gap = function(sc, pf, src_name) {
  sc_long = sc %>%
    pivot_longer(cols = starts_with('cat_'),
                 names_to  = c('category', 'metric'),
                 names_sep = '__',
                 values_to = 'scf_value') %>%
    mutate(category = sub('^cat_', '', category))
  pf_long = pf %>%
    pivot_longer(cols = starts_with('cat_'),
                 names_to  = c('category', 'metric'),
                 names_sep = '__',
                 values_to = 'puf_value') %>%
    mutate(category = sub('^cat_', '', category))
  sc_long %>%
    inner_join(pf_long, by = c('cell_income', 'cell_age',
                               'category', 'metric')) %>%
    mutate(diff_pct = 100 * (puf_value - scf_value) / pmax(abs(scf_value), 1),
           source   = src_name)
}
gap_post = make_gap(scf_c, post_c, 'post_tilt')
gap_pre  = make_gap(scf_c, pre_c,  'pre_tilt')

cat('\nPost-tilt worst 15 gaps (|pct| > 10):\n')
print(gap_post %>% filter(abs(diff_pct) > 10) %>%
        arrange(desc(abs(diff_pct))) %>%
        select(metric, category, cell_income, cell_age,
               scf_value, puf_value, diff_pct) %>%
        head(15))

cat('\nPre-tilt worst 15 gaps (|pct| > 10):\n')
print(gap_pre %>% filter(abs(diff_pct) > 10) %>%
        arrange(desc(abs(diff_pct))) %>%
        select(metric, category, cell_income, cell_age,
               scf_value, puf_value, diff_pct) %>%
        head(15))


#--- Table 4: homeownership rate by cell (policy-relevant) ------------------

cat('\n=== Table 4: homeownership rate by (cell_income × cell_age) ===\n')
homeown_tab = bind_rows(
  scf      %>% group_by(cell_income, cell_age) %>%
    summarise(homeown_rate = weighted.mean(cat_homes > 0, weight),
              .groups = 'drop') %>% mutate(source = 'SCF'),
  puf_pre  %>% group_by(cell_income, cell_age) %>%
    summarise(homeown_rate = weighted.mean(cat_homes > 0, weight),
              .groups = 'drop') %>% mutate(source = 'PUF_pre'),
  puf_post %>% group_by(cell_income, cell_age) %>%
    summarise(homeown_rate = weighted.mean(cat_homes > 0, weight),
              .groups = 'drop') %>% mutate(source = 'PUF_post')
) %>%
  pivot_wider(names_from = source, values_from = homeown_rate)
print(homeown_tab, n = Inf)


#--- Table 5: QC report summary --------------------------------------------

cat('\n=== Table 5: Stage 3 target QC ===\n')
summarize_qc(qc)

cat('\nKept targets by category × margin:\n')
kept_tab = qc %>% filter(status == 'keep') %>%
  count(category, margin) %>%
  pivot_wider(names_from = margin, values_from = n, values_fill = 0L)
print(kept_tab)


#--- Plots ------------------------------------------------------------------

vintage = format(Sys.time(), '%Y%m%d%H%M')
plot_dir = file.path('plots', 'wealth_diagnostics', vintage)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# NW CDF overlay (log-x on the positive side).
nw_cdf_df = bind_rows(
  scf      %>% transmute(source = 'SCF',           x = cat_nw, w = weight),
  puf_pre  %>% transmute(source = 'PUF_pre_tilt',  x = cat_nw, w = weight),
  puf_post %>% transmute(source = 'PUF_post_tilt', x = cat_nw, w = weight)
) %>%
  filter(x > 0) %>%
  group_by(source) %>%
  arrange(x) %>%
  mutate(cum_w = cumsum(w) / sum(w)) %>%
  ungroup()

p1 = ggplot(nw_cdf_df, aes(x = x, y = cum_w, color = source)) +
  geom_step() +
  scale_x_log10(labels = scales::label_dollar(scale = 1e-6, suffix = 'M')) +
  labs(title = 'Weighted CDF of positive NW',
       x = 'Net worth (log scale)', y = 'Cum. weighted fraction',
       color = NULL) +
  theme_minimal()
ggsave(file.path(plot_dir, 'nw_cdf.pdf'), p1, width = 8, height = 5)

# Aggregate total per category, 3-way bar.
p2 = agg_totals %>%
  pivot_longer(cols = c(SCF, PUF_pre_tilt, PUF_post_tilt),
               names_to = 'source', values_to = 'dollars') %>%
  ggplot(aes(x = category, y = dollars / 1e12,
             fill = source)) +
  geom_col(position = 'dodge') +
  labs(title = 'Aggregate $ per category',
       y = 'Total weighted $ (trillions)', x = NULL, fill = NULL) +
  theme_minimal()
ggsave(file.path(plot_dir, 'aggregate_totals.pdf'), p2, width = 9, height = 5)

# Homeownership rate by cell, SCF vs post.
p3 = homeown_tab %>%
  pivot_longer(cols = c(SCF, PUF_pre, PUF_post),
               names_to = 'source', values_to = 'rate') %>%
  ggplot(aes(x = cell_income, y = rate, color = source, group = source)) +
  geom_line() + geom_point() +
  facet_wrap(~ cell_age) +
  labs(title = 'Homeownership by cell',
       y = 'Weighted frac with primary_home > 0', x = NULL,
       color = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(plot_dir, 'homeownership_by_cell.pdf'),
       p3, width = 9, height = 5)


#--- Persist tables --------------------------------------------------------

out_tbl_dir = file.path(output_dir, 'wealth_diagnostics')
dir.create(out_tbl_dir, recursive = TRUE, showWarnings = FALSE)
write_rds(agg_totals,   file.path(out_tbl_dir, 'aggregate_totals.rds'))
write_rds(top_tab,      file.path(out_tbl_dir, 'top_shares.rds'))
write_rds(homeown_tab,  file.path(out_tbl_dir, 'homeownership.rds'))
write_rds(gap_post,     file.path(out_tbl_dir, 'cell_gap_post.rds'))
write_rds(gap_pre,      file.path(out_tbl_dir, 'cell_gap_pre.rds'))
write_csv(agg_totals,   file.path(out_tbl_dir, 'aggregate_totals.csv'))
write_csv(gap_post,     file.path(out_tbl_dir, 'cell_gap_post.csv'))

cat(sprintf('\nTables written to %s\n', out_tbl_dir))
cat(sprintf('Plots written to %s\n', plot_dir))
