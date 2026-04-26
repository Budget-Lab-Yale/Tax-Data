#---------------------------------------------
# dep_status_zero_diagnostic.R
#
# Hypothesis: PUF rows with dep_status == 1 are
# dependent returns (filed by someone claimed
# as dependent on another return). Their
# household wealth is conceptually on the
# parents' return. Imputing wealth to these
# rows double-counts at the macro level.
#
# Test: zero out imputed wealth for dep_status
# == 1 rows; see how much of the +$62T forest
# overshoot it accounts for.
#
# Reports:
#   (1) dep_status field anatomy: count, age,
#       cell distribution, filer cross-tab
#   (2) Imputed NW that lives in dep_status==1
#       rows (pre-tilt and post-tilt)
#   (3) Counterfactual aggregate after zeroing
#       (i)  pre-tilt → zero deps → Σ
#       (ii) post-tilt → zero deps → Σ
#   (4) Top shares + Gini before vs after dep
#       zeroing (using post-tilt as baseline)
#   (5) Per-cell decomposition: how much wealth
#       lives in dep rows by cell?
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: ... <output_dir>')
output_dir = args[1]

estimate_models = 0L  # use cached forest
do_lp           = 0L

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')


#--- Load --------------------------------------------------------------

cat('Loading scf_tax_units + puf_2022...\n')
scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')

snap_path = file.path(output_dir, 'puf_2022_snapshot.rds')
if (file.exists(snap_path)) {
  puf_2022 = read_rds(snap_path)
} else {
  puf_2022 = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                      show_col_types = FALSE)
  for (v in wealth_y_vars) puf_2022[[v]] = NA_real_
}

cat(sprintf('PUF: %d rows, pop %.1fM\n', nrow(puf_2022),
            sum(puf_2022$weight) / 1e6))


#--- (1) dep_status anatomy --------------------------------------------

cat('\n--- (1) dep_status anatomy ---\n')

cat('\ndep_status distribution:\n')
ds_dist = puf_2022 %>%
  group_by(dep_status) %>%
  summarise(n = n(), pop_M = round(sum(weight) / 1e6, 2), .groups = 'drop')
print(as.data.frame(ds_dist), row.names = FALSE)

cat('\nfiler × dep_status (pop M):\n')
fd_dist = puf_2022 %>%
  group_by(filer, dep_status) %>%
  summarise(pop_M = round(sum(weight) / 1e6, 2), .groups = 'drop')
print(as.data.frame(fd_dist), row.names = FALSE)

# Age distribution of dep_status==1
puf_2022 = puf_2022 %>%
  mutate(age_older = pmax(pmin(80L, age1),
                          pmin(80L, ifelse(is.na(age2), 0L, age2))))

cat('\nAge distribution of dep_status==1 rows:\n')
dep_age = puf_2022 %>% filter(dep_status == 1) %>%
  mutate(age_bin = cut(age_older, c(-1, 17, 24, 29, 39, 49, 64, 80),
                        labels = c('<18','18-24','25-29','30-39','40-49',
                                   '50-64','65+'))) %>%
  group_by(age_bin) %>%
  summarise(pop_M = round(sum(weight) / 1e6, 2), .groups = 'drop')
print(as.data.frame(dep_age), row.names = FALSE)


#--- Run wealth imputation (uses cached forests) -----------------------

source('src/imputations/wealth.R')
cat('\n=== Running run_wealth_imputation ===\n')
t0 = Sys.time()
set.seed(76)
result = run_wealth_imputation(puf_2022, scf_tax_units)
cat(sprintf('Total time %.1fs\n', as.numeric(Sys.time() - t0, units = 'secs')))

y_post = result$y          # post-Stage-3
y_pre  = result$y_pre_tilt # pre-Stage-3 (raw forest output)


#--- Build per-row PUF frame with cells + dep flag --------------------

puf_base = puf_2022 %>%
  select(id, weight, age1, age2, dep_status, filer,
         wages, sole_prop, farm,
         scorp_active, scorp_active_loss, scorp_179,
         scorp_passive, scorp_passive_loss,
         part_active, part_active_loss, part_179,
         part_passive, part_passive_loss,
         txbl_int, exempt_int, div_ord, div_pref,
         kg_lt, kg_st,
         gross_ss, gross_pens_dist, ui,
         rent, rent_loss, estate, estate_loss)
puf_age2 = ifelse(is.na(puf_base$age2), 0L, puf_base$age2)
puf_base$age_older = pmax(pmin(80L, puf_base$age1), pmin(80L, puf_age2))
puf_base$income = with(puf_base,
  wages + sole_prop + farm +
  scorp_active  - scorp_active_loss  - scorp_179 +
  scorp_passive - scorp_passive_loss +
  part_active   - part_active_loss   - part_179 +
  part_passive  - part_passive_loss +
  txbl_int + exempt_int + div_ord + div_pref +
  kg_lt + kg_st +
  gross_ss + gross_pens_dist + ui +
  rent - rent_loss + estate - estate_loss)
puf_base = assign_calibration_cells(puf_base, puf_base$income,
                                    puf_base$age_older, puf_base$weight)

attach_meta = function(df_y) {
  df_y %>% inner_join(puf_base %>% select(id, weight, dep_status, age_older,
                                           cell_income, cell_age),
                       by = 'id') %>%
    bind_cols(compute_category_values(.))
}
post = attach_meta(y_post)
pre  = attach_meta(y_pre)

# SCF for reference
scf_age2 = ifelse(is.na(scf_tax_units$age2), 0L, scf_tax_units$age2)
scf_ao   = pmax(pmin(80L, scf_tax_units$age1), pmin(80L, scf_age2))
scf_inc  = with(scf_tax_units,
  wages_scf + business_scf + int_div_scf + capital_gains_scf +
  rent_scf + ss_pens_scf + ui_other_scf)
scf_y    = scf_to_y(scf_tax_units)
scf      = data.frame(weight = scf_y$weight, age_older = scf_ao,
                       income = scf_inc)
scf      = cbind(scf, compute_category_values(scf_y))


#--- (2) Imputed NW in dep_status==1 rows ------------------------------

cat('\n--- (2) Imputed wealth living in dep_status==1 rows ---\n\n')

dep_pre  = pre  %>% filter(dep_status == 1)
nondep_pre = pre %>% filter(dep_status == 0)
dep_post = post %>% filter(dep_status == 1)

scf_total = sum(scf$weight * scf$cat_nw) / 1e12
pre_total  = sum(pre$weight  * pre$cat_nw)  / 1e12
post_total = sum(post$weight * post$cat_nw) / 1e12
dep_pre_total  = sum(dep_pre$weight  * dep_pre$cat_nw)  / 1e12
nondep_pre_total = sum(nondep_pre$weight * nondep_pre$cat_nw) / 1e12
dep_post_total = sum(dep_post$weight * dep_post$cat_nw) / 1e12

cat(sprintf('SCF truth                                 : $%6.2fT\n', scf_total))
cat(sprintf('PUF pre-tilt total NW (current)           : $%6.2fT\n', pre_total))
cat(sprintf('PUF post-tilt total NW (current)          : $%6.2fT\n', post_total))
cat(sprintf('  of which: dep_status==1 (post-tilt)     : $%6.2fT\n', dep_post_total))
cat(sprintf('  of which: dep_status==1 (pre-tilt)      : $%6.2fT\n', dep_pre_total))

cat(sprintf('\nIf we zero dep_status==1 BEFORE Stage 3   : $%.2fT (%.1f%% of $62T closed)\n',
            nondep_pre_total, 100 * (pre_total - nondep_pre_total) /
                                     (pre_total - scf_total)))
cat(sprintf('If we zero dep_status==1 AFTER Stage 3    : $%.2fT (delta from post: -%.2fT)\n',
            post_total - dep_post_total, dep_post_total))


#--- (3) Mean NW imputed to dep rows -----------------------------------

cat('\n--- Mean NW imputed to dep rows by cell × age ---\n')

dep_per_cell = pre %>%
  filter(dep_status == 1) %>%
  group_by(cell_income) %>%
  summarise(dep_pop_M = sum(weight) / 1e6,
            dep_total_T = sum(weight * cat_nw) / 1e12,
            mean_dep_NW = sum(weight * cat_nw) / sum(weight),
            .groups = 'drop')

all_per_cell = pre %>%
  group_by(cell_income) %>%
  summarise(all_pop_M = sum(weight) / 1e6,
            pre_total_T = sum(weight * cat_nw) / 1e12,
            .groups = 'drop')

merged = all_per_cell %>%
  left_join(dep_per_cell, by = 'cell_income') %>%
  mutate(dep_share_pop = dep_pop_M / all_pop_M,
         dep_share_NW = dep_total_T / pre_total_T) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(as.data.frame(merged), row.names = FALSE)


#--- (4) Top shares + Gini: post-tilt vs post-tilt-with-deps-zeroed ----

cat('\n--- (4) Top shares + Gini ---\n')
weighted_quantile = function(x, w, probs) {
  o = order(x); x = x[o]; w = w[o]
  p = cumsum(w) / sum(w); approx(p, x, xout = probs, rule = 2)$y
}
top_share = function(x, w, top_frac) {
  cutoff = weighted_quantile(x, w, 1 - top_frac)
  above  = x >= cutoff
  sum(w[above] * x[above]) / sum(w * x)
}
weighted_gini = function(x, w) {
  o = order(x); x = x[o]; w = w[o]; W = sum(w); mu = sum(w * x) / W
  cw = cumsum(w) - 0.5 * w
  sum(w * x * (2 * cw - W)) / (W^2 * mu)
}

# Variants:
#   - SCF
#   - pre (raw forest, no Stage 3)
#   - post (current Stage 3 rescale)
#   - pre + dep zero (zero dep_status==1 from pre, no Stage 3)
#   - post + dep zero (zero dep_status==1 from post, after Stage 3)
zero_deps = function(d) d %>%
  mutate(across(starts_with('cat_'), ~ if_else(dep_status == 1, 0, .)))

pre_z  = zero_deps(pre)
post_z = zero_deps(post)

tb = function(df, lab) tibble(
  source = lab,
  agg_T  = round(sum(df$weight * df$cat_nw) / 1e12, 2),
  top_01 = round(top_share(df$cat_nw, df$weight, 0.001), 4),
  top_1  = round(top_share(df$cat_nw, df$weight, 0.01),  4),
  top_5  = round(top_share(df$cat_nw, df$weight, 0.05),  4),
  top_10 = round(top_share(df$cat_nw, df$weight, 0.10),  4),
  gini   = round(weighted_gini(df$cat_nw, df$weight),    4)
)

print(bind_rows(
  tb(scf,    'SCF'),
  tb(pre,    'pre  (no Stage 3)'),
  tb(post,   'post (Stage 3)'),
  tb(pre_z,  'pre + zero deps'),
  tb(post_z, 'post + zero deps')
))


#--- (5) Income-pctile share table ------------------------------------

cat('\n--- (5) Share of aggregate NW by income pctile ---\n')

income_bins   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
income_labels = c('p0-20','p20-40','p40-60','p60-80',
                  'p80-90','p90-99','p99-99.9','top0.1%')

inc_shares = function(df, lab) {
  pctile = compute_percentile(df$income %||% 0, df$weight)
  if (is.null(df$income)) {
    # SCF doesn't carry income column directly here; use scf_inc placeholder
    pctile = compute_percentile(scf_inc, df$weight)
  }
  bin = cut(pctile, breaks = income_bins, labels = income_labels,
            include.lowest = TRUE, right = FALSE)
  df %>% mutate(bin = bin) %>%
    group_by(bin) %>%
    summarise(nw_total = sum(weight * cat_nw), .groups = 'drop') %>%
    mutate(share = nw_total / sum(nw_total), source = lab)
}

# Need to attach income to all frames consistently
pre$income = puf_base$income[match(pre$id, puf_base$id)]
post$income = puf_base$income[match(post$id, puf_base$id)]
pre_z$income = pre$income
post_z$income = post$income
scf$income = scf_inc

inc_share_tbl = bind_rows(
  inc_shares(scf,    'SCF'),
  inc_shares(post,   'post (Stage 3)'),
  inc_shares(post_z, 'post + zero deps')
) %>%
  select(bin, source, share) %>%
  pivot_wider(names_from = source, values_from = share) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(as.data.frame(inc_share_tbl), row.names = FALSE)


cat('\nDone.\n')
