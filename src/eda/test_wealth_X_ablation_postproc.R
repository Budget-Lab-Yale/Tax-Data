#--------------------------------------
# test_wealth_X_ablation_postproc.R
#
# Consumes the three per-spec caches
#   resources/cache/wealth_ablation_{small,medium,large}.rds
# produced by src/eda/test_wealth_X_ablation.R
# and builds the diagnostic outputs:
#
#   (1) per-category aggregate match,
#       3 specs × 23 vars vs SCF
#   (2) cut-wise NW/A/D summary by
#       income-pctile group, age bucket,
#       filing status, and composition
#       flags, 3 specs + SCF truth
#   (3) conditional-on-X sanity — for
#       each of 6 cells, compare the
#       imputed-PUF NW distribution
#       under each spec against SCF-
#       native NW in the same cell
#   (4) donor-leaf diagnostic
#
# Re-runs the pipeline to rebuild
# tax_units + scf_tax_units (fast: LP
# and stage1 are cached). Feature
# engineering mirrors the worker exactly
# so id order and cell definitions match
# the donor draws in the per-spec caches.
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)
source('./src/configure.R')
estimate_models = 1
do_lp = 0
set.seed(1337)

source('./src/process_targets.R')
source('./src/process_puf.R')
source('./src/reweight.R')
source('./src/summary.R')
source('./src/create_2017_puf.R')
source('./src/impute_nonfilers.R')
source('./src/imputations/helpers.R')
source('./src/imputations/demographics.R')
source('./src/imputations/ages.R')
source('./src/imputations/stage1_scf_tax_units.R')

#---------------------------------------------------------------------------
# Y schema + scf_to_y (same as worker).
#---------------------------------------------------------------------------

wealth_asset_vars = c(
  'cash', 'equities', 'bonds', 'retirement', 'life_ins', 'annuities',
  'trusts', 'other_fin', 'pass_throughs', 'primary_home', 'other_home',
  're_fund', 'other_nonfin'
)
wealth_debt_vars = c(
  'primary_mortgage', 'other_mortgage', 'credit_lines',
  'credit_cards', 'installment_debt', 'other_debt'
)
wealth_kg_vars = c(
  'kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other'
)
wealth_y_vars = c(wealth_asset_vars, wealth_debt_vars, wealth_kg_vars)

scf_to_y = function(df) {
  if (all(wealth_y_vars %in% names(df))) return(df)
  df %>% mutate(
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
    other_debt       = ODEBT,
    kg_primary_home  = KGHOUSE,
    kg_other_re      = KGORE,
    kg_pass_throughs = KGBUS,
    kg_other         = KGSTMF
  )
}

make_has = function(x) {
  case_when(x >  0 ~ 1L,
            x == 0 ~ 0L,
            TRUE   ~ -1L)
}

#---------------------------------------------------------------------------
# Feature engineering — mirrors worker. Used for cell selection and the
# SCF-native cut-wise comparison. (DRF-side X features are already baked
# into the donor draws from the worker; we don't retrain anything here.)
#---------------------------------------------------------------------------

scf_tax_units = scf_tax_units %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
    # Unfloor SCF income (stage1 inherits SCFP INCOME floor at 0).
    income = wages_scf + business_scf + int_div_scf + capital_gains_scf +
             rent_scf + ss_pens_scf + ui_other_scf,
    pctile_income = compute_percentile(income, weight),
    has_business      = make_has(business_scf),
    has_capital_gains = make_has(capital_gains_scf),
    has_rent          = make_has(rent_scf),
    has_ss_pens       = make_has(ss_pens_scf)
  )

tax_units = tax_units %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
    married = as.integer(!is.na(male2)),
    # Matches worker's income expr (mirrors SCFP INCOME bundling).
    income  = wages +
              sole_prop + farm +
              scorp_active - scorp_active_loss - scorp_179 +
              scorp_passive - scorp_passive_loss +
              part_active - part_active_loss - part_179 +
              part_passive - part_passive_loss +
              txbl_int + exempt_int + div_ord + div_pref +
              kg_lt + kg_st +
              gross_ss + gross_pens_dist +
              ui +
              rent - rent_loss + estate - estate_loss,
    pctile_income = compute_percentile(income, weight),
    wages_puf         = wages,
    business_puf      = sole_prop + farm +
                        scorp_active  - scorp_active_loss  - scorp_179 +
                        scorp_passive - scorp_passive_loss +
                        part_active   - part_active_loss   - part_179 +
                        part_passive  - part_passive_loss,
    int_div_puf       = txbl_int + exempt_int + div_ord + div_pref,
    capital_gains_puf = kg_lt + kg_st,
    rent_puf          = rent - rent_loss + estate - estate_loss,
    ss_pens_puf       = gross_ss + gross_pens_dist,
    ui_other_puf      = ui,
    has_business      = make_has(business_puf),
    has_capital_gains = make_has(capital_gains_puf),
    has_rent          = make_has(rent_puf),
    has_ss_pens       = make_has(ss_pens_puf)
  )

#---------------------------------------------------------------------------
# Load per-spec caches and verify they agree on id order + knobs.
# If any are missing we drop them from the analysis rather than failing
# outright — lets us re-run when adding a new spec without rebuilding
# everything.
#---------------------------------------------------------------------------

all_specs = c('small', 'medium', 'large')
spec_paths = sprintf('resources/cache/wealth_ablation_%s.rds', all_specs)
present = file.exists(spec_paths)
if (!any(present)) stop('No spec artifacts found; cannot run postproc.')
if (!all(present))
  message('Skipping missing specs: ',
          paste(all_specs[!present], collapse = ', '))
spec_names = all_specs[present]
spec_paths = spec_paths[present]

spec_artifacts = lapply(spec_paths, read_rds)
names(spec_artifacts) = spec_names

# Sanity: id vectors should be identical (each worker ran the same
# deterministic pipeline, so tax_units$id order must match).
id_ref = spec_artifacts[[1]]$id
for (nm in spec_names[-1]) {
  if (!identical(id_ref, spec_artifacts[[nm]]$id))
    stop('id order mismatch between spec ', spec_names[1], ' and ', nm,
         '. Did the pipeline run deterministically across the 3 jobs?')
}
if (!identical(id_ref, tax_units$id))
  stop('id order mismatch between spec artifacts and current tax_units. ',
       'The postproc pipeline run diverged from the worker runs.')

# Knobs should agree across specs.
knobs_ref = spec_artifacts[[1]]$knobs
for (nm in spec_names[-1]) {
  k = spec_artifacts[[nm]]$knobs
  if (!identical(k$num.trees, knobs_ref$num.trees) ||
      !identical(k$min.node.size, knobs_ref$min.node.size) ||
      !identical(k$honesty, knobs_ref$honesty) ||
      !identical(k$n_boot, knobs_ref$n_boot) ||
      !identical(k$seed, knobs_ref$seed))
    stop('Knobs differ between spec ', spec_names[1], ' and ', nm)
}

cat(sprintf('\nLoaded %d spec artifacts. Shared knobs:\n', length(spec_names)))
print(knobs_ref)
cat(sprintf('\nper-spec fit times: %s\n',
            paste(sapply(spec_names, function(n)
                         sprintf('%s=%.1fmin', n, spec_artifacts[[n]]$fit_minutes)),
                  collapse = ', ')))

#---------------------------------------------------------------------------
# Build per-spec PUF frame (id + demographics + composition + donor Y +
# totals). Join by position since ids agree.
#---------------------------------------------------------------------------

puf_context = tax_units %>%
  transmute(id, weight, age_older, age_younger, filing_status, married, n_dep,
            income, pctile_income,
            has_business, has_capital_gains, has_ss_pens, has_rent)

build_puf_frame = function(nm) {
  a = spec_artifacts[[nm]]
  bind_cols(puf_context, as_tibble(a$donor_y)) %>%
    mutate(
      total_assets = rowSums(across(all_of(wealth_asset_vars))),
      total_debts  = rowSums(across(all_of(wealth_debt_vars))),
      total_kg     = rowSums(across(all_of(wealth_kg_vars))),
      net_worth    = total_assets - total_debts,
      spec         = nm
    )
}

puf_by_spec = lapply(spec_names, build_puf_frame)
names(puf_by_spec) = spec_names

scf_native = scf_to_y(scf_tax_units) %>%
  mutate(
    total_assets = rowSums(across(all_of(wealth_asset_vars))),
    total_debts  = rowSums(across(all_of(wealth_debt_vars))),
    total_kg     = rowSums(across(all_of(wealth_kg_vars))),
    net_worth    = total_assets - total_debts
  )

#---------------------------------------------------------------------------
# (1) Aggregate match: per-category weighted totals, 3 specs + SCF.
#---------------------------------------------------------------------------

scf_agg = tibble(
  variable = wealth_y_vars,
  scf_bn = sapply(wealth_y_vars,
                  function(v) sum(scf_native[[v]] * scf_native$weight)) / 1e9
)

agg_long = map_dfr(spec_names, function(nm) {
  d = puf_by_spec[[nm]]
  tibble(
    spec     = nm,
    variable = wealth_y_vars,
    puf_bn   = sapply(wealth_y_vars,
                      function(v) sum(d[[v]] * d$weight)) / 1e9
  )
}) %>%
  left_join(scf_agg, by = 'variable') %>%
  mutate(rel_gap = puf_bn / scf_bn - 1,
         variable = factor(variable, levels = wealth_y_vars),
         spec     = factor(spec,     levels = spec_names))

cat('\n==========================================================\n')
cat('  (1) Per-category aggregates, imputed PUF vs SCF ($B)\n')
cat('==========================================================\n\n')
print(agg_long %>%
        select(variable, spec, puf_bn, scf_bn, rel_gap) %>%
        arrange(variable, spec),
      n = 3 * length(wealth_y_vars))

#---------------------------------------------------------------------------
# (2) Cut-wise NW/A/D summary — 3 specs + SCF truth.
#---------------------------------------------------------------------------

pctile_groups = tibble(
  group = factor(c('P1-P25','P25-P50','P50-P75','P75-P90','P90-P99','P99+'),
                 levels = c('P1-P25','P25-P50','P50-P75','P75-P90','P90-P99','P99+')),
  lo    = c(1, 25, 50, 75, 90, 99),
  hi    = c(25, 50, 75, 90, 99, 100)
)

assign_pctile_group = function(p) {
  sapply(p, function(x) {
    for (j in seq_len(nrow(pctile_groups))) {
      if (x > pctile_groups$lo[j] && x <= pctile_groups$hi[j])
        return(as.character(pctile_groups$group[j]))
    }
    NA_character_
  })
}

age_buckets = function(a) cut(a, breaks = c(-Inf, 35, 50, 65, 80, Inf),
                              labels = c('<35','35-49','50-64','65-79','80+'))

cut_summary = function(d, cut_var, cut_name) {
  d %>%
    mutate(cut_value = as.character(.data[[cut_var]])) %>%
    group_by(cut_value) %>%
    summarise(
      n        = n(),
      w_sum    = sum(weight),
      med_NW   = Hmisc::wtd.quantile(net_worth,    weight, 0.5),
      mean_NW  = weighted.mean(net_worth,          weight),
      p90_NW   = Hmisc::wtd.quantile(net_worth,    weight, 0.9),
      med_A    = Hmisc::wtd.quantile(total_assets, weight, 0.5),
      med_D    = Hmisc::wtd.quantile(total_debts,  weight, 0.5),
      .groups  = 'drop'
    ) %>%
    mutate(cut = cut_name)
}

cut_tbl = map_dfr(spec_names, function(nm) {
  d = puf_by_spec[[nm]] %>%
    mutate(pctile_group = assign_pctile_group(pctile_income),
           age_bucket   = as.character(age_buckets(age_older)),
           fs_label     = as.character(filing_status),
           biz_flag     = ifelse(has_business == 1L, 'biz', 'no_biz'),
           capg_flag    = ifelse(has_capital_gains == 1L, 'capg', 'no_capg'))

  bind_rows(
    cut_summary(d, 'pctile_group', 'income pctile group'),
    cut_summary(d, 'age_bucket',   'age bucket'),
    cut_summary(d, 'fs_label',     'filing status'),
    cut_summary(d, 'biz_flag',     'has business flag'),
    cut_summary(d, 'capg_flag',    'has capital gains flag')
  ) %>% mutate(spec = nm)
})

scf_cut_tbl = {
  d = scf_native %>%
    mutate(pctile_group = assign_pctile_group(pctile_income),
           age_bucket   = as.character(age_buckets(age_older)),
           fs_label     = as.character(filestat),
           biz_flag     = ifelse(has_business == 1L, 'biz', 'no_biz'),
           capg_flag    = ifelse(has_capital_gains == 1L, 'capg', 'no_capg'))
  bind_rows(
    cut_summary(d, 'pctile_group', 'income pctile group'),
    cut_summary(d, 'age_bucket',   'age bucket'),
    cut_summary(d, 'fs_label',     'filing status'),
    cut_summary(d, 'biz_flag',     'has business flag'),
    cut_summary(d, 'capg_flag',    'has capital gains flag')
  ) %>% mutate(spec = 'SCF (truth)')
}

cut_all = bind_rows(cut_tbl, scf_cut_tbl)

cat('\n\n==========================================================\n')
cat('  (2) NW / A / D cut-wise summary  (3 PUF specs + SCF truth)\n')
cat('==========================================================\n\n')
print(cut_all %>% arrange(cut, cut_value, spec), n = 200)

#---------------------------------------------------------------------------
# (3) Conditional-on-X sanity.
#---------------------------------------------------------------------------

cell_defs = list(
  A_topinc_midage_married = function(d) with(d, pctile_income >= 80 &
                                                married == 1 &
                                                age_older >= 45 & age_older <= 64),
  B_A_plus_biz            = function(d) with(d, pctile_income >= 80 &
                                                married == 1 &
                                                age_older >= 45 & age_older <= 64 &
                                                has_business == 1),
  C_A_plus_capg           = function(d) with(d, pctile_income >= 80 &
                                                married == 1 &
                                                age_older >= 45 & age_older <= 64 &
                                                has_capital_gains == 1),
  D_age65p_topinc_sspens  = function(d) with(d, age_older >= 65 &
                                                pctile_income >= 80 &
                                                has_ss_pens == 1),
  E_botinc_single_young   = function(d) with(d, pctile_income <= 50 &
                                                married == 0 &
                                                age_older < 35),
  F_top1pct               = function(d) with(d, pctile_income == 100)
)

wtd_ks_D = function(x1, w1, x2, w2, n_grid = 501) {
  grid = quantile(c(x1, x2), probs = seq(0, 1, length.out = n_grid),
                  na.rm = TRUE)
  cdf1 = sapply(grid, function(g) sum(w1[x1 <= g]) / sum(w1))
  cdf2 = sapply(grid, function(g) sum(w2[x2 <= g]) / sum(w2))
  max(abs(cdf1 - cdf2))
}

cond_tbl = map_dfr(names(cell_defs), function(cell_nm) {
  scf_sel = scf_native[cell_defs[[cell_nm]](scf_native), ]
  scf_nw  = scf_sel$net_worth; scf_w = scf_sel$weight

  spec_rows = map_dfr(spec_names, function(nm) {
    d = puf_by_spec[[nm]]
    puf_sel = d[cell_defs[[cell_nm]](d), ]
    if (nrow(puf_sel) == 0) return(tibble())
    tibble(
      cell    = cell_nm, spec = nm,
      n_puf   = nrow(puf_sel),
      w_puf   = sum(puf_sel$weight),
      p25_NW  = Hmisc::wtd.quantile(puf_sel$net_worth, puf_sel$weight, 0.25),
      p50_NW  = Hmisc::wtd.quantile(puf_sel$net_worth, puf_sel$weight, 0.50),
      p75_NW  = Hmisc::wtd.quantile(puf_sel$net_worth, puf_sel$weight, 0.75),
      ks_D    = if (nrow(scf_sel) >= 5)
                  wtd_ks_D(puf_sel$net_worth, puf_sel$weight, scf_nw, scf_w)
                else NA_real_
    )
  })

  truth_row = tibble(
    cell    = cell_nm, spec = 'SCF (truth)',
    n_puf   = nrow(scf_sel),
    w_puf   = sum(scf_sel$weight),
    p25_NW  = if (nrow(scf_sel) > 0)
                Hmisc::wtd.quantile(scf_sel$net_worth, scf_sel$weight, 0.25)
              else NA_real_,
    p50_NW  = if (nrow(scf_sel) > 0)
                Hmisc::wtd.quantile(scf_sel$net_worth, scf_sel$weight, 0.50)
              else NA_real_,
    p75_NW  = if (nrow(scf_sel) > 0)
                Hmisc::wtd.quantile(scf_sel$net_worth, scf_sel$weight, 0.75)
              else NA_real_,
    ks_D    = NA_real_
  )

  bind_rows(spec_rows, truth_row)
})

cat('\n\n==========================================================\n')
cat('  (3) Conditional-on-X sanity (NW distribution within cell)\n')
cat('      KS D compares PUF(spec) to SCF(truth) within the cell.\n')
cat('==========================================================\n\n')
print(cond_tbl %>% arrange(cell, spec), n = 6 * (length(spec_names) + 1))

#---------------------------------------------------------------------------
# (4) Donor-leaf diagnostic.
#---------------------------------------------------------------------------

leaf_tbl = map_dfr(spec_names, function(nm) {
  ls_ = spec_artifacts[[nm]]$leaf_size
  tibble(
    spec   = nm,
    n_feats = spec_artifacts[[nm]]$n_feats,
    min_ls = min(ls_),
    p10    = as.integer(quantile(ls_, 0.10)),
    p50    = as.integer(quantile(ls_, 0.50)),
    p90    = as.integer(quantile(ls_, 0.90)),
    max_ls = max(ls_),
    mean_ls = mean(ls_)
  )
})

cat('\n\n==========================================================\n')
cat('  (4) Donor-leaf size diagnostic\n')
cat('==========================================================\n\n')
print(leaf_tbl)

#---------------------------------------------------------------------------
# Persist.
#---------------------------------------------------------------------------

out_dir = 'plots/wealth_ablation'
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_rds(
  list(
    spec_names    = spec_names,
    knobs         = knobs_ref,
    feats_by_spec = lapply(spec_artifacts, `[[`, 'feats'),
    agg_long      = agg_long,
    cut_all       = cut_all,
    cond_tbl      = cond_tbl,
    leaf_tbl      = leaf_tbl,
    puf_by_spec   = lapply(puf_by_spec, function(d)
                           d %>% select(id, weight, age_older, age_younger, filing_status,
                                        married, n_dep, income, pctile_income,
                                        has_business, has_capital_gains,
                                        has_ss_pens, has_rent,
                                        all_of(wealth_y_vars),
                                        total_assets, total_debts,
                                        total_kg, net_worth, spec)),
    leaf_size_by_spec = lapply(spec_artifacts, `[[`, 'leaf_size')
  ),
  'resources/cache/wealth_ablation.rds'
)

write_csv(agg_long, file.path(out_dir, 'agg_long.csv'))
write_csv(cut_all,  file.path(out_dir, 'cut_all.csv'))
write_csv(cond_tbl, file.path(out_dir, 'cond_tbl.csv'))
write_csv(leaf_tbl, file.path(out_dir, 'leaf_tbl.csv'))

#---------------------------------------------------------------------------
# Plots.
#---------------------------------------------------------------------------

p_agg = agg_long %>%
  ggplot(aes(variable, 100 * rel_gap, fill = spec)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_hline(yintercept = 0, color = 'grey40') +
  scale_fill_brewer(palette = 'Set1') +
  coord_flip() +
  labs(title    = 'Per-category aggregate gap: PUF vs SCF, by X spec',
       subtitle = 'Lower |%| is better. 0% = perfect match.',
       x = NULL, y = 'PUF / SCF − 1 (%)') +
  theme_minimal(base_size = 10)

ggsave(file.path(out_dir, '01_aggregate_gap.png'),
       p_agg, width = 10, height = 10, dpi = 140)

nw_by_p_puf = map_dfr(spec_names, function(nm) {
  puf_by_spec[[nm]] %>%
    group_by(pctile = pctile_income) %>%
    summarise(med_NW = Hmisc::wtd.quantile(net_worth, weight, 0.5),
              .groups = 'drop') %>%
    mutate(source = nm)
})
nw_by_p_scf = scf_native %>%
  group_by(pctile = pctile_income) %>%
  summarise(med_NW = Hmisc::wtd.quantile(net_worth, weight, 0.5),
            .groups = 'drop') %>%
  mutate(source = 'SCF (truth)')
nw_by_p = bind_rows(nw_by_p_puf, nw_by_p_scf)

p_nw = nw_by_p %>%
  ggplot(aes(pctile, med_NW / 1000, color = source, linetype = source)) +
  geom_line(linewidth = 0.9) +
  # pseudo-log on y: ~linear through ±$10K, log in the tails. NW can be
  # negative at the bottom of the income distribution, so plain log fails.
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 10, base = 10),
                     breaks = c(-100, -10, 0, 10, 100, 1000, 10000),
                     labels = scales::label_comma()) +
  scale_color_manual(values = c(small = '#4daf4a', medium = '#377eb8', large_binv2 = '#984ea3',
                                large = '#e41a1c', `SCF (truth)` = 'black')) +
  scale_linetype_manual(values = c(small = 'solid', medium = 'solid', large_binv2 = 'dotdash',
                                    large = 'solid',
                                    `SCF (truth)` = 'dashed')) +
  labs(title = 'Median net worth by income percentile, by X spec',
       x = 'Income percentile',
       y = 'Median NW ($ thousands, pseudo-log)') +
  theme_minimal(base_size = 10)

ggsave(file.path(out_dir, '02_nw_by_pctile.png'),
       p_nw, width = 9, height = 5, dpi = 140)

#---------------------------------------------------------------------------
# NW CDFs: overall, per income decile, top 1%, top 0.1%.
# Replaces the ad-hoc conditional cells — gives a consistent view of
# distribution match at the slices the user actually cares about for
# policy analysis (aggregate, by-decile, and extreme top tail).
#---------------------------------------------------------------------------

# Helper: emit weighted CDF points for a frame's net_worth
cdf_points = function(nw, w, tag, group_label) {
  if (length(nw) == 0) return(tibble())
  o = order(nw)
  tibble(group = group_label, source = tag,
         nw = nw[o],
         cdf = cumsum(w[o]) / sum(w[o]))
}

# Top-fraction helpers (use PUF income on each side to define "top X% income")
top_frac_cutoffs = c(top1 = 0.01, top01 = 0.001)

cdf_groups = c(
  'overall',
  sprintf('dec %d', 1:10),
  'top 1% income',
  'top 0.1% income'
)
cdf_groups = factor(cdf_groups, levels = cdf_groups)

build_cdfs_for = function(d, tag) {
  # overall
  overall = cdf_points(d$net_worth, d$weight, tag, 'overall')
  # per decile
  dec_inc = {
    pos = d$income > 0
    brks = sort(wtd.quantile(d$income[pos], d$weight[pos],
                             probs = seq(0, 1, 0.1)))
    bin = findInterval(d$income, brks, all.inside = TRUE)
    ifelse(pos, bin, 0L)
  }
  per_dec = map_dfr(1:10, function(dd) {
    sel = dec_inc == dd
    cdf_points(d$net_worth[sel], d$weight[sel], tag, sprintf('dec %d', dd))
  })
  # top 1%, top 0.1% by income
  top_cuts = map_dfr(names(top_frac_cutoffs), function(nm) {
    tf   = top_frac_cutoffs[[nm]]
    thr  = wtd.quantile(d$income, d$weight, 1 - tf)
    sel  = d$income > thr
    label = if (nm == 'top1') 'top 1% income' else 'top 0.1% income'
    cdf_points(d$net_worth[sel], d$weight[sel], tag, label)
  })
  bind_rows(overall, per_dec, top_cuts)
}

nw_cdf_df = bind_rows(
  map_dfr(spec_names, function(nm) build_cdfs_for(puf_by_spec[[nm]], nm)),
  build_cdfs_for(scf_native, 'SCF (truth)')
) %>%
  mutate(group  = factor(group, levels = levels(cdf_groups)),
         source = factor(source, levels = c(spec_names, 'SCF (truth)')))

p_cdfs = nw_cdf_df %>%
  ggplot(aes(nw / 1000, cdf, color = source, linetype = source)) +
  geom_step(linewidth = 0.6) +
  facet_wrap(~ group, ncol = 4, scales = 'free_x') +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = 10, base = 10),
                     breaks = c(-1000, -100, 0, 100, 1000, 10000, 100000),
                     labels = scales::label_comma()) +
  scale_color_manual(values = c(small = '#4daf4a', medium = '#377eb8',
                                large = '#e41a1c',
                                `SCF (truth)` = 'black')) +
  scale_linetype_manual(values = c(small = 'solid', medium = 'solid',
                                    large = 'solid',
                                    `SCF (truth)` = 'dashed')) +
  labs(title = 'Net-worth CDF: overall, by income decile, top 1%, top 0.1%',
       subtitle = 'PUF specs (solid) vs SCF (dashed). Pseudo-log x.',
       x = 'Net worth ($ thousands, pseudo-log)', y = 'CDF') +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = 'bold'),
        legend.position = 'bottom')

ggsave(file.path(out_dir, '03_nw_cdfs.png'),
       p_cdfs, width = 13, height = 10, dpi = 140)

leaf_long = map_dfr(spec_names,
                    ~ tibble(spec = .x, leaf_size = spec_artifacts[[.x]]$leaf_size))

p_leaf = leaf_long %>%
  ggplot(aes(leaf_size, fill = spec)) +
  geom_histogram(position = 'identity', alpha = 0.6, bins = 60) +
  scale_x_log10() +
  scale_fill_brewer(palette = 'Set1') +
  facet_wrap(~ spec, ncol = 1, scales = 'free_y') +
  labs(title = 'Donor-leaf size distribution at predict time, by spec',
       subtitle = 'Thinner leaves (left tail) under larger X.',
       x = 'Leaf size (log scale)', y = 'PUF rows') +
  theme_minimal(base_size = 10)

ggsave(file.path(out_dir, '04_leaf_size.png'),
       p_leaf, width = 9, height = 8, dpi = 140)

cat(sprintf('\nPlots written to %s/\n', out_dir))
cat('  01_aggregate_gap.png      — per-category $ aggregate gap, by spec\n')
cat('  02_nw_by_pctile.png       — median NW by income pctile, by spec + truth\n')
cat('  03_nw_cdfs.png            — NW CDFs: overall, deciles, top 1%, top 0.1%\n')
cat('  04_leaf_size.png          — donor-leaf-size distributions, by spec\n')
cat('\nDone.\n')
