#--------------------------------------
# wealth_decomp.R
#
# Oaxaca-Blinder-style decomposition of
# the aggregate wealth gap between the
# imputed PUF and the SCF tax units.
#
# The DRF samples donors conditional on
# X, so E[Y|X] agrees between SCF and
# imputed PUF up to MC noise. Any
# systematic gap in the unconditional
# aggregate therefore has to come from
# the X distribution differing between
# datasets.
#
# Layout of the report:
#   1. Y|X sanity check (cell-wise mean
#      NW on SCF vs imputed PUF; should
#      match up to noise)
#   2. Marginal X distributions
#   3. Aggregate gap by asset bucket
#      (assets/debts/KG) with X and
#      Y|X sums
#   4. Cell-level decomposition of the
#      aggregate NW gap: which X cells
#      contribute most
#   5. Per-category decomposition for
#      each of the 23 Y vars, sorted by
#      |gap$B|
#   6. Within-cell X-covariate comparison
#      for top Y|X cells — the mechanism
#      behind the Y|X residual
#   7. Tail diagnostic (P50/P90/P99) for
#      top Y|X cells — outlier-driven?
#
# Inputs:
#   resources/cache/scf_tax_units.rds
#   resources/cache/wealth_analysis.rds
#   resources/cache/consumption_analysis.rds
#     (for PUF income components used
#      in section 6 mechanism diagnostic)
#
# Outputs:
#   plots/wealth_decomp/decomp_report.txt
#   plots/wealth_decomp/*.png
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)

report_path = 'plots/wealth_decomp/decomp_report.txt'
plot_dir    = 'plots/wealth_decomp'
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

#---------------------------------------------------------------------------
# Load
#---------------------------------------------------------------------------

scf_path = 'resources/cache/scf_tax_units.rds'
puf_path = 'resources/cache/wealth_analysis.rds'
# PUF raw income components (wages, business, int_div, capital_gains, ...)
# aren't in wealth_analysis.rds. Join them from consumption_analysis.rds —
# which caches the full tax_units frame from the pre-consumption-imputation
# pipeline — so the within-cell X diagnostic in section 6 can compare
# PUF/SCF covariate mixes without rerunning the pipeline.
cons_path = 'resources/cache/consumption_analysis.rds'
if (!file.exists(scf_path))
  stop('SCF cache missing: ', scf_path, ' (run src/eda/test_stage1.R)')
if (!file.exists(puf_path))
  stop('PUF cache missing: ', puf_path, ' (run src/eda/test_wealth.R)')
if (!file.exists(cons_path))
  stop('Consumption cache missing: ', cons_path,
       ' (section 6 mechanism diagnostic needs raw PUF income components)')

scf = read_rds(scf_path)
puf = read_rds(puf_path)
puf_cons = read_rds(cons_path)

sink(report_path, split = TRUE)
cat('Wealth imputation decomposition — X-effect vs Y|X\n')
cat('Generated: ', as.character(Sys.time()), '\n', sep = '')
cat(paste0(rep('=', 72), collapse = ''), '\n\n')

#---------------------------------------------------------------------------
# Harmonize schemas
#---------------------------------------------------------------------------

asset_vars = c('cash', 'equities', 'bonds', 'retirement', 'life_ins',
               'annuities', 'trusts', 'other_fin', 'pass_throughs',
               'primary_home', 'other_home', 're_fund', 'other_nonfin')
debt_vars  = c('primary_mortgage', 'other_mortgage', 'credit_lines',
               'credit_cards', 'installment_debt', 'other_debt')
kg_vars    = c('kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other')
y_vars     = c(asset_vars, debt_vars, kg_vars)

scf_h = scf %>%
  transmute(
    weight, age1, income, married,
    n_dep = as.integer(n_dep),
    male1 = as.integer(male1),
    # Income components for the within-cell mechanism diagnostic.
    business_comp      = business_scf,
    int_div_comp       = int_div_scf,
    capital_gains_comp = capital_gains_scf,
    ss_pens_comp       = ss_pens_scf,
    !!!setNames(lapply(y_vars, function(v) rlang::sym(v)), y_vars),
    total_assets = !!rlang::parse_expr(paste(asset_vars, collapse = ' + ')),
    total_debts  = !!rlang::parse_expr(paste(debt_vars,  collapse = ' + ')),
    net_worth    = total_assets - total_debts,
    source = 'SCF'
  )

# PUF income components from consumption_analysis cache. This frame is
# filtered to income > 0 & C > 0, so only income-positive PUF units get
# components attached. That's fine for section 6, which targets top
# Y|X-residual cells — those live in mid-to-high income quintiles where
# puf_cons covers the universe.
puf_x = puf_cons %>%
  transmute(
    id,
    business_comp      = sole_prop + farm +
                         scorp_active  - scorp_active_loss  - scorp_179 +
                         scorp_passive - scorp_passive_loss +
                         part_active   - part_active_loss   - part_179 +
                         part_passive  - part_passive_loss,
    int_div_comp       = txbl_int + exempt_int + div_ord + div_pref,
    capital_gains_comp = kg_lt + kg_st,
    ss_pens_comp       = gross_ss + gross_pens_dist
  )

puf_h = puf %>%
  left_join(puf_x, by = 'id') %>%
  transmute(
    weight, age1, income, married = as.integer(married),
    n_dep = as.integer(n_dep), male1 = as.integer(male1),
    business_comp, int_div_comp, capital_gains_comp, ss_pens_comp,
    !!!setNames(lapply(y_vars, function(v) rlang::sym(v)), y_vars),
    total_assets, total_debts, net_worth,
    source = 'PUF'
  )

#---------------------------------------------------------------------------
# Cell scheme
#   pct_q : income-quintile within dataset, 0 for non-positive income
#   age_b : <35 / 35-54 / 55-64 / 65+
#   mar   : single / married
# Total cells: 5 × 4 × 2 = 40. Chosen to keep cells dense enough that the
# per-cell mean NW is stable — finer binning (adding n_dep) produced sparse
# heavy-tailed cells where μ_SCF was dominated by one billionaire and
# μ_PUF was much lower, breaking the decomposition's assumption.
#---------------------------------------------------------------------------

bin_cells = function(d) {
  pos = d$income > 0
  brks = Hmisc::wtd.quantile(d$income[pos], d$weight[pos], probs = seq(0, 1, 0.2))
  d$pct_q = findInterval(d$income, brks, all.inside = TRUE)
  d$pct_q[!pos] = 0L
  d$age_b = cut(d$age1, c(-Inf, 35, 55, 65, Inf),
                labels = c('<35', '35-54', '55-64', '65+'), right = FALSE)
  d$mar_b = factor(d$married, levels = c(0L, 1L), labels = c('single', 'married'))
  d$dep_b = factor(pmin(as.integer(d$n_dep), 3L),
                   levels = 0:3, labels = c('0', '1', '2', '3+'))
  d$cell  = paste(d$pct_q, d$mar_b, d$age_b, sep = '|')
  d
}
scf_h = bin_cells(scf_h)
puf_h = bin_cells(puf_h)

#---------------------------------------------------------------------------
# 1. Y|X sanity check — cell-wise mean NW on SCF vs PUF should match if the
# DRF is doing its job.
#---------------------------------------------------------------------------

cat('1. Y|X sanity: cell-wise mean NW, SCF vs PUF imputed\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

w_mean = function(x, w) sum(x * w) / sum(w)
w_sum  = function(x, w) sum(x * w)

agg_cell = function(d, ys = c('net_worth')) {
  d %>% group_by(cell) %>%
    summarise(
      w = sum(weight),
      across(all_of(ys), list(m = ~ w_mean(.x, weight))),
      .groups = 'drop'
    )
}
sc = agg_cell(scf_h) %>% rename_with(~ paste0('scf_', .), -cell)
pc = agg_cell(puf_h) %>% rename_with(~ paste0('puf_', .), -cell)
cmp = inner_join(sc, pc, by = 'cell') %>%
  filter(scf_w > 0, puf_w > 0)
cat(sprintf('  cells w/ both sides non-empty: %d of %d (SCF) / %d (PUF)\n',
            nrow(cmp), nrow(sc), nrow(pc)))

# Correlation + summary statistics of the per-cell mean-NW gap
rel_gap = (cmp$puf_net_worth_m - cmp$scf_net_worth_m) /
          pmax(abs(cmp$scf_net_worth_m), 1e-6)
cat(sprintf('  corr(SCF mean_NW, PUF mean_NW) across cells:  %.3f\n',
            cor(cmp$scf_net_worth_m, cmp$puf_net_worth_m)))
cat(sprintf('  median | rel gap | per cell:                  %.3f\n',
            median(abs(rel_gap), na.rm = TRUE)))
cat(sprintf('  95th-pct | rel gap |:                          %.3f\n',
            quantile(abs(rel_gap), 0.95, na.rm = TRUE)))
cat('  (tight = DRF captures Y|X; residual gap is MC noise)\n\n')

# Scatter plot: cell mean NW, SCF vs PUF
p_sanity = cmp %>%
  ggplot(aes(scf_net_worth_m / 1e3, puf_net_worth_m / 1e3,
             size = pmin(scf_w, puf_w))) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = 'grey60') +
  geom_point(alpha = 0.6, color = 'steelblue') +
  scale_size_area(max_size = 5, guide = 'none') +
  labs(title = 'Y|X sanity: per-cell mean NW, SCF vs imputed PUF',
       subtitle = 'Point size = min(SCF, PUF) cell weight. Tight to y=x = DRF is doing its job.',
       x = 'SCF cell mean NW ($K)', y = 'PUF cell mean NW ($K)') +
  theme_minimal(base_size = 10)
ggsave(file.path(plot_dir, '01_yx_sanity.png'), p_sanity,
       width = 8, height = 6, dpi = 140)

#---------------------------------------------------------------------------
# 2. Marginal X comparison (the 5 non-rank features). pctile_income is
# 1..100 uniform by construction in each dataset, so we skip it.
#---------------------------------------------------------------------------

cat('\n2. Marginal X distributions (weighted shares)\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

w_share = function(flag, w) sum(w[flag], na.rm = TRUE) / sum(w, na.rm = TRUE)
marg = function(d, lvls, colname) {
  sapply(lvls, function(lv) w_share(d[[colname]] == lv, d$weight))
}

# married
cat(sprintf('  married share: SCF %.3f  PUF %.3f  diff %+.3f\n',
            w_share(scf_h$married == 1, scf_h$weight),
            w_share(puf_h$married == 1, puf_h$weight),
            w_share(puf_h$married == 1, puf_h$weight) -
            w_share(scf_h$married == 1, scf_h$weight)))

# male1 (NA excluded)
cat(sprintf('  male1 share:   SCF %.3f  PUF %.3f  diff %+.3f\n',
            w_share(scf_h$male1 == 1, scf_h$weight),
            w_share(puf_h$male1 == 1, puf_h$weight),
            w_share(puf_h$male1 == 1, puf_h$weight) -
            w_share(scf_h$male1 == 1, scf_h$weight)))

# Age bands
cat('\n  age bands (weighted share):\n')
age_lv = c('<35', '35-54', '55-64', '65+')
sc_age = marg(scf_h, age_lv, 'age_b')
pu_age = marg(puf_h, age_lv, 'age_b')
for (i in seq_along(age_lv))
  cat(sprintf('    %-6s  SCF %.3f  PUF %.3f  diff %+.3f\n',
              age_lv[i], sc_age[i], pu_age[i], pu_age[i] - sc_age[i]))

# n_dep
cat('\n  n_dep (weighted share):\n')
dep_lv = c('0', '1', '2', '3+')
sc_dep = marg(scf_h, dep_lv, 'dep_b')
pu_dep = marg(puf_h, dep_lv, 'dep_b')
for (i in seq_along(dep_lv))
  cat(sprintf('    %-4s    SCF %.3f  PUF %.3f  diff %+.3f\n',
              dep_lv[i], sc_dep[i], pu_dep[i], pu_dep[i] - sc_dep[i]))

#---------------------------------------------------------------------------
# 3. Aggregate gap split by asset bucket
#
# Decompose the +$T aggregate NW gap into assets / debts / KG, and within
# each, show the X-effect and (Y|X)-residual sums. This is the first look
# at "where the money lives" before diving into cells or categories.
#---------------------------------------------------------------------------

cat('\n3. Aggregate gap by asset bucket\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

bucket_decomp = function(scf_df, puf_df, vars) {
  sc_b = scf_df %>% group_by(cell) %>%
    summarise(W_scf = sum(weight),
              mu_s = w_mean(.data[['bucket_sum']], weight),
              .groups = 'drop')
  pc_b = puf_df %>% group_by(cell) %>%
    summarise(W_puf = sum(weight),
              mu_p = w_mean(.data[['bucket_sum']], weight),
              .groups = 'drop')
  db = full_join(sc_b, pc_b, by = 'cell') %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)),
           x_effect = (W_puf - W_scf) * mu_s / 1e12,
           yx_resid = W_puf * (mu_p - mu_s) / 1e12)
  tibble(
    scf_tn  = w_sum(scf_df[['bucket_sum']], scf_df$weight) / 1e12,
    puf_tn  = w_sum(puf_df[['bucket_sum']], puf_df$weight) / 1e12,
    x_sum   = sum(db$x_effect),
    yx_sum  = sum(db$yx_resid)
  )
}

bucket_list = list(assets = asset_vars, debts = debt_vars, kg = kg_vars,
                   net_worth = c('net_worth'))
cat(sprintf('  %-10s | %10s %10s %10s | %10s %10s\n',
            'bucket', 'SCF ($T)', 'PUF ($T)', 'gap ($T)',
            'X-eff ($T)', 'Y|X ($T)'))
cat(paste0(rep('-', 76), collapse = ''), '\n')
for (b in names(bucket_list)) {
  scf_df = scf_h %>% mutate(bucket_sum =
    rowSums(across(all_of(bucket_list[[b]]))))
  puf_df = puf_h %>% mutate(bucket_sum =
    rowSums(across(all_of(bucket_list[[b]]))))
  r = bucket_decomp(scf_df, puf_df, bucket_list[[b]])
  cat(sprintf('  %-10s | %10.2f %10.2f %+10.2f | %+10.2f %+10.2f\n',
              b, r$scf_tn, r$puf_tn, r$puf_tn - r$scf_tn,
              r$x_sum, r$yx_sum))
}

#---------------------------------------------------------------------------
# 4. Cell-level decomposition of the aggregate NW gap
#
# Gap ≈ Σ_c (w_PUF,c − w_SCF,c) × μ_c^{SCF}, with N_total scaling.
# w_*,c are weighted fractions summing to 1; μ_c^{SCF} is cell mean NW on SCF.
#---------------------------------------------------------------------------

cat('\n\n4. Oaxaca decomposition of aggregate NW gap\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

N_puf = sum(puf_h$weight)
N_scf = sum(scf_h$weight)
agg_puf = w_sum(puf_h$net_worth, puf_h$weight) / 1e12
agg_scf = w_sum(scf_h$net_worth, scf_h$weight) / 1e12
cat(sprintf('  Total NW ($T):  SCF %.2f   PUF %.2f   gap %+.2f\n',
            agg_scf, agg_puf, agg_puf - agg_scf))

# Per-cell weighted share × N = population in cell; per-cell mean NW in each
# dataset. Decomposition:
#   T_PUF,c - T_SCF,c = X-effect + (Y|X)-residual
#      X-effect        = (W_PUF,c - W_SCF,c) * μ_SCF,c
#      (Y|X)-residual  = W_PUF,c * (μ_PUF,c - μ_SCF,c)
# Sum of both = actual per-cell contribution to the aggregate gap.
sc_cell = scf_h %>% group_by(cell) %>%
  summarise(W_scf = sum(weight),
            mu_scf = w_mean(net_worth, weight), .groups = 'drop')
pc_cell = puf_h %>% group_by(cell) %>%
  summarise(W_puf = sum(weight),
            mu_puf = w_mean(net_worth, weight), .groups = 'drop')
dec = full_join(sc_cell, pc_cell, by = 'cell') %>%
  mutate(
    across(c(W_scf, W_puf, mu_scf, mu_puf), ~ replace_na(., 0)),
    x_effect = (W_puf - W_scf) * mu_scf / 1e12,
    yx_resid = W_puf * (mu_puf - mu_scf) / 1e12,
    total    = x_effect + yx_resid
  )

x_sum  = sum(dec$x_effect)
yx_sum = sum(dec$yx_resid)
cat(sprintf('  Σ X-effect                  :  %+.2f  ($T)\n', x_sum))
cat(sprintf('  Σ (Y|X)-residual            :  %+.2f  ($T)\n', yx_sum))
cat(sprintf('  Σ total (X + Y|X)           :  %+.2f  ($T)\n', x_sum + yx_sum))
cat(sprintf('  observed aggregate gap      :  %+.2f  ($T)\n',
            agg_puf - agg_scf))
cat('\n  Interpretation: X-effect is the gap the DRF would produce if μ|X\n')
cat('  agreed exactly. Residual is per-cell mismatch — in sparse/heavy-\n')
cat('  tailed cells, μ_SCF is outlier-dominated and diverges from μ_PUF.\n\n')

dec = dec %>% arrange(desc(abs(total)))

cat('  Top 12 cells by |total contribution|:\n')
cat(sprintf('  %-22s | %8s %8s %10s %10s | %8s %8s %8s\n',
            'cell (incQ|mar|age)', 'W_SCF(M)', 'W_PUF(M)',
            'μ_SCF($K)', 'μ_PUF($K)',
            'X-eff($T)', 'Y|X($T)', 'tot($T)'))
cat(paste0(rep('-', 100), collapse = ''), '\n')
for (i in seq_len(min(12, nrow(dec)))) {
  cat(sprintf('  %-22s | %8.2f %8.2f %10.0f %10.0f | %+8.2f %+8.2f %+8.2f\n',
              dec$cell[i],
              dec$W_scf[i] / 1e6, dec$W_puf[i] / 1e6,
              dec$mu_scf[i] / 1e3, dec$mu_puf[i] / 1e3,
              dec$x_effect[i], dec$yx_resid[i], dec$total[i]))
}

cat('\n  Aggregate X-effect by income quintile:\n')
dec_q = dec %>%
  mutate(pct_q = as.integer(sub('\\|.*', '', cell))) %>%
  group_by(pct_q) %>%
  summarise(x_effect = sum(x_effect),
            yx_resid = sum(yx_resid),
            total    = sum(total), .groups = 'drop') %>%
  arrange(pct_q)
cat(sprintf('  %-8s | %10s %10s %10s\n',
            'pct_q', 'X-eff($T)', 'Y|X($T)', 'total($T)'))
for (i in seq_len(nrow(dec_q)))
  cat(sprintf('  %-8d | %+10.3f %+10.3f %+10.3f\n',
              dec_q$pct_q[i], dec_q$x_effect[i], dec_q$yx_resid[i], dec_q$total[i]))

#---------------------------------------------------------------------------
# 5. Per-category cell-level decomposition (top drivers per Y var)
#
# For each of the 23 Y vars: aggregate X-effect, Y|X-residual, total gap,
# and the single cell with the largest |total| contribution. Sorted by
# |total gap| descending so the biggest drivers bubble to the top.
#---------------------------------------------------------------------------

cat('\n\n5. Per-category decomposition (sorted by |gap$B|)\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

percat = map_dfr(y_vars, function(v) {
  sc_v = scf_h %>% group_by(cell) %>%
    summarise(W_scf = sum(weight),
              mu_s = w_mean(.data[[v]], weight), .groups = 'drop')
  pc_v = puf_h %>% group_by(cell) %>%
    summarise(W_puf = sum(weight),
              mu_p = w_mean(.data[[v]], weight), .groups = 'drop')
  dv = full_join(sc_v, pc_v, by = 'cell') %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)),
           x_effect = (W_puf - W_scf) * mu_s / 1e9,
           yx_resid = W_puf * (mu_p - mu_s) / 1e9,
           total    = x_effect + yx_resid) %>%
    arrange(desc(abs(total)))
  tibble(
    variable = v,
    scf_agg  = w_sum(scf_h[[v]], scf_h$weight) / 1e9,
    puf_agg  = w_sum(puf_h[[v]], puf_h$weight) / 1e9,
    x_sum    = sum(dv$x_effect),
    yx_sum   = sum(dv$yx_resid),
    top_cell = dv$cell[1]
  )
}) %>%
  mutate(gap = puf_agg - scf_agg) %>%
  arrange(desc(abs(gap)))

cat(sprintf('  %-18s | %9s %9s %9s | %9s %9s | %s\n',
            'variable', 'SCF $B', 'PUF $B', 'gap $B',
            'X-eff $B', 'Y|X $B', 'top cell'))
cat(paste0(rep('-', 96), collapse = ''), '\n')
for (i in seq_len(nrow(percat))) {
  cat(sprintf('  %-18s | %9.1f %9.1f %+9.1f | %+9.1f %+9.1f | %s\n',
              percat$variable[i], percat$scf_agg[i], percat$puf_agg[i],
              percat$gap[i], percat$x_sum[i], percat$yx_sum[i],
              percat$top_cell[i]))
}
write_csv(percat, file.path(plot_dir, 'percat_decomp.csv'))

#---------------------------------------------------------------------------
# 6. Mechanism: within-cell X-covariate comparison for top Y|X cells
#
# The "Y|X residual" in the Oaxaca decomposition isn't a DRF failure per se
# — the DRF imputes at the level of its full feature vector X, but our
# decomp cell c (incQ|mar|age) is coarser than X. So
#   μ_PUF,c = E_x[E[Y|X=x]_SCF | c]_PUF
#   μ_SCF,c = E_x[E[Y|X=x]_SCF | c]_SCF
# and the residual = Σ_x [P_PUF(x|c) − P_SCF(x|c)] × E[Y|x]_SCF.
#
# The observable mechanism is: within a given decomp cell, what's the
# distribution of the income-composition X's? If PUF cell c has more
# business-havers or more capital-gains-havers than SCF cell c, the DRF
# routes SCF's high-wealth conditional distributions to those PUF units.
#
# This section takes the top 8 cells by |Y|X residual|, and for each one
# reports weighted share-positive and weighted mean (among positives) for
# business, int_div, capital_gains, ss_pens on both sides. Any material
# difference in composition is the mechanism behind the residual.
#
# NOTE: PUF-side composition vars come from consumption_analysis.rds which
# filters to income > 0 & C > 0. For the top Y|X cells (middle-to-high
# income), this covers essentially the full universe, but any cell with
# pct_q = 0 will show NA on the PUF composition side.
#---------------------------------------------------------------------------

cat('\n\n6. Within-cell X comparison for top Y|X-residual cells\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

comp_vars = c('business_comp', 'int_div_comp',
              'capital_gains_comp', 'ss_pens_comp')

# Top 8 cells by absolute Y|X residual. Use `dec` from section 4.
top_yx = dec %>% arrange(desc(abs(yx_resid))) %>% slice_head(n = 8)

cell_x_stats = function(d, cell_name) {
  sub = d %>% filter(cell == cell_name, !is.na(business_comp))
  if (nrow(sub) == 0) return(NULL)
  w = sub$weight
  W = sum(w)
  map_dfr(comp_vars, function(v) {
    x = sub[[v]]
    pos = !is.na(x) & x > 0
    mean_pos = if (any(pos)) sum(x[pos] * w[pos]) / sum(w[pos]) else NA_real_
    tibble(comp = sub('_comp$', '', v),
           share_pos = sum(w[pos]) / W,
           mean_pos  = mean_pos,
           W_obs     = W)
  })
}

mech = map_dfr(seq_len(nrow(top_yx)), function(i) {
  cc = top_yx$cell[i]
  scf_stats = cell_x_stats(scf_h, cc)
  puf_stats = cell_x_stats(puf_h, cc)
  if (is.null(scf_stats) || is.null(puf_stats)) return(NULL)
  scf_stats %>%
    rename(scf_share_pos = share_pos, scf_mean_pos = mean_pos,
           scf_W = W_obs) %>%
    inner_join(puf_stats %>%
                 rename(puf_share_pos = share_pos, puf_mean_pos = mean_pos,
                        puf_W = W_obs),
               by = 'comp') %>%
    mutate(cell = cc,
           yx_resid_tn = top_yx$yx_resid[i])
})

# Print one block per cell so the pattern is easy to scan.
cat(sprintf('  %-22s | %-18s | %8s %8s %6s | %10s %10s %6s\n',
            'cell (incQ|mar|age)', 'comp',
            'pos_SCF', 'pos_PUF', 'Δpp',
            'μ_SCF|+', 'μ_PUF|+', 'ratio'))
cat(paste0(rep('-', 106), collapse = ''), '\n')
cur_cell = ''
for (i in seq_len(nrow(mech))) {
  cc = mech$cell[i]
  cell_label = if (cc != cur_cell)
    sprintf('%-22s', paste0(cc, sprintf(' [Y|X %+4.1fT]', mech$yx_resid_tn[i])))
    else strrep(' ', 22)
  cur_cell = cc
  ratio = if (!is.na(mech$scf_mean_pos[i]) && !is.na(mech$puf_mean_pos[i]) &&
              mech$scf_mean_pos[i] > 0)
    sprintf('%6.2f', mech$puf_mean_pos[i] / mech$scf_mean_pos[i])
    else '     —'
  cat(sprintf('  %s | %-18s | %7.1f%% %7.1f%% %+6.1f | %10s %10s %s\n',
              cell_label, mech$comp[i],
              100 * mech$scf_share_pos[i],
              100 * mech$puf_share_pos[i],
              100 * (mech$puf_share_pos[i] - mech$scf_share_pos[i]),
              if (is.na(mech$scf_mean_pos[i])) '—'
                else formatC(round(mech$scf_mean_pos[i]), big.mark = ',', format = 'd'),
              if (is.na(mech$puf_mean_pos[i])) '—'
                else formatC(round(mech$puf_mean_pos[i]), big.mark = ',', format = 'd'),
              ratio))
}
cat('\n  Reading: pos_* is weighted share of cell with component > 0.\n')
cat('  μ_*|+ is weighted mean (among positives) in dollars, within-cell.\n')
cat('  A Δpp of +15 for business means PUF has 15 pp more business-havers\n')
cat('  in the cell than SCF — the DRF then assigns SCF-business-haver\n')
cat('  wealth to those extra PUF units, showing up as Y|X residual.\n')

write_csv(mech, file.path(plot_dir, 'within_cell_x_comparison.csv'))

#---------------------------------------------------------------------------
# 7. Tail diagnostic for top Y|X-residual cells
#
# Is the Y|X residual driven by a shift in the full distribution, or by
# the mean being outlier-dominated? Report weighted median, P90, P99 of
# net worth per side for the top 8 Y|X cells. If P50 moves together but
# P99 diverges wildly, the mean is outlier-driven and the residual is a
# sampling-variance artifact rather than a systematic shift.
#---------------------------------------------------------------------------

cat('\n\n7. Tail diagnostic — NW quantiles in top Y|X cells\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

tail_quantiles = function(d, cc) {
  sub = d %>% filter(cell == cc)
  if (nrow(sub) == 0) return(tibble(p50 = NA, p90 = NA, p99 = NA))
  q = Hmisc::wtd.quantile(sub$net_worth, sub$weight,
                          probs = c(0.50, 0.90, 0.99), na.rm = TRUE)
  tibble(p50 = q[1], p90 = q[2], p99 = q[3])
}

tail_rows = map_dfr(seq_len(nrow(top_yx)), function(i) {
  cc = top_yx$cell[i]
  s = tail_quantiles(scf_h, cc)
  p = tail_quantiles(puf_h, cc)
  tibble(cell = cc,
         yx_resid_tn = top_yx$yx_resid[i],
         mu_scf = top_yx$mu_scf[i], mu_puf = top_yx$mu_puf[i],
         scf_p50 = s$p50, scf_p90 = s$p90, scf_p99 = s$p99,
         puf_p50 = p$p50, puf_p90 = p$p90, puf_p99 = p$p99)
})

cat(sprintf('  %-22s | %10s %10s | %10s %10s %10s | %10s %10s %10s\n',
            'cell', 'μ_SCF($K)', 'μ_PUF($K)',
            'SCF P50$K', 'SCF P90$K', 'SCF P99$K',
            'PUF P50$K', 'PUF P90$K', 'PUF P99$K'))
cat(paste0(rep('-', 120), collapse = ''), '\n')
for (i in seq_len(nrow(tail_rows))) {
  cat(sprintf('  %-22s | %10.0f %10.0f | %10.0f %10.0f %10.0f | %10.0f %10.0f %10.0f\n',
              tail_rows$cell[i],
              tail_rows$mu_scf[i] / 1e3, tail_rows$mu_puf[i] / 1e3,
              tail_rows$scf_p50[i] / 1e3, tail_rows$scf_p90[i] / 1e3,
              tail_rows$scf_p99[i] / 1e3,
              tail_rows$puf_p50[i] / 1e3, tail_rows$puf_p90[i] / 1e3,
              tail_rows$puf_p99[i] / 1e3))
}
cat('\n  Reading: if PUF P99 >> SCF P99 but P50 tracks, the Y|X residual is\n')
cat('  driven by DRF drawing heavy donors into cells where SCF has thinner\n')
cat('  tails. If P50 also shifts, the whole distribution moved.\n')

write_csv(tail_rows, file.path(plot_dir, 'top_yx_tail_diagnostic.csv'))

#---------------------------------------------------------------------------
# Plots
#---------------------------------------------------------------------------

# Age distribution overlay
p_age = bind_rows(
  scf_h %>% transmute(age1, weight, source),
  puf_h %>% transmute(age1, weight, source)
) %>%
  ggplot(aes(age1, weight = weight, fill = source)) +
  geom_histogram(position = 'identity', alpha = 0.5, binwidth = 2) +
  scale_fill_manual(values = c('SCF' = 'steelblue', 'PUF' = 'firebrick')) +
  labs(title = 'Age1 distribution: PUF tax units vs SCF tax units (weighted)',
       x = 'Primary age', y = 'Weighted count') +
  theme_minimal(base_size = 10)
ggsave(file.path(plot_dir, '02_age_distribution.png'), p_age,
       width = 9, height = 5, dpi = 140)

# Cell contribution barplot: stacked X-effect and Y|X-residual
p_cell = dec %>% slice_head(n = 12) %>%
  mutate(cell = factor(cell, levels = rev(cell))) %>%
  select(cell, x_effect, yx_resid) %>%
  pivot_longer(-cell, names_to = 'component', values_to = 'contrib') %>%
  ggplot(aes(cell, contrib, fill = component)) +
  geom_col() +
  geom_hline(yintercept = 0, color = 'grey40') +
  scale_fill_manual(values = c('x_effect' = 'steelblue', 'yx_resid' = 'firebrick'),
                    labels = c('X-effect', 'Y|X residual')) +
  coord_flip() +
  labs(title = 'Top 12 cells by |total| contribution to NW gap (PUF − SCF)',
       subtitle = 'X-effect: (W_PUF − W_SCF) × μ_SCF. Y|X residual: W_PUF × (μ_PUF − μ_SCF).',
       x = NULL, y = 'Contribution ($T)') +
  theme_minimal(base_size = 9)
ggsave(file.path(plot_dir, '03_top_cells.png'), p_cell,
       width = 10, height = 6, dpi = 140)

cat('\nArtifacts saved to ', plot_dir, '/\n', sep = '')
cat('  01_yx_sanity.png              — cell mean NW scatter, SCF vs PUF\n')
cat('  02_age_distribution.png       — weighted age histograms overlaid\n')
cat('  03_top_cells.png              — top 12 cells driving the aggregate gap\n')
cat('  percat_decomp.csv             — per-category X/Y|X decomposition\n')
cat('  within_cell_x_comparison.csv  — within-top-cell X mechanism table\n')
cat('  top_yx_tail_diagnostic.csv    — NW quantiles within top Y|X cells\n')
cat('Report at ', report_path, '\n', sep = '')

sink()
