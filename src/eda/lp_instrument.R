#---------------------------------------------------------------------
# lp_instrument.R
#
# Definitive answer to "is the LP actually satisfying its constraints?"
#
# Strategy: source enough of the pipeline to recreate `puf_2017` exactly
# as it enters reweight_lp() in create_2017_puf.R. Then re-run the LP
# with logging on the solve() return code. Then for each constraint,
# compute LHS×weight_deltas and check whether the result lies in the
# tolerance band [lower, upper]. Report violations.
#
# Stochastic imputations (process_puf age_group fill, etc.) are seeded
# in main.R line 29 (set.seed(76)). We mirror that here.
#
# Run via slurm — bypasses sourcing main.R wholesale because we want
# to stop before impute_nonfilers / impute_variables / project_puf.
#---------------------------------------------------------------------

# Load packages
suppressPackageStartupMessages({
  invisible(lapply(readLines('requirements.txt'), library, character.only = TRUE))
})

cat('LP instrumentation start\n')

# Mirror main.R bootstrap
source('./src/configure.R')
set.seed(76)

# Phase 1, up through create_2017_puf, but stop just before reweight_lp()
source('./src/process_targets.R')
cat('process_targets done\n')

source('./src/process_puf.R')
cat(sprintf('process_puf done — puf nrow = %d\n', nrow(puf)))

source('./src/reweight.R')
source('./src/summary.R')

# Replicate the pre-LP scaling block from create_2017_puf.R lines 13-38
puf %<>%
  mutate(married = as.integer(filing_status == 2),
         senior  = as.integer(age_group == 6))

weight_scaling_factors = tables$table_1_6 %>%
  filter(year == 2017) %>%
  group_by(married   = as.integer(filing_status == 2),
           senior    = as.integer(age_group == 6),
           agi_group = agi) %>%
  summarise(count2017 = sum(count), .groups = 'drop') %>%
  left_join(puf %>%
              group_by(married, senior, agi_group) %>%
              summarise(puf_w = sum(weight), .groups = 'drop'),
            by = c('married', 'senior', 'agi_group')) %>%
  mutate(factor = count2017 / puf_w) %>%
  select(-count2017, -puf_w)

puf_2017 = puf %>%
  left_join(weight_scaling_factors, by = c('married', 'senior', 'agi_group')) %>%
  mutate(weight = weight * factor) %>%
  select(-factor)

cat(sprintf('pre-LP scaling done — puf_2017 nrow = %d, total weight = %.2fM\n',
            nrow(puf_2017), sum(puf_2017$weight) / 1e6))

# Build target tibble (replicates create_2017_puf.R lines 46-51)
targets = target_info %>%
  bind_cols(
    purrr::map(.x = 1:nrow(target_info),
               .f = ~ get_target_value(target_info[.x, ])) %>%
      bind_rows()
  )
cat(sprintf('targets built — n constraints = %d\n', nrow(targets)))

#---------------------------------------------------------------------
# Build LHS the same way reweight.R does
#---------------------------------------------------------------------

lhs = matrix(nrow = nrow(puf_2017), ncol = nrow(targets))

for (i in 1:nrow(targets)) {
  this_constraint = targets[i,]
  variable        = this_constraint[["variable"]]
  filing_status   = this_constraint[["filing_status"]] %>% str_split_1(' ') %>% as.numeric()
  age_group       = this_constraint[["age_group"]] %>% str_split_1(' ') %>% as.numeric()
  agi_min         = this_constraint[["agi_min"]]
  agi_max         = this_constraint[["agi_max"]]
  val_flag        = this_constraint[["val_flag"]]

  lhs[, i] = puf_2017$weight *
    (((1 - val_flag) * (puf_2017[[variable]] != 0)) + (val_flag * puf_2017[[variable]])) *
    (puf_2017$filing_status %in% filing_status) *
    (puf_2017$age_group %in% age_group) *
    (puf_2017$E00100 >= agi_min) *
    (puf_2017$E00100 < agi_max)
}
cat('LHS matrix built\n')

# Sanity: any NAs?
n_na = sum(is.na(lhs))
cat(sprintf('NAs in LHS: %d\n', n_na))

#---------------------------------------------------------------------
# Run LP fresh, capture return code
#---------------------------------------------------------------------

cat('\n--- Running LP fresh, capturing return code ---\n')
rhs = targets %>%
  mutate(upper = target_value * (1 + tolerance),
         lower = target_value * (1 - tolerance))

lprw = make.lp(ncol(lhs), nrow(lhs))
set.objfn(lprw, rep(1, nrow(lhs)))
for (i in 1:ncol(lhs)) {
  add.constraint(lprw, lhs[,i], "<=", rhs$upper[i])
  add.constraint(lprw, lhs[,i], ">=", rhs$lower[i])
}
EPS = 0.66
set.bounds(lprw,
           lower = rep(1 - EPS, nrow(lhs)),
           upper = rep(1 + EPS, nrow(lhs)))

t0 = Sys.time()
solution = solve(lprw)
t1 = Sys.time()
cat(sprintf('  solve() return code: %d   (0=optimal, 2=infeasible, 5=suboptimal, 9=presolved)\n',
            solution))
cat(sprintf('  solve time: %.1fs\n', as.numeric(difftime(t1, t0, units='secs'))))

deltas = get.variables(lprw)
cat(sprintf('  deltas: min=%.4f median=%.4f max=%.4f mean=%.4f\n',
            min(deltas), median(deltas), max(deltas), mean(deltas)))

#---------------------------------------------------------------------
# Verify constraint satisfaction
#---------------------------------------------------------------------

actual_lhs = as.numeric(t(lhs) %*% deltas)

slack_upper = rhs$upper - actual_lhs   # >= 0 means satisfied
slack_lower = actual_lhs - rhs$lower   # >= 0 means satisfied

violated_upper = slack_upper < -1e-3
violated_lower = slack_lower < -1e-3
violated = violated_upper | violated_lower

cat(sprintf('\n--- Constraint satisfaction (after solve) ---\n'))
cat(sprintf('  total constraints     : %d\n', nrow(targets)))
cat(sprintf('  violated (any side)   : %d (%.1f%%)\n',
            sum(violated), 100 * mean(violated)))
cat(sprintf('  violated upper        : %d\n', sum(violated_upper)))
cat(sprintf('  violated lower        : %d\n', sum(violated_lower)))

# Worst violations: top 20 by relative deviation
viol_summary = tibble(
  i = 1:nrow(targets),
  variable      = targets$variable,
  filing_status = targets$filing_status,
  age_group     = targets$age_group,
  agi_min       = targets$agi_min,
  agi_max       = targets$agi_max,
  target        = rhs$target_value,
  lower         = rhs$lower,
  upper         = rhs$upper,
  actual        = actual_lhs,
  rel_dev       = (actual_lhs - rhs$target_value) / pmax(abs(rhs$target_value), 1)
) %>%
  filter(violated) %>%
  arrange(desc(abs(rel_dev)))

cat('\n--- Worst 20 violated constraints (by |relative deviation|) ---\n')
print(viol_summary %>% head(20), n = Inf, width = Inf)

# Specifically flag the senior / band-1 cells
cat('\n--- Probe: ag=1 + ag=6 + fs={1,2,3,4} marginal constraints ---\n')
probe = tibble(
  i = 1:nrow(targets),
  variable      = targets$variable,
  filing_status = targets$filing_status,
  age_group     = targets$age_group,
  agi_min       = targets$agi_min,
  agi_max       = targets$agi_max,
  target_M      = rhs$target_value / 1e6,
  actual_M      = actual_lhs / 1e6,
  rel_dev       = (actual_lhs - rhs$target_value) / pmax(abs(rhs$target_value), 1),
  satisfied     = !violated
) %>%
  filter(variable == 'returns',
         age_group %in% c('1', '6') |
         filing_status %in% c('1', '2', '3', '4'))
print(probe, n = Inf, width = Inf)

cat('\nDone.\n')
