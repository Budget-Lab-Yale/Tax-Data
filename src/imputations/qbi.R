#--------------------------------------
# qbi.R
#
# Imputes QBI-related variables: SSTB
# status, W-2 wage bills, employer status
#--------------------------------------


# Read QBI variable imputation parameters and consolidate by entity form and SSTB status
qbi_params = read_csv('./resources/qbi.csv') %>%
  group_by(form, sstb) %>%
  summarise(share_employer = mean(share_employer),
            n              = sum(n),
            wages          = sum(wages) * 1000,
            net_income     = sum(net_income) * 1000,
            .groups = 'drop')

# Calculate probability of SSTB status
sstb_params = qbi_params %>%
  select(form, sstb, n) %>%
  pivot_wider(names_from   = sstb,
              names_prefix = 'sstb',
              values_from  = n) %>%
  mutate(p_sstb = (sstb1 / (sstb0 + sstb1)) / 5) %>%  # Scaling factor calibrated to match QBI totals
  select(form, p_sstb)

# Assumption: wages paid are shared out in proportion to:
# (1) equally
# (2) share of positive net income
# ...plus a Gaussian noise term.
# The idea is that wages are a linear function of (non-loss) profits, with a
# nonzero intercept to account for "fixed costs". The weights are arbitrary,
# roughly calibrated to match aggregate QBI statistics.
pass_thru_micro = puf %>%
  mutate(part  = part_active - part_active_loss + part_passive - part_passive_loss - part_179,
         scorp = scorp_active - scorp_active_loss + scorp_passive - scorp_passive_loss - scorp_179) %>%
  select(id, weight, sole_prop, part, scorp) %>%
  pivot_longer(cols      = -c(id, weight),
               names_to  = 'form',
               values_to = 'net_income') %>%

  # Impute SSTB status
  left_join(sstb_params, by = 'form') %>%
  mutate(sstb = runif(nrow(.)) < p_sstb) %>%

  # Impute employer status (i.e. nonzero wages)
  left_join(qbi_params %>%
              select(form, sstb, share_employer, total_wages = wages),
            by = c('form', 'sstb')) %>%
  mutate(employer = runif(nrow(.)) < (share_employer + 0.2))  # scale up factor to account for the fact that employer share is defined w/r/t/ $10K, not $0K, wage definition


# Impute wages paid
qbi_variables = pass_thru_micro %>%
  left_join(pass_thru_micro %>%
              group_by(form, sstb) %>%
              summarise(total_count  = sum(employer * (net_income != 0) * weight),
                        total_profit = sum(employer * net_income * (net_income > 0) * weight),
                        .groups = 'drop'),
            by = c('form', 'sstb')) %>%
  mutate(share_count  = as.integer(employer * (net_income != 0))   / total_count,
         share_profit = (employer * (net_income > 0) * net_income) / total_profit,
         random_term  = rnorm(nrow(.), mean = 1, sd = 0.15),
         wagebill     = (share_count * total_wages * 0.5 +
                         share_profit * total_wages * 0.5) * random_term) %>%

  # Convert SSTB to boolean and set values to NA for returns with no net income
  mutate(sstb = (sstb == 1),
         across(.cols = c(sstb, wagebill),
                .fns  = ~ if_else(net_income == 0, NA, .))) %>%
  select(id, form, sstb, wagebill) %>%
  pivot_wider(names_from = form,
              values_from = c(sstb, wagebill))


# Add to main dataframe
tax_units %<>%
  left_join(qbi_variables, by = 'id') %>%

  # Add placeholder farm QBI imputations: all farm income is eligible for QBI
  mutate(sstb_farm     = if_else(farm == 0, NA, F),
         wagebill_farm = if_else(farm > 0, farm, if_else(farm == 0, NA, 0)))
