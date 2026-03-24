#--------------------------------------
# capital_gains.R
#
# Imputes capital gains basis and
# holding period using SOCA data,
# S&P 500 returns, and SSA mortality
#--------------------------------------


# Read SOCA basis/sales ratios and S&P 500 total return index
soca_data  = read_csv('resources/soca_basis_sales.csv')
sp500_index = read_csv('resources/sp500_total_return_index.csv')

# Compute trailing annualized S&P 500 return for each SOCA cell
# R(h, t) = (index[t] / index[t-h])^(1/h) - 1, with linear interpolation for fractional years
sp500_interp = approxfun(sp500_index$year, sp500_index$index, rule = 2)
soca_data = soca_data %>%
  mutate(
    trailing_return = (sp500_interp(year) / sp500_interp(year - h))^(1 / h) - 1,
    h_log_return    = h * log(1 + trailing_return)
  )

# Estimate OLS model: log(ratio) = alpha_h + beta * h * log(1 + R)
basis_model = lm(log(basis_sales_ratio) ~ factor(bucket) + I(h_log_return) - 1, data = soca_data)

# Predict 2017 basis/sales ratios for each bucket
buckets_2017 = tibble(
  bucket = c('Under 18 months', '18 months under 2 years', '2 years under 3 years',
             '3 years under 4 years', '4 years under 5 years', '5 years under 10 years',
             '10 years under 15 years', '15 years under 20 years', '20 years or more'),
  h = c(1.25, 1.75, 2.50, 3.50, 4.50, 7.50, 12.50, 17.50, NA)
)

# Representative h for "20 years or more": mean of the exponential splice
# used for HP draws. The exponential rate is pinned by density continuity
# at h=20 (see draw code below), giving E[h | h >= 20] = 20 + 1/lambda.
wb_shape = 0.7711
wb_scale = 9.1458

soca_hp_pre = read_csv('resources/soca_hp_ingredients.csv')
dw_at_boundary_pre   = dweibull(19, shape = wb_shape, scale = wb_scale)
wb_mass_bucket_8_pre = pweibull(19, wb_shape, wb_scale) - pweibull(14, wb_shape, wb_scale)
exp_lambda_pre       = dw_at_boundary_pre / wb_mass_bucket_8_pre * soca_hp_pre$pi_g[8] / soca_hp_pre$pi_g[9]
h_top = 20 + 1 / exp_lambda_pre
rm(soca_hp_pre, dw_at_boundary_pre, wb_mass_bucket_8_pre, exp_lambda_pre)

buckets_2017$h[9] = h_top

buckets_2017 %<>%
  mutate(
    trailing_return = (sp500_interp(2017) / sp500_interp(2017 - h))^(1 / h) - 1,
    h_log_return    = h * log(1 + trailing_return)
  ) %>%
  mutate(predicted_ratio = exp(predict(basis_model, newdata = .)))

# Build basis/sales interpolation function from 2017 predicted knot points
# For h beyond the last knot, extrapolate using model-implied annualized return
g_extrap = (1 / buckets_2017$predicted_ratio[9])^(1 / h_top) - 1
basis_sales_fn_2017 = approxfun(
  x = buckets_2017$h,
  y = buckets_2017$predicted_ratio,
  rule = 2
)
basis_sales_fn = function(h) if_else(h <= h_top, basis_sales_fn_2017(h), 1 / (1 + g_extrap)^h)

# ---- Unconditional holding period imputation ----
# Draw HP from SOCA gain-dollar-weighted (pi_g) or loss-dollar-weighted (pi_l)
# bucket distribution, then smooth within buckets via truncated Weibull.

soca_hp   = read_csv('resources/soca_hp_ingredients.csv')
soca_loss = read_csv('resources/soca_loss_hp.csv')

pi_g  = soca_hp$pi_g
n_h   = length(pi_g)
pi_l  = soca_loss$pi_l

# Weibull smoothing within HP buckets
wb_shape  = 0.7711
wb_scale  = 9.1458
bucket_lo = c(1.0, 1.5, 2.0, 3.0, 4.0,  5.0, 10.0, 15.0, 20.0)
bucket_hi = c(1.5, 2.0, 3.0, 4.0, 5.0, 10.0, 15.0, 20.0,  Inf)

# Exponential rate for the 20+ bucket, pinned by density continuity at h=20.
# The Weibull density just below h=20 (in bucket 8) determines the required
# density just above h=20, which pins lambda for the exponential splice.
dw_at_boundary   = dweibull(19, shape = wb_shape, scale = wb_scale)
wb_mass_bucket_8 = pweibull(19, wb_shape, wb_scale) - pweibull(14, wb_shape, wb_scale)
exp_lambda_20p   = dw_at_boundary / wb_mass_bucket_8 * pi_g[8] / pi_g[9]

rtrunc_weibull = function(n, lo, hi) {
  if (lo >= 20) {
    # Exponential splice for 20+ bucket (uncapped)
    rexp(n, rate = exp_lambda_20p) + 20
  } else {
    x_lo = lo - 1
    x_hi = hi - 1
    F_lo = pweibull(x_lo, shape = wb_shape, scale = wb_scale)
    F_hi = pweibull(x_hi, shape = wb_shape, scale = wb_scale)
    u = runif(n, min = F_lo, max = F_hi)
    1 + qweibull(u, shape = wb_shape, scale = wb_scale)
  }
}

draw_hp = function(idx, probs) {
  buckets = sample(seq_len(n_h), size = length(idx), replace = TRUE, prob = probs)
  hp = numeric(length(idx))
  for (j in seq_len(n_h)) {
    in_j = which(buckets == j)
    if (length(in_j) > 0) hp[in_j] = rtrunc_weibull(length(in_j), bucket_lo[j], bucket_hi[j])
  }
  hp
}

# Draw HP for gains and losses
tax_units$kg_lt_years_held = NA_real_
gain_idx = which(tax_units$kg_lt > 0)
loss_idx = which(tax_units$kg_lt < 0)
if (length(gain_idx) > 0) tax_units$kg_lt_years_held[gain_idx] = draw_hp(gain_idx, pi_g)
if (length(loss_idx) > 0) tax_units$kg_lt_years_held[loss_idx] = draw_hp(loss_idx, pi_l)

# Basis computation
# Gains: basis = |gain| * BSR/(1-BSR)
# Losses: basis = |loss| * loss_BSR/(loss_BSR-1)
loss_bsr_fn = approxfun(
  x = c(soca_loss$h_midpoint[1:8], h_top),
  y = soca_loss$loss_bsr,
  rule = 2
)

tax_units %<>% mutate(
  kg_lt_basis = case_when(
    kg_lt > 0 ~ abs(kg_lt) * basis_sales_fn(kg_lt_years_held) / (1 - basis_sales_fn(kg_lt_years_held)),
    kg_lt < 0 ~ abs(kg_lt) * loss_bsr_fn(kg_lt_years_held) / (loss_bsr_fn(kg_lt_years_held) - 1),
    TRUE ~ NA_real_
  )
)

rm(soca_hp, soca_loss, pi_g, pi_l, gain_idx, loss_idx, loss_bsr_fn,
   rtrunc_weibull, bucket_lo, bucket_hi, wb_shape, wb_scale, draw_hp,
   ssa_life, ssa_qx, acq_ages, acq_weights, p_basis_survive,
   mort_grid_h, mort_grid_p, mort_max_p)
