#------------------------------------------------------------------------------
# refit_filers_2017.R  (S40)
#
# Stage 2 of the TY2017 calibration (src/refit_filers.R): refits filer
# weights by the AGI proxy on the final 2017 values, from the stage-1 weights
# create_2017_puf.R calibrated by E00100 -- the weights the amounts were
# rescaled on, so the refit moves them as little as it can. Non-filers keep
# the pool's weights.
#------------------------------------------------------------------------------

source('./src/refit_filers.R')

filers_2017 = tax_units %>% filter(filer == 1)
refit_2017 = refit_filer_year(filers_2017, 2017L)
tax_units = tax_units %>%
  left_join(refit_2017 %>% rename(weight_refit = weight), by = 'id') %>%
  mutate(weight = if_else(filer == 1, weight_refit, weight)) %>%
  select(-weight_refit)
stopifnot(!any(is.na(tax_units$weight)))
rm(filers_2017, refit_2017)
