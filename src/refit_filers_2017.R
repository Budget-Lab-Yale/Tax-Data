#------------------------------------------------------------------------------
# refit_filers_2017.R  (S40)
#
# Calibrates TY2017 filer weights to Pub 1304 (src/refit_filers.R), starting
# from each record's weight before the 2017 LP (filer_prior_2017, kept by
# create_2017_puf.R). Non-filers keep the pool's weights.
#------------------------------------------------------------------------------

source('./src/refit_filers.R')

# Filers start from their weight before the 2017 LP; non-filers are the pool's.
stopifnot(exists('filer_prior_2017'))
filers_2017 = tax_units %>%
  filter(filer == 1) %>%
  select(-weight) %>%
  inner_join(filer_prior_2017, by = 'id')
stopifnot(nrow(filers_2017) == sum(tax_units$filer == 1))
refit_2017 = refit_filer_year(filers_2017, 2017L)
tax_units = tax_units %>%
  left_join(refit_2017 %>% rename(weight_refit = weight), by = 'id') %>%
  mutate(weight = if_else(filer == 1, weight_refit, weight)) %>%
  select(-weight_refit)
stopifnot(!any(is.na(tax_units$weight)))
rm(filers_2017, refit_2017)
