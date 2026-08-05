#--------------------------------------
# ssn.R
#
# Imputes blindness, gender, and
# SSN status for children
#--------------------------------------



#--------------------------------
# Impute SSN status for children
#--------------------------------

# Source: ITEP (https://itep.org/inclusive-child-tax-credit-reform-would-restore-benefit-to-1-million-young-dreamers/)
share_without_ssn = 1098100 / 72e6

# Assume perfect correlation of SSN status within tax units
tax_units %<>%
  mutate(ssn          = runif(nrow(.)) > share_without_ssn,
         dep_ssn1     = if_else(!is.na(dep_age1), ssn, NA),
         dep_ssn2     = if_else(!is.na(dep_age2), ssn, NA),
         dep_ssn3     = if_else(!is.na(dep_age3), ssn, NA)) %>%
  select(-ssn)