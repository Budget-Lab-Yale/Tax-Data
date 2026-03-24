#--------------------------------------
# placeholders.R
#
# Placeholder imputations that will be
# revisited when time allows
#--------------------------------------


tax_units %<>%
  mutate(

    # Prior-year earnings
    ei_prior_yr = 0,

    # Pretax contributions via employer
    trad_contr_er1 = 0,
    trad_contr_er2 = 0,

    # Divorce year
    divorce_year = if_else(alimony > 0 | alimony_exp > 0, 0, NA),

    # Net operating losses (currently captured through other income residual)
    nols = 0,

    # Other above-the-line deductions (currently captured through other income residual)
    other_above_ded = 0,

    # Mortgage and investment interest deduction info
    first_mort_int   = int_exp,
    second_mort_int  = 0,
    first_mort_bal   = 0,
    second_mort_bal  = 0,
    first_mort_year  = 0,
    second_mort_year = 0,
    inv_int_exp      = 0,

    # Personal property taxes
    salt_pers = 0
  )
