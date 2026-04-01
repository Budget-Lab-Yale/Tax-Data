#--------------------------------------
# impute_variables.R
#
# Models variables not included in PUF
#--------------------------------------

# TODO list for the future
# - improve dependent age imputation
# - vary SSN status by income
# - actually impute pretax contributions
# - higher fidelty wage earnings split for EARNINGS var, including consistent notion of gender/primary status interaction
# - gender based on Saez-Zucman 2016 tabulations


# Shared utilities
source('src/imputations/helpers.R')

# Imputation modules
source('src/imputations/demographics.R')
source('src/imputations/ages.R')
source('src/imputations/ssn.R')
source('src/imputations/earnings_split.R')
source('src/imputations/qbi.R')
source('src/imputations/mobility.R')
source('src/imputations/tips.R')
source('src/imputations/overtime.R')
source('src/imputations/auto_loan.R')
source('src/imputations/childcare.R')
source('src/imputations/mortgage.R')
source('src/imputations/consumption.R')
source('src/imputations/capital_gains.R')
source('src/imputations/placeholders.R')
