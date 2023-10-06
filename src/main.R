#---------------------------------------------
# main.R
# 
# Entry point into Tax-Data processing module
#---------------------------------------------


#----------------
# Set parameters
#----------------

# Load required packages
lapply(readLines('requirements.txt'), library, character.only = T)

# Read runtime configuration params and set filepaths
source('./src/configure.R')

# Set random seed 
set.seed(76)



#----------------
# Build tax data
#----------------

# Read and process targets
source('./src/process_targets.R')

# Read and process PUF
source('./src/process_puf.R')

# Create 2017 PUF
source('./src/reweight.R')
source('./src/create_2017_puf.R')

# TODO add nonfilers and do other imputations

# TODO create 2018 -> 2053 PUFs


