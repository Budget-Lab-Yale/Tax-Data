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

# 1) read and process PUF


# 3) create 2017 PUF

# 4) create 2019 targets for censored vars

# 5) create 2019 PUF 

# 6) add nonfilers and do other imputations

# 7) create 2020 -> 2053 PUFs


