#--------------------------------------------
# configure.R
# 
# Sets runtime parameters, I/O folders, etc
#--------------------------------------------

#----------------
# Read runscript
#----------------

runscript_id = Sys.getenv('TAX_DATA_RUNSCRIPT_ID', unset = 'baseline')
runscript = file.path('./config/runscripts', paste0(runscript_id, '.yaml')) %>% 
  read_yaml()

write_locally_override = Sys.getenv('TAX_DATA_WRITE_LOCALLY', unset = '')
if (write_locally_override != '') {
  runscript$runtime_options$write_locally = tolower(write_locally_override) %in% c('1', 'true', 't', 'yes', 'y')
}

user_id_override = Sys.getenv('TAX_DATA_USER_ID', unset = '')
if (user_id_override != '') {
  runscript$runtime_options$user_id = user_id_override
}


#-----------------
# Set output path
#-----------------

# Read versioning info
output_roots       = read_yaml('./config/interfaces/output_roots.yaml')
interface_versions = read_yaml('./config/interfaces/interface_versions.yaml')

# Allow local path overrides without editing tracked config files
local_root_override = Sys.getenv('TAX_DATA_LOCAL_ROOT', unset = '')
if (local_root_override != '') {
  output_roots$local = local_root_override
}

production_root_override = Sys.getenv('TAX_DATA_PRODUCTION_ROOT', unset = '')
if (production_root_override != '') {
  output_roots$production = production_root_override
}

# Get current date/time to vintage this run
vintage = format(Sys.time(), '%Y%m%d%H')

# Set additional boolean parameters
do_lp           = 0
estimate_models = 0

# Set output root
if (runscript$runtime_options$write_locally) {
  output_root = file.path(output_roots$local, runscript$runtime_options$user_id)
} else {
  output_root = output_roots$production
}

# Resolve dependency reads from the active run context by default. Public/local
# users can also supply an explicit input root when keeping inputs separate
# from outputs.
dependency_root_override = Sys.getenv('TAX_DATA_INPUT_ROOT', unset = '')
if (dependency_root_override != '') {
  dependency_root = dependency_root_override
} else if (runscript$runtime_options$write_locally) {
  dependency_root = output_roots$local
} else {
  dependency_root = output_roots$production
}

# Set output path
output_path = file.path(
  output_root, 
  interface_versions$`Tax-Data`$type, 
  '/Tax-Data', 
  paste0('v', interface_versions$`Tax-Data`$version), 
  vintage, 
  runscript_id
)

# Create output path 
dir.create(output_path, recursive = T)


#-------------------------
# Write dependencies file
#-------------------------

interface_versions %>% 
  map2(.y = names(.),
       .f = ~ tibble(interface = .y,
                     version   = .x$version, 
                     vintage   = runscript$dependency_info[[.y]]$vintage, 
                     scenario  = runscript$dependency_info[[.y]]$scenario)
  ) %>% 
  bind_rows() %>% 
  filter(interface != 'Tax-Data') %>% 
  mutate(ID = runscript_id) %>% 
  relocate(ID) %>% 
  write_csv(
    file.path(
      output_root, 
      interface_versions$`Tax-Data`$type, 
      '/Tax-Data', 
      paste0('v', interface_versions$`Tax-Data`$version), 
      vintage,
      'dependencies.csv'
    )
  )


#-------------------------------------
# Set data dependency input filepaths
#-------------------------------------

interface_paths = interface_versions %>% 
  map2(.y = names(.),
       .f = ~ file.path(
         dependency_root,
         .x$type,
         .y,
         paste0('v', .x$version), 
         runscript$dependency_info[[.y]]$vintage, 
         runscript$dependency_info[[.y]]$scenario
       )
  )

# Read target info
target_info = paste0(runscript_id, '.csv') %>% 
  file.path('./config/target_info', .) %>% 
  read_csv()

# Read variable guide: the full set of variables used for tax simulator input, 
# including crosswalk with PUF name if applicable, description and source, 
# whether the variable is an income/dollar amount variable vs a categorical attribute,
# an instructions for growing it historically and into the future
variable_guide = paste0(runscript_id, '.csv') %>% 
  file.path('./config/variable_guide', .) %>% 
  read_csv(show_col_types = F)

# Read variable-table crosswalk: shows source for each variable available in 
# SOI targets, and shows whether it's available by AGI or not
variable_table_crosswalk = read_csv('./resources/variable_table_crosswalk.csv')
