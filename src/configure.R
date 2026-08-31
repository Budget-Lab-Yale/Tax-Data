#--------------------------------------------
# configure.R
# 
# Sets runtime parameters, I/O folders, etc
#--------------------------------------------

#----------------
# Read runscript
#----------------

runscript_id = 'baseline'
runscript = file.path('./config/runscripts', paste0(runscript_id, '.yaml')) %>% 
  read_yaml()


#-----------------
# Set output path
#-----------------

# Read versioning info
output_roots       = read_yaml('./config/interfaces/output_roots.yaml')
interface_versions = read_yaml('./config/interfaces/interface_versions.yaml')

# Get current date/time to vintage this run
vintage = format(Sys.time(), '%Y%m%d%H')

# Set additional boolean parameters. Default to a full from-scratch run
# (re-solve LP, retrain all imputation models). Override via env for a
# cache-loading run — e.g. a wealth-only iteration that reuses the Phase-1/2
# fits + the LP solve unchanged:
#   TAXDATA_DO_LP=0 TAXDATA_ESTIMATE_MODELS=0 Rscript src/main.R
# A downstream wrapper that assigns these after sourcing configure (e.g.
# main_placeholder.R) still overrides whatever is set here.
do_lp           = as.integer(Sys.getenv('TAXDATA_DO_LP',           unset = '1'))
estimate_models = as.integer(Sys.getenv('TAXDATA_ESTIMATE_MODELS', unset = '1'))

# Set output root
if (runscript$runtime_options$write_locally) {
  output_root = file.path(output_roots$local, runscript$runtime_options$user_id)
} else {
  output_root = output_roots$production
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
         output_roots$production,
         .x$type,
         .y,
         paste0('v', .x$version), 
         runscript$dependency_info[[.y]]$vintage, 
         runscript$dependency_info[[.y]]$scenario
       )
  )

#---------------------------------------------------------------------------
# Every declared interface must resolve to exactly one existing directory.
#
# Without this, an interface declared in interface_versions.yaml but absent
# from the runscript's dependency_info yields NULL vintage and scenario, and
# file.path() propagates zero length rather than erroring: the path becomes
# character(0). Downstream, read_csv(character(0)) returns a 0x0 tibble, so
# every assertion written against the missing data passes VACUOUSLY
# (all(logical(0)) is TRUE, min(NULL) is Inf) and bind_rows appends nothing.
# The pipeline then completes having silently dropped a whole population.
#
# 'Tax-Data' is this model's own output interface and has no dependency row,
# so it is excluded. Interfaces declared but not consumed by this runscript
# are reported rather than tolerated -- a declared interface with no vintage
# is a configuration error, not a default.
#---------------------------------------------------------------------------

unresolved = interface_paths %>%
  keep(~ length(.x) != 1) %>%
  names() %>%
  setdiff('Tax-Data')
if (length(unresolved) > 0) {
  stop('Interfaces declared in config/interfaces/interface_versions.yaml but ',
       'missing a vintage/scenario in config/runscripts/', runscript_id,
       '.yaml: ', paste(unresolved, collapse = ', '),
       '. Add them, or remove them from interface_versions.yaml.')
}

missing_dirs = interface_paths[setdiff(names(interface_paths), 'Tax-Data')] %>%
  keep(~ !dir.exists(.x))
if (length(missing_dirs) > 0) {
  stop('Interface paths do not exist:\n',
       paste0('  ', names(missing_dirs), ': ', unlist(missing_dirs),
              collapse = '\n'))
}

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
