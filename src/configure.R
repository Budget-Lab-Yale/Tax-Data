#--------------------------------------------
# configure.R
# 
# Sets runtime parameters, I/O folders, etc
#--------------------------------------------

#----------------
# Read runscript
#----------------

runscript_id = 'baseline'
runscript = file.path('./config/runscripts/') %>% 
  paste0(runscript_id, '.yaml') %>% 
  read_yaml()


#-----------------
# Set output path
#-----------------

# Read versioning info
output_roots       = read_yaml('./output_roots.yaml')
interface_versions = read_yaml('./interface_versions.yaml')

# Get current date/time to vintage this run
st      = Sys.time()
vintage = paste0(year(st), 
                 month(st) %>%
                   paste0('0', .) %>% 
                   str_sub(-2), 
                 day(st) %>%
                   paste0('0', .) %>% 
                   str_sub(-2), 
                 hour(st) %>%
                   paste0('0', .) %>% 
                   str_sub(-2))

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

# Read target info
target_info = paste0(runscript_id, '.csv') %>% 
  file.path('./config/target_info', .) %>% 
  read_csv()

# Read variable guide: the full set of variables used for tax simulator input, 
# including crosswalk with PUF name if applicable, description and source, 
# whether the variable is an income/dollar amount variable vs a categorical attribute,
# an instructions for growing it historically and into the future
variable_guide = read_csv('./resources/variable_guide.csv')

# Read variable-table crosswalk: shows source for each variable available in 
# SOI targets, and shows whether it's available by AGI or not
variable_table_crosswalk = read_csv('./resources/variable_table_crosswalk.csv')
