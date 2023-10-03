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
  output_root = output_roots$local
}

# Set output path
output_path = file.path(
  output_root, 
  '/Tax-Data', 
  interface_versions$`Tax-Data`$type, 
  paste0('v', interface_versions$`Tax-Data`$version), 
  vintage, 
  runscript_id
)

# Create output path 
dir.create(output_path, recursive = T)


#-------------------------------------
# Set data dependency input filepaths
#-------------------------------------

interface_paths = interface_versions %>% 
  map2(.y = names(.),
       .f = ~ file.path(
         output_roots$production,
        .y,
        .x$type,
        paste0('v', .x$version), 
        runscript$dependency_info[[.y]]$vintage, 
        runscript$dependency_info[[.y]]$scenario
        )
      )


#------------------
# Read target info
#------------------

target_info = read_yaml('./config/target_info/historical.yaml')


