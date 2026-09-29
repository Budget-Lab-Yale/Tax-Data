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

# Get current date/time to vintage this run. TAXDATA_VINTAGE names it
# instead, which is what an A/B needs: the stamp is only to the HOUR, so two
# runs launched together land in one directory and the second silently
# overwrites the first (hit 2026-09-13, lost a comparison run). Name the two
# arms and they cannot collide.
vintage = Sys.getenv('TAXDATA_VINTAGE', unset = format(Sys.time(), '%Y%m%d%H'))
if (!grepl('^[A-Za-z0-9_]+$', vintage)) {
  stop('configure.R: TAXDATA_VINTAGE must be alphanumeric/underscore, got "',
       vintage, '"', call. = FALSE)
}

# Set additional boolean parameters. Default to a full from-scratch run
# (re-solve LP, retrain all imputation models). Override via env for a
# cache-loading run — e.g. a wealth-only iteration that reuses the Phase-1/2
# fits + the LP solve unchanged:
#   TAXDATA_DO_LP=0 TAXDATA_ESTIMATE_MODELS=0 Rscript src/main.R
# A downstream wrapper that assigns these after sourcing configure (e.g.
# main_placeholder.R) still overrides whatever is set here.
do_lp           = as.integer(Sys.getenv('TAXDATA_DO_LP',           unset = '1'))
estimate_models = as.integer(Sys.getenv('TAXDATA_ESTIMATE_MODELS', unset = '1'))

# Create the cache directories. resources/cache is gitignored, so a fresh
# checkout has the path in .gitignore and none of its subdirectories, and the
# code below writes into them without creating them: a from-scratch rebuild on
# a clean clone died at the LP stage with "cannot open file
# 'resources/cache/lp/weight_deltas.rds': No such file or directory" (hit
# 2026-09-21). Creating them here is what makes "the cache is rebuildable from
# code" true rather than true-given-an-undocumented-mkdir.
for (cache_dir in file.path('resources/cache', c('lp', 'qrf'))) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
}

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
# Refuse to write into a vintage that already holds output.
#
# The vintage is stamped to the HOUR, so two runs started in the same hour
# resolve to the same directory and the later one silently overwrites the
# earlier -- which is exactly what happened on 2026-09-13, when two Phase-1
# runs launched together for an A/B comparison both wrote 2026091312 and the
# first result was lost with no error. Tax-Simulator stamps to the minute;
# this repo does not, and changing the format would change the shape of every
# future vintage string, so the guard is here instead.
#
# TAXDATA_ALLOW_VINTAGE_OVERWRITE=1 permits it, for the deliberate case of
# resuming or re-writing a vintage on purpose.
if (dir.exists(output_path) &&
    length(list.files(output_path)) > 0 &&
    Sys.getenv('TAXDATA_ALLOW_VINTAGE_OVERWRITE', unset = '0') != '1') {
  stop('output vintage already has content: ', output_path,
       '\n  The vintage is stamped to the hour, so a run started in the same ',
       'hour as another lands here and would overwrite it.',
       '\n  Wait for the next hour, or set ',
       'TAXDATA_ALLOW_VINTAGE_OVERWRITE=1 to write anyway.')
}
dir.create(output_path, recursive = T, showWarnings = FALSE)


#--------------------------------------------------
# Resolve the pinned interfaces; write dependencies
#--------------------------------------------------

# Shared with src/main_state_weights.R (src/resolve_interfaces.R): the
# dependency table, each pinned interface's directory, and the guards --
# nothing pinned that is undeclared, nothing declared that no runscript pins,
# every pinned directory present.
source('./src/resolve_interfaces.R')
resolved = resolve_interfaces(runscript, runscript_id, interface_versions, output_roots,
                              own_interface   = 'Tax-Data',
                              pinned_anywhere = interfaces_pinned_anywhere())

resolved$dependencies %>%
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

interface_paths = resolved$paths

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
