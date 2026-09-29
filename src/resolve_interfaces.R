#------------------------------------------------------------------------------
# resolve_interfaces.R
#
# Interface resolution shared by every entry point: the dependency table a
# runscript pins, the on-disk path of each pinned interface, and the guards.
# Pure -- reads no files beyond what it is handed, writes nothing. configure.R
# (main.R and its siblings) and src/main_state_weights.R call it and each
# writes its own dependencies.csv under its own output interface.
#
# Extracted from configure.R on 2026-09-28 (Phase 4, D7) so a second entry
# point could resolve a different set of pins without minting a Tax-Data
# vintage. One semantic change, deliberate: "declared but unpinned" used to
# be judged against the one runscript being run; with two entry points that
# pin different interfaces it is judged against ALL runscripts in
# config/runscripts/, which keeps the check's purpose -- no dead
# declarations -- without forcing main.R's runscript to pin the state-weights
# producer's inputs.
#------------------------------------------------------------------------------

# runscript          the parsed runscript yaml (needs $dependency_info)
# runscript_id       its name, for the dependencies table's ID column
# interface_versions config/interfaces/interface_versions.yaml, parsed
# output_roots       config/interfaces/output_roots.yaml, parsed
# own_interface      this entry point's output interface (no dependency row)
# pinned_anywhere    interface names pinned by any runscript in
#                    config/runscripts/ (for the dead-declaration guard)
#
# Returns list(dependencies = tibble(ID, interface, version, vintage, scenario)
#              in interface_versions order, paths = named list of directories)
resolve_interfaces = function(runscript, runscript_id, interface_versions, output_roots,
                              own_interface = 'Tax-Data', pinned_anywhere) {

  declared = setdiff(names(interface_versions), own_interface)
  pinned   = names(runscript$dependency_info)

  # A pin with no declaration has no type or version to build a path from
  undeclared = setdiff(pinned, names(interface_versions))
  if (length(undeclared) > 0) {
    stop('config/runscripts/', runscript_id, '.yaml pins interfaces not declared in ',
         'config/interfaces/interface_versions.yaml: ', paste(undeclared, collapse = ', '))
  }

  #---------------------------------------------------------------------------
  # A declared interface that no runscript pins is a configuration error, not
  # a default. Without this, it would yield NULL vintage and scenario, and
  # file.path() propagates zero length rather than erroring: the path becomes
  # character(0). Downstream, read_csv(character(0)) returns a 0x0 tibble, so
  # every assertion written against the missing data passes VACUOUSLY
  # (all(logical(0)) is TRUE, min(NULL) is Inf) and bind_rows appends nothing.
  # The pipeline then completes having silently dropped a whole population.
  #---------------------------------------------------------------------------
  dead = setdiff(declared, pinned_anywhere)
  if (length(dead) > 0) {
    stop('Interfaces declared in config/interfaces/interface_versions.yaml but pinned by ',
         'no runscript in config/runscripts/: ', paste(dead, collapse = ', '),
         '. Add a vintage/scenario to the runscript that consumes them, or remove them.')
  }

  # The dependency table, in interface_versions order, for the pinned set
  dependencies = interface_versions[intersect(names(interface_versions), pinned)] %>%
    map2(.y = names(.),
         .f = ~ tibble(interface = .y,
                       version   = .x$version,
                       vintage   = runscript$dependency_info[[.y]]$vintage,
                       scenario  = runscript$dependency_info[[.y]]$scenario)) %>%
    bind_rows() %>%
    mutate(ID = runscript_id) %>%
    relocate(ID)

  # Paths: always the production root, never the local one
  paths = interface_versions[intersect(names(interface_versions), pinned)] %>%
    map2(.y = names(.),
         .f = ~ file.path(output_roots$production,
                          .x$type,
                          .y,
                          paste0('v', .x$version),
                          runscript$dependency_info[[.y]]$vintage,
                          runscript$dependency_info[[.y]]$scenario))

  unresolved = paths %>% keep(~ length(.x) != 1) %>% names()
  if (length(unresolved) > 0) {
    stop('config/runscripts/', runscript_id, '.yaml pins ', paste(unresolved, collapse = ', '),
         ' without both a vintage and a scenario.')
  }
  missing_dirs = paths %>% keep(~ !dir.exists(.x))
  if (length(missing_dirs) > 0) {
    stop('Interface paths do not exist:\n',
         paste0('  ', names(missing_dirs), ': ', unlist(missing_dirs), collapse = '\n'))
  }

  list(dependencies = dependencies, paths = paths)
}

# The interfaces pinned by any runscript under config/runscripts/
interfaces_pinned_anywhere = function(dir = './config/runscripts') {
  list.files(dir, pattern = '\\.ya?ml$', full.names = TRUE) %>%
    map(~ names(read_yaml(.x)$dependency_info)) %>%
    unlist() %>%
    unique()
}
