#---------------------------
# project_puf.R 
# 
# TODO
#---------------------------



#------------------------------
# Clean and save base year PUF
#------------------------------

tax_units %>% 
  select(all_of(variable_guide$variable)) 
  # write_csv(file.path(output_path, ..., 'tax_units_2017'.csv))


#-------------
# Project PUF
#-------------

# Read macro projections
macro_projections = bind_rows(
  read_csv(file.path(interface_paths$`Macro-Projections`, 'historical.csv')), 
  read_csv(file.path(interface_paths$`Macro-Projections`, 'projections.csv'))
)

macro_projections
