# run all of the R scripts
library(here)

source(here("code", "00_dependencies.R"))
source(here("code", "01_load_data.R"))
source(here("code", "02_clean_data.R"))
source(here("code", "02.1_create_variables.R"))
source(here("code", "03_descriptive_stats.R"))
source(here("code", "04_exploratory_data_vis.R"))
source(here("code", "05_model.R"))

source(here("code", "99_run_everything.R"))
