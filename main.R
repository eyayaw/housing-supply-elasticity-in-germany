# Do not run this script all at once! This is just for documenting the steps.

## Running these scripts in order may help to replicate the project.
## These scripts handle tasks ranging from data downloading to cleaning and running the regression.

# 1 ----
## download shapes files and administrative areas
source("script/downloaders/get_admin-areas_shapes.R")
source("script/downloaders/download_admin-areas_DESTATIS.R")
source("script/downloaders/get_reference-districts_inkar-bbsr.R")

# 2 ----
source("script/cleaning/clean_stata_files.R")

# 3 ----
source("script/cleaning/clean_house_prices.R")
source("script/estimation/hedonic/hedonic.R")

# 4 ----
# make sure this runs: source("script/cleaning/territorial_changes.R")
source("script/cleaning/clean_regional-atlas_flat-CSV.R")
# permits and completions
source("script/cleaning/tidy_regional-atlas_CSV.R")
source("script/downloaders/get_construction_costs.R")

# 5 ----
## CBDs
source("script/cleaning/clean_population_municipality.R")
source("script/construct_CBDs.R")

# 6 ----
source("script/downloaders/download_dtm.R")
source("script/constraints/compute_TRI.R")
# supply constraints: slope, undevelopable fraction
source("script/downloaders/get_landcover_classes.R")
source("script/cleaning/clean_land-cover_raster-EEA-CLC.R")
source("script/constraints/compute_supply_constraints.R")

# 7 ----
## Bartik shocks
source("script/cleaning/clean_employment_data.R")
source("script/compute_Bartik_shocks.R")

# 8 -----
source("script/merge_datasets.R")
source("script/cleaning/clean_construction_costs.R")
source("script/estimation/prepare_for_estimation.R")

# 9 ----
source("script/estimation/estimations.R")

# 10 ----
source("script/visualization/viz_elev-slope-undev.R")
