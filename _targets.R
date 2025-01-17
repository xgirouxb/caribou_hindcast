# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  # Packages that your targets need for their tasks.
  packages = c(
    # File/folder control tools
    "fs", "readr", "janitor",
    # Data manipulation tools
    "dplyr", "tidyr", "magrittr", "tibble", "purrr", "furrr",
    # Spatial data tools
    "sf", "lwgeom", "quadkeyr", "geojsonsf",
    # Data visualization and notebook tools
    "ggplot2", "plotly", "rmarkdown"
  ),
  # Default data format
  format = "rds",
)

# Run all the R scripts in the R/ folder
tar_source()

# Target list
list(
  # Define study area
  tar_target(
    name = study_area,
    command = define_study_area()
  ),
  # Knit notebook showing details of study area creation
  tar_render(
    name = notebook_study_area,
    path = "notebooks/notes_define_study_area.Rmd"
  ),
  # Import Microsoft global building footprint with 2km buffer for study area
  tar_target(
    name = building_footprint,
    command = get_building_footprint(study_area)
  ),
  # Import, project, and filter AQRéseau+ roads to study area
  tar_target(
    name = aqrp_roads,
    command = get_quebec_aqrp_roads(study_area)
  ),
  # Import, project, and filter ORN element roads to study area
  tar_target(
    name = orn_roads,
    command = get_ontario_orn_roads(study_area)
  ),
  # Import, project, and filter MNR element roads to study area
  tar_target(
    name = mnr_roads,
    command = get_ontario_mnr_roads(study_area)
  ),
  # Preprocess Quebec roads
  tar_target(
    name = quebec_roads,
    command = preprocess_quebec_roads(
      aqrp_roads,
      n_workers = round(parallelly::availableCores()*0.5)
    )
  )
)