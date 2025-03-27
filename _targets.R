# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(geotargets)

# Set target options:
tar_option_set(
  # Packages that your targets need for their tasks.
  packages = c(
    # File/folder control tools
    "fs", "readr", "janitor",
    # Data manipulation tools
    "dplyr", "tidyr", "magrittr", "tibble", "purrr", "furrr", "forcats",
    # Spatial data tools
    "sf", "lwgeom", "quadkeyr", "geojsonsf",
    # Data visualization tools
    "ggplot2", "plotly"
  ),
  # Default data format
  format = "rds",
  # Run garbage collection before launching a target
  garbage_collection = TRUE
)

# Set geotargets options:
geotargets_option_set(gdal_raster_driver = "GTiff")

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
  # Preprocess Québec roads
  tar_target(
    name = quebec_roads,
    command = preprocess_quebec_roads(
      aqrp_roads,
      building_footprint,
      n_workers = round(parallelly::availableCores()*0.5)
    )
  ),
  # Knit notebook illustrating preprocessing steps for Québec roads
  tar_render(
    name = notebook_preprocess_quebec_roads,
    path = "notebooks/notes_preprocess_quebec_roads.Rmd"
  ),
  # Preprocess Ontario roads
  tar_target(
    name = ontario_roads,
    command = preprocess_ontario_roads(
      orn_roads,
      mnr_roads,
      n_workers = round(parallelly::availableCores()*0.5)
    )
  ),
  # Knit notebook illustrating preprocessing steps for Ontario roads
  tar_render(
    name = notebook_preprocess_ontario_roads,
    path = "notebooks/notes_preprocess_ontario_roads.Rmd"
  ),
  # Estimate unpaved road construction years
  tar_target(
    name = unpaved_construction_years,
    command = estimate_road_construction_years(
      quebec_roads,
      ontario_roads,
      canlad_road_construction_years
    )
  ),
  # Compute paved road density
  tar_terra_rast(
    name = paved_road_density,
    command = compute_paved_road_density(
      study_area,
      quebec_roads,
      ontario_roads,
      n_workers = round(parallelly::availableCores()*0.25)
    )
    # # Use once geotargets is updated, currently defaults to INT4S
    # # see https://github.com/njtierney/geotargets/pull/137
    # datatype = "INT2U"
  ),
  # Add study years as target to allow dynamic branching
  tar_target(
    name = study_years,
    command = time_steps
  ),
  # Compute unpaved road density across all study years
  # NB Requires targets established by dynamic branching
  tar_terra_rast(
    name = unpaved_road_density,
    command = compute_unpaved_road_density(
      study_area,
      quebec_roads,
      ontario_roads,
      unpaved_construction_years,
      year = study_years,
      n_workers = round(parallelly::availableCores()*0.25)
    ),
    # # Use once geotargets is updated, currently defaults to INT4S
    # # see https://github.com/njtierney/geotargets/pull/137
    # datatype = "INT2U",
    pattern = map(study_years)
  )
)