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
  
  # -------------------------------------------------------------------------- #
  # Importing data required for pipeline
  # -------------------------------------------------------------------------- #
  
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
  
  # -------------------------------------------------------------------------- #
  # Preprocessing road data prior to analysis
  # -------------------------------------------------------------------------- #
  
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
  
  # -------------------------------------------------------------------------- #
  # Computing road habitat layers 
  # -------------------------------------------------------------------------- #
  
  # Compute paved road density
  tar_terra_rast(
    name = paved_road_density,
    command = compute_paved_road_density(
      study_area,
      quebec_roads,
      ontario_roads,
      n_workers = round(parallelly::availableCores()*0.25)
    ),
    # Unsigned integer
    datatype = "INT2U"
  ),
  # Compute paved road distance
  tar_terra_rast(
    name = paved_road_distance,
    command = compute_paved_road_distance(
      study_area,
      quebec_roads,
      ontario_roads,
      n_workers = round(parallelly::availableCores()*0.25)
    ),
    # Unsigned integer
    datatype = "INT2U"
  ),
  # Compute residual habitat suitability in paved road zone of influence
  tar_terra_rast(
    name = paved_roads_zoi_residual_suitability,
    command = compute_paved_roads_zoi_residual_suitability(
      paved_road_distance
    )
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
    # Unsigned integer
    datatype = "INT2U",
    # Dynamic branching across study years
    pattern = map(study_years)
  ),
  # Compute unpaved road distance across all study years
  # NB Requires targets established by dynamic branching
  tar_terra_rast(
    name = unpaved_road_distance,
    command = compute_unpaved_road_distance(
      study_area,
      quebec_roads,
      ontario_roads,
      unpaved_construction_years,
      year = study_years,
      n_workers = round(parallelly::availableCores()*0.25)
    ),
    # Unsigned integer
    datatype = "INT2U",
    # Dynamic branching across study years
    pattern = map(study_years)
  ),
  # Compute residual habitat suitability in unpaved road zone of influence
  tar_terra_rast(
    name = unpaved_roads_zoi_residual_suitability,
    command = compute_unpaved_roads_zoi_residual_suitability(
      unpaved_road_distance
    ),
    # Signed float
    datatype = "FLT4S",
    # Map across each layer produced by dynamic branching
    pattern = map(unpaved_road_distance)
  ),
  
  # -------------------------------------------------------------------------- #
  # Export pipeline output layers required for HSM inputs
  # -------------------------------------------------------------------------- #
  
  # Export residual habitat suitability in paved road zone of influence
  tar_target(
    name = output_paved_road_zoi_residual_suitability,
    command = export_geotiffs(
      layer = paved_roads_zoi_residual_suitability,
      output_filename = fs::path(
        "outputs/paved_roads_zoi_residual_suitability/",
        "paved_roads_zoi_residual_suitability.tif"
      ),
      gdal_datatype = "FLT4S"
    ),
    # Track output file path as target
    format = "file"
  ),
  # Export residual habitat suitability in unpaved road zone of influence
  tar_target(
    name = output_unpaved_road_zoi_residual_suitability,
    command = export_geotiffs(
      layer = unpaved_roads_zoi_residual_suitability,
      output_filename = fs::path(
        "outputs/unpaved_roads_zoi_residual_suitability/",
        "unpaved_roads_zoi_residual_suitability.tif"
      ),
      # Append layer name (year) to each exported geotiff
      append_layer_name = TRUE,
      gdal_datatype = "FLT4S"
    ),
    # Map across each layer produced by dynamic branching
    pattern = map(unpaved_roads_zoi_residual_suitability),
    # Track output file path as target
    format = "file"
  ),
  # Export paved road density
  tar_target(
    name = output_paved_road_density,
    command = export_geotiffs(
      # Divide density by 1000 for output in km/km2
      layer = paved_road_density/1000,
      output_filename = fs::path(
        "outputs/paved_road_density/", "paved_road_density.tif"
      ),
      gdal_datatype = "FLT4S"
    ),
    # Track output file path as target
    format = "file"
  ),
  # Export unpaved road density
  tar_target(
    name = output_unpaved_road_density,
    command = export_geotiffs(
      # Divide density by 1000 for output in km/km2
      layer = unpaved_road_density/1000,
      output_filename = fs::path(
        "outputs/unpaved_road_density/", "unpaved_road_density.tif"
      ),
      # Append layer name (year) to each exported geotiff
      append_layer_name = TRUE,
      gdal_datatype = "FLT4S"
    ),
    # Map across each layer produced by dynamic branching
    pattern = map(unpaved_road_density),
    # Track output file path as target
    format = "file"
  )
)