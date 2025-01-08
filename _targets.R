# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  # Packages that your targets need for their tasks.
  packages = c(
    # File/folder control tools
    "fs", "readr",
    # Data manipulation tools
    "dplyr", "magrittr", "tibble",
    # Spatial data tools
    "sf",
    # Data visualization and notebook tools
    "ggplot2", "plotly", "rmarkdown"
  ),
  # Default data format
  format = "rds",
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  # Define study area
  tar_target(
    name = study_area,
    command = define_study_area(),
    format = "rds",
    repository = "local"
  )
)