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

# Run all the R scripts in the R/ folder
tar_source()

# Target list
list(
  # Define study area
  tar_target(
    name = study_area,
    command = define_study_area(),
    format = "rds",
    repository = "local"
  ),
  # Knit notebook showing details of study area creation
  tar_render(
    name = notebook_study_area,
    path = "notebooks/notes_define_study_area.Rmd"
  )
)