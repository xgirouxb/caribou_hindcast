# Caribou hindcast - data pipeline for model inputs

## Getting started

The data pipeline in this repo is managed by the `targets` package and all package dependencies are managed by `renv`. Each step of the pipeline is described and defined in the `_targets.R` script, which can be executed by running `targets::tar_make()` in the R console or launching the `_make.R` script using the background job tool in `RStudio`. Run the `renv::restore()` function to install all the package dependencies prior to running the pipeline the first time.

## Data sources

All the data used in this pipeline is designed to be retrieved from URLs and stored locally in a target to ensure reproducible results. Data sources are described in the `_data_sources.R`. The only exception is a table listing the years of logging disturbances extracted from the pixel distributions in the CanLaD data products within variable buffers around each road. This table was produced using `Google Earth Engine`'s JavaScript API with the `extract_canlad_disturbance_years.js` script stored in the `/js` folder.
