export_geotiffs <- function(
    layer,
    output_dir,
    append_layer_name = FALSE,
    gdal_datatype
) {
  
  # Create output directory if it doesn't exist
  if(!fs::dir_exists(output_dir)) { fs::dir_create(output_dir) }
  
  # Create an output file name
  output_filename <- paste0(
    output_dir,
    # Extract name of input target
    deparse(substitute(layer)),
    # Append rast layer name (for layers with multiple years)
    if (append_layer_name) {
      paste0("_", names(layer))
    },
    ".tif"
  )
  
  # Write raster to output folder
  terra::writeRaster(
    x = layer,
    filename = output_filename,
    datatype = gdal_datatype,
    # If this target is running, upstream targets are outdated
    overwrite = TRUE
  )
  
  # Return output_filename to track changes using targets
  return(output_filename)
}