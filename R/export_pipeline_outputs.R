export_geotiffs <- function(
    layer,
    output_filename,
    append_layer_name = FALSE,
    gdal_datatype
) {
  
  # Create output directory if it doesn't exist
  if(!fs::dir_exists(fs::path_dir(output_filename))) { 
    fs::dir_create(fs::path_dir(output_filename)) 
  }

  # Append rast layer name (for layers with multiple years)
  if (append_layer_name) {
    output_filename <- fs::path(
      # Directory
      fs::path_dir(output_filename),
      # New output filename
      paste0(
        # Conserve name
        fs::path_ext_remove(fs::path_file(output_filename)),  
        # Append layer names
        "_", names(layer),    
        # Add original extension
        ".", fs::path_ext(output_filename)
      )
    )
  }    
  
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