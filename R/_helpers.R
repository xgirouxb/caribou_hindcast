# Function to download compressed vector layer datasets from URLs
get_shp_from_url <- function(layer_url, aoi = NULL, shp_glob = "*.shp", file = NULL){
  
  # Temp folders
  temp <- tempfile(); temp_unzipped <- tempfile()
  
  # Download zipped shapefile from URL
  download.file(layer_url, destfile = temp, quiet = TRUE)
  
  # Unzip to second temp directory
  unzip(temp, exdir = temp_unzipped)
  
  # Path to shapefile (sometimes buried in archive folders)
  shp_path <- temp_unzipped %>% 
    fs::dir_ls(recurse = TRUE, type = "file", glob = shp_glob)
  
  # Read simple feature
  sf_obj <- sf::read_sf(shp_path) %>% 
    sf::st_transform(study_proj)
  
  # Filter to bounds of area of interest
  if(!is.null(aoi)) {
    sf_obj <- sf::st_filter(sf_obj, aoi)
  }
  
  # Store local copy for notebooks
  if(!is.null(file)) {
    
    # Create directory if it doesn't exist
    if(!fs::dir_exists(fs::path_dir(file))) {fs::dir_create(fs::path_dir(file))}
    
    # Write to file
    suppressWarnings(sf::write_sf(obj = sf_obj, dsn = file, quiet = TRUE))
  }
  
  # Detach temp files
  unlink(temp); unlink(temp_unzipped)
  
  # Return 
  return(sf_obj)
}