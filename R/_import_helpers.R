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
    sf_obj <- sf_obj %>% 
      # Remove M in XYM geometries if present (avoids sf complaints)
      sf::st_zm() %>%
      # Filter to study area
      sf::st_filter(aoi)
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

# Function to download compressed tables from URLs
get_tbl_from_url <- function(tbl_url, tbl_glob = "*.csv", delimiter = NULL, file = NULL){
  
  # Temp folders
  temp <- tempfile(); temp_unzipped <- tempfile()
  
  # Download zipped table from URL
  download.file(tbl_url, destfile = temp, quiet = TRUE)
  
  # Unzip to second temp directory
  unzip(temp, exdir = temp_unzipped)
  
  # Path to table (sometimes buried in other archived folders and files)
  tbl_path <- temp_unzipped %>% 
    fs::dir_ls(recurse = TRUE, type = "file", glob = tbl_glob)
  
  # Read table
  tbl_obj <- readr::read_delim(
    file = tbl_path,
    delim = delimiter,
    show_col_types = FALSE
  )
  
  # Store local copy for notebooks
  if(!is.null(file)) {
    
    # Create directory if it doesn't exist
    if(!fs::dir_exists(fs::path_dir(file))) {fs::dir_create(fs::path_dir(file))}
    
    # Write to file
    suppressWarnings(readr::write_csv(x = tbl_obj, file = file, progress = FALSE))
  }
  
  # Detach temp files
  unlink(temp); unlink(temp_unzipped)
  
  # Return 
  return(tbl_obj)
}