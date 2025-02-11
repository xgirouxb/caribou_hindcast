#' Check if provided string is a URL
#'
#' @param string A string
#'
#' @return Logical TRUE or FALSE
#'
is_url <- function(string) {
  stringr::str_detect(string, "https|http|ftp")
}

#' Download and unzip archives from a URL to local temporary directory
#'
#' @param archive_url A URL string with the address of the archived file.
#'
#' @return A string with the local directory where the unzipped file is cached.
#'
get_archive_from_url <- function(archive_url) {
  
  # Check if supplied URL string is valid
  if(!is_url(archive_url)) { stop("Invalid URL") }
  
  # Create temp folders
  temp_zip <- tempfile(); temp_unzipped <- tempfile()
  
  # Download zipped archive from URL
  download.file(archive_url, destfile = temp_zip, quiet = TRUE)
  
  # Unzip to second temp directory
  unzip(temp_zip, exdir = temp_unzipped)
  
  # Return temp directory
  return(temp_unzipped)
}

#' Read and filter simple features layers from a local temp directory or URL
#'
#' @param sf_source A URL or local directory where the sf is stored.
#' @param proj An object of class crs with the target projection (see ?sf::st_crs).
#' @param sf_aoi An sf object defining an area of interest to filter the output sf. 
#' @param sf_glob A wildcard passed on to grep() to filter paths (see ?fs::dir_ls).
#' @param file File to write to.
#' 
#' @return An sf object, projected and filtered to aoi if specified.
#'
get_sf_from_source <- function(sf_source, proj = study_proj, sf_aoi = NULL, sf_glob = "*.shp", file = NULL) {
  
  # If the provided source is a URL, download and unzip to local temp directory
  if(is_url(sf_source)) { sf_source <- get_archive_from_url(sf_source) }
  
  # Get local path to vector layer of interest
  sf_path <- sf_source %>% 
    # Check all subfolders in sf_source for path to simple feature of interest
    fs::dir_ls(recurse = TRUE, type = "file", glob = sf_glob)
  
  # Read simple feature object
  sf_obj <- sf::read_sf(sf_path) %>%
    sf::st_transform(proj)
  
  # Filter to area of interest bounds
  if(!is.null(sf_aoi)) {
    sf_obj <- sf_obj %>% 
      # Remove M in XYM geometries if present (avoids sf complaints)
      sf::st_zm() %>%
      # Filter to area of interest
      sf::st_filter(sf_aoi)
  }
  
  # Write to file if argument is provided
  if(!is.null(file)) {
    
    # Create directory if it doesn't exist
    if(!fs::dir_exists(fs::path_dir(file))) {fs::dir_create(fs::path_dir(file))}
    
    # Write to file
    suppressWarnings(sf::write_sf(obj = sf_obj, dsn = file, quiet = TRUE))
  }
  
  # Return 
  return(sf_obj)
}

#' Read a table from a local temp directory or URL
#'
#' @param tbl_source A URL or local directory where the table is stored.
#' @param tbl_glob A wildcard passed on to grep() to filter paths (see ?fs::dir_ls)
#' @param delimiter The delimiter chr used in table (see ?readr::read_delim)
#' @param file File to write to
#' 
#' @return An sf object
#'
get_tbl_from_source <- function(tbl_source, tbl_glob = "*.csv", delimiter = NULL, file = NULL) {
  
  # If the provided source is a URL, download and unzip to local temp directory
  if(is_url(tbl_source)) { tbl_source <- get_archive_from_url(tbl_source) }
  
  # Get path to tbl of interest
  tbl_path <- tbl_source %>% 
    # Check all subfolders in temp_dir for path to tbl of interest
    fs::dir_ls(recurse = TRUE, type = "file", glob = tbl_glob)
  
  # Read table
  tbl_obj <- readr::read_delim(
    file = tbl_path,
    delim = delimiter,
    show_col_types = FALSE,
    guess_max = Inf
  )
  
  # Write to file if argument is provided
  if(!is.null(file)) {
    
    # Create directory if it doesn't exist
    if(!fs::dir_exists(fs::path_dir(file))) { 
      fs::dir_create(fs::path_dir(file))
    }
    
    # Write to file
    suppressWarnings(readr::write_csv(x = tbl_obj, file = file, progress = FALSE))
  }
  
  # Return 
  return(tbl_obj)
}