#' Explode MULTILINESTRINGS into individual segments
#'
#' @param sf_lines An \code{sf} "LINESTRING", "MULTILINESTRING", or "GEOMETRY" 
#'                 object with lines to explode.
#'
#' @return An \code{sf} "LINESTRING" object of exploded LINESTRING segments.
#'
explode_lines <- function(sf_lines){
  
  # Cast to MULTILINESTRING to get consistent geometry types
  sf_lines <- sf::st_cast(sf_lines, "MULTILINESTRING")
  
  # Extract all points from sf object, don't warn about subgeometry attributes
  vertices <- sf::st_cast(sf_lines, "POINT", warn = FALSE)
  
  # Split the MULTILINESTRING using the points
  split_linestring <- sf_lines %>%
    # Split MULTILINESTRING into individual lines (stored as geometry collection)
    lwgeom::st_split(vertices) %>% 
    # Extract individual LINESTRING for each line 
    sf::st_collection_extract("LINESTRING")
  
  # Return exploded LINESTRING sf object
  return(split_linestring)
}