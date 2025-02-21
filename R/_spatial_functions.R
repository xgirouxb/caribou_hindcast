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

#' Compute a line density raster
#'
#' Produces a raster of line densities in the the circular neighbourhood around each cell.
#'
#' @param sf_lines An \code{sf} "LINESTRING" object from which to compute density.
#' @param sf_aoi An \code{sf} "POLYGON" defining the area of interest.
#' @param radius An integer, the radius in metres of the circular neighbourhood.
#' @param template A \code{SpatRaster} to use as a template for output raster.
#'
#' @return A terra \code{SpatRaster} of line density (m/km^2).
#'
line_density <- function(sf_lines, sf_aoi, radius, template) {
  
  # Stop function if projections are mismatched
  if(sf::st_crs(sf_lines) != sf::st_crs(sf_aoi)) {
    stop("st_crs(sf_lines) == st_crs(sfc_aoi) is not TRUE")
  }
  
  # Stop function if units aren't in metres
  if(sf::st_crs(sf_lines)$units != "m") {
    stop("CRS units must be metres")
  }
  
  # If sf_lines is empty, return 0 density raster
  if(nrow(sf_lines) == 0) { 
    
    # Crop raster of 0s to study area
    output_raster <- terra::crop(
      x = terra::subst(template, 1, 0),
      y = terra::vect(sf_aoi),
      mask = TRUE
    )
    
    # Return
    return (output_raster)
  }                      
               
  # Compute length of sf_lines through each pixel, in m
  rast_line_length_m <- terra::rasterizeGeom(
    x = terra::vect(sf_lines),
    y = template,
    fun = "length",
    unit = "m"
  )
  
  # Replace NAs with 0 and mask to template
  rast_line_length_m <- terra::subst(rast_line_length_m, NA, 0) * template
  
  # Define a circular neighbourhood window with supplied radius
  neighbourhood_window <- terra::focalMat(
    x = rast_line_length_m,
    d = radius,
    type = "circle"
  )
  
  # Replace weights by 1 (to count n cells with an intersecting)
  neighbourhood_window[neighbourhood_window > 0] <- 1
  
  # Compute total length of lines within distance "radius" of cell, in m
  neighbourhood_line_length_m <- terra::focal(
    x = rast_line_length_m, 
    w = neighbourhood_window, 
    fun = "sum",
    na.rm = TRUE
  )
  
  # Compute area of circular neighbourhood, in square km
  neighbourhood_area_km2 <- round((pi*radius^2)/10^6, 4)
  
  # Compute line density in km/km2
  output_raster <- neighbourhood_line_length_m/neighbourhood_area_km2
  
  # Crop to aoi
  output_raster <- terra::crop(
    x = output_raster,
    y = terra::vect(sf_aoi),
    mask = TRUE
  )
  
  # Return {terra} SpatRaster
  return(output_raster)
}

#' Compute a line density raster using vector-based analysis
#'
#' Produces a raster of line densities in the the circular neighbourhood around each cell.
#'
#' @param sf_lines An \code{sf} "LINESTRING" object from which to compute density.
#' @param sf_aoi An \code{sf} "POLYGON" defining the area of interest.
#' @param radius An integer, the radius in metres of the circular neighbourhood.
#' @param template A \code{SpatRaster} to use as a template for output raster.
#'
#' @return A terra \code{SpatRaster} of line density (m/km^2).
#'
vector_line_density <- function(sf_lines, sf_aoi, radius, template) {
  
  # Stop function if projections are mismatched
  if(sf::st_crs(sf_lines) != sf::st_crs(sf_aoi)) {
    stop("st_crs(sf_lines) == st_crs(sfc_aoi) is not TRUE")
  }
  
  # Stop function if units aren't in metres
  if(sf::st_crs(sf_lines)$units != "m") {
    stop("CRS units must be metres")
  }
  
  # Convert terra SpatRaster to SpatVect POINTS of cell centres
  cell_centres <- terra::as.points(template, values = FALSE, na.rm = TRUE) %>%
    # Convert to sf POINTS for speed gains (sf::st_length vs terra::perim)
    sf::st_as_sf() %>% 
    # Limit computations to points within distance "radius" of a line
    sf::st_filter(sf::st_buffer(sf_lines, radius)) %>% 
    # Add unique id for each point to join line density table
    dplyr::mutate(id = dplyr::row_number())
  
  # Set attribute-geometry relationships to constant (silences warnings)
  cell_centres <- sf::st_set_agr(cell_centres, "constant")
  sf_lines <- sf::st_set_agr(sf_lines, "constant")
  
  # Produce table of line densities for each point
  line_density_tbl <- cell_centres %>% 
    # Draw neighbourhood circle around each point using supplied radius
    sf::st_buffer(radius) %>%
    # Compute intersection of lines with each neighbourhood circle
    sf::st_intersection(sf_lines) %>% 
    # Compute length of lines within each circle (cast [m] units to numeric)
    dplyr::mutate(line_length = as.numeric(sf::st_length(geometry))) %>% 
    # Convert to table to speed things up in grouped operations
    sf::st_drop_geometry() %>% 
    # Compute total length of lines within each circle
    dplyr::group_by(id) %>%
    dplyr::summarise(line_length = sum(line_length)) %>%
    # Compute within circle line density, convert to m/km^2
    dplyr::mutate(line_density = (line_length)/((pi*radius^2)/1000000)) %>%
    # Remove line length
    dplyr::select(-line_length)
  
  # Join line density to cell points
  cell_centres <- cell_centres %>% 
    dplyr::left_join(line_density_tbl, by = dplyr::join_by(id)) %>% 
    dplyr::select(-id)
  
  # Convert to SpatRaster
  output_raster <- terra::rasterize(
    x = terra::vect(cell_centres),
    y = template,
    field = "line_density"
    # # Assign line density of 0 to cells further than "radius" from a line
    # background = 0
  )
  
  # Replace NAs with 0 (cells further than "radius" from a line)
  output_raster <- terra::subst(output_raster, NA, 0)
  
  # Crop
  output_raster <- terra::crop(
    output_raster,
    terra::vect(sf_aoi),
    # Set values outside aoi to NA
    mask = TRUE
  )
  
  # Return {terra} SpatRaster
  return(output_raster)
}
