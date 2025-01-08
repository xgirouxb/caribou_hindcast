# Function to define study area
define_study_area <- function(){
  
  # Import Quebec boreal caribou ranges
  quebec_ranges <- get_shp_from_url(
    layer_url = quebec_caribou_ranges_url,
    # Save for later
    file = "data/study_area/quebec_caribou_ranges.shp"
  )
  
  # Import Ontario boreal caribou ranges
  ontario_ranges <- get_shp_from_url(
    layer_url = ontario_caribou_ranges_url,
    # Save for later
    file = "data/study_area/ontario_caribou_ranges.shp"
  )
  
  # Import StatCan admin boundaries for Ontario and Quebec
  statcan_admin_bounds <- get_shp_from_url(
    layer_url = statcan_admin_boundaries_url,
    # Save for later
    file = "data/study_area/statcan_admin_bounds.shp"
  )
  
  # Build "relaxed" concave hull of all caribou ranges
  caribou_hull <- dplyr::bind_rows(quebec_ranges, ontario_ranges) %>% 
    # Union
    sf::st_union() %>% 
    # Concave hull
    sf::st_concave_hull(ratio = 0.4)
  
  # Build ON/QC provincial bounds for study area
  qc_on_poly <- statcan_admin_bounds %>%
    # Filter Quebec and Ontario
    dplyr::filter(PRUID %in% c(24, 35)) %>% 
    # Cast MULTIPOLYGON to POLYGON
    sf::st_cast("POLYGON", warn = FALSE) %>%
    # Filter polygons that intersect the caribou range hull
    sf::st_filter(caribou_hull) %>%
    # Fix topology errors
    sf::st_make_valid() %>%
    # Combine into single layer
    sf::st_union() 
  
  # Define study area as intersection of caribou range hull and ON/QC bounds
  study_area <- caribou_hull %>% 
    sf::st_intersection(qc_on_poly)
  
  # Write to file
  sf::write_sf(
    obj = study_area,
    dsn = "data/study_area/study_area.shp"
  )
  
  # Return
  return(study_area)
}