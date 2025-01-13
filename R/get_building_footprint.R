get_building_footprint <- function(study_area, buffer = 2000){
  
  # Read table of with all quadkey tiles (Bing maps tile system)
  quadkey_tile_tbl <- readr::read_csv(
    global_buildings_datalinks_url,
    show_col_types = FALSE,
    progress = FALSE
  ) %>% 
    # Clean up names
    janitor::clean_names() %>% 
    dplyr::rename(quadkey = quad_key) %>%
    # Filter to Canadian quadkeys
    dplyr::filter(location == "Canada") %>% 
    # Parse quadkeys to sf objects 
    quadkeyr::quadkey_df_to_polygon() %>% 
    # Filter for quadkeys that intersect the area of interest (in EPSG:4326)
    sf::st_filter(sf::st_transform(study_area, 4326)) %>% 
    # Drop the geometries
    sf::st_drop_geometry() 
  
  # Download geojson for each quadkey tile and convert to sf
  buildings_sf <- quadkey_tile_tbl %>% 
    dplyr::mutate(
      sf_obj = purrr::map(
        .x = url,
        .f = ~{
          # Read the .csv.gz file at the URL
          readr::read_lines(.x, progress = FALSE) %>%
            # Convert it from geojson format to sf object
            geojsonsf::geojson_sf()
        }
      )
    ) %>% 
    # Convert list of sf objects to sf object
    tidyr::unnest(cols = c(sf_obj)) %>% 
    sf::st_as_sf() %>%
    # Drop non-geometry columns
    dplyr::select(geometry) %>% 
    # Project
    sf::st_transform(study_proj) %>% 
    # Compute a user-defined buffer around each building
    sf::st_buffer(buffer) %>%
    # Merge into single layer
    sf::st_union() %>% 
    # Clip to study area
    sf::st_intersection(study_area)
  
  # Return
  return(buildings_sf)
}