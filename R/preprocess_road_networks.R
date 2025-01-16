preprocess_quebec_roads <- function(aqrp_roads) {
  
  # Initial database cleanup
  intial_cleanup <- aqrp_roads %>% 
    # Replace NAs with a value so they dont get dropped by filter
    tidyr::replace_na(list(cls_rte = "Inconnu")) %>%
    # Remove ferry routes (without dropping NAs)
    dplyr::filter(cls_rte != "Liaison maritime") %>%
    # Reclassify everything that is not paved as unpaved
    dplyr::mutate(
      # See longer form case_when in `notes_preprocess_quebec_roads.Rmd`
      surface_type = dplyr::if_else(etat_rev == "Revêtue", "Paved", "Unpaved")
    ) %>%
    # Select columns of interest
    dplyr::select(
      # Road uuid
      aqrp_uuid,
      # Date of last update (WARNING: uuid can change between large updates)
      version,
      # Road attributes
      cls_rte, surface_type, geometry
    ) %>% 
    # # Testing
    # dplyr::slice_sample(n = 100) %>%
    {.}
  
  # Setup parallel processing
  future::plan(
    strategy = multisession,
    workers = round(parallelly::availableCores()*0.7)
  )

  # Reclassify paved segments on logging roads (e.g., small bridges)
  preprocessed <- intial_cleanup %>%
    # Nest by row
    dplyr::group_nest(dplyr::row_number()) %>% 
    # Extract list
    dplyr::pull(data) %>% 
    # Map over each paved road
    furrr::future_map_if(
      # Predicate function indicating to map over only paved roads
      .p = function(y) any(y$surface_type == "Paved"),
      # Spatial filter logic function
      .f = ~ {
        # Compute number of paved roads nearby
        n_paved_roads_nearby <- intial_cleanup %>%
          # Remove the paved segment of interest from aqrp dataset
          dplyr::filter(aqrp_uuid != .x$aqrp_uuid) %>% 
          # Filter for any intersecting roads within 100 m
          sf::st_filter(sf::st_buffer(.x, 100)) %>%
          # Count how many area paved
          dplyr::filter(surface_type == "Paved") %>%
          nrow()
        
        # If none of the neighbouring roads are paved
        if(n_paved_roads_nearby == 0) {
          # Reclassify the road of interest as unpaved
          dplyr::mutate(.x, surface_type = "Unpaved")
        } else {
          # Else return unchanged road
          .x
        }
      },
      # Pass seed to future to avoid complaints
      .options = furrr_options(seed = TRUE)
    ) %>%
    # Recombine list of sf objects into single table
    purrr::list_rbind() %>%
    # Cast to sf
    sf::st_as_sf()
  
  # Close parallel processing
  future::plan(sequential)
  
  # Return 
  return(preprocessed)
}