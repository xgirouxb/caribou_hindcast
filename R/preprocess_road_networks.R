preprocess_quebec_roads <- function(aqrp_roads, building_footprint, n_workers = 1) {
  
  # STEP 1: Initial database cleanup
  initial_cleanup <- aqrp_roads %>% 
    # Select columns of interest
    dplyr::select(
      # Road uuid
      aqrp_uuid,
      # Road attributes
      etat_rev, cls_rte, cls_che_for,
      # LINESTRINGs
      geometry
    ) %>% 
    # Replace NAs with a placeholder value so they don't get dropped by filter
    tidyr::replace_na(list(cls_rte = "IN", cls_che_for = "IN")) %>%
    # Remove ferry routes
    dplyr::filter(cls_rte != "Liaison maritime") %>%
    # Create a paved/unpaved surface type attribute
    dplyr::mutate(
      surface = dplyr::case_when(
        etat_rev == "Non revêtue" ~ "unpaved",
        etat_rev == "Revêtue" ~ "paved",
        # `Autre` and `ND` as `unknown`
        etat_rev %in% c("Autre", "ND") ~ "unknown",
        # `NA` as `unknown`
        is.na(etat_rev) ~ "unknown",
        # Anything else is `unknown`
        TRUE ~ "unknown"
      )
    ) %>% 
    # # Testing
    # dplyr::slice_sample(n = 100) %>%
    {.}
  
  # STEP 2: Forest roads reclassification
  forest_roads_reclassed <- initial_cleanup %>% 
    # Filter for all classes that aren't NF (non-forest)
    dplyr::filter(cls_che_for != "NF") %>% 
    # Reclassify unknowns in forest roads as unpaved
    dplyr::mutate(
      surface = dplyr::if_else(
        # If unknown surface
        condition = surface == "unknown",
        # Reclassify as unpaved
        true = "unpaved",
        # Otherwise keep the stated surface type
        false = surface
      )
    )
  
  # STEP 3: Non-forest roads reclassification
  
  # Subset non-forest roads with known surface
  nonforest_roads_known_surface <- initial_cleanup %>% 
    dplyr::filter(cls_che_for == "NF", surface != "unknown")
  
  # Reclassify non-forest roads with unknown surface as paved if...
  nonforest_roads_reclassed_paved <- initial_cleanup %>% 
    dplyr::filter(cls_che_for == "NF", surface == "unknown") %>% 
    # ...they intersect the building footprint's 2km buffer
    sf::st_filter(building_footprint) %>% 
    dplyr::mutate(surface = "paved")
  
  # Reclassify remaining non-forest roads with unknown surface as unpaved
  nonforest_roads_reclassed_unpaved <- initial_cleanup %>% 
    dplyr::filter(cls_che_for == "NF", surface == "unknown") %>% 
    dplyr::filter(!(aqrp_uuid %in% nonforest_roads_reclassed_paved$aqrp_uuid)) %>% 
    dplyr::mutate(surface = "unpaved")
  
  # Bind all reclassified subsets back together for next steps
  all_unknowns_reclassed <- dplyr::bind_rows(
    forest_roads_reclassed,
    nonforest_roads_known_surface,
    nonforest_roads_reclassed_paved,
    nonforest_roads_reclassed_unpaved
  )
  
  # Sanity check
  if(any(all_unknowns_reclassed$surface == "unknown")) {
    message("Remaining roads with unknown surface type.")
  }
  
  # STEP 4: Reclassification of small isolated paved road segments
  
  # Reduce size of dataset to distribute across cores
  paved_roads_to_check <- all_unknowns_reclassed %>% 
    dplyr::filter(surface == "paved")
  unpaved_roads <- all_unknowns_reclassed %>% 
    dplyr::filter(surface == "unpaved")
  
  # Setup parallel processing
  if(n_workers > 1) { 
    future::plan(strategy = multisession, workers = n_workers, gc = TRUE)
  }

  # Reclassify paved segments on logging roads (e.g., small bridges)
  paved_roads_reclassified <- paved_roads_to_check %>%
    # Nest by row
    dplyr::group_nest(dplyr::row_number()) %>% 
    # Extract list
    dplyr::pull(data) %>% 
    # Map over each paved road
    furrr::future_map(
      # Spatial logic filtering function
      .f = ~ {
        # Compute number of paved roads nearby
        n_paved_roads_nearby <- paved_roads_to_check %>%
          # Remove the paved segment of interest from aqrp dataset
          dplyr::filter(aqrp_uuid != .x$aqrp_uuid) %>% 
          # Filter for any intersecting roads within 100 m
          sf::st_filter(sf::st_buffer(.x, 100)) %>%
          # Count how many are paved
          dplyr::filter(surface == "paved") %>%
          nrow()
        
        # If none of the neighbouring roads are paved
        if(n_paved_roads_nearby == 0) {
          # Reclassify the road of interest as unpaved
          dplyr::mutate(.x, surface = "unpaved")
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
  
  # Preprocessed roads
  preprocessed <- dplyr::bind_rows(paved_roads_reclassified, unpaved_roads)
  
  # Return 
  return(preprocessed)
}