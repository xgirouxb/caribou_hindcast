preprocess_quebec_roads <- function(aqrp_roads, building_footprint, n_workers = NULL) {
  
  # STEP 1: Initial database cleanup
  initial_cleanup <- aqrp_roads %>% 
    # Select columns of interest
    dplyr::select(
      # Road uuid
      AQRP_UUID,
      # Road attributes
      EtatRev, ClsRte, Cls_CheFor,
      # LINESTRINGs
      geometry
    ) %>% 
    # Replace NAs with a placeholder value so they don't get dropped by filter
    tidyr::replace_na(list(ClsRte = "IN", Cls_CheFor = "IN")) %>%
    # Remove ferry routes
    dplyr::filter(ClsRte != "Liaison maritime") %>%
    # Create a paved/unpaved surface type attribute
    dplyr::mutate(
      surface = dplyr::case_when(
        EtatRev == "Non revêtue" ~ "unpaved",
        EtatRev == "Revêtue" ~ "paved",
        # `Autre` and `ND` as `unknown`
        EtatRev %in% c("Autre", "ND") ~ "unknown",
        # `NA` as `unknown`
        is.na(EtatRev) ~ "unknown",
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
    dplyr::filter(Cls_CheFor != "NF") %>% 
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
    dplyr::filter(Cls_CheFor == "NF", surface != "unknown")
  
  # Reclassify non-forest roads with unknown surface as paved if...
  nonforest_roads_reclassed_paved <- initial_cleanup %>% 
    dplyr::filter(Cls_CheFor == "NF", surface == "unknown") %>% 
    # ...they intersect the building footprint's 2km buffer
    sf::st_filter(building_footprint) %>% 
    dplyr::mutate(surface = "paved")
  
  # Reclassify remaining non-forest roads with unknown surface as unpaved
  nonforest_roads_reclassed_unpaved <- initial_cleanup %>% 
    dplyr::filter(Cls_CheFor == "NF", surface == "unknown") %>% 
    dplyr::filter(!(AQRP_UUID %in% nonforest_roads_reclassed_paved$AQRP_UUID)) %>% 
    dplyr::mutate(surface = "unpaved")
  
  # Bind all reclassified subsets back together for next steps
  all_unknowns_reclassed <- dplyr::bind_rows(
    forest_roads_reclassed,
    nonforest_roads_known_surface,
    nonforest_roads_reclassed_paved,
    nonforest_roads_reclassed_unpaved
  )
  
  # Sanity check for future proofing
  if(any(all_unknowns_reclassed$surface == "unknown")) {
    message("Remaining roads with unknown surface type.")
  }
  
  # STEP 4: Reclassification of small isolated paved road segments
  
  # Reduce size of dataset to distribute across cores
  paved_roads_to_check <- all_unknowns_reclassed %>% 
    dplyr::filter(surface == "paved")
  unpaved_roads <- all_unknowns_reclassed %>% 
    dplyr::filter(surface == "unpaved")
  
  # Setup parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { 
    future::plan(
      strategy = "future::multisession",
      workers = n_workers,
      gc = TRUE
    )
  }

  # Reclassify paved segments on logging roads (e.g., small bridges)
  paved_roads_reclassed <- paved_roads_to_check %>%
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
          dplyr::filter(AQRP_UUID != .x$AQRP_UUID) %>% 
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
      # Pass seed to {future} to avoid complaints
      .options = furrr::furrr_options(seed = TRUE)
    ) %>%
    # Recombine list of sf objects into single table
    purrr::list_rbind() %>%
    # Cast to sf
    sf::st_as_sf()
  
  # Close parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { future::plan(strategy = "future::sequential") }
  
  # Preprocessed roads
  preprocessed <- dplyr::bind_rows(paved_roads_reclassed, unpaved_roads) %>% 
    # Add data source to retrace downstream
    dplyr::mutate(data_source = "aqrp") %>% 
    # Clean up
    dplyr::select(data_source, id = AQRP_UUID, surface, geometry)
  
  # Return 
  return(preprocessed)
}

preprocess_ontario_roads <- function(orn_roads, mnr_roads, n_workers = NULL){
  
  # STEP 1: Initial ORN database cleanup
  orn_initial_cleanup <- orn_roads %>% 
    # Filter for roads only (removes ferry and virtual roads)
    dplyr::filter(ELEM_TYPE == "ROAD ELEMENT") %>%
    # Add columns
    dplyr::mutate(
      # Add data source as attribute
      data_source = "orn",
      # Cast surface to lowercase
      surface = stringr::str_to_lower(PAVEMENT_STATUS)
    ) %>% 
    # Select/rename required columns
    dplyr::select(
      # Database of origin
      data_source,
      # Ontario Geospatial Feature ID
      id = OGF_ID,
      # Road attributes
      surface,
      geometry
    ) %>%
    # # Testing
    # dplyr::glimpse() %>% 
    {.}
  
  # STEP 2: Initial MNR database cleanup
  mnr_initial_cleanup <- mnr_roads %>% 
    # Add columns
    dplyr::mutate(
      # Add data source as attribute
      data_source = "mnr",
      # Reclassify road surfaces as "paved" or "unpaved"
      surface = dplyr::if_else(SURFACE_T == "Paved", "paved", "unpaved"),
      # Get "actual" construction years
      mnr_const_year = dplyr::if_else(
        condition = YR_CONST_M == "Actual" & YR_CONST != 9999,
        true = YR_CONST,
        false = NA_integer_
      )
    ) %>% 
    # Select/rename required columns
    dplyr::select(
      # Database of origin
      data_source,
      # Ontario Geospatial Feature ID
      id = OGF_ID,
      # Road attributes
      surface,
      mnr_const_year,
      geometry
    ) %>%
    # # Testing
    # dplyr::glimpse() %>% 
    {.}
  
  # STEP 3: Build a list of MNR roads to be deleted +
  #         Create a table to join their attributes to corresponding ORN roads
  
  # Speed things up: only operate on intersecting geom from both road networks
  all_intersecting_mnr_roads <- mnr_initial_cleanup %>% 
    sf::st_filter(sf::st_buffer(orn_initial_cleanup, 5))
  all_intersecting_orn_roads <- orn_initial_cleanup %>% 
    sf::st_filter(sf::st_buffer(all_intersecting_mnr_roads, 5))
  
  # Setup parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { 
    future::plan("future::multisession", workers = n_workers, gc = TRUE)
  }
  
  # Build table of inherited MNR attributes to join
  orn_mnr_join_tbl <- all_intersecting_orn_roads %>% 
    # Create a 5m buffer around each road
    sf::st_buffer(5) %>% 
    # Cast to list of individual ORN road buffers
    dplyr::group_nest(id) %>% 
    # Map over each ORN road buffer
    dplyr::mutate(
      # Build a table of exploded MNR segments that fall within ORN buffer
      data = furrr::future_map(
        .x = data,
        .f = ~{
          all_intersecting_mnr_roads %>%
            # Limit operations to MNR roads that intersect this ORN buffer
            sf::st_filter(.x, .predicate = sf::st_intersects) %>% 
            # Explode lines into individual segments\
            explode_lines() %>%
            # Filter lines within ORN buffer (st_contains > st_within for speed)
            # NB see https://github.com/r-spatial/sf/issues/1261
            dplyr::filter(sf::st_contains(x = .x, y = ., sparse = FALSE)[1,]) %>%
            # Drop geometries to return tbl
            sf::st_drop_geometry() %>%
            # Select columns of interest
            dplyr::select(
              inherited_mnr_id = id,
              inherited_mnr_surface = surface,
              mnr_const_year
            )
        },
        # Pass seed to {future} to avoid complaints
        .options = furrr::furrr_options(seed = TRUE)
      )
    ) %>% 
    # Unnest list of tables
    tidyr::unnest(cols = c(data)) %>%
    # Get unique combinations of ORN and inherited MNR roads
    dplyr::distinct() %>%
    # Group by ORN road
    dplyr::group_by(id) %>% 
    # Summarise MNR attributes, assign NA is no MNR segments in ORN road buffer
    dplyr::summarise(
      # If ORN road inherits multiple mnr_id's, flatten them to a single string 
      inherited_mnr_id = stringr::str_flatten(inherited_mnr_id, collapse = ";"),
      # Save MNR surface as "paved" if any road is paved, otherwise unpaved
      inherited_mnr_surface = sort(inherited_mnr_surface)[1],
      # Save the earliest 'Actual' construction year among inherited MNR roads
      mnr_const_year = ifelse(
        # Return NA if all are NA
        all(is.na(mnr_const_year)), NA_integer_,
        min(mnr_const_year, na.rm = TRUE)
      )
    )
  
  # Close parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { future::plan(strategy = "future::sequential") }
  
  # List of MNR road IDs to be removed
  mnr_roads_to_remove <- orn_mnr_join_tbl %>% 
    dplyr::pull(inherited_mnr_id) %>% 
    stringr::str_split(";") %>% 
    purrr::list_c() %>% 
    unique()
  
  # STEP 4: Join inherited MNR road attributes to corresponding ORN roads
  #         Merge MNR and ORN datasets
  
  # Prep MRN for merge
  mrn_roads_to_merge <- mnr_initial_cleanup %>% 
    # Remove MNR roads within ORN roads buffers
    dplyr::filter(!(id %in% mnr_roads_to_remove))
  
  # Prep ORN for merge
  orn_roads_to_merge <- orn_initial_cleanup %>% 
    # Join inherited MRN road attributes
    dplyr::left_join(orn_mnr_join_tbl, by = dplyr::join_by(id)) %>% 
    # Reconcile surface differences with inherited MNR attributes
    dplyr::mutate(surface = case_when(
      is.na(inherited_mnr_surface) ~ surface,
      surface == inherited_mnr_surface ~ surface,
      # If MNR and ORN disagree, choose "paved"
      surface != inherited_mnr_surface ~ sort(c(surface, inherited_mnr_surface))[1],
      TRUE ~ NA_character_
    )) %>% 
    # Clean up
    dplyr::select(-inherited_mnr_surface)
  
  # Merged Ontario road network
  ontario_roads <- dplyr::bind_rows(orn_roads_to_merge, mrn_roads_to_merge) %>%
    # Cast as character to avoid errors when merging with AQRP ids
    dplyr::mutate(id = as.character(id))
  
  # STEP 5: Reclassification of small isolated paved road segments
  
  # Reduce size of dataset to distribute across cores
  paved_roads_to_check <- ontario_roads %>% 
    dplyr::filter(surface == "paved")
  unpaved_roads <- ontario_roads %>% 
    dplyr::filter(surface == "unpaved")
  
  # Setup parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { 
    future::plan(
      strategy = "future::multisession",
      workers = n_workers,
      gc = TRUE
    )
  }
  
  # Reclassify paved segments on logging roads (e.g., small bridges)
  paved_roads_reclassed <- paved_roads_to_check %>%
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
          # Remove the paved segment of interest from ontario dataset
          dplyr::filter(id != .x$id) %>% 
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
      # Pass seed to {future} to avoid complaints
      .options = furrr::furrr_options(seed = TRUE)
    ) %>%
    # Recombine list of sf objects into single table
    purrr::list_rbind() %>%
    # Cast to sf
    sf::st_as_sf()
  
  # Close parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { future::plan(strategy = "future::sequential") }
  
  # Preprocessed ontario roads
  preprocessed <- dplyr::bind_rows(paved_roads_reclassed, unpaved_roads)
  
  # Return 
  return(preprocessed)
}