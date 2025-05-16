estimate_road_construction_years <- function(
    quebec_roads,
    ontario_roads,
    canlad_road_construction_years
) {
  
  # Import CanLaD year of disturbance around each road
  canlad_yod <- readr::read_csv(
    file = canlad_road_construction_years,
    show_col_types = FALSE,
    progress = FALSE
  ) %>% 
    # Cast year of disturbance to integer, fixes dbl-int clash in logical %in% 
    dplyr::mutate(dplyr::across(dplyr::starts_with("yod"), as.integer))
  
  # Import unpaved roads table
  unpaved_roads <- dplyr::bind_rows(
    dplyr::filter(quebec_roads, surface == "unpaved"),
    dplyr::filter(ontario_roads, surface == "unpaved")
  ) %>% 
    # Drop geometries
    sf::st_drop_geometry() %>% 
    # Join CanLaD yod
    dplyr::left_join(canlad_yod, by = dplyr::join_by(id))
  
  # Estimate year of construction
  # Order of priority:
  #           period|  <1985  | 1985-2020 |   > 2020
  # source          |         |           |
  # CanLaD 65-84    |    2    |           |    
  # CanLaD 85-20    |         |     3     |    
  # MNR const_year  |    1    |     4     |    5
  unpaved_construction_years <- unpaved_roads %>% 
    # Merge CanLaD year of disturbance and MNR "Actual" construction date
    dplyr::mutate(
      const_year = dplyr::case_when(
        
        # PRE-1985 PERIOD -----------------------------------------------------#
       
        # IF: MNR records construction before 1985
        # --> assign 1984
        !is.na(mnr_const_year) & mnr_const_year < 1985 ~ 1984,
        # ELSE IF: CanLaD detects change pre-1985
        # --> assign 1984
        !is.na(yod_canlad_65) ~ 1984,
        # ELSE IF: CanLaD detects NO changes, and MNR records NO year
        # --> assign 1984
        is.na(yod_canlad_65) & is.na(yod_canlad_85) & is.na(mnr_const_year) ~ 1984,
        
        # 1985 to 2020 PERIOD -------------------------------------------------#
        
        # ELSE IF: CanLaD detects change between 1985 and 2020
        # --> assign CanLaD year of disturbance
        !is.na(yod_canlad_85) & yod_canlad_85 %in% 1985:2020 ~ yod_canlad_85,
        # ELSE IF: MNR records construction year between 1985 and 2020
        # --> assign MNR construction year
        !is.na(mnr_const_year) & mnr_const_year %in% 1985:2020 ~ mnr_const_year,
        
        # POST-2020 PERIOD ----------------------------------------------------#
        
        # ELSE IF: MNR records construction year after 2020
        # --> assign NA to exclude from study
        !is.na(mnr_const_year) & mnr_const_year > 2020 ~ NA_integer_,
        
        # ELSE IF: Placeholder for sanity check
        TRUE ~ 9999
      )
    ) %>% 
    # Clean up
    dplyr::select(id, const_year)
  
  # Sanity check 
  if(any(unpaved_construction_years$const_year == 9999, na.rm = TRUE)) { 
    stop("Year classification error") 
  }
  
  # Return table of estimated road construction years
  return(unpaved_construction_years)
}