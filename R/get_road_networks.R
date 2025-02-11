# Import AQréseau+ dataset
get_quebec_aqrp_roads <- function(study_area){
  
  # Import AQréseau+ (aqrp) dataset
  aqrp_roads <- get_sf_from_source(
    sf_source = quebec_aqrp_roads_url,
    # Filter to study area
    sf_aoi = study_area,
    # Only read the "Reseau_routier.shp"
    sf_glob = "*routier.shp"
  ) %>% 
    # Clean names
    janitor::clean_names()
  
  # Return
  return(aqrp_roads)
}

# Import Ontario Road Network elements dataset
get_ontario_orn_roads <- function(study_area){
  
  # Import ORN archive and unzip to local directory
  temp_dir <- get_archive_from_url(ontario_orn_roads_url)
  
  # Read the ORN surface table (Paved/Unpaved) for joining
  orn_roads_surface_tbl <- get_tbl_from_source(
    tbl_source = temp_dir,
    tbl_glob =  "*SURFACE.csv",
    delimiter = ";"
  ) %>%
    # Retain required columns
    dplyr::select(
      ogf_id = ORN_ROAD_NET_ELEMENT_ID,
      surface = PAVEMENT_STATUS
    ) %>% 
    # Remove dupes: if a road has > 1 surface, choose "Paved")
    dplyr::arrange(ogf_id, surface) %>% 
    dplyr::distinct(ogf_id, .keep_all = TRUE)
  
  # Import ORN road class table (Paved/Non-paved)
  orn_roads_class_tbl <- get_tbl_from_source(
    tbl_source = temp_dir,
    tbl_glob =  "*CLASS.csv",
    delimiter = ";"
  ) %>%
    # Only retain required columns
    dplyr::select(
      ogf_id = ORN_ROAD_NET_ELEMENT_ID,
      road_class = ROAD_CLASS
    ) %>% 
    # Remove dupes: if a road has > 1 class, choose class based on NRN hierarchy
    dplyr::mutate(
      road_class = factor(
        road_class,
        # NRN hierarchy: see metadata NRN lookup table in ORN_ROAD_CLASS_LIST 
        levels = c(
          "Freeway",
          "Expressway / Highway",
          "Arterial",
          "Collector",
          "Local / Street",
          "Local / Strata",
          "Local / Unknown",
          "Alleyway / Laneway",
          "Ramp",
          "Resource / Recreation",
          "Rapid Transit",
          "Service",
          "Winter"
        )
      )
    ) %>% 
    dplyr::arrange(ogf_id, road_class) %>% 
    dplyr::distinct(ogf_id, .keep_all = TRUE)
    
  # Import ORN dataset
  orn_roads <- get_sf_from_source(
    sf_source = temp_dir,
    # Limit to study area
    sf_aoi = study_area,
    # Only read the ".shp" file
    sf_glob = "*.shp"
  ) %>% 
    # Clean names
    janitor::clean_names() %>% 
    # Join roads surface and class table by ogf_id
    dplyr::left_join(orn_roads_surface_tbl, by = dplyr::join_by(ogf_id)) %>% 
    dplyr::left_join(orn_roads_class_tbl, by = dplyr::join_by(ogf_id))
  
  # Return
  return(orn_roads)
}

# Import Ministry of Natural Resources roads dataset
get_ontario_mnr_roads <- function(study_area){
  
  # Import MNR dataset
  mnr_roads <- get_sf_from_source(
    sf_source = ontario_mnr_roads_url,
    # Limit to study area
    sf_aoi = study_area,
    # Only read the ".shp" file
    sf_glob = "*.shp"
  ) %>%
    # Clean names
    janitor::clean_names()
  
  # Return
  return(mnr_roads)
}