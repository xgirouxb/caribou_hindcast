# Function to import AQréseau+ dataset
get_quebec_aqrp_roads <- function(study_area){
  
  # Import AQréseau+ (aqrp) dataset
  aqrp_roads <- get_shp_from_url(
    layer_url = quebec_aqrp_roads_url,
    # Limit to study area
    aoi = study_area,
    # Only read the "Reseau_routier.shp"
    shp_glob = "*routier.shp"
  ) %>% 
    # Clean names
    janitor::clean_names()
  
  # Return
  return(aqrp_roads)
}

# Function to import Ontario Road Network elements dataset
get_ontario_orn_roads <- function(study_area){
  
  # Import ORN surface table (Paved/Non-paved)
  orn_roads_surface_tbl <- get_tbl_from_url(
    tbl_url = ontario_orn_roads_url,
    tbl_glob =  "*SURFACE.csv",
    delimiter = ";"
  ) %>% 
    dplyr::select(ogf_id = ORN_ROAD_NET_ELEMENT_ID, surface = PAVEMENT_STATUS) 
    
  # Import ORN dataset
  orn_roads <- get_shp_from_url(
    layer_url = ontario_orn_roads_url,
    # Limit to study area
    aoi = study_area,
    # Only read the ".shp" file
    shp_glob = "*.shp"
  ) %>% 
    # Clean names
    janitor::clean_names() %>% 
    # Join surface table by ogf_id
    dplyr::left_join(orn_roads_surface_tbl, by = dplyr::join_by(ogf_id))
  
  # Return
  return(orn_roads)
}
