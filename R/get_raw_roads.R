# Function to import AQréseau+ dataset
get_quebec_aqrp_roads <- function(aoi = study_area){
  
  # Import AQréseau+ (aqrp) dataset
  aqrp_roads <- get_shp_from_url(
    layer_url = quebec_aqrp_roads_url,
    # Limit to study area
    aoi = aoi,
    # Only read the "Reseau_routier.shp"
    shp_glob = "*routier.shp"
  ) %>% 
    # Clean names
    janitor::clean_names()
  
  # Return
  return(aqrp_roads)
}