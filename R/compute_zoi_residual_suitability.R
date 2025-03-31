compute_paved_roads_zoi_residual_suitability <- function(
    paved_road_distance
) {
  
  # Transform distance to paved road into residual suitability in 5km ZOI
  residual_suitability <- terra::app(
    x = paved_road_distance,
    # NB see Leblond et al. 2014 Biol Conserv, figure 2
    # NB maxes out at 96.7431, requires remap to 100
    fun = function(distance_to_road) {
      (-3.32774251389633 * 1e-6 * distance_to_road^2) +
        (3.43368617536310 * 1e-2 * distance_to_road) +
        8.25235251927575 
    }
  )
  
  # Reclassify >= 96.7431% to 100%
  residual_suitability <- terra::classify(
      x = residual_suitability,
      rcl = rbind(c(96.7431, Inf, 100)),
      include.lowest = TRUE
  )
  
  # Return
  return(residual_suitability)
}

compute_unpaved_roads_zoi_residual_suitability <- function(
    unpaved_road_distance
) {
  
  # Transform distance to unpaved road into residual suitability within ZOI
  residual_suitability <- terra::app(
    x = unpaved_road_distance,
    # NB see Leblond et al. 2014 Biol Conserv, figure 2
    # NB maxes out above 100%, requires clamp
    fun = function(distance_to_road) {
        (1.64727383727388 * 1e-9 * distance_to_road^3) -
          (1.76726369784073 * 1e-5 * distance_to_road^2) +
          (6.3221287345140 * 1e-2 * distance_to_road) +
          21.8858052204059
    }
  )
  
  # Clamp to 100% upper bound
  residual_suitability <- terra::clamp(
    x = residual_suitability,
    upper = 100
  )
  
  # Add year as layer name
  names(residual_suitability) <- names(unpaved_road_distance)
  
  # Return
  return(residual_suitability)
}