compute_paved_road_distance <- function(
    study_area,
    quebec_roads,
    ontario_roads,
    n_workers = NULL
) {
  
  # Create temp folder in _scratch for tile outputs
  temp_folder <- "data/_scratch/paved_road_distance/"
  if(!fs::dir_exists(temp_folder)) { fs::dir_create(temp_folder) }
  
  # Empty temp folder
  fs::file_delete(fs::dir_ls(temp_folder))
  
  # Merge Quebec and Ontario paved roads
  paved_roads <- dplyr::bind_rows(
    dplyr::filter(quebec_roads, surface == "paved"),
    dplyr::filter(ontario_roads, surface == "paved") %>% 
      dplyr::mutate(id = as.character(id))
  )
  
  # Create tiles over study area for parallel computation
  tiles <- sf::st_make_grid(study_area, n = c(nx, ny)) %>% 
    # Coerce to sf
    sf::st_as_sf() %>% 
    # Filter to tiles that intersect study area
    sf::st_filter(study_area) %>% 
    # Add tile ID
    dplyr::mutate(tile_id = dplyr::row_number()) %>% 
    # # Testing
    # dplyr::filter(tile_id %in% 1:2) %>%
    {.}
  
  # Setup parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { 
    future::plan(
      strategy = "future::multisession",
      workers = n_workers,
      gc = TRUE
    )
  }
  
  # Compute distance to paved road in tiles 
  furrr::future_walk(
    .x = 1:nrow(tiles),
    .f = ~{
      # Define AOI, add 2-pixel buffer to eliminate edge effects
      # (ensures small tile overlap in downstream mosaic)
      aoi <- sf::st_intersection(study_area, tiles[.x,]) %>% 
        sf::st_buffer(60)
      
      # Define AOI with added ZOI + 2-pixel buffer to compute distance to
      # roads outside tile 
      aoi_buffer <- sf::st_buffer(aoi, zoi + 60)
      
      # Filter paved roads that intersect buffer of AOI
      roads_aoi <- sf::st_filter(paved_roads, aoi_buffer)
      
      # Create template raster 
      template_aoi <- terra::rast(
        x = terra::vect(aoi_buffer),
        resolution = 30,
        vals = 1
      )
      
      # Mask template raster values outside AOI buffer
      template_aoi <- terra::crop(
        x = template_aoi,
        y = terra::vect(aoi_buffer),
        mask = TRUE
      )
      
      # If no roads in AOI, make raster of ZOI distance
      if(nrow(roads_aoi) == 0) {
        
        # Substitute 1 for ZOI max distance
        paved_road_distance_raster <- terra::subst(
          x = template_aoi,
          from = 1,
          to = zoi
        )
        
        # Mask template raster values outside AOI 
        paved_road_distance_raster <- terra::crop(
          x = paved_road_distance_raster,
          y = terra::vect(aoi),
          mask = TRUE
        )
        
      # If there are roads in AOI
      } else {
        
        # Compute distance to paved road
        paved_road_distance_raster <- terra::distance(
          x = template_aoi,
          y = terra::vect(roads_aoi),
          unit = "m",
          rasterize = TRUE
        )
        
        # Reclassify values that exceed ZOI as ZOI max distance
        paved_road_distance_raster <- terra::classify(
          x = paved_road_distance_raster,
          rcl = rbind(c(zoi, Inf, zoi)),
          include.lowest = FALSE
        )
        
        # Mask template raster values outside AOI 
        paved_road_distance_raster <- terra::crop(
          x = paved_road_distance_raster,
          y = terra::vect(aoi),
          mask = TRUE
        )
      }

      # Write to file
      terra::writeRaster(
        x = paved_road_distance_raster,
        filename = paste0(temp_folder, "paved_road_distance_tile", .x, ".tif"),
        datatype = "INT2U",
        overwrite = TRUE
      )
    },
    # Pass seed to {future} to avoid complaints
    .options = furrr::furrr_options(seed = TRUE)
  )
  
  # Close parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { future::plan(strategy = "future::sequential") }
  
  # Retrieve list of output files
  output_files <- fs::dir_ls(temp_folder)
  
  # Check if there are as many output files as tiles
  if(length(1:nrow(tiles)) != length(output_files))
    stop("Missing tiles!")
  
  # Read all files as Raster Collection
  raster_col <- terra::sprc(lapply(output_files, terra::rast))
  
  # Mosaic into single raster
  output_raster <- terra::mosaic(raster_col)
  
  # Empty temp folder
  fs::file_delete(fs::dir_ls(temp_folder))
  
  # Return road distance target
  return(output_raster)
}

compute_unpaved_road_distance <- function(
    study_area,
    quebec_roads,
    ontario_roads,
    unpaved_construction_years,
    year,
    n_workers = NULL
) {
  
  # Create temp folder in _scratch for tile outputs
  temp_folder <- paste0("data/_scratch/unpaved_road_distance_", year, "/")
  if(!fs::dir_exists(temp_folder)) { fs::dir_create(temp_folder) }
  
  # Empty temp folder
  fs::file_delete(fs::dir_ls(temp_folder))
  
  # Merge Quebec and Ontario unpaved roads
  unpaved_roads <- dplyr::bind_rows(
    dplyr::filter(quebec_roads, surface == "unpaved"),
    dplyr::filter(ontario_roads, surface == "unpaved") %>% 
      dplyr::mutate(id = as.character(id))
  ) %>% 
    # Join unpaved road construction years
    dplyr::left_join(unpaved_construction_years, by = dplyr::join_by(id)) %>% 
    # Filter for roads present in supplied year
    dplyr::filter(const_year <= year)
  
  # Create tiles over study area for parallel computation
  tiles <- sf::st_make_grid(study_area, n = c(nx, ny)) %>% 
    # Coerce to sf
    sf::st_as_sf() %>% 
    # Filter to tiles that intersect study area
    sf::st_filter(study_area) %>% 
    # Add tile ID
    dplyr::mutate(tile_id = dplyr::row_number()) %>% 
    # # Testing
    # dplyr::filter(tile_id %in% 1:2) %>%
    {.}
  
  # Setup parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { 
    future::plan(
      strategy = "future::multisession",
      workers = n_workers,
      gc = TRUE
    )
  }
  
  # Compute distance to unpaved road in tiles 
  furrr::future_walk(
    .x = 1:nrow(tiles),
    .f = ~{
      # Define AOI, add 2-pixel buffer to eliminate edge effects
      # (ensures small tile overlap in downstream mosaic)
      aoi <- sf::st_intersection(study_area, tiles[.x,]) %>% 
        sf::st_buffer(60)
      
      # Define AOI with added ZOI + 2-pixel buffer to compute distance to
      # roads outside tile 
      aoi_buffer <- sf::st_buffer(aoi, zoi + 60)
      
      # Filter unpaved roads that intersect buffer of AOI
      roads_aoi <- sf::st_filter(unpaved_roads, aoi_buffer)
      
      # Create template raster 
      template_aoi <- terra::rast(
        x = terra::vect(aoi_buffer),
        resolution = 30,
        vals = 1
      )
      
      # Mask template raster values outside AOI buffer
      template_aoi <- terra::crop(
        x = template_aoi,
        y = terra::vect(aoi_buffer),
        mask = TRUE
      )
      
      # If no roads in AOI, make raster of ZOI max distance
      if(nrow(roads_aoi) == 0) {
        
        # Substitute 1 for ZOI distance value
        unpaved_road_distance_raster <- terra::subst(
          x = template_aoi,
          from = 1,
          to = zoi
        )
        
        # Mask template raster values outside AOI 
        unpaved_road_distance_raster <- terra::crop(
          x = unpaved_road_distance_raster,
          y = terra::vect(aoi),
          mask = TRUE
        )
        
        # If there are roads in AOI
      } else {
        
        # Compute distance to unpaved road
        unpaved_road_distance_raster <- terra::distance(
          x = template_aoi,
          y = terra::vect(roads_aoi),
          unit = "m",
          rasterize = TRUE
        )
        
        # Reclassify distance values that exceed ZOI as ZOI
        unpaved_road_distance_raster <- terra::classify(
          x = unpaved_road_distance_raster,
          rcl = rbind(c(zoi, Inf, zoi)),
          include.lowest = FALSE
        )
        
        # Mask template raster values outside AOI 
        unpaved_road_distance_raster <- terra::crop(
          x = unpaved_road_distance_raster,
          y = terra::vect(aoi),
          mask = TRUE
        )
      }
      
      # Write to file
      terra::writeRaster(
        x = unpaved_road_distance_raster,
        filename = paste0(temp_folder, "unpaved_road_distance_tile", .x, ".tif"),
        datatype = "INT2U",
        overwrite = TRUE
      )
    },
    # Pass seed to {future} to avoid complaints
    .options = furrr::furrr_options(seed = TRUE)
  )
  
  # Close parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { future::plan(strategy = "future::sequential") }
  
  # Retrieve list of output files
  output_files <- fs::dir_ls(temp_folder)
  
  # Check if there are as many output files as tiles
  if(length(1:nrow(tiles)) != length(output_files))
    stop("Missing tiles!")
  
  # Read all files as Raster Collection
  raster_col <- terra::sprc(lapply(output_files, terra::rast))
  
  # Mosaic into single raster
  output_raster <- terra::mosaic(raster_col)
  
  # Add year as layer name
  names(output_raster) <- paste(year)
  
  # Empty temp folder
  fs::file_delete(fs::dir_ls(temp_folder))
  
  # Return road distance target
  return(output_raster)
}