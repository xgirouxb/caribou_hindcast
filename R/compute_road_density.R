compute_paved_road_density <- function(
    study_area,
    quebec_roads,
    ontario_roads,
    n_workers = NULL
  ) {
  
  # Create temp folder in _scratch for tile outputs
  temp_folder <- "data/_scratch/paved_road_density/"
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
  
  # Compute paved road density in tiles 
  furrr::future_walk(
    .x = 1:nrow(tiles),
    .f = ~{
      # Define AOI, add 2-pixel buffer to eliminate edge effects
      # (ensures small tile overlap in downstream mosaic)
      aoi <- sf::st_intersection(study_area, tiles[.x,]) %>% 
        sf::st_buffer(60)
      
      # Define AOI with added radius + 2-pixel buffer to reduce edge effects for
      # circular neighbourhood
      aoi_buffer <- sf::st_buffer(aoi, radius + 60)
      
      # Filter paved roads that intersect AOI buffer 
      roads_aoi <-  sf::st_filter(paved_roads, aoi_buffer)
      
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
      
      # Compute road density (m/km^2) in 1 km neighbourhood window
      paved_road_density_raster <- line_density(
        sf_lines = roads_aoi,
        sf_aoi = aoi,
        radius = radius,
        template = template_aoi
      )
      
      # Write to file
      terra::writeRaster(
        x = paved_road_density_raster,
        filename = paste0(temp_folder, "paved_road_density_tile", .x, ".tif"),
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
  
  # Return road density target
  return(output_raster)
}


compute_unpaved_road_density <- function(
    study_area,
    quebec_roads,
    ontario_roads,
    unpaved_construction_years,
    year,
    n_workers = NULL
) {
  
  # Create temp folder in _scratch for tile outputs
  temp_folder <- paste0("data/_scratch/unpaved_road_density_", year, "/")
  if(!fs::dir_exists(temp_folder)) { fs::dir_create(temp_folder) }
  
  # Delete everything in temp folder
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
  
  # Compute unpaved road density in tiles 
  furrr::future_walk(
    .x = 1:nrow(tiles),
    .f = ~{
      # Define AOI, add 2-pixel buffer to eliminate edge effects
      # (ensures small tile overlap in downstream mosaic)
      aoi <- sf::st_intersection(study_area, tiles[.x,]) %>% 
        sf::st_buffer(60)
      
      # Define AOI with added radius + 2-pixel buffer to reduce edge effects for
      # circular neighbourhood
      aoi_buffer <- sf::st_buffer(aoi, radius + 60)
      
      # Filter unpaved roads that intersect AOI buffer 
      roads_aoi <-  sf::st_filter(unpaved_roads, aoi_buffer)
      
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
      
      # Compute road density (m/km^2) in 1 km neighbourhood window
      unpaved_road_density_raster <- line_density(
        sf_lines = roads_aoi,
        sf_aoi = aoi,
        radius = radius,
        template = template_aoi
      )
      
      # Write to file
      terra::writeRaster(
        x = unpaved_road_density_raster,
        filename = paste0(temp_folder, "unpaved_road_density_tile", .x, ".tif"),
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
    stop(paste0("Year ", year, " has missing tiles!"))
  
  # Read all files as Raster Collection
  raster_col <- terra::sprc(lapply(output_files, terra::rast))
  
  # Mosaic into single raster
  output_raster <- terra::mosaic(raster_col)
  
  # Add year as layer name
  names(output_raster) <- paste(year)
  
  # Empty temp folder
  fs::file_delete(fs::dir_ls(temp_folder))
  
  # Return road density target
  return(output_raster)
}