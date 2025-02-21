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
  
  # Compute unpaved road density in tiles 
  furrr::future_walk(
    .x = 1:nrow(tiles),
    .f = ~{
      # Define AOI
      aoi <- sf::st_intersection(study_area, tiles[.x,])
      
      # Define AOI with added radius and 3 pixel buffer to reduce edge effects
      aoi_buffer <- sf::st_buffer(aoi, radius + 90)
      
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
  
  # # Return road density target (use this when geotargets is updated)
  # return(output_raster)
  
  # Cast to integer as the tar_terra_rast "datatype" arg is in dev
  # see https://github.com/njtierney/geotargets/pull/137
  return(terra::as.int(output_raster))
}