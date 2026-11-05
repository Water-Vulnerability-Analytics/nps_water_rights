#' Calculate Coastal Inundation Sensitivity Indicator
#'
#' Calculates coastal sensitivity using distance to the nearest NOAA CUSP
#' shoreline feature. Requires a pre-processed GeoPackage from `process_CUSP_data()`.
#' 
#' Citation for 50km buffer: Jasechko, S., Perrone, D., Seybold, H., Fan, Y., & Kirchner, J.W. (2020). Groundwater level observations in 250,000 coastal US wells reveal scope of potential seawater intrusion. Nature Communications, 11, 3229.
#'
#' @param water_supplies sf object with water supply point locations
#' @param cusp_gpkg_path character, path to the combined CUSP shorelines GeoPackage
#' @param save logical, whether to save output
#' @param output_dir character, directory path for saving files
#' @param verbose logical, whether to show progress messages
#'
#' @return tibble with water supply ID, park info, and coastal sensitivity indicators
#'
calc_inundation_sensitivity <- function(water_supplies,
                                        cusp_gpkg_path = here("data", "rapid", "inputs", "coast_comp", "cusp_shorelines.gpkg"),
                                        save = FALSE,
                                        output_dir = NULL,
                                        verbose = TRUE) {
  
  # Input validation
  stopifnot(
    "water_supplies must be an sf object" = inherits(water_supplies, "sf"),
    "save is TRUE but output_dir not provided" = !save || !is.null(output_dir),
    "cusp_gpkg_path does not exist — run process_CUSP_data() first" = file.exists(cusp_gpkg_path)
  )
  
  if (verbose) message("Reading CUSP shorelines...")
  
  cusp <- sf::st_read(cusp_gpkg_path, quiet = TRUE) %>%
    sf::st_transform(5070) # NAD83 / CONUS Albers (meters)
  
  ws_proj <- water_supplies %>%
    sf::st_transform(5070)
  
  # Start timing
  start_time <- Sys.time()
  
  # Compute distance to nearest CUSP feature for ALL water supplies
  nearest_idx <- st_nearest_feature(ws_proj, cusp)
  
  dist_to_nearest <- st_distance(ws_proj,
                                 cusp[nearest_idx, ],
                                 by_element = TRUE)
  
  ws_proj$nearest_id <- nearest_idx
  ws_proj$dist_to_nearest <- dist_to_nearest
  
  # Citation: Jasechko et al. (2020) — seawater intrusion scope extends ~50km inland
  # Supplies > 50km from coast are assigned NA (not at risk of coastal intrusion)
  
  results <- ws_proj %>%
    mutate(dist_to_nearest = as.numeric(dist_to_nearest),
           raw_shore_dist_m = if_else(dist_to_nearest > 50000, NA_real_, dist_to_nearest)) %>%
    select(wsd_source_id,
           park_unit,
           park_name,
           raw_shore_dist_m) %>%
    st_drop_geometry()

  # invert = TRUE: greater distance from shore = lower sensitivity score
  # Standardization only over coastal sites (non-coastal have NA raw_shore_dist_m)
  final_results <- results %>%
    add_standardized_indicators("raw_shore_dist", invert = TRUE)
  
  # Save results
  if (save) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    filename <- file.path(output_dir, paste0("inundation_sensitivity_", Sys.Date(), ".csv"))
    write_csv(final_results, filename)
    
    if (verbose) message("Results saved to: ", filename)
  }
  
  # Print timing
  if (verbose) {
    end_time <- Sys.time()
    total_time <- end_time - start_time
    message("\nCoastal sensitivity processing completed in ",
            round(total_time, 2), " ", attr(total_time, "units"))
  }
  
  return(final_results)
}
