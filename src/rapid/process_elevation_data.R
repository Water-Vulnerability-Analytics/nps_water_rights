#' Process Water Supply Elevations
#'
#' Extracts elevation in meters for coastal NPS water supplies using the USGS
#' Elevation Point Query Service and saves the results as an intermediary CSV
#' for use by `calc_storm_surge_exposure()`.
#'
#' Only supplies within 50 km of the coast are processed because supplies
#' farther inland receive a coastal storm-surge weight of zero.
#'
#' @param water_supplies sf object containing water supply point locations
#' @param coast_distances character path to coast_distances.csv
#' @param max_coast_dist_m numeric maximum coastal distance to process
#' @param ncpu numeric number of parallel workers
#' @param save logical, whether to save the output
#' @param output_file character, path where the elevation CSV will be saved
#' @param verbose logical, whether to show progress messages
#'
#' @return tibble containing water supply identifiers and site elevation
#'
process_elevation_data <- function(water_supplies,
                                   coast_distances = here("data", "rapid", "inputs", "coast_comp", "coast_distances.csv"),
                                   max_coast_dist_m = 50000,
                                   ncpu = 6,
                                   save = TRUE,
                                   output_file = here("data", "rapid", "inputs", "water_supply_elevations.csv"),
                                   verbose = TRUE) {
  
  # Input validation
  stopifnot(
    "water_supplies must be an sf object" =
      inherits(water_supplies, "sf"),
    "coast distances file does not exist" =
      file.exists(coast_distances),
    "save is TRUE but output_file was not provided" =
      !save || !is.null(output_file))
  
  coast_distances_df <- readr::read_csv(coast_distances,
                                        show_col_types = FALSE) %>%
    dplyr::filter(coast_dist_m <= max_coast_dist_m) %>%
    dplyr::select(wsd_source_id, park_unit, park_name)
  
  # Only retain coastal supplies
  water_supply_points <- water_supplies %>%
    dplyr::semi_join(
      coast_distances_df,
      by = c("wsd_source_id","park_unit", "park_name")) %>%
    dplyr::select(wsd_source_id,
                  park_unit,
                  park_name)
  
  if (verbose) {
    message(
      "Extracting elevation for ",
      nrow(water_supply_points),
      " coastal water supplies..."
    )
  }
  
  # Extract elevations in parallel
  elevations <- elevatr::get_elev_point(
    locations = water_supply_points,
    src = "epqs",
    units = "meters",
    ncpu = ncpu,
    serial = FALSE
  ) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(
      wsd_source_id,
      park_unit,
      park_name,
      site_elevation_m = elevation
    )
  
  # Save
  if (save) {
    output_dir <- dirname(output_file)
    
    if (!dir.exists(output_dir)) {
      dir.create(
        output_dir,
        recursive = TRUE
      )
    }
    
    readr::write_csv(
      elevations,
      output_file
    )
    
    if (verbose) {
      message("Elevations saved to: ", output_file)
    }
  }
  
  return(elevations)
}