#' Download and tidy historic GridMET climate for points or polygons
#'
#' @description
#' Convenience wrapper around `climateR::getGridMET()` that fetches daily
#' GridMET variables for an `sf` object (points or polygons), harmonizes CRS,
#' and returns a tidy table with per-cell centroid coordinates (`x`, `y`), `date`,
#' precipitation (mm), and temperatures (°C). For polygons, you can choose an
#' **area-weighted** extraction that includes fractional cell overlap weights.
#'
#' @details
#' - **Geometry support:**  
#'   Accepts `POLYGON`, `MULTIPOLYGON`, or `POINT`. Polygons are processed cell-by-cell
#'   (optionally with overlap weights); points return the cell time series at the
#'   point location.
#'
#' - **Exact vs non-exact polygon mode:**  
#'   *`exact = TRUE`* → buffers the AOI by 4 km before download to ensure edge
#'   cells are included, then uses `terra::extract(..., weights = TRUE, exact = TRUE)`
#'   so the output includes an `area_weight` column (fractional overlap per cell).  
#'   *`exact = FALSE`* → crops/masks without weights; `area_weight` is set to `NA`.
#'
#' - **Returned grid (optional):**  
#'   If `return_grid = TRUE` (polygon input), the function creates
#'   **`aoi_gridmet_grid`** in the global environment: an `sf` polygon layer of the
#'   GridMET cells intersecting the AOI in the raster CRS.
#'
#' - **Variables & units:**  
#'   Designed for the trio `vars = c("tmmx","tmmn","pr")` (daily max/min air
#'   temperature in K; precipitation in mm). Temperatures are converted to °C via
#'   `tmax_C = tmmx - 273.15`, `tmin_C = tmmn - 273.15`, and `tmean_C` is their
#'   average. Precipitation is returned as `ppt_mm = pr`.
#'
#' - **Tagging / join key:**  
#'   The column given in `col_name` is temporarily renamed to `join_index`
#'   internally and restored in the output to its original name for joining.
#'
#' @param sf An `sf` object of AOIs: `POLYGON`/`MULTIPOLYGON` or `POINT`.
#' @param col_name Bare column name in `sf` used as an identifier; carried through
#'   to the output (restored from a temporary internal name).
#' @param start,end Character dates `"YYYY-MM-DD"` defining the inclusive time
#'   range to download. Defaults to `"1979-01-01"` and `"2023-12-31"`.
#' @param vars Character vector of GridMET variable names to request. Default
#'   `c("tmmx","tmmn","pr")`. Downstream calculations assume these are present.
#' @param exact Logical. If `TRUE`, perform weighted extraction over polygons
#'   (uses a 4 km buffer to ensure full cell capture and `terra` overlap weights).
#'   If `FALSE`, crop/mask without weights and only fully intersecting cells are 
#'   returned. Ignored for point inputs. Default `FALSE`.
#' @param return_grid Logical. If `TRUE` and `sf` is polygonal, writes an `sf`
#'   layer named `aoi_gridmet_grid` to the **global environment** containing the
#'   intersecting GridMET cell polygons in the raster CRS. Default `FALSE`.


get_climate_historic <- function(sf,
                                 col_name,
                                 start = "1979-01-01",
                                 end = "2023-12-31",
                                 vars = c("tmmx", "tmmn", "pr"),
                                 exact = FALSE,
                                 return_grid = FALSE) {
  
  # 1. Setup and Rename ID column
  sf <- sf %>% dplyr::rename("join_index" = {{col_name}})
  all_climate_data <- vector("list", length = nrow(sf))
  
  if (return_grid) {
    grids <- vector("list", length = nrow(sf))
    names(grids) <- sf$join_index
  }
  
  geom_type <- unique(as.character(sf::st_geometry_type(sf)))
  
  # POLYGON WORKFLOW
  if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    
    for (i in 1:nrow(sf)) {
      aoi <- sf[i,]
      message(paste0('Downloading GridMET for ', aoi$join_index, "..."))
      
      # Download logic
      aoi_fetch <- if(exact) sf::st_buffer(aoi, 4000) else aoi
      clim <- climateR::getGridMET(AOI = aoi_fetch, varname = vars, 
                                   startDate = start, endDate = end)
      
      # Determine CRS and transform AOI
      clim_crs <- terra::crs(clim[[1]])
      aoi_trans <- if(sf::st_crs(aoi) != clim_crs) sf::st_transform(aoi, crs = clim_crs) else aoi
      
      # POPULATE GRID 
      if (return_grid) {
        grids[[i]] <- terra::as.polygons(clim[[1]], values = FALSE, dissolve = FALSE, na.rm = TRUE) %>%
          sf::st_as_sf() %>%
          .[aoi_trans, ]
      }
      
      # Processing Data
      if (!is.data.frame(clim) || inherits(clim[[1]], "SpatRaster")) {
        if (exact) {
          # Weighted Extraction
          all_climate_data[[i]] <- clim %>%
            purrr::map(~terra::extract(., terra::vect(aoi_trans), xy = TRUE, touches = TRUE, weights = TRUE)) %>%
            purrr::map_dfr(~ terra::as.data.frame(., xy = TRUE)) %>%
            data.table::data.table() %>%
            tidyr::pivot_longer(-c(x, y, ID, weight), names_to = "var_temp", values_to = "val") %>%
            tidyr::separate_wider_delim(var_temp, "_", names = c("var", "date")) %>%
            tidyr::drop_na(val) %>%
            dplyr::group_by(x, y, date) %>%
            tidyr::pivot_wider(names_from = "var", values_from = "val") %>%
            dplyr::mutate(date = as.Date(date),
                          ppt_mm = pr,
                          tmax_C = tmmx - 273.15,
                          tmin_C = tmmn - 273.15,
                          tmean_C = (tmax_C + tmin_C)/2,
                          join_index = aoi$join_index) %>%
            dplyr::rename(area_weight = weight) %>%
            dplyr::select(-any_of(c("ID", "tmmx", "tmmn", "pr")))
          
        } else {
          # Standard Crop/Mask
          all_climate_data[[i]] <- clim %>%
            purrr::map(~terra::crop(., terra::vect(aoi_trans), mask = TRUE)) %>%         
            purrr::map_dfr(~ terra::as.data.frame(., xy = TRUE)) %>%
            data.table::data.table() %>%
            tidyr::pivot_longer(-c(x, y), names_to = "var_temp", values_to = "val") %>%
            tidyr::separate_wider_delim(var_temp, "_", names = c("var", "date")) %>%
            tidyr::drop_na(val) %>%
            dplyr::group_by(x, y, date) %>%
            tidyr::pivot_wider(names_from = "var", values_from = "val") %>%
            dplyr::mutate(date = as.Date(date),
                          ppt_mm = if("pr" %in% names(.)) {pr} else {NA_real_},
                          tmax_C = if("tmmx" %in% names(.)) {tmmx - 273.15} else {NA_real_},
                          tmin_C = if("tmmn" %in% names(.)) {tmmn - 273.15} else {NA_real_},
                          tmean_C = (tmax_C + tmin_C)/2,
                          join_index = aoi$join_index,
                          area_weight = NA) %>%
            dplyr::select(-any_of(c("tmmx", "tmmn", "pr")))
        }
      }
    }
    
    # POINTS WORKFLOW
  } else if (any(geom_type == "POINT")) {
    
    for (i in 1:nrow(sf)) {
      aoi <- sf[i,]
      message(paste0('Downloading GridMET for ', aoi$join_index, "..."))
      
      clim <- climateR::getGridMET(AOI = aoi, varname = vars, 
                                   startDate = start, endDate = end)
      
      if (return_grid) {
        # Fetch small grid for point visualization
        point_grid_raw <- climateR::getGridMET(AOI = sf::st_buffer(aoi, 4000), varname = "tmmx", 
                                               startDate = start, endDate = start)
        grids[[i]] <- terra::as.polygons(point_grid_raw[[1]], values = FALSE, dissolve = FALSE) %>%
          sf::st_as_sf() %>%
          .[sf::st_transform(aoi, terra::crs(point_grid_raw[[1]])), ]
      }
      
      all_climate_data[[i]] <- clim %>%
        data.table::data.table() %>%
        dplyr::mutate(x = sf::st_coordinates(sf::st_centroid(aoi))[[1]],
                      y = sf::st_coordinates(sf::st_centroid(aoi))[[2]],
                      date = as.Date(date),
                      ppt_mm = if("pr" %in% names(.)) {pr} else {NA_real_},
                      tmax_C = if("tmmx" %in% names(.)) {tmmx - 273.15} else {NA_real_},
                      tmin_C = if("tmmn" %in% names(.)) {tmmn - 273.15} else {NA_real_},
                      tmean_C = (tmax_C + tmin_C)/2,
                      join_index = aoi$join_index,
                      area_weight = NA) %>%
        dplyr::select(-any_of(c("tmmx", "tmmn", "pr")))
    }
    
  } else {
    stop("Input geometry must be Polygon or Point.")
  }
  
  # FINAL COMBINATION & ASSIGNMENT (POINTS & POLYGONS)
  
  # Combine into one dataframe
  final_df <- dplyr::bind_rows(all_climate_data)
  
  # Restore the original ID column name
  # We use the quosure name to restore the user's specific column name
  id_col_str <- rlang::as_name(rlang::enquo(col_name))
  colnames(final_df)[colnames(final_df) == "join_index"] <- id_col_str
  
  # Global Grid Assignment
  if (return_grid) {
    message("Writing 'aoi_gridmet_grid' to Global Environment.")
    # Bind the grids and add the ID column back
    grid_out <- dplyr::bind_rows(grids, .id = "temp_id")
    colnames(grid_out)[colnames(grid_out) == "temp_id"] <- id_col_str
    assign("aoi_gridmet_grid", grid_out, envir = .GlobalEnv)
  }
  
  return(final_df)
}
