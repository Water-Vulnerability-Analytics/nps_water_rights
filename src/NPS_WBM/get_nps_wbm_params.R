#' Extract NPS WBM parameters at point locations or as area-weighted grid-cell summaries
#'
#' @description
#' Retrieves key inputs for the NPS Water Balance Model (WBM)—elevation, slope,
#' aspect, soil water storage capacity, and January temperature—either at exact
#' point locations or as **area-weighted** means over grid cells intersecting
#' those points within an AOI. Aspect is summarized using a weighted circular
#' mean.
#'
#' @details
#' If the global rasters `dem`, `soil`, `slope`, `aspect`, and `jtemp` are not
#' found in the session, the function calls `load_wbm_rasters(crs = st_crs(points))`
#' to load them (projected to the CRS of `points`).  
#'
#' - When `area_weighted = FALSE`, values are extracted at each input point. This
#'   is the default.
#' - When `area_weighted = TRUE`, you must supply both a polygon `aoi` and a
#'   polygonal `grid` of the associated GridMET cells. The function (i) transforms 
#'   `grid` to the CRS of `points` if needed, (ii) finds grid cells that 
#'   intersect the points and lie within `aoi`, and (iii) computes area-weighted
#'   means for elevation, slope, soil water capacity, and Jennings temperature 
#'   using `terra::extract(..., weights = TRUE, exact = TRUE)`. Aspect is 
#'   summarized with a weighted circular mean (cos/sin).
#'
#' @param points An `sf` point object: locations at which to extract WBM parameters.
#' @param area_weighted Logical. If `FALSE` (default), extract values at points.
#'   If `TRUE`, compute area-weighted summaries over intersecting `grid` cells
#'   within `aoi`.
#' @param aoi (Required when `area_weighted = TRUE`) An `sf` polygon defining the
#'   area of interest used to clip intersecting grid cells.
#' @param grid (Required when `area_weighted = TRUE`) An `sf` polygon grid used
#'   for area-weighted aggregation. Will be transformed to `st_crs(points)` if
#'   CRSs differ.
#'
#' @return
#' A `data.table`/`data.frame` with WBM parameter columns:
#' - `Elev` (elevation),
#' - `Slope`,
#' - `Aspect` (degrees 0–360; circular mean when area-weighted),
#' - `SWC.Max` (soil water storage capacity),
#' - `J.Temp` (Jennings temperature),
#' plus the original attributes of `points` (when `area_weighted = FALSE`) or
#' the non-geometry attributes of intersecting `grid` cells (when `TRUE`).
#' 
get_nps_wbm_params <- function(points, area_weighted = FALSE, aoi = NULL, grid = NULL) {
  
  if (!(exists("dem") && exists("soil")  && exists("jtemp"))) {
    load_wbm_rasters(crs = terra::crs(points))
  }
  
  if(area_weighted == FALSE){
    
    # Extract params at each gridMET centroid point
    point_params <- points %>% 
      data.table::data.table() %>%
      dplyr::mutate(Elev = terra::extract(dem, points)$elevation_cropped,
                    Aspect = terra::extract(aspect, points)$aspect,
                    Slope = terra::extract(slope, points)$slope,
                    SWC.Max =  terra::extract(soil, points)$water_storage,
                    J.Temp = terra::extract(jtemp, points)$merged_jennings2_1)
    
    return(point_params)
    
  } else if(area_weighted == TRUE){
    
    if (is.null(grid)) {
      stop("A grid for `grid` argument must be provided when area_weighted = TRUE")
    }
    
    if (is.null(aoi)) {
      stop("A polygon for `aoi` argument must be provided when area_weighted = TRUE")
    }
    
    # check CRS:
    if (sf::st_crs(grid) != sf::st_crs(points)) {
      grid <- sf::st_transform(grid, crs = sf::st_crs(points))
    }
    
    # find which grid cells intersect with user-supplied GridMET points:
    grid_point_intersect <- grid %>%
      sf::st_join(., points, left = FALSE) %>%
      sf::st_intersection(., dplyr::select(aoi, geometry)) %>%
      dplyr::mutate(grid_id = dplyr::row_number())  
    
    # extract area-weighted means for non-aspect variables separately
    # (purposefully avoided stacking due to extent mismatches between layers)
    elev_extracts <- terra::extract(dem, 
                                    terra::vect(grid_point_intersect), 
                                    fun = "mean",
                                    weights = TRUE,
                                    exact = TRUE) %>%
      dplyr::select(ID, Elev = names(dem)[1])
    
    slope_extracts <- terra::extract(slope, 
                                     terra::vect(grid_point_intersect), 
                                     fun = "mean",
                                     weights = TRUE,
                                     exact = TRUE) %>% 
      dplyr::select(ID, Slope = names(slope)[1])
    
    soil_extracts <- terra::extract(soil, 
                                    terra::vect(grid_point_intersect), 
                                    fun = "mean",
                                    weights = TRUE,
                                    exact = TRUE) %>% 
      dplyr::select(ID, SWC.Max = names(soil)[1])
    
    jtemp_extracts <- terra::extract(jtemp, 
                                     terra::vect(grid_point_intersect), 
                                     fun = "mean",
                                     weights = TRUE,
                                     exact = TRUE) %>% 
      dplyr::select(ID, J.Temp = names(jtemp)[1])
    
    # handle aspect differently (since aspect is radial...)
    aspect_extracts <- terra::extract(aspect, 
                                      terra::vect(grid_point_intersect), 
                                      weights = TRUE,
                                      exact = TRUE)
    
    # get circular mean from degreees in raw aspect
    aspect_means <- aspect_extracts %>%
      data.table::data.table() %>%
      dplyr::group_by(ID) %>%
      dplyr::summarize(Aspect = {
        # degrees to radians
        angles_rad <- aspect * pi / 180  # Convert to radians
        
        # normalize weights
        weights_norm <- weight / sum(weight)  # Normalize weights
        
        # calculate weighted circular mean
        x_sum <- sum(cos(angles_rad) * weights_norm, na.rm = TRUE)
        y_sum <- sum(sin(angles_rad) * weights_norm, na.rm = TRUE)
        
        # convert back to degrees
        mean_angle <- atan2(y_sum, x_sum) * 180 / pi
        
        # ensure positive angle (0-360) a la Claude
        if (mean_angle < 0) mean_angle + 360 else mean_angle
      }
      )
    
    # combine all the extractions -
    point_grid_combo <- elev_extracts %>%
      data.table::data.table() %>%
      dplyr::left_join(slope_extracts, by = "ID") %>%
      dplyr::left_join(soil_extracts, by = "ID") %>%
      dplyr::left_join(aspect_means, by = "ID") %>%
      dplyr::left_join(jtemp_extracts, by = "ID") %>%
      # join back with original grid attributes
      cbind(grid_point_intersect %>% 
              sf::st_drop_geometry()) %>%
      dplyr::select(-c(grid_id, ID)) 
    
    return(point_grid_combo)
  }
}
