#' @description
#' Wrapper function that (1) derives site parameters for each input point (or
#' area-weighted grid cell set), (2) subsets the provided daily climate
#' time series by point and GCM, (3) runs `nps_wbm()` for every
#' point–GCM combination, and (4) returns daily summaries averaged across
#' points for each GCM. Optionally returns the final soil storage and
#' snowpack states for reuse in subsequent runs.
#'
#' @details
#' Site parameters are obtained via `get_nps_wbm_params()` unless `param_list`
#' is provided. When `area_weighted = FALSE`, parameters are extracted at exact 
#' point locations. When `area_weighted = TRUE`, you must provide both `aoi`
#' (polygon) and `grid` (polygonal grid) for area-weighted aggregation.
#'
#' For each GCM listed in `climate_data$GCM`, the function filters the
#' daily records by the point's `x`/`y` and the current GCM, runs
#' `nps_wbm()` with the chosen PET formulation and other controls, and
#' stores outputs in a long table (mm → inches inside `nps_wbm()` for
#' fluxes). Daily results are then averaged across points when 
#' `area_weighted = FALSE`, or computed as area-weighted means when
#' `area_weighted = TRUE` using normalized area weights.
#'
#' If `ret_final_cond = TRUE`, the function also tracks the final‐day
#' soil storage and snowpack (`SOIL`, `PACK`) for each point and returns
#' them alongside the main output. If `use_final = TRUE`, those saved
#' states are used to initialize the next run's `Soil.Init` and
#' `Snowpack.Init`, and `param_list` must be provided containing the
#' final conditions from a previous run.
#'
#' @param points An `sf` point object for model locations.
#' @param climate_data A data frame/tibble of daily climate for all points
#'   and GCMs. Must include, at minimum:  
#'   - `x` (lon, decimal degrees), `y` (lat, decimal degrees),  
#'   - `date` (Date; daily, regular),  
#'   - `ppt_mm` (precipitation [mm/day]),  
#'   - `tmean_C` (mean air temperature [°C]),  
#'   - `GCM` (character model/scenario identifier).  
#'   Additional columns are passed through where relevant.
#' @param Direct_Frac Fraction of rainfall routed directly to runoff
#'   (0–1). Default `0`.
#' @param Return_Rate Daily fraction of the storage reservoir released to
#'   runoff (0–1], where `1` = full same-day release. Default `1`.
#' @param PET_mult Multiplicative PET bias adjustment. Default `1`.
#' @param Soil_mult Multiplicative adjustment to soil storage capacity
#'   (`SWC.Max`). Default `1`.
#' @param PET_Method Character; one of `c("Oudin","Hamon","Penman-Monteith")`.
#'   Controls PET formulation.
#' @param hock_coef Degree-day melt factor for Hock melt model. Default `4`.
#' @param Snowpack.Init Initial snowpack water equivalent [mm]. Default `0`.
#' @param Soil.Init Initial soil water content [mm]. Default `0`.
#' @param Shade.Coeff Shading coefficient used by Oudin PET variant.
#'   Default `1`.
#' @param T.Base Base temperature for growing degree days [°C]. Default `0`.
#' @param area_weighted Logical; if `TRUE`, compute area-weighted site
#'   parameters over `grid` within `aoi`, and return area-weighted daily
#'   summaries using normalized area weights. Default `FALSE`.
#' @param aoi `sf` polygon AOI used when `area_weighted = TRUE`. Default `NULL`.
#' @param grid `sf` polygonal gridMET grids used for area-weighted extraction when
#'   `area_weighted = TRUE`. Default `NULL`.
#' @param ret_final_cond Logical; if `TRUE`, also return a second object
#'   with per-point final soil and snowpack states. Default `FALSE`.
#' @param use_final Logical; if `TRUE`, initialize each point's run with
#'   its stored final soil and snowpack states (requires `param_list` 
#'   containing final conditions from a previous run). Default `FALSE`.
#' @param param_list Optional data frame containing pre-computed site parameters
#'   with final soil and snowpack conditions. Required when `use_final = TRUE`.
#'   When provided, skips parameter derivation via `get_nps_wbm_params()`.
#'   Default `NULL`.
#' @param aggregate If TRUE, summarizes each variable for AOI after running. If 
#'   FALSE, returns water balance results for each grid cell retaining x and y
#'   
#' @return When `ret_final_cond = FALSE`, returns a data frame of daily water
#'   balance results averaged across points (or area-weighted when 
#'   `area_weighted = TRUE`) for each GCM. When `ret_final_cond = TRUE`, 
#'   returns a list with two elements: the daily results data frame and 
#'   the points parameters data frame containing final soil and snowpack 
#'   states for subsequent runs.
#'   
#' @note Performance scales with (#points × #GCMs × #days).
run_NPS_WBM_points <- function(points, 
                               climate_data,
                               Direct_Frac = 0, 
                               Return_Rate = 1, 
                               PET_mult = 1,  
                               Soil_mult = 1, 
                               PET_Method = c("Oudin", "Hamon", "Penman-Monteith"), 
                               hock_coef = 4, 
                               Snowpack.Init = 0,   # according to Mike's code
                               Soil.Init = 0, # Soil.Init is full (1) from Mike's code, but empty (0) in Amber's
                               Shade.Coeff = 1,
                               T.Base = 0, 
                               area_weighted = FALSE, 
                               aoi = NULL, 
                               grid = NULL,
                               ret_final_cond = FALSE,
                               use_final = FALSE,
                               param_list = NULL,
                               aggregate = TRUE) {
  
 if(use_final == TRUE){
   
   points_params <- param_list
   
   
 } else if (ret_final_cond == TRUE) {
  # Get parameters at each point (slope, aspect, elevation)
  points_params <- get_nps_wbm_params(points, area_weighted = area_weighted,
                                      aoi = aoi, grid = grid) 
  
  
    points_params <- points_params %>%
      dplyr::mutate(final_soil = rep(0, nrow(points_params)),
                    final_snowpack = rep(0, nrow(points_params)))
    
 } else{
    
   points_params <- get_nps_wbm_params(points, area_weighted = area_weighted,
                                       aoi = aoi, grid = grid) 
  } 
  
  # get array of all GCMs for projections
  all_gcms <- climate_data %>%
    dplyr::pull(GCM) %>%
    unique()
  
  # Now, initiate output data storage as lists
  DailyWB_all <- vector(mode = "list",
                        length = length(all_gcms))
  
  DailyWB_pts <- vector(mode = "list",
                        length = nrow(points_params))
  
  print("Running wbm...")
  # First loop through each gcm
  for (j in 1:length(all_gcms)) {
    print(all_gcms[j])
    # Now loop through each point in each GCM
    for (i in 1:nrow(points_params)) {
      print(paste0("point: ", as.character(i)))
      
      # If using final soil storage or snowmelt from a previous run,
      # assign it here.
      if (use_final == TRUE) {
        Soil.Init <- points_params[i,]$final_soil
        Snowpack.Init <- points_params[i,]$final_snowpack
      }
      
      # Subset DailYWB for select GCM and Point
      DailyWB <- climate_data %>%
        data.table::data.table() %>%
        dplyr::filter(x == points_params[i,x],
                      y == points_params[i,y],
                      GCM == all_gcms[j]) 
      
      # Run water balance model for each point
      DailyWB_pts[[i]] <- nps_wbm(DailyWB = DailyWB,
                                  point_params = points_params[i,],
                                  Direct_Frac = Direct_Frac, 
                                  Return_Rate = Return_Rate, 
                                  PET_mult = PET_mult,  
                                  Soil_mult = Soil_mult, 
                                  PET_Method = PET_Method, 
                                  hock_coef = hock_coef, 
                                  Snowpack.Init = Snowpack.Init,  
                                  Soil.Init = Soil.Init, 
                                  Shade.Coeff = Shade.Coeff,
                                  T.Base = T.Base) %>% 
        data.table::data.table() 
      
      if(area_weighted == TRUE){
        
        DailyWB_pts[[i]] <- DailyWB_pts[[i]] %>%
          dplyr::mutate(area_weight = as.numeric(points_params[i,"area_weight"]))
        
      }
      
      # Save soil storage and snowpack from final timestep
      if (ret_final_cond == TRUE) {
        points_params[i,]$final_soil <- DailyWB_pts[[i]] %>% dplyr::filter(vars == "SOIL") %>% tail(n = 1) %>% pull(vals)
        points_params[i,]$final_snowpack <- DailyWB_pts[[i]] %>% dplyr::filter(vars == "PACK") %>% tail(n = 1) %>% pull(vals)
      }
      
    }
    
    if (area_weighted == TRUE) {
    # Summarize daily values for each point    
    DailyWB_all[[j]] <- DailyWB_pts %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(date, vars, GCM) %>%
      dplyr::mutate(area_weight_norm = area_weight / sum(area_weight, na.rm = TRUE)) 
    
      if (aggregate == TRUE) {
        DailyWB_all[[j]] <- DailyWB_all[[j]] %>%
          dplyr::summarize(vals = weighted.mean(vals, w = area_weight_norm, na.rm = TRUE), .groups = "drop")

      }
      
      } else {
    
      # Summarize daily values for each point    
      DailyWB_all[[j]] <- DailyWB_pts %>%
        dplyr::bind_rows() 
    
      if (aggregate == TRUE) {
        DailyWB_all[[j]] <- DailyWB_all[[j]] %>%
          dplyr::group_by(date, vars, GCM) %>%
          dplyr::select(-c(x, y)) %>%
          dplyr::summarize(vals = mean(vals, na.rm = TRUE), .groups = "drop") 
        }
    
    }
    
  }
  
  # Bind together all results for all GCMs
  DailyWB_ret <- DailyWB_all %>% 
    dplyr::bind_rows() %>%
    data.table::data.table() %>%
    tidyr::pivot_wider(names_from = vars, values_from = vals) %>%
    dplyr::rename(runoff_in_wbm = RUNOFF,
                  rain_in_wbm = RAIN,
                  snow_in_wbm = SNOW,
                  melt_in_wbm = MELT,
                  aet_in_wbm = AET,
                  precip_in_wbm = ppt_mm,
                  pet_in_wbm = PET_mod,
                  excess_water_in_wbm = excess_wat,
                  soil_in_wbm = SOIL,
                  snowpack_in_wbm = PACK
    )
  
  # Return either list containing WB results and points param or
  # just dataframe containing results depending on whether returning final 
  # conditions.
  if (ret_final_cond == TRUE) {
    return(list(DailyWB_ret, points_params))
  } else {
    return(DailyWB_ret)
  }
}



#test <- run_NPS_WBM_points(points, climate_data, PET_Method = "Oudin", ret_final_cond = "TRUE")
