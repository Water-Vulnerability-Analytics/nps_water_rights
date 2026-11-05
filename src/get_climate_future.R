#' Download and tidy future MACA climate for points or polygons
#'
#' @description
#' Fetches daily MACA downscaled climate with `climateR::getMACA()` for an `sf`
#' object (points or polygons), harmonizes CRS, and returns a tidy table with
#' per-cell coordinates (`x`, `y`), `date`, precipitation (mm), and temperatures
#' (°C). Supports multiple model–scenario combinations (`GCM`) and optional
#' edge-inclusive extraction for polygons.
#'
#' @details
#' ## Geometry support
#' - **POLYGON / MULTIPOLYGON**: downloads the raster stack over each AOI; if an AOI spans
#'   multiple MACA cells, returns one row per *cell–date*.
#' - **POINT**: returns the time series for the cell containing the point.
#'
#' ## Exact mode and coordinate shifting
#' - `exact = TRUE` (polygons only): expands the AOI by 4 km and uses
#'   `terra::extract(..., touches = TRUE)` so cells that merely *touch*
#'   the AOI are included.
#' - When `exact = TRUE`, you must pass `gridmet_points` (an `sf` POINT layer of
#'   **historical/GRIDMET** sample locations with `x`/`y` columns). After extracting
#'   the MACA data, the function **reassigns** each MACA cell’s coordinates
#'   to the **nearest** historical/GRIDMET point using `sf::st_nearest_feature()`.
#'   This produces a single reconciled set of coordinates (`x`, `y`) for both
#'   historical (GRIDMET) and future (MACA) datasets downstream, preventing
#'   sub-grid mismatches when extracting soils, slope, aspect, etc.
#'   - If `gridmet_points` has a different CRS, it is transformed internally.
#'   - In the returned table, the `x`/`y` columns are the **reconciled GRIDMET
#'     coordinates** after shifting (not the original MACA cell centroids).
#' - `exact = FALSE`: crops/masks with cell interiors only; no coordinate shifting
#'   is performed and `x`/`y` reflect the MACA cell centers for the extraction.
#'
#' ## Models & scenarios
#' - `GCM` entries should be strings like `"HadGEM2-CC365.rcp45"`.
#' - If any element equals `"all"`, a curated set of NPS-used model–scenario
#'   combinations for RCP 4.5 & 8.5 is used internally.
#'
#' ## Variables & units
#' - Defaults: `vars = c("tasmax","tasmin","pr")`.
#' - Temperatures (K) are converted to °C:
#'   `tmax_C`, `tmin_C`, and `tmean_C = (tmax_C + tmin_C)/2`.
#' - Precipitation is returned as daily totals in mm (`ppt_mm`).
#' - Other MACA variables available: 
#'    tasmax—Maximum daily temperature near surface
#'    tasmin—Minimum daily temperature near surface
#'    rhsmax—Maximum daily relative humidity near surface
#'    rhsmin—Minimum daily relative humidity near surface
#'    huss—Average daily specific humidity near surface
#'    pr—Average daily precipitation amount at surface
#'    rsds—Average daily downward shortwave radiation at surface
#'    was—Average daily wind speed near surface
#'    uas—Average daily eastward component of wind near surface
#'    vas—Average daily northward component of wind near surface
#'
#' ## Identifier handling
#' - The column named in `col_name` is temporarily renamed to `join_index`
#'   internally and restored in the output to ease joins.
#'
#' @param sf An `sf` object of areas of interest (points or polygons).
#' @param col_name Bare column name in `sf` that uniquely identifies each AOI;
#'   this identifier is preserved in the output.
#' @param start Start date (inclusive) as `"YYYY-MM-DD"`. Default `"2023-01-01"`.
#' @param end End date (inclusive) as `"YYYY-MM-DD"`. Default `"2070-01-01"`.
#' @param vars Character vector of MACA variables to download. Default
#'   `c("tasmax","tasmin","pr")`.
#' @param GCM Character vector of model–scenario strings (e.g.,
#'   `"HadGEM2-CC365.rcp45"`). If it contains `"all"`, a predefined set of
#'   NPS-used futures is used.
#' @param exact Logical; if `TRUE`, include cells that merely touch the AOI
#'   boundary (edge-inclusive extraction). Default `FALSE`.
#' @param gridmet_points Required only when exact is `TRUE`, an `sf` POINT layer representing the
#'   **historical/GRIDMET** sample locations used elsewhere in your workflow.
#'   Must carry numeric `x` and `y` columns (lon/lat in EPSG:4326 preferred).
#'   When provided and `exact = TRUE`, future MACA rows are **shifted** to the
#'   nearest of these points so that `x`/`y` in the output match the historical
#'   coordinate set.  
get_climate_future <- function(sf,
                               col_name,
                               start = "2023-01-01",
                               end = "2070-01-01",
                               vars = c("tasmax", "tasmin", "pr"),
                               GCM = select_cfs$GCM,
                               exact = FALSE,
                               gridmet_points = NULL) {
  
  if(any(GCM == "all")){
    
    GCM  <- c("HadGEM2-ES365.rcp45", 
              "HadGEM2-ES365.rcp85",
              "HadGEM2-CC365.rcp45", 
              "HadGEM2-CC365.rcp85", 
              "CNRM-CM5.rcp45",      
              "CNRM-CM5.rcp85",
              "CCSM4.rcp45", 
              "CCSM4.rcp85",
              "CanESM2.rcp45",
              "CanESM2.rcp85",       
              "MIROC5.rcp45",
              "MIROC5.rcp85",
              "IPSL-CM5A-MR.rcp45",
              "IPSL-CM5A-MR.rcp85", 
              "NorESM1-M.rcp45",     
              "NorESM1-M.rcp85",
              "bcc-csm1-1-m.rcp45",
              "bcc-csm1-1-m.rcp85",
              "IPSL-CM5A-LR.rcp45",
              "IPSL-CM5A-LR.rcp85",  
              "CSIRO-Mk3-6-0.rcp45",
              "CSIRO-Mk3-6-0.rcp85", 
              "BNU-ESM.rcp45",
              "BNU-ESM.rcp85",
              "MRI-CGCM3.rcp45",     
              "MRI-CGCM3.rcp85",
              "GFDL-ESM2M.rcp45", 
              "GFDL-ESM2M.rcp85",
              "GFDL-ESM2G.rcp45",
              "GFDL-ESM2G.rcp85",    
              "MIROC-ESM.rcp45", 
              "MIROC-ESM.rcp85", 
              "inmcm4.rcp45", 
              "inmcm4.rcp85",
              "MIROC-ESM-CHEM.rcp45",
              "MIROC-ESM-CHEM.rcp85",
              "bcc-csm1-1.rcp45",
              "bcc-csm1-1.rcp85", 
              "IPSL-CM5B-LR.rcp45", 
              "IPSL-CM5B-LR.rcp85") 
    
  }
  
  sf <- sf %>%
    dplyr::rename("join_index" = {{col_name}})
  
  all_climate_data <- vector("list", length = nrow(sf) * length(GCM))
  idx <- 0
  models <- str_extract(GCM, "^[^.]+")     # Get models to run
  scenarios <- str_extract(GCM, "(?<=\\.).*")     # Get scenarios (rcps) 
  
  if(any(unique(sf::st_geometry_type(sf)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    
    for (i in 1:nrow(sf)) {
      
      aoi <- sf[i,]
      
      for (j in 1:length(GCM)) {
        
        idx <- idx + 1
        
        print(paste0('Downloading MACA ', models[j], '-', scenarios[j], ' for ', aoi$join_index, "."))
        
        if(exact == TRUE) {
          # download climate data using a 4km buffer to ensure full cell capture at aoi edges
          # getGridMET does NOT always grab all the grid cells that touch an AOI
          pull_aoi <- sf::st_buffer(aoi, 4000)
        } else {
          # download climate data NOT using a 4km buffer to ensure full cell capture at aoi edges
          # (how it was done in BRCA, VALL)
          pull_aoi <- aoi
        }
        
        if (year(as.Date(start)) <= 2005 & year(as.Date(end)) > 2005) {
          
          # 2005 marks the start of RCP forcings. If start and end date span
          # 2005-2006, need to pull in separately
          clim_pre <- climateR::getMACA(AOI = pull_aoi, 
                                        varname = vars,
                                        model = models[j],
                                        scenario = scenarios[j],
                                        startDate = start,
                                        endDate = "2005-12-31") #%>%
          #  rename_with(
          #    .fn = ~ stringr::str_replace(., "historical", scenarios[j]),
          #    .cols = contains("historical") 
          #  )
          
          clim_post <- climateR::getMACA(AOI = pull_aoi, 
                                         varname = vars,#,"pet","vs"),
                                         model = models[j],
                                         scenario = scenarios[j],
                                         startDate = "2006-01-01",
                                         endDate = end) 
          
          clim <- c(clim_pre, clim_post) 
        
        } else {
          
          clim <- climateR::getMACA(AOI = pull_aoi, 
                                    varname = vars,
                                    model = models[j],
                                    scenario = scenarios[j],
                                    startDate = start,
                                    endDate = end) 
          
        }
        
        # If the result is a spatraster (i.e., AOI spanned more than 1 grid cell)
        if(!is.data.frame(clim) | inherits(clim[[1]], "SpatRaster")) {
          
          clim_crs <- sf::st_crs(clim[[1]])
          
          if(exact == TRUE){
            # transform points to match raster CRS if needed
            if(st_crs(gridmet_points) != clim_crs) {
              gridmet_points_trans <- sf::st_transform(gridmet_points, crs = clim_crs)
            } else {
              gridmet_points_trans <- gridmet_points
            }
            # crop with extract to grab all touching 
            clim <- clim %>%
              purrr::map(~terra::extract(., terra::vect(gridmet_points_trans), touches = TRUE, xy = TRUE, ID = FALSE))
            
          } else if(exact == FALSE){
            # transform AOI to match raster CRS if needed
            if(st_crs(aoi) != clim_crs) {
              aoi_trans <- sf::st_transform(aoi, crs = clim_crs)
            } else {
              aoi_trans <- aoi
            }
            # otherwise original method using crop (only complete grids returned)
            clim <- clim %>%
              purrr::map(~terra::crop(., aoi_trans, mask = TRUE))
            
          }
          
          all_climate_data[[idx]] <- clim %>%
            purrr::map_dfr(~ terra::as.data.frame(., xy = TRUE)) %>%
            data.table::data.table() %>%
            tidyr::pivot_longer(-(x:y),
                                names_to = "var_temp",
                                values_to = "val") %>%
            tidyr::separate_wider_delim(var_temp, "_", 
                                        names = c("var","date","GCM",
                                                  "Ensemble","RCP")) %>%
            tidyr::drop_na(val) %>%
            dplyr::group_by(x, y, date) %>%
            tidyr::pivot_wider(names_from = "var", 
                               values_from = "val") %>%
            dplyr::mutate(date = as.Date(date),
                          ppt_mm = pr,
                          tmax_C = tasmax - 273.15,
                          tmin_C = tasmin - 273.15,
                          tmean_C = (tmax_C + tmin_C)/2,
                          join_index = aoi$join_index) %>%
            dplyr::select(-c("tasmax","tasmin","pr")) %>%
            dplyr::mutate(RCP = str_replace(RCP, "historical", scenarios[j]))
          
        } else {
          
          all_climate_data[[idx]] <- clim %>%
            data.table::data.table() %>%
            # names of columns include va_mode_rcp so must rename
            dplyr::rename_with(~ stringr::str_split(.x, "_", n = 2) %>% purrr::map_chr(1)) %>%
            # since polygon grabbed a single grid, does not provide the coordinates
            # of the gridMET cell, so we fill in x and y with the coordinates
            # of the sf object:
            dplyr::mutate(x = sf::st_coordinates(aoi)[[1]],
                          y = sf::st_coordinates(aoi)[[2]]) %>%
            # Then do all other cleaning steps done for polygon sf objects:
            dplyr::mutate(date = as.Date(date),
                          ppt_mm = pr,
                          tmax_C = tasmax - 273.15,
                          tmin_C = tasmin - 273.15,
                          tmean_C = (tmax_C + tmin_C)/2,
                          join_index = aoi$join_index,
                          GCM = models[j],
                          RCP = scenarios[j],
                          Ensemble = "r1i1p1") %>%
            dplyr::select(-c("tasmax","tasmin","pr"))
          
        }
      }
    }
    
    all_climate_data <- all_climate_data %>%
      dplyr::bind_rows()
    
    
    if(exact == TRUE){
      
      points <- all_climate_data %>%
        dplyr::group_by(x, y) %>%
        dplyr::summarise(.groups = "drop") %>%
        sf::st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE) %>%
        dplyr::select("old_x" = x, "old_y" = y) 
      
      # ensure gridmet_points has coords & is POINT sf
      if (sf::st_crs(gridmet_points)$epsg != 4326){
        
        gridmet_points <- st_transform(gridmet_points, 4326)
      }
      
      # for each row in `points`, find the index of its single nearest historical point
      nn_idx <- sf::st_nearest_feature(points, gridmet_points)  # length == nrow(points)
      
      # buffer by 800 m safely (transform to a meters-based CRS)
      points <- points %>%
        dplyr::mutate(rowid = nn_idx) %>%
        left_join(st_drop_geometry(gridmet_points) %>% rowid_to_column() %>% dplyr::select(x, y, rowid),
                  by = "rowid") %>%
        st_drop_geometry() %>%
        dplyr::select(-rowid)
      
      all_climate_data <- all_climate_data %>%
        dplyr::left_join(points, by = c("x" = "old_x", "y" = "old_y")) %>%
        dplyr::ungroup() %>%
        dplyr::select(-c(x,y)) %>%
        dplyr::rename(x = x.y, y = y.y)
      
      
    }
    
    # Rename the join_index column
    colnames(all_climate_data)[colnames(all_climate_data) == "join_index"] <- {{col_name}}
    
    return(all_climate_data)
    
    
    # IF the input geometry is a point...
  } else if(unique(sf::st_geometry_type(sf)) == "POINT") {
    
    for (i in 1:nrow(sf)) {
      
      aoi <- sf[i,]
      
      for (j in 1:length(GCM)) {
        
        idx <- idx + 1
        
        print(paste0('Downloading MACA for ', aoi$join_index, "."))
        
        if (year(as.Date(start)) <= 2005 & year(as.Date(end)) > 2005) {
          
          # 2005 marks the start of RCP forcings. If start and end date span
          # 2005-2006, need to pull in separately
          clim_pre <- climateR::getMACA(AOI = aoi, 
                                        varname = vars,#,"pet","vs"),
                                        model = models[j],
                                        scenario = scenarios[j],
                                        startDate = start,
                                        endDate = "2005-12-31") %>%
            rename_with(
              .fn = ~ stringr::str_replace(., "historical", scenarios[j]),
              .cols = contains("historical") 
            )
          
          clim_post <- climateR::getMACA(AOI = aoi, 
                                         varname = vars,#,"pet","vs"),
                                         model = models[j],
                                         scenario = scenarios[j],
                                         startDate = "2006-01-01",
                                         endDate = end) 
          
          clim <- clim_pre %>%
            bind_rows(clim_post) 
          
        } else {
        
        clim <- climateR::getMACA(AOI = aoi, 
                                  varname = vars,#,"pet","vs"),
                                  model = models[j],
                                  scenario = scenarios[j],
                                  startDate = start,
                                  endDate = end) 
        }
        
        all_climate_data[[idx]] <- clim %>%
          data.table::data.table() %>%
          # names of columns include va_mode_rcp so must rename
          dplyr::rename_with(~ stringr::str_split(.x, "_", n = 2) %>% purrr::map_chr(1)) %>%
          # since point pulls from gridMET do not provide the coordinates
          # of the gridMET cell, we fill in x and y with the coordinates
          # of the sf object:
          dplyr::mutate(x = sf::st_coordinates(aoi)[[1]],
                        y = sf::st_coordinates(aoi)[[2]]) %>%
          # Then do all other cleaning steps done for polygon sf objects:
          dplyr::mutate(date = as.Date(date),
                        ppt_mm = pr,
                        tmax_C = tasmax - 273.15,
                        tmin_C = tasmin - 273.15,
                        tmean_C = (tmax_C + tmin_C)/2,
                        join_index = aoi$join_index,
                        GCM = models[j],
                        RCP = scenarios[j],
                        Ensemble = "r1i1p1") %>%
          dplyr::select(-c("tasmax","tasmin","pr"))
        
      }
    }
    
    all_climate_data <- all_climate_data %>%
      dplyr::bind_rows()
    
    # Rename the join_index column
    colnames(all_climate_data)[colnames(all_climate_data) == "join_index"] <- {{col_name}}
    
    return(all_climate_data)
    
  } else {
    stop("Your sf feature is neither a polygon nor point feature, or it needs to be made valid.")
  }
  
}




