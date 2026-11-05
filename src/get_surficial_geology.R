#' Fetch Surficial Geology Features via ArcGIS REST API
#'
#' This function queries the National Earth Surface v2 FeatureServer and returns
#' geologic units that intersect a provided Area of Interest (AOI). It performs 
#' server-side spatial filtering to minimize data transfer.
#' 
#' REST SOURCE: https://usgs.maps.arcgis.com/home/item.html?id=8323586344b747c6b44731d399ec1307
#' 
#' Relevant Citations:
#' Johnstone, S.A., Colgan, J.P., and Roe, W.P., 2025, A synthesis engine for 
#' constructing geologic maps of the United States: U.S. Geological Survey Data 
#' Report 1210, 36 p., https://doi.org/10.3133/dr1210. 
#'
#' Colgan, J.P., Johnstone, S.A., Campos, J.-M., Platt, B.W., Hirtz, J.A.M., 
#' Barrette, N.C., and Roe, W.P., 2025, Cooperative National Geologic 
#' Map—Earth’s surface geology: U.S. Geological Survey data release, 
#' https://doi.org/10.5066/P146VGVM. 
#'
#' @param aoi An sf or sfc object representing the spatial area to filter by.
#'
#' @return An sf object containing the overlapping geologic features. Returns NULL if the 
#' request fails or 0 rows if no features intersect the AOI.

get_surficial_geology <- function(aoi) {
  
  # --- 1. Input Validation ---
  if (!inherits(aoi, c("sf", "sfc", "sfg"))) {
    stop("Input 'aoi' must be an sf, sfc, or sfg spatial object.")
  }
  
  # Ensure we have a valid CRS to transform from
  if (is.na(st_crs(aoi))) {
    stop("Input 'aoi' has no CRS defined. Please set the CRS using st_set_crs().")
  }
  
  # --- 2. Prepare Spatial Filter ---
  # Transform to WGS84 (EPSG:4326) as it's the most reliable for REST queries
  aoi_4326 <- st_transform(aoi, crs = 4326)
  bb <- st_bbox(aoi_4326)
  
  # Format bounding box for ArcGIS URL: xmin, ymin, xmax, ymax
  bbox_text <- paste(
    round(bb["xmin"], 6), 
    round(bb["ymin"], 6), 
    round(bb["xmax"], 6), 
    round(bb["ymax"], 6), 
    sep = ","
  )
  
  # --- 3. Construct Query ---
  map_server_url <- "https://services.arcgis.com/v01gqwM5QqNysAAi/arcgis/rest/services/National_Earth_Surface_v2/FeatureServer"
  
  # Use URLencode to ensure query parameters are safe
  query_params <- list(
    where = "1=1",
    geometry = bbox_text,
    geometryType = "esriGeometryEnvelope",
    spatialRel = "esriSpatialRelIntersects",
    inSR = "4326",
    outFields = "*",
    returnGeometry = "true",
    f = "geojson"
  )
  
  # Build final URLs
  param_string <- paste0(names(query_params), "=", unlist(query_params), collapse = "&")
  map_units_url <- paste0(map_server_url, "/", 12, "/query?", param_string)
  query_url <- paste0(map_server_url, "/", 6, "/query?", param_string)
  
  # --- 4. Read map units
  map_units_dat <- st_read(map_units_url, quiet = TRUE)
  
  # --- 4. Execute Request with Error Handling ---
  return_dat <- NULL 
  
  tryCatch({
    message("Requesting filtered data from USGS/ArcGIS Service...")
    
    # Use st_read to fetch GeoJSON
    return_dat <- st_read(query_url, quiet = TRUE)
    
    # --- 5. Post-Download Checks ---
    if (nrow(return_dat) == 0) {
      message("Warning: No features found intersecting the provided AOI.")
      
    } else {
      message(sprintf("Success: Downloaded %d features.", nrow(return_dat)))
      
      # Join unit info
      return_dat <- return_dat %>%
        left_join(map_units_dat %>% st_drop_geometry() %>%
                    select(-c(OBJECTID, Symbol, Label)), 
                  by = "MapUnit")
      
      # Check if we likely hit the server's Transfer Limit (usually 1k or 2k)
      # If the count is exactly a 'round' number like 1000, 2000, or 5000, warn the user.
      if (nrow(return_dat) %% 1000 == 0) {
        warning("Result count is a multiple of 1000. You may have hit the server's Max Record Limit.")
      }
    }
    
  }, error = function(e) {
    message("Critical Error: Failed to connect or parse data.")
    message("Details: ", e$message)
    return_dat <- NULL
  })
  
  return(return_dat)
}

# Example
#test <- get_surficial_geology(aoi)
#mapview(park_boundary,
#        color = "seagreen",
#        alpha.regions = 0) +
#  mapview(test,
#          zcol = "MapUnit")
