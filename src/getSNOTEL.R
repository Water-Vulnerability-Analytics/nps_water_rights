#' Pull weather station data for parks
#' 
#' This function imports SNOTEL and SCAN site data
#' 
#' @param aoi An sf polygon/multipolygon object, preferably a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in the same units as the aoi object) to buffer around park boundary
#' @param years The years to pull data for. Default set to "all" which pulls all available years for each site, but can be a string of years 
#' @param timeseries either 'Daily' or 'Hourly
#' @param save Whether to save the resulting file (TRUE/FALSE)
#' @param path If `save == TRUE`, the file path to save to
#' @return An sf object of weather stations within given spatial boundary and attributes
getSNOTEL <- function(aoi, 
                      dist = 0, 
                      years = "all", 
                      timeseries = "Daily", 
                      save = FALSE, 
                      path = NULL) {
    
    # 1. Buffer and Transform AOI 
    aoi_buf <- st_buffer(aoi, dist = dist)
    aoi_wgs84 <- st_transform(aoi_buf, 4326)
    
    # 2. Filter sites quickly using WGS84
    data('SCAN_SNOTEL_metadata', package = 'soilDB')
    sites <- st_as_sf(SCAN_SNOTEL_metadata, coords = c("Longitude", "Latitude"), crs = 4326)
    
    site_aoi <- sites[aoi_wgs84, ] # Spatial Subset
    
    if (nrow(site_aoi) == 0) return(message("No sites found."))
    
    # 3. Determine Year Range
    # Use years[1] to check the first element of the input
    if (years[1] == "all") {
      start_yr <- min(lubridate::year(site_aoi$beginDate), na.rm = TRUE)
      end_yr   <- lubridate::year(Sys.Date())
    } else {
      # If it's a vector like 2010:2012, we just take the min and max
      start_yr <- min(as.numeric(years))
      end_yr   <- max(as.numeric(years))
    }
    
    # 4. BATCH REQUEST 
    message(paste("Fetching data for", nrow(site_aoi), "sites in one go..."))
    
    # fetchSCAN returns a list of dataframes
    all_dat_list <- soilDB::fetchSCAN(
      site.code = site_aoi$Site, 
      year = start_yr:end_yr, 
      timeseries = timeseries
    )
    
    metadata <- all_dat_list[names(all_dat_list) == "metadata"]$metadata
    
    # 5. Clean up efficiently
    final_data <- all_dat_list[names(all_dat_list) != "metadata"] %>%
      purrr::map(~ mutate(.x, across(everything(), as.character))) %>%
      bind_rows() %>%
      left_join(metadata, by = "Site")
    
    return(final_data)
}
