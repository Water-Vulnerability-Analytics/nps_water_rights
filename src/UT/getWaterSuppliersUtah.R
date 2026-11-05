#' Import and clean Utah DNR's Culinary Water Service Area Boundaries, and water use data.
#' This function imports geospatial data from the Utah Geospatial Resource Center 
#' #' 
#' @param aoi An sf polygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in meters) to buffer around spatial object `park`
#' 
#' @return An sf object of water service areas within given spatial boundary and its attributes
getWaterSuppliersUtah <- function(aoi, dist = 0){
  
  call <- "https://opendata.arcgis.com/api/v3/datasets/dc62a286013f447e88fc45480077c944_0/downloads/data?format=shp&spatialRefId=26912&where=1%3D1"
  #download boundary 
  temp1 <- tempfile()
  download.file(paste0(call), destfile = temp1, method = "curl")
  
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)
  
  # add buffer around park boundary
  aoi_buffer <- aoi %>% 
    sf::st_buffer(dist = dist)
  
  ut_wu <- sf::st_read(dsn = temp2) %>%
    st_make_valid() %>% 
    sf::st_transform(st_crs(aoi))
  
  #join sf object and table attributes and filter to park boundary region of interest
  nearby_wu <- ut_wu %>%    
    sf::st_intersection(dplyr::select(aoi_buffer, UNIT_CODE)) %>%
    sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
    dplyr::mutate(park_right = ifelse(grepl("National Park", WRNAME, ignore.case = T), "Park","Non-park"),
                  within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
    dplyr::select(-within)
  
  return(nearby_wu)
  
}


