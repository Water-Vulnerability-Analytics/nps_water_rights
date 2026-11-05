#' Import and clean New Mexico Water Rights Point of Diversion Data
#' This function imports geospatial data from the New Mexico Office of the State Engineer (https://geospatialdata-ose.opendata.arcgis.com/datasets/OSE::ose-pods/about)
#' 
#' @param aoi An sf polygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in the same units used in  `aoi`) to buffer around spatial object `aoi`
#' 
#' @return An sf object of points of diversion within given spatial boundary and attributes
getPODNewMexico <- function(aoi, dist = 0){
  
  # add buffer around park boundary
  aoi_buffer <- aoi %>% 
    sf::st_buffer(dist = dist)
  
  if(st_crs(aoi_buffer)$epsg != 4326){
  
  ## convert park boundary bbox to esri geom object
  filter_geom <- aoi_buffer %>%
    sf::st_transform(4326) %>%
    sf::st_bbox() %>% 
    sf::st_as_sfc(filter_geom) %>% 
    .[[1]] %>% 
    arcgisutils::as_esri_geometry()
 
   } else {
    
     ## convert park boundary bbox to esri geom object
     filter_geom <- aoi_buffer %>%
       sf::st_bbox() %>% 
       sf::st_as_sfc(filter_geom) %>% 
       .[[1]] %>% 
       arcgisutils::as_esri_geometry()
    
  }
  
  ## create the rest API
  url <- paste0("https://services2.arcgis.com/qXZbWTdPDbTjl7Dy/arcgis/rest/services/OSE_PODs/FeatureServer/0/query?returnGeometry=true&where=1=1&outFields=*&f=geojson&geometry=",
                filter_geom,
                "&geometryType=esriGeometryPolygon&inSR=4326&spatialRel=esriSpatialRelIntersects")
  
  if(st_crs(aoi_buffer)$epsg != 4326){
  
  nm_wr <- st_read(url) %>%
    sf::st_transform(st_crs(aoi))
  
  } else {
    
    nm_wr <- st_read(url) 
    
  }
  
  # join sf object and table attributes and filter to park boundary region of interest
  nearby_wr <- nm_wr %>%  
    # subset to only PODs within aoi + buffer
    sf::st_intersection(., aoi_buffer) %>%
    # identify those found within aoi OR just within the buffer
    sf::st_join(., aoi %>% dplyr::mutate(within_aoi = "Within AOI") %>% dplyr::select(within_aoi), left = TRUE) %>%
    # identify PODs owned by park. This doesn't always work. 
    dplyr::mutate(park_right = ifelse(grepl("National Park", own_lname, ignore.case = T), "Park", "Non-park"),
                  within_aoi = ifelse(!is.na(within_aoi), "Within AOI", "Within buffer")) %>%
    sf::st_make_valid()
  
  return(nearby_wr)
 
}