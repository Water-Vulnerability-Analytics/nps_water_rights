#' Import and clean Utah Water Rights Point of Diversion Data
#' This function imports geospatial data from the Utah Geospatial Resource Center (https://opendata.gis.utah.gov/datasets/utahDNR::utah-points-of-diversion/explore?location=40.989324%2C-114.748636%2C-1.00)
#' 
#' col descriptions for POD_supply @ 
#' https://www.arcgis.com/sharing/rest/content/items/5d530e62e6ca42528dd13e0a453a3b73/info/metadata/metadata.xml?format=default&output=html
#' Uses: 
#'      D = Domestic 
#'      I = Irrigation 
#'      M = Municipal 
#'      O = Other 
#'      P = Power 
#'      S = Stockwatering 
#'      X = Mining
#' 
#' @param aoi An sf polygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in the same units used in  `aoi`) to buffer around spatial object `aoi`
#' 
#' @return An sf object of points of diversion within given spatial boundary and attributes
getPODUtah <- function(aoi, dist = 0){
  
  # add buffer around park boundary
  aoi_buffer <- aoi %>% 
    sf::st_buffer(dist = dist)
  
  ## convert park boundary bbox to esri geom object
  filter_geom <- aoi_buffer %>%
    sf::st_transform(4326) %>%
    sf::st_bbox() %>% 
    sf::st_as_sfc(filter_geom) %>% 
    .[[1]] %>% 
    arcgisutils::as_esri_geometry()
 
  ## create the rest API
  # see: https://hub.arcgis.com/datasets/utahDNR::utah-points-of-diversion/about
  url <- paste0("https://services.arcgis.com/ZzrwjTRez6FJiOq4/arcgis/rest/services/Utah_Points_of_Diversion/FeatureServer/0",
                "/query?returnGeometry=true&where=1=1&outFields=*&f=geojson&geometry=",
                filter_geom,
                "&geometryType=esriGeometryPolygon&inSR=4326&spatialRel=esriSpatialRelIntersects")
  
  ut_wr <- st_read(url) %>%
    sf::st_transform(st_crs(aoi))
  
  #join sf object and table attributes and filter to park boundary region of interest
  nearby_wr <- ut_wr %>%    
    sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
    dplyr::mutate(park_right = ifelse(grepl("National Park", OWNER, ignore.case = T), "Park","Non-park"),
                  within_aoi = ifelse(!is.na(within), "Within AOI", NA)) %>%
    dplyr::select(-within)
  
  return(nearby_wr)
  
}
