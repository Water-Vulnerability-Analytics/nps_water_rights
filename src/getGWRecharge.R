#' Import groundwater recharge raster
#' 
#'
#' 
#' @param aoi An sf polygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The buffer distance (in meters) around park boundaries to filter data to
#' 
#' @return A GW recharge raster masked to each specified park boundary
getGWRecharge <- function(aoi, dist=0){
  
  if(!dir.exists(paste0(getwd(),"/templeod_dataall/"))){dir.create(file.path(getwd(),"/templeod_dataall/"), showWarnings = FALSE)}
  
  call <- "https://water.usgs.gov/GIS/dsdl/rech48grd.zip"
  
  #download boundary 
  temp1 <- tempfile()
  download.file(paste0(call), destfile = temp1, method = "curl")
  unzip(temp1, exdir = "templeod_dataall/")
  
  aoi <- aoi %>% 
    sf::st_buffer(., dist = dist)
  
  gw_recharge <- terra::rast("templeod_dataall/rech48grd") %>%
    terra::project("epsg:4269") %>%
    terra::crop(aoi, mask = TRUE) 
  
  unlink(file.path(getwd(),"/templeod_dataall/"), recursive = T)
  
  return(gw_recharge)
}
