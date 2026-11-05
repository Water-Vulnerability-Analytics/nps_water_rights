#' Import Omernik II Ecoregion shapefile
#' 
#' This function pulls Omernik III ecoregion shapefiles within the park(s) of interest. 
#'
#' 
#' @param aoi An sf polygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param save Whether to save (TRUE) the resulting shapefile or not (FALSE)
#' @param path If `save = TRUE`, the file path to save the shapefile
#' 
#' @return A spatial sf object of Omernik III ecoregions for each specified park boundary
#' 
#' @seealso [getParkBoundary()]
getOmernikII <- function(aoi, save=FALSE, path = NULL){
  
  call <- "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/us_eco_l3_state_boundaries.zip"
  
  #download boundary 
  temp1 <- tempfile()
  download.file(paste0(call), destfile = temp1, method = "curl")
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)
  
  #sf::sf_use_s2(FALSE)
  
  ecoregs <- sf::st_read(dsn = temp2) %>%
    sf::st_transform(4269) %>%
    st_make_valid() %>% 
    .[aoi,]
  
  if(save == TRUE){
    sf::st_write(ecoregs, paste0(path, "/omernik_region.shp"), append = FALSE)
    
  }
  
  return(ecoregs)
}
