#' Import and clean Utah Water Rights Point of Diversion Data
#' This function imports [National Pollutant Discharge Elimination System (NPDES)](https://www.epa.gov/npdes) data for a user-specified area of interest. 
#' 
#' @param aoi An sf polygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in meters) to buffer around spatial object aoi
#' 
#' @return An sf object of points of diversion within given spatial boundary and attributes
getNPDES <- function(aoi, dist = 0){
  
  usa <- tigris::states() %>%
    sf::st_transform(4269)
  
  # states needed for download
  
  states <- unique(tolower(sf::st_join(aoi, usa)$STUSPS))

  downloader <- function(states){
    
    call <- paste0("https://ordsext.epa.gov/FLA/www3/state_files/state_combined_",states,".zip")
    temp1 <- tempfile()
    download.file(paste0(call), destfile = temp1, method = "curl")
  
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)
  
  #sf::sf_use_s2(FALSE)
  
  frs <- read_csv(list.files(path=temp2, pattern = "GIS_FILE", full.names = TRUE, recursive = TRUE)) %>%
    filter(!is.na(LATITUDE83)) %>%
    st_as_sf(coords = c("LONGITUDE83","LATITUDE83"), crs = 4269)
  
  return(frs)
  
  }
  
frs <- map(states, downloader) %>%
  bind_rows()
  
  # add buffer around park boundary
  aoi_buffer <- aoi %>% 
    sf::st_buffer(dist = dist)
  
  #join sf object and table attributes and filter to park boundary region of interest
  nearby_frs <- frs %>%    
    sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE))
  
  return(nearby_frs)
  
}


