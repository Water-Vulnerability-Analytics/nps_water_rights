#' Import NPS buildings
#' 
#' https://public-nps.opendata.arcgis.com/datasets/nps::nps-buildings-web-mercator/explore?location=21.734632%2C-117.659450%2C4.08

getBuildings <- function(park_boundary, dist = 0){

  #install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
  call <- "https://opendata.arcgis.com/api/v3/datasets/2bf22e15d269431e9e3380382c0a1546_2/downloads/data?format=shp&spatialRefId=3857&where=1%3D1"
 
  temp1 <- tempfile()
  download.file(paste0(call), destfile = temp1, method = "curl")
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)

  buffer <- park_boundary %>% 
    sf::st_buffer(dist = dist)
  
  buildings <- sf::st_read(temp2, layer = "NPS_-_Buildings_-_Web_Mercator") %>%
    st_make_valid() %>% 
    sf::st_zm() 
  
  if(st_crs(buildings) != st_crs(buffer)){
    buildings <- buildings %>%
      sf::st_transform(st_crs(park_boundary))
  }
  
  buildings <- buildings %>%
    sf::st_join(., dplyr::select(buffer, UNIT_CODE)) %>%
    dplyr::filter(!is.na(UNIT_CODE))

  return(buildings)
  
}

