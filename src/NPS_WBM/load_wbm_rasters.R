#' This function loads and processes the required raster datasets for the NPS Water Balance Model,
#' including digital elevation model, soil storage properties, and calculates terrain derivatives
#' such as slope and aspect. All rasters are projected to the specified coordinate reference system.
#'
#' @param crs Character string or CRS object. The coordinate reference system to project all 
#'   rasters to. This should match the CRS of the points where the WBM will be applied.

load_wbm_rasters <- function(crs) {
  
  # Load DEM (from NPS WBM group) -> in meters
  dem <<- terra::rast(here::here('data/all/elevation_cropped.tif'))
  
  if(is.na(sf::st_crs(dem)) || crs != sf::st_crs(dem)){
    
    dem <<- dem %>%
      terra::project(crs)
    
  }
  
  # Load storage properties raster (from NPS WBM group)
  soil <<- terra::rast(here::here('data/all/water_storage.tif')) * 10 #cm to mm
    
  if(is.na(sf::st_crs(soil)) || crs != sf::st_crs(soil)){
    
    soil <<- soil %>%
      terra::project(crs)
    
  }
  
  # Calculate slope & aspect
  # For slope, 4 better for "smooth" surfaces, 8 better for rough See:
  # https://www.rdocumentation.org/packages/raster/versions/3.1-5/topics/terrain
  slope <<- terra::terrain(dem, v = "slope", 
                           unit = "degrees", 
                           neighbors = 8) 
  
  aspect <<- terra::terrain(dem, 
                            v = "aspect", 
                            unit = "degrees")
  
  jtemp <<- terra::rast("data/all/merged_jennings2.tif")
  st_crs(jtemp, 4326)
  
  
  if(is.na(sf::st_crs(jtemp)) || crs != sf::st_crs(jtemp)){
    
    jtemp <<- jtemp %>%
      terra::project(crs)
    
  }
  
  
}
