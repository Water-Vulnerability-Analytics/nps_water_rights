
getAOIWatersheds <- function(aoi){
  
  geom_col <- attr(aoi, "sf_column")
  
  if (!"geometry" %in% names(aoi)) {
    
    names(aoi)[names(aoi) == geom_col] <- "geometry"
    
    attr(aoi, "sf_column") <- "geometry"
    
    aoi <- aoi %>% st_as_sf()
    
  }
  
  # download flowlines for entire aoi
  nhd_flowlines <- nhdplusTools::get_nhdplus(AOI = aoi, 
                                             realization='flowline',
                                             t_srs = sf::st_crs(aoi))
  
  aoi_flowlines <- nhd_flowlines  %>%
    dplyr::distinct(comid, .keep_all = TRUE) %>%
    nhdplusTools::get_tocomid(., add = TRUE)
  
  # minimize number of origin points by selecting only those that cross AOI boundary
  outsiders <- aoi_flowlines %>%
    dplyr::filter(tocomid==0)
  
  # get the outer boundary of the AOI (handles polygons/multipolygons)
  aoi_boundary <- aoi %>%
    st_make_valid() %>%
    st_union() %>%          
    st_boundary()
  
  suppressWarnings({
  # intersect lines with the AOI boundary, convert to points
  ints  <- st_intersection(aoi_boundary, outsiders) %>%
    st_collection_extract("POINT") %>%      
    st_cast("POINT", warn = FALSE) %>%      
    st_as_sf() 
  })
  
  ws <- vector("list")
  
  for(i in 1:nrow(ints)){
    suppressMessages({suppressWarnings({  # sometimes this doesnt work...
      # try(trace <- get_raindrop_trace(ints[i,], direction = "down"), silent = TRUE)
      
      # so use ints as default for raindrop location in those instances.
      raindrop <- ints[i,] %>% st_as_sfc()
      # try(raindrop <- sf::st_sfc(sf::st_point(trace$intersection_point[[1]][1:2]),
      #                            crs = 4326), silent = TRUE)
      
      try(ws[[i]] <- get_split_catchment(point = raindrop, upstream = TRUE) %>%
            filter(is.na(catchmentID)), silent = TRUE)
    })})
    
    
  }
  
  ws <- ws %>%
    bind_rows() 
  
  if(crs(ws) != crs(aoi)){
    
    ws <- ws %>%
      st_transform(., crs = crs(aoi))
  }
  
  ws <- ws %>%
    bind_rows(aoi) %>%
    st_make_valid() %>%
    summarize() %>%                             
    nngeo::st_remove_holes()                
  
  return(ws) 
}
