nldi_finder <- function(site_no) {
  #get those gages' watersheds using get_nldi_basin in the {nhdplusTools}.
  nldi_nwis <- list(featureSource = "nwissite",
                    featureID = paste0("USGS-", site_no))
  
  gage_basin <- 
  tryCatch({
    invisible(
      suppressMessages(
          nhdplusTools::get_nldi_basin(nldi_feature = nldi_nwis) %>%
          sf::st_transform(., 4269) %>%
          dplyr::mutate(site_no = site_no)))
    }, error = function(e) {
      NA
    })
  
  return(gage_basin)
}
