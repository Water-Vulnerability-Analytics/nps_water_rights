#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Relative Humidity
#'
#' Calculates relative humidity (%) from atmospheric vapor pressure and temperature
#' @param vp Vapor pressure (kPa)
#' @param temp Temperature (deg C)
#' @export
#' get_rh()

get_rh <- function(vp, temp){
  svp <- get_svp(temp)
  rh <- vp/svp
  return(rh)
}
