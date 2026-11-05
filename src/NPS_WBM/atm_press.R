#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Atmospheric Pressure
#'
#' Estimates atmospheric pressure (kPa) at a given elevation.
#' @param elev Elevation (m).
#' @export
#' atm_press()

atm_press = function(elev){
  atm.press = 101.3*((293 - 0.0065*elev)/293)^5.26
  return(atm.press)
}
