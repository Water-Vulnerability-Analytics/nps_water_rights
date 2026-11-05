#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Psychrometric Constant
#'
#' Calculates the psychrometric constant relating partial pressure of water in air to the air temperature, based on atmospheric pressure. Calls the atm_press() function to estimate atmospheric pressure from elevation.
#' @param elev Elevation (m)
#' @export
#' psyc_constant()

psyc_constant = function(elev){
  atm.press = atm_press(elev)
  psyc.const = 0.000665*atm.press
  return(psyc.const)
}
