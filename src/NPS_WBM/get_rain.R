#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Rain
#'
#' Calculates rainfall totals based on precipitation and freeze factor.
#' @param ppt A vector of precipitation values.
#' @param freeze A vector of freeze factor values, calculated from average temperature and Jennings et al., 2018. Values are 0-1.
#' @export
#' get_rain()

get_rain = function(ppt, freeze){
  rain = ppt*freeze
  return(rain)
}
