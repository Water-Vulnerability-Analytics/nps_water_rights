#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Saturation Vapor Pressure
#'
#' Source: equation 11 in FAO, link:
#' http://www.climasouth.eu/sites/default/files/FAO%2056.pdf
#' Calculates mean saturation vapor pressure (kPa) of air based on 
#' temperature (deg C).
#' @param temp Temperature (deg C)
#' @export
#' get_svp()

get_svp = function(temp){
  svp = 0.6108*exp((17.27*temp)/(temp + 237.3))
  return(svp)
}
