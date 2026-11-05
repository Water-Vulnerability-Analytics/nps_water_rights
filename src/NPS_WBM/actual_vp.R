#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Actual Vapor Pressure
#'
#' Calculates actual vapor pressure (kPa) of air based on maximum and minimum relative humidity and maximum and minimum temperature.
#' @param rhmax Daily maximum relative humidity (percent).
#' @param rhmin Daily minimum relative humidity (percent).
#' @param tmax Daily maximum temperature (deg C).
#' @param tmin Daily minimum temperature (deg C).
#' @export
#' actual_vp()

actual_vp = function(rhmax, rhmin, tmax, tmin) {
  e.tmax = get_svp(tmax)
  e.tmin = get_svp(tmin)
  e.a = (e.tmin*(rhmax/100) + e.tmax*(rhmin/100))/2
  return(e.a)
}
