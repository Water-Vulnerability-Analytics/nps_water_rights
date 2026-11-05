#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Outgoing Radiation
#'
#' Calculates outgoing radiation (MJ m^-2 day^-1) based on daily Tmax, Tmin, incoming radiation, actual vapor pressure, and clear-sky radiation.
#' @param tmax Daily maximum temperatures (deg C).
#' @param tmin Daily minimum temperatures (deg C).
#' @param R.s Incoming solar radiation (MJ m^-2 day^-1).
#' @param e.a Actual vapor pressure (kPa).
#' @param R.so Clear-sky radiation (MJ m^-2 day^-1).
#' @export
#' outgoing_rad()

outgoing_rad = function(tmax, tmin, R.s, e.a, R.so){
  R.nl = 4.903e-09*(((tmax + 273.16)^4 + (tmin + 273.16))/2)*(0.34-0.14*sqrt(e.a))*(1.35*(R.s/R.so) - 0.35)
  return(R.nl)
}
