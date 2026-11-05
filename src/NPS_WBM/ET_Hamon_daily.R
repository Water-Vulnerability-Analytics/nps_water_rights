#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Hamon Daily PET
#'
#' Calculates Hamon PET from a daily time series of Tmean and daylength.
#' @param x A daily time series data frame containing tmean_C (deg C), and daylength (hours)
#' @export
#' ET_Hamon_daily()

ET_Hamon_daily = function(x){
  et.hamon = 0.1651*(x$daylength/12)*(216.7*(6.108*exp((17.26*x$tmean_C)/(x$tmean_C+273.3))))/(x$tmean_C+273.3)
  return(et.hamon)
}
