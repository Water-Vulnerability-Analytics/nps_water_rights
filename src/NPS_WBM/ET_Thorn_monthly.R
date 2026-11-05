#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Thornthwaite Monthly PET
#'
#' Calculates PET from monthly Tmean and daylength, according to the Thornthwaite method.
#' @param x A monthly time series data frame containing Date, tmean_C (deg C), and daylength (hours)
#' @export
#' ET_Thorn_monthly()

ET_Thorn_monthly = function(x){
  x$month = strftime(x$Date, "%m")
  N = lubridate::days_in_month(as.numeric(x$month))
  e.s = get_svp(x$tmean_C)
  et.thorn = ifelse(x$tmean_C > 0, 29.8*N*x$daylength*(e.s/(x$tmean_C+273.2)), 0)
  return(et.thorn)
}
