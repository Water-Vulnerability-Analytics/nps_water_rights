#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Runoff or excess input greater than soil water holding capacity
#' 
#' Calculates runoff at daily timesteps based on water reaching soil surface, AET, change in soil moisture, and a runoff coefficient
#' @param ppt A vector of precipitation values.
#' @param w A time series vector of available water for soil charging (rain + snowmelt).
#' @param d_soil A time series vector of change in soil moisture from previous day.
#' @param AET A time series vector of actual evapotranspiration.
#' @param R.coeff A fraction of precpitation that can be shunted to direct runoff.
#' @export
#' get_runoff()

get_runoff=function(ppt, w, d_soil, AET, R.coeff=NULL){
  R.coeff = ifelse(!is.null(R.coeff), R.coeff, 0)
  DRO = ppt*(R.coeff/100)
  runoff = w-d_soil-AET+DRO
  return(runoff)
}
