#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Snowpack
#'
#' Calculates snowpack accumulation at time steps, from a time series of snowfall and melt.
#' @param jtemp the Jennings temperature extracted from the raster based on latitude and longitude.
#' @param snow A time series vector of snowfall values.
#' @param melt A time series vector of snowmelt.
#' @param sp.0 (optional) Initial snowpack value. Default is 0.
#' @export
#' get_snowpack()

get_snowpack = function(jtemp, snow, melt, sp.0=NULL){
  low_thresh_temp = jtemp - 3
  sp.i = ifelse(!is.null(sp.0), sp.0, 0)
  snowpack <- vector()
  for(i in 1:length(melt)){
    snowpack[i] = sp.i+snow[i]-melt[i]
    sp.i=snowpack[i]
  }
  return(snowpack)
}
