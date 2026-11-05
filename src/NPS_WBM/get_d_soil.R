#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Daily change in Soil Water Content (SWC)
#'
#' Calculates daily change in soil water content.
#' @param swc A time series vector of soil water content.
#' @param swc.0 (optional) The initial soil water content value. Default is 0.
#' @export
#' get_d_soil()

get_d_soil=function(swc, swc.0=NULL){
  swc.0 = ifelse(!is.null(swc.0), swc.0, 0)
  d_soil = swc - lag(swc, default=swc.0)
  return(d_soil)
}
