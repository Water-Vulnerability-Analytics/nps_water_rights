#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' #' Slope of Saturation Vapor Curve
#'
#' Calculates the slope of the saturation vapor curve for a given temperature.
#' @param temp A time series vector or single value of temperatures (deg C).
#' @export
#' vapor_curve()

vapor_curve = function(temp){
  vap.curve = 4098*(0.6108*exp((17.27*temp/(temp+237.3)))/(temp+237.3)^2)
  return(vap.curve)
}
