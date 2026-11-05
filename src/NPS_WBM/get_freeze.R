#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Freeze factor using Jennings et al., 2018 thresholds to partition rain and snow
#'
#' Calculates a freeze factor from 0-1 based on a temperature threshold from Jennings et al., 2018 and average temperature.
#' @param jtemp the Jennings temperature extracted from the raster based on latitude and longitude.
#' @param tmean A vector of daily mean temperatures (deg C).
#' @export
#' get_freeze()

get_freeze = function(jtemp, tmean){
  freeze=ifelse(tmean<= (jtemp-3),0,
                ifelse(tmean>=(jtemp+3),1,
                       (1/((jtemp+3) - (jtemp-3)))*(tmean-(jtemp-3))))
}
