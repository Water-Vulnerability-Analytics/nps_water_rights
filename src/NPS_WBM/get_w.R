#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Water reaching soil surface as snow plus rain
#'
#' Calculates water reaching soil surface using rain and snowmelt. 
#' @param rain A vector of daily rain.
#' @param melt A vector of daily snowmelt.
#' @export
#' get_w()

get_w = function(rain, melt){
  w = (melt+rain)
  return(w)
}
