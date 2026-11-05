#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Climatic water deficit
#' 
#' Calculates daily climatic water deficit, which is PET - AET.
#' @param pet A time series vector of PET.
#' @param AET A time series vector of actual evapotranspiration.
#' @export
#' get_deficit()

get_deficit=function(pet, AET){
  deficit = pet-AET
  return(deficit)
}
