#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Water reaching soil surface minus PET
#'
#' Calculates water reaching soil surface minues the PET.
#' @param w A time series vector of water reaching soil surface as snow plus rain.
#' @param pet A time series vector of PET values.
#' @export
#' get_w_pet()

get_w_pet = function(w, pet){
  w_pet = (w-pet)
  return(w_pet)
}
