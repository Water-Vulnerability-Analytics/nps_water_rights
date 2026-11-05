#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Daylength
#'
#' Returns daylength in hours for a series of dates, based on latitude. Calls 
#' the 'geosphere' package.
#' @param dates A series of dates containing year, month, and day
#' @param lat Latitude (degrees)
#' @export
#' get_daylength()

get_daylength = function(dates, lat) {
  yday <- as.numeric(strftime(dates, "%j"))
  dayl_h <- geosphere::daylength(lat, yday)
  return(dayl_h)
}
