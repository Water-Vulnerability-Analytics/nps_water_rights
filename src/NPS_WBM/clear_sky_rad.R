#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Clear Sky Radiation
#'
#' Calculates incoming clear-sky radiation (MJ m^-2 day^-1) based on day-of-year, latitude, and elevation
#' @param doy Day-of-year (Julian date)
#' @param lat Latitude (degrees)
#' @param elev Elevation (m)
#' @export
#' clear_sky_rad()

clear_sky_rad = function(doy, lat, elev){
  d.r = 1 + 0.033*cos(((2*pi)/365)*doy)
  declin = 0.409*sin((((2*pi)/365)*doy)-1.39)
  lat.rad = (pi/180)*lat
  sunset.ang = acos(-tan(lat.rad)*tan(declin))
  R.a = ((24*60)/pi)*0.0820*d.r*(sunset.ang*sin(lat.rad)*sin(declin) + cos(lat.rad)*cos(declin)*sin(sunset.ang))
  R.so = (0.75 + 2e-5*elev)*R.a
  return(R.so)
}
