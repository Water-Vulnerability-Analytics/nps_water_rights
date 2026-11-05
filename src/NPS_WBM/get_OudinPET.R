#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Oudin Daily PET
#'
#' Calculates PET (mm) based on temperature, latitude, and solar radiation 
#' @param doy Day-of-year (Julian date)
#' @param lat Latitude of the site (degrees).
#' @param snowpack A time series vector of snowpack accumulation values.
#' @param tmean A vector of daily mean temperatures (deg C).
#' @param slope Slope of the site (in degrees).
#' @param aspect Aspect of the site (in degrees).
#' @param shade.coeff (optional) A shade coefficient from 0-1. Default is 1.
#' @export
#' get_OudinPET()

get_OudinPET = function(doy, lat, snowpack, tmean, slope, aspect, shade.coeff=NULL){
  d.r = 1 + 0.033*cos((2*pi/365)*doy)
  declin = 0.409*sin((((2*pi)/365)*doy)-1.39)
  lat.rad = (pi/180)*lat
  sunset.ang = acos(-tan(lat.rad)*tan(declin))
  R.a = ((24*60)/pi)*0.082*d.r*((sunset.ang*sin(lat.rad)*sin(declin)) + (cos(lat.rad)*cos(declin)*sin(sunset.ang)))
  Oudin = ifelse(snowpack>2,0,ifelse(tmean>-5,(R.a*(tmean+5)*0.408)/100,0))
  Folded_aspect = abs(180-abs((aspect)-225))
  Heatload = (0.339+0.808*cos(lat*(pi/180))*cos(slope*(pi/180)))-(0.196*sin(lat.rad)*sin(slope*(pi/180)))-(0.482*cos(Folded_aspect*(pi/180))*sin(slope*(pi/180)))
  sc = ifelse(!is.null(shade.coeff), shade.coeff, 1)
  OudinPET = Oudin * Heatload * sc
  return(OudinPET)
}
