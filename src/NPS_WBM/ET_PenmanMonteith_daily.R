#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Penman-Monteith Daily PET
#'
#' Calculates PET (mm) from daily Tmax, Tmin, solar radiation, elevation, and latitude, according to the Penman-Monteith method. May also use daily maximum and minimum relative humidity, atmospheric vapor pressure, and wind speeds.
#' @param x A daily time series data frame containing Date (date object), tmax_C (deg C), tmin_C (deg C), srad (MJ m^-2 day^-1). Optionally contains RHmax (percent), RHmin (percent), vp (kPa), and wind (m/s).
#' @param elev Elevation of the site (m).
#' @param lat Latitude of the site (degrees).
#' @param wind (optional) An estimated value for daily average wind speeds (m/s). Use if input data frame does not contain daily wind speed values.
#' Default at 2.2 m/s (5mph)
#' @export
#' ET_PenmanMonteith_daily()

ET_PenmanMonteith_daily = function(x, elev, lat, wind=2.2){
  #Inputs
  tmax = x$tmax_C
  tmin = x$tmin_C
  tmean = (tmax + tmin)/2
  doy = x$doy
  rh.max = x$RHmax
  rh.min = x$RHmin
  vp = x$vp
  #R.s = x$srad
  R.s = clear_sky_rad(doy, lat, elev)
  u = ifelse(is.null(wind) == TRUE, x$wind, wind)
  psyc.const = psyc_constant(elev)
  vap.curve = vapor_curve(tmean)
  
  #Auxilary calculations for wind terms
  DT = vap.curve/(vap.curve + psyc.const*(1+0.34*u))
  PT = psyc.const/(vap.curve + (psyc.const*(1+0.34*u)))
  TT = (900/(tmean + 273))*u
  
  #Saturation vapor pressure
  e.tmax = get_svp(tmax)
  e.tmin = get_svp(tmin)
  e.s = (e.tmax + e.tmin)/2
  
  #Actual vapor pressure
  if(is.null(vp) == TRUE){
    if(is.null(rh.max) == TRUE){
      e.a = e.tmin
    } else {
      e.a = actual_vp(rh.max, rh.min)
    }
  } else {
    e.a = vp
  }
  
  #Solar angle and radiation calculations
  R.ns = (1 - 0.23)*R.s
  R.so = clear_sky_rad(doy, lat, elev)
  R.nl = outgoing_rad(tmax, tmin, R.s, e.a, R.so)
  R.n = R.ns - R.nl
  R.ng = 0.408*R.n
  
  #ET from radiation
  ET.rad = DT*R.ng
  #ET from wind
  ET.wind = PT*TT*(e.s - e.a)
  #Total ET
  ET.o = ET.rad + ET.wind
  return(ET.o)
}
