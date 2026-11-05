#' ## This version of the NPS water balance model functions are based on:
#' ## Dave Thoma's water balance Excel spreadsheet model and were originally
#' ## created 10/30/2019 by ARC with subsequent updates 4/14/21 by MGS 
#' 
#' Melt
#'
#' Calculates the amount of snowmelt at time steps from snowpack, temperature, and Hock melt factor.
#' @param tmean A vector of daily mean temperatures (deg C).
#' @param jtemp the Jennings temperature extracted from the raster based on latitude and longitude.
#' @param hock A melt factor of daily snowmelt when warm enough to melt.
#' @param snow A time series vector of snowfall values.
#' @param sp.0 (optional) Initial snowpack value. Default is 0.
#' @export
#' get_melt()

get_melt = function(tmean, jtemp, hock, snow, sp.0=NULL){
  sp.0 <- ifelse(!is.null(sp.0), sp.0, 0)
  melt <- vector()
  melt[1] <- ifelse(tmean[1] < (jtemp[1]-3)||sp.0==0, 0,
                   ifelse((tmean[1]-(jtemp[1]-3))*hock>sp.0, 
                          sp.0, (tmean[1]-(jtemp[1]-3))*hock))
  snowpack <- vector()
  snowpack[1] <- sp.0 + snow[1] - melt[1]
  for(i in 2:length(tmean)){
    melt[i] <- ifelse(tmean[i]<(jtemp[i]-3) | snowpack[i-1]==0, 0, 
                     ifelse((tmean[i]-(jtemp[i]-3))*hock>snowpack[i-1], 
                            snowpack[i-1], (tmean[i]-(jtemp[i]-3))*hock))
    snowpack[i] <- snowpack[i-1]+snow[i]-melt[i]
  }
  return(melt)
}
