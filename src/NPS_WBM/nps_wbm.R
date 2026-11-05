#' Run the NPS Water Balance Model (WBM)
#'
#' @description
#' Main driver for the NPS Water Balance Model. Given a daily meteorological
#' time series (`DailyWB`) and site-specific parameters (`point_params`), the
#' function computes daily snow/rain partitioning, snowpack dynamics, potential
#' and actual evapotranspiration, soil water storage, and runoff.
#'
#' @details
#' The model:
#' 1) derives day-of-year and daylength;  
#' 2) partitions precipitation into rain/snow using a temperature-based freeze
#'    factor;  
#' 3) computes melt with a degree-day (Hock) approach;  
#' 4) updates snowpack;  
#' 5) routes a user-defined fraction of rain directly to runoff (`Direct_Frac`);  
#' 6) computes PET using one of `"Oudin"`, `"Hamon"`, or `"Penman-Monteith"` and
#'    applies a multiplicative bias term (`PET_mult`);  
#' 7) updates soil moisture with capacity scaled by `Soil_mult * SWC.Max`;  
#' 8) computes AET, soil change (`DSOIL`), and a simple storage reservoir that
#'    releases water at `Return_Rate`.  
#'
#' **Units:** Inputs expect precipitation in mm and temperature in °C. The output
#' long table converts all fluxes to inches (÷25.4) except temperature columns.
#'
#' @param DailyWB A data frame or tibble with at least the columns:
#'   - `x` (numeric) longitude in decimal degrees,  
#'   - `y` (numeric) latitude in decimal degrees,  
#'   - `date` (Date) daily sequence without gaps,  
#'   - `ppt_mm` (numeric) daily precipitation [mm],  
#'   - `tmean_C` (numeric) daily mean air temperature [°C].  
#'   Optional columns (e.g., `GCM`) are preserved and passed through to output.
#'
#' @param point_params A single-row data frame/tibble of site parameters aligned
#'   to the `DailyWB` time series with (at minimum) columns:  
#'   - `Elev` (numeric) elevation [m],  
#'   - `Aspect` (numeric) aspect [degrees 0–360],  
#'   - `Slope` (numeric) slope [degrees],  
#'   - `SWC.Max` (numeric) soil water storage capacity [mm],  
#'   - `J.Temp` (numeric) Jennings temperature climatology [°C],  
#'   - optionally `y` (latitude) if needed by PET/astronomical helpers.
#'
#' @param Direct_Frac Numeric in [0,1]. Fraction of rainfall routed directly to
#'   runoff (bypassing soil). Default `0`.
#' @param Return_Rate Numeric in (0,1]. Daily fraction of the storage reservoir
#'   released to runoff. `1` means full same-day release. Default `1`.
#' @param PET_mult Numeric multiplier applied to PET to correct known bias.
#'   Default `1`.
#' @param Soil_mult Numeric multiplier applied to soil storage capacity
#'   (`SWC.Max`). Default `1`.
#' @param PET_Method Character; one of `c("Oudin","Hamon","Penman-Monteith")`.
#'   Controls PET formulation. Default matches argument vector behavior; see
#'   examples for explicit selection.
#' @param hock_coef Numeric degree-day factor for melt (Hock). Default `4`.
#'   (For context, Hock 2003 reports ~2.5 for Gooseberry Creek, Utah.)
#' @param Snowpack.Init Numeric initial snowpack [mm water equivalent]. Default `0`.
#' @param Soil.Init Numeric initial soil water content [mm]. Default `0`.
#' @param Shade.Coeff Numeric shading coefficient used by Oudin PET variant.
#'   Default `1`.
#' @param T.Base Numeric base temperature for growing degree days [°C]. Default `0`.
#'
#' @references
#' Hock, R. (2003). Temperature index melt modelling in mountain areas.
#' *Journal of Hydrology*, 282(1–4), 104–115. https://doi.org/10.1016/S0022-1694(03)00257-9
#'
#' 
#' When set to default values (below), the model should run identical to the
#' unmodified version: 
#' Direct_Frac = 0,
#' Return_Rate = 1, 
#' PET_mult = 1, and 
#' Soil_mult = 1
#' 
#' # Hock, 2003 (https://doi.org/10.1016/S0022-1694(03)00257-9)
#    Melt factor for Goosebury Creek in Utah is 2.5
nps_wbm <- function(DailyWB, 
                    point_params, 
                    Direct_Frac = 0, # CSU ADDITION
                    Return_Rate = 1, # CSU ADDITION
                    PET_mult = 1,  # CSU ADDITION
                    Soil_mult = 1, # CSU ADDITION
                    PET_Method = c("Oudin", "Hamon", "Penman-Monteith"), 
                    hock_coef = 4, 
                    Snowpack.Init = 0,   # according to Mike's code
                    Soil.Init = 0, # Soil.Init is full (1) from Mike's code, but empty (0) in Amber's
                    Shade.Coeff = 1,
                    T.Base = 0) {
  
  DailyWB$doy <- yday(DailyWB$date)
  DailyWB$daylength <- get_daylength(DailyWB$date, DailyWB$y)
  DailyWB$jtemp <- as.numeric(point_params$J.Temp) #as.numeric(get_jtemp(DailyWB$y, DailyWB$x)) 
  DailyWB$F <- get_freeze(DailyWB$jtemp, DailyWB$tmean_C)
  DailyWB$RAIN <- get_rain(DailyWB$ppt_mm, DailyWB$F)
  DailyWB$SNOW <- get_snow(DailyWB$ppt_mm, DailyWB$F)
  DailyWB$MELT <- get_melt(DailyWB$tmean_C, DailyWB$jtemp, hock=hock_coef, DailyWB$SNOW, Snowpack.Init)
  DailyWB$PACK <-  get_snowpack(DailyWB$jtemp, DailyWB$SNOW, DailyWB$MELT)
  DailyWB$DIRECT <- DailyWB$RAIN * Direct_Frac # CSU ADD
  #DailyWB$W = DailyWB$MELT + DailyWB$RAIN
  DailyWB$W = DailyWB$MELT + DailyWB$RAIN - DailyWB$DIRECT # CSU MOD
  if(PET_Method == "Hamon"){
    DailyWB$PET <- ET_Hamon_daily(DailyWB)
  } else {
    if(PET_Method == "Penman-Monteith"){
      DailyWB$PET <- ET_PenmanMonteith_daily(DailyWB, point_params$Elev, point_params$y, wind = 2.2)
    } else {
      if(PET_Method == "Oudin"){
        DailyWB$PET <- get_OudinPET(DailyWB$doy, DailyWB$y, DailyWB$PACK, 
                                    DailyWB$tmean_C, point_params$Slope, 
                                    point_params$Aspect, Shade.Coeff)
      } else {
        print("Error - PET method not found")
      }
    }
  }
  DailyWB$PET_mod <- DailyWB$PET * PET_mult # CSU ADD
  #DailyWB$PET = modify_PET(DailyWB$PET, Slope, Aspect, Lat, Shade.Coeff)
  DailyWB$W_PET <- DailyWB$W - DailyWB$PET
  DailyWB$SOIL <- get_soil(DailyWB$W, Soil.Init, DailyWB$PET_mod, DailyWB$W_PET, Soil_mult * point_params$SWC.Max)
  #DailyWB$DSOIL <- diff(c(Soil.Init, DailyWB$SOIL)) # What is the supposed to be doing?
  DailyWB$DSOIL <- get_d_soil(DailyWB$SOIL, swc.0 = Soil.Init)
  DailyWB$AET <- get_AET(DailyWB$W, DailyWB$PET, DailyWB$SOIL, Soil.Init)
  #DailyWB$W_ET_DSOIL = DailyWB$W - DailyWB$AET - DailyWB$DSOIL
  DailyWB$STORAGE_ADD <- DailyWB$W - DailyWB$AET - DailyWB$DSOIL # CSU ADD
  DailyWB$STORAGE_RELEASE <- get_storage(DailyWB$STORAGE_ADD, Return_Rate)["storage_release"] # CSU ADD
  DailyWB$W_ET_DSOIL =  DailyWB$STORAGE_RELEASE + DailyWB$DIRECT
  DailyWB$STORAGE_REMAIN = get_storage(DailyWB$STORAGE_ADD, Return_Rate)["storage_remain"] # CSU ADD
  DailyWB$D = DailyWB$PET - DailyWB$AET
  DailyWB$GDD = get_GDD(DailyWB$tmean_C, T.Base)
  
  # Prepare output dataframe
  DailyWB_out <- DailyWB %>%
    data.table() %>%
    ungroup() %>%
    dplyr::select(date, x, y, GCM, ppt_mm, "RUNOFF" = W_ET_DSOIL, RAIN, SNOW, MELT,AET,PET_mod, tmean_C,"excess_wat" = STORAGE_ADD, PACK, SOIL) %>%
    pivot_longer(-c(date,x,y, GCM), 
                 values_to = "vals", 
                 names_to = "vars") %>%
    mutate(vals = ifelse(vars == "tmean_C", vals, vals /25.4)) # mm to in for all but temp
}