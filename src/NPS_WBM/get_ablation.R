#' Snow Ablation
#'
#' Calculates the amount of snow lost to ablation (vapor/sublimation) based on 
#' temperature and an ablation factor. This water is removed from the snowpack 
#' but does not contribute to liquid runoff.
#' @param tmean A vector of daily mean temperatures (deg C).
#' @param jtemp The Jennings temperature climatology (deg C).
#' @param ab_fac An ablation factor (DDF) for mass loss to the atmosphere.
#' @param snow A time series vector of snowfall values.
#' @param sp.0 (optional) Initial snowpack value. Default is 0.
#' @export
get_ablation <- function(tmean, jtemp, ab_fac, snow, sp.0 = NULL) {
  
  sp.0 <- ifelse(!is.null(sp.0), sp.0, 0)
  
  # Initialize vectors
  ablation <- vector()
  snowpack <- vector()
  
  # --- Step 1: Initial Day ---
  # Threshold logic: tmean must be >= (jtemp - 3) and snowpack must exist
  ablation[1] <- ifelse(tmean[1] < (jtemp[1] - 3) || sp.0 == 0, 0,
                        ifelse((tmean[1] - (jtemp[1] - 3)) * ab_fac > sp.0, 
                               sp.0, 
                               (tmean[1] - (jtemp[1] - 3)) * ab_fac))
  
  snowpack[1] <- sp.0 + snow[1] - ablation[1]
  
  # --- Step 2: Loop through time series ---
  for(i in 2:length(tmean)) {
    # Check threshold and if there is snow to ablate from the previous day
    ablation[i] <- ifelse(tmean[i] < (jtemp[i] - 3) || snowpack[i-1] == 0, 0, 
                          ifelse((tmean[i] - (jtemp[i] - 3)) * ab_fac > snowpack[i-1], 
                                 snowpack[i-1], 
                                 (tmean[i] - (jtemp[i] - 3)) * ab_fac))
    
    # Update snowpack by removing today's ablation
    snowpack[i] <- snowpack[i-1] + snow[i] - ablation[i]
  }
  
  return(ablation)
}
