# Define some objective functions for runoff routing 

#Several summary statistics are used to calibrate and quantify the routing model 
#performance compared to observed discharge at NWIS gages. Statistics include:
# 1.  Nash-Sutcliffe Efficiency (NSE), 
# 2.  Ratio of the Root Mean Square Error to the Standard Deviation (RSR),
# 3.  Percent Bias (PBIAS) (Moriasi, et al., 2007), 
# 4.  The Coefficient of Determination (R2)(Legates and McCabe, 1999). 
# 5.  Sum of squares (SSE)
# 6.  Root-mean squared error (RMSE)
# 7.  Mean absolute error (MAE)

#These statistics are summarized at:
#  https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/calibration/calibration-summary-statistics


#' Nash-Sutcliffe Efficiency (NSE)
#' @param obs Vector of observed values
#' @param sim Vector of simulated/modeled values
#' @return A single numeric value representing the NSE
#' 
#' NSE Interpretation
#' 1.0Perfect match between model and observations.
#' > 0.75Excellent fit; highly reliable for decision making.
#' 0.5 to 0.75Satisfactory to Good fit.
#' 0.0 to 0.5Marginal fit; the model captures some trends but has high error.
#' < 0.0Poor fit; you would be better off just using the average flow.
#' 
NSE <- function(obs, sim) {
  
  # Remove any pairs where either obs or sim is NA
  # This ensures the sums don't return NA
  data_clean <- stats::na.omit(data.frame(obs, sim))
  
  obs_val <- data_clean$obs
  sim_val <- data_clean$sim
  
  # If no data is left after cleaning, return penalty
  if (length(obs_val) == 0) return(-9999)
  
  # Calculate the numerator: Sum of Squared Errors (Residuals)
  numerator <- sum((obs_val - sim_val)^2)
  
  # Calculate the denominator: Sum of Squared Deviations from the Mean
  denominator <- sum((obs_val - mean(obs_val))^2)
  
  # Calculate NSE
  # If denominator is 0 (obs are all the same value), return penalty to avoid Inf
  if (denominator == 0) return(-9999)
  
  nse_val <- 1 - (numerator / denominator)
  
  return(nse_val)
}



# RSR (RMSE-observations standard deviation ratio)
# RSR standardizes the RMSE using the standard deviation of the observations.
#' RMSE-observations standard deviation ratio (RSR)
#' @param obs Vector of observed values
#' @param sim Vector of simulated/modeled values
#' @return A single numeric value representing the RSR
#' 
#' RSR Interpretation
#' 0.00 to 0.50: Very Good fit.
#' 0.50 to 0.60: Good fit.
#' 0.60 to 0.70: Satisfactory fit.
#' > 0.70: Unsatisfactory fit.
RSR <- function(obs, sim) {
  data_clean <- stats::na.omit(data.frame(obs, sim))
  if (nrow(data_clean) == 0) return(1e10) # Return high penalty for bad fit
  
  numerator <- sqrt(sum((data_clean$obs - data_clean$sim)^2))
  denominator <- sqrt(sum((data_clean$obs - mean(data_clean$obs))^2))
  
  if (denominator == 0) return(1e10)
  
  return(numerator / denominator)
}


# PBIAS (Percent Bias)
# Measures the average tendency of the simulated data to be larger or smaller 
# than their observed counterparts.

#' Percent Bias (PBIAS)
#' @param obs Vector of observed values
#' @param sim Vector of simulated/modeled values
#' @return Percent bias (percentage)
#' 
#' PBIAS Interpretation (Streamflow)
#' < ±10%: Very Good.
#' ±10% to ±15%: Good.
#' ±15% to ±25%: Satisfactory.
#' > ±25%: Unsatisfactory.
PBIAS <- function(obs, sim) {
  data_clean <- stats::na.omit(data.frame(obs, sim))
  if (nrow(data_clean) == 0) return(1e10)
  
  denom <- sum(data_clean$obs)
  if (denom == 0) return(1e10)
  
  # Note: Removed abs() to allow negative values (indicates overestimation)
  # If your optimization requires absolute PBIAS, wrap this in abs()
  pbias_val <- (sum(data_clean$obs - data_clean$sim) * 100) / denom
  return(pbias_val)
}




# COD (Coefficient of Determination / $R^2$)
# Describes the proportion of the variance in measured data explained by the 
# model.

#' Coefficient of Determination (R^2)
#' @param obs Vector of observed values
#' @param sim Vector of simulated/modeled values
#' @return Value between 0 and 1.
COD <- function(obs, sim) {
  data_clean <- stats::na.omit(data.frame(obs, sim))
  if (nrow(data_clean) < 2) return(0) # Need at least 2 points for correlation
  
  obs_val <- data_clean$obs
  sim_val <- data_clean$sim
  
  numerator <- sum((obs_val - mean(obs_val)) * (sim_val - mean(sim_val)))
  denominator <- sqrt(sum((obs_val - mean(obs_val))^2)) * sqrt(sum((sim_val - mean(sim_val))^2))
  
  if (denominator == 0) return(0)
  
  r_sq <- (numerator / denominator)^2
  return(r_sq)
}

# SSE (Sum of Squared Errors)
# A simple measure of the total deviation between the model and observations.

#' Sum of Squared Errors (SSE)
#' @param obs Vector of observed values
#' @param sim Vector of simulated/modeled values
#' @return Sum of squares. Lower is better.
SSE <- function(obs, sim) {
  data_clean <- stats::na.omit(data.frame(obs, sim))
  if (nrow(data_clean) == 0) return(1e10)
  
  return(sum((data_clean$sim - data_clean$obs)^2))
}



# RMSE (Root Mean Square Error)
# Commonly used to express the magnitude of error in the same units as the data.

#' Root Mean Square Error (RMSE)
#' @param obs Vector of observed values
#' @param sim Vector of simulated/modeled values
#' @return Error in data units. Lower is better.
RMSE <- function(obs, sim) {
  data_clean <- stats::na.omit(data.frame(obs, sim))
  if (nrow(data_clean) == 0) return(1e10)
  
  return(sqrt(mean((data_clean$sim - data_clean$obs)^2)))
}


# MAE (Mean Absolute Error)
# Similar to RMSE but less sensitive to extreme outliers/outlier peaks.

#' Mean Absolute Error (MAE)
#' @param obs Vector of observed values
#' @param sim Vector of simulated/modeled values
#' @return Mean error in data units. Lower is better.
MAE <- function(obs, sim) {
  data_clean <- stats::na.omit(data.frame(obs, sim))
  if (nrow(data_clean) == 0) return(1e10)
  
  return(mean(abs(data_clean$sim - data_clean$obs)))
}



#' Kling-Gupta Efficiency (KGE)
#' @param obs Vector of observed values
#' @param sim Vector of simulated/modeled values
#' @return A single numeric value representing the KGE (1.0 is perfect)
#' 
#' KGE Interpretation
#' 1.0: Perfect match.
#' > 0.75: Very good performance.
#' 0.5 to 0.75: Good performance.
#' < 0.0: Poor performance (the mean of observations is a better predictor).
#' 
#' Note: KGE is excellent for peak matching as it balances correlation, 
#' bias, and variability (stretching/compressing the peaks).
KGE <- function(obs, sim) {
  
  # Remove any pairs where either obs or sim is NA
  data_clean <- stats::na.omit(data.frame(obs, sim))
  
  if (nrow(data_clean) < 2) return(-9999)
  
  obs_val <- data_clean$obs
  sim_val <- data_clean$sim
  
  # 1. Correlation (r)
  r <- stats::cor(obs_val, sim_val)
  
  # 2. Bias ratio (beta = mean_sim / mean_obs)
  beta <- mean(sim_val) / mean(obs_val)
  
  # 3. Variability ratio (gamma = CV_sim / CV_obs)
  # CV = sd / mean
  cv_sim <- stats::sd(sim_val) / mean(sim_val)
  cv_obs <- stats::sd(obs_val) / mean(obs_val)
  gamma  <- cv_sim / cv_obs
  
  # Calculate KGE components
  # We calculate the Euclidean distance from the ideal point (1,1,1)
  ed <- sqrt((r - 1)^2 + (beta - 1)^2 + (gamma - 1)^2)
  
  kge_val <- 1 - ed
  
  # Handle non-finite results (e.g., if sd is 0)
  if (!is.finite(kge_val)) return(-9999)
  
  return(kge_val)
}
