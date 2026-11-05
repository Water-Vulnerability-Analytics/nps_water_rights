
###############################################################################
#' test_model_fit run model for calibration optimization function or on set
#' of fitted parameters
#'
#' @param params Vector containing parameters to optimize, should be only arguments in calibration function
#' @param calibration_function function to optimize, only inputs should be params
#' @param calibration_function_args arguments for calibration function
#' @param result_col string column name of result if calibration function returns multiple columns
#' @param obs_vals array of observations, should be same length as model output
#' @param date array of dates, should be same length as model output and observations. Optional
#' @param obj_fun The objective function to be minimized/maximized (e.g., NSE, SSE, MAE).
#' @param time_ag Character string specifying the time aggregation ("Daily", "Monthly", or "Annual").
#' @param return_dat Logical indicating whether to return the modeled data.
#' @param silence Logical indicating whether to silence return of obj result
#' @param ... Additional arguments passed to the objective function.
#'
#' @return Optimized parameters and objective function value.

test_model_fit <- function(params, 
                           calibration_function = NULL,
                           calibration_function_args = NULL,
                           result_col = NULL,
                           obs_vals = NULL,
                           date = NULL,
                           obj_fun = NULL, 
                           return_dat = FALSE,
                           silence = FALSE,
                           time_ag = NULL
) {
  
  # 1. Run the routing function
  mod_vals_all <- do.call(calibration_function, args = calibration_function_args) 
  
  # 2. Extract the result column
  if (!is.null(result_col)) {
    mod_vals <- mod_vals_all %>% dplyr::pull(all_of(result_col))
  } else {
    mod_vals <- mod_vals_all %>% dplyr::pull(last(names(.)))
  }
  
  # 3. CRITICAL: Check for NAs or Length Mismatches
  # If the model fails or produces non-finite numbers, return a penalty value
  if (length(mod_vals) != length(obs_vals) || any(!is.finite(mod_vals))) {
    if (!silence) message("Warning: Model produced non-finite values or length mismatch. Penalizing fit.")
    return(ifelse(identical(obj_fun, NSE), -9999, 1e10)) # NSE uses -9999, error functions use a huge number
  }
  
  if (!is.null(date)) {
    if (length(obs_vals) != length(date)) {
      stop("Error: Date and observations must have the same length.")
    }
  }
  
  # 4. Create calibration dataframe
  if (!is.null(date)) {
    df_cal <- data.frame(date = as.Date(date), obs_vals, mod_vals)
  } else {
    df_cal <- data.frame(obs_vals, mod_vals)
  }
  
  # Filter to periods with observations
  dat_cal <- df_cal %>% drop_na(obs_vals)
  
  # 5. Time aggregation (Monthly/Annual)
  if (!is.null(time_ag) && "date" %in% names(dat_cal) && time_ag != "Daily") {
    dat_cal <- dat_cal %>%
      dplyr::mutate(dt = if (time_ag == "Monthly") lubridate::ym(substr(date, 1, 7)) else lubridate::year(date)) %>% 
      dplyr::group_by(dt) %>%
      dplyr::summarize(mod_vals = mean(mod_vals, na.rm = TRUE),
                       obs_vals = mean(obs_vals, na.rm = TRUE), .groups = "drop")
  }
  
  # 6. Objective function evaluation
  obj_val <- obj_fun(sim = dat_cal$mod_vals, obs = dat_cal$obs_vals)
  
  if (!silence) print(paste("Current Fit:", round(obj_val, 4)))
  
  # 7. Return Data or Fit
  if (return_dat) {
    # Prepare full output dataframe
    if (!is.null(date)) {
      if ("date" %in% names(mod_vals_all)) {
        mod_vals_all <- mod_vals_all %>% dplyr::select(-date)
      } 
      df_out <- data.frame(date = as.Date(date), obs_vals, mod_vals_all)
    } else {
      df_out <- data.frame(obs_vals, mod_vals_all)
    }
    return(df_out) 
  } else {
    return(obj_val)
  }
}








# 
# 
# 
# test_model_fit <- function(params, 
#                          calibration_function = NULL,
#                          calibration_function_args = NULL,
#                          result_col = NULL,
#                          obs_vals = NULL,
#                          date = NULL,
#                          obj_fun = NULL, #SSE, MAE
#                          return_dat = FALSE,
#                          silence = FALSE,
#                          time_ag = NULL
# ) {
#   
#   #  mod_vals <- calibration_function(params) %>% pull(last(names(.)))
#   mod_vals_all <- do.call(calibration_function, args = calibration_function_args) 
#   
#   if (!is.null(result_col)) {
#         mod_vals <- mod_vals_all %>% dplyr::pull(result_col)
#   } else {
#         mod_vals <- mod_vals_all %>% dplyr::pull(last(names(.)))
#   }
#   
#   
#   if ( !(length(mod_vals) == length(obs_vals) )) {
#     stop("Error: The observations and modeled values must have the same length.")
#   }
#   
#   if (!is.null(date)) {
#     if (!(length(obs_vals) == length(date))) {
#       stop("Error: Date and modeled values must have the same length.")
#     }
#   }
#   
#   # Join modeled flow with input dataframe
#   if (!is.null(date)) {
#     df_cal <- data.frame(date = as.Date(date), obs_vals, mod_vals)
#   } else {
#     df_cal <- data.frame(obs_vals, mod_vals)
#   }
#   
#   # Select subset of data for calibration - only periods with observations
#   dat_cal <- df_cal %>%
#     drop_na(obs_vals)
#   
#   if (!is.null(time_ag) && "date" %in% names(dat_cal) && time_ag != "Daily") {
#     # Time aggregation
#     dat_cal <- dat_cal %>%
#       dplyr::mutate(dt = if (time_ag == "Monthly") lubridate::ym(substr(date, 1, 7)) else year(date)) %>% 
#       dplyr::group_by(dt) %>%
#       dplyr::summarize(mod_vals = mean(mod_vals, na.rm = TRUE),
#                        obs_vals = mean(obs_vals, na.rm = TRUE), .groups = "drop")
#   }
#   
#   # Objective function evaluation
#   obj_val <- do.call(obj_fun, list(sim = dat_cal$mod_vals, obs = dat_cal$obs_vals))
#   
#   # Join modeled flow with input dataframe
#   if (!is.null(date)) {
#     if ("date" %in% names(mod_vals_all)) {
#       mod_vals_all <- mod_vals_all %>% dplyr::select(-date)
#     } 
#     df_out <- data.frame(date = as.Date(date), obs_vals, mod_vals_all)
#   } else {
#     df_out <- data.frame(obs_vals, mod_vals_all)
#   }
#   
#   
#   if (!silence) {
#     print(obj_val)
#   }
#   
#   if(return_dat){
#     return(df_out) 
#   } else {
#     return(obj_val)
#   }
# } 
