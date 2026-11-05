#' Custom cross correlation function to calculate correlation for positive lags
#' between x (non_lagged variable) and y (lagged variable). Allows option to set
#' minimum number of pairs to use in correlation calculation and returns NA
#' if pairs < min number.
#' 
#' article on x-corr describing issues with R's ccf
#' https://medium.com/@jan.seifert/the-r-cross-correlation-function-f5f426006425 
#' 
#' @param x array that is held constant in ccf analysis
#' @param y array that is lagged during ccf analysis (positive lags only)
#' @param min_pairs minimum number of pairs to use in ccf analysis.

custom_ccf <- function(x, y, min_pairs = 10, max_lag = 5, to_plot = TRUE, na_handle = c("fill","rm")) {
  
  # TESTING DATA
  
  # hg <- hdat %>%
  #   filter(site_no == hdat$site_no[1],
  #          variable == hdat$variable[1])
  # 
  # ggplot(hg) + 
  #   geom_point(aes(x = mean_value, y = mean_pdsi), color = "black") +
  #   geom_smooth(aes(x = mean_value, y = mean_pdsi), color = "black", span = 0.2, se = FALSE) +
  #   geom_point(aes(x = mean_value, y = lag(mean_pdsi,4)), color = "skyblue") +
  #   geom_smooth(aes(x = mean_value, y = lag(mean_pdsi,4)), color = "skyblue", span = 0.2, se = FALSE) +
  #   theme_bw()
  #  
  # to_plot <- TRUE
  # min_pairs <- 10
  # x <- hdat$mean_value #GW LEVEL
  # y <- hdat$mean_pdsi #PDSI
  
  # Determine the maximum lag based on the length of array x and the minimum number of pairs:
  n_lag <- length(x[!is.na(x)]) - min_pairs
  
  # Adjust max_lag to ensure it does not exceed the allowable maximum lag:
  if (n_lag < max_lag){
    max_lag <- n_lag
  }
  
  
  # Create a sequence of lags from 0 to max_lag:
  lags <- seq(0, max_lag)
  
  # use sd and mean of all values, not just those in sample
  mean_x <- mean(x, na.rm = TRUE)
  mean_y <- mean(y, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  sd_y <- sd(y, na.rm = TRUE)
  
  # calculate correlation for a given lag:
  cor_func <- function(lag) {
    # Shift x and y by the current lag:
    cor_x <- x[(1 + lag):length(x)]
    cor_y <- y[1:(length(y) - lag)]
    
    
    if (na_handle == "fill") {
      # Interpolate missing values using linear approximation...
      cor_x_clean <- na.approx(cor_x)
      cor_y_clean <- na.approx(cor_y)
      #... or remove the pairs when either value is NA
    } else if (na_handle == "rm") {
      cor_x_clean <- cor_x[complete.cases(cor_x,cor_y)]
      cor_y_clean <- cor_y[complete.cases(cor_x,cor_y)]
    }
   
    #mean_x <- mean(cor_x_clean)
    #mean_y <- mean(cor_y_clean)
    #sd_x <- sd(cor_x_clean)
    #sd_y <- sd(cor_y_clean)
    
    n <- length(cor_x_clean) # number of valid pairs
   
     # If the number of pairs meets the minimum threshold...
    if (n >= min_pairs) {
      #cor(cor_x, cor_y, use = "pairwise.complete.obs")
      # calculate correlation:
      sum((cor_x - mean_x) * (cor_y - mean_y), na.rm = TRUE) / ((n - 1) * sd_x * sd_y)
   
       } else (NA) # otherwise NA (ie number of pairs is insufficient)
    
  }
  
  # Apply `cor_func()` to each lag in the sequence and store results as a list
  correlation <- purrr::map(lags, cor_func) %>% unlist()
  
  # Create a df of correlation results for each lag
  ccf_result <- data.frame(lags, correlation) %>%
    dplyr::rename(c("lag" = "lags"))
  
  if (to_plot) {
    plot(ccf_result, type = "h", xlab = "Lag", ylab = "Correlation",
         main = "Cross-Correlation Function")
  }
  
  return(ccf_result)
}



