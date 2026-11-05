# Calculate the water year given an array of dates. Optional modification of
# start month in case area follows a different water year.

#' @param dates = date array
#' @param water_year_start_month integer number between 1 and 12 indicating
#'                               the month that water year starts.

calc_water_year <- function(dates, water_year_start_month = 10) {
  # Ensure the input is treated as a date (lubridate functions handle various formats)
  dates <- as_date(dates)
  
  # Extract calendar year and month
  calendar_year <- year(dates)
  month_of_year <- month(dates)
  
  # Apply the water year logic:
  # If the month is October (10) or later, the water year is the next calendar year.
  # Otherwise (January to September), the water year is the current calendar year.
  water_year <- ifelse(month_of_year >= water_year_start_month, calendar_year + 1, calendar_year)
  
  return(water_year)
}
