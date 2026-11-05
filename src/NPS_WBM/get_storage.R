#' CSU Created Function
#'
#' get_storage
#'
#' Calculates daily values of water released from storage (storage_release) and
#' water remaining in storage (storage_remain) in mm.
#' @param storage_add A daily time series of water added to storage (rain + melt - AET)
#' @param return_rate fractional (0-1) indicating the rate at which stored water is returned as runoff
#' @export storage_release daily water released from storage
#' @export storage_remain daily water remaining in storage
get_storage <- function(storage_add = DailyWB$STORAGE_ADD, 
                        return_rate = Return_Rate) {
  storage_release <- 0 * storage_add
  storage_release[1] <- storage_add[1] * return_rate
  
  storage_remain <- 0 * storage_add # Start empty
  storage_remain[1] <- storage_add[1] * (1-return_rate)
  
  for (i in 2:length(storage_release)) {
    storage_release[i] <- (storage_remain[i-1] + storage_add[i])*return_rate
    storage_remain[i] <- (storage_remain[i-1] + storage_add[i]) * (1-return_rate) 
  }
  
  return(list("storage_release" = storage_release,
              "storage_remain" = storage_remain))
}
