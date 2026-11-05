#'
#' This function scans the Global Environment, calculates the memory usage of 
#' every object, and returns a sorted summary of the largest n items.
#'
#' @param n Integer. The number of top objects to return. Defaults to 10.
#'
#' @return A data frame with two columns:
#' \item{object}{The name of the object in the workspace.}
#' \item{readable_size}{The size of the object formatted in B, Kb, Mb, or Gb.}
#' 
#' @details 
#' The function uses \code{object.size} to measure memory. Note that for 
#' some complex objects (like those using reference classes or pointers to 
#' external C++ memory), this may slightly underreport the true RAM usage.
#'

storage_check <- function(n = 10) {
  objs <- ls(envir = .GlobalEnv)
  
  if (length(objs) == 0) return("Workspace is empty.")
  
  # Calculate sizes
  sizes <- sapply(objs, function(x) object.size(get(x)))
  
  # Create the dataframe
  out <- data.frame(
    object = objs,
    size_bytes = sizes
  )
  
  # Sort by size
  out <- out[order(out$size_bytes, decreasing = TRUE), ]
  top_n <- head(out, n)
  
  # FIX: Use sapply to format each size individually
  top_n$readable_size <- sapply(top_n$size_bytes, function(x) {
    format(structure(x, class = "object_size"), units = "auto")
  })
  
  # Return clean output
  return(top_n[, c("object", "readable_size")])
}


