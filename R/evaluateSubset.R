#' Computes distance-based descriptive statistics for a sample of items.
#' 
#' @param indices a numeric or logical vector of the sample of items
#' @param distance_matrix a matrix containing inter-item distances for the
#'   entire population
#' @return a vector of descriptive statistics for the sample
#' @export
evaluateSubset = function (indices, distance_mat) {
  
  # Extract distances for the specified subset
  distance_subset = as.matrix(distance_mat)[indices, indices]
  distance_vec = distance_subset[lower.tri(distance_subset)]
  
  # Compute average and minimum inter-item distance
  result = c(
    mean = mean(distance_vec),
    sd = sd(distance_vec),
    quantile(x = distance_vec, c(0.10, 0.25, 0.50, 0.75, 0.90)),
    min = min(distance_vec),
    max = max(distance_vec)
  )
  
  return(result)
  
}