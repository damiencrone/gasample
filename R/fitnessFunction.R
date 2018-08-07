#' Function to sample items with a genetic algorithm maximizing some measure of 
#' fitness (e.g., everage inter-item dissimilarity) as measured by an inter-item
#' distance matrix.
#' 
#' @param x a binary vector of sampled items
#' @param sample_size the number of items to sample
#' @param distance_mat a population inter-item distance matrix
#' @param lower_tri_funciton the specific function to assess fitness of the 
#'   lower triangle of the sample distance matrix
#' @param required_items character vector of any items required to be included
#'   in the solution
#' @return a value representing the fitness of the current sample
#' @export
fitnessFunction = function(x, sample_size, distance_mat, lower_tri_function = mean, required_items = NULL) {
  
  wrong_n = sum(x) != sample_size
  
  if (!is.null(required_items)) {
    missing_required_items = FALSE
  } else {
    missing_required_items = !all(required_items %in% which(as.logical(x)))
  }
  
  # Cycle through penalty conditions
  if (wrong_n | missing_required_items) {
    
    f = -Inf
    
  } else {
    # Otherwise, if valid set of items in sample
    
    # Construct distance matrix for selected items
    sample_mat = distance_mat[as.logical(x), as.logical(x)]
    
    # Compute fitness (mean inter-stimulus distance)
    sample_lower_tri = sample_mat[lower.tri(sample_mat)]
    f = lower_tri_function(sample_lower_tri)
    
  }
  
  return(f)
  
}