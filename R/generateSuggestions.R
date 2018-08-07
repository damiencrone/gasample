#' Function to generate suggestions to initialise the genetic algorithm with.
#' 
#' @param items a character vector containing the items in the population
#' @param sample_size the number of items to sample
#' @param n_suggestions number of suggestions to initialise the GA with
#' @param required_items character vector of any items required to be included
#'   in the solution
#' @return a matrix suggestions to be passed to the genetic algorithm
#' @export
generateSuggestions = function (items, sample_size, n_suggestions = 1000, required_items = NULL) {
  
  suggestion_mat = matrix(
    data = FALSE,
    nrow = n_suggestions,
    ncol = length(items),
    dimnames = list(NULL, items)
  )
  
  if (!is.null(required_items)) {
    n_to_sample = sample_size - length(required_items)
    items_to_sample = items[!items %in% required_items]
    suggestion_mat[1:n_suggestions, items %in% required_items] = TRUE
  } else {
    n_to_sample = sample_size
    items_to_sample = items
  }
  
  # Loop through initial population members
  for (row_num in 1:nrow(suggestion_mat)) {
    
    ind = sample(x = items_to_sample, size = n_to_sample, replace = FALSE)
    suggestion_mat[row_num, ind] = TRUE
    
  }
  
  return(suggestion_mat)
  
}