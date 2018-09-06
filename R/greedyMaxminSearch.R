#' Function to sample items with a greedy search maximizing minimum inter-item
#' distance.
#' 
#' @param distance_mat a population inter-item distance matrix
#' @param sample_size the number of items to sample
#' @param required_items character vector of any items required to be included
#'   in the solution
#' @return a set of item names maximising minimum inter-item distance
#' @export
greedyMaxminSearch = function (distance_mat, sample_size, required_items = NULL) {
  
  if (class(distance_mat ) == "dist") {
    distance_mat = as.matrix(distance_mat)
  }
  
  items = rownames(distance_mat)
  if (is.null(items)) {
    items = 1:nrow(distance_mat)
  }
  
  largest_dist = which(x = distance_mat == max(distance_mat), arr.ind = TRUE)
  
  if (is.null(required_items)) {
    selected = items[largest_dist[1,]]
  } else {
    selected = required_items
  }
  
  while (length(selected) < sample_size) {
    
    dist_from_selected = distance_mat[selected,]
    farthest = names(which.max(apply(dist_from_selected, 2, min)))
    
    selected = c(
      selected,
      farthest
    )
    
  }
  
  return(selected)
  
}