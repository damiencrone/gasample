#' Function to calculate the total dendrogram height of a distance matrix.
#' Inspired by Petchey, O. L., & Gaston, K. J. (2002). Functional diversity
#' (FD), species richness and community composition. Ecology Letters.
#' 
#' @param x a vector containing the lower triangle of a distance matrix
#' @return the sum of the height of the dendrogram for the given distance matrix
#' @export
sumHeight = function (x) {
  
  require(pracma)
  
  # Perform input conversion
  if (is.vector(x)) {
    x = as.dist(pracma::squareform(x))
  } else if (is.matrix(x)) {
    x = as.dist(x)
  } else if (class(x) != "dist") {
    stop("Unrecognised input type.")
  }
  
  # Perform hierarchical cluster analysis and compute height
  hca = hclust(d = x)
  height_sum = sum(hca$height)
  
  return(height_sum)
  
}