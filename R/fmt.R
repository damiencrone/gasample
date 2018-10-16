#' Internal function to format numbers for printing and plotting.
#' 
#' @param x a number to be formatted
#' @param digits number of decimal places to print
fmt = function (x, digits = 2, lead = T, p = NULL, sigstar = NULL) {
  
  num_str = sprintf(paste0("%.", digits, "f"), x)
  
  # Drop leading zero if requested
  if (!lead) {
    num_str = sub("^(-?)0.", "\\1.", num_str)
  }
  
  # Determine whether to print significance stars
  if (!is.null(p)) {
    # If p value provided, assume significance stars are desired
    sigstar = TRUE
  } else if (is.null(sigstar)) {
    # If no p value provided, and sigstar left unspecified, assume significance
    # stars are NOT desired
    sigstar = FALSE
  }
  
  # Add significance stars if requested
  if (sigstar) {
    if (is.null(p)) {
      # If no separate p value provided, assume x is a p value
      num_str = paste0(num_str, pstar(x))
    } else {
      num_str = paste0(num_str, pstar(p))
    }
  }
  
  return(num_str)
  
}
