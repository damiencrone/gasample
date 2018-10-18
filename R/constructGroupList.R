#' Internal function to construct a list of plotting parameters.
#' 
#' @param items a character vector or list of character vectors representing 
#'   groups of items used to index population
#' @param population a matrix or dataframe containing describing each item in
#'   the population on the dimensions to be plotted
#' @param include_nonsampled logical deonting whether to construct a group for
#'   all items that haven't been selected
#' @param var_name string or vector of length 2 (c(y,x)) with names of variables
#'   of interest
#' @param col string or vector of colours for selected group(s) of items
#' @param selected_point_cex cex for scatterplot points for selected items
#' @param nonselected_col colour for scatterplot points for nonselected items
#' @param nonselected_point_cex cex for scatterplot points for nonselected items
#' @param nonselected_alpha transparency for scatterplot points for nonselected
#'   items
constructGroupList = function (items, population, include_nonsampled = TRUE, var_name = NULL, col = NULL,
                               selected_point_cex = 1/2, nonselected_col = "gray", nonselected_point_cex = 1/4,
                               nonselected_alpha = 1/3) {
  
  single_sample = class(items) == "character" & is.null(names(items))
  raw_data = class(population) %in% c("matrix", "data.frame")
  
  if (class(population) == "character") {
    all_items = population
  } else {
    all_items = rownames(population)
  }
  
  G = list()
  
  if (single_sample) {
    
    if (is.null(col)) {
      col = "black"
    }
    
    G$SEL$items = items
    G$SEL$col = col
    G$SEL$point_cex = selected_point_cex
    G$SEL$point_alpha = 1
    
  }
  
  if (include_nonsampled) {
    
    all_sampled_items = unique(sapply(X = G, FUN = function(x) x$items))
    
    G$NON$items = all_items[!all_items %in% all_sampled_items]
    G$NON$col = nonselected_col
    G$NON$point_cex = nonselected_point_cex
    G$NON$point_alpha = nonselected_alpha
    
  }
  
  # If raw data available and variable of interest specified, exatract some
  # additional information
  if (raw_data & !is.null(var_name)) {
    
    for (i in 1:length(G)) {
      
      items_in_group = G[[i]]$items
      yvar = var_name[1]
      
      G[[i]]$y = population[items_in_group, yvar]
      
      if (length(var_name) == 2) {
        xvar = var_name[2]
        G[[i]]$x = population[items_in_group, xvar]
        G[[i]]$cor = cor.test(x = G[[i]]$x, y = G[[i]]$y)
      }
      
      G[[i]]$nobs = sum(!is.na(G[[i]]$y))
      
      if (G[[i]]$nobs > 1) {
        G[[i]]$mean = mean(G[[i]]$y, na.rm = TRUE)
        G[[i]]$sd = sd(G[[i]]$y, na.rm = TRUE)
        G[[i]]$d = density(G[[i]]$y, na.rm = TRUE)
      }
      
    }
    
  }
  
  # Calculate y offsets
  y_off_vec = seq(from = 1, to = 0, length.out = length(G)+2)[-c(1, length(G)+2)]
  for (i in 1:length(G)) {
    G[[i]]$y_off = y_off_vec[i]
  }
  
  return(G)
  
}