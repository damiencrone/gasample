#' Function to produce scatterplots depicting bivariate distributions across
#' different samples.
#' 
#' @param items a character vector of items used to index dat
#' @param var_name a string contianing the name of the variable to be described
#' @param dat a matrix or dataframe containing describing each item in the 
#'   population on the dimensions to be plotted
#' @param G a list describing each group of items
#' @param xlab string containing x axis label
#' @param ylab string containing y axis label
#' @param xlim an xlim vector
#' @param ylim an ylim vector
#' @param selected_col colour for plotting selected items
#' @param nonselected_col colour for plotting nonselected items
#' @param nonselected_alpha transparency for scatterplot points for nonselected items
#' @param include_nonsampled logical denoting whether to plot nonsampled items
#' @export
plotSampleScatter = function (items = NULL, var_name = NULL, dat = NULL, G = NULL,
                              xlim = NULL, ylim = NULL, xlab = "", ylab = "",
                              selected_col = "black", nonselected_col = "gray",
                              nonselected_alpha = 0.1, include_nonsampled = TRUE) {
  
  group_lists_missing = is.null(G)
  dat_available = !is.null(dat) & !is.null(var_name)
  
  # Construct group lists from data if not already provided
  if (group_lists_missing) {
    
    G = constructGroupList(
      items = items,
      population = dat,
      include_nonsampled = include_nonsampled,
      col = selected_col,
      nonselected_col = nonselected_col,
      nonselected_alpha = nonselected_alpha,
      var_name = var_name
    )
    
  }
  
  if (dat_available & is.null(xlim)) {
    xname = var_name[length(var_name)]
    xlim = range(dat[, xname], na.rm = TRUE)
  } else if (is.null(xlim)) {
    xlim = range(sapply(G, function(x) x$y), na.rm = TRUE)
  }
  
  # Initialise empty plot
  plot(
    x = 0, type = "n", xlab = xlab, ylab = ylab,
    xlim = xlim, ylim = ylim
  )
  
  # Plot points
  for (g in G[length(G):1]) {
    points(
      x = g$x,
      y = g$y,
      col = scales::alpha(g$col, g$point_alpha),
      pch = 19,
      cex = g$point_cex
    )
  }
  
}