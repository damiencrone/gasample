#' Function to produce density curves comparing the distribution of a variable 
#' across different samples.
#' 
#' @param items a character vector of items used to index dat
#' @param var_name a string contianing the name of the variable to be described
#' @param dat a matrix or dataframe containing describing each item in the 
#'   population on the dimensions to be plotted
#' @param G a list describing each group of items
#' @param xlab string containing x axis label
#' @param ylab string containing y axis label
#' @param textsize cex value for descriptive text (default=1)
#' @param selected_col colour for plotting selected items
#' @param nonselected_col colour for plotting nonselected items
#' @param na.rm logical denoting whether to remove missing values from density
#'   calculation
#' @param include_nonsampled logical denoting whether to plot nonsampled items
#' @param xlim an xlim vector
#' @export
plotSampleDensity = function (items = NULL, var_name = NULL, dat = NULL, G = NULL,
                              xlab = "", ylab = "", text_size = 1, selected_col = "black",
                              nonselected_col = "gray", na.rm = TRUE, include_nonsampled = TRUE,
                              xlim = NULL) {
  
  group_lists_missing = is.null(G)
  dat_available = !is.null(dat) & !is.null(var_name)
  
  # Construct group lists from data if not already provided
  if (group_lists_missing) {
    
    G = constructGroupList(
      items = items,
      population = dat,
      include_nonsampled = include_nonsampled,
      nonselected_col = nonselected_col,
      var_name = var_name
    )
    
  }
  
  if (dat_available & is.null(xlim)) {
    xlim = range(dat[, var_name], na.rm = TRUE)
  } else if (is.null(xlim)) {
    xlim = range(sapply(G, function(x) x$y), na.rm = TRUE)
  }
  
  # Compute y axis parameters
  max_y = max(unlist(sapply(G, function(g) g$d$y)), na.rm = TRUE)
  y_mult = 2.5
  ylim = c(0, max_y) * y_mult
  for (i in 1:length(G)) {
    G[[i]]$y_off = max_y + G[[i]]$y_off * max_y * (y_mult-1)
  }
  
  # Initialise empty plot
  plot(
    x = 0, type = "n", xlab = xlab, ylab = ylab, main = "",
    xlim = xlim, ylim = ylim
  )
  
  # Draw density curves
  lapply(
    X = G[length(G):1],
    FUN = function(g) drawDensityCurve(g$d, g$col)
  )
  
  # Add means, SDs, and descriptive text
  for (L in G[length(G):1]) {
    
    points(
      x = L$mean,
      y = L$y_off,
      pch = 19,
      col = L$col
    )
    
    lines(
      x = L$mean + c(-L$sd, L$sd),
      y = rep(L$y_off, 2),
      col = L$col,
      lty = 1
    )
    
    text(
      x = L$mean,
      y = L$y_off,
      labels = paste0("M = ", sprintf("%.2f", L$mean), "; SD = ", sprintf("%.2f", L$sd)),
      col = L$col,
      pos = 1,
      cex = text_size
    )
    
  }
  
}