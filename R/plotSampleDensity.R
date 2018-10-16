#' Function to produce density curves comparing the distribution of a variable 
#' across different samples.
#' 
#' @param items a character vector of items used to index dat
#' @param var_name a string contianing the name of the variable to be described
#' @param dat a matrix or dataframe containing describing each item in the 
#'   population on the dimensions to be plotted
#' @param SEL a list describing selected items containing the fields "items", 
#'   "col" and "y"
#' @param SEL a list describing non-selected items containing the fields 
#'   "items", "col" and "y"
#' @param xlab string containing x axis label
#' @param ylab string containing y axis label
#' @param textsize cex value for descriptive text (default=1)
#' @param selected_col colour for plotting selected items
#' @param nonselected_col colour for plotting nonselected items
#' @param na.rm logical denoting whether to remove missing values from density
#'   calculation
#' @export
plotSampleDensity = function (items = NULL, var_name = NULL, dat = NULL, SEL = NULL, NON = NULL,
                              xlab = "", ylab = "", text_size = 1, selected_col = "black",
                              nonselected_col = "gray", na.rm = TRUE) {
  
  group_lists_missing = is.null(SEL) & is.null(NON)
  
  if (group_lists_missing) {
    
    SEL = list(
      items = items,
      col = selected_col
    )
    NON = list(
      items = rownames(dat)[!rownames(dat) %in% items],
      col = nonselected_col
    )
    
    SEL$y = dat[SEL$items, var_name]
    NON$y = dat[NON$items, var_name]
    
  }
  
  # Compute densities
  d = list(
    selected = density(SEL$y, na.rm = na.rm),
    nonselected = density(NON$y, na.rm = na.rm)
  )
  
  # Compute y axis parameters
  ylim = range(sapply(d, function(d){range(d$y)})) * 2.5
  yrange = ylim[2]-ylim[1]
  NON$y_off = ylim[1] + yrange * 0.6
  SEL$y_off = ylim[1] + yrange * 0.9
  
  # Initialise empty plot
  plot(
    x = 0, type = "n", xlab = xlab, ylab = ylab, main = "",
    xlim = range(c(SEL$y, NON$y)), ylim = ylim
  )
  
  # Draw density curves
  drawDensityCurve(d = d$nonselected, col = NON$col)
  drawDensityCurve(d = d$selected, col = SEL$col)
  
  NON$mean = mean(NON$y)
  SEL$mean = mean(SEL$y)
  NON$sd = sd(NON$y)
  SEL$sd = sd(SEL$y)
  
  # Means
  for (L in list(NON, SEL)) {
    points(
      x = L$mean,
      y = L$y_off,
      pch = 19,
      col = L$col
    )
  }
  
  # Add descriptive text
  for (L in list(NON, SEL)) {
    text(
      x = L$mean,
      y = L$y_off,
      labels = paste0("M = ", sprintf("%.2f", L$mean), "; SD = ", sprintf("%.2f", L$sd)),
      col = L$col,
      pos = 1,
      cex = text_size
    )
  }
  
  # SD bars
  for (L in list(NON, SEL)) {
    lines(
      x = L$mean + c(-L$sd, L$sd),
      y = rep(L$y_off, 2),
      col = L$col,
      lty = 1
    )
  }
  
}