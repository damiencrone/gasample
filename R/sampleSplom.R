#' Function to produce a scatterplot matrix comparing a sample of items to the
#' population from which they were drawn.
#' 
#' @param items a character vector of items used to index dat
#' @param dat a matrix or dataframe containing describing each item in the population on the dimensions to be plotted
#' @param label_vec optional character vector of variable labels
#' @param selected_col colour for plotting selected items
#' @param nonselected_col colour for plotting nonselected items
#' @param mar character vector containing margin parameters
#' @param nonselected_alpha transparency for scatterplot points for nonselected items
#' @export
sampleSplom = function(items, dat, label_vec = NULL, selected_col = "black",
                       nonselected_col = "gray", mar = c(4, 4, 0.5, 0.5),
                       nonselected_alpha = 0.1) {
  
  SEL = list(
    items = items,
    col = selected_col,
    point_cex = 1,
    point_alpha = 1
  )
  NON = list(
    items = rownames(dat)[!rownames(dat) %in% items],
    col = nonselected_col,
    point_cex = 1/2,
    point_alpha = nonselected_alpha
  )
  
  ndim = ncol(dat)
  text_size = 3*1/ndim
  
  if (is.null(label_vec)) {
    label_vec = colnames(dat)
  }
  
  par(
    mfrow = c(ndim, ndim),
    mar = mar
  )
  
  # Loop through y variables
  for (i in 1:ndim) {
    
    yname = label_vec[i]
    SEL$y = dat[SEL$items, i]
    NON$y = dat[NON$items, i]
    
    # Loop through x variables
    for (j in 1:ndim) {
      
      xname = label_vec[j]
      SEL$x = dat[SEL$items, j]
      NON$x = dat[NON$items, j]
      
      if (i == ndim) {
        xlab = xname
      } else {
        xlab = ""
      }
      
      if (j == 1) {
        ylab = yname
      } else {
        ylab = ""
      }
      
      if (i == j) {
        # On diagonal
        
        plotSampleDensity(
          SEL = SEL,
          NON = NON,
          dat = dat,
          xlab = xlab,
          ylab = ylab,
          text_size = text_size
        )
        
      } else if (i > j) {
        # Below diagonal
        
        # Initialise empty plot
        plot(
          x = 0, type = "n", xlab = xlab, ylab = ylab,
          xlim = range(dat[,j]), ylim = range(dat[,i])
        )
        
        # Plot points
        for (L in list(NON, SEL)) {
          points(
            x = L$x,
            y = L$y,
            col = scales::alpha(L$col, L$point_alpha),
            pch = 19,
            cex = L$point_cex
          )
        }
        
      } else if (i < j) {
        # Above diagonal
        
        # Initialise empty plot
        plot(
          x = 0, type = "n", xlab = xlab, ylab = ylab, axes = FALSE,
          xlim = c(0, 1), ylim = c(0, 1)
        )
        
        SEL$cor = cor.test(x = SEL$y, y = SEL$x)
        NON$cor = cor.test(x = NON$y, y = NON$x)
        
        text(
          x = 0.5,
          y = 0.5,
          labels = paste0(
            "Selected r = ", fmt(SEL$cor$estimate, lead = F, p = SEL$cor$p.value), "\n",
            "Non-selected r = ", fmt(NON$cor$estimate, lead = F, p = NON$cor$p.value), "\n",
            "Difference = ", fmt(SEL$cor$estimate - NON$cor$estimate)
          ),
          cex = text_size
        )
        
      }
      
    }
    
  }
  
}