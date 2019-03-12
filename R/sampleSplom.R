#' Function to produce a scatterplot matrix comparing a sample of items to the
#' population from which they were drawn.
#' 
#' @param items a character vector of items used to index dat
#' @param dat a matrix or dataframe containing describing each item in the population on the dimensions to be plotted
#' @param label_vec optional character vector of variable labels
#' @param selected_col colour for plotting selected items
#' @param nonselected_col colour for plotting nonselected items
#' @param mar vector containing margin parameters passed to par()
#' @param mgp vector containing axis label locations passed to par()
#' @param nonselected_alpha transparency for scatterplot points for nonselected items
#' @param xlim an xlim vector or list of xlim vectors
#' @param ylim a ylim vector or list of ylim vectors
#' @param include_nonsampled logical denoting whether to plot nonsampled items
#' @export
sampleSplom = function(items, dat, label_vec = NULL, selected_col = "black",
                       nonselected_col = "gray", mar = c(3, 3, 0.5, 0.5),
                       mgp = c(2, 0.5, 0), nonselected_alpha = 0.1, xlim = NULL,
                       ylim = NULL, text_size = NULL, include_nonsampled = TRUE) {
  
  ndim = ncol(dat)
  xlim_provided = !is.null(xlim)
  xlim_input = xlim
  ylim_provided = !is.null(ylim)
  ylim_input = ylim
  
  if (is.null(text_size)) {
    text_size = 3*1/ndim
  }
  
  if (is.null(label_vec)) {
    label_vec = colnames(dat)
  }
  
  par(
    mfrow = c(ndim, ndim),
    mar = mar,
    mgp = mgp
  )
  
  # Loop through y variables
  for (i in 1:ndim) {
    
    yname = label_vec[i]
    
    if (ylim_provided) {
      
      if (is.vector(ylim_input)) {
        ylim = ylim_input
      } else if (is.list(ylim_input)) {
        ylim = ylim_input[[j]]
      }
      
    } else {
      
      ylim = range(dat[,i])
      
    }
    
    # Loop through x variables
    for (j in 1:ndim) {
      
      xname = label_vec[j]
      
      if (xlim_provided) {
        
        if (is.vector(xlim_input)) {
          xlim = xlim_input
        } else if (is.list(xlim_input)) {
          xlim = xlim_input[[j]]
        }
        
      } else {
        
        xlim = range(dat[,j])
        
      }
      
      G = constructGroupList(
        items = items,
        population = dat,
        include_nonsampled = include_nonsampled,
        var_name = colnames(dat)[c(i, j)],
        col = selected_col,
        nonselected_col = nonselected_col,
        nonselected_alpha = nonselected_alpha
      )
      
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
          G = G,
          dat = dat,
          xlab = xlab,
          ylab = ylab,
          text_size = text_size,
          xlim = xlim
        )
        
      } else if (i > j) {
        # Below diagonal
        
        plotSampleScatter(
          G = G,
          xlim = xlim,
          ylim = ylim,
          xlab = xlab,
          ylab = ylab,
        )
        
      } else if (i < j) {
        # Above diagonal
        
        # Initialise empty plot
        plot(
          x = 0, type = "n", xlab = xlab, ylab = ylab, axes = FALSE,
          xlim = c(0, 1), ylim = c(0, 1)
        )
        
        if (length(G) == 1) {
          
          text(
            x = 0.5,
            y = 0.5,
            labels = paste0(
              "r = ", fmt(G$SEL$cor$estimate, lead = F, p = G$SEL$cor$p.value), "\n",
              "[", fmt(G$SEL$cor$conf.int[1], lead = F), " - ", fmt(G$SEL$cor$conf.int[2], lead = F), "]"
            ),
            cex = text_size*2
          )
          
        } else if (length(G) == 2) {
          
          text(
            x = 0.5,
            y = 0.5,
            labels = paste0(
              "Selected r = ", fmt(G$SEL$cor$estimate, lead = F, p = G$SEL$cor$p.value), "\n",
              "Non-selected r = ", fmt(G$NON$cor$estimate, lead = F, p = G$NON$cor$p.value), "\n",
              "Difference = ", fmt(G$SEL$cor$estimate - G$NON$cor$estimate)
            ),
            cex = text_size
          )
          
        }
        
      }
      
    }
    
  }
  
}