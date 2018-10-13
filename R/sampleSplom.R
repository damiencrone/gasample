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
  
  fmt = function(x){sub("^(-?)0.", "\\1.", sprintf("%.2f", x))}
  pstar = function(p){
    if(p<.001){
      s = "***"
    }else if(p<.01){
      s = "**"
    } else if(p<.05){
      s = "*"
    } else if(p<.01){
      s = "^"
    } else{
      s = ""
    }
  }
  
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
        
        # Compute densities
        d = list(
          selected = density(SEL$y),
          nonselected = density(NON$y)
        )
        
        # Compute y axis parameters
        ylim = range(sapply(d, function(d){range(d$y)})) * 2.5
        yrange = ylim[2]-ylim[1]
        NON$y_off = ylim[1] + yrange * 0.6
        SEL$y_off = ylim[1] + yrange * 0.9
        
        # Initialise empty plot
        plot(
          x = 0, type = "n", xlab = xlab, ylab = ylab, main = "",
          xlim = range(dat[,i]), ylim = ylim
        )
        
        # Unselected density
        lines(
          x = d$nonselected$x,
          y = d$nonselected$y,
          col = NON$col
        )
        
        # Selected density
        lines(
          x = d$selected$x,
          y = d$selected$y,
          col = SEL$col
        )
        
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
        
        # Descriptives
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
          x = 0,type = "n",xlab = xlab,ylab = ylab,axes = FALSE,
          xlim = c(0, 1),ylim = c(0, 1)
        )
        
        SEL$cor = cor.test(x = SEL$y, y = SEL$x)
        NON$cor = cor.test(x = NON$y, y = NON$x)
        
        text(
          x = 0.5,
          y = 0.5,
          labels = paste0(
            "Selected r = ", fmt(SEL$cor$estimate), pstar(SEL$cor$p.value), "\n",
            "Non-selected r = ", fmt(NON$cor$estimate), pstar(NON$cor$p.value), "\n",
            "Difference = ", sprintf("%.2f", SEL$cor$estimate - NON$cor$estimate)
          ),
          cex = text_size
        )
        
      }
      
    }
    
  }
  
}