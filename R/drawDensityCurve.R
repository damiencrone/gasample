#' Internal function to draw density curve.
#' 
#' @param d a density object
#' @param col line colour
drawDensityCurve = function (d = NULL, col = "black") {
  if (!is.null(d)) {
    lines(
      x = d$x,
      y = d$y,
      col = col
    )
  }
}