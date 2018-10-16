#' Internal function to draw density curve.
#' 
#' @param d a density object
#' @param col line colour
drawDensityCurve = function (d, col) {
  lines(
    x = d$x,
    y = d$y,
    col = col
  )
}