#' Easy color interpolation
#' 
#' @param x A vector of values corresponded to the colors
#' @param colors A character vector of colors to interpolate
#' 
#' @return The interpolated color code
#' @examples
#' x <- 1:10
#' plot(x, pch = 19, col = col(x))
#' plot(x, pch = 19, col = col(-x))
#' @export
col <- function(x, colors = c("blue", "red")) {
  rgb( colorRamp(colors)(scale2(x, 0:1)) / 255 )
}