#' Easy color linear interpolation
#' 
#' @param x A vector (or array) of values corresponded to the colors
#' @param colors A character vector of colors to interpolate
#' 
#' @return The interpolated color code
#' @examples
#' x <- 1:10
#' plot(x, pch = 19, col = col(x))
#' plot(x, pch = 19, col = col(-x))
#' @export
col <- function(x, colors = c("blue", "red")) {
  tmp_rgb <- colorRamp(colors)(scale2(x, 0:1))
  rv_ind <- which(!is.na(tmp_rgb[, 1]))
  res <- rep(NA_character_, length(x))
  res[rv_ind] <- rgb(tmp_rgb[rv_ind, ], maxColorValue = 255)
  if(is.array(x)) {
    array(res, dim(x))
  } else {
    res
  }
}