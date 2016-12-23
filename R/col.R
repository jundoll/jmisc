#' Easy color scale maker
#' 
#' @param x A vector (or array) of values corresponded to the colors
#' @param colors A character vector of the base colors to interpolate
#' @param method An interpolation method. Choose from "linear" or "quantile".
#' @param n If `method` is "quantile", select the number of the division.
#' @param ... Undescribed
#' 
#' @return The interpolated color code
#' @examples
#' x <- 1:10
#' plot(x, pch = 19, col = col(x))
#' plot(x, pch = 19, col = col(-x))
#' 
#' x <- sort(rexp(1000, 10))
#' plot(x, 1:length(x), pch = 19, col = col(x))
#' plot(x, 1:length(x), pch = 19, col = col(x, method = "quantile", n = 5))
#' @export
col <- function(x, colors = c("blue", "red"), method = "linear", n, ...) {
  if(method == "linear") {
    tmp_rgb <- colorRamp(colors)(scale2(x, 0:1))
  } else if(method == "quantile") {
    if(missing(n)) {
      n <- 5
    }
    stopifnot(n >= 2)
    breaks <- unique(quantile(x, probs = seq(0, 1, length.out = n+1)))
    x_cut <- cut(x, breaks = breaks, include.lowest = TRUE, ordered_result = TRUE)
    levels(x_cut) <- scale2(seq_along(levels(x_cut)), 0:1)
    tmp_rgb <- colorRamp(colors)(unfactor(x_cut))
  } else {
    stop("Choose the available method.")
  }
  rv_ind <- which(!is.na(tmp_rgb[, 1]))
  res <- rep(NA_character_, length(x))
  res[rv_ind] <- rgb(tmp_rgb[rv_ind, ], maxColorValue = 255)
  if(is.array(x)) {
    array(res, dim(x))
  } else {
    res
  }
}