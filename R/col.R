#' Undescribed
#' 
#' @param Undescribed
#' 
#' @return Undescribed
#' @examples
#' x <- 1:10
#' plot(x, pch = 19, col = col(x))
#' plot(x, pch = 19, col = col(-x))
#' @export
col <- function(x, colors = c("blue", "red")) {
  rgb( colorRamp(colors)(scale2(x, 0:1)) / 255 )
}