#' This let x's scale correspond y's scale.
#' 
#' @param x A vector of values
#' @param y A vector of values
#' @return scaled vector \code{x}
#' @examples
#' scale2(1:10, 0:1)
#' @export
scale2 <- function(x, y) {
  rx <- range(x, na.rm = TRUE)
  ry <- range(y, na.rm = TRUE)
  ((x - rx[1]) * diff(ry)) / diff(rx) + ry[1]
}