#' plus minus
#' 
#' @param x A integer of length 1
#' @param y A integer of length 1
#' @param cont If TRUE, return \code{x+y} and \code{x-y}. If FALSE, return a vector from \code{x-y} to \code{x+y}.
#' @examples
#' pm(4, 5)
#' pm(4, 5, cont = TRUE)
#' @export
pm <- function(x, y, cont = FALSE) {
  if(cont) {
    return((x - y):(x + y))
  }
  else {
    return(c(x - y, x + y))
  }
}