#' Shift vector's elements
#' 
#' @param x A vector of values
#' @param n A integer of length 1. x will shift by the number.
#' @param back If TRUE, will shift \code{x} back by \code{n}.
#' @return shifted \code{x}
#' @examples
#' shift(1:10)
#' shift(1:10, n = 3L)
#' shift(1:10, back = FALSE)
#' shift(1:10, n = -1L)
#' @export
shift <- function(x, n = 1L, back = TRUE) {
  x_len <- length(x)
  n <- as.integer(n)
  if(n < 0) {
    n <- -n
    back <- !back
  }
  if(back) {
    return(x[c((x_len-n+1L):x_len, 1:(x_len-n))])
  }
  else {
    return(x[c((n+1L):x_len, 1:n)])
  }
}