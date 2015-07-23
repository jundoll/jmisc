#' Moving window
#' 
#' @param x Sequence of values
#' @param FUN The function to be applied to each window.
#' @param int
#' 
#' @return first mode value
#' @examples
#' MW(1:10, mean)
#' @export
MW <- function(dat, FUN, int = 2L) {
  stopifnot(int > 1)
  N <- length(dat)
  x <- vector("list", N)
  for(i in 1L:(int-1L)) {
    x[[i]] <- FUN(dat[1L:i])
  }
  for(i in int:N) {
    x[[i]] <- FUN(dat[(i-int+1L):i])
  }
  x <- do.call("rbind", x)
  if(ncol(x) == 1) {
    return(x[, 1])
  } else {
    x
  }
}