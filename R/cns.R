#' Consecutive number sequences
#' 
#' @param x A vector
#' 
#' @return The number of consecutive the value
#' @examples
#' cns(c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE))
#' @export
cns <- function(x) {
  unlist(sapply(rle(x)$lengths, seq))
}