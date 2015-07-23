#' Repeat garbage collection
#' 
#' @param iter The number of repetition GC
#' @export
gc20 <- function(iter = 20) {
  invisible(replicate(iter, gc()))
}