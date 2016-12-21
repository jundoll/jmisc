#' parallel sum
#' 
#' @param ... numeric or character arguments
#' @param na.rm a logical indicating whether missing values should be removed.
psum <- function(..., na.rm = FALSE) {
  dat <- do.call(cbind, list(...))
  res <- rowSums(dat, na.rm = na.rm) 
  idx_na <- !rowSums(!is.na(dat))
  res[idx_na] <- NA
  res
}