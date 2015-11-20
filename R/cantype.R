#' Undescribe
#' 
#' @param Undescribe
#' 
#' @return Undescribe
#' @examples
#' (dat_logi <- as.character(sample(c(TRUE, FALSE), 10, TRUE)))
#' cantype(dat_logi)
#' (dat_int <- as.character(sample(5, 10, TRUE)))
#' cantype(dat_int)
#' (dat_num <- as.character(rnorm(10)))
#' cantype(dat_num)
#' @export
cantype <- function(x, ...) {
  stopifnot(is.vector(x))
  stopifnot(class(x) %in% c("logical", "integer", "numeric", "complex", "character", "factor"))
  type.convert(as.character(x), as.is = TRUE, ...)
}