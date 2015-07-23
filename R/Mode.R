#' Return mode values
#' 
#' @param x A vector of values
#' @param useNA Use NA or not
#' @param ... Arguments that table function have
#' 
#' @return mode values
#' @examples
#' dat <- sample(10, 100, TRUE)
#' Mode(dat)
#' dat_na <- sample(c(1, NA), 100, TRUE, c(0.1, 0.9))
#' Mode(dat_na, useNA = TRUE)
#' @export
Mode <- function(x, useNA = FALSE, ...) {
  if(all(is.na(x))) {
    return(NA)
  } else {
    useNA <- if(!useNA) c("no", "ifany", "always") else "ifany"
    x_tab <- table(x, useNA = useNA, ...)
    cantype(names(which(x_tab == max(x_tab))))
  }
}