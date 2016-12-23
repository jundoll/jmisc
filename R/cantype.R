#' Undescribe
#' 
#' @param x Undescribe
#' @param ... Undescribe
#' 
#' @return Undescribe
#' @examples
#' (dat_logi <- as.character(sample(c(TRUE, FALSE), 10, TRUE)))
#' cantype(dat_logi)
#' (dat_int <- as.character(sample(5, 10, TRUE)))
#' cantype(dat_int)
#' (dat_num <- as.character(rnorm(10)))
#' cantype(dat_num)
#' (dats <- data.frame(dat_logi, dat_int, dat_num))
#' cantype(dats)
#' @export
cantype <- function(x, ...) {
  if(is.list(x)) {
    for(i in 1:length(x)) {
      x[[i]] <- cantype(x[[i]])
    }
  } else if(is.logical(x) | is.integer(x) | is.numeric(x) | is.complex(x) | is.character(x)) {
    return(type.convert(as.character(x), as.is = TRUE, ...))
  } else if(is.factor(x)) {
    return(type.convert(as.character(x), as.is = FALSE, ...))
  } else {
    stop('Unknown error!! Please report the error status.')
  }
  x
}
