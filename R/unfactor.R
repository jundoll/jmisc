#' Undescribe
#' 
#' @param x Undescribed
#' @param ... Undescribed
#' 
#' @return Undescribe
#' @examples
#' unfactor(iris$Species)
#' @export
unfactor <- function(x, ...) {
  if(!is.factor(x)) return(x)
  type.convert(as.character(x), as.is = TRUE, ...)
}