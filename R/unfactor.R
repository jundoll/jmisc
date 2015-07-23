#' Undescribe
#' 
#' @param Undescribe
#' 
#' @return Undescribe
#' @examples
#' unfactor(iris$Species)
#' @export
unfactor <- function(x) {
  cantype(as.character(x))
}