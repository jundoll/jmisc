#' Return the Short Form to the Long Form of the type of an objects
#' 
#' @param x A character that is the acronym of the type of an objects
#' 
#' @return A character vector of the Long Forms
#' @examples 
#' colC("dicfidl")
#' @export
colC <- function(x) {
  replaces(strsplit(x, "")[[1]], c("l", "i", "d", "c", "D", "f"), c("logical", "integer", "numeric", "character", "Date", "factor"))
}