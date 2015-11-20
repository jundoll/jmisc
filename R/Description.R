#' Create the header templete of the file
#' 
#' @param desc A character that is the description
#' @param symbol The symbol that is consists of the header
#' @param sharp The width of the header
#' 
#' @return The header templete of the file
#' @examples Description()
#' @export
Description <- function(desc = "This is a test message.", symbol = "#", sharp = 50) {
  res <- paste(rep(symbol, sharp), collapse = "")
  header <- paste(rep(symbol, 2), collapse = "")
  cat(res, "\n")
  cat(header, "Description:\n")
  cat(header, "  ", desc, "\n", sep = "")
  cat(header, "Create Date:\n")
  cat(header, "  ", as.character(Sys.Date()), "\n", sep = "")
  cat(header, "Last update:\n")
  cat(header, "  ", as.character(Sys.Date()), "\n", sep = "")
  cat(res, "\n\n")
}