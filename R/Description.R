#' Create the header templete of the file
#' 
#' @param desc A character that is the description
#' @param symbol The symbol that is consists of the header
#' @param sym_len The width of the header
#' @param ... Another item
#' 
#' @return The header templete of the file
#' @examples Description()
#' @export
Description <- function(desc = "This is a test message.", symbol = "#", sym_len = 50, ...) {
  res <- paste(rep(symbol, sym_len), collapse = "")
  header <- paste(rep(symbol, 2), collapse = "")
  others <- eval(substitute(list(...)))
  items <- c(list(`Description` = desc), 
             others, 
             list(`Create Date` = as.character(Sys.Date()), `Last update` = as.character(Sys.Date())))
  cat(res, "\n")
  for(i in seq_along(items)) {
    cat(header, paste0(names(items[i]), ":\n"))
    cat(header, "  ", items[[i]], "\n", sep = "")
  }
  cat(res, "\n\n")
}