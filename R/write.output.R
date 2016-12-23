#' Return only first mode value
#' 
#' @param ... sequence of values
#' @param file Undescribed
#' @param append Undescribed
#' @param cout Undescribed
#' 
#' @return first mode value
#' @examples
#' write.output(head(CO2))
#' write.output(head(CO2), cout = '#out: ')
#' @export
write.output <- function(..., file = NULL, append = TRUE, cout = '## ') {
  closecon <- TRUE
  if(is.null(file)) {
    con <- stdout()
    closecon <- FALSE
  }
  else if(is.character(file)) {
    if(append) {
      con <- file(file, "a")
    }
    else {
      con <- file(file, "w")
    }
  }
  else if(inherits(file, "connection")) {
    con <- file
    if(!isOpen(con)) {
      if(append) {
        open(con, "a")
      }
      else {
        open(con, "w")
      }
    }
    else {
      closecon <- FALSE
    }
  }
  else {
    stop("file must be NULL or a character string or connection")
  }
  on.exit(if(closecon) close(con))
  out <- capture.output(...)
  for(i in out) {
    writeLines(paste(cout, i, sep = ''), con=con)
  }
  on.exit()
  if(closecon) {
    close(con)
  }
}