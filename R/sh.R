#' Undescribe
#' 
#' @param Undescribe
#' 
#' @return Undescribe
sh <- function(path, command) {
  if(missing(path) & missing(command)) {
    stop('no query!')
  }
  if(!missing(path) & !file.exists(path)) {
    stop('no file!')
  }
  commands <- vector("list", 0)
  commands$command <- if(!missing(path)) paste('cat', path) else command
  class(commands) <- "sh"
  commands
}

`%|%` <- function(a, b) {
  b2 <- as.list(substitute(b))
  f <- eval(b2[[1]])
  do.call(f,  c(list(a), b2[-1]))
}

print.sh <- function(x) print(x$command)

command <- function(sh, command) {
  sh$command <- c(sh$command, command)
  sh
}

fread <- function(input, ...) UseMethod('fread')
fread.default <- function(...) data.table::fread(...)
fread.sh <- function(input, ...) {
  fread(paste0(input$command, collapse = " | "), ...)
}







