.onLoad <- function(libname, pkgname) {
  ### set options
  op <- options()
  op.jmisc <- list(
    jmisc.proxy.url = NULL,
    jmisc.proxy.port = NULL
  )
  toset <- !(names(op.jmisc) %in% names(op))
  if(any(toset)) options(op.jmisc[toset])

  invisible()
}