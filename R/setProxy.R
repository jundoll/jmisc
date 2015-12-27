#' Undescribed
#' 
#' @param Undescribed
#' 
#' @return Undescribed
#' 
#' @export
setProxy <- function(url = getOption('jmisc.proxy.url'), port = getOption('jmisc.proxy.port')) {
  if(url == "" | is.null(url)) stop("jmisc.proxy.url is unset.\nSetting `getOption('jmisc.proxy.url')`")
  if(port == "" | is.null(port)) stop("jmisc.proxy.port is unset.\nSetting `getOption('jmisc.proxy.port')`")
  httr::set_config(httr::use_proxy(url = url, port = as.integer(port)))
  cat('=== Proxy setting changed. ===\n')
}