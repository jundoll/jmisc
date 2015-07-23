#' Undescribed
#' 
#' @param Undescribed
#' 
#' @return Undescribed
#' 
#' @export
setProxy <- function(url = Sys.getenv('PROXY_URL'), port = Sys.getenv('PROXY_PORT')) {
  if(url == "") stop("PROXY_URL is unset.\nSetting `Sys.setenv('PROXY_URL')`")
  if(port == "") stop("PROXY_PORT is unset.\nSetting `Sys.setenv('PROXY_PORT')`")
  httr::set_config(httr::use_proxy(url = url, port = as.integer(port)))
  cat('=== Proxy setting changed. ===\n')
}