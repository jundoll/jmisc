#' Geocoding by Google Maps API
#' 
#' @param Undescribed
#' 
#' @return Undescribed
#' @export
GEO <- function(address, para = list(sensor = "false", language = "ja", region = "JP"), proxy = NULL) {
  curl_set <- RCurl::getCurlHandle()
  if(!is.null(proxy)) RCurl::curlSetOpt(.opts = list(proxy = proxy), curl = curl_set)
  parameters <- paste0("&", names(para), "=", unname(unlist(para)), collapse = "")
  GeoRequest <- iconv(paste0("http://maps.googleapis.com/maps/api/geocode/json?address=", address, parameters), "", "UTF-8")
  geodata <- vector("list", length(address))
  for(i in seq_along(address)) {
    getJSON <- rjson::fromJSON(RCurl::getURL(URLencode(GeoRequest[i]), curl = curl_set))
    geodata[[i]] <- as.data.frame(getJSON$results[[1]]$geometry$location)
  }
  cbind(address, do.call("rbind", geodata))
}