#' Reverse Geocoding by Google Maps API
#' 
#' @param Undescribed
#' 
#' @return Undescribed
#' @export
rGEO <- function(latlng, para = list(sensor = "false", language = "ja", region = "JP"), proxy = NULL) {
  curl_set <- RCurl::getCurlHandle()
  if(!is.null(proxy)) RCurl::curlSetOpt(.opts = list(proxy = proxy), curl = curl_set)
  parameters <- paste0("&", names(para), "=", unname(unlist(para)), collapse = "")
  GeoRequest <- iconv(paste0("http://maps.googleapis.com/maps/api/geocode/json?latlng=", latlng[, 1], ",", latlng[, 2], parameters), "", "UTF-8")
  geodata <- vector("list", nrow(latlng))
  for(i in 1:nrow(latlng)) {
    getJSON <- rjson::fromJSON(RCurl::getURL(URLencode(GeoRequest[i]), curl = curl_set))
    geodata[[i]] <- data.frame(address = getJSON$results[[1]]$formatted_address)
  }
  cbind(latlng, do.call("rbind", geodata))
}