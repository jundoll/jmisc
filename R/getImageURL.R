#' Get Image's URL by Google Ajax API
#' 
#' @param Undescribed
#' 
#' @return Undescribed
#' @examples
#' # getImageURL(word = 'Paris Hilton', v = '1.0', rsz = 1, start = 1)
#' @export
getImageURL <- function(word, ...) {
  words <- gsub(' ', '%20', word)
  param0 <- list(...)
  param <- paste0('&', names(param0), '=', unname(unlist(param0)), collapse = '')
  query  <- paste0('http://ajax.googleapis.com/ajax/services/search/images?q=', words, param)
  image_url <- sapply(rjson::fromJSON(file = query)$responseData$results, function(x) x$url)
  return(list(urls = image_url, query = query, search = word))
}


