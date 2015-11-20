#' Undescribe
#' 
#' @param Undescribe
#' 
#' @return Undescribe
#' @examples
#' iris %>% group_by_unite(Petal.Width, Species) %>% summarise(count = n()) %>% ununite
#' iris %>% group_by_unite(Petal.Width, Species, col_n = 'unite_name', sep = '|') %>% summarise(count = n()) %>% ununite
#' @export
ununite <- function(data, ...) {
  col_n <- attr(data, 'group_by_unite.col_n')
  origin_n <- attr(data, 'group_by_unite.origin_n')
  sep <- attr(data, 'group_by_unite.sep')
  if(is.null(col_n) | is.null(origin_n) | is.null(sep)) {
    return(data)
  } else {
    readr::type_convert(tidyr::separate_(data = data, col = col_n, into = origin_n, sep = paste0('[', sep, ']'), ...))
  }
}