#' Undescribe
#' 
#' @param Undescribe
#' 
#' @return Undescribe
#' @examples
#' iris %>% group_by_unite(Petal.Width, Species)
#' iris %>% group_by_unite(Petal.Width, Species, col_n = 'unite_name', sep = '|')
#' @export
group_by_unite <- function(data, ..., col_n, sep = "_", remove = TRUE) {
  .dots <- substitute(list(...))
  .dots_char <- sapply(.dots, as.character)[-1]
  if(missing(col_n)) col_n <- paste0(.dots_char, collapse = sep)
  if(length(.dots_char) == 1L) return(group_by_(data, col_n))
  res <- group_by_(unite_(data, col_n, .dots_char, sep = sep, remove = remove), col_n)
  attr(res, 'group_by_unite.col_n') <- col_n
  attr(res, 'group_by_unite.origin_n') <- .dots_char
  attr(res, 'group_by_unite.sep') <- sep
  if(inherits(res, 'tbl_dt')) {
    class(res) <- c('grouped_dt_unite', class(res))
  } else {
    class(res) <- c('grouped_df_unite', class(res))
  }
  return(res)
}