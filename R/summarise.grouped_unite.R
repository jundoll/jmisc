#' Undescribe
#' 
#' @param .data Undescribed
#' @param ... Undescribed
#' 
#' @return Undescribe
summarise <- function(.data, ...) {
  UseMethod("summarise")
}

summarise.default <- function(.data, ...) {
  summarise_(.data, .dots = lazyeval::lazy_dots(...))
}

summarise.grouped_df_unite <- function(.data, ...) {
  class(.data) <- class(.data)[class(.data) != 'grouped_df_unite']
  res <- summarise_(.data, .dots = lazyeval::lazy_dots(...))
  attr(res, 'group_by_unite.col_n') <- attr(.data, 'group_by_unite.col_n')
  attr(res, 'group_by_unite.origin_n') <- attr(.data, 'group_by_unite.origin_n')
  attr(res, 'group_by_unite.sep') <- attr(.data, 'group_by_unite.sep')
  return(res)
}