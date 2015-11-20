#' Undescribe
#' 
#' @param Undescribe
#' 
#' @return Undescribe
print.grouped_df_unite <- function(.data) {
  class(.data) <- class(.data)[class(.data) != 'grouped_df_unite']
  print(.data)
}

print.grouped_dt_unite <- function(.data) {
  class(.data) <- class(.data)[class(.data) != 'grouped_dt_unite']
  print(.data)
}