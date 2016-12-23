#' Undescribed
#' 
#' @param df Undescribed
#' @param value Undescribed
#' 
#' @return Undescribed
#' @examples
#' iris %>% slice(1:3) %>% mutate(rep_num = as.integer(Sepal.Length)) %>% rep_row(rep_num)
#' @export
rep_row <- function(df, value) {
  indice <- rep.int(1:nrow(df), df[[deparse(substitute(value))]])
  dplyr::slice(df, indice)
}
# rep_row_も作成
if(0){
  values <- substitute(value)
  df %>>%
    mutate(1:n()) %>>%
    slice_(.dots = deparse(substitute(rep.int(`1:n()`, value), list(value = values)))) %>>%
    select(-`1:n()`)
}