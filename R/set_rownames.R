#' Undescribed
#' 
#' @param df Undescribed
#' @param var Undescribed
#' @param sep Undescribed
#' 
#' @return Undescribed
#' @examples
#' iris %>% set_rownames(Species) %>% head
#' iris %>% set_rownames('Species') %>% head
#' @export
set_rownames <- function(df, var, sep = '.') {
  stopifnot(is.data.frame(df))
  vars <- substitute(var)
  if(!is.character(vars)) {
    vars <- deparse(vars)
  }
  row_names <- as.character(df[[vars]])
  if(any(duplicated(row_names))) {
    elements <- unique(row_names)
    for(i in seq_along(elements)) {
      index <- which(row_names == elements[i])
      row_names[index] <- paste(elements[i], seq_along(index), sep = sep)
    }
  }
  rownames(df) <- row_names
  select_(df, .dots = setdiff(colnames(df), vars))
}