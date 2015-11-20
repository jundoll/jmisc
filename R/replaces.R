#' Undescribe
#' 
#' @param Undescribe
#' 
#' @return Undescribe
#' @examples replaces(1:10, 1:3, 3:5)
#' @export
replaces <- function(x, from, to, last = FALSE, evaluate = TRUE) {
  N <- min(length(from), length(to))
  gen_ifelse <- function(i = 1L) {
    if(i == N) {
      if(is.logical(last) & !last) {
        as.call(as.list(substitute(ifelse(x == from[i], to[i], x), list(i = N))))
      } else {
        as.call(as.list(substitute(ifelse(x == from[i], to[i], last), list(i = N))))
      }
    } else {
      as.call(as.list(substitute(ifelse(x == from[i], to[i], nest), list(i = i, nest = gen_ifelse(i+1L)))))
    }
  }
  if(evaluate) {
    eval(gen_ifelse())
  } else {
    gen_ifelse()
  }
}