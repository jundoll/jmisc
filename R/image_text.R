#' Undescribed
#' 
#' @param x Undescribed
#' @param x_labels Undescribed
#' @param y_labels Undescribed
#' @param in_labels Undescribed
#' @param ... Undescribed
#' @param mcex Undescribed
#' @param tcex Undescribed
#' 
#' @return Undescribed
#' @examples
#' image_text(iris[1:10, 1:4])
#' image_text(iris[1:10, 1:4], in_labels = TRUE)
#' @export
image_text <- function(x, x_labels = TRUE, y_labels = TRUE, in_labels = FALSE, ..., mcex = 1L, tcex = 1L){
  if(is.null(dim(x))) {
    stop('x must be matrix-like')
  }
  x <- as.matrix(x)
  at_v <- 1 / (2*(dim(x)[2]-1))
  at_v <- seq(0, 1+2*at_v, by = at_v) - at_v
  at_h <- 1 / (2*(dim(x)[1]-1))
  at_h <- seq(0, 1+2*at_h, by = at_h) - at_h
  image(t(x)[, dim(x)[1]:1], axes = FALSE, ...)
  if(x_labels) {
    if(is.null(colnames(x))) colnames(x) <- seq_len(ncol(x))
    mtext(text = colnames(x), side = 3, line = 0, at = at_v[seq(2, length(at_v), by = 2)], cex = mcex)
  }
  if(y_labels) {
    if(is.null(rownames(x))) rownames(x) <- seq_len(nrow(x))
    mtext(text = rownames(x), side = 2, line = 0, at = at_h[rev(seq(2, length(at_h), by = 2))], las = 1, cex = mcex)
  }
  if(in_labels) {
    xy <- as.matrix(expand.grid(x = at_v[seq(2, length(at_v), by = 2)], y = rev(at_h[seq(2, length(at_h), by = 2)])))
    text(xy[order(xy[, 1]), ], labels=x, cex = tcex)
    abline(v = at_v[seq(3, length(at_v)-1, by = 2)], h = at_h[seq(3, length(at_h)-1, by = 2)], col = 8)
  }
}