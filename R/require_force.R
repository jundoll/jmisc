#' Undescribed
#' 
#' @param Undescribed
#' 
#' @return Undescribed
#' @examples
#' require_force(dplyr)
#' @export
require_force <- function(package, ..., load = TRUE) {
  packages <- substitute(package)
  if(!is.character(packages)) {
    packages <- deparse(packages)
  } else {
    package <- as.symbol(packages)
  }
  if(!packages %in% installed.packages()[, 'Package']) {
    if(readline(paste0('May I do install.packages("', packages, '")? [y/n] ')) %in% c('y', 'Y', 'yes', 'Yes', 'YES')) {
      install.packages(packages, ...)
    }
  } 
  if(load) {
    require(packages, character.only = TRUE)
  }
}
