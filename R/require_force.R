#' Load/Attach (and Install) the package
#' 
#' @param pkgs packages
#' @param ... option for `install.packages`
#' @param load `load=1` is use `require`, `load=2` is use `library` 
#' 
#' @return Undescribed
#' @examples
#' require_force(dplyr)
#' require_force('dplyr')
#' @export
require_force <- function(pkgs, ..., load = 1L) {
  packages <- substitute(pkgs)
  if(!is.character(packages)) {
    if(length(packages) == 1L) {
      packages <- deparse(packages)
    } else {
      packages <- pkgs
    }
  }
  for(pkg in packages) {
    if(!pkg %in% installed.packages()[, 'Package']) {
      if(readline(paste0('May I do install.packages("', pkg, '")? [y/n] --> ')) %in% c('y', 'Y', 'yes', 'Yes', 'YES')) {
        cat(paste('Installing required package:', pkg, '\n'))
        install.packages(pkg, ...)
      } else {
        cat(paste('The required package already installed:', pkg, '\n'))
      }
    }
    if(load == 1L) {
      require(packages, character.only = TRUE)
    } else if(load == 2L) {
      library(packages, character.only = TRUE)
    }
  }
}
