% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pm.R
\name{pm}
\alias{pm}
\title{plus minus}
\usage{
pm(x, y, cont = FALSE)
}
\arguments{
\item{x}{A integer of length 1}

\item{y}{A integer of length 1}

\item{cont}{If TRUE, return \code{x+y} and \code{x-y}. If FALSE, return a vector from \code{x-y} to \code{x+y}.}
}
\description{
plus minus
}
\examples{
pm(4, 5)
pm(4, 5, cont = TRUE)
}

