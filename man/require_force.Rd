% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/require_force.R
\name{require_force}
\alias{require_force}
\title{Load/Attach (and Install) the package}
\usage{
require_force(pkgs, ..., load = 1L)
}
\arguments{
\item{pkgs}{packages}

\item{...}{option for `install.packages`}

\item{load}{`load=1` is use `require`, `load=2` is use `library`}
}
\value{
Undescribed
}
\description{
Load/Attach (and Install) the package
}
\examples{
require_force(dplyr)
require_force('dplyr')
}

