% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rep_row.R
\name{rep_row}
\alias{rep_row}
\title{Undescribed}
\usage{
rep_row(df, value)
}
\arguments{
\item{df}{Undescribed}

\item{value}{Undescribed}
}
\value{
Undescribed
}
\description{
Undescribed
}
\examples{
iris \%>\% slice(1:3) \%>\% mutate(rep_num = as.integer(Sepal.Length)) \%>\% rep_row(rep_num)
}

