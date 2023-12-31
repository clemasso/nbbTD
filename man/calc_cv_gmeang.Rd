% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enhancedDenton.R
\name{calc_cv_gmeang}
\alias{calc_cv_gmeang}
\title{Time series cross validation for the customized forecast method 'geometric mean of growth rates'}
\usage{
calc_cv_gmeang(s, h, mw.size = NA, min.origin = 5)
}
\arguments{
\item{s}{\code{"\link{ts}"} object}

\item{h}{forecast horizon}

\item{mw.size}{size of the moving window of growth rates (pm number of growth rates = number of periods - 1). If not defined, consider the maximum size given min.origin.}

\item{min.origin}{minimum number of periods considered to start cross-validation process}
}
\value{
cross-validation errors
}
\description{
The function calculates the geometric mean of the series growth rates as a proxy of the trend over
an horizon defined by the user. Then it computes cross validation errors for each period
(wrt the 'initial' argument)
}
