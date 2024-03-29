% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-psrc-pums.R
\name{codes2labels}
\alias{codes2labels}
\title{Swap variable codes for labels}
\usage{
codes2labels(dt, dyear, vars)
}
\arguments{
\item{dt}{PSRC data.table}

\item{dyear}{The data year}

\item{vars}{PUMS variable/s as an UPPERCASE string element or list}
}
\value{
PSRC data.table with labels
}
\description{
Helper to \code{\link{get_psrc_pums}} function.
Delivers labels instead of values, where labels exist
}
\author{
Michael Jensen
}
