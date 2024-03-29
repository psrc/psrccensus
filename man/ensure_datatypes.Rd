% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-psrc-pums.R
\name{ensure_datatypes}
\alias{ensure_datatypes}
\title{Confirm correct datatypes for group variables and weights}
\usage{
ensure_datatypes(dt)
}
\arguments{
\item{dt}{PSRC data.table}
}
\value{
PSRC data.table with types confirmed
}
\description{
Helper to \code{\link{get_psrc_pums}} function.
Makes certain weights are numeric and grouping variables are factors
}
\author{
Michael Jensen
}
