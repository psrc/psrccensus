% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psrc-pums-stat.R
\name{pce_deflator}
\alias{pce_deflator}
\title{Retrieve annual Personal Consumption Expenditures deflator ratio}
\usage{
pce_deflator(have_yr, want_yr)
}
\arguments{
\item{have_yr}{existing dollar year basis}

\item{want_yr}{dollar year to which you want the basis shifted}
}
\description{
Applies PCE deflator to yield real values in specified dollar-year terms
Helpful when comparing between separate surveys
Requires [St. Louis Federal Reserve (FRED) API key](http://sboysel.github.io/fredr/articles/fredr.html#authentication)
}
\author{
Michael Jensen
}
