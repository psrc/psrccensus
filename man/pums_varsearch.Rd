% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-psrc-pums.R
\name{pums_varsearch}
\alias{pums_varsearch}
\title{Search PUMS variable definitions}
\usage{
pums_varsearch(regex)
}
\arguments{
\item{regex}{search term}
}
\description{
Look for a var_code via search term
}
\examples{
# All variables related to internet; shows code changed in 2020 survey
pums_varsearch("internet")

# Entry for specific variable, 'Employment Status Recode'
pums_varsearch("^ESR$")

}
\author{
Michael Jensen
}
