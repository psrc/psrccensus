% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-decennial-recs.R
\name{dicennial_varsearch}
\alias{dicennial_varsearch}
\title{Search Dicennial Census variables}
\usage{
dicennial_varsearch(regex, year = NULL)
}
\arguments{
\item{regex}{search term}

\item{year}{optionally restrict search to a specific year}
}
\value{
data.table of filtered variable codes and attributes
}
\description{
Includes primary, subject and profile tables
}
\examples{
# Nursing home or medical group quarters
z <- dicennial_varsearch("^Total!!Institutionalized .*(nursing|hospital|hospice)")

# All variables from table POO1
z <- dicennial_varsearch("^P001")

}
\author{
Michael Jensen
}
