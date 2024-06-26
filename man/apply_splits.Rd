% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psrccensus_geographies.R
\name{apply_splits}
\alias{apply_splits}
\title{Internal helper to apply splits in `use_geography_splits` or `census_to_customgeo`}
\usage{
apply_splits(df, rosetta)
}
\arguments{
\item{df}{acs or decennial dataset returned from psrccensus}

\item{rosetta}{splits file either from Elmer TVF or calculated from parcel data}
}
\value{
dataframe summarized to planning geography
}
\description{
Internal helper to apply splits in `use_geography_splits` or `census_to_customgeo`
}
\author{
Michael Jensen
}
