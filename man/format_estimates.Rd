% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format-estimates.R
\name{format_estimates}
\alias{format_estimates}
\title{Format Estimates}
\usage{
format_estimates(table, type = "total", moe = TRUE)
}
\arguments{
\item{table}{A data frame/tibble from `get_acs_recs()` for a single table and time period.}

\item{type}{A character, select either 'total' or 'share'.}

\item{moe}{A logical value, TRUE or FALSE to include or exclude margins of error.}
}
\value{
A data frame of ACS estimates by PSRC counties and region of either total estimates or proportions, with or without margins of
error.
}
\description{
Format estimates of a county summary table with the option to include shares and share margins of error.
}
\examples{
df <-get_acs_recs(geography = 'county',
                               table.names = c('B03002'),
                               years = c(2019),
                               acs.type = 'acs1')

format_estimates(df)
format_estimates(df, type = 'share', moe = FALSE)
}
\author{
Christy Lam
}
