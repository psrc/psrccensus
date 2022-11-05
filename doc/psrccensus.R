## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----keys, results='hide'-----------------------------------------------------
library(psrccensus)
#Sys.setenv(CENSUS_API_KEY = 'PUT YOUR KEY HERE')
Sys.getenv("CENSUS_API_KEY")

## ----keys2, eval=FALSE, results='hide'----------------------------------------
#  # first time, run once in console. Then restart R
#  tidycensus::census_api_key('your api key', install = TRUE)

