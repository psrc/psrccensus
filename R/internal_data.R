#' ACS table series lookup
#' "acs_tbltypes_lookup"
#'
library(magrittr)
library(dplyr)

get_tblnames <- function(surveytype){
  singleyear <- function(year, survey){
    x <- tidycensus::load_variables(year, survey) %>% dplyr::pull(name) %>%
      stringr::str_extract("^([^_]+)(?=_)") %>% na.omit() %>% unique()
    return(x)
  }
  survey <- if(grepl("^/", surveytype)){
    paste0(c("acs1","acs5"), surveytype)
  }else{
    paste0("acs", surveytype)
  }
  census_published_date <- lubridate::now() - lubridate::years(1)
  year_range <- c(2014:lubridate::year(census_published_date)) %>% .[. != 2020]
  inputlist <- expand.grid(year=year_range, survey=survey, stringsAsFactors=FALSE) %>%
    subset(year != 2014 | survey !="acs5/cprofile")
  if(lubridate::month(census_published_date) %in% c(1,9:12)){
    inputlist <- inputlist %>% filter(row_number() <= n()-1)
  }
  rs <- list()
  rs <- mapply(singleyear, inputlist$year, inputlist$survey) %>% unlist() %>% unique()
  return(rs)
}

acs_tbltypes_lookup <- list()
acs_tbltypes_lookup$subject  <- get_tblnames("/subject")
acs_tbltypes_lookup$profile  <- get_tblnames("/profile")
acs_tbltypes_lookup$cprofile <- get_tblnames("/cprofile")
acs_tbltypes_lookup$acsse    <- get_tblnames("se")

usethis::use_data(acs_tbltypes_lookup, internal=TRUE, overwrite=TRUE)
