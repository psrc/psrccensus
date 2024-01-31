#' ACS table series lookup
#' "acs_tbltypes_lookup"
#'
library(magrittr)
library(dplyr)
library(lubridate)

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
  census_published_date <- lubridate::now() - years(1) - days(274)                      # Adjust for Census release lag
  year_range <- c(2014:year(census_published_date)) %>% .[. != 2020]                    # 2020 not published as standard year
  inputlist <- expand.grid(year=year_range, survey=survey, stringsAsFactors=FALSE) %>%  # Create all combos
    subset(year != 2014 | survey !="acs5/cprofile")                                     # This combo doesn't exist
  if(month(census_published_date) %in% c(1:4)){                                         # Between Sept and Jan, 5yr not yet published
    inputlist <- inputlist %>% filter(row_number() <= n()-1)
  }
  rs <- list()
  rs <- mapply(singleyear, inputlist$year, inputlist$survey) %>% unlist() %>% unique()  # Build the table
  return(rs)
}

acs_tbltypes_lookup <- list()
acs_tbltypes_lookup$subject  <- get_tblnames("/subject")
acs_tbltypes_lookup$profile  <- get_tblnames("/profile")
acs_tbltypes_lookup$cprofile <- get_tblnames("/cprofile")
acs_tbltypes_lookup$acsse    <- get_tblnames("se")

usethis::use_data(acs_tbltypes_lookup, internal=TRUE, overwrite=TRUE)                   # Makes part of the package; push this to repo
