#' steps for internal acs table lookup
#'
#' @name acs_tbltypes_lookup
#' @docType data
#' @keywords data
NULL

# library(magrittr)
# library(dplyr)
# library(lubridate)
# library(data.table)
#
# get_tblnames <- function(surveytype){
#   name <- NULL
#
#   singleyear <- function(year, survey){
#     x <- tidycensus::load_variables(year, survey) %>% dplyr::pull(name) %>%
#       stringr::str_extract("^([^_]+)(?=_)") %>% stats::na.omit() %>% unique()
#     return(x)
#   }
#   survey <- if(grepl("^/", surveytype)){
#     paste0(c("acs1","acs5"), surveytype)
#   }else{
#     paste0("acs", surveytype)
#   }
#   census_published_date <- lubridate::now() - lubridate::years(1) - lubridate::days(274) # Adjust for Census release lag
#   year_range <- c(2014:year(census_published_date)) %>% .[. != 2020]                    # 2020 not published as standard year
#   inputlist <- expand.grid(year=year_range, survey=survey, stringsAsFactors=FALSE) %>%  # Create all combos
#     subset(year != 2014 | survey !="acs5/cprofile")                                     # This combo doesn't exist
#   if(month(census_published_date) %in% c(1:4)){                                         # Between Sept and Jan, 5yr not yet published
#     inputlist <- inputlist %>% filter(dplyr::row_number() <= dplyr::n()-1)
#   }
#   rs <- list()
#   rs <- mapply(singleyear, inputlist$year, inputlist$survey) %>% unlist() %>% unique()  # Build the table
#   return(rs)
# }
#
# acs_tbltypes_lookup <- list()
# acs_tbltypes_lookup$subject  <- get_tblnames("/subject")
# acs_tbltypes_lookup$profile  <- get_tblnames("/profile")
# acs_tbltypes_lookup$cprofile <- get_tblnames("/cprofile")
# acs_tbltypes_lookup$acsse    <- get_tblnames("se")
#
# geotype_lookup <- data.table(
#   geo=c("block group", "tract", "place", "county", "msa", "puma", "state"),
#   psrc_label=c("block group", "Tract", "Place", "County", "MSA", "public use microdata area (PUMA)", "State"),
#   api_label=c("block group", "tract", "place", "county", "cbsa", "public use microdata area", "state")
# )
# setkey(geotype_lookup, geo)
#
# pums_labels_xtra <- psrccensus:::pums_labels_xtra
# usethis::use_data(acs_tbltypes_lookup, pums_labels_xtra, geotype_lookup, internal=TRUE, overwrite=TRUE) # Makes part of the package; push this to repo
