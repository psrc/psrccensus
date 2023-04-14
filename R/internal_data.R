#' ACS table series lookup
#' "acs_tbltypes_lookup"
#'
#' library(magrittr)
#'
#' get_tblnames <- function(surveytype){
#'   singleyear <- function(year, survey){
#'     x <- tidycensus::load_variables(year, survey) %>% dplyr::pull(name) %>%
#'       stringr::str_extract("^([^_]+)(?=_)") %>% na.omit() %>% unique()
#'     return(x)
#'   }
#'   survey <- if(grepl("^/", surveytype)){
#'     paste0(c("acs1","acs5"), surveytype)
#'   }else{
#'     "acs"
#'   }
#'   inputlist <- expand.grid(year=c(2014:2019,2021), survey=survey, stringsAsFactors=FALSE)
#'   rs <- list()
#'   rs <- mapply(singleyear, inputlist$year, inputlist$survey) %>% unlist() %>% unique()
#'   return(rs)
#' }
#'
#' acs_tbltypes_lookup <- list()
#' acs_tbltypes_lookup$subject  <- get_tblnames("/subject")
#' acs_tbltypes_lookup$profile  <- get_tblnames("/profile")
#' acs_tbltypes_lookup$cprofile <- get_tblnames("/cprofile")
#' acs_tbltypes_lookup$acsse    <- get_tblnames("se")
#'
#' usethis::use_data(acs_tbltypes_lookup, internal=TRUE, overwrite=TRUE)
