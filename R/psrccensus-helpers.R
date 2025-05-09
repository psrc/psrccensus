utils::globalVariables(c("J","NAME","census_geography","county","estimate","moe",
                       "value","acs_year","regex","name","label","concept"))

#' @importFrom magrittr %<>% %>%
#' @rawNamespace import(data.table, except = c(month, year))
NULL

#' Generate parameter list for all combinations
#'
#' Helper function for \code{\link{get_acs_recs}}; creates a list of parameter combinations for all tables and years
#' @param items Vector of table or data item names
#' @param years Vector of years
#' @param params Base parameters to include with each combination
#' @param item_name character string, i.e. "table" or "data_item"
#' @return List of parameter combinations
create_param_grid <- function(items, years, params, item_name = "item") {
  param_grid <- expand.grid(
    stringsAsFactors = FALSE,
    temp_item = items,
    year = years
  )
  names(param_grid)[1] <- item_name

  param_list <- lapply(1:nrow(param_grid), function(i) {
    new_params <- params
    new_params[[item_name]] <- param_grid[[item_name]][i]
    new_params[["year"]] <- param_grid$year[i]
    return(new_params)
  })

  return(param_list)
}

#' Format Census API return
#'
#' Helper function for \code{\link{get_acs_single}} and \code{\link{get_decennial_single}}
#' @param dt data.table with Census API result
#' @param geography character string, i.e. "block group", "tract", "place", "county", "msa", "puma" or "state"
#' @param census_type character string, i.e. "acs" or "decennial"
#' @return dt with fields formatted
format_cb_summary_tbl <- function(dt, geography, census_type) {
  geography_label <- geotype_lookup[J(geography), get("psrc_label")]
  delimiter <- "\\[,;\\s]+"
  cb_geo_order <- c("block group", "tract", "county", "state")
  pos <- match(geography, cb_geo_order) %>% dplyr::coalesce(3) %>% pmin(3)
  splitfields <- c("name", cb_geo_order[(pmin(pos, 3) + 1):4])

  # Separate NAME column - not currently done for decennial, but could be
  if(census_type=="acs"){
    dt[, (splitfields) := lapply(tstrsplit(NAME, delimiter), trimws)]
    dt[, NAME := NULL]
    setcolorder(dt, splitfields, after = 1)
    dt[, census_geography := (geography_label)]
    # Adjust state field for MSAs
    if (geography=="msa"){
      dt[, state := gsub(" Metro Area$", "", state, ignore.case = TRUE)]
    }
  }

  # Remove county field for subcounty geographies
  if (pos < 3 & "county" %in% colnames(dt)){
    dt[, county := NULL]
  }

  # Census-type specific operations
  if (census_type == "acs") {
    # ACS-specific formatting
    dt[is.na(estimate), estimate := 0]
    dt[is.na(moe), moe := 0]
  } else if (census_type == "decennial") {
    dt[is.na(value), value := 0]
  }
  return(dt)
}

#' Helper function to \code{\link{acs_varsearch}} and \code{\link{decennial_varsearch}}
#'
#' @param survey tidycensus code for target survey
#' @return data.table of filtered variable codes and attributes
pull_varlist <- function(survey){
  x <- tidycensus::load_variables(acs_year, survey) %>% setDT() %>%
    .[grepl(regex, name, ignore.case=TRUE)|
        grepl(regex, label, ignore.case=TRUE)|
        grepl(regex, concept, ignore.case=TRUE)] %>% unique()
  return(x)
}

#' Search published ACS variables
#'
#' Identify desired tables by examining prefix of relevant variable codes
#' Includes primary, subject and profile tables
#' @param regex search term
#' @param year optionally restrict search to a specific year
#' @return data.table of filtered variable codes and attributes
#' @author Michael Jensen
#'
#' @importFrom lubridate now month year
#' @export
acs_varsearch <- function(regex, year=NULL){
  name <- label <- concept <- NULL
  acs_year <- if(is.null(year)){year(now() - months(18))}else{year}
  acstypes <- paste0("acs5", c("","/subject","/profile","/cprofile")) %>%
    c("acsse", recursive=TRUE)
  rs <- lapply(acstypes, pull_varlist)
  rs <- rbindlist(rs, fill=TRUE)
  return(rs)
}

#' Search Decennial Census variables
#'
#' Includes primary, subject and profile tables
#' @param regex search term
#' @param year optionally restrict search to a specific year
#' @return data.table of filtered variable codes and attributes
#' @author Michael Jensen
#'
#' @importFrom lubridate now month year
#' @export
decennial_varsearch <- function(regex, year=NULL){
  name <- label <- concept <- NULL # Instantiate variables locally (for documentation, not function)

  find_decennial_year <- function(year=NULL){
    lag_year <- if(is.null(year)){year(now() - months(18))}else{year}
    dyear <- lag_year - lag_year %% 10
    if(dyear==2020){dyear <- 2010}else{dyear} # Due to pandemic disruption, 2010 is better reference than 2020
    return(dyear)
  }

  dyr <- find_decennial_year(year)
  decennial_types <- c("sf1", "sf2", "pl")
  rs <- lapply(decennial_types, pull_varlist) %>% rbindlist(fill=TRUE)
  return(rs)
}
