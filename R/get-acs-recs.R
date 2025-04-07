#' @importFrom magrittr %<>% %>%
#' @rawNamespace import(data.table, except = c(month, year))
NULL

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

  pull_varlist <- function(survey){
    x <- tidycensus::load_variables(acs_year, survey) %>% setDT() %>%
      .[grepl(regex, name, ignore.case=TRUE)|
          grepl(regex, label, ignore.case=TRUE)|
          grepl(regex, concept, ignore.case=TRUE)] %>% unique()
    return(x)
  }
  acstypes <- paste0("acs5", c("","/subject","/profile","/cprofile")) %>%
    c("acsse", recursive=TRUE)
  rs <- lapply(acstypes, pull_varlist)
  rs <- rbindlist(rs, fill=TRUE)
  return(rs)
}

#' Add shares to Psrccensus ACS object
#'
#' @param df dataframe with Census ACS result
#' @return dataframe with additional share and share_moe fields
#' @author Michael Jensen
#' @export
add_acs_share <- function(df){
  label <- x.estimate <- i.estimate <- x.moe <- i.moe <- concept <- share <- share_moe <- NULL
  rs <- setDT(df)
  tots <- copy(rs) %>% .[grepl("Total:$",label)]
  rs <- rs[tots, `:=`(share=x.estimate/i.estimate,
                      share_moe=tidycensus::moe_prop(x.estimate, i.estimate, x.moe, i.moe)),
           on=.(GEOID, concept, year)]
  if("data.table" %not_in% class(df)){
    rs <- setDF(rs)
  }
  return(rs)
}

#' Add estimate reliability metrics
#'
#' Add coefficient of variation (cv) and relative reliability rating to estimates
#' Census ACS uses 90 percent confidence interval, i.e. z score 1.645
#' http://aws-linux/mediawiki/index.php/Understanding_Error_and_Determining_Statistical_Significance
#' @param df dataframe or data.table already retrieved from the api, or created by a user
#' @param moe character name of the column that has contains margin of error estimates
#' @param estimate character name of the column that has contains data value estimates
#' @author Suzanne Childress
#' @return the data frames with new columns for se, cv, and reliability
#' @export
reliability_calcs <- function(df, moe = "moe", estimate = "estimate") {
  is_data_table <- "data.table" %in% class(df)
  # Make sure input is a data.table
  dt <- copy(df) %>% setDT()

  # Calculate coefficient of variation and reliability measures by reference
  dt[, `:=`(
    cv = ifelse(get(..estimate) > 0, get(..moe)/1.645/get(..estimate), NA_real_),
    reliability = NA_character_
  )]

  # Set reliability categories based on CV values
  dt[cv <= 0.12, reliability := "High"]
  dt[cv > 0.12 & cv <= 0.40, reliability := "Medium"]
  dt[cv > 0.40, reliability := "Low"]
  dt[is.na(cv) & estimate == 0, reliability := "N/A"]

  if(!is_data_table){
    dt <- setDF(dt)
  }

  return(dt)
}

#' Label ACS variables
#'
#' Helper function for \code{\link{get_acs_single}} to provide variable labels and concept--i.e. topic--alongside codes
#' @param dt data.table with Census API result
#' @param table Census table code
#' @param year the year--or last year--of the ACS survey
#' @param acs.type either acs1 or acs5
#'
#' @return dataframe with labels appended
label_acs_variables <- function(dt, table, year, acs.type){
  name <- label <- concept <- NULL # instantiate variables
  survey <- if(table %in% acs_tbltypes_lookup$subject){
    paste0(acs.type,"/subject")
  }else if(table %in% acs_tbltypes_lookup$profile){
    paste0(acs.type,"/profile")
  }else if(table %in% acs_tbltypes_lookup$cprofile){
    paste0(acs.type,"/cprofile")
  }else if(table %in% acs_tbltypes_lookup$acsse){
    "acsse"
  }else{
    acs.type
  }
  labels <- tidycensus::load_variables(year=year, dataset=survey) %>%
    setDT()
  labels <- labels[, .(variable = name, label, concept)] %>% setkey("variable")
  dt <- merge(dt, labels, by = c("variable"), all.x = TRUE)

  return(dt)
}

#' Generate parameter list for all combinations
#'
#' Helper function for \code{\link{get_acs_recs}}; creates a list of parameter combinations for all tables and years
#' @param table.names Vector of table names
#' @param years Vector of years
#' @param params Base parameters to include with each combination
#' @return List of parameter combinations
create_param_grid <- function(table.names, years, params) {
  # Create all combinations of tables and years
  param_grid <- expand.grid(
    table = table.names,
    year = years,
    stringsAsFactors = FALSE
  )

  # Convert to list of parameter sets
  param_list <- lapply(1:nrow(param_grid), function(i) {
    c(list(
      table = param_grid$table[i],
      year = param_grid$year[i]
    ), params)
  })

  return(param_list)
}

#' Add regional totals
#'
#' Helper function for \code{\link{get_acs_single}} to add regional totals to county estimates
#' @param dt data.table with Census API result
#' @return dt with regional totals
add_regional_acs <- function(dt){
  # Aggregate by variable
  total_dt <- dt[, .(
    sumest = sum(estimate),
    summoe = tidycensus::moe_sum(moe, estimate)
  ), by = .(variable, state)]
  setnames(total_dt, c("sumest", "summoe"), c("estimate", "moe"))
  total_dt[, `:=`(GEOID = "REGION", name = "Region", census_geography = "Region")]

  # Combine with original data
  dtx <- rbindlist(list(dt, total_dt), use.names = TRUE, fill = TRUE)

  return(dtx)
}

#' Format ACS API return
#'
#' Helper function for \code{\link{get_acs_single}}
#' @param dt data.table with Census API result
#' @param geography_label one of:
#' \itemize{
#' \item method1 - "block group"
#' \item method2 - "Tract"
#' \item method3 - "Place"
#' \item method4 - "County"
#' \item method5 - "MSA"
#' \item method6 - "public use microdata area (PUMA)"
#' \item method7 - "State"
#' }
#' @return dt with fields formatted
format_acs <- function(dt, geography_label) {
  delimiter <- if(grepl("PUMA", geography_label)){"\\, "}else{"\\; "}
  cb_geo_order <- c("block group", "tract", "county", "state")
  pos <- match(tolower(geography_label), cb_geo_order) %>% dplyr::coalesce(3) %>% pmin(3)
  splitfields <- c("name", cb_geo_order[(pmin(pos, 3) + 1):4])

  # Separate NAME column
  dt[, (splitfields) := lapply(tstrsplit(NAME, delimiter), trimws)]
  dt[, NAME := NULL]
  setcolorder(dt, splitfields, after = 1)
  dt[, census_geography := geography_label]
  # Remove county field for subcounty geographies
  if (pos < 3 & "county" %in% colnames(dt)){
    dt[, county := NULL]
  }

  # Replace missing estimate and moe values with zero
  dt[is.na(estimate), estimate := 0]
  dt[is.na(moe), moe := 0]

  return(dt)
}


#' Get single ACS table
#'
#' Core function to get ACS data for any geography
#' @param params list of parameter combinations from \code{\link{create_param_grid}}
#' @return dt formatted ACS data for a single geography and survey
get_acs_single <- function(params) {
  # Extract parameters
  table <- params$table
  year <- params$year
  geography <- params$geography
  state <- params$state
  counties <- params$counties
  FIPS <- params$FIPS
  place_FIPS <- params$place_FIPS
  acs.type <- params$acs.type

  dt <- tryCatch({
    # Geography-specific parameters and processing
    if (geography == "county") {
      # County-specific data retrieval
      dt <- tidycensus::get_acs(
        state = state,
        geography = 'county',
        county = counties,
        year = year,
        survey = acs.type,
        table = table) %>% setDT() %>%
        psrccensus:::format_acs("County")

      # Region total for PSRC counties
      if (identical(counties, c("King", "Kitsap", "Pierce", "Snohomish"))) {
        dt <- add_regional_acs(dt)
      }

    } else if (geography == "msa") {
      # MSA-specific data retrieval
      dt <- tidycensus::get_acs(
        geography = "metropolitan statistical area/micropolitan statistical area",
        year = year,
        survey = acs.type,
        table = table) %>% setDT()

      dt <- dt[GEOID %in% FIPS] %>%
        format_acs("MSA")

    } else if (geography == "place") {
      # Place-specific data retrieval
      dt <- tidycensus::get_acs(
        state = state,
        geography = 'place',
        year = year,
        survey = acs.type,
        table = table) %>% setDT()

      # Filter for requested FIPS or PSRC places if unspecified
      if (is.null(place_FIPS)) {
        psrc_places <- get_psrc_places(year) %>% sf::st_drop_geometry()
        place_FIPS <- unique(psrc_places$GEOID)
      }
      dt <- dt[GEOID %in% place_FIPS] %>%
        format_acs("Place")

      # Update census_geography based on name suffix
      dt[endsWith(name, "city"), census_geography := "City"]
      dt[endsWith(name, "CDP"), census_geography := "CDP"]

      # Remove suffixes from name by reference
      dt[, name := gsub(" (city|CDP)", "", name)]

    } else if (geography == "tract") {
      # Tract-specific data retrieval
      dt <- tidycensus::get_acs(
        state = state,
        county = counties,
        geography = 'tract',
        year = year,
        survey = "acs5",
        table = table) %>% setDT() %>%
        psrccensus:::format_acs("Tract")

    } else if (geography == "block group") {
      # Block group-specific data retrieval
      dt <- tidycensus::get_acs(
        state = state,
        county = counties,
        geography = "block group",
        year = year,
        survey = "acs5",
        table = table) %>% setDT() %>%
        psrccensus:::format_acs("block group")

    } else if (geography == "puma") {
      # PUMA-specific data retrieval
      dt <- tidycensus::get_acs(
        state = state,
        geography = "public use microdata area",
        year = year,
        table = table) %>% setDT() %>%
        format_acs("public use microdata area (PUMA)")

      dt[, name := gsub("[\\;|\\,]\\s*$", "", name)]

    } else if (geography == "state") {
      # State-specific data retrieval
      dt <- tidycensus::get_acs(
        geography = "state",
        state = state,
        year = year,
        survey = acs.type,
        table = table) %>% setDT() %>% format_acs("State")
    }
  },
  error = function(e) {
    warning(paste("Error retrieving data for table:", table,
                  "year:", year, "geography:", geography,
                  "- Error:", e$message))
    return(NULL)  # Return NULL for failed requests
  })

  # Add labels
  dt <- label_acs_variables(dt, table, year, acs.type)

  # Add metadata
  dt[, `:=`(acs_type = acs.type, year = year)]

  # Filter out any regional medians/averages
  dt <- dt[!(name == "Region" & (grepl("Median", label) | grepl("Average", label)))]

  return(dt)
}

#' ACS Estimates
#'
#' Generate ACS estimates for multiple tables by tracts, counties, MSAs, or places
#' for multiple years using data.table for improved performance.
#'
#' @param state A character string state name or abbreviation. Defaults to Washington.
#' @param geography A character string as either 'tract', 'county', 'msa', 'place', 'block group', 'puma', or 'state'.
#' @param counties A character string or vector of counties. Defaults to PSRC counties.
#' @param table.names A character string or vector of Census table codes.
#' @param years A numeric value or vector of years. An ACS year equal or greater than 2010 to the latest available year.
#' @param FIPS Character string for FIPS codes for specific MSA geographies. Defaults to Seattle & Bremerton MSA c("14740","42660")
#' @param place_FIPS Character string of FIPS codes (with state prefix) for specific Census Places. If NULL, Places within the PSRC Region will be returned.
#' @param acs.type A character string as either 'acs1', 'acs3' or acs5'.
#'
#' @author Craig Helmann
#'
#' @return a data frame of ACS estimates by selected geography for selected table codes. Includes variable names.
#' @export
get_acs_recs <- function(geography,
                         state="Washington",
                         counties = c('King', 'Kitsap', 'Pierce', 'Snohomish'),
                         table.names,
                         years,
                         FIPS = c("14740","42660"),
                         place_FIPS=NULL,
                         acs.type) {

  # Validates geography parameter
  valid_geographies <- c('county', 'msa', 'place', 'tract', 'block group', 'puma', 'state')
  if (!(geography %in% valid_geographies)) {
    stop(paste("Invalid geography. Must be one of:", paste(valid_geographies, collapse=", ")))
  }

  # Force acs5 for certain geographies
  if (geography %in% c('tract', 'block group', 'puma') && acs.type != 'acs5') {
    warning(paste0("For ", geography, " geography, only acs5 is supported. Switching to acs5."))
    acs.type <- 'acs5'
  }

  # Create base parameter list
  base_params <- list(
    geography = geography,
    state = state,
    counties = counties,
    FIPS = FIPS,
    place_FIPS = place_FIPS,
    acs.type = acs.type
  )

  # Generate parameter grid for all combinations
  param_grid <- create_param_grid(table.names, years, base_params)

  # Process all table/year combinations
  rs <- lapply(param_grid, get_acs_single)

  if (length(rs) == 0) {
    sad <- data.frame()
    return(sad)
  } else {

    # Filter out NULLs
    rs <- rs[!sapply(rs, is.null)]

    # Combine tables/years
    rs <- rbindlist(rs, fill = TRUE)

    # Calculate reliability
    rs <- reliability_calcs(rs)

    rs <- setDF(rs)

    gc(verbose = FALSE)

    return(rs)
  }
}
