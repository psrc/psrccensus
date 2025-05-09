utils::globalVariables(c("GEOID","estimate","moe","variable","state","cv","reliability","estimate","moe"))

#' @importFrom magrittr %<>% %>%
#' @rawNamespace import(data.table, except = c(month, year))
NULL

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
  dt <- setDT(dt) %>% setkey("variable") %>% merge(labels, all.x = TRUE) %>%
    setcolorder("GEOID")

  return(dt)
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
  ), by = c("variable")]
  setnames(total_dt, c("sumest", "summoe"), c("estimate", "moe"))
  total_dt[, `:=`(GEOID = "REGION", name = "Region", census_geography = "Region")]

  # Combine with original data
  dtx <- rbindlist(list(dt, total_dt), use.names = TRUE, fill = TRUE)

  return(dtx)
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
  api_geo <- geotype_lookup[J(geography), get("api_label")]

  tryCatch({
    # Geography-specific parameters and processing
    if (geography %in% c("county", "tract", "block group")) {
      dt <- tidycensus::get_acs(
        state = state,
        geography = api_geo,
        county = counties,
        year = year,
        survey = acs.type,
        table = table) %>% setDT() %>%
        format_cb_summary_tbl(geography, census_type = "acs")

      # Region total for PSRC counties
      if(geography == "county" &
         identical(unique(dt$GEOID), c('53033', '53035', '53053', '53061'))) {
        dt <- add_regional_acs(dt)
      }

    } else if (geography == "msa") {
      dt <- tidycensus::get_acs(
        geography = api_geo,
        year = year,
        survey = acs.type,
        table = table) %>% setDT()

      dt <- dt[GEOID %in% FIPS] %>%
        format_cb_summary_tbl(geography, census_type = "acs")

    } else if (geography %in% c("place", "puma", "state")) {
      dt <- tidycensus::get_acs(
        state = state,
        geography = api_geo,
        year = year,
        survey = acs.type,
        table = table) %>% setDT()

      if (geography == "place"){
        # Filter for requested FIPS or PSRC places if unspecified
        if (is.null(place_FIPS)) {
          psrc_places <- get_psrc_places(year) %>% sf::st_drop_geometry()
          place_FIPS <- unique(psrc_places$GEOID)
        }
        dt <- dt[GEOID %in% place_FIPS] %>%
          format_cb_summary_tbl(geography, census_type = "acs")

        # Update census_geography based on name suffix
        dt[endsWith(name, "city"), census_geography := "City"]
        dt[endsWith(name, "CDP"), census_geography := "CDP"]

        # Remove suffixes from name by reference
        dt[, name := gsub(" (city|CDP)", "", name)]

      }else if (geography == "puma") {
        dt[, name := gsub("[\\;|\\,]\\s*$", "", name)]
      }
    }
  },
  error = function(e) {
    warning(paste("Error retrieving data for table:", table,
                  "year:", year, "geography:", geography,
                  "- Error:", e$message))
    return(NULL)  # Return NULL for failed requests
  })

  if (!is.null(dt)) {
    # Add labels
    dt <- label_acs_variables(dt, table, year, acs.type)

    # Add metadata
    dt[, `:=`(acs_type = acs.type, year = year)]

    # Filter out any regional medians/averages
    dt <- dt[!(name == "Region" & (grepl("Median", label) | grepl("Average", label)))]
  }

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
  param_grid <- create_param_grid(
    items = table.names,
    years = years,
    params = base_params,
    item_name = "table"  # Specify the name of the item column
  )

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
