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
reliability_calcs <- function(df, estimate = "estimate", moe = "moe") {
  estimate_sym <- rlang::ensym(estimate)
  moe_sym <- rlang::ensym(moe)

  dfx <- dplyr::mutate(df,
      se = !!moe_sym / 1.645,
      cv = se / !!estimate_sym,
      reliability = dplyr::case_when(
        !!estimate_sym == 0 ~ "estimate is 0, cannot compute",
        cv <= 0.15 ~ "good",
        cv > 0.15 & cv <= 0.30 ~ "fair",
        cv > 0.30 & cv <= 0.50 ~ "use with caution",
        cv > 0.50 ~ "use with extreme caution",
        .default = "missing or N/A"
      )
    )

  return(dfx)
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
  ), by = c("state","variable")]
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
  api_params <- copy(params)

  # Extract parameters for processing logic
  geography <- api_params$geo_name
  is_table  <- api_params$is_table
  filter_fips <- if(!is.null(api_params$fips)){api_params$fips}else{NULL}
  data_item <- if(api_params$is_table){api_params$table}else{api_params$variables}

  # Remove unneeded parameters
  to_remove <- c("geo_name", "is_table", "fips")
  api_params <- api_params[!names(api_params) %in% to_remove]

  tryCatch({
    # Make the API call
    dt <- do.call(tidycensus::get_acs, api_params) %>% setDT()

    # Filter by FIPS for MSA or place
    if (geography %in% c("msa","place") && !is.null(filter_fips)) {
      dt <- dt[GEOID %in% filter_fips]
      if(nrow(dt) == 0) {
        warning(paste("No data found for FIPS codes:",
                      paste(filter_fips, collapse = ", ")))
      }
    } else if (geography == "place") {
      # Filter for requested FIPS or PSRC places if unspecified
      psrc_places <- get_psrc_places(params$year) %>% sf::st_drop_geometry()
      place_FIPS <- unique(psrc_places$GEOID)
      dt <- dt[GEOID %in% place_FIPS]
      if(nrow(dt) == 0) {
        warning("No data found for specified places")
      }
    }

    # Format the result
    dt <- format_cb_summary_tbl(dt, geography, census_type = "acs")

    # Add region total for PSRC counties
    if (all(geography == "county" &&
        identical(unique(dt$GEOID), c('53033', '53035', '53053', '53061')))) {
      dt <- add_regional_acs(dt)
    }
  },
  error = function(e) {
    warning(paste("Error retrieving data for item:", data_item,
                  "year:", params$year, "geography:", geography,
                  "- Error:", e$message))
    return(NULL)
  })

  if (!is.null(dt)) {
    # Add labels
    ref_table <- if(!is_table){sub("_.*$", "", data_item)}else{data_item}
    dt <- label_acs_variables(dt, ref_table, params$year, params$survey)

    # Add metadata
    dt[, `:=`(acs_type = params$survey, year = params$year)]
  }

  # # Filter out regional medians/averages  ## Where would these be produced?
  # dt <- dt[!(GEOID == "Region" & (grepl("Median", label) | grepl("Average", label)))]

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
#' @param table.names A character string or vector of Census table; alternative to individual variable codes
#' @param variables A character string or vector of individual Census variable codes; alternative to table codes
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
                         variables,
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

  # Either table.names or variables must be provided
  if (is.null(table.names) && is.null(variables)) {
    stop("Either table.names or variables must be provided")
  }

  # Determine which data items to use (table.names or variables)
  is_table <- !is.null(table.names)
  data_items <- if (is_table) table.names else variables
  item_name <- if (is_table) "table" else "variables"

  # Create base parameter list
  base_params <- list(
    geo_name = geography,
    geography = geotype_lookup[J(geography), get("api_label")],
    survey = acs.type,
    is_table = is_table
  )

  # Add geography-specific parameters
  if (geography != "msa") {
    base_params$state <- state
  }

  if (geography %in% c("county", "tract", "block group")) {
    base_params$county <- counties
  }

  # Add filtering parameters (not used directly by API but needed for post-processing)
  if (!is.null(FIPS) | !is.null(place_FIPS)) {
    base_params$fips <- dplyr::coalesce(FIPS, place_FIPS)
  }

  # Generate parameter grid for all combinations using the combined function
  param_grid <- create_param_grid(
    items = data_items,
    years = years,
    params = base_params
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
