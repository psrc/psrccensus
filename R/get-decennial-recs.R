utils::globalVariables(c("GEOID","value","variable","J","county","name","census_geography"))

#' @importFrom magrittr %<>% %>%
#' @rawNamespace import(data.table, except = c(month, year))
NULL

#' Label Decennial variables
#'
#' Helper function for \code{\link{get_decennial_single}} to provide variable labels and concept alongside codes
#' @param dt data.table with Census API result
#' @param year the year of the Decennial Census survey
#' @param sumfile the summary file type (e.g., 'sf1', 'dp')
#'
#' @return dataframe with labels appended
label_decennial_variables <- function(dt, year, sumfile){
  name <- label <- concept <- NULL # instantiate variables
  labels <- tidycensus::load_variables(year=year, dataset=sumfile) %>% setDT()

  # Clean 2000 Census labels if needed
  if(year == 2000) {
    labels[, concept := stringr::str_extract(concept, '.*(?=\\s(\\[)*)')]
  }

  labels <- labels[, .(variable = name, label, concept)] %>% setkey("variable")
  dt <- setDT(dt) %>% setkey("variable") %>% merge(labels, all.x = TRUE)

  return(dt)
}

#' Add regional totals
#'
#' Helper function for \code{\link{get_decennial_single}} to add regional totals to county estimates
#' @param dt data.table with Census API result
#' @return dt with regional totals
add_regional_decennial <- function(dt){
  # Aggregate by variable
  total_dt <- dt[, .(sumvalue = sum(value)), by = c("variable")]
  setnames(total_dt, "sumvalue", "value")

  #total_dt[, `:=`(GEOID = "REGION", name = "Region", census_geography = "Region")]
  total_dt[, `:=`(GEOID = "REGION", NAME = "Region")]

  # Combine with original data
  dtx <- rbindlist(list(dt, total_dt), use.names = TRUE, fill = TRUE)

  return(dtx)
}

#' Get single Decennial Census table
#'
#' Core function to get Decennial Census data for any geography
#' @param params list of parameter combinations from \code{\link{create_param_grid}}
#' @return dt formatted Decennial Census data for a single geography and year
get_decennial_single <- function(params) {
  # Extract parameters
  data_item <- params$data_item
  year <- params$year
  geography <- params$geography
  state <- params$state
  counties <- params$counties
  fips <- params$fips
  sumfile <- params$sumfile
  is_table <- params$is_table
  api_geo <- geotype_lookup[J(geography), get("api_label")]

  tryCatch({
    # Geography-specific parameters and processing
    if (geography %in% c("county", "tract", "block group")) {
      dt <- tidycensus::get_decennial(
        state = state,
        geography = api_geo,
        county = counties,
        year = year,
        sumfile = sumfile,
        if(!is_table){variables = data_item},
        if(is_table){table= data_item},
      ) %>% setDT() %>%
        format_cb_summary_tbl(geography, census_type = "decennial")

      # Add county field based on GEOID
      dt[, county := substr(GEOID, 1, 5)]

      # Region total for PSRC counties
      if(geography=="county" &
         identical(unique(dt$GEOID), c('53033', '53035', '53053', '53061'))) {
          dt <- add_regional_decennial(dt)
        }

    } else if (geography == "msa") {
      dt <- tidycensus::get_decennial(
        geography = api_geo,
        year = year,
        sumfile = sumfile,
        if(!is_table){variables = data_item},
        if(is_table){table= data_item},
      ) %>% setDT()

      # Filter for requested FIPS if specified
      if (!is.null(fips)) {
        dt <- dt[GEOID %in% fips]
      }

      dt <- format_cb_summary_tbl(dt, geography, census_type = "decennial")

    } else if (geography %in% c("place", "puma", "state")) {
      dt <- tidycensus::get_decennial(
        state = state,
        geography = api_geo,
        year = year,
        sumfile = sumfile,
        if(!is_table){variables = data_item},
        if(is_table){table= data_item},
      ) %>% setDT()

      if(geography=="place"){
        # Filter for requested FIPS or PSRC places if needed
        if (!is.null(fips)) {
          dt <- dt[GEOID %in% fips]
        } else if (year > 2010) {
          psrc_places <- get_psrc_places(year) %>% sf::st_drop_geometry()
          place_fips <- unique(psrc_places$GEOID)
          dt <- dt[GEOID %in% place_fips]
        }

        dt <- format_cb_summary_tbl(dt, geography, census_type = "decennial")

        # Update census_geography based on name suffix
        dt[endsWith(name, "city"), census_geography := "City"]
        dt[endsWith(name, "CDP"), census_geography := "CDP"]

        # Remove suffixes from name by reference
        dt[, name := gsub(" (city|CDP)", "", name)]
      } else if (geography == "puma") {
        # Clean up name field by removing trailing separators
        dt[, name := gsub("[\\;|\\,]\\s*$", "", name)]
      }
    }
  },
  error = function(e) {
    warning(paste("Error retrieving data for item:", data_item,
                  "year:", year, "geography:", geography,
                  "- Error:", e$message))
    return(NULL)  # Return NULL for failed requests
  })

  # Add labels and metadata if data was retrieved successfully
  if (!is.null(dt)) {
    dt <- label_decennial_variables(dt, year, sumfile)

    # Add metadata
    dt[, `:=`(year = year)] # Could add , sumfile = sumfile
    setcolorder(dt, c("GEOID","NAME")) %>% setcolorder("year", before="label")# to match pre-refactored code
  }

  return(dt)
}

#' Add shares to Decennial Census result
#'
#' @param df dataframe with Decennial Census result
#' @return dataframe with additional share field
#' @author Michael Jensen
#'
#' @export
add_decennial_share <- function(df){
  label <- x.value <- i.value <- concept <- share <- NULL
  input_type <- class(df)
  rs <- setDT(df)
  tots <- copy(rs) %>% .[grepl("Total$", label)]
  rs <- rs[tots, `:=`(share = x.value/i.value),
           on = .(GEOID, concept, year)]
  if("data.table" %not_in% input_type){
    rs <- setDF(rs)
  }
  return(rs)
}

#' Decennial Census Estimates
#'
#' Generate decennial estimates for multiple tables by tracts, counties, MSAs, or places.
#'
#' @param geography A character string as either 'tract', 'county', 'block group', 'msa', or 'place'.
#' @param counties A character string or vector of counties. Defaults to PSRC counties.
#' @param table_codes A character string or vector of Census table codes,
#' the table code will be padded with 0s such as "H001", as opposed to "H1"
#' @param variables A character string or vector of Census variables
#' @param years Numeric or a vector of numeric years. A decennial year or years equal or greater than 2000.
#' @param sumfile A character string for which summary file to use such as "sf1" or "dp"
#' @param fips Character. Single code or vector of either MSA or place fips codes.
#' @param state A character string state abbreviation. Defaults to 'WA'.
#'
#' @author Christy Lam
#'
#' @return a tibble of decennial estimates by selected geography for selected table codes. Includes variable names.
#' @examples
#' tbl_names <- paste0('PCT020', LETTERS[1:6])
#' get_decennial_recs(geography = 'county', table_codes = tbl_names, years = 2010)
#'
#' get_decennial_recs(geography = 'county', table_codes = 'P001', years = c(2000, 2010))
#'
#' get_decennial_recs(geography = 'tract', table_codes = tbl_names, years = 2010)
#'
#' get_decennial_recs(geography = 'place',
#'                    table_codes = 'PCT013',
#'                    years = 2010,
#'                    fips = c("5363000", "5308850"))
#'
#' get_decennial_recs(geography = 'msa',
#'                    table_codes = c("H001"),
#'                    years = 2010,
#'                    fips = c('42660', "28420"))
#'
#' get_decennial_recs(geography = 'block group',
#'                    table_codes = c('H001', 'H006'),
#'                    years = 2010)
#'
#' get_decennial_recs(geography="tract",
#'                    variables="DP1_0092C",
#'                    years=2020, sumfile="dp")
#' @export
get_decennial_recs <- function(geography, counties = c('King', 'Kitsap', 'Pierce', 'Snohomish'),
                               sumfile = 'sf1', years, variables = NULL, table_codes = NULL,
                               fips = NULL, state = 'WA') {

  # Validates geography parameter
  valid_geographies <- c('county', 'msa', 'place', 'tract', 'block group')
  if (!(geography %in% valid_geographies)) {
    stop(paste("Invalid geography. Must be one of:", paste(valid_geographies, collapse=", ")))
  }

  # Either table_codes or variables must be provided
  if (is.null(table_codes) && is.null(variables)) {
    stop("Either table_codes or variables must be provided")
  }

  # Determine which data items to use (table_codes or variables)
  is_table <- !is.null(table_codes)
  data_items <- if (is_table) table_codes else variables
  item_name <- if (is_table) "table" else "variables"

  # Create base parameter list
  base_params <- list(
    geography = geography,
    state = state,
    counties = counties,
    fips = fips,
    sumfile = sumfile,
    is_table = is_table
  )

  # Generate parameter grid for all combinations using the combined function
  param_grid <- create_param_grid(
    items = data_items,
    years = years,
    params = base_params,
    item_name = "data_item"  # Specify what to name the item column
  )

  # Process all data_item/year combinations
  rs <- lapply(param_grid, get_decennial_single)

  if (length(rs) == 0 || all(sapply(rs, is.null))) {
    warning("No data was retrieved for the specified parameters")
    return(data.frame())
  } else {
    # Filter out NULLs
    rs <- rs[!sapply(rs, is.null)]

    # Combine results
    rs <- rbindlist(rs, fill = TRUE)

    # Convert to data.frame if needed
    rs <- setDF(rs)

    # Warn for potentially differing concepts across years
    if (length(years) > 1) {
      message('\nConcept for table codes may differ across Census years. Please double check with tidycensus::load_variables()\n')
    }

    return(rs)
  }
}
