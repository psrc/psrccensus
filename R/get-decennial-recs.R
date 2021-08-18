utils::globalVariables("GEOID")

#' Decennial Estimates by Tract and County
#'
#' Generate decennial estimates for multiple tables by multiple tracts and/or counties
#'
#' @param geography A character string as either 'tract', 'county', 'msa', or 'place'.
#' @param counties A character string or vector of counties. Defaults to PSRC counties.
#' @param table_codes A character string or vector of Census table codes.
#' @param year Numeric value. A decennial year equal or greater than 2010
#' @param state A character string state abbreviation
#'
#' @author Christy Lam
#'
#' @return a tibble of decennial estimates by either tracts in a county/counties for selected table codes. Does not include
#' variable names.
#'@keywords internal
get_decennial_tract_county <- function(geography, counties = c('King', 'Kitsap', 'Pierce', 'Snohomish'),
                                       table_codes, year, state = 'WA') {
  get_decennial_geogs <- purrr::partial(tidycensus::get_decennial,
                                        geography = geography,
                                        state = state,
                                        table = table)
  dfs <- NULL
  for(table in table_codes) {

    all_geogs <- purrr::map(counties, ~get_decennial_geogs(county = .x))

    # append via recursion
    df <- purrr::reduce(all_geogs, dplyr::bind_rows)
    ifelse(is.null(dfs), dfs <- df, dfs <- dplyr::bind_rows(dfs, df))
  }
  return(dfs)
}
#'
#' Decennial Estimates by MSA
#'
#' Generate decennial estimates for multiple tables by MSA(s).
#'
#' @param table_codes A character string or vector of Census table codes.
#' @param year Numeric value. A decennial year equal or greater than 2010
#' @param fips Character value. Single code or vector of MSA fips codes.
#'
#' @author Christy Lam
#'
#' @return a tibble of decennial estimates by MSA(s) for selected table codes. Does not include
#' variable names.
#'@keywords internal
get_decennial_msa <- function(table_codes, year, fips = NULL) {
  msa_geog <- 'metropolitan statistical area/micropolitan statistical area'

  dfs <- NULL
  for(table_code in table_codes) {
    df <- tidycensus::get_decennial(geography = msa_geog,
                                    state = NULL,
                                    table = table_code)
    ifelse(is.null(dfs), dfs <- df, dfs <- dplyr::bind_rows(dfs, df))
  }

  if(!is.null(fips)) dfs <- dplyr::filter(dfs, GEOID %in% fips)

  return(dfs)
}
#'
#' Decennial Estimates by Place
#'
#' Generate decennial estimates for multiple tables by place(s).
#'
#' @param table_codes A character string or vector of Census table codes.
#' @param year Numeric value. A decennial year equal or greater than 2010
#' @param fips Character value. Single code or vector of place fips codes.
#' @param state A character string state abbreviation
#'
#' @author Christy Lam
#'
#' @return a tibble of decennial estimates by place(s) for selected table codes. Does not include
#' variable names.
#'@keywords internal
get_decennial_place <- function(table_codes, year, fips = NULL, state = 'WA') {
  dfs <- NULL
  for(table_code in table_codes) {
    df <- tidycensus::get_decennial(geography = 'place',
                                    state = state,
                                    table = table_code)
    ifelse(is.null(dfs), dfs <- df, dfs <- dplyr::bind_rows(dfs, df))
  }

  if(!is.null(fips)) dfs <- dplyr::filter(dfs, GEOID %in% fips)

  return(dfs)
}
#'
#' Decennial Estimates
#'
#' Generate decennial estimates for multiple tables by tracts, counties, MSAs, or places.
#' Currently only working for SF1 tables.
#' @param geography A character string as either 'tract', 'county', 'msa', or 'place'.
#' @param counties A character string or vector of counties. Defaults to PSRC counties.
#' @param table_codes A character string or vector of Census table codes,
#' the table code will be padded with 0s such as "H001", as opposed to "H1
#' @param year Numeric. A decennial year equal or greater than 2010.
#' @param fips Character. Single code or vector of either MSA or place fips codes.
#'
#' @author Christy Lam
#'
#' @return a tibble of decennial estimates by selected geography for selected table codes. Includes variable names.
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#' tbl_names <- paste0('PCT020', LETTERS[1:6])
#' get_decennial_recs(geography = 'county', table_codes = tbl_names, year = 2010)
#'
#' get_decennial_recs(geography = 'tract', table_codes = tbl_names, year = 2010)
#'
#' get_decennial_recs(geography = 'place',
#'                    table_codes = 'PCT013',
#'                    year = 2010,
#'                    fips = c("5363000", "5308850"))
#'
#' get_decennial_recs(geography = 'msa',
#'                    table_codes = c("H001", "P001"),
#'                    year = 2010,
#'                    fips = c('42660', "28420"))
#' @export
get_decennial_recs <- function(geography, counties = c('King', 'Kitsap', 'Pierce', 'Snohomish'), table_codes, year,
                               fips = NULL) {

  if(geography %in% c('tract', 'county')) {
    dfs <- get_decennial_tract_county(geography = geography, table_codes = table_codes, year = year)
  } else if (geography == 'msa'){
    dfs <- get_decennial_msa(table_codes, year, fips = fips)
  } else if(geography == 'place') {
    dfs <- get_decennial_place(table_codes, year, fips = fips)
  }
  # add labels
  vars <- tidycensus::load_variables(year, "sf1")
  df_join <- dplyr::left_join(dfs, vars, by = c("variable" = "name"))
  return(df_join)
}
