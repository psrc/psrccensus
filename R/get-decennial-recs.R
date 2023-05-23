utils::globalVariables("GEOID")

#' Search Dicennial Census variables
#'
#' Includes primary, subject and profile tables
#' @param regex search term
#' @param year optionally restrict search to a specific year
#' @return data.table of filtered variable codes and attributes
#'
#' @examples
#' # Nursing home or medical group quarters
#' z <- dicennial_varsearch("^Total!!Institutionalized .*(nursing|hospital|hospice)")
#'
#' # All variables from table POO1
#' z <- dicennial_varsearch("^P001")
#'
#' @rawNamespace import(data.table, except = c(month, year))
#' @importFrom lubridate now month year
#' @export
dicennial_varsearch <- function(regex, year=NULL){
  name <- label <- concept <- NULL # Instantiate tidycensus::pums_variables variable locally (for documentation, not function)
  find_dicennial_year <- function(year=NULL){
    lag_year <- if(is.null(year)){year(now() - months(18))}else{year}
    dyear <- lag_year - lag_year %% 10
    if(dyear==2020){dyear <- 2010}else{dyear} # Due to pandemic disruption, 2010 is better reference than 2020
    return(dyear)
  }
  dyr <- find_dicennial_year(year)
  pull_varlist <- function(survey){
    x <- tidycensus::load_variables(dyr, survey) %>% setDT() %>%
      .[grepl(regex, name, ignore.case=TRUE)|
        grepl(regex, label, ignore.case=TRUE)|
        grepl(regex, concept, ignore.case=TRUE)]# %>% unique()
    return(x)
  }
  dicennial_types <- c("sf1", "sf2", "pl")
  rs <- list()
  rs <- lapply(dicennial_types, pull_varlist) %>% rbindlist(fill=TRUE)
  return(rs)
}

#' Decennial Estimates by Tract or Block Group and County
#'
#' Generate decennial estimates for multiple tables by multiple tracts and/or counties
#'
#' @param geography A character string as either 'tract', 'county', 'block group'.
#' @param counties A character string or vector of counties. Defaults to PSRC counties.
#' @param table_codes A character string or vector of Census table codes.
#' @param years Numeric or a vector of numeric years. A decennial year or years equal or greater than 2000.
#' @param state A character string state abbreviation
#'
#' @author Christy Lam
#'
#' @return a tibble of decennial estimates by either tracts in a county/counties for selected table codes and years. Does not include
#' variable names.
#'@keywords internal
get_decennial_tract_county_bg <- function(geography, counties = c('King', 'Kitsap', 'Pierce', 'Snohomish'),
                                          table_codes, years, state = 'WA') {
  get_decennial_geogs <- purrr::partial(tidycensus::get_decennial,
                                        geography = geography,
                                        state = state,
                                        table = table)
  dfs <- NULL
  for(year in years) {
    for(table in table_codes) {
      tryCatch(
        all_geogs <- purrr::map(counties, ~get_decennial_geogs(county = .x, year = year)),
        error = function(e) print(paste('API error, Decennial data for', year, 'may not be available.'))
      )
      if(exists('all_geogs')) {
        d <- purrr::reduce(all_geogs, dplyr::bind_rows)
        d$year <- year
        ifelse(is.null(dfs), dfs <- d, dfs <- dplyr::bind_rows(dfs, d))
        rm(all_geogs)
      }
    }
  }

  # create regional summary for county geography
  if(geography == 'county' & identical(unique(dfs$GEOID), c('53033', '53035', '53053', '53061'))) {
    region <- dfs %>%
      dplyr::group_by(.data$variable, .data$year) %>%
      dplyr::summarise(value = sum(.data$value)) %>%
      dplyr::mutate(GEOID = 'REGION', NAME = 'Region')
    dfs <- dplyr::bind_rows(dfs, region)
  }

  return(dfs)
}
#'
#' Decennial Estimates by MSA
#'
#' Generate decennial estimates for multiple tables by MSA(s).
#'
#' @param table_codes A character string or vector of Census table codes.
#' @param years Numeric or a vector of numeric years. A decennial year or years equal or greater than 2000.
#' @param fips Character value. Single code or vector of MSA fips codes.
#'
#' @author Christy Lam
#'
#' @return a tibble of decennial estimates by MSA(s) for selected table codes. Does not include
#' variable names.
#'@keywords internal
get_decennial_msa <- function(table_codes, years, fips = NULL) {
  msa_geog <- 'metropolitan statistical area/micropolitan statistical area'

  dfs <- NULL
  for(year in years) {
    for(table_code in table_codes) {
      tryCatch(
        d <- tidycensus::get_decennial(geography = msa_geog,
                                       state = NULL,
                                       table = table_code,
                                       year = year),
        error = function(e) print(paste('API error, the year', year, 'requested may not be available.'))
      )
      if(exists('d')) {
        d$year <- year
        ifelse(is.null(dfs), dfs <- d, dfs <- dplyr::bind_rows(dfs, d))
        rm(d)
      }
    }
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
#' @param years Numeric or a vector of numeric years. A decennial year or years equal or greater than 2000.
#' @param fips Character value. Single code or vector of place fips codes (including state prefix). If NULL, Places within the PSRC Region will be returned.
#' @param state A character string state abbreviation
#'
#' @author Christy Lam
#'
#' @return a tibble of decennial estimates by place(s) for selected table codes. Does not include
#' variable names.
#'@keywords internal
get_decennial_place <- function(table_codes, years, fips = NULL, state = 'WA') {
  dfs <- NULL
  for(year in years) {
    for(table_code in table_codes) {
      if(is.null(fips) & year>2010){psrc_places <- get_psrc_places(year) %>% dplyr::pull(GEOID)}
      tryCatch(
        d <- tidycensus::get_decennial(geography = 'place',
                                       state = state,
                                       table = table_code,
                                       year = year),
        error = function(e) print(paste('API error, the year', year, 'requested may not be available.'))
      )
      if(exists('d')) {
        d$year <- year
        if(!is.null(fips)){d %<>% filter(GEOID %in% fips)}else if(year>2010){d %<>% filter(GEOID %in% psrc_places)}
        ifelse(is.null(dfs), dfs <- d, dfs <- dplyr::bind_rows(dfs, d))
        rm(d)
      }
    }
  }

  if(!is.null(fips)) dfs <- dplyr::filter(dfs, GEOID %in% fips)

  return(dfs)
}
#'
#' Decennial Estimates
#'
#' Generate decennial estimates for multiple tables by tracts, counties, MSAs, or places.
#' Currently only working for SF1 tables.
#' @param geography A character string as either 'tract', 'county', 'block group', 'msa', or 'place'.
#' @param counties A character string or vector of counties. Defaults to PSRC counties.
#' @param table_codes A character string or vector of Census table codes,
#' the table code will be padded with 0s such as "H001", as opposed to "H1
#' @param years Numeric or a vector of numeric years. A decennial year or years equal or greater than 2000.
#' @param fips Character. Single code or vector of either MSA or place fips codes.
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
#'                    table_codes = c("H001", "P001"),
#'                    years = c(2000, 2010),
#'                    fips = c('42660', "28420"))
#'
#' get_decennial_recs(geography = 'block group',
#'                    table_codes = c('H001', 'H006'),
#'                    years = 2010)
#' @export
get_decennial_recs <- function(geography, counties = c('King', 'Kitsap', 'Pierce', 'Snohomish'), table_codes, years,
                               fips = NULL) {

  if(geography %in% c('tract', 'county', 'block group')) {
    dfs <- get_decennial_tract_county_bg(geography = geography, table_codes = table_codes, years = years)
  } else if (geography == 'msa'){
    dfs <- get_decennial_msa(table_codes, years, fips = fips)
  } else if(geography == 'place') {
    dfs <- get_decennial_place(table_codes, years, fips = fips)
  }

  # add labels
  final_dfs <- NULL
  data_years <- unique(dfs$year)
  for(data_year in data_years) {
    vars <- tidycensus::load_variables(data_year, "sf1")

    if(data_year == 2000) { # clean labels
      vars$concept <- stringr::str_extract(vars$concept, '.*(?=\\s(\\[)*)')
    }

    df_join <- dfs %>%
      dplyr::filter(year == data_year) %>%
      dplyr::left_join(vars, by = c("variable" = "name"))
    ifelse(is.null(final_dfs), final_dfs <- df_join, final_dfs <- dplyr::bind_rows(final_dfs, df_join))
  }
  if(length(years) > 1) message('\nConcept for table codes may differ across Census years. Please double check with tidycensus::load_variables()\n')
  return(final_dfs)
}

#' Add shares to Psrccensus ACS object
#'
#' @param df dataframe with Psrccensus Decennial result
#' @return dataframe with additional share and share_moe fields
#' @rawNamespace import(data.table, except = c(month, year))
#' @export
add_decennial_share <- function(df){
  label <- x.value <- i.value <- concept <- share <- share_moe <- NULL
  input_type <- class(df)
  rs <- setDT(df)
  tots <- copy(rs) %>% .[grepl("Total$",label)]
  rs %<>% .[tots, `:=`(share=x.value/i.value),
            on=.(GEOID, concept, year)]
  if("data.table" %not_in% input_type){rs %<>% setDT()}
  return(rs)
}

