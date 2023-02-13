#' ACS Estimates by County
#'
#' Generate ACS estimates for multiple tables by multiple counties
#' This is a helper function for the main function get_acs_recs.
#' @param state A character string state name or abbreviation. Defaults to Washington.
#' @param counties A character string or vector of counties. Defaults to PSRC counties.
#' @param table.names A character string or vector of Census table codes.
#' @param years A numeric value or vector of years. An ACS year equal or greater than 2010 to the latest available year.
#' @param acs.type A character string as either 'acs1', 'acs3' or acs5'.
#'
#' @author Craig Helmann
#'
#' @return a tibble of acs estimates by counties for selected table codes and years with a regional aggregation.
#' Includes detailed variable names.
#'
#'@importFrom magrittr %>% %<>%
#'@importFrom rlang .data
#'@importFrom dplyr filter

get_acs_county <- function (state="Washington", counties = c("King","Kitsap","Pierce","Snohomish"), table.names, years, acs.type) {

  census.data <- NULL
  for (table in table.names) {

    yearly.data <- NULL

    for (year in years) {

      # Download ACS Data
      tbl <- tidycensus::get_acs(state=state, geography='county', county=counties, year=year, survey=acs.type, table=table)


      # Split County Name for County and State
      tbl <- tbl %>% tidyr::separate(col=.data$NAME, into=c("name", "state"),sep=",") %>%
                     dplyr::mutate(estimate =tidyr::replace_na(.data$estimate,0))

      tbl$state <- trimws(tbl$state, "l")

      # Create a Total for Region if all 4 PSRC Counties are pulled and then join to county table
      if (identical(counties, c("King","Kitsap","Pierce","Snohomish"))){

        total <- tbl %>%
          dplyr::select(.data$variable, .data$estimate, .data$moe) %>%
          dplyr::mutate(estimate =tidyr::replace_na(.data$estimate,0))%>%
          dplyr::group_by(.data$variable) %>%
          dplyr::summarize(sumest = sum(.data$estimate), summoe = tidycensus::moe_sum(.data$moe, .data$estimate)) %>%
          dplyr::rename(estimate=.data$sumest, moe=.data$summoe) %>%
          dplyr::mutate(GEOID="REGION", name="Region", state=state)
        tbl <- dplyr::bind_rows(list(tbl,total))
      }

      # Add labels to the data - The labels can differ for each year so loading now
      labels <- tidycensus::load_variables(year=year, dataset=acs.type)
      if("geography" %in% names(labels)){
        labels %<>% select(-geography)
        }
      labels <- dplyr::rename(.data=labels, variable = .data$name)
      tbl <- dplyr::left_join(tbl, labels,by=c("variable"))

      # Add column for Census Geography, Type and Year of Data
      tbl <- tbl %>%
        dplyr::mutate(census_geography="County", acs_type = acs.type, year=year)

      # Median and average calculations are more complicated, may need PUMS, filter out for now:
      tbl  <- tbl %>% dplyr::filter(.data$name !='Region' |
                               (!(stringr::str_detect(.data$label, 'Median'))&!(stringr::str_detect(.data$label, 'Average'))))

      # Store yearly data into final yearly data for current table - append if a year already exists
      ifelse(is.null(yearly.data), yearly.data <- tbl, yearly.data <- dplyr::bind_rows(yearly.data, tbl))

    }

    # Store table data into final census data - append if a table set already exists
    ifelse(is.null(census.data), census.data <- yearly.data, census.data <- dplyr::bind_rows(census.data, yearly.data))

  }

  return(census.data)
}



#' ACS Estimates by MSA
#'
#' Generate ACS estimates for multiple tables by multiple MSA's
#'
#' @param table.names A character string or vector of Census table codes.
#' @param years A numeric value or vector of years. An ACS year equal or greater than 2010 to the latest available year.
#' @param acs.type A character string as either 'acs1', 'acs3' or acs5'.
#' @param FIPS Character string for FIPS codes for specific MSA geographies. Defaults to Seattle & Bremerton MSA c("14740","42660")
#'
#' @author Craig Helmann
#'
#' @return a tibble of acs estimates by MSA for selected table codes and years. Includes detailed variable names.
#'
#'@importFrom magrittr %>%
#'@importFrom rlang .data
#'@keywords internal
get_acs_msa <- function (table.names, years, acs.type, FIPS = c("14740","42660")) {

  census.data <- NULL
  for (table in table.names) {

    yearly.data <- NULL

    for (year in years) {

      # Download ACS Data
      tbl <- tidycensus::get_acs(geography="metropolitan statistical area/micropolitan statistical area", year=year, survey=acs.type, table=table) %>%
        dplyr::filter(GEOID %in% FIPS) %>%
        tidyr::separate(col=.data$NAME, into=c("name", "state"),sep=",")
      tbl$state <- trimws(tbl$state, "l")

      # Add labels to the data - The labels can differ for each year so loading now
      labels <- tidycensus::load_variables(year=year, dataset=acs.type)
      if("geography" %in% names(labels)){
        labels %<>% select(-geography)
      }
      labels %<>% dplyr::rename(variable = .data$name)
      tbl <- dplyr::left_join(tbl,labels,by=c("variable"))

      # Add column for Census Geography, Type and Year of Data
      tbl <- tbl %>% dplyr::mutate(census_geography="MSA", acs_type = acs.type, year=year)

      # Store yearly data into final yearly data for current table - append if a year already exists
      ifelse(is.null(yearly.data), yearly.data <- tbl, yearly.data <- dplyr::bind_rows(yearly.data, tbl))

    }

    # Store table data into final census data - append if a table set already exists
    ifelse(is.null(census.data), census.data <- yearly.data, census.data <- dplyr::bind_rows(census.data, yearly.data))

  }

  return(census.data)
}

#' ACS Estimates by Place
#'
#' Generate ACS estimates for multiple tables by multiple places
#'
#' @param state A character string state name or abbreviation. Defaults to Washington.
#' @param table.names A character string or vector of Census table codes.
#' @param years A numeric value or vector of years. An ACS year equal or greater than 2010 to the latest available year.
#' @param acs.type A character string as either 'acs1', 'acs3' or acs5'.
#' @param place_FIPS Character string of FIPS codes (with state prefix) for specific Census Places. If NULL, Places within the PSRC Region will be returned.
#'
#' @author Craig Helmann
#'
#' @return a tibble of acs estimates by Place for selected table codes and years. Includes detailed variable names.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @keywords internal

get_acs_place <- function (state="Washington", table.names, years, acs.type, place_FIPS=NULL) {

  census.data <- NULL

  for (table in table.names) {

    yearly.data <- NULL

    for (year in years) {
      # Determine Places within Region
      if(is.null(place_FIPS) & year>2010){psrc_places <- get_psrc_places(year) %>% sf::st_drop_geometry() %>% unique() %>%
        dplyr::pull(GEOID)}

      # Download ACS Data
      tbl <- tidycensus::get_acs(state=state, geography='place', year=year, survey=acs.type, table=table) %>%
        tidyr::separate(col=.data$NAME, into=c("name", "state"),sep=",")
      if(!is.null(place_FIPS)){tbl %<>% filter(GEOID %in% place_FIPS)}else if(year>2010){tbl %<>% filter(GEOID %in% psrc_places)}
      tbl$state <- trimws(tbl$state, "l")

      # Add labels to the data - The labels can differ for each year so loading now
      labels <- tidycensus::load_variables(year=year, dataset=acs.type)
      if("geography" %in% names(labels)){
        labels %<>% select(-geography)
      }
      labels %<>% dplyr::rename(variable = .data$name)
      tbl <- dplyr::left_join(tbl,labels,by=c("variable"))

      # Add column for Census Geography, Type and Year of Data
      tbl <- tbl %>%
        dplyr::mutate(acs_type = acs.type, year=year) %>%
        dplyr::mutate(census_geography = dplyr::case_when(
          endsWith(name, "city") ~ "City",
          endsWith(name, "CDP") ~ "CDP")) %>%
        dplyr::mutate(name = gsub(" city", "", .data$name)) %>%
        dplyr::mutate(name = gsub(" CDP", "", .data$name))

      # Store yearly data into final yearly data for current table - append if a year already exists
      ifelse(is.null(yearly.data), yearly.data <- tbl, yearly.data <- dplyr::bind_rows(yearly.data, tbl))

    }

    # Store table data into final census data - append if a table set already exists
    ifelse(is.null(census.data), census.data <- yearly.data, census.data <- dplyr::bind_rows(census.data, yearly.data))

  }

  return(census.data)
}

#' ACS Estimates by Census Tract
#'
#' Generate ACS 5yr estimates for multiple tables by Census Tracts in multiple counties
#'
#' @param state A character string state name or abbreviation. Defaults to Washington.
#' @param counties A character string or vector of counties. Defaults to PSRC counties.
#' @param table.names A character string or vector of Census table codes.
#' @param years A numeric value or vector of years. An ACS year equal or greater than 2010 to the latest available year.
#'
#' @author Craig Helmann
#'
#' @return a tibble of acs estimates by tracts for selected table codes. Includes detailed variable names.
#'
#'@importFrom magrittr %>%
#'@importFrom rlang .data
#'@keywords internal
get_acs_tract <- function (state="Washington", counties = c("King","Kitsap","Pierce","Snohomish"), table.names, years) {

  census.data <- NULL
  for (table in table.names) {

    yearly.data <- NULL

    for (year in years) {

      # Download ACS Data
      tbl <- tidycensus::get_acs(state=state, county=counties, geography='tract', year=year, survey="acs5", table=table) %>%
        tidyr::separate(col=.data$NAME, into=c("name", "county", "state"),sep=",")
      tbl$county <- trimws(tbl$county, "l")
      tbl$state <- trimws(tbl$state, "l")
      county.names <- paste(counties,"County")

      # Add labels to the data - The labels can differ for each year so loading now
      labels <- tidycensus::load_variables(year=year, dataset="acs5")
      if("geography" %in% names(labels)){
        labels %<>% select(-geography)
      }
      labels %<>% dplyr::rename(variable = .data$name)
      tbl <- dplyr::left_join(tbl,labels,by=c("variable"))

      # Add column for Census Geography, Type and Year of Data
      tbl <- tbl %>%
        dplyr::mutate(census_geography="Tract", acs_type = "acs5", year=year) %>%
        dplyr::select(-.data$county)

      # Store yearly data into final yearly data for current table - append if a year already exists
      ifelse(is.null(yearly.data), yearly.data <- tbl, yearly.data <- dplyr::bind_rows(yearly.data, tbl))

    }

    # Store table data into final census data - append if a table set already exists
    ifelse(is.null(census.data), census.data <- yearly.data, census.data <- dplyr::bind_rows(census.data, yearly.data))

  }

  return(census.data)
}

#' ACS Estimates by Census Block Group
#'
#' Generate ACS 5yr estimates for multiple tables by Census Block Groups in multiple counties
#'
#' @param state A character string state name or abbreviation. Defaults to Washington.
#' @param counties A character string or vector of counties. Defaults to PSRC counties.
#' @param table.names A character string or vector of Census table codes.
#' @param years A numeric value or vector of years. An ACS year equal or greater than 2010 to the latest available year.
#'
#' @author Craig Helmann
#'
#' @return a tibble of acs estimates by tracts for selected table codes. Includes detailed variable names.
#'
#'@importFrom magrittr %>%
#'@importFrom rlang .data
#'@keywords internal
get_acs_blockgroup <- function (state="Washington", counties = c("King","Kitsap","Pierce","Snohomish"), table.names, years) {

  census.data <- NULL
  for (table in table.names) {

    yearly.data <- NULL

    for (year in years) {

      # Download ACS Data
      tbl <- tidycensus::get_acs(state=state, county=counties, geography="block group", year=year, survey="acs5", table=table) %>%
        tidyr::separate(col=.data$NAME, into=c("name", "tract", "county", "state"),sep=",")
      tbl$county <- trimws(tbl$county, "l")
      tbl$state <- trimws(tbl$state, "l")
      county.names <- paste(counties,"County")

      # Add labels to the data - The labels can differ for each year so loading now
      labels <- tidycensus::load_variables(year=year, dataset="acs5")
      if("geography" %in% names(labels)){
        labels %<>% select(-geography)
      }
      labels %<>% dplyr::rename(variable = .data$name)
      tbl <- dplyr::left_join(tbl,labels,by=c("variable"))

      # Add column for Census Geography, Type and Year of Data
      tbl <- tbl %>%
        dplyr::mutate(census_geography="block group", acs_type = "acs5", year=year) %>%
        dplyr::select(-.data$county)

      # Store yearly data into final yearly data for current table - append if a year already exists
      ifelse(is.null(yearly.data), yearly.data <- tbl, yearly.data <- dplyr::bind_rows(yearly.data, tbl))

    }

    # Store table data into final census data - append if a table set already exists
    ifelse(is.null(census.data), census.data <- yearly.data, census.data <- dplyr::bind_rows(census.data, yearly.data))

  }

  return(census.data)
}

#' ACS Estimates
#'
#' Generate ACS estimates for multiple tables by tracts, counties, MSAs, or places for multiple years.
#'
#' @param state A character string state name or abbreviation. Defaults to Washington.
#' @param geography A character string as either 'tract', 'county', 'msa', or 'place'.
#' @param counties A character string or vector of counties. Defaults to PSRC counties.
#' @param table.names A character string or vector of Census table codes.
#' @param years A numeric value or vector of years. An ACS year equal or greater than 2010 to the latest available year.
#' @param FIPS Character string for FIPS codes for specific MSA geographies. Defaults to Seattle & Bremerton MSA c("14740","42660")
#' @param acs.type A character string as either 'acs1', 'acs3' or acs5'.
#'
#' @author Craig Helmann
#'
#' @return a tibble of ACS estimates by selected geography for selected table codes. Includes variable names.
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#' get_acs_recs(geography = 'county',
#'              table.names = c('B03002',"C17002"),
#'              years=c(2017,2019),
#'              acs.type = 'acs1')
#'
#' get_acs_recs(geography = 'county',
#'              table.names = c('B03002'),
#'              counties=c("Kitsap"),
#'              years=c(2019),
#'              acs.type = 'acs5')
#'
#' get_acs_recs(geography = 'msa',
#'              table.names = c('B03002',"C17002"),
#'              years=c(2019),
#'              acs.type = 'acs1')
#'
#' get_acs_recs(geography = 'msa',
#'              table.names = c("C17002"),
#'              years=c(2019),
#'              FIPS = c("14740"),
#'              acs.type = 'acs5')
#'
#' get_acs_recs(geography = 'place',
#'              table.names = c("C17002"),
#'              years=c(2019),
#'              acs.type = 'acs5')
#'
#' get_acs_recs(geography = 'tract',
#'              counties=c("Pierce"),
#'              table.names = c('B03002'),
#'              years=c(2018,2019))
#'
#' get_acs_recs(geography = 'block group',
#'              counties=c("Pierce"),
#'              table.names = c('B03002'),
#'              years=c(2018,2019))
#'
#' @export
get_acs_recs <- function(geography, state="Washington", counties = c('King', 'Kitsap', 'Pierce', 'Snohomish'), table.names, years, FIPS = c("14740","42660"), place_FIPS=NULL, acs.type) {

  if(geography == 'county') {
    dfs <- get_acs_county(state, counties, table.names, years, acs.type)
  } else if (geography == 'msa'){
    dfs <- get_acs_msa(table.names, years, acs.type, FIPS)
  } else if(geography == 'place') {
    dfs <- get_acs_place(state, table.names, years, acs.type, place_FIPS)
  } else if(geography == 'tract') {
    dfs <- get_acs_tract(state, counties, table.names, years)
  } else if(geography == 'block group') {
    dfs <- get_acs_blockgroup(state, counties, table.names, years)
}

  return(dfs)
}
