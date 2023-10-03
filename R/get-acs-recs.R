#' @importFrom magrittr %<>% %>%
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
#' @rawNamespace import(data.table, except = c(month, year))
#' @importFrom lubridate now month year
#' @export
acs_varsearch <- function(regex, year=NULL){
  acs_year <- if(is.null(year)){year(now() - months(18))}else{year}
  name <- label <- concept <- NULL # Instantiate tidycensus::pums_variables variable locally (for documentation, not function)
  pull_varlist <- function(survey){
    x <- tidycensus::load_variables(acs_year, survey) %>% setDT() %>%
      .[grepl(regex, name, ignore.case=TRUE)|
          grepl(regex, label, ignore.case=TRUE)|
          grepl(regex, concept, ignore.case=TRUE)] %>% unique()
    return(x)
  }
  acstypes <- paste0("acs5", c("","/subject","/profile","/cprofile")) %>% c("acsse", recursive=TRUE)
  rs <- list()
  rs <- lapply(acstypes, pull_varlist) %>% rbindlist(fill=TRUE)
  return(rs)
}

#' Label ACS variables
#'
#' Helper function to provide variable labels and concept--i.e. topic--alongside codes
#' @param df dataframe with Census API result
#' @param table.name Census table code
#' @param year the year--or last year--of the ACS survey
#' @param acs.type either acs1 or acs5
#'
#' @return dataframe with labels appended

label_acs_variables <- function(df, table.name, year, acs.type){
  name <- label <- concept <- NULL # instantiate variables
  survey <- if(table.name %in% acs_tbltypes_lookup$subject){paste0(acs.type,"/subject")
  }else if(table.name %in% acs_tbltypes_lookup$profile){paste0(acs.type,"/profile")
  }else if(table.name %in% acs_tbltypes_lookup$cprofile){paste0(acs.type,"/cprofile")
  }else if(table.name %in% acs_tbltypes_lookup$acsse){"acsse"
  }else{acs.type}
  labels <- tidycensus::load_variables(year=year, dataset=survey) %>%
    dplyr::select(c(name, label, concept)) %>% dplyr::rename(variable=name)
  df <- dplyr::left_join(df, labels, by=c("variable"))
  return(df)
}

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
#'@importFrom rlang .data
#'@importFrom dplyr filter

get_acs_county <- function (state="Washington", counties = c("King","Kitsap","Pierce","Snohomish"), table.names, years, acs.type) {

  census.data <- geography <- NAME <- name <- variable <- NULL
  estimate <- moe <- sumest <- summoe <- label <- NULL

  for (table in table.names) {

    yearly.data <- NULL

    for (year in years) {

      # Download ACS Data
      tbl <- tidycensus::get_acs(state=state, geography='county', county=counties, year=year, survey=acs.type, table=table)


      # Split County Name for County and State
      tbl <- tbl %>% tidyr::separate(col=NAME, into=c("name", "state"),sep=",") %>%
        dplyr::mutate(estimate =tidyr::replace_na(estimate,0))

      tbl$state <- trimws(tbl$state, "l")

      # Create a Total for Region if all 4 PSRC Counties are pulled and then join to county table
      if (identical(counties, c("King","Kitsap","Pierce","Snohomish"))){

        total <- tbl %>%
          dplyr::select(variable, estimate, moe) %>%
          dplyr::mutate(estimate =tidyr::replace_na(estimate,0))%>%
          dplyr::group_by(variable) %>%
          dplyr::summarize(sumest = sum(estimate), summoe = tidycensus::moe_sum(moe, estimate)) %>%
          dplyr::rename(estimate=sumest, moe=summoe) %>%
          dplyr::mutate(GEOID="REGION", name="Region", state=state)
        tbl <- dplyr::bind_rows(list(tbl,total))
      }

      # Add labels, column for Census Geography, Type and Year of Data
      tbl <- tbl %>% label_acs_variables(table, year, acs.type) %>%
        dplyr::mutate(census_geography="County", acs_type = acs.type, year=year)

      # Median and average calculations are more complicated, may need PUMS, filter out for now:
      tbl  <- tbl %>% dplyr::filter(name !='Region' |
                                      (!(stringr::str_detect(label, 'Median'))&!(stringr::str_detect(label, 'Average'))))

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
#'@importFrom rlang .data
#'@keywords internal
get_acs_msa <- function (table.names, years, acs.type, FIPS = c("14740","42660")) {

  census.data <- geography <- NAME <- name <- NULL
  for (table in table.names) {

    yearly.data <- NULL

    for (year in years) {

      # Download ACS Data
      tbl <- tidycensus::get_acs(geography="metropolitan statistical area/micropolitan statistical area", year=year, survey=acs.type, table=table) %>%
        dplyr::filter(GEOID %in% FIPS) %>%
        tidyr::separate(col=NAME, into=c("name", "state"),sep=",")
      tbl$state <- trimws(tbl$state, "l")

      # Add labels, column for Census Geography, Type and Year of Data
      tbl %<>% label_acs_variables(table, year, acs.type) %>%
        dplyr::mutate(census_geography="MSA", acs_type=acs.type, year=year) %>%


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
#' @importFrom rlang .data
#' @keywords internal

get_acs_place <- function (state="Washington", table.names, years, acs.type, place_FIPS=NULL) {

  census.data <- geography <- NAME <- name <- NULL

  for (table in table.names) {

    yearly.data <- NULL

    for (year in years) {
      # Determine Places within Region
      if(is.null(place_FIPS)){psrc_places <- get_psrc_places(year) %>% sf::st_drop_geometry() %>% unique() %>%
        dplyr::pull(GEOID)}

      # Download ACS Data
      tbl <- tidycensus::get_acs(state=state, geography='place', year=year, survey=acs.type, table=table) %>%
        tidyr::separate(col=NAME, into=c("name", "state"),sep=",")
      if(!is.null(place_FIPS)){tbl %<>% filter(GEOID %in% place_FIPS)}else{tbl %<>% filter(GEOID %in% psrc_places)}
      tbl$state <- trimws(tbl$state, "l")

      # Add labels, column for Census Geography, Type and Year of Data
      tbl %<>% label_acs_variables(table, year, acs.type) %>%
        dplyr::mutate(acs_type = acs.type, year=year) %>%
        dplyr::mutate(census_geography = dplyr::case_when(
          endsWith(name, "city") ~ "City",
          endsWith(name, "CDP") ~ "CDP")) %>%
        dplyr::mutate(name = gsub(" city", "", name)) %>%
        dplyr::mutate(name = gsub(" CDP", "", name))

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
#'@importFrom rlang .data
#'@keywords internal
get_acs_tract <- function (state="Washington", counties = c("King","Kitsap","Pierce","Snohomish"), table.names, years) {

  census.data <- geography <- NAME <- name <- county <- NULL
  for (table in table.names) {

    yearly.data <- NULL

    for (year in years) {

      # Download ACS Data
      tbl <- tidycensus::get_acs(state=state, county=counties, geography='tract', year=year, survey="acs5", table=table) %>%
        tidyr::separate(col=NAME, into=c("name", "county", "state"),sep=",")
      tbl$county <- trimws(tbl$county, "l")
      tbl$state <- trimws(tbl$state, "l")
      county.names <- paste(counties,"County")

      # Add label, column for Census Geography, Type and Year of Data
      tbl %<>% label_acs_variables(table, year, "acs5") %>%
        dplyr::mutate(census_geography="Tract", acs_type = "acs5", year=year) %>%
        dplyr::select(-county)


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
#'@importFrom rlang .data
#'@keywords internal
get_acs_blockgroup <- function (state="Washington", counties = c("King","Kitsap","Pierce","Snohomish"), table.names, years) {

  census.data <- geography <- NAME <- name <- county <- NULL
  for (table in table.names) {

    yearly.data <- NULL

    for (year in years) {

      # Download ACS Data
      tbl <- tidycensus::get_acs(state=state, county=counties, geography="block group", year=year, survey="acs5", table=table) %>%
        tidyr::separate(col=NAME, into=c("name", "tract", "county", "state"),sep=",")
      tbl$county <- trimws(tbl$county, "l")
      tbl$state <- trimws(tbl$state, "l")
      county.names <- paste(counties,"County")

      # Add labels, column for Census Geography, Type and Year of Data
      tbl %<>% label_acs_variables(table, year, "acs5") %>%
        dplyr::mutate(census_geography="block group", acs_type="acs5", year=year) %>%
        dplyr::select(-county)

      # Store yearly data into final yearly data for current table - append if a year already exists
      ifelse(is.null(yearly.data), yearly.data <- tbl, yearly.data <- dplyr::bind_rows(yearly.data, tbl))

    }

    # Store table data into final census data - append if a table set already exists
    ifelse(is.null(census.data), census.data <- yearly.data, census.data <- dplyr::bind_rows(census.data, yearly.data))

  }

  return(census.data)
}

#' Add estimate reliability information
#'
#' After gathering data, add reliability information using moe and estimate
#'
#' @param dfs acs data frames already retrieved from the api
#' @author Suzanne
#' @return the data frames with new columns for se, cv, and reliability
#' @export

reliability_calcs<- function(dfs){
  # A coefficient of variation (CV) measures the relative amount of sampling error that is associated with a sample       #estimate. The CV is calculated as the ratio of the SE for an estimate to the estimate itself and is usually
  #  expressed as a percent.
  # Note that since both the ACS and household travel survey are reported using a 90 percent confidence interval,         # where the Margin of Error (MOE) is reported in place of standard error, you can convert it to standard error by       # dividing by 1.645.
  # to do: put 1.645 in a better place in the package, it's a magic number
  # http://aws-linux/mediawiki/index.php/Understanding_Error_and_Determining_Statistical_Significance
  zscore_90<-1.645

  dfs%<>%dplyr::mutate(se=moe/zscore_90)%>%
    dplyr::mutate(cv= se/estimate)%>%
    dplyr::mutate(reliability=
                    dplyr::case_when(
                      estimate==0 ~ 'estimate is 0, cannot compute',
                      cv<=0.15 ~ 'good',
                      cv>0.15 & cv<=0.30 ~ 'fair',
                      cv>0.30 & cv<=0.50 ~ 'use with caution',
                      cv>0.50 ~ 'use with extreme caution',
                      .default = 'missing or N/A'
                    ))

  return(dfs)
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
#' @param place_FIPS Character string of FIPS codes (with state prefix) for specific Census Places. If NULL, Places within the PSRC Region will be returned.
#' @param acs.type A character string as either 'acs1', 'acs3' or acs5'.
#'
#' @author Craig Helmann
#'
#' @return a tibble of ACS estimates by selected geography for selected table codes. Includes variable names.
#' @examples
#' get_acs_recs(geography = 'county',
#'              table.names = c('B03002'),
#'              years=c(2017,2019),
#'              acs.type = 'acs1')
#'
#' get_acs_recs(geography = 'county',
#'              table.names = c("C17002"),
#'              counties=c("Kitsap"),
#'              years=c(2019),
#'              acs.type = 'acs5')
#'
#' @export
get_acs_recs <- function(geography, state="Washington", counties = c('King', 'Kitsap', 'Pierce', 'Snohomish'), table.names, years, FIPS = c("14740","42660"), place_FIPS=NULL, acs.type) {
  #this should probably be stored in different place, but I haven't figured out how
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

  dfs<-reliability_calcs(dfs)

  return(dfs)
}

#' Add shares to Psrccensus ACS object
#'
#' @param df dataframe with Census ACS result
#' @return dataframe with additional share and share_moe fields
#' @author Michael Jensen
#' @rawNamespace import(data.table, except = c(month, year))
#' @export
add_acs_share <- function(df){
  label <- x.estimate <- i.estimate <- x.moe <- i.moe <- concept <- share <- share_moe <- NULL
  input_type <- class(df)
  rs <- setDT(df)
  tots <- copy(rs) %>% .[grepl("Total:$",label)]
  rs %<>% .[tots, `:=`(share=x.estimate/i.estimate,
                       share_moe=tidycensus::moe_prop(x.estimate, i.estimate, x.moe, i.moe)),
            on=.(GEOID, concept, year)]
  if("data.table" %not_in% input_type){rs %<>% setDT()}
  return(rs)
}
