#' Get equity tracts
#'
#' Retrieve a 0-1 variable for whether each tract in the region is in an equity geography or not
#'
#' @param elmer_connection A DBI connection object created from the library odbc
#' @param equity_type string, The type of equity geography either 'equity_geog_vs_50_percent' or 'equity_geog_vs_reg_total'
#' @param equity_group string, The group of people described by the equity geography, options are:'racial_equity_geographies', 'poverty_equity_geographies',
#' 'disability_equity_geographies', 'elderly_equity_geographies', 'limited_english_equity_geographies'
#' @param year string, the year for which the equity geography is defined
#' @author Suzanne Childress
#'
#' @return a data frame with all tracts in the region with a 0-1 variable for whether they are in an equity geography or not
#'
#'
#' @examples
#' library(odbc)
#' library(DBI)
#' library(dplyr)
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")
#' }
#' elmer_connection <- dbConnect(odbc::odbc(),
#'                              driver = "SQL Server",
#'                              server = "AWS-PROD-SQL\\Sockeye",
#'                              database = "Elmer",
#'                              trusted_connection = "yes")
#'
#' poverty_tracts<-get_equity_geographies(elmer_connection, 'equity_geog_vs_reg_total', 'poverty_equity_geographies','2018')
#' head(poverty_tracts)
#' @export
get_equity_geographies<- function(elmer_connection, equity_type='equity_geog_vs_50_percent',equity_group='racial_equity_geographies',year='2016'){
  query_geo=paste0('SELECT geoid, ', equity_type, ' FROM census.', equity_group,'(',year, ', \'Tract\')')
  table_equity_geo<-DBI::dbGetQuery(elmer_connection, DBI::SQL(query_geo))
  table_equity_geo
}
