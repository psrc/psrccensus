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
#' elmer_connection <- DBI::dbConnect(odbc::odbc(),
#'                              driver = "SQL Server",
#'                              server = "AWS-PROD-SQL\\Sockeye",
#'                              database = "Elmer",
#'                              trusted_connection = "yes")
#'
#' poverty_tracts<-get_equity_geographies(elmer_connection, 'equity_geog_vs_reg_total', 'poverty_equity_geographies','2018')
#' means_transport<-get_acs_recs('tract',table.names=c('S0801_C01'),   years=c(2019),
#'                              acs.type = 'acs1')


#' tot_workers<-means_transport%>%filter(variable=='S0801_C01_001')
#' transit<-means_transport%>%filter(variable=='S0801_C01_009')

#' poverty_wrkrs<-merge(poverty_tracts, tot_workers, by.x='geoid', by.y='GEOID')
#' poverty_transit<-merge(poverty_wrkrs, transit, by.x='geoid', by.y='GEOID')

#' poverty_transit_calcs<-poverty_transit%>%mutate(transit=estimate.x*estimate.y/100)%>%
#' mutate(workers=estimate.x)%>%select('geoid', 'transit', 'workers', 'equity_geog_vs_reg_total')
#' poverty_transit_summary<-poverty_transit_calcs%>%group_by(equity_geog_vs_reg_total)%>%
#' summarise(sum_workers=sum(workers), sum_transit=sum(transit, na.rm=TRUE))%>%
#' mutate(transit_share=sum_transit/sum_workers)
#' poverty_transit_summary
#' @export
get_equity_geographies<- function(elmer_connection, equity_type='equity_geog_vs_50_percent',equity_group='racial_equity_geographies',year='2016'){
  query_geo=paste0('SELECT geoid, ', equity_type, ' FROM census.', equity_group,'(',year, ', \'Tract\')')
  table_equity_geo<-DBI::dbGetQuery(elmer_connection, DBI::SQL(query_geo))
  table_equity_geo
}
