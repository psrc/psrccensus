#' Get equity tracts
#'
#' Retrieve a 0-1 variable for whether each tract in the region is in an equity geography or not
#'
#' @param equity_type string, The type of equity geography either '50' or 'regavg'
#' @param equity_group string, The group of people described by the equity geography, options are:
#'                             'race',
#'                             'poverty',
#'                             'disability',
#'                             'elderly',
#'                             'limited_english'
#' @param year string, the year for which the equity geography is defined
#' @author Suzanne Childress
#'
#' @return a data frame with all tracts in the region with a 0-1 variable for whether they are in an equity geography or not
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")
#' }
#'
#'get_equity_geographies(equity_type = 'regavg', equity_group = 'disability', year = '2019')
#'
#' @export
get_equity_geographies <- function(equity_type = '50', equity_group = 'race', year = '2019'){
  # determine column suffix (type)
  type <- switch(equity_type,
                 "50" = "vs_50_percent",
                 "regavg" = "vs_reg_average")

  group <- switch(equity_group,
                  'race' = 'poc',
                  'poverty' = 'in_poverty',
                  'disability' = 'disabled',
                  'elderly' = 'older',
                  'limited_english' = 'lep')

  column_name <- paste('equity_geog', group, type, sep = "_")
  if(nchar(column_name) > 31) column_name <- stringr::str_sub(column_name, 1, 31)
  column_names <- paste0("geoid,", column_name)
  arc_root <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
  url <- paste0(arc_root,
                "/equity_tracts_2019/FeatureServer/0/query/?where=0=0&outFields=",
                column_names,
                "&f=pgeojson")
  equity_tracts <- sf::st_read(url) %>% sf::st_drop_geometry()
}
