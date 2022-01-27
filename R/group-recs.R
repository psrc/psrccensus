#' Group ACS or Census Data
#'
#' Group ACS to Census Data into pre-defined commonly used groupings, stored in the file: inst/extdata/variables_groupings.
#'
#' @param tbl A data frame of census or acs data
#' @param this_group_name A character string that describes the grouping of the data. Should match the group_name in the variables_cats table
#'
#' @author Suzanne Childress
#'
#' @return a tibble of grouped ACS or Census estimates
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#'
#' inc_poverty<-get_acs_recs(geography = 'county',
#'                          table.names = c('C17002'),
#'                          years=c(2019),
#'                          acs.type = 'acs5')
#' group_recs(inc_poverty, 'Poverty Group 100 Percent')
#' @export
group_recs <- function(tbl, this_group_name){
  # this is kind of a hard code for the file name and location, may want to revisit
  variables_groupings<-read.csv(system.file('extdata', 'variables_groupings.csv', package='psrccensus'))
  #variables_groupings<-read.csv('C:/Users/SChildress/Documents/GitHub/psrccensus/inst/extdata/variables_groupings.csv')

  this_variable_grouping<- variables_groupings%>%dplyr::filter(.data$group_name==!!this_group_name)

  tbl_w_cats<-dplyr::left_join(tbl, this_variable_grouping, by ='variable') %>%
    tidyr::drop_na(.data$grouping)

  #the column names between acs and census are slightly different
  # for acs:
  if("estimate" %in% colnames(tbl_w_cats)){
    tbl_grouped <- tbl_w_cats %>%
      dplyr::group_by(dplyr::across(c(.data$GEOID, .data$name, .data$grouping, .data$group_name)))%>%
      dplyr::summarise(estimate=sum(.data$estimate),
                       moe=tidycensus::moe_sum(.data$moe,.data$estimate, na.rm=TRUE))
  }
  # for census:
  else{
    tbl_grouped <- tbl_w_cats%>%
      dplyr::group_by(dplyr::across(-c(.data$value,.data$label, .data$variable )))%>%
      dplyr::summarise(value=sum(.data$value))

  }
  tbl_grouped

}
