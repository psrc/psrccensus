#' Group ACS or Census Data
#'
#' Group ACS to Census Data into pre-defined commonly used groupings, stored in the file: variables_groupings.
#'
#' @param tbl A data frame of census or acs data
#' @param variables_groupings A data frame that provides look-ups between the variables and their categories
#' @param group_name A character string that describes the grouping of the data. Should match the group_name in the variables_cats table
#'
#' @author Suzanne Childress
#'
#' @return a tibble of grouped ACS or Census estimates
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#'
#' age_sex_acs<-get_acs_recs(geography = 'county',
#'                      table.names = c('B01001'),
#'                      years=c(2019),
#'                      acs.type = 'acs1')
#  group_recs(age_sex_acs, 'Broad Age')
#'
#' sex_age_census<-get_decennial_recs(geography = 'county',
#'                                   table_codes = c('P012'),
#'                                   year=2010)
#'
#' group_recs(sex_age_census, 'Broad Age')

#'
#' income_acs<-get_acs_recs(geography = 'county',
#' table.names = c('B19001'),
#' years=c(2019),
#' acs.type = 'acs1')
#' group_recs(income_acs, 'Broad Income')
#'
#' @export
group_recs <- function(tbl, group_name){
  variables_groupings<-system.file('extdata', 'variables_groupings.csv', package='psrccensus')
  tbl_w_cats<-merge(tbl, variables_groupings)

  #the column names between acs and census are slightly different
  # for acs:
  if("estimate" %in% colnames(tbl_w_cats)){
    tbl_grouped <- tbl_w_cats%>%
      dplyr::group_by(dplyr::across(-c(estimate, moe,label, variable )))%>%
      dplyr::summarise(estimate=sum(estimate),
                       moe=tidycensus::moe_sum(moe,estimate))
  }
  # for census:
  else{
    tbl_grouped <- tbl_w_cats%>%
      dplyr::group_by(dplyr::across(-c(value,label, variable )))%>%
      dplyr::summarise(value=sum(value))

  }
  tbl_grouped

}
