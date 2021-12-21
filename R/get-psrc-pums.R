#' Inflation adjustment for PUMS
#'
#' This is a helper function for the adjust_dollars function
#' @param dt The data table as conditioned by adjust_dollars function
#' @param dollar_var The reported PUMS variable to adjust
#' @param adj_var The PUMS adjustment factor: either ADJINC for income or ADJHSG for housing cost
#'
#' @author Michael Jensen
#'
#' @return the data.table with the input variable adjusted
#'
#' @importFrom magrittr %<>% %>%
#' @import data.table

adjust_inflation <-function(dt, dollar_var, adj_var){
  dt %<>% .[, (dollar_var):=round(as.numeric(gsub(",", "", get(dollar_var))) * as.numeric(get(adj_var)),0)]
  return(dt)
}

#' Dollar variable adjustment for PUMS
#'
#' This helper function is used in both the psrc_pums_groupvar and psrc_pums_targetvar functions.
#' It matches the corresponding adjustment factor with the appropriate PUMS variable and applies the adjust_inflation function.
#' @param dt The data table
#' @param dollar_var The reported PUMS variable to adjust
#'
#' @author Michael Jensen
#'
#' @return the data.table with dollar value adjusted
#'
#' @importFrom magrittr %<>% %>%
#' @import data.table

adjust_dollars <- function(dt, dollar_var){
  if(dollar_var %chin% c("FINCP","HINCP","INTP","OIP","PAP","PERNP","PINCP","RETP","SEMP","SSIP","SSP","WAGP")){
    dt %<>% adjust_inflation(dollar_var, "ADJINC")                                                 # Applied to income variables (either person or housing table)
  }
  if(dollar_var %chin% c("CONP","ELEP","FULP","GASP","GRNTP","INSP","MHP","MRGP","SMOCP","RNTP","SMP","WATP","TAXAMT")){
    dt %<>% adjust_inflation(dollar_var, "ADJHSG")                                                 # Applied to housing cost variables (housing table)
  }
  dt %<>% .[, c("ADJINC","ADJHSG"):=NULL]
  return(dt)
}

#' Restrict PUMS API pull to PSRC region
#'
#' This helper function is used in both the psrc_pums_groupvar and psrc_pums_targetvar functions.
#' It filters the Washington State API pull to the PSRC region and removes State/label columns
#' @param dt The data table
#'
#' @author Michael Jensen
#'
#' @return the filtered data.table
#'
#' @importFrom magrittr %<>% %>%
#' @import data.table

clip2region <- function(dt){
  dt %<>% .[as.integer(PUMA) %/% 100 %in% c(115,116,117,118)] %>%
    .[, grep(("^ST$|^ST_label$"), colnames(.)):=NULL]
  return(dt)
}

#' Add County Name to PUMS API Result
#'
#' This helper function is used in both the psrc_pums_groupvar and psrc_pums_targetvar functions.
#' It attaches the county name.
#' @param dt The data table
#'
#' @author Michael Jensen
#'
#' @return the filtered data.table
#'
#' @importFrom magrittr %<>% %>%
#' @import data.table

add_county <- function(dt){
  county_lookup <- data.frame(PUMA3=c(115,116,117,118),
                              COUNTY=c("Pierce","King","Snohomish","Kitsap")) %>%
    setDT() %>% setkey(PUMA3)
  dt %<>% .[, PUMA3:=(as.integer(PUMA) %/% 100)] %>%
    .[county_lookup, COUNTY:=COUNTY, on=.(PUMA3=PUMA3)]
  return(dt)
}

#' Process the PUMS Grouping Variable
#'
#' This is one of two primary helper functions to the get_psrc_pums data assembly function.
#' It calls the Census API to process the optional grouping variable.
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param group_var The exact PUMS variable intended for grouping, as a string in UPPERCASE
#' @param tbl_ref Either "person" or "housing", as determined by the get_psrc_pums function
#' @param bin_defs Optional argument: if a single number, used as Ntile; if a list, used as custom bin breakpoints
#'
#' @author Michael Jensen
#'
#' @return the filtered data.table
#'
#' @importFrom magrittr %<>% %>%
#' @import data.table

psrc_pums_groupvar <- function(span, dyear, group_var, tbl_ref, bin_defs){
  dt   <- tidycensus::get_pums(variables=c(group_var,"PUMA","ADJINC","ADJHSG"), state="WA",
                   survey=paste0("acs", span), year=dyear, recode=if(dyear>2016){TRUE}else{FALSE}) %>% # Recode isn't available prior to 2017
    setDT() %>% clip2region() %>% adjust_dollars(group_var)
  if(exists("df$SPORDER") & tbl_ref!="person"){
    dt %<>% .[SPORDER==1] %>% .[, SPORDER:=NULL]                                                   # If target variable is household and group variable is person,
  }                                                                                                # uses 'householder' attributes, per convention (albeit odd)
#  if(!is.null(bin_defs) & length(bin_defs)>1){
#    dt %<>% .[, (group_var):=cut(group_var, breaks=bin_defs, right=T, labels=F)]                  # Manual grouping categories
#  }else if(!is.null(bin_defs) & length(bin_defs)==1){
#    dt %<>% .[, (group_var):=ntile(group_var, bin_defs)]                                          # Ntile grouping categories
#  }
 return(dt)
  }

#' Process the PUMS Target Variable
#'
#' This is one of two primary helper functions to the get_psrc_pums data assembly function.
#' It calls the Census API to process the primary variable used for summarization.
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param tbl_ref Either "person" or "housing", as determined by the get_psrc_pums function
#'
#' @author Michael Jensen
#'
#' @return the filtered data.table
#'
#' @importFrom magrittr %<>% %>%

psrc_pums_targetvar <- function(span, dyear, target_var, tbl_ref){
  dt <- tidycensus::get_pums(variables=c(target_var,"PUMA","ADJINC","ADJHSG"), state="WA",         # Include inflation adjustment fields
                 survey=paste0("acs", span), year=dyear, recode=if(dyear>2016){TRUE}else{FALSE},   # Recode unavailable prior to 2017
                 rep_weights=tbl_ref) %>% data.table::setDT() %>% clip2region() %>%                # Replication weights for the appropriate table
    adjust_dollars(target_var)
  return(dt)
}

#' Assemble the requested PUMS data
#'
#' This is the main assembly function.
#' It draws from psrc_pums_targetvar and psrc_pums_groupvar and feeds the specific summary stat calls.
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param group_var The exact PUMS variable intended for grouping, as a string in UPPERCASE
#' @param bin_defs Optional argument: if a single number, used as Ntile; if a list, used as custom bin breakpoints
#'
#' @author Michael Jensen
#'
#' @return A srvyr object, absent grouping but otherwise ready for summation.
#'
#' @importFrom magrittr %<>% %>%
#' @import data.table
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#' get_psrc_pums("region", 1, 2019, "AGEP", "SEX")
#' @export
get_psrc_pums <- function(span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  varlist       <- c(target_var)
  pums_vars     <- tidycensus::pums_variables %>% setDT() %>% .[year==dyear & survey==paste0("acs", span)] # Retrieve variable definitions
  tbl_ref       <- pums_vars[var_code==target_var, unique(level)]                                  # Table corresponding to unit of analysis (for rep weights)
  key_ref       <- pums_vars[var_code==group_var, unique(level)]                                   # Table corresponding to grouping variable (for join)
  dt_key        <- if(tbl_ref=="person" & key_ref!="housing"){c("SERIALNO","SPORDER")
  }else{"SERIALNO"}                                                                                # To match join
  rwgt_ref      <- if(tbl_ref=="person"){"PWGTP"}else{"WGTP"}
  dt <- psrc_pums_targetvar(span, dyear, target_var, tbl_ref) %>% add_county()                     # Target variable via API
  rw <- colnames(dt) %>% .[grep(paste0(rwgt_ref,"\\d+"),.)]
  if(!is.null(group_var)){
    groupvar_label <- paste0(group_var,"_label")
    varlist %<>% c(groupvar_label)
    group_var_dt <- psrc_pums_groupvar(span, dyear, group_var, tbl_ref, bin_defs=NULL) %>%         # Grouping variable via API
      setkeyv(dt_key)
    dt %<>% setkeyv(dt_key) %>% .[group_var_dt, (groupvar_label):=get(groupvar_label), on=key(.)]  # Link data tables
  }
  dt %<>% setDF() %>%
    srvyr::as_survey_rep(variables=all_of(varlist),                                                # Create srvyr object with replication weights for MOE
                         weights=all_of(rwgt_ref),
                         repweights=all_of(rw),
                         combined_weights=TRUE,
                         mse=TRUE,
                         type="other",
                         scale=4/80,
                         rscale=length(all_of(rw)))
  return(dt)
}

#' Genericized Call for PUMS Summary Statistics
#'
#' This function is given specific form by related median/mean region/county functions.
#' It draws from get_psrc_pums.
#' @param stat_type Desired survey statistic
#' @param geo_scale Either "county" or "region"
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param group_var The exact PUMS variable intended for grouping, as a string in UPPERCASE
#' @param bin_defs Optional argument: if a single number, used as Ntile; if a list, used as custom bin breakpoints
#'
#' @author Michael Jensen
#'
#' @return A table with the variable names, desired statistic, and margin of error
#'
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang sym
#' @import dplyr srvyr
psrc_pums_stat <- function(stat_type, geo_scale, span, dyear, target_var, group_var, bin_defs){
  result_name <- sym(stat_type)
  srvyrf_name <- as.name(paste0("survey_",stat_type))
  se_name     <- paste0(stat_type,"_se")
  moe_name    <- paste0(stat_type,"_moe")
  df <- get_psrc_pums(span, dyear, target_var, group_var, bin_defs)
  if(!is.null(group_var) & stat_type!="ratio"){
    groupvar_label <- paste0(group_var,"_label")
    dt %<>% group_by(!!as.name(groupvar_label))
    }
  if(geo_scale=="county"){
    df %<>% group_by(COUNTY, .add=TRUE)
    }
  rs <- summarize(df, !!result_name:=(as.function(!!srvyrf_name)(!!as.name(target_var), vartype="se", level=0.95))) %>%  # Generate the weighted statistic
    mutate(!!sym(moe_name):=!!sym(se_name) * 1.645) %>% select(-se_name)
  return(rs)
}

#' Regional PUMS total
#'
#' This function uses psrc_pums_stat specific to the regional total.
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param group_var The exact PUMS variable intended for grouping, as a string in UPPERCASE
#' @param bin_defs Optional argument: if a single number, used as Ntile; if a list, used as custom bin breakpoints
#'
#' @author Michael Jensen
#'
#' @return A table with the variable names, regional total, and margin of error
#'
#' @importFrom magrittr %<>% %>%
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#' psrc_pums_total(1, 2019, "AGEP", "SEX")
#' @export
psrc_pums_total <- function(span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  rs <- psrc_pums_stat(stat_type="total", geo_scale="region", span, dyear, target_var, group_var, bin_defs)
  return(rs)
}

#' Regional PUMS count
#'
#' This function uses psrc_pums_stat specific to the regional count.
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param group_var The exact PUMS variable intended for grouping, as a string in UPPERCASE
#' @param bin_defs Optional argument: if a single number, used as Ntile; if a list, used as custom bin breakpoints
#'
#' @author Michael Jensen
#'
#' @return A table with the variable names, regional count (i.e. tally), and margin of error
#'
#' @importFrom magrittr %<>% %>%
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#' psrc_pums_count(1, 2019, "AGEP", "SEX")
#' @export
psrc_pums_count <- function(span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  rs <- psrc_pums_stat("tally", "region", span, dyear, target_var, group_var, bin_defs)
  return(rs)
}

#' Regional PUMS median
#'
#' This function uses psrc_pums_stat specific to the regional median.
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param group_var The exact PUMS variable intended for grouping, as a string in UPPERCASE
#' @param bin_defs Optional argument: if a single number, used as Ntile; if a list, used as custom bin breakpoints
#'
#' @author Michael Jensen
#'
#' @return A table with the variable names, regional median, and margin of error
#'
#' @importFrom magrittr %<>% %>%
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#' psrc_pums_median(1, 2019, "AGEP", "SEX")
#' @export
psrc_pums_median <- function(span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  rs <- psrc_pums_stat("median", "region", span, dyear, target_var, group_var, bin_defs)
  return(rs)
}

#' Regional PUMS mean
#'
#' This function uses psrc_pums_stat specific to the regional mean.
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param group_var The exact PUMS variable intended for grouping, as a string in UPPERCASE
#' @param bin_defs Optional argument: if a single number, used as Ntile; if a list, used as custom bin breakpoints
#'
#' @author Michael Jensen
#'
#' @return A table with the variable names, regional mean, and margin of error
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
# psrc_pums_mean(1, 2019, "AGEP", "SEX")
#' @export
psrc_pums_mean <- function(span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  rs <- psrc_pums_stat("mean", "region", span, dyear, target_var, group_var, bin_defs)
  return(rs)
}

#' Regional PUMS ratio
#'
#' This function uses psrc_pums_stat specific to the regional mean.
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param numerator The exact PUMS target variable intended as numerator, as a string in UPPERCASE
#' @param denominator The exact PUMS variable intended as denominator, as a string in UPPERCASE
#'
#' @author Michael Jensen
#'
#' @return A table with the variable names, regional mean, and margin of error
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
# psrc_pums_ratio(1, 2019, "RNTP", "HINCP")
#' @export
psrc_pums_ratio <- function(span, dyear, numerator, denominator){
  rs <- psrc_pums_stat("ratio", "region", span, dyear, numerator, denominator)
  return(rs)
}

#' County PUMS totals
#'
#' This function uses psrc_pums_stat specific to county totals.
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param group_var The exact PUMS variable intended for grouping, as a string in UPPERCASE
#' @param bin_defs Optional argument: if a single number, used as Ntile; if a list, used as custom bin breakpoints
#'
#' @author Michael Jensen
#'
#' @return A table with the variable names, PSRC county totals, and margin of error
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
# county_pums_total(1, 2019, "AGEP", "SEX")
#' @export
county_pums_total <- function(span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  rs <- psrc_pums_stat("total", "county", span, dyear, target_var, group_var, bin_defs)
  return(rs)
}

#' County PUMS count
#'
#' This function uses psrc_pums_stat specific to county unit counts.
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param group_var The exact PUMS variable intended for grouping, as a string in UPPERCASE
#' @param bin_defs Optional argument: if a single number, used as Ntile; if a list, used as custom bin breakpoints
#'
#' @author Michael Jensen
#'
#' @return A table with the variable names, PSRC county unit counts (i.e. tallies), and margin of error
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#' county_pums_count(1, 2019, "AGEP", "SEX")
#' @export
county_pums_count <- function(span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  rs <- psrc_pums_stat("tally", "county", span, dyear, target_var, group_var, bin_defs)
  return(rs)
}

#' County PUMS median
#'
#' This function uses psrc_pums_stat specific to county medians.
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param group_var The exact PUMS variable intended for grouping, as a string in UPPERCASE
#' @param bin_defs Optional argument: if a single number, used as Ntile; if a list, used as custom bin breakpoints
#'
#' @author Michael Jensen
#'
#' @return A table with the variable names, PSRC county medians, and margin of error
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#' county_pums_median(1, 2019, "AGEP", "SEX")
#' @export
county_pums_median <- function(span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  rs <- psrc_pums_stat("median", "county", span, dyear, target_var, group_var, bin_defs)
  return(rs)
}

#' County PUMS mean
#'
#' This function uses psrc_pums_stat specific to county means.
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param group_var The exact PUMS variable intended for grouping, as a string in UPPERCASE
#' @param bin_defs Optional argument: if a single number, used as Ntile; if a list, used as custom bin breakpoints
#'
#' @author Michael Jensen
#'
#' @return A table with the variable names, PSRC county means, and margin of error
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#' county_pums_mean(1, 2019, "AGEP", "SEX")
#' @export
county_pums_mean <- function(span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  rs <- psrc_pums_stat("mean", "county", span, dyear, target_var, group_var, bin_defs)
  return(rs)
}

#' County PUMS ratio
#'
#' This function uses psrc_pums_stat specific to county ratios.
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param numerator The exact PUMS target variable intended as numerator, as a string in UPPERCASE
#' @param denominator The exact PUMS variable intended as denominator, as a string in UPPERCASE
#'
#' @author Michael Jensen
#'
#' @return A table with the variable names, county ratios, and margin of error
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#' psrc_pums_ratio(1, 2019, "RNTP", "HINCP")
#' @export
psrc_pums_ratio <- function(span, dyear, numerator, denominator){
  rs <- psrc_pums_stat("ratio", "county", span, dyear, numerator, denominator)
  return(rs)
}
