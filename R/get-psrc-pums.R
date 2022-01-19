#' @importFrom magrittr %<>% %>%
#' @author Michael Jensen
NULL

`%not_in%` <- Negate(`%in%`)

#' NA recode for PUMS
#'
#' Helper to the \code{\link{get_psrc_pums}} function
#' @param dt data.table with Census Bureau "N/A" code--"b" or "bbb..."
#' @return filtered input data.table with values "b" recoded to NA
#'
#' @import data.table
pums_recode_na <- function(dt){
  for(col in colnames(dt)) set(dt, i=grep("^b+$",dt[[col]]), j=col, value=NA)
  return(dt)
}

#' Dollar variable adjustment for PUMS
#'
#' Helper to \code{\link{get_psrc_pums}} function.
#' Applies the Census Bureau-specified inflation adjustment to dollar values. See vignette for brief discussion.
#' Option to bypass this inflation adjustment exists, in order to match estimates generated without it.
#' @param dt The data.table
#' @return full input data.table with all dollar values adjusted
#'
#' @import data.table
adjust_dollars <- function(dt){
  dt[, c("ADJINC","ADJHSG"):=lapply(.SD, as.numeric), .SDcols=c("ADJINC","ADJHSG")]                # Why they arrive as strings, I have no idea
  income_cols <- grep("^FINCP$|^HINCP$|^INTP$|^OIP$|^PAP$|^PERNP$|^PINCP$|^RETP$|^SEMP$|^SSIP$|^SSP$|^WAGP$", colnames(dt), value=TRUE)
  cost_cols <- grep("CONP$|^ELEP$|^FULP$|^GASP$|^GRNTP$|^INSP$|^MHP$|^MRGP$|^SMOCP$|^RNTP$|^SMP$|^WATP$|^TAXAMT", colnames(dt), value=TRUE)
  if(length(income_cols)>0){
    dt[, (income_cols):=lapply(.SD, function(x) x * ADJINC), .SDcols=income_cols]                  # Adjust income variables
  }
  if(length(cost_cols)>0){
    .[, (cost_cols):=lapply(.SD, function(x) x * ADJHSG), .SDcols=cost_cols]                       # Adjust cost variables
  }
  dt[, grep("^ADJINC$|^ADJHSG$", colnames(dt)):=NULL]                                              # Drop inflation adjustment variables
  return(dt)
}

#' Add County Name to PUMS API Result
#'
#' Helper to \code{\link{get_psrc_pums}} function.
#' Attaches county name.
#' @param dt PSRC data.table
#' @return PSRC data.table with county names
#'
#' @import data.table

add_county <- function(dt){
  county_lookup <- data.frame(PUMA3=c(115,116,117,118),
                              COUNTY=as.factor(c("Pierce","King","Snohomish","Kitsap"))) %>%
    setDT() %>% setkey(PUMA3)
  dt %<>% .[, PUMA3:=(as.integer(PUMA) %/% 100)] %>%
    .[county_lookup, COUNTY:=COUNTY, on=.(PUMA3=PUMA3)]
  return(dt)
}

#' Retrieve and assemble PUMS data
#'
#' The primary PUMS function
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param level Either "p" or "h", for "persons" or "households" respectively
#' @param vars PUMS variable/s as an UPPERCASE string element or list
#' @param dollar_adj Default TRUE; adjust income and cost values for inflation using the averaged factors provided with PUMS
#' @return A srvyr object with appropriate sampling weight and replication weights
#'
#' @importFrom tidyselect all_of
#' @import data.table
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#' get_psrc_pums(span=1, dyear=2019, level="p", vars=c("AGEP","SEX"))
#'
#' @export
get_psrc_pums <- function(span, dyear, level, vars, dollar_adj=TRUE){
  varlist <- if(dollar_adj==TRUE){
    unlist(vars) %>% c("ADJINC","ADJHSG") %>% unique()                                 # Include adjustment variables
  }else{vars}
  vf         <- list(TYPE=1, SPORDER=1)
  tbl_ref    <- if(level=="p"){"person"}else{"housing"}
  rwgt_ref   <- if(tbl_ref=="person"){"PWGTP"}else{"WGTP"}
  unit_var   <- if(tbl_ref=="person"){"SPORDER"}else{"SERIALNO"}
  key_var    <- c("SERIALNO", unit_var) %>% unique()
  dt <- tidycensus::get_pums(variables=varlist,                                                    # Include inflation adjustment fields
                             state="WA",
                             puma = c(11501:11520,11601:11630,11701:11720,11801:11810),            # Generous list, i.e. isn't limited to existing PUMAs
                             year=dyear, survey=paste0("acs", span),
                             variables_filter=if(tbl_ref=="housing"){vf}else{NULL},                # Household variables filter for occupied housing, not GQ or vacant
                             recode=FALSE,
                             rep_weights=tbl_ref) %>% setDT() %>%                                  # Replication weights for the appropriate table
    pums_recode_na() %>% add_county()
  rw <- colnames(dt) %>% .[grep(paste0(rwgt_ref,"\\d+"),.)]                                        # Specify replication weights
  if(dollar_adj==TRUE){dt%<>% adjust_dollars()}                                                    # Apply standard inflation adjustment
  recoder <-  tidycensus::pums_variables %>% setDT() %>%
    .[var_code %in% vars & recode==TRUE & val_min==val_max, .(var_code, val_max, val_label)] %>%
    unique() %>% setkeyv("val_max")                                                                # Get the value-label correspondence for any/all factor variables
  ftr_vars <- recoder$var_code %>% unique()
  nonnum_vars <- ftr_vars %>% unlist() %>% c(unit_var)
  num_vars <- vars[vars %not_in% nonnum_vars]
  dt[, (num_vars):=lapply(.SD, as.numeric), .SDcols=num_vars]                                      # Ensure non-factor, non-key columns are numeric
  if(nrow(recoder)>0){
    for (v in ftr_vars){
        setkeyv(dt, v)
        dt[recoder[var_code==v], (v):=as.factor(i.val_label)]                                      # Convert group_var to label if relevant/available
      }
    }
  varlist <- unlist(vars) %>% c("COUNTY", unit_var) %>% unique()
  dt %<>% setDF() %>% srvyr::as_survey_rep(variables=all_of(varlist),                              # Create srvyr object with replication weights for MOE
                                           weights=all_of(rwgt_ref),
                                           repweights=all_of(rw),
                                           combined_weights=TRUE,
                                           mse=TRUE,
                                           type="other",
                                           scale=4/80,
                                           rscale=rep(1:length(all_of(rw))))
  return(dt)
}

#' Generic call for PUMS summary statistics
#'
#' Given specific form by related \code{\link{regional_pums_stat}} and \code{\link{county_pums_stat}} functions.
#' @param so The srvyr object returned by \code{\link{get_psrc_pums}}
#' @param target_var Numeric PUMS analysis variable, as an UPPERCASE string
#' @param group_var Factor variable/s for grouping, as an UPPERCASE string element or list
#' @param stat_type Desired survey statistic
#' @param geo_scale Either "county" or "region"
#' @return A summary tibble, including variable names, summary statistic and margin of error
#'
#' @importFrom rlang sym
#' @importFrom srvyr summarise survey_tally survey_total survey_median survey_mean
psrc_pums_stat <- function(so, target_var, group_vars, stat_type, geo_scale){
  srvyrf_name <- as.name(paste0("survey_",stat_type))                                              # specific srvyr function name
  se_name     <- paste0(stat_type,"_se")                                                           # specific srvyr standard error field
  moe_name    <- paste0(stat_type,"_moe")                                                          # margin of error
  so %<>% dplyr::ungroup()
  if(!is.null(group_vars)){
    so %<>% dplyr::group_by(across(all_of(group_vars)), .drop=FALSE)                                # Apply grouping
  }
  if(geo_scale=="county"){so %<>% dplyr::group_by(COUNTY, .add=TRUE)}
  if(stat_type=="count"){
    rs <- survey_tally(so, name="count", vartype="se")
  }else{
    rs <- summarise(so, !!stat_type:=(as.function(!!srvyrf_name)(!!as.name(target_var), na.rm=TRUE, vartype="se", level=0.90)))
  }
  rs %<>% dplyr::mutate(!!sym(moe_name):=eval(sym(se_name)) * 1.645) %>%
    dplyr::select(-!!as.name(se_name)) %>% dplyr::arrange(.by_group = TRUE)
  if(geo_scale=="county"){
    rs %<>% dplyr::relocate(COUNTY) %>% dplyr::arrange(COUNTY)
  }
  so %<>% dplyr::ungroup()
  return(rs)
}

#' Regional PUMS summary statistics
#'
#' Separate function for total, count, median, or mean
#'
#' @param so The srvyr object returned by \code{\link{get_psrc_pums}}
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param group_var Factor variable/s for grouping, as an UPPERCASE string element or list
#' @name regional_pums_stat
#' @return A table with the variable names and labels, summary statistic and margin of error
NULL

#' @rdname regional_pums_stat
#' @title Generate regional PUMS totals
#' @export
psrc_pums_total <- function(so, target_var, group_vars=NULL){
  rs <- psrc_pums_stat(so, target_var, group_vars, "total", "region")
  return(rs)
}

#' @rdname regional_pums_stat
#' @title Generate regional PUMS count
#' @export
psrc_pums_count <- function(so, group_vars=NULL){
  rs <- psrc_pums_stat(so, group_vars, "count", "region")
  return(rs)
}

#' @rdname regional_pums_stat
#' @title Generate regional PUMS median
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#' get_psrc_pums(1, 2019, "h", "HINCP", "TEN") %>% psrc_pums_median("HINCP", "TEN")
#'
#' @export
psrc_pums_median <- function(so, target_var, group_vars=NULL){
  rs <- psrc_pums_stat(so, target_var, group_vars, "median", "region")
  return(rs)
}

#' @rdname regional_pums_stat
#' @title Generate regional PUMS mean
#' @export
psrc_pums_mean <- function(so, target_var, group_vars=NULL){
  rs <- psrc_pums_stat(so, target_var, group_vars, "mean", "region")
  return(rs)
}

#' PUMS summary statistics by county
#'
#' Separate function for total, count, median, or mean
#'
#' @param so The srvyr object returned by \code{\link{get_psrc_pums}}
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param group_var Factor variable/s for grouping, as an UPPERCASE string element or list
#' @name county_pums_stat
#' @return A table with the counties, variable names and labels, summary statistic, and margin of error
NULL

#' @rdname county_pums_stat
#' @title Generate PUMS totals by county
#' @export
county_pums_total <- function(so, target_var, group_vars=NULL){
  rs <- psrc_pums_stat(so, target_var, group_vars, "total", "county")
  return(rs)
}

#' @rdname county_pums_stat
#' @title Generate PUMS counts <count> by county
#' @export
county_pums_count <- function(so, group_vars=NULL){
  rs <- psrc_pums_stat(so, group_vars, "count", "region")
  return(rs)
}

#' @rdname county_pums_stat
#' @title Generate PUMS medians by county
#'
#' @examples
#' \dontrun{Sys.getenv("CENSUS_API_KEY")}
#' get_psrc_pums(1, 2019, "p", AGEP") %>% county_pums_median("AGEP")
#'
#' @export
county_pums_median <- function(so, target_var, group_vars=NULL){
  rs <- psrc_pums_stat(so, target_var, group_vars, "median", "county")
  return(rs)
}

#' @rdname county_pums_stat
#' @title Generate PUMS averages <mean> by county
#' @export
county_pums_mean <- function(so, target_var, group_vars=NULL){
  rs <- psrc_pums_stat(so, target_var, group_vars, "mean", "county")
}
