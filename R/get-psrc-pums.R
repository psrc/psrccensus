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
    pums_recode_na() %>% add_county() %>% setcolorder(c(unit_var, "COUNTY"))
  rw <- colnames(dt) %>% .[grep(paste0(rwgt_ref,"\\d+"),.)]                                        # Specify replication weights
  if(dollar_adj==TRUE){dt%<>% adjust_dollars()}                                                    # Apply standard inflation adjustment
  recoder <-  tidycensus::pums_variables %>% setDT() %>%
    .[var_code %in% vars & recode==TRUE & val_min==val_max, .(var_code, val_max, val_label)] %>%
    unique() %>% setkeyv("val_max")                                                                # Get the value-label correspondence for any/all factor variables
  ftr_vars <- recoder$var_code %>% unique()
  nonnum_vars <- ftr_vars %>% unlist() %>% c(unit_var)
  num_vars <- vars[vars %not_in% nonnum_vars]
  if(nrow(recoder)>0){
    for (v in ftr_vars){
        setkeyv(dt, v)
        dt[recoder[var_code==v], (v):=as.factor(i.val_label)]                                      # Convert group_vars to label if relevant/available
      }
  }
  dt[, (num_vars):=lapply(.SD, as.numeric), .SDcols=num_vars]                                      # Ensure variables to summarize are numeric
  dt[, (ftr_vars):=lapply(.SD, as.factor), .SDcols=ftr_vars]                                       # Ensure grouping variables are factors (required by srvyr)
  varlist <- c(unit_var, "COUNTY", unlist(vars)) %>% unique()
  dt %<>% setDF() %>% dplyr::relocate(all_of(varlist)) %>%
    srvyr::as_survey_rep(variables=all_of(varlist),                                                # Create srvyr object with replication weights for MOE
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
#' Given specific form by related \code{\link{pums_stat}} functions.
#' @param so The srvyr object returned by \code{\link{get_psrc_pums}}
#' @param stat_type Desired survey statistic
#' @param target_var Numeric PUMS analysis variable, as an UPPERCASE string
#' @param group_vars Factor variable/s for grouping, as an UPPERCASE string element or list
#' @return A summary tibble, including variable names, summary statistic and margin of error
#'
#' @importFrom rlang sym
#' @importFrom srvyr interact cascade survey_tally survey_total survey_median survey_mean survey_prop
psrc_pums_stat <- function(so, stat_type, target_var, group_vars){
  prefix <- if(stat_type %in% c("count","share")){""}else{paste0(target_var,"_")}
  so %<>% dplyr::ungroup()
  if(!is.null(group_vars)){
    so %<>% srvyr::group_by(dplyr::across(tidyselect::all_of(group_vars)))                         # Apply grouping
  }
  so %<>% srvyr::group_by(COUNTY, .add=TRUE)
  if(stat_type=="count"){
    rs <- cascade(so, count:=survey_total(na.rm=TRUE, vartype="se", level=0.90))
  }else if(stat_type=="summary"){
    rs <- cascade(so, count:=survey_total(na.rm=TRUE, vartype="se", level=0.90),
            !!paste0(prefix,"total"):=survey_total(!!as.name(target_var), na.rm=TRUE, vartype="se", level=0.90),
            !!paste0(prefix,"median"):=survey_median(!!as.name(target_var), na.rm=TRUE, vartype="se", level=0.90),
            !!paste0(prefix,"mean"):=survey_mean(!!as.name(target_var), na.rm=TRUE, vartype="se", level=0.90))
  }else{
    srvyrf_name <- as.name(paste0("survey_",stat_type))                                            # specific srvyr function name
    rs <- cascade(so,
            !!paste0(prefix, stat_type):=(as.function(!!srvyrf_name)(!!as.name(target_var), na.rm=TRUE, vartype="se", level=0.90)))
  }
  rs %<>% purrr::modify_if(is.factor, as.character) %>% setDT() %>%
    .[, grep("_se", colnames(.)):=lapply(.SD, function(x) x * 1.645), .SDcols=grep("_se", colnames(.))] %>%
    setnames(grep("_se", colnames(.)), stringr::str_replace(grep("_se", colnames(.), value=TRUE), "_se", "_moe"))
  setcolorder(rs, c("COUNTY"))
  setorder(rs, COUNTY, na.last=TRUE)
  rs[is.na(COUNTY), COUNTY:="Region"]
  if(!is.null(group_vars)){
    rs[, (group_vars):=lapply(.SD, function(x) {x[is.na(x)] <- "Total" ; x}), .SDcols=group_vars]
  }
  so %<>% dplyr::ungroup()
  return(rs)
}

#' PUMS summary statistics
#'
#' Separate function for total, count, median, mean
#'
#' @param so The srvyr object returned by \code{\link{get_psrc_pums}}
#' @param target_var The exact PUMS target variable intended, as a string in UPPERCASE
#' @param group_vars Factor variable/s for grouping, as an UPPERCASE string element or list
#' @name pums_stat
#' @return A table with the variable names and labels, summary statistic and margin of error
NULL

#' @rdname pums_stat
#' @title Generate PUMS county and regional counts
#' @export
psrc_pums_count <- function(so, target_var=NULL, group_vars=NULL){
  rs <- psrc_pums_stat(so=so, stat_type="count", target_var=NULL, group_vars=group_vars)
  return(rs)
}
#' @rdname pums_stat
#' @title Generate PUMS county and regional totals
#' @export
psrc_pums_sum <- function(so, target_var, group_vars=NULL){
  rs <- psrc_pums_stat(so, stat_type="total", target_var=target_var, group_vars=group_vars)
  return(rs)
}

#' @rdname pums_stat
#' @title Generate PUMS county and regional medians
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")}
#' get_psrc_pums(1, 2019, "h", "HINCP", "TEN") %>% psrc_pums_median("HINCP", "TEN")
#'
#' @export
psrc_pums_median <- function(so, target_var, group_vars=NULL){
  rs <- psrc_pums_stat(so=so, stat_type="median", target_var=target_var, group_vars=group_vars)
  return(rs)
}

#' @rdname pums_stat
#' @title Generate PUMS county and regional means
#' @export
psrc_pums_mean <- function(so, target_var, group_vars=NULL){
  rs <- psrc_pums_stat(so=so, stat_type="mean", target_var=target_var, group_vars=group_vars)
  return(rs)
}

#' @rdname pums_stat
#' @title Generate PUMS county and regional means
#' @export
psrc_pums_summary <- function(so, target_var, group_vars=NULL){
  rs <- psrc_pums_stat(so=so, stat_type="summary", target_var=target_var, group_vars=group_vars)
  return(rs)
}
