#' @importFrom magrittr %<>% %>%
NULL

globalVariables(c(":=", "!!", ".", "enquos"))

`%not_in%` <- Negate(`%in%`)

#' Generic call for PUMS summary statistics
#'
#' Given specific form by related \code{\link{pums_stat}} functions.
#' @inheritParams pums_stat
#' @param stat_type Desired survey statistic
#' @return A summary tibble, including variable names, summary statistic and margin of error
#' @author Michael Jensen
#'
#' @importFrom tidyselect all_of where
#' @importFrom dplyr across if_all ungroup
#' @importFrom srvyr interact cascade survey_tally survey_total survey_median survey_mean survey_prop
psrc_pums_stat <- function(so, stat_type, stat_var, group_vars, incl_na=TRUE, rr=FALSE){
  count <- share <- COUNTY <- DATA_YEAR <- reliability <- NULL                                     # Bind variables locally (for documentation, not function)
  gv_pat <- paste0("^", group_vars ,"$", collapse="|")
  prefix <- if(stat_type %in% c("count","share")){""}else{paste0(stat_var,"_")}
  so %<>% mutate(across(.cols=where(is.numeric) & grep(gv_pat, colnames(.)), factor))              # Convert any numeric grouping variables to factor datatype
  if(all(group_vars!="keep_existing")){so %<>% ungroup()}                                          # "keep_existing" is power-user option for srvyr::combine() groupings;
  if(all(!is.null(group_vars) & group_vars!="keep_existing")){                                     # -- otherwise the package ungroups before and afterward
    if(incl_na==FALSE){so %<>% filter(if_all(all_of(group_vars), ~ !is.na(.)))}                    # Allows users to exclude w/o removing observations from the data object itself
    so %<>% srvyr::group_by(across(all_of(group_vars)))
  }
  if(stat_type=="count"){
    rs <- suppressMessages(cascade(so,
                                   count:=survey_total(na.rm=TRUE, vartype=c("se","cv")),
                                   share:=survey_prop(proportion=FALSE),
                                   .fill="Total"))
  }else if(stat_type=="median"){
    rs <- suppressMessages(cascade(so,
                                   !!paste0(prefix, "median"):=survey_median(!!as.name(stat_var),
                                                                             na.rm=TRUE,
                                                                             vartype=c("se","cv"),
                                                                             interval_type="quantile",
                                                                             qrule="school"),
                                   .fill="Total"))
  }else if(stat_type=="summary"){
    rs <- suppressMessages(cascade(so,
                                   count:=survey_total(na.rm=TRUE),
                                   share:=survey_prop(proportion=FALSE),
                                   !!paste0(prefix, "median"):=survey_median(!!as.name(stat_var),
                                                                             na.rm=TRUE,
                                                                             vartype=c("se","cv"),
                                                                             interval_type="quantile",
                                                                             qrule="school"),
                                   !!paste0(prefix, "mean"):=survey_mean(!!as.name(stat_var), na.rm=TRUE),
                                   .fill="Total"))
  }else{
    srvyrf_name <- as.name(paste0("survey_",stat_type))                                            # Specific srvyr function name
    rs <- suppressMessages(cascade(so,
                                   !!paste0(prefix, stat_type):=(as.function(!!srvyrf_name)(!!as.name(stat_var), na.rm=TRUE, vartype=c("se","cv"))),
                                   .fill="Total"))
  }
  rs %<>% setDT() %>%
    .[, grep("_se$", colnames(.)):=lapply(.SD, function(x) x * 1.645), .SDcols=grep("_se$", colnames(.))] %>%  # Convert standard error to MOE
    setnames(grep("_se$", colnames(.)), stringr::str_replace(grep("_se$", colnames(.), value=TRUE), "_se$", "_moe")) %>% # -- and revise column name to match
    setnames(grep("_cv", colnames(.)), "reliability")
  if(rr==TRUE){
    rs %<>% .[, reliability:=dplyr::case_when(reliability <= .15 ~ "good",                          # Optional categorical variance measure
                                              reliability <= .3  ~ "fair",
                                              reliability <= .5  ~ "weak",
                                              reliability >= .5  ~ "unreliable")]
  }else if(rr=="cv"){
    setnames(rs, "reliability", "cv")
  }else{
    rs[,reliability:=NULL]                                                                         # Drop if not selected
  }
  if("COUNTY" %not_in% colnames(rs)){rs[,COUNTY:="Region"]}
  if("DATA_YEAR" %not_in% colnames(rs)){rs[, DATA_YEAR:=unique(so[[7]]$DATA_YEAR)]}
  setcolorder(rs, c("DATA_YEAR","COUNTY"))
  setorderv(rs, c("DATA_YEAR","COUNTY"))
  rs[COUNTY=="Total", COUNTY:="Region"]
  rs[DATA_YEAR!="Total"]
  if(all(group_vars!="keep_existing")){so %<>% ungroup()}                                          # "keep_existing" is power-user option for srvyr::combine() groupings;
  return(rs)
}

#' PUMS summary statistics
#'
#' Separate function for total, count, median, mean
#'
#' @param so The srvyr object returned by \code{\link{get_psrc_pums}}
#' @param stat_var The numeric variable to summarize
#' @param group_vars Factor variable/s for grouping, as an UPPERCASE string element or list
#' @param incl_na option to remove NA from group_vars (if FALSE, the total may not reflect the full dataset)
#' @param rr optional relative reliability column, i.e. coefficient of variation as category levels (breakpoints: .15/.3./.5 -> good/fair/weak/unreliable)
#' @name pums_stat
#' @return A table with the variable names and labels, summary statistic and margin of error
#' @author Michael Jensen
NULL

#' @rdname pums_stat
#' @title Generate PUMS counts
#' @export
psrc_pums_count <- function(so, stat_var=NULL, group_vars=NULL, incl_na=TRUE, rr=FALSE){
  rs <- psrc_pums_stat(so=so, stat_type="count", stat_var=NULL, group_vars=group_vars, incl_na=incl_na, rr=rr)
  return(rs)
}
#' @rdname pums_stat
#' @title Generate PUMS sums
#' @export
psrc_pums_sum <- function(so, stat_var, group_vars=NULL, incl_na=TRUE, rr=FALSE){
  rs <- psrc_pums_stat(so, stat_type="total", stat_var=stat_var, group_vars=group_vars, incl_na=incl_na, rr=rr)
  return(rs)
}

#' @rdname pums_stat
#' @title Generate PUMS medians
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' so <- get_psrc_pums(1, 2019, "h", c("HINCP", "TEN"))
#' rs <- psrc_pums_median(so, "HINCP", "TEN")
#'}
#' @export
psrc_pums_median <- function(so, stat_var, group_vars=NULL, incl_na=TRUE, rr=FALSE){
  rs <- psrc_pums_stat(so=so, stat_type="median", stat_var=stat_var, group_vars=group_vars, incl_na=incl_na, rr=rr)
  return(rs)
}

#' @rdname pums_stat
#' @title Generate PUMS means
#' @export
psrc_pums_mean <- function(so, stat_var, group_vars=NULL, incl_na=TRUE, rr=FALSE){
  rs <- psrc_pums_stat(so=so, stat_type="mean", stat_var=stat_var, group_vars=group_vars, incl_na=incl_na, rr)
  return(rs)
}

#' @rdname pums_stat
#' @title Generate PUMS counts, sums, medians and means with one command
#' @export
psrc_pums_summary <- function(so, stat_var, group_vars=NULL, incl_na=TRUE, rr=FALSE){
  rs <- psrc_pums_stat(so=so, stat_type="summary", stat_var=stat_var, group_vars=group_vars, incl_na=incl_na, rr=rr)
  return(rs)
}

#' PUMS bulk summary statistics
#'
#' Generate a statistic separately for a list of grouping variables
#' List input items can be multiple, i.e. character vector
#'
#' @param so The srvyr object returned by \code{\link{get_psrc_pums}}
#' @param stat_type Desired survey statistic
#' @param stat_var The numeric variable to summarize
#' @param group_var_list List with each list item a grouping variable or set of grouping variables
#' @param incl_na option to remove NA from group_vars (if FALSE, the total may not reflect the full dataset)
#' @param rr optional relative reliability column, i.e. coefficient of variation as category levels (breakpoints: .15/.3./.5 -> good/fair/weak/unreliable)
#' @return A table with the variable names and labels, summary statistic and margin of error
#' @author Michael Jensen
#'
#' @importFrom data.table rbindlist
#' @importFrom dplyr mutate rename relocate
#' @export
pums_bulk_stat <- function(so, stat_type, stat_var=NULL, group_var_list, incl_na=TRUE, rr=FALSE){
  var_name <- DATA_YEAR <- COUNTY <- NULL                                                          # Bind variables locally (for documentation, not function)
  list_stat <- function(x){
    rsub <- psrc_pums_stat(so=so,
                           stat_type=stat_type,
                           stat_var=stat_var,
                           group_vars=x,
                           incl_na=incl_na,
                           rr=rr)
  }
  df <- list()
  df <- lapply(group_var_list, list_stat) %>%
    lapply(FUN=function(y){mutate(y, "var_name"=colnames(y)[3])}) %>% rbindlist(use.names=FALSE) %>%
    rename(var_value=colnames(.)[3]) %>% relocate(DATA_YEAR, COUNTY, var_name)
  return(df)
}

#' Z Score
#'
#' Stat to determine if two estimates are different
#'
#' @param x numeric vector, first estimate and corresponding MOE to compare
#' @param y numeric vector, second estimate and corresponding MOE to compare
#' @return Z score; if larger than 1, difference is significant
#' @author Michael Jensen
#'
#' @export
z_score <- function(x, y){
  z <- abs(x[1] - y[1]) / sqrt(x[2]^2 - y[2]^2)
  return(z)
}

#' Retrieve annual Personal Consumption Expenditures deflator ratio
#'
#' Applies PCE deflator to yield real values in specified dollar-year terms
#' Helpful when comparing between separate surveys
#' Requires [St. Louis Federal Reserve (FRED) API key](http://sboysel.github.io/fredr/articles/fredr.html#authentication)
#'
#' @param have_yr existing dollar year basis
#' @param want_yr dollar year to which you want the basis shifted
#' @author Michael Jensen
#'
#' @importFrom lubridate as_date
#'
#' @export
pce_deflator <- function(have_yr, want_yr){
  deflator <- NULL
  if(fredr::fredr_has_key()){
    drange <- c(have_yr, want_yr)
    x <- fredr::fredr(series_id = "DPCERG3A086NBEA",                                               # Requires FRED key in .Renviron, see fredr documentation
                      observation_start=as_date(paste0(min(drange),"-01-01")),
                      observation_end=as_date(paste0(max(drange),"-01-01")),
                      frequency="a")
    deflator <- last(x$value) / first(x$value)
    if(have_yr > want_yr){deflator <- 1 / deflator}                                                # Positions reversed if using basis older than reported.
  }
  return(deflator)
}

#' PUMS inflation adjustment to a year other than the survey year
#'
#' Applies PCE deflator to yield real values in specified dollar-year terms
#' Helpful when comparing between separate surveys
#' Requires [St. Louis Federal Reserve (FRED) API key](http://sboysel.github.io/fredr/articles/fredr.html#authentication)
#'
#' @param so srvyr object returned by get_psrc_pums
#' @param refyear dollar year in which value should be returned be returned
#' @author Michael Jensen
#'
#' @importFrom lubridate as_date
#' @importFrom dplyr mutate across all_of
#'
#' @export
real_dollars <- function(so, refyear){
  if(fredr::fredr_has_key()){
    dyear <- unique(so[[7]]$DATA_YEAR)
    deflator <- pce_deflator(dyear, refyear)
    income_cols <- grep("^FINCP$|^HINCP$|^INTP$|^OIP$|^PAP$|^PERNP$|^PINCP$|^RETP$|^SEMP$|^SSIP$|^SSP$|^WAGP$", colnames(so), value=TRUE)
    cost_cols <- grep("^CONP$|^ELEP$|^FULP$|^GASP$|^GRNTP$|^INSP$|^MHP$|^MRGP$|^SMOCP$|^RNTP$|^SMP$|^WATP$|^TAXAMT", colnames(so), value=TRUE)
    dollar_cols <- c(income_cols, cost_cols)
    if(length(dollar_cols)>0){
      so %<>% mutate(across(all_of(dollar_cols), ~ .x * deflator, .names=paste0("{.col}", refyear)))
    }
    return(so)
  }else{
    warning("You are missing a FRED key (see fredr documentation). No change made.")
    return(so)
  }
}
