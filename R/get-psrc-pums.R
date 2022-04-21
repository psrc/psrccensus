#' @importFrom magrittr %<>% %>%
#' @author Michael Jensen
NULL

globalVariables(c(":=", "!!", ".", "enquos"))

`%not_in%` <- Negate(`%in%`)

stuff <- function(x){unique(x) %>% paste(collapse=",")}

#' Search PUMS variable definitions
#'
#' Look for a var_code via search term
#' @param regex search term
#' @return data.table of filtered variable attributes
#'
#' @import data.table
#' @export
pums_varsearch <- function(regex){
  var_code <- var_label <- NULL                                                                    # Bind tidycensus::pums_variables variable locally (for documentation, not function)
  rs <- tidycensus::pums_variables %>% setDT() %>% .[, .(var_code, var_label)] %>%
    .[!grepl("flag$", var_label) & (grepl(regex, var_label, ignore.case=TRUE)|grepl(regex, var_code, ignore.case=TRUE))] %>% unique()
  return(rs)
}

#' NA recode for PUMS
#'
#' Helper to the \code{\link{get_psrc_pums}} function
#' @param dt data.table with Census Bureau "N/A" code--"b" or "bbb..."
#' @return filtered input data.table with values "b" recoded to NA
#'
#' @import data.table
pums_recode_na <- function(dt){
  for(col in colnames(dt)) set(dt, i=grep("^b+$|^N.A.$|^N.A.//$|^NA$|^$",dt[[col]]), j=col, value=NA) # Recode all PUMS NA variations to the NA R recognizes
  dt %<>% .[, which(unlist(lapply(., function(x)!all(is.na(x))))), with=FALSE]                     # Drop columns composed completely of N/A values
  return(dt)
}

#' Read PUMS csv
#'
#' Helper to the \code{\link{pums_ftp_gofer}} function
#' @param target_file either .csv from ftp site, or compressed .gz file on server
#' @param dyear The data year
#' @return unzipped table
#'
#' @import data.table
read_pums <- function(target_file, dyear){
  data_type <- var_code <- NULL                                                                    # Bind tidycensus::pums_variables variable locally (for documentation, not function)
  ddyear <- if(dyear>2016){dyear}else{2017}                                                        # To filter data dictionary; 2017 is earliest available
  type_lookup <- tidycensus::pums_variables %>% setDT() %>% .[year==ddyear] %>%
    .[, .(data_type=min(data_type)), by=var_code] %>% unique()                                     # Create datatype correspondence from data dictionary
  num_types <- copy(type_lookup) %>% .[data_type=="num", var_code] %>% paste()
  chr_types <- copy(type_lookup) %>% .[data_type=="chr", var_code] %>% paste()                     # For dyears in the dictionary, datatype from the dictionary
  if(dyear<2017){
    var_codes <- data.table::fread(target_file, sep=",", nrows=1, stringsAsFactors=FALSE)
    allwgts <- grep("^(P)?WGTP(\\d+)?$", colnames(var_codes), value=TRUE)
    num_types %<>% c(allwgts) %>% unique()
    chr_types <- colnames(var_codes) %>% .[. %not_in% num_types]                                   # For dyears before the dictionary, keep known numbers as numbers
    num_types <- colnames(var_codes) %>% .[. %not_in% chr_types]                                   # Reflect back so as not to specify variables that don't exist in that year
  }
  col_typelist <- list(character = chr_types, numeric = num_types)
  dt <- data.table::fread(target_file, sep=",", stringsAsFactors=FALSE, colClasses=col_typelist) %>% # The room where it happens; reads the file with correct datatypes
    filter2region(dyear)
  return(dt)
}

#' Filter PUMS to Region
#'
#' Helper to \code{\link{read_pums}} function
#' @param dt data.table
#' @param dyear The data year
#' @return filtered data.table
#'
#' @import data.table
filter2region <- function(dt, dyear){
  PUMA <- SERIALNO <- NULL                                                                         # Bind tidycensus::pums_variables variable locally (for documentation, not function)
  pumayr <- as.character(floor(dyear/10)*10) %>% stringr::str_sub(3,4) %>% paste0("PUMA",.)        # PUMA boundary version correlates to last diennial census
  dt %<>% pums_recode_na() %>%
    setnames(c(pumayr),c("PUMA"), skip_absent=TRUE) %>%                                            # Single PUMA column name
    .[, which(grepl("^PUMA\\d\\d", colnames(.))):=NULL] %>%                                        # Where multiple PUMA fields reported, use latest
    .[, colnames(.) %not_in% c("RT","DIVISION","REGION","ST"), with=FALSE] %>%                     # Drop variables static to our region
    setkey(SERIALNO)
  if(dyear>2011){
    dt %<>% .[(as.integer(PUMA) %/% 100) %in% c(115:118)]                                          # Filter to PSRC region
  }else if(dyear>=2000 & dyear<2012){
    dt %<>% .[(as.integer(PUMA) %/% 100) %in% c(15:18)]                                            # PUMAs renumbered in 2012
  }
  return(dt)
}

#' Fetch ZIP
#'
#' Helper to the \code{\link{pums_ftp_gofer}} function
#' @param zip_filepath ftp URL location
#' @param target_file .zip archive file to read
#' @param dyear The data year
#' @return unzipped table
fetch_zip <- function(zip_filepath, target_file, dyear){
  temp <- tempfile()
  download.file(zip_filepath, temp)
  unzip(temp, files=target_file, exdir=getwd())
  csv_target <- paste0(getwd(),"/", target_file)
  dt <- read_pums(target_file, dyear)
  unlink(temp)
  rm(temp)
  return(dt)
}

#' Fetch FTP
#'
#' Helper to the \code{\link{pums_ftp_gofer}} function
#' This is used on both the person and household table to prep them for join
#' @param span Either 1 for acs1 or 5 for acs5
#' @param dyear The data year
#' @param level Either "p" or "h", for "persons" or "households" respectively
#' @return data.table
#'
#' @import data.table
fetch_ftp <- function(span, dyear, level){
  subdir <- if(dyear>2006){paste0("/", as.character(span), "-Year/")}else{"/"}
  url <- paste0("https://www2.census.gov/programs-surveys/acs/data/pums/",                         # No multi-year PUMS before 2007
                as.character(dyear), subdir, "csv_", level, "wa.zip")                              # 3yr from 2007-13, 5yr from 2009-present
  csv_name <- dplyr::case_when(dyear>2016 ~paste0("psam_", level, "53.csv"),                       # Filename patterns depend on year
                               dyear==2000 ~paste0("c2ss", level, "wa.csv"),
                               dyear<2017 ~paste0("ss", stringr::str_sub(as.character(dyear),-2L), level, "wa.csv"))
  if(!httr::http_error(url)){                                                                      # First verify the target file exists
    dt <- fetch_zip(url, csv_name, dyear) %>%
      setDT()
    file.remove(csv_name)
    return(dt)
  }else{return(NULL)}
}

#' PUMS ftp go-fer
#'
#' Download & filter data from the Census Bureau Microdata ftp server
#' Helper to the \code{\link{get_psrc_pums}} function
#' @inheritParams fetch_ftp
#' @param vars PUMS variable/s as an UPPERCASE string element or list
#' @param dir Directory for .gz file, if already downloaded. Default NULL uses the Census ftp.
#' @return data.table with all requested variables,
#' sample & replication weights, and if needed, inflation adjustments
#'
#' @import data.table
pums_ftp_gofer <- function(span, dyear, level, vars, dir=NULL){
  PRACE <- HISP <- RAC1P <- SPORDER <- SERIALNO <- HRACE <- TYPE <- VACS <- NULL                   # Bind variables locally (for documentation, not function)
  unit_key <- if(level=="p"){c("SERIALNO","SPORDER")}else{"SERIALNO"}
  if(!is.null(dir)){                                                                               # For server tool; gz files already downloaded & filtered
    hfile <- paste0(dir,"/", dyear, "h", span, ".gz")
    pfile <- paste0(dir,"/", dyear, "p", span, ".gz")
    dt_h  <- suppressWarnings(read_pums(hfile, dyear))
    dt_p  <- suppressWarnings(read_pums(pfile, dyear))
  }else{
    dt_h <- suppressWarnings(fetch_ftp(span, dyear, "h"))                                          # Otherwise, ftp source
    dt_p <- suppressWarnings(fetch_ftp(span, dyear, "p")) %>%
      .[, PRACE:=fcase(as.integer(HISP)!=1, "H",                                                   # PSRC non-overlapping race category (Hispanic its own race)
                       RAC1P %in% c("3","4","5"), "I",
                       !is.na(RAC1P), RAC1P)]
  }
  setkeyv(dt_h, "SERIALNO")
  dt_p %<>% setkeyv("SERIALNO") %>%
    .[, which(grepl("^PUMA$|^ADJINC$|^ADJUST", colnames(.))):=NULL]                                # Remove duplicate columns
  tmp_p <- copy(dt_p) %>% .[!is.na(SPORDER), .(SERIALNO, PRACE)]                                                                                                  #     not as.integer because non-number values exist
  hh_me <- tmp_p[, .(HRACE=stuff(PRACE)), by=.(SERIALNO)] %>% setkey("SERIALNO")                   # Summarize households by race/ethnic composition
  hh_me[(HRACE %like% ","), HRACE:="M"]                                                            # - Characterize multiracial
  dt_h %<>% merge(hh_me, by="SERIALNO", all.x=TRUE)
  adjvars <- if("ADJINC" %in% colnames(dt_h)){c("ADJINC","ADJHSG")}else{"ADJUST"}
  dt_h[, (adjvars):=lapply(.SD, function(x){as.numeric(x)/1000000}), .SDcols=adjvars]              # Adjustment factors in ftp version without decimal
  if(level=="h"){                                                                                  # For household analysis:                                                               #    filter out GQ or vacant units &
    dt_p %<>% .[as.integer(SPORDER)==1]                                                            #  - keep only householder person attributes
    dt <- merge(dt_h, dt_p, by="SERIALNO", all.x=TRUE) %>% .[TYPE==1 & is.na(VACS)]                #  - filter out GQ & vacant
  }else if(level=="p"){                                                                            # For population analysis, keep only individuals
    dt <- merge(dt_p, dt_h, by="SERIALNO", all.x=TRUE) %>% .[!is.na(SPORDER)]
  }
  if("BINCOME" %in% vars){dt %<>% psrc_bincome()}                                                  # See psrc-pums-groupings for custom binned variables
  if("BIN_AGE" %in% vars){dt %<>% psrc_bin_age()}                                                  # - "
  if("BIN_POVRATIO" %in% vars){dt %<>% psrc_bin_povratio()}                                        # - "
  if("OWN_RENT" %in% vars){dt %<>% psrc_own_rent()}                                                # - "
  if("BIN_YBL" %in% vars){dt %<>% psrc_bin_ybl(dyear)}                                             # - "
  swgt <- if(level=="p"){"PWGTP"}else{"WGTP"}                                                      # Specify sample weight
  setnames(dt, toupper(names(dt)))                                                                 # All column names to uppercase
  wgtrgx <- paste0("^",swgt,"\\d+$")
  rwgt <- grep(wgtrgx, colnames(dt), value=TRUE)                                                   # Specify replication weights
  varlist <- c(unlist(unit_key),"PUMA", unlist(vars), swgt, rwgt, adjvars) %>% unique()            # Columns to keep
  if("BIN_YBL" %in% vars){varlist <- varlist[!(varlist %in% "BIN_YBL")] %>% c("YBL")}              # Swap; to be used later
  dt %<>% .[, colnames(.) %in% varlist, with=FALSE]                                                # Keep only specified columns
  dt[, `:=`(DATA_YEAR=dyear, PRODUCT=paste0("acs", span), UNIT=level)]                             # Add fields to identify the dataset
  dt[, (unit_key):=lapply(.SD, as.character), .SDcols=unit_key]                                    # Confirm datatype for keys (fread may return int for early years)
  return(dt)
}

#' PUMS API go-fer
#'
#' Call the Census Bureau Microdata API
#' Helper to the \code{\link{get_psrc_pums}} function
#' @inheritParams pums_ftp_gofer
#' @return data.table with all requested variables,
#' sample & replication weights, and if needed, inflation adjustments
#'
#' @import data.table
pums_api_gofer <- function(span, dyear, level, vars){
  varlist <- unlist(vars) %>% c("ADJINC","ADJHSG") %>% unique()                                    # Include adjustment variables
  vf         <- list(TYPE=1, SPORDER=1)
  tbl_ref    <- if(level=="p"){"person"}else{"housing"}
  dt <- tidycensus::get_pums(variables=varlist,                                                    # Include inflation adjustment fields
                             state="WA",
                             puma = c(11501:11520,11601:11630,11701:11720,11801:11810),            # Generous list, i.e. isn't limited to existing PUMAs
                             year=dyear, survey=paste0("acs", span),
                             variables_filter=if(tbl_ref=="housing"){vf}else{NULL},                # Household variables filter for occupied housing, not GQ or vacant
                             recode=FALSE,
                             rep_weights=tbl_ref) %>%                                              # Replication weights for the appropriate table
    setDT() %>% pums_recode_na()
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
  adj_inc <- if("ADJINC" %in% colnames(dt)){"ADJINC"}else{"ADJUST"}
  adj_hsg <- if("ADJHSG" %in% colnames(dt)){"ADJHSG"}else{"ADJUST"}
  adj_cols <- c(adj_inc, adj_hsg) %>% unique()
  dt[, (adj_cols):=lapply(.SD, as.numeric), .SDcols=adj_cols]                                      # Why they arrive as strings, I have no idea
  income_cols <- grep("^FINCP$|^HINCP$|^INTP$|^OIP$|^PAP$|^PERNP$|^PINCP$|^RETP$|^SEMP$|^SSIP$|^SSP$|^WAGP$", colnames(dt), value=TRUE)
  cost_cols <- grep("^CONP$|^ELEP$|^FULP$|^GASP$|^GRNTP$|^INSP$|^MHP$|^MRGP$|^SMOCP$|^RNTP$|^SMP$|^WATP$|^TAXAMT", colnames(dt), value=TRUE)
  if(length(income_cols)>0){
    dt[, (income_cols):=lapply(.SD, function(x) {as.numeric(x) * get(adj_inc)}), .SDcols=income_cols] # Adjust income variables
  }
  if(length(cost_cols)>0){
    dt[, (cost_cols):=lapply(.SD, function(x) {as.numeric(x) * get(adj_hsg)}), .SDcols=cost_cols]  # Adjust cost variables
  }
  dt %<>% .[, which(grepl("^ADJINC$|^ADJHSG$|^ADJUST$", colnames(.))):=NULL]                       # Drop inflation adjustment variables
  return(dt)
}

#' Add County Name to PUMS API Result
#'
#' Helper to \code{\link{get_psrc_pums}} function.
#' Attaches county name.
#' @param dt PSRC data.table
#' @param dyear The data year
#' @return PSRC data.table with county names
#'
#' @import data.table
add_county <- function(dt, dyear){
  PUMA <- COUNTY <- NULL                                                                           # Bind variables locally (for documentation, not function)
  PUMA3 <- if(dyear>2011){c(115:118)}else if(dyear<=2011 & dyear>=2000){c(15:18)}                  # PUMAs renumbered in 2012
  county_lookup <- data.frame(PUMA3, COUNTY=as.factor(c("Pierce","King","Snohomish","Kitsap"))) %>%
    setDT() %>% setkey(PUMA3)
  dt %<>% .[, PUMA3:=(as.integer(PUMA) %/% 100)] %>%
    .[county_lookup, COUNTY:=COUNTY, on=.(PUMA3=PUMA3)] %>%
    .[, PUMA3:=NULL]
  return(dt)
}

#' Swap variable codes for labels
#'
#' Helper to \code{\link{get_psrc_pums}} function.
#' Delivers labels instead of values, where labels exist
#' @param dt PSRC data.table
#' @param dyear The data year
#' @param vars PUMS variable/s as an UPPERCASE string element or list
#' @return PSRC data.table with labels
#'
#' @import data.table
codes2labels <- function(dt, dyear, vars){
  recode <- val_min <- val_max <- var_code <- val_label <- i.val_label <- NULL                     # Bind variables locally (for documentation, not function)
  ddyear  <- if(dyear>2016){dyear}else{2017}
  recoder <- list()
  recoder[[1]] <- tidycensus::pums_variables %>% setDT() %>%                                       # Get the value-label correspondence for any/all factor variables
    .[recode==TRUE & val_min==val_max & year==ddyear, .(var_code, val_max, val_label)] %>%
    unique()
  recoder[[2]] <- copy(recoder[[1]]) %>% .[var_code=="RAC1P" & val_max %not_in% c("3","4","5")] %>%
    rbind(list(
      c(rep("",3)),
      c("I","H","M"),
      c("American Indian or Alaskan Native Alone", "Hispanic or Latino","Multiple Races"))) %>%    # PSRC non-overlapping race category (Hispanic as a race)
    .[,var_code:="HRACE"]
  recoder[[3]] <- copy(recoder[[2]]) %>% .[, var_code:="PRACE"]
  recoder %<>% rbindlist() %>% setDT() %>% .[var_code %in% vars]%>% setkeyv("val_max")             # Add to label lookup; filter variables
  recode_vars <- recoder$var_code %>% unique()
  if(nrow(recoder)>0){
    for (v in recode_vars){
      setkeyv(dt, v)
      dt[recoder[var_code==v], (v):=as.factor(i.val_label)]                                        # The room where it happens; group_vars to label (from value) if relevant/available
    }
  }
  return(dt)
}

#' Confirm correct datatypes for group variables and weights
#'
#' Helper to \code{\link{get_psrc_pums}} function.
#' Makes certain weights are numeric and grouping variables are factors
#' @param dt PSRC data.table
#' @return PSRC data.table with types confirmed
#'
#' @import data.table
ensure_datatypes <- function(dt){
  allwgts <- grep("^WGTP(\\d+)?$|^PWGTP(\\d+)?$", colnames(dt), value=TRUE)
  dt[, (allwgts):=lapply(.SD, as.numeric), .SDcols=allwgts]                                        # Ensure weights are numeric
  ftr_vars <- dt[1, sapply(dt, is.character), with=FALSE] %>% colnames() %>%
    .[. %not_in% c("SERIALNO","SPORDER","PUMA","DATA_YEAR","PRODUCT","UNIT")]
  if(length(ftr_vars)>0){
    dt[, (ftr_vars):=lapply(.SD, as.factor), .SDcols=ftr_vars]                                     # Ensure grouping variables are factors (required by srvyr)
  }
  return(dt)
}


#' Retrieve and assemble PUMS data
#'
#' The primary PUMS function
#' @inheritParams pums_ftp_gofer
#' @param labels Default TRUE, recodes varible values to the corresponding label
#' @return A srvyr object with appropriate sampling weight and replication weights
#'
#' @importFrom tidyselect all_of
#' @import data.table
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")
#' get_psrc_pums(span=1, dyear=2019, level="p", vars=c("AGEP","SEX"))}
#'
#' @export
get_psrc_pums <- function(span, dyear, level, vars, dir=NULL, labels=TRUE){
  vf <- list(TYPE=1, SPORDER=1)
  unit_var <- if(level=="p"){c("SERIALNO","SPORDER")}else{"SERIALNO"}
  dt <- pums_ftp_gofer(span, dyear, level, vars, dir)
  swgt <- if(level=="p"){"PWGTP"}else{"WGTP"}                                                      # Specify sample weight
  rwgt <- paste0(swgt, 1:80)                                                                       # Specify replication weights
  dt %<>% add_county(dyear) %>% setcolorder(c(unit_var, "COUNTY")) %>%
    adjust_dollars()                                                                               # Apply standard inflation adjustment
  if(labels==TRUE){dt %<>% codes2labels(dyear, vars)}                                              # Replace codes with labels where available
  if("BIN_YBL" %in% vars){dt %<>% psrc_bin_ybl()}
  dt %<>% ensure_datatypes()                                                                       # Confirm correct datatypes for weights and group_vars
  varlist <- c(unlist(unit_var), "DATA_YEAR", "PRODUCT", "UNIT", "COUNTY", unlist(vars)) %>% unique()
  dt %<>% setDF() %>% dplyr::relocate(all_of(varlist)) %>%
    srvyr::as_survey_rep(variables=varlist,                                                        # Create srvyr object with replication weights for MOE
                         weights=swgt,
                         repweights=rwgt,
                         combined_weights=TRUE,
                         mse=TRUE,
                         type="other",
                         scale=4/80,
                         rscale=rep(1,80))
  return(dt)
}

#' Generic call for PUMS summary statistics
#'
#' Given specific form by related \code{\link{pums_stat}} functions.
#' @inheritParams pums_stat
#' @param stat_type Desired survey statistic
#' @return A summary tibble, including variable names, summary statistic and margin of error
#'
#' @importFrom rlang sym
#' @importFrom srvyr interact cascade survey_tally survey_total survey_median survey_mean survey_prop
psrc_pums_stat <- function(so, stat_type, stat_var, group_vars){

  count <- share <- COUNTY <- DATA_YEAR <- NULL                                                    # Bind variables locally (for documentation, not function)
  prefix <- if(stat_type %in% c("count","share")){""}else{paste0(stat_var,"_")}
  so %<>% dplyr::ungroup()
  if(!is.null(group_vars)){
    so %<>% srvyr::group_by(dplyr::across(!!!enquos(group_vars)))
  }
  if(stat_type=="count"){
    rs <- suppressMessages(cascade(so,
                                   count:=survey_total(na.rm=TRUE),
                                   share:=survey_prop(),
                                   .fill="Total"))
  }else if(stat_type=="median"){
    rs <- suppressMessages(cascade(so,
                                   !!paste0(prefix, "median"):=survey_median(!!as.name(stat_var), na.rm=TRUE, interval_type = "quantile", qrule="school"),
                                   .fill="Total"))
  }else if(stat_type=="summary"){
    rs <- suppressMessages(cascade(so,
                                   count:=survey_total(na.rm=TRUE),
                                   share:=survey_prop(),
                                   !!paste0(prefix, "median"):=survey_median(!!as.name(stat_var), na.rm=TRUE, interval_type = "quantile", qrule="school"),
                                   !!paste0(prefix, "mean"):=survey_mean(!!as.name(stat_var), na.rm=TRUE),
                                   .fill="Total"))
  }else{
    srvyrf_name <- as.name(paste0("survey_",stat_type))                                            # Specific srvyr function name
    rs <- suppressMessages(cascade(so,
                                   !!paste0(prefix, stat_type):=(as.function(!!srvyrf_name)(!!as.name(stat_var), na.rm=TRUE)),
                                   .fill="Total"))
  }
  rs %<>% purrr::modify_if(is.factor, as.character) %>% setDT() %>%
    .[, grep("_se", colnames(.)):=lapply(.SD, function(x) x * 1.645), .SDcols=grep("_se", colnames(.))] %>%
    setnames(grep("_se", colnames(.)), stringr::str_replace(grep("_se", colnames(.), value=TRUE), "_se", "_moe"))
  if("COUNTY" %not_in% colnames(rs)){rs[,COUNTY:="Region"]}
  if("DATA_YEAR" %not_in% colnames(rs)){rs[, DATA_YEAR:=unique(so[[7]]$DATA_YEAR)]}
  setcolorder(rs, c("DATA_YEAR","COUNTY"))
  setorderv(rs, c("DATA_YEAR","COUNTY"))
  rs[COUNTY=="Total", COUNTY:="Region"]
  rs[DATA_YEAR!="Total"]
  so %<>% dplyr::ungroup()
  return(rs)
}

#' PUMS summary statistics
#'
#' Separate function for total, count, median, mean
#'
#' @param so The srvyr object returned by \code{\link{get_psrc_pums}}
#' @param stat_var The numeric variable to summarize
#' @param group_vars Factor variable/s for grouping, as an UPPERCASE string element or list
#' @name pums_stat
#' @return A table with the variable names and labels, summary statistic and margin of error
NULL

#' @rdname pums_stat
#' @title Generate PUMS county and regional counts
#' @export
psrc_pums_count <- function(so, stat_var=NULL, group_vars=NULL){
  rs <- psrc_pums_stat(so=so, stat_type="count", stat_var=NULL, group_vars=group_vars)
  return(rs)
}
#' @rdname pums_stat
#' @title Generate PUMS county and regional totals
#' @export
psrc_pums_sum <- function(so, stat_var, group_vars=NULL){
  rs <- psrc_pums_stat(so, stat_type="total", stat_var=stat_var, group_vars=group_vars)
  return(rs)
}

#' @rdname pums_stat
#' @title Generate PUMS county and regional medians
#'
#' @examples
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")
#' library(magrittr)
#' so <- get_psrc_pums(1, 2019, "h", c("HINCP", "TEN"))
#' rs <- psrc_pums_median(so, "HINCP", "TEN")
#'}
#' @export
psrc_pums_median <- function(so, stat_var, group_vars=NULL){
  rs <- psrc_pums_stat(so=so, stat_type="median", stat_var=stat_var, group_vars=group_vars)
  return(rs)
}

#' @rdname pums_stat
#' @title Generate PUMS county and regional means
#' @export
psrc_pums_mean <- function(so, stat_var, group_vars=NULL){
  rs <- psrc_pums_stat(so=so, stat_type="mean", stat_var=stat_var, group_vars=group_vars)
  return(rs)
}

#' @rdname pums_stat
#' @title Generate PUMS county and regional means
#' @export
psrc_pums_summary <- function(so, stat_var, group_vars=NULL){
  rs <- psrc_pums_stat(so=so, stat_type="summary", stat_var=stat_var, group_vars=group_vars)
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
#' @return A table with the variable names and labels, summary statistic and margin of error
#'
#' @importFrom data.table rbindlist setDF
#' @importFrom dplyr mutate rename relocate
#' @export
pums_bulk_stat <- function(so, stat_type, stat_var=NULL, group_var_list){
  var_name <- NULL                                                                                 # Bind variables locally (for documentation, not function)
  list_stat <- function(x){
    rsub <- psrc_pums_stat(so=so,
                           stat_type=stat_type,
                           stat_var=stat_var,
                           group_vars=x)
  }
  df <- list()
  df <- lapply(group_var_list, list_stat) %>%
    lapply(FUN=function(y){mutate(y, "var_name"=colnames(y)[1])}) %>% rbindlist(use.names=FALSE) %>%
    rename(var_value=colnames(.)[1]) %>% relocate(var_name)
  return(df)
}

#' Z Score
#'
#' Stat to determine if two estimates are different
#'
#' @param x numeric vector, first estimate and corresponding MOE to compare
#' @param y numeric vector, second estimate and corresponding MOE to compare
#' @return Z score; if larger than 1, difference is significant
#'
#' @export
z_score <- function(x, y){
  z <- abs(x[1] - y[1]) / sqrt(x[2]^2 - y[2]^2)
  return(z)
}
