#' @importFrom magrittr %<>% %>%
NULL

globalVariables(c(":=", "!!", ".", "enquos"))

`%not_in%` <- Negate(`%in%`)

stuff <- function(x){unique(x) %>% paste(collapse=",")}

#' Search PUMS variable definitions
#'
#' Look for a var_code via search term
#' @param regex search term
#' @author Michael Jensen
#'
#' @examples
#' # All variables related to internet; shows code changed in 2020 survey
#' pums_varsearch("internet")
#'
#' # Entry for specific variable, 'Employment Status Recode'
#' pums_varsearch("^ESR$")
#'
#' @rawNamespace import(data.table, except = c(month, year))
#' @export
pums_varsearch <- function(regex){
  var_code <- var_label <- NULL                                                                    # Bind tidycensus::pums_variables variable locally (for documentation, not function)
  rs <- tidycensus::pums_variables %>% setDT() %>% .[, .(years=stuff(year)), by=.(var_code, var_label)] %>%
    .[!grepl("flag$", var_label) & (grepl(regex, var_label, ignore.case=TRUE)|grepl(regex, var_code, ignore.case=TRUE))] %>% unique()
  return(rs)
}

#' NA recode for PUMS
#'
#' Helper to the \code{\link{get_psrc_pums}} function
#' @param dt data.table with Census Bureau "N/A" code--"b" or "bbb..."
#' @return filtered input data.table with values "b" recoded to NA
#' @author Michael Jensen
#'
#' @rawNamespace import(data.table, except = c(month, year))
pums_recode_na <- function(dt){
  for(col in colnames(dt)) set(dt, i=grep("^b+$|^N.A.(\\/)?|^NA$|^$",dt[[col]]), j=col, value=NA) # Recode all PUMS NA variations to the NA R recognizes
  dt %<>% .[, which(unlist(lapply(., function(x)!all(is.na(x))))), with=FALSE]                     # Drop columns composed completely of N/A values
  return(dt)
}

#' Read PUMS csv
#'
#' Helper to the \code{\link{pums_ftp_gofer}} function
#' @param target_file either .csv from ftp site, or compressed .gz file on server
#' @param dyear The data year
#' @return unzipped table
#' @author Michael Jensen
#'
#' @rawNamespace import(data.table, except = c(month, year))
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
#' @author Michael Jensen
#'
#' @rawNamespace import(data.table, except = c(month, year))
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
    dt %<>% .[(as.integer(PUMA) %/% 100) %in% c(14,10,17,20)]                                            # PUMAs renumbered in 2012
  }
  return(dt)
}

#' Fetch ZIP
#'
#' Helper to the \code{\link{pums_ftp_gofer}} function
#' @param zip_filepath ftp URL location
#' @param target_file .zip archive file to read
#' @param dyear The data year
#'
#' @return unzipped table
#' @author Michael Jensen
fetch_zip <- function(zip_filepath, target_file, dyear){
  options(download.file.method="libcurl", url.method="libcurl", timeout=300)
  temp1 <- tempfile()
  curl::curl_download(zip_filepath, temp1, quiet=FALSE)
  temp2 <- utils::unzip(temp1, target_file, exdir=tempdir())
  dt <- read_pums(temp2, dyear)
  unlink(temp1)
  basename(temp2) %>% unlink(recursive=TRUE)
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
#' @author Michael Jensen
#'
#' @rawNamespace import(data.table, except = c(month, year))
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
#' @author Michael Jensen
#'
#' @rawNamespace import(data.table, except = c(month, year))
pums_ftp_gofer <- function(span, dyear, level, vars, dir=NULL){
  SPORDER <- SERIALNO <- TYPE <- TYPEHUGQ <- VACS <- DIS <- NULL
  RAC1P <- PRACE <- ARACE <- HRACE <- HISP <- AGEP <- WORKER <- COW <- NULL

  # Bind variables locally (for documentation, not function)
  unit_key <- if(level %in% c("p","persons")){c("SERIALNO","SPORDER")}else{"SERIALNO"}
  if(!is.null(dir)){                                                                               # For server tool; gz files already downloaded & filtered
    hfile <- paste0(dir,"/", dyear, "h", span, ".gz")
    pfile <- paste0(dir,"/", dyear, "p", span, ".gz")
    dt_h  <- suppressWarnings(read_pums(hfile, dyear))
    dt_p  <- suppressWarnings(read_pums(pfile, dyear))
    dt_p[, PRACE:=fcase(as.integer(HISP)!=1, "H",                                                  # PSRC non-overlapping race category (Hispanic its own race)
                        RAC1P %in% c("3","4","5"), "I",
                        !is.na(RAC1P), as.character(RAC1P))]
  }else{
    dt_h <- suppressWarnings(fetch_ftp(span, dyear, "h")) %>% setDT()                              # Otherwise, ftp source
    dt_p <- suppressWarnings(fetch_ftp(span, dyear, "p")) %>% setDT()
    dt_p[, PRACE:=fcase(as.integer(HISP)!=1, "H",                                                  # PSRC non-overlapping race category (Hispanic its own race)
                       RAC1P %in% c("3","4","5"), "I",
                       !is.na(RAC1P), as.character(RAC1P))]
  }
  if("TYPE" %in% colnames(dt_h)){setnames(dt_h,"TYPE","TYPEHUGQ")}
  setkeyv(dt_h, "SERIALNO")
  dt_p %<>% setkeyv("SERIALNO") %>%
    .[, which(grepl("^PUMA$|^ADJINC$|^ADJUST", colnames(.))):=NULL]                                # Remove duplicate columns
  tmp_p <- copy(dt_p) %>% .[!is.na(SPORDER), .(SERIALNO, AGEP, PRACE, DIS, COW)]
  tmp_p[COW>0 & COW<9, WORKER:=1L]
  if(!any(grepl("^DIS$", colnames(tmp_p)))){tmp_p[, DIS:=0]}
  pp_hh <- tmp_p[, .(HRACE=stuff(PRACE),
                     HDIS=min(DIS, na.rm=TRUE),
                     NWRK=sum(WORKER, na.rm=TRUE)), by=.(SERIALNO)] %>%                            # Summarize households for race/ethnic composition, disability status
    setkey("SERIALNO")
  pp_hh[(HRACE %like% ","), HRACE:="M"]                                                            # - Characterize multiracial or household-level disability
  pp_aa <- tmp_p[AGEP > 18, .(ARACE=stuff(PRACE), ADIS=min(DIS)), by=.(SERIALNO)] %>%              # Summarize households for race/ethnic composition, disability status
    setkey("SERIALNO")
  pp_aa[(ARACE %like% ","), ARACE:="M"]                                                            # - Characterize multiracial or household-level disability
  dt_h %<>% merge(pp_hh, by="SERIALNO", all.x=TRUE)                                                # Relate household-composition variables
  dt_h %<>% merge(pp_aa, by="SERIALNO", all.x=TRUE)                                                # Relate adult-restricted household-composition variables
  adjvars <- if("ADJINC" %in% colnames(dt_h)){c("ADJINC","ADJHSG")}else{"ADJUST"}
  dt_h[, (adjvars):=lapply(.SD, function(x){as.numeric(x)/1000000}), .SDcols=adjvars]              # Adjustment factors in ftp version without decimal
  if(level %in% c("h","households")){                                                              # For household analysis:                                                               #    filter out GQ or vacant units &
    dt_p %<>% .[as.integer(SPORDER)==1]                                                            #  - keep only householder person attributes
    dt <- merge(dt_h, dt_p, by="SERIALNO", all.x=TRUE) %>% .[TYPEHUGQ==1 & is.na(VACS)]            #  - filter out GQ & vacant
  }else if(level %in% c("p","persons")){                                                           # For population analysis, keep only individuals
    dt <- merge(dt_p, dt_h, by="SERIALNO", all.x=TRUE) %>% .[!is.na(SPORDER)]
  }
  if("BINCOME" %in% vars){dt %<>% psrc_bincome()}                                                  # See psrc-pums-groupings for custom binned variables
  if("BIN_AGE" %in% vars){dt %<>% psrc_bin_age()}                                                  # - "
  if("BIN_POVRATIO" %in% vars){dt %<>% psrc_bin_povratio()}                                        # - "
  if("OWN_RENT" %in% vars){dt %<>% psrc_own_rent()}                                                # - "
  if("ED_ATTAIN" %in% vars){dt %<>% psrc_ed_attain(dyear)}                                         # - "
  if("BIN_YBL" %in% vars){dt %<>% psrc_bin_ybl(dyear)}                                             # - "
  if("MI_JOBSECTOR" %in% vars){dt %<>% psrc_mi_jobsector()}                                        # - "
  if("LUM_JOBSECTOR" %in% vars){dt %<>% psrc_lum_jobsector()}                                      # - "
  if("STANDARD_JOBSECTOR" %in% vars){dt %<>% psrc_standard_jobsector()}                            # - "
  swgt <- if(level %in% c("p","persons")){"PWGTP"}else{"WGTP"}                                     # Specify sample weight
  setnames(dt, toupper(names(dt)))                                                                 # All column names to uppercase
  wgtrgx <- paste0("^",swgt,"\\d+$")
  rwgt <- grep(wgtrgx, colnames(dt), value=TRUE)                                                   # Specify replication weights
  varlist <- c(unlist(unit_key),"PUMA", unlist(vars), swgt, rwgt, adjvars) %>% unique()            # Columns to keep
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
#' @author Michael Jensen
#'
#' @rawNamespace import(data.table, except = c(month, year))
pums_api_gofer <- function(span, dyear, level, vars){
  varlist <- unlist(vars) %>% c("ADJINC","ADJHSG") %>% unique()                                    # Include adjustment variables
  vf <- if(dyear<2020){list(TYPE=1, SPORDER=1)}else{list(TYPEHUGQ=1, SPORDER=1)}
  tbl_ref    <- if(level %in% c("p","persons")){"person"}else{"housing"}
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
#' @author Michael Jensen
#'
#' @rawNamespace import(data.table, except = c(month, year))
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
#' @author Michael Jensen
#'
#' @rawNamespace import(data.table, except = c(month, year))
add_county <- function(dt, dyear){
  PUMA <- COUNTY <- NULL                                                                           # Bind variables locally (for documentation, not function)
  PUMA3 <- if(dyear>2011){c(115:118)}else if(dyear<=2011 & dyear>=2000){c(14,20,10,17)}            # PUMAs renumbered in 2012
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
#' @author Michael Jensen
#'
#' @rawNamespace import(data.table, except = c(month, year))
codes2labels <- function(dt, dyear, vars){
  recode <- val_min <- val_max <- var_code <- val_label <- i.val_label <- NULL                     # Bind variables locally (for documentation, not function)
  ddyear  <- if(dyear>2016){dyear}else{2017}                                                       # Temporary - until 2005-15 lookup is ready
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
  recoder[[4]] <- copy(recoder[[1]]) %>% .[var_code=="DIS"] %>% .[,var_code:="HDIS"]
  recoder[[5]] <- copy(recoder[[2]]) %>% .[, var_code:="ARACE"]
  recoder[[6]] <- copy(recoder[[4]]) %>% .[, var_code:="ADIS"]
  recoder %<>% rbindlist() %>% setDT() %>% .[var_code %in% vars] %>% setkeyv("val_max")            # Add to label lookup; filter variables
  chg_vars <- c("YBL","RELP","SCHG","SCHL")                                                        # The code-to-label match for these vars
  if(dyear < 2012 & any(vars %in% chg_vars)){                                                      # -- changed in 2012 but names were kept
    recoder %<>% .[var_code %not_in% chg_vars]                                                     # -- so keep codes to avoid miscategorization
  }
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
#' @author Michael Jensen
#'
#' @rawNamespace import(data.table, except = c(month, year))
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
#' @author Michael Jensen
#'
#' @importFrom tidyselect all_of
#' @rawNamespace import(data.table, except = c(month, year))
#'
#' @examples
#' \dontrun{
#' get_psrc_pums(span=1, dyear=2019, level="p", vars=c("AGEP","SEX"))}
#'
#' @export
get_psrc_pums <- function(span, dyear, level, vars, dir=NULL, labels=TRUE){
                           # These vars kept name but changed dictionary;
  if(dyear<2017){
    warning(paste("Use data dictionary to confirm earlier-year codes are identical to 2017",         # Until an archive lookup is finished
                  "try 'labels=FALSE' option w/ manual recode for accuracy"))                        # -- warn users to verify labels are OK
  }
  unit_var <- if(level %in% c("p","persons")){c("SERIALNO","SPORDER")}else{"SERIALNO"}
  dt <- pums_ftp_gofer(span, dyear, level, vars, dir)
  swgt <- if(level %in% c("p","persons")){"PWGTP"}else{"WGTP"}                                     # Specify sample weight
  rwgt <- paste0(swgt, 1:80)                                                                       # Specify replication weights
  dt %<>% add_county(dyear) %>% setcolorder(c(unit_var, "COUNTY")) %>%
    adjust_dollars()                                                                               # Apply standard inflation adjustment
  if(labels==TRUE){dt %<>% codes2labels(dyear, vars)}                                              # Replace codes with labels where available
  dt %<>% ensure_datatypes()                                                                       # Confirm correct datatypes for weights and group_vars
  varlist <- c(unlist(unit_var), "DATA_YEAR", "PRODUCT", "UNIT", "COUNTY", unlist(vars)) %>% unique()
  dt %<>% setDF() %>% dplyr::relocate(all_of(varlist)) %>%
    srvyr::as_survey_rep(variables=all_of(varlist),                                                # Create srvyr object with replication weights for MOE
                         weights=all_of(swgt),
                         repweights=all_of(rwgt),
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
#' @importFrom data.table rbindlist setDF
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
