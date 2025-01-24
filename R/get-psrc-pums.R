#' @importFrom magrittr %<>% %>%
#' @rawNamespace import(data.table, except = c(month, year))
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
pums_recode_na <- function(dt){
  for(col in colnames(dt)) set(dt, i=grep("^b+$|^N.A.(\\/)?|^NA$|^$",dt[[col]]), j=col, value=NA)  # Recode all PUMS NA variations to the NA R recognizes
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
read_pums <- function(target_file, dyear){
  var_code <- var_label <- data_type <- NULL                                                       # Bind tidycensus::pums_variables variable locally (for documentation, not function)
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
  dt <- suppressWarnings(                                                                          # fread warns when colClasses items aren't present; OK to use combined list
    data.table::fread(target_file, sep=",", stringsAsFactors=FALSE, colClasses=col_typelist)       # The room where it happens; reads the file with correct datatypes
    ) %>% filter2region(dyear)

  return(dt)
}

#' Filter PUMS to Region
#'
#' Helper to \code{\link{read_pums}} function
#' @param dt data.table
#' @param dyear The data year
#' @return filtered data.table
#' @author Michael Jensen
filter2region <- function(dt, dyear){
  PUMA <- PUMA00 <- PUMA10 <- PUMA20 <- psrc_pumas <- SERIALNO <- NULL                             # Bind tidycensus::pums_variables variable locally (for documentation, not function)
  dt %<>% pums_recode_na() %>%
    .[, colnames(.) %not_in% c("RT","DIVISION","REGION","ST","STATE"), with=FALSE]                 # Drop variables static to our region

  if("PUMA" %in% colnames(dt)){                                                                    # For 1yr data or spans with identical geography
    psrc_pumas <- dplyr::case_when(dyear > 2021 ~c(233,235,253,261),
                                   dyear > 2011 ~c(115:118),                                       # New decadal PUMA code scheme begins use in year 2012, 2022, etc
                                   dyear > 2001 ~c(14,10,17,20))
    dt %<>% .[(as.integer(PUMA) %/% 100) %in% get("psrc_pumas")]                                   # Filter region by code
  }else{
    if("PUMA00" %in% colnames(dt)){                                                                # For multiyear data with differing PUMA geographies
      dt[(as.integer(PUMA00) %/% 100) %in% c(14,10,17,20), PUMA:=PUMA00]                           # Populate new PUMA field for region
    }
    if("PUMA10" %in% colnames(dt)){
      dt[(as.integer(PUMA10) %/% 100) %in% c(115:118), PUMA:=PUMA10]
    }
    if("PUMA20" %in% colnames(dt)){
      dt[(as.integer(PUMA20) %/% 100) %in% c(233,235,253,261), PUMA:=PUMA20]
    }
    dt %<>% .[!is.na(PUMA)] %>% .[, which(grepl("^PUMA\\d\\d", colnames(.))):=NULL]                # Filter region & drop limited PUMA fields                                                                       # Filter by dropping empty
  }
  dt %<>% setkey(SERIALNO)
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
#' @importFrom readr read_rds
pums_ftp_gofer <- function(span, dyear, level, vars, dir=NULL){
  SPORDER <- SERIALNO <- TYPE <- TYPEHUGQ <- VACS <- DIS <- NULL
  RAC1P <- PRACE <- ARACE <- HRACE <- HISP <- AGEP <- WORKER <- ESR <- NULL

  # Bind variables locally (for documentation, not function)
  unit_key <- if(level %in% c("p","persons")){c("SERIALNO","SPORDER")}else{"SERIALNO"}
  if(!is.null(dir)){                                                                               # For server tool; gz files already downloaded & filtered
    hfile <- paste0(dir,"/", dyear, "h", span, ".gz")
    pfile <- paste0(dir,"/", dyear, "p", span, ".gz")
    dt_h  <- readr::read_rds(hfile) %>% setDT()
    dt_p  <- readr::read_rds(pfile) %>% setDT()
    dt_p[, PRACE:=fcase(as.integer(HISP)!=1, "H",                                                  # PSRC non-overlapping race category (Hispanic its own race)
                        RAC1P %in% c("3","4","5"), "I",
                        !is.na(RAC1P), as.character(RAC1P))]
  }else{
    dt_h <- fetch_ftp(span, dyear, "h") %>% setDT()                                                # Otherwise, ftp source
    dt_p <- fetch_ftp(span, dyear, "p") %>% setDT()
    dt_p[, PRACE:=fcase(as.integer(HISP)!=1, "H",                                                  # PSRC non-overlapping race category (Hispanic its own race)
                       RAC1P %in% c("3","4","5"), "I",
                       !is.na(RAC1P), as.character(RAC1P))]
  }
  if("TYPE" %in% colnames(dt_h)){setnames(dt_h,"TYPE","TYPEHUGQ")}
  setkeyv(dt_h, "SERIALNO")
  dt_p %<>% setkeyv("SERIALNO") %>%
    .[, which(grepl("^PUMA$|^ADJINC$|^ADJUST", colnames(.))):=NULL]                                # Remove duplicate columns
  tmp_p <- copy(dt_p) %>% .[!is.na(SPORDER), .(SERIALNO, AGEP, PRACE, DIS, ESR)]
  tmp_p[ESR %in% c(1,2,4,5), WORKER:=1L]
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
  dt %<>% setDT()
  if("BINCOME" %in% vars){dt %<>% psrc_bincome()}                                                  # See psrc-pums-groupings for custom binned variables
  if("BIN_AGE" %in% vars){dt %<>% psrc_bin_age()}                                                  # - "
  if("BIN_POVRATIO" %in% vars){dt %<>% psrc_bin_povratio()}                                        # - "
  if("OWN_RENT" %in% vars){dt %<>% psrc_own_rent()}                                                # - "
  if("ED_ATTAIN" %in% vars){dt %<>% psrc_ed_attain(dyear)}                                         # - "
  if("BIN_YBL" %in% vars){dt %<>% psrc_bin_ybl(dyear)}                                             # - "
  if("MI_JOBSECTOR" %in% vars){dt %<>% psrc_mi_jobsector()}                                        # - "
  if("LUM_JOBSECTOR" %in% vars){dt %<>% psrc_lum_jobsector()}                                      # - "
  if("STANDARD_JOBSECTOR" %in% vars){dt %<>% psrc_standard_jobsector()}                            # - "
  if("LUM_OCCUPATION" %in% vars){dt %<>% psrc_lum_occupation()}                                    # - "
  if("SOCP2" %in% vars){dt %<>% psrc_socp2()}                                                      # - "
  if("SOCP3" %in% vars){dt %<>% psrc_socp3()}                                                      # - "
  if("SOCP5" %in% vars){dt %<>% psrc_socp5()}                                                      # - "
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
#' @return PSRC data.table with county names
#' @author Michael Jensen
add_county <- function(dt){
  PUMA <- COUNTY <- PUMA_start <- puma_start <- county <- NULL                                     # Bind variables locally (for documentation, not function)
  county_lookup <- data.frame(
    GeoYr=c(rep(2000,4),rep(2010,4),rep(2020,4)),
    puma_start=c(14,20,10,17,115:118,253,233,261,235),                                             # New PUMAs every 20X2 year
    county=as.factor(rep(c("Pierce","King","Snohomish","Kitsap"),3))) %>%                          # . . . mind the order here
    setDT() %>% setkey(puma_start)
  dt %<>% .[, PUMA_start:=(as.integer(PUMA) %/% 100)] %>%
    .[county_lookup, COUNTY:=county, on=.(PUMA_start=puma_start)] %>%                              # Recode all--multiyear PUMS may have two PUMA schemes
    .[, PUMA_start:=NULL]
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
codes2labels <- function(dt, dyear, vars){
  recode <- val_min <- val_max <- var_code <- val_label <- i.val_label <- NULL                     # Bind variables locally (for documentation, not function)
  ddyear  <- if(dyear>2016){dyear}else{2017}                                                       # Temporary - until 2005-15 lookup is ready
  recoder <- list()
  recoder[[1]] <- tidycensus::pums_variables %>% setDT() %>%                                       # Get the value-label correspondence for any/all factor variables
    .[recode==TRUE & val_min==val_max & year==ddyear, .(var_code, val_max, val_label)] %>%
    unique()
  recoder[[2]] <- copy(recoder[[1]]) %>%
    .[var_code=="RAC1P" & val_max %not_in% c("3","4","5")] %>%
    rbind(list(
      c(rep("",3)),
      c("I","H","M"),
      c("American Indian or Alaskan Native alone", "Hispanic or Latino","Multiple Races"))) %>%    # PSRC non-overlapping race category (Hispanic as a race)
    .[,var_code:="HRACE"]
  recoder[[3]] <- copy(recoder[[2]]) %>% .[, var_code:="PRACE"]
  recoder[[4]] <- copy(recoder[[1]]) %>% .[var_code=="DIS"] %>% .[,var_code:="HDIS"]
  recoder[[5]] <- copy(recoder[[2]]) %>% .[, var_code:="ARACE"]
  recoder[[6]] <- copy(recoder[[4]]) %>% .[, var_code:="ADIS"]
  recoder[[7]] <- pums_labels_xtra
  recoder %<>% rbindlist(use.names=TRUE) %>% setDT() %>%
    .[var_code %in% vars] %>% setkeyv("val_max")                                                   # Add to label lookup; filter variables
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
#'
#' @examples
#' \dontrun{
#' get_psrc_pums(span=1, dyear=2019, level="p", vars=c("AGEP","SEX"))}
#'
#' @export
get_psrc_pums <- function(span, dyear, level, vars, dir=NULL, labels=TRUE){
                           # These vars kept name but changed dictionary;
  if(dyear==2020 & span==1){
    warning(paste("Due to collection issues, there was no public release of 1-yr 2020 PUMS data.",
                  "Data with experimental weights exists, but may not be directly comparable."))
    return(NULL)
  }else if(dyear<2017){
    warning(paste("Use data dictionary to confirm earlier-year codes are identical to 2017",       # Until an archive lookup is finished
                  "try 'labels=FALSE' option w/ manual recode for accuracy"))                      # -- warn users to verify labels are OK
  }
  unit_var <- if(level %in% c("p","persons")){c("SERIALNO","SPORDER")}else{"SERIALNO"}
  dt <- pums_ftp_gofer(span, dyear, level, vars, dir)
  swgt <- if(level %in% c("p","persons")){"PWGTP"}else{"WGTP"}                                     # Specify sample weight
  rwgt <- paste0(swgt, 1:80)                                                                       # Specify replication weights
  dt %<>% add_county() %>% setcolorder(c(unit_var, "COUNTY")) %>%
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
