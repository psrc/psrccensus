#' @importFrom magrittr %<>% %>%
#' @import data.table
#' @author Michael Jensen
NULL

`%not_in%` <- Negate(`%in%`)

#' PSRC standard broad income groupings
#'
#' @param dt the data.table
#' @return the data.table with an additional binned household income field, "BINCOME"
psrc_bincome <- function(dt){
  BINCOME <- HINCP <- NULL                                                                         # Bind variables locally (for documentation, not function)
  dt %<>% setDT() %>%
   .[, BINCOME:=factor(fcase(HINCP <  25000,"Under $25,000",
                             HINCP <  50000, "$25,000-$49,999",
                             HINCP <  75000, "$50,000-$74,999",
                             HINCP < 100000, "$75,000-$99,999",
                             HINCP >=100000, "$100,000 or more",
                             !is.na(HINCP),  "Else / Prefer not to answer"),
                      levels=c("Under $25,000",
                               "$25,000-$49,999",
                               "$50,000-$74,999",
                               "$75,000-$99,999",
                               "$100,000 or more",
                               "Else / Prefer not to answer"))]
  return(dt)
}

#' PSRC standard age groupings
#'
#' @param dt the data.table
#' @return the data.table with an additional binned age field, "BIN_AGE"
psrc_bin_age <- function(dt){
  BIN_AGE <- AGEP <- NULL                                                                          # Bind variables locally (for documentation, not function)
  dt %<>% setDT() %>%
   .[, BIN_AGE:=factor(fcase(AGEP < 5 , "under 5 years",
                             AGEP < 12, "between 5 and 11 years",
                             AGEP < 16, "between 12 and 15 years",
                             AGEP < 18, "between 16 and 17 years",
                             AGEP < 25, "between 18 and 24 years",
                             AGEP < 35, "between 25 and 35 years",
                             AGEP < 45, "between 35 and 45 years",
                             AGEP < 55, "between 45 and 55 years",
                             AGEP < 65, "between 55 and 65 years",
                             AGEP < 75, "between 65 and 75 years",
                             AGEP < 85, "between 75 and 85 years",
                             AGEP > 84, "85 years and over",
                             !is.na(AGEP), "Else / Prefer not to answer"),
                      levels=c("under 5 years",
                               "between 5 and 11 years",
                               "between 12 and 15 years",
                               "between 16 and 17 years",
                               "between 18 and 24 years",
                               "between 25 and 35 years",
                               "between 35 and 45 years",
                               "between 45 and 55 years",
                               "between 55 and 65 years",
                               "between 65 and 75 years",
                               "between 75 and 85 years",
                               "85 years and over",
                               "Else / Prefer not to answer"))]
  return(dt)
}

#' PSRC income to poverty ratio groupings
#'
#' @param dt the data.table
#' @return the data.table with an additional binned age field, "BIN_POVRATIO"
psrc_bin_povratio <- function(dt){
  BIN_POVRATIO <- POVPIP <- NULL                                                                   # Bind variables locally (for documentation, not function)
    dt %<>% setDT() %>%
   .[, BIN_POVRATIO:=factor(fcase(POVPIP < 50 , "under 0.50",
                                  POVPIP < 100, "0.50 to 0.99",
                                  POVPIP < 125, "1.00 to 1.24",
                                  POVPIP < 150, "1.25 to 1.49",
                                  POVPIP < 185, "1.50 to 1.84",
                                  POVPIP < 200, "1.85 to 1.99",
                                  POVPIP >=200, "2.00 and over",
                                  !is.na(POVPIP), "Else"),
                            levels=c("under 0.50",
                                     "0.50 to 0.99",
                                     "1.00 to 1.24",
                                     "1.25 to 1.49",
                                     "1.50 to 1.84",
                                     "1.85 to 1.99",
                                     "2.00 and over",
                                     "Else"))]
  return(dt)
}

#' ACS year structure built groupings
#'
#' @param dt the data.table
#' @param dyear the data year
#' @return the data.table with an additional binned age field, "BIN_YBL"
#'
#' @importFrom stringr str_extract
psrc_bin_ybl<- function(dt, dyear){
  BIN_YBL <- YBL_chr <- val_label <- val_max <- var_code <- NULL                                   # Bind variables locally (for documentation, not function)
  ddyear <- if(dyear>2016){dyear}else if(dyear>2008){2017}
  lkup <- tidycensus::pums_variables %>% setDT() %>%
    .[var_code=="YBL" & year==ddyear, .(val_max, val_label)] %>% unique() %>% setkey("val_max")
  dt[lkup , YBL_chr:=val_label, on=c(YBL="val_max")]
  dt[, BIN_YBL:=factor(fcase(grepl("^200", YBL_chr), "2000 to 2009",
                             grepl("^199", YBL_chr), "1990 to 1999",
                             grepl("^198", YBL_chr), "1980 to 1989",
                             grepl("^197", YBL_chr), "1970 to 1979",
                             grepl("^196", YBL_chr), "1960 to 1969",
                             grepl("^195", YBL_chr), "1950 to 1959",
                             grepl("^194", YBL_chr), "1940 to 1949",
                             grepl("^193", YBL_chr), "1939 or earlier",
                             between(as.integer(str_extract(YBL_chr,"^\\d+")), 2010, 2014), "2010 to 2013",
                             (as.integer(str_extract(YBL_chr,"^\\d+")) > 2013), "2014 or later",
                             !is.na(YBL_chr), "Else"),
                      levels=c("2014 or later",
                               "2010 to 2013",
                               "2000 to 2009",
                               "1990 to 1999",
                               "1980 to 1989",
                               "1970 to 1979",
                               "1960 to 1969",
                               "1950 to 1959",
                               "1940 to 1949",
                               "1939 or earlier",
                               "Else"))] %>%
  .[, YBL_chr:=NULL]
  return(dt)
}

#' PSRC tenure groupings
#'
#' @param dt the data.table
#' @return the data.table with dichotomous tenure field, "OWN_RENT"
psrc_own_rent<- function(dt){
  OWN_RENT <- TEN <- NULL                                                                          # Bind variables locally (for documentation, not function)
  dt %<>% setDT() %>%
  .[, OWN_RENT:=factor(fcase(as.integer(as.character(TEN)) %in% c(1,2), "Owned",
                             as.integer(as.character(TEN)) %in% c(3,4), "Rented",
                             !is.na(TEN), "Else"),
                       levels=c("Owned",
                                "Rented",
                                "Else"))]
  return(dt)
}

#' PSRC educational attainment groupings
#'
#' @param dt the data.table
#' @return the data.table with educational attainment field, "ED_ATTAIN"
psrc_ed_attain<- function(dt, dyear){
  ED_ATTAIN <- SCHL <- NULL                                                                        # Bind variables locally (for documentation, not function)
  if(dyear>2011){
    dt %<>% setDT() %>%
      .[, ED_ATTAIN:=factor(fcase(between(as.integer(as.character(SCHL)),22,24), "Postgraduate Degree",
                                  as.integer(as.character(SCHL))==21,            "Bachelor Degree",
                                  between(as.integer(as.character(SCHL)),18,20), "Some College",
                                  between(as.integer(as.character(SCHL)),16,17), "HS Diploma/GED",
                                  between(as.integer(as.character(SCHL)),1,15),  "Less than HS diploma"),
                            levels=c("Less than HS diploma","HS Diploma/GED","Some College","Bachelor Degree","Postgraduate Degree"))]
  }else{
    dt %<>% setDT() %>%
      .[, ED_ATTAIN:=factor(fcase(between(as.integer(as.character(SCHL)),14,16), "Postgraduate Degree",
                                  as.integer(as.character(SCHL))==13,            "Bachelor Degree",
                                  between(as.integer(as.character(SCHL)),10,11), "Some College",
                                  as.integer(as.character(SCHL))==9,             "HS Diploma/GED",
                                  between(as.integer(as.character(SCHL)),1,8),   "Less than HS diploma"),
                            levels=c("Less than HS diploma","HS Diploma/GED","Some College","Bachelor Degree","Postgraduate Degree"))]
  }
  return(dt)
}

#' PSRC manufacturing-industrial category
#'
#' @param dt the data.table
#' @return the data.table with manufacturing-industrial category field, "MI_JOBSECTOR"             # When NAICS changes, new dyear/definition set should be added
psrc_mi_jobsector <- function(dt){
  MI_JOBSECTOR <- NAICSP <- NULL                                                                   # Bind variables locally (for documentation, not function)
  dt %<>% setDT()
  if(any(grepl("^NAICSP\\d+$", colnames(dt)))){
    dt[, grep("^NAICSP\\d+$", colnames(dt)):=lapply(.SD, as.character), .SDcols=patterns("^NAICSP\\d+$")]
    dt[, NAICSP:=fcoalesce(.SD), .SDcols=patterns("^NAICSP\\d+$")]
  }
  dt[, MI_JOBSECTOR:=factor(fcase(grepl("^221|^45411|^5121|^515|^517|^5616|^56173|^562|^6242|^8113|^8114|^8123",
                                    as.character(NAICSP)),               "Other Industrial",
                                  grepl("^42", as.character(NAICSP)),    "Warehousing & Wholesale",
                                  grepl("^48|^49", as.character(NAICSP)),"Transportation, Distribution & Logistics (TDL)",
                                  grepl("^23", as.character(NAICSP)),    "Construction",
                                  grepl("^33|^3M", as.character(NAICSP)),"Manufacturing",
                                  !is.na(NAICSP),                         NA_character_),
                            levels=c("Construction","Manufacturing","Warehousing & Wholesale",
                                     "Transportation, Distribution & Logistics (TDL)","Other Industrial"))]
  return(dt)
}
