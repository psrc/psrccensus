## ----setup, echo = FALSE, message = FALSE-------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(psrccensus)
library(magrittr)
library(dplyr)
library(srvyr)
library(knitr)

## ----mutate using labels, message=FALSE, eval=FALSE---------------------------
#  pums19_5p <- get_psrc_pums(5, 2019, "p", c("AGEP","SCHL","ESR"))                           # Pull the data;
#                                                                                             #    rather than pipe the result, use a separate assignment
#  pums19_5p %<>% mutate(                                                                     #    so any issues with mutate() don't negate your download
#    ed_25up = factor(                                                                        # Use Factor datatype for categorical variables
#        case_when(AGEP<25                               ~ NA_character_,                     # Type-specific NA constant
#                  grepl("^(Bach|Mast|Prof|Doct)", SCHL) ~ "Bachelor's degree or higher",     # Regex is concise; handy since PUMS labels can be wordy
#                  !is.na(SCHL)                          ~ "Less than a Bachelor's degree")), # !is.na() criteria
#    emp_25up = factor(                                                                       # Mutate() can assign more than one variable
#        case_when(AGEP<25                               ~ NA_character_,                     # Type-specific NA constant
#                  grepl("at work$", ESR)                ~ "Employed",                        # Concise regex again; use care, checking the data dictionary
#                  !is.na(ESR)                           ~ as.character(ESR)),                # Retain NA for children under 16
#        levels=c("Employed","Unemployed","Not in labor force")))                             # Preferred ordering via `levels=`
#  
#  emp_ed_all  <- psrc_pums_count(pums19_5p, group_vars=c("emp_25up", "ed_25up"))             # These shares reflect the entire population
#  emp_ed_25up <- psrc_pums_count(pums19_5p, group_vars=c("emp_25up", "ed_25up"), incl_na=FALSE) # No NA; same counts but shares for only age 25+, as intended

## ----mutate using values, message=FALSE, eval=FALSE---------------------------
#  pvars <- c("AGEP","FOD1P","FOD2P","INDP","ESR")
#  ftr_int <- function(x){as.integer(as.character(x))}                                        # Micro-helper conversion function
#  pums18_5p <- get_psrc_pums(5, 2018, "p", pvars, labels=FALSE)                              # Labels=FALSE leaves values--but they are still Factors!
#  
#  pums18_5p %<>% mutate(
#     med_deg = factor(
#         case_when(between(ftr_int(FOD1P), 6100, 6199)|between(ftr_int(FOD2P), 6100, 6199) ~ "Medical Degree",
#                   TRUE ~ "No medical degree")),                                             # Regardless of age
#     med_field = factor(
#         case_when(ftr_int(ESR) %in% c(1,2,4,5) & between(ftr_int(INDP), 7970, 8290) ~ "Medical industry, employed",
#                   ftr_int(ESR)==3 & between(ftr_int(INDP), 7970, 8290) ~ "Medical industry, not currently employed",
#                   ftr_int(ESR) %in% c(1,2,4,5) & !is.na(INDP)          ~ "Non-medical industry, employed",
#                   ftr_int(ESR)==3 & !is.na(INDP)                       ~ "Non-medical industry, not currently employed",
#                   TRUE ~ NA_character_)))                                                   # Leaving out those not in the workforce
#  
#  med_deg_work_all  <- psrc_pums_count(pums18_5p, group_vars=c("med_field","med_deg"))       # These shares reflect the entire population
#  med_deg_work_only <- psrc_pums_count(pums18_5p, group_vars=c("med_field","med_deg"), incl_na=FALSE) # These shares limited to workforce
#  

