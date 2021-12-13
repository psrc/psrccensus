## confirm/install necessary packages
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}
using("data.table","magrittr","stringr","survey", "srvyr","tidycensus","dplyr","rlang")

# Inflation adjustments -------------------------
## Dollar values in PUMS reported throughout the year; this applies the included inflation adjustment to make values comparable
adjust_inflation <-function(dt, dollar_var, adj_var){
  dt %<>% .[, (dollar_var):=round(as.numeric(gsub(",", "", get(dollar_var))) * as.numeric(get(adj_var)),0)]      # Inflation adjustment for dollar variables
  return(dt)
}

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

# Processing steps ------------------------------
## Restrict to PSRC 4-county region
clip2region <- function(dt){
  dt %<>% .[as.integer(PUMA) %/% 100 %in% c(115,116,117,118)] %>% 
    .[, grep(("^ST$|^ST_label$"), colnames(.)):=NULL] 
  return(dt)
}

## Add county name
add_county <- function(dt){
  county_lookup <- data.frame(PUMA3=c(115,116,117,118),
                              COUNTY=c("Pierce","King","Snohomish","Kitsap")) %>% 
    setDT() %>% setkey(PUMA3)
  dt %<>% .[, PUMA3:=(as.integer(PUMA) %/% 100)] %>%
    .[county_lookup, COUNTY:=COUNTY, on=.(PUMA3=PUMA3)]
  return(dt)
}


# Primary data assembly -------------------------
## Process the grouping variable
psrc_pums_groupvar <- function(span, dyear, group_var, bin_defs=NULL){
  dt   <- get_pums(variables=c(group_var,"PUMA","ADJINC","ADJHSG"), state="WA", 
                   survey=paste0("acs", span), year=dyear, recode=if(dyear>2016){TRUE}else{FALSE}) %>% # Recode isn't available prior to 2017
          setDT() %>% clip2region() %>% adjust_dollars(group_var) %>% setkey(SERIALNO)
  if(exists("df$SPORDER")){
    dt %<>% .[SPORDER==1] %>% .[, SPORDER:=NULL]
  }
#  if(!is.null(bin_defs) & length(bin_defs)>1){
#    dt %<>% .[, (group_var):=cut(group_var, breaks=bin_defs, right=T, labels=F)]                  # Manual grouping categories
#  }else if(!is.null(bin_defs) & length(bin_defs)==1){
#    dt %<>% .[, (group_var):=ntile(group_var, bin_defs)]                                          # Ntile grouping categories
#  }
 return(dt) 
  }  

## Process the target variable
psrc_pums_targetvar <- function(span, dyear, target_var, tbl_ref){
  dt <- get_pums(variables=c(target_var,"PUMA","ADJINC","ADJHSG"), state="WA",                     # Include inflation adjustment fields
                 survey=paste0("acs", span), year=dyear, recode=if(dyear>2016){TRUE}else{FALSE},   # Recode unavailable prior to 2017
                 rep_weights=tbl_ref) %>% setDT() %>% clip2region() %>%                            # Replication weights for the appropriate table
    adjust_dollars(target_var)
  return(dt)
}

## Assemble the pieces, return a srvyr object
get_psrc_pums <- function(geo_scale, span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  varlist       <- c(target_var)
  pums_vars     <- pums_variables %>% setDT() %>% .[year==dyear & survey==paste0("acs", span)]     # Retrieve variable definitions
  tbl_ref       <- pums_vars[var_code==target_var, unique(level)]                                  # Table corresponding to unit of analysis (for rep weights)
  key_ref       <- pums_vars[var_code==group_var, unique(level)]                                   # Table corresponding to grouping variable (for join)
  dt_key        <- if(tbl_ref=="person" & key_ref!="housing"){c("SERIALNO","SPORDER")
                    }else{"SERIALNO"}                                                              # To match join
  rwgt_ref      <- if(tbl_ref=="person"){"PWGTP"}else{"WGTP"}
  dt <- psrc_pums_targetvar(span, dyear, target_var, tbl_ref) %>% add_county() %>%                 # Target variable via API
    setkeyv(dt_key) 
  rw <- colnames(dt) %>% .[grep(paste0(rwgt_ref,"\\d+"),.)]
  if(!is.null(group_var)){
    groupvar_label <- paste0(group_var,"_label")
    varlist %<>% c(groupvar_label)
    group_var_dt <- psrc_pums_groupvar(span, dyear, group_var, bin_defs=NULL)                      # Grouping variable via API\
    dt %<>% .[group_var_dt, (groupvar_label):=get(groupvar_label), on=key(.)]                      # Link data tables
  }
  dt %<>% setDF() %>% 
    as_survey_rep(variables=all_of(varlist),                                                       # Create srvyr object with replication weights for MOE
                  weights=all_of(rwgt_ref),
                  repweights=all_of(rw),
                  combined_weights=TRUE,
                  mse=TRUE,
                  type="other",
                  scale=4/80,
                  rscale=length(all_of(rw)))
  if(!is.null(group_var)){dt %<>% group_by(!!as.name(groupvar_label))}
  if(geo_scale=="county"){dt %<>% group_by(COUNTY, .add=TRUE)}
  return(dt)
}

# Statistical calls -----------------------------
## Generalized call
psrc_pums_stat <- function(stat_type, geo_scale, span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  result_name <- sym(stat_type)
  srvyrf_name <- as.name(paste0("survey_",stat_type))
  se_name     <- paste0(stat_type,"_se")
  moe_name    <- paste0(stat_type,"_moe")
  df <- get_psrc_pums(geo_scale, span, dyear, target_var, group_var, bin_defs)
  rs <- summarize(df, !!result_name:=(as.function(!!srvyrf_name)(
                  !!as.name(target_var), vartype="se", level=0.95))) %>%                           # Generate the weighted statistic
    mutate(!!sym(moe_name):=!!sym(se_name) * 1.645) %>% setDT() %>%                                # Margin of Error using standard error
    setnames(grep("_label", colnames(.)), c("group_label")) %>%                                    # For clarity in multi-call datasets
    .[, target_var:=as.character(target_var)] %>% .[, group_var:=as.character(group_var)] %>% 
    .[, (se_name):=NULL] %>% 
    setcolorder(c("target_var", "group_var", "group_label", as.character(result_name), as.character(moe_name)))
  return(rs)
}

## User-called Regional median/MOE function
psrc_pums_median <- function(span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  rs <- psrc_pums_stat(stat_type="median", geo_scale="region", span, dyear, target_var, group_var, bin_defs)
  return(rs)
}

## User-called Regional mean/MOE function 
psrc_pums_mean <- function(span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  rs <- psrc_pums_stat("mean", "region", span, dyear, target_var, group_var, bin_defs)
  return(rs)
}

## User-called County median/MOE function
county_pums_median <- function(span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  rs <- psrc_pums_stat("median", "county", span, dyear, target_var, group_var, bin_defs)
  return(rs)
}

## User-called County mean/MOE function 
county_pums_mean <- function(span, dyear, target_var, group_var=NULL, bin_defs=NULL){
  rs <- psrc_pums_stat("mean", "county", span, dyear, target_var, group_var, bin_defs)
  return(rs)
}

# Test ----------------------------------------------------------
# span <- 1
# dyear <- 2019
# group_var <- "SEX"
# target_var <- "AGEP"
# bin_defs <- 4
# tbl_ref <- "person"
# x <- psrc_pums_groupvar(1, 2019, "SEX")
# y <- psrc_pums_targetvar(1, 2019, "AGEP","person")
# df <- get_psrc_pums("region", 1, 2019, "AGEP", "SEX")
# rs <- psrc_pums_stat("median", "region", 1, 2019, "AGEP", "SEX")
# v <- psrc_pums_median(1, 2019, "AGEP", "SEX")
# stat_type <- "median"
# geo_scale <- "region"
# span <- 1
# dyear <- 2019
# target_var <- "AGEP"
# group_var <- "SEX"