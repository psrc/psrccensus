## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(psrccensus)
library(dplyr)

## ----acs examples, message=FALSE----------------------------------------------
get_acs_recs(geography = 'county',
             table.names = 'B03002',
             years = 2019,
             acs.type = 'acs1')


## ----census examples, message=FALSE-------------------------------------------
get_decennial_recs(geography = 'msa',
                   table_codes = c("H001", "P001"),
                   years = c(2010),
                   fips = c('42660', "28420"))


## ----tract_map example, message=FALSE-----------------------------------------
library(sf)
library(dplyr)

tract.big.tbl <- get_acs_recs(geography ='tract', 
                              table.names = 'B03002',
                              years = 2019)

tract.tbl <- tract.big.tbl %>% 
  filter(label=='Estimate!!Total:!!Not Hispanic or Latino:!!Black or African American alone')

tract.url <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/tract2010_nowater/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson"
#'
tract.lyr<-st_read(tract.url)

create_tract_map(tract.tbl, 
                 tract.lyr, 
                 map.title='Black, non-Hispanic Population',
                 map.title.position='topleft', 
                 legend.title='Black, Non-Hispanic Population',
                 legend.subtitle='by Census Tract')


