#' @importFrom magrittr %<>% %>%
#' @author Michael Jensen
NULL

globalVariables(c(":=", "!!", ".", "enquos"))

`%not_in%` <- Negate(`%in%`)

#' Retrieve set of Census Places within the PSRC region
#'
#' @param year of census geography
#' @return string vector of Place FIPS codes
#'
#' @importFrom sf st_transform st_buffer st_join st_intersects
#' @importFrom dplyr filter select rename
#' @export
get_psrc_places <- function(year){
  psrc_region <- counties <- COUNTYFP <- place_lookup <- places <- GEOID <- NAME <- geometry <- NULL
  psrc_region <- tigris::counties("53", cb=TRUE, progress_bar=FALSE) %>%
    filter(COUNTYFP %in% c("033","035","053","061")) %>% dplyr::summarize() %>%
    st_transform(2285) # planar projection to allow intersect
  place_lookup <- tigris::places("53", year=(if(year>2013){year}else{2014}), cb=TRUE, progress_bar=FALSE) %>%
    select(c(GEOID, NAME, geometry)) %>% st_transform(2285) %>% st_buffer(-1) %>% # To avoid any overlap
    st_join(psrc_region, join=st_intersects, left=FALSE)
  return(place_lookup)
}

#' Helper to translate psrccensus estimates to planning geographies
#'
#' @param df acs or decennial dataset returned from psrccensus
#' @param planning_geog_type planning geography type as listed in Elmer.general.geography_splits
#' @param wgt measure share used as split weight
#'            either "total_pop" (default), "household_pop", "group_quarters_pop", "housing_units" or "occupied_housing_units"
#' @param agg_fct aggregation
#'
#' @rawNamespace import(data.table, except = c(month, year))
use_geography_splits <- function(df, planning_geog_type, wgt="total_pop", agg_fct="sum"){

  testInteger <- function(x){
    func <- function(x){all.equal(x, as.integer(x), check.attributes = FALSE)}
    test <- x %>% lapply(func) %>% unlist() %>% all()
    if(test==TRUE){return(TRUE)
    }else{return(FALSE)}
  }
  est_is_integer <- testInteger(df$estimate)

  geography_splits_helper <- function(df, planning_geog_type, wgt="total_pop", agg_fct="sum"){
    digits <- geo <- data_geog_type <- ofm_estimate_year <- value <- estimate <- moe <- NULL       # For roxygen
    fullwgt <- paste0("percent_of_", wgt)
    data_year <- dplyr::pull(df, year) %>% unique()
    ofm_vintage <- if(data_year %in% 2010:2019){2020
                   }else if(data_year %in% 2020:2022){2022
                   }
    cb_geo_yr <- (data_year - (data_year %% 10)) %% 100 %>% as.character()
    fips_lookup <- data.frame(digits=c(11,12,15), geo=c("tract","blockgroup","block")) %>% setDT()
    fips_length <- dplyr::pull(df, GEOID) %>% as.character() %>% nchar() %>% max()
    cb_geo <- fips_lookup[digits==fips_length, geo] %>% paste0(cb_geo_yr)                          # Stored as census geography & last two digits of yr
    if(data_year < 2010){
      warning("Splits for this year are not yet stored in Elmer")
      return(NULL)
    }else if(length(data_year)!=1){
      warning("Data is not specific to single year")
      return(NULL)
    }else if(is.null(cb_geo)){
      warning("Only census tracts, block groups, or blocks are valid")
      return(NULL)
    }else if(agg_fct!="sum"){
      warning("Aggregations currently limited to sums (no shares or medians)")
      return(NULL)
    }else{
      options(useFancyQuotes = FALSE)
      sql_str <- paste0("SELECT * FROM Elmer.general.get_geography_splits(",                       # SQL table-value function returns data
                       paste(sQuote(cb_geo), sQuote(planning_geog_type),
                       data_year, ofm_vintage, sep=", "), ");")
      group_cols <- grep("(year|variable|label|concept|acs_type)", colnames(df), value=TRUE) %>%
        append("planning_geog", after=0)
      value_col <- grep("(value|estimate)", colnames(df), ignore.case=TRUE, value=TRUE) %>% tolower() # Decennial:value; ACS:estimate
      rosetta <- psrcelmer::get_query(sql_str) %>% setDT() %>%                                     # Must be on PSRC VPN to connect to Elmer
        .[(data_geog_type=={{cb_geo}} &                                                            # Keep only necessary rows & columns
           planning_geog_type=={{planning_geog_type}} &
           ofm_estimate_year=={{data_year}}),
          grepl("(_geog$|^percent_of)", colnames(.)), with=FALSE] %>%
        setnames("data_geog", "GEOID") %>% setkey(GEOID)
      df %<>% setDT() %>% setkey(GEOID) %>% merge(rosetta, allow.cartesian=TRUE)                   # Merge on key=GEOID
      if(agg_fct=="sum" & value_col=="value"){                                                     # Decennial
        rs <- df[, value=sum(value * get(fullwgt)), by=mget(group_cols)]
      }else if(agg_fct=="sum" & value_col=="estimate"){                                            # ACS
        rsi <- df[, .(estimate = sum(estimate * get(fullwgt)),
                     moe = tidycensus::moe_sum((moe * get(fullwgt)), (estimate * get(fullwgt)), na.rm=TRUE)), # MOE calculation
                 by=mget(group_cols)]
        if(est_is_integer){rsi[, `:=`(estimate=round(estimate), moe=round(moe))]}
      }else{rsi <- NULL}
      return(rsi)
    }
  }

  rso <- df %>% split(.$year)                                                                      # In case table has multiple years
  rso %<>% mapply(geography_splits_helper, df=.,
                  planning_geog_type=planning_geog_type,
                  wgt=wgt, SIMPLIFY=FALSE) %>% rbindlist()
  return(rso)
}


#' Translate psrccensus data to planning geographies
#'
#' @param df acs or decennial dataset returned from psrccensus
#' @param wgt either "total_pop" (default), "household_pop", "group_quarters_pop", "housing_units" or "occupied_housing_units"
#' @name census_to_psrcgeo
#'
#' @importFrom data.table rbindlist
#' @return A table with the variable names and labels, summary statistic and margin of error
NULL

#' @rdname census_to_psrcgeo
#' @title Translate psrccensus data to Regional Geography Classes
#' @export
census_to_rgs <- function(df, wgt="total_pop"){
  rs <- use_geography_splits(df, planning_geog_type="Regional Geography Class (2022 RTP)", wgt=wgt)
  return(rs)
}

#' @rdname census_to_psrcgeo
#' @title Translate psrccensus data to Regional Growth Centers
#' @export
census_to_rgc <- function(df, wgt="total_pop"){
  rs <- use_geography_splits(df, planning_geog_type="Regional Growth Center (2022 RTP)", wgt=wgt)
  return(rs)
}

#' @rdname census_to_psrcgeo
#' @title Translate psrccensus data to Regional Manufacturing-Industrial Centers
#' @export
census_to_mic <- function(df, wgt="total_pop"){
  rs <- use_geography_splits(df, planning_geog_type="MIC (2022 RTP)", wgt=wgt)
  return(rs)
}

#' @rdname census_to_psrcgeo
#' @title Translate psrccensus data to Traffic Analysis Zones
#' @export
census_to_taz <- function(df, wgt="total_pop"){
  rs <- use_geography_splits(df, planning_geog_type="TAZ (2010)", wgt=wgt)
  return(rs)
}

#' @rdname census_to_psrcgeo
#' @title Translate psrccensus data to HCT Station Areas (VISION 2050)
#' @export
census_to_hct <- function(df, wgt="total_pop"){
  rs <- use_geography_splits(df, planning_geog_type="HCT Station Areas (VISION 2050)", wgt=wgt)
  return(rs)
}
