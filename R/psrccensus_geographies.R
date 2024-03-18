#' @importFrom magrittr %<>% %>%
NULL

globalVariables(c(":=", "!!", ".", "enquos"))

`%not_in%` <- Negate(`%in%`)

#' Retrieve set of Census Places within the PSRC region
#'
#' @param year of census geography
#' @return string vector of Place FIPS codes
#' @author Michael Jensen
#'
#' @importFrom sf st_transform st_buffer st_join st_intersects
#' @importFrom dplyr filter select rename
#' @export
get_psrc_places <- function(year){
  psrc_region <- counties <- COUNTYFP <- place_lookup <- places <- GEOID <- NAME <- geometry <- NULL # For Roxygen
  psrc_region <- tigris::counties("53", cb=TRUE, progress_bar=FALSE) %>%
    filter(COUNTYFP %in% c("033","035","053","061")) %>% dplyr::summarize() %>%
    st_transform(2285) # planar projection to allow intersect
  place_lookup <- tigris::places("53", year=(if(year>2013){year}else{2014}), cb=TRUE, progress_bar=FALSE) %>%
    select(c(GEOID, NAME, geometry)) %>% st_transform(2285) %>% st_buffer(-1) %>% # To avoid any overlap
    st_join(psrc_region, join=st_intersects, left=FALSE)
  return(place_lookup)
}

#' Helper to ascertain datatype of variable
#'
#' @param x vector, e.g. variable
#' @return string with census geography & year
testInteger <- function(x){
  func <- function(x){all.equal(x, as.integer(x), check.attributes = FALSE)}
  test <- x %>% lapply(func) %>% unlist() %>% all()
  if(test==TRUE){return(TRUE)
  }else{return(FALSE)}
}

#' Helper to ascertain census geography from psrccensus table
#'
#' @param df acs or decennial dataset returned from psrccensus
#' @return string with census geography & year
#' @author Michael Jensen
#'
#' @importFrom dplyr pull
#' @import glue glue
#'
identify_censusgeo <- function(df){
  data_year <- cb_geo_yr <- fips_lookup <- fips_length <- cb_geo <- digits <- geo <- NULL # For Roxygen
  tryCatch({
    data_year <- dplyr::pull(df, year) %>% unique()
    cb_geo_yr <- (data_year - (data_year %% 10)) %% 100 %>% as.character()
    fips_lookup <- data.frame(digits=c(11,12,15), geo=c("tract","blockgroup","block")) %>% setDT()
    fips_length <- dplyr::pull(df, GEOID) %>% as.character() %>% nchar() %>% max()
    cb_geo <- fips_lookup[digits==fips_length, geo] %>% paste0(cb_geo_yr)
    return(cb_geo)
  }, error = function(e) {
    print(glue::glue("Error: {e}"))
    return(NULL)
  })
}

#' Helper to warn if input table doesn't meet criteria for geographic conversion
#'
#' @param df acs or decennial dataset returned from psrccensus
#' @return NULL (if fails); 1 (if succeeds)
#' @author Michael Jensen
#'
#' @importFrom dplyr pull
#'
test_df_for_conversion <- function(df){
  data_year <- cb_geo <- NULL # For Roxygen
  data_year <- dplyr::pull(df, year) %>% unique()
  if(data_year < 2010){
    warning("Splits for this year are not yet stored in Elmer")
    return(FALSE)
  }else if(length(data_year)!=1){
    warning("Data is not specific to single year")
    return(FALSE)
  }else if(is.null(identify_censusgeo(df))){
    warning("Only census tracts, block groups, or blocks are valid")
    return(FALSE)
  }else{
    return(TRUE)
  }
}

#' Helper to verify custom geometry input meets criteria for geographic conversion
#'
#' @param custom_geo custom sf file with intended geography/geometry
#' @param custom_geo_var grouping field from custom geography file, i.e. geography label
#' @return verified sf object
#' @author Michael Jensen
#'
#' @importFrom dplyr pull select
#' @importFrom sf st_make_valid st_transform
#'
verify_custom_geo <- function(custom_geo, custom_geo_var){
  tryCatch({
    customgeo <- custom_geo %>% sf::st_make_valid() %>%
      dplyr::select(!!custom_geo_var) %>% sf::st_transform(2285)
    return(customgeo)
  },
  error = function(cond) {
    message("custom_geo input must be sf polygon file")
    message("custom_geo_var must exist as column label in sf polygon file")
    return(NULL)
  })
}

#' Helper to translate psrccensus estimates to planning geographies
#'
#' @param df acs or decennial dataset returned from psrccensus
#' @param planning_geog_type planning geography type as listed in Elmer.general.geography_splits
#' @param wgt measure share used as split weight
#'            either "total_pop", "household_pop", "group_quarters_pop", "housing_units" or "occupied_housing_units"
#' @param ofm_vintage for deprecated splits; otherwise keep default. See <http://aws-linux/mediawiki/index.php/Get_geography_splits_(Elmer_Function)>
#' @param parcel_year for deprecated baseyear; otherwise keep default. See <http://aws-linux/mediawiki/index.php/Get_geography_splits_(Elmer_Function)>
#' @return table with planning geography units in place of census geography units
#' @author Michael Jensen
#'
#' @importFrom psrcelmer get_query
#' @importFrom dplyr pull
#' @importFrom tidycensus moe_sum
#' @rawNamespace import(data.table, except = c(month, year))
use_geography_splits <- function(df, planning_geog_type, wgt, ofm_vintage="NULL", parcel_year="NULL"){

  geography_splits_helper <- function(df){
    digits <- geo <- data_geog_type <- ofm_estimate_year <- value <- estimate <- moe <- NULL       # For roxygen
    if(!test_df_for_conversion(df)==TRUE){
      return(NULL)
    }else{
      options(useFancyQuotes = FALSE)
      fullwgt <- paste0("percent_of_", wgt)
      data_year <- dplyr::pull(df, year) %>% unique()
      cb_geo <- identify_censusgeo(df)
      sql_str <- paste0("SELECT * FROM Elmer.general.get_any_geography_splits(",                   # SQL table-value function returns data
                   paste(sQuote(cb_geo), sQuote(planning_geog_type),
                   data_year, ofm_vintage, parcel_year, sep=", "), ");")
      group_cols <- grep("(state|year|variable|label|concept|acs_type)", colnames(df), value=TRUE) %>%
        append("planning_geog", after=0)
      value_col <- grep("(value|estimate)", colnames(df), value=TRUE)                              # Decennial:value; ACS:estimate
      rosetta <- psrcelmer::get_query(sql_str) %>% setDT() %>%                                     # Must be on PSRC VPN to connect to Elmer
        .[(data_geog_type=={{cb_geo}} &                                                            # Keep only necessary rows & columns
           planning_geog_type=={{planning_geog_type}} &
           ofm_estimate_year=={{data_year}}),
          grepl("(_geog$|^percent_of)", colnames(.)), with=FALSE] %>%
        setnames("data_geog", "GEOID") %>% setkey(GEOID)
      df %<>% setDT() %>% setkey(GEOID) %>% merge(rosetta, allow.cartesian=TRUE)                   # Merge on key=GEOID
      est_is_integer <- testInteger(df %>% dplyr::pull({{value_col}}))
      if(value_col=="value"){                                                                      # Decennial
        rsi <- df[, .(value=sum(value * get(fullwgt))), by=mget(group_cols)]
      }else if(value_col=="estimate"){                                                             # ACS
        rsi <- df[, .(estimate = sum(estimate * get(fullwgt)),
                     moe = tidycensus::moe_sum((moe * get(fullwgt)), (estimate * get(fullwgt)), na.rm=TRUE)), # MOE calculation
                 by=mget(group_cols)]
        if(est_is_integer){rsi[, `:=`(estimate=round(estimate), moe=round(moe))]}
      }else{rsi <- NULL}
      return(rsi)
      }
    }
  rso <- df %>% split(.$year) %>%
    lapply(geography_splits_helper) %>% rbindlist()
  return(rso)
}


#' Translate psrccensus data to planning geographies
#'
#' @param df acs or decennial dataset returned from psrccensus
#' @param wgt either "total_pop" (default), "household_pop", "group_quarters_pop", "housing_units" or "occupied_housing_units"
#' @name census_to_psrcgeo
#'
#' @importFrom data.table rbindlist
#' @return Equivalent table with planning geography units instead of census geography units, and translated value and margin of error
#' @author Michael Jensen
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
  rs <- use_geography_splits(df, planning_geog_type="Regional Growth Center (12/12/2023)", wgt=wgt)
  return(rs)
}

#' @rdname census_to_psrcgeo
#' @title Translate psrccensus data to Regional Manufacturing-Industrial Centers
#' @export
census_to_mic <- function(df, wgt="total_pop"){
  rs <- use_geography_splits(df, planning_geog_type="MIC (1/5/2024)", wgt=wgt)
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

#' @rdname census_to_psrcgeo
#' @title Translate psrccensus data to 2020 Jurisdictional boundaries
#' @export
census_to_juris <- function(df, wgt="total_pop"){
  rs <- use_geography_splits(df, planning_geog_type="Jurisdiction 2020", wgt=wgt)
  return(rs)
}

#' Function to flexibly convert psrccensus estimates to any regional sf geography
#'
#' @param df acs or decennial dataset returned from psrccensus
#' @param custom_geo custom sf file with intended geography/geometry
#' @param custom_geo_var grouping variable from custom geography file, i.e. geography label
#' @param wgt measure share used as split weight
#'            either "total_pop" (default), "household_pop", "group_quarters_pop", "housing_units" or "occupied_housing_units"
#' @return table with custom geographic units in place of census geography units
#' @author Michael Jensen
#'
#' @importFrom psrcelmer get_query
#' @importFrom dplyr pull
#' @importFrom tidycensus moe_sum
#' @importFrom sf st_make_valid st_transform st_join st_drop_geometry st_as_sf st_crs
#' @rawNamespace import(data.table, except = c(month, year))
#'
#' @export
census_to_customgeo <- function(df, custom_geo, custom_geo_var, wgt="total_pop"){
  parcel_dim_id <- x_coord_state_plane <- y_coord_state_plane <- customgeo <- rso <- NULL # For roxygen

  ready_geosplits_helper <- function(df){
    planning_geog <- fraction <- value <- estimate <- moe <- rsi <- NULL
    if(!test_df_for_conversion(df)==TRUE){
      return(NULL)
    }else{
      data_year <- pull(df, year) %>% unique() # Census data year from df
      group_cols <- grep("(state|year|variable|label|concept|acs_type)", colnames(df), value=TRUE) %>%
        append("planning_geog", after=0)
      value_col <- grep("(value|estimate)", colnames(df), value=TRUE)
      cb_geo <- identify_censusgeo(df)
      sql <- paste("SELECT p.parcel_dim_id, p.x_coord_state_plane, p.y_coord_state_plane,",
                   paste0("p.", cb_geo), "AS GEOID,",paste0("p.", wgt, "_share_", cb_geo),
                   "AS fraction FROM Elmer.general.parcel_level_census_splits AS p",
                   "WHERE p.estimate_year=", data_year, ";")
      message("1 of 2 - Downloading parcel-level splits from Elmer. This may take a minute or two.")
      parcel_share <- psrcelmer::get_query(sql) %>% setDT() %>% .[, GEOID:=as.character(GEOID)]
      parcel_geo <- dplyr::select(parcel_share, c(parcel_dim_id, x_coord_state_plane, y_coord_state_plane)) %>%
        sf::st_as_sf(coords = c(2:3))
      sf::st_crs(parcel_geo) <- 2285
      message("2 of 2 - Performing spatial join & applying splits. This may take several seconds.")
      parcel_geo <- parcel_geo %>% sf::st_join(customgeo, left=FALSE) %>%                          # Associate parcels with custom geog
        sf::st_drop_geometry() %>% setDT() %>% setkey(parcel_dim_id)
      parcel_share %<>% setDT() %>% setkey(parcel_dim_id) %>%
        .[parcel_geo, planning_geog:=get(custom_geo_var)] %>%
        .[, .(split=sum(fraction)), by=.(GEOID, planning_geog)] %>% setkey(GEOID)                  # Summarize to custom geog x census geog level
      df %<>% setDT() %>% setkey(GEOID) %>% merge(parcel_share, allow.cartesian=TRUE)              # Merge on key=GEOID
      est_is_integer <- testInteger(df %>% dplyr::pull({{value_col}}))
      if(value_col=="value"){                                                                      # Decennial
        rsi <- df[, .(value=sum(value * split)), by=mget(group_cols)]
      }else if(value_col=="estimate"){                                                             # ACS
        rsi <- df[, .(estimate = sum(estimate * split),
                      moe = tidycensus::moe_sum((moe * split), (estimate * split), na.rm=TRUE)), # MOE calculation
                  by=mget(group_cols)]
        if(est_is_integer){rsi[, `:=`(estimate=round(estimate), moe=round(moe))]}
      }
    return(rsi)
    }
  }

  customgeo <- verify_custom_geo(custom_geo, custom_geo_var)
  if(is.null(customgeo)){
    return(NULL)
  }else{
    rso <- df %>% split(.$year) %>%                                                                    # In case table has multiple years
      lapply(ready_geosplits_helper) %>% rbindlist()
  }
  return(rso)
}
