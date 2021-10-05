#' Census Tract Mapping
#'
#' Generate Census Tract Maps for Census Data
#'
#' @param tract.tbl A data frame of census data by Census Tract to map.
#' @param tract.lyr A spatial layer of Census Tracts.
#' @param map.title Character. Map title.
#' @param map.subtitle Character. A map subtitle that will appear below the map title.
#' @param map.title.position Character. Place the map title and subtitle in 'bottomleft','bottomright', topleft', or 'topright'.
#' @param legend.title Character. Legend title.
#' @param legend.subtitle Character. A legend subtitle that will appear below the legend subtitle.
#' @param map.lat A numeric value for the latitude of the center point for your map. Defaults to 47.615 (PSRC Region)
#' @param map.lon A numeric value for the longitude of the center point for your map. Defaults to -122.257 (PSRC Region)
#' @param map.zoom A numeric value for the default zoom level for your map. Defaults to 8.5 (PSRC Region)
#' @param wgs84 A code for mapping, this should be pulled out of the function or something
#'
#' @author Suzanne Childress
#'
#' @return a leaflet map object.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' library(sf)
#' library(dplyr)
#' \dontrun{
#' Sys.getenv("CENSUS_API_KEY")
#' }
#'
#' tract.big.tbl <- psrccensus::get_acs_recs(geography='tract',table.names=c('B03002'),years=c(2019))
#' tract.tbl <- tract.big.tbl %>%
#' filter(label=='Estimate!!Total:!!Not Hispanic or Latino:!!Black or African American alone')
#'
#' gdb.nm <- paste0("MSSQL:server=",
#' "AWS-PROD-SQL\\Sockeye",
#' ";database=",
#' "ElmerGeo",
#' ";trusted_connection=yes")
#'
#' spn <-  2285
#'
#' tract_layer_name <- "dbo.tract2010_nowater"
#'
#' tract.lyr <- st_read(gdb.nm, tract_layer_name, crs = spn)
#'
#' create_tract_map(tract.tbl=tract.tbl, tract.lyr=tract.lyr,
#' map.title='Black, non-Hispanic Population',
#',map.title.position='topleft', legend.title='Black, Non-Hispanic Population',
#' legend.subtitle='by Census Tract')
#'
#' @export
create_tract_map <- function(tract.tbl, tract.lyr,
                             map.title = NULL, map.subtitle = NULL,
                             map.title.position = NULL,
                             legend.title = NULL, legend.subtitle = NULL,
                             map.lat=47.615, map.lon=-122.257, map.zoom=8.5, wgs84=4326){


  # Summarize and Aggregate Tract Data by Year and Attribute to Map and join to tract layer for mapping
  # rename census value column to estimate to match ACS
  # also allow for the easy mapping of equity geographies
  tbl <- tract.tbl %>%
    dplyr::rename_at(dplyr::vars(matches("value")),function(x) "estimate") %>%
    dplyr::rename_at(dplyr::vars(matches("equity_geog_vs_50_percent")),function(x) "estimate") %>%
    dplyr::rename_at(dplyr::vars(matches('equity_geog_vs_reg_total')),function(x) "estimate") %>%
    dplyr::rename_at(dplyr::vars(matches("geoid")),function(x) "GEOID") %>%
    dplyr::select(.data$GEOID,.data$estimate) %>%
    dplyr::mutate(dplyr::across(c('GEOID'), as.character))%>%
    dplyr::group_by(.data$GEOID) %>%
    dplyr::summarise(Total=sum(.data$estimate))

    tract.lyr<-tract.lyr%>%
    # make geo names across 2010 and 2020
    dplyr::rename_at(dplyr::vars(matches("geoid10")),function(x) "geoid") %>%
    dplyr::rename_at(dplyr::vars(matches("geoid20")),function(x) "geoid")

  c.layer <- dplyr::left_join(tract.lyr,tbl, by = c("geoid"="GEOID")) %>%
    sf::st_transform(wgs84)

  pal <- leaflet::colorNumeric(palette="Purples", domain = c.layer$Total)


  labels <- paste0("Census Tract: ", c.layer$geoid, '<p></p>',
                   'Total: ', prettyNum(round(c.layer$Total, -1), big.mark = ",")) %>% lapply(htmltools::HTML)

  m <- leaflet::leaflet() %>%
    leaflet::addMapPane(name = "polygons", zIndex = 410) %>%
    leaflet::addMapPane(name = "maplabels", zIndex = 500) %>% # higher zIndex rendered on top

    leaflet::addProviderTiles("CartoDB.VoyagerNoLabels") %>%
    leaflet::addProviderTiles("CartoDB.VoyagerOnlyLabels",
                              options = leaflet::leafletOptions(pane = "maplabels"),
                              group = "Labels") %>%

    leaflet::addEasyButton(leaflet::easyButton(icon="fa-globe",
                                               title="Region",
                                               onClick=leaflet::JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) %>%
    leaflet::addPolygons(data=c.layer,
                         fillOpacity = 0.7,
                         fillColor = pal(c.layer$Total),
                         weight = 0.7,
                         color = "#BCBEC0",
                         group="Population",
                         opacity = 0,
                         stroke=FALSE,
                         options = leaflet::leafletOptions(pane = "polygons"),
                         dashArray = "",
                         highlight = leaflet::highlightOptions(
                           weight =5,
                           color = "76787A",
                           dashArray ="",
                           fillOpacity = 0.7,
                           bringToFront = TRUE),
                         label = labels,
                         labelOptions = leaflet::labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto")) %>%

    leaflet::addLegend(pal = pal,
                       values = c.layer$Total,
                       position = "bottomright",
                       title = paste(legend.title, '<br>', legend.subtitle)) %>%

    leaflet::addControl(html = paste(map.title, '<br>', map.subtitle),
                        position = map.title.position,
                        layerId = 'mapTitle') %>%

    leaflet::addLayersControl(baseGroups = "CartoDB.VoyagerNoLabels",
                              overlayGroups = c("Labels", "Population")) %>%

    leaflet::setView(lng=map.lon, lat=map.lat, zoom=map.zoom)

  return(m)

}
