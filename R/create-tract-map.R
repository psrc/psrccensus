#' Census Tract Mapping
#'
#' Generate Census Tract Maps for Census Data
#'
#' @param tract.tbl A data frame of census data by Census Tract to map.
#' @param tract.lyr A spatial layer of Census Tracts.
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
#' Sys.getenv("CENSUS_API_KEY")
#' tract.tbl <- psrccensus::get_acs_recs(geography='tract',table.names=c('B02018'),years=c(2019))
#' gdb.nm = paste0("MSSQL:server=","AWS-PROD-SQL\\Sockeye",
#' ";database=","ElmerGeo",";trusted_connection=yes")
#' spn = 2285
#' tract_layer_name="dbo.tract2010_nowater"
#' tract.lyr <- st_read(gdb.nm, tract_layer_name, crs = spn)
#' create_tract_map(tract.tbl, tract.lyr)
#' @export
create_tract_map<-function(tract.tbl, tract.lyr, map.lat=47.615, map.lon=-122.257, map.zoom=8.5, wgs84=4326){

  # Summarize and Aggregate Tract Data by Year and Attribute to Map and join to tract layer for mapping
  tbl <- tract.tbl %>%
    dplyr::select(.data$GEOID,.data$estimate) %>%
    dplyr::mutate(dplyr::across(c('GEOID'), as.character))%>%
    dplyr::group_by(.data$GEOID) %>%
    dplyr::summarise(Total=sum(.data$estimate))

  c.layer <- dplyr::left_join(tract.lyr,tbl, by = c("geoid10"="GEOID")) %>%
    sf::st_transform(wgs84)

  # Calculate Bins from Data and create color palette
  rng <- range(c.layer$Total)
  max_bin <- max(abs(rng))
  round_to <- 10^floor(log10(max_bin))
  max_bin <- ceiling(max_bin/round_to)*round_to
  breaks <- (sqrt(max_bin)*c(0.1, 0.2,0.4, 0.6, 0.8, 1))^2
  bins <- c(0, breaks)

  pal <- leaflet::colorBin("YlOrRd", domain = c.layer$Total, bins = bins)

  labels <- paste0("Census Tract ", c.layer$geoidstr, '<p></p>',
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
                         opacity = 0.7,
                         weight = 0.7,
                         color = "#BCBEC0",
                         group="Population",
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
                       values = c.layer$estimate,
                       position = "bottomright") %>%

    leaflet::addLayersControl(baseGroups = "CartoDB.VoyagerNoLabels",
                              overlayGroups = c("Labels", "Population")) %>%

    leaflet::setView(lng=map.lon, lat=map.lat, zoom=map.zoom)

  return(m)

}
