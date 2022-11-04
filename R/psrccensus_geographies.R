#' @importFrom magrittr %<>% %>%
#' @author Michael Jensen
NULL

globalVariables(c(":=", "!!", ".", "enquos"))

`%not_in%` <- Negate(`%in%`)

#' List Census Places within the PSRC region
#'
#' @param year of census geography
#' @return string vector of Place FIPS codes
#'
#' @importFrom sf st_transform st_buffer st_join st_intersects st_drop_geometry
#' @importFrom dplyr filter select rename
#' @export
get_psrc_places <- function(year){
  psrc_region <- counties <- COUNTYFP <- place_lookup <- places <- GEOID <- NAME <- geometry <- NULL
  psrc_region <- tigris::counties("53", cb=TRUE) %>%
    filter(COUNTYFP %in% c("033","035","053","061")) %>% dplyr::summarize() %>%
    st_transform(2285) # planar projection to allow intersect
  place_lookup <- tigris::places("53", year=year, cb=TRUE) %>%
    select(c(GEOID, NAME, geometry)) %>% st_transform(2285) %>% st_buffer(-1) %>% # To avoid any overlap
    st_join(psrc_region, join=st_intersects, left=FALSE)
}
