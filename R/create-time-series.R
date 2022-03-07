
#' Time Series Visualization
#'
#' Generate Time Series Charts for Census Data
#'
#' @param table Data frame of census data by Counties or Region.
#' @param variable Character string as Census code for a variable to plot.
#' @param ts.title Character. Chart title. If not specified, the title will be based on a variable name given by Census.
#' @param print.table Boolean. If TRUE table will be returned.
#'
#' @author Polina Butrina
#'
#' @return a time series chart or table.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' library(psrccensus)
#'
#' {
#' Sys.getenv("CENSUS_API_KEY")
#' }
#'
#' ts.table <- get_acs_recs(geography = 'county', table.names = c('B01001'), years=c(2019,2018,2017),
#'                     acs.type = 'acs5')
#'
#' get_time_series(ts.table, "B01001_002")
#'
#' @export
get_time_series <- function(table, variable, ts.title = NULL, print.table = FALSE) {
  variable <-  rlang::enquo(variable)
  df_for_plot <- table %>%
    dplyr::rename_at(dplyr::vars(tidyselect::matches("value")), function(x) "estimate") %>%
    dplyr::rename_at(dplyr::vars(tidyselect::matches("NAME")), function(x) "name") %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::filter(variable == (!!variable))

  if('Region' %in% df_for_plot$name) {
    # remove state from name
    df_for_plot <- df_for_plot %>%
      dplyr::mutate(name = ifelse(stringr::str_detect(.data$name, ', Washington'), stringr::str_extract(.data$name, "^.*(?=,)"), .data$name))

    counties <- c('King County', 'Kitsap County', 'Pierce County', 'Snohomish County', 'Region')
    df_for_plot$name <- factor(df_for_plot$name, levels = counties)
  }

  if(print.table == TRUE) {
    df_for_plot <-  df_for_plot %>%
      dplyr::select(.data$name, .data$variable, .data$estimate, .data$moe, .data$label, .data$year)
    return(df_for_plot)
  }

  if(missing(ts.title)) {
    concept <- stringr::str_to_title(df_for_plot$concept[1])

    if(stringr::str_detect(df_for_plot$variable[1], '_') == TRUE) {
      table.code <- stringr::str_extract(df_for_plot$variable[1], "^\\w+(?=_)")
    } else {
      table.code <- stringr::str_sub(df_for_plot$variable[1], 1, (nchar(df_for_plot$variable[1])-3))
    }
    ts.title <- paste0(concept,": ", table.code)
  }

  ts.subtitle <- stringr::str_replace_all(df_for_plot$label[1], "!!", " > ") %>%
    stringr::str_replace_all(":", " ")

  m <-  ggplot2::ggplot(df_for_plot, ggplot2::aes(x = .data$year,
                                                  y = .data$estimate,
                                                  group = .data$name,
                                                  color = .data$name)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(color = NULL,
                  title = ts.title,
                  x = NULL,
                  y = NULL,
                  subtitle = ts.subtitle)
  return(m)
}
