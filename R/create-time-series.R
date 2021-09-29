
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
#' library(rlang)
#' library(dplyr)
#' library(rlang)
#' library(scales)
#' library(stringr)
#' library(psrccensus)
#' library(ggplot2)
#'
#' {
#' Sys.getenv("CENSUS_API_KEY")
#' }
#'
#' ts.table = get_acs_recs(geography = 'county', table.names = c('B01001'), years=c(2019,2018,2017),
#'                     acs.type = 'acs5')

#'
#' get_time_series(ts.table, "B01001_002")
#'
#' @export

get_time_series  = function(table,
                            variable,
                            ts.title = NULL,
                            print.table = FALSE) {
  variable = rlang::enquo(variable)

  df_for_plot = table %>%
    dplyr::rename_at(dplyr::vars(matches("value")), function(x)
      "estimate") %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::filter(variable == (!!variable))

  if (print.table == TRUE) {
    df_for_plot = df_for_plot %>%
      select(name, variable, estimate, moe, label, year)
    return(df_for_plot)
  }

  if (missing(ts.title)) {
    ts.title = stringr::str_replace_all(df_for_plot$label[1], "[!:]", " ")
    ts.title = stringr::str_replace_all(ts.title, "   ", " ")
  }

  legend.title = df_for_plot$census_geography[1]

  m = ggplot2::ggplot(df_for_plot, ggplot2::aes(
    x = year,
    y = estimate,
    group = name,
    color = name
  )) +
    ggplot2::geom_line(size = 1) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::geom_point() +
    ggplot2::ggtitle(ts.title) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(color = legend.title)
  return (m)
}
