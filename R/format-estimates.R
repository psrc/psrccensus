#' Format Estimates
#'
#' Format estimates of a county summary table with the option to include shares and share margins of error.
#'
#' @param table A data frame/tibble from `get_acs_recs()` for a single table and time period.
#' @param type A character, select either 'total' or 'share'.
#' @param moe A logical value, TRUE or FALSE to include or exclude margins of error.
#'
#' @author Christy Lam
#'
#' @return A data frame of ACS estimates by PSRC counties and region of either total estimates or proportions, with or without margins of
#' error.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' df <-get_acs_recs(geography = 'county',
#'                                table.names = c('B03002'),
#'                                years = c(2019),
#'                                acs.type = 'acs1')
#'
#' format_estimates(df)
#' format_estimates(df, type = 'share', moe = FALSE)
#'
#' t <- get_acs_recs(geography = 'county',
#'                   counties = c("King", "Kitsap"),
#'                   table.names = c('B03002'),
#'                   years = c(2019),
#'                   acs.type = 'acs1')
#'
#' format_estimates(t, type = 'share', moe = TRUE)
#' @export
format_estimates <- function(table, type = 'total', moe = TRUE){

  estimate <- summary_est <- summary_moe <- name <- geog <- NULL   # Bind variables locally (for documentation, not function)
  # replace_na moe with 0s
  df <- table %>%
    dplyr::mutate(moe = tidyr::replace_na(moe, 0))
  # each unique name, subset as tbl
  geogs <- unique(df$name)
  df_list <- purrr::map(geogs, ~df %>% dplyr::filter(df$name == .x))

  # add summary_est, summary_moe
  df_list <- purrr::map(df_list, ~.x %>% dplyr::mutate(summary_est = .x[.x$variable == stringr::str_subset(.x$variable, "001$"), 'estimate'][[1]],
                                                       summary_moe = .x[.x$variable == stringr::str_subset(.x$variable, "001$"), 'moe'][[1]]))

  # add share, share_moe
  adf <- purrr::reduce(df_list, dplyr::bind_rows)

  adf <- adf %>%
    dplyr::mutate(share = estimate/summary_est,
                  share_moe = tidycensus::moe_prop(estimate, summary_est, moe, summary_moe))

  # select columns
  if(type == 'total') {
    adf <- adf %>%
      dplyr::select(-dplyr::starts_with('summary'), -dplyr::contains('share'))
  } else {
    adf <- adf %>%
      dplyr::select(-dplyr::starts_with('summary'), -estimate, -moe)
  }

  if(moe == FALSE) {
    adf <- adf %>% dplyr::select(-dplyr::contains('moe'))
  }

  # organize column names
  rm.cols <- c('GEOID', 'state', 'census_geography', 'name')
  val.cols <- setdiff(colnames(adf), c(rm.cols, c('variable', 'label', 'concept', 'acs_type', 'year')))

  # pivot table
  adf <- adf %>%
    dplyr::mutate(geog = stringr::str_extract(name, "^\\w+")) %>%
    dplyr::select(-dplyr::all_of(rm.cols)) %>%
    tidyr::pivot_wider(names_from = geog,
                       values_from = dplyr::all_of(val.cols),
                       names_glue = '{geog}_{.value}')

  # reorder columns
  sort.cols <- setdiff(colnames(adf), c('variable', 'label', 'concept', 'acs_type', 'year'))
  new.col.order <- c(c('variable', 'label', 'concept', 'acs_type', 'year'), sort(sort.cols))
  adf <- adf %>%
    dplyr::select(dplyr::all_of(new.col.order))

  region.col <- stringr::str_subset(colnames(adf), "^Region")

  if(!is.null(region.col)) {
  adf <- adf %>%
    dplyr::relocate(dplyr::starts_with('Region'), .after = dplyr::last_col())
  }

  return(adf)
}
