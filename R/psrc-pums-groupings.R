#' @importFrom magrittr %<>% %>%
#' @importFrom dplyr filter mutate case_when
#' @author Michael Jensen
NULL

`%not_in%` <- Negate(`%in%`)

#' PSRC standard broad income groupings
#'
#' @param so The srvyr object returned by \code{\link{get_psrc_pums}}, with field HINCP (required)
#' @return The srvyr object with an additional binned household income field, "BINCOME"
#' @export
psrc_bincome <- function(so){
  so %<>% mutate(BINCOME=factor(case_when(
                                  HINCP <  25000 ~"Under $25,000",
                                  HINCP <  50000 ~"$25,000-$49,999",
                                  HINCP <  75000 ~"$50,000-$74,999",
                                  HINCP < 100000 ~"$75,000-$99,999",
                                  HINCP >=100000 ~"$100,000 or more",
                                  TRUE ~ "Else / Prefer not to answer"),
                          levels=c("Under $25,000",
                                   "$25,000-$49,999",
                                   "$50,000-$74,999",
                                   "$75,000-$99,999",
                                   "$100,000 or more",
                                   "Else / Prefer not to answer")))
  return(so)
}

#' PSRC standard age groupings
#'
#' @param so The srvyr object returned by \code{\link{get_psrc_pums}}, with field AGEP (required)
#' @return The srvyr object with an additional binned age field, "BINAGE"
#' @export
psrc_bin_age <- function(so){
  so %<>% mutate(BINAGE=factor(case_when(
                                  AGEP < 5  ~"under 5 years",
                                  AGEP < 12 ~"between 5 and 11 years",
                                  AGEP < 16 ~"between 12 and 15 years",
                                  AGEP < 18 ~"between 16 and 17 years",
                                  AGEP < 25 ~"between 18 and 24 years",
                                  AGEP < 35 ~"between 25 and 35 years",
                                  AGEP < 45 ~"between 35 and 45 years",
                                  AGEP < 55 ~"between 45 and 55 years",
                                  AGEP < 65 ~"between 55 and 65 years",
                                  AGEP < 75 ~"between 65 and 75 years",
                                  AGEP < 85 ~"between 75 and 85 years",
                                  AGEP >=85 ~"85 years and over",
                                  TRUE ~ "Else / Prefer not to answer"),
                          levels=c("under 5 years",
                                   "between 5 and 11 years",
                                   "between 12 and 15 years",
                                   "between 16 and 17 years",
                                   "between 18 and 24 years",
                                   "between 25 and 35 years",
                                   "between 35 and 45 years",
                                   "between 45 and 55 years",
                                   "between 55 and 65 years",
                                   "between 65 and 75 years",
                                   "between 75 and 85 years",
                                   "Else / Prefer not to answer")))
  return(so)
}
