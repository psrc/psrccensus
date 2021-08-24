library(psrccensus)
library(stringr)
library(tidyr)
library(dplyr)
library(htmltools)

Sys.getenv("CENSUS_API_KEY")

# format_estimates() takes three arguments, two of which have defaults.
# 'type' can be 'total' or 'share'. 'moe' can be TRUE or FALSE.
# test <- format_estimates(mydf, type = 'share', moe = FALSE)

mydf <- get_acs_recs(geography = 'county',
                   table.names = c('B03002'),
                   years=c(2019),
                   acs.type = 'acs1')

format_estimates <- function(df, type = 'total', moe = TRUE) {
  # check if data.frame census_geography is County
  if(!unique((df$census_geography)) %in% c("County", 'county')) {
    return(message('Error: ACS estimates dataframe must be "County" in census_geography'))
  }

  # check that data is for one time period
  if(length(unique((df$year))) != 1) {
    return(message('Error: ACS estimates dataframe must be limited to one time period'))
  }

  z <- 1.96

  df1 <- df %>%
    select(-GEOID) %>%
    mutate(label = str_replace_all(label, ":", ""))

  df1_wide <- df1 %>%
    pivot_wider(names_from = c(name, year),
                values_from = c(estimate, moe),
                names_glue = "{name}_{year}_{.value}"
    )

  # calculate shares and share MOE
  df2 <- df1_wide %>%
    # create shares
    mutate(across(ends_with('estimate'), ~.x/.x[[1]], .names = "{.col}_share")) %>%
    # calc 1-p
    mutate(across(ends_with('share'), ~1-.x, .names = "{.col}_p1")) %>%
    # calc p(1-p)
    mutate(across(ends_with('share'), .names = "{.col}_pp1")*across(ends_with("p1"))) %>%
    # create new col with n (denominator)
    mutate(across(ends_with('estimate'), ~.x[[1]], .names = "{.col}_n")) %>%
    # calc sqrt of p(1-p)/n
    mutate(sqrt(across(ends_with('pp1'), .names = "{.col}_sqrt")/across(ends_with('n')))) %>%
    # multiply by z
    mutate(z*across(ends_with("sqrt"), .names = "{.col}_sharemoe"))

  # return clean dataframe
  if(type == 'total') {
    t <- df2 %>%
      select(variable, ends_with('estimate'), ends_with('_moe'))
  } else if(type == 'share') {
    t <- df2 %>%
      select(variable, ends_with('share'), ends_with('sharemoe')) %>%
      rename_with(~str_replace_all(.x, '_estimate', ''), .cols = ends_with('share')) %>%
      rename_with(~str_replace_all(.x, 'estimate_share_pp1_sqrt_sharemoe', 'share_moe'), .cols = ends_with('sharemoe'))
  }

  if(moe == FALSE) {
    t <- t %>% select(-ends_with('moe'))
  }

  new_colorder <- c('variable', sort(colnames(t)[2:length(colnames(t))]))

  t <- t %>%
    select(all_of(new_colorder)) %>%
    relocate(starts_with('Region'), .after = last_col())

  # append id columns to selected dataframe
  id.cols <- c('variable', 'label', 'concept', 'census_geography', 'state', 'acs_type')

  tt <- df2 %>%
    select(all_of(id.cols)) %>%
    left_join(t, by = 'variable')

  return(tt)
}


