# Format Estimates

Format estimates of a county summary table with the option to include
shares and share margins of error.

## Usage

``` r
format_estimates(table, type = "total", moe = TRUE)
```

## Arguments

- table:

  A data frame/tibble from \`get_acs_recs()\` for a single table and
  time period.

- type:

  A character, select either 'total' or 'share'.

- moe:

  A logical value, TRUE or FALSE to include or exclude margins of error.

## Value

A data frame of ACS estimates by PSRC counties and region of either
total estimates or proportions, with or without margins of error.

## Author

Christy Lam

## Examples

``` r
df <-get_acs_recs(geography = 'county',
                               table.names = c('B03002'),
                               years = c(2019),
                               acs.type = 'acs1')
#> Getting data from the 2019 1-year ACS
#> The 1-year ACS provides data for geographies with populations of 65,000 and greater.
#> Loading ACS1 variables for 2019 from table B03002. To cache this dataset for faster access to ACS tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per ACS dataset.
#> Using FIPS code '53' for state 'Washington'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'

format_estimates(df)
#> # A tibble: 21 × 30
#>    variable   label        concept acs_type  year King_cv King_estimate King_moe
#>    <chr>      <chr>        <chr>   <chr>    <dbl>   <dbl>         <dbl>    <dbl>
#>  1 B03002_001 Estimate!!T… HISPAN… acs1      2019 0             2252782        0
#>  2 B03002_002 Estimate!!T… HISPAN… acs1      2019 0             2030140        0
#>  3 B03002_003 Estimate!!T… HISPAN… acs1      2019 0.00150       1302544     3208
#>  4 B03002_004 Estimate!!T… HISPAN… acs1      2019 0.0192         147822     4678
#>  5 B03002_005 Estimate!!T… HISPAN… acs1      2019 0.0908          13321     1990
#>  6 B03002_006 Estimate!!T… HISPAN… acs1      2019 0.0101         424590     7085
#>  7 B03002_007 Estimate!!T… HISPAN… acs1      2019 0.0709          15702     1831
#>  8 B03002_008 Estimate!!T… HISPAN… acs1      2019 0.303            6574     3281
#>  9 B03002_009 Estimate!!T… HISPAN… acs1      2019 0.0448         119587     8804
#> 10 B03002_010 Estimate!!T… HISPAN… acs1      2019 0.402            2639     1744
#> # ℹ 11 more rows
#> # ℹ 22 more variables: King_reliability <chr>, King_se <dbl>, Kitsap_cv <dbl>,
#> #   Kitsap_estimate <dbl>, Kitsap_moe <dbl>, Kitsap_reliability <chr>,
#> #   Kitsap_se <dbl>, Pierce_cv <dbl>, Pierce_estimate <dbl>, Pierce_moe <dbl>,
#> #   Pierce_reliability <chr>, Pierce_se <dbl>, Snohomish_cv <dbl>,
#> #   Snohomish_estimate <dbl>, Snohomish_moe <dbl>, Snohomish_reliability <chr>,
#> #   Snohomish_se <dbl>, Region_cv <dbl>, Region_estimate <dbl>, …
format_estimates(df, type = 'share', moe = FALSE)
#> # A tibble: 21 × 25
#>    variable   label      concept acs_type  year King_cv King_reliability King_se
#>    <chr>      <chr>      <chr>   <chr>    <dbl>   <dbl> <chr>              <dbl>
#>  1 B03002_001 Estimate!… HISPAN… acs1      2019 0       good                  0 
#>  2 B03002_002 Estimate!… HISPAN… acs1      2019 0       good                  0 
#>  3 B03002_003 Estimate!… HISPAN… acs1      2019 0.00150 good               1950.
#>  4 B03002_004 Estimate!… HISPAN… acs1      2019 0.0192  good               2844.
#>  5 B03002_005 Estimate!… HISPAN… acs1      2019 0.0908  good               1210.
#>  6 B03002_006 Estimate!… HISPAN… acs1      2019 0.0101  good               4307.
#>  7 B03002_007 Estimate!… HISPAN… acs1      2019 0.0709  good               1113.
#>  8 B03002_008 Estimate!… HISPAN… acs1      2019 0.303   use with caution   1995.
#>  9 B03002_009 Estimate!… HISPAN… acs1      2019 0.0448  good               5352.
#> 10 B03002_010 Estimate!… HISPAN… acs1      2019 0.402   use with caution   1060.
#> # ℹ 11 more rows
#> # ℹ 17 more variables: King_share <dbl>, Kitsap_cv <dbl>,
#> #   Kitsap_reliability <chr>, Kitsap_se <dbl>, Kitsap_share <dbl>,
#> #   Pierce_cv <dbl>, Pierce_reliability <chr>, Pierce_se <dbl>,
#> #   Pierce_share <dbl>, Snohomish_cv <dbl>, Snohomish_reliability <chr>,
#> #   Snohomish_se <dbl>, Snohomish_share <dbl>, Region_cv <dbl>,
#> #   Region_reliability <chr>, Region_se <dbl>, Region_share <dbl>
```
