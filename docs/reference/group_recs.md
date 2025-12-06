# Group ACS or Census Data

Group ACS to Census Data into pre-defined commonly used groupings,
stored in the file: inst/extdata/variables_groupings.

## Usage

``` r
group_recs(tbl, this_group_name)
```

## Arguments

- tbl:

  A data frame of census or acs data

- this_group_name:

  A character string that describes the grouping of the data. Should
  match the group_name in the variables_cats table

## Value

a tibble of grouped ACS or Census estimates

## Author

Suzanne Childress

## Examples

``` r
inc_poverty<-get_acs_recs(geography = 'county',
                         table.names = c('C17002'),
                         years=c(2019),
                         acs.type = 'acs5')
#> Getting data from the 2015-2019 5-year ACS
#> Loading ACS5 variables for 2019 from table C17002. To cache this dataset for faster access to ACS tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per ACS dataset.
#> Using FIPS code '53' for state 'Washington'
#> Using FIPS code '033' for 'King County'
#> Using FIPS code '035' for 'Kitsap County'
#> Using FIPS code '053' for 'Pierce County'
#> Using FIPS code '061' for 'Snohomish County'
group_recs(inc_poverty, 'Poverty Group 100 Percent')
#> # A tibble: 15 × 6
#> # Groups:   GEOID, name, grouping [15]
#>    GEOID  name             grouping   group_name                estimate    moe
#>    <chr>  <chr>            <chr>      <chr>                        <dbl>  <dbl>
#>  1 53033  King County      Over 100%  Poverty Group 100 Percent  1971959  9778.
#>  2 53033  King County      Total      Poverty Group 100 Percent  2165562  1132 
#>  3 53033  King County      Under 100% Poverty Group 100 Percent   193603  5334.
#>  4 53035  Kitsap County    Over 100%  Poverty Group 100 Percent   234868  2889.
#>  5 53035  Kitsap County    Total      Poverty Group 100 Percent   257272   703 
#>  6 53035  Kitsap County    Under 100% Poverty Group 100 Percent    22404  1517.
#>  7 53053  Pierce County    Over 100%  Poverty Group 100 Percent   770285  7055.
#>  8 53053  Pierce County    Total      Poverty Group 100 Percent   859999  1014 
#>  9 53053  Pierce County    Under 100% Poverty Group 100 Percent    89714  3637.
#> 10 53061  Snohomish County Over 100%  Poverty Group 100 Percent   728130  5771.
#> 11 53061  Snohomish County Total      Poverty Group 100 Percent   787169   902 
#> 12 53061  Snohomish County Under 100% Poverty Group 100 Percent    59039  2696.
#> 13 REGION Region           Over 100%  Poverty Group 100 Percent  3705242 13676.
#> 14 REGION Region           Total      Poverty Group 100 Percent  4070002  1902.
#> 15 REGION Region           Under 100% Poverty Group 100 Percent   364760  7159.
```
