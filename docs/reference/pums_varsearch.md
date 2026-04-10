# Search PUMS variable definitions

Look for a var_code via search term

## Usage

``` r
pums_varsearch(regex)
```

## Arguments

- regex:

  search term

## Author

Michael Jensen

## Examples

``` r
# All variables related to internet; shows code changed in 2020 survey
pums_varsearch("internet")
#>      var_code
#>        <char>
#> 1:     ACCESS
#> 2:    HISPEED
#> 3:   OTHSVCEX
#> 4:  SATELLITE
#> 5: ACCESSINET
#>                                                                             var_label
#>                                                                                <char>
#> 1:                                                             Access to the Internet
#> 2: Broadband (high speed) Internet service such as cable, fiber optic, or DSL service
#> 3:                                                             Other Internet service
#> 4:                                                         Satellite Internet service
#> 5:                                                             Access to the Internet
#>                                      years
#>                                     <char>
#> 1:                          2017,2018,2019
#> 2: 2017,2018,2019,2021,2022,2023,2024,2020
#> 3: 2017,2018,2019,2021,2022,2023,2024,2020
#> 4: 2017,2018,2019,2021,2022,2023,2024,2020
#> 5:                2021,2022,2023,2024,2020

# Entry for specific variable, 'Employment Status Recode'
pums_varsearch("^ESR$")
#>    var_code                var_label                                   years
#>      <char>                   <char>                                  <char>
#> 1:      ESR Employment status recode 2017,2018,2019,2021,2022,2023,2024,2020
```
