# Function to flexibly convert psrccensus estimates to any regional sf geography

Function to flexibly convert psrccensus estimates to any regional sf
geography

## Usage

``` r
census_to_customgeo(df, custom_geo, custom_geo_var, wgt = "total_pop")
```

## Arguments

- df:

  acs or decennial dataset returned from psrccensus

- custom_geo:

  custom sf file with intended geography/geometry

- custom_geo_var:

  grouping variable from custom geography file, i.e. geography label

- wgt:

  measure share used as split weight either "total_pop" (default),
  "household_pop", "group_quarters_pop", "housing_units" or
  "occupied_housing_units"

## Value

table with custom geographic units in place of census geography units

## Author

Michael Jensen
