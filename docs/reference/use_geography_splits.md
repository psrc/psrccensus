# Helper to translate psrccensus estimates to planning geographies

Helper to translate psrccensus estimates to planning geographies

## Usage

``` r
use_geography_splits(
  df,
  planning_geog_type,
  wgt,
  ofm_vintage = "default",
  parcel_year = "default"
)
```

## Arguments

- df:

  acs or decennial dataset returned from psrccensus

- planning_geog_type:

  planning geography type as listed in Elmer.general.geography_splits

- wgt:

  measure share used as split weight either "total_pop",
  "household_pop", "group_quarters_pop", "housing_units" or
  "occupied_housing_units"

- ofm_vintage:

  for deprecated splits; otherwise keep default. See
  \<http://aws-linux/mediawiki/index.php/Get_geography_splits\_(Elmer_Function)\>

- parcel_year:

  for deprecated baseyear; otherwise keep default. See
  \<http://aws-linux/mediawiki/index.php/Get_geography_splits\_(Elmer_Function)\>

## Value

table with planning geography units in place of census geography units

## Author

Michael Jensen
