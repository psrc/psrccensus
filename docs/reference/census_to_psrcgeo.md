# Translate psrccensus data to planning geographies

Translate psrccensus data to planning geographies

Translate psrccensus data to Regional Geography Classes

Translate psrccensus data to Regional Growth Centers

Translate psrccensus data to Regional Manufacturing-Industrial Centers

Translate psrccensus data to Traffic Analysis Zones

Translate psrccensus data to HCT Station Areas (VISION 2050)

Translate psrccensus data to 2020 Jurisdictional boundaries

## Usage

``` r
census_to_rgs(df, wgt = "total_pop")

census_to_rgc(df, wgt = "total_pop")

census_to_mic(df, wgt = "total_pop")

census_to_taz(df, wgt = "total_pop")

census_to_hct(df, wgt = "total_pop")

census_to_juris(df, wgt = "total_pop")
```

## Arguments

- df:

  acs or decennial dataset returned from psrccensus

- wgt:

  either "total_pop" (default), "household_pop", "group_quarters_pop",
  "housing_units" or "occupied_housing_units"

## Value

Equivalent table with planning geography units instead of census
geography units, and translated value and margin of error

## Author

Michael Jensen
