# ACS Estimates

Generate ACS estimates for multiple tables by tracts, counties, MSAs, or
places for multiple years using data.table for improved performance.

## Usage

``` r
get_acs_recs(
  geography,
  state = "Washington",
  counties = c("King", "Kitsap", "Pierce", "Snohomish"),
  table.names,
  variables,
  years,
  FIPS = c("14740", "42660"),
  place_FIPS = NULL,
  acs.type
)
```

## Arguments

- geography:

  A character string as either 'tract', 'county', 'msa', 'place', 'block
  group', 'puma', or 'state'.

- state:

  A character string state name or abbreviation. Defaults to Washington.

- counties:

  A character string or vector of counties. Defaults to PSRC counties.

- table.names:

  A character string or vector of Census table; alternative to
  individual variable codes

- variables:

  A character string or vector of individual Census variable codes;
  alternative to table codes

- years:

  A numeric value or vector of years. An ACS year equal or greater than
  2010 to the latest available year.

- FIPS:

  Character string for FIPS codes for specific MSA geographies. Defaults
  to Seattle & Bremerton MSA c("14740","42660")

- place_FIPS:

  Character string of FIPS codes (with state prefix) for specific Census
  Places. If NULL, Places within the PSRC Region will be returned.

- acs.type:

  A character string as either 'acs1', 'acs3' or acs5'.

## Value

a data frame of ACS estimates by selected geography for selected table
codes. Includes variable names.

## Author

Craig Helmann
