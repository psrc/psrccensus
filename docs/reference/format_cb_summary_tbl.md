# Format Census API return

Helper function for
[`get_acs_single`](https://psrc.github.io/psrccensus/reference/get_acs_single.md)
and
[`get_decennial_single`](https://psrc.github.io/psrccensus/reference/get_decennial_single.md)

## Usage

``` r
format_cb_summary_tbl(dt, geography, census_type)
```

## Arguments

- dt:

  data.table with Census API result

- geography:

  character string, i.e. "block group", "tract", "place", "county",
  "msa", "puma" or "state"

- census_type:

  character string, i.e. "acs" or "decennial"

## Value

dt with fields formatted
