# Confirm correct datatypes for group variables and weights

Helper to
[`get_psrc_pums`](https://psrc.github.io/psrccensus/reference/get_psrc_pums.md)
function. Makes certain weights are numeric and grouping variables are
factors

## Usage

``` r
ensure_datatypes(dt)
```

## Arguments

- dt:

  PSRC data.table

## Value

PSRC data.table with types confirmed

## Author

Michael Jensen
