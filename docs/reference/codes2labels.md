# Swap variable codes for labels

Helper to
[`get_psrc_pums`](https://psrc.github.io/psrccensus/reference/get_psrc_pums.md)
function. Delivers labels instead of values, where labels exist

## Usage

``` r
codes2labels(dt, dyear, vars)
```

## Arguments

- dt:

  PSRC data.table

- dyear:

  The data year

- vars:

  PUMS variable/s as an UPPERCASE string element or list

## Value

PSRC data.table with labels

## Author

Michael Jensen
