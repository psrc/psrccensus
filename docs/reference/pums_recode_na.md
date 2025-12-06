# NA recode for PUMS

Helper to the
[`get_psrc_pums`](https://psrc.github.io/psrccensus/reference/get_psrc_pums.md)
function

## Usage

``` r
pums_recode_na(dt)
```

## Arguments

- dt:

  data.table with Census Bureau "N/A" code–"b" or "bbb..."

## Value

filtered input data.table with values "b" recoded to NA

## Author

Michael Jensen
