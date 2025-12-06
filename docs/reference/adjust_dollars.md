# Dollar variable adjustment for PUMS

Helper to
[`get_psrc_pums`](https://psrc.github.io/psrccensus/reference/get_psrc_pums.md)
function. Applies the Census Bureau-specified inflation adjustment to
dollar values. See vignette for brief discussion. Option to bypass this
inflation adjustment exists, in order to match estimates generated
without it.

## Usage

``` r
adjust_dollars(dt)
```

## Arguments

- dt:

  The data.table

## Value

full input data.table with all dollar values adjusted

## Author

Michael Jensen
